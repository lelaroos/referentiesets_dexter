library(tidyverse)

load("Workspaces/extracted_dataframes.RData")

# Scores table ------------------------------------------------------------

# Create unique person id's
tot_ext <- tot_ext  %>% 
  mutate(person_id = row_number(), .before = 1)

# function that takes an "extended" df and turns it into longer format
longdf_ext <- function(df){
  df %>% 
    pivot_longer(cols = starts_with("T", ignore.case = FALSE),
                 names_to = 'item_id',
                 values_to = 'score') %>% 
    filter(!is.na(score)) %>% 
    # if score = 99 (missing response) change to score = 0
    mutate(score = replace(score, score == 99, 0))
}

# Pivot scores table to longer format
scores_table <- longdf_ext(tot_ext) %>% 
  rename(booklet_id = test.versie) # rename test.versie to booklet_id


# Design table ------------------------------------------------------------

# Rename V1 to booklet id
tot_design <- tot_design %>% 
  rename(booklet_id = V1)

# Pivot design table to long format
design <- tot_design %>% 
  pivot_longer(cols = starts_with("V"),
               names_to = "item_position", # column number = position + 1
               names_prefix = "V",
               names_transform = list(item_position = as.integer),
               values_to = 'item_id',
               values_drop_na = T) %>% 
  filter(item_id != "") %>% # remove empty cells
  mutate(item_position = item_position - 1) # columns started at 2 so minus 1


# Response table ----------------------------------------------------------

# Add person id's
tot_resp <- tot_resp  %>% 
  mutate(person_id = row_number(), .before = 1)

# Identify item id's
booklets_resp <- tot_resp %>% 
  group_split(booklet_id)

# Function that takes a data frame containing data from a single booklet 
# and changes the column names into the corresponding item id's.

identify_items <- function(df){
  booklet <- df$booklet_id[1] # booklet_id
  for (v in 7:ncol(df)){
    pos <- v - 6 # position in booklet
    # use position and booklet_id to identify the item_id in the design df
    item_id <- design %>% 
      filter(booklet_id == booklet & item_position == pos) 
    colnames(df)[v] <- as.character(item_id[4]) # rename column to item_id
  }
  df %>% discard(~all(is.na(.) | . =="")) # remove NA/empty cells
}

# Use this function to creade response table 

# Add booklet 1 
response_table_w <- identify_items(booklets_resp[[1]]) 

# Iteratively add other booklets 

for (i in 2:length(booklets_resp)){
  df <- identify_items(booklets_resp[[i]])
  response_table_w <- response_table_w %>%  
    full_join(df)
}

# Pivot response table to longer format

response_table <- response_table_w %>% 
  pivot_longer(cols = starts_with("T", ignore.case = FALSE),
               names_to = 'item_id',
               values_to = 'response') %>% 
  filter(!is.na(response)) %>% 
  mutate(response = str_trim(response)) # remove empty space in response

# There's a problem with item T3F_54, it has been split up into 
# three items: T3F_54, T3F_54A, and T3F_54B. 

# The problem does not occur in the scoring, where we only have T3F_54. 
# We also have no responses for T3F_54B, so we assume that the responses 
# to T3F_54A are the actual responses to question T3F_54.

response_table <- response_table %>% 
  mutate(item_id = replace(item_id, item_id == "T3F_54A", "T3F_54"))

# In the design data frame we also remove T3F_54B, 
# and remove the A from T3F_54A as well.

design <- design %>% 
  filter(item_id != "T3F_54B") %>% 
  mutate(item_id = replace(item_id, item_id == "T3F_54A", "T3F_54"))


# Scoring rules ------------------------------------------------------------

# Add responses and scores together to create rules
full_table <- response_table %>% 
  full_join(scores_table)

# Obtain scoring rules
scoring_rules <- full_table %>% 
  mutate(item_score = score) %>% 
  distinct(item_id, response, item_score)

# Change problematic scoring rules
scoring_rules <- scoring_rules %>% 
  mutate(item_score = 
           case_when(
             item_id == "T3F_06" & response == "A" ~ 1,
             item_id == "T3F_06" & response == "D" ~ 0,
             item_id == "T3F_21" & response == "B" ~ 1,
             item_id == "T3F_23" & response == "BABA" ~ 1,
             TRUE ~ item_score
           ) 
  ) %>% 
  distinct(item_id, response, item_score)

# Final data cleaning -----------------------------------------------------
# Remove redundant information 

person_properties <- response_table %>% 
  distinct(person_id, school_id)

response_data <- response_table %>% 
  select(person_id, booklet_id, item_id, response)

design_final <- design %>% 
  select(booklet_id, item_id, item_position)


# Save data and rules -----------------------------------------------------

save(person_properties, response_data, scoring_rules, design_final,
     file = "Workspaces/data_for_dexter.RData") 


