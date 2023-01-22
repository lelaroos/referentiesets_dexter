library(xml2)
library(tidyverse)

## This read_messy_csv function was provided by Jesse Koops to easily extract the Referentieset data from the internet:

url = 'http://www.toetsspecials.nl/html/referentiesets_openbaar/taal.shtm'
doc = read_html(url)

filenames = xml_find_all(doc, "//a[contains(@href,'csv') ]/@href") %>% 
  xml_text() %>%
  paste0('http://www.toetsspecials.nl', .)

read_messy_csv = function(file_url, ...)
{
  tmp = tempfile(fileext='.csv')
  download.file(url=file_url, destfile=tmp)
  l = readLines(tmp)
  sp = str_count(l,';')
  ncol = max(sp)
  l = paste0(l, sapply(ncol-sp, function(x) paste0(rep(';',x), collapse='')))
  writeLines(l,tmp)
  read.csv2(tmp, ...) 
}

## We use this function to obtain all relevant data files:

# first create names for dataframes
clean_filenames <- filenames %>% 
  str_replace("http://www.toetsspecials.nl/html/referentiesets_openbaar/taal/taal_", "") %>% 
  str_replace(".csv", "")

# extract list of school types per file
types = sapply(strsplit(clean_filenames, "_"), "[", 1)


ext_list <- list()
des_list <- list()
resp_list <- list()

# then extract all data
for (i in 1:length(filenames)){
  type = types[i]
  
  # create "extended" data frames
  if (str_detect(filenames[i], "extended")){
    df = read_messy_csv(filenames[i]) %>% 
      # add school id and school type as variables
      mutate(school_id = paste(school.label, type, sep = "_"), 
             .before = 1) %>% 
      mutate(school_type = type, 
             .before = 1) 
    # give the dataframe the correct name
    assign(clean_filenames[i], df, envir = .GlobalEnv)
  }
  
  # create "antwoord" data frames 
  else if (str_detect(filenames[i], "antwoord")){
    df = read_messy_csv(filenames[i], header = F) 
    colnames(df)[1] <- "booklet_id"
    colnames(df)[2] <- "school.label"
    colnames(df)[3]<- "student.label"
    df = df %>% 
      # add school id and school type as variables 
      mutate(school_id = paste(school.label, type, sep = "_"),
             .before = 1) %>% 
      mutate(school_type = type, .before = 1)
    
    # give the dataframe the correct name
    assign(clean_filenames[i], df, envir = .GlobalEnv)
  }
  
  # create "design" data frames
  else if (str_detect(filenames[i], "design")){
    df = read_messy_csv(filenames[i], header = F) %>%  
      # add school type
      mutate(school_type = type, .before = 1) 

    # give the dataframe the correct name
    assign(clean_filenames[i], df, envir = .GlobalEnv)
  }
}

## Create data frames
# Extended data frames to create scores table
ext_list <- list(BBKB_extended, GT_extended, HAVO_extended, 
                 MBO2_extended, MBO4_extended, PO_extended,
                 VWO_extended)
tot_ext <- bind_rows(ext_list)

# Design data frames to create design table
des_list <- list(BBKB_design, GT_design, HAVO_design, 
                 MBO2_design, MBO4_design, PO_design, VWO_design)

tot_design <- bind_rows(des_list)

# Antwoord data frames to create response table
resp_list <- list(BBKB_antwoord, GT_antwoord, HAVO_antwoord, 
                  MBO2_antwoord, MBO4_antwoord, PO_antwoord, VWO_antwoord)
tot_resp <- bind_rows(resp_list) 


## Save data frames
saveRDS(tot_ext, "Data frames/tot_ext.rds")
saveRDS(tot_design, "Data frames/tot_design.rds")
saveRDS(tot_resp, "Data frames/tot_resp.rds")

save(tot_ext, tot_design, tot_resp,file = "Workspaces/extracted_dataframes.RData") 
