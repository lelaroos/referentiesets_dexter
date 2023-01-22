library(dexter)

load("Workspaces/data_for_dexter.RData")


# Create dexter project ---------------------------------------------------

db = start_new_project(scoring_rules, ':memory:')

add_response_data(db, response_data, design_final)
add_person_properties(db, person_properties)


# Simple test-item analysis -----------------------------------------------

tia = tia_tables(db)

summary(tia$items[c("pvalue", "rit", "rir")])

