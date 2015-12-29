library(readr)
library(dplyr)

# Load original full data (poorly encoded)
load(file.path(PROJHOME, "data_raw", "responses_orgs_bad.RData"))
load(file.path(PROJHOME, "data_raw", "responses_countries_bad.RData"))
responses.orgs.bad <- responses.orgs
responses.countries.bad <- responses.countries


# Load properly encoded data (missing the manually coded country columns)
responses.orgs.good <- read_csv(file.path(PROJHOME, "data_raw", 
                                          "responses_orgs.csv")) %>%
  select(survey.id, Q4.1)

responses.countries.good <- read_csv(file.path(PROJHOME, "data_raw", 
                                               "responses_countries.csv")) %>%
  select(survey.id, Q3.5_5_TEXT, Q3.10:Q3.13, Q3.14:Q3.17, 
         Q3.18_4_TEXT, Q3.21_4_TEXT, Q3.24.Text = Q3.24, Q3.30)


# Replace bad columns
responses.orgs <- responses.orgs.bad %>%
  select(-Q4.1) %>%  # Get rid of bad columns
  left_join(responses.orgs.good, by="survey.id") %>%  # Add clean columns
  select_(.dots=colnames(responses.orgs.bad))  # Maintain original column order

responses.countries <- responses.countries.bad %>%
  select(-c(Q3.5_5_TEXT, Q3.10:Q3.13, Q3.14:Q3.17, Q3.18_4_TEXT, 
            Q3.21_4_TEXT, Q3.24.Text, Q3.30)) %>%  # Get rid of bad columns
  left_join(responses.countries.good, by="survey.id") %>%  # Add clean columns
  select_(.dots=colnames(responses.countries.bad)) %>%  # Maintain original column order
  group_by(survey.id, loop.number) %>% 
  slice(1) %>% ungroup()  # left_join() duplicates some rows inexplicably


# Write files
write_rds(responses.orgs, 
          file.path(PROJHOME, "data_raw", "responses_orgs_clean.rds"),
          compress="xz")
write_rds(responses.countries, 
          file.path(PROJHOME, "data_raw", "responses_countries_clean.rds"),
          compress="xz")
