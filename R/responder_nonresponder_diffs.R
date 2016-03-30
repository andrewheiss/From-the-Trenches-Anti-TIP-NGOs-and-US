library(dplyr)
library(tidyr)
library(ggplot2)
library(ggstance)
library(readxl)
library(countrycode)

master.list <- read_excel("~/Research/••Projects/Human trafficking/Master NGO list.xlsx", na=".")

actual.responses <- master.list %>% 
  filter(`Survey status` == "Done") %>% select(ID)

non.responses <- master.list %>%
  filter(Contacted != "No") %>%
  filter(!(`Survey status` %in% c("Done", "E-mail dead"))) %>%
  filter(is.na(`Remove?`)) %>% select(ID)

# Sanity check
which(actual.responses$ID %in% non.responses$ID)

# Make variable for response/no response
df <- master.list %>%
  mutate(responses = ifelse(ID %in% actual.responses$ID, "Response", 
                            ifelse(ID %in% non.responses$ID, "No response", 
                                   NA))) %>%
  filter(!is.na(responses)) %>%
  mutate(region = countrycode(ISO3, "iso3c", "region"),
         continent = countrycode(ISO3, "iso3c", "continent"))

# ---------------------------------
# Check for differences in region
# ---------------------------------
responses.hq.region <- df %>%
  xtabs(~ continent + responses, .) %>% print

(hq.region.chi <- chisq.test(responses.hq.region))

# Cramer's V for standardized measure of association
assocstats(responses.hq.region)

# Components of chi-squared
(components <- hq.region.chi$residuals^2)
round(1-pchisq(components, hq.region.chi$parameter), 3)

# Visualize differences
mosaic(responses.hq.region,
       labeling_args=list(set_varnames=c(continent="Region", 
                                         responses="Responded to survey"),
                          gp_labels=(gpar(fontsize=8))), 
       gp_varnames=gpar(fontsize=10, fontface=2))

# More Asian NGOs didn't respond; more NGOs from Africa and Oceania responded


# -------------------------------
# Check for differences in type
# -------------------------------
responses.type <- df %>%
  xtabs(~ Type + responses, .) %>% print

(type.chi <- chisq.test(responses.type))

# Cramer's V for standardized measure of association
assocstats(responses.type)

# Components of chi-squared
(components <- type.chi$residuals^2)
round(1-pchisq(components, type.chi$parameter), 3)

# Visualize differences
mosaic(responses.type,
       labeling_args=list(set_varnames=c(Type="Type of NGO", 
                                         responses="Responded to survey"),
                          gp_labels=(gpar(fontsize=8))), 
       gp_varnames=gpar(fontsize=10, fontface=2))

# No difference in type of advocacy (though this measure is pretty inconsistent)

# ------------------------------------
# Check for differences in TIP focus
# ------------------------------------
responses.focus <- df %>%
  mutate(focus = ifelse(tolower(`Primary focus on TIP?`) == "yes", "Yes",
                        ifelse(tolower(`Primary focus on TIP?`) == "no", "No", 
                               `Primary focus on TIP?`))) %>%
  filter(focus != "") %>%
  xtabs(~ focus + responses, .) %>% print

prop.table(responses.focus, margin=1)

(focus.chi <- chisq.test(responses.focus))

# Cramer's V for standardized measure of association
assocstats(responses.focus)

# Components of chi-squared
(components <- focus.chi$residuals^2)
round(1-pchisq(components, focus.chi$parameter), 3)

# Visualize differences
mosaic(responses.focus,
       labeling_args=list(set_varnames=c(Type="Type of NGO", 
                                         responses="Responded to survey"),
                          gp_labels=(gpar(fontsize=8))), 
       gp_varnames=gpar(fontsize=10, fontface=2))

# Big difference in TIP focus; organizations with a primary focus on TIP issues
# far more likely to respond to the survey
