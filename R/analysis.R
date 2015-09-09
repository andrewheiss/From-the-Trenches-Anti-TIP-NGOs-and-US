library(readr)
library(dplyr)
library(ggplot2)

# Load data and add labels
load("data_raw/responses_orgs.RData")
load("data_raw/responses_countries.RData")
responses.orgs.labs <- read_csv("data_raw/response_orgs_labels.csv")
responses.countries.labs <- read_csv("data_raw/response_countries_labels.csv")

Hmisc::label(responses.orgs, self=FALSE) <- responses.orgs.labs$varlabel
Hmisc::label(responses.countries, self=FALSE) <- responses.countries.labs$varlabel


# Find country averages of government improvement, etc. - then show that X number of countries show improvement, etc. 
# Report by organization and by country - how many countries has the US had a positive influence + how many NGOs say the US has had a positive influence
thing <- responses.countries %>%
  left_join(improvement, by = "Q3.25") %>%
  group_by(work.country) %>%
  summarize(improvement = mean(Q3.25_num, na.rm=TRUE),
            num.responses = n()) %>%
  filter(num.responses > 5) %>%
  arrange(desc(improvement))

ggplot(thing, aes(x=work.country, y=improvement)) + 
  geom_bar(stat="identity") + 
  coord_flip()
hist(thing$improvement)
  # select(Q3.25, Q3.25_num)

improvement <- data_frame(Q3.25 = c("Negative", "Positive", "Mixed", "Don't know"),
                          Q3.25_num = c(-1, 1, 0, NA))

responses.countries %>% 
  xtabs(~ Q3.25 + Q3.26, .)





ggplot(responses.orgs, aes(x = Q1.5.factor)) + geom_bar() + 
  labs(x = Hmisc::label(responses.orgs$Q1.5))

plot(responses.orgs$Q1.5.factor)
