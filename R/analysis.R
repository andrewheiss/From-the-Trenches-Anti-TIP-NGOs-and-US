#' ---
#' title: "Anti-TIP NGO survey analysis"
#' author: "Andrew Heiss"
#' date: "`r format(Sys.time(), '%B %e, %Y')`"
#' output: 
#'   html_document: 
#'     css: fixes.css
#'     toc: yes
#'     highlight: pygments
#'     theme: cosmo
#' ---

#+ include=FALSE
# Set up knitr
library(knitr)
knitr::opts_chunk$set(warning=FALSE, message=FALSE)

# ---------------
#' # Munge data
# ---------------
# Load libraries
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(scales)
library(Cairo)
library(grid)
library(vcd)
library(countrycode)

# Load data and add labels
# TODO: Use non-unicode-mangled data
load(file.path(PROJHOME, "data_raw", "responses_orgs.RData"))
load(file.path(PROJHOME, "data_raw", "responses_countries.RData"))
responses.orgs.labs <- read_csv(file.path(PROJHOME, "data_raw", "response_orgs_labels.csv"))
responses.countries.labs <- read_csv(file.path(PROJHOME, "data_raw", "response_countries_labels.csv"))
 
Hmisc::label(responses.orgs, self=FALSE) <- responses.orgs.labs$varlabel
Hmisc::label(responses.countries, self=FALSE) <- responses.countries.labs$varlabel

# Add a nicer short ID to each response to reference in the article
set.seed(1234)
nicer.ids <- data_frame(survey.id = responses.orgs$survey.id) %>%
  mutate(clean.id = 1000 + sample(1:n()))
write_csv(nicer.ids, path=file.path(PROJHOME, "data", "id_lookup_WILL_BE_OVERWRITTEN.csv"))

#' The data is split into two sets: responses.orgs has a row for each surveyed
#' organization and responses.countries has a row for each country
#' organizations responded for (1-4 countries per organization). For ease of
#' analysis, this combines them into one larger dataframe (so
#' organization-level data is repeated). It also removes columns that were
#' added manually, where an RA coded whether a country was mentioned in
#' different questions (with a colum for each country!).
responses.all <- responses.orgs %>% 
  left_join(responses.countries, by="survey.id") %>%
  select(-contains("_c", ignore.case=FALSE)) %>%  # Get rid of all the dummy vars
  left_join(nicer.ids, by="survey.id") %>%
  select(survey.id, clean.id, everything())

#' Convert some responses into numeric indexes:
#' 
#' * **Importance** (Q3.19): Most important actor = 2; Somewhat important actor = 1; 
#'     Not an important actor = 0; Don't know = NA
#' * **Positivity** (Q3.25; *only offered if Q3.19 was most or somewhat important*): 
#'     Negative = -1; Positive = 1; Mixed = 0; Don't know = NA
#' * **Improvement** (Q3.26): Improved = 1; Remained constant = 0; 
#'     Slowed down = -1; Don't know = NA
importance <- data_frame(Q3.19 = levels(responses.countries$Q3.19),
                         importance = c(2, 1, 0, NA))
positivity <- data_frame(Q3.25 = levels(responses.countries$Q3.25),
                         positivity = c(-1, 1, 0, NA))
improvement <- data_frame(Q3.26 = levels(responses.countries$Q3.26),
                          improvement = c(1, 0, -1, NA))

# Cho data
tip.change <- read_csv(file.path(PROJHOME, "data", "policy_index.csv")) %>%
  group_by(countryname) %>%
  summarise(avg_tier = mean(tier, na.rm=TRUE),
            change_tip = -(last(na.omit(tier), default=NA) - 
                             first(na.omit(tier), default=NA)),
            change_policy = last(na.omit(p), default=NA) - 
              first(na.omit(p), default=NA)) %>%
  mutate(cow = countrycode(countryname, "country.name", "cown"))

# Funding
funding.raw <- read_csv(file.path(PROJHOME, "data_raw", "funding_clean.csv")) %>%
  mutate(cowcode = ifelse(country == "Serbia", 555, cowcode),
         countryname = countrycode(cowcode, "cown", "country.name"),
         countryname = ifelse(cowcode == 555, "Serbia", countryname)) %>%
  filter(!is.na(countryname)) 

funding.all <- funding.raw %>%
  group_by(countryname) %>%
  summarise(total.funding = sum(amount, na.rm=TRUE),
            avg.funding = mean(amount, na.rm=TRUE)) 

funding.ngos <- funding.raw %>%
  filter(recipient_type %in% c("NGO", "NPO")) %>%
  group_by(countryname) %>%
  summarise(total.funding.ngos = sum(amount, na.rm=TRUE),
            avg.funding.ngos = mean(amount, na.rm=TRUE)) 

responses.full <- responses.all %>%
  left_join(tip.change, by=c("work.country" = "countryname")) %>%
  left_join(funding.all, by=c("work.country" = "countryname")) %>%
  left_join(funding.ngos, by=c("work.country" = "countryname")) %>%
  left_join(positivity, by = "Q3.25") %>%
  left_join(importance, by = "Q3.19") %>%
  left_join(improvement, by = "Q3.26") %>%
  mutate(received.funding = ifelse(Q3.18_3 != 1 | is.na(Q3.18_3), FALSE, TRUE),
         us.involvement = ifelse(Q3.18_5 != 1 | is.na(Q3.18_5), TRUE, FALSE),
         Q3.19 = factor(Q3.19, levels=c("Most important actor", 
                                        "Somewhat important actor", 
                                        "Not an important actor", 
                                        "Don't know"),
                        ordered=TRUE),
         Q3.25_collapsed = ifelse(Q3.25 == "Negative", NA, Q3.25))

# Useful functions
theme_clean <- function(base_size=9, base_family="Source Sans Pro Light") {
  ret <- theme_bw(base_size, base_family) + 
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          axis.title.x=element_text(vjust=-0.2), axis.title.y=element_text(vjust=1.5),
          title=element_text(vjust=1.2, family="Source Sans Pro Semibold"),
          panel.border = element_blank(), axis.line=element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(size=0.25, colour="grey90"),
          axis.ticks=element_blank(),
          legend.position="bottom", 
          axis.title=element_text(size=rel(0.8), family="Source Sans Pro Semibold"),
          strip.text=element_text(size=rel(0.9), family="Source Sans Pro Semibold"),
          strip.background=element_rect(fill="#ffffff", colour=NA),
          panel.margin=unit(1, "lines"), legend.key.size=unit(.7, "line"),
          legend.key=element_blank())
  
  ret
}

# Return a data frame of counts and proportions for multiple responses
separate.answers.summary <- function(df, cols, labels, total=FALSE) {
  cols.to.select <- which(colnames(df) %in% cols)
  
  denominator <- df %>%
    select(cols.to.select) %>%
    mutate(num.answered = rowSums(., na.rm=TRUE)) %>%
    filter(num.answered > 0) %>%
    nrow()
  
  df <- df %>%
    select(survey.id, cols.to.select) %>%
    gather(question, value, -survey.id) %>%
    mutate(question = factor(question, labels=labels, ordered=TRUE)) %>%
    group_by(question) %>%
    summarize(response = sum(value, na.rm=TRUE), 
              pct = round(response / denominator * 100, 2),
              plot.pct = response / denominator)
  
  colnames(df) <- c("Answer", "Responses", "%", "plot.pct")
  
  if (total) {
    df <- df %>% select(1:3)
    df <- rbind(as.matrix(df), c("Total responses", denominator, "—"))
  }
  
  return(list(df=df, denominator=denominator))
}

# -----------------------------------------------
#' # NGO opinions of US activity and importance
# -----------------------------------------------
# Select just the columns that have cowcodes embedded in them
active.embassies.raw <- responses.countries %>%
  select(contains("_c", ignore.case=FALSE)) %>%
  mutate_each(funs(as.numeric(levels(.))[.]))  # Convert values to numeric

# Select only the rows where they responded (i.e. not all columns are NA)
num.responses <- active.embassies.raw %>%
  rowwise() %>% do(all.missing = all(!is.na(.))) %>%
  ungroup() %>% mutate(all.missing = unlist(all.missing)) %>%
  summarise(total = sum(all.missing))

# Tidy cowcode columns and summarize most commonly mentioned countries
active.embassies <- active.embassies.raw %>%
  gather(country.raw, num) %>%
  group_by(country.raw) %>% summarise(num = sum(num, na.rm=TRUE)) %>%
  mutate(country.raw = str_replace(country.raw, "Q.*c", ""),
         country = countrycode(country.raw, "cown", "country.name"),
         country = ifelse(country.raw == "2070", "European Union", country)) %>%
  ungroup() %>% mutate(prop = num / num.responses$total,
                       prop.nice = sprintf("%.1f%%", prop * 100))

#' Which embassies or foreign governments NGOs were reported as active partners
#' in the fight against human trafficking?
active.embassies.top <- active.embassies %>%
  arrange(num) %>% select(-country.raw) %>%
  filter(num > 10) %>%
  mutate(country = factor(country, levels=country, ordered=TRUE)) %>%
  arrange(desc(num))

active.embassies %>% arrange(desc(num)) %>% 
  select(-country.raw) %>% filter(num > 10)
nrow(active.embassies)  # Number of countries mentioned
num.responses$total  # Total responses

# Most active embassies
# Save Q3.7 to a CSV for hand coding
most.active <- responses.countries %>%
  select(Q3.7) %>%
  filter(!is.na(Q3.7))
write_csv(most.active, path=file.path(PROJHOME, "data", 
                                      "most_active_WILL_BE_OVERWRITTEN.csv"))

# Read in hand-coded CSV
if (file.exists(file.path(PROJHOME, "data", "most_active.csv"))) {
  most.active <- read_csv(file.path(PROJHOME, "data", "most_active.csv"))
} else {
  stop("data/most_active.csv is missing")
}

# Split comma-separated countries, unnest them into multiple rows, and 
# summarize most active countries
most.active.clean <- most.active %>%
  transform(clean = strsplit(clean, ",")) %>%
  unnest(clean) %>%
  mutate(clean = str_trim(clean)) %>%
  group_by(clean) %>%
  summarise(total = n()) %>%
  mutate(prop = total / nrow(most.active),
         prop.nice = sprintf("%.1f%%", prop * 100))

#' Which countries or embassies have been the *most* active?
most.active.clean %>% arrange(desc(total))
nrow(most.active.clean) - 1  # Subtract one because of "None"s

#' Over the last 10–15 years, has the United States or its embassy been active in the fight against human trafficking in X?
responses.countries$Q3.8 %>% table %>% print %>% prop.table

#' Side-by-side graph of active countries + most active countries
plot.data <- active.embassies.top %>%
  bind_rows(data_frame(num=0, country=c("All", "None"), 
                       prop=0, prop.nice="")) %>%
  arrange(num) %>%
  mutate(country = factor(country, levels=country, ordered=TRUE))

plot.data.active <- most.active.clean %>%
  filter(clean %in% plot.data$country) %>%
  mutate(country = factor(clean, levels=levels(plot.data$country), ordered=TRUE))

fig.active <- ggplot(plot.data, aes(x=country, y=num)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label = prop.nice), size=3.5, hjust=1.3, 
            family="Source Sans Pro Light") + 
  labs(x=NULL, y="Number of times country was mentioned as a partner in anti-TIP work") + 
  scale_y_continuous(breaks=seq(0, max(active.embassies$num), by=25), 
                     trans="reverse", expand = c(.1, .1)) + 
  coord_flip() + 
  theme_clean() + 
  theme(axis.text.y = element_blank(), 
        axis.line.y = element_blank(),
        plot.margin = unit(c(1,0.5,1,1), "lines"))

fig.most.active <- ggplot(plot.data.active, aes(x=country, y=total)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label = prop.nice), size=3.5, hjust=-0.3, 
            family="Source Sans Pro Light") + 
  labs(x=NULL, y="Number of times country was mentioned as the most active partner in anti-TIP work") + 
  scale_y_continuous(expand = c(.15, .15)) + 
  coord_flip() + 
  theme_clean() + 
  theme(axis.text.y = element_text(hjust=0.5), 
        axis.line.y = element_blank(),
        plot.margin = unit(c(1,1,1,0), "lines"))
  
fig.embassies <- arrangeGrob(fig.active, fig.most.active, nrow=1)
grid.draw(fig.embassies)
ggsave(fig.embassies, filename=file.path(PROJHOME, "figures", "fig_embassies.pdf"),
       width=5, height=2, units="in", device=cairo_pdf, scale=2.5)
ggsave(fig.embassies, filename=file.path(PROJHOME, "figures", "fig_embassies.png"),
       width=5, height=2, units="in", scale=2.5)


#' Actual US activities
cols <- c("Q3.9_1", "Q3.9_2", "Q3.9_3", "Q3.9_4", "Q3.9_5",
          "Q3.9_6", "Q3.9_7", "Q3.9_8", "Q3.9_9", "Q3.9_10")
labels <- c("Asking for legislation", "Convening conferences or workshops",
            "Raising awareness", "Providing resources or funding",
            "Increasing government attention", "Training government officials",
            "Contributing to a government action plan", "Other", "Don't know",
            "The US has not been involved in trafficking issues")

activities <- separate.answers.summary(responses.countries, cols, labels)
activities$denominator  # Number of responses
activities$df

plot.data <- activities$df %>% 
  mutate(Answer=factor(Answer, levels=rev(labels), ordered=TRUE))

fig.activities <- ggplot(plot.data, aes(x=Answer, y=Responses)) +
  geom_bar(aes(y=plot.pct), stat="identity") + 
  labs(x=NULL, y=NULL) + 
  scale_y_continuous(labels = percent, 
                     breaks = seq(0, max(round(plot.data$plot.pct, 1)), by=0.1)) + 
  coord_flip() + theme_clean()
fig.activities
ggsave(fig.activities, filename=file.path(PROJHOME, "figures", "fig_activities.pdf"),
       width=6.5, height=5, units="in", device=cairo_pdf)
ggsave(fig.activities, filename=file.path(PROJHOME, "figures", "fig_activities.png"),
       width=6.5, height=5, units="in")


# ----------------------------------------------------------------
#' # Variations in opinion of US based on tier rating statistics
# ----------------------------------------------------------------
#' Average tier doesn't show much because it doesn't show any changes in
#' time---just how bad the country is in general
ggplot(responses.full, aes(x=Q3.25, y=avg_tier)) +
  geom_violin() + 
  geom_point(alpha=0.5, position="jitter") +
  labs(x="Opinion of US", y="Average TIP tier rating") + 
  coord_flip()

#' Change in TIP score is a little more interesting---countries that move the
#' most over time only see positive NGO opinions
ggplot(responses.full, aes(x=Q3.25, y=change_tip, fill=Q3.25)) + 
  geom_violin() + 
  geom_point(alpha=0.3, position=position_jitterdodge()) + 
  labs(x="Opinion of US", y="Change in TIP tier rating") + 
  coord_flip()

#' Using the Cho score shows a little more variation, but essentially tells the
#' same thing. NGOs that work in countries that have seen the most actual 
#' improvement in TIP policies almost unanimously have a positive opinion of
#' the US.
ggplot(responses.full, aes(x=Q3.25, y=change_policy, fill=Q3.25)) +
  geom_violin() + 
  geom_point(alpha=0.3, position=position_jitterdodge()) + 
  labs(x="Opinion of US", y="Change in TIP policy index") + 
  coord_flip()

#' NGO opinions of the *importance* of the US's work are unrelated to actual
#' changes, though.
plot.data <- responses.full %>% select(Q3.19, change_policy) %>% filter(!is.na(Q3.19))
ggplot(plot.data, aes(x=Q3.19, y=change_policy, fill=Q3.19)) +
  geom_violin() + 
  geom_point(alpha=0.3, position=position_jitterdodge()) + 
  labs(x="US importance", y="Change in TIP policy index") + 
  coord_flip()


# -----------------------------------------------------------------
#' # Variations in opinion of US based on involvement with the US
# -----------------------------------------------------------------
#' General NGO interaction with the US has no bearing on NGO opinions of the US
us.table <- responses.full %>%
  xtabs(~ Q3.25_collapsed + us.involvement, .) %>% print
chisq.test(us.table)
set.seed(1234)
coindep_test(us.table, n=5000)
mosaic(us.table, 
       labeling_args=list(set_varnames=c(us.involvement="US involvement", 
                                         Q3.25_collapsed="Opinion of US"),
                          gp_labels=(gpar(fontsize=8))), 
       gp_varnames=gpar(fontsize=10, fontface=2))

#' More specifically, the receipt of funding has no bearing on NGO opinions of the US
funding.table <- responses.full %>%
  xtabs(~ Q3.25_collapsed + received.funding, .) %>% print
chisq.test(funding.table)
set.seed(1234)
coindep_test(funding.table, n=5000)
mosaic(funding.table, 
       labeling_args=list(set_varnames=c(received.funding="Received US funding", 
                                         Q3.25_collapsed="Opinion of US"),
                          gp_labels=(gpar(fontsize=8))), 
       gp_varnames=gpar(fontsize=10, fontface=2))

#' NGOs in countries that receive substantial US funding have an opinion of the 
#' US, but there's no difference between positive and mixed opinions. This is
#' true accounting for both average and total funding.

#' Opinions are not driven by cooptation - look at chapter 1 for boomerang type stuff - cooptation by donors - so in this case, the NGOs aren't just being bought out
ggplot(responses.full, aes(x=Q3.25_collapsed, y=total.funding, fill=Q3.25_collapsed)) + 
  geom_violin() + 
  geom_point(alpha=0.3, position=position_jitterdodge()) + 
  labs(x="Opinion of US", y="Total TIP funding to country") + 
  scale_y_continuous(labels=dollar)

ggplot(responses.full, aes(x=Q3.25, y=avg.funding, fill=Q3.25)) + 
  geom_violin() + 
  geom_point(alpha=0.3, position=position_jitterdodge()) + 
  labs(x="Opinion of US", y="Average TIP funding to country") + 
  scale_y_continuous(labels=dollar)

#' The same is true if looking just at US funding designated for just NGOs and NPOs
ggplot(responses.full, aes(x=Q3.25_collapsed, y=total.funding.ngos, fill=Q3.25_collapsed)) + 
  geom_violin() + 
  geom_point(alpha=0.3, position=position_jitterdodge()) + 
  labs(x="Opinion of US", y="Total NGO-only TIP funding to country") + 
  scale_y_continuous(labels=dollar)

ggplot(responses.full, aes(x=Q3.25_collapsed, y=avg.funding.ngos, fill=Q3.25_collapsed)) + 
  geom_violin() + 
  geom_point(alpha=0.3, position=position_jitterdodge()) + 
  labs(x="Opinion of US", y="Average NGO-only TIP funding to country") + 
  scale_y_continuous(labels=dollar)

plot.data <- responses.full %>% select(Q3.19, total.funding) %>% filter(!is.na(Q3.19))
ggplot(plot.data, aes(x=Q3.19, y=total.funding, fill=Q3.19)) + 
  geom_violin() + 
  geom_point(alpha=0.3, position=position_jitterdodge()) + 
  labs(x="US importance", y="Total TIP funding to country") + 
  scale_y_continuous(labels=dollar) +
  coord_flip()


# Type of work
# TODO: work.country is not the most reliable identifier---there be NAs
# Q2.2_X
asdf <- responses.full %>% 
  select(survey.id, matches("Q2.2_\\d$")) %>%
  gather(type_of_work, value, -survey.id) %>%
  mutate(type_of_work = factor(type_of_work, levels=paste0("Q2.2_", seq(1:4)), 
                               labels=c("Organs", "Sex", "Labor", "Other"), 
                               ordered=TRUE))
asdf %>%
  group_by(type_of_work) %>%
  summarise(bloop = n(),
            derp = sum(value, na.rm=TRUE),
            asdf = derp / n())

# Should be 30, 408, 294, 116 with 479 total

# Q2.3_
# Q2.4_X


# Opinion of the US vs. importance
# Can't do this because 3.25 is censored by 3.19
responses.full %>% 
  xtabs(~ Q3.25 + Q3.19, .)

table(responses.full$Q3.25)
table(responses.full$Q3.19)

sum(table(responses.full$Q3.25))
sum(table(responses.full$Q3.19))

responses.full$Q3.19 %>%
  table %>% print %>% prop.table


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# Find country averages of government improvement, etc. - then show that X number of countries show improvement, etc. 
# Report by organization and by country - how many countries has the US had a positive influence + how many NGOs say the US has had a positive influence
country.indexes <- responses.countries %>%
  left_join(positivity, by = "Q3.25") %>%
  left_join(importance, by = "Q3.19") %>%
  left_join(improvement, by = "Q3.26") %>%
  group_by(work.country) %>%
  # Needs mutate + mutate_each + select + unique because you can't mix 
  # summarise + summarise_each. See http://stackoverflow.com/a/31815540/120898
  mutate(num.responses = n()) %>%
  mutate_each(funs(score = mean(., na.rm=TRUE), stdev = sd(., na.rm=TRUE), 
                   n = sum(!is.na(.))),
              c(positivity, importance, improvement)) %>%
  select(work.country, num.responses, 
         matches("positivity_|importance_|improvement_")) %>%
  unique %>%
  ungroup() %>%
  filter(num.responses >= 10) %>%
  arrange(desc(num.responses)) %>%
  mutate(country_label = paste0(work.country, " (", num.responses, ")")) %>%
  mutate(work.country = factor(work.country, levels=work.country, ordered=TRUE))


full <- left_join(country.indexes, tip.change,
                  by=c("work.country" = "countryname")) %>%
  filter(num.responses >= 10) %>%
  mutate(country_label = ifelse(num.responses >= 10, work.country, ""))


ggplot(full, aes(x=improvement_score, y=change_policy, label=work.country)) + 
  geom_point() + geom_text(vjust=1.5) +
  geom_hline(yintercept=0) + 
  scale_x_continuous(limits=c(0, 1)) + 
  scale_y_continuous(limits=c(-2, 6))


ggplot(country.indexes, aes(x=work.country, y=improvement_score)) + 
  geom_bar(stat="identity") + 
  coord_flip()

# Compare improvement scores with actual changes in TIP score to get a sense of if NGO experiences reflect changes in rankings

ggplot(country.indexes, aes(x=work.country, y=positivity_score)) + 
  geom_bar(stat="identity") + 
  coord_flip()


importance.plot <- country.indexes %>%
  arrange(importance_score) %>%
  mutate(country_label = factor(country_label, levels=unique(country_label), 
                                ordered=TRUE))

importance.levels <- data_frame(start=c(0, 1, 2),
                                end=c(1, 2, 3),
                                level=c("Not important", "Somewhat important", 
                                        "Most important"),
                                level.ordered=factor(level, levels=level, ordered=TRUE))

avg.importance <- ggplot(importance.plot, aes(x=country_label, y=importance_score)) + 
  geom_rect(data=importance.levels, aes(x=NULL, y=NULL, ymin=start, ymax=end, 
                                        xmin=0, xmax=Inf, fill=level), alpha=0.5) + 
  geom_pointrange(data=, aes(ymax=importance_score + importance_stdev,
                             ymin=importance_score - importance_stdev)) + 
  labs(x="Country (number of responses)", 
       y="Importance of the US in anti-TIP efforts (mean)") + 
  scale_fill_manual(values=c("#0074D9", "#85144B", "#2ECC40"), name=NULL) + 
  coord_flip() + 
  theme_bw() + theme(legend.position="bottom")
avg.importance
ggsave(avg.importance, filename=file.path(PROJHOME, "figures", "fig_avg_importance.pdf"), 
       width=6.5, height=5, units="in")

responses.countries %>% 
  xtabs(~ Q3.25 + Q3.19, .)


# Importance opinions
importance.opinions <- responses.all %>%
  filter(Q3.19 == "Not an important actor") %>%
  select(survey.id, clean.id, Q3.19, contains("TEXT"), Q4.1)

responses.all$Q3.19 %>%
  table %>% print %>% prop.table


responses.countries %>% 
  xtabs(~ Q3.25 + Q3.26, .)

ggplot(responses.orgs, aes(x = Q1.5.factor)) + geom_bar() + 
  labs(x = Hmisc::label(responses.orgs$Q1.5))


# Importance of US
asdf <- responses.all %>% 
  select(clean.id, Q1.2, Q3.8, Q3.6, Q3.7)

inconsistent.no <- c(1020, 1152, 1226, 1267, 1323, 1405, 1515)
inconsistent.dont.know <- c(1051, 1512)

qwer <- asdf %>%
  mutate(us.active = ifelse(clean.id %in% c(inconsistent.no, inconsistent.dont.know),
                            "Yes", as.character(Q3.8)))

qwer$us.active %>% table %>% print %>% prop.table

sdfg <- qwer %>% group_by(clean.id) %>% 
  summarize(said.no = ifelse(any(us.active == "No", na.rm=TRUE), TRUE, FALSE))
sdfg$said.no %>% table %>% print %>% prop.table




# US importance and positivity



# Importance of report 
responses.orgs$Q2.5 %>% table %>% print %>% prop.table
responses.countries$Q3.23 %>% table %>% print %>% prop.table

heard.of.tip <- responses.countries %>% 
  left_join(responses.orgs, by="survey.id") %>%
  filter(Q2.5 == "Yes") %>%
  group_by(survey.id) %>%
  mutate(know.score = ifelse(Q3.22 == "Don't know", FALSE, TRUE)) %>%
  select(know.score) %>% unique

heard.of.tip$know.score %>% table %>% print %>% prop.table


# Opinions of report
opinions <- responses.all %>% 
  select(clean.id, Q1.2, home.country, work.country, Q3.21_1, Q3.21_4_TEXT, Q3.24.Text)

not.used.tip.ids <- c(1094, 1099, 1106, 1114, 1157, 1221, 1244, 1269, 
                      1314, 1330, 1354, 1357, 1393, 1425)
not.used.tip <- responses.all %>%
  mutate(no.response = ifelse(is.na(Q3.21_1) & is.na(Q3.21_2) & 
                                is.na(Q3.21_3) & is.na(Q3.21_4), TRUE, FALSE),
         explicit.no = ifelse(clean.id %in% not.used.tip.ids, TRUE, FALSE)) %>%
  select(clean.id, Q1.2, Q3.21_1, Q3.21_2, Q3.21_3, Q3.21_4, no.response, explicit.no) %>%
  group_by(clean.id) %>%
  summarize(no.response = ifelse(sum(no.response) > 0, TRUE, FALSE),
            explicit.no = ifelse(sum(explicit.no) > 0, TRUE, FALSE))

not.used.tip$explicit.no %>% table




# Does opinion of the US vary by:
# * Tier rating (average) or improvement in Cho score?
# * Whether an NGO has received US funding (or where the COUNTRY has received more TIP grants?)
# * Whether an NGO has interacted with the US
# * Whether a country is rich or poor (or some other quality)
# * Whether an NGO focuses on certain types of work?
# * In which countries does the US seem to have had more collaboration with NGOs?

