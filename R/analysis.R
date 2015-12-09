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
knitr::opts_chunk$set(warning=FALSE, message=FALSE,
                      tidy.opts=list(width.cutoff=120),  # For code
                      options(width=120))  # For results

# ---------------
#' # Munge data
# ---------------
# Load libraries
library(readr)
library(dplyr)
library(tidyr)
library(readxl)
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
importance.levels <- data_frame(start=c(0, 1, 2),
                                end=c(1, 2, 3),
                                level=c("Not important", "Somewhat important", 
                                        "Most important"),
                                level.ordered=factor(level, levels=level, ordered=TRUE))

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

# Democracy (Freedom House)
fh.url <- "https://freedomhouse.org/sites/default/files/Individual%20Country%20Ratings%20and%20Status%2C%201973-2015%20%28FINAL%29.xls"
fh.tmp <- paste0(tempdir(), basename(fh.url))
download.file(fh.url, fh.tmp)

fh.raw <- read_excel(fh.tmp, skip=6)
# Calculate the number of years covered in the data (each year has three columns)
num.years <- (ncol(fh.raw) - 1)/3

# Create combinations of all the variables and years
var.years <- expand.grid(var = c('PR', 'CL', 'Status'), 
                         year = 1972:(1972 + num.years - 1))

colnames(fh.raw) <- c('country', paste(var.years$var, var.years$year, sep="_"))

# Split columns and convert to long
fh <- fh.raw %>%
  gather(var.year, value, -country) %>%
  separate(var.year, into=c("indicator", "year"), sep="_") %>%
  filter(!is.na(country)) %>%
  spread(indicator, value) %>%
  mutate(year = as.numeric(year),
         CL = suppressWarnings(as.integer(CL)),
         PR = suppressWarnings(as.integer(PR)),
         Status = factor(Status, levels=c("NF", "PF", "F"), 
                         labels=c("Not free", "Partially free", "Free"),
                         ordered=TRUE),
         total.freedom = CL + PR,
         country.clean = countrycode(country, "country.name", "country.name")) %>%
  filter(!is.na(CL) & !is.na(PR)) %>%
  # All the cases we're interested in are after 2000, so we can remove these
  # problematic double countries
  filter(!(country %in% c("Germany, E.", "Germany, W.", "USSR", "Vietnam, N.", 
                          "Vietnam, S.", "Yemen, N.", "Yemen, S."))) %>%
  # Again, because we only care about post-2000 Serbia, merge with Yugoslavia
  mutate(country.clean = ifelse(country.clean == "Yugoslavia", 
                                "Serbia", country.clean)) %>%
  select(-country, country=country.clean)

fh.summary <- fh %>%
  filter(year >= 2000) %>%
  group_by(country) %>%
  summarize(total.freedom = mean(total.freedom, na.rm=TRUE))

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
  left_join(fh.summary, by=c("work.country" = "country")) %>%
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

country.indexes <- responses.full %>%
  filter(!is.na(work.country)) %>%
  group_by(work.country) %>%
  # Needs mutate + mutate_each + select + unique because you can't mix 
  # summarise + summarise_each. See http://stackoverflow.com/a/31815540/120898
  mutate(num.responses = n()) %>%
  mutate_each(funs(score = mean(., na.rm=TRUE), stdev = sd(., na.rm=TRUE), 
                   n = sum(!is.na(.))),
              c(positivity, importance, improvement)) %>%
  select(work.country, num.responses, matches("positivity_|importance_|improvement_")) %>%
  unique %>%
  ungroup() %>%
  arrange(desc(num.responses))


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

# Create a character vector of significance stars
add.stars <- function(x) {
  as.character(symnum(x, corr = FALSE,
                      cutpoints = c(0,  .001,.01,.05, .1, 1),
                      symbols = c("***","**","*","."," ")))
}


# --------------------------------
#' # NGO opinions of US activity
# --------------------------------
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


# ----------------------------------
#' # NGO opinions of US importance
# ----------------------------------
#' ## General importance
plot.data <- responses.full %>%
  group_by(Q3.19) %>%
  summarize(num = n()) %>%
  na.omit() %>%
  mutate(prop = num / sum(num),
         prop.nice = sprintf("%.1f%%", prop * 100),
         Q3.19 = factor(Q3.19, levels=rev(levels(Q3.19)), ordered=TRUE))
plot.data

fig.us_importance <- ggplot(plot.data, aes(x=Q3.19, y=prop)) + 
  geom_bar(stat="identity") + 
  labs(x=NULL, y=NULL) + 
  scale_y_continuous(labels = percent, 
                     breaks = seq(0, max(round(plot.data$num, 1)), by=0.1)) + 
  coord_flip() + theme_clean()
fig.us_importance
ggsave(fig.us_importance, filename=file.path(PROJHOME, "figures", "fig_us_importance.pdf"),
       width=6.5, height=5, units="in", device=cairo_pdf)
ggsave(fig.us_importance, filename=file.path(PROJHOME, "figures", "fig_us_importance.png"),
       width=6.5, height=5, units="in")

#' Average importance by country
importance.plot <- country.indexes %>%
  filter(num.responses >= 10) %>%
  arrange(importance_score) %>%
  mutate(country_label = factor(work.country, levels=unique(work.country), 
                                labels=paste0(work.country, " (", num.responses, ")"),
                                ordered=TRUE)) 

fig.avg_importance <- ggplot(importance.plot, aes(x=country_label, y=importance_score)) + 
  geom_rect(data=importance.levels, aes(x=NULL, y=NULL, ymin=start, ymax=end, 
                                        xmin=0, xmax=Inf, fill=level.ordered), alpha=0.5) + 
  geom_pointrange(aes(ymax=importance_score + importance_stdev,
                      ymin=importance_score - importance_stdev)) + 
  labs(x="Country (number of responses)", 
       y="Importance of the US in anti-TIP efforts (mean)") + 
  scale_fill_manual(values=c("grey90", "grey60", "grey30"), name=NULL) + 
  coord_flip() + 
  theme_clean() + theme(legend.position="bottom")
fig.avg_importance
ggsave(fig.avg_importance, filename=file.path(PROJHOME, "figures", "fig_avg_importance.pdf"),
       width=6.5, height=5, units="in", device=cairo_pdf)
ggsave(fig.avg_importance, filename=file.path(PROJHOME, "figures", "fig_avg_importance.png"),
       width=6.5, height=5, units="in")


#' ## Are opinions of the US's importance associated with…?
df.importance <- responses.full %>% 
  select(Q3.19, work.country, change_policy, avg_tier, change_tip, change_policy, 
         importance, received.funding, us.involvement, total.funding, 
         total.freedom) %>% 
  filter(!is.na(Q3.19)) %>%
  mutate(importance_factor = factor(Q3.19, ordered=FALSE),
         log.total.funding = log1p(total.funding))

#' ### Average tier rating 
#' 
#' Average tier doesn't show much because it doesn't show any changes in
#' time—just how bad the country is in general?
importance.means <- df.importance %>%
  group_by(Q3.19) %>%
  summarize(avg_points = mean(avg_tier, na.rm=TRUE),
            var_points = var(avg_tier, na.rm=TRUE)) %>%
  print

#' Plot group means and distributions
fig.importance <- ggplot(df.importance, aes(x=Q3.19, y=avg_tier)) +
  geom_violin(fill="grey90") + 
  geom_point(alpha=0.05, show.legend=FALSE) +
  geom_point(data=importance.means, aes(x=Q3.19, y=avg_points), size=5, show.legend=FALSE) + 
  labs(x="Opinion of US importance", y="Average TIP tier rating") + 
  coord_flip() + theme_clean()
fig.importance

#' Those means appear slightly different from each other. Is that really the
#' case? Check with ANOVA, which assumes homogenous variance across groups.
#' Throw every possible test at it—if null is rejected (p < 0.05 or whatever)
#' then variance is likely heterogenous:
#' ([helpful reference](http://www.r-bloggers.com/analysis-of-variance-anova-for-multiple-comparisons/))
bartlett.test(avg_tier ~ importance_factor, data=df.importance)
car::leveneTest(avg_tier ~ importance_factor, data=df.importance)
fligner.test(avg_tier ~ importance_factor, data=df.importance)  # Uses median
kruskal.test(avg_tier ~ importance_factor, data=df.importance)  # Nonparametric

#' All of those p-values are tiny, so it's clear that variance is not the same
#' across groups. However, there's a [rule of
#' thumb](http://stats.stackexchange.com/q/56971/3025) ([super detailed
#' example](http://stats.stackexchange.com/a/91881/3025)) that ANOVA is robust
#' to heterogeneity of variance as long as the largest variance is less than 
#' four times the smallest variance.
#' 
#' Given that rule of thumb, the variance here isn't that much of an issue 
df.importance %>% group_by(importance_factor) %>%
  summarise(variance = var(avg_tier, na.rm=TRUE)) %>%
  do(data_frame(ratio = max(.$variance) / min(.$variance)))

#' It would be cool to use Bayesian ANOVA to account for non-homogenous
#' variances (see [John Kruschke's
#' evangelizing](http://doingbayesiandataanalysis.blogspot.mx/2011/04/anova-with-non-homogeneous-variances.html)),
#' since it handles violations of ANOVA assumptions nicely. However, in his
#' example, the ratio of min/max variance is huge, so it *does* lead to big
#' differences in results:
#
# read_csv("http://www.indiana.edu/~kruschke/DoingBayesianDataAnalysis/Programs/NonhomogVarData.csv") %>%
#   group_by(Group) %>%
#   summarise(variance = var(Y)) %>%
#   do(data_frame(ratio = max(.$variance) / min(.$variance)))
#   # ratio = 64
#
#' With the variance issue handled, run the ANOVA:
importance.aov <- aov(avg_tier ~ importance_factor, data=df.importance)
summary(importance.aov)

#' There is some significant difference between groups. Look at pairwise
#' comparisons between all the groups to (kind of) decompose that finding:
(importance.pairs <- TukeyHSD(importance.aov, "importance_factor"))

#' Plot the differences:
importance.pairs.plot <- data.frame(importance.pairs$importance_factor) %>%
  mutate(pair = row.names(.),
         pair = factor(pair, levels=pair, ordered=TRUE),
         stars = add.stars(p.adj))

fig.importance.pairs <- ggplot(importance.pairs.plot, 
                               aes(x=pair, y=diff, ymax=upr, ymin=lwr)) + 
  geom_hline(yintercept=0) + 
  geom_text(aes(label=stars), nudge_x=0.25) +
  geom_pointrange() + 
  theme_clean() + coord_flip()
fig.importance.pairs

#' Another way of checking group means in non-homogenous data is to use ordinal
#' logistic regression. Here's an ordered logit and corresponding predicted
#' probabilities:
model.importance <- ordinal::clm(Q3.19 ~ avg_tier, data=df.importance, 
                                 link="logit", Hess=TRUE)
summary(model.importance)

# Predicted probabilities
newdata <- data_frame(avg_tier = seq(0, 3, 0.1))
pred.importance <- predict(model.importance, newdata, interval=TRUE)

# Create plot data
pred.plot.lower <- cbind(newdata, pred.importance$lwr) %>%
  gather(importance, lwr, -c(1:ncol(newdata)))
pred.plot.upper <- cbind(newdata, pred.importance$upr) %>%
  gather(importance, upr, -c(1:ncol(newdata)))

pred.plot.data <- cbind(newdata, pred.importance$fit) %>%
  gather(importance, importance_prob, -c(1:ncol(newdata))) %>%
  left_join(pred.plot.lower, by=c("avg_tier", "importance")) %>%
  left_join(pred.plot.upper, by=c("avg_tier", "importance"))

importance.colors <- c("grey20", "grey40", "grey60", "grey80")
ggplot(pred.plot.data, aes(x=avg_tier, y=importance_prob)) +  
  geom_ribbon(aes(ymax=upr, ymin=lwr, fill=importance), 
              alpha=0.2) + 
  geom_line(aes(colour=importance), size=2) + 
  scale_y_continuous(labels=percent) + 
  labs(x="Average tier rating in country", 
       y="Predicted probability of assigning importance") + 
  # scale_fill_manual(values=importance.colors, name=NULL) + 
  # scale_colour_manual(values=importance.colors, name=NULL) +
  theme_clean()

#' ### Change in TIP scores
#' Opinions of importance are not related to changes in TIP score. The average
#' change in TIP rating is the same for each possible answer of importance.
ggplot(df.importance, aes(x=Q3.19, y=change_tip)) + 
  geom_violin(fill="grey90") + 
  geom_point(stat="summary", fun.y="mean", size=5) + 
  labs(x="Opinion of US", y="Change in TIP tier rating") + 
  coord_flip() + theme_clean()

#' Variance is equal in all groups:
kruskal.test(change_tip ~ importance_factor, data=df.importance)

#' ANOVA shows no differences:
change.anova <- aov(change_tip ~ importance_factor, data=df.importance) 
summary(change.anova)
TukeyHSD(change.anova)

#' ### Change in Cho scores
#' Opinions of importance vary slightly by changes in Cho policy scores.
#' Respondents who indicated that the US was more important tended to work in
#' countries with greater changes in TIP policy.
ggplot(df.importance, aes(x=Q3.19, y=change_policy)) + 
  geom_violin(fill="grey90") + 
  geom_point(stat="summary", fun.y="mean", size=5) + 
  labs(x="Opinion of US", y="Change in TIP policy index") + 
  coord_flip() + theme_clean()

#' Variance is equal in all groups:
kruskal.test(change_policy ~ importance_factor, data=df.importance)

#' ANOVA *almost* shows some differences (biggest difference between "not
#' important" and "most important" at p = 0.057):
cho.change.anova <- aov(change_policy ~ importance_factor, data=df.importance) 
summary(cho.change.anova)  # (⌐○Ϟ○)  ♥  \(•◡•)/
TukeyHSD(cho.change.anova)

#' ### US funding (org received)
#' Organizations that have been received funding from the US are more likely to
#' consider the US to play an important role in the countries they work in.
funding.table <- df.importance %>%
  xtabs(~ Q3.19 + received.funding, .) %>% print

#' There's an overall significant difference (though two of the cells are
#' really small here)
(funding.chi <- chisq.test(funding.table))

# Cramer's V for standardized measure of association
assocstats(funding.table)

# Components of chi-squared
(components <- funding.chi$residuals^2)
round(1-pchisq(components, funding.chi$parameter), 3)

# Visualize differences
mosaic(funding.table,
       labeling_args=list(set_varnames=c(received.funding="Received US funding", 
                                         Q3.19="Opinion of US"),
                          gp_labels=(gpar(fontsize=8))), 
       gp_varnames=gpar(fontsize=10, fontface=2))

#' ### US funding (country total)
#' Opinions of importance are strongly associated with US TIP funding given to
#' a country. Organizations are more likely to think the US is an important
#' actor if they work in countries receiving more anti-TIP funding.
ggplot(df.importance, aes(x=Q3.19, y=log.total.funding)) + 
  geom_violin(fill="grey90") + 
  geom_point(alpha=0.05) + 
  geom_point(stat="summary", fun.y="mean", size=5) + 
  labs(x="Opinion of US", y="Total TIP funding to country (logged)") + 
  scale_y_continuous(labels=trans_format("exp", dollar_format())) +
  coord_flip() + theme_clean()

#' Variance is not equal in all groups:
kruskal.test(log.total.funding ~ importance_factor, data=df.importance)

#' Ratio is 2.8ish, which is below 4, so heterogenous variance is okayish:
df.importance %>% group_by(importance_factor) %>%
  summarise(variance = var(log.total.funding, na.rm=TRUE)) %>%
  do(data_frame(ratio = max(.$variance) / min(.$variance)))

#' ANOVA shows significant differences:
funding.anova <- aov(log.total.funding ~ importance_factor, data=df.importance) 
summary(funding.anova)
(funding.pairs <- TukeyHSD(funding.anova))

#' See those differences
funding.pairs.plot <- data.frame(funding.pairs$importance_factor) %>%
  mutate(pair = row.names(.),
         pair = factor(pair, levels=pair, ordered=TRUE),
         stars = add.stars(p.adj))

fig.funding.pairs <- ggplot(funding.pairs.plot, 
                            aes(x=pair, y=diff, ymax=upr, ymin=lwr)) + 
  geom_hline(yintercept=0) + 
  geom_text(aes(label=stars), nudge_x=0.25) +
  geom_pointrange() + 
  theme_clean() + coord_flip()
fig.funding.pairs

#' ### TODO: Country rich/poor/democratic
#' ### TODO: Type of TIP work

#' ### Interaction with the US
#' Organizations that have been involved with the US are more likely to
#' consider the US to play an important role in the countries they work in.
involvement.table <- df.importance %>%
  xtabs(~ Q3.19 + us.involvement, .) %>% print

#' There's an overall significant difference
(involvement.chi <- chisq.test(involvement.table))

# Cramer's V for standardized measure of association
assocstats(involvement.table)

# Components of chi-squared
(components <- involvement.chi$residuals^2)
1-pchisq(components, involvement.chi$parameter)

# Visualize differences
mosaic(involvement.table,
       labeling_args=list(set_varnames=c(us.involvement="US involvement", 
                                         Q3.19="Opinion of US"),
                          gp_labels=(gpar(fontsize=8))), 
       gp_varnames=gpar(fontsize=10, fontface=2))


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
#' ---
#' 
#' Does opinion of the US vary by:
#' 
#' * Tier rating (average) or improvement in Cho score?
#' * Whether an NGO has received US funding (or where the COUNTRY has received more TIP grants?)
#' * Whether an NGO has interacted with the US
#' * Whether a country is rich or poor (or some other quality)
#' * Whether an NGO focuses on certain types of work?
#' * TODO: In which countries does the US seem to have had more collaboration with NGOs?
#' * TODO: Explaining variation in opinion of US positivity? (no because censoring)
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

# Find country averages of government improvement, etc. - then show that X number of countries show improvement, etc. 
# Report by organization and by country - how many countries has the US had a positive influence + how many NGOs say the US has had a positive influence


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

