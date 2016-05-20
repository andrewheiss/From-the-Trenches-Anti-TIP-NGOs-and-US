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
#'     includes:
#'       after_body: jump.html
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
library(magrittr)
library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(ggplot2)
library(gridExtra)
library(ggstance)
library(scales)
library(Cairo)
library(grid)
library(vcd)
library(countrycode)
library(maptools)
library(rgdal)

# Load data and add labels
responses.orgs <- readRDS(file.path(PROJHOME, "data_raw", 
                                    "responses_orgs_clean.rds"))
responses.countries <- readRDS(file.path(PROJHOME, "data_raw", 
                                         "responses_countries_clean.rds"))

responses.orgs.labs <- read_csv(file.path(PROJHOME, "data_raw", 
                                          "response_orgs_labels.csv"))
responses.countries.labs <- read_csv(file.path(PROJHOME, "data_raw", 
                                               "response_countries_labels.csv"))

Hmisc::label(responses.orgs, self=FALSE) <- responses.orgs.labs$varlabel
Hmisc::label(responses.countries, self=FALSE) <- responses.countries.labs$varlabel

# Add survey sources
phone <- readRDS(file.path(PROJHOME, "data_raw", "phone.rds"))
linkedin <- readRDS(file.path(PROJHOME, "data_raw", "linkedin.rds"))

responses.orgs %<>% 
  mutate(survey.method = ifelse(survey.id %in% phone, "Phone", 
                                ifelse(survey.id %in% linkedin, "LinkedIn", 
                                       "Online")))

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
# TODO: Someday get this directly from the internet, like Freedom House data
#   * http://www.economics-human-trafficking.org/data-and-reports.html
# Except that data doesn't include tier scores, so that would need to come 
# from somewhere else...
tip.change <- read_csv(file.path(PROJHOME, "data", "policy_index.csv")) %>%
  group_by(countryname) %>%
  summarise(avg_tier = mean(tier, na.rm=TRUE),
            improve_tip = (last(na.omit(tier), default=NA) - 
                             first(na.omit(tier), default=NA)),
            change_policy = last(na.omit(p), default=NA) - 
              first(na.omit(p), default=NA)) %>%
  mutate(countryname = countrycode(countryname, "country.name", "country.name"))

# Democracy (Freedom House)
if (!file.exists(file.path(PROJHOME, "data_external", "freedom_house.xls"))) {
  fh.url <- paste0("https://freedomhouse.org/sites/default/files/", 
                   "Individual%20Country%20Ratings%20and%20Status%2C%20",
                   "1973-2015%20%28FINAL%29.xls")
  fh.tmp <- file.path(PROJHOME, "data_external", "freedom_house.xls")
  download.file(fh.url, fh.tmp)
}

fh.raw <- read_excel(file.path(PROJHOME, "data_external", "freedom_house.xls"), 
                     skip=6)

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
  filter(work.only.us != "Yes") %>%
  mutate(work.country.clean = countrycode(work.country, 
                                          "country.name", "country.name"),
         work.country.clean = ifelse(is.na(work.country), 
                                     "Global", work.country.clean),
         work.country = work.country.clean) %>%
  left_join(tip.change, by=c("work.country" = "countryname")) %>%
  left_join(funding.all, by=c("work.country" = "countryname")) %>%
  left_join(funding.ngos, by=c("work.country" = "countryname")) %>%
  left_join(fh.summary, by=c("work.country" = "country")) %>%
  left_join(positivity, by = "Q3.25") %>%
  left_join(importance, by = "Q3.19") %>%
  left_join(improvement, by = "Q3.26") %>%
  mutate(received.funding = ifelse(Q3.18_3 != 1 | is.na(Q3.18_3), FALSE, TRUE),
         us.involvement = ifelse(Q3.18_5 != 1 | is.na(Q3.18_5), TRUE, FALSE),
         us.hq = ifelse(home.country == "United States", TRUE, FALSE),
         Q3.19 = factor(Q3.19, levels=c("Most important actor", 
                                        "Somewhat important actor", 
                                        "Not an important actor", 
                                        "Don't know"),
                        ordered=TRUE),
         Q3.25_collapsed = ifelse(Q3.25 == "Negative", NA, Q3.25)) %>%
  mutate(home.region = countrycode(home.country, "country.name", "continent"),
         home.region = ifelse(home.country == "Kosovo", "Europe", home.region),
         home.region = ifelse(home.country == "TWN", "Asia", home.region),
         home.region = ifelse(home.region == "Oceania", "Asia", home.region),
         home.region = ifelse(home.region == "Asia", "Asia and Oceania", home.region),
         work.region = countrycode(work.country, "country.name", "continent"),
         work.region = ifelse(work.country == "Kosovo", "Europe", work.region),
         work.region = ifelse(work.country == "TWN", "Asia", work.region),
         work.region = ifelse(work.region == "Oceania", "Asia", work.region),
         work.region = ifelse(work.region == "Asia", "Asia and Oceania", work.region)) %>%
  mutate(home.iso3 = countrycode(home.country, "country.name", "iso3c"),
         home.iso3 = ifelse(home.country == "Kosovo", "KOS", home.iso3)) %>%
  mutate(work.iso3 = countrycode(work.country, "country.name", "iso3c"),
         work.iso3 = ifelse(work.country == "Kosovo", "KOS", work.iso3))

country.indexes <- responses.full %>%
  filter(!is.na(work.country)) %>%
  group_by(work.country) %>%
  # Needs mutate + mutate_each + select + unique because you can't mix 
  # summarise + summarise_each. See http://stackoverflow.com/a/31815540/120898
  mutate(num.responses = n()) %>%
  mutate_each(funs(score = mean(., na.rm=TRUE), 
                   stdev = sd(., na.rm=TRUE), 
                   n = sum(!is.na(.)),
                   std.err = sd(., na.rm=TRUE) / sqrt(sum(!is.na(.)))),
              c(positivity, importance, improvement)) %>%
  select(work.country, num.responses, matches("positivity_|importance_|improvement_")) %>%
  unique %>%
  ungroup() %>%
  arrange(desc(num.responses))

# Load map data
if (!file.exists(file.path(PROJHOME, "data_external", "map_data", 
                           "ne_110m_admin_0_countries.VERSION.txt"))) {
  map.url <- paste0("http://www.naturalearthdata.com/", 
                    "http//www.naturalearthdata.com/download/110m/cultural/", 
                    "ne_110m_admin_0_countries.zip")
  map.tmp <- file.path(PROJHOME, "data_external", basename(map.url))
  download.file(map.url, map.tmp)
  unzip(map.tmp, exdir=file.path(PROJHOME, "data_external", "map_data"))
  unlink(map.tmp)
}

countries.map <- readOGR(file.path(PROJHOME, "data_external", "map_data"), 
                         "ne_110m_admin_0_countries")
countries.robinson <- spTransform(countries.map, CRS("+proj=robin"))
countries.ggmap <- fortify(countries.robinson, region="iso_a3") %>%
  filter(!(id %in% c("ATA", -99))) %>%  # Get rid of Antarctica and NAs
  mutate(id = ifelse(id == "GRL", "DNK", id))  # Greenland is part of Denmark

# All possible countries (to fix the South Sudan issue)
possible.countries <- data_frame(id = unique(as.character(countries.ggmap$id)))


# Save data
# TODO: Make responses.full more anonymous before making it public
# TODO: Save as CSV and Stata too instead of just R
saveRDS(responses.full, file.path(PROJHOME, "data", "responses_full.rds"))
saveRDS(country.indexes, file.path(PROJHOME, "data", "country_indexes.rds"))

# CSV for book plots
df.book.plots <- responses.full %>% 
  select(clean.id, starts_with("Q3.18"), 
         starts_with("Q3.21"), -contains("TEXT"))
write_csv(df.book.plots, 
          file.path(PROJHOME, "data", "data_q3_18_21.csv"))

df.book.importance.positivity <- responses.full %>%
  select(clean.id, Q3.19, Q3.25, Q3.25_collapsed)
write_csv(df.book.importance.positivity,
          file.path(PROJHOME, "data", "data_q3_19_25.csv"))

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

# For maps
theme_blank_map <- function(base_size=9, base_family="Source Sans Pro Light") {
  ret <- theme_bw(base_size, base_family) + 
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          title=element_text(vjust=1.2, family="Source Sans Pro Semibold"),
          panel.border=element_blank(), axis.line=element_blank(),
          panel.grid=element_blank(), axis.ticks=element_blank(),
          axis.title=element_blank(), axis.text=element_blank(),
          legend.text=element_text(size=rel(0.7), family="Source Sans Pro Light"),
          legend.title=element_text(size=rel(0.7), family="Source Sans Pro Semibold"),
          strip.text=element_text(size=rel(1), family="Source Sans Pro Semibold"))
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


#-----------------------------------------
#' # General information from the survey
#-----------------------------------------
#' ## Response rate
#' Calculating the response rate is a little tricky because of all the
#' different ways we sent out invitations for and conducted the survey
#' (LinkedIn, e-mail, phone). 
#' 
#' ### Denominator calculations
entries.in.list <- 1421  # Organizations in the master NGO list
no.details <- 98  # Organizations that we never tried to make any contact with ever
bounced.invitations <- 132  # Contact information was dead; never saw the survey
not.ngos <- 69  # Organizations that weren't NGOs
duplicates <- 19  # Duplicate entries

orgs.saw.survey <- entries.in.list - no.details - bounced.invitations
viable.entries <- orgs.saw.survey - not.ngos - duplicates

#' We assume that there were `r orgs.saw.survey` organizations that saw the link to the survey at least three times. Of those, `r not.ngos` entries were clearly not NGOs or do not work on human trafficking issues (some were government offices, others only do fundraising, etc.), and most of these organizations responded to the e-mail explaining their situation. `r duplicates` entires were duplicates of other entries (generally one entry used the foreign name and one used the English translation).
#' 
#' Given all this, the denominator for our survey's response rate is `r viable.entries`.
#' 
#' ### Numerator calculations
(num.total.responses <- nrow(responses.full %>% group_by(survey.id) %>% slice(1)))

#' ### Final response rate
num.total.responses / viable.entries

#' ## Other survey metadata
#' How many times did respondents loop through the country questions?
(num.country.orgs <- nrow(responses.full))

responses.full %>%
  group_by(survey.id) %>%
  summarise(loops = n()) %>%
  do(data.frame(table(.$loops, dnn="Countries"))) %>%
  mutate(prop = Freq / sum(Freq))

#' How did respondents take the survey?
responses.full %>%
  group_by(survey.id) %>%
  slice(1) %>%  # Select each organization's first country
  group_by(survey.method) %>%
  summarise(num = n()) %>%
  mutate(prop = num / sum(num))

#' How many respondents answered at least one free response question?
responses.full %>%
  select(Q3.10:Q3.17, Q3.24.Text, Q3.30, Q4.1) %T>%
  {cat("Number of free response questions:", ncol(.), "\n")} %>%
  rowwise() %>% do(wrote.something = !all(is.na(.))) %>%
  ungroup() %>% mutate(wrote.something = unlist(wrote.something)) %>%
  bind_cols(select(responses.full, survey.id)) %>%
  group_by(survey.id) %>%
  summarise(wrote.something = ifelse(sum(wrote.something) > 0, TRUE, FALSE)) %T>%
  {cat("Number responses:", nrow(.), "\n")} %>%
  do(as.data.frame(table(.$wrote.something, dnn="Wrote something"))) %>%
  mutate(Percent = Freq / sum(Freq))

#' Export free response questions for manual analysis
responses.full %>% 
  select(survey.id, clean.id, home.country, work.country, 
         Q3.10:Q3.13, Q3.14:Q3.17, Q3.24.Text, Q3.30, Q4.1) %>%
  write_csv(path=file.path(PROJHOME, "data", "free_responses.csv"))

#' How many organizations did not list the US as an anti-TIP actor but later
#' indicated US support?
no.mention.us <- responses.countries %>% 
  select(survey.id, Q3.6_c2) %>% filter(Q3.6_c2 == 0)# filter(!is.na(Q3.6_c2))

inndicated.us.activity <- responses.full %>%
  select(survey.id, Q3.8) %>% filter(Q3.8 == "Yes")

sum(no.mention.us$survey.id %in% inndicated.us.activity$survey.id)

#' How many organizations completed the survey after it turned to US-related questions? That's hard to tell because the US questions were in the loop, and Qualtrics does not keep completion statistics for questions potenitally hidden by display logic.
final.qualtrics.count <- 511

survey.duration.completed <- responses.full %>%
  select(survey.id, start.time, end.time) %>%
  group_by(survey.id) %>% slice(1) %>% ungroup() %>%
  mutate(time.spent = end.time - start.time,
         mins.spent = time.spent / 60)

median(as.numeric(survey.duration.completed$mins.spent))

survey.duration <- read_csv(file.path(PROJHOME, "data_raw", "response_times.csv")) %>%
  mutate(pct = num / sum(num),
         cum.num = cumsum(num),
         validish.num = sum(num) - cum.num) %T>%
  {print(head(.))}

longer.than.five <- filter(survey.duration, minutes_spent==6)$validish.num

#' We attempted to estimate the post-US completion rate in two ways. First, we 
#' counted the number of respondents that spent more than 5 minutes taking the 
#' survey (assuming that is sufficient time to make it to the US-focused 
#' questions), yielding `r longer.than.five` respondents, which is `r 
#' longer.than.five - final.qualtrics.count` more than the final number of 
#' organizations (`r final.qualtrics.count`). (Sidenote:
#' `r final.qualtrics.count` is bigger than the final official count of `r 
#' num.total.responses` because we filtered out US-only responses completed
#' prior to adding internal validation logic and we removed duplicate or
#' invalid responses prior to analysis) However, because the survey filtered
#' out organizations that only work in the US, most respondents were let out of
#' the survey early. Assuming those who took more than 5 minutes completed the 
#' survey, we had a completion rate of 
#' `r round(final.qualtrics.count / longer.than.five, 3) * 100`%.

pct.completed <- read_csv(file.path(PROJHOME, "data_raw", "pct_completed.csv")) %>%
  mutate(pct = num / sum(num))

more.than.20 <- sum(filter(pct.completed, pct_complete >= 0.2)$num)

#' Second, we looked at overall completion rates, which again are not entirely 
#' accurate because of Qualtrics' internal question looping logic—i.e., 
#' organizations that completed the full survey are only internally marked as 
#' completing 20-30% of it. `r more.than.20` organizations completed at least 
#' 20% of the survey. Assuming at least 20% represents approximate survey 
#' completion, we had a completion rate of `r round(final.qualtrics.count / 
#' more.than.20, 3) * 100`%.
#' 
#' Thus, given that we filtered out all US-only NGOs in our response rate
#' calculations, and given that we cannot precisely know the true number of
#' survey dropouts, we upwardly bias our completion rate estimate to 90ish%.


# --------------------
#' # Sector overview
# --------------------
#' ## Location
#' Where are these NGOs based?
hq.countries <- responses.full %>%
  group_by(survey.id) %>% slice(1) %>% ungroup() %>%
  rename(id = home.iso3) %>%
  group_by(id) %>%
  summarize(num.ngos = n()) %>%
  ungroup() %>%
  right_join(possible.countries, by="id") %>%
  mutate(num.ceiling = ifelse(num.ngos >= 10, 10, num.ngos),
         prop = num.ngos / sum(num.ngos, na.rm=TRUE)) %>%
  arrange(desc(num.ngos)) %T>%
  {print(head(.))}

hq.regions <- responses.full %>%
  group_by(survey.id) %>% slice(1) %>% ungroup() %>%
  filter(!is.na(home.region)) %>%
  group_by(home.region) %>%
  summarise(num = n()) %>% ungroup() %>%
  mutate(prop = num / sum(num)) %T>%
  {print(head(.))}

hq.map <- ggplot(hq.countries, aes(fill=num.ceiling, map_id=id)) +
  geom_map(map=countries.ggmap, size=0.15, colour="black") + 
  expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) + 
  coord_equal() +
  scale_fill_gradient(low="grey95", high="grey20", breaks=seq(2, 10, 2), 
                      labels=c(paste(seq(2, 8, 2), "  "), "10+"),
                      na.value="#FFFFFF", name="NGOs based in country",
                      guide=guide_colourbar(ticks=FALSE, barwidth=6)) + 
  theme_blank_map() +
  theme(legend.position="bottom", legend.key.size=unit(0.65, "lines"),
        strip.background=element_rect(colour="#FFFFFF", fill="#FFFFFF"))

#' Where do these NGOs work?
work.countries <- responses.full %>%
  rename(id = work.iso3) %>%
  group_by(id) %>%
  summarize(num.ngos = n()) %>%
  ungroup() %>%
  right_join(possible.countries, by="id") %>%
  mutate(num.ceiling = ifelse(num.ngos >= 10, 10, num.ngos),
         prop = num.ngos / sum(num.ngos, na.rm=TRUE)) %>%
  arrange(desc(num.ngos)) %T>%
  {print(head(.))}

work.regions <- responses.full %>%
  filter(!is.na(work.region)) %>%
  group_by(work.region) %>%
  summarise(num = n()) %>% ungroup() %>%
  mutate(prop = num / sum(num)) %T>%
  {print(head(.))}

work.map <- ggplot(work.countries, aes(fill=num.ceiling, map_id=id)) +
  geom_map(map=countries.ggmap, size=0.15, colour="black") + 
  expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) + 
  coord_equal() +
  scale_fill_gradient(low="grey95", high="grey20", breaks=seq(2, 10, 2), 
                      labels=c(paste(seq(2, 8, 2), "  "), "10+"),
                      na.value="#FFFFFF", name="NGOs working in country",
                      guide=guide_colourbar(ticks=FALSE, barwidth=6)) + 
  theme_blank_map() +
  theme(legend.position="bottom", legend.key.size=unit(0.65, "lines"),
        strip.background=element_rect(colour="#FFFFFF", fill="#FFFFFF"))

ggsave(work.map, filename=file.path(PROJHOME, "figures", "fig_work_map.pdf"),
       width=5, height=2.5, units="in", device=cairo_pdf)
ggsave(work.map, filename=file.path(PROJHOME, "figures", "fig_work_map.png"),
       width=5, height=2.5, units="in", type="cairo", dpi=300)

#' Combined maps
fig.maps <- arrangeGrob(hq.map, work.map, nrow=1)
grid.draw(fig.maps)
ggsave(fig.maps, filename=file.path(PROJHOME, "figures", "fig_maps.pdf"),
       width=6, height=3, units="in", device=cairo_pdf, scale=1.5)
ggsave(fig.maps, filename=file.path(PROJHOME, "figures", "fig_maps.png"),
       width=6, height=3, units="in", type="cairo", scale=1.5, dpi=300)

#' Save map count data for use in other stuff (like Judith's book)
write_csv(work.countries, path="~/Desktop/data_figureA_work_map.csv")
write_csv(hq.countries, path="~/Desktop/data_figureA_hq_map.csv")

#' Side-by-side graph of home vs. work regions
plot.hq <- hq.regions %>%
  arrange(num) %>%
  mutate(region = factor(home.region, levels=home.region, ordered=TRUE),
         prop.nice = sprintf("%.1f%%", prop * 100))

plot.work <- work.regions %>%
  mutate(region = factor(work.region, levels=levels(plot.hq$region), ordered=TRUE),
         prop.nice = sprintf("%.1f%%", prop * 100))

fig.hq <- ggplot(plot.hq, aes(x=region, y=num)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label = prop.nice), size=2.5, hjust=1.3, 
            family="Source Sans Pro Light") + 
  labs(x=NULL, y="NGOs based in region") + 
  scale_y_continuous(trans="reverse", expand = c(.1, .1),
                     breaks = seq(0, 250, by=50)) + 
  coord_flip(ylim=c(0, 250)) + 
  theme_clean() + 
  theme(axis.text.y = element_blank(), 
        axis.line.y = element_blank(),
        plot.margin = unit(c(1,-0.25,1,1), "lines"))

fig.work <- ggplot(plot.work, aes(x=region, y=num)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label = prop.nice), size=2.5, hjust=-0.3, 
            family="Source Sans Pro Light") + 
  labs(x=NULL, y="NGOs working in region") + 
  scale_y_continuous(expand = c(.1, .1),
                     breaks = seq(0, 250, by=50)) + 
  coord_flip(ylim=c(0, 250)) + 
  theme_clean() + 
  theme(axis.text.y = element_text(hjust=0.5), 
        axis.line.y = element_blank(),
        plot.margin = unit(c(1,1,1,0), "lines"))

fig.locations <- arrangeGrob(fig.hq, fig.work, nrow=1,
                             widths=c(0.45, 0.55))
grid.draw(fig.locations)
ggsave(fig.locations, filename=file.path(PROJHOME, "figures", "fig_locations.pdf"),
       width=5, height=1.5, units="in", device=cairo_pdf)
ggsave(fig.locations, filename=file.path(PROJHOME, "figures", "fig_locations.png"),
       width=5, height=1.5, units="in", type="cairo", dpi=300)

#' Where do different regional NGOs work?
responses.full %>%
  filter(!is.na(work.region)) %>%
  group_by(home.region, work.region) %>%
  summarise(num = n()) %>%
  group_by(home.region) %>%
  mutate(prop.home.region = num / sum(num)) %>%
  print

#' ## Amount and type of TIP work
orgs.only <- responses.full %>%
  group_by(survey.id) %>% slice(1) %>% ungroup()

#' How much time and resources do these NGOs spend on trafficking?
fig.time <- ggplot(data=orgs.only,
                   aes(x=as.numeric(Q2.1)/100, y=(..count.. / sum(..count..)))) + 
  geom_histogram(binwidth=0.1) + 
  labs(x="Proportion of time spent on trafficking", y="Proportion of responses") + 
  scale_x_continuous(labels=percent, limits=c(0, 1), breaks=seq(0, 1, 0.2)) +
  scale_y_continuous(labels=percent, breaks=seq(0, 0.12, 0.02)) + 
  coord_cartesian(ylim=c(0, 0.125)) + 
  theme_clean() + theme(panel.grid.major.y = element_line(size=0.25, colour="grey90"))
fig.time
ggsave(fig.time, filename=file.path(PROJHOME, "figures", "fig_time.pdf"),
       width=5, height=2, units="in", device=cairo_pdf)
ggsave(fig.time, filename=file.path(PROJHOME, "figures", "fig_time.png"),
       width=5, height=2, units="in", type="cairo", dpi=300)

#' Summary stats of time spent
orgs.only %>%
  summarise(avg.time = mean(Q2.1, na.rm=TRUE),
            med.time = median(Q2.1, na.rm=TRUE),
            min.time = min(Q2.1, na.rm=TRUE),
            max.time = max(Q2.1, na.rm=TRUE))

#' How much do organizations know about trafficking in the countries they work in?
responses.full %>%
  group_by(Q3.3) %>%
  summarise(num = n()) %>%
  filter(!is.na(Q3.3)) %>%
  mutate(prop = num / sum(num)) %T>%
  {print(sum(.$num))}

#' What trafficking issues is the organization involved with?
cols <- c("Q2.2_1", "Q2.2_2", "Q2.2_3", "Q2.2_4")
labels <- c("Organ trafficking", "Sex trafficking",
            "Labor trafficking", "Other")

issues <- separate.answers.summary(orgs.only, cols, labels)
issues$denominator  # Number of responses
issues$df

plot.data <- issues$df %>% 
  mutate(Answer=factor(Answer, levels=rev(labels), ordered=TRUE))

fig.issues <- ggplot(plot.data, aes(x=Answer, y=Responses)) +
  geom_bar(aes(y=plot.pct), stat="identity") + 
  labs(x=NULL, y=NULL) + 
  scale_y_continuous(labels = percent, 
                     breaks = seq(0, max(round(plot.data$plot.pct, 1)), by=0.1)) + 
  coord_flip() + theme_clean()
fig.issues

#' How many NGOs deal with both sex and labor trafficking?
orgs.only %>% 
  mutate(sex = ifelse(is.na(Q2.2_2), 0, 1),
         labor = ifelse(is.na(Q2.2_3), 0, 1),
         both = sex == 1 & labor == 1) %>% 
  summarise(sex = sum(sex), labor = sum(labor), 
            num.both = sum(both), prop.both = num.both / issues$denominator)

#' Other responses about issues NGOs deal with
# orgs.only %>% filter(!is.na(Q2.2_4_TEXT)) %>% 
#   select(clean.id, Q2.2_4_TEXT) %>% View

#' Which kinds of victims do NGOs help?
cols <- c("Q2.3_1", "Q2.3_2", "Q2.3_3")
labels <- c("Children", "Adults", "Other")

victims <- separate.answers.summary(orgs.only, cols, labels)
victims$denominator  # Number of responses
victims$df

plot.data <- victims$df %>% 
  mutate(Answer=factor(Answer, levels=rev(labels), ordered=TRUE))

fig.victims <- ggplot(plot.data, aes(x=Answer, y=Responses)) +
  geom_bar(aes(y=plot.pct), stat="identity") + 
  labs(x=NULL, y=NULL) + 
  scale_y_continuous(labels = percent, 
                     breaks = seq(0, max(round(plot.data$plot.pct, 1)), by=0.1)) + 
  coord_flip() + theme_clean()
fig.victims

#' Other responses about victims NGOs deal with
# orgs.only %>% filter(!is.na(Q2.3_3_TEXT)) %>% 
#   select(clean.id, Q2.3_3_TEXT) %>% View

#' How are the types of victims distributed between the main trafficking issues?
victims.issues <- orgs.only %>% 
  select(organs=Q2.2_1, sex=Q2.2_2, labor=Q2.2_3, other.issue=Q2.2_4, 
         children=Q2.3_1, adults=Q2.3_2, other.victims=Q2.3_3) %>%
  mutate_each(funs(!is.na(.)))

victims.issues %>%
  filter(children) %>%
  summarise(num = n(),
            sex = sum(sex), sex.prop = sex / n(),
            labor = sum(labor), labor.prop = labor / n())

victims.issues %>%
  filter(adults) %>%
  summarise(num = n(),
            sex = sum(sex), sex.prop = sex / n(),
            labor = sum(labor), labor.prop = labor / n())

#' Which efforts do NGOs focus on?
cols <- c("Q2.4_1", "Q2.4_2", "Q2.4_3", "Q2.4_4", "Q2.4_5")
labels <- c("Prevention and education", "Prosecutions and legal issues",
            "Victim protection", "Victim assistance", "Other")

efforts <- separate.answers.summary(orgs.only, cols, labels)
efforts$denominator  # Number of responses
efforts$df

plot.data <- efforts$df %>% 
  arrange(plot.pct) %>%
  mutate(Answer=factor(Answer, levels=Answer, ordered=TRUE))

fig.efforts <- ggplot(plot.data, aes(x=Answer, y=Responses)) +
  geom_bar(aes(y=plot.pct), stat="identity") + 
  labs(x=NULL, y=NULL) + 
  scale_y_continuous(labels = percent, 
                     breaks = seq(0, max(round(plot.data$plot.pct, 1)), by=0.1)) + 
  coord_flip() + theme_clean()
fig.efforts

#' Other responses about efforts NGOs engage in
# orgs.only %>% filter(!is.na(Q2.4_5_TEXT)) %>%
#   select(clean.id, Q2.4_5_TEXT) %>% View

#' How are the types of efforts distributed between the main trafficking issues?
efforts.issues <- orgs.only %>% 
  select(prevention=Q2.4_1, legal=Q2.4_2, protection=Q2.4_3, assistance=Q2.4_4, 
         sex=Q2.2_2, labor=Q2.2_3) %>%
  mutate_each(funs(!is.na(.)))

efforts.issues %>%
  filter(prevention) %>%
  summarise(num = n(),
            sex = sum(sex), sex.prop = sex / n(),
            labor = sum(labor), labor.prop = labor / n())

efforts.issues %>%
  filter(legal) %>%
  summarise(num = n(),
            sex = sum(sex), sex.prop = sex / n(),
            labor = sum(labor), labor.prop = labor / n())

efforts.issues %>%
  filter(protection) %>%
  summarise(num = n(),
            sex = sum(sex), sex.prop = sex / n(),
            labor = sum(labor), labor.prop = labor / n())

efforts.issues %>%
  filter(assistance) %>%
  summarise(num = n(),
            sex = sum(sex), sex.prop = sex / n(),
            labor = sum(labor), labor.prop = labor / n())

#' ## Work with other actors
#' Which institutions have been active in anti-TIP work?
cols <- c("Q3.5_1", "Q3.5_2", "Q3.5_3", "Q3.5_4", "Q3.5_5")
labels <- c("The national government", "NGOs and civil society",
            "Foreign governments", "International organizations", "Other")

other.actors <- separate.answers.summary(responses.full, cols, labels)
other.actors$denominator  # Number of responses
other.actors$df

plot.data <- other.actors$df %>% 
  arrange(plot.pct) %>%
  mutate(Answer=factor(Answer, levels=Answer, ordered=TRUE))

fig.other.actors <- ggplot(plot.data, aes(x=Answer, y=Responses)) +
  geom_bar(aes(y=plot.pct), stat="identity") + 
  labs(x=NULL, y=NULL) + 
  scale_y_continuous(labels = percent, 
                     breaks = seq(0, max(round(plot.data$plot.pct, 1)), by=0.1)) + 
  coord_flip() + theme_clean()
fig.other.actors

#' Other responses
others <- responses.full %>% select(survey.id, starts_with("Q3.5_5_")) %>%
  filter(!is.na(Q3.5_5_TEXT)) %>%
  # Convert values to numeric
  mutate_each(funs(as.numeric(levels(.))[.]), -c(survey.id, Q3.5_5_TEXT))

cols <- c(c("Q3.5_5_LawEnforcement", "Q3.5_5_Education", 
            "Q3.5_5_DomesticGovernment", "Q3.5_5_ReligiousGroups", 
            "Q3.5_5_Embassies", "Q3.5_5_InternationalGroups", 
            "Q3.5_5_NGOs", "Q3.5_5_Other", "Q3.5_5_Media", 
            "Q3.5_5_ExperienceofSurviviors", 
            "Q3.5_5_PrivateSector", "Q3.5_5_Unions"))

labels <- c("Law Enforcement", "Education", 
            "Domestic Government", "Religious Groups", 
            "Embassies", "International Groups", 
            "NGOs", "Other", "Media", 
            "Experience of Surviviors", 
            "Private Sector", "Unions")

(others.summary <- separate.answers.summary(others, cols, labels))

#' How hard is the government working?
responses.full %>%
  group_by(Q3.20) %>%
  summarise(num = n()) %>%
  filter(!is.na(Q3.20)) %>%
  mutate(prop = num / sum(num))

#' Have NGOs used the TIP report to talk with any of these groups?
cols <- c("Q3.21_1", "Q3.21_2", "Q3.21_3", "Q3.21_4")
labels <- c("National government", "Another government",
            "Other NGOs", "Other")

tip.discuss <- separate.answers.summary(responses.full, cols, labels)
tip.discuss$denominator  # Number of responses
tip.discuss$df

plot.data <- tip.discuss$df %>% 
  arrange(plot.pct) %>%
  mutate(Answer=factor(Answer, levels=Answer, ordered=TRUE))

fig.tip.discuss <- ggplot(plot.data, aes(x=Answer, y=Responses)) +
  geom_bar(aes(y=plot.pct), stat="identity") + 
  labs(x=NULL, y=NULL) + 
  scale_y_continuous(labels = percent, 
                     breaks = seq(0, max(round(plot.data$plot.pct, 1)), by=0.1)) + 
  coord_flip() + theme_clean()
fig.tip.discuss


#' ## Government restrictions and oversight
#' Do members of the government or ruling party sit on the NGO's board?
responses.full %>%
  group_by(Q3.27) %>%
  summarise(num = n()) %>%
  filter(!is.na(Q3.27)) %>%
  mutate(prop = num / sum(num))

#' For those that said yes, is the NGO required to have a government official
#' sit on the board?
responses.full %>%
  group_by(Q3.28) %>%
  summarise(num = n()) %>%
  filter(!is.na(Q3.28)) %>%
  mutate(prop = num / sum(num))

#' How restricted does the NGO feel by the host government?
responses.full %>%
  group_by(Q3.29) %>%
  summarise(num = n()) %>%
  filter(!is.na(Q3.29)) %>%
  mutate(prop = num / sum(num)) %T>%
  {cat("Number of responses:", sum(.$num), "\n")}

#' Which countries do NGOs say are restrictive?
responses.full %>%
  filter(Q3.29 %in% c("Somewhat restricted", "Very restricted")) %>%
  group_by(work.country) %>%
  summarise(num = n()) %>%
  arrange(desc(num)) %T>%
  {cat("Number of countries:", nrow(.), "\n")}

#' Explanations of restrictions
# responses.full %>% filter(!is.na(Q3.30)) %>%
#   select(clean.id, Q3.30) %>% View

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
  geom_text(aes(label = prop.nice), size=2.5, hjust=1.3, 
            family="Source Sans Pro Light") + 
  labs(x=NULL, y="Number of times country was mentioned\nas a partner in anti-TIP work") + 
  scale_y_continuous(breaks=seq(0, 275, by=50), 
                     trans="reverse", expand = c(.1, .1)) + 
  coord_flip(ylim=c(0, 280)) + 
  theme_clean() + 
  theme(axis.text.y = element_blank(), 
        axis.line.y = element_blank(),
        plot.margin = unit(c(1,-0.25,1,1), "lines"))

fig.most.active <- ggplot(plot.data.active, aes(x=country, y=total)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label = prop.nice), size=2.5, hjust=-0.3, 
            family="Source Sans Pro Light") + 
  labs(x=NULL, y="Number of times country was mentioned\nas the most active partner in anti-TIP work") + 
  scale_y_continuous(breaks=seq(0, 200, by=50),
                     expand = c(.15, .15)) + 
  coord_flip(ylim=c(0, 225)) + 
  theme_clean() + 
  theme(axis.text.y = element_text(hjust=0.5), 
        axis.line.y = element_blank(),
        plot.margin = unit(c(1,1,1,0), "lines"))
  
fig.embassies <- arrangeGrob(fig.active, fig.most.active, nrow=1)
grid.draw(fig.embassies)
ggsave(fig.embassies, filename=file.path(PROJHOME, "figures", "fig_embassies.pdf"),
       width=5, height=3, units="in", device=cairo_pdf)
ggsave(fig.embassies, filename=file.path(PROJHOME, "figures", "fig_embassies.png"),
       width=5, height=3, units="in", type="cairo", dpi=300)

saveRDS(active.embassies, file.path(PROJHOME, "data", "active_embassies.rds"))
saveRDS(most.active.clean, file.path(PROJHOME, "data", "most_active_embassies.rds"))
write_csv(plot.data, 
          file.path(PROJHOME, "data", 
                    "data_figure2_x_active_embassies_plot.csv"))
write_csv(plot.data.active, 
          file.path(PROJHOME, "data", 
                    "data_figure2_x_most_active_embassies_plot.csv"))


#' Actual US activities
cols <- c("Q3.9_1", "Q3.9_2", "Q3.9_3", "Q3.9_4", "Q3.9_5",
          "Q3.9_6", "Q3.9_7", "Q3.9_8", "Q3.9_9", "Q3.9_10")
labels <- c("Asking for legislation", "Convening conferences\nor workshops",
            "Raising awareness", "Providing resources\nor funding",
            "Increasing government\nattention", "Training government\nofficials",
            "Contributing to a\ngovernment action plan", "Other", "Don't know",
            "The US has not been\ninvolved in trafficking issues")

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
       width=5.25, height=3, units="in", device=cairo_pdf)
ggsave(fig.activities, filename=file.path(PROJHOME, "figures", "fig_activities.png"),
       width=5.25, height=3, units="in", type="cairo", dpi=300)


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

#' Average importance by country
importance.plot <- country.indexes %>%
  filter(num.responses >= 10) %>%
  arrange(importance_score) %>%
  mutate(country_label = factor(work.country, levels=unique(work.country), 
                                labels=paste0(work.country, " (", num.responses, ")"),
                                ordered=TRUE)) 

fig.avg_importance <- ggplot(importance.plot, aes(y=country_label, x=importance_score)) + 
  geom_pointrangeh(aes(xmax=importance_score + (qnorm(0.975) * importance_std.err),
                       xmin=importance_score + (qnorm(0.025) * importance_std.err)),
                   size=0.25) + 
  labs(y="Country (number of responses)", x=NULL,
       title="Importance of the US in anti-TIP efforts") +
  scale_x_continuous(breaks=c(0, 1, 2), 
                     labels=c("Not important", "Somewhat important", "Most important")) +
  coord_cartesian(xlim=c(0, 2.1)) +
  theme_clean() + theme(legend.position="bottom", plot.title=element_text(hjust=0.5))
fig.avg_importance
ggsave(fig.avg_importance, filename=file.path(PROJHOME, "figures", "fig_avg_importance.pdf"),
       width=6.5, height=3, units="in", device=cairo_pdf)
ggsave(fig.avg_importance, filename=file.path(PROJHOME, "figures", "fig_avg_importance.png"),
       width=6.5, height=3, units="in", type="cairo", dpi=300)


#' ## Are opinions of the US's importance associated with…?
df.importance <- responses.full %>% 
  select(Q3.19, work.country, change_policy, avg_tier, improve_tip, change_policy, 
         importance, received.funding, us.involvement, total.funding, 
         total.freedom, us.hq, time.spent=Q2.1) %>% 
  filter(!is.na(Q3.19)) %>%
  filter(Q3.19 != "Don't know") %>%
  mutate(importance_factor = factor(Q3.19, ordered=FALSE),
         log.total.funding = log1p(total.funding),
         time.spent = as.numeric(time.spent))

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

#' ### Improvement in TIP scores
#' Opinions of importance are not related to changes in TIP score. The average
#' Improvement in TIP rating is the same for each possible answer of importance.
ggplot(df.importance, aes(x=Q3.19, y=improve_tip)) + 
  geom_violin(fill="grey90") + 
  geom_point(stat="summary", fun.y="mean", size=5) + 
  labs(x="Opinion of US", y="Improvement in TIP tier rating") + 
  coord_flip() + theme_clean()

#' Variance is equal in all groups:
kruskal.test(improve_tip ~ importance_factor, data=df.importance)

#' ANOVA shows no differences:
change.anova <- aov(improve_tip ~ importance_factor, data=df.importance) 
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

#' ANOVA shows no differences
cho.change.anova <- aov(change_policy ~ importance_factor, data=df.importance) 
summary(cho.change.anova)  # (⌐○Ϟ○)  ♥  \(•◡•)/
TukeyHSD(cho.change.anova)

#' ### US funding received by the responding organization
#' Organizations that have been received funding from the US are more likely to
#' consider the US to play an important role in the countries they work in.
funding.table <- df.importance %>%
  xtabs(~ importance_factor + received.funding, .) %>% print

#' Organizations that received funding:
df.importance %>%
  xtabs(~ received.funding, .) %T>% print %>% prop.table

#' When removing all funded NGOs from the sample, what percent think the US is important?
df.importance %>%
  filter(received.funding == FALSE) %>%
  xtabs(~ importance_factor, .) %T>% print %>% prop.table

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
                                         importance_factor="Opinion of US"),
                          gp_labels=(gpar(fontsize=8))), 
       gp_varnames=gpar(fontsize=10, fontface=2))

#' ### US funding received by the country the organization works in
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

#' Ratio is 3ish, which is below 4, so heterogenous variance is okayish:
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

#' ### Democracy (Freedom House political rights + civil liberties)
#' US importance appears to be associated with the level of democracy in a
#' country. NGOs working in countries with worse democracy (higher numbers of
#' the total freedom scale) are more likely to see the US as the most important
#' anti-TIP actor in that country. Or, rather, on average total freedom is 
#' worse in countries where NGOs indicate the US as the most important actor.
fig.importance.freedom <- ggplot(df.importance, aes(x=Q3.19, y=total.freedom)) + 
  # geom_violin(fill="grey90") + 
  geom_point(alpha=0.05, size=0.25) + 
  geom_point(stat="summary", fun.y="mean", size=2) + 
  scale_y_continuous(breaks=seq(2, 14, by=4)) + 
  labs(x=NULL,
       y="Total freedom (political rights + civil liberties; higher is worse)") + 
  coord_flip() + theme_clean(6)
fig.importance.freedom

#' Variance is not equal in all groups:
kruskal.test(total.freedom ~ importance_factor, data=df.importance)

#' Ratio between min and max variance is low, so we're okay:
df.importance %>% group_by(importance_factor) %>%
  summarise(variance = var(total.freedom, na.rm=TRUE)) %>%
  do(data_frame(ratio = max(.$variance) / min(.$variance)))

#' ANOVA shows significant differences:
democracy.anova <- aov(total.freedom ~ importance_factor, data=df.importance) 
summary(democracy.anova)
(democracy.pairs <- TukeyHSD(democracy.anova))

#' View the differences:
democracy.pairs.plot <- data.frame(democracy.pairs$importance_factor) %>%
  mutate(pair = row.names(.),
         pair = factor(pair, levels=pair, ordered=TRUE),
         stars = add.stars(p.adj))

fig.democracy.pairs <- ggplot(democracy.pairs.plot, 
                            aes(x=pair, y=diff, ymax=upr, ymin=lwr)) + 
  geom_hline(yintercept=0) + 
  geom_text(aes(label=stars), nudge_x=0.25) +
  geom_pointrange() + 
  theme_clean() + coord_flip()
fig.democracy.pairs

#' ### Time spent on trafficking
#' The time NGOs spend on trafficking issues does not appear to be associated
#' with their opinion of US importance.
ggplot(df.importance, aes(x=Q3.19, y=time.spent)) + 
  geom_violin(fill="grey90") + 
  geom_point(alpha=0.05) + 
  geom_point(stat="summary", fun.y="mean", size=5) + 
  labs(x="Opinion of US", y="Time spent on trafficking issues") + 
  coord_flip() + theme_clean()

#' Variance is not equal in all groups:
kruskal.test(time.spent ~ importance_factor, data=df.importance)

#' Ratio between min and max variance is low, so we're okay:
df.importance %>% group_by(importance_factor) %>%
  summarise(variance = var(time.spent, na.rm=TRUE)) %>%
  do(data_frame(ratio = max(.$variance) / min(.$variance)))

#' ANOVA shows some small overall signifcant differences, but when decomposed,
#' that effect is coming only from the tiny "Don't know-Somewhat important actor"
#' difference.
time.anova <- aov(time.spent ~ importance_factor, data=df.importance) 
summary(time.anova)
(time.pairs <- TukeyHSD(time.anova))

#' ### Interaction with the US
#' Organizations that have been involved with the US are more likely to
#' consider the US to play an important role in the countries they work in.
involvement.table <- df.importance %>%
  xtabs(~ importance_factor + us.involvement, .) %>% print

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
                                         importance_factor="Opinion of US"),
                          gp_labels=(gpar(fontsize=8))), 
       gp_varnames=gpar(fontsize=10, fontface=2))

#' ### Headquarters in the US
#' NGOs with headquarters in the US are not significantly different from their
#' foreign counterparts in their opinions of the importance of the US.
hq.table <- df.importance %>%
  xtabs(~ importance_factor + us.hq, .) %>% print

#' There's no overall significant difference
(hq.chi <- chisq.test(hq.table))

# Cramer's V is really low
assocstats(hq.table)

# Components of chi-squared
(components <- hq.chi$residuals^2)
1-pchisq(components, hq.chi$parameter)

# Visualize differences
mosaic(hq.table,
       labeling_args=list(set_varnames=c(us.involvement="US involvement", 
                                         importance_factor="Opinion of US"),
                          gp_labels=(gpar(fontsize=8))), 
       gp_varnames=gpar(fontsize=10, fontface=2))


#' # NGO opinions of US positivity
#' ## General positivity
#' **Important caveat**: Respondents were only asked about their opinions of
#' the US's work (Q3.25) if they indicated that the US was a somewhat important
#' actor or the most important actor (Q3.19)
df.positivity <- responses.full %>% 
  select(Q3.25=Q3.25_collapsed, work.country, change_policy, avg_tier, 
         improve_tip, change_policy, importance, received.funding, us.involvement, 
         total.funding, total.freedom, us.hq, time.spent=Q2.1) %>% 
  filter(!is.na(Q3.25)) %>%
  filter(Q3.25 != "Don't know") %>%
  mutate(positivity.factor = factor(Q3.25, ordered=FALSE),
         log.total.funding = log1p(total.funding),
         time.spent = as.numeric(time.spent))

plot.data <- df.positivity %>%
  group_by(Q3.25) %>%
  summarize(num = n()) %>%
  na.omit() %>%
  mutate(prop = num / sum(num),
         prop.nice = sprintf("%.1f%%", prop * 100),
         Q3.25 = factor(Q3.25, levels=rev(levels(df.positivity$positivity.factor)), 
                        ordered=TRUE))
plot.data

fig.us_positivity <- ggplot(plot.data, aes(x=Q3.25, y=prop)) + 
  geom_bar(stat="identity") + 
  labs(x=NULL, y=NULL) + 
  scale_y_continuous(labels = percent, 
                     breaks = seq(0, max(round(plot.data$num, 1)), by=0.1)) + 
  coord_flip() + theme_clean()
fig.us_positivity

#' Average positivity by country
positivity.plot <- country.indexes %>%
  filter(num.responses >= 10,
         positivity_score > 0) %>%
  arrange(positivity_score) %>%
  mutate(country_label = factor(work.country, levels=unique(work.country), 
                                labels=paste0(work.country, " (", num.responses, ")"),
                                ordered=TRUE)) 

fig.avg_positivity <- ggplot(positivity.plot, aes(y=country_label, x=positivity_score)) + 
  geom_pointrangeh(aes(xmax=positivity_score + (qnorm(0.975) * positivity_std.err),
                       xmin=positivity_score + (qnorm(0.025) * positivity_std.err)),
                   size=0.25) + 
  labs(y="Country (number of responses)", x=NULL,
       title="Positivity of the US in anti-TIP efforts") + 
  scale_x_continuous(breaks=c(-1, 0, 1), labels=c("Negative", "Mixed", "Positive")) + 
  coord_cartesian(xlim=c(-1, 1.05)) +
  theme_clean() + theme(legend.position="bottom", plot.title=element_text(hjust=0.5))
fig.avg_positivity

ggsave(fig.avg_positivity, filename=file.path(PROJHOME, "figures", "fig_avg_positivity.pdf"),
       width=5, height=2.75, units="in", device=cairo_pdf)
ggsave(fig.avg_positivity, filename=file.path(PROJHOME, "figures", "fig_avg_positivity.png"),
       width=5, height=2.75, units="in", type="cairo", dpi=300)


#' Both importance and positivity
blank <- rectGrob(gp=gpar(col="white"))
fig.avg_countries <- arrangeGrob(fig.avg_importance, blank, fig.avg_positivity,
                                 ncol=1, heights=c(0.48, 0.04, 0.48))
grid::grid.draw(fig.avg_countries)

ggsave(fig.avg_countries, filename=file.path(PROJHOME, "figures", "fig_avg_countries.pdf"),
       width=5, height=3.75, units="in", device=cairo_pdf)
ggsave(fig.avg_countries, filename=file.path(PROJHOME, "figures", "fig_avg_countries.png"),
       width=5, height=3.75, units="in", type="cairo", dpi=300)

#' ## Are opinions of the US's positivity associated with…?
#' ### Average tier rating 
ggplot(df.positivity, aes(x=positivity.factor, y=avg_tier)) +
  geom_violin(fill="grey90") + 
  geom_point(alpha=0.05, show.legend=FALSE) +
  geom_point(stat="summary", fun.y="mean", size=5) + 
  labs(x="Opinion of US", y="Average TIP tier rating") + 
  coord_flip() + theme_clean()

t.test(avg_tier ~ positivity.factor, data=df.positivity)

#' ### Improvement in TIP scores
#' NGOs who have positive opinions of the US are more likely to work in
#' countries where the TIP rating has (slightly) decreased on average between
#' 2000 and 2015. This may be because assigning a worse TIP rating to a country
#' represents increased US diplomatic and economic pressure—it is a possible
#' sign that NGOs like scorecard diplomacy.
ggplot(df.positivity, aes(x=positivity.factor, y=improve_tip)) + 
  geom_violin(fill="grey90") + 
  geom_point(stat="summary", fun.y="mean", size=5) + 
  labs(x="Opinion of US", y="Improvement in TIP tier rating") + 
  coord_flip() + theme_clean()

#' Difference is significant
t.test(improve_tip ~ positivity.factor, data=df.positivity)

#' ### Change in Cho scores
#' In contrast to the changes in actual TIP scores, NGOs that work in countries
#' that show greater improvement in overall TIP policies are more likely to
#' have a positive opinion of the US, perhaps because they are happy about the
#' actual on-the-ground improvements.
ggplot(df.positivity, aes(x=positivity.factor, y=change_policy)) + 
  geom_violin(fill="grey90") + 
  geom_point(stat="summary", fun.y="mean", size=5) + 
  labs(x="Opinion of US", y="Change in TIP policy index") + 
  coord_flip() + theme_clean()

#' Difference is significant
t.test(change_policy ~ positivity.factor, data=df.positivity)
df.positivity %>% group_by(positivity.factor) %>%
  summarise(variance = var(change_policy, na.rm=TRUE)) %>%
  do(data_frame(ratio = max(.$variance) / min(.$variance)))

#' ### US funding received by the responding organization
funding.table.pos <- df.positivity %>%
  xtabs(~ positivity.factor + received.funding, .) %>% print

#' Distribution isn't significantly different
(funding.chi.pos <- chisq.test(funding.table.pos))

# Cramer's V for standardized measure of association
assocstats(funding.table.pos)

# Components of chi-squared
(components <- funding.chi.pos$residuals^2)
round(1-pchisq(components, funding.chi.pos$parameter), 3)

# Visualize differences
mosaic(funding.table.pos,
       labeling_args=list(set_varnames=c(received.funding="Received US funding", 
                                         positivity.factor="Opinion of US"),
                          gp_labels=(gpar(fontsize=8))), 
       gp_varnames=gpar(fontsize=10, fontface=2))

#' ### US funding received by the country the organization works in
#' 
ggplot(df.positivity, aes(x=positivity.factor, y=log.total.funding)) + 
  geom_violin(fill="grey90") + 
  geom_point(alpha=0.05) + 
  geom_point(stat="summary", fun.y="mean", size=5) + 
  labs(x="Opinion of US", y="Total TIP funding to country (logged)") + 
  scale_y_continuous(labels=trans_format("exp", dollar_format())) +
  coord_flip() + theme_clean()

#' Difference is significant
t.test(log.total.funding ~ positivity.factor, data=df.positivity)

#' ### Democracy (Freedom House political rights + civil liberties)
#' 
ggplot(df.positivity, aes(x=positivity.factor, y=total.freedom)) + 
  geom_violin(fill="grey90") + 
  geom_point(alpha=0.05) + 
  geom_point(stat="summary", fun.y="mean", size=5) + 
  labs(x="Opinion of US", 
       y="Total freedom (political rights + civil liberties; higher is worse)") + 
  coord_flip() + theme_clean()

#' Difference is not quite significant
t.test(total.freedom ~ positivity.factor, data=df.positivity)

#' ### Time spent on trafficking
#' The time NGOs spend on trafficking issues does not appear to be associated
#' with their opinion of US importance.
ggplot(df.positivity, aes(x=positivity.factor, y=time.spent)) + 
  geom_violin(fill="grey90") + 
  geom_point(alpha=0.05) + 
  geom_point(stat="summary", fun.y="mean", size=5) + 
  labs(x="Opinion of US", y="Time spent on trafficking issues") + 
  coord_flip() + theme_clean()

#' Difference is no significant:
t.test(time.spent ~ positivity.factor, data=df.positivity)

#' ### Interaction with the US
#' 
involvement.table.pos <- df.positivity %>%
  xtabs(~ positivity.factor + us.involvement, .) %>% print

#' There's no significant difference
(involvement.chi.pos <- chisq.test(involvement.table.pos))

# Tiny Cramer's V
assocstats(involvement.table.pos)

# Components of chi-squared
(components <- involvement.chi.pos$residuals^2)
1-pchisq(components, involvement.chi.pos$parameter)

# Visualize differences
mosaic(involvement.table.pos,
       labeling_args=list(set_varnames=c(us.involvement="US involvement", 
                                         positivity.factor="Opinion of US"),
                          gp_labels=(gpar(fontsize=8))), 
       gp_varnames=gpar(fontsize=10, fontface=2))

#' ### Headquarters in the US
#' NGOs with headquarters in the US are not significantly different from their
#' foreign counterparts in their opinions of the US in general.
hq.table.pos <- df.positivity %>%
  xtabs(~ positivity.factor + us.hq, .) %>% print

#' There's no overall significant difference, but some of the cells are too small
(hq.chi.pos <- chisq.test(hq.table.pos))

# Cramer's V is really, really low
assocstats(hq.table.pos)

# Components of chi-squared
(components <- hq.chi.pos$residuals^2)
1-pchisq(components, hq.chi.pos$parameter)

# Visualize differences
mosaic(hq.table.pos,
       labeling_args=list(set_varnames=c(us.involvement="US involvement", 
                                         positivity.factor="Opinion of US"),
                          gp_labels=(gpar(fontsize=8))), 
       gp_varnames=gpar(fontsize=10, fontface=2))


#' # Miscellaneous questions
#' 
#' ## Anonymity 
#' 
#' How many organizations requested anonymity?
responses.orgs %>% group_by(Q1.3) %>% 
  summarise(Freq = n()) %>% ungroup() %>%
  mutate(Percent = Freq / sum(Freq))

#' ## Correlation between US funding and US cooperation
#' 
#' What is the correlation between answering yes to funding and yes to direct
#' cooperation in Q3.18? Are the same organizations answering the question and
#' are the questions capturing the same thing?

# Convert all Q3.18 responses to 0/1 if they answered at least one of the options
contact.with.us <- responses.full %>% select(contains("Q3.18")) %>%
  rowwise() %>% do(answered.something = !all(is.na(.))) %>%
  ungroup() %>% mutate(answered.something = unlist(answered.something)) %>%
  bind_cols(select(responses.full, clean.id, contains("Q3.18"))) %>%
  mutate(direct.contact = ifelse(answered.something & is.na(Q3.18_1), 0, Q3.18_1),
         direct.cooperation = ifelse(answered.something & is.na(Q3.18_2), 0, Q3.18_2),
         received.funding = ifelse(answered.something & is.na(Q3.18_3), 0, Q3.18_3),
         other = ifelse(answered.something & is.na(Q3.18_4), 0, Q3.18_4),
         nothing = ifelse(answered.something & is.na(Q3.18_5), 0, Q3.18_5),
         dont.know = ifelse(answered.something & is.na(Q3.18_6), 0, Q3.18_6)) %>%
  filter(answered.something) %>% select(-c(answered.something, contains("Q3.18")))

#' None of the responses are that corrleated. Contact, cooperation, and funding
#' are all positively correlated at around 0.32-0.46, and having no contact is
#' negatively correlated with those three (as would be expected). They're not
#' really measuring the same thing, though.
#' 
#' This is also evident with the Chronbach's α for contact, cooperation, and
#' funding, which is 0.66. A hypothetical index varaible combining those three
#' variables would not be very internally consistent.
cor.matrix <- as.data.frame(cor(select(contact.with.us, -clean.id))) %>%
  mutate(variable = row.names(.),
         variable = factor(variable, levels=rev(variable), ordered=TRUE))

cor.plot <- cor.matrix %>% gather(variable2, value, -variable) %>%
  mutate(clean.value = round(value, 2))

ggplot(cor.plot, aes(x=variable2, variable, fill=value)) + 
  geom_tile() + 
  geom_text(aes(label=clean.value), size=3, hjust=0.5, color="black",
            family="Source Sans Pro Semibold") + 
  labs(x=NULL, y=NULL, fill="Correlation") + 
  scale_fill_gradient2(midpoint=0, low="#91bfdb", mid="#ffffbf", high="#fc8d59") +
  coord_fixed() +
  theme_clean(12) + theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1),
                          panel.grid=element_blank())

contact.with.us %>% 
  select(direct.contact, direct.cooperation, received.funding) %>% 
  as.data.frame %>%  # Because psych::alpha chokes on data_frames
  psych::alpha()


#' ## Maps of countries where the US has been active
countries.where.active <- responses.full %>%
  select(survey.id, work.country, work.iso3) %>%
  left_join(select(responses.countries, survey.id, Q3.6_c2), by="survey.id") %>%
  filter(Q3.6_c2 == 1) %>%
  group_by(work.iso3) %>%
  summarise(num = n()) %>%
  ungroup() %>%
  right_join(possible.countries, by=c("work.iso3"="id")) %>%
  mutate(presence = ifelse(is.na(num) | num < 1, FALSE, TRUE))

#' Gradient scale
ggplot(countries.where.active, aes(fill=num, map_id=work.iso3)) +
  geom_map(map=countries.ggmap, size=0.15, colour="black") + 
  expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) + 
  coord_equal() +
  scale_fill_gradient(low="grey95", high="grey20", breaks=seq(2, 16, 4), 
                      na.value="#FFFFFF", name="NGOs reporting US activity",
                      guide=guide_colourbar(ticks=FALSE, barwidth=6)) + 
  theme_blank_map() +
  theme(legend.position="bottom", legend.key.size=unit(0.65, "lines"),
        strip.background=element_rect(colour="#FFFFFF", fill="#FFFFFF"))

#' Binary
ggplot(countries.where.active, aes(fill=presence, map_id=work.iso3)) +
  geom_map(map=countries.ggmap, size=0.15, colour="black") + 
  expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) + 
  coord_equal() +
  scale_fill_manual(values=c("#FFFFFF", "grey50"), na.value="#FFFFFF", guide=FALSE) +
  theme_blank_map()
