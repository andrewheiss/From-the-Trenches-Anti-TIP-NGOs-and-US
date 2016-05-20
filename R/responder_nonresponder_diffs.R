library(dplyr)
library(tidyr)
library(ggplot2)
library(ggstance)
library(scales)
library(Cairo)
library(readxl)
library(countrycode)
library(vcd)

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

master.list <- read_excel("~/Research/••Projects/Human trafficking/Master NGO list.xlsx", na=".")

actual.responses <- master.list %>% 
  filter(`Survey status` == "Done") %>% select(ID)

non.responses <- master.list %>%
  filter(Contacted != "No") %>%
  filter(!(`Survey status` %in% c("Done", "E-mail dead"))) %>%
  filter(is.na(`Remove?`)) %>% select(ID)

# responses.full <- readRDS(file.path(PROJHOME, "data", "responses_full.rds"))
# orgs.only <- responses.full %>%
#   group_by(survey.id) %>% slice(1) %>% ungroup()

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

responses.hq.region.plot <- responses.hq.region %>%
  as.data.frame %>%
  group_by(continent) %>%
  mutate(group.total = sum(Freq)) %>%
  group_by(continent, responses) %>%
  summarise(freq = sum(Freq),
            freq.prop = freq / max(group.total))

fig.hq.region <- ggplot(responses.hq.region.plot, 
                        aes(x=freq.prop, y=continent, fill=responses)) + 
  geom_barh(stat="identity", position="dodge") +
  labs(x="Proportion", y=NULL, title="Differences in headquarters location") +
  scale_x_continuous(labels=percent) +
  scale_fill_manual(values=c("grey80", "grey30"), name=NULL) +
  theme_clean()

ggsave(fig.hq.region,
       filename=file.path("~", "Research", "•Judith", "jk_misc",
                          "final_figures", "additional_figures",
                          "diffs_hq_region.pdf"),
       width=5, height=3.5, units="in", device=cairo_pdf)
ggsave(fig.hq.region,
       filename=file.path("~", "Research", "•Judith", "jk_misc",
                          "final_figures", "additional_figures",
                          "diffs_hq_region.png"),
       width=5, height=3.5, units="in", type="cairo", dpi=300)


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

responses.type.plot <- responses.type %>%
  as.data.frame %>%
  group_by(Type) %>%
  mutate(group.total = sum(Freq)) %>%
  group_by(Type, responses) %>%
  summarise(freq = sum(Freq),
            freq.prop = freq / max(group.total))

fig.type <- ggplot(responses.type.plot, 
                   aes(x=freq.prop, y=Type, fill=responses)) + 
  geom_barh(stat="identity", position="dodge") +
  labs(x="Proportion", y=NULL, title="Differences in type of NGO") +
  scale_x_continuous(labels=percent) +
  scale_fill_manual(values=c("grey80", "grey30"), name=NULL) +
  theme_clean()

ggsave(fig.type,
       filename=file.path("~", "Research", "•Judith", "jk_misc",
                          "final_figures", "additional_figures",
                          "diffs_type.pdf"),
       width=5, height=3.5, units="in", device=cairo_pdf)
ggsave(fig.type,
       filename=file.path("~", "Research", "•Judith", "jk_misc",
                          "final_figures", "additional_figures",
                          "diffs_type.png"),
       width=5, height=3.5, units="in", type="cairo", dpi=300)


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

responses.focus
prop.table(responses.focus, 1)
prop.table(responses.focus, 2)

responses.focus.plot <- responses.focus %>%
  as.data.frame %>%
  group_by(focus) %>%
  mutate(group.total = sum(Freq)) %>%
  group_by(focus, responses) %>%
  summarise(freq = sum(Freq),
            freq.prop = freq / max(group.total))

fig.focus <- ggplot(responses.focus.plot, 
                    aes(x=freq.prop, y=focus, fill=responses)) + 
  geom_barh(stat="identity", position="dodge") +
  labs(x="Proportion", y=NULL, title="Differences in focus of NGO",
       subtitle="Primary focus of NGO is TIP") +
  scale_x_continuous(labels=percent) +
  scale_fill_manual(values=c("grey80", "grey30"), name=NULL) +
  theme_clean()

ggsave(fig.focus,
       filename=file.path("~", "Research", "•Judith", "jk_misc",
                          "final_figures", "additional_figures",
                          "diffs_focus.pdf"),
       width=5, height=2, units="in", device=cairo_pdf)
ggsave(fig.focus,
       filename=file.path("~", "Research", "•Judith", "jk_misc",
                          "final_figures", "additional_figures",
                          "diffs_focus.png"),
       width=5, height=2, units="in", type="cairo", dpi=300)

