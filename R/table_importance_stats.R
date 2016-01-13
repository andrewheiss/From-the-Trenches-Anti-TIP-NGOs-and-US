source(file.path(PROJHOME, "R", "table_functions.R"))

# Initial dataframe
df <- data_frame(Variable = NA, ` ` = NA, `Test statistics` = NA)

# Democracy
fig.importance.freedom <- ggplot(df.importance, aes(x=importance.factor, y=total.freedom)) + 
  # geom_violin(fill="grey90") + 
  geom_point(alpha=0.05, size=0.25) + 
  geom_point(stat="summary", fun.y="mean", size=2) + 
  scale_y_continuous(breaks=seq(2, 14, by=4)) + 
  labs(x=NULL, y=NULL) + 
  coord_flip() + theme_clean(6)

democracy.anova <- aov(total.freedom ~ importance.factor, data=df.importance)

df <- bind_rows(df, generate.stats.row("Total freedom (political rights + civil liberties; higher is worse)", 
                                       fig.importance.freedom, 
                                       democracy.anova, "importance_freedom"))


# Tier rating
fig.importance.tier <- ggplot(df.importance, aes(x=importance.factor, y=avg_tier)) + 
  geom_point(alpha=0.05, size=0.25) + 
  geom_point(stat="summary", fun.y="mean", size=2) + 
  labs(x=NULL, y=NULL) + 
  coord_flip() + theme_clean(6)

tier.anova <- aov(avg_tier ~ importance.factor, data=df.importance)

df <- bind_rows(df, generate.stats.row("Average TIP tier rating (from 2000-2015)", 
                                       fig.importance.tier, 
                                       tier.anova, "importance_tier"))


# Tier change
fig.importance.tier.change <- ggplot(df.importance, aes(x=importance.factor, y=improve_tip)) + 
  geom_point(alpha=0.05, size=0.25) + 
  geom_point(stat="summary", fun.y="mean", size=2) + 
  labs(x=NULL, y=NULL) + 
  coord_flip() + theme_clean(6)

tier.change.anova <- aov(improve_tip ~ importance.factor, data=df.importance)

df <- bind_rows(df, generate.stats.row("Improvement in TIP tier rating", 
                                       fig.importance.tier.change, 
                                       tier.change.anova, "importance_tier_change"))


# Cho change
fig.importance.cho <- ggplot(df.importance, aes(x=importance.factor, y=change_policy)) + 
  geom_point(alpha=0.05, size=0.25) + 
  geom_point(stat="summary", fun.y="mean", size=2) + 
  labs(x=NULL, y=NULL) + 
  coord_flip() + theme_clean(6)

cho.anova <- aov(change_policy ~ importance.factor, data=df.importance)

df <- bind_rows(df, generate.stats.row("Change in TIP policy index", 
                                       fig.importance.cho, 
                                       cho.anova, "importance_cho"))


# Country received funding
fig.importance.fund.country <- ggplot(df.importance, aes(x=importance.factor, y=log.total.funding)) + 
  geom_point(alpha=0.05, size=0.25) + 
  geom_point(stat="summary", fun.y="mean", size=2) + 
  labs(x=NULL, y=NULL) + 
  scale_y_continuous(labels=trans_format("exp", dollar_format())) +
  coord_flip() + theme_clean(6)

fund.country.anova <- aov(log.total.funding ~ importance.factor, data=df.importance)

df <- bind_rows(df, generate.stats.row("Country received US funding for TIP issues", 
                                       fig.importance.fund.country, 
                                       fund.country.anova, "importance_fund_country"))


# Save as Markdown table
cat(pandoc.table.return(slice(df, 2:n()), split.tables=Inf,
                        justify=c("left", "center", "left"), 
                        style="multiline", 
                        caption="Associations between country-level factors and opinions of US importance {#tbl:importance_stats_country}"), 
    file=file.path(PROJHOME, "manuscript", "tables", "summary_importance_stats_country.md"))


# Initial dataframe
df <- data_frame(Variable = NA, ` ` = NA, `Test statistics` = NA)

# Organization received funding
plot.data <- df.importance %>%
  group_by(received.funding, importance.factor) %>%
  summarise(num = n()) %>%
  ungroup() %>%
  mutate(prop = num / sum(num))

fig.importance.funding <- ggplot(plot.data, 
                                 aes(x=importance.factor, y=prop, 
                                     fill=received.funding)) + 
  geom_bar(stat="identity", position="dodge") + 
  labs(x=NULL, y=NULL) + 
  scale_fill_manual(values=c("grey65", "grey35"), guide=FALSE) + 
  scale_y_continuous(labels=percent) + 
  coord_flip() + theme_clean(6)

funding.chi <- df.importance %>%
  xtabs(~ importance.factor + received.funding, .) %>%
  chisq.test(.)

df <- bind_rows(df, generate.stats.row("Organization received US funding (dark bars = yes)", 
                                       fig.importance.funding, 
                                       funding.chi, "importance_funding"))


# Interaction with the US
plot.data <- df.importance %>%
  group_by(us.involvement, importance.factor) %>%
  summarise(num = n()) %>%
  ungroup() %>%
  mutate(prop = num / sum(num))

fig.importance.interaction <- ggplot(plot.data, 
                                 aes(x=importance.factor, y=prop, 
                                     fill=us.involvement)) + 
  geom_bar(stat="identity", position="dodge") + 
  labs(x=NULL, y=NULL) + 
  scale_fill_manual(values=c("grey65", "grey35"), guide=FALSE) + 
  scale_y_continuous(labels=percent) + 
  coord_flip() + theme_clean(6)

interaction.chi <- df.importance %>%
  xtabs(~ importance.factor + us.involvement, .) %>%
  chisq.test(.)

df <- bind_rows(df, generate.stats.row("Organization interacted with the US (dark bars = yes)", 
                                       fig.importance.interaction, 
                                       interaction.chi, "importance_interaction"))


# US HQ
plot.data <- df.importance %>%
  group_by(us.hq, importance.factor) %>%
  summarise(num = n()) %>%
  ungroup() %>%
  mutate(prop = num / sum(num))

fig.importance.hq <- ggplot(plot.data, 
                                 aes(x=importance.factor, y=prop, 
                                     fill=us.hq)) + 
  geom_bar(stat="identity", position="dodge") + 
  labs(x=NULL, y=NULL) + 
  scale_fill_manual(values=c("grey65", "grey35"), guide=FALSE) + 
  scale_y_continuous(labels=percent) + 
  coord_flip() + theme_clean(6)

hq.chi <- df.importance %>%
  xtabs(~ importance.factor + us.hq, .) %>%
  chisq.test(.)

df <- bind_rows(df, generate.stats.row("Organization is based in the US (dark bars = yes)", 
                                       fig.importance.hq, 
                                       hq.chi, "importance_hq"))


# Save as Markdown table
cat(pandoc.table.return(slice(df, 2:n()), split.tables=Inf,
                        justify=c("left", "center", "left"), 
                        style="multiline", 
                        caption="Associations between organization-level factors and opinions of US importance {#tbl:importance_stats_org}"), 
    file=file.path(PROJHOME, "manuscript", "tables", "summary_importance_stats_org.md"))
