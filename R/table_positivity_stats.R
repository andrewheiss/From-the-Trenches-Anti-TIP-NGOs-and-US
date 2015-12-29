source(file.path(PROJHOME, "R", "table_functions.R"))

# Initial dataframe
df <- data_frame(Variable = NA, ` ` = NA, `Test statistics` = NA)

# Democracy
fig.positivity.freedom <- ggplot(df.positivity, aes(x=positivity.factor, y=total.freedom)) + 
  # geom_violin(fill="grey90") + 
  geom_point(alpha=0.05, size=0.25) + 
  geom_point(stat="summary", fun.y="mean", size=2) + 
  scale_y_continuous(breaks=seq(2, 14, by=4)) + 
  labs(x=NULL, y=NULL) + 
  coord_flip() + theme_clean(6)

democracy.ttest <- t.test(total.freedom ~ positivity.factor, data=df.positivity)

df <- bind_rows(df, generate.stats.row("Total freedom (political rights + civil liberties; higher is worse)", 
                                       fig.positivity.freedom, 
                                       democracy.ttest, "positivity_freedom"))


# Tier rating
fig.positivity.tier <- ggplot(df.positivity, aes(x=positivity.factor, y=avg_tier)) + 
  geom_point(alpha=0.05, size=0.25) + 
  geom_point(stat="summary", fun.y="mean", size=2) + 
  labs(x=NULL, y=NULL) + 
  coord_flip() + theme_clean(6)

tier.ttest <- t.test(avg_tier ~ positivity.factor, data=df.positivity)

df <- bind_rows(df, generate.stats.row("Average TIP tier rating (from 2000-2015)", 
                                       fig.positivity.tier, 
                                       tier.ttest, "positivity_tier"))


# Tier change
fig.positivity.tier.change <- ggplot(df.positivity, aes(x=positivity.factor, y=improve_tip)) + 
  geom_point(alpha=0.05, size=0.25) + 
  geom_point(stat="summary", fun.y="mean", size=2) + 
  labs(x=NULL, y=NULL) + 
  coord_flip() + theme_clean(6)

tier.change.ttest <- t.test(improve_tip ~ positivity.factor, data=df.positivity)

df <- bind_rows(df, generate.stats.row("Improvement in TIP tier rating", 
                                       fig.positivity.tier.change, 
                                       tier.change.ttest, "positivity_tier_change"))


# Cho change
fig.positivity.cho <- ggplot(df.positivity, aes(x=positivity.factor, y=change_policy)) + 
  geom_point(alpha=0.05, size=0.25) + 
  geom_point(stat="summary", fun.y="mean", size=2) + 
  labs(x=NULL, y=NULL) + 
  coord_flip() + theme_clean(6)

cho.ttest <- t.test(change_policy ~ positivity.factor, data=df.positivity)

df <- bind_rows(df, generate.stats.row("Change in TIP policy index", 
                                       fig.positivity.cho, 
                                       cho.ttest, "positivity_cho"))


# Country received funding
fig.positivity.fund.country <- ggplot(df.positivity, aes(x=positivity.factor, y=log.total.funding)) + 
  geom_point(alpha=0.05, size=0.25) + 
  geom_point(stat="summary", fun.y="mean", size=2) + 
  labs(x=NULL, y=NULL) + 
  scale_y_continuous(labels=trans_format("exp", dollar_format())) +
  coord_flip() + theme_clean(6)

fund.country.ttest <- t.test(log.total.funding ~ positivity.factor, data=df.positivity)

df <- bind_rows(df, generate.stats.row("Change in TIP policy index", 
                                       fig.positivity.fund.country, 
                                       fund.country.ttest, "positivity_fund_country"))


# Organization received funding
plot.data <- df.positivity %>%
  group_by(received.funding, positivity.factor) %>%
  summarise(num = n()) %>%
  ungroup() %>%
  mutate(prop = num / sum(num))

fig.positivity.funding <- ggplot(plot.data, 
                                 aes(x=positivity.factor, y=prop, 
                                     fill=received.funding)) + 
  geom_bar(stat="identity", position="dodge") + 
  labs(x=NULL, y=NULL) + 
  scale_fill_manual(values=c("grey65", "grey35"), guide=FALSE) + 
  scale_y_continuous(labels=percent) + 
  coord_flip() + theme_clean(6)

funding.chi <- df.positivity %>%
  xtabs(~ positivity.factor + received.funding, .) %>%
  chisq.test(.)

df <- bind_rows(df, generate.stats.row("Organization received US funding", 
                                       fig.positivity.funding, 
                                       funding.chi, "positivity_funding"))


# Interaction with the US
plot.data <- df.positivity %>%
  group_by(us.involvement, positivity.factor) %>%
  summarise(num = n()) %>%
  ungroup() %>%
  mutate(prop = num / sum(num))

fig.positivity.interaction <- ggplot(plot.data, 
                                 aes(x=positivity.factor, y=prop, 
                                     fill=us.involvement)) + 
  geom_bar(stat="identity", position="dodge") + 
  labs(x=NULL, y=NULL) + 
  scale_fill_manual(values=c("grey65", "grey35"), guide=FALSE) + 
  scale_y_continuous(labels=percent) + 
  coord_flip() + theme_clean(6)

interaction.chi <- df.positivity %>%
  xtabs(~ positivity.factor + us.involvement, .) %>%
  chisq.test(.)

df <- bind_rows(df, generate.stats.row("Organization interacted with the US", 
                                       fig.positivity.interaction, 
                                       interaction.chi, "positivity_interaction"))


# US HQ
plot.data <- df.positivity %>%
  group_by(us.hq, positivity.factor) %>%
  summarise(num = n()) %>%
  ungroup() %>%
  mutate(prop = num / sum(num))

fig.positivity.hq <- ggplot(plot.data, 
                                 aes(x=positivity.factor, y=prop, 
                                     fill=us.hq)) + 
  geom_bar(stat="identity", position="dodge") + 
  labs(x=NULL, y=NULL) + 
  scale_fill_manual(values=c("grey65", "grey35"), guide=FALSE) + 
  scale_y_continuous(labels=percent) + 
  coord_flip() + theme_clean(6)

hq.chi <- df.positivity %>%
  xtabs(~ positivity.factor + us.hq, .) %>%
  chisq.test(.)

df <- bind_rows(df, generate.stats.row("Organization is based in the US", 
                                       fig.positivity.hq, 
                                       hq.chi, "positivity_funding"))


# Save as Markdown table
cat(pandoc.table.return(slice(df, 2:n()), split.tables=Inf,
                        justify=c("left", "center", "left"), 
                        style="multiline", 
                        caption="CAPTION HERE {#tbl:positivity_stats}"), 
    file=file.path(PROJHOME, "manuscript", "tables", "summary_positivity_stats.md"))