source(file.path(PROJHOME, "R", "table_functions.R"))
library(ggstance)
library(stringr)
library(lazyeval)
library(gridExtra)

# Blank plot for spacing things in arrangeGrob()
blank <- rectGrob(gp=gpar(col="white"))

df.importance <- df.importance %>%
  mutate(importance.factor = factor(importance.factor, 
                                    levels=levels(importance.factor),
                                    labels=str_replace_all(levels(importance.factor),
                                                           c(" actor" = "", " an" = "")),
                                    ordered=TRUE))

df.positivity <- df.positivity %>%
  mutate(positivity.factor = factor(positivity.factor, 
                                    levels=levels(positivity.factor),
                                    labels=str_replace_all(levels(positivity.factor),
                                                           c(" actor" = "", " an" = "")),
                                    ordered=TRUE))

plot.group.means <- function(var.name, title=NULL, subtitle=NULL, 
                             xlab=NULL, xlim=NULL, log.x=FALSE) {
  summary_dots <- list(
    n = ~ n(),
    avg = interp(~ mean(val, na.rm = T), val=as.name(var.name)),
    stdev = interp(~ sd(val, na.rm = T), val = as.name(var.name))
  )
  
  plot.data.importance <- df.importance %>%
    group_by(importance.factor) %>%
    summarise_(.dots=summary_dots) %>%
    mutate(std.error = stdev / sqrt(n),
           lower = avg + (qnorm(0.025) * std.error),
           upper = avg + (qnorm(0.975) * std.error)) %>%
    rename(response = importance.factor) %>%
    mutate(question = "Importance",
           response = factor(response, levels=rev(levels(response)), ordered=TRUE))
  plot.data.importance$stats <- pretty.stats(aov(get(var.name) ~ importance.factor,
                                                 data=df.importance))
  
  plot.data.positivity <- df.positivity %>%
    group_by(positivity.factor) %>%
    summarise_(.dots=summary_dots) %>%
    mutate(std.error = stdev / sqrt(n),
           lower = avg + (qnorm(0.025) * std.error),
           upper = avg + (qnorm(0.975) * std.error)) %>%
    rename(response = positivity.factor) %>%
    mutate(question = "Positivity")
  plot.data.positivity$stats <- pretty.stats(t.test(get(var.name) ~ positivity.factor,
                                                    data=df.positivity))
  
  plot.data <- suppressWarnings(bind_rows(plot.data.importance, plot.data.positivity)) %>%
    mutate(response = factor(response, levels=c(levels(plot.data.importance$response),
                                                levels(plot.data.positivity$response)),
                             ordered=TRUE))
  
  final <- ggplot(plot.data, aes(x=avg, y=response, xmin=lower, xmax=upper)) +
    geom_pointrangeh(size=0.25) +
    labs(x=xlab, y=NULL, title=title, subtitle=subtitle) +
    geom_text(aes(label=stats, x=Inf, y=-Inf-3), hjust="right", vjust="bottom", size=1.5,
              family="Source Sans Pro Light") +
    facet_wrap(~ question, scales="free_y", ncol=1) +
    theme_clean(6) + theme(strip.text=element_blank(),
                           plot.title=element_text(hjust=0.5))
  
  if (log.x) {
    final <- final +
      scale_x_continuous(limits=xlim, 
                         labels=trans_format("exp", dollar_format()))
  } else {
    final <- final + scale_x_continuous(limits=xlim)
  }
  
  final
}


plot.group.props <- function(var.name, title=NULL, xlab=NULL) {
  plot.data.importance <- df.importance %>%
    rename_(var.name = interp(~val, val=as.name(var.name)),
            response = "importance.factor") %>%
    group_by(var.name, response) %>%
    summarise(num = n()) %>% ungroup() %>%
    mutate(prop = num / sum(num),
           question = "Importance",
           response = factor(response, levels=rev(levels(response)), ordered=TRUE))
  
  chisq <- df.importance %>%
    xtabs(~ importance.factor + get(var.name), .) %>%
    chisq.test(.)
  
  plot.data.importance$stats <- pretty.stats(chisq)
  
  
  plot.data.positivity <- df.positivity %>%
    rename_(var.name = interp(~val, val=as.name(var.name)),
            response = "positivity.factor") %>%
    group_by(var.name, response) %>%
    summarise(num = n()) %>% ungroup() %>%
    mutate(prop = num / sum(num),
           question = "Positivity")
  
  chisq <- df.positivity %>%
    xtabs(~ positivity.factor + get(var.name), .) %>%
    chisq.test(.)
  
  plot.data.positivity$stats <- pretty.stats(chisq)
  
  
  plot.data <- suppressWarnings(bind_rows(plot.data.importance, plot.data.positivity)) %>%
    mutate(response = factor(response, levels=c(levels(plot.data.importance$response),
                                                levels(plot.data.positivity$response)),
                             ordered=TRUE),
           var.name = factor(var.name, labels=c("No    ", "Yes"), ordered=TRUE))
  
  ggplot(plot.data, aes(y=response, x=prop, colour=var.name)) +
    geom_pointrangeh(aes(xmin=0, xmax=prop), position=position_dodge(width=0.25), size=0.25) +
    # geom_barh(stat="identity", position="dodge") + 
    geom_text(aes(label=stats, x=Inf, y=-Inf-3), hjust="right", vjust="bottom", size=1.5,
              family="Source Sans Pro Light", parse=TRUE) +
    scale_colour_manual(values=c("grey75", "grey25"), name=NULL) +
    scale_x_continuous(labels=percent) +
    labs(x=NULL, y=NULL, title=title) +
    facet_wrap(~ question, scales="free_y", ncol=1) +
    theme_clean(6) + theme(strip.text=element_blank(),
                           plot.title=element_text(hjust=0.5),
                           legend.position="bottom",
                           legend.margin = unit(0.1, "lines"))
}


# Country-level factors
plot.democracy <- plot.group.means("total.freedom", 
                                   xlab="Total freedom", #xlim=c(0, 14),
                                   title="Total freedom",
                                   subtitle="Political rights + civil liberties; higher values are worse")

plot.tiers <- plot.group.means("avg_tier", 
                               xlab="Tier rating", #xlim=c(0, 3),
                               title="TIP tier rating",
                               subtitle="Higher values are worse")

plot.improve.tip <- plot.group.means("improve_tip", 
                                     xlab="Change in TIP tier rating",
                                     title="Change in TIP tier rating",
                                     subtitle="Most recent TIP score − initial TIP score; higher values are worse")

plot.cho.change <- plot.group.means("change_policy", 
                                    xlab="Change in TIP policy index",
                                    title="Change in TIP policy index",
                                    subtitle="Most recent 3P score − initial 3P score; higher values are better")

plot.funding <- plot.group.means("log.total.funding", xlim=c(8, 16.1),
                                 xlab="TIP funding (logged)", log.x=TRUE,
                                 title="TIP funding received by country",
                                 subtitle="Logged")

# Combine everything
plot.country <- arrangeGrob(plot.democracy, plot.tiers, 
                            blank, blank,
                            plot.cho.change, plot.funding,
                            ncol=2, heights=c(0.475, 0.05, 0.475))

ggsave(plot.country, filename=file.path(PROJHOME, "figures", "fig_means_country.pdf"),
       width=5, height=3.5, units="in", device=cairo_pdf)
ggsave(plot.country, filename=file.path(PROJHOME, "figures", "fig_means_country.png"),
       width=5, height=3.5, units="in", type="cairo", dpi=300)


# Organization-level factors
plot.org.funding <- plot.group.props("received.funding", 
                                     title="Organization received US funding")

plot.involvement <- plot.group.props("us.involvement", 
                                     title="Organization interacted with the US")

plot.us.hq <- plot.group.props("us.hq", 
                               title="Organization is based in the US")

plot.org <- arrangeGrob(plot.org.funding, blank, 
                        plot.involvement, blank, 
                        plot.us.hq, ncol=1,
                        heights=c(0.31666, 0.025, 0.31666, 0.025, 0.31666))

ggsave(plot.org, filename=file.path(PROJHOME, "figures", "fig_means_org.pdf"),
       width=2.5, height=5, units="in", device=cairo_pdf)
ggsave(plot.org, filename=file.path(PROJHOME, "figures", "fig_means_org.png"),
       width=2.5, height=5, units="in", type="cairo", dpi=300)
