library(magrittr)
library(dplyr)
library(broom)
library(ggplot2)
library(scales)
library(vcd)
library(Cairo)
library(pander)

# Load data
responses.full <- readRDS(file.path(PROJHOME, "data", "responses_full.rds"))
orgs.only <- responses.full %>%
  group_by(survey.id) %>% slice(1) %>% ungroup()

df.importance <- responses.full %>% 
  select(Q3.19, work.country, change_policy, avg_tier, improve_tip, change_policy, 
         importance, received.funding, us.involvement, total.funding, 
         total.freedom, us.hq, time.spent=Q2.1) %>% 
  filter(!is.na(Q3.19)) %>%
  filter(Q3.19 != "Don't know") %>%
  mutate(importance.factor = factor(Q3.19, ordered=FALSE),
         log.total.funding = log1p(total.funding),
         time.spent = as.numeric(time.spent))

df.positivity <- responses.full %>% 
  select(Q3.25=Q3.25_collapsed, work.country, change_policy, avg_tier, 
         improve_tip, change_policy, importance, received.funding, us.involvement, 
         total.funding, total.freedom, us.hq, time.spent=Q2.1) %>% 
  filter(!is.na(Q3.25)) %>%
  filter(Q3.25 != "Don't know") %>%
  mutate(positivity.factor = factor(Q3.25, ordered=FALSE),
         log.total.funding = log1p(total.funding),
         time.spent = as.numeric(time.spent))

# Helpful functions
theme_spark <- function() {
  theme(line = element_blank(), rect = element_blank(), text = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_rect(fill="transparent", colour=NA),
        plot.background = element_rect(fill="transparent", colour=NA),
        plot.margin = unit(c(0, 0, -0.25, -0.25), "lines"), complete=TRUE)
}

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
          strip.text=element_text(size=rel(1), family="Source Sans Pro Semibold"),
          strip.background=element_rect(fill="#ffffff", colour=NA),
          panel.margin=unit(1, "lines"), legend.key.size=unit(.7, "line"),
          legend.key=element_blank())
  
  ret
}

generate.row.mult.responses <- function(df, question.id, question, 
                                        cols, labels, sorted=FALSE) {
  answer.summary <- suppressWarnings(separate.answers.summary(df, cols, labels))
  
  if (sorted) {
    df.summary <- answer.summary$df %>%
      arrange(desc(plot.pct)) %>%
      mutate(Answer=factor(Answer, levels=Answer, ordered=TRUE))
  } else {
    df.summary <- answer.summary$df %>%
      mutate(Answer=factor(Answer, levels=labels, ordered=TRUE))
  }
  
  df.summary <- df.summary %>%
    mutate(text.summary = paste0(Answer, " (", `%`, "%; ", Responses, ")"))
  
  fig <- ggplot(df.summary, aes(x=Answer, y=Responses)) +
    geom_bar(aes(y=plot.pct), stat="identity") + 
    labs(x=NULL, y=NULL) + theme_spark()
  
  file.name <- gsub("\\.| ", "_", tolower(question.id))
  file.pdf <- file.path(PROJHOME, 
                        "figures", "summary_table", 
                        paste0(file.name, ".pdf"))
  file.png <- file.path(PROJHOME, 
                        "figures", "summary_table", 
                        paste0(file.name, ".png"))
  md.img <- paste0("![](figures/summary_table/", file.name, ".pdf)")
  # md.img <- paste0("![](", file.pdf, ")")

  ggsave(fig, filename=file.pdf, width=1, height=0.2)
  ggsave(fig, filename=file.png, width=1, height=0.2, type="cairo", bg="transparent")
  
  row.final <- data_frame(Question = paste0(question.id, ": ", question),
                          ` ` = md.img,
                          Summary = paste0(paste(df.summary$text.summary, collapse="; "),
                                           "; N=", answer.summary$denominator))
  row.final
}

generate.row.single <- function(df, question.id, question, x) {
  df.summary <- df %>%
    # Wonky way to filter with NSE
    filter_(lazyeval::interp(~ !is.na(x), x=as.name(x))) %>%
    group_by_(.dots=x) %>%
    summarise(num = n()) %>%
    mutate(pct = round(num / sum(num) * 100, 2),
           plot.pct = num / sum(num)) %>%
    select(var.to.plot = 1, everything()) %>%
    mutate(text.summary = paste0(var.to.plot, " (", pct, "%; ", num, ")"))
  df.summary

  fig <- ggplot(df.summary, aes(x=var.to.plot, y=plot.pct)) +
    geom_bar(stat="identity") +
    labs(x=NULL, y=NULL) + theme_spark()

  file.name <- gsub("\\.| ", "_", tolower(question.id))
  file.pdf <- file.path(PROJHOME,
                        "figures", "summary_table",
                        paste0(file.name, ".pdf"))
  file.png <- file.path(PROJHOME,
                        "figures", "summary_table",
                        paste0(file.name, ".png"))
  md.img <- paste0("![](figures/summary_table/", file.name, ".pdf)")
  # md.img <- paste0("![](", file.pdf, ")")

  ggsave(fig, filename=file.pdf, width=1, height=0.2)
  ggsave(fig, filename=file.png, width=1, height=0.2, type="cairo", bg="transparent")

  row.final <- data_frame(Question = paste0(question.id, ": ", question),
                          ` ` = md.img,
                          Summary = paste0(paste(df.summary$text.summary, collapse="; "),
                                           "; N=", sum(df.summary$num)))
  row.final
}

generate.row.single.num <- function(df, question.id, question, x, binwidth) {
  df.plot <- df %>%
    # Wonky way to filter with NSE
    filter_(lazyeval::interp(~ !is.na(x), x=as.name(x))) %>%
    select_(.dots=x) %>% select(var.to.plot = 1) %>%
    mutate(var.to.plot = as.numeric(var.to.plot))
  
  df.summary <- df.plot %>%
    summarise(num = n(),
              avg = mean(var.to.plot),
              med = median(var.to.plot),
              stdev = sd(var.to.plot),
              minimum = min(var.to.plot),
              maximum = max(var.to.plot)) %>%
    mutate(text.summary = paste0("Mean: ", round(avg, 2), 
                                 "; median: ", round(med, 2), 
                                 "; standard deviation: ", round(stdev, 2)))
  
  fig <- ggplot(data=df.plot, aes(x=var.to.plot)) +
    geom_histogram(binwidth=binwidth) + theme_spark()
  fig
  
  file.name <- gsub("\\.| ", "_", tolower(question.id))
  file.pdf <- file.path(PROJHOME,
                        "figures", "summary_table",
                        paste0(file.name, ".pdf"))
  file.png <- file.path(PROJHOME,
                        "figures", "summary_table",
                        paste0(file.name, ".png"))
  md.img <- paste0("![](figures/summary_table/", file.name, ".pdf)")
  # md.img <- paste0("![](", file.pdf, ")")
  
  ggsave(fig, filename=file.pdf, width=1, height=0.2)
  ggsave(fig, filename=file.png, width=1, height=0.2, type="cairo", bg="transparent")
  
  row.final <- data_frame(Question = paste0(question.id, ": ", question),
                          ` ` = md.img,
                          Summary = paste0(paste(df.summary$text.summary, collapse="; "),
                                           "; N=", sum(df.summary$num)))
  row.final
}

generate.row.text <- function(df, question.id, question, x) {
  df.summary <- df %>%
    # Wonky way to filter with NSE
    filter_(lazyeval::interp(~ !is.na(x), x=as.name(x))) %>%
    select_(.dots=x) %>% select(Answer = 1)
  
  row.final <- data_frame(Question = paste0(question.id, ": ", question),
                          ` ` = "—",
                          Summary = "Free response answer",
                          `Number of responses` = nrow(df.summary))
  row.final
}

pretty.stats <- function(stats) {
  # Pretty format test statistics
  if ("aov" %in% class(stats)) {
    df <- tidy(stats) %>%
      slice(1)
    
    if (df$p.value > 0.001) {
      out <- sprintf("F (%d, %d) = %.2f, p = %.3f", 
                     df$df, df.residual(stats), df$statistic, df$p.value)
    } else {
      out <- sprintf("F (%d, %d) = %.2f, p < 0.001", 
                     df$df, df.residual(stats), df$statistic)
    }
  } else if (labels(stats$statistic) == "X-squared") {
    df <- tidy(stats)
    n <- sum(stats$observed)
    cramer <- assocstats(stats$observed)$cramer

    if (df$p.value > 0.001) {
      # out <- sprintf("χ^2 (%d, N = %d) = %.2f, p = %.3f, ϕ~c~ = %.2f",
      #                df$parameter, n, df$statistic, df$p.value, cramer)
      out <- sprintf("chi^2*' '(%d, ~N == %d) == %.2f~~p == %.3f~~phi[c] == '%.2f'",
                                df$parameter, n, df$statistic, df$p.value, cramer)
    } else {
      # out <- sprintf("χ^2 (%d, N = %d) = %.2f, p < 0.001, ϕ~c~ = %.2f",
      #                df$parameter, n, df$statistic, cramer)
      out <- sprintf("chi^2*' '(%d, ~N == %d) == %.2f~~p < 0.001~~phi[c] =='%.2f'",
                     df$parameter, n, df$statistic, cramer)
    }
  } else if (labels(stats$statistic) == "t") {
    df <- tidy(stats)
    
    if (df$p.value > 0.001) {
      out <- sprintf("t (%.2f) = %.2f, p = %.3f",
                     df$parameter, df$statistic, df$p.value)
    } else {
      out <- sprintf("t (%.2f) = %.2f, p < 0.001",
                     df$parameter, df$statistic, df$p.value)
    }
  }
  
  return(out)
}

generate.stats.row <- function(var.name, fig, stats, file.name) {
  # Save image
  file.pdf <- file.path(PROJHOME, 
                        "figures", "summary_table", 
                        paste0(file.name, ".pdf"))
  file.png <- file.path(PROJHOME, 
                        "figures", "summary_table", 
                        paste0(file.name, ".png"))
  md.img <- paste0("![](figures/summary_table/", file.name, ".pdf)")
  # md.img <- paste0("![](", file.pdf, ")")
  
  ggsave(fig, filename=file.pdf, width=3, height=0.5, device=cairo_pdf)
  ggsave(fig, filename=file.png, width=3, height=0.5, type="cairo", bg="transparent")
  
  # Save as row
  row.final <- data_frame(Variable = var.name,
                          ` ` = md.img,
                          `Test statistics` = pretty.stats(stats))
  row.final
}
