library(magrittr)
library(dplyr)
library(ggplot2)
library(pander)

# Load data
responses.full <- readRDS(file.path(PROJHOME, "data", "responses_full.rds"))
orgs.only <- responses.full %>%
  group_by(survey.id) %>% slice(1) %>% ungroup()

# Helpful functions
theme_spark <- function() {
  theme(line = element_blank(), rect = element_blank(), text = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_rect(fill="transparent", colour=NA),
        plot.background = element_rect(fill="transparent", colour=NA),
        plot.margin = unit(c(0, 0, -0.25, -0.25), "lines"), complete=TRUE)
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
                          Summary = paste(df.summary$text.summary, collapse="; "),
                          `Number of responses` = answer.summary$denominator)
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
                          Summary = paste(df.summary$text.summary, collapse="; "),
                          `Number of responses` = sum(df.summary$num))
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
                          Summary = paste(df.summary$text.summary, collapse="; "),
                          `Number of responses` = sum(df.summary$num))
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
