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
