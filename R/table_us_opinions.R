source(file.path(PROJHOME, "R", "table_functions.R"))
source(file.path(PROJHOME, "R", "shared.R"))

# --------------------------
# Table for opinions of US
# --------------------------
# Initial dataframe
df <- data_frame(Question = NA, ` ` = NA, Summary = NA)

# Q3.19
question.id <- "Q3.19"
var.name <- "Q3.19"
question <- "Overall, how important a role would you say that the United States or its embassy have played in fighting trafficking in your country over the last 10–15 years?"

df <- bind_rows(df, generate.row.single(responses.full, question.id, 
                                        question, var.name))

# Q3.25
question.id <- "Q3.25"
var.name <- "Q3.25"
question <- "Overall, has the US influence on human trafficking policy in your country been positive or negative?"

df <- bind_rows(df, generate.row.single(responses.full, question.id, 
                                        question, var.name))

# Save as Markdown table
cat(pandoc.table.return(slice(df, 2:n()), split.tables=Inf,
                        justify=c("left", "center", "left"), 
                        style="multiline", 
                        caption="Summary of responses related to opinions of the US {#tbl:us-opinions}"), 
    file=file.path(PROJHOME, "manuscript", "tables", "summary_us_opinions.md"))
