source(file.path(PROJHOME, "R", "table_functions.R"))

# --------------------------------------
# Table for "What do anti-TIP NGOs do?
# --------------------------------------
# Initial dataframe
df <- data_frame(Question = NA, ` ` = NA,
                 Summary = NA, `Number of responses` = NA)

# Q2.5
question.id <- "Q2.5"
var.name <- "Q2.5"
question <- "The US State Department issues an annual Trafficking in Persons (TIP) report. Have you ever heard of this annual report?"

df <- bind_rows(df, generate.row.single(orgs.only, question.id, 
                                        question, var.name))

# Q3.21
question.id <- "Q3.21"
question <- "Has your organization used the US State Department’s Trafficking in Persons (TIP) report to discuss trafficking issues with any of these groups?"
cols <- c("Q3.21_1", "Q3.21_2", "Q3.21_3", "Q3.21_4")
labels <- c("National government", "Another government", "Other NGOs", "Other")

df <- bind_rows(df, generate.row.mult.responses(responses.full, question.id, 
                                                question, cols, labels))

# Q3.22
question.id <- "Q3.22"
var.name <- "Q3.22"
question <- "Which TIP tier rating did your country receive this year?"

df <- bind_rows(df, generate.row.single(responses.full, question.id, 
                                        question, var.name))

# Q3.23
question.id <- "Q3.23"
var.name <- "Q3.23"
question <- "Have you ever heard—in public or private—officials in your country mention the TIP tier rating?"

df <- bind_rows(df, generate.row.single(responses.full, question.id, 
                                        question, var.name))

# Save as Markdown table
cat(pandoc.table.return(slice(df, 2:n()), split.tables=Inf,
                        justify=c("left", "center", "left", "center"), 
                        style="multiline", 
                        caption="Summary of survey responses related to how anti-TIP NGOs use the State Department's annual TIP report {#tbl:tip_report}"), 
    file=file.path(PROJHOME, "manuscript", "tables", "summary_tip_report.md"))
