source(file.path(PROJHOME, "R", "table_functions.R"))

# --------------------------------------
# Table for "What do anti-TIP NGOs do?
# --------------------------------------
# Initial dataframe
df <- data_frame(Question = NA, ` ` = NA,
                 Summary = NA, `Number of responses` = NA)

# Q3.5
question.id <- "Q3.5"
question <- "Which of these institutions have been active in fighting human trafficking in your country over the last 10–15 years?"
cols <- c("Q3.5_1", "Q3.5_2", "Q3.5_3", "Q3.5_4", "Q3.5_5")
labels <- c("The national government", "NGOs and civil society",
            "Embassies or foreign governments", "International organizations", "Other")

df <- bind_rows(df, generate.row.mult.responses(responses.full, question.id, 
                                                question, cols, labels))

# Q3.20
question.id <- "Q3.20"
var.name <- "Q3.20"
question <- "In your view, how hard is the government of your country working to combat trafficking in persons?"

df <- bind_rows(df, generate.row.single(responses.full, question.id, 
                                        question, var.name))

# Q3.21
question.id <- "Q3.21"
question <- "Has your organization used the US State Department’s Trafficking in Persons (TIP) report to discuss trafficking issues with any of these groups?"
cols <- c("Q3.21_1", "Q3.21_2", "Q3.21_3", "Q3.21_4")
labels <- c("National government", "Another government", "Other NGOs", "Other")

df <- bind_rows(df, generate.row.mult.responses(responses.full, question.id, 
                                                question, cols, labels))

# Q3.29
question.id <- "Q3.29"
var.name <- "Q3.29"
question <- "How much is your organization’s work restricted by government regulations in your country?"

df <- bind_rows(df, generate.row.single(responses.full, question.id, 
                                        question, var.name))

# Save as Markdown table
cat(pandoc.table.return(slice(df, 2:n()), split.tables=Inf,
                        justify=c("left", "center", "left", "center"), 
                        style="multiline"), 
    file=file.path(PROJHOME, "manuscript", "tables", "summary_how_work_others.md"))