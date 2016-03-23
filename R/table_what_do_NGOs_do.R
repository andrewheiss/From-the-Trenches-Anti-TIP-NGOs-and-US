source(file.path(PROJHOME, "R", "table_functions.R"))

# --------------------------------------
# Table for "What do anti-TIP NGOs do?
# --------------------------------------
# Initial dataframe
df <- data_frame(Question = NA, ` ` = NA, Summary = NA)

# Q2.1
question.id <- "Q2.1"
var.name <- "Q2.1"
question <- "About what percent of your organization’s time and resources are spent on fighting against trafficking or helping victims of trafficking?"
binwidth <- 10

df <- bind_rows(df, generate.row.single.num(orgs.only, question.id, 
                                            question, var.name, binwidth))

# Q3.3
question.id <- "Q3.3"
var.name <- "Q3.3"
question <- "How much does your organization know about human trafficking policy in your country?"

df <- bind_rows(df, generate.row.single(responses.full, question.id, 
                                        question, var.name))

# Q2.2
question.id <- "Q2.2"
question <- "Which human trafficking issues is your organization most involved with?"
cols <- c("Q2.2_1", "Q2.2_2", "Q2.2_3", "Q2.2_4")
labels <- c("Organ trafficking", "Sex trafficking",
            "Labor trafficking", "Other")

df <- bind_rows(df, generate.row.mult.responses(orgs.only, question.id, 
                                                question, cols, labels))

# Q2.3
question.id <- "Q2.3"
question <- "Which kinds of victims is your organization most involved with?"
cols <- c("Q2.3_1", "Q2.3_2", "Q2.3_3")
labels <- c("Children", "Adults", "Other")

df <- bind_rows(df, generate.row.mult.responses(orgs.only, question.id, 
                                                question, cols, labels))

# Q2.4
question.id <- "Q2.4"
question <- "Which efforts does your organization focus on most?"
cols <- c("Q2.4_1", "Q2.4_2", "Q2.4_3", "Q2.4_4", "Q2.4_5")
labels <- c("Prevention and education", "Prosecutions and legal issues",
            "Victim protection", "Victim assistance", "Other")

df <- bind_rows(df, generate.row.mult.responses(orgs.only, question.id, 
                                                question, cols, labels))

# Save as Markdown table
cat(pandoc.table.return(slice(df, 2:n()), split.tables=Inf,
                        justify=c("left", "center", "left"), 
                        style="multiline", 
                        caption="Summary of survey responses related to what anti-TIP NGOs do {#tbl:what-do}"), 
    file=file.path(PROJHOME, "manuscript", "tables", "summary_what_do.md"))
