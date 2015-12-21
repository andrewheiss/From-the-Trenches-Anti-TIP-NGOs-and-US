source(file.path(PROJHOME, "R", "table_functions.R"))

# Initial dataframe
# The first row will be ignored when writing the file with cat(). This is just
# so there's something for bind_rows to start appending to.
df <- data_frame(Question = NA, ` ` = NA,
                 Summary = NA, `Number of responses` = NA)

# Q1.5
question.id <- "Q1.5"
var.name <- "Q1.5.factor"
question <- "In how many countries has your organization done most of its advocacy work over the past 10 years?"

df <- bind_rows(df, generate.row.single(orgs.only, question.id, 
                                        question, var.name))

# Q2.1
question.id <- "Q2.1"
var.name <- "Q2.1"
question <- "About what percent of your organization’s time and resources are spent on fighting against trafficking or helping victims of trafficking?"
binwidth <- 10

df <- bind_rows(df, generate.row.single.num(orgs.only, question.id, 
                                            question, var.name, binwidth))

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

# Q2.5
question.id <- "Q2.5"
var.name <- "Q2.5"
question <- "The US State Department issues an annual Trafficking in Persons (TIP) report. Have you ever heard of this annual report?"

df <- bind_rows(df, generate.row.single(orgs.only, question.id, 
                                        question, var.name))

# Q2.6
question.id <- "Q2.6"
var.name <- "Q2.6"
question <- "In Fall 2013 an NGO named WalkFree published a “Human Trafficking Index” that rated countries on how many trafficking victims they have. Have you heard of this index?"

df <- bind_rows(df, generate.row.single(orgs.only, question.id, 
                                        question, var.name))

# Q3.3
question.id <- "Q3.3"
var.name <- "Q3.3"
question <- "How much does your organization know about human trafficking policy in your country?"

df <- bind_rows(df, generate.row.single(responses.full, question.id, 
                                        question, var.name))

# Q3.4
question.id <- "Q3.4"
var.name <- "Q3.4"
question <- "How often does your organization work directly with the government of your country?"

df <- bind_rows(df, generate.row.single(responses.full, question.id, 
                                        question, var.name))

# Q3.5
question.id <- "Q3.5"
question <- "Which of these institutions have been active in fighting human trafficking in your country over the last 10–15 years?"
cols <- c("Q3.5_1", "Q3.5_2", "Q3.5_3", "Q3.5_4", "Q3.5_5")
labels <- c("The national government", "NGOs and civil society",
            "Embassies or foreign governments", "International organizations", "Other")

df <- bind_rows(df, generate.row.mult.responses(responses.full, question.id, 
                                                question, cols, labels))

# Q3.8
question.id <- "Q3.8"
var.name <- "Q3.8"
question <- "Over the last 10–15 years, has the United States or its embassy been active in the fight against human trafficking in your country?"

df <- bind_rows(df, generate.row.single(responses.full, question.id, 
                                        question, var.name))

# Q3.9
question.id <- "Q3.9"
question <- "Has the United States or its embassy been involved in any of the following activities in your country?"
cols <- c("Q3.9_1", "Q3.9_2", "Q3.9_3", "Q3.9_4", "Q3.9_5",
          "Q3.9_6", "Q3.9_7", "Q3.9_8", "Q3.9_9", "Q3.9_10")
labels <- c("Asking for legislation", "Convening conferences or workshops",
            "Raising awareness", "Providing resources or funding",
            "Increasing government attention", "Training government officials",
            "Contributing to a government action plan", "Other", "Don't know",
            "The US has not been involved in trafficking issues")

df <- bind_rows(df, generate.row.mult.responses(responses.full, question.id, 
                                                question, cols, labels))

# Q3.10
question.id <- "Q3.10"
var.name <- "Q3.10"
question <- "Please explain how the United States asked the government to pass or amend anti-trafficking laws in your country."

df <- bind_rows(df, generate.row.text(responses.full, question.id, 
                                      question, var.name))

# Q3.11
question.id <- "Q3.11"
var.name <- "Q3.11"
question <- "Please explain how the United States convened conferences or workshops on trafficking in your country."

df <- bind_rows(df, generate.row.text(responses.full, question.id, 
                                      question, var.name))

# Q3.12
question.id <- "Q3.12"
var.name <- "Q3.12"
question <- "Please explain how the United States raised awareness about trafficking in your country."

df <- bind_rows(df, generate.row.text(responses.full, question.id, 
                                      question, var.name))

# Q3.13
question.id <- "Q3.13"
var.name <- "Q3.13"
question <- "Please explain how the United States provided resources or funding for anti-trafficking programs in your country."

df <- bind_rows(df, generate.row.text(responses.full, question.id, 
                                      question, var.name))

# Q3.14
question.id <- "Q3.14"
var.name <- "Q3.14"
question <- "Please explain how the United States increased government attention to trafficking in your country."

df <- bind_rows(df, generate.row.text(responses.full, question.id, 
                                      question, var.name))

# Q3.15
question.id <- "Q3.15"
var.name <- "Q3.15"
question <- "Please explain how the United States trained government officials in your country."

df <- bind_rows(df, generate.row.text(responses.full, question.id, 
                                      question, var.name))

# Q3.16
question.id <- "Q3.16"
var.name <- "Q3.16"
question <- "Please explain how the United States contributed to a government action plan in your country."

df <- bind_rows(df, generate.row.text(responses.full, question.id, 
                                      question, var.name))

# Q3.17
question.id <- "Q3.17"
var.name <- "Q3.17"
question <- "Please explain how else the US government has been involved in trafficking issues in your country."

df <- bind_rows(df, generate.row.text(responses.full, question.id, 
                                      question, var.name))

# Q3.18
question.id <- "Q3.18"
question <- "Over the last 10–15 years, has your organization worked directly with or had direct contact with the US embassy or government on human trafficking issues?"
cols <- c("Q3.18_1", "Q3.18_2", "Q3.18_3", "Q3.18_4", "Q3.18_5", "Q3.18_6")
labels <- c("Direct contact (meetings)", "Direct cooperation", 
            "Our organization received funding", "Other", 
            "We have not had any contact or funding from the US", "Don't know")

df <- bind_rows(df, generate.row.mult.responses(responses.full, question.id, 
                                                question, cols, labels))

# Q3.19
question.id <- "Q3.19"
var.name <- "Q3.19"
question <- "Overall, how important a role would you say that the United States or its embassy have played in fighting trafficking in your country over the last 10–15 years?"

df <- bind_rows(df, generate.row.single(responses.full, question.id, 
                                        question, var.name))

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

# Q3.24
question.id <- "Q3.24"
var.name <- "Q3.24.Text"
question <- "What was their reason for mentioning the TIP report?"

df <- bind_rows(df, generate.row.text(responses.full, question.id, 
                                      question, var.name))

# Q3.25
question.id <- "Q3.25"
var.name <- "Q3.25"
question <- "Overall, has the US influence on human trafficking policy in your country been positive or negative?"

df <- bind_rows(df, generate.row.single(responses.full, question.id, 
                                        question, var.name))

# Q3.26
question.id <- "Q3.26"
var.name <- "Q3.26"
question <- "How have the government of your country's efforts to combat trafficking changed over the past 10-15 years?"

df <- bind_rows(df, generate.row.single(responses.full, question.id, 
                                        question, var.name))

# Q3.27
question.id <- "Q3.27"
var.name <- "Q3.27"
question <- "Does a member of the government or ruling party of your country sit on your board?"

df <- bind_rows(df, generate.row.single(responses.full, question.id, 
                                        question, var.name))

# Q3.28
question.id <- "Q3.28"
var.name <- "Q3.28"
question <- "Is your organization required by law to have a member of the government or ruling party sit on your board?"

df <- bind_rows(df, generate.row.single(responses.full, question.id, 
                                        question, var.name))

# Q3.29
question.id <- "Q3.29"
var.name <- "Q3.29"
question <- "How much is your organization’s work restricted by government regulations in your country?"

df <- bind_rows(df, generate.row.single(responses.full, question.id, 
                                        question, var.name))

# Q4.1
question.id <- "Q4.1"
var.name <- "Q4.1"
question <- "Do you have any additional comments?"

df <- bind_rows(df, generate.row.text(responses.full, question.id, 
                                      question, var.name))

# Save as Markdown table
cat(pandoc.table.return(slice(df, 2:n()), split.tables=Inf,
                        justify=c("left", "center", "left", "center"), 
                        style="multiline"), 
    file=file.path(PROJHOME, "manuscript", "tables", "summary.md"))
