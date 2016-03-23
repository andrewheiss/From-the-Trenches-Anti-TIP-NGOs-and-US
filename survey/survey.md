# Anti-TIP NGO survey

![](tip_banner.png)

We would like to ask you some questions about your organization and its work with human trafficking issues over the past 10 years. This survey is part of a study by Prof. Judith Kelley from Duke University, USA. The study is about the global fight against human trafficking. Your participation is entirely voluntary. Results from the survey will only be used in aggregate so that it will not be possible to identify any individual organization. You may choose to keep your answers anonymous if you wish. As part of the research effort we have compiled a database of NGOs all over the world working on human trafficking. In exchange for your participation, we will update your organization's data and share the link to the database with you.

If you have any questions, you will be given contact information for Prof. Kelley.

Please feel free to respond in your own language when possible.

---

## Background questions

**Q1.2** What is the name of your organization?

*Free response text field*

**Q1.3** Would you like your organization to remain anonymous in this study's reports?

* No
* Yes

**Q1.4** Where is your organization's main office?

*Drop-down list of countries*

**Q1.5** In how many countries has your organization done most of its advocacy work over the past 10 years?

*Numeric field allowing for 1, 2, 3, 4, or 5*

---


## Organizational questions

Please answer the following questions about your organization's current human trafficking efforts. If your organization no longer works with human trafficking issues, please answer the questions about any of your organization's efforts in trafficking issues over the past 10 years.

**Q2.1** About what percent of your organization's time and resources are spent on fighting against trafficking or helping victims of trafficking?

*Numeric slider ranging from 0–100*

**Q2.2** Which human trafficking issues is your organization most involved with? *(Check all that apply)*

* Organ trafficking
* Sex trafficking
* Labor trafficking
* Other: __________

**Q2.3** Which kinds of victims is your organization most involved with? *(Check all that apply)*

* Children
* Adults
* Other: __________

**Q2.4** Which efforts does your organization focus on most? *(Check all that apply)*

* Prevention and education
* Prosecutions and legal issues
* Victim protection
* Victim assistance
* Other: __________

**Q2.5** The US State Department issues an annual Trafficking in Persons (TIP) report. Have you ever heard of this annual report?

* No
* Yes

**Q2.6** In Fall 2013 an NGO named WalkFree published a "Human Trafficking Index" that rated countries on how many trafficking victims they have. Have you heard of this index?

* No
* Yes

---

## Country-specific questions

We will now ask you a series of questions related to {the country your organization works in the most}[^first_time] / {other countries your organization has worked in}[^other_times].

Please answer the following questions about your organization's current human trafficking efforts. If your organization no longer works with human trafficking issues, please answer the questions about any of your organization's efforts in trafficking issues over the past 10 years.

**Q3.1**[^other_times] We would like to ask you more questions related to the other countries you work in. Would you like to answer additional questions for another country?

* No
* Yes

(`if Q3.1 == 'No' goto Q4.1 else goto Q3.2`)

---

**Q3.2** Where {else}[^other_times] has your organization done advocacy work {the most}[^first_time]?

*Drop-down list of countries*

---

**Q3.3** How much does your organization know about human trafficking policy in {country_name}?

* None
* Very little
* Little
* Some
* A lot
* Don't know

**Q3.4** How often does your organization work directly with the government of {country_name} on human trafficking problems?

* Never
* Rarely
* Sometimes
* Monthly
* Weekly
* Daily
* Don't know

**Q3.5** In {country_name}, which of these institutions have been active in fighting human trafficking over the last 10–15 years? *(Check all that apply)*

* The national government
* NGOs and civil society organizations like your own
* Embassies or foreign governments
* International organizations (such as the International Labor Organization, the International Organization on Migration, regional organizations, the UNDP, or others?)
* Other: __________

**Q3.6** Which embassies or foreign governments have been active in fighting human trafficking in {country_name}? (If none, leave blank)

*Free response text field*

**Q3.7** Which of these embassies or foreign governments have been the most active? (If none, leave blank)

*Free response text field*

---

(`if Q3.2 != 'United States'`)  
**Q3.8** Over the last 10–15 years, has the United States or its embassy been active in the fight against human trafficking in {country_name}?

* No
* Yes
* Don't know

(`if Q3.2 != 'United States'`)  
**Q3.9** Has the United States or its embassy been involved in any of the following activities in {country_name}? *(Check all that apply)*

* Asking the government to pass or amend anti-trafficking laws
* Convening conferences or workshops on trafficking
* Raising awareness about trafficking
* Providing resources or funding for anti-trafficking programs
* Increasing government attention to trafficking
* Training of government officials (including police or security)
* Contributing to a government action plan
* Other
* Don't know
* The United States has not been involved in trafficking issues

**Q3.10** Please explain how the United States asked the government to pass or amend anti-trafficking laws in {country_name}: *(Feel free to respond in your own language)*

*Free response text field*

**Q3.11** Please explain how the United States convened conferences or workshops on trafficking in {country_name}: *(Feel free to respond in your own language)*

*Free response text field*

**Q3.12** Please explain how the United States raised awareness about trafficking in {country_name}: *(Feel free to respond in your own language)*

*Free response text field*

**Q3.13** Please explain how the United States provided resources or funding for anti-trafficking programs in {country_name}: *(Feel free to respond in your own language)*

*Free response text field*

**Q3.14** Please explain how the United States increased government attention to trafficking in {country_name}: *(Feel free to respond in your own language)*

*Free response text field*

**Q3.15** Please explain how the United States trained government officials in {country_name}: *(Feel free to respond in your own language)*

*Free response text field*

**Q3.16** Please explain how the United States contributed to a government action plan in {country_name}: *(Feel free to respond in your own language)*

*Free response text field*

**Q3.17** Please explain how else the US government has been involved in trafficking issues in {country_name}: *(Feel free to respond in your own language)*

*Free response text field*

---

(`if Q3.2 != 'United States'`)  
**Q3.18** Over the last 10–15 years, has your organization worked directly with or had direct contact with the US embassy or government on human trafficking issues? *(Check all that apply)*

* Direct contact (meetings, other communication)
* Direct cooperation
* Our organization received funding
* Other: __________
* We have not had any contact or funding from the US or the US embassy
* Don't know

(`if Q3.2 != 'United States'`)  
**Q3.19** Overall, how important a role would you say that the United States or its embassy have played in fighting trafficking in {country_name} over the last 10–15 years?

* The United States has been one of the most important actors
* The United States has been a somewhat important actor
* The United States has not been an important actor
* Don't know

---

**Q3.20** In your view, how hard is the government of {country_name} working to combat trafficking in persons?

* Not hard at all
* Not too hard
* Somewhat hard
* Very hard
* Extremely hard
* Don't know

(`if Q2.5 == 'Yes'`)  
**Q3.21** Has your organization used the US State Department's Trafficking in Persons (TIP) report to discuss trafficking issues with any of these groups? *(Check all that apply)*

* Government of {country_name}
* Another country's government
* Other NGOs
* Other: __________

(`if Q2.5 == 'Yes'`)  
**Q3.22** Which TIP tier rating did {country_name} receive this year?

* Tier 1
* Tier 2
* Watch list
* Tier 3
* Don't know

(`if Q2.5 == 'Yes'`)  
**Q3.23** Have you ever heard—in public or private—officials in {country_name} mention the TIP tier rating?

* No
* Yes

(`if Q3.23 == 'Yes'`)  
**Q3.24** What was their reason for mentioning it? *(Feel free to respond in your own language)*

*Free response text field*

(`if Q3.19 == 'The United States has been one of the most important actors' or Q3.19 == 'The United States has been a somewhat important actor'`)  
**Q3.25** Overall, has the US influence on human trafficking policy in {country_name} been positive or negative?

* Negative
* Positive
* Mixed
* Don't know

---

**Q3.26** Would you say that the government of {country_name}'s efforts to combat trafficking over the past 10–15 years have:

* Improved
* Remained constant
* Slowed down
* Don't know

**Q3.27** Does a member of the government or ruling party of {country_name} sit on your board?

* No
* Yes
* Don't know

(`if Q3.27 == 'Yes'`)  
**Q3.28** Is your organization required by law to have a member of the government or ruling party sit on your board?

* No
* Yes
* Don't know

**Q3.29** How much is your organization's work restricted by government regulations in {country_name}?

* Not restricted at all
* Very little restricted
* A little restricted
* Somewhat restricted
* Very restricted
* Don't know

(`if Q3.29 == 'A little restricted' or Q3.29 == 'Somewhat restricted' or Q3.29 == 'Very restricted'`)  
**Q3.30** How is your organization's work restricted by government regulations in {country_name}? *(Feel free to respond in your own language)*

*Free response text field*

---

## Final questions

**Q4.1** Do you have any additional comments? *(Feel free to respond in your own language)*

*Free response text field*

---

**Q4.2** May we contact you for any follow up questions? This would be very helpful for us.

* No
* Yes

**Q4.3** Would you liked to be notified of the results of this survey once it is completed?

* No
* Yes

`(if Q4.2 == 'Yes' or Q4.3 == 'Yes')`  
**Q4.4** Please provide an e-mail address we can use to contact you:

`(if Q4.2 == 'Yes' or Q4.3 == 'Yes')`  
**Q4.5** Please provide a phone number we can use to contact you (optional):

---

Thank you for participating in this research effort. Should you have any questions or concerns about this survey, please feel free to contact Prof. Kelley at {email_address} or telephone at {phone_number}.


[^first_time]: Shown the first time through the set of country-specific questions.

[^other_times]: Shown in all subsequent loops of country-specific questions.
