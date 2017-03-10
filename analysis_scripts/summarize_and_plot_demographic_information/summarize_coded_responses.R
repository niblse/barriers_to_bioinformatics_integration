#summarize respondents in reduced barrier categories

#some attempts at plots
require(ggplot2)
require(tidyverse)

#load dataframe and create directories
dir.create("./output_plots", recursive = TRUE)
dir.create("./output_tables", recursive = TRUE)
df <- read_csv("../../data_cleaning_scripts/04_decode_survey_responses/output/decoded_df.csv")

# remove any non-US respondents
countries <- c("United States","Puerto Rico")
df <- df%>%
  filter(Country_Country %in% countries )

#load reduced barrier columns for each question


data.relavant  <- df %>%
  select(q06_Faculty_issues_reduced,
         q06_Curriculum_issues_reduced,
         q06_Student_issues_reduced,
         q06_Institutional_issues_reduced,
         q06_Resource_issues_reduced,
         q06_Facilities_issues_reduced,
         q33_Curriculum_issues_reduced,
         q33_Faculty_issues_reduced,
         q33_Facility_issues_reduced,
         q33_Resources_issues_reduced,
         q33_Student_issues_reduced,
         q33_Institutional_issues_reduced,
         q29_Faculty_issues_reduced, 
         q29_Facilities_issues_reduced, 
         q29_Resources_issues_reduced,
         q29_Institutional_issues_reduced, 
         q29_Student_issues_reduced, 
         q29_Curriculum_issues_reduced, 
         q38_Faculty_issues_reduced, 
         q38_Curriculum_issues_reduced, 
         q38_Resources_issues_reduced, 
         q38_Student_issues_reduced, 
         q38_Facilities_issues_reduced, 
         q38_Institutional_issues_reduced, 
         q38_State_issues_reduced, 
         q38_Accredited_issues_reduced)

sum.table <- as.data.frame(colSums(data.relavant))
sum.table <- t(sum.table)
sum.table <- as.data.frame(sum.table)
write_csv(sum.table, "./output_tables/reduced_questions_sums.csv")
