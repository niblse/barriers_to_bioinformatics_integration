#This script accepts either the combined_raw.csv or the binary_scored.csv and collapses 
# coding of several subfactors (barriers) into the a summed barrier for analysis

#load tidyverse library
library(tidyverse)

#read in coded csv sheets
df <- read_csv("binary_scores.csv")

#create a new dataframe to add append to column-by-column

#df_tmp <- df[1:23]


#coded columns (excludes text responses) are columns 

#PREVENTING YOU
#add question row
#df_tmp[24] <- df[24]

#df[25:30] - Faculty Issues
df %>%
  mutate(FACULTY_ISSUES_0 = rowSums(df[25:30])) -> df

#df[31:40] - Curriculum Issues
df %>%
  mutate(CURRICULUM_ISSUES_0 = rowSums(df[31:40])) -> df

#df[41:49] - Resources Issues
df %>%
  mutate(RESOURCE_ISSUES_0 = rowSums(df[41:49])) -> df

#df[50:52] - Student Issues
df %>%
  mutate(STUDENT_ISSUES_0 = rowSums(df[50:52])) -> df

#df[53:54] - Facilities Issues
df %>%
  mutate(FACILITY_ISSUES_0 = rowSums(df[53:54])) -> df

#df[55:56] - Institutional Support
df %>%
  mutate(INSTIUTIONAL_ISSUES_0 = rowSums(df[55:56])) -> df

#df[57] - State Restrictions
df %>%
  mutate(STATE_ISSUES_0 = rowSums(df[57])) -> df

#df[58] - Accreditation
df %>%
  mutate(ACCREDITATION_ISSUES_0 = rowSums(df[58])) -> df



#BARRIERS TO IMPLEMENTATION
#df[60:64] - Faculty Issues
df %>%
  mutate(FACULTY_ISSUES_1 = rowSums(df[60:64])) -> df

#df[65:72] - Curriculum Issues
df %>%
  mutate(CURRICULUM_ISSUES_1 = rowSums(df[65:72])) -> df

#df[73:75] - Student Issues
df %>%
  mutate(STUDENT_ISSUES_1 = rowSums(df[73:75])) -> df

#df[76:80] - Institutional Support
df %>%
  mutate(INSTITUTIONAL_ISSUES_1 = rowSums(df[76:80])) -> df

#df[81:84] - Resource Issues
df %>%
  mutate(RESOURCE_ISSUES_1 = rowSums(df[81:84])) -> df

#df[85:87] - Facilities Issues
df %>%
  mutate(FACILITIES_ISSUES_1 = rowSums(df[85:87])) -> df




#TECHNICAL BARRIERS
#df[90:94] - Faculty Issues
df %>%
  mutate(FACULTY_ISSUES_2 = rowSums(df[90:94])) -> df

#df[95:99] - Facilities Issues
df %>%
  mutate(FACILITIES_ISSUES_2 = rowSums(df[95:99])) -> df

#df[100:104] - Resources Issues
df %>%
  mutate(RESOURCE_ISSUES_2 = rowSums(df[100:104])) -> df

#df[105:108] - Institutional Support
df %>%
  mutate(INSTITUTIONAL_ISSUES_2 = rowSums(df[105:108])) -> df

#df[109:113] - Student Issues
df %>%
  mutate(STUDENT_ISSUES_2 = rowSums(df[109:113])) -> df

#df[114:117] - Curriculum Issues
df %>%
  mutate(CURRICULUM_ISSUES_2 = rowSums(df[114:117])) -> df




#IMPORTANT CHALLENGES
#df[119:124] - Faculty Issues
df %>%
  mutate(FACULTY_ISSUES_3 = rowSums(df[119:124])) -> df

#df[125:127] - Facility Issues
df %>%
  mutate(FACILITY_ISSUES_3 = rowSums(df[125:127])) -> df

#df[128:131] - Resources Issues
df %>%
  mutate(RESOURCES_ISSUES_3 = rowSums(df[128:131])) -> df

#df[132:138] - Student Issues
df %>%
  mutate(STUDENT_ISSUES_3 = rowSums(df[132:138])) -> df

#df[139:145] - Curriculum Issues
df %>%
  mutate(CURRICULUM_ISSUES_3 = rowSums(df[139:145])) -> df

#df[146:148] - Institutional Support
df %>%
  mutate(INSTITUTIONAL_ISSUES_3 = rowSums(df[146:148])) -> df


#STUDENT DEFICIENCIES 
#df[150:176]
df %>%
  mutate(STUDENT_DEFFICIENCIES_ISSUES_4 = rowSums(df[150:176])) -> df

write_csv(df, "./reduced_factors.csv")
