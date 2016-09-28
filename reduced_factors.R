#This script accepts either the combined_raw.csv or the binary_scored.csv and collapses 
# coding of several subfactors (barriers) into the a summed barrier for analysis

#load tidyverse library
library(tidyverse)

#read in coded csv sheets
df <- read_csv("binary_scores.csv")



#For each coded question, create two new columns:
#qXX_ISSUE_Sum: Numeric sum of all coded responses in a super-catagory (e.g. faculty isues)
#qXX_ISSUE_reduced: Binary value, 1 (1 or more reponses in super-catagory),
#0 for no responses in super-catagory


#Q38_PREVENTING YOU
#df[7:12] - Faculty Issues
q31_1df <- df %>%
  mutate(q31_Faculty_issues_sum = rowSums(df[7:12])) %>%
  mutate(q31_Faculty_issues_reduced = ifelse(rowSums(df[7:12]) == 0,0,1))

#df[13:22] - Curriculum Issues
q31_2df <- df %>%
  mutate(q31_Curriculum_issues_sum = rowSums(df[13:22])) %>%
  mutate(q31_Curriculum_issues_reduced = ifelse(rowSums(df[13:22]) == 0,0,1))

#df[23:31] - Resources Issues
q31_3df <- df %>%
  mutate(q31_Resources_issues_sum = rowSums(df[23:31])) %>%
  mutate(q31_Resources_issues_reduced = ifelse(rowSums(df[23:31]) == 0,0,1))

#df[32:34] - Student Issues
q31_4df <- df %>%
  mutate(q31_Student_issues_sum = rowSums(df[32:34])) %>%
  mutate(q31_Student_issues_reduced = ifelse(rowSums(df[32:34]) == 0,0,1))

#df[35:36] - Facilities Issues
q31_5df <- df %>%
  mutate(q31_Facilities_issues_sum = rowSums(df[35:36])) %>%
  mutate(q31_Facilities_issues_reduced = ifelse(rowSums(df[35:36]) == 0,0,1))

#df[37:38] - Institutional Support
q31_6df <- df %>%
  mutate(q31_Institutional_issues_sum = rowSums(df[37:38])) %>%
  mutate(q31_Institutional_issues_reduced = ifelse(rowSums(df[37:38]) == 0,0,1))

#df[39] - State Restrictions
q31_7df <- df %>%
  mutate(q31_State_issues_sum = rowSums(df[39])) %>%
  mutate(q31_State_issues_reduced = ifelse(rowSums(df[39]) == 0,0,1))

#df[40] - Not Accredited
q31_8df <- df %>%
  mutate(q31_Accredited_issues_sum = rowSums(df[40])) %>%
  mutate(q31_Accredited_issues_reduced = ifelse(rowSums(df[40]) == 0,0,1))



df_supreeme <- bind_cols(df,
                         q31_1df[,189:190], 
                         q31_2df[,189:190],
                         q31_3df[,189:190],
                         q31_4df[,189:190],
                         q31_5df[,189:190],
                         q31_6df[,189:190],
                         q31_7df[,189:190],
                         q31_8df[,189:190]
                         )

write_csv(df_supreeme, "./sum_and_reduced_factors.csv")

#Q06_BARRIERS TO IMPLEMENTATION
#df[43:47] - Faculty Issues
#df[48:55] - Curriculum Issues
#df[56:58] - Student Issues
#df[59:63] - Institutional Support
#df[64:67] - Resource Issues
#df[68:70] - Facilities Issues

#df[43:70]


#Q29-30_TECHNICAL BARRIERS
#df[80:84] - Faculty Issues
#df[85:89] - Facilities Issues
#df[90:94] - Resources Issues
#df[95:98] - Institutional Support
#df[99:103] - Student Issues
#df[104:107] - Curriculum Issues

#df[80:107]


#Q33_IMPORTANT CHALLENGES
#df[124:129] - Faculty Issues
#df[130:132] - Facility Issues
#df[133:136] - Resources Issues
#df[137:143] - Student Issues
#df[144:150] - Curriculum Issues
#df[151:153] - Institutional Support

#df[124:153]


#Q41_STUDENT DEFICIENCIES 
#df[156:182]

#df[156:182]





#coded columns (excludes text responses) are columns 

#Q38_PREVENTING YOU

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
