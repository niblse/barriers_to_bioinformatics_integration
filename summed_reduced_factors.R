#This script accepts the binary_scored.csv and collapses 
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
q38_1df <- df %>%
  mutate(q38_Faculty_issues_sum = rowSums(df[7:12])) %>%
  mutate(q38_Faculty_issues_reduced = ifelse(rowSums(df[7:12]) == 0,0,1))

#df[13:22] - Curriculum Issues
q38_2df <- df %>%
  mutate(q38_Curriculum_issues_sum = rowSums(df[13:22])) %>%
  mutate(q38_Curriculum_issues_reduced = ifelse(rowSums(df[13:22]) == 0,0,1))

#df[23:31] - Resources Issues
q38_3df <- df %>%
  mutate(q38_Resources_issues_sum = rowSums(df[23:31])) %>%
  mutate(q38_Resources_issues_reduced = ifelse(rowSums(df[23:31]) == 0,0,1))

#df[32:34] - Student Issues
q38_4df <- df %>%
  mutate(q38_Student_issues_sum = rowSums(df[32:34])) %>%
  mutate(q38_Student_issues_reduced = ifelse(rowSums(df[32:34]) == 0,0,1))

#df[35:36] - Facilities Issues
q38_5df <- df %>%
  mutate(q38_Facilities_issues_sum = rowSums(df[35:36])) %>%
  mutate(q38_Facilities_issues_reduced = ifelse(rowSums(df[35:36]) == 0,0,1))

#df[37:38] - Institutional Support
q38_6df <- df %>%
  mutate(q38_Institutional_issues_sum = rowSums(df[37:38])) %>%
  mutate(q38_Institutional_issues_reduced = ifelse(rowSums(df[37:38]) == 0,0,1))

#df[39] - State Restrictions
q38_7df <- df %>%
  mutate(q38_State_issues_sum = rowSums(df[39])) %>%
  mutate(q31_State_issues_reduced = ifelse(rowSums(df[39]) == 0,0,1))

#df[40] - Not Accredited
q38_8df <- df %>%
  mutate(q38_Accredited_issues_sum = rowSums(df[40])) %>%
  mutate(q38_Accredited_issues_reduced = ifelse(rowSums(df[40]) == 0,0,1))



q38_cols <- bind_cols( q38_1df[,189:190],
                       q38_2df[,189:190],
                       q38_3df[,189:190],
                       q38_4df[,189:190],
                       q38_5df[,189:190],
                       q38_6df[,189:190],
                       q38_7df[,189:190],
                       q38_8df[,189:190]
                       )

#Q06_BARRIERS TO IMPLEMENTATION
#df[43:47] - Faculty Issues
q06_1df <- df %>%
  mutate(q06_Faculty_issues_sum = rowSums(df[43:47])) %>%
  mutate(q06_Faculty_issues_reduced = ifelse(rowSums(df[43:47]) == 0,0,1))

#df[48:55] - Curriculum Issues
q06_2df <- df %>%
  mutate(q06_Curriculum_issues_sum = rowSums(df[48:55])) %>%
  mutate(q06_Curriculum_issues_reduced = ifelse(rowSums(df[48:55]) == 0,0,1))

#df[56:58] - Student Issues
q06_3df <- df %>%
  mutate(q06_Student_issues_sum = rowSums(df[56:58])) %>%
  mutate(q06_Student_issues_reduced = ifelse(rowSums(df[56:58]) == 0,0,1))

#df[59:63] - Institutional Support
q06_4df <- df %>%
  mutate(q06_Institutional_issues_sum = rowSums(df[59:63])) %>%
  mutate(q06_Institutional_issues_reduced = ifelse(rowSums(df[59:63]) == 0,0,1))

#df[64:67] - Resource Issues
q06_5df <- df %>%
  mutate(q06_Resource_issues_sum = rowSums(df[64:67])) %>%
  mutate(q06_Resource_issues_reduced = ifelse(rowSums(df[64:67]) == 0,0,1))

#df[68:70] - Facilities Issues
q06_6df <- df %>%
  mutate(q06_Facilities_issues_sum = rowSums(df[68:70])) %>%
  mutate(q06_Facilities_issues_reduced = ifelse(rowSums(df[68:70]) == 0,0,1))

q06_cols <- bind_cols( q06_1df[,189:190],
                       q06_2df[,189:190],
                       q06_3df[,189:190],
                       q06_4df[,189:190],
                       q06_5df[,189:190],
                       q06_6df[,189:190]
                       )
                       


#Q29-30_TECHNICAL BARRIERS
#df[80:84] - Faculty Issues
q29_30_1df <- df %>%
  mutate(q29_Faculty_issues_sum = rowSums(df[80:84])) %>%
  mutate(q29_Faculty_issues_reduced = ifelse(rowSums(df[80:84]) == 0,0,1))

#df[85:89] - Facilities Issues
q29_30_2df <- df %>%
  mutate(q29_Facilities_issues_sum = rowSums(df[85:89])) %>%
  mutate(q29_Facilities_issues_reduced = ifelse(rowSums(df[85:89]) == 0,0,1))

#df[90:94] - Resources Issues
q29_30_3df <- df %>%
  mutate(q29_Resources_issues_sum = rowSums(df[90:94])) %>%
  mutate(q29_Resources_issues_reduced = ifelse(rowSums(df[90:94]) == 0,0,1))

#df[95:98] - Institutional Support
q29_30_4df <- df %>%
  mutate(q29_Institutional_issues_sum = rowSums(df[95:98])) %>%
  mutate(q29_Institutional_issues_reduced = ifelse(rowSums(df[95:98]) == 0,0,1))

#df[99:103] - Student Issues
q29_30_5df <- df %>%
  mutate(q29_Student_issues_sum = rowSums(df[99:103])) %>%
  mutate(q29_Student_issues_reduced = ifelse(rowSums(df[99:103]) == 0,0,1))

#df[104:107] - Curriculum Issues
q29_30_6df <- df %>%
  mutate(q29_Curriculum_issues_sum = rowSums(df[104:107])) %>%
  mutate(q29_Curriculum_issues_reduced = ifelse(rowSums(df[104:107]) == 0,0,1))

q29_cols <- bind_cols( q29_30_1df[,189:190],
                       q29_30_2df[,189:190],
                       q29_30_3df[,189:190],
                       q29_30_4df[,189:190],
                       q29_30_5df[,189:190],
                       q29_30_6df[,189:190]
                       )


#Q33_IMPORTANT CHALLENGES
#df[124:129] - Faculty Issues
q33_1df <- df %>%
  mutate(q33_Faculty_issues_sum = rowSums(df[124:129])) %>%
  mutate(q33_Faculty_issues_reduced = ifelse(rowSums(df[124:129]) == 0,0,1))

#df[130:132] - Facility Issues
q33_2df <- df %>%
  mutate(q33_Facility_issues_sum = rowSums(df[130:132])) %>%
  mutate(q33_Facility_issues_reduced = ifelse(rowSums(df[130:132]) == 0,0,1))

#df[133:136] - Resources Issues
q33_3df <- df %>%
  mutate(q33_Resources_issues_sum = rowSums(df[133:136])) %>%
  mutate(q33_Resources_issues_reduced = ifelse(rowSums(df[133:136]) == 0,0,1))

#df[137:143] - Student Issues
q33_4df <- df %>%
  mutate(q33_Student_issues_sum = rowSums(df[137:143])) %>%
  mutate(q33_Student_issues_reduced = ifelse(rowSums(df[137:143]) == 0,0,1))

#df[144:150] - Curriculum Issues
q33_5df <- df %>%
  mutate(q33_Curriculum_issues_sum = rowSums(df[144:150])) %>%
  mutate(q33_Curriculum_issues_reduced = ifelse(rowSums(df[144:150]) == 0,0,1))

#df[151:153] - Institutional Support
q33_6df <- df %>%
  mutate(q33_Institutional_issues_sum = rowSums(df[151:153])) %>%
  mutate(q33_Institutional_issues_reduced = ifelse(rowSums(df[151:153]) == 0,0,1))


q33_cols <- bind_cols( q33_1df[,189:190],
                       q33_2df[,189:190],
                       q33_3df[,189:190],
                       q33_4df[,189:190],
                       q33_5df[,189:190],
                       q33_6df[,189:190]
                       )


#Q41_STUDENT DEFICIENCIES 
#df[156:182]
q41_1df <- df %>%
  mutate(q41_Student_deficiencies_sum = rowSums(df[156:182])) %>%
  mutate(q41_Student_deficiencies_reduced = ifelse(rowSums(df[156:182]) == 0,0,1))

q41_cols <- bind_cols( q41_1df[,189:190])

#Create final dataframe from df, each scored qustion's summed and reduced columns
#appear after the question, followed by the scored sub-catagories


summed_reduced_df <- bind_cols(df[1:6],
                               q38_cols,
                               df[7:42],
                               q06_cols,
                               df[43:79],
                               q29_cols,
                               df[80:123],
                               q33_cols,
                               df[124:155],
                               q41_cols,
                               df[156:188]
                               )

#write to file
write_csv(summed_reduced_df, "./summed_reduced_factors.csv")
