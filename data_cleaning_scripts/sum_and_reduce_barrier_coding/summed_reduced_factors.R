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
#These sums eliminate any column indicated as "General" as this is already a super-catagory



#Q38_PREVENTING YOU
#df[8:12] - Faculty Issues
q38_1df <- df %>%
  mutate(q38_Faculty_issues_sum = rowSums(df[8:12])) %>%
  mutate(q38_Faculty_issues_reduced = ifelse(rowSums(df[8:12]) == 0,0,1))

#df[14:22] - Curriculum Issues
q38_2df <- df %>%
  mutate(q38_Curriculum_issues_sum = rowSums(df[14:22])) %>%
  mutate(q38_Curriculum_issues_reduced = ifelse(rowSums(df[14:22]) == 0,0,1))

#df[24:31] - Resources Issues
q38_3df <- df %>%
  mutate(q38_Resources_issues_sum = rowSums(df[24:31])) %>%
  mutate(q38_Resources_issues_reduced = ifelse(rowSums(df[24:31]) == 0,0,1))

#df[33:34] - Student Issues
q38_4df <- df %>%
  mutate(q38_Student_issues_sum = rowSums(df[33:34])) %>%
  mutate(q38_Student_issues_reduced = ifelse(rowSums(df[33:34]) == 0,0,1))

#df[36:36] - Facilities Issues
q38_5df <- df %>%
  mutate(q38_Facilities_issues_sum = rowSums(df[36:36])) %>%
  mutate(q38_Facilities_issues_reduced = ifelse(rowSums(df[36:36]) == 0,0,1))

#df[38:38] - Institutional Support
q38_6df <- df %>%
  mutate(q38_Institutional_issues_sum = rowSums(df[38:38])) %>%
  mutate(q38_Institutional_issues_reduced = ifelse(rowSums(df[38:38]) == 0,0,1))

#df[39] - State Restrictions
q38_7df <- df %>%
  mutate(q38_State_issues_sum = rowSums(df[39])) %>%
  mutate(q38_State_issues_reduced = ifelse(rowSums(df[39]) == 0,0,1))

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
#df[44:47] - Faculty Issues
q06_1df <- df %>%
  mutate(q06_Faculty_issues_sum = rowSums(df[44:47])) %>%
  mutate(q06_Faculty_issues_reduced = ifelse(rowSums(df[44:47]) == 0,0,1))

#df[49:55] - Curriculum Issues
q06_2df <- df %>%
  mutate(q06_Curriculum_issues_sum = rowSums(df[49:55])) %>%
  mutate(q06_Curriculum_issues_reduced = ifelse(rowSums(df[49:55]) == 0,0,1))

#df[57:58] - Student Issues
q06_3df <- df %>%
  mutate(q06_Student_issues_sum = rowSums(df[57:58])) %>%
  mutate(q06_Student_issues_reduced = ifelse(rowSums(df[57:58]) == 0,0,1))

#df[60:63] - Institutional Support
q06_4df <- df %>%
  mutate(q06_Institutional_issues_sum = rowSums(df[60:63])) %>%
  mutate(q06_Institutional_issues_reduced = ifelse(rowSums(df[60:63]) == 0,0,1))

#df[65:67] - Resource Issues
q06_5df <- df %>%
  mutate(q06_Resource_issues_sum = rowSums(df[65:67])) %>%
  mutate(q06_Resource_issues_reduced = ifelse(rowSums(df[65:67]) == 0,0,1))

#df[69:70] - Facilities Issues
q06_6df <- df %>%
  mutate(q06_Facilities_issues_sum = rowSums(df[69:70])) %>%
  mutate(q06_Facilities_issues_reduced = ifelse(rowSums(df[69:70]) == 0,0,1))

q06_cols <- bind_cols( q06_1df[,189:190],
                       q06_2df[,189:190],
                       q06_3df[,189:190],
                       q06_4df[,189:190],
                       q06_5df[,189:190],
                       q06_6df[,189:190]
                       )
                       


#Q29-30_TECHNICAL BARRIERS
#df[81:84] - Faculty Issues
q29_30_1df <- df %>%
  mutate(q29_Faculty_issues_sum = rowSums(df[81:84])) %>%
  mutate(q29_Faculty_issues_reduced = ifelse(rowSums(df[81:84]) == 0,0,1))

#df[86:89] - Facilities Issues
q29_30_2df <- df %>%
  mutate(q29_Facilities_issues_sum = rowSums(df[86:89])) %>%
  mutate(q29_Facilities_issues_reduced = ifelse(rowSums(df[86:89]) == 0,0,1))

#df[91:94] - Resources Issues
q29_30_3df <- df %>%
  mutate(q29_Resources_issues_sum = rowSums(df[91:94])) %>%
  mutate(q29_Resources_issues_reduced = ifelse(rowSums(df[91:94]) == 0,0,1))

#df[96:98] - Institutional Support
q29_30_4df <- df %>%
  mutate(q29_Institutional_issues_sum = rowSums(df[96:98])) %>%
  mutate(q29_Institutional_issues_reduced = ifelse(rowSums(df[96:98]) == 0,0,1))

#df[100:103] - Student Issues
q29_30_5df <- df %>%
  mutate(q29_Student_issues_sum = rowSums(df[100:103])) %>%
  mutate(q29_Student_issues_reduced = ifelse(rowSums(df[100:103]) == 0,0,1))

#df[105:107] - Curriculum Issues
q29_30_6df <- df %>%
  mutate(q29_Curriculum_issues_sum = rowSums(df[105:107])) %>%
  mutate(q29_Curriculum_issues_reduced = ifelse(rowSums(df[105:107]) == 0,0,1))

q29_cols <- bind_cols( q29_30_1df[,189:190],
                       q29_30_2df[,189:190],
                       q29_30_3df[,189:190],
                       q29_30_4df[,189:190],
                       q29_30_5df[,189:190],
                       q29_30_6df[,189:190]
                       )


#Q33_IMPORTANT CHALLENGES
#df[125:129] - Faculty Issues
q33_1df <- df %>%
  mutate(q33_Faculty_issues_sum = rowSums(df[125:129])) %>%
  mutate(q33_Faculty_issues_reduced = ifelse(rowSums(df[125:129]) == 0,0,1))

#df[131:132] - Facility Issues
q33_2df <- df %>%
  mutate(q33_Facility_issues_sum = rowSums(df[131:132])) %>%
  mutate(q33_Facility_issues_reduced = ifelse(rowSums(df[131:132]) == 0,0,1))

#df[134:136] - Resources Issues
q33_3df <- df %>%
  mutate(q33_Resources_issues_sum = rowSums(df[134:136])) %>%
  mutate(q33_Resources_issues_reduced = ifelse(rowSums(df[134:136]) == 0,0,1))

#df[138:143] - Student Issues
q33_4df <- df %>%
  mutate(q33_Student_issues_sum = rowSums(df[138:143])) %>%
  mutate(q33_Student_issues_reduced = ifelse(rowSums(df[138:143]) == 0,0,1))

#df[145:150] - Curriculum Issues
q33_5df <- df %>%
  mutate(q33_Curriculum_issues_sum = rowSums(df[145:150])) %>%
  mutate(q33_Curriculum_issues_reduced = ifelse(rowSums(df[145:150]) == 0,0,1))

#df[152:153] - Institutional Support
q33_6df <- df %>%
  mutate(q33_Institutional_issues_sum = rowSums(df[152:153])) %>%
  mutate(q33_Institutional_issues_reduced = ifelse(rowSums(df[152:153]) == 0,0,1))


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
