#this script takes the "combined_raw_df.csv" created by the combined_coded_csvs.R script and
# replaces NA in the scored/coded responses with 0
# ensures scored/coded responses are numeric

#load tidyverse library
library(tidyverse)

#read in coded csv sheets
df <- read_csv("combined_raw_df.csv")


#coded columns (excludes text responses) are columns 

#PREVENTING YOU
#df[25:30] - Faculty Issues
#df[31:40] - Curriculum Issues
#df[41:49] - Resources Issues
#df[50:52] - Student Issues
#df[53:54] - Facilities Issues
#df[55:56] - Institutional Support
#df[57] - State Restrictions
#df[58] - Not Accredited

#df[25:58]
df[,25:58][is.na(df[,25:58])]<-0

#BARRIERS TO IMPLEMENTATION
#df[60:64] - Faculty Issues
#df[65:72] - Curriculum Issues
#df[73:75] - Student Issues
#df[76:80] - Institutional Support
#df[81:84] - Resource Issues
#df[85:87] - Facilities Issues

#df[60:87]
df[,60:87][is.na(df[,60:87])]<-0

#TECHNICAL BARRIERS
#df[90:94] - Faculty Issues
#df[95:99] - Facilities Issues
#df[100:104] - Resources Issues
#df[105:108] - Institutional Support
#df[109:113] - Student Issues
#df[114:117] - Curriculum Issues

#df[90:117]
df[,90:117][is.na(df[,90:117])]<-0

#IMPORTANT CHALLENGES
#df[119:124] - Faculty Issues
#df[125:127] - Facility Issues
#df[128:131] - Resources Issues
#df[132:138] - Student Issues
#df[139:145] - Curriculum Issues
#df[146:148] - Institutional Support

#df[119:148]
df[,119:148][is.na(df[,119:148])]<-0

#STUDENT DEFICIENCIES 
#df[150:176]

#df[150:176]
df[,150:176][is.na(df[,150:176])]<-0

#save binary scored  df in csv format
write_csv(df, "./binary_scores.csv")




