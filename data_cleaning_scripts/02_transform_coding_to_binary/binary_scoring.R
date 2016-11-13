#this script takes the "combined_raw_df.csv" created by the combined_coded_csvs.R script and
# replaces NA in the scored/coded responses with 0
# ensures scored/coded responses are numeric

#load tidyverse library
library(tidyverse)

#read in coded csv sheets
df <- read_csv("../01_combine_excel_outputs_to_csv/output/combined_raw_df.csv")


#coded columns (excludes text responses) are columns 

#PREVENTING YOU
df[,7:40][is.na(df[,7:40])]<-0

#BARRIERS TO IMPLEMENTATION
df[,43:70][is.na(df[,43:70])]<-0

#TECHNICAL BARRIERS
df[,80:107][is.na(df[,80:107])]<-0

#IMPORTANT CHALLENGES
df[,124:153][is.na(df[,124:153])]<-0

#STUDENT DEFICIENCIES 
df[,156:182][is.na(df[,156:182])]<-0

#save binary scored  df in csv format
dir.create("./output/", recursive = TRUE)
write_csv(df, "./output/binary_scores.csv")




