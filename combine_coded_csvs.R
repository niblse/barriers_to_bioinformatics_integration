#this script combines the coded survey results and combines generated in excel 
#and generates a combined R dataframe

#load tidyverse library
library(tidyverse)

#read in master sheet to generate headers
master <- read.csv("./master.csv",header = FALSE, stringsAsFactors = FALSE)

#get row 1 headers
row_1_header <- as.vector(master[1,], mode = "character") 
row_2_header <-as.vector(master[2,], mode = "character") 
df0_header <- paste(row_1_header,row_2_header, sep="_")

#read in master excel sheets as df0
# skip the first 2 lines (use 2nd line as column name)
# master.csv has some null values, replace this with NA
df0 <- read_csv("./master.csv", skip = 2, na = "#NULL!", col_names = df0_header)

#read in master excel sheets
# skip the first 2 lines (use 2nd line as column name)
# master.csv has some null values, replace this with NA

df1 <- read_csv("./01_preventing_you.csv", skip = 1, na = c("#NULL!","") )      #Q38
df2 <- read_csv("./02_barriers_dev_imp.csv", skip = 1, na = c("#NULL!",""))     #Q06
df3 <- read_csv("./03_technical_barriers.csv", skip = 1, na = c("#NULL!",""))   #Q29-30
df4 <- read_csv("./04_important_challenges.csv", skip = 1, na = c("#NULL!","")) #Q33
df5 <- read_csv("./05_student_deficiencies.csv", skip = 1, na = c("#NULL!","")) #Q41

#clean extranous rows from dfs
df1 <- select(df1, 1:45)
df3 <- select(df3, 1:40)
df4 <- select(df4, 1:41)
df5 <- select(df5, 1:38)

#Improve column names for imported coded dataframes by adding question numbers

#Q38
Q38_header <- colnames(df1) 
Q38_string <- replicate(45, "Q38", simplify = "vector")
Q38_header_new <- paste(Q38_string,Q38_header, sep="_")
colnames(df1) <- Q38_header_new

#Q06
Q06_header <- colnames(df2) 
Q06_string <- replicate(39, "Q06", simplify = "vector")
Q06_header_new <- paste(Q06_string,Q06_header, sep="_")
colnames(df2) <- Q06_header_new

#Q29-30
Q29_header <- colnames(df3) 
Q29_string <- replicate(40, "Q29-30", simplify = "vector")
Q29_header_new <- paste(Q29_string,Q29_header, sep="_")
colnames(df3) <- Q29_header_new

#Q33
Q33_header <- colnames(df4) 
Q33_string <- replicate(41, "Q33", simplify = "vector")
Q33_header_new <- paste(Q33_string,Q33_header, sep="_")
colnames(df4) <- Q33_header_new

#Q41
Q41_header <- colnames(df5) 
Q41_string <- replicate(38, "Q41", simplify = "vector")
Q41_header_new <- paste(Q41_string,Q41_header, sep="_")
colnames(df5) <- Q41_header_new

#combine appropriate columms into new df
combined_raw_df <- bind_cols(data.frame(df0[,1:6], 
                                        stringsAsFactors = FALSE),
                             data.frame(df1[,11:45],
                                        stringsAsFactors = FALSE), 
                             data.frame(df0[,7:8],
                                        stringsAsFactors = FALSE),
                             data.frame(df2[,12:39],
                                        stringsAsFactors = FALSE),
                             data.frame(df0[,9:17],
                                        stringsAsFactors = FALSE),
                             data.frame(df3[,13:40],
                                        stringsAsFactors = FALSE), 
                             data.frame(df0[,18:33],
                                        stringsAsFactors = FALSE),
                             data.frame(df4[,12:41],
                                        stringsAsFactors = FALSE),
                             data.frame(df0[,34:35],
                                        stringsAsFactors = FALSE),
                             data.frame(df5[,12:38],
                                        stringsAsFactors = FALSE),
                             data.frame(df0[,36:41],
                                        stringsAsFactors = FALSE)
                             )

#save combined df in csv format
write_csv(combined_raw_df, "./combined_raw_df.csv")

