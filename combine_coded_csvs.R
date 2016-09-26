#this script combines the coded survey results and combines generated in excel and generates a combined R dataframe

#load tidyverse library
library(tidyverse)

#read in coded excel sheets
# skip the first line (use 2nd line as column name)
# master.csv has some null values, replace this with NA
df0 <- read_csv("./master.csv", skip = 1, na = "#NULL!")
df1 <- read_csv("./01_preventing_you.csv", skip = 1, na = "#NULL!")
df2 <- read_csv("./02_barriers_dev_imp.csv", skip = 1, na = "#NULL!")
df3 <- read_csv("./03_technical_barriers.csv", skip = 1, na = "#NULL!")
df4 <- read_csv("./04_important_challenges.csv", skip = 1, na = "#NULL!")
df5 <- read_csv("./05_student_deficiencies.csv", skip = 1, na = "#NULL!")


#create a dfs of the appropriate columns 

#df of columns and identifiers and questions relevant to our barriers analysis from master dataframe
df0cols1 <- data.frame(df0$ID,
             df0$EmailAddress,
             df0$IPAddress,
             df0$Sex,
             df0$Race,
             df0$Ethnicity,
             df0$City,
             df0$State,
             df0$Country,
             df0$Region,
             df0$Division,
             df0$`Highest earned degree. If "other," please explain.`,
             df0$`Year of highest earned degree.`,
             df0$`Which of the following best describes your level of bioinformatics training?`,
             df0$`What is the Carnegie classification of your institution?`,
             df0$`Is your institution classified as minority-serving?`,
             df0$`What is the total number of students (undergraduate and graduate) at your institution?`,
             df0$`What is the total number of undergraduate students at your institution?`,
             df0$`How many full-time faculty are in your department/unit? (Do not include part-time faculty or adju...`,
             df0$`How many undergraduate students are in your department/unit (all majors)?`,
             df0$`Please select the statement belOw that best describes yOu.`,
             df0$`Please select the statement belOw that best describes yOur current teaching Of biOinfOrmatics cOn...`,
             df0$`In yOur OpiniOn, are additiOnal undergraduate cOurses with biOinfOrmatics cOntent needed at yOur...`,
             
            stringsAsFactors = FALSE)

#second dataframe composed of columms from the master dataframe; should be irrlevant to our analysis
df0cols2 <- data.frame(df0$`As part of our work, we are building an online repository of bioinformatics content assessments a...`,df0$`Highest earned degree. If "other," please explain.-TEXT`,
                       df0$`For each undergraduate course you teach that includes bioinformatics content, please provide the...`,
                       df0$`Highest earned degree. If "other," please explain.-TEXT`,
                       df0$`What is the name of your department/unit (e.g., Department of Biology, Biochemistry Department, S...`,
                       df0$`Additional comments:`,
                       df0$`Optional: Please give certificate or minor name, department/unit in which it's offered, and websi...`,
                       df0$`Optional: Please give name, department/unit in which it's offered, and website URL (if available):`,
                       df0$`Briefly describe the format of the bioinformatics training (e.g., boot camp, short course, etc.)...`,
                       df0$`Briefly describe your audience for this training.`,
                       df0$`In your opinion, what are the biggest bioinformatics needs of those taking your training?`,
                       df0$`What reasons do your students provide as to why they are taking your training (e.g., professional...`,
                       df0$`If there are bioinformatics competencies you feel are missing in the above, please describe them...`,
                       stringsAsFactors = FALSE)

df1cols1 <- data.frame(df1[11:45],
                       stringsAsFactors = FALSE)
df1cols2 <- data.frame(df2[11:39],
                       stringsAsFactors = FALSE)
df1cols3 <- data.frame(df3[11:40],
                       stringsAsFactors = FALSE)
df1cols4 <- data.frame(df4[11:41],
                       stringsAsFactors = FALSE)
df1cols5 <- data.frame(df5[11:38],
                       stringsAsFactors = FALSE)

#combine appropriate columms into new df
combined_raw_df <- bind_cols(df0cols1,
                     df1cols1,
                     df1cols2,
                     df1cols3,
                     df1cols4,
                     df1cols5,
                     df0cols2)


#save combined df in csv format
write_csv(combined_raw_df, "./combined_raw_df.csv")

