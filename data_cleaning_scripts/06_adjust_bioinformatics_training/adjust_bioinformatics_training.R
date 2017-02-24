# this script creates a dataframe where level of bioinformatics training is grouped into custom bins

require(tidyverse)


############ LOAD THE PREPARED SURVEY DATA ###########################################
#read in cleaned dataframe "decoded_df.csv"
master.df <- read_csv("../04_decode_survey_responses/output/decoded_df.csv")    

########## MODIFY Training responses

#"1_No training/experience" > No Training
#"2_No formal training (self-taught)" > "Self Taught"
#"3_Short workshop/bootcamp"     >     "Workshops and Bootcamps"
#"4_Some undergraduate courses"      >"Formal Training" 
#"6_Undergraduate certificate"       >"Formal Training" 
#"7_Undergraduate degree"           >"Formal Training"  
#"8_Post-graduate certificate"        >"Formal Training" 
#"9_Graduate course\tGraduate degree" >"Formal Training" 

#add 'faculty_preperation' column
master.df$faculty_preperation <- master.df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.

#reassign values

master.df$faculty_preperation[master.df$faculty_preperation == "1_No training/experience" ] <- "No Training"
master.df$faculty_preperation[master.df$faculty_preperation == "2_No formal training (self-taught)" ] <- "Self Taught"
master.df$faculty_preperation[master.df$faculty_preperation == "3_Short workshop/bootcamp"  ] <- "Workshops and Bootcamps"
master.df$faculty_preperation[master.df$faculty_preperation == "4_Some undergraduate courses"  ] <- "Formal Training"
master.df$faculty_preperation[master.df$faculty_preperation == "6_Undergraduate certificate"  ] <- "Formal Training"
master.df$faculty_preperation[master.df$faculty_preperation == "7_Undergraduate degree" ] <- "Formal Training"
master.df$faculty_preperation[master.df$faculty_preperation == "8_Post-graduate certificate" ] <- "Formal Training"
master.df$faculty_preperation[master.df$faculty_preperation == "9_Graduate course\tGraduate degree" ] <- "Formal Training"

# write dataframe for future refference

dir.create("./output")
write_csv(master.df, "./output/decoded_df_w_faculty_preperation.csv")