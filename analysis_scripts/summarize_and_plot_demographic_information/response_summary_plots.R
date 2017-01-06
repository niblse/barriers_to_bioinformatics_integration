#some attempts at plots
require(ggplot2)
require(tidyverse)

#load dataframe and create directories
dir.create("./output_plots", recursive = TRUE)
dir.create("./output_tables", recursive = TRUE)
df <- read_csv("../../data_cleaning_scripts/04_decode_survey_responses/output/decoded_df.csv")


# remove any non-US respondants
countries <- c("United States","Puerto Rico")
df <- df%>%
  filter(Country_Country %in% countries )

#generate single variable summary plots

#question 14 - gender

df %>%
  ggplot()+
  aes(x= reorder(df$Q14_Sex,
                 df$Q14_Sex, 
                 function(x)-length(x)))+
  geom_bar()+
  scale_x_discrete(labels=c("Female", "Male", "Not Provided"))+
  ggtitle("Q14 - Survey Responses by Gender")+
  xlab("gender")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")

ggsave("./output_plots/q14_survey_responses_by_gender.png")

#write as a table
Q14.gender.table <- as.data.frame(table(as.factor(df$Q14_Sex)),stringsAsFactors = FALSE)
total.responses=sum(Q14.gender.table$Freq)
Q14.gender.table <- Q14.gender.table%>%
  group_by(Var1)%>%
  mutate(percentage = (Freq/total.responses)*100)
write.csv(Q14.gender.table, file = "./output_tables/Q14.gender.table.csv")



#question 57 - Institution
df %>%
  ggplot()+
  aes(x= reorder(df$Q57_Please.select.the.statement.belOw.that.best.describes.yOu.,
                 df$Q57_Please.select.the.statement.belOw.that.best.describes.yOu.,
                 function(x)-length(x)))+
  geom_bar()+
  scale_x_discrete(labels=c("Teaching at 4-year", "Teaching at 2-year", "Other"))+
  ggtitle("Q57 - Survey Responses by Institution Type")+
  xlab("institution type")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")
ggsave("./output_plots/q57_survey_responses_by_institution_type.png")

#write as a table
Q57.institution.table <- as.data.frame(table(as.factor(df$Q57_Please.select.the.statement.belOw.that.best.describes.yOu.)),stringsAsFactors = FALSE)
total.responses=sum(Q57.institution.table$Freq)
Q57.institution.table <- Q57.institution.table%>%
  group_by(Var1)%>%
  mutate(percentage = (Freq/total.responses)*100)
write.csv(Q57.institution.table, file = "./output_tables/Q57.institution.table.csv")


#question 1 - Current teaching
df %>%
  ggplot()+
  aes(x= reorder(df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn..., 
                 df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn..., 
                 function(x)-length(x)))+
  geom_bar()+
  scale_x_discrete(labels=c("life-science majors: teaching bioinformatics course", 
                            "life-science majors: including bioinformatics",
                            "life-science majors: not currently including bioinformatics", 
                            "life science graduate advisor"))+
  ggtitle("Q1 - Survey Responses by Teaching Category")+
  xlab("teaching category")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("./output_plots/q1_survey_responses_by_teaching_category.png")

#write as a table
Q1.currently.teaching.table <- as.data.frame(table(as.factor(df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...)),stringsAsFactors = FALSE)
total.responses=sum(Q1.currently.teaching.table$Freq)
Q1.currently.teaching.table <- Q1.currently.teaching.table%>%
  group_by(Var1)%>%
  mutate(percentage = (Freq/total.responses)*100)
write.csv(Q1.currently.teaching.table, file = "./output_tables/Q1.currently.teaching.table.csv")

#question State 
state_df <- df%>%
  filter(Country_Country == "United States")

state_df %>%
  ggplot()+
  aes(x= reorder(state_df$State_State, 
                 state_df$State_State, 
                 function(x)-length(x)))+
  geom_bar()+
  ggtitle("QState - Survey Responses by State")+
  xlab("state (including Washington DC; excluding Puerto Rico)")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("./output_plots/qstate_survey_responses_by_state.png")


#write as a table
QState.table <- as.data.frame(table(as.factor(df$State_State)),stringsAsFactors = FALSE)
total.responses=sum(QState.table$Freq)
QState.table <- QState.table%>%
  group_by(Var1)%>%
  mutate(percentage = (Freq/total.responses)*100)
write.csv(QState.table, file = "./output_tables/QState.table.csv")



#question 21, carnegie classification 

df %>%
  ggplot()+
  aes(x= reorder(df$Q21_What.is.the.Carnegie.classification.of.your.institution.,
                 df$Q21_What.is.the.Carnegie.classification.of.your.institution.,
                 function(x)-length(x)))+
  geom_bar()+
  scale_x_discrete(labels=c("4; Doctoral", "2; Baccalaureate","3; Master's", "1; Associate's", "Don't know"))+
  ggtitle("Q21 - Survey Responses by Carnegie Classification")+
  xlab("carnegie classification")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")
ggsave("./output_plots/q21_survey_responses_by_carnegie_classification.png")


#write as a table
Q21.carnegie.classification.table <- as.data.frame(table(as.factor(df$Q21_What.is.the.Carnegie.classification.of.your.institution.)),stringsAsFactors = FALSE)
total.responses=sum(Q21.carnegie.classification.table$Freq)
Q21.carnegie.classification.table <- Q21.carnegie.classification.table%>%
  group_by(Var1)%>%
  mutate(percentage = (Freq/total.responses)*100)
write.csv(Q21.carnegie.classification.table, file = "./output_tables/Q21.carnegie.classification.table.csv")



#question 18, year of degree

df %>%
  ggplot()+
  aes(x= reorder(df$Q18_Year.of.highest.earned.degree.,
                 df$Q18_Year.of.highest.earned.degree.,
                 function(x)-length(x)))+
  geom_bar()+
  ggtitle("Q18 - Survey Responses by Degree Year")+
  xlab("year of degree")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("./output_plots/q18_survey_responses_by_year_of_degree.png")


#write as a table
Q18.degree.year.table <- as.data.frame(table(as.factor(df$Q18_Year.of.highest.earned.degree.)),stringsAsFactors = FALSE)
total.responses=sum(Q18.degree.year.table$Freq)
Q18.degree.year.table <- Q18.degree.year.table%>%
  group_by(Var1)%>%
  mutate(percentage = (Freq/total.responses)*100)
write.csv(Q18.degree.year.table, file = "./output_tables/Q18.degree.year.table.csv")


#question 3, level of training

df %>%
  ggplot()+
  aes(x= reorder(df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.,
                 df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.,
                 function(x)-length(x)))+
  geom_bar()+
  scale_x_discrete(labels=c("No formal training",
                            "Short workshop/bootcamp",
                            "Post-graduate certificate",
                            "No training/experience",
                            "Graduate course/degree",
                            "Some undergraduate courses",
                            "Undergraduate degree",
                            "Undergraduate certificate"
                            ))+
  ggtitle("Q3 - Survey Responses by Level of Bioinformatics Training")+
  xlab("training level")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("./output_plots/q3_survey_responses_by_level_of_training.png")


#write as a table
Q3.level.of.training.table <- as.data.frame(table(as.factor(df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.)),stringsAsFactors = FALSE)
total.responses=sum(Q3.level.of.training.table$Freq)
Q3.level.of.training.table <- Q3.level.of.training.table%>%
  group_by(Var1)%>%
  mutate(percentage = (Freq/total.responses)*100)
write.csv(Q3.level.of.training.table, file = "./output_tables/Q3.level.of.training.table.csv")


#question 22, MSI

df %>%
  ggplot()+
  aes(x= reorder(df$Q22_Is.your.institution.classified.as.minority.serving.,
                 df$Q22_Is.your.institution.classified.as.minority.serving.,
                 function(x)-length(x)))+
  geom_bar()+
  scale_x_discrete(labels=c("No",
                            "Yes",
                            "Don't know"))+
  ggtitle("Q22 - Minority Serving Institution")+
  xlab("institution type")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")
ggsave("./output_plots/q22_survey_responses_by_MSI.png")


#write as a table
Q22.msi.table <- as.data.frame(table(as.factor(df$Q22_Is.your.institution.classified.as.minority.serving.)),stringsAsFactors = FALSE)
total.responses=sum(Q22.msi.table$Freq)
Q22.msi.table <- Q22.msi.table%>%
  group_by(Var1)%>%
  mutate(percentage = (Freq/total.responses)*100)
write.csv(Q22.msi.table, file = "./output_tables/Q22.msi.table.csv")


#question 23, number of students (grad+undergrad)

df %>%
  ggplot()+
  aes(x= reorder(df$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution.,
                 df$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution.,
                 function(x)-length(x)))+
  geom_bar()+
  scale_x_discrete(labels=c("5-15,000",
                            "> 15,000",
                            "< 5,000", 
                            "Don't know"))+
  ggtitle("Q23 - Number of undegraduate and graduate students")+
  xlab("student enrollment")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")

ggsave("./output_plots/q23_survey_responses_by_undegraduate_and_graduate_enrollment.png")

#write as a table
Q23.total.enrollment.table <- as.data.frame(table(as.factor(df$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution.)),stringsAsFactors = FALSE)
total.responses=sum(Q22.total.enrollment.table$Freq)
Q23.total.enrollment.table <- Q23.total.enrollment.table%>%
  group_by(Var1)%>%
  mutate(percentage = (Freq/total.responses)*100)
write.csv(Q23.total.enrollment.table, file = "./output_tables/Q23.total.enrollment.table.csv")


#question 24, number of undergraduate students 

df %>%
  ggplot()+
  aes(x= reorder(df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution.,
                 df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution.,
                 function(x)-length(x)))+
  geom_bar()+
  scale_x_discrete(labels=c("< 5,000",
                            "5-15,000",
                            "> 15,000", 
                            "Don't know"))+
  ggtitle("Q24 - Number of undegraduate students")+
  xlab("student enrollment")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")
ggsave("./output_plots/q24_survey_responses_by_undegraduate_enrollment.png")

#write as a table
Q24.undergraduate.enrollment.table <- as.data.frame(table(as.factor(df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution.)),stringsAsFactors = FALSE)
total.responses=sum(Q24.undergraduate.enrollment.table$Freq)
Q24.undergraduate.enrollment.table <- Q24.undergraduate.enrollment.table%>%
  group_by(Var1)%>%
  mutate(percentage = (Freq/total.responses)*100)
write.csv(Q24.undergraduate.enrollment.table, file = "./output_tables/Q24.undergraduate.enrollment.table.csv")


#question 26, number of full-time faculty 

df %>%
  ggplot()+
  aes(x= reorder(df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...,
                 df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...,
                 function(x)-length(x)))+
  geom_bar()+
  scale_x_discrete(labels=c("10-20",
                            "< 10",
                            "21-30",
                            "31-40",
                            "> 50",
                            "Don't know", 
                            "41-50"))+
  ggtitle("Q26 - Number of full-time faculty")+
  xlab("faculty size")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")
ggsave("./output_plots/q26_survey_responses_by_faculty_size.png")


#write as a table
Q26.full.time.faculty.table <- as.data.frame(table(as.factor(df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...)),stringsAsFactors = FALSE)
total.responses=sum(Q26.full.time.faculty.table$Freq)
Q26.full.time.faculty.table <- Q26.full.time.faculty.table%>%
  group_by(Var1)%>%
  mutate(percentage = (Freq/total.responses)*100)
write.csv(Q26.full.time.faculty.table, file = "./output_tables/Q26.full.time.faculty.table.csv")


#question 27, undergrads in department

df %>%
  ggplot()+
  aes(x= reorder(df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..,
                 df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..,
                 function(x)-length(x)))+
  geom_bar()+
  scale_x_discrete(labels=c("101-500",
                            "501-2000",
                            "51-100",
                            "Don't know",
                            "< 50", 
                            "> 2000"))+
  ggtitle("Q27 - Number of undegraduates in department")+
  xlab("student enrollment")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")
ggsave("./output_plots/q27_survey_responses_by_undergrads_in_dept.png")

#write as a table
Q27.undergrads.in.dept.table <- as.data.frame(table(as.factor(df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..)),stringsAsFactors = FALSE)
total.responses=sum(Q26.full.time.faculty.table$Freq)
Q27.undergrads.in.dept.table <- Q27.undergrads.in.dept.table%>%
  group_by(Var1)%>%
  mutate(percentage = (Freq/total.responses)*100)
write.csv(Q27.undergrads.in.dept.table, file = "./output_tables/Q27.undergrads.in.dept.table.csv")

#question Region, respondants by region

df %>%
  ggplot()+
  aes(x= reorder(df$Region_Region,
                 df$Region_Region,
                 function(x)-length(x)))+
  geom_bar()+
  scale_x_discrete(labels=c("SO",
                            "MW",
                            "NE",
                            "WE"))+
  ggtitle("QRegion - Number of respondants by region")+
  xlab("region")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")
ggsave("./output_plots/qRegion_survey_responses_by_region.png")
  

#write as a table
Qregion.table <- as.data.frame(table(as.factor(df$Region_Region)),stringsAsFactors = FALSE)
total.responses=sum(Qregion.table$Freq)
Qregion.table <- Qregion.table%>%
  group_by(Var1)%>%
  mutate(percentage = (Freq/total.responses)*100)
write.csv(Qregion.table, file = "./output_tables/Qregion.table.csv")

df %>%
  ggplot()+
  aes(x= reorder(df$Q15_Race,
                 df$Q15_Race,
                 function(x)-length(x)))+
  geom_bar()+
  scale_x_discrete(labels=c("White",
                            "Rather Not Say",
                            "Asian",
                            "Black or African American",
                            "Native Hawaiian or other Pacific Islander", 
                            "American Indian or Alaska Native",
                            "NA"))+
  ggtitle("Q15 Race")+
  xlab("region")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")
ggsave("./output_plots/q15_Race.png")


#write as a table
Q15.race.table <- as.data.frame(table(as.factor(df$Q15_Race)),stringsAsFactors = FALSE)
total.responses=sum(Q15.race.table$Freq)
Q15.race.table <- Q15.race.table%>%
  group_by(Var1)%>%
  mutate(percentage = (Freq/total.responses)*100)
write.csv(Q15.race.table, file = "./output_tables/Q15.race.table.csv")
