#some attempts at plots
require(ggplot2)
require(tidyverse)

#load dataframe and create directories
dir.create("./output_plots", recursive = TRUE)
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
  


#change to factors
#df <- df %>% mutate_if(is.character,as.factor)

# change numerics to factors
#df <- df %>% mutate_if(is.numeric,as.factor)



