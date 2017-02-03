# load required libraries
require(ggplot2)
require(tidyverse)
require(reshape2)
require(pwr)
require(FactoMineR)
require(factoextra)


data <- read_csv("../data_cleaning_scripts/04_decode_survey_responses/output/decoded_df.csv")

data.active <- data%>%
  select(q33_Faculty_issues_reduced,
        q33_Curriculum_issues_reduced,
        q33_Facility_issues_reduced,
        q33_Institutional_issues_reduced,
        q33_Resources_issues_reduced,
        q33_Student_issues_reduced,
        Q21_What.is.the.Carnegie.classification.of.your.institution.,
        Q14_Sex,
        Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.,
        Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...)


#manually format nice names
data.active$q33_Facility_issues_reduced[data.active$q33_Facility_issues_reduced == "1"] <- "FACILITIES BARRIERS"
data.active$q33_Facility_issues_reduced[data.active$q33_Facility_issues_reduced == "0"] <- "no reported faclities barriers"
data.active$q33_Faculty_issues_reduced[data.active$q33_Faculty_issues_reduced == "1"] <- "FACULTY BARRIERS"
data.active$q33_Faculty_issues_reduced[data.active$q33_Faculty_issues_reduced == "0"] <- "no reported faculty barriers"
data.active$q33_Curriculum_issues_reduced[data.active$q33_Curriculum_issues_reduced == "1"] <- "CURRICULUM BARRIERS"
data.active$q33_Curriculum_issues_reduced[data.active$q33_Curriculum_issues_reduced == "0"] <- "no reported curriculumn barriers"
data.active$q33_Institutional_issues_reduced[data.active$q33_Institutional_issues_reduced == "1"] <- "INSTITUTIONAL BARRIERS"
data.active$q33_Institutional_issues_reduced[data.active$q33_Institutional_issues_reduced == "0"] <- "no reported institutional barriers"
data.active$q33_Resources_issues_reduced[data.active$q33_Resources_issues_reduced == "1"] <- "RESOURCE BARRIERS"
data.active$q33_Resources_issues_reduced[data.active$q33_Resources_issues_reduced == "0"] <- "no reported resource barriers"
data.active$q33_Student_issues_reduced[data.active$q33_Student_issues_reduced == "1"] <- "STUDENT BARRIERS"
data.active$q33_Student_issues_reduced[data.active$q33_Student_issues_reduced == "0"] <- "no reported student barriers"
data.active$Q21_What.is.the.Carnegie.classification.of.your.institution.[data.active$Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College"] <- "Carnegie Classification: Associates"
data.active$Q21_What.is.the.Carnegie.classification.of.your.institution.[data.active$Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College"] <- "Carnegie Classification: Baccalaureate"
data.active$Q21_What.is.the.Carnegie.classification.of.your.institution.[data.active$Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)"] <- "Carnegie Classification: Masters"
data.active$Q21_What.is.the.Carnegie.classification.of.your.institution.[data.active$Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)"] <- "Carnegie Classification: Doctoral"
data.active$Q14_Sex[data.active$Q14_Sex == "1_Female"  ] <- "Female"
data.active$Q14_Sex[data.active$Q14_Sex == "2_Male"   ] <- "Male"
data.active$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.[data.active$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training. == "1_No training/experience"] <- "Bioinformatics Training: None"
data.active$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.[data.active$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training. == "2_No formal training (self-taught)"] <- "Bioinformatics Training: Self Taught"
data.active$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.[data.active$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training. == "3_Short workshop/bootcamp"] <- "Bioinformatics Training: Short Workshop"
data.active$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.[data.active$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training. == "4_Some undergraduate courses"] <- "Bioinformatics Training: Some Undergraduate Training"
data.active$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.[data.active$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training. == "6_Undergraduate certificate"] <- "Bioinformatics Training: Undergraduate Certificate"
data.active$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.[data.active$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training. == "7_Undergraduate degree"] <- "Bioinformatics Training: Undergraduate Degree"
data.active$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.[data.active$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training. == "8_Post-graduate certificate"] <- "Bioinformatics Training: Post-graduate Certificate"
data.active$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.[data.active$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training. == "9_Graduate course	Graduate degree"] <- "Bioinformatics Training: Graduate Course/Degree"
data.active$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...[data.active$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "1_Dedicated course for life-science majors"] <- "Teaching: Dedicated Bioinformatics Course"
data.active$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...[data.active$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "2_Include 'substantial' bioinformatics in courses for life-science majors"  ] <- "Teaching: Integrating Bioinformatics"
data.active$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...[data.active$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "3_Do NOT currently, but would like to include 'substantial' bioinformatics in courses for life-science majors"] <- "Teaching: Not Integrating Bioinformatics"


#filter out non-relavant responders


data.active <- data.active%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... != "4_Graduate supervisors in the life sciences")%>%
  filter(Q14_Sex != "3_Rather not say")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. != "5_Don't know")





reduced.mca <- MCA(X = as.matrix(data.active),
                     quali.sup = 1:6,
                     graph = FALSE)





fviz_mca_biplot(reduced.mca, 
                invisible=c("ind"),
                habillage =  "Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...",
                addEllipses = TRUE, 
                labelsize = 3,
                ellipse.level = 0.95, 
                title = "test")+
  theme_minimal()



