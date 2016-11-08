#MCA of (q38) by carnegie classification (q21)
require(ggplot2)
require(tidyverse)
require(reshape2)
require(FactoMineR)


# install.packages("devtools")
#devtools::install_github("kassambara/factoextra")
require(factoextra)

#Create folder and start a ReadMe for this analysis
dir.create("./output_tables/analysis_of_barriers_q38_by_carnegie_q21/", recursive = TRUE)
dir.create("./output_plots/analysis_of_barriers_q38_by_carnegie_q21/", recursive = TRUE)


#readme <- "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/ReadMe.md"
#write("#ReadMe",readme)
#write(R.version.string, readme, append = TRUE)

#load dataframe
df <- read_csv("decoded_df.csv")
#write("\n Input for script:decoded_df.csv", readme, append = TRUE )

# remove any non-US respondants

countries <- c("United States","Puerto Rico")
df <- df%>%
  filter(Country_Country %in% countries )
#write("\n input dataframe was filtered to include only US and Puerto Rico", readme, append = TRUE )


# retain rows (respondants) for which Carnegie classification is known

question_responses = c("1_Associate's College",
                       "2_Baccalaureate College",
                       "3_Master's (Small, Medium, Large)",
                       "4_Doctoral University (High, Higher, Highest Research Activity)")

df <-  df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. %in% question_responses)


#calculate overall n

overall_n = df%>%
  count()
overall_n <- overall_n$n


#crete a dataframe that contains only the raw scored barriers and the q21 variable
df <- df%>%
  select(q38_Faculty_issues_reduced, 
         q38_Curriculum_issues_reduced, 
         q38_Resources_issues_reduced, 
         q38_Student_issues_reduced, 
         q38_Facilities_issues_reduced, 
         q38_Institutional_issues_reduced, 
         q38_State_issues_reduced, 
         q38_Accredited_issues_reduced, 
         Q21_What.is.the.Carnegie.classification.of.your.institution.)

#nicely code the subsetted dataframe

df$q38_Faculty_issues_reduced[df$q38_Faculty_issues_reduced == 1] <- "Faculty Issues"
df$q38_Faculty_issues_reduced[df$q38_Faculty_issues_reduced == 0] <- "No Faculty Issues"
df$q38_Curriculum_issues_reduced[df$q38_Curriculum_issues_reduced == 1] <- "Curriculumn Issues"
df$q38_Curriculum_issues_reduced[df$q38_Curriculum_issues_reduced == 0] <- "No Curriculumn Issues"
df$q38_Resources_issues_reduced[df$q38_Resources_issues_reduced == 1 ] <- "Resources Issues"
df$q38_Resources_issues_reduced[df$q38_Resources_issues_reduced == 0 ] <- "No Resources Issues"
df$q38_Student_issues_reduced[df$q38_Student_issues_reduced == 1 ] <- "Student Issues"
df$q38_Student_issues_reduced[df$q38_Student_issues_reduced == 0 ] <- "No Student Issues"
df$q38_Facilities_issues_reduced[df$q38_Facilities_issues_reduced == 1] <- "Facilities Issues"
df$q38_Facilities_issues_reduced[df$q38_Facilities_issues_reduced == 0] <- "No Facilities Issues"
df$q38_Institutional_issues_reduced[df$q38_Institutional_issues_reduced == 1] <- "Institutional Issues"
df$q38_Institutional_issues_reduced[df$q38_Institutional_issues_reduced == 0] <- "No Institutional Issues"
df$q38_State_issues_reduced[df$q38_State_issues_reduced == 1] <- "State Issues"
df$q38_State_issues_reduced[df$q38_State_issues_reduced == 0] <- "No State Issues"
df$q38_Accredited_issues_reduced[df$q38_Accredited_issues_reduced == 1] <- "Accredidation Issues"
df$q38_Accredited_issues_reduced[df$q38_Accredited_issues_reduced == 0] <- "No Accredidation Issues"
df$Q21_What.is.the.Carnegie.classification.of.your.institution.[df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College"] <- "Associates"
df$Q21_What.is.the.Carnegie.classification.of.your.institution.[df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College" ] <- "Baccalaureate"
df$Q21_What.is.the.Carnegie.classification.of.your.institution.[df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)"] <- "Masters"
df$Q21_What.is.the.Carnegie.classification.of.your.institution.[df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)"] <- "Doctoral"


#make df an R data frame
df <- data.frame(df)


#create the MCA
analysis.mca <- MCA(as.data.frame.list(df), graph = FALSE)
print(analysis.mca)
summary.MCA(analysis.mca)

#Plot the MCA
fviz_mca_biplot(analysis.mca, 
                invisible="ind",
                habillage = df$Q21_What.is.the.Carnegie.classification.of.your.institution.,
                addEllipses = TRUE, 
                ellipse.level = 0.95, 
                title = paste("MCA analysis of Carnegie Classifications (Q21) and Technical Barriers (Q38)\n
                n=",overall_n))+
  theme_minimal()
ggsave("./output_plots/analysis_of_barriers_q38_by_carnegie_q21/MCA_biplot_q38_by_21.png")


