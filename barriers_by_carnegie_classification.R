#Plots and analysis of barriers (q38) by carnegie classification (q21)
require(ggplot2)
require(tidyverse)
require(reshape2)

#Create folder and start a ReadMe for this analysis
dir.create("./output_tables/analysis_of_barriers_q38_by_carnegie_q21/", recursive = TRUE)
dir.create("./output_plots/analysis_of_barriers_q38_by_carnegie_q21/", recursive = TRUE)
readme <- "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/ReadMe.md"
write("#ReadMe",readme)
write(R.version.string, readme, append = TRUE)

#load dataframe
df <- read_csv("decoded_df.csv")
write("\n Input for script:decoded_df.csv", readme, append = TRUE )

# remove any non-US respondants

countries <- c("United States","Puerto Rico")
df <- df%>%
  filter(Country_Country %in% countries )
write("\n input dataframe was filtered to include only US and Puerto Rico", readme, append = TRUE )

#calculate n for this filtering

question_responses = c("1_Associate's College",
                       "2_Baccalaureate College",
                       "3_Master's (Small, Medium, Large)",
                       "4_Doctoral University (High, Higher, Higohest Research Activity)")

overall_n = df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. %in% question_responses)%>%
  count()
overall_n <- overall_n$n
write(paste("### Value of n for this analysis is the number of respondants who
            declared a Carnegie classification. n=",overall_n), readme, append = TRUE)

#Carnegie Data Frame

#Generate summary stats for raw scored sub-catagories

scored_sub_catagories <- df%>%
  select(Q38_Faculty.Issues...General, 
         Q38_Faculty.Issue...No.Expertise..Training, 
         Q38_Faculty.Issue...Time, 
         Q38_Faculty.Issue...Does.not.know.how.to.design.curricula.or.incorporate.with.existing.curriculum, 
         Q38_Faculty.Issue...Lack.of.Faculty.interest.at.Institution, 
         Q38_Faculty.Issues...Faculty.is.new.to.current.Dept, 
         Q38_Curriculum.Issues...General, 
         Q38_Curriculum.Issue...Course.Load.Full..No.time.space.for.Content, 
         Q38_Curric.Issues...Does.not.Fit.into.current.Current.Course.Structure, 
         Q38_Curriculum.Issue...Time.for.Curriculum.Development, 
         Q38_Curric.Issues...Lack.of.Curric.Control.not.in.curent.Curric., 
         Q38_Curric.Issues...Bioinf..Taught.in.other.courses.at.institution, 
         Q38_Curric.Issue...Class.Size, 
         Q38_Curric.Issues...Plans.to.teach.Bioinf..In.the.future..but.not.currently.available., 
         Q38_Curric.Issue...Bioinfo.Conent.too.Massive, 
         Q38_Curric.Issues...Content.needs.to.be.introduced.in.multiple.courses,
         Q38_Resource.Issues...General, 
         Q38_Resources...Access.to.Quality.Exercises..Content, 
         Q38_Resources...Access.to.developed.Bioinf.Lesson.Plans.Bioinf.Curric,
         Q38_Resources...Access.to.Approp.Introductory.Content, 
         Q38_Resources...Unable.to.identify.access.best.current.Bioinf.material, 
         Q38_Resource.Issues...Funding, 
         Q38_Resource.Issues...Not.available.in.course.textbook, 
         Q38_Resource.Issues...Access.to.Quality.Online.Exerices.Conent, 
         Q38_Resource.Issues..TA.s.lack.approp.skils,
         Q38_Student.Issues...General, 
         Q38_Student.Issues...UG.Students.Lack.Approp.Background.Knowledge, 
         Q38_Student.Issues...Lack.of.interest.in.topic,
         Q38_Facilities.Issues...General, 
         Q38_Facilities.Issue..Access.to.Appropriate.Facilities..Equipment,
         Q38_Inst.Dept..Support...General, 
         Q38_Inst.Dept.Issues...Institutional.Inertia, 
         Q38_State.restrictions, 
         Q38_Not.accredited,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)

scored_sub_Assoc <- scored_sub_catagories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

scored_sub_Bacca<- scored_sub_catagories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

scored_sub_Master<- scored_sub_catagories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

scored_sub_Doc<- scored_sub_catagories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

raw_scored_df <- data.frame(scored_sub_Assoc[1:34],
                            scored_sub_Bacca[1:34], 
                            scored_sub_Master[1:34], 
                            scored_sub_Doc[1:34])

write.csv(raw_scored_df, file = "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/q38_by_q21_counts_by_sub_categories.csv" )
write("\n## q38_by_q21_counts_by_sub_categories.csv \n contains the sum of responses 
      for all scored sub-categories where respondants indicated their Carnegie classification.\n 
      Users who gave an answer to q38 but did not indicate a Carnegie categories or who were 
      unsure are removed", readme, append = TRUE )


#transpose the dataframe and restore its dataframeness
transposed_raw_scored_df <- t(raw_scored_df)
raw_scored_totals_df <- data.frame(transposed_raw_scored_df)

# create a df containing the chi-squared values
raw_scored_totals_chi <- rbind(raw_scored_totals_df,sapply(raw_scored_totals_df,chisq.test,simulate.p.value = TRUE)[3,])
rownames(raw_scored_totals_chi)[5] <- "chiValues"

#generate table of chivalues
melted_raw_scored_totals_chi <- melt(as.matrix(raw_scored_totals_chi))
write.csv(melted_raw_scored_totals_chi,file= "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/q38_by_21_counts_and_chi_barriers_by_sub_catagory.csv")
write("\n## q38_by_21_counts_and_chi_barriers_by_sub_catagory.csv \n contains the sum of responses 
      for all scored sub-categories where respondents indicated their Carnegie classification. \n 
      Users who gave an answer to q38 but did not indicate a Carnegie categories or who were unsure 
      are removed. \n chisq.test from the R stats package; simulate.p.value = TRUE to account for small
      values of n", readme, append = TRUE )

#write a table that outputs sig chivalues for sub-catagories by Carnegie classification
melted_raw_scored_totals_chi %>%
  filter(Var1 == "chiValues")%>%
  filter(value <= 0.05)%>%
  write.csv(,file= "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/q38_by_21_signifigant_barriers_by_sub_catagory.csv")

write("\n## q38_by_21_signifigant_barriers_by_sub_catagory.csv\n 
      contains chisq.test values from **q38_by_21_signifigant_barriers_by_sub_catagory.csv** that were 
      0.05 or less.", readme, append = TRUE )

#melt the dataframe using reshape - do as matrix to preserve row names
melted_raw_scored_totals_df <- melt(as.matrix(raw_scored_totals_df))

#plot the raw score values

positions = c("scored_sub_Assoc.1.34.",
              "scored_sub_Bacca.1.34.",
              "scored_sub_Master.1.34.",
              "scored_sub_Doc.1.34.")
categories = c ("Associates", 
                "Baccalaureate",
                "Masters",
                "Doctoral")
legend_labels = c("Faculty Issues: General",
                  "Faculty Issues: Expertise/training", 
                  "Faculty Issues: Time",
                  "Faculty Issues: Curriculumn design/integration",
                  "Faculty Issues: Lack of interest",
                  "Faculty Issues: New Faculty",)

melted_raw_scored_totals_df%>%
  ggplot()+
  aes(x= Var1, y = value, fill = Var2)+
  geom_bar(stat="identity", position = "dodge")+
  xlab("Carnegie Classification")+
  ylab("number of issues scored")+
  scale_x_discrete(limits = positions, labels = categories )+
  scale_fill_discrete(name="Q38 Barriers by Carnegie Classification - sub-catagories")+
  ggtitle("Q21- Barriers (summed sub-catagories) by Carnegie Classification")+
  theme(legend.position="bottom")+
  theme(legend.key=element_blank(), legend.key.size=unit(1,"point"))+
  guides(fill=guide_legend(nrow=40,byrow=TRUE))
 

ggsave("./output_plots/analysis_of_barriers_q38_by_carnegie_q21/q38_barriers_summed_by_carnegie_classification_sub_cats.png")


#Generate a sum by carnegie catagory for each of the barrier summed super-catagories
Assoc_var_summed <- df%>%
  select(q38_Faculty_issues_sum, 
         q38_Curriculum_issues_sum, 
         q38_Resources_issues_sum, 
         q38_Student_issues_sum, 
         q38_Facilities_issues_sum, 
         q38_Institutional_issues_sum, 
         q38_State_issues_sum, 
         q38_Accredited_issues_sum, 
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()


Bacca_var_summed <- df%>%
  select(q38_Faculty_issues_sum, 
         q38_Curriculum_issues_sum, 
         q38_Resources_issues_sum, 
         q38_Student_issues_sum, 
         q38_Facilities_issues_sum, 
         q38_Institutional_issues_sum, 
         q38_State_issues_sum, 
         q38_Accredited_issues_sum, 
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Master_var_summed <- df%>%
  select(q38_Faculty_issues_sum, 
         q38_Curriculum_issues_sum, 
         q38_Resources_issues_sum, 
         q38_Student_issues_sum, 
         q38_Facilities_issues_sum, 
         q38_Institutional_issues_sum, 
         q38_State_issues_sum, 
         q38_Accredited_issues_sum, 
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Doc_var_summed<- df%>%
  select(q38_Faculty_issues_sum, 
         q38_Curriculum_issues_sum, 
         q38_Resources_issues_sum, 
         q38_Student_issues_sum, 
         q38_Facilities_issues_sum, 
         q38_Institutional_issues_sum, 
         q38_State_issues_sum, 
         q38_Accredited_issues_sum, 
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

#create a data frame from the sums
q21_q38_summed_df <- data.frame(Assoc_var_summed[1:8],
                       Bacca_var_summed[1:8],
                       Doc_var_summed[1:8],
                       Master_var_summed[1:8])


write.csv(q21_q38_summed_df, file = "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/q38_by_q21_counts_by_summed_super_categories.csv" )
write("\n## q38_by_q21_counts_by_summed_super_categories.csv\n contains the sum of responses 
      for all scored super-categories where respondants indicated their Carnegie classification.\n 
      Users who gave an answer to q38 but did not indicate a Carnegie categories or who were 
      unsure are removed", readme, append = TRUE )


#transpose the dataframe and restore its dataframeness
q21_q38_summed_df <- t(q21_q38_summed_df)
q21_q38_summed_df <- data.frame(q21_q38_summed_df)

# create a df containing the chi-squared values
q21_q38_summed_df_chi <- rbind(q21_q38_summed_df,sapply(q21_q38_summed_df,chisq.test,simulate.p.value = TRUE)[3,])
rownames(q21_q38_summed_df_chi)[5] <- "chiValues"

#melt the dataframe using reshape - do as matrix to preserve row names
melted_q21_q38_summed_df_chi <- melt(as.matrix(q21_q38_summed_df_chi))
write.csv(melted_q21_q38_summed_df_chi,file= "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/q38_by_21_counts_and_chi_barriers_by_super_catagory.csv")
write("\n## q38_by_21_counts_and_chi_barriers_by_super_catagory.csv \n contains the sum of responses 
      for all scored super-categories where respondents indicated their Carnegie classification. \n 
      Users who gave an answer to q38 but did not indicate a Carnegie categories or who were unsure 
      are removed. \n chisq.test from the R stats package; simulate.p.value = TRUE to account for small
      values of n", readme, append = TRUE )

#write a table that outputs sig chivalues for super catagories by Carnegie classification
melted_q21_q38_summed_df_chi %>%
  filter(Var1 == "chiValues")%>%
  filter(value <= 0.05)%>%
  write.csv(,file= "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/q38_by_21_signifigant_barriers_by_summed_super_catagory.csv")

write("\n## q38_by_21_signifigant_barriers_by_summed_super_catagory.csv\n 
      contains chisq.test values from **q38_by_21_counts_and_chi_barriers_by_super_catagory.csv** that were 
      0.05 or less.", readme, append = TRUE )


#melt the dataframe using reshape - do as matrix to preserve row names
melted_q21_q38_summed_df <- melt(as.matrix(q21_q38_summed_df))

# Plot the barrier totals by summed super catagories

positions = c("Assoc_var_summed.1.8.",
              "Bacca_var_summed.1.8.",
              "Master_var_summed.1.8.",
              "Doc_var_summed.1.8.")
categories = c ("Associates", 
                "Baccalaureate",
                "Masters",
                "Doctoral")
legend_labels = c ("Faculty Issues (summed)",
                   "Curriculum Issues (summed)",
                   "Resource Issues (summed)",
                   "Student Issues (summed)",
                   "Facilities Issues (summed)",
                   "Institutional Issues (summed)",
                   "State Issues (summed)",
                   "Accredidation Issues (summed)")
melted_q21_q38_summed_df%>%
  ggplot()+
  aes(x= Var1, y = value, fill = Var2)+
  geom_bar(stat="identity", position = "dodge")+
  scale_x_discrete(limits = positions, labels = categories )+
  scale_fill_discrete(name="Q38 Barrier Classifications", labels = legend_labels)+
  xlab("Carnegie Classification")+
  ylab("number of issues scored")+
  ggtitle(paste("Q21- Barriers (summed super-catagories) by Carnegie Classification\n n=",overall_n))
ggsave("./output_plots/analysis_of_barriers_q38_by_carnegie_q21/q38_barriers_summed_by_carnegie_classification.png")


#Generate a sum by carnegie catagory for each of the barrier reduced super-catagories
Assoc_var_reduced <- df%>%
  select(q38_Faculty_issues_reduced, 
         q38_Curriculum_issues_reduced, 
         q38_Resources_issues_reduced, 
         q38_Student_issues_reduced, 
         q38_Facilities_issues_reduced, 
         q38_Institutional_issues_reduced, 
         q38_State_issues_reduced, 
         q38_Accredited_issues_reduced, 
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()


Bacca_var_reduced <- df%>%
  select(q38_Faculty_issues_reduced, 
         q38_Curriculum_issues_reduced, 
         q38_Resources_issues_reduced, 
         q38_Student_issues_reduced, 
         q38_Facilities_issues_reduced, 
         q38_Institutional_issues_reduced, 
         q38_State_issues_reduced, 
         q38_Accredited_issues_reduced, 
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Master_var_reduced <- df%>%
  select(q38_Faculty_issues_reduced, 
         q38_Curriculum_issues_reduced, 
         q38_Resources_issues_reduced, 
         q38_Student_issues_reduced, 
         q38_Facilities_issues_reduced, 
         q38_Institutional_issues_reduced, 
         q38_State_issues_reduced, 
         q38_Accredited_issues_reduced, 
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Doc_var_reduced<- df%>%
  select(q38_Faculty_issues_reduced, 
         q38_Curriculum_issues_reduced, 
         q38_Resources_issues_reduced, 
         q38_Student_issues_reduced, 
         q38_Facilities_issues_reduced, 
         q38_Institutional_issues_reduced, 
         q38_State_issues_reduced, 
         q38_Accredited_issues_reduced, 
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

#create a data frame from the reduced columns
q21_q38_reduced_df <- data.frame(Assoc_var_reduced[1:8],
                       Bacca_var_reduced[1:8],
                       Doc_var_reduced[1:8],
                       Master_var_reduced[1:8])

write.csv(q21_q38_reduced_df, file = "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/q38_by_q21_counts_by_reduced_super_categories.csv" )
write("\n## q38_by_q21_counts_by_reduced_super_categories.csv\n contains the sum of responses 
      for all scored super-categories where respondants indicated their Carnegie classification.\n 
      Users who gave an answer to q38 but did not indicate a Carnegie categories or who were 
      unsure are removed", readme, append = TRUE )

#transpose the dataframe and restore its dataframeness
q21_q38_reduced_df <- t(q21_q38_reduced_df)
q21_q38_reduced_df <- data.frame(q21_q38_reduced_df)


# create a df containing the chi-squared values
q21_q38_reduced_df_chi <- rbind(q21_q38_reduced_df,sapply(q21_q38_reduced_df,chisq.test,simulate.p.value = TRUE)[3,])
rownames(q21_q38_reduced_df_chi)[5] <- "chiValues"

#melt the dataframe using reshape - do as matrix to preserve row names
melted_q21_q38_reduced_df_chi<- melt(as.matrix(q21_q38_reduced_df_chi))
write.csv(melted_q21_q38_reduced_df_chi,file= "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/q38_by_21_counts_and_chi_barriers_by_reduced_super_catagory.csv")
write("\n## q38_by_21_counts_and_chi_barriers_by_reduced_super_catagory.csv \n contains the sum of responses 
      for all scored-and-reduced super-categories where respondents indicated their Carnegie classification. \n 
      Users who gave an answer to q38 but did not indicate a Carnegie categories or who were unsure 
      are removed. \n chisq.test from the R stats package; simulate.p.value = TRUE to account for small
      values of n", readme, append = TRUE )

#write a table that outputs sig chivalues for super catagories by Carnegie classification
melted_q21_q38_reduced_df_chi %>%
  filter(Var1 == "chiValues")%>%
  filter(value <= 0.05)%>%
  write.csv(,file= "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/q38_by_21_signifigant_barriers_by_reduced_super_catagory.csv")

write("\n## q38_by_21_signifigant_barriers_by_reduced_super_catagory.csv\n 
      contains chisq.test values from **q38_by_21_counts_and_chi_barriers_by_reduced_super_catagory.csv** that were 
      0.05 or less.", readme, append = TRUE )



#melt the dataframe using reshape - do as matrix to preserve row names
melted_q21_q38_reduced_df <- melt(as.matrix(q21_q38_reduced_df))

positions = c("Assoc_var_reduced.1.8.",
              "Bacca_var_reduced.1.8.",
              "Master_var_reduced.1.8.",
              "Doc_var_reduced.1.8.")
categories = c ("Associates", 
                "Baccalaureate",
                "Masters",
                "Doctoral")
legend_labels = c ("Faculty Issues (reduced)",
                   "Curriculum Issues (reduced)",
                   "Resource Issues (reduced)",
                   "Student Issues (reduced)",
                   "Facilities Issues (reduced)",
                   "Institutional Issues (reduced)",
                   "State Issues (reduced)",
                   "Accredidation Issues (reduced)")
melted_q21_q38_reduced_df%>%
  ggplot()+
  aes(x= Var1, y = value, fill = Var2)+
  geom_bar(stat="identity", position = "dodge")+
  scale_x_discrete(limits = positions, labels = categories )+
  xlab("Carnegie Classification")+
  ylab("number of issues scored")+
  scale_fill_discrete(name="Q38 Barrier Classifications", labels = legend_labels)+
  ggtitle(paste("Q21- Barriers (reduced super-catagories) by Carnegie Classification\n n=",overall_n))
ggsave("./output_plots/analysis_of_barriers_q38_by_carnegie_q21/q38_barriers_reduced_by_carnegie_classification.png")








