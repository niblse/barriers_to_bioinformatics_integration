#Plots and analysis of barriers (q38) by carnegie classification (q21)
require(ggplot2)
require(tidyverse)
require(reshape2)
require(corrplot)

#Create folders and start a ReadMe for this analysis
dir.create("./output_tables/analysis_of_barriers_q38_by_carnegie_q21/", recursive = TRUE)
dir.create("./output_plots/analysis_of_barriers_q38_by_carnegie_q21/", recursive = TRUE)
readme <- "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/ReadMe.md"
write("#ReadMe",readme)
write(R.version.string, readme, append = TRUE)

#load dataframe
df <- read_csv("../../data_cleaning_scripts/04_decode_survey_responses/output/decoded_df.csv")
write("\n Input for script:decoded_df.csv", readme, append = TRUE )

# remove any non-US respondants

countries <- c("United States","Puerto Rico")
df <- df%>%
  filter(Country_Country %in% countries )
write("\n input dataframe was filtered to include only US and Puerto Rico", readme, append = TRUE )

#Create a table of response statistics

Q21_associates_responses <- df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College") %>%
  count()
Q21_associates_responses <- Q21_associates_responses$n

Q21_baccalaureate_responses<- df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College") %>%
  count()
Q21_baccalaureate_responses <- Q21_baccalaureate_responses$n

Q21_masters_responses<- df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)") %>%
  count()
Q21_masters_responses <- Q21_masters_responses$n

Q21_doctoral_responses<- df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)") %>%
  count()
Q21_doctoral_responses <- Q21_doctoral_responses$n

overall_n <- sum(Q21_associates_responses, 
                 Q21_baccalaureate_responses, 
                 Q21_doctoral_responses, 
                 Q21_masters_responses)

write(paste("### Value of n for this analysis is the number of respondants who
            declared a Carnegie classification. n=",overall_n), readme, append = TRUE)

#calculate number of responses (Q38) in each catagory (Q21)

Q38_Q21_associates_responses <- df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College" &
           !is.na(df$Q38_What.is.preventing.yOu.frOm.including.biOinfOrmatics.cOntent.in.these.cOurses.))%>%
  count()
Q38_Q21_associates_responses <- Q38_Q21_associates_responses$n

Q38_Q21_baccalaureate_responses <- df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College" &
           !is.na(df$Q38_What.is.preventing.yOu.frOm.including.biOinfOrmatics.cOntent.in.these.cOurses.))%>%
  count()
Q38_Q21_baccalaureate_responses <- Q38_Q21_baccalaureate_responses$n

Q38_Q21_masters_responses <- df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)" &
           !is.na(df$Q38_What.is.preventing.yOu.frOm.including.biOinfOrmatics.cOntent.in.these.cOurses.))%>%
  count()
Q38_Q21_masters_responses <- Q38_Q21_masters_responses$n

Q38_Q21_doctoral_responses <- df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)" &
           !is.na(df$Q38_What.is.preventing.yOu.frOm.including.biOinfOrmatics.cOntent.in.these.cOurses.))%>%
  count()
Q38_Q21_doctoral_responses <- Q38_Q21_doctoral_responses$n



#Carnegie Data Frame

#Generate summary stats for raw scored sub-catagories

Q38_scored_sub_catagories <- df%>%
  select(Q38_Faculty.Issue...No.Expertise..Training, 
         Q38_Faculty.Issue...Time, 
         Q38_Faculty.Issue...Does.not.know.how.to.design.curricula.or.incorporate.with.existing.curriculum, 
         Q38_Faculty.Issue...Lack.of.Faculty.interest.at.Institution, 
         Q38_Faculty.Issues...Faculty.is.new.to.current.Dept, 
         Q38_Curriculum.Issue...Course.Load.Full..No.time.space.for.Content, 
         Q38_Curric.Issues...Does.not.Fit.into.current.Current.Course.Structure, 
         Q38_Curriculum.Issue...Time.for.Curriculum.Development, 
         Q38_Curric.Issues...Lack.of.Curric.Control.not.in.curent.Curric., 
         Q38_Curric.Issues...Bioinf..Taught.in.other.courses.at.institution, 
         Q38_Curric.Issue...Class.Size, 
         Q38_Curric.Issues...Plans.to.teach.Bioinf..In.the.future..but.not.currently.available., 
         Q38_Curric.Issue...Bioinfo.Conent.too.Massive, 
         Q38_Curric.Issues...Content.needs.to.be.introduced.in.multiple.courses,
         Q38_Resources...Access.to.Quality.Exercises..Content, 
         Q38_Resources...Access.to.developed.Bioinf.Lesson.Plans.Bioinf.Curric,
         Q38_Resources...Access.to.Approp.Introductory.Content, 
         Q38_Resources...Unable.to.identify.access.best.current.Bioinf.material, 
         Q38_Resource.Issues...Funding, 
         Q38_Resource.Issues...Not.available.in.course.textbook, 
         Q38_Resource.Issues...Access.to.Quality.Online.Exerices.Conent, 
         Q38_Resource.Issues..TA.s.lack.approp.skils,
         Q38_Student.Issues...UG.Students.Lack.Approp.Background.Knowledge, 
         Q38_Student.Issues...Lack.of.interest.in.topic,
         Q38_Facilities.Issue..Access.to.Appropriate.Facilities..Equipment,
         Q38_Inst.Dept.Issues...Institutional.Inertia, 
         Q38_State.restrictions, 
         Q38_Not.accredited,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)

Q38_scored_sub_Assoc <- Q38_scored_sub_catagories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Q38_scored_sub_Bacca<- Q38_scored_sub_catagories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Q38_scored_sub_Master<- Q38_scored_sub_catagories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Q38_scored_sub_Doc<- Q38_scored_sub_catagories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Q38_raw_scored_df <- data.frame(Q38_scored_sub_Assoc[1:28],
                                Q38_scored_sub_Bacca[1:28], 
                                Q38_scored_sub_Master[1:28], 
                                Q38_scored_sub_Doc[1:28])

write.csv(Q38_raw_scored_df, file = "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/q38_by_q21_counts_by_sub_categories.csv" )
write("\n## q38_by_q21_counts_by_sub_categories.csv \n contains the sum of responses 
      for all scored sub-categories where respondants indicated their Carnegie classification.\n 
      Users who gave an answer to q38 but did not indicate a Carnegie categories or who were 
      unsure are removed", readme, append = TRUE )


#transpose the dataframe and restore its dataframeness
Q38_transposed_raw_scored_df <- t(Q38_raw_scored_df)
Q38_raw_scored_totals_df <- data.frame(Q38_transposed_raw_scored_df)

# create a df containing the chi-squared values
Q38_raw_scored_totals_chi <- rbind(Q38_raw_scored_totals_df,sapply(Q38_raw_scored_totals_df,
                                                                   chisq.test,
                                                                   simulate.p.value = TRUE)[3,])
rownames(Q38_raw_scored_totals_chi)[5] <- "chiValues"

#generate table of chivalues
Q38_melted_raw_scored_totals_chi <- melt(as.matrix(Q38_raw_scored_totals_chi))
write.csv(Q38_melted_raw_scored_totals_chi,file= "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/q38_by_21_counts_and_chi_barriers_by_sub_catagory.csv")
write("\n## q38_by_21_counts_and_chi_barriers_by_sub_catagory.csv \n contains the sum of responses 
      for all scored sub-categories where respondents indicated their Carnegie classification. \n 
      Users who gave an answer to q38 but did not indicate a Carnegie categories or who were unsure 
      are removed. \n chisq.test from the R stats package; simulate.p.value = TRUE to account for small
      values of n", readme, append = TRUE )

#write a table that outputs sig chivalues for sub-catagories by Carnegie classification
Q38_melted_raw_scored_totals_chi %>%
  filter(Var1 == "chiValues")%>%
  filter(value <= 0.05)%>%
  write.csv(,file= "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/q38_by_21_signifigant_barriers_by_sub_catagory.csv")

write("\n## q38_by_21_signifigant_barriers_by_sub_catagory.csv\n 
      contains chisq.test values from **q38_by_21_signifigant_barriers_by_sub_catagory.csv** that were 
      0.05 or less.", readme, append = TRUE )

#melt the dataframe using reshape - do as matrix to preserve row names
Q38_melted_raw_scored_totals_df <- melt(as.matrix(Q38_raw_scored_totals_df))

#plot the raw score values

positions = c("Q38_scored_sub_Assoc.1.28.",
              "Q38_scored_sub_Bacca.1.28.",
              "Q38_scored_sub_Master.1.28.",
              "Q38_scored_sub_Doc.1.28.")
categories = c (paste("Associates, Q38 n(+r)=",Q38_Q21_associates_responses), 
                paste("Baccalaureate, Q38 n(+r)=",Q38_Q21_baccalaureate_responses),
                paste("Masters, Q38 n(+r)=",Q38_Q21_masters_responses),
                paste("Doctoral, Q38 n(+r)=", Q38_Q21_doctoral_responses))
legend_labels = c("Faculty Issues: Expertise/training", 
                  "Faculty Issues: Time",
                  "Faculty Issues: Curriculum design/integration",
                  "Faculty Issues: Lack of interest",
                  "Faculty Issues: New Faculty",
                  "Curriculum Issues: No space",
                  "Curriculum Issues: Incompatible",
                  "Curriculum Issues: Time needed to develop",
                  "Curriculum Issues: No control",
                  "Curriculum Issues: Covered elsewhere",
                  "Curriculum Issues: Class size",
                  "Curriculum Issues: Plan for future coverage",
                  "Curriculum Issues: Too much content",
                  "Curriculum Issues: Content requires several courses",
                  "Resource Issues: Access to exercises",
                  "Resource Issues: Access to lesson plans",
                  "Resource Issues: Acess to intro content",
                  "Resource Issues: Unable to vet content",
                  "Resource Issues: Funding",
                  "Resource Issues: No appropriate text",
                  "Resource Issues: No access to online exercieses",
                  "Resource Issues: No qualified TAs",
                  "Student Issues: Background knowledge",
                  "Student Issues: Interest in topic",
                  "Facilities: Access to equipment",
                  "Institutional: Inertia",
                  "State Restrictions", 
                  "Accredidation")

Q38_melted_raw_scored_totals_df%>%
  ggplot()+
  aes(x= Var1, y = value, fill = Var2)+
  geom_bar(stat="identity", position = "dodge")+
  xlab("Carnegie Classification")+
  ylab("number of issues scored")+
  scale_x_discrete(limits = positions, labels = categories )+
  scale_fill_discrete(name= "Q38 Barriers by Carnegie Classification
                      \n
                      n(r+) = number of individuals commenting", labels = legend_labels)+
  ggtitle(paste("Q21- Barriers (summed sub-catagories) by Carnegie Classification \n n=",overall_n))+
  theme(legend.position="bottom")+
  theme(legend.key=element_blank(), legend.key.size=unit(1,"point"))+
  guides(fill=guide_legend(nrow=10,byrow=TRUE))
 
ggsave("./output_plots/analysis_of_barriers_q38_by_carnegie_q21/q38_barriers_summed_by_carnegie_classification_sub_cats.png")

#plot the biggest barriers

#Caculate percentages biggest barriers and sort
Q38_biggest_barriers_df <- Q38_melted_raw_scored_totals_df %>%
  group_by(Var2)%>%
  mutate(summed_score = sum(value))%>%
  mutate(percentage = (value / summed_score) * 100) 

#Sort the biggest barriers 
Q38_biggest_barriers_df <- Q38_biggest_barriers_df %>%
  arrange(summed_score, Var2, Var1)

write.csv(Q38_biggest_barriers_df,file= "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/scored_barriers_andpercentages_by_sub_catagory.csv")


#add proportion calculation to barriers table where porpotion is out of total respondants
# in a category. e.g. if 100 answered 'Associates' to Q21 but 50 left a + comment in Q38
# then 50% of respondants at an Associates institution encountered a barrier

Q38_biggest_barriers_df_w_prop <- Q38_biggest_barriers_df %>%
  mutate(proportion = ifelse(Var1 == "Q38_scored_sub_Assoc.1.28.", value/Q21_associates_responses,
                             ifelse(Q38_biggest_barriers_df$Var1 == "Q38_scored_sub_Bacca.1.28.", value/Q21_baccalaureate_responses,
                                    ifelse(Q38_biggest_barriers_df$Var1 == "Q38_scored_sub_Master.1.28.", value/Q21_masters_responses, 
                                           ifelse(Q38_biggest_barriers_df$Var1 == "Q38_scored_sub_Doc.1.28.", value/Q21_doctoral_responses, NA)))))
  
write.csv(Q38_biggest_barriers_df_w_prop,file= "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/scored_barriers_percentages_andporportons_by_sub_catagory.csv")



# Create a new df only containing unique barriers and their total values

Q38_barriers_counts_df <- Q38_biggest_barriers_df %>%
  distinct(Var2, .keep_all = TRUE)

#plot barriers 
Q38_barriers_counts_df %>%
  arrange(summed_score, Var2)%>%
  ggplot()+
  aes(x= Var2, y=summed_score)+
  geom_bar(stat="identity")



#create plot showing the top five barriers and percentage of institution types reporting
positions = c("Q38_scored_sub_Assoc.1.28.",
              "Q38_scored_sub_Bacca.1.28.",
              "Q38_scored_sub_Master.1.28.",
              "Q38__scored_sub_Doc.1.28.")
categories = c (paste("Associates, Q38 n(r)=",Q38_Q21_associates_responses), 
                paste("Baccalaureate, Q38 n(r)=",Q38_Q21_baccalaureate_responses),
                paste("Masters, Q38 n(r)=",Q38_Q21_masters_responses),
                paste("Doctoral, Q38 n(r)=", Q38_Q21_doctoral_responses))
legend_labels = c("Faculty Training", 
                  "Faculty Time", 
                  "Full Course Load", 
                  "Integration into Curriculum", 
                  "Student Preperation")
Q38_biggest_barriers_df %>%
  filter(summed_score >=45)%>%
  arrange(Var1)%>%
  ggplot()+
  aes(x=Var2, y=percentage, fill=Var1 )+
  scale_fill_discrete(name="Institution Types", labels = categories)+
  scale_x_discrete(labels = legend_labels)+
  xlab("barrier")+
  ylab("percentage of issues reported in barrier category")+
  ggtitle(paste("Top 5 Reported Barriers by Carnegie Classification\n n=",overall_n))+
  geom_bar(stat="identity")+
ggsave("./output_plots/analysis_of_barriers_q38_by_carnegie_q21/top5__barrier_distribution_by_carnegie_classification.png")


#plot signifigant (by Chi test) distibutions of barriers by carnegie classification
#create plot showing the top five barriers and percentage of institution types reporting
positions = c("Q38_scored_sub_Assoc.1.28.",
              "Q38_scored_sub_Bacca.1.28.",
              "Q38_scored_sub_Master.1.28.",
              "Q38_scored_sub_Doc.1.28.")
categories = c (paste("Associates, Q38 n(r)=",Q38_Q21_associates_responses), 
                paste("Baccalaureate, Q38 n(r)=",Q38_Q21_baccalaureate_responses),
                paste("Masters, Q38 n(r)=",Q38_Q21_masters_responses),
                paste("Doctoral, Q38 n(r)=", Q38_Q21_doctoral_responses))
legend_labels = c("Integration into Course Structure", 
                  "Lack of Curriculum Control", 
                  "Access to Introductory Content", 
                  "Student Peperation")
Q38_biggest_barriers_df %>%
  filter(Var2 == "Q38_Curric.Issues...Does.not.Fit.into.current.Current.Course.Structure"| 
         Var2 == "Q38_Curric.Issues...Lack.of.Curric.Control.not.in.curent.Curric."| 
         Var2 == "Q38_Resources...Access.to.Approp.Introductory.Content"| 
         Var2 == "Q38_Student.Issues...UG.Students.Lack.Approp.Background.Knowledge")%>%
  arrange(Var1)%>%
  ggplot()+
  aes(x=Var2, y=percentage, fill=Var1 )+
  scale_fill_discrete(name="Institution Types", labels = categories)+
  scale_x_discrete(labels = legend_labels)+
  xlab("barrier")+
  ylab("percentage of issues reported in barrier category")+
  ggtitle(paste("Signifigantly Different Barriers by Carnegie Classification\n n=",overall_n))+
  geom_bar(stat="identity")
ggsave("./output_plots/analysis_of_barriers_q38_by_carnegie_q21/sig_different_barrier_distribution_by_carnegie_classification.png")


#Plot as absolute proportional value of respondants
positions = c("Q38_scored_sub_Assoc.1.28.",
              "Q38_scored_sub_Bacca.1.28.",
              "Q38_scored_sub_Master.1.28.",
              "Q38_scored_sub_Doc.1.28.")
categories = c (paste("Associates, Q38 n(r)=",Q38_Q21_associates_responses), 
                paste("Baccalaureate, Q38 n(r)=",Q38_Q21_baccalaureate_responses),
                paste("Masters, Q38 n(r)=",Q38_Q21_masters_responses),
                paste("Doctoral, Q38 n(r)=", Q38_Q21_doctoral_responses))
legend_labels = c("Integration into Course Structure", 
                  "Lack of Curriculum Control", 
                  "Access to Introductory Content", 
                  "Student Peperation")
Q38_biggest_barriers_df_w_prop %>%
  filter(Var2 == "Q38_Curric.Issues...Does.not.Fit.into.current.Current.Course.Structure"| 
           Var2 == "Q38_Curric.Issues...Lack.of.Curric.Control.not.in.curent.Curric."| 
           Var2 == "Q38_Resources...Access.to.Approp.Introductory.Content"| 
           Var2 == "Q38_Student.Issues...UG.Students.Lack.Approp.Background.Knowledge")%>%
  arrange(Var1)%>%
  ggplot()+
  aes(x=Var2, y=proportion, fill=Var1 )+
  scale_fill_discrete(name="Institution Types", labels = categories)+
  scale_x_discrete(labels = legend_labels)+
  xlab("barrier")+
  ylab("percentage of issues reported in barrier category")+
  ggtitle(paste("Signifigantly Different Barriers by Carnegie Classification\n n=",overall_n))+
  geom_bar(stat="identity", position = "dodge")


  
#Generate a sum by carnegie catagory for each of the barrier summed super-catagories
q38_Assoc_var_summed <- df%>%
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


q38_Bacca_var_summed <- df%>%
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

q38_Master_var_summed <- df%>%
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

q38_Doc_var_summed<- df%>%
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
q21_q38_summed_df <- data.frame(q38_Assoc_var_summed[1:8],
                                q38_Bacca_var_summed[1:8],
                                q38_Doc_var_summed[1:8],
                                q38_Master_var_summed[1:8])


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

positions = c("q38_Assoc_var_summed.1.8.",
              "q38_Bacca_var_summed.1.8.",
              "q38_Master_var_summed.1.8.",
              "q38_Doc_var_summed.1.8.")
categories = c (paste("Associates, Q38 n(r)=",Q38_Q21_associates_responses), 
                paste("Baccalaureate, Q38 n(r)=",Q38_Q21_baccalaureate_responses),
                paste("Masters, Q38 n(r)=",Q38_Q21_masters_responses),
                paste("Doctoral, Q38 n(r)=", Q38_Q21_doctoral_responses))
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
q38_Assoc_var_reduced <- df%>%
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


q38_Bacca_var_reduced <- df%>%
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

q38_Master_var_reduced <- df%>%
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

q38_Doc_var_reduced<- df%>%
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
q21_q38_reduced_df <- data.frame(q38_Assoc_var_reduced[1:8],
                                 q38_Bacca_var_reduced[1:8],
                                 q38_Doc_var_reduced[1:8],
                                 q38_Master_var_reduced[1:8])

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

positions = c("q38_Assoc_var_reduced.1.8.",
              "q38_Bacca_var_reduced.1.8.",
              "q38_Master_var_reduced.1.8.",
              "q38_Doc_var_reduced.1.8.")
categories = c (paste("Associates, Q38 n(r)=",Q38_Q21_associates_responses), 
                paste("Baccalaureate, Q38 n(r)=",Q38_Q21_baccalaureate_responses),
                paste("Masters, Q38 n(r)=",Q38_Q21_masters_responses),
                paste("Doctoral, Q38 n(r)=", Q38_Q21_doctoral_responses))
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


#calculate correlation plots

#correlation summed
q38_cor_summed <- cor(q21_q38_summed_df)
png(height=1200, width=1200, pointsize=25, file="./output_plots/analysis_of_barriers_q38_by_carnegie_q21/test_corr_plot.png")
corrplot(cor_summed, 
         order = "hclust", 
         tl.srt=45)
dev.off()

#Plots and analysis of barriers (q06) by carnegie classification (q21)

#Create folders and start a ReadMe for this analysis
dir.create("./output_tables/analysis_of_barriers_q06_by_carnegie_q21/", recursive = TRUE)
dir.create("./output_plots/analysis_of_barriers_q06_by_carnegie_q21/", recursive = TRUE)
readme <- "./output_tables/analysis_of_barriers_q06_by_carnegie_q21/ReadMe.md"
write("#ReadMe",readme)
write(R.version.string, readme, append = TRUE)


#calculate number of responses (Q06) in each catagory (Q21)

Q06_Q21_associates_responses <- df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College" &
           !is.na(df$Q6_Optional..Please.describe.briefly..include.any.barriers.to.development.and.or.implementation.))%>%
  count()
Q06_Q21_associates_responses <- Q06_Q21_associates_responses$n

Q06_Q21_baccalaureate_responses <- df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College" &
           !is.na(df$Q6_Optional..Please.describe.briefly..include.any.barriers.to.development.and.or.implementation.))%>%
  count()
Q06_Q21_baccalaureate_responses  <- Q06_Q21_baccalaureate_responses $n

Q06_Q21_masters_responses <- df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)" &
           !is.na(df$Q6_Optional..Please.describe.briefly..include.any.barriers.to.development.and.or.implementation.))%>%
  count()
Q06_Q21_masters_responses <- Q06_Q21_masters_responses$n

Q06_Q21_doctoral_responses <- df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)" &
           !is.na(df$Q6_Optional..Please.describe.briefly..include.any.barriers.to.development.and.or.implementation.))%>%
  count()
Q06_Q21_doctoral_responses <- Q06_Q21_doctoral_responses$n




#Generate summary stats for raw scored sub-catagories

Q06_scored_sub_catagories <- df%>%
    select(Q06_Faculty.Issue...No.Expertise..Training,
           Q06_Faculty.Issue...Time, 
           Q06_Faculty.Issue...Not.enough.Faculty.members, 
           Q06_Faculty.Issues...Interest.in.Topic, 
           Q06_Curriculum.Issue...Course.Load.Full..No.time.space.for.Content, 
           Q06_Curric.Isues..Curric.Revision.needed, 
           Q06_Curric.Isues...Bioinf.approaches.need.to.be.taugh.at.every.level.integrate.current.classes, 
           Q06_Curic.Issues...Bioinf.Program.Courses.under.develop, 
           Q06_Curric.Isues...Not.a.req.for.Life.Sci.Majors..Pre.Med, 
           Q06_Curric.Issue...Class.Size.too.large, 
           Q06_Curric.Issues...Not.enough.classes.seats.available, 
           Q06_Student.issue...Lack.of.Student.interest.in.quant.skills.and.related.courses, 
           Q06_Student.Issues...Student.Background.is.insufficient, 
           Q06_Inst.Dept.Support...Institutional.Inertia, 
           Q06_Inst.Dept.Support...Inter.Departmental.Cooperation, 
           Q06_Inst.Dept.Support...IT.Supprt, 
           Q06_Inst.Dept.Support...State.Regulation,
           Q06_Resource.Issue...Funding, 
           Q06_Resource.Issues...Access.to.Approp.Software..Op.sys, 
           Q06_Resources...Access.to.developed.Bioinf.Lesson.Plans.Bioinf.Curric, 
           Q06_Facilities...Computer.Labs..Laq.Equip.limited.or.not.available, 
           Q06_Facilities...Servers, 
           Q21_What.is.the.Carnegie.classification.of.your.institution.)

Q06_scored_sub_Assoc <- Q06_scored_sub_catagories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Q06_scored_sub_Bacca<- Q06_scored_sub_catagories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Q06_scored_sub_Master<- Q06_scored_sub_catagories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Q06_scored_sub_Doc<- Q06_scored_sub_catagories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Q06_raw_scored_df <- data.frame(Q06_scored_sub_Assoc[1:22],
                                Q06_scored_sub_Bacca[1:22], 
                                Q06_scored_sub_Master[1:22], 
                                Q06_scored_sub_Doc[1:22])


write.csv(Q06_raw_scored_df, file = "./output_tables/analysis_of_barriers_q06_by_carnegie_q21/q06_by_q21_counts_by_sub_categories.csv" )
write("\n## q06_by_q21_counts_by_sub_categories.csv \n contains the sum of responses 
      for all scored sub-categories where respondants indicated their Carnegie classification.\n 
      Users who gave an answer to q06 but did not indicate a Carnegie categories or who were 
      unsure are removed", readme, append = TRUE )


#transpose the dataframe and restore its dataframeness
Q06_transposed_raw_scored_df <- t(Q06_raw_scored_df)
Q06_raw_scored_totals_df <- data.frame(Q06_transposed_raw_scored_df)

# create a df containing the chi-squared values
Q06_raw_scored_totals_chi <- rbind(Q06_raw_scored_totals_df,sapply(Q06_raw_scored_totals_df,
                                                                   chisq.test,
                                                                   simulate.p.value = TRUE)[3,])
rownames(Q06_raw_scored_totals_chi)[5] <- "chiValues"

#generate table of chivalues
Q06_melted_raw_scored_totals_chi <- melt(as.matrix(Q06_raw_scored_totals_chi))
write.csv(Q06_melted_raw_scored_totals_chi,file= "./output_tables/analysis_of_barriers_q06_by_carnegie_q21/q06_by_21_counts_and_chi_barriers_by_sub_catagory.csv")
write("\n## q06_by_21_counts_and_chi_barriers_by_sub_catagory.csv \n contains the sum of responses 
      for all scored sub-categories where respondents indicated their Carnegie classification. \n 
      Users who gave an answer to q06 but did not indicate a Carnegie categories or who were unsure 
      are removed. \n chisq.test from the R stats package; simulate.p.value = TRUE to account for small
      values of n", readme, append = TRUE )

#write a table that outputs sig chivalues for sub-catagories by Carnegie classification
Q06_melted_raw_scored_totals_chi %>%
  filter(Var1 == "chiValues")%>%
  filter(value <= 0.05)%>%
  write.csv(,file= "./output_tables/analysis_of_barriers_q06_by_carnegie_q21/q06_by_21_signifigant_barriers_by_sub_catagory.csv")

write("\n## q06_by_21_signifigant_barriers_by_sub_catagory.csv\n 
      contains chisq.test values from **q06_by_21_signifigant_barriers_by_sub_catagory.csv** that were 
      0.05 or less.", readme, append = TRUE )


#melt the dataframe using reshape - do as matrix to preserve row names
Q06_melted_raw_scored_totals_df <- melt(as.matrix(Q06_raw_scored_totals_df))

#plot the raw score values

positions = c("Q06_scored_sub_Assoc.1.22.",
              "Q06_scored_sub_Bacca.1.22.",
              "Q06_scored_sub_Master.1.22.",
              "Q06_scored_sub_Doc.1.22.")
categories = c (paste("Associates, Q06 n(+r)=",Q06_Q21_associates_responses), 
                paste("Baccalaureate, Q06 n(+r)=",Q06_Q21_baccalaureate_responses),
                paste("Masters, Q06 n(+r)=",Q06_Q21_masters_responses),
                paste("Doctoral, Q06 n(+r)=", Q06_Q21_doctoral_responses))
legend_labels = c("Faculty Issues: Expertise/training", 
                  "Faculty Issues: Time",
                  "Faculty Issues: Too few faculty",
                  "Faculty Issues: Lack of interest",
                  "Curriculum Issues: No space",
                  "Curriculum Issues: Incompatible with current curriculum",
                  "Curriculum Issues: Integration needed at every level",
                  "Curriculum Issues: Bioinformatics curriculum is under development",
                  "Curriculum Issues: Not required for life sci/pre-med majors",
                  "Curriculum Issues: Class size",
                  "Curriculum Issues: Too few seats",
                  "Student Issues: Lack of interest",
                  "Student Issues: Background knowledge",
                  "Institutional: Institutional inertia", 
                  "Institutional: Inter-departmental cooperation", 
                  "Institutional: IT support", 
                  "Institutional: State regulation", 
                  "Resource Issues: Funding", 
                  "Resource Issues: Access to software",
                  "Resource Issues: Access to bioinformatics lesson plans/curricula",
                  "Facilities: Access to computer(s)/labs",
                  "Facilities: Servers")
Q06_melted_raw_scored_totals_df%>%
  ggplot()+
  aes(x= Var1, y = value, fill = Var2)+
  geom_bar(stat="identity", position = "dodge")+
  xlab("Carnegie Classification")+
  ylab("number of issues scored")+
  theme(legend.position="bottom")+
  theme(legend.key=element_blank(), legend.key.size=unit(1,"point"))+
  scale_x_discrete(limits = positions, labels = categories )+
  scale_fill_discrete(name= "Q06 Barriers by Carnegie Classification \n
                      n(r+) = number of individuals commenting",
                      labels = legend_labels)+
  ggtitle(paste("Q21- Barriers (summed sub-catagories) by Carnegie Classification \n n=",overall_n))+
  guides(fill=guide_legend(nrow=20,byrow=TRUE))

ggsave("./output_plots/analysis_of_barriers_q06_by_carnegie_q21/q06_barriers_summed_by_carnegie_classification_sub_cats.png")



#plot the biggest barriers

#Caculate percentages biggest barriers 
Q06_biggest_barriers_df <- Q06_melted_raw_scored_totals_df %>%
  group_by(Var2)%>%
  mutate(summed_score = sum(value))%>%
  mutate(percentage = (value / summed_score) * 100)

#Sort the biggest barriers 
Q06_biggest_barriers_df <- Q06_biggest_barriers_df %>%
  arrange(summed_score, Var2, Var1)

write.csv(Q06_biggest_barriers_df,file= "./output_tables/analysis_of_barriers_q06_by_carnegie_q21/scored_barriers_andpercentages_by_sub_catagory.csv")

# Create a new df only containing unique barriers and their total values

Q06_barriers_counts_df <- Q06_biggest_barriers_df %>%
  distinct(Var2, .keep_all = TRUE)

#plot barriers 
Q06_barriers_counts_df %>%
  arrange(summed_score, Var2)%>%
  ggplot()+
  aes(x= Var2, y=summed_score)+
  geom_bar(stat="identity")

#create plot showing the top five barriers and percentage of institution types reporting
positions = c("Q06_scored_sub_Assoc.1.22.",
              "Q06_scored_sub_Bacca.1.22.",
              "Q06_scored_sub_Master.1.22.",
              "Q06_scored_sub_Doc.1.22.")
categories = c (paste("Associates, Q06 n(+r)=",Q06_Q21_associates_responses), 
                paste("Baccalaureate, Q06 n(+r)=",Q06_Q21_baccalaureate_responses),
                paste("Masters, Q06 n(+r)=",Q06_Q21_masters_responses),
                paste("Doctoral, Q06 n(+r)=", Q06_Q21_doctoral_responses))
legend_labels = c("Faculty Training", 
                  "Faculty Time", 
                  "Full Course Load", 
                  "Integration into Curriculum", 
                  "Student Preperation")
Q06_biggest_barriers_df %>%
  filter(summed_score >=35)%>%
  arrange(Var1)%>%
  ggplot()+
  aes(x=Var2, y=percentage, fill=Var1 )+
  scale_fill_discrete(name="Institution Types", labels = categories)+
  #scale_x_discrete(labels = legend_labels)+
  xlab("barrier")+
  ylab("percentage of issues reported in barrier category")+
  ggtitle(paste("Top 5 Reported Barriers (Q6) by Carnegie Classification\n n=",overall_n))+
  geom_bar(stat="identity")+
  ggsave("./output_plots/analysis_of_barriers_q38_by_carnegie_q21/top5__barrier_distribution_by_carnegie_classification.png")
