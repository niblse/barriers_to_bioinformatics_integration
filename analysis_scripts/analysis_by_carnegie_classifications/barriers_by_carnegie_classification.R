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

#calculate number of responses (Q38) in each category (Q21)

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

#Generate summary stats for raw scored sub-categories

Q38_scored_sub_categories <- df%>%
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

Q38_scored_sub_Assoc <- Q38_scored_sub_categories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Q38_scored_sub_Bacca<- Q38_scored_sub_categories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Q38_scored_sub_Master<- Q38_scored_sub_categories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Q38_scored_sub_Doc<- Q38_scored_sub_categories%>%
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
write.csv(Q38_melted_raw_scored_totals_chi,file= "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/q38_by_21_counts_and_chi_barriers_by_sub_category.csv")
write("\n## q38_by_21_counts_and_chi_barriers_by_sub_category.csv \n contains the sum of responses 
      for all scored sub-categories where respondents indicated their Carnegie classification. \n 
      Users who gave an answer to q38 but did not indicate a Carnegie categories or who were unsure 
      are removed. \n chisq.test from the R stats package; simulate.p.value = TRUE to account for small
      values of n", readme, append = TRUE )

#write a table that outputs sig chivalues for sub-categories by Carnegie classification
Q38_melted_raw_scored_totals_chi %>%
  filter(Var1 == "chiValues")%>%
  filter(value <= 0.05)%>%
  write.csv(,file= "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/q38_by_21_signifigant_barriers_by_sub_category.csv")

write("\n## q38_by_21_signifigant_barriers_by_sub_category.csv\n 
      contains chisq.test values from **q38_by_21_signifigant_barriers_by_sub_category.csv** that were 
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
  ggtitle(paste("Q21- Barriers (summed sub-categories) by Carnegie Classification \n n=",overall_n))+
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

write.csv(Q38_biggest_barriers_df,file= "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/q38_scored_barriers_andpercentages_by_sub_category.csv")


#add proportion calculation to barriers table where porpotion is out of total respondants
# in a category. e.g. if 100 answered 'Associates' to Q21 but 50 left a + comment in Q38
# then 50% of respondants at an Associates institution encountered a barrier

Q38_biggest_barriers_df_w_prop <- Q38_biggest_barriers_df %>%
  mutate(proportion = ifelse(Var1 == "Q38_scored_sub_Assoc.1.28.", value/Q21_associates_responses,
                             ifelse(Q38_biggest_barriers_df$Var1 == "Q38_scored_sub_Bacca.1.28.", value/Q21_baccalaureate_responses,
                                    ifelse(Q38_biggest_barriers_df$Var1 == "Q38_scored_sub_Master.1.28.", value/Q21_masters_responses, 
                                           ifelse(Q38_biggest_barriers_df$Var1 == "Q38_scored_sub_Doc.1.28.", value/Q21_doctoral_responses, NA)))))
  
write.csv(Q38_biggest_barriers_df_w_prop,file= "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/q38_scored_barriers_percentages_andporportons_by_sub_category.csv")



# Create a new df only containing unique barriers and their total values

Q38_barriers_counts_df <- Q38_biggest_barriers_df %>%
  distinct(Var2, .keep_all = TRUE)

#plot barriers 

Q38_barriers_counts_df %>%
  ggplot()+
  aes(x= reorder(Var2,summed_score), y= summed_score)+
  #scale_x_discrete(limits = positions)+
  coord_flip()+
  ylab("number of issues scored")+
  xlab("percived barriers as scored")+
  ggtitle(paste("Q38- Largest percived barriers\n n=",overall_n))+
  geom_bar(stat = "identity")
ggsave("./output_plots/analysis_of_barriers_q38_by_carnegie_q21/Largest_barriers_q38_ordered.png")




#create plot showing the top five barriers and percentage of institution types reporting
positions = c("Q38_scored_sub_Assoc.1.28.",
              "Q38_scored_sub_Bacca.1.28.",
              "Q38_scored_sub_Master.1.28.",
              "Q38__scored_sub_Doc.1.28.")
categories = c (paste("Associates, Q38 n(+r)=",Q38_Q21_associates_responses), 
                paste("Baccalaureate, Q38 n(+r)=",Q38_Q21_baccalaureate_responses),
                paste("Masters, Q38 n(+r)=",Q38_Q21_masters_responses),
                paste("Doctoral, Q38 n(+r)=", Q38_Q21_doctoral_responses))
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
categories = c (paste("Associates, Q38 n(+r)=",Q38_Q21_associates_responses), 
                paste("Baccalaureate, Q38 n(+r)=",Q38_Q21_baccalaureate_responses),
                paste("Masters, Q38 n(+r)=",Q38_Q21_masters_responses),
                paste("Doctoral, Q38 n(+r)=", Q38_Q21_doctoral_responses))
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
categories = c (paste("Associates, Q38 n(+r)=",Q38_Q21_associates_responses), 
                paste("Baccalaureate, Q38 n(+r)=",Q38_Q21_baccalaureate_responses),
                paste("Masters, Q38 n(+r)=",Q38_Q21_masters_responses),
                paste("Doctoral, Q38 n(+r)=", Q38_Q21_doctoral_responses))
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
  ylab("Proportion of inividuals reporting issue in Q38 vs. classification in Q21")+
  ggtitle(paste("Proportional Makeup of Signifigantly Different Barriers by Carnegie Classification\n n=",overall_n))+
  geom_bar(stat="identity", position = "dodge")
ggsave("./output_plots/analysis_of_barriers_q38_by_carnegie_q21/proportional_top5_barrier_distribution_by_carnegie_classification.png")


  
#Generate a sum by carnegie category for each of the barrier summed super-categories
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
write.csv(melted_q21_q38_summed_df_chi,file= "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/q38_by_21_counts_and_chi_barriers_by_summed_super_category.csv")
write("\n## q38_by_21_counts_and_chi_barriers_by_summed_super_category.csv \n contains the sum of responses 
      for all scored super-categories where respondents indicated their Carnegie classification. \n 
      Users who gave an answer to q38 but did not indicate a Carnegie categories or who were unsure 
      are removed. \n chisq.test from the R stats package; simulate.p.value = TRUE to account for small
      values of n", readme, append = TRUE )

#write a table that outputs sig chivalues for super categories by Carnegie classification
melted_q21_q38_summed_df_chi %>%
  filter(Var1 == "chiValues")%>%
  filter(value <= 0.05)%>%
  write.csv(,file= "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/q38_by_21_signifigant_barriers_by_summed_super_category.csv")

write("\n## q38_by_21_signifigant_barriers_by_summed_super_category.csv\n 
      contains chisq.test values from **q38_by_21_counts_and_chi_barriers_by_summed_super_category.csv** that were 
      0.05 or less.", readme, append = TRUE )


#melt the dataframe using reshape - do as matrix to preserve row names
melted_q21_q38_summed_df <- melt(as.matrix(q21_q38_summed_df))

# Plot the barrier totals by summed super categories

positions = c("q38_Assoc_var_summed.1.8.",
              "q38_Bacca_var_summed.1.8.",
              "q38_Master_var_summed.1.8.",
              "q38_Doc_var_summed.1.8.")
categories = c (paste("Associates, Q38 n(+r)=",Q38_Q21_associates_responses), 
                paste("Baccalaureate, Q38 n(+r)=",Q38_Q21_baccalaureate_responses),
                paste("Masters, Q38 n(+r)=",Q38_Q21_masters_responses),
                paste("Doctoral, Q38 n(+r)=", Q38_Q21_doctoral_responses))
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
  ggtitle(paste("Q38- Barriers (summed super-categories) by Carnegie Classification (Q21)\n n=",overall_n))
ggsave("./output_plots/analysis_of_barriers_q38_by_carnegie_q21/q38_barriers_summed_by_carnegie_classification.png")


#Generate a sum by carnegie category for each of the barrier reduced super-categories
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
write.csv(melted_q21_q38_reduced_df_chi,file= "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/q38_by_21_counts_and_chi_barriers_by_reduced_super_category.csv")
write("\n## q38_by_21_counts_and_chi_barriers_by_reduced_super_category.csv \n contains the sum of responses 
      for all scored-and-reduced super-categories where respondents indicated their Carnegie classification. \n 
      Users who gave an answer to q38 but did not indicate a Carnegie categories or who were unsure 
      are removed. \n chisq.test from the R stats package; simulate.p.value = TRUE to account for small
      values of n", readme, append = TRUE )

#write a table that outputs sig chivalues for super categories by Carnegie classification
melted_q21_q38_reduced_df_chi %>%
  filter(Var1 == "chiValues")%>%
  filter(value <= 0.05)%>%
  write.csv(,file= "./output_tables/analysis_of_barriers_q38_by_carnegie_q21/q38_by_21_signifigant_barriers_by_reduced_super_category.csv")

write("\n## q38_by_21_signifigant_barriers_by_reduced_super_category.csv\n 
      contains chisq.test values from **q38_by_21_counts_and_chi_barriers_by_reduced_super_category.csv** that were 
      0.05 or less.", readme, append = TRUE )



#melt the dataframe using reshape - do as matrix to preserve row names
melted_q21_q38_reduced_df <- melt(as.matrix(q21_q38_reduced_df))

positions = c("q38_Assoc_var_reduced.1.8.",
              "q38_Bacca_var_reduced.1.8.",
              "q38_Master_var_reduced.1.8.",
              "q38_Doc_var_reduced.1.8.")
categories = c (paste("Associates, Q38 n(+r)=",Q38_Q21_associates_responses), 
                paste("Baccalaureate, Q38 n(+r)=",Q38_Q21_baccalaureate_responses),
                paste("Masters, Q38 n(+r)=",Q38_Q21_masters_responses),
                paste("Doctoral, Q38 n(+r)=", Q38_Q21_doctoral_responses))
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
  ggtitle(paste("Q21- Barriers (reduced super-categories) by Carnegie Classification\n n=",overall_n))
ggsave("./output_plots/analysis_of_barriers_q38_by_carnegie_q21/q38_barriers_reduced_by_carnegie_classification.png")


#calculate correlation plots

#correlation summed
q38_cor_summed <- cor(q21_q38_summed_df)
png(height=1200, width=1200, pointsize=25, file="./output_plots/analysis_of_barriers_q38_by_carnegie_q21/test_corr_plot.png")
corrplot(q38_cor_summed, 
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


#calculate number of responses (Q06) in each category (Q21)

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




#Generate summary stats for raw scored sub-categories

Q06_scored_sub_categories <- df%>%
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

Q06_scored_sub_Assoc <- Q06_scored_sub_categories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Q06_scored_sub_Bacca<- Q06_scored_sub_categories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Q06_scored_sub_Master<- Q06_scored_sub_categories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Q06_scored_sub_Doc<- Q06_scored_sub_categories%>%
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
write.csv(Q06_melted_raw_scored_totals_chi,file= "./output_tables/analysis_of_barriers_q06_by_carnegie_q21/q06_by_21_counts_and_chi_barriers_by_sub_category.csv")
write("\n## q06_by_21_counts_and_chi_barriers_by_sub_category.csv \n contains the sum of responses 
      for all scored sub-categories where respondents indicated their Carnegie classification. \n 
      Users who gave an answer to q06 but did not indicate a Carnegie categories or who were unsure 
      are removed. \n chisq.test from the R stats package; simulate.p.value = TRUE to account for small
      values of n", readme, append = TRUE )

#write a table that outputs sig chivalues for sub-categories by Carnegie classification
Q06_melted_raw_scored_totals_chi %>%
  filter(Var1 == "chiValues")%>%
  filter(value <= 0.05)%>%
  write.csv(,file= "./output_tables/analysis_of_barriers_q06_by_carnegie_q21/q06_by_21_signifigant_barriers_by_sub_category.csv")

write("\n## q06_by_21_signifigant_barriers_by_sub_category.csv\n 
      contains chisq.test values from **q06_by_21_signifigant_barriers_by_sub_category.csv** that were 
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
  ggtitle(paste("Q21- Barriers (summed sub-categories) by Carnegie Classification \n n=",overall_n))+
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

write.csv(Q06_biggest_barriers_df,file= "./output_tables/analysis_of_barriers_q06_by_carnegie_q21/scored_barriers_andpercentages_by_sub_category.csv")

# Create a new df only containing unique barriers and their total values

Q06_barriers_counts_df <- Q06_biggest_barriers_df %>%
  distinct(Var2, .keep_all = TRUE)

#plot barriers 
Q06_barriers_counts_df %>%
  ggplot()+
  aes(x= reorder(Var2,summed_score), y= summed_score)+
  #scale_x_discrete(limits = positions)+
  coord_flip()+
  ylab("number of issues scored")+
  xlab("percived barriers as scored")+
  ggtitle(paste("Q06- Largest percived barriers\n n=",overall_n))+
  geom_bar(stat = "identity")
ggsave("./output_plots/analysis_of_barriers_q06_by_carnegie_q21/Largest_barriers_q06_ordered.png")









#create plot showing the top five barriers and percentage of institution types reporting
positions = c("Q06_scored_sub_Assoc.1.22.",
              "Q06_scored_sub_Bacca.1.22.",
              "Q06_scored_sub_Master.1.22.",
              "Q06_scored_sub_Doc.1.22.")
categories = c (paste("Associates, Q06 n(+r)=",Q06_Q21_associates_responses), 
                paste("Baccalaureate, Q06 n(+r)=",Q06_Q21_baccalaureate_responses),
                paste("Masters, Q06 n(+r)=",Q06_Q21_masters_responses),
                paste("Doctoral, Q06 n(+r)=", Q06_Q21_doctoral_responses))
legend_labels = c("Faculty training", 
                  "Faculty time", 
                  "Too few faculty", 
                  "Full course load", 
                  "Student Preperation")
Q06_biggest_barriers_df %>%
  filter(summed_score >=35)%>%
  ggplot()+
  aes(x=Var2, y=percentage, fill=Var1 )+
  scale_fill_discrete(name="Institution Types", labels = categories)+
  scale_x_discrete(labels = legend_labels)+
  xlab("barrier")+
  ylab("percentage of issues reported in barrier category")+
  ggtitle(paste("Top 5 Reported Barriers (Q6) by Carnegie Classification\n n=",overall_n))+
  geom_bar(stat="identity")
ggsave("./output_plots/analysis_of_barriers_q06_by_carnegie_q21/top5__barrier_distribution_by_carnegie_classification.png")





#create plot showing the top five barriers by proportion of institution types reporting
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
                  "Too few Faculty members", 
                  "Course load", 
                  "Student Preperation")
Q06_biggest_barriers_df_w_prop %>%
  filter(summed_score >=35)%>%
  ggplot()+
  aes(x=Var2, y=proportion, fill=Var1 )+
  scale_fill_discrete(name="Institution Types", labels = categories)+
  scale_x_discrete(labels = legend_labels)+
  xlab("barrier")+
  ylab("Proportion of inividuals reporting issue in Q38 vs. classification in Q21")+
  ggtitle(paste("Top 5 Reported Barriers (Q6) by Proprotion Carnegie Classification\n n=",overall_n))+
  geom_bar(stat="identity", position = "dodge")
  ggsave("./output_plots/analysis_of_barriers_q06_by_carnegie_q21/proportional_top5__barrier_distribution_by_carnegie_classification.png")









#plot signifigant (by Chi test) distibutions of barriers by carnegie classification
#create plot showing the top five barriers and percentage of institution types reporting
positions = c("Q06_scored_sub_Assoc.1.22.",
              "Q06_scored_sub_Bacca.1.22.",
              "Q06_scored_sub_Master.1.22.",
              "Q06_scored_sub_Doc.1.22.")
categories = c (paste("Associates, Q06 n(+r)=",Q06_Q21_associates_responses), 
                paste("Baccalaureate, Q06 n(+r)=",Q06_Q21_baccalaureate_responses),
                paste("Masters, Q06 n(+r)=",Q06_Q21_masters_responses),
                paste("Doctoral, Q06 n(+r)=", Q06_Q21_doctoral_responses))
legend_labels = c("Faculty expertise/training", 
                  "Faculty time", 
                  "Not enough faculty members")
Q06_biggest_barriers_df %>%
  filter(Var2 == "Q06_Faculty.Issue...No.Expertise..Training"| 
           Var2 == "Q06_Faculty.Issue...Time"| 
           Var2 == "Q06_Faculty.Issue...Not.enough.Faculty.members")%>%
  ggplot()+
  aes(x=Var2, y=percentage, fill=Var1 )+
  scale_fill_discrete(name="Institution Types", labels = categories)+
  scale_x_discrete(labels = legend_labels)+
  xlab("barrier")+
  ylab("percentage of issues reported in barrier category")+
  ggtitle(paste("Signifigantly Different Barriers (Q06) by Carnegie Classification\n n=",overall_n))+
  geom_bar(stat="identity")+
ggsave("./output_plots/analysis_of_barriers_q06_by_carnegie_q21/sig_different_barrier_distribution_by_carnegie_classification.png")










#add proportion calculation to barriers table where porpotion is out of total respondants
# in a category. e.g. if 100 answered 'Associates' to Q21 but 50 left a + comment in Q38
# then 50% of respondants at an Associates institution encountered a barrier

Q06_biggest_barriers_df_w_prop <- Q06_biggest_barriers_df %>%
  mutate(proportion = ifelse(Var1 == "Q06_scored_sub_Assoc.1.22.", value/Q21_associates_responses,
                             ifelse(Q06_biggest_barriers_df$Var1 == "Q06_scored_sub_Bacca.1.22.", value/Q21_baccalaureate_responses,
                                    ifelse(Q06_biggest_barriers_df$Var1 == "Q06_scored_sub_Master.1.22.", value/Q21_masters_responses, 
                                           ifelse(Q06_biggest_barriers_df$Var1 == "Q06_scored_sub_Doc.1.22.", value/Q21_doctoral_responses, NA)))))

write.csv(Q06_biggest_barriers_df_w_prop,file= "./output_tables/analysis_of_barriers_q06_by_carnegie_q21/scored_barriers_percentages_andporportons_by_sub_category.csv")











#Plot as absolute proportional value of respondants
positions = c("Q06_scored_sub_Assoc.1.22.",
              "Q06_scored_sub_Bacca.1.22.",
              "Q06_scored_sub_Master.1.22.",
              "Q06_scored_sub_Doc.1.22.")
categories = c (paste("Associates, Q06 n(+r)=",Q06_Q21_associates_responses), 
                paste("Baccalaureate, Q06 n(+r)=",Q06_Q21_baccalaureate_responses),
                paste("Masters, Q06 n(+r)=",Q06_Q21_masters_responses),
                paste("Doctoral, Q06 n(+r)=", Q06_Q21_doctoral_responses))
legend_labels = c("Faculty expertise/training", 
                  "Faculty time", 
                  "Not enough faculty members")
Q06_biggest_barriers_df_w_prop %>%
  filter(Var2 == "Q06_Faculty.Issue...No.Expertise..Training"| 
           Var2 == "Q06_Faculty.Issue...Time"| 
           Var2 == "Q06_Faculty.Issue...Not.enough.Faculty.members")%>%
  arrange(Var1)%>%
  ggplot()+
  aes(x=Var2, y=proportion, fill=Var1 )+
  scale_fill_discrete(name="Institution Types", labels = categories)+
  scale_x_discrete(labels = legend_labels)+
  xlab("barrier")+
  ylab("Proportion of inividuals reporting issue in Q06 vs. classification in Q21")+
  ggtitle(paste("Proportional Makeup of Signifigantly Different Barriers (Q06) by Carnegie Classification\n n=",overall_n))+
  geom_bar(stat="identity", position = "dodge")
ggsave("./output_plots/analysis_of_barriers_q06_by_carnegie_q21/proportional_sig_barrier_distribution_by_carnegie_classification.png")











#Generate a sum by carnegie category for each of the barrier summed super-categories
q06_Assoc_var_summed <- df%>%
  select(q06_Faculty_issues_sum,
         q06_Curriculum_issues_sum, 
         q06_Student_issues_sum, 
         q06_Institutional_issues_sum, 
         q06_Resource_issues_sum, 
         q06_Facilities_issues_sum,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()


q06_Bacca_var_summed <- df%>%
  select(q06_Faculty_issues_sum,
         q06_Curriculum_issues_sum, 
         q06_Student_issues_sum, 
         q06_Institutional_issues_sum, 
         q06_Resource_issues_sum, 
         q06_Facilities_issues_sum,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

q06_Master_var_summed <- df%>%
  select(q06_Faculty_issues_sum,
         q06_Curriculum_issues_sum, 
         q06_Student_issues_sum, 
         q06_Institutional_issues_sum, 
         q06_Resource_issues_sum, 
         q06_Facilities_issues_sum,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

q06_Doc_var_summed<- df%>%
  select(q06_Faculty_issues_sum,
         q06_Curriculum_issues_sum, 
         q06_Student_issues_sum, 
         q06_Institutional_issues_sum, 
         q06_Resource_issues_sum, 
         q06_Facilities_issues_sum,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

#create a data frame from the sums
q06_q21_summed_df <- data.frame(q06_Assoc_var_summed[1:6],
                                q06_Bacca_var_summed[1:6],
                                q06_Doc_var_summed[1:6],
                                q06_Master_var_summed[1:6])


write.csv(q06_q21_summed_df, file = "./output_tables/analysis_of_barriers_q06_by_carnegie_q21/q06_by_q21_counts_by_summed_super_categories.csv" )
write("\n## q06_by_q21_counts_by_summed_super_categories.csv\n contains the sum of responses 
      for all scored super-categories where respondants indicated their Carnegie classification.\n 
      Users who gave an answer to q06 but did not indicate a Carnegie categories or who were 
      unsure are removed", readme, append = TRUE )



#transpose the dataframe and restore its dataframeness
q06_q21_summed_df <- t(q06_q21_summed_df)
q06_q21_summed_df <- data.frame(q06_q21_summed_df)

# create a df containing the chi-squared values
q21_q06_summed_df_chi <- rbind(q06_q21_summed_df,sapply(q06_q21_summed_df,
                                                        chisq.test,
                                                        simulate.p.value = TRUE)[3,])
rownames(q21_q06_summed_df_chi)[5] <- "chiValues"

#melt the dataframe using reshape - do as matrix to preserve row names
melted_q21_q06_summed_df_chi <- melt(as.matrix(q21_q06_summed_df_chi))
write.csv(melted_q21_q06_summed_df_chi,file= "./output_tables/analysis_of_barriers_q06_by_carnegie_q21/q06_by_21_counts_and_chi_barriers_by_summed_super_category.csv")
write("\n## q06_by_21_counts_and_chi_barriers_by_summed_super_category.csv \n contains the sum of responses 
      for all scored super-categories where respondents indicated their Carnegie classification. \n 
      Users who gave an answer to q38 but did not indicate a Carnegie categories or who were unsure 
      are removed. \n chisq.test from the R stats package; simulate.p.value = TRUE to account for small
      values of n", readme, append = TRUE )

#write a table that outputs sig chivalues for super categories by Carnegie classification
melted_q21_q06_summed_df_chi %>%
  filter(Var1 == "chiValues")%>%
  filter(value <= 0.05)%>%
  write.csv(,file= "./output_tables/analysis_of_barriers_q06_by_carnegie_q21/q06_by_21_signifigant_barriers_by_summed_super_category.csv")

write("\n## q06_by_21_signifigant_barriers_by_summed_super_category.csv\n 
      contains chisq.test values from **q38_by_21_counts_and_chi_barriers_by_super_category.csv** that were 
      0.05 or less.", readme, append = TRUE )


#melt the dataframe using reshape - do as matrix to preserve row names
melted_q21_q06_summed_df <- melt(as.matrix(q06_q21_summed_df))

# Plot the barrier totals by summed super categories

positions = c("q06_Assoc_var_summed.1.6.",
              "q06_Bacca_var_summed.1.6.",
              "q06_Master_var_summed.1.6.",
              "q06_Doc_var_summed.1.6.")
categories = c (paste("Associates, Q06 n(+r)=",Q06_Q21_associates_responses), 
                paste("Baccalaureate, Q06 n(+r)=",Q06_Q21_baccalaureate_responses),
                paste("Masters, Q06 n(+r)=",Q06_Q21_masters_responses),
                paste("Doctoral, Q06 n(+r)=", Q06_Q21_doctoral_responses))
legend_labels = c ("Faculty Issues (summed)",
                   "Curriculum Issues (summed)",
                   "Student Issues (summed)",
                   "Institutional Issues (summed)",
                   "Resource Issues (summed)",
                   "Facilities Issues (summed)")
melted_q21_q06_summed_df%>%
  ggplot()+
  aes(x= Var1, y = value, fill = Var2)+
  geom_bar(stat="identity", position = "dodge")+
  scale_x_discrete(limits = positions, labels = categories )+
  scale_fill_discrete(name="Q06 Barrier Classifications", labels = legend_labels)+
  xlab("Carnegie Classification")+
  ylab("number of issues scored")+
  ggtitle(paste("Q06- Barriers (summed super-categories) by Carnegie Classification (Q21)\n n=",overall_n))
ggsave("./output_plots/analysis_of_barriers_q06_by_carnegie_q21/q06_barriers_summed_by_carnegie_classification.png")



#Generate a sum by carnegie category for each of the barrier reduced super-categories
q06_Assoc_var_reduced <- df%>%
  select(q06_Faculty_issues_reduced,
         q06_Curriculum_issues_reduced, 
         q06_Student_issues_reduced, 
         q06_Institutional_issues_reduced, 
         q06_Resource_issues_reduced, 
         q06_Facilities_issues_reduced,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()


q06_Bacca_var_reduced <- df%>%
  select(q06_Faculty_issues_reduced,
         q06_Curriculum_issues_reduced, 
         q06_Student_issues_reduced, 
         q06_Institutional_issues_reduced, 
         q06_Resource_issues_reduced, 
         q06_Facilities_issues_reduced,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

q06_Master_var_reduced <- df%>%
  select(q06_Faculty_issues_reduced,
         q06_Curriculum_issues_reduced, 
         q06_Student_issues_reduced, 
         q06_Institutional_issues_reduced, 
         q06_Resource_issues_reduced, 
         q06_Facilities_issues_reduced,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

q06_Doc_var_reduced<- df%>%
  select(q06_Faculty_issues_reduced,
         q06_Curriculum_issues_reduced, 
         q06_Student_issues_reduced, 
         q06_Institutional_issues_reduced, 
         q06_Resource_issues_reduced, 
         q06_Facilities_issues_reduced,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

#create a data frame from the sums
q06_q21_reduced_df <- data.frame(q06_Assoc_var_reduced[1:6],
                                q06_Bacca_var_reduced[1:6],
                                q06_Doc_var_reduced[1:6],
                                q06_Master_var_reduced[1:6])


write.csv(q06_q21_reduced_df, file = "./output_tables/analysis_of_barriers_q06_by_carnegie_q21/q06_by_q21_counts_by_reduced_super_categories.csv" )
write("\n## q06_by_q21_counts_by_reduced_super_categories.csv\n contains the sum of responses 
      for all scored super-categories where respondants indicated their Carnegie classification.\n 
      Users who gave an answer to q06 but did not indicate a Carnegie categories or who were 
      unsure are removed", readme, append = TRUE )

#transpose the dataframe and restore its dataframeness
q06_q21_reduced_df <- t(q06_q21_reduced_df)
q06_q21_reduced_df <- data.frame(q06_q21_reduced_df)


# create a df containing the chi-squared values
q06_q21_reduced_df_chi <- rbind(q06_q21_reduced_df,sapply(q06_q21_reduced_df,
                                                          chisq.test,
                                                          simulate.p.value = TRUE)[3,])
rownames(q06_q21_reduced_df_chi)[5] <- "chiValues"

#melt the dataframe using reshape - do as matrix to preserve row names
melted_q06_q21_reduced_df_chi<- melt(as.matrix(q06_q21_reduced_df_chi))
write.csv(melted_q06_q21_reduced_df_chi,file= "./output_tables/analysis_of_barriers_q06_by_carnegie_q21/q06_by_21_counts_and_chi_barriers_by_reduced_super_category.csv")
write("\n## q06_by_21_counts_and_chi_barriers_by_reduced_super_category.csv \n contains the sum of responses 
      for all scored-and-reduced super-categories where respondents indicated their Carnegie classification. \n 
      Users who gave an answer to q06 but did not indicate a Carnegie categories or who were unsure 
      are removed. \n chisq.test from the R stats package; simulate.p.value = TRUE to account for small
      values of n", readme, append = TRUE )

#write a table that outputs sig chivalues for super categories by Carnegie classification
melted_q06_q21_reduced_df_chi %>%
  filter(Var1 == "chiValues")%>%
  filter(value <= 0.05)%>%
  write.csv(,file= "./output_tables/analysis_of_barriers_q06_by_carnegie_q21/q06_by_21_signifigant_barriers_by_reduced_super_category.csv")

write("\n## q06_by_21_signifigant_barriers_by_reduced_super_category.csv\n 
      contains chisq.test values from **q06_by_21_counts_and_chi_barriers_by_reduced_super_category.csv** that were 
      0.05 or less.", readme, append = TRUE )



#melt the dataframe using reshape - do as matrix to preserve row names
melted_q06_q21_reduced_df <- melt(as.matrix(q06_q21_reduced_df))

positions = c("q06_Assoc_var_reduced.1.6.",
              "q06_Bacca_var_reduced.1.6.",
              "q06_Master_var_reduced.1.6.",
              "q06_Doc_var_reduced.1.6.")
categories = c (paste("Associates, Q06 n(+r)=",Q06_Q21_associates_responses), 
                paste("Baccalaureate, Q06 n(+r)=",Q06_Q21_baccalaureate_responses),
                paste("Masters, Q06 n(+r)=",Q06_Q21_masters_responses),
                paste("Doctoral, Q06 n(+r)=", Q06_Q21_doctoral_responses))
legend_labels = c ("Faculty Issues (reduced)",
                   "Curriculum Issues (reduced)",
                   "Student Issues (reduced)",
                   "Institutional Issues (reduced)",
                   "Resource Issues (reduced)",
                   "Facilities Issues (reduced)")
melted_q06_q21_reduced_df%>%
  ggplot()+
  aes(x= Var1, y = value, fill = Var2)+
  geom_bar(stat="identity", position = "dodge")+
  scale_x_discrete(limits = positions, labels = categories )+
  xlab("Carnegie Classification")+
  ylab("number of issues scored")+
  scale_fill_discrete(name="Q38 Barrier Classifications", labels = legend_labels)+
  ggtitle(paste("Q06- Barriers (reduced super-categories) by Carnegie Classification\n n=",overall_n))
ggsave("./output_plots/analysis_of_barriers_q06_by_carnegie_q21/q06_barriers_reduced_by_carnegie_classification.png")








#Plots and analysis of technical barriers (q29_30) by carnegie classification (q21)

#Create folders and start a ReadMe for this analysis
dir.create("./output_tables/analysis_of_barriers_q29-30_by_carnegie_q21/", recursive = TRUE)
dir.create("./output_plots/analysis_of_barriers_q29-30_by_carnegie_q21/", recursive = TRUE)
readme <- "./output_tables/analysis_of_barriers_q29-30_by_carnegie_q21/ReadMe.md"
write("#ReadMe",readme)
write(R.version.string, readme, append = TRUE)


#calculate number of responses (Q29-30) in each category (Q21)

Q29_Q21_associates_responses <- df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College" &
           !is.na(df$Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g.....))%>%
  count()
Q29_Q21_associates_responses <- Q29_Q21_associates_responses$n

Q29_Q21_baccalaureate_responses <- df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College" &
           !is.na(df$Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g.....))%>%
  count()
Q29_Q21_baccalaureate_responses  <- Q29_Q21_baccalaureate_responses $n

Q29_Q21_masters_responses <- df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)" &
           !is.na(df$Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g.....))%>%
  count()
Q29_Q21_masters_responses <- Q29_Q21_masters_responses$n

Q29_Q21_doctoral_responses <- df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)" &
           !is.na(df$Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g.....))%>%
  count()
Q29_Q21_doctoral_responses <- Q29_Q21_doctoral_responses$n



#analyze Q29 yes/no responses - gather into df by carnegie classification

Q29_Assoc_techbarrier_y_df <- df%>%
  select(Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g.....,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  filter(Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g..... == "1_Yes")%>%
  count()
Q29_Assoc_techbarrier_n_df <- df%>%
  select(Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g.....,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  filter(Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g..... == "2_No")%>%
  count()

Q29_Bacca_techbarrier_y_df <- df%>%
  select(Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g.....,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College")%>%
  filter(Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g..... == "1_Yes")%>%
  count()
Q29_Bacca_techbarrier_n_df <- df%>%
  select(Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g.....,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College")%>%
  filter(Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g..... == "2_No")%>%
  count()

  
Q29_Master_techbarrier_y_df <- df%>%
  select(Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g.....,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)")%>%
  filter(Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g..... == "1_Yes")%>%
  count()
Q29_Master_techbarrier_n_df <- df%>%
  select(Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g.....,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)")%>%
  filter(Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g..... == "2_No")%>%
  count()
  
  
Q29_Doc_techbarrier_y_df <- df%>%
  select(Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g.....,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)")%>%
  filter(Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g..... == "1_Yes")%>%
  count()
Q29_Doc_techbarrier_n_df <- df%>%
  select(Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g.....,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)")%>%
  filter(Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g..... == "2_No")%>%
  count()

#create a data frame from the yes/no counts

Q29_yes_responses <- c(as.numeric(Q29_Assoc_techbarrier_y_df),
                  as.numeric(Q29_Bacca_techbarrier_y_df),
                  as.numeric(Q29_Master_techbarrier_y_df),
                  as.numeric(Q29_Doc_techbarrier_y_df) )
Q29_no_responses <- c(as.numeric(Q29_Assoc_techbarrier_n_df),
                  as.numeric(Q29_Bacca_techbarrier_n_df),
                  as.numeric(Q29_Master_techbarrier_n_df),
                  as.numeric(Q29_Doc_techbarrier_n_df))
tally_rows <- c("Associates", 
                "Baccalaureate", 
                "Masters", 
                "Doctoral")

Q29_ynresponses_df <- data.frame(Q29_yes_responses, Q29_no_responses, row.names = tally_rows)


#calculate percentages of respondants
#add columns for percentages
#manually (!) calculate percentages for by rows/columns

Q29_ynresponses_df$Q29_percent_yes_responses <- NA
Q29_ynresponses_df$Q29_percent_no_responses <- NA
Q29_ynresponses_df[1,3] <- Q29_ynresponses_df[1,1]/Q29_Q21_associates_responses
Q29_ynresponses_df[1,4] <- Q29_ynresponses_df[1,2]/Q29_Q21_associates_responses
Q29_ynresponses_df[2,3] <- Q29_ynresponses_df[2,1]/Q29_Q21_baccalaureate_responses
Q29_ynresponses_df[2,4] <- Q29_ynresponses_df[2,2]/Q29_Q21_baccalaureate_responses
Q29_ynresponses_df[3,3] <- Q29_ynresponses_df[3,1]/Q29_Q21_masters_responses
Q29_ynresponses_df[3,4] <- Q29_ynresponses_df[3,2]/Q29_Q21_masters_responses
Q29_ynresponses_df[4,3] <- Q29_ynresponses_df[4,1]/Q29_Q21_doctoral_responses
Q29_ynresponses_df[4,4] <- Q29_ynresponses_df[4,2]/Q29_Q21_doctoral_responses

write.csv(Q29_ynresponses_df, file="./output_tables/analysis_of_barriers_q29-30_by_carnegie_q21/Q29_response_table.csv")

#plot Q29 responses
positions <- c("Associates", 
                "Baccalaureate", 
                "Masters", 
                "Doctoral")
categories = c (paste("Associates, Q29 n(+r)=",sum(Q29_ynresponses_df[1,1:2])), 
                paste("Baccalaureate, Q29 n(+r)=",sum(Q29_ynresponses_df[2,1:2])),
                paste("Masters, Q29 n(+r)=",sum(Q29_ynresponses_df[3,1:2])),
                paste("Doctoral, Q29 n(+r)=", sum(Q29_ynresponses_df[4,1:2])))
legend_labels = c ("Yes",
                   "No")
melt(as.matrix(Q29_ynresponses_df))%>%
  filter(Var2 == "Q29_percent_yes_responses" | Var2 == "Q29_percent_no_responses")%>%
  ggplot()+
  aes(x=Var1, y=value, fill=Var2)+
  scale_x_discrete(limits = positions, labels = categories )+
  xlab("Carnegie classification")+
  ylab("percentages of responses in institutional category")+
  scale_fill_discrete(name="Response", labels = legend_labels)+
  geom_bar(stat="identity", position = "dodge")+
  ggtitle(paste("At your current institution, do you face any technical barriers in teaching bioinformatics? \n",
subtitle="Q29- Technical Barriers by Carnegie Classification n=",overall_n))
ggsave("./output_plots/analysis_of_barriers_q29-30_by_carnegie_q21/q21_facing_technical_barriers_by_carnegie_classification.png")




#question 30

#calculate number of responses by category

Q30_Q21_associates_responses <- df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College" &
           !is.na(df$Q30_Optional..Please.describe.))%>%
  count()
Q30_Q21_associates_responses <- Q30_Q21_associates_responses$n

Q30_Q21_baccalaureate_responses  <- df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College" &
           !is.na(df$Q30_Optional..Please.describe.))%>%
  count()
Q30_Q21_baccalaureate_responses  <- Q30_Q21_baccalaureate_responses $n

Q30_Q21_masters_responses <- df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)" &
           !is.na(df$Q30_Optional..Please.describe.))%>%
  count()
Q30_Q21_masters_responses <- Q30_Q21_masters_responses$n

Q30_Q21_doctoral_responses <- df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)" &
           !is.na(df$Q30_Optional..Please.describe.))%>%
  count()
Q30_Q21_doctoral_responses <- Q30_Q21_doctoral_responses$n

#Carnegie Data Frame

#Generate summary stats for raw scored sub-categories


Q30_scored_sub_categories <- df%>%
  select(Q29.30_Faculty...No.Expertise.Training, 
         Q29.30_Faculty...Time, 
         Q29.30_Faculty...not.interested.in.topic, 
         Q29.30_Faculty...no.computer.sci.Faculty, 
         Q29.30_Facilities...Computer.Labs.limited.or.not.available, 
         Q29.30_Facilities...Computers.are.too.old..inadequate, 
         Q29.30_Facilities...Servers, 
         Q29.30_Facilities...Internet.Access.Limited, 
         Q29.30_Resources...Operating.System.Availability.Issues, 
         Q29.30_Resources...Approp..Software, 
         Q29.30_Resources...no.high.performance.systems.available, 
         Q29.30_Resources...Funding, 
         Q29.30_Inst.Dept.Support...No.IT.support, 
         Q29.30_Inst.Dept.Support...No.Sub.to.Pay.site.Databases, 
         Q29.30_Inst.Dept.Support...Comp.Sci.Dept.Will.not.support.Bioinf, 
         Q29.30_Student.Issues...Access.to.Approp.Software.off.campus, 
         Q29.30_Student.Issues...Basic.Computing.Knowledge, 
         Q29.30_Student.Issues...No.access.to.computers.at.home, 
         Q29.30_Student.Issues...No.interest.in.Bioinf, 
         Q29.30_Curriculum...Need.to.Develop.new.program,
         Q29.30_Curriculum.Class.Size.too.large,
         Q29.30_Curriculum...Access.to.developed.Bioinf.Lesson.Plans.Bioinf.Curric, 
         Q21_What.is.the.Carnegie.classification.of.your.institution.)

Q30_scored_sub_Assoc <- Q30_scored_sub_categories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Q30_scored_sub_Bacca<- Q30_scored_sub_categories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Q30_scored_sub_Master<- Q30_scored_sub_categories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Q30_scored_sub_Doc<- Q30_scored_sub_categories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Q30_raw_scored_df <- data.frame(Q30_scored_sub_Assoc[1:22],
                                Q30_scored_sub_Bacca[1:22], 
                                Q30_scored_sub_Master[1:22], 
                                Q30_scored_sub_Doc[1:22])

write.csv(Q30_raw_scored_df, file = "./output_tables/analysis_of_barriers_q29-30_by_carnegie_q21/q30_by_q21_counts_by_sub_categories.csv" )
write("\n## q30_by_q21_counts_by_sub_categories.csv \n contains the sum of responses 
      for all scored sub-categories where respondants indicated their Carnegie classification.\n 
      Users who gave an answer to q30 but did not indicate a Carnegie categories or who were 
      unsure are removed", readme, append = TRUE )


#Remove overly sparse data (all categories ==0; can't be tested by ChiSquared)

## Move rownames into columns to keep

Q30_raw_scored_df_rownames <- rownames_to_column(Q30_raw_scored_df)%>%
  rename(barrier = rowname)

Q30_raw_scored_df_filtersparse <- Q30_raw_scored_df_rownames%>%
  filter(Q30_scored_sub_Assoc.1.22. > 0 |
           Q30_scored_sub_Bacca.1.22. > 0 |
           Q30_scored_sub_Master.1.22. > 0 |
           Q30_scored_sub_Doc.1.22. > 0)

#Restore barriers to rowname
row.names(Q30_raw_scored_df_filtersparse) <- Q30_raw_scored_df_filtersparse[,1]
Q30_raw_scored_df_filtersparse <- Q30_raw_scored_df_filtersparse%>%
  select(-barrier)

#transpose the dataframe and restore its dataframeness
Q30_transposed_raw_scored_df <- t(Q30_raw_scored_df_filtersparse)
Q30_raw_scored_totals_df <- data.frame(Q30_transposed_raw_scored_df)

# create a df containing the chi-squared values
Q30_raw_scored_totals_chi <- rbind(Q30_raw_scored_totals_df,sapply(Q30_raw_scored_totals_df,
                                                                   chisq.test,
                                                                   simulate.p.value = TRUE)[3,])
rownames(Q30_raw_scored_totals_chi)[5] <- "chiValues"

#generate table of chivalues
Q30_melted_raw_scored_totals_chi <- melt(as.matrix(Q30_raw_scored_totals_chi))
write.csv(Q30_melted_raw_scored_totals_chi,file= "./output_tables/analysis_of_barriers_q29-30_by_carnegie_q21/q30_by_21_counts_and_chi_barriers_by_sub_category.csv")
write("\n## q30_by_21_counts_and_chi_barriers_by_sub_category.csv \n contains the sum of responses 
      for all scored sub-categories where respondents indicated their Carnegie classification. \n 
      Users who gave an answer to q38 but did not indicate a Carnegie categories or who were unsure 
      are removed. Sparse values (any barriers with 0 respondants) were removed \n 
      chisq.test from the R stats package; simulate.p.value = TRUE to account for small
      values of n", readme, append = TRUE )

#write a table that outputs sig chivalues for sub-categories by Carnegie classification
Q30_melted_raw_scored_totals_chi %>%
  filter(Var1 == "chiValues")%>%
  filter(value <= 0.05)%>%
  write.csv(,file= "./output_tables/analysis_of_barriers_q29-30_by_carnegie_q21/q30_by_21_signifigant_barriers_by_sub_category.csv")

write("\n## q30_by_21_signifigant_barriers_by_sub_category.csv\n 
      contains chisq.test values from **q30_by_21_signifigant_barriers_by_sub_category.csv** that were 
      0.05 or less.", readme, append = TRUE )


#melt the dataframe using reshape - do as matrix to preserve row names
Q30_melted_raw_scored_totals_df <- melt(as.matrix(Q30_raw_scored_totals_df))

#plot the raw score values

positions = c("Q30_scored_sub_Assoc.1.22.",
              "Q30_scored_sub_Bacca.1.22.",
              "Q30_scored_sub_Master.1.22.",
              "Q30_scored_sub_Doc.1.22.")
categories = c (paste("Associates, Q30 n(+r)=",Q30_Q21_associates_responses), 
                paste("Baccalaureate, Q30 n(+r)=",Q30_Q21_baccalaureate_responses),
                paste("Masters, Q30 n(+r)=",Q30_Q21_masters_responses),
                paste("Doctoral, Q30 n(+r)=", Q30_Q21_doctoral_responses))
legend_labels = c("Faculty Issues: Expertise/training", 
                  "Faculty Issues: Time",
                  "Faculty Issues: No comp-sci faculty",
                  "Facilities Issues: Access to computer labs",
                  "Facilities Issues: Inadaquate computers",
                  "Facilities Issues: Access to servers",
                  "Facilities Issues: Access to internet",
                  "Resource Issues: Access to operating systems",
                  "Resource Issues: Apropriate software", 
                  "Resource Issues: Access to high-performance computing", 
                  "Resource Issues: Funding", 
                  "Institutional Issues: Access to IT support", 
                  "Institutional Issues: Subscriptions/licenses", 
                  "Institutional Issues: No support from comp-sci faculty",
                  "Student Issues: Access to software off-campus", 
                  "Student Issues: Basic computing knowledge", 
                  "Student Issues: Access to computers Off-campus", 
                  "Curriculum Issues: New program development required", 
                  "Curriculum Issues: Class size", 
                  "Curriculum Issues: Access to Bioinformatics Lesson Plans")

Q30_melted_raw_scored_totals_df%>%
  ggplot()+
  aes(x= Var1, y = value, fill = Var2)+
  geom_bar(stat="identity", position = "dodge")+
  xlab("Carnegie Classification")+
  ylab("number of issues scored")+
  scale_x_discrete(limits = positions, labels = categories )+
  scale_fill_discrete(name= "Q30 Technical Barriers by Carnegie Classification
                     \n
                   n(r+) = number of individuals commenting", labels = legend_labels)+
  ggtitle(paste("Q21- Barriers (summed sub-categories) by Carnegie Classification \n n=",overall_n))+
  theme(legend.position="bottom")+
  theme(legend.key=element_blank(), legend.key.size=unit(1,"point"))+
  guides(fill=guide_legend(nrow=7,byrow=TRUE))

ggsave("./output_plots/analysis_of_barriers_q29-30_by_carnegie_q21/q30_barriers_summed_by_carnegie_classification_sub_cats.png")

#plot the biggest barriers

#Caculate percentages biggest barriers and sort
Q30_biggest_barriers_df <- Q30_melted_raw_scored_totals_df %>%
  group_by(Var2)%>%
  mutate(summed_score = sum(value))%>%
  mutate(percentage = (value / summed_score) * 100) 

#Sort the biggest barriers 
Q30_biggest_barriers_df <- Q30_biggest_barriers_df %>%
  arrange(summed_score, Var2, Var1)

write.csv(Q30_biggest_barriers_df,file= "./output_tables/analysis_of_barriers_q29-30_by_carnegie_q21/q30_scored_barriers_andpercentages_by_sub_category.csv")


#add proportion calculation to barriers table where porpotion is out of total respondants
# in a category. e.g. if 100 answered 'Associates' to Q21 but 50 left a + comment in Q38
# then 50% of respondants at an Associates institution encountered a barrier

Q30_biggest_barriers_df_w_prop <- Q30_biggest_barriers_df %>%
  mutate(proportion = ifelse(Var1 == "Q30_scored_sub_Assoc.1.22.", value/Q21_associates_responses,
                             ifelse(Q30_biggest_barriers_df$Var1 == "Q30_scored_sub_Bacca.1.22.", value/Q21_baccalaureate_responses,
                                    ifelse(Q30_biggest_barriers_df$Var1 == "Q30_scored_sub_Master.1.22.", value/Q21_masters_responses, 
                                           ifelse(Q30_biggest_barriers_df$Var1 == "Q30_scored_sub_Doc.1.22.", value/Q21_doctoral_responses, NA)))))

write.csv(Q30_biggest_barriers_df_w_prop,file= "./output_tables/analysis_of_barriers_q29-30_by_carnegie_q21/q30_scored_barriers_percentages_andporportons_by_sub_category.csv")


# Create a new df only containing unique barriers and their total values

Q30_barriers_counts_df <- Q30_biggest_barriers_df %>%
  distinct(Var2, .keep_all = TRUE)

Q30_barriers_counts_df %>%
  ggplot()+
  aes(x= reorder(Var2,summed_score), y= summed_score)+
  #scale_x_discrete(limits = positions)+
  coord_flip()+
  ylab("number of issues scored")+
  xlab("percived barriers as scored")+
  ggtitle(paste("Q30- Largest percived barriers\n n=",overall_n))+
  geom_bar(stat = "identity")
ggsave("./output_plots/analysis_of_barriers_q29-30_by_carnegie_q21/Largest_barriers_q29-30_ordered.png")



#create plot showing the top five barriers and percentage of institution types reporting
positions = c("Q30_scored_sub_Assoc.1.22.",
              "Q30_scored_sub_Bacca.1.22.",
              "Q30_scored_sub_Master.1.22.",
              "Q30_scored_sub_Doc.1.22.")
categories = c (paste("Associates, Q30 n(+r)=",Q30_Q21_associates_responses), 
                paste("Baccalaureate, Q30 n(+r)=",Q30_Q21_baccalaureate_responses),
                paste("Masters, Q30 n(+r)=",Q30_Q21_masters_responses),
                paste("Doctoral, Q30 n(+r)=", Q30_Q21_doctoral_responses))
legend_labels = c("Access to Computer Labs",
                  "Access to Operating Systems",
                  "Access to Software", 
                  "Access to HPC", 
                  "Lack of IT Support")
Q30_biggest_barriers_df %>%
  filter(summed_score >=41)%>%
  ggplot()+
  aes(x=Var2, y=percentage, fill=Var1 )+
  scale_fill_discrete(name="Institution Types", labels = categories)+
  scale_x_discrete(labels = legend_labels)+
  xlab("barrier")+
  ylab("percentage of issues reported in barrier category")+
  ggtitle(paste("Top 5 Reported Technical Barriers (Q29,30) by Carnegie Classification\n n=",overall_n))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave("./output_plots/analysis_of_barriers_q29-30_by_carnegie_q21/q30_top5__barrier_distribution_by_carnegie_classification.png")


#plot signifigant (by Chi test) distibutions of barriers by carnegie classification
#create plot showing the top five barriers and percentage of institution types reporting
positions = c("Q30_scored_sub_Assoc.1.22.",
              "Q30_scored_sub_Bacca.1.22.",
              "Q30_scored_sub_Master.1.22.",
              "Q30_scored_sub_Doc.1.22.")
categories = c (paste("Associates, Q30 n(+r)=",Q30_Q21_associates_responses), 
                paste("Baccalaureate, Q30 n(+r)=",Q30_Q21_baccalaureate_responses),
                paste("Masters, Q30 n(+r)=",Q30_Q21_masters_responses),
                paste("Doctoral, Q30 n(+r)=", Q30_Q21_doctoral_responses))
legend_labels = c("Access to HPC", 
                  "IT Support", 
                  "Student Access to Computing Off-campus")
Q30_biggest_barriers_df %>%
  filter(Var2 == "Q29.30_Resources...no.high.performance.systems.available"| 
           Var2 == "Q29.30_Inst.Dept.Support...No.IT.support"| 
           Var2 == "Q29.30_Student.Issues...No.access.to.computers.at.home")%>%
  arrange(Var1)%>%
  ggplot()+
  aes(x=Var2, y=percentage, fill=Var1 )+
  scale_fill_discrete(name="Institution Types", labels = categories)+
  scale_x_discrete(labels = legend_labels)+
  xlab("barrier")+
  ylab("percentage of issues reported in barrier category")+
  ggtitle(paste("Signifigantly Different Technical Barriers (Q29,30) by Carnegie Classification\n n=",overall_n))+
  geom_bar(stat="identity")
ggsave("./output_plots/analysis_of_barriers_q29-30_by_carnegie_q21/q30_sig_different_barrier_distribution_by_carnegie_classification.png")


#Plot as absolute proportional value of respondants
positions = c("Q30_scored_sub_Assoc.1.22.",
              "Q30_scored_sub_Bacca.1.22.",
              "Q30_scored_sub_Master.1.22.",
              "Q30_scored_sub_Doc.1.22.")
categories = c (paste("Associates, Q30 n(+r)=",Q30_Q21_associates_responses), 
                paste("Baccalaureate, Q30 n(+r)=",Q30_Q21_baccalaureate_responses),
                paste("Masters, Q30 n(+r)=",Q30_Q21_masters_responses),
                paste("Doctoral, Q30 n(+r)=", Q30_Q21_doctoral_responses))
legend_labels = c("Access to HPC", 
                  "IT Support", 
                  "Student Access to Computing Off-campus")
Q30_biggest_barriers_df_w_prop %>%
  filter(Var2 == "Q29.30_Resources...no.high.performance.systems.available"| 
           Var2 == "Q29.30_Inst.Dept.Support...No.IT.support"| 
           Var2 == "Q29.30_Student.Issues...No.access.to.computers.at.home")%>%
  arrange(Var1)%>%
  ggplot()+
  aes(x=Var2, y=proportion, fill=Var1 )+
  scale_fill_discrete(name="Institution Types", labels = categories)+
  scale_x_discrete(labels = legend_labels)+
  xlab("barrier")+
  ylab("Proportion of inividuals reporting issue in Q29,Q30 vs. classification in Q21")+
  ggtitle(paste("Proportional Makeup of Signifigantly Different Barriers by Carnegie Classification\n n=",overall_n))+
  geom_bar(stat="identity", position = "dodge")
ggsave("./output_plots/analysis_of_barriers_q29-30_by_carnegie_q21/q30_proportional_top5_barrier_distribution_by_carnegie_classification.png")




#Generate a sum by carnegie category for each of the barrier summed super-categories
q29.30_Assoc_var_summed <- df%>%
  select(q29_Faculty_issues_sum,
         q29_Facilities_issues_sum,
         q29_Resources_issues_sum,
         q29_Institutional_issues_sum, 
         q29_Student_issues_sum,
         q29_Curriculum_issues_sum,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()


q29.30_Bacca_var_summed <- df%>%
  select(q29_Faculty_issues_sum,
         q29_Facilities_issues_sum,
         q29_Resources_issues_sum,
         q29_Institutional_issues_sum, 
         q29_Student_issues_sum,
         q29_Curriculum_issues_sum,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

q29.30_Master_var_summed <- df%>%
  select(q29_Faculty_issues_sum,
         q29_Facilities_issues_sum,
         q29_Resources_issues_sum,
         q29_Institutional_issues_sum, 
         q29_Student_issues_sum,
         q29_Curriculum_issues_sum,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

q29.30_Doc_var_summed <- df%>%
  select(q29_Faculty_issues_sum,
         q29_Facilities_issues_sum,
         q29_Resources_issues_sum,
         q29_Institutional_issues_sum, 
         q29_Student_issues_sum,
         q29_Curriculum_issues_sum,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

#create a data frame from the sums
q21_q29.30_summed_df <- data.frame(q29.30_Assoc_var_summed[1:6],
                                q29.30_Bacca_var_summed[1:6],
                                q29.30_Master_var_summed[1:6], 
                                q29.30_Doc_var_summed[1:6])


write.csv(q21_q29.30_summed_df, file = "./output_tables/analysis_of_barriers_q29-30_by_carnegie_q21/q29-30_by_q21_counts_by_summed_super_categories.csv" )
write("\n## q29-30_by_q21_counts_by_summed_super_categories.csv\n contains the sum of responses 
      for all scored super-categories where respondants indicated their Carnegie classification.\n 
      Users who gave an answer to q38 but did not indicate a Carnegie categories or who were 
      unsure are removed", readme, append = TRUE )



#transpose the dataframe and restore its dataframeness
q21_q29.30_summed_df <- t(q21_q29.30_summed_df)
q21_q29.30_summed_df <- data.frame(q21_q29.30_summed_df)

# create a df containing the chi-squared values
q21_q29.30_summed_df_chi <- rbind(q21_q29.30_summed_df,sapply(q21_q29.30_summed_df,chisq.test,simulate.p.value = TRUE)[3,])
rownames(q21_q29.30_summed_df_chi)[5] <- "chiValues"

#melt the dataframe using reshape - do as matrix to preserve row names
melted_q21_q29.30_summed_df_chi <- melt(as.matrix(q21_q29.30_summed_df_chi))
write.csv(melted_q21_q29.30_summed_df_chi,file= "./output_tables/analysis_of_barriers_q29-30_by_carnegie_q21/q29.30_by_21_counts_and_chi_barriers_by_summed_super_category.csv")
write("\n## q29.30_by_21_counts_and_chi_barriers_by_summed_super_category.csv \n contains the sum of responses 
      for all scored super-categories where respondents indicated their Carnegie classification. \n 
      Users who gave an answer to q38 but did not indicate a Carnegie categories or who were unsure 
      are removed. \n chisq.test from the R stats package; simulate.p.value = TRUE to account for small
      values of n", readme, append = TRUE )

#write a table that outputs sig chivalues for super categories by Carnegie classification
melted_q21_q29.30_summed_df_chi %>%
  filter(Var1 == "chiValues")%>%
  filter(value <= 0.05)%>%
  write.csv(,file= "./output_tables/analysis_of_barriers_q29-30_by_carnegie_q21/q29.30_by_21_signifigant_barriers_by_summed_super_category.csv")

write("\n## q29.30_by_21_signifigant_barriers_by_summed_super_category.csv\n 
      contains chisq.test values from **q29.30_by_21_counts_and_chi_barriers_by_super_category.csv** that were 
      0.05 or less.", readme, append = TRUE )


#melt the dataframe using reshape - do as matrix to preserve row names
melted_q21_q29.30_summed_df <- melt(as.matrix(q21_q29.30_summed_df))

# Plot the barrier totals by summed super categories

positions = c("q29.30_Assoc_var_summed.1.6.",
              "q29.30_Bacca_var_summed.1.6.",
              "q29.30_Master_var_summed.1.6.",
              "q29.30_Doc_var_summed.1.6.")
categories = c (paste("Associates, Q30 n(+r)=",Q30_Q21_associates_responses), 
                paste("Baccalaureate, Q30 n(+r)=",Q30_Q21_baccalaureate_responses),
                paste("Masters, Q30 n(+r)=",Q30_Q21_masters_responses),
                paste("Doctoral, Q30 n(+r)=", Q30_Q21_doctoral_responses))
legend_labels = c ("Faculty Issues (summed)",
                   "Facilities Issues (summed)",
                   "Resource Issues (summed)",
                   "Institutional Issues (summed)",
                   "Student Issues (summed)",
                   "Curriculum Issues (summed)")
melted_q21_q29.30_summed_df%>%
  ggplot()+
  aes(x= Var1, y = value, fill = Var2)+
  geom_bar(stat="identity", position = "dodge")+
  scale_x_discrete(limits = positions, labels = categories )+
  scale_fill_discrete(name="Q29,Q30 Barrier Classifications", labels = legend_labels)+
  xlab("Carnegie Classification")+
  ylab("number of issues scored")+
  ggtitle(paste("Q29,Q30- Barriers (summed super-categories) by Carnegie Classification (Q21)\n n=",overall_n))

ggsave("./output_plots/analysis_of_barriers_q29-30_by_carnegie_q21/q29-30_barriers_summed_by_carnegie_classification.png")








#Generate a df by carnegie category for each of the barrier reduced super-categories
q29.30_Assoc_var_reduced <- df%>%
  select(q29_Faculty_issues_reduced,
         q29_Facilities_issues_reduced,
         q29_Resources_issues_reduced,
         q29_Institutional_issues_reduced, 
         q29_Student_issues_reduced,
         q29_Curriculum_issues_reduced,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()


q29.30_Bacca_var_reduced <- df%>%
  select(q29_Faculty_issues_reduced,
         q29_Facilities_issues_reduced,
         q29_Resources_issues_reduced,
         q29_Institutional_issues_reduced, 
         q29_Student_issues_reduced,
         q29_Curriculum_issues_reduced,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

q29.30_Master_var_reduced <- df%>%
  select(q29_Faculty_issues_reduced,
         q29_Facilities_issues_reduced,
         q29_Resources_issues_reduced,
         q29_Institutional_issues_reduced, 
         q29_Student_issues_reduced,
         q29_Curriculum_issues_reduced,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

q29.30_Doc_var_reduced<- df%>%
  select(q29_Faculty_issues_reduced,
         q29_Facilities_issues_reduced,
         q29_Resources_issues_reduced,
         q29_Institutional_issues_reduced, 
         q29_Student_issues_reduced,
         q29_Curriculum_issues_reduced,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

#create a data frame from the sums
q21_q29.30_reduced_df <- data.frame(q29.30_Assoc_var_reduced[1:6],
                                   q29.30_Bacca_var_reduced[1:6],
                                   q29.30_Master_var_reduced[1:6], 
                                   q29.30_Doc_var_reduced[1:6])


write.csv(q21_q29.30_reduced_df, file = "./output_tables/analysis_of_barriers_q29-30_by_carnegie_q21/q29-30_by_q21_counts_by_reduced_super_categories.csv" )
write("\n## q29-30_by_q21_counts_by_reduced_super_categories.csv\n contains the sum of responses 
      for all scored super-categories where respondants indicated their Carnegie classification.\n 
      Users who gave an answer to q38 but did not indicate a Carnegie categories or who were 
      unsure are removed", readme, append = TRUE )



#transpose the dataframe and restore its dataframeness
q21_q29.30_reduced_df <- t(q21_q29.30_reduced_df)
q21_q29.30_reduced_df <- data.frame(q21_q29.30_reduced_df)

# create a df containing the chi-squared values
q21_q29.30_reduced_df_chi <- rbind(q21_q29.30_reduced_df,sapply(q21_q29.30_reduced_df,chisq.test,simulate.p.value = TRUE)[3,])
rownames(q21_q29.30_reduced_df_chi)[5] <- "chiValues"

#melt the dataframe using reshape - do as matrix to preserve row names
melted_q21_q29.30_reduced_df_chi <- melt(as.matrix(q21_q29.30_reduced_df_chi))
write.csv(melted_q21_q29.30_reduced_df_chi,file= "./output_tables/analysis_of_barriers_q29-30_by_carnegie_q21/q29.30_by_21_counts_and_chi_barriers_by_super_category.csv")
write("\n## q29.30_by_21_counts_and_chi_barriers_by_super_category.csv \n contains the sum of responses 
      for all scored super-categories where respondents indicated their Carnegie classification. \n 
      Users who gave an answer to q38 but did not indicate a Carnegie categories or who were unsure 
      are removed. \n chisq.test from the R stats package; simulate.p.value = TRUE to account for small
      values of n", readme, append = TRUE )

#write a table that outputs sig chivalues for super categories by Carnegie classification
melted_q21_q29.30_reduced_df_chi %>%
  filter(Var1 == "chiValues")%>%
  filter(value <= 0.05)%>%
  write.csv(,file= "./output_tables/analysis_of_barriers_q29-30_by_carnegie_q21/q29.30_by_21_signifigant_barriers_by_reduced_super_category.csv")

write("\n## q29.30_by_21_signifigant_barriers_by_reduced_super_category.csv\n 
      contains chisq.test values from **q29.30_by_21_counts_and_chi_barriers_by_reduced_super_category.csv** that were 
      0.05 or less.", readme, append = TRUE )


#melt the dataframe using reshape - do as matrix to preserve row names
melted_q21_q29.30_reduced_df <- melt(as.matrix(q21_q29.30_reduced_df))

# Plot the barrier totals by reduced super categories

positions = c("q29.30_Assoc_var_reduced.1.6.",
              "q29.30_Bacca_var_reduced.1.6.",
              "q29.30_Master_var_reduced.1.6.",
              "q29.30_Doc_var_reduced.1.6.")
categories = c (paste("Associates, Q30 n(+r)=",Q30_Q21_associates_responses), 
                paste("Baccalaureate, Q30 n(+r)=",Q30_Q21_baccalaureate_responses),
                paste("Masters, Q30 n(+r)=",Q30_Q21_masters_responses),
                paste("Doctoral, Q30 n(+r)=", Q30_Q21_doctoral_responses))
legend_labels = c ("Faculty Issues (reduced)",
                   "Facilities Issues (reduced)",
                   "Resource Issues (reduced)",
                   "Institutional Issues (reduced)",
                   "Student Issues (reduced)",
                   "Curriculum Issues (reduced)")
melted_q21_q29.30_reduced_df%>%
  ggplot()+
  aes(x= Var1, y = value, fill = Var2)+
  geom_bar(stat="identity", position = "dodge")+
  scale_x_discrete(limits = positions, labels = categories )+
  scale_fill_discrete(name="Q29,Q30 Barrier Classifications", labels = legend_labels)+
  xlab("Carnegie Classification")+
  ylab("number of issues scored")+
  ggtitle(paste("Q29,Q30- Barriers (reduced super-categories) by Carnegie Classification (Q21)\n n=",overall_n))

ggsave("./output_plots/analysis_of_barriers_q29-30_by_carnegie_q21/q29-30_barriers_reduced_by_carnegie_classification.png")







#question 33

#Create folders and start a ReadMe for this analysis
dir.create("./output_tables/analysis_of_barriers_q33_by_carnegie_q21/", recursive = TRUE)
dir.create("./output_plots/analysis_of_barriers_q33_by_carnegie_q21/", recursive = TRUE)
readme <- "./output_tables/analysis_of_barriers_q33_by_carnegie_q21/ReadMe.md"
write("#ReadMe",readme)
write(R.version.string, readme, append = TRUE)

#calculate number of responses by category

Q33_Q21_associates_responses <- df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College" &
           !is.na(df$Q33_In.your.opinion..what.do.you.think.are.the.most.important.challenges.currently.facing.those.educa...))%>%
  count()
Q33_Q21_associates_responses <- Q33_Q21_associates_responses$n

Q33_Q21_baccalaureate_responses  <- df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College" &
           !is.na(df$Q33_In.your.opinion..what.do.you.think.are.the.most.important.challenges.currently.facing.those.educa...))%>%
  count()
Q33_Q21_baccalaureate_responses  <- Q33_Q21_baccalaureate_responses $n

Q33_Q21_masters_responses <- df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)" &
           !is.na(df$Q33_In.your.opinion..what.do.you.think.are.the.most.important.challenges.currently.facing.those.educa...))%>%
  count()
Q33_Q21_masters_responses <- Q33_Q21_masters_responses$n

Q33_Q21_doctoral_responses <- df%>%
  filter(df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)" &
           !is.na(df$Q33_In.your.opinion..what.do.you.think.are.the.most.important.challenges.currently.facing.those.educa...))%>%
  count()
Q33_Q21_doctoral_responses <- Q33_Q21_doctoral_responses$n


#Carnegie Data Frame

#Generate summary stats for raw scored sub-categories

Q33_scored_sub_categories <- df%>%
  select(Q33_Faculty...No.Expertise.Training, 
         Q33_Facutly...Time,
         Q33_Faculty...Differences.of.opinion, 
         Q33_Faculty...Content.Development, 
         Q33_Faculty...Not.enough.Faculty, 
         Q33_Facilities...Computer.Labs.limited.or.not.available, 
         Q33_Facilities...Computers.are.too.old..inadequate,
         Q33_Resources...Acces.to.Approp..Software, 
         Q33_Resources...Funding..general.,
         Q33_Resources...Funding..software.License.fees., 
         Q33_Student.Issues...Lack.Approp.Background.Knowledge.Skills, 
         Q33_Student.Issues...No.interest.in.Bioinf, 
         Q33_Student.issues...Intimidated.by.topic, 
         Q33_Student.Isses...Multitude.of.varying.student.backgrounds, 
         Q33_Student.Issues...Lack.Basic.Computing.Knowledge, 
         Q33_Student.Issues...Career.prospects, 
         Q33_Curric.Issues...Lack.of.integration.of.maerial, 
         Q33_Curric.Isues...To.much.conent.in.Life.Sci.curric, 
         Q33_Curric.Issues...How.quickly.the.material.tech.changes,
         Q33_Curriculum...Access.to.developed.Bioinf.Lesson.Plans.Bioinf.Curric, 
         Q33_Curric.Issues...Making.Comp.Sci.courses.consistently.relevant, 
         Q33_Curric.Issues...To.much.curric.influence.from.Professional.schools, 
         Q33_Inst.Dept.Support...Inter.Departmental.Cooperation, 
         Q33_Inst.Dept.Support...No.IT.support,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)

Q33_scored_sub_Assoc <- Q33_scored_sub_categories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Q33_scored_sub_Bacca<- Q33_scored_sub_categories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Q33_scored_sub_Master<- Q33_scored_sub_categories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Q33_scored_sub_Doc<- Q33_scored_sub_categories%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

Q33_raw_scored_df <- data.frame(Q33_scored_sub_Assoc[1:24],
                                Q33_scored_sub_Bacca[1:24], 
                                Q33_scored_sub_Master[1:24], 
                                Q33_scored_sub_Doc[1:24])

write.csv(Q33_raw_scored_df, file = "./output_tables/analysis_of_barriers_q33_by_carnegie_q21/q33_by_q21_counts_by_sub_categories.csv" )
write("\n## q33_by_q21_counts_by_sub_categories.csv \n contains the sum of responses 
      for all scored sub-categories where respondants indicated their Carnegie classification.\n 
      Users who gave an answer to q33 but did not indicate a Carnegie categories or who were 
      unsure are removed", readme, append = TRUE )



#transpose the dataframe and restore its dataframeness
Q33_transposed_raw_scored_df <- t(Q33_raw_scored_df)
Q33_raw_scored_totals_df <- data.frame(Q33_transposed_raw_scored_df)

# create a df containing the chi-squared values
Q33_raw_scored_totals_chi <- rbind(Q33_raw_scored_totals_df,sapply(Q33_raw_scored_totals_df,
                                                                   chisq.test,
                                                                   simulate.p.value = TRUE)[3,])
rownames(Q33_raw_scored_totals_chi)[5] <- "chiValues"

#generate table of chivalues
Q33_melted_raw_scored_totals_chi <- melt(as.matrix(Q33_raw_scored_totals_chi))
write.csv(Q33_melted_raw_scored_totals_chi,file= "./output_tables/analysis_of_barriers_q33_by_carnegie_q21/q33_by_21_counts_and_chi_barriers_by_sub_category.csv")
write("\n## q33_by_21_counts_and_chi_barriers_by_sub_category.csv \n contains the sum of responses 
      for all scored sub-categories where respondents indicated their Carnegie classification. \n 
      Users who gave an answer to q33 but did not indicate a Carnegie categories or who were unsure 
      are removed. \n chisq.test from the R stats package; simulate.p.value = TRUE to account for small
      values of n", readme, append = TRUE )

#write a table that outputs sig chivalues for sub-categories by Carnegie classification
Q33_melted_raw_scored_totals_chi %>%
  filter(Var1 == "chiValues")%>%
  filter(value <= 0.05)%>%
  write.csv(,file= "./output_tables/analysis_of_barriers_q33_by_carnegie_q21/q33_by_21_signifigant_barriers_by_sub_category.csv")

write("\n## q33_by_21_signifigant_barriers_by_sub_category.csv\n 
      contains chisq.test values from **q33_by_21_signifigant_barriers_by_sub_category.csv** that were 
      0.05 or less.", readme, append = TRUE )

#melt the dataframe using reshape - do as matrix to preserve row names
Q33_melted_raw_scored_totals_df <- melt(as.matrix(Q33_raw_scored_totals_df))

#plot the raw score values

positions = c("Q33_scored_sub_Assoc.1.24.",
              "Q33_scored_sub_Bacca.1.24.",
              "Q33_scored_sub_Master.1.24.",
              "Q33_scored_sub_Doc.1.24.")
categories = c (paste("Associates, Q33 n(+r)=",Q33_Q21_associates_responses), 
                paste("Baccalaureate, Q33 n(+r)=",Q33_Q21_baccalaureate_responses),
                paste("Masters, Q33 n(+r)=",Q33_Q21_masters_responses),
                paste("Doctoral, Q33 n(+r)=", Q33_Q21_doctoral_responses))
legend_labels = c("Faculty Issues: Expertise/Training", 
                  "Faculty Issues: Time",
                  "Faculty Issues: Differences of opinion",
                  "Faculty Issues: Content development",
                  "Faculty Issues: Too few faculty",
                  "Faculities Issues: Access to computer labs",
                  "Faculities Issues: Inadaquate computers",
                  "Resource Issues: Access to software",
                  "Resource Issues: Funding",
                  "Resource Issues: Software/license fees",
                  "Student Issues: Lack of background skills/knowledge",
                  "Student Issues: Lack of interest",
                  "Student Issues: Intimidated by topic",
                  "Student Issues: Varying student backrounds",
                  "Student Issues: Lack of basic computing skills",
                  "Student Issues: Career prospects",
                  "Curriculum Issues: Lack of integration", 
                  "Curriculum Issues: Too much content", 
                  "Curriculum Issues: Quickly changing technologies", 
                  "Curriculum Issues: Access to bioinformatics lesson plans/curriculum", 
                  "Curriculum Issues: Difficulty establishing relevance", 
                  "Curriculum Issues: Influence from professional schools",
                  "Institutional Issues: Lack of inter-departmental cooperation",
                  "Institutional Issues: Lack of IT support")
Q33_melted_raw_scored_totals_df%>%
  ggplot()+
  aes(x= Var1, y = value, fill = Var2)+
  geom_bar(stat="identity", position = "dodge")+
  xlab("Carnegie Classification")+
  ylab("number of issues scored")+
  scale_x_discrete(limits = positions, labels = categories )+
  scale_fill_discrete(name= "Q33 Barriers by Carnegie Classification
                     \n
                    n(r+) = number of individuals commenting", labels = legend_labels)+
  ggtitle(paste("Q21- Barriers (summed sub-categories) by Carnegie Classification \n n=",overall_n))+
  theme(legend.position="bottom")+
  theme(legend.key=element_blank(), legend.key.size=unit(1,"point"))+
  guides(fill=guide_legend(nrow=12,byrow=TRUE))

ggsave("./output_plots/analysis_of_barriers_q33_by_carnegie_q21/q33_barriers_summed_by_carnegie_classification_sub_cats.png")


#plot the biggest barriers

#Caculate percentages biggest barriers and sort
Q33_biggest_barriers_df <- Q33_melted_raw_scored_totals_df %>%
  group_by(Var2)%>%
  mutate(summed_score = sum(value))%>%
  mutate(percentage = (value / summed_score) * 100) 

#Sort the biggest barriers 
Q33_biggest_barriers_df <- Q33_biggest_barriers_df %>%
  arrange(summed_score, Var2, Var1)

write.csv(Q33_biggest_barriers_df,file= "./output_tables/analysis_of_barriers_q33_by_carnegie_q21/q33_scored_barriers_andpercentages_by_sub_category.csv")


#add proportion calculation to barriers table where porpotion is out of total respondants
# in a category. e.g. if 100 answered 'Associates' to Q21 but 50 left a + comment in Q33
# then 50% of respondants at an Associates institution encountered a barrier

Q33_biggest_barriers_df_w_prop <- Q33_biggest_barriers_df %>%
  mutate(proportion = ifelse(Var1 == "Q33_scored_sub_Assoc.1.24.", value/Q21_associates_responses,
                             ifelse(Q33_biggest_barriers_df$Var1 == "Q33_scored_sub_Bacca.1.24.", value/Q21_baccalaureate_responses,
                                    ifelse(Q33_biggest_barriers_df$Var1 == "Q33_scored_sub_Master.1.24.", value/Q21_masters_responses, 
                                           ifelse(Q33_biggest_barriers_df$Var1 == "Q33_scored_sub_Doc.1.24.", value/Q21_doctoral_responses, NA)))))

write.csv(Q33_biggest_barriers_df_w_prop,file= "./output_tables/analysis_of_barriers_q33_by_carnegie_q21/q33_scored_barriers_percentages_andporportons_by_sub_category.csv")



# Create a new df only containing unique barriers and their total values

Q33_barriers_counts_df <- Q33_biggest_barriers_df %>%
  distinct(Var2, .keep_all = TRUE)%>%
  arrange(desc(summed_score))


#plot barriers


Q33_barriers_counts_df %>%
  ggplot()+
  aes(x= reorder(Var2,summed_score), y= summed_score)+
  #scale_x_discrete(limits = positions)+
  coord_flip()+
  ylab("number of issues scored")+
  xlab("percived barriers as scored")+
  ggtitle(paste("Q33- Largest percived barriers\n n=",overall_n))+
  geom_bar(stat = "identity")
ggsave("./output_plots/analysis_of_barriers_q33_by_carnegie_q21/Largest_barriers_q33_ordered.png")



#create plot showing the top five barriers and percentage of institution types reporting
positions = c("Q33_scored_sub_Assoc.1.24.",
              "Q33_scored_sub_Bacca.1.24.",
              "Q33_scored_sub_Master.1.24.",
              "Q33_scored_sub_Doc.1.24.")
categories = c (paste("Associates, Q33 n(+r)=",Q33_Q21_associates_responses), 
                paste("Baccalaureate, Q33 n(+r)=",Q33_Q21_baccalaureate_responses),
                paste("Masters, Q33 n(+r)=",Q33_Q21_masters_responses),
                paste("Doctoral, Q33 n(+r)=", Q33_Q21_doctoral_responses))
legend_labels = c("Faculty Expertise/training",
                  "Faculty time",
                  "Student background", 
                  "Student interest in Bioinformatics", 
                  "Lack of integration into curriculum")
Q33_biggest_barriers_df %>%
  filter(summed_score >=82)%>%
  ggplot()+
  aes(x=Var2, y=percentage, fill=Var1 )+
  scale_fill_discrete(name="Institution Types", labels = categories)+
  scale_x_discrete(labels = legend_labels)+
  xlab("barrier (unordered)")+
  ylab("percentage of issues reported in barrier category")+
  ggtitle(paste("Top 5 Reported Technical Barriers Q33 by Carnegie Classification\n n=",overall_n))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("./output_plots/analysis_of_barriers_q33_by_carnegie_q21/q33_top5__barrier_distribution_by_carnegie_classification.png")






#plot signifigant (by Chi test) distibutions of barriers by carnegie classification
#create plot showing the top five barriers and percentage of institution types reporting
positions = c("Q33_scored_sub_Assoc.1.24.",
              "Q33_scored_sub_Bacca.1.24.",
              "Q33_scored_sub_Master.1.24.",
              "Q33_scored_sub_Doc.1.24.")
categories = c (paste("Associates, Q33 n(+r)=",Q33_Q21_associates_responses), 
                paste("Baccalaureate, Q33 n(+r)=",Q33_Q21_baccalaureate_responses),
                paste("Masters, Q33 n(+r)=",Q33_Q21_masters_responses),
                paste("Doctoral, Q33 n(+r)=", Q33_Q21_doctoral_responses))
legend_labels = c("Faculty time", 
                  "Too few faculty", 
                  "Lack of student background",
                  "Lack of student interest in bioinformatics", 
                  "Student intimidation", 
                  "Varying student backgrounds", 
                  "Difficulty integrating material", 
                  "Too much content in curriculum", 
                  "Pace of technology change")
Q33_biggest_barriers_df %>%
  filter(Var2 == "Q33_Facutly...Time"| 
           Var2 == "Q33_Faculty...Not.enough.Faculty"| 
           Var2 == "Q33_Student.Issues...Lack.Approp.Background.Knowledge.Skills"|
           Var2 == "Q33_Student.Issues...No.interest.in.Bioinf"|
           Var2 == "Q33_Student.issues...Intimidated.by.topic"|
           Var2 == "Q33_Student.Isses...Multitude.of.varying.student.backgrounds"|
           Var2 == "Q33_Curric.Issues...Lack.of.integration.of.maerial"|
           Var2 == "Q33_Curric.Isues...To.much.conent.in.Life.Sci.curric"|
           Var2 == "Q33_Curric.Issues...How.quickly.the.material.tech.changes"
           )%>%
  arrange(Var1)%>%
  ggplot()+
  aes(x=Var2, y=percentage, fill=Var1 )+
  scale_fill_discrete(name="Institution Types", labels = categories)+
  scale_x_discrete(labels = legend_labels)+
  xlab("barrier")+
  ylab("percentage of issues reported in barrier category")+
  ggtitle(paste("Signifigantly Different Technical Barriers Q33 by Carnegie Classification\n n=",overall_n))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("./output_plots/analysis_of_barriers_q33_by_carnegie_q21/q33_sig_different_barrier_distribution_by_carnegie_classification.png")


#Plot as absolute proportional value of respondants
positions = c("Q33_scored_sub_Assoc.1.24.",
              "Q33_scored_sub_Bacca.1.24.",
              "Q33_scored_sub_Master.1.24.",
              "Q33_scored_sub_Doc.1.24.")
categories = c (paste("Associates, Q33 n(+r)=",Q33_Q21_associates_responses), 
                paste("Baccalaureate, Q33 n(+r)=",Q33_Q21_baccalaureate_responses),
                paste("Masters, Q33 n(+r)=",Q33_Q21_masters_responses),
                paste("Doctoral, Q33 n(+r)=", Q33_Q21_doctoral_responses))
legend_labels = c("Faculty time", 
                  "Too few faculty", 
                  "Lack of student background",
                  "Lack of student interest in bioinformatics", 
                  "Student intimidation", 
                  "Varying student backgrounds", 
                  "Difficulty integrating material", 
                  "Too much content in curriculum", 
                  "Pace of technology change")
Q33_biggest_barriers_df_w_prop %>%
  filter(Var2 == "Q33_Facutly...Time"| 
           Var2 == "Q33_Faculty...Not.enough.Faculty"| 
           Var2 == "Q33_Student.Issues...Lack.Approp.Background.Knowledge.Skills"|
           Var2 == "Q33_Student.Issues...No.interest.in.Bioinf"|
           Var2 == "Q33_Student.issues...Intimidated.by.topic"|
           Var2 == "Q33_Student.Isses...Multitude.of.varying.student.backgrounds"|
           Var2 == "Q33_Curric.Issues...Lack.of.integration.of.maerial"|
           Var2 == "Q33_Curric.Isues...To.much.conent.in.Life.Sci.curric"|
           Var2 == "Q33_Curric.Issues...How.quickly.the.material.tech.changes"
  )%>%
  arrange(Var1)%>%
  ggplot()+
  aes(x=Var2, y=proportion, fill=Var1 )+
  scale_fill_discrete(name="Institution Types", labels = categories)+
  scale_x_discrete(labels = legend_labels)+
  xlab("barrier")+
  ylab("Proportion of inividuals reporting issue in Q29,Q30 vs. classification in Q21")+
  ggtitle(paste("Proportional Makeup of Signifigantly Different Barriers (Q33) by Carnegie Classification\n n=",overall_n))+
  geom_bar(stat="identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("./output_plots/analysis_of_barriers_q33_by_carnegie_q21/q33_proportional_top5_barrier_distribution_by_carnegie_classification.png")




#Generate a sum by carnegie category for each of the barrier summed super-categories
q33_Assoc_var_summed <- df%>%
  select(q33_Faculty_issues_sum,
         q33_Facility_issues_sum,
         q33_Resources_issues_sum,
         q33_Student_issues_sum,
         q33_Curriculum_issues_sum, 
         q33_Institutional_issues_sum,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()


q33_Bacca_var_summed <- df%>%
  select(q33_Faculty_issues_sum,
         q33_Facility_issues_sum,
         q33_Resources_issues_sum,
         q33_Student_issues_sum,
         q33_Curriculum_issues_sum, 
         q33_Institutional_issues_sum,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

q33_Master_var_summed <- df%>%
  select(q33_Faculty_issues_sum,
         q33_Facility_issues_sum,
         q33_Resources_issues_sum,
         q33_Student_issues_sum,
         q33_Curriculum_issues_sum, 
         q33_Institutional_issues_sum,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

q33_Doc_var_summed <- df%>%
  select(q33_Faculty_issues_sum,
         q33_Facility_issues_sum,
         q33_Resources_issues_sum,
         q33_Student_issues_sum,
         q33_Curriculum_issues_sum, 
         q33_Institutional_issues_sum,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

#create a data frame from the sums
q33_q21_summed_df <- data.frame(q33_Assoc_var_summed[1:6],
                                q33_Bacca_var_summed[1:6],
                                q33_Doc_var_summed[1:6],
                                q33_Master_var_summed[1:6])


write.csv(q33_q21_summed_df, file = "./output_tables/analysis_of_barriers_q33_by_carnegie_q21/q33_by_q21_counts_by_summed_super_categories.csv" )
write("\n## q33_by_q21_counts_by_summed_super_categories.csv\n contains the sum of responses 
      for all scored super-categories where respondants indicated their Carnegie classification.\n 
      Users who gave an answer to q33 but did not indicate a Carnegie categories or who were 
      unsure are removed", readme, append = TRUE )



#transpose the dataframe and restore its dataframeness
q33_q21_summed_df <- t(q33_q21_summed_df)
q33_q21_summed_df <- data.frame(q33_q21_summed_df)

# create a df containing the chi-squared values
q21_q33_summed_df_chi <- rbind(q33_q21_summed_df,sapply(q33_q21_summed_df,
                                                        chisq.test,
                                                        simulate.p.value = TRUE)[3,])
rownames(q21_q33_summed_df_chi)[5] <- "chiValues"

#melt the dataframe using reshape - do as matrix to preserve row names
melted_q21_q33_summed_df_chi <- melt(as.matrix(q21_q33_summed_df_chi))
write.csv(melted_q21_q33_summed_df_chi,file= "./output_tables/analysis_of_barriers_q33_by_carnegie_q21/q33_by_21_counts_and_chi_barriers_by_super_category.csv")
write("\n## q33_by_21_counts_and_chi_barriers_by_super_category.csv \n contains the sum of responses 
      for all scored super-categories where respondents indicated their Carnegie classification. \n 
      Users who gave an answer to q38 but did not indicate a Carnegie categories or who were unsure 
      are removed. \n chisq.test from the R stats package; simulate.p.value = TRUE to account for small
      values of n", readme, append = TRUE )

#write a table that outputs sig chivalues for super categories by Carnegie classification
melted_q21_q33_summed_df_chi %>%
  filter(Var1 == "chiValues")%>%
  filter(value <= 0.05)%>%
  write.csv(,file= "./output_tables/analysis_of_barriers_q33_by_carnegie_q21/q33_by_21_signifigant_barriers_by_summed_super_category.csv")

write("\n## q33_by_21_signifigant_barriers_by_summed_super_category.csv\n 
      contains chisq.test values from **q33_by_21_counts_and_chi_barriers_by_super_category.csv** that were 
      0.05 or less.", readme, append = TRUE )


#melt the dataframe using reshape - do as matrix to preserve row names
melted_q21_q33_summed_df <- melt(as.matrix(q33_q21_summed_df))

# Plot the barrier totals by summed super categories

positions = c("q33_Assoc_var_summed.1.6.",
              "q33_Bacca_var_summed.1.6.",
              "q33_Master_var_summed.1.6.",
              "q33_Doc_var_summed.1.6.")
categories = c (paste("Associates, Q33 n(+r)=",Q33_Q21_associates_responses), 
                paste("Baccalaureate, Q33 n(+r)=",Q33_Q21_baccalaureate_responses),
                paste("Masters, Q33 n(+r)=",Q33_Q21_masters_responses),
                paste("Doctoral, Q33 n(+r)=", Q33_Q21_doctoral_responses))
legend_labels = c ("Faculty Issues (summed)",
                   "Facilty Issues (summed)",
                   "Resources Issues (summed)",
                   "Student Issues (summed)",
                   "Curriculum Issues (summed)",
                   "Institutional Issues (summed)")
melted_q21_q33_summed_df%>%
  ggplot()+
  aes(x= Var1, y = value, fill = Var2)+
  geom_bar(stat="identity", position = "dodge")+
  scale_x_discrete(limits = positions, labels = categories )+
  scale_fill_discrete(name="Q33 Barrier Classifications", labels = legend_labels)+
  xlab("Carnegie Classification")+
  ylab("number of issues scored")+
  ggtitle(paste("Q33- Barriers (summed super-categories) by Carnegie Classification (Q21)\n n=",overall_n))
ggsave("./output_plots/analysis_of_barriers_q33_by_carnegie_q21/q33_barriers_summed_by_carnegie_classification.png")


#Generate a sum by carnegie category for each of the barrier reduced super-categories

q33_Assoc_var_reduced <- df%>%
  select(q33_Faculty_issues_reduced,
         q33_Facility_issues_reduced,
         q33_Resources_issues_reduced,
         q33_Student_issues_reduced,
         q33_Curriculum_issues_reduced, 
         q33_Institutional_issues_reduced,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()


q33_Bacca_var_reduced <- df%>%
  select(q33_Faculty_issues_reduced,
         q33_Facility_issues_reduced,
         q33_Resources_issues_reduced,
         q33_Student_issues_reduced,
         q33_Curriculum_issues_reduced, 
         q33_Institutional_issues_reduced,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

q33_Master_var_reduced <- df%>%
  select(q33_Faculty_issues_reduced,
         q33_Facility_issues_reduced,
         q33_Resources_issues_reduced,
         q33_Student_issues_reduced,
         q33_Curriculum_issues_reduced, 
         q33_Institutional_issues_reduced,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

q33_Doc_var_reduced <- df%>%
  select(q33_Faculty_issues_reduced,
         q33_Facility_issues_reduced,
         q33_Resources_issues_reduced,
         q33_Student_issues_reduced,
         q33_Curriculum_issues_reduced, 
         q33_Institutional_issues_reduced,
         Q21_What.is.the.Carnegie.classification.of.your.institution.)%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)")%>%
  mutate_if(is.character,as.numeric)%>%
  colSums()

#create a data frame from the sums
q33_q21_reduced_df <- data.frame(q33_Assoc_var_reduced[1:6],
                                q33_Bacca_var_reduced[1:6],
                                q33_Doc_var_reduced[1:6],
                                q33_Master_var_reduced[1:6])


write.csv(q33_q21_reduced_df, file = "./output_tables/analysis_of_barriers_q33_by_carnegie_q21/q33_by_q21_counts_by_reduced_super_categories.csv" )
write("\n## q33_by_q21_counts_by_reduced_super_categories.csv\n contains the sum of responses 
      for all scored super-categories where respondants indicated their Carnegie classification.\n 
      Users who gave an answer to q33 but did not indicate a Carnegie categories or who were 
      unsure are removed", readme, append = TRUE )



#transpose the dataframe and restore its dataframeness
q33_q21_reduced_df <- t(q33_q21_reduced_df)
q33_q21_reduced_df <- data.frame(q33_q21_reduced_df)

# create a df containing the chi-squared values
q21_q33_reduced_df_chi <- rbind(q33_q21_reduced_df,sapply(q33_q21_reduced_df,
                                                        chisq.test,
                                                        simulate.p.value = TRUE)[3,])
rownames(q21_q33_reduced_df_chi)[5] <- "chiValues"

#melt the dataframe using reshape - do as matrix to preserve row names
melted_q21_q33_reduced_df_chi <- melt(as.matrix(q21_q33_reduced_df_chi))
write.csv(melted_q21_q33_reduced_df_chi,file= "./output_tables/analysis_of_barriers_q33_by_carnegie_q21/q33_by_21_counts_and_chi_barriers_by_reduced_super_category.csv")
write("\n## q33_by_21_counts_and_chi_barriers_by_reduced_super_category.csv \n contains the sum of responses 
      for all scored super-categories where respondents indicated their Carnegie classification. \n 
      Users who gave an answer to q38 but did not indicate a Carnegie categories or who were unsure 
      are removed. \n chisq.test from the R stats package; simulate.p.value = TRUE to account for small
      values of n", readme, append = TRUE )

#write a table that outputs sig chivalues for super categories by Carnegie classification
melted_q21_q33_reduced_df_chi %>%
  filter(Var1 == "chiValues")%>%
  filter(value <= 0.05)%>%
  write.csv(,file= "./output_tables/analysis_of_barriers_q33_by_carnegie_q21/q33_by_21_signifigant_barriers_by_reduced_super_category.csv")

write("\n## q33_by_21_signifigant_barriers_by_reduced_super_category.csv\n 
      contains chisq.test values from **q33_by_21_signifigant_barriers_by_reduced_super_category.csv** that were 
      0.05 or less.", readme, append = TRUE )


#melt the dataframe using reshape - do as matrix to preserve row names
melted_q21_q33_reduced_df <- melt(as.matrix(q33_q21_reduced_df))

# Plot the barrier totals by reduced super categories

positions = c("q33_Assoc_var_reduced.1.6.",
              "q33_Bacca_var_reduced.1.6.",
              "q33_Master_var_reduced.1.6.",
              "q33_Doc_var_reduced.1.6.")
categories = c (paste("Associates, Q33 n(+r)=",Q33_Q21_associates_responses), 
                paste("Baccalaureate, Q33 n(+r)=",Q33_Q21_baccalaureate_responses),
                paste("Masters, Q33 n(+r)=",Q33_Q21_masters_responses),
                paste("Doctoral, Q33 n(+r)=", Q33_Q21_doctoral_responses))
legend_labels = c ("Faculty Issues (reduced)",
                   "Facilty Issues (reduced)",
                   "Resources Issues (reduced)",
                   "Student Issues (reduced)",
                   "Curriculum Issues (reduced)",
                   "Institutional Issues (reduced)")
melted_q21_q33_reduced_df%>%
  ggplot()+
  aes(x= Var1, y = value, fill = Var2)+
  geom_bar(stat="identity", position = "dodge")+
  scale_x_discrete(limits = positions, labels = categories )+
  scale_fill_discrete(name="Q33 Barrier Classifications", labels = legend_labels)+
  xlab("Carnegie Classification")+
  ylab("number of issues scored")+
  ggtitle(paste("Q33- Barriers (reduced super-categories) by Carnegie Classification (Q21)\n n=",overall_n))
ggsave("./output_plots/analysis_of_barriers_q33_by_carnegie_q21/q33_barriers_reduced_by_carnegie_classification.png")
