# load required libraries
require(ggplot2)
require(tidyverse)
require(reshape2)

############ LOAD THE SUMMARIES RESPONSE DATA ###########################################


#read question dataframes and concatenate 


#q6
q21.carnegie.q6 <- read_csv("./barriers_by_carnegie_classification/analysis_of_Q06_barriers_to_implementation_by_Q21_carnegie_classification_/output_tables/tally_of_raw_score_of_Q06_barriers_to_implementation_by_Q21_carnegie_classification_proportions.csv")
q15.race.q6 <- read_csv("./barriers_by_ethnicity/analysis_of_Q06_barriers_to_implementation_by_race_ethnicity_/output_tables/tally_of_raw_score_of_Q06_barriers_to_implementation_by_race_ethnicity_proportions.csv")
urep.q6 <- read_csv("./barriers_by_ethnicity/analysis_of_Q06_barriers_to_implementation_by_STEM_representation_status_/output_tables/tally_of_raw_score_of_Q06_barriers_to_implementation_by_STEM_representation_status_proportions.csv")
q14.sex.q6 <- read_csv("./barriers_by_gender/analysis_of_Q06_barriers_to_implementation_by_gender_/output_tables/tally_of_raw_score_of_Q06_barriers_to_implementation_by_gender_proportions.csv")
q17.degree.q6 <- read_csv("./barriers_by_highest_degree_earned/analysis_of_Q06_barriers_to_implementation_by_Q17_highest_degree_/output_tables/tally_of_raw_score_of_Q06_barriers_to_implementation_by_Q17_highest_degree_proportions.csv")
q22.msi.q6 <- read_csv("./barriers_by_msi/analysis_of_Q06_barriers_to_implementation_by_Q22_msi_/output_tables/tally_of_raw_score_of_Q06_barriers_to_implementation_by_Q22_msi_proportions.csv")
q3.training.q6 <- read_csv("./barriers_by_level_of_bioinformatics_training/analysis_of_Q06_barriers_to_implementation_by_Q3_level_of_bioinformatics_training_/output_tables/tally_of_raw_score_of_Q06_barriers_to_implementation_by_Q3_level_of_bioinformatics_training_proportions.csv")
q24.uenrollment.q6 <- read_csv("./barriers_by_undergraduate_enrollment/analysis_of_Q06_barriers_to_implementation_by_Q24_undergraduate_enrollment_/output_tables/tally_of_raw_score_of_Q06_barriers_to_implementation_by_Q24_undergraduate_enrollment_proportions.csv")

#q30
q21.carnegie.q30 <- read_csv("./barriers_by_carnegie_classification/analysis_of_Q29-30_technical_challenges_by_Q21_carnegie_classification_/output_tables/tally_of_raw_score_of_Q29-30_technical_challenges_by_Q21_carnegie_classification_proportions.csv")
q15.race.q30 <- read_csv("./barriers_by_ethnicity/analysis_of_Q29-30_technical_challenges_by_race_ethnicity_/output_tables/tally_of_raw_score_of_Q29-30_technical_challenges_by_race_ethnicity_proportions.csv")
urep.q30 <- read_csv("./barriers_by_ethnicity/analysis_of_Q29-30_technical_challenges_by_STEM_representation_status_/output_tables/tally_of_raw_score_of_Q29-30_technical_challenges_by_STEM_representation_status_proportions.csv")
q14.sex.q30 <- read_csv("./barriers_by_gender/analysis_of_Q29-30_technical_challenges_by_gender_/output_tables/tally_of_raw_score_of_Q29-30_technical_challenges_by_gender_proportions.csv")
q17.degree.q30 <- read_csv("./barriers_by_highest_degree_earned/analysis_of_Q29-30_technical_challenges_by_Q17_highest_degree_/output_tables/tally_of_raw_score_of_Q29-30_technical_challenges_by_Q17_highest_degree_proportions.csv")
q22.msi.q30 <- read_csv("./barriers_by_msi/analysis_of_Q29-30_technical_challenges_by_Q22_msi_/output_tables/tally_of_raw_score_of_Q29-30_technical_challenges_by_Q22_msi_proportions.csv")
q3.training.q30 <- read_csv("./barriers_by_level_of_bioinformatics_training/analysis_of_Q29-30_technical_challenges_by_Q3_level_of_bioinformatics_training_/output_tables/tally_of_raw_score_of_Q29-30_technical_challenges_by_Q3_level_of_bioinformatics_training_proportions.csv")
q24.uenrollment.q30 <- read_csv("./barriers_by_undergraduate_enrollment/analysis_of_Q29-30_technical_challenges_by_Q24_undergraduate_enrollment_/output_tables/tally_of_raw_score_of_Q29-30_technical_challenges_by_Q24_undergraduate_enrollment_proportions.csv")

#q33                
q21.carnegie.q33 <- read_csv("./barriers_by_carnegie_classification/analysis_of_Q33_educator_challenges_by_Q21_carnegie_classification_/output_tables/tally_of_raw_score_of_Q33_educator_challenges_by_Q21_carnegie_classification_proportions.csv")
q15.race.q33 <- read_csv("./barriers_by_ethnicity/analysis_of_Q33_educator_challenges_by_race_ethnicity_/output_tables/tally_of_raw_score_of_Q33_educator_challenges_by_race_ethnicity_proportions.csv")
urep.q33 <- read_csv("./barriers_by_ethnicity/analysis_of_Q33_educator_challenges_by_STEM_representation_status_/output_tables/tally_of_raw_score_of_Q33_educator_challenges_by_STEM_representation_status_proportions.csv")
q14.sex.q33 <- read_csv("./barriers_by_gender/analysis_of_Q33_educator_challenges_by_gender_/output_tables/tally_of_raw_score_of_Q33_educator_challenges_by_gender_proportions.csv")
q17.degree.q33 <- read_csv("./barriers_by_highest_degree_earned/analysis_of_Q33_educator_challenges_by_Q17_highest_degree_/output_tables/tally_of_raw_score_of_Q33_educator_challenges_by_Q17_highest_degree_proportions.csv")
q22.msi.q33 <- read_csv("./barriers_by_msi/analysis_of_Q33_educator_challenges_by_Q22_msi_/output_tables/tally_of_raw_score_of_Q33_educator_challenges_by_Q22_msi_proportions.csv")
q3.training.q33 <- read_csv("./barriers_by_level_of_bioinformatics_training/analysis_of_Q33_educator_challenges_by_Q3_level_of_bioinformatics_training_/output_tables/tally_of_raw_score_of_Q33_educator_challenges_by_Q3_level_of_bioinformatics_training_proportions.csv")
q24.uenrollment.q33 <- read_csv("./barriers_by_undergraduate_enrollment/analysis_of_Q33_educator_challenges_by_Q24_undergraduate_enrollment_/output_tables/tally_of_raw_score_of_Q33_educator_challenges_by_Q24_undergraduate_enrollment_proportions.csv")

#q38
q21.carnegie.q38 <- read_csv("./barriers_by_carnegie_classification/analysis_of_Q38_barriers_to_inclusion_by_Q21_carnegie_classification_/output_tables/tally_of_raw_score_of_Q38_barriers_to_inclusion_by_Q21_carnegie_classification_proportions.csv")
q15.race.q38 <- read_csv("./barriers_by_ethnicity/analysis_of_Q38_barriers_to_inclusion_by_race_ethnicity_/output_tables/tally_of_raw_score_of_Q38_barriers_to_inclusion_by_race_ethnicity_proportions.csv")
urep.q38 <- read_csv("./barriers_by_ethnicity/analysis_of_Q38_barriers_to_inclusion_by_STEM_representation_status_/output_tables/tally_of_raw_score_of_Q38_barriers_to_inclusion_by_STEM_representation_status_proportions.csv")
q14.sex.q38 <- read_csv("./barriers_by_gender/analysis_of_Q38_barriers_to_inclusion_by_gender_/output_tables/tally_of_raw_score_of_Q38_barriers_to_inclusion_by_gender_proportions.csv")
q17.degree.q38 <- read_csv("./barriers_by_highest_degree_earned/analysis_of_Q38_barriers_to_inclusion_by_Q17_highest_degree_/output_tables/tally_of_raw_score_of_Q38_barriers_to_inclusion_by_Q17_highest_degree_proportions.csv")
q22.msi.q38 <- read_csv("./barriers_by_msi/analysis_of_Q38_barriers_to_inclusion_by_Q22_msi_/output_tables/tally_of_raw_score_of_Q38_barriers_to_inclusion_by_Q22_msi_proportions.csv")
q3.training.q38 <- read_csv("./barriers_by_level_of_bioinformatics_training/analysis_of_Q38_barriers_to_inclusion_by_Q3_level_of_bioinformatics_training_/output_tables/tally_of_raw_score_of_Q38_barriers_to_inclusion_by_Q3_level_of_bioinformatics_training_proportions.csv")
q24.uenrollment.q38 <- read_csv("./barriers_by_undergraduate_enrollment/analysis_of_Q38_barriers_to_inclusion_by_Q24_undergraduate_enrollment_/output_tables/tally_of_raw_score_of_Q38_barriers_to_inclusion_by_Q24_undergraduate_enrollment_proportions.csv")



# Calculate N for all 8 stratfying categories
#q6
q21.carnegie.q6.count <- read_csv("./barriers_by_carnegie_classification/analysis_of_Q06_barriers_to_implementation_by_Q21_carnegie_classification_/output_tables/count_of_responses_to_Q06_barriers_to_implementation.csv")
q15.race.q6.count <- read_csv("./barriers_by_ethnicity/analysis_of_Q06_barriers_to_implementation_by_race_ethnicity_/output_tables/count_of_responses_to_Q06_barriers_to_implementation.csv")
urep.q6.count <- read_csv("./barriers_by_ethnicity/analysis_of_Q06_barriers_to_implementation_by_STEM_representation_status_/output_tables/count_of_responses_to_Q06_barriers_to_implementation.csv")
q14.sex.q6.count <- read_csv("./barriers_by_gender/analysis_of_Q06_barriers_to_implementation_by_gender_/output_tables/count_of_responses_to_Q06_barriers_to_implementation.csv")
q17.degree.q6.count <- read_csv("./barriers_by_highest_degree_earned/analysis_of_Q06_barriers_to_implementation_by_Q17_highest_degree_/output_tables/count_of_responses_to_Q06_barriers_to_implementation.csv")
q22.msi.q6.count <- read_csv("./barriers_by_msi/analysis_of_Q06_barriers_to_implementation_by_Q22_msi_/output_tables/count_of_responses_to_Q06_barriers_to_implementation.csv")
q3.training.q6.count <- read_csv("./barriers_by_level_of_bioinformatics_training/analysis_of_Q06_barriers_to_implementation_by_Q3_level_of_bioinformatics_training_/output_tables/count_of_responses_to_Q06_barriers_to_implementation.csv")
q24.uenrollment.q6.count <- read_csv("./barriers_by_undergraduate_enrollment/analysis_of_Q06_barriers_to_implementation_by_Q24_undergraduate_enrollment_/output_tables/count_of_responses_to_Q06_barriers_to_implementation.csv")

#q30
q21.carnegie.q30.count<- read_csv("./barriers_by_carnegie_classification/analysis_of_Q29-30_technical_challenges_by_Q21_carnegie_classification_/output_tables/count_of_responses_to_Q29-30_technical_challenges.csv")
q15.race.q30.count <- read_csv("./barriers_by_ethnicity/analysis_of_Q29-30_technical_challenges_by_race_ethnicity_/output_tables/count_of_responses_to_Q29-30_technical_challenges.csv")
urep.q30.count <- read_csv("./barriers_by_ethnicity/analysis_of_Q29-30_technical_challenges_by_STEM_representation_status_/output_tables/count_of_responses_to_Q29-30_technical_challenges.csv")
q14.sex.q30.count <- read_csv("./barriers_by_gender/analysis_of_Q29-30_technical_challenges_by_gender_/output_tables/count_of_responses_to_Q29-30_technical_challenges.csv")
q17.degree.q30.count <- read_csv("./barriers_by_highest_degree_earned/analysis_of_Q29-30_technical_challenges_by_Q17_highest_degree_/output_tables/count_of_responses_to_Q29-30_technical_challenges.csv")
q22.msi.q30.count <- read_csv("./barriers_by_msi/analysis_of_Q29-30_technical_challenges_by_Q22_msi_/output_tables/count_of_responses_to_Q29-30_technical_challenges.csv")
q3.training.q30.count <- read_csv("./barriers_by_level_of_bioinformatics_training/analysis_of_Q29-30_technical_challenges_by_Q3_level_of_bioinformatics_training_/output_tables/count_of_responses_to_Q29-30_technical_challenges.csv")
q24.uenrollment.q30.count <- read_csv("./barriers_by_undergraduate_enrollment/analysis_of_Q29-30_technical_challenges_by_Q24_undergraduate_enrollment_/output_tables/count_of_responses_to_Q29-30_technical_challenges.csv")

#q33                
q21.carnegie.q33.count <- read_csv("./barriers_by_carnegie_classification/analysis_of_Q33_educator_challenges_by_Q21_carnegie_classification_/output_tables/count_of_responses_to_Q33_educator_challenges.csv")
q15.race.q33.count <- read_csv("./barriers_by_ethnicity/analysis_of_Q33_educator_challenges_by_race_ethnicity_/output_tables/count_of_responses_to_Q33_educator_challenges.csv")
urep.q33.count <- read_csv("./barriers_by_ethnicity/analysis_of_Q33_educator_challenges_by_STEM_representation_status_/output_tables/count_of_responses_to_Q33_educator_challenges.csv")
q14.sex.q33.count <- read_csv("./barriers_by_gender/analysis_of_Q33_educator_challenges_by_gender_/output_tables/count_of_responses_to_Q33_educator_challenges.csv")
q17.degree.q33.count <- read_csv("./barriers_by_highest_degree_earned/analysis_of_Q33_educator_challenges_by_Q17_highest_degree_/output_tables/count_of_responses_to_Q33_educator_challenges.csv")
q22.msi.q33.count <- read_csv("./barriers_by_msi/analysis_of_Q33_educator_challenges_by_Q22_msi_/output_tables/count_of_responses_to_Q33_educator_challenges.csv")
q3.training.q33.count <- read_csv("./barriers_by_level_of_bioinformatics_training/analysis_of_Q33_educator_challenges_by_Q3_level_of_bioinformatics_training_/output_tables/count_of_responses_to_Q33_educator_challenges.csv")
q24.uenrollment.q33.count <- read_csv("./barriers_by_undergraduate_enrollment/analysis_of_Q33_educator_challenges_by_Q24_undergraduate_enrollment_/output_tables/count_of_responses_to_Q33_educator_challenges.csv")


#q38
q21.carnegie.q38.count <- read_csv("./barriers_by_carnegie_classification/analysis_of_Q38_barriers_to_inclusion_by_Q21_carnegie_classification_/output_tables/count_of_responses_to_Q38_barriers_to_inclusion.csv")
q15.race.q38.count <- read_csv("./barriers_by_ethnicity/analysis_of_Q38_barriers_to_inclusion_by_race_ethnicity_/output_tables/count_of_responses_to_Q38_barriers_to_inclusion.csv")
urep.q38.count <- read_csv("./barriers_by_ethnicity/analysis_of_Q38_barriers_to_inclusion_by_STEM_representation_status_/output_tables/count_of_responses_to_Q38_barriers_to_inclusion.csv")
q14.sex.q38.count <- read_csv("./barriers_by_gender/analysis_of_Q38_barriers_to_inclusion_by_gender_/output_tables/count_of_responses_to_Q38_barriers_to_inclusion.csv")
q17.degree.q38.count <- read_csv("./barriers_by_highest_degree_earned/analysis_of_Q38_barriers_to_inclusion_by_Q17_highest_degree_/output_tables/count_of_responses_to_Q38_barriers_to_inclusion.csv")
q22.msi.q38.count <- read_csv("./barriers_by_msi/analysis_of_Q38_barriers_to_inclusion_by_Q22_msi_/output_tables/count_of_responses_to_Q38_barriers_to_inclusion.csv")
q3.training.q38.count <- read_csv("./barriers_by_level_of_bioinformatics_training/analysis_of_Q38_barriers_to_inclusion_by_Q3_level_of_bioinformatics_training_/output_tables/count_of_responses_to_Q38_barriers_to_inclusion.csv")
q24.uenrollment.q38.count <- read_csv("./barriers_by_undergraduate_enrollment/analysis_of_Q38_barriers_to_inclusion_by_Q24_undergraduate_enrollment_/output_tables/count_of_responses_to_Q38_barriers_to_inclusion.csv")



caclculate.n <- function(df){
  #scrape the count of faculty in stratafying category
  df <- t(df)
  df <- data.frame(df[-1,], stringsAsFactors = FALSE)
  n <- sum(as.numeric(df$X2))
  return(as.numeric(n))
  
}




#q6
q21.carnegie.q6.count.n <- caclculate.n(q21.carnegie.q6.count)
q15.race.q6.count.n <- caclculate.n(q15.race.q6.count)
urep.q6.count.n <- caclculate.n(urep.q6.count)
q14.sex.q6.count.n <- caclculate.n(q14.sex.q6.count)
q17.degree.q6.count.n <- caclculate.n(q17.degree.q6.count)
q22.msi.q6.count.n <- caclculate.n(q22.msi.q6.count)
q3.training.q6.count.n <- caclculate.n(q3.training.q6.count)
q24.uenrollment.q6.count.n <- caclculate.n(q24.uenrollment.q6.count)
#q30
q21.carnegie.q30.count.n <- caclculate.n(q21.carnegie.q30.count)
q15.race.q30.count.n <- caclculate.n(q15.race.q30.count)
urep.q30.count.n <- caclculate.n(urep.q30.count)
q14.sex.q30.count.n <- caclculate.n(q14.sex.q30.count)
q17.degree.q30.count.n <- caclculate.n(q17.degree.q30.count)
q22.msi.q30.count.n <- caclculate.n(q22.msi.q30.count)
q3.training.q30.count.n <- caclculate.n(q3.training.q30.count)
q24.uenrollment.q30.count.n <- caclculate.n(q24.uenrollment.q30.count)
#q33
q21.carnegie.q33.count.n <- caclculate.n(q21.carnegie.q33.count)
q15.race.q33.count.n <- caclculate.n(q15.race.q33.count)
urep.q33.count.n <- caclculate.n(urep.q33.count)
q14.sex.q33.count.n <- caclculate.n(q14.sex.q33.count)
q17.degree.q33.count.n <- caclculate.n(q17.degree.q33.count)
q22.msi.q33.count.n <- caclculate.n(q22.msi.q33.count)
q3.training.q33.count.n <- caclculate.n(q3.training.q33.count)
q24.uenrollment.q33.count.n <- caclculate.n(q24.uenrollment.q33.count)
#q38
q21.carnegie.q38.count.n <- caclculate.n(q21.carnegie.q38.count)
q15.race.q38.count.n <- caclculate.n(q15.race.q38.count)
urep.q38.count.n <- caclculate.n(urep.q38.count)
q14.sex.q38.count.n <- caclculate.n(q14.sex.q38.count)
q17.degree.q38.count.n <- caclculate.n(q17.degree.q38.count)
q22.msi.q38.count.n <- caclculate.n(q22.msi.q38.count)
q3.training.q38.count.n <- caclculate.n(q3.training.q38.count)
q24.uenrollment.q38.count.n <- caclculate.n(q24.uenrollment.q38.count)







##### Add N to question dataframes

#q6
q21.carnegie.q6[,"N"] <- q21.carnegie.q6.count.n
q15.race.q6[,"N"] <- q15.race.q6.count.n
urep.q6[,"N"] <- urep.q6.count.n
q14.sex.q6[,"N"] <- q14.sex.q6.count.n
q17.degree.q6[,"N"] <- q17.degree.q6.count.n
q22.msi.q6[,"N"] <- q22.msi.q6.count.n
q3.training.q6[,"N"] <- q3.training.q6.count.n
q24.uenrollment.q6[,"N"] <- q3.training.q6.count.n
#q30
q21.carnegie.q30[,"N"] <- q21.carnegie.q30.count.n
q15.race.q30[,"N"] <- q15.race.q30.count.n
urep.q30[,"N"] <- urep.q30.count.n
q14.sex.q30[,"N"] <- q14.sex.q30.count.n
q17.degree.q30[,"N"] <- q17.degree.q30.count.n
q22.msi.q30[,"N"] <- q22.msi.q30.count.n
q3.training.q30[,"N"] <- q3.training.q30.count.n
q24.uenrollment.q30[,"N"] <- q3.training.q30.count.n
#q33
q21.carnegie.q33[,"N"] <- q21.carnegie.q33.count.n
q15.race.q33[,"N"] <- q15.race.q33.count.n
urep.q33[,"N"] <- urep.q33.count.n
q14.sex.q33[,"N"] <- q14.sex.q33.count.n
q17.degree.q33[,"N"] <- q17.degree.q33.count.n
q22.msi.q33[,"N"] <- q22.msi.q33.count.n
q3.training.q33[,"N"] <- q3.training.q33.count.n
q24.uenrollment.q33[,"N"] <- q3.training.q33.count.n
#q38
q21.carnegie.q38[,"N"] <- q21.carnegie.q38.count.n
q15.race.q38[,"N"] <- q15.race.q38.count.n
urep.q38[,"N"] <- urep.q38.count.n
q14.sex.q38[,"N"] <- q14.sex.q38.count.n
q17.degree.q38[,"N"] <- q17.degree.q38.count.n
q22.msi.q38[,"N"] <- q22.msi.q38.count.n
q3.training.q38[,"N"] <- q3.training.q38.count.n
q24.uenrollment.q38[,"N"] <- q3.training.q38.count.n


########### Sort out the top 5 barriers for each question

sort.and.retain.barriers <- function(df){
  df.sorted <- df%>%
    arrange(desc(summed_score))%>%
    distinct(summed_score, .keep_all = TRUE)%>%
    head(n=5)
  
  df.filtered <- df%>%
    filter(summed_score %in% df.sorted$summed_score)
  
  return(df.filtered)
}




########## Caculate percentage of total respondants in group

total.percentage.responding <- function(df){
  
  df <- df%>%
    group_by(Var2)%>%
    mutate(total.percent = summed_score/N)
  return(df)
  
}

######## Select columns and name frame

selected.columns <- function(df){
  
  df <- df%>%
    select(Var2, total.percent)%>%
    distinct(Var2, .keep_all = TRUE)
  return(df)
}


######### Summarize the number of time a barrier was indicated
### This number is != to the number of faculty indicated the barrier, but the number of time barrier was scored
### Faculty indicating multiple barriers are captured multiple times


caculate.top5.barriers <- function(df){
  df <- sort.and.retain.barriers(df)
  df <- total.percentage.responding(df)
  df <- selected.columns(df)
  return(df)
}





# Gather top 5 barriers for Q38 for all stratfying catgeories

#q6
q21.carnegie.q6.top5 <- caculate.top5.barriers(q21.carnegie.q6)
q15.race.q6.top5 <- caculate.top5.barriers(q15.race.q6)
urep.q6.top5 <- caculate.top5.barriers(urep.q6)
q14.sex.q6.top5 <- caculate.top5.barriers(q14.sex.q6)
q17.degree.q6.top5 <- caculate.top5.barriers(q17.degree.q6)
q22.msi.q6.top5 <- caculate.top5.barriers(q22.msi.q6)
q3.training.q6.top5 <- caculate.top5.barriers(q3.training.q6)
q24.uenrollment.q6.top5 <- caculate.top5.barriers(q24.uenrollment.q6)
#q30
q21.carnegie.q30.top5 <- caculate.top5.barriers(q21.carnegie.q30)
q15.race.q30.top5 <- caculate.top5.barriers(q15.race.q30)
urep.q30.top5 <- caculate.top5.barriers(urep.q30)
q14.sex.q30.top5 <- caculate.top5.barriers(q14.sex.q30)
q17.degree.q30.top5 <- caculate.top5.barriers(q17.degree.q30)
q22.msi.q30.top5 <- caculate.top5.barriers(q22.msi.q30)
q3.training.q30.top5 <- caculate.top5.barriers(q3.training.q30)
q24.uenrollment.q30.top5 <- caculate.top5.barriers(q24.uenrollment.q30)
#q33
q21.carnegie.q33.top5 <- caculate.top5.barriers(q21.carnegie.q33)
q15.race.q33.top5 <- caculate.top5.barriers(q15.race.q33)
urep.q33.top5 <- caculate.top5.barriers(urep.q33)
q14.sex.q33.top5 <- caculate.top5.barriers(q14.sex.q33)
q17.degree.q33.top5 <- caculate.top5.barriers(q17.degree.q33)
q22.msi.q33.top5 <- caculate.top5.barriers(q22.msi.q33)
q3.training.q33.top5 <- caculate.top5.barriers(q3.training.q33)
q24.uenrollment.q33.top5 <- caculate.top5.barriers(q24.uenrollment.q33)
#q38
q21.carnegie.q38.top5 <- caculate.top5.barriers(q21.carnegie.q38)
q15.race.q38.top5 <- caculate.top5.barriers(q15.race.q38)
urep.q38.top5 <- caculate.top5.barriers(urep.q38)
q14.sex.q38.top5 <- caculate.top5.barriers(q14.sex.q38)
q17.degree.q38.top5 <- caculate.top5.barriers(q17.degree.q38)
q22.msi.q38.top5 <- caculate.top5.barriers(q22.msi.q38)
q3.training.q38.top5 <- caculate.top5.barriers(q3.training.q38)
q24.uenrollment.q38.top5 <- caculate.top5.barriers(q24.uenrollment.q38)

#Add Category names to dataframes
#q6
q21.carnegie.q6.top5[,"Demographic"] <- paste("Carnegie Classification \nn=", q21.carnegie.q6.count.n, sep = " ")
q15.race.q6.top5[,"Demographic"] <- paste("Ethnicity \nn=", q15.race.q6.count.n, sep = " ")
urep.q6.top5[,"Demographic"] <- paste("Underrepresented Status \nn=", urep.q6.count.n, sep = " ")
q14.sex.q6.top5[,"Demographic"] <- paste("Sex \nn=", q14.sex.q6.count.n, sep = " ")
q17.degree.q6.top5[,"Demographic"] <- paste("Highest Degree Earned \nn=", q17.degree.q6.count.n, sep = " ")
q22.msi.q6.top5[,"Demographic"] <- paste("Minority Serving Institution \nn=", q22.msi.q6.count.n, sep = " ")
q3.training.q6.top5[,"Demographic"] <- paste("Level of Bioinformatics Training \nn=", q3.training.q6.count.n, sep = " ")
q24.uenrollment.q6.top5[,"Demographic"] <- paste("Undergraduate Enrollment \nn=", q24.uenrollment.q6.count.n, sep = " ")
#q30
q21.carnegie.q30.top5[,"Demographic"] <- paste("Carnegie Classification \nn=", q21.carnegie.q30.count.n, sep = " ")
q15.race.q30.top5[,"Demographic"] <- paste("Ethnicity \nn=", q15.race.q30.count.n, sep = " ")
urep.q30.top5[,"Demographic"] <- paste("Underrepresented Status \nn=", urep.q30.count.n, sep = " ")
q14.sex.q30.top5[,"Demographic"] <- paste("Sex \nn=", q14.sex.q30.count.n, sep = " ")
q17.degree.q30.top5[,"Demographic"] <- paste("Highest Degree Earned \nn=", q17.degree.q30.count.n, sep = " ")
q22.msi.q30.top5[,"Demographic"] <- paste("Minority Serving Institution \nn=", q22.msi.q30.count.n, sep = " ")
q3.training.q30.top5[,"Demographic"] <- paste("Level of Bioinformatics Training \nn=", q3.training.q30.count.n, sep = " ")
q24.uenrollment.q30.top5[,"Demographic"] <- paste("Undergraduate Enrollment \nn=", q24.uenrollment.q30.count.n, sep = " ")
#q33
q21.carnegie.q33.top5[,"Demographic"] <- paste("Carnegie Classification \nn=", q21.carnegie.q33.count.n, sep = " ")
q15.race.q33.top5[,"Demographic"] <- paste("Ethnicity \nn=", q15.race.q33.count.n, sep = " ")
urep.q33.top5[,"Demographic"] <- paste("Underrepresented Status \nn=", urep.q33.count.n, sep = " ")
q14.sex.q33.top5[,"Demographic"] <- paste("Sex \nn=", q14.sex.q33.count.n, sep = " ")
q17.degree.q33.top5[,"Demographic"] <- paste("Highest Degree Earned \nn=", q17.degree.q33.count.n, sep = " ")
q22.msi.q33.top5[,"Demographic"] <- paste("Minority Serving Institution \nn=", q22.msi.q33.count.n, sep = " ")
q3.training.q33.top5[,"Demographic"] <- paste("Level of Bioinformatics Training \nn=", q3.training.q33.count.n, sep = " ")
q24.uenrollment.q33.top5[,"Demographic"] <- paste("Undergraduate Enrollment \nn=", q24.uenrollment.q33.count.n, sep = " ")
#q38
q21.carnegie.q38.top5[,"Demographic"] <- paste("Carnegie Classification \nn=", q21.carnegie.q38.count.n, sep = " ")
q15.race.q38.top5[,"Demographic"] <- paste("Ethnicity \nn=", q15.race.q38.count.n, sep = " ")
urep.q38.top5[,"Demographic"] <- paste("Underrepresented Status \nn=", urep.q38.count.n, sep = " ")
q14.sex.q38.top5[,"Demographic"] <- paste("Sex \nn=", q14.sex.q38.count.n, sep = " ")
q17.degree.q38.top5[,"Demographic"] <- paste("Highest Degree Earned \nn=", q17.degree.q38.count.n, sep = " ")
q22.msi.q38.top5[,"Demographic"] <- paste("Minority Serving Institution \nn=", q22.msi.q38.count.n, sep = " ")
q3.training.q38.top5[,"Demographic"] <- paste("Level of Bioinformatics Training \nn=", q3.training.q38.count.n, sep = " ")
q24.uenrollment.q38.top5[,"Demographic"] <- paste("Undergraduate Enrollment \nn=", q24.uenrollment.q38.count.n, sep = " ")


#Add Q number to each df

#q6
q21.carnegie.q6.top5["question"] <- "Q6: What are your barriers \nto development/implementation \nof courses with bioinformatics"
q15.race.q6.top5["question"] <- "Q6: What are your barriers \nto development/implementation \nof courses with bioinformatics"
urep.q6.top5["question"] <- "Q6: What are your barriers \nto development/implementation \nof courses with bioinformatics"
q14.sex.q6.top5["question"] <- "Q6: What are your barriers \nto development/implementation \nof courses with bioinformatics"
q17.degree.q6.top5["question"] <- "Q6: What are your barriers \nto development/implementation \nof courses with bioinformatics"
q22.msi.q6.top5["question"] <- "Q6: What are your barriers \nto development/implementation \nof courses with bioinformatics"
q3.training.q6.top5["question"] <- "Q6: What are your barriers \nto development/implementation \nof courses with bioinformatics"
q24.uenrollment.q6.top5["question"] <- "Q6: What are your barriers \nto development/implementation \nof courses with bioinformatics"
#q30
q21.carnegie.q30.top5["question"] <- "Q30: 'Describe the technical barriers \nyou face in teaching bioinformatics'"
q15.race.q30.top5["question"] <- "Q30: 'Describe the technical barriers \nyou face in teaching bioinformatics'"
urep.q30.top5["question"] <- "Q30: 'Describe the technical barriers \nyou face in teaching bioinformatics'"
q14.sex.q30.top5["question"] <- "Q30: 'Describe the technical barriers \nyou face in teaching bioinformatics'"
q17.degree.q30.top5["question"] <- "Q30: 'Describe the technical barriers \nyou face in teaching bioinformatics'"
q22.msi.q30.top5["question"] <- "Q30: 'Describe the technical barriers \nyou face in teaching bioinformatics'"
q3.training.q30.top5["question"] <- "Q30: 'Describe the technical barriers \nyou face in teaching bioinformatics'"
q24.uenrollment.q30.top5["question"] <- "Q30: 'Describe the technical barriers \nyou face in teaching bioinformatics'"
#q33
q21.carnegie.q33.top5["question"] <- "Q33: What are the most \nimportant challenges to facing educators in \neducating undergraduates in bioinformatics"
q15.race.q33.top5["question"] <- "Q33: What are the most \nimportant challenges to facing educators in \neducating undergraduates in bioinformatics"
urep.q33.top5["question"] <- "Q33: What are the most \nimportant challenges to facing educators in \neducating undergraduates in bioinformatics"
q14.sex.q33.top5["question"] <- "Q33: What are the most \nimportant challenges to facing educators in \neducating undergraduates in bioinformatics"
q17.degree.q33.top5["question"] <- "Q33: What are the most \nimportant challenges to facing educators in \neducating undergraduates in bioinformatics"
q22.msi.q33.top5["question"] <- "Q33: What are the most \nimportant challenges to facing educators in \neducating undergraduates in bioinformatics"
q3.training.q33.top5["question"] <- "Q33: What are the most \nimportant challenges to facing educators in \neducating undergraduates in bioinformatics"
q24.uenrollment.q33.top5["question"] <- "Q33: What are the most \nimportant challenges to facing educators in \neducating undergraduates in bioinformatics"
#q38
q21.carnegie.q38.top5["question"] <- "Q38: What is preventing you \nfrom including bioinformatics \ncontent in these courses?"
q15.race.q38.top5["question"] <- "Q38: What is preventing you \nfrom including bioinformatics \ncontent in these courses?"
urep.q38.top5["question"] <- "Q38: What is preventing you \nfrom including bioinformatics \ncontent in these courses?"
q14.sex.q38.top5["question"] <- "Q38: What is preventing you \nfrom including bioinformatics \ncontent in these courses?"
q17.degree.q38.top5["question"] <- "Q38: What is preventing you \nfrom including bioinformatics \ncontent in these courses?"
q22.msi.q38.top5["question"] <- "Q38: What is preventing you \nfrom including bioinformatics \ncontent in these courses?"
q3.training.q38.top5["question"] <- "Q38: What is preventing you \nfrom including bioinformatics \ncontent in these courses?"
q24.uenrollment.q38.top5["question"] <- "Q38: What is preventing you \nfrom including bioinformatics \ncontent in these courses?"

#Add n (completed sample size) and error to each df

#q6
q21.carnegie.q6.top5["N"] <- q21.carnegie.q6.count.n
q15.race.q6.top5["N"] <- q15.race.q6.count.n
urep.q6.top5["N"] <- urep.q6.count.n
q14.sex.q6.top5["N"] <- q14.sex.q6.count.n
q17.degree.q6.top5["N"] <- q17.degree.q6.count.n
q22.msi.q6.top5["N"] <- q22.msi.q6.count.n
q3.training.q6.top5["N"] <- q3.training.q6.count.n
q24.uenrollment.q6.top5["N"] <- q24.uenrollment.q6.count.n
#q30
q21.carnegie.q30.top5["N"] <- q21.carnegie.q30.count.n
q15.race.q30.top5["N"] <- q15.race.q30.count.n
urep.q30.top5["N"] <- urep.q30.count.n
q14.sex.q30.top5["N"] <- q14.sex.q30.count.n
q17.degree.q30.top5["N"] <- q17.degree.q30.count.n
q22.msi.q30.top5["N"] <- q22.msi.q30.count.n
q3.training.q30.top5["N"] <- q3.training.q30.count.n
q24.uenrollment.q30.top5["N"] <- q24.uenrollment.q30.count.n
#q33
q21.carnegie.q33.top5["N"] <- q21.carnegie.q33.count.n
q15.race.q33.top5["N"] <- q15.race.q33.count.n
urep.q33.top5["N"] <- urep.q33.count.n
q14.sex.q33.top5["N"] <- q14.sex.q33.count.n
q17.degree.q33.top5["N"] <- q17.degree.q33.count.n
q22.msi.q33.top5["N"] <- q22.msi.q33.count.n
q3.training.q33.top5["N"] <- q3.training.q33.count.n
q24.uenrollment.q33.top5["N"] <- q24.uenrollment.q33.count.n
#q38
q21.carnegie.q38.top5["N"] <- q21.carnegie.q38.count.n
q15.race.q38.top5["N"] <- q15.race.q38.count.n
urep.q38.top5["N"] <- urep.q38.count.n
q14.sex.q38.top5["N"] <- q14.sex.q38.count.n
q17.degree.q38.top5["N"] <- q17.degree.q38.count.n
q22.msi.q38.top5["N"] <- q22.msi.q38.count.n
q3.training.q38.top5["N"] <- q3.training.q38.count.n
q24.uenrollment.q38.top5["N"] <- q24.uenrollment.q38.count.n


## Add error

add.error <- function(df){
  
  n.sample <- as.numeric(levels(as.factor(df$N)))
  N.population <- 100000
  P.proportion_size <-  .5 
  z.confidence.interval <-  1.96
  numer <- z.confidence.interval * sqrt(P.proportion_size*(1-P.proportion_size))
  denom <- sqrt(((N.population - 1)*n.sample)/(N.population - n.sample))
  error.percentage <- numer/denom
  
  df <- df%>%
    mutate(ymax = total.percent + (total.percent * error.percentage))%>%
    mutate(ymin = total.percent - (total.percent * error.percentage))
  
}


#calculate error and add to df

#q6
q21.carnegie.q6.top5<- add.error(q21.carnegie.q6.top5)
q15.race.q6.top5<- add.error(q15.race.q6.top5)
urep.q6.top5<- add.error(urep.q6.top5)
q14.sex.q6.top5<- add.error(q14.sex.q6.top5)
q17.degree.q6.top5<- add.error(q17.degree.q6.top5)
q22.msi.q6.top5<- add.error(q22.msi.q6.top5)
q3.training.q6.top5<- add.error(q3.training.q6.top5)
q24.uenrollment.q6.top5<- add.error(q24.uenrollment.q6.top5)
#q30
q21.carnegie.q30.top5<- add.error(q21.carnegie.q30.top5)
q15.race.q30.top5<- add.error(q15.race.q30.top5)
urep.q30.top5<- add.error(urep.q30.top5)
q14.sex.q30.top5<- add.error(q14.sex.q30.top5)
q17.degree.q30.top5<- add.error(q17.degree.q30.top5)
q22.msi.q30.top5<- add.error(q22.msi.q30.top5)
q3.training.q30.top5<- add.error(q3.training.q30.top5)
q24.uenrollment.q30.top5<- add.error(q24.uenrollment.q30.top5)
#q33
q21.carnegie.q33.top5<- add.error(q21.carnegie.q33.top5)
q15.race.q33.top5<- add.error(q15.race.q33.top5)
urep.q33.top5<- add.error(urep.q33.top5)
q14.sex.q33.top5<- add.error(q14.sex.q33.top5)
q17.degree.q33.top5<- add.error(q17.degree.q33.top5)
q22.msi.q33.top5<- add.error(q22.msi.q33.top5)
q3.training.q33.top5<- add.error(q3.training.q33.top5)
q24.uenrollment.q33.top5<- add.error(q24.uenrollment.q33.top5)
#q38
q21.carnegie.q38.top5<- add.error(q21.carnegie.q38.top5)
q15.race.q38.top5<- add.error(q15.race.q38.top5)
urep.q38.top5<- add.error(urep.q38.top5)
q14.sex.q38.top5<- add.error(q14.sex.q38.top5)
q17.degree.q38.top5<- add.error(q17.degree.q38.top5)
q22.msi.q38.top5<- add.error(q22.msi.q38.top5)
q3.training.q38.top5<- add.error(q3.training.q38.top5)
q24.uenrollment.q38.top5<- add.error(q24.uenrollment.q38.top5)




#Plot barrier percentages by Stratefying category

#combine dataframes

total.top5 <- rbind(  q21.carnegie.q6.top5,
                      q15.race.q6.top5,
                      urep.q6.top5,
                      q14.sex.q6.top5,
                      q17.degree.q6.top5,
                      q22.msi.q6.top5,
                      q3.training.q6.top5,
                      q24.uenrollment.q6.top5,
                      q21.carnegie.q30.top5,
                      q15.race.q30.top5,
                      urep.q30.top5,
                      q14.sex.q30.top5,
                      q17.degree.q30.top5,
                      q22.msi.q30.top5,
                      q3.training.q30.top5,
                      q24.uenrollment.q30.top5,
                      q21.carnegie.q33.top5,
                      q15.race.q33.top5,
                      urep.q33.top5,
                      q14.sex.q33.top5,
                      q17.degree.q33.top5,
                      q22.msi.q33.top5,
                      q3.training.q33.top5,
                      q24.uenrollment.q33.top5,
                      q21.carnegie.q38.top5,
                      q15.race.q38.top5,
                      urep.q38.top5,
                      q14.sex.q38.top5,
                      q17.degree.q38.top5,
                      q22.msi.q38.top5,
                      q3.training.q38.top5,
                      q24.uenrollment.q38.top5)


#testplot

#setup ordering


total.top5$Var2 <- factor(total.top5$Var2, levels = total.top5$Var2[order(desc(total.top5$total.percent))])


limits <- aes(ymax = total.top5$ymax, ymin = total.top5$ymin)
error.dodge <- position_dodge(width=0.9)

total.top5%>%
  ggplot()+
  aes(x = Var2, y=total.percent, fill = Var2)+
  geom_bar(stat = "identity", position = "dodge")+
  xlab("Top 5 Barriers for respondants parsed by demographic categories")+
  ylab("Proportion of respondants reporting barrier within each demographic")+
  facet_grid(question ~ Demographic)+
  theme_minimal()+
  theme(strip.text.x = element_text(size = 8, angle = 90))+
  theme(axis.text.x=element_blank())+
  ggtitle("Top 5 Barriers across stratafying demographics")

ggsave("top_5_plot.png",width = 13.8, 
       height = 8.81, 
       units = "in")




#Set some graph labels and variables

positions = c ("Faculty Issues: Expertise/training", 
               "Faculty Issues: Time", 
               "Curriculum Issues: No space", 
               "Curriculum Issues: Incompatible with current curriculum", 
               "Student Issues: Background knowledge")

facet.names <- c("Carnegie Classification", 
                 "Ethnicity", 
                 "Underrepresented Status", 
                 "Sex", 
                 "Highest Degree Earned", 
                 "Minority Serving Institution", 
                 "Level of Bioinformatics Training", 
                 "Undergraduate Enrollment")

#reorder levels
total.top5$Var2 <- factor(total.top5$Var2, levels = c("Faculty Issues: Expertise/training", 
                                                      "Faculty Issues: Time", 
                                                      "Curriculum Issues: No space", 
                                                      "Curriculum Issues: Incompatible with current curriculum", 
                                                      "Student Issues: Background knowledge") )

total.top5%>%
  ggplot()+
  aes(x = Var2, y=total.percent, fill = Var2)+
  geom_bar(stat = "identity", position = "dodge")+
  scale_x_discrete(limits = positions)+
  xlab("Top 5 Barriers for respondants parsed by demographic categories")+
  ylab("Proportion of respondants reporting barrier within each demographic")+
  scale_fill_discrete(name= "Q38 Coded Barrier")+
  facet_grid(.~ Demographic)+
  theme_minimal()+
  theme(strip.text.x = element_text(size = 8, angle = 90))+
  theme(axis.text.x=element_blank())+
  ggtitle("Top 5 Barriers for Q38 across stratafying demographics")


ggsave("top_5_Q38_plot.png")


