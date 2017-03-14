# This script helps to complie numbers and summary stats from outputs of other analysis scripts

require(tidyverse)


data <- read_csv("../../data_cleaning_scripts/04_decode_survey_responses/output/decoded_df.csv")

# remove any non-US respondents
countries <- c("United States","Puerto Rico")
data <- data%>%
  filter(Country_Country %in% countries )

survey.n <- nrow(data)

#Create blank df
overall_summary.csv <- data.frame( NULL, stringsAsFactors = FALSE)

#Overall number of us Responses
overall_summary.csv[1,"Question"] <- "All"
overall_summary.csv[1,"Statistic"] <- "All Answers"
overall_summary.csv[1,"Demographic"] <- "All US respondents"
overall_summary.csv[1,"Value"] <- survey.n
overall_summary.csv[1,"Percentage of"] <- "Total N"
overall_summary.csv[1,"Percentage"] <- (survey.n / survey.n)

#Question 06 - number of written responses
overall_summary.csv[2,"Question"] <- "Barriers to Implementation(Q06)"
overall_summary.csv[2,"Statistic"] <- "Number of free text responses"
overall_summary.csv[2,"Demographic"] <- "All"
overall_summary.csv[2,"Value"] <- length(which(!is.na(data$Q6_Optional..Please.describe.briefly..include.any.barriers.to.development.and.or.implementation.)))
overall_summary.csv[2,"Percentage of"] <- "Total N"
overall_summary.csv[2,"Percentage"] <- (overall_summary.csv[2,"Value"] / survey.n)

#Question 33 - number of written responses
overall_summary.csv[3,"Question"] <- "Educator Challenges(Q33)"
overall_summary.csv[3,"Statistic"] <- "Number of free text responses"
overall_summary.csv[3,"Demographic"] <- "All"
overall_summary.csv[3,"Value"] <- length(which(!is.na(data$Q33_In.your.opinion..what.do.you.think.are.the.most.important.challenges.currently.facing.those.educa...)))
overall_summary.csv[3,"Percentage of"] <- "Total N"
overall_summary.csv[3,"Percentage"] <- (overall_summary.csv[3,"Value"] / survey.n)

#Question 38 - number of written responses
overall_summary.csv[4,"Question"] <- "Barriers to Inclusion(Q38)"
overall_summary.csv[4,"Statistic"] <- "Number of free text responses"
overall_summary.csv[4,"Demographic"] <- "All"
overall_summary.csv[4,"Value"] <- length(which(!is.na(data$Q38_What.is.preventing.yOu.frOm.including.biOinfOrmatics.cOntent.in.these.cOurses.)))
overall_summary.csv[4,"Percentage of"] <- "Total N"
overall_summary.csv[4,"Percentage"] <- (overall_summary.csv[4,"Value"] / survey.n)
  
#Question 29/30 - 
overall_summary.csv[5,"Question"] <- "Technical Barriers(Q29,30) "
overall_summary.csv[5,"Statistic"] <- "Number of free text responses"
overall_summary.csv[5,"Demographic"] <- "All"
overall_summary.csv[5,"Value"] <-   length(which(!is.na(data$Q30_Optional..Please.describe.)))
overall_summary.csv[5,"Percentage of"] <- "Total N"
overall_summary.csv[5,"Percentage"] <- (overall_summary.csv[5,"Value"] / survey.n)


# Get qualifying demographics

#Carnegie Classification

cc.all.tmp <- read_csv("../barriers_by_carnegie_classification/analysis_of_Q06_barriers_to_implementation_by_Q21_carnegie_classification_/output_tables/count_of_responses_to_Q6_implementation.csv")
overall_summary.csv[6,"Question"] <- "33, 06, 29-30"
overall_summary.csv[6,"Statistic"] <- "Maximum number of respondents"
overall_summary.csv[6,"Demographic"] <- "Carnegie classification"
overall_summary.csv[6,"Value"] <-   sum(as.numeric(cc.all.tmp[2,2:length(cc.all.tmp)]))
overall_summary.csv[6,"Percentage of"] <- "Total N"
overall_summary.csv[6,"Percentage"] <- ( overall_summary.csv[6,"Value"] /survey.n )

cc.38.tmp <- read_csv("../barriers_by_carnegie_classification/analysis_of_Q38_barriers_to_inclusion_by_Q21_carnegie_classification_/output_tables/count_of_responses_to_Q38_inclusion.csv")
overall_summary.csv[7,"Question"] <- "38"
overall_summary.csv[7,"Statistic"] <- "Maximum number of respondents"
overall_summary.csv[7,"Demographic"] <- "Carnegie classification"
overall_summary.csv[7,"Value"] <-   sum(as.numeric(cc.38.tmp[2,2:length(cc.38.tmp)]))
overall_summary.csv[7,"Percentage of"] <- "Total N"
overall_summary.csv[7,"Percentage"] <- ( overall_summary.csv[7,"Value"] /survey.n )


#Current teaching

ct.all.tmp <- read_csv("../barriers_by_current_teaching/analysis_of_Q06_barriers_to_implementation_by_Q1_bioinformatics_integration_/output_tables/count_of_responses_to_Q6_implementation.csv")
overall_summary.csv[8,"Question"] <- "33, 06, 29-30"
overall_summary.csv[8,"Statistic"] <- "Maximum number of respondents"
overall_summary.csv[8,"Demographic"] <- "Current teaching"
overall_summary.csv[8,"Value"] <-   sum(as.numeric(ct.all.tmp[2,2:length(ct.all.tmp)]))
overall_summary.csv[8,"Percentage of"] <- "Total N"
overall_summary.csv[8,"Percentage"] <- ( overall_summary.csv[8,"Value"] /survey.n )

ct.38.tmp <- read_csv("../barriers_by_current_teaching/analysis_of_Q38_barriers_to_inclusion_by_Q1_bioinformatics_integration_/output_tables/count_of_responses_to_Q38_inclusion.csv")
overall_summary.csv[9,"Question"] <- "38"
overall_summary.csv[9,"Statistic"] <- "Maximum number of respondents"
overall_summary.csv[9,"Demographic"] <- "Current teaching"
overall_summary.csv[9,"Value"] <-   sum(as.numeric(ct.38.tmp[2,2:length(ct.38.tmp)]))
overall_summary.csv[9,"Percentage of"] <- "Total N"
overall_summary.csv[9,"Percentage"] <- ( overall_summary.csv[9,"Value"] /survey.n )


#Ethnicity faculty STEM representation status

ethicity.all.tmp <- read_csv("../barriers_by_ethnicity/analysis_of_Q06_barriers_to_implementation_by_STEM_representation_status_/output_tables/count_of_responses_to_Q6_implementation.csv")
overall_summary.csv[10,"Question"] <- "33, 06, 29-30"
overall_summary.csv[10,"Statistic"] <- "Maximum number of respondents"
overall_summary.csv[10,"Demographic"] <- "Ethnicity/STEM Representation"
overall_summary.csv[10,"Value"] <-   sum(as.numeric(ethicity.all.tmp[2,2:length(ethicity.all.tmp)]))
overall_summary.csv[10,"Percentage of"] <- "Total N"
overall_summary.csv[10,"Percentage"] <- ( overall_summary.csv[10,"Value"] /survey.n )

ethnicity.38.tmp <- read_csv("../barriers_by_ethnicity/analysis_of_Q38_barriers_to_inclusion_by_STEM_representation_status_/output_tables/count_of_responses_to_Q38_inclusion.csv")
overall_summary.csv[11,"Question"] <- "38"
overall_summary.csv[11,"Statistic"] <- "Maximum number of respondents"
overall_summary.csv[11,"Demographic"] <- "Ethnicity/STEM Representation"
overall_summary.csv[11,"Value"] <-   sum(as.numeric(ethnicity.38.tmp [2,2:length(ethnicity.38.tmp )]))
overall_summary.csv[11,"Percentage of"] <- "Total N"
overall_summary.csv[11,"Percentage"] <- ( overall_summary.csv[11,"Value"] /survey.n )


#Highest degree earned

degree.all.tmp <- read_csv("../barriers_by_highest_degree_earned/analysis_of_Q06_barriers_to_implementation_by_Q17_highest_degree_/output_tables/count_of_responses_to_Q6_implementation.csv")
overall_summary.csv[12,"Question"] <- "33, 06, 29-30"
overall_summary.csv[12,"Statistic"] <- "Maximum number of respondents"
overall_summary.csv[12,"Demographic"] <- "Highest degree earned"
overall_summary.csv[12,"Value"] <-   sum(as.numeric(degree.all.tmp[2,2:length(degree.all.tmp)]))
overall_summary.csv[12,"Percentage of"] <- "Total N"
overall_summary.csv[12,"Percentage"] <- ( overall_summary.csv[12,"Value"] /survey.n )

degree.38.tmp <- read_csv("../barriers_by_highest_degree_earned/analysis_of_Q38_barriers_to_inclusion_by_Q17_highest_degree_/output_tables/count_of_responses_to_Q38_inclusion.csv")
overall_summary.csv[13,"Question"] <- "38"
overall_summary.csv[13,"Statistic"] <- "Maximum number of respondents"
overall_summary.csv[13,"Demographic"] <- "Highest degree earned"
overall_summary.csv[13,"Value"] <-   sum(as.numeric(degree.38.tmp [2,2:length(degree.38.tmp )]))
overall_summary.csv[13,"Percentage of"] <- "Total N"
overall_summary.csv[13,"Percentage"] <- ( overall_summary.csv[13,"Value"] /survey.n )

#Level of Bioinformatics Training

training.all.tmp <- read_csv("../barriers_by_level_of_bioinformatics_training/analysis_of_Q06_barriers_to_implementation_by_altQ3_level_of_bioinformatics_training_/output_tables/count_of_responses_to_Q6_implementation.csv")
overall_summary.csv[14,"Question"] <- "33, 06, 29-30"
overall_summary.csv[14,"Statistic"] <- "Maximum number of respondents"
overall_summary.csv[14,"Demographic"] <- "Level of Bioinformatics Training"
overall_summary.csv[14,"Value"] <-   sum(as.numeric(training.all.tmp[2,2:length(training.all.tmp)]))
overall_summary.csv[14,"Percentage of"] <- "Total N"
overall_summary.csv[14,"Percentage"] <- ( overall_summary.csv[14,"Value"] /survey.n )

training.38.tmp <- read_csv("../barriers_by_level_of_bioinformatics_training/analysis_of_Q38_barriers_to_inclusion_by_Q3_level_of_bioinformatics_training_/output_tables/count_of_responses_to_Q38_inclusion.csv")
overall_summary.csv[15,"Question"] <- "38"
overall_summary.csv[15,"Statistic"] <- "Maximum number of respondents"
overall_summary.csv[15,"Demographic"] <- "Level of Bioinformatics Training"
overall_summary.csv[15,"Value"] <-   sum(as.numeric(training.38.tmp [2,2:length(training.38.tmp )]))
overall_summary.csv[15,"Percentage of"] <- "Total N"
overall_summary.csv[15,"Percentage"] <- ( overall_summary.csv[15,"Value"] /survey.n )

#MSI Institutional Status

msi.all.tmp <- read_csv("../barriers_by_msi/analysis_of_Q06_barriers_to_implementation_by_Q22_msi_/output_tables/count_of_responses_to_Q6_implementation.csv")
overall_summary.csv[16,"Question"] <- "33, 06, 29-30"
overall_summary.csv[16,"Statistic"] <- "Maximum number of respondents"
overall_summary.csv[16,"Demographic"] <- "MSI Institutional Status"
overall_summary.csv[16,"Value"] <-   sum(as.numeric(msi.all.tmp[2,2:length(msi.all.tmp)]))
overall_summary.csv[16,"Percentage of"] <- "Total N"
overall_summary.csv[16,"Percentage"] <- ( overall_summary.csv[16,"Value"] /survey.n )

msi.38.tmp <- read_csv("../barriers_by_msi/analysis_of_Q38_barriers_to_inclusion_by_Q22_msi_/output_tables/count_of_responses_to_Q38_inclusion.csv")
overall_summary.csv[17,"Question"] <- "38"
overall_summary.csv[17,"Statistic"] <- "Maximum number of respondents"
overall_summary.csv[17,"Demographic"] <- "MSI Institutional Status"
overall_summary.csv[17,"Value"] <-   sum(as.numeric(msi.38.tmp [2,2:length(msi.38.tmp)]))
overall_summary.csv[17,"Percentage of"] <- "Total N"
overall_summary.csv[17,"Percentage"] <- ( overall_summary.csv[17,"Value"] /survey.n )


#Sex

sex.all.tmp <- read_csv("../barriers_by_sex/analysis_of_Q06_barriers_to_implementation_by_sex_/output_tables/count_of_responses_to_Q6_implementation.csv")
overall_summary.csv[18,"Question"] <- "33, 06, 29-30"
overall_summary.csv[18,"Statistic"] <- "Maximum number of respondents"
overall_summary.csv[18,"Demographic"] <- "Sex"
overall_summary.csv[18,"Value"] <-   sum(as.numeric(sex.all.tmp[2,2:length(sex.all.tmp)]))
overall_summary.csv[18,"Percentage of"] <- "Total N"
overall_summary.csv[18,"Percentage"] <- ( overall_summary.csv[18,"Value"] /survey.n )

sex.38.tmp <- read_csv("../barriers_by_sex/analysis_of_Q38_barriers_to_inclusion_by_sex_/output_tables/count_of_responses_to_Q38_inclusion.csv")
overall_summary.csv[19,"Question"] <- "38"
overall_summary.csv[19,"Statistic"] <- "Maximum number of respondents"
overall_summary.csv[19,"Demographic"] <- "Sex"
overall_summary.csv[19,"Value"] <-   sum(as.numeric(sex.38.tmp[2,2:length(sex.38.tmp)]))
overall_summary.csv[19,"Percentage of"] <- "Total N"
overall_summary.csv[19,"Percentage"] <- ( overall_summary.csv[19,"Value"] /survey.n )


#Undergaduate enrollment

uenroll.all.tmp <- read_csv("../barriers_by_undergraduate_enrollment/analysis_of_Q06_barriers_to_implementation_by_Q24_undergraduate_enrollment_/output_tables/count_of_responses_to_Q6_implementation.csv")
overall_summary.csv[20,"Question"] <- "33, 06, 29-30"
overall_summary.csv[20,"Statistic"] <- "Maximum number of respondents"
overall_summary.csv[20,"Demographic"] <- "Undergaduate enrollment"
overall_summary.csv[20,"Value"] <-   sum(as.numeric(uenroll.all.tmp[2,2:length(uenroll.all.tmp)]))
overall_summary.csv[20,"Percentage of"] <- "Total N"
overall_summary.csv[20,"Percentage"] <- ( overall_summary.csv[20,"Value"] /survey.n )

uenroll.38.tmp <- read_csv("../barriers_by_undergraduate_enrollment/analysis_of_Q38_barriers_to_inclusion_by_Q24_undergraduate_enrollment_/output_tables/count_of_responses_to_Q38_inclusion.csv")
overall_summary.csv[21,"Question"] <- "38"
overall_summary.csv[21,"Statistic"] <- "Maximum number of respondents"
overall_summary.csv[21,"Demographic"] <- "Undergaduate enrollment"
overall_summary.csv[21,"Value"] <-   sum(as.numeric(uenroll.38.tmp[2,2:length(uenroll.38.tmp)]))
overall_summary.csv[21,"Percentage of"] <- "Total N"
overall_summary.csv[21,"Percentage"] <- ( overall_summary.csv[21,"Value"] /survey.n )


#Degree year


degree.yr.all.tmp <- read_csv("../barriers_by_degree_year/analysis_of_Q06_barriers_to_implementation_by_Q18_degree_year_/output_tables/count_of_responses_to_Q6_implementation.csv")
overall_summary.csv[22,"Question"] <- "33, 06, 29-30"
overall_summary.csv[22,"Statistic"] <- "Maximum number of respondents"
overall_summary.csv[22,"Demographic"] <- "Binned degree year"
overall_summary.csv[22,"Value"] <-   sum(as.numeric(degree.yr.all.tmp[2,2:length(degree.yr.all.tmp)]))
overall_summary.csv[22,"Percentage of"] <- "Total N"
overall_summary.csv[22,"Percentage"] <- ( overall_summary.csv[22,"Value"] /survey.n )

degree.yr.38.tmp <- read_csv("../barriers_by_degree_year/analysis_of_Q38_barriers_to_inclusion_by_Q18_degree_year_/output_tables/count_of_responses_to_Q38_inclusion.csv")
overall_summary.csv[23,"Question"] <- "38"
overall_summary.csv[23,"Statistic"] <- "Maximum number of respondents"
overall_summary.csv[23,"Demographic"] <- "Binned degree year"
overall_summary.csv[23,"Value"] <-   sum(as.numeric(degree.yr.38.tmp[2,2:length(degree.yr.38.tmp)]))
overall_summary.csv[23,"Percentage of"] <- "Total N"
overall_summary.csv[23,"Percentage"] <- ( overall_summary.csv[23,"Value"] /survey.n )



#calculate error for sample sizes

N.population <- 100000 # estimated population of full/part-time life science faculty in US
P.proportion_size <-  .5 
z.confidence.interval <-  1.96

overall_summary.csv <- overall_summary.csv%>%
  mutate(question_margin_of_error = (z.confidence.interval * sqrt(P.proportion_size*(1-P.proportion_size)))/
           sqrt(((N.population - 1)*overall_summary.csv$Value)/(N.population - overall_summary.csv$Value))
           )

write_csv(overall_summary.csv, "./output_tables/summary_sample_sizes_for_selected_demographics.csv")






