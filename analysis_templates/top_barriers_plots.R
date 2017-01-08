# load required libraries
require(ggplot2)
require(tidyverse)
require(reshape2)

############ LOAD THE SUMMARIES RESPONSE DATA ###########################################


#read question dataframes and concatenate 

q21.carnegie.q38 <- read_csv("./barriers_by_carnegie_classification/analysis_of_Q38_barriers_to_inclusion_by_Q21_carnegie_classification_/output_tables/tally_of_raw_score_of_Q38_barriers_to_inclusion_by_Q21_carnegie_classification_proportions.csv")
q21.carnegie.q33 <- read_csv("./barriers_by_carnegie_classification/analysis_of_Q33_educator_challenges_by_Q21_carnegie_classification_/output_tables/tally_of_raw_score_of_Q33_educator_challenges_by_Q21_carnegie_classification_proportions.csv")
q21.carnegie.q30 <- read_csv("./barriers_by_carnegie_classification/analysis_of_Q29-30_technical_challenges_by_Q21_carnegie_classification_/output_tables/tally_of_raw_score_of_Q29-30_technical_challenges_by_Q21_carnegie_classification_proportions.csv")
q21.carnegie.q06 <- read_csv("./barriers_by_carnegie_classification/analysis_of_Q06_barriers_to_implementation_by_Q21_carnegie_classification_/output_tables/tally_of_raw_score_of_Q06_barriers_to_implementation_by_Q21_carnegie_classification_proportions.csv")

# response dataframe
# This dataframe will have N and is the same for all 4 stratafying questions

q21.responses <- read_csv("./barriers_by_carnegie_classification/analysis_of_Q38_barriers_to_inclusion_by_Q21_carnegie_classification_/output_tables/count_of_responses_to_Q38_barriers_to_inclusion.csv")

caclculate.n <- function(df){
  #scrape the count of faculty in stratafying category
  df <- t(df)
  df <- data.frame(df[-1,], stringsAsFactors = FALSE)
  n <- sum(as.numeric(df$X2))
  return(as.numeric(n))
  
}

overall.n <- caclculate.n(q21.responses)

##### Add N to question dataframes

q21.carnegie.q06[,"N"] <- overall.n
q21.carnegie.q30[,"N"] <- overall.n
q21.carnegie.q33[,"N"] <- overall.n
q21.carnegie.q38[,"N"] <- overall.n

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

q21.carnegie.q06 <- caculate.top5.barriers(q21.carnegie.q06)
q21.carnegie.q30 <- caculate.top5.barriers(q21.carnegie.q30)
q21.carnegie.q33 <- caculate.top5.barriers(q21.carnegie.q33)
q21.carnegie.q38 <- caculate.top5.barriers(q21.carnegie.q38)
