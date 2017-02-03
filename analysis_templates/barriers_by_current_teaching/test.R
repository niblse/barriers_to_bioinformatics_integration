



require(ggplot2)
require(tidyverse)
require(reshape2)
require(corrplot)
require(pwr)



############ LOAD THE PREPARED SURVEY DATA ###########################################
#read in cleaned dataframe "decoded_df.csv"
master.df <- read_csv("../../data_cleaning_scripts/04_decode_survey_responses/output/decoded_df.csv")