
# load required libraries
require(ggplot2)
require(tidyverse)
require(reshape2)
require(pwr)
require(FactoMineR)
require(factoextra)

#Read in a sample dataset

data <- read_csv("./barriers_by_carnegie_classification/prop_analysis_of_Q38_barriers_to_inclusion_by_Q21_carnegie_classification_/output_tables_prop/sum_table_Q38_barriers_to_inclusion_by_Q21_carnegie_classification.csv")

#subset relavant values