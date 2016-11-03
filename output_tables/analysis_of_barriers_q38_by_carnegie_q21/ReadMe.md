#ReadMe
R version 3.3.0 (2016-05-03)

Input for script:decoded_df.csv

input dataframe was filtered to include only US and Puerto Rico

## q38_by_q21_counts_by_sub_catagories.csv 
contains the sum of responses for all scored sub categories where respondants indicated their carnegie classification; users who gave an answer to q38 but did notindicate a carnegie catagory or who were unsure are removed


## q38_by_21_counts_and_chi_barriers_by_sub_catagory.csv 

contains the sum of responses for all scored sub-categories where respondents indicated their Carnegie classification. 
Users who gave an answer to q38 but did not indicate a Carnegie categories or who were unsure are removed. chisq.test from the R stats package; simulate.p.value = TRUE to account for small values of n

## q38_by_21_signifigant_barriers_by_sub_catagory.csv
 
      contains chisq.test values from **q38_by_21_signifigant_barriers_by_sub_catagory.csv** that were 
      0.05 or less.

## q38_by_q21_counts_by_summed_super_categories.csv
 contains the sum of responses 
      for all scored super-categories where respondants indicated their Carnegie classification.
 
      Users who gave an answer to q38 but did not indicate a Carnegie categories or who were 
      unsure are removed

## q38_by_q21_counts_by_summed_super_categories.csv
 contains the sum of responses 
      for all scored super-categories where respondants indicated their Carnegie classification.
 
      Users who gave an answer to q38 but did not indicate a Carnegie categories or who were 
      unsure are removed

## q38_by_21_counts_and_chi_barriers_by_super_catagory.csv 
 contains the sum of responses 
      for all scored super-categories where respondents indicated their Carnegie classification. 
 
      Users who gave an answer to q38 but did not indicate a Carnegie categories or who were unsure 
      are removed. 
 chisq.test from the R stats package; simulate.p.value = TRUE to account for small
      values of n
