#ReadMe
R version 3.3.0 (2016-05-03)

## q06_by_q21_counts_by_sub_categories.csv 
 contains the sum of responses 
      for all scored sub-categories where respondants indicated their Carnegie classification.
 
      Users who gave an answer to q06 but did not indicate a Carnegie categories or who were 
      unsure are removed

## q06_by_21_counts_and_chi_barriers_by_sub_catagory.csv 
 contains the sum of responses 
      for all scored sub-categories where respondents indicated their Carnegie classification. 
 
      Users who gave an answer to q06 but did not indicate a Carnegie categories or who were unsure 
      are removed. 
 chisq.test from the R stats package; simulate.p.value = TRUE to account for small
      values of n

## q06_by_21_signifigant_barriers_by_sub_catagory.csv
 
      contains chisq.test values from **q06_by_21_signifigant_barriers_by_sub_catagory.csv** that were 
      0.05 or less.
