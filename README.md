
---

Creating CSV files from Excel;
Exported individual sheets as MS-DOS Comma Separated (.csv)
Microsoft Excel for Mac version 15.25.1 (160826)

Files created;



|file name|notes|valid responses (according to the xls sheet)|Folder|
|---------|-----|--------------------------------------|-----|
|00_raw.csv|raw unscored responses dataset|NA|
|01_preventing_you.csv|scored according to "What is preventing yOu frOm including biOinfOrmatics cOntent in these cOurses?" question|517|data_cleaning_scripts/combine_excel_outputs_to_csv/
|02_barriers_dev_imp.csv|scored according to "In your opinion, are additional undergraduate courses with bioinformatics content needed at your institution? Optional: Please describe briefly; include any barriers to development and/or implementation." question|371|data_cleaning_scripts/combine_excel_outputs_to_csv/
|03_technical_barriers.csv|scored according to "Optional: Please describe." question|318|data_cleaning_scripts/combine_excel_outputs_to_csv/
|04_important_challenges.csv|scored according to "In your opinion, what do you think are the most important challenges currently facing those educating undergrad life scientists in bioinformatics" question|744|data_cleaning_scripts/combine_excel_outputs_to_csv/
|05_student_deficiencies.csv|scored according to "In your opinion, what bioinformatics skill(s) are incoming graduate students most deficient in?" question|72|data_cleaning_scripts/combine_excel_outputs_to_csv/
|combine_coded_csvs.R|This script assembles the separate excel sheets (e.g. 01-05_....csv) into one csv file and improves header names by combining the two header rows into one header|NA|data_cleaning_scripts/combine_excel_outputs_to_csv/
|combined_raw_df.csv|Output of combine_coded_csvs.R |NA|
|binary_scoring.R|Transforms combined_raw_df.csv by changing all NA scores to 0, all other scores are 1 |NA|
|binary_scores.csv|Output of binary_scoring.R |NA|
|summed_reduced_factors.R|This script takes binary_scores.csv and creates two new column types: A **sum** column takes similar groupings of barriers and sums all responses - the sum **does not** include  barrier columns labeled "general" as the general columns are which are binary indications of any score in a category (e.g. faculty issues). A **reduced** column transforms the value of a **sum** column into a binary; 0 for any sum == 0, 1 for any sum != 0 | NA|
|summed_reduced_factors.csv|Output of summed_reduced_factors.R||
|Header_mapping.tab|Tab-delimited mapping of survey values||
|Header_mapping.txt|Tab-delimited (as a .txt file) mapping of survey values||
|decoded_responses.R|Takes the summed_reduced_factors.csv file and replaces all survey responses (not scored barriers) and transforms values from drop-down selections (e.g. 1,2,3) into their human-readable responses (e.g. male, female, other). The decoded response retains the value as recorded in the survey output for (e.g. Q14_Sex the decoded values "1_Female" indicates that the survey score was originally "1" was interpreted here as Female). The survey output was kept in place to avoid possible manual error in the decoding script.|NA|
|decoded_df.csv|Output of decoded_responses.R|NA|


# notes
The notes in the tabel above should improve what question is "scored according to" by having the question number


---


