#README

### NIBLSE 2016 Survey on Bioinformatics in Undergraduate Education
### Analysis of barriers to bioinformatics integration

NIBLSE is supported by NSF Award #1539900

NIBLSE homepage on QUBES hub: [https://qubeshub.org/groups/niblse](https://qubeshub.org/groups/niblse)

## Items in this directory

|Item|Notes|
|----|-----|
|01_combine_excel_outputs_to_csv/|Contains `combine_coded_csvs.R` as well as raw data files; <br>used to combine several Excel sheets into a single csv file and adjust missing values from NULL to NA; <br>*output:* `combined_raw_df.csv`|
|02_transform_coding_to_binary/|Contains `binary_scoring.R`; <br>used to adjust scored free-text responses into a numeric format where 'NA' values are transformed to '0' ; <br>*output:* `binary_scores.csv`|
|03_sum_and_reduce_barrier_coding/|Contains `summed_reduced_factors.R`; <br>used to create columns that summarize scored free-text responses into larger grouping; <br>*output:* `summed_reduced_factors.csv`|
|04_decode_survey_responses/|Contains `decoded_responses.R`as well as survey mapping header files (`Header_mapping.tab`, `Header_mapping.txt` ); <br>used in conjunction with `Header_mapping.txt` to transform numeric survey codings into readable options; <br>*output:* `decoded_df.csv` <br>**Note:** This output is used in the majority analyses, except those analyzing survey responses by ethnicity.|
|05_adjust_ethnicities/|Contains `adjust_ethnicities.R`; <br>used to adjust survey ethnicity information into categories that match with standard race/ethnicity demographic categories and also separately categorizes respondents identified as races/ethnicities underrepresented in science; <br>*output:* `decoded_df_w_ethnicity.csv`|

## Additional notes

### Script details

*01_combine_excel_outputs_to_csv/combine_coded_csvs.R*

- From `data.xlxs`, 6 files representing coded free-text responses   
(`01_preventing_you.csv`, `02_barriers_dev_imp.csv`, `03_technical_barriers.csv`, `04_important_challenges.csv`, `05_student_deficiencies.csv`) and other survey responses (`master.csv`) were exported from Excel (Microsoft Excel for Mac version 15.25.1 (160826)) as CSV files ( MS-DOS Comma Separated (.csv)). 

*02_transform_coding_to_binary/binary_scoring.R*
- Free-text responses from the survey were categorized (presence = '1') into several categories. NA values in these scoring columns were assigned a value of 0; NA values in other survey responses were not altered. 

*03_sum_and_reduce_barrier_coding/summed_reduced_factors.R*

- For coded rows, new variables were created to summarize coded data in one of two ways
    - Summed: For every individual (row) and for all related coded responses (e.g Faculty Time, Faculty Preparation, ...) a super-category (e.g. Faculty) is created, and multiple issues reported by a single individual are summed numerically: (e.g. Faculty Time (1), Faculty Preparation (1) -> 'Faculty'(2));row header: 'qXX_ISSUE_reduced'
    - Reduced: For every individual (row) and for all related coded responses (e.g Faculty Time, Faculty Preparation, ...) a super-category (e.g. Faculty) is created, and multiple issues reported by a single individual are assigned '1' if at least one issue is scored in the related categories: (e.g. Faculty Time (1), Faculty Preparation (1) -> 'Faculty'(1)); row header: 'qXX_ISSUE_reduced'

*04_decode_survey_responses/decoded_responses.R*
- multiple-option questions from the Qualtrics survey are reported as numerical values. These values are modified to include intelligible entries on the survey by extracting their decoded values from the `Header_mapping.txt` file (converted from .tab to .txt). During the conversion, the original Qualtrics numerical output is retained (e.g. '2' -> '2_Male'). 

*05_adjust_ethnicities/adjust_ethnicities.R*
- Adjusts Qualtrics scoring which collected information on hispanic ethnicity in a separate column. Respondents categorized into the following categories:
    - America Indian/Alaskan Native
    - Black not hispanic
    - Hispanic
    - Native Hawaiian or Other Pacific Islander
    - Asian
    - White

a separate category 'underrepresented' was created for non-white, non-asian respondents who are not considered underrepresented in life science.  



