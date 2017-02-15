# this script creates a dataframe where the ehnicities reported are cleaned to account for hispanic/non-hispanic, and to indicate underrepresented groups in science

############ LOAD THE PREPARED SURVEY DATA ###########################################
#read in cleaned dataframe "decoded_df.csv"
master.df <- read_csv("../04_decode_survey_responses/output/decoded_df.csv")    

########## MODIFY ETHNICTY REPONSES FOR HISPANIC/NON-HISPANIC and UNDERREPRESENTED ETHNICITIES

# Unite race and ethnicity
master.df <- master.df%>%
  unite(combined_ethnicity, Q15_Race, Q16_Ethnicity, sep = "_", remove = FALSE)

#Fix NA values
master.df$combined_ethnicity[master.df$combined_ethnicity == "NA_NA"] <- NA

#generate column for tracked ethnicity values
#America Indian/Alaskan Native
#Black not hispanic
#Hispanic
#Native Hawaiian or Other Pacific Islander
#Asian
#White

master.df <- master.df%>%
  mutate(tracked_ethnicities = ifelse( test = combined_ethnicity == "1_American Indian or Alaska Native_2_Not Hispanic or Latino"
                                       , yes = "American Indian or Alaskan Native", 
                                       no = ifelse( test = combined_ethnicity == "2_Asian_2_Not Hispanic or Latino" |
                                                      combined_ethnicity == "2_Asian_3_Rather not say" |
                                                      combined_ethnicity == "2_Asian_NA"
                                                    , yes = "Asian", 
                                                    no = ifelse( test = combined_ethnicity == "3_Black or African American_1_Hispanic or Latino" |
                                                                   combined_ethnicity == "5_White_1_Hispanic or Latino" |
                                                                   combined_ethnicity == "6_Rather not say_1_Hispanic or Latino" |
                                                                   combined_ethnicity == "NA_1_Hispanic or Latino"
                                                                 , yes = "Hispanic", 
                                                                 no = ifelse( test = combined_ethnicity == "3_Black or African American_2_Not Hispanic or Latino" |
                                                                                combined_ethnicity == "3_Black or African American_NA"
                                                                              , yes = "Black or African American",
                                                                              no = ifelse( test = combined_ethnicity == "4_Native Hawaiian or Other Pacific Islander_2_Not Hispanic or Latino"
                                                                                           , yes = "Native Hawaiian or Other Pacific Islander", 
                                                                                           no = ifelse( test = combined_ethnicity == "5_White_2_Not Hispanic or Latino" |
                                                                                                          combined_ethnicity == "5_White_3_Rather not say" |
                                                                                                          combined_ethnicity == "5_White_NA" 
                                                                                                        , yes =  "White", 
                                                                                                        no = ifelse( test = combined_ethnicity == "6_Rather not say_NA" |
                                                                                                                       combined_ethnicity == "NA_2_Not Hispanic or Latino" 
                                                                                                                     , yes = NA, no = NA))))))))

#generate column for underrepresented vs non-underrepresented

master.df <- master.df%>%
  mutate(representation = ifelse (test = tracked_ethnicities == "Asian" |
                                    tracked_ethnicities == "White", 
                                  yes = "Non-underrepresented", 
                                  no = "underrepresented"))

# write dataframe for future refference

dir.create("./output")
write_csv(master.df, "./output/decoded_df_w_ethnicity.csv")