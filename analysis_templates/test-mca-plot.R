# load required libraries
require(ggplot2)
require(tidyverse)
require(reshape2)
require(pwr)
require(FactoMineR)
require(factoextra)

#Read in data
data <- read_csv("../data_cleaning_scripts/04_decode_survey_responses/output/decoded_df.csv")

#remove non-us respondants

#remove respondants not in US/Puerto Rico
remove.non.us.repondants <- function(df){
  countries <- c("United States","Puerto Rico")
  df <- df%>%
    filter(Country_Country %in% countries )
  return(df)
}

data <- remove.non.us.repondants(data)


# Select relavant columns for analysis

data.relavant  <- data%>%
  select(q06_Faculty_issues_reduced,
         q06_Curriculum_issues_reduced,
         q06_Student_issues_reduced,
         q06_Institutional_issues_reduced,
         q06_Resource_issues_reduced,
         q06_Facilities_issues_reduced,
         q33_Curriculum_issues_reduced,
         q33_Faculty_issues_reduced,
         q33_Facility_issues_reduced,
         q33_Resources_issues_reduced,
         q33_Resources_issues_reduced,
         q33_Student_issues_reduced,
         q33_Curriculum_issues_reduced,
         q29_Faculty_issues_reduced, 
         q29_Facilities_issues_reduced, 
         q29_Resources_issues_reduced,
         q29_Institutional_issues_reduced, 
         q29_Student_issues_reduced, 
         q29_Curriculum_issues_reduced, 
         q33_Institutional_issues_reduced,
         q38_Faculty_issues_reduced, 
         q38_Curriculum_issues_reduced, 
         q38_Resources_issues_reduced, 
         q38_Student_issues_reduced, 
         q38_Facilities_issues_reduced, 
         q38_Institutional_issues_reduced, 
         q38_State_issues_reduced, 
         q38_Accredited_issues_reduced,
         Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...,
         Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training., 
         Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur..., 
         Q14_Sex,
         Q15_Race, 
         Q16_Ethnicity,
         Q17_Highest.earned.degree..If..other...please.explain.,
         Q18_Year.of.highest.earned.degree.,
         Q21_What.is.the.Carnegie.classification.of.your.institution., 
         Q22_Is.your.institution.classified.as.minority.serving., 
         Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution., 
         Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution., 
         Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju..., 
         Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..,
         Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g....., 
         Q57_Please.select.the.statement.belOw.that.best.describes.yOu., 
         State_State, 
         Region_Region)

########## MODIFY ETHNICTY REPONSES FOR HISPANIC/NON-HISPANIC and UNDERREPRESENTED ETHNICITIES

# Unite race and ethnicity
data.relavant <- data.relavant%>%
  unite(combined_ethnicity, Q15_Race, Q16_Ethnicity, sep = "_", remove = FALSE)

#Fix NA values
data.relavant$combined_ethnicity[data.relavant$combined_ethnicity == "NA_NA"] <- NA

#generate column for tracked ethnicity values
#America Indian/Alaskan Native
#Black not hispanic
#Hispanic
#Native Hawaiian or Other Pacific Islander
#Asian
#White

data.relavant <- data.relavant%>%
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

data.relavant <- data.relavant%>%
  mutate(representation = ifelse (test = tracked_ethnicities == "Asian" |
                                    tracked_ethnicities == "White", 
                                  yes = "Non-underrepresented", 
                                  no = "underrepresented"))









#manually format nice names

#reduced columns

#q6
data.relavant$q06_Faculty_issues_reduced[data.relavant$q06_Faculty_issues_reduced == "1"] <- "Q6 FACULTY BARRIERS"
data.relavant$q06_Curriculum_issues_reduced[data.relavant$q06_Curriculum_issues_reduced == "1"] <- "Q6 CURRICULUM BARRIERS"
data.relavant$q06_Student_issues_reduced[data.relavant$q06_Student_issues_reduced== "1"] <- "Q6 STUDENT BARRIERS"
data.relavant$q06_Institutional_issues_reduced[data.relavant$q06_Institutional_issues_reduced == "1"] <- "Q6 INSTITUTIONAL BARRIERS"
data.relavant$q06_Resource_issues_reduced[data.relavant$q06_Resource_issues_reduced == "1"] <- "Q6 RESOURCE BARRIERS"
data.relavant$q06_Facilities_issues_reduced[data.relavant$q06_Facilities_issues_reduced == "1"] <- "Q6 FACILITIES BARRIERS"
data.relavant$q06_Faculty_issues_reduced[data.relavant$q06_Faculty_issues_reduced == "0"] <- "Q6 no faculty barriers"
data.relavant$q06_Curriculum_issues_reduced[data.relavant$q06_Curriculum_issues_reduced == "0"] <- "Q6 no curriculum barriers"
data.relavant$q06_Student_issues_reduced[data.relavant$q06_Student_issues_reduced== "0"] <- "Q6 no student barriers"
data.relavant$q06_Institutional_issues_reduced[data.relavant$q06_Institutional_issues_reduced == "0"] <- "Q6 no institutional barriers"
data.relavant$q06_Resource_issues_reduced[data.relavant$q06_Resource_issues_reduced == "0"] <- "Q6 no resource barriers"
data.relavant$q06_Facilities_issues_reduced[data.relavant$q06_Facilities_issues_reduced == "0"] <- "Q6 no facilities barriers"


#q29
data.relavant$q29_Faculty_issues_reduced[data.relavant$q29_Faculty_issues_reduced == "1"] <- "Q29 FACULTY BARRIERS"
data.relavant$q29_Curriculum_issues_reduced[data.relavant$q29_Curriculum_issues_reduced == "1"] <- "Q29 CURRICULUM BARRIERS"
data.relavant$q29_Student_issues_reduced[data.relavant$q29_Student_issues_reduced== "1"] <- "Q29 STUDENT BARRIERS"
data.relavant$q29_Institutional_issues_reduced[data.relavant$q29_Institutional_issues_reduced == "1"] <- "Q29 INSTITUTIONAL BARRIERS"
data.relavant$q29_Resources_issues_reduced[data.relavant$q29_Resources_issues_reduced == "1"] <- "Q29 RESOURCE BARRIERS"
data.relavant$q29_Facilities_issues_reduced[data.relavant$q29_Facilities_issues_reduced == "1"] <- "Q29 FACILITIES BARRIERS"
data.relavant$q29_Faculty_issues_reduced[data.relavant$q29_Faculty_issues_reduced == "0"] <- "Q29 no faculty barriers"
data.relavant$q29_Curriculum_issues_reduced[data.relavant$q29_Curriculum_issues_reduced == "0"] <- "Q29 no curriculum barriers"
data.relavant$q29_Student_issues_reduced[data.relavant$q29_Student_issues_reduced== "0"] <- "Q29 no student barriers"
data.relavant$q29_Institutional_issues_reduced[data.relavant$q29_Institutional_issues_reduced == "0"] <- "Q29 no institutional barriers"
data.relavant$q29_Resources_issues_reduced[data.relavant$q29_Resources_issues_reduced == "0"] <- "Q29 no resource barriers"
data.relavant$q29_Facilities_issues_reduced[data.relavant$q29_Facilities_issues_reduced == "0"] <- "Q29 no facilities barriers"


#q33
data.relavant$q33_Facility_issues_reduced[data.relavant$q33_Facility_issues_reduced == "1"] <- "Q33 FACILITIES BARRIERS"
data.relavant$q33_Faculty_issues_reduced[data.relavant$q33_Faculty_issues_reduced == "1"] <- "Q33 FACULTY BARRIERS"
data.relavant$q33_Curriculum_issues_reduced[data.relavant$q33_Curriculum_issues_reduced == "1"] <- "Q33 CURRICULUM BARRIERS"
data.relavant$q33_Institutional_issues_reduced[data.relavant$q33_Institutional_issues_reduced == "1"] <- "Q33 INSTITUTIONAL BARRIERS"
data.relavant$q33_Resources_issues_reduced[data.relavant$q33_Resources_issues_reduced == "1"] <- "Q33 RESOURCE BARRIERS"
data.relavant$q33_Student_issues_reduced[data.relavant$q33_Student_issues_reduced == "1"] <- "Q33 STUDENT BARRIERS"
data.relavant$q33_Facility_issues_reduced[data.relavant$q33_Facility_issues_reduced == "0"] <- "Q33 no reported faclities barriers"
data.relavant$q33_Faculty_issues_reduced[data.relavant$q33_Faculty_issues_reduced == "0"] <- "Q33 no reported faculty barriers"
data.relavant$q33_Curriculum_issues_reduced[data.relavant$q33_Curriculum_issues_reduced == "0"] <- "Q33 no reported curriculumn barriers"
data.relavant$q33_Institutional_issues_reduced[data.relavant$q33_Institutional_issues_reduced == "0"] <- "Q33 no reported institutional barriers"
data.relavant$q33_Resources_issues_reduced[data.relavant$q33_Resources_issues_reduced == "0"] <- "Q33 no reported resource barriers"
data.relavant$q33_Student_issues_reduced[data.relavant$q33_Student_issues_reduced == "0"] <- "Q33 no reported student barriers"

#q38
data.relavant$q38_Faculty_issues_reduced[data.relavant$q38_Faculty_issues_reduced=="1"] <- "Q38 FACULTY BARRIERS"
data.relavant$q38_Curriculum_issues_reduced[data.relavant$q38_Curriculum_issues_reduced=="1"] <- "Q38 CURRICULUM BARRIERS" 
data.relavant$q38_Resources_issues_reduced[data.relavant$q38_Resources_issues_reduced=="1"] <- "Q38 RESOURCE BARRIERS" 
data.relavant$q38_Student_issues_reduced[data.relavant$q38_Student_issues_reduced=="1"] <- "Q38 STUDENT BARRIERS" 
data.relavant$q38_Facilities_issues_reduced[data.relavant$q38_Facilities_issues_reduced=="1"] <- "Q38 FACILITIES BARRIERS"
data.relavant$q38_Institutional_issues_reduced[data.relavant$q38_Institutional_issues_reduced=="1"] <- "Q38 INSTITUTIONAL BARRIERS" 
data.relavant$q38_State_issues_reduced[data.relavant$q38_State_issues_reduced=="1"] <- "Q38 STATE BARRIERS" 
data.relavant$q38_Accredited_issues_reduced[data.relavant$q38_Accredited_issues_reduced=="1"] <- "Q38 ACCREDITATION BARRIERS"
data.relavant$q38_Faculty_issues_reduced[data.relavant$q38_Faculty_issues_reduced=="0"] <- "Q38 no faculty barriers"
data.relavant$q38_Curriculum_issues_reduced[data.relavant$q38_Curriculum_issues_reduced=="0"] <- "Q38 no curriculum barriers" 
data.relavant$q38_Resources_issues_reduced[data.relavant$q38_Resources_issues_reduced=="0"] <- "Q38 resources no barriers" 
data.relavant$q38_Student_issues_reduced[data.relavant$q38_Student_issues_reduced=="0"] <- "Q38 no student barriers" 
data.relavant$q38_Facilities_issues_reduced[data.relavant$q38_Facilities_issues_reduced=="0"] <- "Q38 no facilities barriers"
data.relavant$q38_Institutional_issues_reduced[data.relavant$q38_Institutional_issues_reduced=="0"] <- "Q38 no institutional barriers" 
data.relavant$q38_State_issues_reduced[data.relavant$q38_State_issues_reduced=="0"] <- "Q38 no state barriers" 
data.relavant$q38_Accredited_issues_reduced[data.relavant$q38_Accredited_issues_reduced=="0"] <- "Q38 no accreditation barriers"


#Q1
data.relavant$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...[data.relavant$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "1_Dedicated course for life-science majors"] <- "Teaching: Dedicated Bioinformatics Course"
data.relavant$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...[data.relavant$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "2_Include 'substantial' bioinformatics in courses for life-science majors"  ] <- "Teaching: Integrating Bioinformatics"
data.relavant$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...[data.relavant$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "3_Do NOT currently, but would like to include 'substantial' bioinformatics in courses for life-science majors"] <- "Teaching: Not Integrating Bioinformatics"

#Q3
data.relavant$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.[data.relavant$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training. == "1_No training/experience"] <- "Bioinformatics Training: None"
data.relavant$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.[data.relavant$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training. == "2_No formal training (self-taught)"] <- "Bioinformatics Training: Self Taught"
data.relavant$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.[data.relavant$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training. == "3_Short workshop/bootcamp"] <- "Bioinformatics Training: Short Workshop"
data.relavant$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.[data.relavant$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training. == "4_Some undergraduate courses"] <- "Bioinformatics Training: Some Undergraduate Training"
data.relavant$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.[data.relavant$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training. == "6_Undergraduate certificate"] <- "Bioinformatics Training: Undergraduate Certificate"
data.relavant$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.[data.relavant$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training. == "7_Undergraduate degree"] <- "Bioinformatics Training: Undergraduate Degree"
data.relavant$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.[data.relavant$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training. == "8_Post-graduate certificate"] <- "Bioinformatics Training: Post-graduate Certificate"
data.relavant$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.[data.relavant$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training. == "9_Graduate course	Graduate degree"] <- "Bioinformatics Training: Graduate Course/Degree"

#Q5
data.relavant$Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur...[data.relavant$Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur... == "1_Yes" ] <- "Additional Bioinformatics Courses Needed"
data.relavant$Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur...[data.relavant$Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur... == "2_No" ] <- "No additional Bioinformatics Courses Needed"

#14
data.relavant$Q14_Sex[data.relavant$Q14_Sex == "1_Female"  ] <- "Female"
data.relavant$Q14_Sex[data.relavant$Q14_Sex == "2_Male"   ] <- "Male"

#17
data.relavant$Q17_Highest.earned.degree..If..other...please.explain.[data.relavant$Q17_Highest.earned.degree..If..other...please.explain. == "1_B.S. (or equivalent)" ] <- "B.S. Degree"
data.relavant$Q17_Highest.earned.degree..If..other...please.explain.[data.relavant$Q17_Highest.earned.degree..If..other...please.explain. == "2_M.S. (or equivalent)"  ] <- "M.S. Degree"
data.relavant$Q17_Highest.earned.degree..If..other...please.explain.[data.relavant$Q17_Highest.earned.degree..If..other...please.explain. == "3_Professional degree (e.g., M.D.)" ] <- "Professional Degree"
data.relavant$Q17_Highest.earned.degree..If..other...please.explain.[data.relavant$Q17_Highest.earned.degree..If..other...please.explain. == "4_Ph.D. (or equivalent)"  ] <- "PhD"

##18
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "1_2016"] <- "2016"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "2_2015"] <- "2015"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "3_2014"] <- "2014"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "4_2013"] <- "2013"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "5_2012"] <- "2012"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "6_2011"] <- "2011"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "7_2010"] <- "2010"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "8_2009"] <- "2009"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "9_2008"] <- "2008"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "10_2007"] <- "2007"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "11_2006"] <- "2006"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "12_2005"] <- "2005"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "13_2004"] <- "2004"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. ==  "14_2003"] <- "2003"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "15_2002"] <- "2002"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "16_2001"] <- "2001"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "17_2000"] <- "2000"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "18_1999"] <- "1999"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "19_1998"] <- "1998"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "20_1997"] <- "1997"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "21_1996"] <- "1996"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "22_1995"] <- "1995"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "23_1994"] <- "1994"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "24_1993"] <- "1993"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "25_1992"] <- "1992"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "26_1991"] <- "1991"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "27_1990"] <- "1990"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "28_1989"] <- "1989"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "29_1988"] <- "1989"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "30_1987"] <- "1987"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "31_1986"] <- "1986"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "32_1985"] <- "1985"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "33_1984"] <- "1984"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. ==  "34_1983"] <- "1983"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "35_1982"] <- "1982"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "36_1981"] <- "1981"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "37_1980"] <- "1980"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "38_1979"] <- "1979"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "39_1978"] <- "1978"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "40_1977"] <- "1977"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "41_1976"] <- "1976"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "42_1975"] <- "1975"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "43_1974"] <- "1974"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "44_1973"] <- "1973"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "45_1972"] <- "1972"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "46_1971"] <- "1971"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "47_1970"] <- "1970"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "48_1969"] <- "1969"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "49_1968"] <- "1968"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "50_1967"] <- "1967"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "51_1966"] <- "1966"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "52_1965"] <- "1965"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "53_1964"] <- "1964"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "54_1963"] <- "1963"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "55_1962"] <- "1962"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "56_1961"] <- "1961"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "57_1960"] <- "1960"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "58_1959"] <- "1959"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "59_1958"] <- "1958"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "60_1957"] <- "1957"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "61_1956"] <- "1956"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "62_1955"] <- "1955"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "63_1954"] <- "1954"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "64_1953"] <- "1953"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "65_1952"] <- "1952"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "66_1951"] <- "1951"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "67_1950"] <- "1950"
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "68_Rather Not Say"] <- "Rather Not Say"


#21
data.relavant$Q21_What.is.the.Carnegie.classification.of.your.institution.[data.relavant$Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College"] <- "Carnegie Classification: Associates"
data.relavant$Q21_What.is.the.Carnegie.classification.of.your.institution.[data.relavant$Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College"] <- "Carnegie Classification: Baccalaureate"
data.relavant$Q21_What.is.the.Carnegie.classification.of.your.institution.[data.relavant$Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)"] <- "Carnegie Classification: Masters"
data.relavant$Q21_What.is.the.Carnegie.classification.of.your.institution.[data.relavant$Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)"] <- "Carnegie Classification: Doctoral"






#filter out unsure respondants

#filter out non-relavant responders
data.filtered <- data.relavant%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... != "4_Graduate supervisors in the life sciences")%>%
  filter(Q14_Sex != "3_Rather not say")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. != "5_Don't know")%>%
  filter(Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur... != "3_Don't know")%>%
  filter(Q17_Highest.earned.degree..If..other...please.explain. != "4_Ph.D. (or equivalent)")%>%
  



data.active <- data%>%
  select(q33_Faculty_issues_reduced,
         q33_Curriculum_issues_reduced,
         q33_Facility_issues_reduced,
         q33_Institutional_issues_reduced,
         q33_Resources_issues_reduced,
         q33_Student_issues_reduced,
         Q21_What.is.the.Carnegie.classification.of.your.institution.,
         Q14_Sex,
         Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.,
         Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...)










reduced.mca <- MCA(X = as.matrix(data.active),
                     quali.sup = 1:6,
                     graph = FALSE)





fviz_mca_biplot(reduced.mca, 
                invisible=c("ind"),
                habillage =  "Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...",
                addEllipses = TRUE, 
                labelsize = 3,
                ellipse.level = 0.95, 
                title = "test")+
  theme_minimal()


