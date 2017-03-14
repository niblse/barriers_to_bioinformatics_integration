# load required libraries
require(ggplot2)
require(tidyverse)
require(reshape2)
require(pwr)
require(FactoMineR)
require(factoextra)

#Read in data
data <- read_csv("../data_cleaning_scripts/04_decode_survey_responses/output/decoded_df.csv")
data.ethnicity <- read_csv("../data_cleaning_scripts/05_adjust_ethnicities/output/decoded_df_w_ethnicity.csv")
data.training <- read_csv("../data_cleaning_scripts/06_adjust_bioinformatics_training/output/decoded_df_w_faculty_preperation.csv")
data.degree <- read_csv("../data_cleaning_scripts/07_adjust_degree_year/output/decoded_df_w_bin_degree_years.csv")
#remove non-us respondents

#remove respondents not in US/Puerto Rico
remove.non.us.repondants <- function(df){
  countries <- c("United States","Puerto Rico")
  df <- df%>%
    filter(Country_Country %in% countries )
  return(df)
}

data <- remove.non.us.repondants(data)
data.ethnicity<- remove.non.us.repondants(data.ethnicity)
data.training<- remove.non.us.repondants(data.training)
data.degree <- remove.non.us.repondants(data.degree)
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
         q33_Student_issues_reduced,
         q33_Institutional_issues_reduced,
         q29_Faculty_issues_reduced, 
         q29_Facilities_issues_reduced, 
         q29_Resources_issues_reduced,
         q29_Institutional_issues_reduced, 
         q29_Student_issues_reduced, 
         q29_Curriculum_issues_reduced, 
         q38_Faculty_issues_reduced, 
         q38_Curriculum_issues_reduced, 
         q38_Resources_issues_reduced, 
         q38_Student_issues_reduced, 
         q38_Facilities_issues_reduced, 
         q38_Institutional_issues_reduced, 
         q38_State_issues_reduced, 
         q38_Accredited_issues_reduced,
         Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...,
         Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur..., 
         Q14_Sex,
         Q15_Race, 
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

data.ethnicity.relavant <- data.ethnicity%>%
  select(tracked_ethnicities, representation)

data.training.relavant <- data.training%>%
  select(faculty_preperation)

data.degree.relavant <- data.degree%>%
  select(bin_degree_yrs)

data.relavant <- bind_cols(data.relavant, data.ethnicity.relavant, data.training.relavant,data.degree.relavant )


#manually format nice names; "Don't know responses" treated as NAs

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
data.relavant$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...[data.relavant$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "4_Graduate supervisors in the life sciences"] <- NA

#Q5
data.relavant$Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur...[data.relavant$Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur... == "1_Yes" ] <- "Additional Bioinformatics Courses Needed"
data.relavant$Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur...[data.relavant$Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur... == "2_No" ] <- "No additional Bioinformatics Courses Needed"
data.relavant$Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur...[data.relavant$Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur... == "3_Don't know"] <- "Unsure if additional Bioinformatics Courses Needed"

#14
data.relavant$Q14_Sex[data.relavant$Q14_Sex == "1_Female"  ] <- "Female"
data.relavant$Q14_Sex[data.relavant$Q14_Sex == "2_Male"   ] <- "Male"
data.relavant$Q14_Sex[data.relavant$Q14_Sex == "3_Rather not say"   ] <- NA

#17
data.relavant$Q17_Highest.earned.degree..If..other...please.explain.[data.relavant$Q17_Highest.earned.degree..If..other...please.explain. == "1_B.S. (or equivalent)" ] <- "B.S. Degree"
data.relavant$Q17_Highest.earned.degree..If..other...please.explain.[data.relavant$Q17_Highest.earned.degree..If..other...please.explain. == "2_M.S. (or equivalent)"  ] <- "M.S. Degree"
data.relavant$Q17_Highest.earned.degree..If..other...please.explain.[data.relavant$Q17_Highest.earned.degree..If..other...please.explain. == "3_Professional degree (e.g., M.D.)" ] <- "Professional Degree"
data.relavant$Q17_Highest.earned.degree..If..other...please.explain.[data.relavant$Q17_Highest.earned.degree..If..other...please.explain. == "4_Ph.D. (or equivalent)"  ] <- "PhD"
data.relavant$Q17_Highest.earned.degree..If..other...please.explain.[data.relavant$Q17_Highest.earned.degree..If..other...please.explain. == "5_Other"] <- "Other Degree"

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
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "14_2003"] <- "2003"
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
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "34_1983"] <- "1983"
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
data.relavant$Q18_Year.of.highest.earned.degree.[data.relavant$Q18_Year.of.highest.earned.degree. == "68_Rather Not Say"] <- NA

#21
data.relavant$Q21_What.is.the.Carnegie.classification.of.your.institution.[data.relavant$Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College"] <- "Carnegie Classification: Associates"
data.relavant$Q21_What.is.the.Carnegie.classification.of.your.institution.[data.relavant$Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College"] <- "Carnegie Classification: Baccalaureate"
data.relavant$Q21_What.is.the.Carnegie.classification.of.your.institution.[data.relavant$Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)"] <- "Carnegie Classification: Masters"
data.relavant$Q21_What.is.the.Carnegie.classification.of.your.institution.[data.relavant$Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)"] <- "Carnegie Classification: Doctoral"
data.relavant$Q21_What.is.the.Carnegie.classification.of.your.institution.[data.relavant$Q21_What.is.the.Carnegie.classification.of.your.institution. == "5_Don't know"] <- NA

#22
data.relavant$Q22_Is.your.institution.classified.as.minority.serving.[data.relavant$Q22_Is.your.institution.classified.as.minority.serving. == "1_Yes" ] <- "Minority Serving Institution"
data.relavant$Q22_Is.your.institution.classified.as.minority.serving.[data.relavant$Q22_Is.your.institution.classified.as.minority.serving. == "2_No" ] <- "Non-minority Serving Institution"
data.relavant$Q22_Is.your.institution.classified.as.minority.serving.[data.relavant$Q22_Is.your.institution.classified.as.minority.serving. == "3_Don't know" ] <- NA

#23
data.relavant$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution.[data.relavant$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution. == "1_'< 5,000' student'" ] <- "Total Students: < 5,000"
data.relavant$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution.[data.relavant$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution. == "2_'5,000 - 15,000' students"  ] <- "Total Students: 5-15,000"
data.relavant$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution.[data.relavant$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution. == "3_'> 15,000' students"  ] <- "Total Students: > 15,000"
data.relavant$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution.[data.relavant$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution. == "4_Don't know"  ] <- NA

#24
data.relavant$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution.[data.relavant$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution. == "1_'< 5,000' students" ] <- "Total Undergraduates < 5,000"
data.relavant$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution.[data.relavant$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution. == "2_'5,000 - 15,000' students" ] <- "Total Undergraduates: 5-15,000"
data.relavant$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution.[data.relavant$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution. == "3_'> 15,000' students"  ] <- "Total Undergraduates > 15,000"
data.relavant$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution.[data.relavant$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution. == "4_Don't know" ] <- NA

#26
data.relavant$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...[data.relavant$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju... == "1_'< 10'" ] <- "Full-time faculty < 10"
data.relavant$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...[data.relavant$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju... == "2_'10 - 20'" ] <- "Full-time faculty 10-20"
data.relavant$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...[data.relavant$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju... == "3_'21 - 30'"] <- "Full-time faculty 21-30"
data.relavant$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...[data.relavant$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju... == "4_'31 - 40'" ] <- "Full-time faculty 31-40"
data.relavant$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...[data.relavant$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju... == "5_'41 - 50'" ] <- "Full-time faculty 41-50"
data.relavant$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...[data.relavant$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju... == "6_'> 50'"] <- "Full-time faculty > 50"
data.relavant$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...[data.relavant$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju... == "7_Don't know"] <- NA

#27
data.relavant$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..[data.relavant$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors.. == "1_'< 50'" ] <- "Department undergrads < 50"
data.relavant$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..[data.relavant$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors.. == "2_'51 - 100'"] <- "Department undergrads: 51-100"
data.relavant$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..[data.relavant$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors.. == "3_'101 - 500'"] <- "Department undergrads: 101-500"
data.relavant$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..[data.relavant$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors.. == "4_'501 - 2000'"] <- "Department undergrads: 500-2,000"
data.relavant$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..[data.relavant$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors.. == "5_'> 2000'" ] <- "Department undergrads > 2,000"
data.relavant$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..[data.relavant$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors.. == "6_Don't know"] <- NA

#29
data.relavant$Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g.....[data.relavant$Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g..... == "1_Yes"] <- "Technical barriers reported"
data.relavant$Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g.....[data.relavant$Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g..... == "2_No" ] <- "No technical barriers reported"

#57
data.relavant$Q57_Please.select.the.statement.belOw.that.best.describes.yOu.[data.relavant$Q57_Please.select.the.statement.belOw.that.best.describes.yOu. == "1_Teach at 4-year institution"] <- "4-year Institution"
data.relavant$Q57_Please.select.the.statement.belOw.that.best.describes.yOu.[data.relavant$Q57_Please.select.the.statement.belOw.that.best.describes.yOu. == "2_Teach at 2-year institution"] <- "2-year Institution"
data.relavant$Q57_Please.select.the.statement.belOw.that.best.describes.yOu.[data.relavant$Q57_Please.select.the.statement.belOw.that.best.describes.yOu. == "3_Teach at 'Other'"] <- "OTHER Institution type"




#### MCA calculation and plotting function

#Define Question Column Names

q6.cols <- c("q06_Faculty_issues_reduced",
        "q06_Curriculum_issues_reduced",
        "q06_Student_issues_reduced",
        "q06_Institutional_issues_reduced",
        "q06_Resource_issues_reduced",
        "q06_Facilities_issues_reduced")
q33.cols <- c("q33_Curriculum_issues_reduced",
         "q33_Faculty_issues_reduced",
         "q33_Facility_issues_reduced",
         "q33_Resources_issues_reduced",
         "q33_Student_issues_reduced",
         "q33_Institutional_issues_reduced")
q29.cols <- c("q29_Faculty_issues_reduced", 
         "q29_Facilities_issues_reduced", 
         "q29_Resources_issues_reduced",
         "q29_Institutional_issues_reduced", 
         "q29_Student_issues_reduced", 
         "q29_Curriculum_issues_reduced")
q38.cols <- c("q38_Faculty_issues_reduced", 
         "q38_Curriculum_issues_reduced", 
         "q38_Resources_issues_reduced", 
         "q38_Student_issues_reduced", 
         "q38_Facilities_issues_reduced", 
         "q38_Institutional_issues_reduced", 
         "q38_State_issues_reduced", 
         "q38_Accredited_issues_reduced")
Q1 <- "Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn..."
Q3 <- "faculty_preperation" 
Q5 <- "Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur..."
Q14 <- "Q14_Sex"
Q17 <- "Q17_Highest.earned.degree..If..other...please.explain."
Q18 <- "Q18_Year.of.highest.earned.degree."
Q21 <- "Q21_What.is.the.Carnegie.classification.of.your.institution."
Q22 <- "Q22_Is.your.institution.classified.as.minority.serving."
Q23 <- "Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution."
Q24 <- "Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution."
Q26 <- "Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju..."
Q27 <- "Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors.."
Q29 <- "Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g....."
Q57 <- "Q57_Please.select.the.statement.belOw.that.best.describes.yOu."
state <- "State_State"
region <- "Region_Region"
tracked_ethnicities <- "tracked_ethnicities"
representation <- "representation"
degree_year <- "bin_degree_yrs"



calculate_mca_and_plot <-  function(df, 
                                    #data frame for analysis
                                    qualitative_supplimentary_columns, 
                                    #qualitative_supplimentary_columns - these are variables that will be 'predicted' based on the other rows - a vector in the form c(x,y,...) 
                                    active_columns, 
                                    #active columns - these are the variables to include in the MCA analsis - a vector in the form c(x,y,...)
                                    active_filters,
                                    #active_filter - these are the variables that you wish to filter to remove NA values, analagous to active_columns a vector in the form c(x,y,...)
                                    habillage_col, 
                                    #column containing the variable you want to draw confidence elipses around, 
                                    habillage_nice_name, 
                                    #nice name for habbiliage column/variable, 
                                    plot_title 
                                    #title of plot
){
  


    n.supp.cols <- length(qualitative_supplimentary_columns)
    
    df.selected <- df%>%
      select_(.dots = c(qualitative_supplimentary_columns, active_columns))
    
    df.na.filtered <- df.selected%>%
      drop_na()
    
    return(df.na.filtered)
    
  }
 

# Draw selected correspondance plots

dir.create("./mca_plots", recursive = TRUE)


#Question Q1 - Teaching of bioinformatics status
tmp <- calculate_mca_and_plot(df = data.relavant, 
                              qualitative_supplimentary_columns = c(Q1), 
                              active_columns = c(
                                                 Q3, #level of bioinformatics training 
                                                 Q14, #Sex 
                                                 Q21, #Carnegie classification 
                                                 Q22, #MSI status                               
                                                 Q24 #Undergraduate enrollment
                              ))

MCA.object <- MCA(X = as.matrix(tmp),
                  quali.sup = 1,
                  graph = FALSE)

n_scored <- nrow(tmp)

fviz_mca_biplot(MCA.object,
                invisible = "quali.sup",
                col.var = "darkblue" , 
                habillage = 1,
                label = "var",
                pointsize = 2,
                alpha.ind = 0.4,
                addEllipses = TRUE,
                repel = TRUE,
                labelsize = 4,
                legend.title = "Respondent's bioinformatics\nteaching status (ellipse = 80%)",
                ellipse.level = 0.80)+
  theme_minimal()+
  ggtitle(paste("Multiple Correspondence Analysis; Q1: Teaching Bioinformatics, with selected factors n=", dim(tmp[1]), sep = ""), 
          subtitle = "Level of bioinformatics training\nSex\nCarnegie classification\nMSI status\nUndergraduate enrollment")

ggsave(filename = "./mca_plots/Q1_teaching_status.png", 
       width = 13.8, 
       height = 8.81, 
       units = "in")

#Question Q3 - Faculty preperation
tmp <- calculate_mca_and_plot(df = data.relavant, 
                              qualitative_supplimentary_columns = c(Q3), 
                              active_columns = c(
                              					         Q1, #bioinformatics teaching status                              					
                                                 Q14, #Sex 
                                                 Q21, #Carnegie classification 
                                                 Q22, #MSI status                               
                                                 Q24 #Undergraduate enrollment
                              ))

MCA.object <- MCA(X = as.matrix(tmp),
                  quali.sup = 1,
                  graph = FALSE)

n_scored <- nrow(tmp)

fviz_mca_biplot(MCA.object,
                invisible = "quali.sup",
                col.var = "darkblue" , 
                habillage = 1,
                label = "var",
                pointsize = 2,
                alpha.ind = 0.4,
                addEllipses = TRUE,
                repel = TRUE,
                labelsize = 4,
                legend.title = "Respondent's bioinformatics\ntraining (ellipse = 80%)",
                ellipse.level = 0.80)+
  theme_minimal()+
  ggtitle(paste("Multiple Correspondence Analysis; Q3: Bioinformatics Training, with selected factors n=", dim(tmp[1]), sep = ""), 
          subtitle = "Current bioinformatics teaching (Teaching)\nSex\nCarnegie classification\nMSI status\nUndergraduate enrollment")

ggsave(filename = "./mca_plots/Q3_bioinformatics_training.png", 
       width = 13.8, 
       height = 8.81, 
       units = "in")


#Question Q5 - Opinion on the need for further courses
tmp <- calculate_mca_and_plot(df = data.relavant, 
                              qualitative_supplimentary_columns = c(Q5), 
                              active_columns = c(
                                                 Q1, #bioinformatics teaching status 
                                				         Q3, #bioinformatics training                             					
                                                 Q14, #Sex 
                                                 Q21, #Carnegie classification 
                                                 Q22, #MSI status                               
                                                 Q24 #Undergraduate enrollment
                              ))

MCA.object <- MCA(X = as.matrix(tmp),
                  quali.sup = 1,
                  graph = FALSE)

n_scored <- nrow(tmp)

fviz_mca_biplot(MCA.object,
                invisible = "quali.sup",
                col.var = "darkblue" , 
                habillage = 1,
                label = "var",
                pointsize = 2,
                alpha.ind = 0.4,
                addEllipses = TRUE,
                repel = TRUE,
                labelsize = 4,
                legend.title = "Respondent's opinion on the need\nfor additional bioinformatics courses (ellipse = 80%)",
                ellipse.level = 0.80)+
  theme_minimal()+
  ggtitle(paste("Multiple Correspondence Analysis; Q5: Need for additional courses, with selected factors n=", dim(tmp[1]), sep = ""), 
          subtitle = "Level of bioinformatics training\nCurrent bioinformatics teaching (Teaching)\nSex\nCarnegie classification\nMSI status\nUndergraduate enrollment")

ggsave(filename = "./mca_plots/Q5_more_courses.png", 
       width = 13.8, 
       height = 8.81, 
       units = "in")


#Question Q14- Sex
tmp <- calculate_mca_and_plot(df = data.relavant, 
                              qualitative_supplimentary_columns = c(Q14), 
                              active_columns = c(
                                                 Q1, #bioinformatics teaching status
                                				         Q3, #bioinformatics training                             					                                      
                                                 Q21, #Carnegie classification
                                                 Q22, #MSI status                               
                                                 Q24 #Undergraduate enrollment
                              ))

MCA.object <- MCA(X = as.matrix(tmp),
                  quali.sup = 1,
                  graph = FALSE)

n_scored <- nrow(tmp)

fviz_mca_biplot(MCA.object,
                invisible = "quali.sup",
                col.var = "darkblue" , 
                habillage = 1,
                label = "var",
                pointsize = 2,
                alpha.ind = 0.4,
                addEllipses = TRUE,
                repel = TRUE,
                labelsize = 4,
                legend.title = "Respondent's sex (ellipse = 80%)",
                ellipse.level = 0.80)+
  theme_minimal()+
  ggtitle(paste("Multiple Correspondence Analysis; Q14: Sex, with selected factors n=", dim(tmp[1]), sep = ""), 
          subtitle = "Level of bioinformatics training\nCurrent bioinformatics teaching (Teaching)\nCarnegie classification\nMSI status\nUndergraduate enrollment")

ggsave(filename = "./mca_plots/Q14_sex.png", 
       width = 13.8, 
       height = 8.81, 
       units = "in")


#Question Q21- Carnegie Classiication
tmp <- calculate_mca_and_plot(df = data.relavant, 
                              qualitative_supplimentary_columns = c(Q21), 
                              active_columns = c(
                                                 Q1, #bioinformatics teaching status 
                                				         Q3, #bioinformatics training                             					
                                                 Q14, #Sex 
                                                 Q22, #MSI status                               
                                                 Q24 #Undergraduate enrollment
                              ))

MCA.object <- MCA(X = as.matrix(tmp),
                  quali.sup = 1,
                  graph = FALSE)

n_scored <- nrow(tmp)

fviz_mca_biplot(MCA.object,
                invisible = "quali.sup",
                col.var = "darkblue" , 
                habillage = 1,
                label = "var",
                pointsize = 2,
                alpha.ind = 0.4,
                addEllipses = TRUE,
                repel = TRUE,
                labelsize = 4,
                legend.title = "Respondent's Institutional Carnegie classification (ellipse = 80%)",
                ellipse.level = 0.80)+
  theme_minimal()+
  ggtitle(paste("Multiple Correspondence Analysis; Q14: Carnegie classification, with selected factors n=", dim(tmp[1]), sep = ""), 
          subtitle = "Level of bioinformatics training\nCurrent bioinformatics teaching (Teaching)\nSex\nMSI status\nUndergraduate enrollment")

ggsave(filename = "./mca_plots/Q21_carnegie_classification.png", 
       width = 13.8, 
       height = 8.81, 
       units = "in")


#Question Q22- MSI status
tmp <- calculate_mca_and_plot(df = data.relavant, 
                              qualitative_supplimentary_columns = c(Q22), 
                              active_columns = c(
                                                 Q1, #bioinformatics teaching status
                                				         Q3, #bioinformatics training                             					
                                                 Q14, #Sex 
                                                 Q21, #Carnegie classification                             
                                                 Q24 #Undergraduate enrollment
                              ))

MCA.object <- MCA(X = as.matrix(tmp),
                  quali.sup = 1,
                  graph = FALSE)

n_scored <- nrow(tmp)

fviz_mca_biplot(MCA.object,
                invisible = "quali.sup",
                col.var = "darkblue" , 
                habillage = 1,
                label = "var",
                pointsize = 2,
                alpha.ind = 0.4,
                addEllipses = TRUE,
                repel = TRUE,
                labelsize = 4,
                legend.title = "Respondent's Institutional MSI status (ellipse = 80%)",
                ellipse.level = 0.80)+
  theme_minimal()+
  ggtitle(paste("Multiple Correspondence Analysis; Q22 MSI status, with selected factors n=", dim(tmp[1]), sep = ""), 
          subtitle = "Level of bioinformatics training\nCurrent bioinformatics teaching (Teaching)\nSex\nCarnegie classification\nUndergraduate enrollment")

ggsave(filename = "./mca_plots/Q22_msi_status.png", 
       width = 13.8, 
       height = 8.81, 
       units = "in")


#Question Q24- Ugrad enrollment
tmp <- calculate_mca_and_plot(df = data.relavant, 
                              qualitative_supplimentary_columns = c(Q24), 
                              active_columns = c(
                                                  Q1, #bioinformatics teaching status 
                                				          Q3, #bioinformatics training                             					
                                                 Q14, #Sex 
                                                 Q21, #Carnegie classification
                                                 Q22 #MSI status
                              ))

MCA.object <- MCA(X = as.matrix(tmp),
                  quali.sup = 1,
                  graph = FALSE)

n_scored <- nrow(tmp)

fviz_mca_biplot(MCA.object,
                invisible = "quali.sup",
                col.var = "darkblue" , 
                habillage = 1,
                label = "var",
                pointsize = 2,
                alpha.ind = 0.4,
                addEllipses = TRUE,
                repel = TRUE,
                labelsize = 4,
                legend.title = "Respondent's Institutional undergraduate enrollment (ellipse = 80%)",
                ellipse.level = 0.80)+
  theme_minimal()+
  ggtitle(paste("Multiple Correspondence Analysis; Q24 Undergraduate enrollment, with selected factors n=", dim(tmp[1]), sep = ""), 
          subtitle = "Level of bioinformatics training\nCurrent bioinformatics teaching (Teaching)\nSex\nCarnegie classification\nMSI status")

ggsave(filename = "./mca_plots/Q24_ugrad_enrollment.png", 
       width = 13.8, 
       height = 8.81, 
       units = "in")


#Question Q17- highest degree
tmp <- calculate_mca_and_plot(df = data.relavant, 
                              qualitative_supplimentary_columns = c(Q17), 
                              active_columns = c(
                                                  Q1, #bioinformatics teaching status
                                				          Q3, #bioinformatics training                             					
                                                 Q14, #Sex 
                                                 Q21, #Carnegie classification
                                                 Q22, #MSI status                               
                                                 Q24  #Undergraduate enrollment
                              ))

MCA.object <- MCA(X = as.matrix(tmp),
                  quali.sup = 1,
                  graph = FALSE)

n_scored <- nrow(tmp)

fviz_mca_biplot(MCA.object,
                invisible = "quali.sup",
                col.var = "darkblue" , 
                habillage = 1,
                label = "var",
                pointsize = 2,
                alpha.ind = 0.4,
                addEllipses = TRUE,
                repel = TRUE,
                labelsize = 4,
                legend.title = "Respondent's highest degree (ellipse = 80%)",
                ellipse.level = 0.80)+
  theme_minimal()+
  ggtitle(paste("Multiple Correspondence Analysis; Q17 Highest degree earned, with selected factors n=", dim(tmp[1]), sep = ""), 
          subtitle = "Level of bioinformatics training\nCurrent bioinformatics teaching (Teaching)\nSex\nCarnegie classification\nMSI status\nUndergraduate enrollment")

ggsave(filename = "./mca_plots/Q17_degree.png", 
       width = 13.8, 
       height = 8.81, 
       units = "in")


#Question Q15 - Ethnicity/stem representation
tmp <- calculate_mca_and_plot(df = data.relavant, 
                              qualitative_supplimentary_columns = c(representation), 
                              active_columns = c(
                                Q1, #bioinformatics teaching status
                                Q3, #bioinformatics training                             					
                                Q14, #Sex 
                                Q21, #Carnegie classification
                                Q22, #MSI status                               
                                Q24  #Undergraduate enrollment
                              ))

MCA.object <- MCA(X = as.matrix(tmp),
                  quali.sup = 1,
                  graph = FALSE)

n_scored <- nrow(tmp)

fviz_mca_biplot(MCA.object,
                invisible = "quali.sup",
                col.var = "darkblue" , 
                habillage = 1,
                label = "var",
                pointsize = 2,
                alpha.ind = 0.4,
                addEllipses = TRUE,
                repel = TRUE,
                labelsize = 4,
                legend.title = "Respondent's status in STEM (ellipse = 80%)",
                ellipse.level = 0.80)+
  theme_minimal()+
  ggtitle(paste("Multiple Correspondence Analysis; Q15 STEM representation, with selected factors n=", dim(tmp[1]), sep = ""), 
          subtitle = "Level of bioinformatics training\nCurrent bioinformatics teaching (Teaching)\nSex\nCarnegie classification\nMSI status\nUndergraduate enrollment")

ggsave(filename = "./mca_plots/Q15_STEM.png", 
       width = 13.8, 
       height = 8.81, 
       units = "in")


#Question Q18 - adjusted degree years
tmp <- calculate_mca_and_plot(df = data.relavant, 
                              qualitative_supplimentary_columns = c(degree_year), 
                              active_columns = c(
                                Q1, #bioinformatics teaching status
                                Q3, #bioinformatics training                             					
                                Q14, #Sex 
                                Q21, #Carnegie classification
                                Q22, #MSI status                               
                                Q24  #Undergraduate enrollment
                              ))

MCA.object <- MCA(X = as.matrix(tmp),
                  quali.sup = 1,
                  graph = FALSE)

n_scored <- nrow(tmp)

fviz_mca_biplot(MCA.object,
                invisible = "quali.sup",
                col.var = "darkblue" , 
                habillage = 1,
                label = "var",
                pointsize = 2,
                alpha.ind = 0.4,
                addEllipses = TRUE,
                repel = TRUE,
                labelsize = 4,
                legend.title = "Respondent's year of degree (ellipse = 80%)",
                ellipse.level = 0.80)+
  theme_minimal()+
  ggtitle(paste("Multiple Correspondence Analysis; Q18 Year of Degree, with selected factors n=", dim(tmp[1]), sep = ""), 
          subtitle = "Level of bioinformatics training\nCurrent bioinformatics teaching (Teaching)\nSex\nCarnegie classification\nMSI status\nUndergraduate enrollment")

ggsave(filename = "./mca_plots/Q18_degree.png", 
       width = 13.8, 
       height = 8.81, 
       units = "in")

