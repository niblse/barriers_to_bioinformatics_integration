#some attempts at plots
require(ggplot2)
require(tidyverse)

#load dataframe and create directories
dir.create("./output_plots", recursive = TRUE)
dir.create("./output_tables", recursive = TRUE)
df <- read_csv("../../data_cleaning_scripts/04_decode_survey_responses/output/decoded_df.csv")
df.ethnicity <- read_csv("../../data_cleaning_scripts/05_adjust_ethnicities/output/decoded_df_w_ethnicity.csv")
df.training <- read_csv("../../data_cleaning_scripts/06_adjust_bioinformatics_training/output/decoded_df_w_faculty_preperation.csv")
df.degree <- read_csv("../../data_cleaning_scripts/07_adjust_degree_year/output/decoded_df_w_bin_degree_years.csv")


# remove any non-US respondents
countries <- c("United States","Puerto Rico")
df <- df%>%
  filter(Country_Country %in% countries )
df.ethnicity <- df.ethnicity%>%
  filter(Country_Country %in% countries )
df.training <- df.training%>%
  filter(Country_Country %in% countries )
df.degree <- df.degree%>%
  filter(Country_Country %in% countries )
survey.n <- nrow(df)

#Q1
df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...[df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "1_Dedicated course for life-science majors"] <- "Teaching: Dedicated Bioinformatics Course"
df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...[df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "2_Include 'substantial' bioinformatics in courses for life-science majors"  ] <- "Teaching: Integrating Bioinformatics"
df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...[df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "3_Do NOT currently, but would like to include 'substantial' bioinformatics in courses for life-science majors"] <- "Teaching: Not Integrating Bioinformatics"
df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...[df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "4_Graduate supervisors in the life sciences"] <- NA

#Q5
df$Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur...[df$Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur... == "1_Yes" ] <- "Additional Bioinformatics Courses Needed"
df$Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur...[df$Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur... == "2_No" ] <- "No additional Bioinformatics Courses Needed"
df$Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur...[df$Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur... == "3_Don't know"] <- "Unsure if additional Bioinformatics Courses Needed"

#14
df$Q14_Sex[df$Q14_Sex == "1_Female"  ] <- "Female"
df$Q14_Sex[df$Q14_Sex == "2_Male"   ] <- "Male"
df$Q14_Sex[df$Q14_Sex == "3_Rather not say"   ] <- NA

#17
df$Q17_Highest.earned.degree..If..other...please.explain.[df$Q17_Highest.earned.degree..If..other...please.explain. == "1_B.S. (or equivalent)" ] <- "B.S. Degree"
df$Q17_Highest.earned.degree..If..other...please.explain.[df$Q17_Highest.earned.degree..If..other...please.explain. == "2_M.S. (or equivalent)"  ] <- "M.S. Degree"
df$Q17_Highest.earned.degree..If..other...please.explain.[df$Q17_Highest.earned.degree..If..other...please.explain. == "3_Professional degree (e.g., M.D.)" ] <- "Professional Degree"
df$Q17_Highest.earned.degree..If..other...please.explain.[df$Q17_Highest.earned.degree..If..other...please.explain. == "4_Ph.D. (or equivalent)"  ] <- "PhD"
df$Q17_Highest.earned.degree..If..other...please.explain.[df$Q17_Highest.earned.degree..If..other...please.explain. == "5_Other"] <- "Other Degree"

##18
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "1_2016"] <- "2016"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "2_2015"] <- "2015"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "3_2014"] <- "2014"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "4_2013"] <- "2013"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "5_2012"] <- "2012"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "6_2011"] <- "2011"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "7_2010"] <- "2010"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "8_2009"] <- "2009"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "9_2008"] <- "2008"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "10_2007"] <- "2007"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "11_2006"] <- "2006"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "12_2005"] <- "2005"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "13_2004"] <- "2004"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "14_2003"] <- "2003"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "15_2002"] <- "2002"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "16_2001"] <- "2001"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "17_2000"] <- "2000"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "18_1999"] <- "1999"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "19_1998"] <- "1998"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "20_1997"] <- "1997"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "21_1996"] <- "1996"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "22_1995"] <- "1995"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "23_1994"] <- "1994"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "24_1993"] <- "1993"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "25_1992"] <- "1992"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "26_1991"] <- "1991"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "27_1990"] <- "1990"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "28_1989"] <- "1989"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "29_1988"] <- "1989"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "30_1987"] <- "1987"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "31_1986"] <- "1986"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "32_1985"] <- "1985"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "33_1984"] <- "1984"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "34_1983"] <- "1983"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "35_1982"] <- "1982"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "36_1981"] <- "1981"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "37_1980"] <- "1980"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "38_1979"] <- "1979"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "39_1978"] <- "1978"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "40_1977"] <- "1977"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "41_1976"] <- "1976"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "42_1975"] <- "1975"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "43_1974"] <- "1974"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "44_1973"] <- "1973"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "45_1972"] <- "1972"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "46_1971"] <- "1971"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "47_1970"] <- "1970"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "48_1969"] <- "1969"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "49_1968"] <- "1968"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "50_1967"] <- "1967"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "51_1966"] <- "1966"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "52_1965"] <- "1965"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "53_1964"] <- "1964"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "54_1963"] <- "1963"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "55_1962"] <- "1962"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "56_1961"] <- "1961"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "57_1960"] <- "1960"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "58_1959"] <- "1959"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "59_1958"] <- "1958"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "60_1957"] <- "1957"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "61_1956"] <- "1956"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "62_1955"] <- "1955"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "63_1954"] <- "1954"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "64_1953"] <- "1953"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "65_1952"] <- "1952"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "66_1951"] <- "1951"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "67_1950"] <- "1950"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "68_Rather Not Say"] <- NA

#21
df$Q21_What.is.the.Carnegie.classification.of.your.institution.[df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College"] <- "Carnegie Classification: Associates"
df$Q21_What.is.the.Carnegie.classification.of.your.institution.[df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College"] <- "Carnegie Classification: Baccalaureate"
df$Q21_What.is.the.Carnegie.classification.of.your.institution.[df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)"] <- "Carnegie Classification: Masters"
df$Q21_What.is.the.Carnegie.classification.of.your.institution.[df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)"] <- "Carnegie Classification: Doctoral"
df$Q21_What.is.the.Carnegie.classification.of.your.institution.[df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "5_Don't know"] <- NA

#22
df$Q22_Is.your.institution.classified.as.minority.serving.[df$Q22_Is.your.institution.classified.as.minority.serving. == "1_Yes" ] <- "Minority Serving Institution"
df$Q22_Is.your.institution.classified.as.minority.serving.[df$Q22_Is.your.institution.classified.as.minority.serving. == "2_No" ] <- "Non-minority Serving Institution"
df$Q22_Is.your.institution.classified.as.minority.serving.[df$Q22_Is.your.institution.classified.as.minority.serving. == "3_Don't know" ] <- NA

#23
df$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution.[df$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution. == "1_'< 5,000' student'" ] <- "Total Students: < 5,000"
df$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution.[df$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution. == "2_'5,000 - 15,000' students"  ] <- "Total Students: 5-15,000"
df$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution.[df$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution. == "3_'> 15,000' students"  ] <- "Total Students: > 15,000"
df$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution.[df$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution. == "4_Don't know"  ] <- NA

#24
df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution.[df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution. == "1_'< 5,000' students" ] <- "Total Undergraduates < 5,000"
df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution.[df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution. == "2_'5,000 - 15,000' students" ] <- "Total Undergraduates: 5-15,000"
df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution.[df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution. == "3_'> 15,000' students"  ] <- "Total Undergraduates > 15,000"
df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution.[df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution. == "4_Don't know" ] <- NA

#26
df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...[df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju... == "1_'< 10'" ] <- "Full-time faculty < 10"
df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...[df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju... == "2_'10 - 20'" ] <- "Full-time faculty 10-20"
df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...[df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju... == "3_'21 - 30'"] <- "Full-time faculty 21-30"
df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...[df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju... == "4_'31 - 40'" ] <- "Full-time faculty 31-40"
df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...[df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju... == "5_'41 - 50'" ] <- "Full-time faculty 41-50"
df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...[df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju... == "6_'> 50'"] <- "Full-time faculty > 50"
df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...[df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju... == "7_Don't know"] <- NA

#27
df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..[df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors.. == "1_'< 50'" ] <- "Department undergrads < 50"
df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..[df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors.. == "2_'51 - 100'"] <- "Department undergrads: 51-100"
df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..[df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors.. == "3_'101 - 500'"] <- "Department undergrads: 101-500"
df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..[df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors.. == "4_'501 - 2000'"] <- "Department undergrads: 500-2,000"
df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..[df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors.. == "5_'> 2000'" ] <- "Department undergrads > 2,000"
df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..[df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors.. == "6_Don't know"] <- NA

#29
df$Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g.....[df$Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g..... == "1_Yes"] <- "Technical barriers reported"
df$Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g.....[df$Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g..... == "2_No" ] <- "No technical barriers reported"

#57
df$Q57_Please.select.the.statement.belOw.that.best.describes.yOu.[df$Q57_Please.select.the.statement.belOw.that.best.describes.yOu. == "1_Teach at 4-year institution"] <- "4-year Institution"
df$Q57_Please.select.the.statement.belOw.that.best.describes.yOu.[df$Q57_Please.select.the.statement.belOw.that.best.describes.yOu. == "2_Teach at 2-year institution"] <- "2-year Institution"
df$Q57_Please.select.the.statement.belOw.that.best.describes.yOu.[df$Q57_Please.select.the.statement.belOw.that.best.describes.yOu. == "3_Teach at 'Other'"] <- "OTHER Institution type"



#generate single variable summary plots

#question 18 - adjusted degree year

df %>%
  ggplot()+
  aes(x= reorder(df.degree$bin_degree_yrs,
                 df.degree$bin_degree_yrs, 
                 function(x)-length(x)))+
  geom_bar()+
  ggtitle("Q18 - adjusted Year of Degree Survey Respondents")+
  xlab("Degree years")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")

ggsave("./output_plots/q18_survey_responses_by_degree_year_bin.png")

#write as a table
Q18.degree.bin.table <- as.data.frame(table(as.factor(df.degree$bin_degree_yrs), useNA = "always"),stringsAsFactors = TRUE)
total.responses=sum(Q18.degree.bin.table$Freq)
Q18.degree.bin.table <- Q18.degree.bin.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))%>%
  mutate(question = "Q18 bin degree")
write.csv(Q18.degree.bin.table, file = "./output_tables/Q18.bin.degree.table.csv")

#generate single variable summary plots

#question 14 - gender

df %>%
  ggplot()+
  aes(x= reorder(df$Q14_Sex,
                 df$Q14_Sex, 
                 function(x)-length(x)))+
  geom_bar()+
  #scale_x_discrete(labels=c("Female", "Male", "Not Provided"))+
  ggtitle("Q14 - Survey Responses by Sex")+
  xlab("sex")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")

ggsave("./output_plots/q14_survey_responses_by_sex.png")

#write as a table
Q14.gender.table <- as.data.frame(table(as.factor(df$Q14_Sex), useNA = "always"),stringsAsFactors = FALSE)
total.responses=sum(Q14.gender.table$Freq)
Q14.gender.table <- Q14.gender.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))%>%
  mutate(question = "Q14 sex")
write.csv(Q14.gender.table, file = "./output_tables/Q14.sex.table.csv")



#question 57 - Institution
df %>%
  ggplot()+
  aes(x= reorder(df$Q57_Please.select.the.statement.belOw.that.best.describes.yOu.,
                 df$Q57_Please.select.the.statement.belOw.that.best.describes.yOu.,
                 function(x)-length(x)))+
  geom_bar()+
  #scale_x_discrete(labels=c("Teaching at 4-year", "Teaching at 2-year", "Other"))+
  ggtitle("Q57 - Survey Responses by Institution Type")+
  xlab("institution type")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")
ggsave("./output_plots/q57_survey_responses_by_institution_type.png")

#write as a table
Q57.institution.table <- as.data.frame(table(as.factor(df$Q57_Please.select.the.statement.belOw.that.best.describes.yOu.), useNA = "always"),stringsAsFactors = FALSE)
total.responses=sum(Q57.institution.table$Freq)
Q57.institution.table <- Q57.institution.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))
write.csv(Q57.institution.table, file = "./output_tables/Q57.institution.table.csv")


#question 1 - Current teaching
df %>%
  ggplot()+
  aes(x= reorder(df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn..., 
                 df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn..., 
                 function(x)-length(x)))+
  geom_bar()+
  #scale_x_discrete(labels=c("life-science majors: teaching bioinformatics course", 
  #                          "life-science majors: including bioinformatics",
  #                          "life-science majors: not currently including bioinformatics", 
  #                          "life science graduate advisor"))+
  ggtitle("Q1 - Survey Responses by Teaching Category")+
  xlab("teaching category")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("./output_plots/q1_survey_responses_by_teaching_category.png")

#write as a table
Q1.currently.teaching.table <- as.data.frame(table(as.factor(df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...), useNA = "always"),stringsAsFactors = FALSE)
total.responses=sum(Q1.currently.teaching.table$Freq)
Q1.currently.teaching.table <- Q1.currently.teaching.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))%>%
  mutate(question = "Q1 Bioinformatics teaching")
write.csv(Q1.currently.teaching.table, file = "./output_tables/Q1.currently.teaching.table.csv")

#question State 
state_df <- df%>%
  filter(Country_Country == "United States")

state_df %>%
  ggplot()+
  aes(x= reorder(state_df$State_State, 
                 state_df$State_State, 
                 function(x)-length(x)))+
  geom_bar()+
  ggtitle("QState - Survey Responses by State")+
  xlab("state (including Washington DC; excluding Puerto Rico)")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("./output_plots/qstate_survey_responses_by_state.png")


#write as a table
QState.table <- as.data.frame(table(as.factor(df$State_State), useNA = "always"),stringsAsFactors = FALSE)
total.responses=sum(QState.table$Freq)
QState.table <- QState.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))
write.csv(QState.table, file = "./output_tables/QState.table.csv")



#question 21, carnegie classification 

  ggplot()+
  aes(x= reorder(df$Q21_What.is.the.Carnegie.classification.of.your.institution.,
                 df$Q21_What.is.the.Carnegie.classification.of.your.institution.,
                 function(x)-length(x)))+
  geom_bar()+
  #scale_x_discrete(labels=c("4; Doctoral", "2; Baccalaureate","3; Master's", "1; Associate's", "Don't know"))+
  ggtitle("Q21 - Survey Responses by Carnegie Classification")+
  xlab("carnegie classification")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")
ggsave("./output_plots/q21_survey_responses_by_carnegie_classification.png")


#write as a table
Q21.carnegie.classification.table <- as.data.frame(table(as.factor(df$Q21_What.is.the.Carnegie.classification.of.your.institution.), useNA = "always"),stringsAsFactors = FALSE)
total.responses=sum(Q21.carnegie.classification.table$Freq)
Q21.carnegie.classification.table <- Q21.carnegie.classification.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))%>%
  mutate(question = "Q21 Carnegie classification")
write.csv(Q21.carnegie.classification.table, file = "./output_tables/Q21.carnegie.classification.table.csv")



#question 18, year of degree

df %>%
  ggplot()+
  aes(x= reorder(df$Q18_Year.of.highest.earned.degree.,
                 df$Q18_Year.of.highest.earned.degree.,
                 function(x)-length(x)))+
  geom_bar()+
  ggtitle("Q18 - Survey Responses by Degree Year")+
  xlab("year of degree")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("./output_plots/q18_survey_responses_by_year_of_degree.png")


#write as a table
Q18.degree.year.table <- as.data.frame(table(as.factor(df$Q18_Year.of.highest.earned.degree.), useNA = "always"),stringsAsFactors = FALSE)
total.responses=sum(Q18.degree.year.table$Freq)
Q18.degree.year.table <- Q18.degree.year.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))
write.csv(Q18.degree.year.table, file = "./output_tables/Q18.degree.year.table.csv")


#question 3, level of training

df %>%
  ggplot()+
  aes(x= reorder(df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.,
                 df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.,
                 function(x)-length(x)))+
  geom_bar()+
  #scale_x_discrete(labels=c("No formal training",
  #                          "Short workshop/bootcamp",
  #                          "Post-graduate certificate",
  #                          "No training/experience",
  #                          "Graduate course/degree",
  #                          "Some undergraduate courses",
  #                          "Undergraduate degree",
  #                          "Undergraduate certificate"
  #                          ))+
  ggtitle("Q3 - Survey Responses by Level of Bioinformatics Training")+
  xlab("training level")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("./output_plots/q3_survey_responses_by_level_of_training.png")


#write as a table
Q3.level.of.training.table <- as.data.frame(table(as.factor(df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.), useNA = "always"),stringsAsFactors = FALSE)
total.responses=sum(Q3.level.of.training.table$Freq)
Q3.level.of.training.table <- Q3.level.of.training.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))%>%
  mutate(question = "Q3 Level of Training")
write.csv(Q3.level.of.training.table, file = "./output_tables/Q3.level.of.training.table.csv")


#question 22, MSI

df %>%
  ggplot()+
  aes(x= reorder(df$Q22_Is.your.institution.classified.as.minority.serving.,
                 df$Q22_Is.your.institution.classified.as.minority.serving.,
                 function(x)-length(x)))+
  geom_bar()+
  #scale_x_discrete(labels=c("No",
  #                          "Yes",
  #                          "Don't know"))+
  ggtitle("Q22 - Minority Serving Institution")+
  xlab("institution type")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")
ggsave("./output_plots/q22_survey_responses_by_MSI.png")


#write as a table
Q22.msi.table <- as.data.frame(table(as.factor(df$Q22_Is.your.institution.classified.as.minority.serving.), useNA = "always"),stringsAsFactors = FALSE)
total.responses=sum(Q22.msi.table$Freq)
Q22.msi.table <- Q22.msi.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))%>%
  mutate(question = "Q22 MSI Status")
write.csv(Q22.msi.table, file = "./output_tables/Q22.msi.table.csv")


#question 23, number of students (grad+undergrad)

df %>%
  ggplot()+
  aes(x= reorder(df$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution.,
                 df$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution.,
                 function(x)-length(x)))+
  geom_bar()+
  #scale_x_discrete(labels=c("5-15,000",
  #                          "> 15,000",
  #                          "< 5,000", 
  #                          "Don't know"))+
  ggtitle("Q23 - Number of undegraduate and graduate students")+
  xlab("student enrollment")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")

ggsave("./output_plots/q23_survey_responses_by_undegraduate_and_graduate_enrollment.png")

#write as a table
Q23.total.enrollment.table <- as.data.frame(table(as.factor(df$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution.), useNA = "always"),stringsAsFactors = FALSE)
total.responses=sum(Q23.total.enrollment.table$Freq)
Q23.total.enrollment.table <- Q23.total.enrollment.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))
write.csv(Q23.total.enrollment.table, file = "./output_tables/Q23.total.enrollment.table.csv")


#question 24, number of undergraduate students 

df %>%
  ggplot()+
  aes(x= reorder(df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution.,
                 df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution.,
                 function(x)-length(x)))+
  geom_bar()+
  #scale_x_discrete(labels=c("< 5,000",
  #                          "5-15,000",
  #                          "> 15,000", 
  #                          "Don't know"))+
  ggtitle("Q24 - Number of undegraduate students")+
  xlab("student enrollment")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")
ggsave("./output_plots/q24_survey_responses_by_undegraduate_enrollment.png")

#write as a table
Q24.undergraduate.enrollment.table <- as.data.frame(table(as.factor(df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution.), useNA = "always"),stringsAsFactors = FALSE)
total.responses=sum(Q24.undergraduate.enrollment.table$Freq)
Q24.undergraduate.enrollment.table <- Q24.undergraduate.enrollment.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))%>%
  mutate(question = "Q24 Undergrad Enrollment")
write.csv(Q24.undergraduate.enrollment.table, file = "./output_tables/Q24.undergraduate.enrollment.table.csv")


#question 26, number of full-time faculty 

df %>%
  ggplot()+
  aes(x= reorder(df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...,
                 df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...,
                 function(x)-length(x)))+
  geom_bar()+
  #scale_x_discrete(labels=c("10-20",
  #                          "< 10",
  #                          "21-30",
  #                          "31-40",
  #                          "> 50",
  #                          "Don't know", 
  #                          "41-50"))+
  ggtitle("Q26 - Number of full-time faculty")+
  xlab("faculty size")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")
ggsave("./output_plots/q26_survey_responses_by_faculty_size.png")


#write as a table
Q26.full.time.faculty.table <- as.data.frame(table(as.factor(df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...), useNA = "always"),stringsAsFactors = FALSE)
total.responses=sum(Q26.full.time.faculty.table$Freq)
Q26.full.time.faculty.table <- Q26.full.time.faculty.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))
write.csv(Q26.full.time.faculty.table, file = "./output_tables/Q26.full.time.faculty.table.csv")


#question 27, undergrads in department

df %>%
  ggplot()+
  aes(x= reorder(df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..,
                 df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..,
                 function(x)-length(x)))+
  geom_bar()+
  #scale_x_discrete(labels=c("101-500",
  #                          "501-2000",
  #                          "51-100",
  #                          "Don't know",
  #                          "< 50", 
  #                          "> 2000"))+
  ggtitle("Q27 - Number of undegraduates in department")+
  xlab("student enrollment")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")
ggsave("./output_plots/q27_survey_responses_by_undergrads_in_dept.png")

#write as a table
Q27.undergrads.in.dept.table <- as.data.frame(table(as.factor(df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..), useNA = "always"),stringsAsFactors = FALSE)
total.responses=sum(Q26.full.time.faculty.table$Freq)
Q27.undergrads.in.dept.table <- Q27.undergrads.in.dept.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))
write.csv(Q27.undergrads.in.dept.table, file = "./output_tables/Q27.undergrads.in.dept.table.csv")

#question Region, respondents by region

df %>%
  ggplot()+
  aes(x= reorder(df$Region_Region,
                 df$Region_Region,
                 function(x)-length(x)))+
  geom_bar()+
  #scale_x_discrete(labels=c("SO",
  #                          "MW",
  #                          "NE",
  #                          "WE"))+
  ggtitle("QRegion - Number of respondents by region")+
  xlab("region")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")
ggsave("./output_plots/qRegion_survey_responses_by_region.png")
  

#write as a table
Qregion.table <- as.data.frame(table(as.factor(df$Region_Region), useNA = "always"),stringsAsFactors = FALSE)
total.responses=sum(Qregion.table$Freq)
Qregion.table <- Qregion.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))
write.csv(Qregion.table, file = "./output_tables/Qregion.table.csv")

#question Ethnicity/Race

df %>%
  ggplot()+
  aes(x= reorder(df$Q15_Race,
                 df$Q15_Race,
                 function(x)-length(x)))+
  geom_bar()+
  #scale_x_discrete(labels=c("White",
  #                          "Rather Not Say",
  #                          "Asian",
  #                          "Black or African American",
  #                          "Native Hawaiian or other Pacific Islander", 
  #                          "American Indian or Alaska Native",
  #                          "NA"))+
  ggtitle("Q15 Race")+
  xlab("ethnicity")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")
ggsave("./output_plots/q15_Race.png")


#write as a table
Q15.race.table <- as.data.frame(table(as.factor(df$Q15_Race), useNA = "always"),stringsAsFactors = FALSE)
total.responses=sum(Q15.race.table$Freq)
Q15.race.table <- Q15.race.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))%>%
  mutate(question = "Q15 Race")
write.csv(Q15.race.table, file = "./output_tables/Q15.race.table.csv")

#question tracked ethnicities

df.ethnicity %>%
  ggplot()+
  aes(x= reorder(df.ethnicity$tracked_ethnicities,
                 df.ethnicity$tracked_ethnicities,
                 function(x)-length(x)))+
  geom_bar()+
  #scale_x_discrete(labels=c("White",
  #                          "Asian",
  #                          "Hispanic",
  #                          "Black or African American",
  #                          "Native Hawaiian or other Pacific Islander", 
  #                          "American Indian or Alaska Native",
  #                          "NA"))+
  ggtitle("Q15 Race adjusted ethenicities")+
  xlab("ethnicity")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")+
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave("./output_plots/q15_Race_adjusted.png")


#write as a table
Q15.race.table.adj <- as.data.frame(table(as.factor(df.ethnicity$tracked_ethnicities), useNA = "always"),stringsAsFactors = FALSE)
total.responses=sum(Q15.race.table.adj$Freq)
Q15.race.table.adj <- Q15.race.table.adj%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))%>%
  mutate(question = "Q15 Race adj")

write.csv(Q15.race.table.adj, file = "./output_tables/Q15.race.table.adj.csv")

#question STEM representation

df.training %>%
  ggplot()+
  aes(x= reorder(df.training$faculty_preperation,
                 df.training$faculty_preperation,
                 function(x)-length(x)))+
  geom_bar()+
  #scale_x_discrete(labels=c("Self Taught",
  #                          "Formal Training",
  #                          "Workshops and Bootcamps",
  #                          "No Training",
  #                          "NA"))+
  ggtitle("Q3 adjusted training levels")+
  xlab("Training")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")+
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave("./output_plots/q3_training_adjusted.png")


#write as a table
Q3.training.table.adj <- as.data.frame(table(as.factor(df.training$faculty_preperation), useNA = "always"),stringsAsFactors = FALSE)
total.responses=sum(Q3.training.table.adj$Freq)
Q3.training.table.adj <- Q3.training.table.adj%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))%>%
  mutate(question = "Q3 Bioinformatics Training adj")

write.csv(Q3.training.table.adj, file = "./output_tables/Q3.training.table.adj.csv")

#Highest degree

df %>%
  ggplot()+
  aes(x= reorder(df$Q17_Highest.earned.degree..If..other...please.explain.,
                 df$Q17_Highest.earned.degree..If..other...please.explain.,
                 function(x)-length(x)))+
  geom_bar()+
  ggtitle("Q17 Highest Degree Earned")+
  xlab("Degree")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")+
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave("./output_plots/q17_degree.png")


#write as a table
Q17.degree.table <- as.data.frame(table(as.factor(df$Q17_Highest.earned.degree..If..other...please.explain.), useNA = "always"),stringsAsFactors = FALSE)
total.responses=sum(Q17.degree.table$Freq)
Q17.degree.table <- Q17.degree.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))%>%
  mutate(question = "Q17 Highest Degree")

write.csv(Q17.degree.table, file = "./output_tables/Q17.degree.table.csv")


#combine tables
master.df.p1 <- rbind(Q21.carnegie.classification.table,
                   Q1.currently.teaching.table,
                   Q15.race.table,
                   Q15.race.table.adj,
                   Q17.degree.table,
                   Q3.level.of.training.table,
                   Q3.training.table.adj,
                   Q22.msi.table,
                   Q14.gender.table,
                   Q24.undergraduate.enrollment.table)

#combine tables for specially formatted output

master.df.p2 <- rbind(Q21.carnegie.classification.table,
                      Q1.currently.teaching.table,
                      Q15.race.table,
                      Q15.race.table.adj,
                      Q17.degree.table,
                      Q3.level.of.training.table,
                      Q3.training.table.adj,
                      Q22.msi.table,
                      Q14.gender.table,
                      Q24.undergraduate.enrollment.table, 
                      Q18.degree.bin.table)


# write for summary 

write.csv(master.df.p2, file = "./output_tables/summary_demographics_w_NAs.csv")

#reorder columns
#master.df <- master.df%>%
#  select(question, Var1, Freq, percentage, percentage_of_total_N)


#Q38 Filter

#filter the data frame - Q38 was only shown to users who answered 
#"3_Do NOT currently, but would like to include 'substantial' bioinformatics in courses for life-science majors" to Q1 

df <- df%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "Teaching: Not Integrating Bioinformatics")

df.ethnicity <- df.ethnicity%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "3_Do NOT currently, but would like to include 'substantial' bioinformatics in courses for life-science majors")

df.training <- df.training%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "3_Do NOT currently, but would like to include 'substantial' bioinformatics in courses for life-science majors")

df.degree <- df.degree%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "3_Do NOT currently, but would like to include 'substantial' bioinformatics in courses for life-science majors")



#question 18 - adjusted degree year

df %>%
  ggplot()+
  aes(x= reorder(df.degree$bin_degree_yrs,
                 df.degree$bin_degree_yrs, 
                 function(x)-length(x)))+
  geom_bar()+
  ggtitle("Q18 - adjusted Year of Degree Survey Respondents - 38 filter")+
  xlab("degree years")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")

ggsave("./output_plots/q18_survey_responses_by_degree_year_bin_38.png")

#write as a table
Q18.degree.bin.table <- as.data.frame(table(as.factor(df.degree$bin_degree_yrs)),stringsAsFactors = FALSE)
total.responses=sum(Q18.degree.bin.table$Freq)
Q18.degree.bin.table <- Q18.degree.bin.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))%>%
  mutate(question = "Q18 bin degree_38.filter")
write.csv(Q18.degree.bin.table, file = "./output_tables/Q18.bin.degree.table_38.filter.csv")





#generate single variable summary plots

#question 14 - gender

df %>%
  ggplot()+
  aes(x= reorder(df$Q14_Sex,
                 df$Q14_Sex, 
                 function(x)-length(x)))+
  geom_bar()+
  #scale_x_discrete(labels=c("Female", "Male", "Not Provided"))+
  ggtitle("Q14 - Survey Responses by Sex - 38 filter")+
  xlab("sex")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")

ggsave("./output_plots/q14_survey_responses_by_sex_38.filter.png")

#write as a table
Q14.gender.table <- as.data.frame(table(as.factor(df$Q14_Sex)),stringsAsFactors = FALSE)
total.responses=sum(Q14.gender.table$Freq)
Q14.gender.table <- Q14.gender.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))%>%
  mutate(question = "Q14 sex_38.filter")
write.csv(Q14.gender.table, file = "./output_tables/Q14.sex.table_38.filter.csv")


#question 1 - Current teaching
df %>%
  ggplot()+
  aes(x= reorder(df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn..., 
                 df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn..., 
                 function(x)-length(x)))+
  geom_bar()+
  #scale_x_discrete(labels=c("life-science majors: teaching bioinformatics course", 
  #                          "life-science majors: including bioinformatics",
  #                          "life-science majors: not currently including bioinformatics", 
  #                          "life science graduate advisor"))+
  ggtitle("Q1 - Survey Responses by Teaching Category_38.filter")+
  xlab("teaching category")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("./output_plots/q1_survey_responses_by_teaching_category_38.filter.png")

#write as a table
Q1.currently.teaching.table <- as.data.frame(table(as.factor(df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...)),stringsAsFactors = FALSE)
total.responses=sum(Q1.currently.teaching.table$Freq)
Q1.currently.teaching.table <- Q1.currently.teaching.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))%>%
  mutate(question = "Q1 Bioinformatics teaching_38.filter")
write.csv(Q1.currently.teaching.table, file = "./output_tables/Q1.currently.teaching.table_38.filter.csv")


#question 21, carnegie classification 

df %>%
  ggplot()+
  aes(x= reorder(df$Q21_What.is.the.Carnegie.classification.of.your.institution.,
                 df$Q21_What.is.the.Carnegie.classification.of.your.institution.,
                 function(x)-length(x)))+
  geom_bar()+
  #scale_x_discrete(labels=c("4; Doctoral", "2; Baccalaureate","3; Master's", "1; Associate's", "Don't know"))+
  ggtitle("Q21 - Survey Responses by Carnegie Classification_38.filter")+
  xlab("carnegie classification")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")
ggsave("./output_plots/q21_survey_responses_by_carnegie_classification_38.filter.png")


#write as a table
Q21.carnegie.classification.table <- as.data.frame(table(as.factor(df$Q21_What.is.the.Carnegie.classification.of.your.institution.)),stringsAsFactors = FALSE)
total.responses=sum(Q21.carnegie.classification.table$Freq)
Q21.carnegie.classification.table <- Q21.carnegie.classification.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))%>%
  mutate(question = "Q21 Carnegie classification_38.filter")
write.csv(Q21.carnegie.classification.table, file = "./output_tables/Q21.carnegie.classification.table_38.filter.csv")



#question 3, level of training

df %>%
  ggplot()+
  aes(x= reorder(df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.,
                 df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.,
                 function(x)-length(x)))+
  geom_bar()+
  #scale_x_discrete(labels=c("No formal training",
  #                          "Short workshop/bootcamp",
  #                          "Post-graduate certificate",
  #                          "No training/experience",
  #                          "Graduate course/degree",
  #                          "Some undergraduate courses",
  #                          "Undergraduate degree",
  #                          "Undergraduate certificate"
  #                          ))+
  ggtitle("Q3 - Survey Responses by Level of Bioinformatics Training_38.filter")+
  xlab("training level")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("./output_plots/q3_survey_responses_by_level_of_training_38.filter.png")


#write as a table
Q3.level.of.training.table <- as.data.frame(table(as.factor(df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.)),stringsAsFactors = FALSE)
total.responses=sum(Q3.level.of.training.table$Freq)
Q3.level.of.training.table <- Q3.level.of.training.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))%>%
  mutate(question = "Q3 Level of Training_38.filter")
write.csv(Q3.level.of.training.table, file = "./output_tables/Q3.level.of.training.table_38.filter.csv")


#question 22, MSI

df %>%
  ggplot()+
  aes(x= reorder(df$Q22_Is.your.institution.classified.as.minority.serving.,
                 df$Q22_Is.your.institution.classified.as.minority.serving.,
                 function(x)-length(x)))+
  geom_bar()+
  #scale_x_discrete(labels=c("No",
  #                          "Yes",
  #                          "Don't know"))+
  ggtitle("Q22 - Minority Serving Institution_38.filter")+
  xlab("institution type")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")
ggsave("./output_plots/q22_survey_responses_by_MSI_38.filter.png")


#write as a table
Q22.msi.table <- as.data.frame(table(as.factor(df$Q22_Is.your.institution.classified.as.minority.serving.)),stringsAsFactors = FALSE)
total.responses=sum(Q22.msi.table$Freq)
Q22.msi.table <- Q22.msi.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))%>%
  mutate(question = "Q22 MSI Status_38.filter")
write.csv(Q22.msi.table, file = "./output_tables/Q22.msi.table_38.filter.csv")



#question 24, number of undergraduate students 

df %>%
  ggplot()+
  aes(x= reorder(df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution.,
                 df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution.,
                 function(x)-length(x)))+
  geom_bar()+
  #scale_x_discrete(labels=c("< 5,000",
  #                          "5-15,000",
  #                          "> 15,000", 
  #                          "Don't know"))+
  ggtitle("Q24 - Number of undegraduate students_38.filter")+
  xlab("student enrollment")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")
ggsave("./output_plots/q24_survey_responses_by_undegraduate_enrollment_38.filter.png")

#write as a table
Q24.undergraduate.enrollment.table <- as.data.frame(table(as.factor(df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution.)),stringsAsFactors = FALSE)
total.responses=sum(Q24.undergraduate.enrollment.table$Freq)
Q24.undergraduate.enrollment.table <- Q24.undergraduate.enrollment.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))%>%
  mutate(question = "Q24 Undergrad Enrollment_38.filter")
write.csv(Q24.undergraduate.enrollment.table, file = "./output_tables/Q24.undergraduate.enrollment.table_38.filter.csv")



#question Ethnicity/Race

df %>%
  ggplot()+
  aes(x= reorder(df$Q15_Race,
                 df$Q15_Race,
                 function(x)-length(x)))+
  geom_bar()+
  #scale_x_discrete(labels=c("White",
  #                          "Rather Not Say",
  #                          "Asian",
  #                          "Black or African American",
  #                          "Native Hawaiian or other Pacific Islander", 
  #                          "American Indian or Alaska Native",
  #                          "NA"))+
  ggtitle("Q15 Race_38.filter")+
  xlab("ethnicity")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")
ggsave("./output_plots/q15_Race_38.filter.png")


#write as a table
Q15.race.table <- as.data.frame(table(as.factor(df$Q15_Race)),stringsAsFactors = FALSE)
total.responses=sum(Q15.race.table$Freq)
Q15.race.table <- Q15.race.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))%>%
  mutate(question = "Q15 Race_38.filter")
write.csv(Q15.race.table, file = "./output_tables/Q15.race.table_38.filter.csv")

#question tracked ethnicities

df.ethnicity %>%
  ggplot()+
  aes(x= reorder(df.ethnicity$tracked_ethnicities,
                 df.ethnicity$tracked_ethnicities,
                 function(x)-length(x)))+
  geom_bar()+
  #scale_x_discrete(labels=c("White",
  #                          "Asian",
  #                          "Hispanic",
  #                          "Black or African American",
  #                          "Native Hawaiian or other Pacific Islander", 
  #                          "American Indian or Alaska Native",
  #                          "NA"))+
  ggtitle("Q15 Race adjusted ethenicities_38.filter")+
  xlab("ethnicity")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")+
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave("./output_plots/q15_Race_adjusted_38.filter.png")


#write as a table
Q15.race.table.adj <- as.data.frame(table(as.factor(df.ethnicity$tracked_ethnicities)),stringsAsFactors = FALSE)
total.responses=sum(Q15.race.table.adj$Freq)
Q15.race.table.adj <- Q15.race.table.adj%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))%>%
  mutate(question = "Q15 Race adj_38.filter")

write.csv(Q15.race.table.adj, file = "./output_tables/Q15.race.table.adj_38.filter.csv")

#question STEM representation

df.training %>%
  ggplot()+
  aes(x= reorder(df.training$faculty_preperation,
                 df.training$faculty_preperation,
                 function(x)-length(x)))+
  geom_bar()+
  #scale_x_discrete(labels=c("Self Taught",
  #                          "Formal Training",
  #                          "Workshops and Bootcamps",
  #                          "No Training",
  #                          "NA"))+
  ggtitle("Q3 adjusted training levels_38.filter")+
  xlab("Training")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")+
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave("./output_plots/q3_training_adjusted_38.filter.png")


#write as a table
Q3.training.table.adj <- as.data.frame(table(as.factor(df.training$faculty_preperation)),stringsAsFactors = FALSE)
total.responses=sum(Q3.training.table.adj$Freq)
Q3.training.table.adj <- Q3.training.table.adj%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))%>%
  mutate(question = "Q3 Bioinformatics Training adj_38.filter")

write.csv(Q3.training.table.adj, file = "./output_tables/Q3.training.table.adj_38.filter.csv")

#Highest degree

df %>%
  ggplot()+
  aes(x= reorder(df$Q17_Highest.earned.degree..If..other...please.explain.,
                 df$Q17_Highest.earned.degree..If..other...please.explain.,
                 function(x)-length(x)))+
  geom_bar()+
  ggtitle("Q17 Highest Degree Earned_38.filter")+
  xlab("Degree")+
  stat_count(aes(label=..count..), vjust= -0.5, geom="text", position="identity")+
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave("./output_plots/q17_degree_38.filter.png")


#write as a table
Q17.degree.table <- as.data.frame(table(as.factor(df$Q17_Highest.earned.degree..If..other...please.explain.)),stringsAsFactors = FALSE)
total.responses=sum(Q17.degree.table$Freq)
Q17.degree.table <- Q17.degree.table%>%
  mutate(percentage = (Freq/total.responses))%>%
  mutate(percentage_of_total_N = (Freq/survey.n))%>%
  mutate(question = "Q17 Highest Degree_38.filter")

write.csv(Q17.degree.table, file = "./output_tables/Q17.degree.table_38.filter.csv")


#combine tables
master.df.p2 <- rbind(Q21.carnegie.classification.table,
                   Q1.currently.teaching.table,
                   Q15.race.table,
                   Q15.race.table.adj,
                   Q17.degree.table,
                   Q3.level.of.training.table,
                   Q3.training.table.adj,
                   Q22.msi.table,
                   Q14.gender.table,
                   Q24.undergraduate.enrollment.table)

master.df <- rbind(master.df.p1, master.df.p2)

#reorder columns
master.df <- master.df%>%
  select(question, Var1, Freq, percentage, percentage_of_total_N)%>%
  arrange(question)

write_csv(master.df, "../summarize_and_plot_demographic_information/output_tables/summary_table.csv")


#set col values to numeric
master.df$percentage <- as.numeric(master.df$percentage)

master.plot <- master.df%>%
  arrange(Var1, question)%>%
  unite(Var1, question, col = plot_label, sep = "_")


master.plot%>%
  ggplot()+
  aes(x=plot_label, y=percentage)+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("../summarize_and_plot_demographic_information/output_plots/master_plot.png")


