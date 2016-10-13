#This transforms the summed_reduced_factors.csv by translating survey responses (e.g. 1,2,3..)
#Into their text choices

library(tidyverse)

df <- read_csv("./summed_reduced_factors.csv")

#Transform question 57
df$Q57_Please.select.the.statement.belOw.that.best.describes.yOu.[df$Q57_Please.select.the.statement.belOw.that.best.describes.yOu. == "1"] <- "1_Teach at 4-year institution"
df$Q57_Please.select.the.statement.belOw.that.best.describes.yOu.[df$Q57_Please.select.the.statement.belOw.that.best.describes.yOu. == "2"] <- "2_Teach at 2-year institution"
df$Q57_Please.select.the.statement.belOw.that.best.describes.yOu.[df$Q57_Please.select.the.statement.belOw.that.best.describes.yOu. == "3"] <- "3_Teach at 'Other'"

#Transform question 1
df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...[df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "1"] <- "1_Dedicated course for life-science majors"
df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...[df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "2"] <- "2_Include 'substantial' bioinformatics in courses for life-science majors"
df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...[df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "3"] <- "3_Do NOT currently, but would like to include 'substantial' bioinformatics in courses for life-science majors"
df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...[df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "4"] <- "4_Graduate supervisors in the life sciences"

#Transform question 5
df$Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur...[df$Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur... == "1"] <- "1_Yes"
df$Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur...[df$Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur... == "2"] <- "2_No"
df$Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur...[df$Q5_In.yOur.OpiniOn..are.additiOnal.undergraduate.cOurses.with.biOinfOrmatics.cOntent.needed.at.yOur... == "3"] <- "3_Don't know"

#Transform question 29
df$Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g.....[df$Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g..... == "1"] <- "1_Yes"
df$Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g.....[df$Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g..... == "2"] <- "2_No"

#Transform question 14
df$Q14_Sex[df$Q14_Sex == "1"] <- "1_Female"
df$Q14_Sex[df$Q14_Sex == "2"] <- "2_Male"
df$Q14_Sex[df$Q14_Sex == "3"] <- "3_Rather not say"

#Transform question 15
df$Q15_Race[df$Q15_Race == "1"] <- "1_American Indian or Alaska Native"
df$Q15_Race[df$Q15_Race == "2"] <- "2_Asian"
df$Q15_Race[df$Q15_Race == "3"] <- "3_Black or African American"
df$Q15_Race[df$Q15_Race == "4"] <- "4_Native Hawaiian or Other Pacific Islander"
df$Q15_Race[df$Q15_Race == "5"] <- "5_White"
df$Q15_Race[df$Q15_Race == "6"] <- "6_Rather not say"

#Transform question 16
df$Q16_Ethnicity[df$Q16_Ethnicity == "1"] <- "1_Hispanic or Latino"
df$Q16_Ethnicity[df$Q16_Ethnicity == "2"] <- "2_Not Hispanic or Latino"
df$Q16_Ethnicity[df$Q16_Ethnicity == "3"] <- "3_Rather not say"

#Transform question 17 
df$Q17_Highest.earned.degree..If..other...please.explain.[df$Q17_Highest.earned.degree..If..other...please.explain. == "1"] <- "1_B.S. (or equivalent)"
df$Q17_Highest.earned.degree..If..other...please.explain.[df$Q17_Highest.earned.degree..If..other...please.explain. == "2"] <- "2_M.S. (or equivalent)"
df$Q17_Highest.earned.degree..If..other...please.explain.[df$Q17_Highest.earned.degree..If..other...please.explain. == "3"] <- "3_Professional degree (e.g., M.D.)"
df$Q17_Highest.earned.degree..If..other...please.explain.[df$Q17_Highest.earned.degree..If..other...please.explain. == "4"] <- "4_Ph.D. (or equivalent)"
df$Q17_Highest.earned.degree..If..other...please.explain.[df$Q17_Highest.earned.degree..If..other...please.explain. == "5"] <- "5_Other"

#Transform question 18
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "1"] <- "1_2016"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "2"] <- "2_2015"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "3"] <- "3_2014"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "4"] <- "4_2013"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "5"] <- "5_2012"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "6"] <- "6_2011"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "7"] <- "7_2010"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "8"] <- "8_2009"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "9"] <- "9_2008"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "10"] <- "10_2007"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "11"] <- "11_2006"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "12"] <- "12_2005"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "13"] <- "13_2004"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "14"] <- "14_2003"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "15"] <- "15_2002"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "16"] <- "16_2001"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "17"] <- "17_2000"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "18"] <- "18_1999"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "19"] <- "19_1998"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "20"] <- "20_1997"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "21"] <- "21_1996"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "22"] <- "22_1995"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "23"] <- "23_1994"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "24"] <- "24_1993"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "25"] <- "25_1992"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "26"] <- "26_1991"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "27"] <- "27_1990"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "28"] <- "28_1989"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "29"] <- "29_1988"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "30"] <- "30_1987"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "31"] <- "31_1986"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "32"] <- "32_1985"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "33"] <- "33_1984"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "34"] <- "34_1983"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "35"] <- "35_1982"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "36"] <- "36_1981"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "37"] <- "37_1980"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "38"] <- "38_1979"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "39"] <- "39_1978"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "40"] <- "40_1977"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "41"] <- "41_1976"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "42"] <- "42_1975"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "43"] <- "43_1974"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "44"] <- "44_1973"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "45"] <- "45_1972"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "46"] <- "46_1971"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "47"] <- "47_1970"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "48"] <- "48_1969"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "49"] <- "49_1968"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "50"] <- "50_1967"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "51"] <- "51_1966"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "52"] <- "52_1965"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "53"] <- "53_1964"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "54"] <- "54_1963"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "55"] <- "55_1962"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "56"] <- "56_1961"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "57"] <- "57_1960"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "58"] <- "58_1959"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "59"] <- "59_1958"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "60"] <- "60_1957"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "61"] <- "61_1956"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "62"] <- "62_1955"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "63"] <- "63_1954"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "64"] <- "64_1953"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "65"] <- "65_1952"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "66"] <- "66_1951"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "67"] <- "67_1950"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "68"] <- "68_Rather Not Say"

#Transform question 3 (NO OPTION 5?)
df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.[df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training. == "1"] <- "1_No training/experience"
df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.[df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training. == "2"] <- "2_No formal training (self-taught)"
df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.[df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training. == "3"] <- "3_Short workshop/bootcamp"
df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.[df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training. == "4"] <- "4_Some undergraduate courses"
df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.[df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training. == "6"] <- "6_Undergraduate certificate"
df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.[df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training. == "7"] <- "7_Undergraduate degree"
df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.[df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training. == "8"] <- "8_Post-graduate certificate"
df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training.[df$Q3_Which.of.the.following.best.describes.your.level.of.bioinformatics.training. == "9"] <- "9_Graduate course	Graduate degree"

#Transform question 21
df$Q21_What.is.the.Carnegie.classification.of.your.institution.[df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "1"] <- "1_Associate's College"
df$Q21_What.is.the.Carnegie.classification.of.your.institution.[df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "2"] <- "2_Baccalaureate College"
df$Q21_What.is.the.Carnegie.classification.of.your.institution.[df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "3"] <- "3_Master's (Small, Medium, Large)"
df$Q21_What.is.the.Carnegie.classification.of.your.institution.[df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "4"] <- "4_Doctoral University (High, Higher, Highest Research Activity)"
df$Q21_What.is.the.Carnegie.classification.of.your.institution.[df$Q21_What.is.the.Carnegie.classification.of.your.institution. == "5"] <- "5_Don't know"

#Transform question 22
df$Q22_Is.your.institution.classified.as.minority.serving.[df$Q22_Is.your.institution.classified.as.minority.serving. == "1"] <- "1_Yes"
df$Q22_Is.your.institution.classified.as.minority.serving.[df$Q22_Is.your.institution.classified.as.minority.serving. == "2"] <- "2_No"
df$Q22_Is.your.institution.classified.as.minority.serving.[df$Q22_Is.your.institution.classified.as.minority.serving. == "3"] <- "3_Don't know"

#Transform question 23
df$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution.[df$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution. == "1" ] <- "1_'< 5,000' student'"
df$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution.[df$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution. == "2" ] <- "2_'5,000 - 15,000' students"
df$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution.[df$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution. == "3" ] <- "3_'> 15,000' students"
df$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution.[df$Q23_What.is.the.total.number.of.students..undergraduate.and.graduate..at.your.institution. == "4" ] <- "4_Don't know"

#Transform question 24
df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution.[df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution. == "1"] <- "1_'< 5,000' students"
df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution.[df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution. == "2"] <- "2_'5,000 - 15,000' students"
df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution.[df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution. == "3"] <- "3_'> 15,000' students"
df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution.[df$Q24_What.is.the.total.number.of.undergraduate.students.at.your.institution. == "4"] <- "4_Don't know"

#Transform question 26
df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...[df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju... == "1"] <- "1_'< 10'"
df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...[df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju... == "2"] <- "2_'10 - 20'"
df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...[df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju... == "3"] <- "3_'21 - 30'"
df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...[df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju... == "4"] <- "4_'31 - 40'"
df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...[df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju... == "5"] <- "5_'41 - 50'"
df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...[df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju... == "6"] <- "6_'> 50'"
df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju...[df$Q26_How.many.full.time.faculty.are.in.your.department.unit...Do.not.include.part.time.faculty.or.adju... == "7"] <- "7_Don't know"

#Transform question 27
df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..[df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors.. == "1"] <- "1_'< 50'"
df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..[df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors.. == "2"] <- "2_'51 - 100'"
df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..[df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors.. == "3"] <- "3_'101 - 500'"
df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..[df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors.. == "4"] <- "4_'501 - 2000'"
df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..[df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors.. == "5"] <- "5_'> 2000'"
df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors..[df$Q27_How.many.undergraduate.students.are.in.your.department.unit..all.majors.. == "6"] <- "6_Don't know"

#save decoded df in csv format
write_csv(df, "./decoded_df.csv")