#This transforms the summed_reduced_factors.csv by translating survey responses (e.g. 1,2,3..)
#Into their text choices

library(tidyverse)

df <- read_csv("./summed_reduced_factors.csv")

#Transform question 57

df$Q57_Please.select.the.statement.belOw.that.best.describes.yOu.[df$Q57_Please.select.the.statement.belOw.that.best.describes.yOu. == "1"] <- "Teach at 4-year institution"
df$Q57_Please.select.the.statement.belOw.that.best.describes.yOu.[df$Q57_Please.select.the.statement.belOw.that.best.describes.yOu. == "2"] <- "Teach at 2-year institution"
df$Q57_Please.select.the.statement.belOw.that.best.describes.yOu.[df$Q57_Please.select.the.statement.belOw.that.best.describes.yOu. == "3"] <- "Teach at 'other'"

#Transform question 1

df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...[df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "1"] <- "Dedicated course for life-science majors"
df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...[df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "2"] <- "Include 'substantial' bioinformatics in courses for life-science majors"
df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...[df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "3"] <- "Do NOT currently, but would like to include 'substantial' bioinformatics in courses for life-science majors"
df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...[df$Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "4"] <- "Graduate supervisors in the life sciences"

#Transform question 29
df$Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g.....[df$Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g..... == "1"] <- "Yes"
df$Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g.....[df$Q29_At.your.current.institution..do.you.face.any.technical.barriers.in.teaching.bioinformatics..e.g..... == "2"] <- "No"

#Transform question 14
df$Q14_Sex[df$Q14_Sex == "1"] <- "Female"
df$Q14_Sex[df$Q14_Sex == "2"] <- "Male"
df$Q14_Sex[df$Q14_Sex == "3"] <- "Rather not say"

#Transform question 15
df$Q15_Race[df$Q15_Race == "1"] <- "American Indian or Alaska Native"
df$Q15_Race[df$Q15_Race == "2"] <- "Asian"
df$Q15_Race[df$Q15_Race == "3"] <- "Black or African American"
df$Q15_Race[df$Q15_Race == "4"] <- "Native Hawaiian or Other Pacific Islander"
df$Q15_Race[df$Q15_Race == "5"] <- "White"
df$Q15_Race[df$Q15_Race == "6"] <- "Rather not say"

#Transform question 16
df$Q16_Ethnicity[df$Q16_Ethnicity == "1"] <- "Hispanic or Latino"
df$Q16_Ethnicity[df$Q16_Ethnicity == "2"] <- "Not Hispanic or Latino"
df$Q16_Ethnicity[df$Q16_Ethnicity == "3"] <- "Rather not say"

#Transform question 17 
df$Q17_Highest.earned.degree..If..other...please.explain.[df$Q17_Highest.earned.degree..If..other...please.explain. == "1"] <- "B.S. (or equivalent)"
df$Q17_Highest.earned.degree..If..other...please.explain.[df$Q17_Highest.earned.degree..If..other...please.explain. == "2"] <- "M.S. (or equivalent)"
df$Q17_Highest.earned.degree..If..other...please.explain.[df$Q17_Highest.earned.degree..If..other...please.explain. == "3"] <- "Professional degree (e.g., M.D.)"
df$Q17_Highest.earned.degree..If..other...please.explain.[df$Q17_Highest.earned.degree..If..other...please.explain. == "4"] <- "Ph.D. (or equivalent)"
df$Q17_Highest.earned.degree..If..other...please.explain.[df$Q17_Highest.earned.degree..If..other...please.explain. == "5"] <- "Other"

#Transform question 18
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "1"] <- "2016"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "2"] <- "2015"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "3"] <- "2014"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "4"] <- "2013"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "5"] <- "2012"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "6"] <- "2011"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "7"] <- "2010"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "8"] <- "2009"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "9"] <- "2008"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "10"] <- "2007"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "11"] <- "2006"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "12"] <- "2005"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "13"] <- "2004"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "14"] <- "2003"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "15"] <- "2002"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "16"] <- "2001"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "17"] <- "2000"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "18"] <- "1999"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "19"] <- "1998"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "20"] <- "1997"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "21"] <- "1996"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "22"] <- "1995"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "23"] <- "1994"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "24"] <- "1993"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "25"] <- "1992"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "26"] <- "1991"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "27"] <- "1990"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "28"] <- "1989"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "29"] <- "1988"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "30"] <- "1987"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "31"] <- "1986"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "32"] <- "1985"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "33"] <- "1984"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "34"] <- "1983"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "35"] <- "1982"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "36"] <- "1981"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "37"] <- "1980"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "38"] <- "1979"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "39"] <- "1978"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "40"] <- "1977"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "41"] <- "1976"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "42"] <- "1975"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "43"] <- "1974"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "44"] <- "1973"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "45"] <- "1972"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "46"] <- "1971"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "47"] <- "1970"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "48"] <- "1969"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "49"] <- "1968"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "50"] <- "1967"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "51"] <- "1966"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "52"] <- "1965"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "53"] <- "1964"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "54"] <- "1963"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "55"] <- "1962"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "56"] <- "1961"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "57"] <- "1960"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "58"] <- "1959"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "59"] <- "1958"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "60"] <- "1957"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "61"] <- "1956"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "62"] <- "1955"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "63"] <- "1954"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "64"] <- "1953"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "65"] <- "1952"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "66"] <- "1951"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "67"] <- "1950"
df$Q18_Year.of.highest.earned.degree.[df$Q18_Year.of.highest.earned.degree. == "68"] <- "Rather Not Say"





