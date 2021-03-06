# Counting script 
# Count the number of repondents (and percentage) for given combinations of survey questions
require(tidyverse)

# How many repondents are integrating bioinformatics by degree year?

data.df <- read_csv("../../data_cleaning_scripts/07_adjust_degree_year/output/decoded_df_w_bin_degree_years.csv")

#training dataframe with binned years

data.training <- read_csv("../../data_cleaning_scripts/06_adjust_bioinformatics_training/output/decoded_df_w_faculty_preperation.csv")

data.training <- cbind(data.training, 
                       data.df$bin_degree_yrs)


#remove non-us repondents
remove.non.us.repondants <- function(df){
  countries <- c("United States","Puerto Rico")
  df <- df%>%
    filter(Country_Country %in% countries )
  return(df)
}

data.df <- remove.non.us.repondants(data.df)
data.training<- remove.non.us.repondants(data.training)

# data.df bin_degree_yrs

# Integrators in..

# 1980s
count.1980 <-  data.df%>%
  filter(bin_degree_yrs == "1980-1989")%>%
  filter(!is.na(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...) )%>%
  nrow()

count.1980.integrators <- data.df%>%
  filter(bin_degree_yrs == "1980-1989")%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "1_Dedicated course for life-science majors" |
         Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "2_Include 'substantial' bioinformatics in courses for life-science majors")%>%
  nrow()

percentage.integrators.1980 <- (count.1980.integrators/count.1980)*100

# 1990s
count.1990 <-  data.df%>%
  filter(bin_degree_yrs == "1990-1999")%>%
  filter(!is.na(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...) )%>%
  nrow()

count.1990.integrators <- data.df%>%
  filter(bin_degree_yrs == "1990-1999")%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "1_Dedicated course for life-science majors" |
           Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "2_Include 'substantial' bioinformatics in courses for life-science majors")%>%
  nrow()

percentage.integrators.1990 <- (count.1990.integrators/count.1990)*100

# 2000s

count.2000 <-  data.df%>%
  filter(bin_degree_yrs == "2000-2009")%>%
  filter(!is.na(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...) )%>%
  nrow()

count.2000.integrators <- data.df%>%
  filter(bin_degree_yrs == "2000-2009")%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "1_Dedicated course for life-science majors" |
           Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "2_Include 'substantial' bioinformatics in courses for life-science majors")%>%
  nrow()

percentage.integrators.2000 <- (count.2000.integrators/count.2000)*100

# 2010-16

count.2010 <-  data.df%>%
  filter(bin_degree_yrs == "2010-2016")%>%
  filter(!is.na(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...) )%>%
  nrow()

count.2010.integrators <- data.df%>%
  filter(bin_degree_yrs == "2010-2016")%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "1_Dedicated course for life-science majors" |
           Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "2_Include 'substantial' bioinformatics in courses for life-science majors")%>%
  nrow()

percentage.integrators.2010 <- (count.2010.integrators/count.2010)*100


#What percentage of respondents by degree decade are at associates schools?

# 1980s
c.count.1980 <-  data.df%>%
  filter(bin_degree_yrs == "1980-1989")%>%
  filter(!is.na(Q21_What.is.the.Carnegie.classification.of.your.institution.) )%>%
  nrow()

c.count.1980.integrators <- data.df%>%
  filter(bin_degree_yrs == "1980-1989")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College" )%>%
  nrow()

percentage.assoc.1980 <- (c.count.1980.integrators/c.count.1980)*100

# 1990s
c.count.1990 <-  data.df%>%
  filter(bin_degree_yrs == "1990-1999")%>%
  filter(!is.na(Q21_What.is.the.Carnegie.classification.of.your.institution.) )%>%
  nrow()

c.count.1990.integrators <- data.df%>%
  filter(bin_degree_yrs == "1990-1999")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College" )%>%
  nrow()

percentage.assoc.1990 <- (c.count.1990.integrators/c.count.1990)*100

# 2000s

c.count.2000 <-  data.df%>%
  filter(bin_degree_yrs == "2000-2009")%>%
  filter(!is.na(Q21_What.is.the.Carnegie.classification.of.your.institution.) )%>%
  nrow()

c.count.2000.integrators <- data.df%>%
  filter(bin_degree_yrs == "2000-2009")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College" )%>%
  nrow()

percentage.assoc.2000 <- (c.count.2000.integrators/c.count.2000)*100

# 2010-16

c.count.2010 <-  data.df%>%
  filter(bin_degree_yrs == "2010-2016")%>%
  filter(!is.na(Q21_What.is.the.Carnegie.classification.of.your.institution.) )%>%
  nrow()

c.count.2010.integrators <- data.df%>%
  filter(bin_degree_yrs == "2010-2016")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College" )%>%
  nrow()

percentage.assoc.2010 <- (c.count.2010.integrators/c.count.2010)*100

#What percentage of respondents by degree decade are at masters schools?

# 1980s
c.count.1980 <-  data.df%>%
  filter(bin_degree_yrs == "1980-1989")%>%
  filter(!is.na(Q21_What.is.the.Carnegie.classification.of.your.institution.) )%>%
  nrow()

c.count.1980.integrators <- data.df%>%
  filter(bin_degree_yrs == "1980-1989")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)" )%>%
  nrow()

percentage.masters.1980 <- (c.count.1980.integrators/c.count.1980)*100

# 1990s
c.count.1990 <-  data.df%>%
  filter(bin_degree_yrs == "1990-1999")%>%
  filter(!is.na(Q21_What.is.the.Carnegie.classification.of.your.institution.) )%>%
  nrow()

c.count.1990.integrators <- data.df%>%
  filter(bin_degree_yrs == "1990-1999")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)" )%>%
  nrow()

percentage.masters.1990 <- (c.count.1990.integrators/c.count.1990)*100

# 2000s

c.count.2000 <-  data.df%>%
  filter(bin_degree_yrs == "2000-2009")%>%
  filter(!is.na(Q21_What.is.the.Carnegie.classification.of.your.institution.) )%>%
  nrow()

c.count.2000.integrators <- data.df%>%
  filter(bin_degree_yrs == "2000-2009")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)" )%>%
  nrow()

percentage.masters.2000 <- (c.count.2000.integrators/c.count.2000)*100

# 2010-16

c.count.2010 <-  data.df%>%
  filter(bin_degree_yrs == "2010-2016")%>%
  filter(!is.na(Q21_What.is.the.Carnegie.classification.of.your.institution.) )%>%
  nrow()

c.count.2010.integrators <- data.df%>%
  filter(bin_degree_yrs == "2010-2016")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)" )%>%
  nrow()

percentage.masters.2010 <- (c.count.2010.integrators/c.count.2010)*100

#What percentage of respondents by degree decade are at bachelors schools?

# 1980s
c.count.1980 <-  data.df%>%
  filter(bin_degree_yrs == "1980-1989")%>%
  filter(!is.na(Q21_What.is.the.Carnegie.classification.of.your.institution.) )%>%
  nrow()

c.count.1980.integrators <- data.df%>%
  filter(bin_degree_yrs == "1980-1989")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College" )%>%
  nrow()

percentage.bacca.1980 <- (c.count.1980.integrators/c.count.1980)*100

# 1990s
c.count.1990 <-  data.df%>%
  filter(bin_degree_yrs == "1990-1999")%>%
  filter(!is.na(Q21_What.is.the.Carnegie.classification.of.your.institution.) )%>%
  nrow()

c.count.1990.integrators <- data.df%>%
  filter(bin_degree_yrs == "1990-1999")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College" )%>%
  nrow()

percentage.bacca.1990 <- (c.count.1990.integrators/c.count.1990)*100

# 2000s

c.count.2000 <-  data.df%>%
  filter(bin_degree_yrs == "2000-2009")%>%
  filter(!is.na(Q21_What.is.the.Carnegie.classification.of.your.institution.) )%>%
  nrow()

c.count.2000.integrators <- data.df%>%
  filter(bin_degree_yrs == "2000-2009")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College" )%>%
  nrow()

percentage.bacca.2000 <- (c.count.2000.integrators/c.count.2000)*100

# 2010-16

c.count.2010 <-  data.df%>%
  filter(bin_degree_yrs == "2010-2016")%>%
  filter(!is.na(Q21_What.is.the.Carnegie.classification.of.your.institution.) )%>%
  nrow()

c.count.2010.integrators <- data.df%>%
  filter(bin_degree_yrs == "2010-2016")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College" )%>%
  nrow()

percentage.bacca.2010 <- (c.count.2010.integrators/c.count.2010)*100

#What percentage of respondents by degree decade are at doctorate schools?

# 1980s
c.count.1980 <-  data.df%>%
  filter(bin_degree_yrs == "1980-1989")%>%
  filter(!is.na(Q21_What.is.the.Carnegie.classification.of.your.institution.) )%>%
  nrow()

c.count.1980.integrators <- data.df%>%
  filter(bin_degree_yrs == "1980-1989")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)" )%>%
  nrow()

percentage.doct.1980 <- (c.count.1980.integrators/c.count.1980)*100

# 1990s
c.count.1990 <-  data.df%>%
  filter(bin_degree_yrs == "1990-1999")%>%
  filter(!is.na(Q21_What.is.the.Carnegie.classification.of.your.institution.) )%>%
  nrow()

c.count.1990.integrators <- data.df%>%
  filter(bin_degree_yrs == "1990-1999")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)" )%>%
  nrow()

percentage.doct.1990 <- (c.count.1990.integrators/c.count.1990)*100

# 2000s

c.count.2000 <-  data.df%>%
  filter(bin_degree_yrs == "2000-2009")%>%
  filter(!is.na(Q21_What.is.the.Carnegie.classification.of.your.institution.) )%>%
  nrow()

c.count.2000.integrators <- data.df%>%
  filter(bin_degree_yrs == "2000-2009")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)" )%>%
  nrow()

percentage.doct.2000 <- (c.count.2000.integrators/c.count.2000)*100

# 2010-16

c.count.2010 <-  data.df%>%
  filter(bin_degree_yrs == "2010-2016")%>%
  filter(!is.na(Q21_What.is.the.Carnegie.classification.of.your.institution.) )%>%
  nrow()

c.count.2010.integrators <- data.df%>%
  filter(bin_degree_yrs == "2010-2016")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)" )%>%
  nrow()

percentage.doct.2010 <- (c.count.2010.integrators/c.count.2010)*100


# regardless of integration status where are 2010-16 faculty by institution type?

count.all.faculty.assoc <- data.df%>%
  filter(!is.na(bin_degree_yrs))%>%
  filter(!is.na(Q21_What.is.the.Carnegie.classification.of.your.institution.))%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  nrow()
count.2010.faculty.assoc <- data.df%>%
  filter(bin_degree_yrs == "2010-2016")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  nrow()

count.all.faculty.bacca <- data.df%>%
  filter(!is.na(bin_degree_yrs))%>%
  filter(!is.na(Q21_What.is.the.Carnegie.classification.of.your.institution.))%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College" )%>%
  nrow()
count.2010.faculty.bacca <- data.df%>%
  filter(bin_degree_yrs == "2010-2016")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College" )%>%
  nrow()
count.all.faculty.masters <- data.df%>%
  filter(!is.na(bin_degree_yrs))%>%
  filter(!is.na(Q21_What.is.the.Carnegie.classification.of.your.institution.))%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)" )%>%
  nrow()
count.2010.faculty.masters <- data.df%>%
  filter(bin_degree_yrs == "2010-2016")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)" )%>%
  nrow()
count.all.faculty.doct <- data.df%>%
  filter(!is.na(bin_degree_yrs))%>%
  filter(!is.na(Q21_What.is.the.Carnegie.classification.of.your.institution.))%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)" )%>%
  nrow()
count.2010.faculty.doct <- data.df%>%
  filter(bin_degree_yrs == "2010-2016")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)" )%>%
  nrow()

# total number of respondents who answered the inst type and degree question

n_instType_degree <- sum(count.all.faculty.assoc, 
                         count.all.faculty.bacca, 
                         count.all.faculty.masters, 
                         count.all.faculty.doct)

per.2010.assoc <- count.2010.faculty.assoc/count.all.faculty.assoc
per.2010.bacca <- count.2010.faculty.bacca/count.all.faculty.bacca
per.2010.masters<- count.2010.faculty.masters/count.all.faculty.masters
per.2010.doct<- count.2010.faculty.doct/count.all.faculty.doct


# proportion test for faculty distribution at each institution type

dis_2010_testing_table <- data.frame("type" = c("assoc", 
                                                "bacca", 
                                                "master",
                                                "doct"), 
                                   "N_type"= c(count.all.faculty.assoc, 
                                               count.all.faculty.bacca,
                                               count.all.faculty.masters, 
                                               count.all.faculty.doct), 
                                   "n_2010_faculty" = c(count.2010.faculty.assoc, 
                                                        count.2010.faculty.bacca, 
                                                        count.2010.faculty.masters, 
                                                        count.2010.faculty.doct), 
                                   stringsAsFactors = FALSE
)

dis_2010_testing_table.proptest <- prop.test(dis_2010_testing_table$n_2010_faculty, 
                                             dis_2010_testing_table$N_type)$p.value



# How formally trained are the newer faculty?

# 1980s
t.count.1980 <-  data.training%>%
  filter(data.df$bin_degree_yrs == "1980-1989")%>%
  filter(!is.na(faculty_preperation) )%>%
  nrow()

t.count.1980.formaltrained <- data.training%>%
  filter(data.df$bin_degree_yrs == "1980-1989")%>%
  filter(faculty_preperation == "Formal Training" )%>%
  nrow()

percentage.ftrained.1980 <- (t.count.1980.formaltrained/t.count.1980)*100

# 1990s
t.count.1990 <-  data.training%>%
  filter(data.df$bin_degree_yrs == "1990-1999")%>%
  filter(!is.na(faculty_preperation) )%>%
  nrow()

t.count.1990.formaltrained <- data.training%>%
  filter(data.df$bin_degree_yrs == "1990-1999")%>%
  filter(faculty_preperation == "Formal Training" )%>%
  nrow()

percentage.ftrained.1990 <- (t.count.1990.formaltrained/t.count.1990)*100

# 2000s

t.count.2000 <-  data.training%>%
  filter(data.df$bin_degree_yrs == "2000-2009")%>%
  filter(!is.na(faculty_preperation) )%>%
  nrow()

t.count.2000.formaltrained <- data.training%>%
  filter(data.df$bin_degree_yrs == "2000-2009")%>%
  filter(faculty_preperation == "Formal Training" )%>%
  nrow()

percentage.ftrained.2000 <- (t.count.2000.formaltrained/t.count.2000)*100

# 2010-16

t.count.2010 <-  data.training%>%
  filter(data.df$bin_degree_yrs == "2010-2016")%>%
  filter(!is.na(faculty_preperation) )%>%
  nrow()

t.count.2010.formaltrained <- data.training%>%
  filter(data.df$bin_degree_yrs == "2010-2016")%>%
  filter(faculty_preperation == "Formal Training" )%>%
  nrow()

percentage.ftrained.2010 <- (t.count.2010.formaltrained/t.count.2010)*100


#calculate n of formal training question 

n.formal.trainig <- sum(t.count.1980, t.count.1990, t.count.2000, t.count.2010)


# How many integrators by sex

# male
count.male <-  data.df%>%
  filter(Q14_Sex == "2_Male")%>%
  filter(!is.na(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...) )%>%
  nrow()

count.male.integrators <- data.df%>%
  filter(Q14_Sex == "2_Male")%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "1_Dedicated course for life-science majors" |
           Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "2_Include 'substantial' bioinformatics in courses for life-science majors")%>%
  nrow()

percentage.integrators.male <- (count.male.integrators/count.male)*100


# female
count.female <-  data.df%>%
  filter(Q14_Sex == "1_Female")%>%
  filter(!is.na(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...) )%>%
  nrow()

count.female.integrators <- data.df%>%
  filter(Q14_Sex == "1_Female")%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "1_Dedicated course for life-science majors" |
           Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "2_Include 'substantial' bioinformatics in courses for life-science majors")%>%
  nrow()

percentage.integrators.female <- (count.female.integrators/count.female)*100
