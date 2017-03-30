# Counting script 
# Count the number of repondents (and percentage) for given combinations of survey questions
require(tidyverse)

# How many repondents are integrating bioinformatics by degree year?

data.df <- read_csv("../../data_cleaning_scripts/07_adjust_degree_year/output/decoded_df_w_bin_degree_years.csv")

#remove non-us repondents
remove.non.us.repondants <- function(df){
  countries <- c("United States","Puerto Rico")
  df <- df%>%
    filter(Country_Country %in% countries )
  return(df)
}

data.df <- remove.non.us.repondants(data.df)

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


#What percentage of respondents by degree decade are at associates schools

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



#What percentage of respondents by degree decade are at doctorate schools

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