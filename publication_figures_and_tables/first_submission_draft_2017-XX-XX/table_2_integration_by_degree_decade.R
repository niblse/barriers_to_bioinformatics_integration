# Counting script 
# Count the number of repondents (and percentage) for given combinations of survey questions
require(tidyverse)
require(stargazer)

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


# summarize this info in a dataframe

# people who answered question 1 and who had a degree from 1980-2016
Q1.n <- sum(count.1980, 
            count.1990,
            count.2000,
            count.2010)

int_summary_data_frame <- data.frame("Decade_of_degree"=c('1980s', 
                                                      '1990s',
                                                      '2000s', 
                                                      '2010s'
                                                      )
                                 ,
                                 'N_faculty_w_degrees'=c(count.1980,
                                                          count.1990,
                                                          count.2000,
                                                          count.2010
                                                          ),
                                 'n_faculty_integrators'=c(count.1980.integrators, 
                                                           count.1990.integrators,
                                                           count.2000.integrators,
                                                           count.2010.integrators)
                                 ,stringsAsFactors = FALSE
                                 )
int_summary_data_frame <- int_summary_data_frame%>%
  mutate(percentage_of_integrators = round(((n_faculty_integrators /N_faculty_w_degrees) * 100 ), digits=1))
                            

# proportion test 
prop_test_integrators_by_decade <- prop.test(int_summary_data_frame$n_faculty_integrators,
                                             int_summary_data_frame$N_faculty_w_degrees)$p.value
int_summary_data_frame$question_n <- Q1.n
int_summary_data_frame$chivalue <- prop_test_integrators_by_decade


# create html table

int_summary_data_frame%>%
  select(Decade_of_degree,
         N_faculty_w_degrees,
         percentage_of_integrators
         )%>%
  stargazer(., type = "html", 
            summary = FALSE, 
            rownames = FALSE, 
            digits = 1,
            title = "Number of faculty integrating bioinformatics, by degree decade", 
            out = "./integrator_percentage.html",
            covariate.labels = c("Decade", "N", "Faculty Integrating (%)"), 
            column.separate = c(1,2,3))

# write the csv table

write.csv(int_summary_data_frame, file = "./integrators_by_degree_decade.csv")


#What percentage of respondents by degree decade by carnegie classification

# 1980s
assoc.count.1980 <-  data.df%>%
  filter(bin_degree_yrs == "1980-1989")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College" )%>%
  filter(!is.na(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...))%>% 
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... != "4_Graduate supervisors in the life sciences")%>%
  nrow()

bacca.count.1980 <-  data.df%>%
  filter(bin_degree_yrs == "1980-1989")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College" )%>%
  filter(!is.na(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...))%>% 
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... != "4_Graduate supervisors in the life sciences")%>%
  nrow()

maste.count.1980 <-  data.df%>%
  filter(bin_degree_yrs == "1980-1989")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)" )%>%
  filter(!is.na(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...))%>% 
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... != "4_Graduate supervisors in the life sciences")%>%
  nrow()

docto.count.1980 <-  data.df%>%
  filter(bin_degree_yrs == "1980-1989")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)" )%>%
  filter(!is.na(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...))%>% 
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... != "4_Graduate supervisors in the life sciences")%>%
  nrow()


# 1990s
assoc.count.1990 <-  data.df%>%
  filter(bin_degree_yrs == "1990-1999")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College" )%>%
  filter(!is.na(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...))%>% 
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... != "4_Graduate supervisors in the life sciences")%>%
  nrow()

bacca.count.1990 <-  data.df%>%
  filter(bin_degree_yrs == "1990-1999")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College" )%>%
  filter(!is.na(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...))%>% 
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... != "4_Graduate supervisors in the life sciences")%>%
  nrow()

maste.count.1990 <-  data.df%>%
  filter(bin_degree_yrs == "1990-1999")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)" )%>%
  filter(!is.na(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...))%>% 
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... != "4_Graduate supervisors in the life sciences")%>%
  nrow()

docto.count.1990 <-  data.df%>%
  filter(bin_degree_yrs == "1990-1999")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)" )%>%
  filter(!is.na(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...))%>% 
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... != "4_Graduate supervisors in the life sciences")%>%
  nrow()


#2000s
assoc.count.2000 <-  data.df%>%
  filter(bin_degree_yrs == "2000-2009")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College" )%>%
  filter(!is.na(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...))%>% 
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... != "4_Graduate supervisors in the life sciences")%>%
  nrow()

bacca.count.2000 <-  data.df%>%
  filter(bin_degree_yrs == "2000-2009")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College" )%>%
  filter(!is.na(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...))%>% 
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... != "4_Graduate supervisors in the life sciences")%>%
  nrow()

maste.count.2000 <-  data.df%>%
  filter(bin_degree_yrs == "2000-2009")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)" )%>%
  filter(!is.na(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...))%>% 
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... != "4_Graduate supervisors in the life sciences")%>%
  nrow()

docto.count.2000 <-  data.df%>%
  filter(bin_degree_yrs == "2000-2009")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)" )%>%
  filter(!is.na(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...))%>% 
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... != "4_Graduate supervisors in the life sciences")%>%
  nrow()


#2010s
assoc.count.2010 <-  data.df%>%
  filter(bin_degree_yrs == "2010-2016")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College" )%>%
  filter(!is.na(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...))%>% 
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... != "4_Graduate supervisors in the life sciences")%>%
  nrow()

bacca.count.2010 <-  data.df%>%
  filter(bin_degree_yrs == "2010-2016")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College" )%>%
  filter(!is.na(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...))%>% 
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... != "4_Graduate supervisors in the life sciences")%>%
  nrow()

maste.count.2010 <-  data.df%>%
  filter(bin_degree_yrs == "2010-2016")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)" )%>%
  filter(!is.na(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...))%>% 
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... != "4_Graduate supervisors in the life sciences")%>%
  nrow()

docto.count.2010 <-  data.df%>%
  filter(bin_degree_yrs == "2010-2016")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)" )%>%
  filter(!is.na(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn...))%>% 
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... != "4_Graduate supervisors in the life sciences")%>%
  nrow()



#create a dataframe that indicates how many faculty are located at institution carnegie types by decade


cc_summary_data_frame <- data.frame("Decade_of_degree"=c('1980s', 
                                                          '1990s',
                                                          '2000s', 
                                                          '2010s'), 
                                    "n_Associates"=c(assoc.count.1980,
                                                     assoc.count.1990,
                                                     assoc.count.2000,
                                                     assoc.count.2010
                                                   ), 
                                    "n_Baccalaureate" =c(bacca.count.1980,
                                                         bacca.count.1990,
                                                         bacca.count.2000,
                                                         bacca.count.2010
                                                         ), 
                                    "n_Master_s"=c(maste.count.1980, 
                                                 maste.count.1990, 
                                                 maste.count.2000, 
                                                 maste.count.2010
                                                 ), 
                                    "n_Doctoral"=c(docto.count.1980,
                                                 docto.count.1990,
                                                 docto.count.2000,
                                                 docto.count.2010),
                                    stringsAsFactors = FALSE
                                    )

# Get the summary numbers 
cc_summary_data_frame$N_Associates <- sum(cc_summary_data_frame$n_Associates)
cc_summary_data_frame$N_Baccalaureate <- sum(cc_summary_data_frame$n_Baccalaureate)
cc_summary_data_frame$N_Master_s <- sum(cc_summary_data_frame$n_Master_s)
cc_summary_data_frame$N_Doctoral <- sum(cc_summary_data_frame$n_Doctoral)

#faculty percentages
cc_summary_data_frame <- cc_summary_data_frame%>%
  mutate(percentage_associates = round((n_Associates/N_Associates)*100,digits = 1))%>%
  mutate(percentage_baccalaureate = round((n_Baccalaureate/N_Baccalaureate)*100,digits = 1))%>%
  mutate(percentage_masters = round((n_Master_s/N_Master_s)*100,digits = 1))%>%
  mutate(percentage_doctoral = round((n_Doctoral/N_Doctoral)*100,digits = 1))

#order table nicely
cc_summary_data_frame <- cc_summary_data_frame%>%
  select(Decade_of_degree,
         N_Associates, 
         n_Associates, 
         percentage_associates, 
         N_Baccalaureate, 
         n_Baccalaureate, 
         percentage_baccalaureate, 
         N_Master_s, 
         n_Master_s, 
         percentage_masters, 
         N_Doctoral, 
         n_Doctoral, 
         percentage_doctoral)

# are recent graduates placed equally across carnegie classifications

recent_graduates <- data.frame(
  "Carnegie_classifcation"= c("Asssociates", 
                              "Baccalaureate", 
                              "Masters", 
                              "Doctoral"
                              ), 
  "n_faculty" = c(cc_summary_data_frame[4,"n_Associates"], 
                    cc_summary_data_frame[4,"n_Baccalaureate"], 
                    cc_summary_data_frame[4,"n_Master_s"], 
                    cc_summary_data_frame[4,"n_Doctoral"]
                  ),
  "N_faculty" = c(cc_summary_data_frame[4,"N_Associates"], 
                   cc_summary_data_frame[4,"N_Baccalaureate"], 
                   cc_summary_data_frame[4,"N_Master_s"], 
                   cc_summary_data_frame[4,"N_Doctoral"]
                   ),
  stringsAsFactors = FALSE
  )

# proportion test chi value

prop_test_2010faculty_by_carnegie <- prop.test(recent_graduates$n_faculty, recent_graduates$N_faculty)$p.value

#add percentage to recent graduate
recent_graduates <- recent_graduates%>%
  mutate(percentage= round((n_faculty/N_faculty)*100,digits = 1))



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
