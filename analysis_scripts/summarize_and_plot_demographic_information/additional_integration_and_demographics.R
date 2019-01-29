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




# count of faculty at MSI and non MSI institutions

count.msi <- data.df%>%
  filter(Q22_Is.your.institution.classified.as.minority.serving. == "1_Yes")%>%
  nrow()

count.nonmsi <- data.df%>%
  filter(Q22_Is.your.institution.classified.as.minority.serving. == "2_No")%>%
  nrow()

denom.msi_status <- count.nonmsi+count.msi



# How are faculty at MSI distributed across Carnegie Classifications

# caculate N from total group of respondents for both questions
N.asso <- data.df%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  filter(Q22_Is.your.institution.classified.as.minority.serving. !=  "3_Don't know")%>%
  nrow()

N.bacc <- data.df%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College")%>%
  filter(Q22_Is.your.institution.classified.as.minority.serving. !=  "3_Don't know")%>%
  nrow()

N.mast <- data.df%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)")%>%
  filter(Q22_Is.your.institution.classified.as.minority.serving. !=  "3_Don't know")%>%
  nrow()

N.doct <- data.df%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)")%>%
  filter(Q22_Is.your.institution.classified.as.minority.serving. !=  "3_Don't know")%>%
  nrow()


# Sum total N and get denominators
denom.carnegie <- N.asso + N.bacc + N.mast + N.doct
  
msifaculty.asso <- data.df%>%
  filter(Q22_Is.your.institution.classified.as.minority.serving. == "1_Yes")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  nrow()
msifaculty.bacca <- data.df%>%
  filter(Q22_Is.your.institution.classified.as.minority.serving. == "1_Yes")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College")%>%
  nrow()

msifaculty.mast <- data.df%>%
  filter(Q22_Is.your.institution.classified.as.minority.serving. == "1_Yes")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)")%>%
  nrow()

msifaculty.doct <- data.df%>%
  filter(Q22_Is.your.institution.classified.as.minority.serving. == "1_Yes")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)")%>%
  nrow()


# calculate percentages

msi.asso.percentage <- msifaculty.asso/denom.carnegie
msi.bacc.percentage <- msifaculty.bacca/denom.carnegie
msi.mast.percentage <- msifaculty.mast/denom.carnegie
msi.doct.percentage <- msifaculty.doct/denom.carnegie

# significance test

#Create dataframe for test

msi_testing_table <- data.frame("carnegie_classfication" = c("Associates", 
                                                             "Baccalaureate", 
                                                             "Master's",
                                                             "Doctoral"), 
                                "N_carnegie"= c(N.asso, 
                                                N.bacc,
                                                N.mast, 
                                                N.doct), 
                                "n_msi_faculty" = c(msifaculty.asso, 
                                                    msifaculty.bacca, 
                                                    msifaculty.mast, 
                                                    msifaculty.doct), 
                                stringsAsFactors = FALSE
                                )

msi_by_carnegie.proptest <- prop.test(msi_testing_table$n_msi_faculty, 
                                      msi_testing_table$N_carnegie)$p.value


msicalculation.n <-  denom.carnegie

# Calculate MSI percentages at institution degree types out of 100% of MSI respondents
  
#Denominator (e.g. answered both the MSI and one of the category questions)

N.asso <- data.df%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  filter(Q22_Is.your.institution.classified.as.minority.serving. !=  "3_Don't know")%>%
  nrow()

N.bacc <- data.df%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College")%>%
  filter(Q22_Is.your.institution.classified.as.minority.serving. !=  "3_Don't know")%>%
  nrow()

N.mast <- data.df%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)")%>%
  filter(Q22_Is.your.institution.classified.as.minority.serving. !=  "3_Don't know")%>%
  nrow()

N.doct <- data.df%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)")%>%
  filter(Q22_Is.your.institution.classified.as.minority.serving. !=  "3_Don't know")%>%
  nrow()



# Get number of  affirmative MSI at each insitution type

N.asso.msi <- data.df%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  filter(Q22_Is.your.institution.classified.as.minority.serving. ==  "1_Yes")%>%
  nrow()

N.bacc.msi <- data.df%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College")%>%
  filter(Q22_Is.your.institution.classified.as.minority.serving. ==  "1_Yes")%>%
  nrow()

N.mast.msi <- data.df%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)")%>%
  filter(Q22_Is.your.institution.classified.as.minority.serving. ==  "1_Yes")%>%
  nrow()

N.doct.msi <- data.df%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)")%>%
  filter(Q22_Is.your.institution.classified.as.minority.serving. ==  "1_Yes")%>%
  nrow()


onehundredpercent.msi.denominator <- N.asso.msi + N.bacc.msi + N.mast.msi + N.doct.msi


# calculate percentages (of faculty at MSI's what percent per institution type)

hundred.msi.asso.percentage <- N.asso.msi/onehundredpercent.msi.denominator
hundred.msi.bacc.percentage <- N.bacc.msi/onehundredpercent.msi.denominator
hundred.msi.mast.percentage <- N.mast.msi/onehundredpercent.msi.denominator
hundred.msi.doct.percentage <- N.doct.msi/onehundredpercent.msi.denominator





# level of integration at MSIs


count.notintegrating <- data.df%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "3_Do NOT currently, but would like to include 'substantial' bioinformatics in courses for life-science majors")%>%
  filter(Q22_Is.your.institution.classified.as.minority.serving. == "1_Yes" |
           Q22_Is.your.institution.classified.as.minority.serving. == "2_No" )%>%
  nrow()

count.integrating<- data.df%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "2_Include 'substantial' bioinformatics in courses for life-science majors")%>%
  filter(Q22_Is.your.institution.classified.as.minority.serving. == "1_Yes" |
           Q22_Is.your.institution.classified.as.minority.serving. == "2_No" )%>%
  nrow()

count.dedicatedcourse<- data.df%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "1_Dedicated course for life-science majors")%>%
  filter(Q22_Is.your.institution.classified.as.minority.serving. == "1_Yes" |
           Q22_Is.your.institution.classified.as.minority.serving. == "2_No" )%>%
  nrow()


denom.integration <-  count.notintegrating +  count.integrating + count.dedicatedcourse


# count of integrators in MSI vs Non-MSI context

count.msi.integrators <- data.df%>%
  filter(Q22_Is.your.institution.classified.as.minority.serving. == "1_Yes")%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "1_Dedicated course for life-science majors" |
           Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "2_Include 'substantial' bioinformatics in courses for life-science majors" )%>%
  nrow()

count.nonmsi.inetgrators <- data.df%>%
  filter(Q22_Is.your.institution.classified.as.minority.serving. == "2_No")%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "1_Dedicated course for life-science majors" |
           Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "2_Include 'substantial' bioinformatics in courses for life-science majors" )%>%
  nrow()


percent.msi.integrating <-  count.msi.integrators/denom.integration
percent.nonmsi.integrators <- count.nonmsi.inetgrators/denom.integration




# How does integration work at various training levels. 

train.count.notintegrating <- data.training%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "3_Do NOT currently, but would like to include 'substantial' bioinformatics in courses for life-science majors")%>%
  filter(!is.na(faculty_preperation))%>%
  nrow()

train.count.integrating <- data.training%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "2_Include 'substantial' bioinformatics in courses for life-science majors")%>%
  filter(!is.na(faculty_preperation))%>%
  nrow()
  
train.count.dedicated <- data.training%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "1_Dedicated course for life-science majors")%>%
  filter(!is.na(faculty_preperation))%>%
  nrow()

train.count.denom <- train.count.dedicated + train.count.integrating + train.count.notintegrating


# get integration counts for training types


no.training.count <- data.training%>%
  filter(faculty_preperation == "No Training")%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "1_Dedicated course for life-science majors" |
           Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "2_Include 'substantial' bioinformatics in courses for life-science majors" )%>%
  nrow()
  
selftaught.count<- data.training%>%
  filter(faculty_preperation == "Self Taught")%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "1_Dedicated course for life-science majors" |
           Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "2_Include 'substantial' bioinformatics in courses for life-science majors" )%>%
  nrow()

workshops.count<- data.training%>%
  filter(faculty_preperation == "Workshops and Bootcamps")%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "1_Dedicated course for life-science majors" |
           Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "2_Include 'substantial' bioinformatics in courses for life-science majors" )%>%
  nrow()

formal.count<- data.training%>%
  filter(faculty_preperation == "Formal Training")%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "1_Dedicated course for life-science majors" |
           Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "2_Include 'substantial' bioinformatics in courses for life-science majors" )%>%
  nrow()



# create table counting data frame

integration_by_training <- data.frame("training"=c("No Training", 
                                                   "Self Taught", 
                                                   "Workshops and Bootcamps", 
                                                   "Formal Training" 
                                                   ), 
                                      "percentage_integrators"= c(
                                        (no.training.count/train.count.denom)*100, 
                                        (selftaught.count/train.count.denom)*100, 
                                        (workshops.count/train.count.denom)*100, 
                                        (formal.count/train.count.denom)*100
                                      ), 
                                      stringsAsFactors = FALSE)



#examine male/female differences across carnegie classification, training levels, and degree years


denom.sex.count.integrating <- data.df%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "2_Include 'substantial' bioinformatics in courses for life-science majors")%>%
  filter(Q14_Sex == "1_Female" |
           Q14_Sex == "2_Male")%>%
  nrow()
denom.sex.count.notintegrating<- data.df%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "3_Do NOT currently, but would like to include 'substantial' bioinformatics in courses for life-science majors")%>%
  filter(Q14_Sex == "1_Female" |
           Q14_Sex == "2_Male")%>%
  nrow()
denom.sex.count.dedicated<- data.df%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "1_Dedicated course for life-science majors")%>%
  filter(Q14_Sex == "1_Female" |
           Q14_Sex == "2_Male")%>%
  nrow()
denom.sex.count.asso<- data.df%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  filter(Q14_Sex == "1_Female" |
           Q14_Sex == "2_Male")%>%
  nrow()
denom.sex.count.bacc<- data.df%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College")%>%
  filter(Q14_Sex == "1_Female" |
           Q14_Sex == "2_Male")%>%
  nrow()
denom.sex.count.mast<- data.df%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)" )%>%
  filter(Q14_Sex == "1_Female" |
           Q14_Sex == "2_Male")%>%
  nrow()
denom.sex.count.doct<- data.df%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)")%>%
  filter(Q14_Sex == "1_Female" |
           Q14_Sex == "2_Male")%>%
  nrow()
denom.sex.count.1980 <- data.df%>%
  filter(bin_degree_yrs == "1980-1989")%>%
  filter(Q14_Sex == "1_Female" |
           Q14_Sex == "2_Male")%>%
  nrow()
denom.sex.count.1990<- data.df%>%
  filter(bin_degree_yrs == "1990-1999")%>%
  filter(Q14_Sex == "1_Female" |
           Q14_Sex == "2_Male")%>%
  nrow()
denom.sex.count.2000<- data.df%>%
  filter(bin_degree_yrs == "2000-2009")%>%
  filter(Q14_Sex == "1_Female" |
           Q14_Sex == "2_Male")%>%
  nrow()
denom.sex.count.2010<- data.df%>%
  filter(bin_degree_yrs == "2010-2016")%>%
  filter(Q14_Sex == "1_Female" |
           Q14_Sex == "2_Male")%>%
  nrow()
denom.sex.count.notraining <- data.training%>%
  filter(faculty_preperation == "No Training")%>%
  filter(Q14_Sex == "1_Female" |
           Q14_Sex == "2_Male")%>%
  nrow()
denom.sex.count.selftaught<- data.training%>%
  filter(faculty_preperation == "Self Taught")%>%
  filter(Q14_Sex == "1_Female" |
           Q14_Sex == "2_Male")%>%
  nrow()

denom.sex.count.workshops<- data.training%>%
  filter(faculty_preperation == "Workshops and Bootcamps")%>%
  filter(Q14_Sex == "1_Female" |
           Q14_Sex == "2_Male")%>%
  nrow()

denom.sex.count.formal<- data.training%>%
  filter(faculty_preperation == "Formal Training")%>%
  filter(Q14_Sex == "1_Female" |
           Q14_Sex == "2_Male")%>%
  nrow()


#calculate male/female percentages for each demographic category


#Level of integration 

male.integration <- data.df%>%
  filter(Q14_Sex == "2_Male")%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "1_Dedicated course for life-science majors" |
           Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "2_Include 'substantial' bioinformatics in courses for life-science majors" )%>%
  nrow()
  
female.integration <- data.df%>%
  filter(Q14_Sex == "1_Female")%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "1_Dedicated course for life-science majors" |
           Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "2_Include 'substantial' bioinformatics in courses for life-science majors" )%>%
  nrow()

#integration stats by sex

sex.integration.denom <- denom.sex.count.integrating + denom.sex.count.dedicated + denom.sex.count.notintegrating
male.int.percent <- male.integration/sex.integration.denom
female.int.percent <- female.integration/sex.integration.denom


#male female distribution at different carnegie classified institutuions. 

carnegie.denom <- denom.sex.count.asso + denom.sex.count.bacc + denom.sex.count.mast + denom.sex.count.doct


male.asso<- data.df%>%
  filter(Q14_Sex == "2_Male")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  nrow()
female.asso<- data.df%>%
  filter(Q14_Sex == "1_Female")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "1_Associate's College")%>%
  nrow()
male.bacc<- data.df%>%
  filter(Q14_Sex == "2_Male")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College")%>%
  nrow()
female.bacc<- data.df%>%
  filter(Q14_Sex == "1_Female")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "2_Baccalaureate College")%>%
  nrow()
male.mast<- data.df%>%
  filter(Q14_Sex == "2_Male")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)")%>%
  nrow()
female.mast<- data.df%>%
  filter(Q14_Sex == "1_Female")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "3_Master's (Small, Medium, Large)")%>%
  nrow()
male.doct<- data.df%>%
  filter(Q14_Sex == "2_Male")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)")%>%
  nrow()
female.doct<- data.df%>%
  filter(Q14_Sex == "1_Female")%>%
  filter(Q21_What.is.the.Carnegie.classification.of.your.institution. == "4_Doctoral University (High, Higher, Highest Research Activity)")%>%
  nrow()




percent.male.asso <- male.asso/carnegie.denom
percent.female.asso <- female.asso/carnegie.denom
percent.male.bacc <- male.bacc/carnegie.denom
percent.female.bacc <- female.bacc/carnegie.denom
percent.male.mast <- male.mast/carnegie.denom
percent.female.mast <- female.mast/carnegie.denom
percent.male.doct <- male.doct/carnegie.denom
percent.female.doct <- female.doc/carnegie.denom





# calculate percents for degree year

degree.denom <-  denom.sex.count.1980+denom.sex.count.1990+denom.sex.count.2000+denom.sex.count.2010

percent.male.1980<- data.df%>%
  filter(bin_degree_yrs == "1980-1989")%>%
  filter(Q14_Sex == "2_Male")%>%
  nrow()
percent.female.1980<- data.df%>%
  filter(bin_degree_yrs == "1980-1989")%>%
  filter(Q14_Sex == "1_Female")%>%
  nrow()
percent.male.1990<- data.df%>%
  filter(bin_degree_yrs == "1990-1999")%>%
  filter(Q14_Sex == "2_Male")%>%
  nrow()
percent.female.1990<- data.df%>%
  filter(bin_degree_yrs == "1990-1999")%>%
  filter(Q14_Sex == "1_Female")%>%
  nrow()
percent.male.2000<- data.df%>%
  filter(bin_degree_yrs == "2000-2009")%>%
  filter(Q14_Sex == "2_Male")%>%
  nrow()
percent.female.2000<- data.df%>%
  filter(bin_degree_yrs == "2000-2009")%>%
  filter(Q14_Sex == "1_Female")%>%
  nrow()
percent.male.2010<- data.df%>%
  filter(bin_degree_yrs == "2010-2016")%>%
  filter(Q14_Sex == "2_Male")%>%
  nrow()
percent.female.2010<- data.df%>%
  filter(bin_degree_yrs == "2010-2016")%>%
  filter(Q14_Sex == "1_Female")%>%
  nrow()


percentage.male.1980 <- percent.male.1980/degree.denom
percentage.female.1980 <- percent.female.1980/degree.denom
percentage.male.1990 <- percent.male.1990/degree.denom
percentage.female.1990 <- percent.female.1990/degree.denom
percentage.male.2000 <- percent.male.2000/degree.denom
percentage.female.2000 <- percent.female.2000/degree.denom
percentage.male.2010 <- percent.male.2010/degree.denom
percentage.female.2010 <- percent.female.2010/degree.denom


# across training types


training.denom <- denom.sex.count.notraining+denom.sex.count.selftaught+denom.sex.count.workshops+denom.sex.count.formal


percent.male.notraining<- data.training%>%
  filter(faculty_preperation == "No Training")%>%
  filter(Q14_Sex == "2_Male")%>%
  nrow()
percent.female.notraining<- data.training%>%
  filter(faculty_preperation == "No Training")%>%
  filter(Q14_Sex == "1_Female")%>%
  nrow()
percent.male.self<- data.training%>%
  filter(faculty_preperation ==  "Self Taught")%>%
  filter(Q14_Sex == "2_Male")%>%
  nrow()
percent.female.self<- data.training%>%
  filter(faculty_preperation ==  "Self Taught")%>%
  filter(Q14_Sex == "1_Female")%>%
  nrow()
percent.male.workshop<- data.training%>%
  filter(faculty_preperation == "Workshops and Bootcamps")%>%
  filter(Q14_Sex == "2_Male")%>%
  nrow()
percent.female.workshop<- data.training%>%
  filter(faculty_preperation == "Workshops and Bootcamps")%>%
  filter(Q14_Sex == "1_Female")%>%
  nrow()
percent.male.formal<- data.training%>%
  filter(faculty_preperation == "Formal Training")%>%
  filter(Q14_Sex == "2_Male")%>%
  nrow()
percent.female.formal<- data.training%>%
  filter(faculty_preperation == "Formal Training")%>%
  filter(Q14_Sex == "1_Female")%>%
  nrow()



percentage.male.notrain <- percent.male.notraining/training.denom
percentage.female.notrain<- percent.female.notraining/training.denom
percentage.male.self<- percent.male.self/training.denom
percentage.female.self<- percent.female.self/training.denom
percentage.male.work<- percent.male.workshop/training.denom
percentage.female.work<- percent.female.workshop/training.denom
percentage.male.formal<- percent.male.formal/training.denom
percentage.female.formal<- percent.female.formal/training.denom
