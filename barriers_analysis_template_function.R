require(ggplot2)
require(tidyverse)
require(reshape2)
require(corrplot)

#read in cleaned dataframe "decoded_df.csv"
df <- read_csv("./data_cleaning_scripts/04_decode_survey_responses/output/decoded_df.csv")

#remove respondants not in US/Puerto Rico
remove.non.us.repondants <- function(df){
  countries <- c("United States","Puerto Rico")
  df <- df%>%
    filter(Country_Country %in% countries )
  return(df)
}

df <- remove.non.us.repondants(df)

######### REMOVE NON RESPONDANTS ########################
# We will only analyze data that match the category.levels you have selected
# assuming you removed NA's or 'Don't knows, we will only analyze data for 
# respondants for which we have a response to the stratfying category

remove.non.repondants <- function(df){
  responses <- c("United States","Puerto Rico")
  df <- df%>%
    filter_(lazyeval::interp(quote(x %in% y), x=as.name(category.column.name), y=category.levels))
  return(df)
}


############# SET MANUAL VARIABLES  #############################

# Question to examine

# Set the variable (Question) that will be analyzed ("COLUMN_NAME)
# enter the subsetting variable for this column (e.g df$COLUMN_NAME)
# Set a 'nice name' to describe this question
# Set a 'safe name' for naming variables
question.column.name <- "Q38_What.is.preventing.yOu.frOm.including.biOinfOrmatics.cOntent.in.these.cOurses."
question.column.name.nice <- "Barriers to Including Bioinformatics (Q38)"
question.column.name.safe <- "Q38_barriers_to_inclusion"

# Category to stratify by

# Set the variable (Question) that will be analyzed ("COLUMN_NAME)
# Set the subsetting variable for this column (e.g df$COLUMN_NAME)
# Set a 'nice name' to describe this category
# Set a 'safe name' for naming variables
category.column.name <- "Q21_What.is.the.Carnegie.classification.of.your.institution."
category.column.name.nice <- "Carnegie Classification (Q21)"
category.column.name.safe <- "Q21_carnegie_classification"


#Get the levels (possible answers) within that catagories

category.levels <- levels(as.factor(category.column.subset))

#Set levels to retain ( excluding for example responses such as 'Don't Know or 'NA')
# select the range of values within catagory.levels to use (e.g. category.levels[1:4])
category.levels <- category.levels[1:4]
print("Category levels used:")
print(category.levels)

# execute function to remove non-respondants
df <- remove.non.repondants(df)

# Set the question and category subsets
question.column.subset <- df$Q38_What.is.preventing.yOu.frOm.including.biOinfOrmatics.cOntent.in.these.cOurses.
category.column.subset <-  df$Q21_What.is.the.Carnegie.classification.of.your.institution.


# Create a dataframe of your selected catagories and assign them easy-to-interprit names

#Nice names data frame - create a datframe of nice names for your categories
category.df <- data.frame ("Associates"= category.levels[1], 
                           "Baccalaureate" = category.levels[2] , 
                           "Masters" = category.levels[3] , 
                           "Doctoral" = category.levels[4], 
                           stringsAsFactors = FALSE)

# The first row of category.df contains the original value of the category level, rename row
rownames(category.df)[1] <- "category_key"

#Create a blank row that will will come in handy later for storing other values
category.df.blank <- category.df
category.df.blank[2,] <- NA

######### STRATIFYING CATEGORY SUBSETS  #################

# Question 38


category.raw.scored.columns <- c("Q38_Faculty.Issue...No.Expertise..Training", 
                          "Q38_Faculty.Issue...Time", 
                          "Q38_Faculty.Issue...Does.not.know.how.to.design.curricula.or.incorporate.with.existing.curriculum", 
                          "Q38_Faculty.Issue...Lack.of.Faculty.interest.at.Institution", 
                          "Q38_Faculty.Issues...Faculty.is.new.to.current.Dept", 
                          "Q38_Curriculum.Issue...Course.Load.Full..No.time.space.for.Content", 
                          "Q38_Curric.Issues...Does.not.Fit.into.current.Current.Course.Structure", 
                          "Q38_Curriculum.Issue...Time.for.Curriculum.Development", 
                          "Q38_Curric.Issues...Lack.of.Curric.Control.not.in.curent.Curric.", 
                          "Q38_Curric.Issues...Bioinf..Taught.in.other.courses.at.institution", 
                          "Q38_Curric.Issue...Class.Size", 
                          "Q38_Curric.Issues...Plans.to.teach.Bioinf..In.the.future..but.not.currently.available.", 
                          "Q38_Curric.Issue...Bioinfo.Conent.too.Massive", 
                          "Q38_Curric.Issues...Content.needs.to.be.introduced.in.multiple.courses",
                          "Q38_Resources...Access.to.Quality.Exercises..Content", 
                          "Q38_Resources...Access.to.developed.Bioinf.Lesson.Plans.Bioinf.Curric",
                          "Q38_Resources...Access.to.Approp.Introductory.Content", 
                          "Q38_Resources...Unable.to.identify.access.best.current.Bioinf.material", 
                          "Q38_Resource.Issues...Funding", 
                          "Q38_Resource.Issues...Not.available.in.course.textbook", 
                          "Q38_Resource.Issues...Access.to.Quality.Online.Exerices.Conent", 
                          "Q38_Resource.Issues..TA.s.lack.approp.skils",
                          "Q38_Student.Issues...UG.Students.Lack.Approp.Background.Knowledge", 
                          "Q38_Student.Issues...Lack.of.interest.in.topic",
                          "Q38_Facilities.Issue..Access.to.Appropriate.Facilities..Equipment",
                          "Q38_Inst.Dept.Issues...Institutional.Inertia",  
                          "Q38_State.restrictions", 
                          "Q38_Not.accredited", 
                          category.column.name)

category.raw.scored.df <- df%>%
  select(one_of(category.raw.scored.columns))

category.summed.columns <- c("q38_Faculty_issues_sum", 
                             "q38_Curriculum_issues_sum", 
                             "q38_Resources_issues_sum", 
                             "q38_Student_issues_sum", 
                             "q38_Facilities_issues_sum", 
                             "q38_Institutional_issues_sum", 
                             "q38_State_issues_sum", 
                             "q38_Accredited_issues_sum",
                             category.column.name)

category.summed.df<- df%>%
  select(one_of(category.summed.columns))

category.reduced.columns <- c("q38_Faculty_issues_reduced", 
                             "q38_Curriculum_issues_reduced", 
                             "q38_Resources_issues_reduced", 
                             "q38_Student_issues_reduced", 
                             "q38_Facilities_issues_reduced", 
                             "q38_Institutional_issues_reduced", 
                             "q38_State_issues_reduced", 
                             "q38_Accredited_issues_reduced",
                             category.column.name)

category.reduced.df<- df%>%
  select(one_of(category.reduced.columns))




######### CREATE DIRECTORIES ############################

#Create directory for table outputs

table.dir.path <- paste("./analysis_of",
                        question.column.name.safe, 
                        "by",
                        category.column.name.safe,
                        "/output_tables/",
                        sep = "_"
                        )
plot.dir.path <- paste("./analysis_of",
                      question.column.name.safe, 
                      "by",
                      category.column.name.safe,
                      "/output_plots/",
                      sep = "_"
                      )

#Create folders and start a ReadMe for this analysis
dir.create(table.dir.path, recursive = TRUE)
dir.create(plot.dir.path, recursive = TRUE)







######### SUMMARY STATISTICS ############################

# 1. Calculate number of responses for each possible response

# Craate a df to hold the response counts
response.counts.by.category <- category.df.blank
rownames(response.counts.by.category)[2] <- "response_count"


# use dplyr filter to find all rows in the matser df that match your chosen category levels
# which in our df are the row 1 category keys. 

for (category in colnames(response.counts.by.category)) {
  response.counts.by.category["response_count",category] <- df%>%
    # filter pipe below will return counts for all respondants that match the category
    filter(category.column.subset == response.counts.by.category["category_key",category])%>%
    count()
}

# 2. Calculate "overall n" for the stratifying category - these are the number of respondants 
# who answered to your chosen category, and who will be analyzed 

n.respondants <- sum(as.numeric(response.counts.by.category["response_count",]))

# 3. Plot summary statistics

# reshape the data for plotting and convert value to numeric types
summary.response.df <- response.counts.by.category["response_count",]
summary.response.df <- melt(as.matrix(summary.response.df))
summary.response.df[,"value"] <- as.numeric(as.character(summary.response.df$value))

# ggplot barplot 
summary.response.df%>%
  ggplot()+
  aes(x=Var2, y=value)+
  geom_bar(stat="identity", position = "dodge")+
  ylab("Individual Responses")+
  xlab(category.column.name.nice)+
  ggtitle(paste(" Count of Respondants\n",
                question.column.name.nice,
                "by",
                category.column.name.nice, 
                "\n n=",
                n.respondants))+
  theme_minimal()

summary.response.plotname <- paste("count_of_respondants",
                                   question.column.name.safe,
                                   "by",
                                   category.column.name.safe,
                                   ".png",
                                   sep = "_")

ggsave(paste(plot.dir.path,summary.response.plotname, sep= ""))


