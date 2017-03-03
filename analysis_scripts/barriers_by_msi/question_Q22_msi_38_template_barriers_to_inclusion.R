# load required libraries
require(ggplot2)
require(tidyverse)
require(reshape2)
require(pwr)
require(gplots)
require(corrplot)

############ LOAD THE PREPARED SURVEY DATA ###########################################
#read in cleaned dataframe "decoded_df.csv"
master.df <- read_csv("../../data_cleaning_scripts/04_decode_survey_responses/output/decoded_df.csv")

#filter the data frame - Q38 was only shown to users who answered 
#"3_Do NOT currently, but would like to include 'substantial' bioinformatics in courses for life-science majors" to Q1 

master.df <- master.df%>%
  filter(Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn... == "3_Do NOT currently, but would like to include 'substantial' bioinformatics in courses for life-science majors")


############# MANUAL!!!! SET 1st SET OF MANUAL VARIABLES  #############################

# set the variable (Question) that will be analyzed: "COLUMN_NAME"
category.column.name <- "Q22_Is.your.institution.classified.as.minority.serving."

# set a 'nice' (e.g. human readable) name to describe this category: "Category (Q#)"
category.column.name.nice <- "Minority-serving Institution (Q22)"

# set a 'safe name' for naming variables: "Q#_category_category"
category.column.name.safe <- "Q22_msi"

# set a 'short' for naming filename variables: "Q#_category"
category.column.name.short <- "Q22_msi"

#set a nice name in upper and lower case that describes the category kinds 
#(e.g. gender, institution type): ""

category.nice.name.caps <- "Institution Type"
category.nice.name.lower <- "institution type"


############# GET CATEGORIES TO ANALYZE  ##############################################
#All questions are analyzed by a stratafying category (e.g. gender)

#subset the category column
category.column.subset <-  master.df[[category.column.name]]
#Get the levels (possible answers) within that catagories
category.levels <- levels(as.factor(category.column.subset))

############# MANUAL!!!! SET 2nd SET OF MANUAL VARIABLES  #############################

#Set levels to retain ( excluding for example responses such as 'Don't Know or 'NA')
# select the range of values within catagory.levels to use (e.g. category.levels[1:4])
category.levels <- category.levels[1:2]

#######################################################################################

# Reset the category subset
category.column.subset <-  master.df[[category.column.name]]



############ TEMPLATE VARIABLES    ############################
# These variables should not change and are specific to a template that analyzes a particular question
# Q38 Variables

#Names
# variable (Question) that will be analyzed ("COLUMN_NAME)
question.column.name <- "Q38_What.is.preventing.yOu.frOm.including.biOinfOrmatics.cOntent.in.these.cOurses."
# subsetting variable for this column (e.g master.df$COLUMN_NAME)
question.column.name.nice <- "Barriers to Including Bioinformatics (Q38)"
# 'nice name' to describe this question
question.column.name.safe <- "Q38_barriers_to_inclusion"
# 'short name' to describe this question
question.column.name.short <- "Q38_inclusion"
# 'safe name' for naming variables
question.column.subset <- master.df[[question.column.name]]


########## Cleaning Steps ###################################################

#remove respondents not in US/Puerto Rico
remove.non.us.repondants <- function(df){
  countries <- c("United States","Puerto Rico")
  df <- df%>%
    filter(Country_Country %in% countries )
  return(df)
}

master.df.us.only <- remove.non.us.repondants(master.df)

# We will only analyze data that match the category.levels you have selected
# assuming you removed NA's or 'Don't knows, we will only analyze data for 
# respondents for which we have a response to the stratfying category

remove.non.relavant.repondants <- function(df){
  responses <- c("United States","Puerto Rico")
  df <- df%>%
    filter_(lazyeval::interp(quote(x %in% y), x=as.name(category.column.name), y=category.levels))
  return(df)
}

relavant.respondents.df <- remove.non.relavant.repondants(master.df.us.only)

#RESET SUBSET VALUES
category.column.subset <- relavant.respondents.df[[category.column.name]]
question.column.subset <- relavant.respondents.df[[question.column.name]]


############# column selection #################################################
#select columns that will be used in this analysis and create sets of nice names for those columns

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

category.raw.scored.df <- relavant.respondents.df%>%
  select(one_of(category.raw.scored.columns))

# Create a set of nice names for these columns

category.raw.scored.columns.nice.names <- c("Faculty Issues: Expertise/training", 
                                            "Faculty Issues: Time",
                                            "Faculty Issues: Curriculum design/integration",
                                            "Faculty Issues: Lack of interest",
                                            "Faculty Issues: New Faculty",
                                            "Curriculum Issues: No space",
                                            "Curriculum Issues: Incompatible with current curriculum",
                                            "Curriculum Issues: Time needed to develop",
                                            "Curriculum Issues: No control",
                                            "Curriculum Issues: Covered elsewhere",
                                            "Curriculum Issues: Class size",
                                            "Curriculum Issues: Plan for future coverage",
                                            "Curriculum Issues: Too much content",
                                            "Curriculum Issues: Content requires several courses",
                                            "Resource Issues: Access to exercises",
                                            "Resource Issues: Access to lesson plans",
                                            "Resource Issues: Access to intro content",
                                            "Resource Issues: Unable to vet content",
                                            "Resource Issues: Funding",
                                            "Resource Issues: No appropriate textbook",
                                            "Resource Issues: No access to online exercises",
                                            "Resource Issues: No qualified TAs",
                                            "Student Issues: Background knowledge",
                                            "Student Issues: Interest in topic",
                                            "Facilities: Access to equipment",
                                            "Institutional: Inertia",
                                            "State Restrictions", 
                                            "Accreditation")

# Select the reduced columns (columns where coded responses have been summarized into
# binary values (1 = at least one faculty issue reported , 0 = no faculty issues reported

category.reduced.columns <- c("q38_Faculty_issues_reduced", 
                              "q38_Curriculum_issues_reduced", 
                              "q38_Resources_issues_reduced", 
                              "q38_Student_issues_reduced", 
                              "q38_Facilities_issues_reduced", 
                              "q38_Institutional_issues_reduced", 
                              "q38_State_issues_reduced", 
                              "q38_Accredited_issues_reduced",
                              category.column.name)

category.reduced.df<- relavant.respondents.df%>%
  select(one_of(category.reduced.columns))

# create a set of nice names

category.reduced.columns.nice.names <- c("Faculty Issues",
                                        "Curriculum Issues", 
                                        "Resource Issues", 
                                        "Student Issues",
                                        "Facilities Issues", 
                                        "Institutional Issues",
                                        "State Issues", 
                                        "Accredidation Issues")

######### CREATE DIRECTORIES #########################################################

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



############ MANUAL!!!!  CREATE A SET OF NICE NAMES FOR PLOTTING #######################
# Create a dataframe of your selected catagories and assign them easy-to-interprit names

#Nice names data frame - create a datframe of nice names for your categories, you must
#complete the df for all of the categories
#example:
#category.df <- data.frame ("Associates"= category.levels[1], 
#                           "Baccalaureate" = category.levels[2] , 
#                           "Masters" = category.levels[3] , 
#                           "Doctoral" = category.levels[4], 
#                           stringsAsFactors = FALSE)
# In the nice names...
# Use underscores which will be replace by spaces
# Use X to be replaced in plots by ","
# Use K for lines starting with numbers to be replaced by ""
# Use D to be replaced by "-"
# All lines where these subsitutions are done have a comment "SUBSTITUTION" 

category.df <- data.frame ("MinorityDServing"= category.levels[1],
                           "NotDMinorityDServing"= category.levels[2],  
                           stringsAsFactors = FALSE)

#Set an ordering for plotting - must match category.levels names
col.order <- c("1_Yes", "2_No")

#With substitutions - must match category.df
col.order2 <- c("MinorityDServing",
				"NotDMinorityDServing")

#Nice Labels for plotting
nice.lables.list <- c("Minority-Serving",
				"Not Minority-Serving") 

######### DATA FRAME FORMATTING AND CLEANING STEPS  ###################################

#Create and format nice name dataframe
nice.names.df <- data.frame(t(category.df))
nice.names.df$rownames <- rownames(nice.names.df)
rownames(nice.names.df) <- nice.names.df$t.category.df.
nice.names.df$rownames <- as.character(nice.names.df$rownames)
nice.category.names <- nice.names.df$rownames

# The first row of category.df contains the original value of the category level, rename row
rownames(category.df)[1] <- "category_key"

#Create a blank row that will will come in handy later for storing other values
category.df.blank <- category.df
category.df.blank[2,] <- NA

########## ANALYSIS FUNCTIONS ########################################################
# The remainder of this script should not change based on the question or category

### count responses
# Determine how many respondents identifed in the accepted stratafying categories 
# (e.g. how many identifed as male or female)


num.cat.respondents <- function(df,
                                category.df.blank, 
                                category.column.subset,
                                table.dir.path,
                                question.column.name.safe){
  
  # Craate a df to hold the response counts
  response.counts.by.category <- category.df.blank
  rownames(response.counts.by.category)[2] <- "response_count"
  
  #use dplyr filter to find all rows in the matser df that match your chosen category levels
  # which in our df are the row 1 category keys. 
  
  for (category in colnames(response.counts.by.category)) {
    response.counts.by.category["response_count",category] <- df%>%
      # filter pipe below will return counts for all respondents that match the category
      filter(category.column.subset == response.counts.by.category["category_key",category])%>%
      count()
  }
  
  response.counts.by.category.filename <- paste(table.dir.path,
                                                "count_of_responses_to_", 
                                                question.column.name.short, 
                                                ".csv", 
                                                sep = "")
  write.csv(response.counts.by.category, file = response.counts.by.category.filename)
  
  # 2. Calculate "overall n" for the stratifying category - these are the number of respondents 
  # who answered to your chosen category, and who will be analyzed 
  
  n.respondents <- sum(as.numeric(response.counts.by.category["response_count",]))
  
  return(list(n.respondents = n.respondents,
              response.counts.by.category = response.counts.by.category))
}


# calculate number of respondents who identified within a stratfying category and write to file
n.respondents.object <- num.cat.respondents(category.raw.scored.df,
                                            category.df.blank, 
                                            category.column.subset,
                                            table.dir.path,
                                            question.column.name.safe)

n.respondents <- as.numeric(n.respondents.object[1])
response.counts.by.category <-as.data.frame(n.respondents.object[2])

# fix response.counts.by.category column names and rebuild in correct order
colnames(response.counts.by.category) <- response.counts.by.category["category_key",]
response.counts.by.category <- response.counts.by.category%>%
  select(one_of(col.order))

##### PLOTTING SUMMMARY STATISTICS FUNCTION ############


plot.summary.statistics <- function(df,
                                    nice.names.df,
                                    category.column.name.nice,
                                    question.column.name.nice,
                                    question.column.name.safe,
                                    category.column.name.safe){
  
  
  # reshape the data for plotting and convert value to numeric types
  summary.response.df <- response.counts.by.category["response_count",]
  summary.response.df <- melt(as.matrix(summary.response.df))
  summary.response.df[,"value"] <- as.numeric(as.character(summary.response.df$value))
  
  # ggplot barplot 
  
  summary.response.df%>%
    ggplot()+
    aes(x=Var2, y=value)+
    geom_bar(stat="identity", 
             position = "dodge")+
    ylab("Individual Responses")+
    xlab(category.column.name.nice)+
    ggtitle(paste(" Count of respondents\n",
                  question.column.name.nice,
                  "by",
                  category.column.name.nice, 
                  "\n n=",
                  n.respondents))+
    theme_minimal()+
    scale_x_discrete(labels= nice.lables.list)
  
  
  summary.response.plotname <- paste("count_of_respondents",
                                     question.column.name.short,
                                     "by",
                                     category.column.name.short,
                                     ".png",
                                     sep = "_")
  
  ggsave(paste(plot.dir.path, summary.response.plotname, sep= ""), 
         width = 13.8, 
         height = 8.81, 
         units = "in")
  
}

#generate the summary statistic plot
plot.summary.statistics(response.counts.by.category,
                        nice.names.df,
                        category.column.name.nice,
                        question.column.name.nice,
                        question.column.name.safe,
                        category.column.name.safe)







######### RAW SCORE ANALYSIS #############################################################################

#Generate summary stats for raw scored sub-categories

raw.score.analysis <- function(df,
                               category.levels,
                               file.name.switch = 0, 
                               column.nice.names.f
){
  #prepare a blank dataframe
  raw.scored.analysis.df <- df
  
  
  #generate the correct index numbers for the dataframe to analyze (not counting last column)
  raw.scored.analysis.index <- length(df) - 1
  
  #generate sums for each scored category by stratficiation category
  
  for (category in category.levels){
    for (score.category in colnames(df)[1:raw.scored.analysis.index] ){
      raw.scored.analysis.df[category,score.category] <- df %>%
        filter_(lazyeval::interp(quote(x == y), 
                                 x=as.name(category.column.name), 
                                 y=category))%>%
        select_(score.category)%>%
        mutate_if(is.character,as.numeric)%>%
        colSums()%>%
        as.numeric()
    }
  }
  
  
  #remove extraneous rows, tranpose, restore dataframeness, remove extraneous last row, add nice names
  raw.scored.analysis.df <- tail(as.matrix(raw.scored.analysis.df), length(category.levels))
  raw.scored.analysis.df <- t(raw.scored.analysis.df)
  raw.scored.analysis.df <- data.frame(raw.scored.analysis.df, stringsAsFactors = FALSE)
  raw.scored.analysis.df <- head(raw.scored.analysis.df, nrow(raw.scored.analysis.df) -1)
  colnames(raw.scored.analysis.df) <- colnames(category.df)
  rownames(raw.scored.analysis.df) <- column.nice.names.f
  
  
  #write the summary table
  
  ifelse(file.name.switch == 0, 
         
         raw.score.counts.by.category.filename <- paste(table.dir.path,
                                                        "tally_raw_of_", 
                                                        question.column.name.short,
                                                        "_by_",
                                                        category.column.name.short,
                                                        ".csv", 
                                                        sep = "")
         ,
         raw.score.counts.by.category.filename <- paste(table.dir.path,
                                                        "tally_reduced_of_", 
                                                        question.column.name.short,
                                                        "_by_",
                                                        category.column.name.short,
                                                        ".csv", 
                                                        sep = "")
  )
  
  
  
  
  
  write.csv(raw.scored.analysis.df, file = raw.score.counts.by.category.filename)
  
  
  # calculate raw score totals and preserve row names
  raw.scored.analysis.tallied.df <- raw.scored.analysis.df%>%
    mutate_if(is.character,as.numeric)%>%
    mutate("Totals_in_all_categories" = Reduce("+",.[1:(length(category.levels))]))
  rownames(raw.scored.analysis.tallied.df) <-column.nice.names.f
  raw.scored.analysis.tallied.df$Barrier <- rownames(raw.scored.analysis.tallied.df)
  
  return(raw.scored.analysis.tallied.df)
  
}


#tally up the number of individuals reponding in a given category of barrier
raw.scored.analysis.tallied.df <- raw.score.analysis(category.raw.scored.df, 
                                                     category.levels,
                                                     file.name.switch = 0, 
                                                     category.raw.scored.columns.nice.names)







#plot raw score tallies
plot.tallied.scored <- function(raw.scored.analysis.tallied.df, 
                                question.column.name.nice, 
                                n.respondents , 
                                file.name.switch = 0
){
  
  raw.scored.analysis.tallied.df.plot.title <- paste(" Absolute number of issues scored across all categories\n",
                                                     question.column.name.nice,
                                                     "\n",
                                                     "n=",
                                                     n.respondents
  )
  
  raw.scored.analysis.tallied.df%>%
    ggplot()+
    aes(x= reorder(Barrier,Totals_in_all_categories), y= Totals_in_all_categories)+
    #scale_x_discrete(limits = positions)+
    coord_flip()+
    ylab("number of issues scored")+
    xlab("percived barriers as scored")+
    ggtitle(raw.scored.analysis.tallied.df.plot.title)+
    geom_bar(stat = "identity")+
    theme_minimal()
  
  ifelse(file.name.switch == 0, 
         
         raw.scored.analysis.tallied.df.plot.filename <- paste("tally_of_raw_coded_barriers_all_categories",
                                                               question.column.name.short,
                                                               "by",
                                                               category.column.name.short,
                                                               ".png",
                                                               sep = "_")
         ,
         raw.scored.analysis.tallied.df.plot.filename <- paste("tally_of_reduced_coded_barriers_all_categories",
                                                               question.column.name.short,
                                                               "by",
                                                               category.column.name.short,
                                                               ".png",
                                                               sep = "_")
  )
  
  ggsave(paste(plot.dir.path,raw.scored.analysis.tallied.df.plot.filename, sep= ""), 
         width = 13.8, 
         height = 8.81, 
         units = "in")
}

#plot number of respondents by barriers (all categories)
plot.tallied.scored(raw.scored.analysis.tallied.df, 
                    question.column.name.nice, 
                    file.name.switch = 0,
                    n.respondents)

########### Calculate indiviual proportion of users reporting idenifying with a barrier

proportional.responses <- function(df,
                                   category.levels, 
                                   response.counts.by.category, 
                                   table.dir.path,
                                   question.column.name.safe,
                                   category.column.name.safe,
                                   col.order
){
  
  #crate new df from raw.scored.analysis.df and change column names to category key names (safe)
  raw.scored.analysis.df <- df[,1:(length(category.levels))]
  raw.scored.analysis.wkey.df <- raw.scored.analysis.df
  colnames(raw.scored.analysis.wkey.df) <- category.levels
  
  #remake df according to prefered order
  raw.scored.analysis.wkey.df.tmp <- raw.scored.analysis.wkey.df%>%
    select(one_of(col.order))
  rownames(raw.scored.analysis.wkey.df.tmp) <- rownames(raw.scored.analysis.wkey.df)
  raw.scored.analysis.wkey.df <- raw.scored.analysis.wkey.df.tmp
  
  #melt and transpose and make "value" column numeric, preserving column name
  raw.scored.analysis.wkey.df <- t(raw.scored.analysis.wkey.df)
  raw.scored.analysis.wkey.df <- melt(as.matrix(raw.scored.analysis.wkey.df))
  raw.scored.analysis.wkey.df$value <- as.numeric(as.character(raw.scored.analysis.wkey.df$value))
  
  
  #Caculate percentages of responses in barrier category attribitable to stratification category
  # add summed_score column with this value
  responses.summed.by.barriers <- raw.scored.analysis.wkey.df %>%
    group_by(Var2)%>%
    mutate(summed_score = sum(as.numeric(as.character(value))))%>%
    mutate(percentage = (value / summed_score) * 100) 
  
  #add proportion calculation to barriers table where porpotion is out of total respondents
  # in a category. e.g. if 100 answered 'Associates' to Q21 but 50 left a + comment in Q38
  # then 50% of respondents at an Associates institution encountered a barrier
  
  #modify response.counts.by.category df for this analysis
  response.counts.df <- data.frame(response.counts.by.category["response_count",], stringsAsFactors = FALSE)
  colnames(response.counts.df) <- response.counts.by.category["category_key",]
  
  #create response column as factor
  responses.summed.by.barriers$responses <- responses.summed.by.barriers$Var1
  responses.summed.by.barriers$responses <- as.character(responses.summed.by.barriers$responses)
  # replace responses with the number of responses in that category
  for (category in category.levels) {
    responses.summed.by.barriers$responses[responses.summed.by.barriers$responses == category] <- response.counts.df[,category]
    
  }
  responses.summed.by.barriers$responses <- as.numeric(as.character(responses.summed.by.barriers$responses))
  # calculate the proportional response
  proportional.responses.summed.by.barriers<- responses.summed.by.barriers %>%
    mutate(proportion = value/responses)
  
  #Add nice names to dataframe
  
  proportional.responses.summed.by.barriers <- proportional.responses.summed.by.barriers%>%
    mutate(nice_names = Var1)
  proportional.responses.summed.by.barriers$nice_names <- as.character(proportional.responses.summed.by.barriers$nice_names)
  # replace responses with the number of responses in that category
  for (category in category.levels) {
    proportional.responses.summed.by.barriers$nice_names[proportional.responses.summed.by.barriers$nice_names == category] <- nice.names.df[category, "rownames"]
    
  }
  
  
  proportional.responses.summed.by.barriers.filename <- paste(table.dir.path,
                                                              "tally_of_raw_score_of_", 
                                                              question.column.name.short,
                                                              "_by_",
                                                              category.column.name.short,
                                                              "_proportions",
                                                              ".csv", 
                                                              sep = "")
  write.csv(proportional.responses.summed.by.barriers, 
            file = proportional.responses.summed.by.barriers.filename)
  
  return(proportional.responses.summed.by.barriers)
  
}

proportional.responses.summed.by.barriers <- proportional.responses(raw.scored.analysis.tallied.df,
                                                                    category.levels, 
                                                                    response.counts.by.category, 
                                                                    table.dir.path,
                                                                    question.column.name.safe,
                                                                    category.column.name.safe,
                                                                    col.order)

############# Return top5 barriers ##############################################################

top5.barriers.by.proportion <- function(df){
  top.rows <- proportional.responses.summed.by.barriers%>%
    arrange(desc(summed_score))%>%
    distinct(Var2)%>%
    head(n=5)
  
  top.rows <- top.rows$Var2
  
  proportional.responses.summed.by.barriers.top5 <- proportional.responses.summed.by.barriers%>%
    filter(Var2 %in% top.rows)
  
  return(proportional.responses.summed.by.barriers.top5)
  
}

proportional.responses.summed.by.barriers.top5 <- top5.barriers.by.proportion(proportional.responses.summed.by.barriers)

############# Plot top5 barriers ##############################################################

plot.of.top5.barriers <- function(df,
                                  question.column.name.nice,
                                  category.nice.name.lower,
                                  n.respondents,
                                  category.nice.name.caps,
                                  question.column.name.safe,
                                  category.column.name.safe
){
  
  #setup ordering of plot
  proportional.responses.summed.by.barriers.top5.plot <- df
  proportional.responses.summed.by.barriers.top5.plot$Var2 <- 
    factor(proportional.responses.summed.by.barriers.top5.plot$Var2, levels = 
             proportional.responses.summed.by.barriers.top5.plot$Var2[order(desc(proportional.responses.summed.by.barriers.top5.plot$summed_score))])
  
  
  
  #correct nice_names for plotting
  #SUBSTITUTION
  
  
  
  #replace underscores with spaces
  proportional.responses.summed.by.barriers.top5.plot$nice_names <- gsub("_",
                                                                         " ",
                                                                         proportional.responses.summed.by.barriers.top5.plot$nice_names)
  #replace 'X' with ','
  proportional.responses.summed.by.barriers.top5.plot$nice_names <- gsub("X",
                                                                         ",",
                                                                         proportional.responses.summed.by.barriers.top5.plot$nice_names)
  #replace 'K' with ""
  proportional.responses.summed.by.barriers.top5.plot$nice_names <- gsub("K",
                                                                         "",
                                                                         proportional.responses.summed.by.barriers.top5.plot$nice_names)
  #replace 'D' with '-'
  proportional.responses.summed.by.barriers.top5.plot$nice_names <- gsub("D",
                                                                         "-",
                                                                         proportional.responses.summed.by.barriers.top5.plot$nice_names)
  
  
  
  #plot
  proportional.responses.summed.by.barriers.top5.plot%>%
    ggplot()+
    aes(x=Var2, y=proportion, fill=Var1)+
    geom_bar(stat = "identity", position = "dodge")+
    labs(x = question.column.name.nice, 
         y = "percentage of respondents", 
         title = "Top 5 Most Commonly Reported Barriers to Including Bioinformatics in Existing Courses",
         subtitle = paste("Shown as percentage of users within each",
                          category.nice.name.lower,
                          "n=",n.respondents ))+
    theme_minimal()+
    theme(axis.text.x=element_text(angle=-20, hjust = 0, vjust = 1))+
    scale_fill_discrete(name= category.nice.name.caps, labels=nice.lables.list)
  
  proportional.responses.summed.by.barriers.top5.plot.filename <- paste("top_5_reported_barriers_proprotional_by_cat",
                                                                        question.column.name.short,
                                                                        "by",
                                                                        category.column.name.short,
                                                                        ".png",
                                                                        sep = "_")
  
  ggsave(paste(plot.dir.path,proportional.responses.summed.by.barriers.top5.plot.filename, sep= ""), 
         width = 13.8, 
         height = 8.81, 
         units = "in")
}



plot.of.top5.barriers(proportional.responses.summed.by.barriers.top5,
                      question.column.name.nice,
                      category.nice.name.lower,
                      n.respondents,
                      category.nice.name.caps,
                      question.column.name.safe,
                      category.column.name.safe)

########### Create new columns for proportion tests

proportion_table <- proportional.responses.summed.by.barriers%>%
  mutate(positive_scored_response = value)%>%
  mutate(null_scored_response = responses - value)


############ significantly Different Barriers Across Categories  #########################################

sig.diff.chi.analysis <- function(df){
  # calculate chi-values (by proportion test) on each category and return significantly different barriers
  proportional.responses.summed.by.barriers.grouped <- group_by(df, Var2)
  proportional.responses.summed.by.barriers.grouped.prop <- proportional.responses.summed.by.barriers.grouped%>%
    do(prop_test_chi_pvalue = prop.test(.$positive_scored_response,.$responses)$p.value)
  
  proportional.sig.responses.summed.by.barriers.filename <- paste(table.dir.path,
                                                                  "sig_diff_", 
                                                                  question.column.name.short,
                                                                  "_barriers_by_",
                                                                  category.column.name.short,
                                                                  "_prop_test",
                                                                  ".csv", 
                                                                  sep = "")
  write.csv(as.matrix(proportional.responses.summed.by.barriers.grouped.prop), file = proportional.sig.responses.summed.by.barriers.filename)
  
  return(proportional.responses.summed.by.barriers.grouped.prop)
}

#calculate chi values
# remove NaN Values
proportion_table_non_zero <- proportion_table%>%
  filter(summed_score != 0)

# for valid chi tests, remove scored categories where any scored category has less than 5 respondents
proportion_table_minimal_scoring <- proportion_table_non_zero%>%
  group_by(Var2)%>%
  filter(all(value >= 5))


#execute function to test for signifigance
proportional.sig.responses.summed.by.barriers <- sig.diff.chi.analysis(proportion_table_minimal_scoring)


# Add signifigance to proportion table

proportion_table_summary <- proportion_table_minimal_scoring%>%
  group_by(Var2)%>%
  left_join(., proportional.sig.responses.summed.by.barriers)

#coerce proportion test values into numeric forms
proportion_table_summary$prop_test_chi_pvalue <- as.numeric(proportion_table_summary$prop_test_chi_pvalue)



######Caclualte Margins of Error ####################################################################
# Interval estimate of population proportion at 95% confidence interval

proportion_table_summary <- proportion_table_summary%>%
  mutate(proportion_error = as.numeric(sqrt((proportion * (1 - proportion)/responses))*qnorm(.975)))%>%
  mutate(ymax = proportion + (proportion * proportion_error))%>%
  mutate(ymin = proportion - (proportion * proportion_error))

#CREATE FRAME FOR SAVING
proportion_table_summary.filename <- paste(table.dir.path,
                                           "sum_table_",
                                           question.column.name.short,
                                           "_by_",
                                           category.column.name.short,
                                           ".csv",
                                           sep = "")
write_csv(proportion_table_summary,path =  proportion_table_summary.filename)

######### POWER ANALYSIS #############################################################################
# Calculate possible effect size given 80% power for a chi.test statistic 

effect.size <- round(pwr.chisq.test(w = NULL, 
                                    N = n.respondents, 
                                    df = (length(category.levels) - 1), 
                                    sig.level = 0.05, 
                                    power = 0.8)$w, digits = 3)


effect_statement <- if(effect.size <= .1){
  paste("Question effect size at 80% power is ",effect.size, ", sufficent for detecting small effects [.1]", sep= "")
}else if (effect.size > .1 | effect.size <= .5){
  paste("Question effect size at 80% power is ",effect.size, ", sufficent for detecting medium effects [.3]", sep= "")
}else if (effect.size >= .5){
  paste("Question effect size at 80% power is ",effect.size, ", sufficent for detecting large effects [.5]", sep="" )
}

############ Plot significantly different barriers ####################################


plot.sig.barriers <- function(df, 
                              category.df,
                              category.levels,
                              category.nice.name.caps,
                              category.nice.name.lower,
                              n.respondents,
                              question.column.name.safe,
                              category.column.name.safe){
  
  #significant barriers
  sig.barriers <- df%>%
    filter(prop_test_chi_pvalue <= 0.05)
  sigs <- as.character(sig.barriers$Var2)
  sigs <- c(sigs)
  
  #plot significant barriers
  
  #reformat Var2 names as chr
  df$Var2 <- as.character(df$Var2)
  
  proportional.sig.responses.summed.by.barriers.plot <-df%>%
    filter(Var2 %in% sigs)
  
  #setup plot ordering
  
  proportional.sig.responses.summed.by.barriers.plot <- proportional.sig.responses.summed.by.barriers.plot
  proportional.sig.responses.summed.by.barriers.plot$Var2 <- 
    factor(proportional.sig.responses.summed.by.barriers.plot$Var2, levels = 
             proportional.sig.responses.summed.by.barriers.plot$Var2[order(desc(proportional.sig.responses.summed.by.barriers.plot$summed_score))])
  
  
  
  
  #plot
  
  # create legend lables that show the value of n for a stratfying category
  legend.labels <- df%>%
    ungroup()%>%
    select(nice_names, responses)%>%
    head(., n = length(category.levels))%>%
    mutate(legend = paste(nice_names, " (","n=", responses,")", sep = ""))
  
  #correct nice_names for plotting
  #SUBSTITUTION
  
  #replace underscores with spaces
  legend.labels$legend <- gsub("_",
                               " ",
                               legend.labels$legend)
  #replace 'X' with ','
  legend.labels$legend <- gsub("X",
                               ",",
                               legend.labels$legend)
  #replace 'K' with ""
  legend.labels$legend <- gsub("K",
                               "",
                               legend.labels$legend)
  #replace 'D' with '-'
  legend.labels$legend <- gsub("D",
                               "-",
                               legend.labels$legend)
  # create labels that show how many positive (coded) responses
  
  x.labels <- proportional.sig.responses.summed.by.barriers.plot%>%
    arrange(desc(summed_score))%>%
    select(Var2, summed_score)%>%
    distinct(Var2, .keep_all = TRUE)%>%
    mutate(x.labels = paste(Var2, " \n(", "N(cr+)=", summed_score, ")", sep = ""))
  
  # get values of error bars
  
  error.limits <- aes(ymax = proportional.sig.responses.summed.by.barriers.plot$ymax, ymin = proportional.sig.responses.summed.by.barriers.plot$ymin)
  error.dodge <- position_dodge(width=0.9)
  
  proportional.sig.responses.summed.by.barriers.plot%>%
    ggplot()+
    aes(x=Var2, y=proportion, fill=Var1)+
    geom_bar(stat = "identity", position = "dodge")+
    labs(x = question.column.name.nice,
         y = "percentage of respondents", 
         title = paste("Barriers Differing significantly by", category.nice.name.caps),
         subtitle = paste("Shown as percentage of users within each ",
                          category.nice.name.lower,
                          " n=",n.respondents, "\n", effect_statement, sep = "" ))+
    theme_minimal()+
    theme(axis.text.x=element_text(angle=-20, hjust = 0, vjust = 1))+
    scale_fill_discrete(name= category.nice.name.caps, labels = legend.labels$legend)+
    scale_x_discrete(labels = x.labels$x.labels)+
    geom_errorbar(error.limits, position = error.dodge, width = .2)
  
  
  
  
  proportional.sig.responses.summed.by.barriers.plot.filename <- paste("barriers_differing_signifigantly_by_category_proprotional_by_category",
                                                                       question.column.name.short,
                                                                       "by",
                                                                       category.column.name.short,
                                                                       ".png",
                                                                       sep = "_")
  
  ggsave(paste(plot.dir.path,proportional.sig.responses.summed.by.barriers.plot.filename, sep= ""), 
         width = 13.8, 
         height = 8.81, 
         units = "in")
  
  
}


# plot significantly different responses
plot.sig.barriers(proportion_table_summary, 
                  category.df,
                  category.levels,
                  category.nice.name.caps,
                  category.nice.name.lower,
                  n.respondents,
                  question.column.name.safe,
                  category.column.name.safe)

pdf(NULL)
############## Analysis of Reduced Categories ##################################



# baloon plot of percentages of faculty reporting barrier by stratafying category


#get tallies for reduced columns
reduced.tally.df <- raw.score.analysis(category.reduced.df,
                                       category.levels,
                                       file.name.switch = 1,
                                       category.reduced.columns.nice.names)

# Select relavant columns and ensure values are numeric
reduced.tally.df <- as.matrix(reduced.tally.df[,1:(length(reduced.tally.df)-2)])

reduced.tally.df <- reduced.tally.df%>%
  as.data.frame()%>%
  select(one_of(col.order2))%>%
  as.matrix()

#convert reponse numbers to percentages

#get denominators (number of responses for each stratafying category)
responses.n.for.all.categories <- proportional.responses.summed.by.barriers%>%
  ungroup()%>%
  select(responses, nice_names)%>%
  head(.,n=length(category.levels))%>%
  t()
colnames(responses.n.for.all.categories) <- responses.n.for.all.categories["nice_names",]
responses.n.for.all.categories <- head(responses.n.for.all.categories, n=1)
responses.n.for.all.categories[1,] <- as.numeric(responses.n.for.all.categories[1,])


#convert tally of reduced columns to percentages
for(categories in colnames(reduced.tally.df)){
  denom <- as.numeric(responses.n.for.all.categories[,categories])
  reduced.tally.df[,categories] <- round(((reduced.tally.df[,categories] / denom)*100), digits = 1)
}


#generate baloon plot

reduced.baloonplot.filename <- paste("reduced_baloonplot",
                                     question.column.name.short,
                                     "by",
                                     category.column.name.short,
                                     ".png",
                                     sep = "_")
# Create df for baloon plot
reduced.tally.df.baloon <- reduced.tally.df

#correct nice_names for plotting
#SUBSTITUTION
colnames(reduced.tally.df.baloon) <- chartr("XD", 
                                            ",-", 
                                            colnames(reduced.tally.df.baloon))
colnames(reduced.tally.df.baloon) <- gsub("_", 
                                          " ", 
                                          colnames(reduced.tally.df.baloon))
colnames(reduced.tally.df.baloon) <- gsub("K", 
                                          "", 
                                          colnames(reduced.tally.df.baloon))
#reorder baloon df


png(filename = paste(plot.dir.path,reduced.baloonplot.filename, sep= ""),
    width = 13.8, 
    height = 8.81, 
    units = "in", 
    res = 600)

balloonplot(as.table(reduced.tally.df.baloon),
            main = paste("Percentages of Faculty Reporting", question.column.name.nice, "by", category.nice.name.caps, 
                         "\n area proprotional to percentage", sep = " "),
            show.margins = FALSE, 
            show.zeros = FALSE, 
            xlab = "Barriers", 
            ylab = category.nice.name.caps,
            text.size = 0.7
)

dev.off()
pdf(NULL)

# Barplot



#transpose and restore dataframeness

reduced.tally.df.t <- t(reduced.tally.df)
reduced.tally.df.t <- data.frame(reduced.tally.df.t)

# Restore nice names
colnames(reduced.tally.df.t) <- category.reduced.columns.nice.names

#melt as matrix for plotting
reduced.tally.df.m <- melt(as.matrix(reduced.tally.df.t))

#correct nice_names for plotting
#SUBSTITUTION

#replace underscores with spaces
reduced.tally.df.m$Var1 <- gsub("_",
                                " ",
                                reduced.tally.df.m$Var1)
#replace 'X' with ','
reduced.tally.df.m$Var1 <- gsub("X",
                                ",",
                                reduced.tally.df.m$Var1)
#replace 'K' with  ""
reduced.tally.df.m$Var1 <- gsub("K",
                                "",
                                reduced.tally.df.m$Var1)
#replace 'D' with -
reduced.tally.df.m$Var1 <- gsub("D",
                                "-",
                                reduced.tally.df.m$Var1)



#create plot
#convert percentages to decimal
reduced.tally.df.m.plot <- reduced.tally.df.m
reduced.tally.df.m.plot$value <- reduced.tally.df.m.plot$value/100 

#setup ordering of plot
reduced.tally.df.m.plot$Var2 <- 
  factor(reduced.tally.df.m.plot$Var2, levels = 
           reduced.tally.df.m.plot$Var2[order(desc(reduced.tally.df.m.plot$value))])



reduced.tally.df.m.plot%>%
  ggplot()+
  aes(x= Var1, y = value, fill = Var2)+
  geom_bar(stat="identity", position = "dodge")+
  labs(x = category.column.name.nice,
       y = "percentage of respondents", 
       title = paste("Percentages of Faculty Responding within Reduced Barrier Categories"),
       subtitle = paste("Shown as percentage of users within each",
                        category.nice.name.lower,
                        "n=",n.respondents ))+
  theme_minimal()+
  scale_fill_discrete(name= "Reduced Barrier Categories")+
  scale_x_discrete(limits = head(reduced.tally.df.m.plot$Var1, n = length(category.levels)))

reduced.summary.percentage.filename <- paste("reduced_category_barplot",
                                             question.column.name.short,
                                             "by",
                                             category.column.name.short,
                                             ".png",
                                             sep = "_")


ggsave(paste(plot.dir.path,reduced.summary.percentage.filename, sep= ""), 
       width = 13.8, 
       height = 8.81, 
       units = "in")





# Correlation plot
correlation.plot.filename <- paste("reduced_category_correlation",
                                   question.column.name.short,
                                   "by",
                                   category.column.name.short,
                                   ".png",
                                   sep = "_")

png(filename = paste(plot.dir.path,correlation.plot.filename, sep= ""),
    width = 14, 
    height = 14, 
    units = "in", 
    res = 600)

reduced.tally.df.correlated <- cor(reduced.tally.df.t)
corrplot(reduced.tally.df.correlated, 
         order = "hclust", 
         tl.srt=45)
dev.off()



