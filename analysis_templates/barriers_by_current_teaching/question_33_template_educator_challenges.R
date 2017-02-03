# load required libraries
require(ggplot2)
require(tidyverse)
require(reshape2)
require(corrplot)
require(pwr)

############ LOAD THE PREPARED SURVEY DATA ###########################################
#read in cleaned dataframe "decoded_df.csv"
master.df <- read_csv("../../data_cleaning_scripts/04_decode_survey_responses/output/decoded_df.csv")

############# MANUAL!!!! SET 1st SET OF MANUAL VARIABLES  #############################


# set the variable (Question) that will be analyzed: "COLUMN_NAME"
category.column.name <- "Q1_Please.select.the.statement.belOw.that.best.describes.yOur.current.teaching.Of.biOinfOrmatics.cOn..."

# set a 'nice' (e.g. human readable) name to describe this category: "Category (Q#)"
category.column.name.nice <- "Q1 Bioinformatics Integration"

# set a 'safe name' for naming variables: "Q#_category_category"
category.column.name.safe <- "Q1_bioinformatics_integration"


#set a nice name in upper and lower case that describes the category kinds 
#(e.g. gender, institution type): ""

category.nice.name.caps <- "Bioinformatics Integration"
category.nice.name.lower <- "bioinformatics integration"

############# GET CATEGORIES TO ANALYZE  ##############################################
#All questions are analyzed by a stratafying category (e.g. gender)

#subset the category column
category.column.subset <-  master.df[[category.column.name]]
#Get the levels (possible answers) within that catagories
category.levels <- levels(as.factor(category.column.subset))

############# MANUAL!!!! SET 2nd SET OF MANUAL VARIABLES  #############################

#Set levels to retain ( excluding for example responses such as 'Don't Know or 'NA')
# select the range of values within catagory.levels to use (e.g. category.levels[1:4])
category.levels <- category.levels[1:3]

#######################################################################################

# Reset the category subset
category.column.subset <-  master.df[[category.column.name]]



############ TEMPLATE VARIABLES    ############################
# These variables should not change and are specific to a template that analyzes a particular question
# Q38 Variables

#Names
# variable (Question) that will be analyzed ("COLUMN_NAME)
question.column.name <- "Q33_In.your.opinion..what.do.you.think.are.the.most.important.challenges.currently.facing.those.educa..."
# subsetting variable for this column (e.g master.df$COLUMN_NAME)
question.column.name.nice <- "Educator Challenges to Including Bioinformatics (Q33)"
# 'nice name' to describe this question
question.column.name.safe <- "Q33_educator_challenges"
# 'safe name' for naming variables
question.column.subset <- master.df[[question.column.name]]


########## Cleaning Steps ###################################################

#remove respondants not in US/Puerto Rico
remove.non.us.repondants <- function(df){
  countries <- c("United States","Puerto Rico")
  df <- master.df%>%
    filter(Country_Country %in% countries )
  return(df)
}

master.df.us.only <- remove.non.us.repondants(master.df)

# We will only analyze data that match the category.levels you have selected
# assuming you removed NA's or 'Don't knows, we will only analyze data for 
# respondants for which we have a response to the stratfying category

remove.non.relavant.repondants <- function(df){
  responses <- c("United States","Puerto Rico")
  df <- df%>%
    filter_(lazyeval::interp(quote(x %in% y), x=as.name(category.column.name), y=category.levels))
  return(df)
}

relavant.respondants.df <- remove.non.relavant.repondants(master.df.us.only)

#RESET SUBSET VALUES
category.column.subset <- relavant.respondants.df[[category.column.name]]
question.column.subset <- relavant.respondants.df[[question.column.name]]


############# column selection #################################################


category.raw.scored.columns <- c("Q33_Faculty...No.Expertise.Training", 
                                 "Q33_Facutly...Time",
                                 "Q33_Faculty...Differences.of.opinion", 
                                 "Q33_Faculty...Content.Development", 
                                 "Q33_Faculty...Not.enough.Faculty", 
                                 "Q33_Facilities...Computer.Labs.limited.or.not.available", 
                                 "Q33_Facilities...Computers.are.too.old..inadequate",
                                 "Q33_Resources...Acces.to.Approp..Software", 
                                 "Q33_Resources...Funding..general.",
                                 "Q33_Resources...Funding..software.License.fees.", 
                                 "Q33_Student.Issues...Lack.Approp.Background.Knowledge.Skills", 
                                 "Q33_Student.Issues...No.interest.in.Bioinf", 
                                 "Q33_Student.issues...Intimidated.by.topic", 
                                 "Q33_Student.Isses...Multitude.of.varying.student.backgrounds", 
                                 "Q33_Student.Issues...Lack.Basic.Computing.Knowledge", 
                                 "Q33_Student.Issues...Career.prospects", 
                                 "Q33_Curric.Issues...Lack.of.integration.of.maerial", 
                                 "Q33_Curric.Isues...To.much.conent.in.Life.Sci.curric", 
                                 "Q33_Curric.Issues...How.quickly.the.material.tech.changes",
                                 "Q33_Curriculum...Access.to.developed.Bioinf.Lesson.Plans.Bioinf.Curric", 
                                 "Q33_Curric.Issues...Making.Comp.Sci.courses.consistently.relevant", 
                                 "Q33_Curric.Issues...To.much.curric.influence.from.Professional.schools", 
                                 "Q33_Inst.Dept.Support...Inter.Departmental.Cooperation", 
                                 "Q33_Inst.Dept.Support...No.IT.support", 
                                 category.column.name)

category.raw.scored.df <- relavant.respondants.df%>%
  select(one_of(category.raw.scored.columns))

# Create a set of nice names for these columns

category.raw.scored.columns.nice.names <- c("Faculty Issues: Expertise/training", 
                                            "Faculty Issues: Time",
                                            "Faculty Issues: Differences of opinion",
                                            "Faculty Issues: Content development",
                                            "Faculty Issues: Too few faculty",
                                            "Faculities Issues: Access to computer labs",
                                            "Faculities Issues: Inadaquate computers",
                                            "Resource Issues: Access to software",
                                            "Resource Issues: Funding",
                                            "Resource Issues: Software/license fees",
                                            "Student Issues: Lack of background skills/knowledge",
                                            "Student Issues: Lack of interest",
                                            "Student Issues: Intimidated by topic",
                                            "Student Issues: Varying student backrounds",
                                            "Student Issues: Lack of basic computing skills",
                                            "Student Issues: Career prospects",
                                            "Curriculum Issues: Lack of integration", 
                                            "Curriculum Issues: Too much content", 
                                            "Curriculum Issues: Quickly changing technologies", 
                                            "Curriculum Issues: Access to bioinformatics lesson plans/curriculum", 
                                            "Curriculum Issues: Difficulty establishing relevance", 
                                            "Curriculum Issues: Influence from professional schools",
                                            "Institutional Issues: Lack of inter-departmental cooperation",
                                            "Institutional Issues: Lack of IT support")

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


category.df <- data.frame ("Dedicated Course"= category.levels[1],
                           "Integrating Bioinformatics"= category.levels[2],
                           "Not Integrating Bioinformatics"= category.levels[3],
                           stringsAsFactors = FALSE)

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

### count responses
# Determine how many respondents identifed in the accepted stratafying categories 
# (e.g. how many identifed as male or female)


num.cat.respondants <- function(df,
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
      # filter pipe below will return counts for all respondants that match the category
      filter(category.column.subset == response.counts.by.category["category_key",category])%>%
      count()
  }
  
  response.counts.by.category.filename <- paste(table.dir.path,
                                                "count_of_responses_to_", 
                                                question.column.name.safe, 
                                                ".csv", 
                                                sep = "")
  write.csv(response.counts.by.category, file = response.counts.by.category.filename)
  
  # 2. Calculate "overall n" for the stratifying category - these are the number of respondants 
  # who answered to your chosen category, and who will be analyzed 
  
  n.respondants <- sum(as.numeric(response.counts.by.category["response_count",]))
  
  return(list(n.respondants = n.respondants,
              response.counts.by.category = response.counts.by.category))
}


# calculate number of respondants who identified within a stratfying category and write to file
n.respondants.object <- num.cat.respondants(category.raw.scored.df,
                                      category.df.blank, 
                                      category.column.subset,
                                      table.dir.path,
                                      question.column.name.safe)

n.respondants <- as.numeric(n.respondants.object[1])
response.counts.by.category <-as.data.frame(n.respondants.object[2])


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
  categories <- nice.category.names 
  summary.response.df%>%
  ggplot()+
    aes(x=Var2, y=value)+
    geom_bar(stat="identity", 
             position = "dodge")+
    ylab("Individual Responses")+
    xlab(category.column.name.nice)+
    scale_x_discrete(labels = categories )+
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
  
  ggsave(paste(plot.dir.path,summary.response.plotname, sep= ""), 
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

#### CALCULATE SURVEY POWER ##########################

analysis_power <- function(n.respondants, category.levels){
  
  small_effect <- pwr.chisq.test(w = .20, 
                                 N = n.respondants, 
                                 df = (length(category.levels)-1))
  medium_effect <- pwr.chisq.test(w = .50, 
                                  N = n.respondants, 
                                  df = (length(category.levels)-1))
  large_effect <- pwr.chisq.test(w = .80, 
                                 N = n.respondants, 
                                 df = (length(category.levels)-1))
  
  power_df <- data.frame("p_small_effect" =  small_effect$power, 
                         "p_medium_effect" = medium_effect$power, 
                         "p_large_effect" = large_effect$power, 
                         stringsAsFactors = FALSE)
  
  power_df.filename <- paste(table.dir.path,
                             "power_analysis_for_chi_tests",
                             question.column.name.safe,
                             ".csv",
                             sep = "")
  
  write.csv(power_df, file = power_df.filename)
  
}

analysis_power(n.respondants, category.levels)


######### RAW SCORE ANALYSIS #############################################################################

#Generate summary stats for raw scored sub-categories

raw.score.analysis <- function(df,
                               category.levels 
                               ){
  #prepare a blank dataframe
  raw.scored.analysis.df <- df
  
  #generate the correct index numbers for the dataframe to analyze (not counting last column)
  raw.scored.analysis.index <- length(category.raw.scored.df) - 1
  
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
  rownames(raw.scored.analysis.df) <- category.raw.scored.columns.nice.names
  
  
  #write the summary table
  raw.score.counts.by.category.filename <- paste(table.dir.path,
                                                 "tally_of_raw_score_of_", 
                                                 question.column.name.safe,
                                                 "_by_",
                                                 category.column.name.safe,
                                                 ".csv", 
                                                 sep = "")
  write.csv(raw.scored.analysis.df, file = raw.score.counts.by.category.filename)
  
  
  # calculate raw score totals and preserve row names
  raw.scored.analysis.tallied.df <- raw.scored.analysis.df%>%
    mutate_if(is.character,as.numeric)%>%
    mutate("Totals_in_all_categories" = Reduce("+",.[1:(length(category.levels))]))
  rownames(raw.scored.analysis.tallied.df) <-category.raw.scored.columns.nice.names
  raw.scored.analysis.tallied.df$Barrier <- rownames(raw.scored.analysis.tallied.df)
  
  return(raw.scored.analysis.tallied.df)
}


#tally up the number of individuals reponding in a given category of barrier
raw.scored.analysis.tallied.df <- raw.score.analysis(category.raw.scored.df, 
                                   category.levels)







#plot raw score tallies
plot.tallied.scored <- function(raw.scored.analysis.tallied.df, 
                                question.column.name.nice, 
                                n.respondants 
                                ){
  
  raw.scored.analysis.tallied.df.plot.title <- paste(" Absolute number of issues scored across all categories\n",
                                                     question.column.name.nice,
                                                     "\n",
                                                     "n=",
                                                     n.respondants
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
  
  raw.scored.analysis.tallied.df.plot.filename <- paste("tally_of_raw_coded_barriers_all_cateroies",
                                                        question.column.name.safe,
                                                        "by",
                                                        category.column.name.safe,
                                                        ".png",
                                                        sep = "_")
  
  ggsave(paste(plot.dir.path,raw.scored.analysis.tallied.df.plot.filename, sep= ""), 
         width = 13.8, 
         height = 8.81, 
         units = "in")
}

#plot number of respondants by barriers (all categories)
plot.tallied.scored(raw.scored.analysis.tallied.df, 
                    question.column.name.nice, 
                    n.respondants)

########### Calculate indiviual proportion of users reporting idenifying with a barrier

proportional.responses <- function(df,
                                   category.levels, 
                                   response.counts.by.category, 
                                   table.dir.path,
                                   question.column.name.safe,
                                   category.column.name.safe
                                   ){
  
  #crate new df from raw.scored.analysis.df and change column names to category key names (safe)
  raw.scored.analysis.df <- df[,1:(length(category.levels))]
  raw.scored.analysis.wkey.df <- raw.scored.analysis.df
  colnames(raw.scored.analysis.wkey.df) <- category.levels
  
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
  
  #add proportion calculation to barriers table where porpotion is out of total respondants
  # in a category. e.g. if 100 answered 'Associates' to Q21 but 50 left a + comment in Q38
  # then 50% of respondants at an Associates institution encountered a barrier
  
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
                                                              question.column.name.safe,
                                                              "_by_",
                                                              category.column.name.safe,
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
                                                                    category.column.name.safe)

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
                                  n.respondants,
                                  category.nice.name.caps,
                                  question.column.name.safe,
                                  category.column.name.safe
                                  ){
  
  #setup ordering of plot
  proportional.responses.summed.by.barriers.top5.plot <- df
  proportional.responses.summed.by.barriers.top5.plot$Var2 <- 
    factor(proportional.responses.summed.by.barriers.top5.plot$Var2, levels = 
             proportional.responses.summed.by.barriers.top5.plot$Var2[order(desc(proportional.responses.summed.by.barriers.top5.plot$summed_score))])
  
  proportional.responses.summed.by.barriers.top5.plot$nice_names <- 
    factor(proportional.responses.summed.by.barriers.top5.plot$nice_names, levels = 
             colnames(category.df))
  
  #plot
  proportional.responses.summed.by.barriers.top5.plot%>%
    ggplot()+
    aes(x=Var2, y=proportion, fill=nice_names)+
    geom_bar(stat = "identity", position = "dodge")+
    labs(x = question.column.name.nice, 
         y = "proportion of respondants", 
         title = "Top 5 Most Commonly Reported Barriers to Including Bioinformatics in Existing Courses",
         subtitle = paste("Shown as proportion of users within each",
                          category.nice.name.lower,
                          "n=",n.respondants ))+
    theme_minimal()+
    theme(axis.text.x=element_text(angle=-20, hjust = 0, vjust = 1))+
    scale_fill_discrete(name= category.nice.name.caps)
  
  proportional.responses.summed.by.barriers.top5.plot.filename <- paste("top_5_reported_barriers_proprotional_by_category",
                                                                        question.column.name.safe,
                                                                        "by",
                                                                        category.column.name.safe,
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
                      n.respondants,
                      category.nice.name.caps,
                      question.column.name.safe,
                      category.column.name.safe)

############ Signifigantly Different Barriers Across Categories  #########################################

sig.diff.chi.analysis <- function(df){
  # calculate chi-values on each category and return signifigantly different barriers
  proportional.responses.summed.by.barriers.grouped <- group_by(df, Var2)
  proportional.responses.summed.by.barriers.grouped.chi <- proportional.responses.summed.by.barriers.grouped%>%
    do(chi_test = chisq.test(.$value,simulate.p.value = TRUE)$p.value)
  
  proportional.sig.responses.summed.by.barriers.filename <- paste(table.dir.path,
                                                                  "signifigantly_different", 
                                                                  question.column.name.safe,
                                                                  "_barriers_by_",
                                                                  category.column.name.safe,
                                                                  "_chi_squared_results",
                                                                  ".csv", 
                                                                  sep = "")
  write.csv(as.matrix(proportional.responses.summed.by.barriers.grouped.chi), file = proportional.sig.responses.summed.by.barriers.filename)
  
  return(proportional.responses.summed.by.barriers.grouped.chi)
}

#calculate chi values
# remove NaN Values
proportional.responses.summed.by.barriers <- proportional.responses.summed.by.barriers%>%
  filter(summed_score != 0)
proportional.sig.responses.summed.by.barriers <- sig.diff.chi.analysis(proportional.responses.summed.by.barriers)

############ Plot signifigantly different barriers ####################################


plot.sig.barriers <- function(df, 
                              proportional.responses.summed.by.barriers, 
                              category.df,
                              category.nice.name.caps,
                              category.nice.name.lower,
                              n.respondants,
                              question.column.name.safe,
                              category.column.name.safe){
  
  #signifigant barriers
  sig.barriers <- df%>%
    filter(chi_test <= 0.05)
  sigs <- as.character(sig.barriers$Var2)
  sigs <- c(sigs)
  
  #plot signifigant barriers
  
  #reformat Var2 names as chr
  proportional.responses.summed.by.barriers$Var2 <- as.character(proportional.responses.summed.by.barriers$Var2)
  
  proportional.sig.responses.summed.by.barriers.plot <-proportional.responses.summed.by.barriers%>%
    filter(Var2 %in% sigs)
  
  #setup plot ordering
  
  proportional.sig.responses.summed.by.barriers.plot <- proportional.sig.responses.summed.by.barriers.plot
  proportional.sig.responses.summed.by.barriers.plot$Var2 <- 
    factor(proportional.sig.responses.summed.by.barriers.plot$Var2, levels = 
             proportional.sig.responses.summed.by.barriers.plot$Var2[order(desc(proportional.sig.responses.summed.by.barriers.plot$summed_score))])
  
  proportional.sig.responses.summed.by.barriers.plot$nice_names <- 
    factor(proportional.sig.responses.summed.by.barriers.plot$nice_names, levels = 
             colnames(category.df))
  
  
  #plot
  
  proportional.sig.responses.summed.by.barriers.plot%>%
    ggplot()+
    aes(x=Var2, y=proportion, fill=nice_names)+
    geom_bar(stat = "identity", position = "dodge")+
    labs(x = question.column.name.nice,
         y = "proportion of respondants", 
         title = paste("Barriers Differing Signifigantly by", category.nice.name.caps),
         subtitle = paste("Shown as proportion of users within each",
                          category.nice.name.lower,
                          "n=",n.respondants ))+
    theme_minimal()+
    theme(axis.text.x=element_text(angle=-20, hjust = 0, vjust = 1))+
    scale_fill_discrete(name= category.nice.name.caps)
  
  
  proportional.sig.responses.summed.by.barriers.plot.filename <- paste("barriers_differing_signifigantly_by_category_proprotional_by_category",
                                                                       question.column.name.safe,
                                                                       "by",
                                                                       category.column.name.safe,
                                                                       ".png",
                                                                       sep = "_")
  
  ggsave(paste(plot.dir.path,proportional.sig.responses.summed.by.barriers.plot.filename, sep= ""), 
         width = 13.8, 
         height = 8.81, 
         units = "in")
}

plot.sig.barriers(proportional.sig.responses.summed.by.barriers, 
                  proportional.responses.summed.by.barriers, 
                  category.df,
                  category.nice.name.caps,
                  category.nice.name.lower,
                  n.respondants,
                  question.column.name.safe,
                  category.column.name.safe)





