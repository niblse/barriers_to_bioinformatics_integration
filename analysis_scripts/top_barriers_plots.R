# load required libraries
require(ggplot2)
require(tidyverse)
require(reshape2)
require(pwr)
require(FactoMineR)
require(factoextra)

#Read in data
data.df <- read_csv("../data_cleaning_scripts/04_decode_survey_responses/output/decoded_df.csv")

#remove non-us respondents

#remove respondents not in US/Puerto Rico
remove.non.us.repondants <- function(df){
  countries <- c("United States","Puerto Rico")
  df <- df%>%
    filter(Country_Country %in% countries )
  return(df)
}

data.df <- remove.non.us.repondants(data.df)


# Give nice names to Q1

sample.size <- nrow(data.df)

# Select relavant columns and add nice names


#q33
#In your opinion, what do you think are the most
#important challenges currently facing those educating
#undergraduate life scientists in bioinformatics?

q33.reduced.cols <- data.df%>%
  select(q33_Faculty_issues_reduced, 
         q33_Facility_issues_reduced, 
         q33_Resources_issues_reduced, 
         q33_Student_issues_reduced, 
         q33_Curriculum_issues_reduced, 
         q33_Institutional_issues_reduced)

colnames(q33.reduced.cols) <- c("Faculty Issues (reduced)",
                                "Facilities Issues (reduced)", 
                                "Resource Issues (reduced)", 
                                "Student Issues (reduced)",
                                "Curriculum Issues (reduced)", 
                                "Institutional Issues (reduced)")

q33.scored.cols <- data.df%>%
  select(Q33_Faculty...No.Expertise.Training, 
         Q33_Facutly...Time,
         Q33_Faculty...Differences.of.opinion, 
         Q33_Faculty...Content.Development, 
         Q33_Faculty...Not.enough.Faculty, 
         Q33_Facilities...Computer.Labs.limited.or.not.available, 
         Q33_Facilities...Computers.are.too.old..inadequate,
         Q33_Resources...Acces.to.Approp..Software, 
         Q33_Resources...Funding..general.,
         Q33_Resources...Funding..software.License.fees., 
         Q33_Student.Issues...Lack.Approp.Background.Knowledge.Skills, 
         Q33_Student.Issues...No.interest.in.Bioinf, 
         Q33_Student.issues...Intimidated.by.topic, 
         Q33_Student.Isses...Multitude.of.varying.student.backgrounds, 
         Q33_Student.Issues...Lack.Basic.Computing.Knowledge, 
         Q33_Student.Issues...Career.prospects, 
         Q33_Curric.Issues...Lack.of.integration.of.maerial, 
         Q33_Curric.Isues...To.much.conent.in.Life.Sci.curric, 
         Q33_Curric.Issues...How.quickly.the.material.tech.changes,
         Q33_Curriculum...Access.to.developed.Bioinf.Lesson.Plans.Bioinf.Curric, 
         Q33_Curric.Issues...Making.Comp.Sci.courses.consistently.relevant, 
         Q33_Curric.Issues...To.much.curric.influence.from.Professional.schools, 
         Q33_Inst.Dept.Support...Inter.Departmental.Cooperation, 
         Q33_Inst.Dept.Support...No.IT.support)

colnames(q33.scored.cols)<- c("Faculty Issues: Expertise/training", 
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
#38
#What is preventing you from including
#bioinformatics content in these courses?

q38.reduced.cols <- data.df%>%
  select(q38_Faculty_issues_reduced, 
         q38_Curriculum_issues_reduced, 
         q38_Resources_issues_reduced, 
         q38_Student_issues_reduced, 
         q38_Facilities_issues_reduced, 
         q38_Institutional_issues_reduced, 
         q38_State_issues_reduced, 
         q38_Accredited_issues_reduced)

colnames(q38.reduced.cols) <- c("Faculty Issues (reduced)",
                                "Curriculum Issues (reduced)", 
                                "Resource Issues (reduced)", 
                                "Student Issues (reduced)",
                                "Facilities Issues (reduced)", 
                                "Institutional Issues (reduced)",
                                "State Issues (reduced)", 
                                "Accredidation Issues (reduced)")

q38.scored.cols <- data.df%>%
  select(Q38_Faculty.Issue...No.Expertise..Training, 
         Q38_Faculty.Issue...Time, 
         Q38_Faculty.Issue...Does.not.know.how.to.design.curricula.or.incorporate.with.existing.curriculum, 
         Q38_Faculty.Issue...Lack.of.Faculty.interest.at.Institution, 
         Q38_Faculty.Issues...Faculty.is.new.to.current.Dept, 
         Q38_Curriculum.Issue...Course.Load.Full..No.time.space.for.Content, 
         Q38_Curric.Issues...Does.not.Fit.into.current.Current.Course.Structure, 
         Q38_Curriculum.Issue...Time.for.Curriculum.Development, 
         Q38_Curric.Issues...Lack.of.Curric.Control.not.in.curent.Curric., 
         Q38_Curric.Issues...Bioinf..Taught.in.other.courses.at.institution, 
         Q38_Curric.Issue...Class.Size, 
         Q38_Curric.Issues...Plans.to.teach.Bioinf..In.the.future..but.not.currently.available., 
         Q38_Curric.Issue...Bioinfo.Conent.too.Massive, 
         Q38_Curric.Issues...Content.needs.to.be.introduced.in.multiple.courses,
         Q38_Resources...Access.to.Quality.Exercises..Content, 
         Q38_Resources...Access.to.developed.Bioinf.Lesson.Plans.Bioinf.Curric,
         Q38_Resources...Access.to.Approp.Introductory.Content, 
         Q38_Resources...Unable.to.identify.access.best.current.Bioinf.material, 
         Q38_Resource.Issues...Funding, 
         Q38_Resource.Issues...Not.available.in.course.textbook, 
         Q38_Resource.Issues...Access.to.Quality.Online.Exerices.Conent, 
         Q38_Resource.Issues..TA.s.lack.approp.skils,
         Q38_Student.Issues...UG.Students.Lack.Approp.Background.Knowledge, 
         Q38_Student.Issues...Lack.of.interest.in.topic,
         Q38_Facilities.Issue..Access.to.Appropriate.Facilities..Equipment,
         Q38_Inst.Dept.Issues...Institutional.Inertia,  
         Q38_State.restrictions, 
         Q38_Not.accredited)


colnames(q38.scored.cols) <- c("Faculty Issues: Expertise/training", 
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


#q33
q33.reduced.cols <- q33.reduced.cols%>%
  summarise_each(funs(sum))
q33.reduced.cols <- q33.reduced.cols/sample.size
rownames(q33.reduced.cols) <- "percentage"

q33.scored.cols <- q33.scored.cols%>%
  summarise_each(funs(sum))
q33.scored.cols <- q33.scored.cols/sample.size
rownames(q33.scored.cols) <- "percentage"

#q38
q38.reduced.cols <- q38.reduced.cols%>%
  summarise_each(funs(sum))
q38.reduced.cols <- q38.reduced.cols/sample.size
rownames(q38.reduced.cols) <- "percentage"

q38.scored.cols <- q38.scored.cols%>%
  summarise_each(funs(sum))
q38.scored.cols <- q38.scored.cols/sample.size
rownames(q38.scored.cols) <- "percentage"


#transpose and combine tallies and add question as variable name


q33.reduced.cols <- data.frame(t(q33.reduced.cols), stringsAsFactors = FALSE)
q33.reduced.cols$question <- "Q33 Reduced"
q33.reduced.cols$barrier <- rownames(q33.reduced.cols)
rownames(q33.reduced.cols) <- NULL

q33.scored.cols <- data.frame(t(q33.scored.cols), stringsAsFactors = FALSE)
q33.scored.cols$question <- "Q33 scored"
q33.scored.cols$barrier <- rownames(q33.scored.cols)
rownames(q33.scored.cols) <- NULL

q38.reduced.cols <- data.frame(t(q38.reduced.cols), stringsAsFactors = FALSE)
q38.reduced.cols$question <- "Q38 Reduced"
q38.reduced.cols$barrier <- rownames(q38.reduced.cols)
rownames(q38.reduced.cols) <- NULL

q38.scored.cols <- data.frame(t(q38.scored.cols), stringsAsFactors = FALSE)
q38.scored.cols$question <- "Q38 scored"
q38.scored.cols$barrier <- rownames(q38.scored.cols)
rownames(q38.scored.cols) <- NULL

#combine all columns for plotting and set up ordering by percentage
total.reduced.cols <- rbind(q33.reduced.cols,
                            q38.reduced.cols)
total.reduced.cols$barrier <- factor(total.reduced.cols$barrier, levels = total.reduced.cols$barrier[order(desc(total.reduced.cols$percentage))])


total.scored.cols <- rbind(q33.scored.cols,
                           q38.scored.cols)
total.scored.cols$barrier <- factor(total.scored.cols$barrier, levels = total.scored.cols$barrier[order(desc(total.scored.cols$percentage))])

dir.create("./top5_plots", recursive = TRUE)

total.reduced.cols%>%
  ggplot()+
  aes(x = barrier, y=percentage, fill = barrier)+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(~ question)+
  ggtitle(paste("Summary of barriers as reported by all respondents for Q33 and Q38 \n", 
                "n=", sample.size,
                sep = "")) +
  xlab("Summarized barrier categories")+
  ylab("Proportion of respondents reporting barrier")+
  theme_minimal()+
  theme(axis.text.x = element_blank())

ggsave( filename= "./top5_plots/reduced_barriers_Q33_Q38.png", 
        width = 13.8, 
        height = 8.81, 
        units = "in")
    
pdf(NULL)

total.scored.cols%>%
  filter(percentage >= 0.05)%>%
  ggplot()+
  aes(x = barrier, y=percentage, fill = barrier)+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(~ question)+
  ggtitle(paste("Scored barriers as reported by at least 5% of respodants for Q33 and Q38 \n", 
                "n=", sample.size,
                sep = "")) +
  xlab("Scored barrier categories")+
  ylab("Proportion of respondents reporting barrier")+
  theme_minimal()+
  theme(axis.text.x = element_blank())


ggsave( filename= "./top5_plots/scored_barriers_Q33_Q38.png", 
        width = 13.8, 
        height = 8.81, 
        units = "in")

pdf(NULL)