# load required libraries
require(ggplot2)
require(tidyverse)
require(reshape2)


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


# get sample size

sample.size <- nrow(data.df)

# Select relavant columns and add nice names



#q6
#Please describe briefly; 
#include any barriers to development and/or implementation.”

q06.reduced.cols <- data.df%>%
  select(q06_Faculty_issues_reduced,
         q06_Student_issues_reduced,
         q06_Curriculum_issues_reduced,
         q06_Institutional_issues_reduced,
         q06_Facilities_issues_reduced,
         q06_Resource_issues_reduced)

colnames(q06.reduced.cols)<- c("Faculty Issues (reduced)",
                              "Student Issues (reduced)",
                              "Curriculum Issues (reduced)",
                              "Institutional Issues (reduced)",
                              "Facilities Issues (reduced)",
                              "Resource Issues (reduced)")
q06.scored.cols <- data.df%>%
  select(Q06_Faculty.Issue...No.Expertise..Training,
         Q06_Faculty.Issue...Time, 
         Q06_Faculty.Issue...Not.enough.Faculty.members, 
         Q06_Faculty.Issues...Interest.in.Topic, 
         Q06_Curriculum.Issue...Course.Load.Full..No.time.space.for.Content, 
         Q06_Curric.Isues..Curric.Revision.needed, 
         Q06_Curric.Isues...Bioinf.approaches.need.to.be.taugh.at.every.level.integrate.current.classes, 
         Q06_Curic.Issues...Bioinf.Program.Courses.under.develop, 
         Q06_Curric.Isues...Not.a.req.for.Life.Sci.Majors..Pre.Med, 
         Q06_Curric.Issue...Class.Size.too.large, 
         Q06_Curric.Issues...Not.enough.classes.seats.available, 
         Q06_Student.issue...Lack.of.Student.interest.in.quant.skills.and.related.courses, 
         Q06_Student.Issues...Student.Background.is.insufficient, 
         Q06_Inst.Dept.Support...Institutional.Inertia, 
         Q06_Inst.Dept.Support...Inter.Departmental.Cooperation, 
         Q06_Inst.Dept.Support...IT.Supprt, 
         Q06_Inst.Dept.Support...State.Regulation,
         Q06_Resource.Issue...Funding, 
         Q06_Resource.Issues...Access.to.Approp.Software..Op.sys, 
         Q06_Resources...Access.to.developed.Bioinf.Lesson.Plans.Bioinf.Curric, 
         Q06_Facilities...Computer.Labs..Laq.Equip.limited.or.not.available, 
         Q06_Facilities...Servers)

colnames(q06.scored.cols)<- c("Faculty Issues: Expertise/training", 
                              "Faculty Issues: Time",
                              "Faculty Issues: Too few faculty",
                              "Faculty Issues: Lack of interest",
                              "Curriculum Issues: No space",
                              "Curriculum Issues: Incompatible with current curriculum",
                              "Curriculum Issues: Integration needed at every level",
                              "Curriculum Issues: Bioinformatics curriculum is under development",
                              "Curriculum Issues: Not required for life sci/pre-med majors",
                              "Curriculum Issues: Class size",
                              "Curriculum Issues: Too few seats",
                              "Student Issues: Lack of interest",
                              "Student Issues: Background knowledge",
                              "Institutional: Institutional inertia", 
                              "Institutional: Inter-departmental cooperation", 
                              "Institutional: IT support", 
                              "Institutional: State regulation", 
                              "Resource Issues: Funding", 
                              "Resource Issues: Access to software",
                              "Resource Issues: Access to bioinformatics lesson plans/curricula",
                              "Facilities: Access to computer(s)/labs",
                              "Facilities: Servers")

#29
# At your current institution, do you face any technical barriers 
# in teaching bioinformatics, e.g., availability of a computer lab, 
# different operating systems, access to high performance computing 
# for teaching, IT support?

q29.reduced.cols <- data.df%>%
  select(q29_Faculty_issues_reduced, 
         q29_Student_issues_reduced, 
         q29_Curriculum_issues_reduced,
         q29_Institutional_issues_reduced, 
         q29_Facilities_issues_reduced, 
         q29_Resources_issues_reduced)

colnames(q29.reduced.cols)<- c("Faculty Issues (reduced)",
                               "Student Issues (reduced)",
                               "Curriculum Issues (reduced)",
                               "Institutional Issues (reduced)",
                               "Facilities Issues (reduced)",
                               "Resource Issues (reduced)")

q29.scored.cols <- data.df%>%
  select(Q29.30_Faculty...No.Expertise.Training, 
         Q29.30_Faculty...Time, 
         Q29.30_Faculty...not.interested.in.topic, 
         Q29.30_Faculty...no.computer.sci.Faculty, 
         Q29.30_Student.Issues...Access.to.Approp.Software.off.campus, 
         Q29.30_Student.Issues...Basic.Computing.Knowledge, 
         Q29.30_Student.Issues...No.access.to.computers.at.home, 
         Q29.30_Student.Issues...No.interest.in.Bioinf, 
         Q29.30_Curriculum...Need.to.Develop.new.program,
         Q29.30_Curriculum.Class.Size.too.large,
         Q29.30_Curriculum...Access.to.developed.Bioinf.Lesson.Plans.Bioinf.Curric,
         Q29.30_Inst.Dept.Support...No.IT.support, 
         Q29.30_Inst.Dept.Support...No.Sub.to.Pay.site.Databases, 
         Q29.30_Inst.Dept.Support...Comp.Sci.Dept.Will.not.support.Bioinf, 
         Q29.30_Facilities...Computer.Labs.limited.or.not.available, 
         Q29.30_Facilities...Computers.are.too.old..inadequate, 
         Q29.30_Facilities...Servers, 
         Q29.30_Facilities...Internet.Access.Limited, 
         Q29.30_Resources...Operating.System.Availability.Issues, 
         Q29.30_Resources...Approp..Software, 
         Q29.30_Resources...no.high.performance.systems.available, 
         Q29.30_Resources...Funding)

colnames(q29.scored.cols)<- c("Faculty Issues: Expertise/training", 
                              "Faculty Issues: Time",
                              "Faculty Issues: No interest in topic",
                              "Faculty Issues: No comp-sci faculty",
                              "Student Issues: Access to software off-campus", 
                              "Student Issues: Basic computing knowledge", 
                              "Student Issues: Access to computers Off-campus", 
                              "Student Issues: No interest in bioinformatics",
                              "Curriculum Issues: New program development required", 
                              "Curriculum Issues: Class size", 
                              "Curriculum Issues: Access to bioinformatics lesson plans",
                              "Institutional Issues: Access to IT support", 
                              "Institutional Issues: Subscriptions/licenses", 
                              "Institutional Issues: No support from comp-sci faculty",
                              "Facilities Issues: Access to computer labs",
                              "Facilities Issues: Inadaquate computers",
                              "Facilities Issues: Access to servers",
                              "Facilities Issues: Access to internet",
                              "Resource Issues: Access to operating systems",
                              "Resource Issues: Apropriate software", 
                              "Resource Issues: Access to high-performance computing", 
                              "Resource Issues: Funding")

#q33
#In your opinion, what do you think are the most
#important challenges currently facing those educating
#undergraduate life scientists in bioinformatics?

q33.reduced.cols <- data.df%>%
  select(q33_Faculty_issues_reduced, 
         q33_Student_issues_reduced, 
         q33_Curriculum_issues_reduced, 
         q33_Institutional_issues_reduced,
         q33_Facility_issues_reduced, 
         q33_Resources_issues_reduced)

colnames(q33.reduced.cols) <- c("Faculty Issues (reduced)",
                                "Student Issues (reduced)",
                                "Curriculum Issues (reduced)",
                                "Institutional Issues (reduced)",
                                "Facilities Issues (reduced)",
                                "Resource Issues (reduced)")

q33.scored.cols <- data.df%>%
  select(Q33_Faculty...No.Expertise.Training, 
         Q33_Facutly...Time,
         Q33_Faculty...Differences.of.opinion, 
         Q33_Faculty...Content.Development, 
         Q33_Faculty...Not.enough.Faculty, 
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
         Q33_Inst.Dept.Support...No.IT.support,
         Q33_Facilities...Computer.Labs.limited.or.not.available, 
         Q33_Facilities...Computers.are.too.old..inadequate,
         Q33_Resources...Acces.to.Approp..Software, 
         Q33_Resources...Funding..general.,
         Q33_Resources...Funding..software.License.fees.)

colnames(q33.scored.cols)<- c("Faculty Issues: Expertise/training", 
                              "Faculty Issues: Time",
                              "Faculty Issues: Differences of opinion",
                              "Faculty Issues: Content development",
                              "Faculty Issues: Too few faculty",
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
                              "Institutional Issues: Lack of IT support",
                              "Faculities Issues: Access to computer labs",
                              "Faculities Issues: Inadaquate computers",
                              "Resource Issues: Access to software",
                              "Resource Issues: Funding",
                              "Resource Issues: Software/license fees")
#38
#What is preventing you from including
#bioinformatics content in these courses?

q38.reduced.cols <- data.df%>%
  select(q38_Faculty_issues_reduced, 
         q38_Student_issues_reduced, 
         q38_Curriculum_issues_reduced, 
         q38_Institutional_issues_reduced, 
         q38_Facilities_issues_reduced, 
         q38_Resources_issues_reduced, 
         q38_State_issues_reduced, 
         q38_Accredited_issues_reduced)

colnames(q38.reduced.cols) <- c("Faculty Issues (reduced)",
                                "Student Issues (reduced)", 
                                "Curriculum Issues (reduced)", 
                                "Institutional Issues (reduced)", 
                                "Facilities Issues (reduced)", 
                                "Resource Issues (reduced)", 
                                "State Issues (reduced)", 
                                "Accredidation Issues (reduced)")

q38.scored.cols <- data.df%>%
  select(Q38_Faculty.Issue...No.Expertise..Training, 
         Q38_Faculty.Issue...Time, 
         Q38_Faculty.Issue...Lack.of.Faculty.interest.at.Institution, 
         Q38_Faculty.Issue...Does.not.know.how.to.design.curricula.or.incorporate.with.existing.curriculum, 
         Q38_Faculty.Issues...Faculty.is.new.to.current.Dept, 
         Q38_Student.Issues...UG.Students.Lack.Approp.Background.Knowledge, 
         Q38_Student.Issues...Lack.of.interest.in.topic,
         Q38_Curriculum.Issue...Course.Load.Full..No.time.space.for.Content, 
         Q38_Curric.Issues...Does.not.Fit.into.current.Current.Course.Structure, 
         Q38_Curriculum.Issue...Time.for.Curriculum.Development, 
         Q38_Curric.Issues...Lack.of.Curric.Control.not.in.curent.Curric., 
         Q38_Curric.Issues...Bioinf..Taught.in.other.courses.at.institution, 
         Q38_Curric.Issue...Class.Size, 
         Q38_Curric.Issues...Plans.to.teach.Bioinf..In.the.future..but.not.currently.available., 
         Q38_Curric.Issue...Bioinfo.Conent.too.Massive, 
         Q38_Curric.Issues...Content.needs.to.be.introduced.in.multiple.courses,
         Q38_Inst.Dept.Issues...Institutional.Inertia,  
         Q38_Facilities.Issue..Access.to.Appropriate.Facilities..Equipment,
         Q38_Resources...Access.to.Quality.Exercises..Content, 
         Q38_Resources...Access.to.developed.Bioinf.Lesson.Plans.Bioinf.Curric,
         Q38_Resources...Access.to.Approp.Introductory.Content, 
         Q38_Resources...Unable.to.identify.access.best.current.Bioinf.material, 
         Q38_Resource.Issues...Funding, 
         Q38_Resource.Issues...Not.available.in.course.textbook, 
         Q38_Resource.Issues...Access.to.Quality.Online.Exerices.Conent, 
         Q38_Resource.Issues..TA.s.lack.approp.skils,
         Q38_State.restrictions, 
         Q38_Not.accredited)


colnames(q38.scored.cols) <- c("Faculty Issues: Expertise/training", 
                               "Faculty Issues: Time",
                               "Faculty Issues: Lack of interest",
                               "Faculty Issues: Curriculum design/integration",
                               "Faculty Issues: New Faculty",
                               "Student Issues: Background knowledge",
                               "Student Issues: Interest in topic",
                               "Curriculum Issues: No space",
                               "Curriculum Issues: Incompatible with current curriculum",
                               "Curriculum Issues: Time needed to develop",
                               "Curriculum Issues: No control",
                               "Curriculum Issues: Covered elsewhere",
                               "Curriculum Issues: Class size",
                               "Curriculum Issues: Plan for future coverage",
                               "Curriculum Issues: Too much content",
                               "Curriculum Issues: Content requires several courses",
                               "Institutional: Inertia",
                               "Facilities: Access to equipment",
                               "Resource Issues: Access to exercises",
                               "Resource Issues: Access to lesson plans",
                               "Resource Issues: Access to intro content",
                               "Resource Issues: Unable to vet content",
                               "Resource Issues: Funding",
                               "Resource Issues: No appropriate textbook",
                               "Resource Issues: No access to online exercises",
                               "Resource Issues: No qualified TAs",
                               "State Restrictions", 
                               "Accreditation")


#q06
q06.reduced.cols <- q06.reduced.cols%>%
  summarise_each(funs(sum))
q06.reduced.cols <- q06.reduced.cols/sample.size
rownames(q06.reduced.cols) <- "percentage"

q06.scored.cols <- q06.scored.cols%>%
  summarise_each(funs(sum))
q06.scored.cols <- q06.scored.cols/sample.size
rownames(q06.scored.cols) <- "percentage"

#q29
q29.reduced.cols <- q29.reduced.cols%>%
  summarise_each(funs(sum))
q29.reduced.cols <- q29.reduced.cols/sample.size
rownames(q29.reduced.cols) <- "percentage"

q29.scored.cols <- q29.scored.cols%>%
  summarise_each(funs(sum))
q29.scored.cols <- q29.scored.cols/sample.size
rownames(q29.scored.cols) <- "percentage"

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

q06.reduced.cols <- data.frame(t(q06.reduced.cols), stringsAsFactors = FALSE)
q06.reduced.cols$question <- "Q06 Reduced"
q06.reduced.cols$barrier <- rownames(q06.reduced.cols)
rownames(q06.reduced.cols) <- NULL

q06.scored.cols <- data.frame(t(q06.scored.cols), stringsAsFactors = FALSE)
q06.scored.cols$question <- "Q06 scored"
q06.scored.cols$barrier <- rownames(q06.scored.cols)
rownames(q06.scored.cols) <- NULL

q29.reduced.cols <- data.frame(t(q29.reduced.cols), stringsAsFactors = FALSE)
q29.reduced.cols$question <- "Q29 Reduced"
q29.reduced.cols$barrier <- rownames(q29.reduced.cols)
rownames(q29.reduced.cols) <- NULL

q29.scored.cols <- data.frame(t(q29.scored.cols), stringsAsFactors = FALSE)
q29.scored.cols$question <- "Q29 scored"
q29.scored.cols$barrier <- rownames(q29.scored.cols)
rownames(q29.scored.cols) <- NULL

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




total.reduced.cols <- rbind(q06.reduced.cols, 
                            q29.reduced.cols,
                            q33.reduced.cols,
                            q38.reduced.cols)
total.reduced.cols$barrier <- factor(total.reduced.cols$barrier, levels = total.reduced.cols$barrier[order(desc(total.reduced.cols$percentage))])


#Rename questions for plotting

total.reduced.cols$question[total.reduced.cols$question == "Q06 Reduced"] <- "q2.Q6"
total.reduced.cols$question[total.reduced.cols$question == "Q29 Reduced"] <- "q4.Q29"
total.reduced.cols$question[total.reduced.cols$question == "Q33 Reduced"] <- "q1.Q33"
total.reduced.cols$question[total.reduced.cols$question == "Q38 Reduced"] <- "q3.Q38"



#create data frame of facet lables

label.df.r <- list("q2.Q6" = "Q6: Please describe briefly any\n barriers to development and/or implementation\n of new bioinformatics courses?",
                       "q4.Q29" = "Q29: Do you face any technical\n barriers in teaching bioinformatics?", 
                       "q1.Q33" = "Q33: What do you think are the\n most important challenges currently facing\n those educating undergraduate\n life scientists in bioinformatics?", 
                       "q3.Q38" = "Q38*: What is preventing you from\n including bioinformatics content?"
                       )

facet_labeller.reduce <- function(variable,value){
  return(label.df.r[value])
  
}


total.scored.cols <- rbind(q06.scored.cols,
                           q29.scored.cols,
                           q33.scored.cols,
                           q38.scored.cols)
total.scored.cols$barrier <- factor(total.scored.cols$barrier, levels = total.scored.cols$barrier[order(desc(total.scored.cols$percentage))])


total.scored.cols$question[total.scored.cols$question == "Q06 scored"] <- "q2.Q6"
total.scored.cols$question[total.scored.cols$question == "Q29 scored"] <- "q4.Q29"
total.scored.cols$question[total.scored.cols$question == "Q33 scored"] <- "q1.Q33"
total.scored.cols$question[total.scored.cols$question == "Q38 scored"] <- "q3.Q38"

label.df.s <- list("q2.Q6" = "Q6: Please describe briefly any\n barriers to development and/or\n implementation of new \nbioinformatics courses?",
                 "q4.Q29" = "Q29: Do you face any technical\n barriers in teaching bioinformatics?", 
                 "q1.Q33" = "Q33: What do you think are the\n most important challenges currently\n facing those educating\n undergraduate life scientists\nin bioinformatics?", 
                 "q3.Q38" = "Q38*: What is preventing you from\n including bioinformatics content?"
)

facet_labeller.score <- function(variable,value){
  return(label.df.s[value])
  
}


#create directory
dir.create("./top5_plots", recursive = TRUE)
dir.create("./top5_plots/ouput_tables", recursive = TRUE)


# Order plot by percentages

total.reduced.cols <- total.reduced.cols%>%
  group_by(question)%>%
  arrange(.,desc(percentage))

total.reduced.cols%>%
  filter(percentage >= 0.05)%>%
  ggplot()+
  aes(x = barrier, y=percentage, fill = barrier)+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(~ question, labeller = facet_labeller.reduce)+
  ggtitle(paste("Summary of barriers (reduced) as reported by at least 5% respondents for 4 barriers questions\n", 
                "n=", sample.size,
                sep = "")) +
  xlab("Summarized barrier categories")+
  ylab("Percentage of respondents reporting barrier")+
  scale_fill_discrete(name = "Barrier")+
  theme_bw()+
  theme(axis.text.x = element_blank())


ggsave( filename= "./top5_plots/reduced_barriers_Q06_Q29_Q33_Q38.png", 
        width = 13.8, 
        height = 8.81, 
        units = "in")

dev.off()
pdf(NULL)

write_csv(total.reduced.cols, "./top5_plots/ouput_tables/reduced_values.csv")

# Order plot by percentages

total.scored.cols <- total.scored.cols%>%
  group_by(question)%>%
  arrange(.,desc(percentage))


total.scored.cols%>%
  filter(percentage >= 0.05)%>%
  ggplot()+
  aes(x = barrier, y=percentage, fill = barrier)+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(~ question, labeller = facet_labeller.score)+
  ggtitle(paste("Scored barriers as reported by at least 5% of respondents\n", 
                "n=", sample.size,
                sep = "")) +
  xlab("Scored barrier categories")+
  ylab("Percentage of respondents reporting barrier")+
  theme_bw()+
  theme(axis.text.x = element_blank())


ggsave( filename= "./top5_plots/scored_barriers_Q06_Q29_Q33_Q38.png", 
        width = 13.8, 
        height = 8.81, 
        units = "in")


dev.off()
pdf(NULL)

write_csv(total.scored.cols, "./top5_plots/ouput_tables/scored_values.csv")


#plot q33 scored by itself

label.df.s.33 <- list("q1.Q33" = "Q33: What do you think are the most important challenges currently facing those educating\n undergraduate life scientists in bioinformatics?")

facet_labeller.score.33 <- function(variable,value){
  return(label.df.s.33[value])
  
}



# filter out Q33 and order for plotting
total.scored.cols <- total.scored.cols%>%
  filter(question == "q1.Q33")%>%
  filter(percentage >= 0.05)%>%
  group_by(question)%>%
  arrange(.,desc(percentage))


total.scored.cols$barrier <- factor(total.scored.cols$barrier, levels = total.scored.cols$barrier[order(desc(total.scored.cols$percentage))])


total.scored.cols%>%
  ggplot()+
  aes(x = barrier, y=percentage, fill = barrier)+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(~ question, labeller = facet_labeller.score.33)+
  ggtitle(paste("Scored barriers as reported by at least 5% of respondents\n", 
                "n=", sample.size,
                sep = "")) +
  xlab("Scored barrier categories")+
  ylab("Percentage of respondents reporting barrier")+
  scale_fill_discrete(name="Detailed barrier category")+
  theme_bw()+
  theme(axis.text.x = element_blank())


ggsave( filename= "./top5_plots/scored_barriers_Q33.png", 
        width = 13.8, 
        height = 8.81, 
        units = "in")


dev.off()
pdf(NULL)



