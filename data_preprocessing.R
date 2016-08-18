library(reshape2)
library(dplyr)
library(stringr)
library(readr)


##### reading in lists one by one. All of the lists contain duplicate information about ID and such. Deleting from all to later combine
##### them without issues
list1 <- read_csv("../../Media/2016/CSV/Sheet_1.csv")
names(list1) <- make.names(names(list1))

list2 <- read_csv("../../Media/2016/CSV/Sheet_2.csv")
names(list2) <- make.names(names(list2))
namesdrop <- list2 %>%
  select(starts_with("Email.Address"), 
         starts_with("First.Name"), 
         starts_with("LastName"), 
         starts_with("Custom.Data"),
         starts_with("StartDate"),
         starts_with("EndDate"),
         starts_with("IP.Address"),
         starts_with("CollectorID")) %>%
  colnames()
list2 <- list2[, !(names(list2) %in% namesdrop)]

list3 <- read_csv("../../Media/2016/CSV/Sheet_3.csv")
names(list3) <- make.names(names(list3))

namesdrop <- list3 %>%
  select(starts_with("Email.Address"), 
         starts_with("First.Name"), 
         starts_with("LastName"), 
         starts_with("Custom.Data"),
         starts_with("StartDate"),
         starts_with("EndDate"),
         starts_with("IP.Address"),
         starts_with("CollectorID")) %>%
  colnames()
list3 <- list3[, !(names(list3) %in% namesdrop)]

list4 <- read_csv("../../Media/2016/CSV/Sheet_4.csv")
names(list4) <- make.names(names(list4))

namesdrop <- list4 %>%
  select(starts_with("Email.Address"), 
         starts_with("First.Name"), 
         starts_with("LastName"), 
         starts_with("Custom.Data"),
         starts_with("StartDate"),
         starts_with("EndDate"),
         starts_with("IP.Address"),
         starts_with("CollectorID")) %>%
  colnames()
list4 <- list4[, !(names(list4) %in% namesdrop)]


##### inner joining all lists by RespondentID
bigtable <- inner_join(list1, list2, by = "RespondentID")
bigtable <- inner_join(bigtable, list3, by = "RespondentID")
bigtable <- inner_join(bigtable, list4, by = "RespondentID")

##### processing the columns with Universities. Since we had complicated logic, SurveyMonkey recorder each logic step in separate
##### columns. It makes it complicated.
bigtable_processed <- bigtable %>% 
  select(RespondentID, starts_with("University."), starts_with("A.3..")) %>%
  melt(id = "RespondentID", na.rm = TRUE)  

#saving ids with students who chose "Other, please specify" option when choosing universities
problem_ids <- bigtable_processed %>% filter(grepl("other", .$variable, ignore.case = TRUE)) %>% select(RespondentID)
write.csv(problem_ids, "problem_ids.csv")

bigtable_processed$variable <- gsub("(University.\\d{1}).+\\d{0,1}$", "\\1", bigtable_processed$variable)  
bigtable_processed$variable[grepl("A.3", bigtable_processed$variable)] <- "University.1" 
bigtable_processed <- reshape(bigtable_processed, direction = "wide", idvar = "RespondentID", timevar = "variable")



##### Deleting extra columns from the table
namesdrop <- bigtable %>%
  select(starts_with("University."), 
         starts_with("A.3..")) %>%
  colnames()
bigtable <- bigtable[, !(names(bigtable) %in% namesdrop)]
bigtable <- inner_join(bigtable, bigtable_processed, by = "RespondentID")

##### deleting empty columns E-mail... 
namesdrop <- bigtable %>%
  select(starts_with("Email.Address"), 
         starts_with("First.Name"), 
         starts_with("LastName"), 
         starts_with("Custom.Data")) %>%
  colnames()
bigtable <- bigtable[, !(names(bigtable) %in% namesdrop)]

##### turning columns with dates in appropriate format
bigtable$StartDate <- as.POSIXct(bigtable$StartDate, format = "%m/%d/%Y %H:%M:%S")
bigtable$EndDate <- as.POSIXct(bigtable$EndDate, format = "%m/%d/%Y %H:%M:%S")

#######################################################################################################
write.csv(x = bigtable, file = paste0("../../Media/2016/Master_tables/", Sys.Date(), "-bigtable.csv"), fileEncoding = "UTF-8")




### replaced multiple dots with "_" mark



write.csv(names(dataset), "names.csv", fileEncoding = "UTF-8")
write.csv(tenormore, "tenormore.csv", fileEncoding = "UTF-8")

#######################################################################################
### cleaning names of some courses
bigtable <- read.csv("../Media/2015/Master_tables/bigtable.csv", na.strings = c("", " ", "No answer", "N/A", "NA"), header = TRUE)
bigtable$X <- NULL
bigtable_temp <- bigtable

bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course. <- gsub("GEMMA-Master`s Degree in Women`s and Gender Studies", "GEMMA-Master's Degree in Women's and Gender Studies", bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course.)
bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course. <- gsub("CEMACUBE-Common European Master`s Course in Biomedical Engineering", "CEMACUBE-Common European Master's Course in Biomedical Engineering", bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course.)
bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course. <- gsub("IMQP-International Master in Quaternary and Prehistory Master International en Quaternaire et Pr?f?f?,©histoire", "IMQP-International Master in Quaternary and Prehistory, Master International en Quaternaire et Prehistoire", bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course.)
bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course. <- gsub("MITRA-M?f?f?,©diation interculturelle: identit?f?f?,©s, mobilit?f?f?,©s, conflits", "MITRA-Mediation interculturelle: identites, mobilites, conflits", bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course.)
bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course. <- gsub("AFEPA-European Master?f¢?,?????,?Ts programme in Agricultural, Food and Environmental Policy Analysis ", "AFEPA-European Master's programme in Agricultural, Food and Environmental Policy Analysis", bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course.)
bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course. <- gsub("EMARO+-European Master on Advanced Robotics +", "EMARO - European Master on Advanced Robotics", bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course.)
bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course. <- gsub("MUNDUS JOURNALISM\t-Erasmus Mundus Masters Journalism, Media and Globalisation", "MUNDUS JOURNALISM - Erasmus Mundus Masters Journalism, Media and Globalisation", bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course.)

write.csv(x = bigtable_temp, file = "bigtable.csv") #"2015-11-05 16:25:32 CET"

#######################################################################################
### cleaning up names of universities
### file from google docs
#university_names <- read.csv("C:/Users/Misha/Downloads/University names - Sheet1.csv")

z <- university_names %>%
  select(A.2.name.of.Erasmus.Mundus.master.course., University.1, University.2, University.3, University.4)%>%
  melt(na.rm = TRUE, id = "A.2.name.of.Erasmus.Mundus.master.course.") %>%
  group_by(A.2.name.of.Erasmus.Mundus.master.course.) %>%
  distinct(value)
write.csv(z, "second_sheet.csv") #shared online at "University names"

### checking if everything stayed the same
tenormore <- university_names %>%
  select(A.2.name.of.Erasmus.Mundus.master.course.) %>%
  group_by(A.2.name.of.Erasmus.Mundus.master.course.) %>%
  summarise(respondents = n()) %>%
  filter(respondents >= 10)
colnames(tenormore) <- c("Course", "Respondents")

university_names <- university_names[(university_names$A.2.name.of.Erasmus.Mundus.master.course. %in% tenormore$Course),]

#######################################################################################
### merging 
bigtable <- read.csv("../Media/2015/Master_tables/bigtable.csv", na.strings = c("", " ", "No answer", "N/A", "NA"), header = TRUE)
bigtable$X <- NULL
bigtable_temp <- bigtable
bigtable_temp$University.1 <- NULL
bigtable_temp$University.2 <- NULL
bigtable_temp$University.3 <- NULL
bigtable_temp$University.4 <- NULL

university_names$A.2.name.of.Erasmus.Mundus.master.course. <- NULL

bigtable_temp <- left_join(x = bigtable_temp, y = university_names, by = "RespondentID_")

write.csv(x = bigtable_temp, file = "bigtable.csv") #"2015-11-15 22:45:04 CET"

#######################################################################################
### cleaning typos and other things in a master table

bigtable <- read.csv("../Media/2015/Master_tables/bigtable.csv", na.strings = c("", " ", "No answer", "N/A", "NA"), header = TRUE)
bigtable$X <- NULL
bigtable_temp <- bigtable
names(bigtable_temp) <- gsub("C.1.Rate.the.following.items._Consistency.of.moduleÃƒ..s.assessment.across.universities", "C.1.Rate.the.following.items._Consistency.of.module's.assessment.across.universities", names(bigtable_temp))
names(bigtable_temp) <- gsub("Formalised.system.offered.by.the.university.consortium.through.which.students.can.share.their.opinions.and.provide.feedback.on.the.EM.course", "Formalised.system.by.university.consortium.for.students.to.share.opinions.and.feedback.on.course", names(bigtable_temp))
names(bigtable_temp) <- gsub("L.2.a.Rate.the.following.statements.about.internship._Overall.quality.of.the.internship_2", "L.2.a.Rate.the.following.statements.about.internship._Overall.quality.of.the.internship", names(bigtable_temp))
names(bigtable_temp) <- gsub("Health.Insurance", ("Health.insurance"), names(bigtable_temp))

bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course. <- gsub("&", "and", bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course.)
bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course. <- gsub(":", ".", bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course.)

#######################################################################################
### anonymyzing for course browser
bigtable <- read.csv("../Media/2015/Master_tables/bigtable.csv", na.strings = c("", " ", "No answer", "N/A", "NA"), header = TRUE)
bigtable$X <- NULL
bigtable_temp <- bigtable
bigtable_temp$RespondentID_ <- NULL
bigtable_temp$StartDate_ <- NULL
bigtable_temp$CollectorID_ <- NULL
bigtable_temp$EndDate_ <- NULL
bigtable_temp$IP.Address_ <- NULL
bigtable_temp$A.1.Please.create.a.unique.identification.code.Just.type.in.birthday.in.the.format.DD.MO.YY.and.the.first.two.letters.of.first.name.followed.by.the.first.two.letters.of.first.name.and.first.two.letter.of.last.name.For.example.Maris.Miller.was.born.on.October.9.1975.So.her.code.would.be.091075MAMI._Open.Ended.Response <- NULL
bigtable_temp$A.8.Age._Response <- NULL
bigtable_temp$A.5.When.did.you.start.EM.Course._Response <- NULL
bigtable_temp$A.6.When.did.you.will.you.finish.EM.Course._Response <- NULL

write.csv(x = bigtable_temp, file = "course_browser_anonymized.csv") # "2016-03-08 12:15:02 MSK"


##### function to extract information from an IP address of a respondent. Possibly will only use it for visualizations and such
##### Result of a function is stored in ip.csv.
# freegeoip <- function(ip, format = ifelse(length(ip)==1,'list','dataframe'))
# {
#   if (1 == length(ip))
#   {
#     # a single IP address
#     require(rjson)
#     url <- paste(c("http://freegeoip.net/json/", ip), collapse='')
#     ret <- fromJSON(readLines(url, warn=FALSE))
#     if (format == 'dataframe')
#       ret <- data.frame(t(unlist(ret)))
#     return(ret)
#   } else {
#     ret <- data.frame()
#     for (i in 1:length(ip))
#     {
#       r <- freegeoip(ip[i], format="dataframe")
#       ret <- rbind(ret, r)
#     }
#     return(ret)
#   }
# }   
# #ip <- freegeoip(as.character(bigtable$IP.Address_))
# ip_addresses <- as.data.frame(ip)
# bigtable <- inner_join(bigtable, ip_addresses, by = c("IP.Address_" = "ip"))
# write.csv(x = ip, file = "ip.csv")
