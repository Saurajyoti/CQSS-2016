library(dplyr)
library(readr)
### For qualitative team
dataset <- read_csv("../../Media/2016/Master_tables/bigtable.csv")
qualitative_df <- dataset %>%
  select(RespondentID,
         A.2..Select.the.name.of.your.Erasmus.Mundus.Joint.Master.Degree..EMJMD..,
         A.2..Select.the.name.of.your.Erasmus.Mundus.Joint.Master.Degree..EMJMD_Other..please.specify.,
         University.1,
         University.2,
         University.3,
         University.4,
         starts_with("A.4.."),
         A.8..Gender.,
         A.9..Have.you.been.awarded.an.Erasmus.Mundus.scholarship.for.your.master.studies.,
         contains("Open.Ended.Response"))

qualitative_df$A.1..Please.create.a.unique.identification.code..Just.type.in.your.birthday.in.the.format.DD.MO.YY.followed.by.the.first.two.letters.of.your.first.name.and.first.two.letter.of.your.last.name..For.example..Maris.Miller.was.born.on.October.9..1975..So.her.code.would.be.091075MAMI..Your.answer.to.this.question.will.remain.anonymous.and.will.not.be.shared.with.your.EMJMD_Open.Ended.Response <- NULL
write.csv(x = qualitative_df, file = "../../Media/2016/Master_tables/qualitative_team.csv", fileEncoding = "UTF-8")

### For qualitative team - open-ended questions with info on universities
first_university <- dataset %>%
  select(RespondentID,
         A.2..Select.the.name.of.your.Erasmus.Mundus.Joint.Master.Degree..EMJMD..,
         University.1,
         starts_with("I.1.3"),
         starts_with("I.3.2"))
second_university <- dataset %>%
  select(RespondentID,
         A.2..Select.the.name.of.your.Erasmus.Mundus.Joint.Master.Degree..EMJMD..,
         University.2,
         starts_with("H.1.3"),
         starts_with("H.3.2"))
third_university <- dataset %>%
  select(RespondentID,
         A.2..Select.the.name.of.your.Erasmus.Mundus.Joint.Master.Degree..EMJMD..,
         University.3,
         starts_with("G.1.3"),
         starts_with("G.3.2"))
fourth_university <- dataset %>%
  select(RespondentID,
         A.2..Select.the.name.of.your.Erasmus.Mundus.Joint.Master.Degree..EMJMD..,
         University.4,
         starts_with("F.1.3"),
         starts_with("F.3.2"))

#since questions are always the same, binding four datasets together
z <- rbind(first_university, 
           setNames(second_university, names(first_university)),
           setNames(third_university, names(first_university)),
           setNames(fourth_university, names(first_university)))

#leaving only respondents with some info on which university they stayed in.
z <- z[!is.na(z$University.1),]

write.csv(z, "open_ended_per_university.csv", fileEncoding = "UTF-8")
