library(dplyr)
library(readr)
### For qualitative team
dataset <- read_csv("../../Media/2016/Master_tables/bigtable.csv")
qualitative_df <- dataset %>%
  select(RespondentID,
         A.2..Select.the.name.of.your.Erasmus.Mundus.Joint.Master.Degree..EMJMD..,
         A.2..Select.the.name.of.your.Erasmus.Mundus.Joint.Master.Degree..EMJMD_Other..please.specify.,
         value.University.1,
         value.University.2,
         value.University.3,
         value.University.4,
         starts_with("A.4.."),
         A.8..Gender.,
         A.9..Have.you.been.awarded.an.Erasmus.Mundus.scholarship.for.your.master.studies.,
         contains("Open.Ended.Response"))

qualitative_df$A.1..Please.create.a.unique.identification.code..Just.type.in.your.birthday.in.the.format.DD.MO.YY.followed.by.the.first.two.letters.of.your.first.name.and.first.two.letter.of.your.last.name..For.example..Maris.Miller.was.born.on.October.9..1975..So.her.code.would.be.091075MAMI..Your.answer.to.this.question.will.remain.anonymous.and.will.not.be.shared.with.your.EMJMD_Open.Ended.Response <- NULL
write.csv(x = qualitative_df, file = "../../Media/2016/Master_tables/qualitative_team.csv", fileEncoding = "UTF-8")


### For Patric
bigtable$X <- NULL
bigtable$B.2.2.a.If.you.feel.comfortable.describe.any.inappropriate.conduct.or.sexual.harassment.issues.you.have.witnessed.or.have.been.the.subject.of.and.the.support.you.have.received.The.answers.to.this.question.will.not.be.shared.with.Erasmus.Mundus.course._Open.Ended.Response <- NULL
bigtable$B.2.2.Rate.the.support.received.on.the.following.issues._Inappropriate.conduct.or.sexual.harassment.issues <- NULL

write.csv(x = bigtable, file = "../Media/2015/Master_tables/patrick.csv")
