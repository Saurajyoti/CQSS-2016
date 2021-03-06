source("functions.R")
z <- dataset %>%
  dplyr::select(starts_with("B.1.1"),
         starts_with("B.1.3"),
         starts_with("B.2.1"),
         starts_with("B.2.2"),
         starts_with("C.1"), 
         C.3.Please.rate.academic.satisfaction.with.EM.course._,
         C.4.Please.rate.overall.satisfaction.with.EM.course._)
for(i in seq_along(z)) {
  ### this step will also reduce all other answers to NA's
  z[,i] <- factor(z[,i], levels = likert_levels)
}
colnames(z) <- gsub("Rate.the.information.and.support.received.before.the.start.of.Erasmus.Mundus.master.course.on.the.following.aspects.", "", colnames(z))
colnames(z) <- gsub("Rate.the.introduction.process.to.the.following.units.or.people.as.part.of.the.orientation.program.", "", colnames(z))
colnames(z) <- gsub("Rate.the.helpfulness.of.the.following.units.of.people.", "", colnames(z))
colnames(z) <- gsub("Rate.the.following.items", "", colnames(z))
colnames(z) <- gsub("Rate.the.support.received.on.the.following.issues.", "", colnames(z))

z <- z[complete.cases(z),]

z <- data.matrix(z)

z[,36] <- factor(z[,36])

#########################################################################################################################################
#PCA
fa.parallel(z, fa = "pc", n.iter = 100, show.legend = FALSE)
pc <- principal(z, nfactors = 4)

#########################################################################################################################################
#LFA
covariances <- cov(z)
correlations <- cov2cor(covariances)
fa <- fa(correlations, nfactors = 10, rotate = "none", fm = "pa")

#########################################################################################################################################
library(MASS)
library(caTools)
#LDA
spl <- sample.split(z[,36], SplitRatio = 0.7)
train <- z[spl,]
test <- z[!spl,]
lda.fit <- lda(C.4.Please.rate.overall.satisfaction.with.EM.course._ ~ ., data = train)
lda.class <- predict(lda.fit, test$C.4.Please.rate.overall.satisfaction.with.EM.course._)$class

#########################################################################################################################################
#kmeans
kmeans_z <- kmeans(z, 5)


#########################################################################################################################################
f1 <- function(x) if(sum(!is.na(x))>9) mean(as.numeric(x), na.rm=TRUE) else NA_real_

### different colors depending on scaled/not scaled
#mycolors <- brewer.pal(length(seq(1,4, by = 0.5)), "BrBG)
mycolors <- brewer.pal(length(c(-Inf,-2:2,Inf)), "BrBG")

means <- overall %>%
  select(A.2.Select.the.name.of.Erasmus.Mundus.master.course._Response.,
         starts_with(questions[1])) %>%
  group_by(A.2.Select.the.name.of.Erasmus.Mundus.master.course._Response.) %>%
  summarise_each(funs(f1))

#storing the names of the courses for future use
rownames_store <- means$A.2.Select.the.name.of.Erasmus.Mundus.master.course._Response.
means$A.2.Select.the.name.of.Erasmus.Mundus.master.course._Response. <- NULL

#logical vector to find out rows with all NA's
vector <- !!rowSums(!is.na(means)) 

means <- means[vector,] #deleting the rows with all NA's
rownames(means) <- rownames_store[vector] #writing down the names of the courses
colnames(means) <- gsub("\\.", " ", colnames(means)) #making names of questions readable
colnames(means) <- gsub("(.*?)_(.*)", "\\2", colnames(means)) #leaving just the dimension name
wrap_function_x <- wrap_format(35)
colnames(means) <- wrap_function_x(colnames(means))
wrap_function_y <- wrap_format(50)
rownames(means) <- wrap_function_y(rownames(means))

#means_matrix <- data.matrix(means)        
means_matrix <- scale(data.matrix(means))

library(blockcluster)
means_matrix <- means_matrix[complete.cases(means_matrix),]
out <- cocluster(means_matrix, datatype = "continuous", nbcocluster = c(4,2))
plot(out)
summary(out)

#########################################################################################################################################
library(caTools)
set.seed(3000)
spl <- sample.split(dataset$B.1.2.Rate.the.orientation.welcoming.program.provided.upon.arrival._, SplitRatio = 0.7)
welcoming <- dataset %>%
  select(B.1.2.Rate.the.orientation.welcoming.program.provided.upon.arrival._,
         starts_with("B.1.3"))
train <- subset(welcoming, spl == TRUE)
test <- subset(welcoming, spl != TRUE)

library(rpart)
library(rpart.plot)
stevenstree <- rpart(B.1.2.Rate.the.orientation.welcoming.program.provided.upon.arrival._ ~ ., data = welcoming, method = "class", minbucket = 25)
prp(stevenstree)
predictcart <- predict(stevenstree, newdata = test, type = "class")
table(test$B.1.2.Rate.the.orientation.welcoming.program.provided.upon.arrival._, predictcart)
