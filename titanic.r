library(ggplot2) 
library(dplyr) 
library(titanic) 
library(corrplot) 
data_train<-titanic_train %>% mutate(Survived = as.factor(Survived), 
Pclass = as.factor(Pclass), 
Sex = as.factor(Sex), 
Embarked = as.factor(Embarked), 
SibSp = as.numeric(SibSp)) 
require(car)
data_train$Cabin <- recode(data_train$Cabin, "'' = NA")
data_train$Embarked <- recode(data_train$Embarked, "'' = NA")
require(stringr) 
data_train$Title <-data_train$Name %>% str_extract(., "\\w+\\.") %>% str_sub(.,1, -2) 
mean_title <- data_train %>% group_by(Title) %>% 
summarise(count = n(), Missing = sum(is.na(Age)), Mean = round(mean(Age, na.rm = T), 2)) 
impute.mean <- function (impute_col, filter_var, var_levels) { 
for (lev in var_levels) { 
impute_col[(filter_var == lev) & is.na(impute_col)] <- 
mean(impute_col[filter_var == lev], na.rm = T) 
} 
return (impute_col) 
} 
data_train$Age <- impute.mean(data_train$Age, data_train$Title, c("Dr", "Master", "Mrs", "Miss", "Mr")) 
summary(data_train$Age) 

data_train$Fare[data_train$Fare == 0] <- NA 
data_train$Fare <- impute.mean(data_train$Fare, data_train$Pclass, as.numeric(levels(data_train$Pclass))) 
library(colorspace) 
colours <- rainbow_hcl(4, start = 30, end = 300) 
change.titles <- function(data, old_title, new_title) {
  for (title in old_title) {
    data$Title[data$Title == title] <- new_title
  }
  return (data$Title)
}
data_train$Title <- change.titles(data_train, 
                               c("Capt", "Col", "Don", "Dr", 
                               "Jonkheer", "Lady", "Major", 
                               "Rev", "Sir", "Countess"),
                               "Aristocratic")
data_train$Title <- change.titles(data_train, c("Ms"), 
                               "Mrs")
data_train$Title <- change.titles(data_train, c("Mlle", "Mme"), "Miss")
data_train$Title <- as.factor(data_train$Title)
Surv_rate_family <- data_train %>% group_by(Family = SibSp + Parch) %>% 
        summarise(Rate = mean(as.numeric(as.character(Survived))))
data_train$Family <- data_train$SibSp + data_train$Parch
data_train$isFamily <- as.factor(as.numeric(data_train$Family > 0))
data_train$isCabin <- factor(ifelse(is.na(data_train$Cabin),0,1))

# Графики

ggplot(data_train, aes(x = factor(Pclass, labels = c("first", "second", "third")), 
                       y = Age, fill = factor(Pclass))) + 
        geom_boxplot() + scale_fill_manual (values=colours) + 
        ylab("Age") + xlab("Class of cabin room") + guides(fill=FALSE)

ggplot(data_train, aes(x = factor(Title, 
                                  c("Aristocratic", "Mrs", "Mr", "Miss", "Master")), 
                       y = Age)) + geom_boxplot(fill= colours[3]) + guides(fill=FALSE) +
        guides(fill=guide_legend(title=NULL)) + ylab("Age") + xlab(NULL)

ggplot(data_train, aes(x = SibSp, y = Parch, 
                       color = factor(Survived, labels = c("dead", "Survived")))) + 
        geom_point(shape = 1, size = 4, 
                   position=position_jitter(width=0.3,height=.3)) +
        guides(color=guide_legend(title=NULL)) + 
        xlab("siblings") + 
        ylab("parents , children")

ggplot(Surv_rate_family, aes(x = as.factor(Family), y = Rate)) + 
        geom_bar(stat = "identity", width=.6, fill= colours[3]) + xlab("availability of a cabin room")+ ylab("survive percentage") 

ggplot(data_train, aes(x = factor(Family), y = as.numeric(as.character(Survived)))) + 
        stat_summary( fun.y = mean, ymin=0, ymax=1, geom="bar", size=4, fill= colours[2]) +
        xlab("relatives on board") + ylab("survive perentage") + facet_grid(Sex ~ .)

ggplot(data_train, aes(x = factor(isFamily, labels =c("no", "yes")), y = as.numeric(as.character(Survived)))) +
        stat_summary( fun.y = "mean", geom="bar", ymin=0, ymax=1, fill= colours[2]) + 
        facet_grid(Pclass ~ Sex) + ylab("survive percentage") + xlab("relatives on boat")

ggplot( data_train, aes(x=factor(isCabin, labels =c("no", "yes")),y=as.numeric(as.character(Survived))) ) +
        stat_summary( fun.y = mean, ymin=0, ymax=1, geom="bar", size=4, fill= colours[3]) + 
        ylab("survive percentage") + xlab("availability of a cabin room")

ggplot(data_train, aes(x = factor(isCabin, labels =c("no", "yes")), y = as.numeric(as.character(Survived)))) +
        stat_summary( fun.y = "mean", geom="bar", ymin=0, ymax=1, fill= colours[3]) + 
        facet_grid(Pclass ~ Sex) + ylab("survive percentage") + xlab("availability of a cabin room")

corplot_data <- data_train %>% 
        select(Survived, Pclass, Sex, Age, Fare, Embarked, Family, isFamily, isCabin) %>%
        mutate(Survived = as.numeric(Survived), Pclass = as.numeric(Pclass),
               Sex = as.numeric(Sex), Embarked = as.numeric(Embarked),
               isFamily = as.numeric(isFamily), isCabin = as.numeric(isCabin))
corr_train_data <- cor(corplot_data, use = "na.or.complete")
corrplot(corr_train_data, 
            upper.panel="number", mar=c(1,2,1,1), main='Correlation between symptoms')


#сама Модель 

data_train2=data_train 
require(plyr) 
require(dplyr) 
data_train2$Survived = revalue(data_train2$Survived, c("0"="Died", "1" = "Survived")) 
data_train2$Pclass =revalue(data_train2$Pclass, c("1"="First", "2"="Second", "3"="Third")) 
data_train2$Sex = revalue(data_train2$Sex , c("female"="Female", "male"="Male")) 
data_train2$isFamily =revalue(data_train2$isFamily, c("0"="No", "1"="Yes")) 
data_train2$isCabin = revalue(data_train2$isCabin, c("0"="No", "1"="Yes")) 

require(caret) 
set.seed(111) 
split <- createDataPartition(data_train2$Survived, p = 0.8, list = FALSE) 
train <- slice(data_train2, split) 
test <- slice(data_train2, -split) 

cv_ctrl <- trainControl(method = "repeatedcv", repeats = 10, 
summaryFunction = twoClassSummary, 
classProbs = TRUE) 

set.seed(111) 
glm.tune.1 <- train(Survived ~ Pclass + Sex + isCabin, 
data = train, 
method = "glm", 
metric = "ROC", 
trControl = cv_ctrl) 
glm.tune.1 



confusionMatrix(data = predict(glm.tune.1, newdata=test), test$Survived )
