library(ggplot2) 
library(dplyr) 
library(titanic) 
data_train<-titanic_train %>% mutate(Survived = as.factor(Survived), 
Pclass = as.factor(Pclass), 
Sex = as.factor(Sex), 
Embarked = as.factor(Embarked), 
SibSp = as.numeric(SibSp)) 


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
ggplot(data_train, aes(x = factor(Family), y = as.numeric(as.character(Survived)))) + 
        stat_summary( fun.y = mean, ymin=0, ymax=1, geom="bar", size=4, fill= colours[2]) +
        xlab("relatives on board") + ylab("survived perentage") + facet_grid(Sex ~ .)
