library(ggplot2)
library(ggthemes)
library(scales)
library(mice)
library(dplyr)
library(randomForest)

# read data

train <- "train.csv"
test <- "test.csv"
train <- read.table(train,stringsAsFactors = F,header = T,sep = ",",na.strings = "")
test <- read.table(test,stringsAsFactors = F,header = T,sep = ",",na.strings = "")

# merge train set and test set

full <- bind_rows(train,test)

# title

full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# merge rare titles

rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
full$Title[full$Title %in% rare_title] <- 'Rare Title'

# consolidate forms of each kind of title

full$Title[full$Title == 'Mlle'] <- 'Miss'
full$Title[full$Title == 'Ms'] <- 'Miss'
full$Title[full$Title == 'Mme'] <- 'Miss'
table(full$Title, full$Survived)

# plot situation of survival of people with different titles

mosaicplot(table(full$Sex, full$Title), shade=TRUE)

# Fsize

full$Fsize <- full$SibSp + full$Parch + 1
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) + geom_bar(stat='count', position='dodge') + scale_x_continuous(breaks=c(1:11)) + labs(x = 'Family Size') + theme_few()

full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

# missing value

full[c(62, 830), 'Embarked']

# delete specified rows

embark_fare <- full %>% filter(PassengerId != 62 & PassengerId != 830)

# boxplot

ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format())
full$Embarked[c(62, 830)] <- 'C'

# 1044

full[1044, ]
ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ],
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) +
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format())
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)
# use value of red line to denote Fare of 1044

# age

par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', col='lightgreen', ylim=c(0,0.04))
full$Age <- mice_output$Age

#ggplot missing value

ggplot(full[1:891,], aes(Age, fill = factor(Survived))) +
  geom_histogram() +
  facet_grid(.~Sex)
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

# Show counts

table(full$Mother, full$Survived)
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'
table(full$Mother, full$Survived)
full$Child <- factor(full$Child)
full$Mother <- factor(full$Mother)

# training

train <- full[1:891,]
test <- full[892:1309,]
set.seed(9999)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child , data = train)
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
importance <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# plot to see the importance of each variable

ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat='identity') +
  geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust=0, vjust=0.55, size = 4, colour = 'red') + labs(x = 'Variables') +
  coord_flip()

# test

prediction <- predict(rf_model, test)

# save

solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(solution, file = 'predict.csv', row.names = FALSE)
