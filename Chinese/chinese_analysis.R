library(tidyverse)
library(postHoc)
library(DescTools)
library(agricolae)
library(RColorBrewer)
library(sjPlot)
library(ordinal)
library(emmeans)
library(openxlsx)
library(rmarkdown)

rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

setwd("C:\\Users\\herts\\OneDrive\\Desktop\\Manu\\connectives\\Chinese")

#chineseData <- read.csv("Results_1234.csv")
chineseEvenSo <- read.csv("Results_1234_evenSo.csv")
chineseEvenSo[, "Value"]  <- as.numeric(chineseEvenSo[, "Value"])
chineseEvenSo[, "ReactionTime"]  <- as.numeric(chineseEvenSo[, "ReactionTime"])
chineseEvenSo[, "TrialNumber"]  <- as.numeric(chineseEvenSo[, "TrialNumber"])
chineseEvenSo[, "Participant"]  <- as.numeric(chineseEvenSo[, "Participant"])
#View(chineseEvenSo)

# Replace Condition labels for consistency with other sets
chineseEvenSo$Condition[chineseEvenSo$Condition == "Plausible"] = "PLAUS_NOCONN"
chineseEvenSo$Condition[chineseEvenSo$Condition == "Implausible"] = "IMPLAUS_NOCONN"
chineseEvenSo$Condition[chineseEvenSo$Condition == "Evenso-plausible"] = "PLAUS_CONN"
chineseEvenSo$Condition[chineseEvenSo$Condition == "Evenso-implausible"] = "IMPLAUS_CONN"

# Remove NA rows to only look at coherence scores

coherenceScores <- na.omit(chineseEvenSo)
colnames(coherenceScores)[colnames(coherenceScores)=="Value"] = "Score"
#View(coherenceScores)

# Means by Sentence Condition
chineseCondNorms <- aggregate(Score~Condition, data=coherenceScores,mean); 
names(chineseCondNorms) = c("Condition","Mean") 

chineseCondSD <- aggregate(Score~Condition, data=coherenceScores,sd); 
names(chineseCondSD)= c("Condition","SD") 

chineseCondNorms <- merge(chineseCondNorms,chineseCondSD, by = "Condition")
View(chineseCondNorms)

# Distribution
chinDist <- ggplot(coherenceScores, aes(x=Score)) +
  geom_histogram(binwidth=.5, colour="black", fill="steelblue") +
  geom_density(aes(x=Score)) +
  geom_vline(aes(xintercept=mean(Score, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  ggtitle("Chinese Adversative Distribution: Even-so")
chinDist

# Density curve
ggplot(coherenceScores, aes(x=Score)) + geom_density()

#Box plots
chinscoreBox <- ggplot(coherenceScores, aes(x = Condition, y = Score, fill = Condition)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="blue", fill="blue") +
  ggtitle("Chinese Adversative Scores") 
ggplotly(chinscoreBox)

####################
# Models
####################

coherenceScores$Value <- as.factor(coherenceScores$Value)
coherenceScores$Participant <- as.factor(coherenceScores$Participant)

chinclmmModel <- clmm(Value~Condition + (1|Participant), data = coherenceScores) 
summary(chinclmmModel) 

emmeans(chinclmmModel,pairwise ~ Condition | Value, mode = "prob")


