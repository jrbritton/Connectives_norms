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

chineseData <- read.csv("Results_1234.csv")
View(chineseData)

# Remove NA rows to only look at coherence scores

coherenceScores <- na.omit(chineseData)
View(coherenceScores)
table(coherenceScores$TrialNumber)


# Means by Sentence Condition
chineseCondNorms <- aggregate(Value~Condition, data=coherenceScores,mean); 
names(chineseCondNorms) = c("Condition","Mean") 

chineseCondSD <- aggregate(Value~Condition, data=coherenceScores,sd); 
names(chineseCondSD)= c("Condition","SD") 

chineseCondNorms <- merge(chineseCondNorms,chineseCondSD, by = "Condition")
View(chineseCondNorms)

# Distribution
chinDist <- ggplot(coherenceScores, aes(x=Value)) +
  geom_histogram(binwidth=.5, colour="black", fill="steelblue") +
  geom_density(aes(x=Value)) +
  geom_vline(aes(xintercept=mean(Value, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  ggtitle("Distribution: Chinese ('Even-so')")

chinDist

ggplot(coherenceScores, aes(x=Value)) + geom_histogram(binwidth=.5)

# Density curve
ggplot(coherenceScores, aes(x=Value)) + geom_density()

#Box plots
chinscoreBox <- ggplot(coherenceScores, aes(x = Condition, y = Value, fill = Condition)) +
  geom_boxplot() +
  ggtitle("Chinese: Even-so")
chinscoreBox

####################
# Models
####################

coherenceScores$Value <- as.factor(coherenceScores$Value)
coherenceScores$Participant <- as.factor(coherenceScores$Participant)

chinclmmModel <- clmm(Value~Condition + (1|Participant), data = coherenceScores) 
summary(chinclmmModel) 

emmeans(chinclmmModel,pairwise ~ Condition | Value, mode = "prob")


