library(tidyverse)
library(lme4)
library(postHoc)
library(DescTools)
library(agricolae)
library(RColorBrewer)
library(sjPlot)
library(ordinal)
library(emmeans)
library(stringr)
library(openxlsx)
library(plotly)

setwd("C:\\Users\\herts\\OneDrive\\Desktop\\Manu\\connectives\\survey_data")

Sys.setlocale(category="LC_ALL", locale = "English_United States.1252")

rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

frenchData <- read.csv("frenchData.csv", fileEncoding="UTF-8-BOM")
frenchSentConds <- read.csv("french_conditions2.csv", fileEncoding="UTF-8-BOM")
colnames(frenchData)[colnames(frenchData)=="Time.spent.on.page.3"] = "Time"

table(frenchSentConds$CONDITION)

frenchData1a <- frenchData[c(1:5), c(1,4,5,7,10:89,638)]
frenchData1b <- frenchData[c(6:11), c(1,4,5,7,90:166,638)]
frenchData2a <- frenchData[c(12:16), c(1,4,5,7,167:246,638)]
frenchData2b <- frenchData[c(17:21), c(1,4,5,7,247:323,638)]
frenchData3a <- frenchData[c(22:26), c(1,4,5,7,324:403,638)]
frenchData3b <- frenchData[c(27:32), c(1,4,5,7,404:480,638)]
frenchData4a <- frenchData[c(33:38), c(1,4,5,7,481:560,638)]
frenchData4b <- frenchData[c(39:44), c(1,4,5,7,561:637,638)]

# Melt data to long format

frenchData1a <- reshape2::melt(frenchData1a, id.vars = c(1:4,85))
colnames(frenchData1a)[colnames(frenchData1a)=="variable"] = "Item"
colnames(frenchData1a)[colnames(frenchData1a)=="value"] = "Score"

frenchData1b <- reshape2::melt(frenchData1b, id.vars = c(1:4,82))
colnames(frenchData1b)[colnames(frenchData1b)=="variable"] = "Item"
colnames(frenchData1b)[colnames(frenchData1b)=="value"] = "Score"

frenchData2a <- reshape2::melt(frenchData2a, id.vars = c(1:4,85))
colnames(frenchData2a)[colnames(frenchData2a)=="variable"] = "Item"
colnames(frenchData2a)[colnames(frenchData2a)=="value"] = "Score"

frenchData2b <- reshape2::melt(frenchData2b, id.vars = c(1:4,82))
colnames(frenchData2b)[colnames(frenchData2b)=="variable"] = "Item"
colnames(frenchData2b)[colnames(frenchData2b)=="value"] = "Score"

frenchData3a <- reshape2::melt(frenchData3a, id.vars = c(1:4,85))
colnames(frenchData3a)[colnames(frenchData3a)=="variable"] = "Item"
colnames(frenchData3a)[colnames(frenchData3a)=="value"] = "Score"

frenchData3b <- reshape2::melt(frenchData3b, id.vars = c(1:4,82))
colnames(frenchData3b)[colnames(frenchData3b)=="variable"] = "Item"
colnames(frenchData3b)[colnames(frenchData3b)=="value"] = "Score"

frenchData4a <- reshape2::melt(frenchData4a, id.vars = c(1:4,85))
colnames(frenchData4a)[colnames(frenchData4a)=="variable"] = "Item"
colnames(frenchData4a)[colnames(frenchData4a)=="value"] = "Score"

frenchData4b <- reshape2::melt(frenchData4b, id.vars = c(1:4,82))
colnames(frenchData4b)[colnames(frenchData4b)=="variable"] = "Item"
colnames(frenchData4b)[colnames(frenchData4b)=="value"] = "Score"

# Rbind and merge all data 

allDataFre <- rbind(frenchData1a,frenchData1b,frenchData2a,frenchData2b,
                 frenchData3a,frenchData3b,frenchData4a,frenchData4b)
allDataFre <- merge(allDataFre,frenchSentConds, by.x = "Item", by.y = "SOS_ID")
#View(frenchSentConds)
#View(allDataFre)
write.csv(allDataFre,"C:/Users/herts/OneDrive/Desktop/french_advers_data.csv", fileEncoding = "UTF-8")
# Means by Sentence Condition

frenchCondNorms <- aggregate(Score~CONDITION, data=allDataFre,mean); 
names(frenchCondNorms) = c("CONDITION","Mean") 

frenchCondSD <- aggregate(Score~CONDITION, data=allDataFre,sd); 
names(frenchCondSD)= c("CONDITION","SD") 

frenchCondNorms <- merge(frenchCondNorms,frenchCondSD, by = "CONDITION")
#View(frenchCondNorms)

# Plot for distributions

frenchAdvDist <- ggplot(allDataFre, aes(x=Score)) +
  geom_histogram(binwidth=.5, colour="black", fill="steelblue") +
  geom_vline(aes(xintercept=mean(Score, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  ggtitle("French Adversative Distribution: Cependant")
frenchAdvDist

# Exclude participant 33 from time analysis 
freTimeData <- subset(allDataFre, Part != 33)

freTimeBox <- ggplot(timeData, aes(x = Quest, y = Time, fill = Quest)) +
  geom_boxplot() +
  ggtitle("Time to complete questionnaire")
freTimeBox

# Scores per condition
freScoreBox <- ggplot(allDataFre, aes(x = CONDITION, y = Score, fill = CONDITION)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="blue", fill="blue") +
  ggtitle("French Adversative Scores: Cependant")
ggplotly(freScoreBox)

# Conditions Model

allDataFre$Score <- as.factor(allDataFre$Score)
allDataFre$Part <- as.factor(allDataFre$Part)
write.csv(allDataFre,"C:/Users/herts/OneDrive/Desktop/french_advers_data.csv", fileEncoding = "UTF-8")

frenchAdv_clmm <- clmm(Score ~ CONDITION + Gender + (1|Part), data = allDataFre) 
summary(frenchAdv_clmm) 

emmeans(frenchAdv_clmm,pairwise ~ CONDITION | Score, mode = "prob")

##############################################
# Concessive data
##############################################

# Concessive Analysis

frenchConcData <- read.csv("frenchConcess_data.csv", fileEncoding='UTF-8-BOM')
colnames(frenchConcData)[colnames(frenchConcData)=="TIME003"] = "Time"
frenchConcConds <- read.csv("frenchConcConditions.csv", fileEncoding="UTF-8-BOM")
table(frenchConcConds$CONDITION)

# Update rows if data is added
frenchConcData1a <- frenchConcData[c(1:5), c(2,4:6,8:87,636)]
frenchConcData1b <- frenchConcData[c(6:10), c(2,4:6,88:164,636)]
frenchConcData2a <- frenchConcData[c(11:15), c(2,4:6,165:244,636)]
frenchConcData2b <- frenchConcData[c(16:19), c(2,4:6,245:321,636)]
frenchConcData3a <- frenchConcData[c(20:24), c(2,4:6,322:401,636)]
frenchConcData3b <- frenchConcData[c(25:29), c(2,4:6,402:478,636)]
frenchConcData4a <- frenchConcData[c(30:34), c(2,4:6,479:558,636)]
frenchConcData4b <- frenchConcData[c(35:38), c(2,4:6,559:635,636)]

# Melt data to long format
frenchConcData1a <- reshape2::melt(frenchConcData1a, id.vars = c(1:4,85))
colnames(frenchConcData1a)[colnames(frenchConcData1a)=="variable"] = "Item"
colnames(frenchConcData1a)[colnames(frenchConcData1a)=="value"] = "Score"

frenchConcData1b <- reshape2::melt(frenchConcData1b, id.vars = c(1:4,82))
colnames(frenchConcData1b)[colnames(frenchConcData1b)=="variable"] = "Item"
colnames(frenchConcData1b)[colnames(frenchConcData1b)=="value"] = "Score"

frenchConcData2a <- reshape2::melt(frenchConcData2a, id.vars = c(1:4,85))
colnames(frenchConcData2a)[colnames(frenchConcData2a)=="variable"] = "Item"
colnames(frenchConcData2a)[colnames(frenchConcData2a)=="value"] = "Score"

frenchConcData2b <- reshape2::melt(frenchConcData2b, id.vars = c(1:4,82))
colnames(frenchConcData2b)[colnames(frenchConcData2b)=="variable"] = "Item"
colnames(frenchConcData2b)[colnames(frenchConcData2b)=="value"] = "Score"

frenchConcData3a <- reshape2::melt(frenchConcData3a, id.vars = c(1:4,85))
colnames(frenchConcData3a)[colnames(frenchConcData3a)=="variable"] = "Item"
colnames(frenchConcData3a)[colnames(frenchConcData3a)=="value"] = "Score"

frenchConcData3b <- reshape2::melt(frenchConcData3b, id.vars = c(1:4,82))
colnames(frenchConcData3b)[colnames(frenchConcData3b)=="variable"] = "Item"
colnames(frenchConcData3b)[colnames(frenchConcData3b)=="value"] = "Score"

frenchConcData4a <- reshape2::melt(frenchConcData4a, id.vars = c(1:4,85))
colnames(frenchConcData4a)[colnames(frenchConcData4a)=="variable"] = "Item"
colnames(frenchConcData4a)[colnames(frenchConcData4a)=="value"] = "Score"

frenchConcData4b <- reshape2::melt(frenchConcData4b, id.vars = c(1:4,82))
colnames(frenchConcData4b)[colnames(frenchConcData4b)=="variable"] = "Item"
colnames(frenchConcData4b)[colnames(frenchConcData4b)=="value"] = "Score"

# Rbind and merge all data 

allConcDataFre <- rbind(frenchConcData1a,frenchConcData1b,frenchConcData2a,frenchConcData2b,
                    frenchConcData3a,frenchConcData3b,frenchConcData4a,frenchConcData4b)
allConcDataFre <- merge(allConcDataFre,frenchConcConds, by.x = "Item", by.y = "SOS_ID")
write.csv(allConcDataFre,"C:/Users/herts/OneDrive/Desktop/french_concess_data.csv", fileEncoding = "UTF-8")

# Means
frenchConcCondNorms <- aggregate(Score~CONDITION, data=allConcDataFre,mean); 
names(frenchConcCondNorms) = c("CONDITION","Mean") 

frenchConcCondSD <- aggregate(Score~CONDITION, data=allConcDataFre,sd); 
names(frenchConcCondSD) = c("CONDITION","SD") 

frenchConcCondNorms <- merge(frenchConcCondNorms,frenchConcCondSD, by = "CONDITION")
#View(frenchConcCondNorms)

# Plot for distributions

frenchConcDist <- ggplot(allConcDataFre, aes(x=Score)) +
  geom_histogram(binwidth=.5, colour="black", fill="steelblue") +
  geom_vline(aes(xintercept=mean(Score, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  ggtitle("French Concessive Distribution: Malgr� cela")
frenchConcDist

# Time plots
freTimeBox2 <- ggplot(allConcDataFre, aes(x = Quest, y = Time, fill = Quest)) +
  geom_boxplot() +
  ggtitle("Time to complete questionnaire")
freTimeBox2

# Score plots per Condition

freScoreBox2 <- ggplot(allConcDataFre, aes(x = CONDITION, y = Score, fill = CONDITION)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="blue", fill="blue") +
  ggtitle("French Concessive Scores: Malgr� cela")
ggplotly(freScoreBox2)

####################
# Conditions Model
####################

allConcDataFre$Score <- as.factor(allConcDataFre$Score)
allConcDataFre$Part <- as.factor(allConcDataFre$Part)

frenchConc_clmm <- clmm(Score~CONDITION + Gender + (1|Part), data = allConcDataFre) 
summary(frenchConc_clmm) 

emmeans(frenchConc_clmm,pairwise ~ CONDITION | Score, mode = "prob")



