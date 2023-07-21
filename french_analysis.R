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

setwd("C:\\Users\\herts\\OneDrive\\Desktop\\Manu\\connectives\\survey_data")

frenchData <- read.csv("french_data.csv")
frenchSentConds <- read.csv("frenchConditions.csv", encoding = "French")
frenchSentConds <- frenchSentConds[,c(-2:-4)]

colnames(frenchData)[colnames(frenchData)=="Time.spent.on.page.3"] = "Time"

table(frenchData$Age)

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
#View(allDataFre)
allDataFre <- merge(allDataFre,frenchSentConds, by.x = "Item", by.y = "SOS_ID")
#View(frenchSentConds)
# Means by Sentence Condition

frenchCondNorms <- aggregate(Score~CONDITION, data=allDataFre,mean); 
names(frenchCondNorms) = c("CONDITION","Mean") 

frenchCondSD <- aggregate(Score~CONDITION, data=allDataFre,sd); 
names(frenchCondSD)= c("CONDITION","SD") 

frenchCondNorms <- merge(frenchCondNorms,frenchCondSD, by = "CONDITION")
View(frenchCondNorms)

# Plot for distributions

ggplot(allDataFre, aes(x=Score)) +
  geom_histogram(binwidth=.5, colour="black", fill="steelblue") +
  geom_vline(aes(xintercept=mean(Score, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

# Exclude participant 33 from time analysis 
timeData <- subset(allDataFre, Part != 33)

timeBoxFre <- ggplot(timeData, aes(x = Quest, y = Time, fill = Quest)) +
  geom_boxplot() +
  ggtitle("Time to complete questionnaire")
timeBoxFre

#timeNorms <- aggregate(Time~Quest, data=italianData,mean); 
#names(timeNorms) = c("Quest","Mean") 

#write.csv(timeNorms,"C:/Users/herts/OneDrive/Desktop/timeNorms.csv", fileEncoding = "UTF-8")

# Add a column for all the participant data

allDataFre$all <- "Parts"

questTimeBoxFre <- ggplot(allDataFre, aes(x = questAB, y = Time, fill = questAB)) +
  geom_boxplot() +
  ggtitle("Time to complete questionnaire")
questTimeBoxFre


scoreBoxFre <- ggplot(allDataFre, aes(x = CONDITION, y = Score, fill = CONDITION)) +
  geom_boxplot() +
  ggtitle("Sentence Conditions")
scoreBoxFre

# Conditions Model

allDataFre$Score <- as.factor(allDataFre$Score)
allDataFre$Part <- as.factor(allDataFre$Part)


clmmFrench <- clmm(Score~CONDITION + TARGET_WORD + (1|Part), data = allDataFre) 
summary(clmmFrench) 

emmeans(clmmFrench,pairwise ~ CONDITION | Score, mode = "prob")

#####################
# Outliers
#####################

# Implaus_NoConn

impNoConn_outliersFre <- subset(allDataFre, CONDITION == "IMPLAUS_NOCONN")
impNoConn_outliersFre <- subset(impNoConn_outliersFre, Score > 4)
#View(impNoConn_outliersFre)

# Plaus_NoConn

plausNoConn_outliersFre <- subset(allDataFre, CONDITION == "PLAUS_NOCONN")
plausNoConn_outliersFre <- subset(plausNoConn_outliersFre, Score < 5)
#View(plausNoConn_outliersFre)



##############################################
# Concessive data
##############################################

frenchConcRotas <- read.csv("frenchConcRotas.csv")

table(frenchConcRotas$CONDITION)









