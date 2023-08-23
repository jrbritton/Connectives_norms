library(tidyverse)
library(lme4)
library(postHoc)
library(DescTools)
library(agricolae)
library(RColorBrewer)
library(sjPlot)
library(ordinal)
library(emmeans)
library(openxlsx)
library(gridExtra)

rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

setwd("C:\\Users\\herts\\OneDrive\\Desktop\\Manu\\connectives\\survey_data")

Sys.setlocale(category="LC_ALL", locale = "English_United States.1252")

italianData <- read.csv("italianData.csv")
italianConds <- read.csv("italianConditions.csv", encoding = "Italian")
italianConds <- italianConds[,c(-2:-4)]
#View(italianData)
#View(italianConds)
# A pie chart of age distribution
table(italianConds$CONDITION)

# ageSum <- data.frame(table(italianData$Age))
# pieColours <- c("maroon", "deepskyblue", "lightgreen", "yellow3", "purple",
#                 "brown2", "orange", "pink", "turquoise", "green4", "orange4")
# 
# agePie <- pie(ageSum$Freq, labels = ageSum$Var1, col = pieColours, border = FALSE)


# Change the values here later when more data is collected 

italianData1a <- italianData[c(1:5), c(2,4,5,7,8:87,636)]
italianData1b <- italianData[c(6:10), c(2,4,5,7,88:164,636)]
italianData2a <- italianData[c(11:15), c(2,4,5,7,165:244,636)]
italianData2b <- italianData[c(16:20), c(2,4,5,7,245:321,636)]
italianData3a <- italianData[c(21:25), c(2,4,5,7,322:401,636)]
italianData3b <- italianData[c(26:30), c(2,4,5,7,402:478,636)]
italianData4a <- italianData[c(31:35), c(2,4,5,7,479:558,636)]
italianData4b <- italianData[c(36:40), c(2,4,5,7,559:635,636)]

# This function will melt data to long format. 
# Specify the df and the columns to transpose as a numeric vector.
# Requires reshape2 / tidyverse
meltFunc <- function(df, cols){
  df <- reshape2::melt(df, id.vars = c(cols))
}

print(testvec)
testvec <- c(1:4,85)
test <- meltFunc(italianData1a, testvec)
View(test)  
# Melt data to long format
italianData1a <- reshape2::melt(italianData1a, id.vars = c(1:4,85))
colnames(italianData1a)[colnames(italianData1a)=="variable"] = "Item"
colnames(italianData1a)[colnames(italianData1a)=="value"] = "Score"

italianData1b <- reshape2::melt(italianData1b, id.vars = c(1:4,82))
colnames(italianData1b)[colnames(italianData1b)=="variable"] = "Item"
colnames(italianData1b)[colnames(italianData1b)=="value"] = "Score"

italianData2a <- reshape2::melt(italianData2a, id.vars = c(1:4,85))
colnames(italianData2a)[colnames(italianData2a)=="variable"] = "Item"
colnames(italianData2a)[colnames(italianData2a)=="value"] = "Score"

italianData2b <- reshape2::melt(italianData2b, id.vars = c(1:4,82))
colnames(italianData2b)[colnames(italianData2b)=="variable"] = "Item"
colnames(italianData2b)[colnames(italianData2b)=="value"] = "Score"

italianData3a <- reshape2::melt(italianData3a, id.vars = c(1:4,85))
colnames(italianData3a)[colnames(italianData3a)=="variable"] = "Item"
colnames(italianData3a)[colnames(italianData3a)=="value"] = "Score"

italianData3b <- reshape2::melt(italianData3b, id.vars = c(1:4,82))
colnames(italianData3b)[colnames(italianData3b)=="variable"] = "Item"
colnames(italianData3b)[colnames(italianData3b)=="value"] = "Score"

italianData4a <- reshape2::melt(italianData4a, id.vars = c(1:4,85))
colnames(italianData4a)[colnames(italianData4a)=="variable"] = "Item"
colnames(italianData4a)[colnames(italianData4a)=="value"] = "Score"

italianData4b <- reshape2::melt(italianData4b, id.vars = c(1:4,82))
colnames(italianData4b)[colnames(italianData4b)=="variable"] = "Item"
colnames(italianData4b)[colnames(italianData4b)=="value"] = "Score"

# Rbind and merge all data 

allData <- rbind(italianData1a,italianData1b,italianData2a,italianData2b,
                 italianData3a,italianData3b,italianData4a,italianData4b)

#View(allData)

allData <- merge(allData,italianConds, by.x = "Item", by.y = "SOS_ID")

# Means by Sentence Condition
#View(allData)
italianCondNorms <- aggregate(Score~CONDITION, data=allData,mean); 
names(italianCondNorms) = c("CONDITION","Mean") 

italianCondSD <- aggregate(Score~CONDITION, data=allData,sd); 
names(italianCondSD)= c("CONDITION","SD") 

italianCondNorms <- merge(italianCondNorms,italianCondSD, by = "CONDITION")
#View(italianCondNorms)

# Table theme
tableTheme <- ttheme_minimal(
  core=list(bg_params = list(fill = "lightblue")),
            fg_params=list(fontface=3),
            colhead=list(fg_params=list(col="steelblue", fontface=4L)),
  core=list(fg_params=list(hjust=0, x=0.1)),
  rowhead=list(fg_params=list(hjust=0, x=0)))

# Plot table for norms
normsTable <- tableGrob(italianCondNorms, theme=tableTheme)
grid.arrange(normsTable)

#write.csv(italianCondNorms,"C:/Users/herts/OneDrive/Desktop/condNorms.csv", fileEncoding = "UTF-8")

# Means by Word

italianWordNorms <- aggregate(Score~TARGET_WORD, data=allData,mean); 
names(italianWordNorms) = c("TARGET_WORD","Mean") 

italianWordSD <- aggregate(Score~TARGET_WORD, data=allData,sd); 
names(italianWordSD)= c("TARGET_WORD","SD") 

italianWordNorms <- merge(italianWordNorms,italianWordSD, by = "TARGET_WORD")
#View(italianWordNorms)

condsWords <- italianConds[,c(2,3)]

# sc1 <- subset(allData, CONDITION == "PLAUS_NOCONN")
# sc2 <- subset(allData, CONDITION == "IMPLAUS_NOCONN")
# sc3 <- subset(allData, CONDITION == "PLAUS_CONN")
# sc4 <- subset(allData, CONDITION == "IMPLAUS_CONN")

# Add a column for all the participant data

italianData$all <- "Parts"

# Create a vector of 'a' and 'b' to add to italianData for each quest type

questAB <- c(rep('a',5),rep('b',5),rep('a',5),rep('b',5),rep('a',5),
            rep('b',5),rep('a',5),rep('b',5))

italianData$questAB <- questAB

# Plot for distributions
#view(allData)
ggplot(allData, aes(x=Score)) + geom_histogram(binwidth=.5)
# qplot(dat$rating, binwidth=.5)

# Draw with black outline, white fill
ggplot(allData, aes(x=Score)) +
  geom_histogram(binwidth=.5, colour="black", fill="steelblue") +
  geom_density(aes(x=Score)) +
  geom_vline(aes(xintercept=mean(Score, na.rm=T)),   # Ignore NA values for mean
           color="red", linetype="dashed", size=1) +
  ggtitle("Distribution: Adversative ('Tuttavia')")

# Density curve
ggplot(allData, aes(x=Score)) + geom_density()

timeBox <- ggplot(italianData, aes(x = Quest, y = Time, fill = Quest)) +
  geom_boxplot() +
  ggtitle("Time to complete questionnaire")
timeBox
View(italianData)
#timeNorms <- aggregate(Time~Quest, data=italianData,mean); 
#names(timeNorms) = c("Quest","Mean") 

#write.csv(timeNorms,"C:/Users/herts/OneDrive/Desktop/timeNorms.csv", fileEncoding = "UTF-8")

questTimeBox <- ggplot(italianData, aes(x = questAB, y = Time, fill = questAB)) +
  geom_boxplot() +
  ggtitle("Time to complete questionnaire")
questTimeBox

scoreBox <- ggplot(allData, aes(x = CONDITION, y = Score, fill = CONDITION)) +
  geom_boxplot() +
  ggtitle("Sentence Conditions: Adversative ('Tuttavia')")
scoreBox


#####################
# Outliers
#####################

# Implaus_NoConn

impNoConn_outliers <- subset(allData, CONDITION == "IMPLAUS_NOCONN")
impNoConn_outliers <- subset(impNoConn_outliers, Score > 4)
impNoConn_outliers <- subset(impNoConn_outliers, Time < 800)
#View(impNoConn_outliers)

table(impNoConn_outliers$Part)
# Plaus_NoConn

plausNoConn_outliers <- subset(allData, CONDITION == "PLAUS_NOCONN")
plausNoConn_outliers <- subset(plausNoConn_outliers, Score < 5)
plausNoConn_outliers <- subset(plausNoConn_outliers, Time < 800)
#View(plausNoConn_outliers)


# Conditions model
# 
# condModel <- aov(Score ~ CONDITION, data = allData)
# summary(condModel)
# 
# condTukeyTest <- TukeyHSD(condModel)
# condTukeyTest
# 
# condTukeyTest2 <- HSD.test(condModel, "CONDITION", group = FALSE)
# condTukeyTest2 
# 
# par(mfrow=c(2,2))
# plot(condModel)
# par(mfrow=c(1,1))
# 
# tukeyPlot <- TukeyHSD(condModel)
# plot(tukeyPlot, las = 1)
# 
# png(filename="tukeplot.png", width=900, bg="white")
# par(mar=c(5,6,4,1)+.1)
# 
# tukeyPlot <- TukeyHSD(condModel)
# axis(2, at=1:3, las=1, cex.axis=1.3)
# dev.off()

# High and low scores for Implaus_NoConn and Plaus_NoConn

impNoConnHi <- subset(allData, CONDITION == "IMPLAUS_NOCONN" & Score >= 4)

plausNoConnLo <- subset(allData, CONDITION == "PLAUS_NOCONN" & Score <= 4)

Sys.setlocale(category="LC_ALL", locale = "English_United States.1252")

italianAllRotas <- read.csv("italianAllRotas.csv")
italianAllRotas <- subset(italianAllRotas, select = c(1:5))
View(italianAllRotas)
impNoConnHi <- merge(italianAllRotas,impNoConnHi, by.x = "SOS_ID", 
                     by.y = "Item")

plausNoConnLo <- merge(italianAllRotas,plausNoConnLo, by.x = "SOS_ID", 
                       by.y = "Item")
View(italianConds)
#write.csv(plausNoConnLo,"C:/Users/herts/OneDrive/Desktop/impNoConn_low.csv", fileEncoding = "UTF-8")

# allData[allData == "SC1"] <- 1
# allData[allData == "SC2"] <- 2
# allData[allData == "SC3"] <- 3
# allData[allData == "SC4"] <- 4

# allData$Score <- as.numeric(allData$Score)
# allData$SENT_COND <- as.numeric(allData$SENT_COND)
# Binomial model

# allData$Score[allData$Score == 1] <- "Very Incoherent"
# allData$Score[allData$Score == 2] <- "Incoherent"
# allData$Score[allData$Score == 3] <- "Somewhat Incoherent"
# allData$Score[allData$Score == 4] <- "Neutral"
# allData$Score[allData$Score == 5] <- "Somewhat Coherent"
# allData$Score[allData$Score == 6] <- "Coherent"
# allData$Score[allData$Score == 7] <- "Very Coherent"

allData$Score <- as.factor(allData$Score)
allData$Part <- as.factor(allData$Part)

itaclmmModel <- clmm(Score~CONDITION + Gender + (1|Part), data = allData) 
summary(itaclmmModel) 

emmeans(itaclmmModel,pairwise ~ CONDITION | Score, mode = "prob")

# Calculate clause lengths

italianConds <- read.csv("italianConditions.csv", encoding = "Italian")

# Counting words in a sentence
sent_len = c()  # Empty vector

# Counting words in a sentence

# strsplit - each element of a vector becomes a vector
# outputs to a list which is indexed by [[1]]

for (var in italianConds$SENTENCE){
  var = length(strsplit(var, " ")[[1]]) # " " space between words
  it = var     # 'it' is the value of var after one iteration
  sent_len = c(sent_len, it) # add the value of it to empty vector
}

sent_len = data.frame(sent_len)  # make for loop output into a data frame
View(sent_len)
sent_lengths <- data.frame(table(sent_len))
View(sent_lengths)

colnames(sent_lengths) <- c("WordCount", "SentNum")
italianConds <- cbind(italianConds,sent_len)
View(italianConds)

#write.xlsx(sent_lengths,"C:/Users/herts/OneDrive/Desktop/sent_lengths.xlsx", 
#           colNames = TRUE)
#write.xlsx(italianAllRotas,"C:/Users/herts/OneDrive/Desktop/allItalianRotas.xlsx", 
#           colNames = TRUE)

# Subset sent lengths by word count

words18 <- subset(italianConds, sent_len == 18)
words19 <- subset(italianConds, sent_len == 19)
words20 <- subset(italianConds, sent_len == 20)
words21 <- subset(italianConds, sent_len == 21)
words22 <- subset(italianConds, sent_len == 22)
words23 <- subset(italianConds, sent_len == 23)
words24 <- subset(italianConds, sent_len == 24)
words25 <- subset(italianConds, sent_len == 25)
words26 <- subset(italianConds, sent_len == 26)

allwords <- rbind(words18,words19,words20,words21,words22,words23,words24,words25,words26)

write_excel_csv(allwords, delim = ",", "C:/Users/herts/OneDrive/Desktop/allwords.csv") 

clauses2 <- read.csv("clauses2.csv")
table(clauses2$CONDITION)

##############################################
# Concessive Data
##############################################

italianConcess_Data <- read.csv("italianConcess_Data.csv")
italianConds <- read.csv("italianConditions.csv", encoding = "Italian")
italianConds <- italianConds[,c(-2:-4)]
View(italianConcess_Data)


italianConcData1a <- italianConcess_Data[c(1:5), c(2,4,5,7,8:87,636)]
italianConcData1b <- italianConcess_Data[c(6:9), c(2,4,5,7,88:164,636)]
italianConcData2a <- italianConcess_Data[c(10:15), c(2,4,5,7,165:244,636)]
italianConcData2b <- italianConcess_Data[c(16:20), c(2,4,5,7,245:321,636)]
italianConcData3a <- italianConcess_Data[c(21:24), c(2,4,5,7,322:401,636)]
italianConcData3b <- italianConcess_Data[c(25:30), c(2,4,5,7,402:478,636)]
italianConcData4a <- italianConcess_Data[c(31:35), c(2,4,5,7,479:558,636)]
italianConcData4b <- italianConcess_Data[c(36:40), c(2,4,5,7,559:635,636)]

# Melt data to long format

italianConcData1a <- reshape2::melt(italianConcData1a, id.vars = c(1:4,85))
colnames(italianConcData1a)[colnames(italianConcData1a)=="variable"] = "Item"
colnames(italianConcData1a)[colnames(italianConcData1a)=="value"] = "Score"

italianConcData1b <- reshape2::melt(italianConcData1b, id.vars = c(1:4,82))
colnames(italianConcData1b)[colnames(italianConcData1b)=="variable"] = "Item"
colnames(italianConcData1b)[colnames(italianConcData1b)=="value"] = "Score"

italianConcData2a <- reshape2::melt(italianConcData2a, id.vars = c(1:4,85))
colnames(italianConcData2a)[colnames(italianConcData2a)=="variable"] = "Item"
colnames(italianConcData2a)[colnames(italianConcData2a)=="value"] = "Score"

italianConcData2b <- reshape2::melt(italianConcData2b, id.vars = c(1:4,82))
colnames(italianConcData2b)[colnames(italianConcData2b)=="variable"] = "Item"
colnames(italianConcData2b)[colnames(italianConcData2b)=="value"] = "Score"

italianConcData3a <- reshape2::melt(italianConcData3a, id.vars = c(1:4,85))
colnames(italianConcData3a)[colnames(italianConcData3a)=="variable"] = "Item"
colnames(italianConcData3a)[colnames(italianConcData3a)=="value"] = "Score"

italianConcData3b <- reshape2::melt(italianConcData3b, id.vars = c(1:4,82))
colnames(italianConcData3b)[colnames(italianConcData3b)=="variable"] = "Item"
colnames(italianConcData3b)[colnames(italianConcData3b)=="value"] = "Score"

italianConcData4a <- reshape2::melt(italianConcData4a, id.vars = c(1:4,85))
colnames(italianConcData4a)[colnames(italianConcData4a)=="variable"] = "Item"
colnames(italianConcData4a)[colnames(italianConcData4a)=="value"] = "Score"

italianConcData4b <- reshape2::melt(italianConcData4b, id.vars = c(1:4,82))
colnames(italianConcData4b)[colnames(italianConcData4b)=="variable"] = "Item"
colnames(italianConcData4b)[colnames(italianConcData4b)=="value"] = "Score"

# Rbind and merge all data 

allConcData <- rbind(italianConcData1a,italianConcData1b,italianConcData2a,italianConcData2b,
                 italianConcData3a,italianConcData3b,italianConcData4a,italianConcData4b)

allConcData <- merge(allConcData,italianConds, by.x = "Item", by.y = "SOS_ID")
View(allConcData)

# Means by Sentence Condition

concCondNorms <- aggregate(Score~CONDITION, data=allConcData,mean); 
names(concCondNorms) = c("CONDITION","Mean") 

concCondSD <- aggregate(Score~CONDITION, data=allConcData,sd); 
names(concCondSD)= c("CONDITION","SD") 

concCondNorms <- merge(concCondNorms,concCondSD, by = "CONDITION")
View(concCondNorms)

# Plot for distributions

# Draw with black outline, white fill
ggplot(allConcData, aes(x=Score)) +
  geom_histogram(binwidth=.5, colour="black", fill="steelblue") +
  geom_density(aes(x=Score)) +
  geom_vline(aes(xintercept=mean(Score, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  ggtitle("Distribution: Concessive ('Nonostante cio')")

# Density curve
ggplot(allConcData, aes(x=Score)) + geom_density()

timeBox <- ggplot(allConcData, aes(x = Quest, y = Time, fill = Quest)) +
  geom_boxplot() +
  ggtitle("Time to complete questionnaire")
timeBox

#timeNorms <- aggregate(Time~Quest, data=italianData,mean); 
#names(timeNorms) = c("Quest","Mean") 

#write.csv(timeNorms,"C:/Users/herts/OneDrive/Desktop/timeNorms.csv", fileEncoding = "UTF-8")

scoreBox <- ggplot(allConcData, aes(x = CONDITION, y = Score, fill = CONDITION)) +
  geom_boxplot() +
  ggtitle("Sentence Conditions: Concessive ('Nonostante cio')")
scoreBox

# Means by Sentence Condition
concCondNorms <- aggregate(Score~CONDITION, data=allConcData,mean); 
names(concCondNorms) = c("CONDITION","Mean") 

concCondSD <- aggregate(Score~CONDITION, data=allConcData,sd); 
names(concCondSD)= c("CONDITION","SD") 

concCondNorms <- merge(concCondNorms,concCondSD, by = "CONDITION")
#View(concCondNorms)


# Clmm for Concessive data

allConcData$Score <- as.factor(allConcData$Score)
allConcData$Part <- as.factor(allConcData$Part)

concClmmModel <- clmm(Score~CONDITION + Gender + (1|Part), data = allConcData) 
summary(concClmmModel) 

emmeans(concClmmModel,pairwise ~ CONDITION | Score, mode = "prob")

# Chi-square analysis on adversative and concessive sents

View(allData)

adversData <- subset(allData, CONDITION == "PLAUS_CONN" | CONDITION == 
                       "IMPLAUS_CONN")

# Rename CONN to ADV for Adversative
adversData$CONDITION[adversData$CONDITION == "PLAUS_CONN"] = "PLAUS_ADV"
adversData$CONDITION[adversData$CONDITION == "IMPLAUS_CONN"] = "IMPLAUS_ADV"

# Same for concessive data
concData <- subset(allConcData, CONDITION == "PLAUS_CONN" | CONDITION == 
                       "IMPLAUS_CONN")

# Rename CONN to ADV for Adversative
concData$CONDITION[concData$CONDITION == "PLAUS_CONN"] = "PLAUS_CONC"
concData$CONDITION[concData$CONDITION == "IMPLAUS_CONN"] = "IMPLAUS_CONC"
# Rename education column
colnames(concData)[colnames(concData)=="Education"] = "Edu"

# R bind all data
advConcData <- rbind(adversData, concData)
advConcData$Score <- as.numeric(advConcData$Score)
View(advConcData)

median(advConcData$Score)
quantile(advConcData$scoreBox, 0.5)

table(advConcData$Score, advConcData$CONDITION)

# Bar plot
ggplot(advConcData) +
  aes(x = Score, fill = CONDITION) +
  geom_bar(position = "dodge")

ggplot(advConcData) +
  aes(x = Score, fill = CONDITION) +
  geom_bar(position = "fill")

# Make Coherent and Incoherent variables from high and low scores
# Higher or lower than the median

advConcData$Score <- ifelse(advConcData$Score < median(advConcData$Score),
                   "Incoherent", "Coherent")

plausData <- subset(advConcData, CONDITION == "PLAUS_CONC" | CONDITION ==
                      "PLAUS_ADV")
  
implausData <- subset(advConcData, CONDITION == "IMPLAUS_CONC" | CONDITION ==
                        "IMPLAUS_ADV")


# Chi-square
chi_advConc <- chisq.test(table(advConcData$Score, advConcData$CONDITION))
chi_advConc

chi_plaus <-chisq.test(table(plausData$Score, plausData$CONDITION))
chi_plaus

chi_implaus <- chisq.test(table(implausData$Score, implausData$CONDITION))
chi_implaus


# Bar Plots

ggplot(plausData) +
  aes(x = Score, fill = CONDITION) +
  geom_bar(position = "dodge")

ggplot(implausData) +
  aes(x = Score, fill = CONDITION) +
  geom_bar(position = "dodge")

ggplot(implausData) +
  aes(x = Score, fill = CONDITION) +
  geom_bar(position = "fill")





