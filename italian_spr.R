library(tidyverse)

setwd("C:\\Users\\herts\\OneDrive\\Desktop\\Manu\\connectives\\spr_task")

Sys.setlocale(category="LC_ALL", locale = "English_United States.1252")

readingSents <- read.csv("readingSents.csv")
#View(readingSents)
# Calculate clause lengths

sent_len = c()  # Empty vector

for (var in readingSents$CLIP_SENT){
  var = length(strsplit(var, " ")[[1]])
  it = var     # 'it' is the value of var after one iteration
  sent_len = c(sent_len, it) # add the value of it to empty vector
}

sent_len = data.frame(sent_len)  # make for loop output into a data frame

readingSents <- cbind(readingSents, sent_len)

sent_len = c()  # Empty vector

for (var in readingSents$CLIP_TARG){
  var = length(strsplit(var, " ")[[1]])
  it = var     # 'it' is the value of var after one iteration
  sent_len = c(sent_len, it) # add the value of it to empty vector
}

sent_len = data.frame(sent_len)  # make for loop output into a data frame

readingSents <- cbind(readingSents, sent_len)
table(readingSents$CONDITION)

write.csv(readingSents,"C:/Users/herts/OneDrive/Desktop/readingSents2.csv", 
          fileEncoding = "UTF-8")

###########################
# SPR sentences
###########################

# Check counts

spr_stims <- read.csv("spr_stims.csv")
table(spr_stims$TARGET_WORD)

# Group 1

# This nested loop adds five to to counter and appends to group1Seq
# It restarts after three cycles by adding 1 to counter
# This is to create a sequence that adds five three times, and then adds one.
# Groups 1 - 4 have different values for groupSeq and counter in order to adjust 
# the starting position before the loop restarts.
# For example, group 3 starts at -7 so that we get +5 (-2), +5 (3), +5 (8), +1 (9)

group1Seq <- c(1)

counter <- 1

for(num in 1:25){
  for(cycle in 1:3){
    counter <- counter + 5
    group1Seq <- c(group1Seq, counter)
  }
  counter <- counter + 1
  group1Seq <- c(group1Seq, counter)
}

print(group1Seq)
group1Seq <- group1Seq[-(101)]
length(group1Seq) # should be 100


# Subset the columns of data based on the group vector
sentCond1 = spr_stims[c(group1Seq),]
View(sentCond1)

# Group 2 stims

group2Seq <- c()

counter2 <- -3
for(num in 1:25){
  for(cycle in 1:3){
    counter2 <- counter2 + 5
    group2Seq <- c(group2Seq, counter2)
  }
  counter2 <- counter2 + 1
  group2Seq <- c(group2Seq, counter2)
}

print(group2Seq)
length(group2Seq) # Check this is 100

sentCond2 = spr_stims[c(group2Seq),]
View(sentCond2)

# Group 3 stims

group3Seq <- c()

counter3 <- -7
for(num in 1:26){
  for(cycle in 1:3){
    counter3 <- counter3 + 5
    group3Seq <- c(group3Seq, counter3)
  }
  counter3 <- counter3 + 1
  group3Seq <- c(group3Seq, counter3)
}

print(group3Seq)
length(group3Seq)
# Remove excess numbers, including starting point (-2)
group3Seq <- group3Seq[! group3Seq %in% c(-2, 403, 408, 409)]

sentCond3 = spr_stims[c(group3Seq),]
View(sentCond3)

###############
# Group 4 stims
###############

group4Seq <- c()

counter4 <- -11
for(num in 1:26){
  for(cycle in 1:3){
    counter4 <- counter4 + 5
    group4Seq <- c(group4Seq, counter4)
  }
  counter4 <- counter4 + 1
  group4Seq <- c(group4Seq, counter4)
}

print(group4Seq)
group4Seq <- group4Seq[! group4Seq %in% c(-6, -1, 404, 405)]
length(group4Seq)

sentCond4 = spr_stims[c(group4Seq),]
View(sentCond4)

write.csv(sentCond1,"C:/Users/herts/OneDrive/Desktop/spr_group1.csv", fileEncoding = "UTF-8")
write.csv(sentCond2,"C:/Users/herts/OneDrive/Desktop/spr_group2.csv", fileEncoding = "UTF-8")
write.csv(sentCond3,"C:/Users/herts/OneDrive/Desktop/spr_group3.csv", fileEncoding = "UTF-8")
write.csv(sentCond4,"C:/Users/herts/OneDrive/Desktop/spr_group4.csv", fileEncoding = "UTF-8")


