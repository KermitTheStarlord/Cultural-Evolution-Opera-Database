# Analysis of the OperaBase database from Harvard.
# Analysis of wether the neutral model applies to this database.
# Code by Arthur Bert
# Master in Cogntive Sciences, ENS PSL, 2023
# SocSci201 Cultural Evolution, Olivier Morin.

# 1. Set-up and parameters.

library(tidyverse)
library(ggplot2)
library(folio)

rawData <- read.csv(
  "/Users/admin/Documents/Études/Cogmaster/2 Cultural Evolution/Asignment1/Data Opera/Operas_socsci.csv"
  ,header=TRUE,sep=";")

# 2. Uniformizing the Data

# Date of birth are sometimes on a YYYYMMDD basis, sometimes on a YYYY basis.
# Let's make it so that YYYY becomes YYYY0000.

for (i in 1:(nrow(rawData))){
  if (is.na(rawData[i,5])==FALSE){
    if (rawData[i,5]<10000){
      rawData[i,5] <- rawData[i,5]*10000
    }
  }
}

# Date of death are sometimes on a YYYYMMDD basis, sometimes on a YYYY basis.
# Let's make it si that YYYY becomes YYYY0000.

for (i in 1:(nrow(rawData))){
  if (is.na(rawData[i,6])==FALSE){
    if (rawData[i,6]<10000){
      rawData[i,6] <- rawData[i,6]*10000
    }
  }
}

# 3 Analysis

# 3.1. Nationality of Works over (representation) time.

# This is the size of the categorical step, in month.
# The dataset starts in June 2012, and ends in August 2018.
# Therefor the dataset spans 7 + 12*5 + 8 = 75 months.
STEPS <- 2
NUMBERSTEPS <- ceiling(75/STEPS)

# We extract the different nationalities present in the dataset.
nationality <- c(unique(rawData$Nationality.of.Work))

# We create the nationalityOverTime DataFrame
nationalityOverTime = data.frame(matrix(0, 1, 2))
colnames(nationalityOverTime) <- c("nationality","time")

for (i in 1:(nrow(rawData))){
  nat = rawData[i,"Nationality.of.Work"]
  yearRep = rawData[i,"Start.Date"]%/%10000
  if (yearRep == 2012){
    monthSinceStart = (rawData[i,"Start.Date"] - 20120000)%/%100 - 5
  } else if (yearRep == 2018){
    monthSinceStart = (rawData[i,"Start.Date"] - 20180000)%/%100 + 12*5 + 7
  } else {
    monthSinceStart = (rawData[i,"Start.Date"]%/%10000 - 2013)*12 + 7 + (rawData[i,"Start.Date"] - rawData[i,"Start.Date"]%/%10000*10000)%/%100 
  }
  
  x = substr(rawData$Nationality.of.Work[i], start=1, stop=2)
  if (!(x %in% c("at","de","fr","hu","it","ru","uk"))) {
    x <- "ot"
  }
  j = ceiling(monthSinceStart/STEPS)
  nationalityOverTime[i,] <- list(x,j)
}

# Now we plot
ggplot(data = nationalityOverTime, aes(x=time,fill=nationality)) +
  geom_bar(position="fill")

# 3.2. Is Haendel trendy ? Composers of works over time.

# This is the size of the categorical step, in month.
# The dataset starts in June 2012, and ends in August 2018.
# Therefor the dataset spans 7 + 12*5 + 8 = 75 months.
STEPS <- 2
NUMBERSTEPS <- ceiling(75/STEPS)

# We extract the most popular composers of the dataset
composers <- data.frame(table(rawData$Nationality.of.Composer))
composers <- composers[composers$Freq >= 100,]
colnames(composers) <- c("composer","Freq")

# We create the composerOverTime DataFrame
composersOverTime <- data.frame(matrix(0,1,2))

colnames(composersOverTime) <- c("composer","time")

for (i in 1:(nrow(rawData))){
  yearRep = rawData[i,"Start.Date"]%/%10000
  if (yearRep == 2012){
    monthSinceStart = (rawData[i,"Start.Date"] - 20120000)%/%100 - 5
  } else if (yearRep == 2018){
    monthSinceStart = (rawData[i,"Start.Date"] - 20180000)%/%100 + 12*5 + 7
  } else {
    monthSinceStart = (rawData[i,"Start.Date"]%/%10000 - 2013)*12 + 7 + (rawData[i,"Start.Date"] - rawData[i,"Start.Date"]%/%10000*10000)%/%100 
  }
  
  #This can be ommited to show all nationality
  x = rawData$Nationality.of.Composer[i]
  if (!(x %in% composers$composer)) {
    x <- "other"
  }
  j = ceiling(monthSinceStart/STEPS)
  composersOverTime[i,] <- list(x,j)
}



# Now we plot
ggplot(data = composersOverTime, aes(x=time,fill=composer)) +
  geom_bar(position="fill")

# 3.3. And what about individual play ?

# We extract the most popular play of the dataset
play <- data.frame(table(rawData$Name.of.Work))
play <- play[play$Freq >= 300,]
colnames(play) <- c("play","Freq")

# We create the playOverTime DataFrame
playOverTime <- data.frame(matrix(0,1,2))

colnames(playOverTime) <- c("play","time")

for (i in 1:(nrow(rawData))){
  yearRep = rawData[i,"Start.Date"]%/%10000
  if (yearRep == 2012){
    monthSinceStart = (rawData[i,"Start.Date"] - 20120000)%/%100 - 5
  } else if (yearRep == 2018){
    monthSinceStart = (rawData[i,"Start.Date"] - 20180000)%/%100 + 12*5 + 7
  } else {
    monthSinceStart = (rawData[i,"Start.Date"]%/%10000 - 2013)*12 + 7 + (rawData[i,"Start.Date"] - rawData[i,"Start.Date"]%/%10000*10000)%/%100 
  }
  x = rawData$Name.of.Work[i]
  if (!(x %in% play$play)) {
    x <- "other"
  }
  j = ceiling(monthSinceStart/STEPS)
  playOverTime[i,] <- list(x,j)
}

# Now we plot
ggplot(data = playOverTime[playOverTime$play != "other",], aes(x=time,fill=play)) +
  geom_bar(position="fill")

# 3.4. How are the different plays presented in general ?

# We extract the most popular play of the dataset
play <- data.frame(table(rawData$Name.of.Work))
play <- play[play$Freq >= 100,]
colnames(play) <- c("play","Freq")
play$Rank <- rank(play$Freq,ties.method="first")

# We plot them
ggplot(data = play, aes(x=Rank,y=log((Freq/33124)))) +
  geom_point(color=factor(ceiling(log(play$Freq))))