sleep <- read.csv(file="~/Documents/github/mySleepAnalysis/sleepdata.csv", header=T, sep=";")

#split end time
sleep$End <- as.character(sleep$End)

#get the date
sleep$Date <- sapply(strsplit(sleep$End, " "),"[[",1)
sleep$Date <- strptime(sleep$Date, format="%Y-%m-%d")

#fix sleep quality
sleep$Sleep.quality <- as.integer(gsub("%", "", as.character(sleep$Sleep.quality)))

#convert Time in bed to minutes
StringToMinutes <- function(x) {
  # Converts a String with format HH:MM:SS to a decimal minutes representation
  #
  # Args:
  #   x: String with format HH:MM
  #
  # Returns:
  #   Float minutes
  x <- as.numeric(x)
  x[1]*60 + x[2]
}

sleep$Time.in.bed <- sapply(strsplit(as.character(sleep$Time.in.bed), ":"), StringToMinutes)

#correlation sleep quality and time in bed
library(ggplot2)
p <- ggplot(sleep, aes(Time.in.bed, Sleep.quality)) + theme_bw()
p <- p + geom_point(aes(colour=Sleep.Notes)) + ggtitle("Sleep Quality vs. Time in Bed") + xlab("Time in Bed in Minutes") + ylab("Sleep Quality in %")
p + geom_smooth(method=lm)

#plot sleep duration over time
p <- ggplot(sleep, aes(Date, Time.in.bed)) + theme_bw()
p <- p + geom_point(aes(colour=Sleep.quality), size=3.5) + scale_colour_gradient(limits=c(30, 100), low="red", high="green", space="Lab") 
p <- p + ggtitle("Sleep Duration Over Time") + xlab("Date") + ylab("Duration in Minutes")
p + geom_smooth(method=loess)

#group sleep quality by month
library(lubridate)
df <- data.frame(
  date = sleep$Date,
  x = sleep$Sleep.quality
)
df$my <- floor_date(df$date, "month")

library(plyr)
group <- ddply(df, "my", summarise, x = mean(x))

p <- ggplot(group, aes(my, x)) + theme_bw()
p <- p + ggtitle("Sleep Quality by Month Time in Bed") + xlab("Month") + ylab("Sleep Quality in %")
p + geom_bar(stat="identity", colour="black", fill="lightblue")
