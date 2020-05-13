# mid-semester project

ait582_proj_data=read.csv("ait582-airline-data.csv",header=TRUE)

par(mfrow=c(1,1))
#Create the histogram bars
library(ggplot2)
library(dplyr)

#bar plot for seat class
seatclass <- c('1','2','3')
count <- c(216,184,489)

df<- data.frame(seatclass,count)
ggplot(df, aes(x=seatclass, y=count),title = 'Distribution of Seatclass') + 
  geom_bar(stat = "identity")

#barplot for gender
gender <- c('M','F')
count <- c(578,313)

df_g<- data.frame(gender,count)
ggplot(df_g, aes(x=gender, y=count),xlab="Gender", ylab=Count, title="Distribution of customers by Gender") + 
  geom_bar(stat = "identity")

#plot data skewness

m<-mean(ait582_proj_data$AGE)
std<-sqrt(var(ait582_proj_data$AGE))
hist(ait582_proj_data$AGE,prob=T,main="Age")
curve(dnorm(x, mean=m, sd=std), col="darkblue", lwd=2, add=TRUE)

#Loading the mice package
library(mice)

#Loading the following package for looking at the missing values
library(VIM)
library(lattice)

md.pattern(ait582_proj_data)

#plotmissing values
airline_miss = aggr(ait582_proj_data, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, 
                   labels=names(ait582_proj_data), cex.axis=.7, gap=3, 
                   ylab=c("Proportion of missingness","Missingness Pattern"))

#Drawing margin plot
marginplot(ait582_proj_data[, c("AGE", "CUSTOMERID")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)

#Imputing missing values using mean
df_missing = data.frame(x = 1:6, y = c(1:3,rep(NA,10)))
df_missing$y[is.na(df_missing$y)] = mean(df_missing$y, na.rm=TRUE)

# create scatter plot

qplot(ait582_proj_data$FARE,ait582_proj_data$SEATCLASS, data= ait582_proj_data, xlim=c(0,50))


