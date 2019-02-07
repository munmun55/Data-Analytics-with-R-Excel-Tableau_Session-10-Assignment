#a
library(readxl)
#AirQualityUCI <-read_excel(unzip("E:/munmun_acadgild/acadgild data analytics/supporting files/AirQualityUCI.zip"))
AirQualityUCI <- read_excel("E:/munmun_acadgild/acadgild data analytics/supporting files/AirQualityUCI.xlsx")
View(AirQualityUCI)
dim(AirQualityUCI)
str(AirQualityUCI)

#b
library(psych)
describe(AirQualityUCI)

#c
col1<- mapply(anyNA,AirQualityUCI)
col1
summary(AirQualityUCI)
is.na(AirQualityUCI)

#or

AirQualityUCI[AirQualityUCI == -200] <- NA
View(AirQualityUCI)
library(VIM)
aggr(AirQualityUCI, col=c('pink','yellow'),
     numbers=TRUE, sortVars=TRUE,
     labels=names(AirQualityUCI), cex.axis=.7,
     gap=3, ylab=c("Missing data","Pattern"))    # graphical presentation of NAs

sapply(AirQualityUCI, function(x) sum(is.na(x)))     # count of NAs

# Variable NMHC(GT) is having 90% of missing values.
# Hence, NMHC(GT) is not considered and omitted from the data frame

AirQualityUCI$`NMHC(GT)` <- NULL

#d

names(AirQualityUCI)
AirQualityUCI$Date1 <- as.numeric(as.Date(AirQualityUCI$Date))
library(mice)
imputed <- mice(Air[,-c(1,2,4)], m=5, maxit = 5, method = 'cart', seed = 100) # impute missing values
summary(imputed)
complete <- complete(imputed) # replaces the NAs with imputed values
str(complete)
sapply(complete, function(x) sum(is.na(x)))  # check missing values

#e

summary(AirQualityUCI)
plot(AirQualityUCI$`NOx(GT)`~AirQualityUCI$`PT08.S2(NMHC)`)
plot(AirQualityUCI$`PT08.S1(CO)`~AirQualityUCI$`PT08.S3(NOx)`)
plot(AirQualityUCI$`NO2(GT)`~AirQualityUCI$`PT08.S4(NO2)`)
plot(AirQualityUCI$`PT08.S5(O3)`~AirQualityUCI$T)
#or

pairs(AirQualityUCI)    # graph
#-----------------------------------------------------------------------------
final <- complete
final$Date <- AirQualityUCI$Date
final$Time <- AirQualityUCI$Time
library(stringr)
AirQualityUCI$Time1 <- sub(".+? ", "", AirQualityUCI$Time)
AirQualityUCI$datetime <- as.POSIXct(paste(AirQualityUCI$Date, AirQualityUCI$Time1), format="%Y-%m-%d %H:%M:%S")
View(AirQualityUCI)
str(AirQualityUCI)

# f. Test relevant hypothesis for valid relations

t.test(AirQualityUCI$`CO(GT)`, AirQualityUCI$`PT08.S1(CO)`, paired = T)
t.test(AirQualityUCI$`C6H6(GT)`, AirQualityUCI$`PT08.S2(NMHC)`, paired = T)
t.test(AirQualityUCI$`NOx(GT)`, AirQualityUCI$`PT08.S3(NOx)`, paired = T)

mod <- lm(AirQualityUCI$`CO(GT)`~AirQualityUCI$Date1)
summary(mod)

mod <- lm(AirQualityUCI$`CO(GT)`~AirQualityUCI$T)
summary(mod)

mod <- lm(AirQualityUCI$`CO(GT)`~AirQualityUCI$RH)
summary(mod)

#g Create cross tabulations with derived variables

mydata<-AirQualityUCI
View(mydata) # 2-Way Frequency Table
attach(mydata)
#mytable <- table(A,B) # A will be rows, B will be columns
#mytable # print table
margin.table(mytable, 1) # A frequencies (summed over B)
prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages


range(AirQualityUCI$RH)

final <- within(AirQualityUCI,
                {
                  RHcat <- NA
                  RHcat[RH<20] <- "Very Low"
                  RHcat[RH>=20 & RH<=40] <- "Low"
                  RHcat[RH>40 & RH<=60] <- "Medium"
                  RHcat[RH>60 & RH<=80] <- "High"
                  RHcat[RH>80] <- "Very High"
                })

mytable <- xtabs(`CO(GT)` ~ +RHcat, data = final)
ftable(mytable)  # print table 
summary(mytable) # chi-square test of indepedence

mytable <- xtabs(`C6H6(GT)` ~  +RHcat, data = final)
ftable(mytable)  # print table 
summary(mytable) # chi-square test of indepedence

mytable <- xtabs(`NOx(GT)` ~  +RHcat, data = final)
ftable(mytable)  # print table 
summary(mytable) # chi-square test of indepedence

with(final, tapply(`NO2(GT)`, list(RHcat=RHcat), sd)) # using with()
with(final, tapply(`NO2(GT)`, list(RHcat=RHcat), mean))

#h. Check for trends and patterns in time series.

#plot time series
tsAirqualityUCI <- EuStockMarkets[, 1] # ts data
decomposedRes <- decompose(tsAirqualityUCI, type="mult") # use type = "additive" for additive components
plot (decomposedRes) # see plot below
stlRes <- stl(tsAirqualityUCI, s.window = "periodic")
plot(AirQualityUCI$T, type = "l")
#or
library(xts)

timeseries <- xts(final$`CO(GT)`, final$datetime)
plot(timeseries)
summary(timeseries)

ts (AirQualityUCI, frequency = 4, start = c(1959, 2))# frequency 4 =>Quarterly Data
ts (1:10, frequency = 12, start = 1990) # freq 12 => Monthly data.
ts (AirQualityUCI, start=c(2009), end=c(2014), frequency=1) # Yearly Data
ts (1:1000, frequency = 365, start = 1990) # freq 365 => daily data.

# i. Find out the most polluted time of the day and the name of the chemical compound
names(AirQualityUCI)
library(dplyr)

polluted <- AirQualityUCI%>%group_by(Time)%>%
  select(Time, `CO(GT)`, `C6H6(GT)`, `NO2(GT)`, `NOx(GT)` )%>%
  summarise(CO = mean(`CO(GT)`), C6H6 = mean(`C6H6(GT)`), NO2 = mean(`NO2(GT)`), NOX =mean(`NOx(GT)`))%>%
  
  polluted[c(which.max(polluted$CO),which.max(polluted$C6H6),which.max(polluted$NO2),which.max(polluted$NOX)),]

# 19:00:00 is the most polluted time of the day with CO, C6H6, NO2 & NOx



