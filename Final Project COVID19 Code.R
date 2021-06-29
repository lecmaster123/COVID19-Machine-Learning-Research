library("sqldf")
library("ggplot2")
library("openintro")
library("tidyverse")
library("kernlab")
library("randomForest")
library("readxl")

#https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-Jurisdi/unsk-b7fc
url <- "https://data.cdc.gov/api/views/unsk-b7fc/rows.csv?accessType=DOWNLOAD"
idkwhat <- read.csv(url)
#seriescomplete_yes means people total vacced

#wellsee <- sqldf('select * from idkwhat where Date = "06/02/2021"')

#Convert Date field to type Date
idkwhat$Date <- as.Date(idkwhat$Date, "%m/%d/%Y")
#Grab most recent date and store into new dataframe
wellsee <- idkwhat[idkwhat$Date==max(idkwhat$Date),]

#Reorder by location
wellsee <- wellsee[order(wellsee$Location)]

#Get state names based on state abbreviations
stnames <- abbr2state(wellsee$Location)

#Add in stnames field to dataframe
wellsee <- data.frame(wellsee, stnames)

#Want to look for NA states
sqldf('select Location, stnames from wellsee')

#Query to get rid of NAs for dataframe
wellsee <- sqldf('select * from wellsee where stnames != "NA"')

#Change name of columns
names(wellsee)[names(wellsee)=="Series_Complete_Yes"] <- "Total_Vaccinated"
names(wellsee)[names(wellsee)=="Series_Complete_Pop_Pct"] <- "Pct_People_Total_Vaccinated"

#Make state names lowercase
wellsee$stnames <- tolower(wellsee$stnames)

us <- map_data("state")

newmap <- ggplot(wellsee, aes(map_id=stnames), inherit.aes = FALSE)
newmap <- newmap + geom_map(map=us, aes(fill=Total_Vaccinated))
newmap <- newmap + expand_limits(x=us$long, y=us$lat)
newmap <- newmap + coord_map() + ggtitle("Fully Vaccine Dist")
newmap

#Brighter the color of state, the more a state is vaccinated
newmappercent <- ggplot(wellsee, aes(map_id=stnames), inherit.aes = FALSE)
newmappercent <- newmappercent + geom_map(map=us, aes(fill=Pct_People_Total_Vaccinated))
newmappercent <- newmappercent + expand_limits(x=us$long, y=us$lat)
newmappercent <- newmappercent + coord_map() + ggtitle("Percent of State Fully Vaccinated")
newmappercent


###############RUNNING LINEAR MODELS
########Checking out new york data and make prediction of how many total vaccines in 2022

newyorkdata <- sqldf('select * from idkwhat where location="NY"')
newyorkdata <- newyorkdata[newyorkdata$Date >= "2021-03-05",]
#Plot graph
plot(newyorkdata$Date, newyorkdata$Series_Complete_Yes)
lmyork <- lm(formula=Series_Complete_Yes~Date, newyorkdata)
#Summary says we're 91% accurate
summary(lmyork)
#Draw line of best fit
abline(lmyork)
#Predict how many people will be totally vaccinated by 2022 based on linear model
predict(lmyork, data.frame(Date=as.Date("2021-06-09")))


#testing age as a factor in total vaccinations
lmyorkold <- lm(formula=Series_Complete_Yes~Series_Complete_65Plus+Series_Complete_18Plus+Series_Complete_12Plus, newyorkdata)
summary(lmyorkold)


#Do same thing for all of US vaccination totals##########

totalVaccPredict <- sqldf('select distinct SUM(Series_Complete_Yes) as Total_Vaccine, Date from idkwhat group by Date')
totalVaccPredict <- totalVaccPredict[!totalVaccPredict$Total_Vaccine == 0,]

plot(totalVaccPredict$Date, totalVaccPredict$Total_Vaccine)
lmUS <- lm(formula=Total_Vaccine~Date, totalVaccPredict)
#Summary says we're 99% accurate
summary(lmUS)
#Draw line of best fit
abline(lmUS)
#Predict how many people will be totally vaccinated by 2022 based on linear model
predict(lmUS, data.frame(Date=as.Date("2021-07-9")))



######################USE THIS
#https://www.kff.org/other/state-indicator/total-health-care-employment/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
url4 <- "/Users/alec_arroyo/Documents/Sryacuse Data Science Courses/Introduction to Data Science/raw_data.csv"
healthwork <- read.csv(url4)

healthwork$Location <- tolower(healthwork$Location)
healthwork <- healthwork[,c(-3)]
healthmerge <- merge(wellsee, healthwork, by.x="stnames",by.y="Location")


url5 <- "/Users/alec_arroyo/Downloads/csvData.csv"
medianmoney <- read.csv(url5)
medianmoney$State <- tolower(medianmoney$State)

healthmerge <- merge(healthmerge, medianmoney, by.x="stnames",by.y="State")

#https://worldpopulationreview.com/state-rankings/crime-rate-by-state
#url6 <- "blob:https://worldpopulationreview.com/dd2b812f-fb5a-42ee-82d8-b3627be298f1"
url6 <- "/Users/alec_arroyo/Downloads/dd2b812f-fb5a-42ee-82d8-b3627be298f1.csv"
crimepop <- read.csv(url6)
crimepop$State <- tolower(crimepop$State)

healthmerge <- merge(healthmerge, crimepop, by.x="stnames",by.y="State")

#https://www.kff.org/other/state-indicator/flu-vaccination-rate/?currentTimeframe=0&selectedDistributions=flu-vaccination-rate&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D#notes
url7 <- "/Users/alec_arroyo/Documents/Sryacuse Data Science Courses/Introduction to Data Science/Flu Vaccine Stuff.xlsx"
flu <- read_excel(url7)
flu$Location <- tolower(flu$Location)

healthmerge <- merge(healthmerge, flu, by.x="stnames",by.y="Location")

names(healthmerge)[names(healthmerge)=="Total.Health.Care.Employment"] <- "Total_Healthcare_Workers_Employed"
names(healthmerge)[names(healthmerge)=="HouseholdIncome"] <- "Household_Median_Income"
names(healthmerge)[names(healthmerge)=="homicideRate2017"] <- "Crime_Rate"

plot(healthmerge$Total_Healthcare_Workers_Employed, healthmerge$Total_Vaccinated)
plot(healthmerge$Household_Median_Income, healthmerge$Total_Vaccinated)
plot(healthmerge$Crime_Rate, healthmerge$Total_Vaccinated)
plot(healthmerge$Flu_Vaccination_Rate, healthmerge$Total_Vaccinated)
lmUSDate <- lm(formula=Total_Vaccinated~Total_Healthcare_Workers_Employed+Household_Median_Income+Crime_Rate+Flu_Vaccination_Rate, healthmerge)
#Summary says we're 99% accurate
summary(lmUSDate)

#Create field for category whether total vacc is low or high (43.034% higher or lower derived from mean)
healthmerge$goodbad <- ifelse(healthmerge$Pct_People_Total_Vaccinated>43.034 ,"High", "Low")
healthmerge <- healthmerge[,-c(73)]
healthmerge$goodbad <- as.character(healthmerge$goodbad)
healthmerge$goodbad <- as.factor(healthmerge$goodbad)

#Dataset with varaibles we want to test with
randomVariables <- data.frame(healthmerge$Total_Vaccinated, healthmerge$Pct_People_Total_Vaccinated, healthmerge$Total_Healthcare_Workers_Employed, healthmerge$Household_Median_Income, healthmerge$Crime_Rate, healthmerge$Flu_Vaccination_Rate, healthmerge$Series_Complete_12Plus,  healthmerge$Series_Complete_18Plus, healthmerge$Series_Complete_65Plus, healthmerge$goodbad)

#Rename columns
names(randomVariables)[names(randomVariables)=="healthmerge.Total_Vaccinated"] <- "Total_Vaccinated"
names(randomVariables)[names(randomVariables)=="healthmerge.Pct_People_Total_Vaccinated"] <- "Pct_People_Total_Vaccinated"
names(randomVariables)[names(randomVariables)=="healthmerge.Total_Healthcare_Workers_Employed"] <- "Total_Healthcare_Workers_Employed"
names(randomVariables)[names(randomVariables)=="healthmerge.Household_Median_Income"] <- "Household_Median_Income"
names(randomVariables)[names(randomVariables)=="healthmerge.Crime_Rate"] <- "Crime_Rate"
names(randomVariables)[names(randomVariables)=="healthmerge.Flu_Vaccination_Rate"] <- "Flu_Vaccination_Rate"
names(randomVariables)[names(randomVariables)=="healthmerge.Series_Complete_12Plus"] <- "Series_Complete_12Plus"
names(randomVariables)[names(randomVariables)=="healthmerge.Series_Complete_18Plus"] <- "Series_Complete_18Plus"
names(randomVariables)[names(randomVariables)=="healthmerge.Series_Complete_65Plus"] <- "Series_Complete_65Plus"
names(randomVariables)[names(randomVariables)=="healthmerge.goodbad"] <- "HighLowRating"

#RandomForest Algorithm
randrftotvacc <- randomForest(x=randomVariables[,-10], y=randomVariables[,10])
randrftotvacc
importance(randrftotvacc)
#testing
prediction <- predict(randrftotvacc, randomVariables[1,-10])
prediction

#Proof
randomVariables[1,]


#Testing with new data
sampling <- c(1796682, 45.2, 537000, 55789, 9.9, 15608521, 13584942, 637433 ,0.323)
prea <- predict(randrftotvacc, sampling)
