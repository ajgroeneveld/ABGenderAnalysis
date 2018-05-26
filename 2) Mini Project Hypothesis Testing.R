#################################
#Code to perform hypothesis tests
#################################

#This is Part 2 containing the hypothesis tests
#The CSV used is generated in Part 1


#Get the Gender Data we wrangled in the previous part of the assignment (Mini-Project-1)
GenderData = read.csv("2016ABPayDataWithGender.csv", stringsAsFactors = FALSE)
View(GenderData)

######################
#Clean Data###########
######################

## To get Salary in numeric format...

#load libraries used for cleaning data
install.packages("dplyr")
install.packages("stringr")
library(dplyr)
library(stringr)

#The salary data needs to be converted from character to numeric 
#Get rid of the $ sign and the , in the salaries
GenderData$Salary <- str_replace_all(GenderData$Salary, fixed("$"), "")
GenderData$Salary <- str_replace_all(GenderData$Salary, fixed(","), "")

#Convert the character to numeric
GenderData$Salary <- as.numeric(GenderData$Salary)

##Filter for only male and female
MaleFemale.Data <- GenderData  %>% filter((Gender =="male") | (Gender =="female"))
View(MaleFemale.Data) 
MaleFemale.Data  <- subset(MaleFemale.Data, select = -X)
#MaleFemale.Data is our final dataset we can proceed to filter and use to answer questions

######################
#Hypothesis Test 1####
######################

#Hypothesis test to compare mean female salary to mean male salary
FemaleSalary <- MaleFemale.Data %>% filter(Gender =="female")
MaleSalary <- MaleFemale.Data %>% filter(Gender =="male")

q1results <- t.test(FemaleSalary$Salary, MaleSalary$Salary, 
                    alternative = "less", paired = FALSE, conf.level = 0.95)
#find p-value
q1results$p.value

#########################
####Hypothesis Test 2####
#########################

#Hypothesis test to compare proportion of females in our dataset to proportion of males
test2successvector <- c(nrow(FemaleSalary), nrow(MaleSalary))
test2totalsvector <- c(nrow(MaleFemale.Data), nrow(MaleFemale.Data))

q2results <- prop.test(test2successvector, 
                       test2totalsvector, p = NULL, 
                       alternative = "less", conf.level = 0.95)

#find p-value
q2results$p.value

#########################
####Hypothesis Test 3####
#########################

#hypothesis test to compare proportion of Females in executive 
#and director positions to proportion of Males

ExecutiveDirectorList <- MaleFemale.Data %>% filter((grepl("Executive", MaleFemale.Data$Position)|
                                                  ((grepl("Director", MaleFemale.Data$Position)))))
View(ExecutiveDirectorList)

ExecutiveDirectorFemale <- ExecutiveDirectorList %>% filter(Gender =="female")
ExecutiveDirectorMale <- ExecutiveDirectorList %>% filter(Gender =="male")

test3successvector <- c(nrow(ExecutiveDirectorFemale),nrow(ExecutiveDirectorMale))
test3totalsvector <- c(nrow(ExecutiveDirectorList), nrow(ExecutiveDirectorList))

q3results <- prop.test(test3successvector, 
                       test3totalsvector, p = NULL, 
                       alternative = "less", conf.level = 0.95)
#find p-value
q3results$p.value


#########################
####Bar Graphs####
#########################

##Create Bar Graphs of number of females and males in our data
##Everyone in our data had a minimum salary of $105,906 which is the threshold for reporting
library(ggplot2)
counts <- table(MaleFemale.Data$Gender)
barplot(counts, col = c("thistle1","lightblue"), 
        main="Employees Earning $105,906 Or Higher", 
        xlab="Gender", ylab = "Number of Employees", names.arg = c("Female", "Male"),
        ylim = c(0,2000))

##Create a bar plot to compare amount of females and males in executive positions
counts2 <- table(ExecutiveDirectorList$Gender)
barplot(counts2, col = c("indianred1","steelblue2"), 
        main="Alberta Government Employees in Executive Positions", 
        xlab="Gender", ylab = "Number of Executives and Directors", 
        names.arg = c("Female", "Male"), ylim = c(0,400))


