#install.packages("RSelenium")
library(RSelenium)
library(rvest)


#This is part 1 of 2 which is the data scraping aspect of the mini project.
#We separated it in order to save time during marking. This process will produce the final csv which we use in Part 2.


#This file takes around 30 mins to run due to the required loading time while gathering the data.



#Using RSelenium tutorial.R file to scrape data


rD <- rsDriver(browser = "phantomjs")
remDr <- rD[["client"]]

remDr$open() # This opens a connection so that we can browse the web.



#Go to the Alberta Salary Disclosure page and take a look
remDr$navigate("https://www.alberta.ca/salary-disclosure-table.cfm")
remDr$screenshot(display = TRUE)

#We want the Year filter button in the 5th table header
webElem1<-remDr$findElement(using="css", value = "th:nth-child(5) > a > span") 
webElem1$clickElement()

#Navigate specifically to the Year filter and enter 2016
tryCatch({
  suppressMessages({
    webElem1<-remDr$findElement(using="css", value = "body > div > form > div > span > span > input") 
    webElem1$clickElement()
    webElem1$sendKeysToElement(list('2016'))
  })
}, 
error = function(e) {
  NA_character_
}
)
#Now we navigate to the filter button through the CSS and click
webElem1<-remDr$findElement(using="css", value = "body > div > form > div > div > button") 
webElem1$clickElement()

#Create a blank frame
allTableData <- data.frame()
#We have to look at how many pages we need and change it here. Not very dynamic
numberOfPages <- 356


#This takes 20-30 minutes. Not for the faint of heart
for (i in 1:numberOfPages){

  #Grab all Data elements
  webElements <- remDr$findElements(using = 'tag name', "td")
  tempAllTableData <- unlist(lapply(webElements, function(x){x$getElementText()}))
  tempAllTableData <- as.data.frame(tempAllTableData)
  #Bind the new page and the existing data together
  allTableData<-rbind(allTableData,tempAllTableData)
  
  #Click the Next arrow for the next page
  webElem1<-remDr$findElement(using="link text", value = "Go to the next page") 
  webElem1$clickElement()
  
  #Unfortunately anything less than a 3 second sleep does not load completely and leads to errors
  Sys.sleep(3)
}

#Let's back it up so we don't lose it
#Because that would be devastating
write.csv(allTableData,file="2016ABData.csv")

#Convert data to list for easier operations
#Only take the second column "tempAllTableData"
datavector <- as.vector(allTableData["tempAllTableData"])

#Sort the data into individual vectors
temp <- list()
for(i in 1:6){
  temp[[i]] = datavector[[1]][seq(i,nrow(datavector),10)]
}

#Merge vectors into one dataframe
Data2016 = do.call(rbind, Map(data.frame,Ministry = temp[1], Name=temp[2], 
                              Position=temp[3],Classification=temp[4], 
                              Year=temp[5], Salary=temp[6]))

#Delete unneeded variables; all except Data
rm(datavector,temp,allTableData,tempAllTableData)

#Need to determine Gender from names
#install.packages("gender")
library(gender)

#install.packages("tidyr")
library(tidyr)

#We need to split the Full name into First and middle
NameSplitted1 <- separate(data = Data2016, col = Name, into = c("LastName", "FirstAndMiddle"), sep = ",")
#View(NameSplitted1)

#Unfortunately some names also include middle names or initials
#We need to split them up further

#Splits last name from middle and first
##Use Below code to split out middle intial if required
NameSplitted2 <- separate(data = NameSplitted1, col = FirstAndMiddle, into = c("X", "FirstName"), sep = " ")
#View(NameSplitted2)
ABData <- subset(NameSplitted2, select = -X )



#Now we have a set of data that is perfectly split up. All that's missing is the gender
#Let's apply the gender function to all unique first names
temp <- gender(unlist(ABData["FirstName"]))
temp <- unique(temp)

#Now we have a whole list of unique first names and genders associated with them
#We need to rename the columns so that they match our existing dataset
colnames(temp)[1] <- "FirstName"
colnames(temp)[4] <- "Gender"

#Now we merge them based on the first name
ABPayDataWithGender <- merge(temp[,c("FirstName","Gender")],ABData, by="FirstName", all.y = TRUE)

#Delete temporary variables
rm(temp,ABData,NameSplitted1,NameSplitted2,Data2016)

#Back it up again
write.csv(ABPayDataWithGender,file="2016ABPayDataWithGender.csv")


