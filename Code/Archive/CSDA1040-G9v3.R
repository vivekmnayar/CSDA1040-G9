#Install following two packages in R if not already installed
#install.packages("recommenderlab")
#install.packages("data.table")
library(plyr)
library(dplyr)
library(recommenderlab)
library(data.table)
library(reshape2)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(corrplot)
library(qgraph)
library(methods)
library(Matrix)

#Importing Data sets
setwd("~/Course5/week1/BX-CSV/CSDA1040-G9")

#Without the "quote" all records won't be loaded
BookRatings = read.csv("BX-Book-Ratings.csv", stringsAsFactors = FALSE,
                       strip.white = TRUE, quote="", sep=';')
str(BookRatings)

Books <- read.csv("BX-Books.csv" , stringsAsFactors = FALSE,
                  strip.white = TRUE, quote="", sep=';')
str(Books)

Users <- read.csv("BX-Users.csv" , stringsAsFactors = FALSE,
                  strip.white = TRUE, quote="", sep=';')
str(Users)

#Merge the three csv files
RatingsISBNUsers <- merge(merge(Books, BookRatings, by='X.ISBN.', all=T), Users, by='X.User.ID.', all=T)

#Renaming the column names

RatingsISBNUsers <- plyr:: rename(RatingsISBNUsers, c("X.User.ID."="USERID", "X.ISBN."="ISBN", "X.Book.Title."="BOOK_TITLE", 
                                                      "X.Book.Author."="BOOK_AUTHOR", "X.Year.Of.Publication."="PUB_YEAR", 
                                                      "X.Publisher."="PUBLISHER", "X.Image.URL.S."="IMAGE_URL_S",
                                                      "X.Image.URL.M."="IMAGE_URL_M", "X.Image.URL.L."="IMAGE_URL_L",
                                                      "X.Book.Rating."="BOOK_RATING", "X.Location."="LOCATION", "X.Age."="AGE"))


#After reviewing features we decided to keep only 3 columns 
UserISBNRating <- select(RatingsISBNUsers,USERID,ISBN,PUB_YEAR, BOOK_RATING, AGE)

#Check for null values in the selected attributes
sum(is.na(UserISBNRating$USERID) )
sum(is.na(UserISBNRating$ISBN) )
sum(is.na(UserISBNRating$PUB_YEAR) )
sum(is.na(UserISBNRating$BOOK_RATING) )
sum(is.na(UserISBNRating$AGE) )

#remove null values in USERID & BOOK_RATING attributes and check to confirm no more null value
UserISBNRating2 <- UserISBNRating[!(UserISBNRating$USERID == "" | is.na(UserISBNRating$USERID)), ]
sum(is.na(UserISBNRating2$USERID) )
str(UserISBNRating2$PUB_YEAR) 
UserISBNRating2 <- UserISBNRating2[!(UserISBNRating2$BOOK_RATING == "" | is.na(UserISBNRating2$BOOK_RATING)), ]
UserISBNRating2 <- UserISBNRating2[!(UserISBNRating2$ISBN == ""| is.na(UserISBNRating2$ISBN)), ]
UserISBNRating2 <- UserISBNRating2[!(UserISBNRating2$PUB_YEAR == ""| is.na(UserISBNRating2$PUB_YEAR)), ]
summary(UserISBNRating2)

UserISBNRating2 <- UserISBNRating2[!(UserISBNRating2$PUB_YEAR == "NA"| is.na(UserISBNRating2$PUB_YEAR)), ]
UserISBNRating2 <- UserISBNRating2[!(UserISBNRating2$ISBN == "NA"| is.na(UserISBNRating2$ISBN)), ]
summary(UserISBNRating2)


sum(is.na(UserISBNRating2$BOOK_RATING) )
sum(is.na(UserISBNRating2$ISBN) )
sum(is.na(UserISBNRating2$AGE) )
sum(is.na(UserISBNRating2$PUB_YEAR) )

#Replace all "" with blank
UserISBNRating2[] <- lapply(UserISBNRating2, gsub, pattern='"', replacement='')
summary(UserISBNRating2)

#Convert BOOK_RATING column to numeric
UserISBNRating2$BOOK_RATING <- as.numeric(UserISBNRating2$BOOK_RATING)
UserISBNRating2$ISBN <- as.numeric(UserISBNRating2$ISBN)
UserISBNRating2$USERID <- as.numeric(UserISBNRating2$USERID)
UserISBNRating2$PUB_YEAR <- as.numeric(UserISBNRating2$PUB_YEAR)
UserISBNRating2$AGE <- as.numeric(UserISBNRating2$AGE)


#We still have about 217900 age attributes missing values. So, update missing 
#values with mean value

UserISBNRating2$AGE <- ifelse(is.na(UserISBNRating2$AGE), median(UserISBNRating2$AGE, na.rm=TRUE), UserISBNRating2$AGE)
summary(UserISBNRating2)
str(UserISBNRating2)

hist(UserISBNRating2$BOOK_RATING, col = "lightblue", border = "pink")
str(UserISBNRating2)

#remove rows where BOOK_RATING=0 - Assuming 0 implies "no rating"
UserISBNRating3 <- UserISBNRating2[UserISBNRating2$BOOK_RATING > 0,]
str(UserISBNRating3)
    
hist(UserISBNRating3$AGE, col = "green", border = "pink")
str(UserISBNRating3)

#Remove records where age is < 10 and > 70
UserISBNRating4 <- UserISBNRating3[UserISBNRating3$AGE > 9,]
UserISBNRating4 <- UserISBNRating4[UserISBNRating4$AGE < 70,]
str(UserISBNRating4)

hist(UserISBNRating4$AGE, col = "green", border = "pink")
str(UserISBNRating4)

hist(UserISBNRating4$BOOK_RATING, col = "red", border = "pink")
str(UserISBNRating4)

g <- acast(UserISBNRating3, USERID ~ BOOK_RATING)
# Check the class of g
class(g)

# Convert it as a matrix
R<-as.matrix(g)

# Create a boxplot of the dataset, outliers are shown as two distinct points
#boxplot(warpbreaks)$out
boxplot(UserISBNRating4)$out






