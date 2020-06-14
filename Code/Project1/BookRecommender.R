#Import libraries
library(recommenderlab)
library(reshape2)
library(ggplot2)
library(plyr)
library(dplyr)
#Importing Data sets

BookRatings <- read.csv("~/Documents/BigDataAnalytics2/Course 2 - 1040/Project1/Data/BX-Book-Ratings.csv", stringsAsFactors = FALSE, sep=";")

View(BookRatings)

Books <- read.csv("~/Documents/BigDataAnalytics2/Course 2 - 1040/Project1/Data/BX-Books.csv", stringsAsFactors = FALSE, sep=";")

View(Books)

Users <- read.csv2("~/Documents/BigDataAnalytics2/Course 2 - 1040/Project1/Data/BX-Users.csv", stringsAsFactors = FALSE)

View(Users)

BookRatingsISBN <- merge(BookRatings, Books, by.x = "ISBN", by.y = "ISBN")

View(BookRatingsISBN)

#Merged dataset
BookRatingsISBNUser <- merge(BookRatingsISBN, Users, by.x = "User.ID", by.y = "User.ID")

View(BookRatingsISBNUser)

sum(BookRatingsISBNUser$Age == "NULL")

hist(BookRatingsISBNUser$Book.Rating, col = "lightblue", border = "pink")

BookRatingDF <- data.frame(BookRatingsISBNUser$User.ID, BookRatingsISBNUser$ISBN, BookRatingsISBNUser$Book.Rating)

BookRatingDF2 <- rename(BookRatingDF, c("BookRatingsISBNUser.User.ID"="UserID", "BookRatingsISBNUser.ISBN"="ISBN", "BookRatingsISBNUser.Book.Rating"="BookRating"))

View(BookRatingDF2)

str(BookRatingDF2)

BookRatingDF2$ISBN <- as.character(BookRatingDF2$ISBN)

BookRatingDF3 <- BookRatingDF2[BookRatingDF2$BookRating > 0,]

hist(BookRatingDF3$BookRating, col = "lightblue", border = "pink")

summary(BookRatingDF4)

BookRatingDF4[rowCounts(MovieLense) > 50, colCounts(MovieLense) > 100]

##Dedup
BookRatingDF3 %>% group_by(UserID,ISBN) %>% summarise(count=n()) %>% filter(count>1)

BookRatingDF3 %>% group_by(ISBN) %>%filter(n()>10)

BookRatingDF3[BookRatingDF3$ISBN=="0452264464"]

result <- filter(BookRatingDF3, ISBN == '002542730X')

## Book with atleast 10 users
BookRatingISBNList <- BookRatingDF3 %>% group_by(ISBN) %>% summarise(count=n()) %>% filter(count>10)

BookRatingDF4 <- merge(BookRatingDF3, BookRatingISBNList, by.x = "ISBN", by.y = "ISBN")

#BookRatingDF41=select(BookRatingDF4, -c(count))

BookRatingDF41 <- data.frame(BookRatingDF4$UserID,BookRatingDF4$ISBN,BookRatingDF4$BookRating)

str(BookRatingDF41)

BookRatingDF42 <- BookRatingDF41 %>% rename("UserID"="BookRatingDF4.UserID", "ISBN"="BookRatingDF4.ISBN", "BookRating"="BookRatingDF4.BookRating")

BookRatingDF42$ISBN <- as.character(BookRatingDF42$ISBN)
str(BookRatingDF42)

##Matrix

BookRatingDF5 <- acast(BookRatingDF42, UserID ~ ISBN)
# Check the class of BookRatingDF5
class(BookRatingDF5)
str(BookRatingDF5)
rownames(BookRatingDF5)

# Convert it as a matrix
BookRatingDF6<-as.matrix(BookRatingDF5)
str(BookRatingDF6)

# Convert BookRatingDF6 into realRatingMatrix data structure
# realRatingMatrix is a recommenderlab sparse-matrix like data-structure
BookRatingDF7 <- as(BookRatingDF6, "realRatingMatrix")
BookRatingDF7

# view BookRatingDF7 in other possible ways
as(BookRatingDF7, "list") # A list
as(BookRatingDF7, "matrix") # A sparse matrix
  
#Turn it into data-frame
head(as(BookRatingDF7, "data.frame"))

# normalize the rating matrix
BookRatingDF8 <- normalize(BookRatingDF7)
BookRatingDF8
as(BookRatingDF8, "list")


# Draw an image plot of raw-ratings & normalized ratings
#? A column represents one specific movie and ratings by users
#?? are shaded.
#?? Note that some items are always rated 'black' by most users
#??? while some items are not rated by many users
#???? On the other hand a few users always give high ratings
#????? as in some cases a series of black dots cut across items
image(BookRatingDF7, main = "Raw Ratings")
image(BookRatingDF8, main = "Normalized Ratings")

# Can also turn the matrix into a 0-1 binary matrix
BookRatingDF9 <- binarize(BookRatingDF7, minRating=1)
as(BookRatingDF9, "matrix")


# Create a recommender object (model)
#?? Run anyone of the following four code lines.
#???? Do not run all four
#?????? They pertain to four different algorithms.
#??????? UBCF: User-based collaborative filtering
#??????? IBCF: Item-based collaborative filtering
#????? Parameter 'method' decides similarity measure
#??????? Cosine or Jaccard
rec=Recommender(BookRatingDF7[1:nrow(BookRatingDF7)],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))
#rec=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Jaccard",nn=5, minRating=1))
#rec=Recommender(r[1:nrow(r)],method="IBCF", param=list(normalize = "Z-score",method="Jaccard",minRating=1))
#rec=Recommender(r[1:nrow(r)],method="POPULAR")


# Depending upon your selection, examine what you got
print(rec)
names(getModel(rec))
getModel(rec)$nn


############Create predictions#############################
# This prediction does not predict movie ratings for test.
#?? But it fills up the user 'X' item matrix so that
#??? for any userid and movieid, I can find predicted rating
#???? dim(r) shows there are 6040 users (rows)
#????? 'type' parameter decides whether you want ratings or top-n items
#???????? get top-10 recommendations for a user, as:
#???????????? predict(rec, r[1:nrow(r)], type="topNList", n=10)
recom <- predict(rec, BookRatingDF7[1:nrow(BookRatingDF7)], type="ratings")
recom



