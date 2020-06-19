#Import libraries
library(recommenderlab)
library(reshape2)
library(ggplot2)
library(plyr)
library(dplyr)
#Importing Data sets

BookRatings <- read.csv("~/Documents/BigDataAnalytics2/Course 2 - 1040/Project1/Data/BX-Book-Ratings.csv", stringsAsFactors = FALSE,
                        strip.white = TRUE, quote="", sep=';')

View(BookRatings)

Books <- read.csv("~/Documents/BigDataAnalytics2/Course 2 - 1040/Project1/Data/BX-Books.csv", stringsAsFactors = FALSE,
                  strip.white = TRUE, quote="", sep=';')

View(Books)

Users <- read.csv2("~/Documents/BigDataAnalytics2/Course 2 - 1040/Project1/Data/BX-Users.csv", stringsAsFactors = FALSE,
                   strip.white = TRUE, quote="", sep=';')

View(Users)

BookRatingsISBN <- merge(BookRatings, Books, by.x = "X.ISBN.", by.y = "X.ISBN.")

View(BookRatingsISBN)

#Merged dataset
BookRatingsISBNUser <- merge(BookRatingsISBN, Users, by.x = "X.User.ID.", by.y = "X.User.ID.")

View(BookRatingsISBNUser)

#Replace all " with blank

BookRatingsISBNUser[] <- lapply(BookRatingsISBNUser, gsub, pattern='"', replacement='')

#Convert BOOK_RATING column to numeric
BookRatingsISBNUser$X.User.ID. <- as.character(BookRatingsISBNUser$X.User.ID.)
BookRatingsISBNUser$X.ISBN. <- as.character(BookRatingsISBNUser$X.ISBN.)
BookRatingsISBNUser$X.Book.Rating. <- as.numeric(BookRatingsISBNUser$X.Book.Rating.)

hist(BookRatingsISBNUser$X.Book.Rating., col = "lightblue", border = "pink")

BookRatingDF <- data.frame(BookRatingsISBNUser$X.User.ID., BookRatingsISBNUser$X.ISBN., BookRatingsISBNUser$X.Book.Rating.)

BookRatingDF2 <- BookRatingDF %>% rename("UserID"="BookRatingsISBNUser.X.User.ID.", "ISBN"="BookRatingsISBNUser.X.ISBN.", "BookRating"="BookRatingsISBNUser.X.Book.Rating.")

View(BookRatingDF2)

str(BookRatingDF2)

BookRatingDF2$UserID <- as.character(BookRatingDF2$UserID)
BookRatingDF2$ISBN <- as.character(BookRatingDF2$ISBN)

BookRatingDF3 <- BookRatingDF2[BookRatingDF2$BookRating > 0,]

hist(BookRatingDF3$BookRating, col = "lightblue", border = "pink")

##Dedup
BookRatingDFUserlist <- BookRatingDF3 %>% group_by(UserID,ISBN) %>% summarise(count=n()) %>% filter(count==1)

##BookRatingDF3 %>% group_by(ISBN) %>%filter(n()>10)

##BookRatingDF3[BookRatingDF3$ISBN=="0452264464"]

##result <- filter(BookRatingDF3, ISBN == '002542730X')

BookRatingDF3 <- merge(BookRatingDF3, BookRatingDFUserlist, by.x = c("ISBN","UserID"), by.y = c("ISBN","UserID"))

BookRatingDF3 %>% group_by(UserID,ISBN) %>% summarise(count=n()) %>% filter(count>1)

## Book with atleast 10 users
BookRatingISBNList <- BookRatingDF3 %>% group_by(ISBN) %>% summarise(count=n()) %>% filter(count>10)

BookRatingDF4 <- merge(BookRatingDF3, BookRatingISBNList, by.x = "ISBN", by.y = "ISBN")

#Users with atleast 10 ratings
Userlist <- BookRatingDF4 %>% group_by(UserID) %>% summarise(count=n()) %>% filter(count>10)

BookRatingDF40 <- merge(BookRatingDF4, Userlist, by.x = "UserID", by.y = "UserID")

#BookRatingDF41=select(BookRatingDF4, -c(count))

BookRatingDF41 <- data.frame(BookRatingDF40$UserID,BookRatingDF40$ISBN,BookRatingDF40$BookRating)

str(BookRatingDF41)

BookRatingDF42 <- BookRatingDF41 %>% rename("UserID"="BookRatingDF40.UserID", "ISBN"="BookRatingDF40.ISBN", "BookRating"="BookRatingDF40.BookRating")

BookRatingDF42$UserID <- as.character(BookRatingDF42$UserID)
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


#Restrict dataset to users who have rated at least 5 books and books that have been rated by at least 20 users

ratings = BookRatingDF7[rowCounts(BookRatingDF7) > 15, colCounts(BookRatingDF7) > 50]
dim(ratings)

image(ratings, main = "Raw Ratings")

#Normalize ratings

ratings.n = normalize(ratings)
ratings.n.vec = as.vector(ratings.n@data)
ratings.n.vec = ratings.n.vec[ratings.n.vec != 0]
hist(ratings.n.vec, main="Histogram of Normalized Ratings", xlab="Rating")

#Split the dataset into train and test

percent_train = 0.8
#min(rowCounts(ratings.n))
items_to_keep = 10        # items to use for each user
rating_threshold = 7      # good rating implies >=3
n_eval = 1                # number of times to run eval

eval_sets = evaluationScheme(data = BookRatingDF7, method = "split",
                             train = percent_train, given = items_to_keep,
                             goodRating = rating_threshold, k = n_eval)

eval_sets

##UBCF

eval_recommender = Recommender(data = getData(eval_sets, "train"),
                               method = "UBCF", parameter = NULL)
items_to_recommend = 5

eval_prediction = predict(object = eval_recommender,
                          newdata = getData(eval_sets, "known"),
                          n = items_to_recommend,
                          type = "ratings")

eval_accuracy = calcPredictionAccuracy(x = eval_prediction,
                                       data = getData(eval_sets, "unknown"),
                                       byUser = TRUE)
head(eval_accuracy)

##################IBCF

eval_recommender_IBCF = Recommender(data = getData(eval_sets, "train"),
                               method = "IBCF", parameter = NULL)
items_to_recommend_IBCF = 5
eval_prediction_IBCF = predict(object = eval_recommender_IBCF,
                          newdata = getData(eval_sets, "known"),
                          n = items_to_recommend_IBCF,
                          type = "ratings")
eval_accuracy_IBCF = calcPredictionAccuracy(x = eval_prediction_IBCF,
                                       data = getData(eval_sets, "unknown"),
                                       byUser = TRUE)
head(eval_accuracy_IBCF)


##################

models_to_evaluate = list(UBCF_cos = list(name = "UBCF", param = list(method = "cosine")),
                          UBCF_cor = list(name = "UBCF", param = list(method = "pearson")),
                          random = list(name = "RANDOM", param=NULL))


models_to_evaluate = list(IBCF_cos = list(name = "IBCF", param = list(method = "cosine")),
                          IBCF_cor = list(name = "IBCF", param = list(method = "pearson")),
                          UBCF_cos = list(name = "UBCF", param = list(method = "cosine")),
                          UBCF_cor = list(name = "UBCF", param = list(method = "pearson")),
                          random = list(name = "RANDOM", param=NULL))

n_recommendations = c(1, 3, 5, 10, 15, 20)
results = evaluate(x = eval_sets, method = models_to_evaluate, n = n_recommendations)

# Draw ROC curve
plot(results, y = "ROC", annotate = 1, legend="topleft")
title("ROC Curve")

# Draw precision / recall curve
plot(results, y = "prec/rec", annotate=1)
title("Precision-Recall")

##############################end#####################
