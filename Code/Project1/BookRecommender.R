#Importing Data sets

BookRatings <- read.csv("~/Documents/BigDataAnalytics2/Course 2 - 1040/Project1/Data/BX-Book-Ratings.csv", sep=";")

View(BookRatings)

Books <- read.csv("~/Documents/BigDataAnalytics2/Course 2 - 1040/Project1/Data/BX-Books.csv", sep=";")

View(Books)

Users <- read.csv2("~/Documents/BigDataAnalytics2/Course 2 - 1040/Project1/Data/BX-Users.csv")

View(Users)

BookRatingsISBN <- merge(BookRatings, Books, by.x = "ISBN", by.y = "ISBN")

View(BookRatingsISBN)

BookRatingsISBNUser <- merge(BookRatingsISBN, Users, by.x = "User.ID", by.y = "User.ID")

View(BookRatingsISBNUser)

sum(BookRatingsISBNUser$Age == "NULL")

hist(BookRatingsISBNUser$Book.Rating, col = "lightblue", border = "pink")


