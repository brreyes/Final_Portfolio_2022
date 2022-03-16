## Brandon Reyes IST 707 Project


dev.off() # Clear the graph window

cat('\014')  # Clear the console

rm(list=ls()) # Clear user objects from the environment

#install.packages("readr")
#library(readr)
#install.packages("quanteda")
#library(quanteda)
#setwd("~/Desktop/IST707")

#load our data
movies<-read_csv(file='movies.csv')
summary(movies)
View(movies)
head(movies)

ratings<-read_csv(file='ratings.csv')
View(ratings)
summary(ratings)
head(ratings)

#install.packages("ggplot2")
#library(ggplot2)
#install.packages("recommenderlab")
#library(recommenderlab)
#library(data.table)

# Data Preparation
#create dataframe with just genres column
genres<-as.data.frame(movies$genres)
View(genres)
summary(genres)
head(genres)

#Lets split up all the genres into columns
newgenres<-as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE))
View(newgenres)
head(newgenres)

#rename columns
colnames(newgenres)<-c(1:10)

# combine names of genres into names list
genre_names<-c("Action","Adventure","Animation","Children","Comedy","Crime","Documentary","Drama","Fantasy","Film-Noir","Horror",
              "Musical","Mystery","Romance","Sci-Fi","Thriller","War","Western")

#We have 10329 movies
#lets create a matrix of these movies and genres, which is 18
#we add 1 to the movies to leave space for the head row
genre_mat<-matrix(0,10330,18)

#set first row of the matrix to our list of genre names
genre_mat[1,]<-genre_names

#set column names to genre list
colnames(genre_mat)<-genre_names

#loop for whole matrix

for (i in 1:nrow(newgenres)) {
  for (c in 1:ncol(newgenres)) {
    genmat_col = which(genre_mat[1,] == newgenres[i,c])
    genre_mat[i+1,genmat_col] <- 1
  }
}

#convert into a new dataframe and remove first row
genredf <- as.data.frame(genre_mat[-1,], stringsAsFactors=FALSE)
summary(genredf)
View(genredf)

#convert genre dataframe observations from characters to integers
for (c in 1:ncol(genredf)) {
  genredf[,c] <- as.integer(genredf[,c])
}

str(genredf)


#Lets now create a new dataframe from the titles column of our movies dataset
#This dataframe will help us search for a movie by genre

#Lets separate the year out of the titles in our movies dataset
#Put them into a dataframe
year <- as.data.frame(movies$title, stringsAsFactors=FALSE)

head(year)
#1                   Toy Story (1995)
#2                     Jumanji (1995)
#3            Grumpier Old Men (1995)
#4           Waiting to Exhale (1995)
#5 Father of the Bride Part II (1995)
#6                        Heat (1995)
#the year is in the title

#structure of this dataframe
str(year)
View(year)

year$`movies$title`<-as.character(year$`movies$title`)

#function to extract the year out from the string of the title
right <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#updating our year dataframe to have just the year from the title column
year <- as.data.frame(substr(right(right(year$`movies$title`, 6),5),1,4))
View(year)

## Create a new dataframe from ratings dataset

ratings_df <- ratings

#lets now bin the ratings and give them values
#ratings of >=4 are given a value to 1 to represent a like
#ratings of <=3 are given a value of -1 to represent a dislike
# and below are mapped to -1,(dislikes)

for (i in 1:nrow(ratings_df)){
  if (ratings_df[i,3] > 3){
    ratings_df[i,3] <- 1
  }
  else{
    ratings_df[i,3] <- 0
  }
}


# convert ratings_df

ratings_df2 <- dcast(ratings_df, movieId~userId, value.var = "rating", na.rm=FALSE)

#give all NAs a value of 0
for (i in 1:ncol(ratings_df2)){
  ratings_df2[which(is.na(ratings_df2[,i]) == TRUE),i] <- 0
}

#remove movieId col
ratings_df2 = ratings_df2[,-1]
View(ratings_df2)

# Lets remove the movies that don't have ratings 
movie_id <- length(unique(movies$movieId))

ratings_movieid <- length(unique(ratings$movieId))

movies_df <- movies[-which((movies$movieId %in% ratings$movieId) == FALSE),]

rownames(movies_df) <- NULL
View(movies_df)

#Remove the movies that don't have ratings in our genredf

genre_matnew <- genredf[-which((movies$movieId %in% ratings$movieId) == FALSE),]

rownames(genre_matnew) <- NULL
View(genre_matnew)

# calculate the dot product of the genre matrix and 
# the ratings matrix and obtain the user profiles

result = matrix(0,18,668) 

for (c in 1:ncol(ratings_df2)){
  for (i in 1:ncol(genre_matnew)){
    result[i,c] <- sum((genre_matnew[,i]) * (ratings_df2[,c])) #ratings per genre
  }
}


#Convert to dummy variables

for (c in 1:ncol(result)){
  for (i in 1:nrow(result)){
    if (result[i,c] < 0){
      result[i,c] <- 0
    }
    else {
      result[i,c] <- 1
    }
  }
}

View(genres)

#install.packages("wordcloud")
#library(wordcloud)
#install.packages("RColorBrewer")
#library(RColorBrewer)
#install.packages("wordcloud2")
#library(wordcloud2)
#install.packages("tm")
#library(tm)

#Create a vector containing only the text
text <- movies$genres
docs <- Corpus(VectorSource(text))
set.seed(1234) # for reproducibility
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
wordcloud(words = df$word, freq = df$freq, 
          min.freq = 1,           
          max.words=200, 
          random.order=FALSE, 
          rot.per=0.35,            
          colors=brewer.pal(8,"Dark2"))

#####################################################


#Use Jaccard distance for any similarities between users
#library(reshape2)

ratmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
View(ratmat)

#remove user ids column
ratmat <- as.matrix(ratmat[,-1])

#####################################################

## User-Based Collaborative Filtering

#Convert rating matrix into a recommenderlab sparse matrix
ratmat <- as(ratmat, "realRatingMatrix")

# Lets see how similar the first four users are 
# create similarity matrix using cosine similarity method 
# and 30 nearest neighbors
sim_users <- similarity(ratmat[1:4, ], 
                               method = "cosine", 
                               which = "users")

as.matrix(sim_users)

image(as.matrix(sim_users), main = "User similarity")

#similarity between the first 4 movies
sim_items <- similarity(ratmat[, 1:4], method =
                                 "cosine", which = "items")

as.matrix(sim_items)

image(as.matrix(sim_items), main = "Items Similarity")

################################

# Explore rating values
vec_ratings <- as.vector(ratmat@data)

# unique rating values
unique(vec_ratings) 
#[1] 0.0 4.0 3.0 3.5 4.5 2.5 5.0 1.5 1.0 2.0 0.5

# count of each rating value
table_ratings <- table(vec_ratings)

table_ratings
#vec_ratings
#0           0.5       1     1.5       2     2.5       3     3.5       4     4.5       5 
#5830409    1369    2811    1790    7545    5542   20013   13118   26736    8533   13164 


# rating = 0 are NA values
#lets get the ratings that are not NA aka not 0
vec_ratings <- vec_ratings[vec_ratings != 0]

vec_ratings <- factor(vec_ratings)

qplot(vec_ratings) + 
  ggtitle("Rating Distribution")

########################

## Movie Views

# count views for each movie
movie_views <- colCounts(ratmat)

# create dataframe of views
viewsdf <- data.frame(movie = names(movie_views),
                          views = movie_views)
View(viewsdf)

# sort views in descending order 
viewsdf <- viewsdf[order(viewsdf$views, 
                                 decreasing = TRUE), ]

ggplot(viewsdf[1:6,], aes(x = movie, y = views)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(labels=subset(movies_df, movies_df$movieId == viewsdf[1:6,]$movie)$title) +
  ggtitle("Top Movies Views")

#############################################

#Heatmap of Ratings

image(ratmat, main = "Rating matrix Heatmap")

#specify further to get a zoomed in shot
image(ratmat[1:10, 1:15], main = "Rating Matrix Heatmap")

# Top users and Movies
image(ratmat[rowCounts(ratmat) > quantile(rowCounts(ratmat), 0.99),
                colCounts(ratmat) > quantile(colCounts(ratmat), 0.99)], 
      main = "Top Movies Heatmap")

#Lets normalize our ratings data

ratmat_norm <- normalize(ratmat)

#plot again using normalized ratmat
image(ratmat_norm[rowCounts(ratmat_norm) > quantile(rowCounts(ratmat_norm), 0.99),
                     colCounts(ratmat_norm) > quantile(colCounts(ratmat_norm), 0.99)], 
      main = "Top Movies Heatmap")

##############################################################3

# User-Based Collaborative Filtering recommender model
#cosine method
#30 nearest neighbors

rec_model <- Recommender(ratmat_norm, 
                                 method = "UBCF", 
                                 param=list(method="Cosine",nn=30))

model_info <- getModel(rec_model)
View(model_info)

model_info$data
#610 x 9723 rating matrix of class ‘realRatingMatrix’ with 100621 ratings.
#Normalized using center on rows.

#Top 10 recommendations for our first user-profile
top_10 <- predict(rec_model, 
                 ratmat[1], 
                 n=10)

top_10

#convert top_10 to readable list
top_10_list <- as(top_10, 
                 "list")
top_10_list
# [1] "3567"   "913"    "55276"  "30803"  "27611"  "4223"   "106489" "5066"   "42728"  "55052" 

#Get names of our recommendations
top_10_result <- matrix(0,10)
for (i in 1:10){
  top_10_result[i] <- as.character(subset(movies, 
                                         movies$movieId == as.integer(top_10_list[[1]][i]))$title)
}

top_10_result
#[1,] "Bossa Nova (2000)"                          
#[2,] "Maltese Falcon, The (1941)"                 
#[3,] "Michael Clayton (2007)"                     
#[4,] "3-Iron (Bin-jip) (2004)"                    
#[5,] "Battlestar Galactica (2003)"                
#[6,] "Enemy at the Gates (2001)"                  
#[7,] "Hobbit: The Desolation of Smaug, The (2013)"
#[8,] "Walk to Remember, A (2002)"                 
#[9,] "Tristan & Isolde (2006)"                    
#[10,] "Atonement (2007)" 

# Let us now evaluate our model
#Using 5 k-fold cross validation with a Given-3 protocol

evaluation <- evaluationScheme(ratmat, 
                                      method="cross-validation", 
                                      k=5, given=3, 
                                      goodRating=5) 
evaluation2 <- evaluate(evaluation, 
                               method="UBCF", 
                               n=c(1,3,5,10,15))

evaluation_results <- getConfusionMatrix(evaluation2)[[1]]
evaluation_results

##################################

movie_ratings <- ratmat[rowCounts(ratmat) > 50,
                            colCounts(ratmat) > 50]

movie_ratings 
#378 x 435 rating matrix of class ‘realRatingMatrix’ with 36041 ratings.

movie_min <- quantile(rowCounts(movie_ratings), 0.98)

user_min <- quantile(colCounts(movie_ratings), 0.98)

image(movie_ratings[rowCounts(movie_ratings) > movie_min,
                     colCounts(movie_ratings) > user_min], 
      main = "Best users and movies Heatmap")

user_rating_avg <- rowMeans(movie_ratings)

#distribution of average rating per user
qplot(user_rating_avg) + stat_bin(binwidth = 0.1) +
  ggtitle("Average User Rating")
#we see some left-skewedness

# Let's normalize our data again 

movie_ratings_norm <- normalize(movie_ratings)

sum(rowMeans(movie_ratings_norm) > 0.00001)
#[1] 0

image(movie_ratings_norm[rowCounts(movie_ratings_norm) > movie_min,
                          colCounts(movie_ratings_norm) > user_min], 
      main = "Best users and movies Heatmap")

###################################################

# Converting data into dummy variables

#Movie data
#1 means the movie has been watched, 0 otherwise

movie_watched <- binarize(movie_ratings, minRating = 1)
movie_min_dummy <- quantile(rowCounts(movie_ratings), 0.95)
user_min_dummy <- quantile(colCounts(movie_ratings), 0.95)

image(movie_watched[rowCounts(movie_ratings) > movie_min_dummy,
                             colCounts(movie_ratings) > user_min_dummy], 
      main = "Best users and movies Heatmap")

#Rating data
#1 means rating above 3

good_movie <- binarize(movie_ratings, minRating = 3)

image(good_movie[rowCounts(movie_ratings) > movie_min_dummy, 
                          colCounts(movie_ratings) > user_min_dummy], 
      main = "Best users and movies Heatmap")

#########################################################################

# Item-based Collaborative Filtering Model

#Splitting our training/test sets

which_train <- sample(x = c(TRUE, FALSE), 
                      size = nrow(movie_ratings),
                      replace = TRUE, 
                      prob = c(0.8, 0.2))

train <- movie_ratings[which_train, ]
test <- movie_ratings[!which_train, ]


###MODEL 2
# It is now time to build our second model to recommend movies 

rec_registry <- recommenderRegistry$get_entries(dataType ="realratmatrix")

rec_registry$IBCF_realratmatrix$parameters
#NULL

rec_model2 <- Recommender(data = train, 
                          method = "IBCF",
                          parameter = list(k = 30))

rec_model2
#Recommender of type ‘IBCF’ for ‘realRatingMatrix’ 
#learned using 294 users.

class(rec_model2)

model2_info <- getModel(rec_model2)
model2_info
#$description
#[1] "IBCF: Reduced similarity matrix"

#$sim
#435 x 435 sparse Matrix of class "dgCMatrix"
#[[ suppressing 43 column names ‘2’, ‘3’, ‘6’ ... ]]
#[[ suppressing 43 column names ‘2’, ‘3’, ‘6’ ... ]]

#2  . .         .         0.4736162 .         . .         . .         . .         . . .         . .         .
#3  . .         .         .         .         . .         . 0.5740527 . 0.5708257 . . .         . .         .
#6  . .         .         .         0.3623403 . 0.5542996 . .         . 0.4474470 . . .         . 0.4008498 .
#7  . .         .         .         .         . .         . .         . .         . . .         . .         .
#10 . .         .         .         .         . 0.4061017 . .         . .         . . .         . .         .
#11 . .         .         .         .         . .         . .         . .         . . .         . .         .
#16 . .         0.5542996 .         .         . .         . .         . .         . . .         . 0.4690122 .
#17 . .         .         .         .         . .         . .         . .         . . 0.6561405 . .         .
#19 . 0.5740527 .         .         .         . .         . .         . .         . . .         . .         .
#21 . .         .         .         .         . .         . .         . .         . . 0.4815126 . .         .
#25 . 0.5708257 .         .         .         . .         . .         . .         . . 0.5661351 . .         .
#32 . .         .         .         .         . .         . .         . .         . . .         . .         .

#2  . . .         0.6673341 . . . .         .         . .         .         .         . .         . 0.5974445
#3  . . .         .         . . . .         .         . .         0.5378089 .         . .         . 0.5641048
#6  . . .         .         . . . .         .         . .         .         .         . .         . .        
#7  . . .         .         . . . 0.5249105 0.8248231 . .         0.6197529 .         . .         . .        
#10 . . 0.5426749 .         . . . .         .         . .         .         0.4205230 . .         . .        
#11 . . 0.4964326 .         . . . .         .         . .         .         .         . .         . .        
#16 . . .         .         . . . .         .         . .         .         .         . .         . .        
#17 . . .         .         . . . .         .         . .         .         .         . .         . .        
#19 . . .         .         . . . .         .         . 0.5241434 .         0.5064832 . 0.4977622 . .        
#21 . . .         .         . . . .         .         . .         .         .         . 0.4172674 . .        
#25 . . .         .         . . . .         0.5753133 . .         .         .         . .         . .        
#32 . . .         .         . . . .         .         . .         .         .         . .         . .        

#2  .         .         . . . .         .         .         . ......
#3  .         .         . . . .         .         .         . ......
#6  .         .         . . . .         .         .         . ......
#7  .         0.6466869 . . . .         .         .         . ......
#10 .         .         . . . 0.5465805 .         .         . ......
#11 .         .         . . . .         .         .         . ......
#16 .         .         . . . .         .         .         . ......
#17 .         0.5375126 . . . .         .         .         . ......
#19 0.5361141 .         . . . .         0.5207035 .         . ......
#21 .         .         . . . 0.4434858 0.4266377 .         . ......
#25 .         .         . . . .         .         .         . ......
#32 .         .         . . . .         .         0.4156836 . ......

#..............................
#........suppressing 392 columns and 412 rows in show(); maybe adjust 'options(max.print= *, width = *)'
#..............................
#[[ suppressing 43 column names ‘2’, ‘3’, ‘6’ ... ]]

#80463  . . . .         .         .         . .         . .         . .         . . .         . .         .
#81845  . . . 0.6985405 .         .         . .         . .         . .         . . .         . .         .
#89745  . . . .         .         .         . .         . .         . 0.6200393 . . 0.6652439 . .         .
#91500  . . . .         .         .         . .         . .         . .         . . 0.7486351 . 0.6140108 .
#91529  . . . .         .         0.6619116 . .         . .         . .         . . .         . .         .
#99114  . . . .         .         .         . .         . .         . 0.5923765 . . .         . .         .
#106782 . . . 0.8202008 .         .         . .         . .         . .         . . 0.5454440 . .         .
#109374 . . . .         .         .         . .         . 0.9721992 . 0.5473698 . . 0.9561795 . .         .
#109487 . . . .         .         .         . .         . .         . .         . . .         . .         .
#112852 . . . .         0.6336704 0.5659793 . .         . .         . .         . . 0.6710622 . .         .
#122904 . . . .         .         .         . 0.7888581 . .         . .         . . 0.8898398 . .         .

#80463  .         .         . . . . .         . . .         .         . 0.4640819 0.5651859 . . .        
#81845  .         .         . . . . .         . . .         .         . .         .         . . .        
#89745  .         .         . . . . .         . . .         .         . .         0.6232591 . . .        
#91500  .         .         . . . . .         . . 0.5294626 .         . .         .         . . .        
#91529  .         0.5358590 . . . . .         . . .         .         . .         0.6717345 . . 0.6772684
#99114  .         .         . . . . .         . . .         .         . .         .         . . 0.6712168
#106782 0.7711158 .         . . . . .         . . .         .         . .         .         . . .        
#109374 0.9962962 .         . . . . .         . . .         0.5084689 . .         .         . . 0.7677984
#109487 .         .         . . . . 0.7352464 . . .         .         . .         .         . . .        
#112852 .         0.6657977 . . . . .         . . .         .         . 0.6069905 0.5941548 . . .        
#122904 0.9982702 0.7578630 . . . . .         . . .         .         . .         0.7938801 . . .        

#80463  .         . . . . . 0.8851442 . ......
#81845  0.7884863 . . . . . .         . ......
#89745  .         . . . . . .         . ......
#91500  .         . . . . . .         . ......
#91529  .         . . . . . .         . ......
#99114  .         . . . . . 0.5900341 . ......
#106782 .         . . . . . .         . ......
#109374 .         . . . . . 0.6256553 . ......
#109487 .         . . . . . .         . ......
#112852 .         . . . . . 1.0000000 . ......
#122904 .         . . . . . .         . ......

#$k
#[1] 30

#$method
#[1] "Cosine"

#$normalize
#[1] "center"

#$normalize_sim_matrix
#[1] FALSE

#$alpha
#[1] 0.5

#$na_as_zero
#[1] FALSE

#$verbose
#[1] FALSE

class(model2_info$sim) 

dim(model2_info$sim)
# [1] 435 435

top_items <- 20

image(model2_info$sim[1:top_items, 1:top_items],
      main = "Heatmap of First 20 Observations")

row_sums <- rowSums(model2_info$sim > 0)

table(row_sums)
# 30
#435

col_sums <- colSums(model2_info$sim > 0)

qplot(col_sums) + stat_bin(binwidth = 1) + ggtitle("Column Count Distrbution")

# Prediction of recommender on the dataset

n_rec_pred <- 10 

rec_pred <- predict(object = rec_model2, 
                          newdata = test, 
                          n = n_rec_pred)

rec_pred
#Recommendations as ‘topNList’ with n = 10 for 84 users.

rec_user_1 <- rec_pred@items[[1]] 
#first user

movie_user_1 <- rec_pred@itemLabels[rec_user_1]

movie_user_2 <- movie_user_1

for (i in 1:10){
  movie_user_2[i] <- as.character(subset(movies, 
                                          movies$movieId == movie_user_1[i])$title)
}


movie_user_2
# [1] "What's Eating Gilbert Grape (1993)" "Maverick (1994)"                   
#[3] "Pretty Woman (1990)"                "Sound of Music, The (1965)"        
#[5] "Full Monty, The (1997)"             "Gattaca (1997)"                    
#[7] "Rain Man (1988)"                    "Untouchables, The (1987)"          
#[9] "Shakespeare in Love (1998)"         "Training Day (2001)" 

#Create a matrix with recommendations for every user
rec_mat <- sapply(rec_pred@items, 
                      function(x){ as.integer(colnames(movie_ratings)[x]) })
dim(rec_mat)
#[1] 10 84

rec_mat[,1:6]
#      [,1] [,2] [,3] [,4] [,5]  [,6]
#[1,]  337   11 1197  253 4848   551
#[2,]  368  420 1220  590   11  4034
#[3,]  597  592 1270  594   70  8961
#[4,] 1035 2194 1304 1035  172 72998
#[5,] 1641 4720 1682 1097  368  3671
#[6,] 1653 2716 1954 1197  474 59315
#[7,] 1961 1220 2194 1201  508   509
#[8,] 2194 8665 2716 1207  520  1220
#[9,] 2396  527 3481 1220  597   223
#[10,] 4776 4370 5952 1222  780  2291

#Item number distribution
item_num <- factor(table(rec_mat))

dist_title <- "IBCF Item Amount Distribution"
qplot(item_num) + ggtitle(dist_title)
#We can see this is right skewed

item_num_sort <- sort(item_num, decreasing = TRUE)
item_num_head <- head(item_num_sort, n = 4)

top_items_table <- data.frame(as.integer(names(item_num_head)),
                        item_num_head)

for (i in 1:4){
  top_items_table[i,1] <- as.character(subset(movies, 
                                        movies$movieId == top_items_table[i,1])$title)
}


colnames(top_items_table) <- c("Movie", "Item Amount")
head(top_items_table)
#                              Movie Item Amount
#923             Citizen Kane (1941)           9
#1197     Princess Bride, The (1987)           9
#1215        Army of Darkness (1993)           9
#11   American President, The (1995)           8

##############################################################

# USER-based Collaborative Filtering Model (UBCF)

rec_registry2 <- recommenderRegistry$get_entries(dataType ="realRatMatrix")

rec_registry2$UBCF_realratmatrix$parameters
#NULL

rec_model3 <- Recommender(data = train, method = "UBCF")

rec_model3
model3_info <- getModel(rec_model3)

model3_info$data
#294 x 435 rating matrix of class ‘realRatingMatrix’ with 28372 ratings.
#Normalized using center on rows.

####################################################

# TEST SET

n_rec_test <- 10

rec_pred_2 <- predict(object = rec_model3,
                          newdata = test, 
                          n = n_rec_test)

rec_pred_2
#Recommendations as ‘topNList’ with n = 10 for 84 users.

#Results Exploration

#matrix of our predictions
rec_mat_2 <- sapply(rec_pred_2@items, 
                      function(x){ as.integer(colnames(movie_ratings)[x]) })

rec_mat[, 1:6]
#     [,1] [,2] [,3] [,4] [,5]  [,6]
#[1,]  337   11 1197  253 4848   551
#[2,]  368  420 1220  590   11  4034
#[3,]  597  592 1270  594   70  8961
#[4,] 1035 2194 1304 1035  172 72998
#[5,] 1641 4720 1682 1097  368  3671
#[6,] 1653 2716 1954 1197  474 59315
#[7,] 1961 1220 2194 1201  508   509
#[8,] 2194 8665 2716 1207  520  1220
#[9,] 2396  527 3481 1220  597   223
#[10,] 4776 4370 5952 1222  780  2291

item_num_2 <- factor(table(rec_mat_2))

dist_title <- "UBCF Item Amount Distribution"
qplot(item_num_2) + ggtitle(dist_title)
#Right skewed

item_num_sort_2 <- sort(item_num_2, decreasing = TRUE)
item_num_head_2 <- head(item_num_sort_2, n = 4)

top_items_table_2 <- data.frame(as.integer(names(item_num_head_2)), item_num_head_2)

for (i in 1:4){
  top_items_table_2[i,1] <- as.character(subset(movies, 
                                        movies$movieId == top_items_table_2[i,1])$title)
}

colnames(top_items_table_2) <- c("Movie", "Item Amount")
head(top_items_table_2)
#                                           Movie Item Amount
#904                           Rear Window (1954)          20
#1148 Wallace & Gromit: The Wrong Trousers (1993)          19
#1617                    L.A. Confidential (1997)          19
#1234                           Sting, The (1973)          15

#######################################################################

# Lets evaluating our reccomendation Model
# Split data into training/testing

train_split <- 0.8

min(rowCounts(movie_ratings)) # 11
# We will generate 6 items' recomendations and run it 1 time
items_given <- 6 
rating_good <- 3
eval_n <- 1 

eval_scheme <- evaluationScheme(data = movie_ratings, 
                              method = "split",
                              train = train_split, 
                              given = items_given, 
                              goodRating = rating_good, 
                              k = eval_n)

eval_scheme
#Evaluation scheme with 6 items given
#Method: ‘split’ with 1 run(s).
#Training set proportion: 0.800
#Good ratings: >=3.000000
#Data set: 378 x 435 rating matrix of class ‘realRatingMatrix’ with 36041 ratings.

getData(eval_scheme, "train")
#get data on train evaluation
#302 x 435 rating matrix of class ‘realRatingMatrix’ with 28960 ratings.

getData(eval_scheme, "known")
#get data on known items in recommendation model
#76 x 435 rating matrix of class ‘realRatingMatrix’ with 456 ratings.

getData(eval_scheme, "unknown") 
#get data on test items 
#76 x 435 rating matrix of class ‘realRatingMatrix’ with 6625 ratings.

qplot(rowCounts(getData(eval_scheme, "unknown"))) + 
  geom_histogram(binwidth = 10) + 
  ggtitle("Unknown User Items")
#right skewed

#This time we will use the Bootstrap method which reduces our computation time

eval_scheme2 <- evaluationScheme(data = movie_ratings, 
                              method = "bootstrap", 
                              train = train_split, 
                              given = items_given,
                              goodRating = rating_good, 
                              k = eval_n)

#create table of our evaluation to plot
train_table <- table(eval_scheme2@runsTrain[[1]])
n_reps <- factor(as.vector(train_table))

qplot(n_reps) + 
  ggtitle("Train Dataset Reps")
#reps means items repeated
# right skewed

#This time we will use cross-validation method to further evaluate our model
# run 4 times

eval_n2 <- 4
eval_scheme3 <- evaluationScheme(data = movie_ratings, 
                              method = "cross-validation",
                              given = items_given, 
                              goodRating = rating_good,
                              k = eval_n2)

eval_length <- sapply(eval_scheme3@runsTrain, length)
eval_length
#[1] 282 282 282 282

# Lets evaluate the movie ratings

eval_scheme4 <- evaluationScheme(data = movie_ratings, 
                              method = "cross-validation",
                              given = items_given, 
                              goodRating = rating_good,
                              k = eval_n2)

eval_rec <- Recommender(data = getData(eval_scheme4, "train"),
                                method = "IBCF", 
                                parameter = NULL)

#evaluate 10 items by predicting
eval_pred <- predict(object = eval_rec, 
                           newdata = getData(eval_scheme4, "known"), 
                           n = 10, 
                           type = "ratings")

qplot(rowCounts(eval_pred)) + 
  geom_histogram(binwidth = 10) +
  ggtitle("Movies Per User Distribution")
#a bit of normal distribution with slight left skewness

eval_pred_accuracy <- calcPredictionAccuracy(x = eval_pred, 
                                        data = getData(eval_scheme4, "unknown"), 
                                        byUser = TRUE)

head(eval_pred_accuracy)
#          RMSE       MSE       MAE
#[1,] 1.5575739 2.4260363 1.4258304
#[2,] 1.2440038 1.5475454 0.7437519
#[3,] 0.6392315 0.4086170 0.5002785
#[4,] 0.6648072 0.4419687 0.5451720
#[5,] 0.9618194 0.9250966 0.6816631
#[6,] 1.1108442 1.2339749 0.8536316


qplot(eval_pred_accuracy[, "RMSE"]) + 
  geom_histogram(binwidth = 0.1) +
  ggtitle("User RMSE Distribution")
#slight right skewness

##################################################
#Evaluate our model

results2 <- evaluate(x = eval_scheme4, 
                    method = "IBCF", 
                    n = seq(10, 100, 10))
#IBCF run fold/sample [model time/prediction time]
#1  [0.199sec/0.027sec] 
#2  [0.258sec/0.039sec] 
#3  [0.206sec/0.032sec] 
#4  [0.19sec/0.03sec] 

head(getConfusionMatrix(results2)[[1]])
#            TP       FP       FN       TN   N precision     recall        TPR        FPR  n
#[1,]  2.322917  7.46875 65.23958 353.9688 429 0.2372340 0.03977564 0.03977564 0.02063099 10
#[2,]  4.468750 15.11458 63.09375 346.3229 429 0.2281915 0.07437262 0.07437262 0.04173103 20
#[3,]  6.250000 23.12500 61.31250 338.3125 429 0.2127660 0.10251628 0.10251628 0.06380033 30
#[4,]  8.291667 30.80208 59.27083 330.6354 429 0.2122099 0.13026620 0.13026620 0.08494691 40
#[5,]  9.989583 38.73958 57.57292 322.6979 429 0.2050999 0.15578618 0.15578618 0.10705540 50
#[6,] 11.989583 46.32292 55.57292 315.1146 429 0.2055609 0.18912661 0.18912661 0.12806109 60

sum_cols <- c("TP", "FP", "FN", "TN")
sum_indices <- Reduce("+", getConfusionMatrix(results2))[, sum_cols]
head(sum_indices)
#           TP        FP       FN       TN
#[1,] 10.87500  28.60417 309.0938 1367.427
#[2,] 20.54167  58.41667 299.4271 1337.615
#[3,] 29.29167  89.12500 290.6771 1306.906
#[4,] 38.31250 119.33333 281.6562 1276.698
#[5,] 47.25000 149.40625 272.7188 1246.625
#[6,] 56.39583 179.10417 263.5729 1216.927

plot(results2, annotate = TRUE, main = "ROC Curve")

plot(results2, "prec/rec", annotate = TRUE, main = "Precision Recall")

#Model Comparison

models_comp <- list(
  IBCF_cos = list(name = "IBCF", 
                  param = list(method = "cosine")),
  IBCF_pear = list(name = "IBCF", 
                  param = list(method = "pearson")),
  UBCF_cos = list(name = "UBCF", 
                  param = list(method = "cosine")),
  UBCF_pear = list(name = "UBCF", 
                  param = list(method = "pearson")),
  random = list(name = "RANDOM", param=NULL)
)

n_recs <- c(1, 5, seq(10, 100, 10))

list_results <- evaluate(x = eval_scheme4, 
                         method = models_comp, 
                         n = n_recs)
#There was an error in running UBCF_pear

sapply(list_results, class) == "evaluationResults"
# IBCF_cos  IBCF_pear  UBCF_cos    random 
# TRUE      TRUE       TRUE        TRUE 

avg_list_results <- lapply(list_results, avg)
head(avg_list_results$IBCF_cos[, 5:8])
#       N precision      recall         TPR
#[1,] 429 0.2820455 0.004713841 0.004713841
#[2,] 429 0.2858854 0.020775740 0.020775740
#[3,] 429 0.2753443 0.038617542 0.038617542
#[4,] 429 0.2600285 0.070659118 0.070659118
#[5,] 429 0.2471841 0.100923811 0.100923811
#[6,] 429 0.2428072 0.130340412 0.130340412

######################################################

# Which is the best model?

plot(list_results, annotate = 1, legend = "topleft") 
title("ROC Curve")

## Optimizing a numeric parameter

k_vec <- c(5, 10, 20, 30, 40)
vecs_to_eval <- lapply(k_vec, function(k){
  list(name = "IBCF",
       param = list(method = "cosine", k = k))
})

names(vecs_to_eval) <- paste0("IBCF_k_", k_vec)

n_recs2 <- c(1, 5, seq(10, 100, 10))
list_results2 <- evaluate(x = eval_scheme4, 
                         method = vecs_to_eval, 
                         n = n_recs2)
#all ran
#IBCF run fold/sample [model time/prediction time]
#1  [0.213sec/0.03sec] 
#2  [0.335sec/0.032sec] 
#3  [0.184sec/0.024sec] 
#4  [0.191sec/0.023sec] 
#IBCF run fold/sample [model time/prediction time]
#1  [0.245sec/0.025sec] 
#2  [0.16sec/0.02sec] 
#3  [0.173sec/0.022sec] 
#4  [0.173sec/0.068sec] 
#IBCF run fold/sample [model time/prediction time]
#1  [0.175sec/0.022sec] 
#2  [0.169sec/0.026sec] 
#3  [0.174sec/0.024sec] 
#4  [0.188sec/0.021sec] 
#IBCF run fold/sample [model time/prediction time]
#1  [0.175sec/0.024sec] 
#2  [0.166sec/0.025sec] 
#3  [0.17sec/0.025sec] 
#4  [0.163sec/0.025sec] 
#IBCF run fold/sample [model time/prediction time]
#1  [0.175sec/0.027sec] 
#2  [0.164sec/0.027sec] 
#3  [0.17sec/0.026sec] 
#4  [0.165sec/0.027sec]

plot(list_results2, annotate = 1, legend = "topleft") 
title("ROC Curve IBCF")

plot(list_results2, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-Recall IBCF")