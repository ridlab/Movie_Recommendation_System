if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#Function to calculate the root mean squared error
RMSE <- function(True_Ratings,Predicted_Ratings){
  sqrt(mean((True_Ratings-Predicted_Ratings)^2))
}

#LSE of the mean
mu <- mean(edx$rating)
First_model_result <- RMSE(validation$rating,mu)

#Adding Movie Effect
movie_average <- edx %>% group_by(movieId) %>% summarize(m_i = mean(rating-mu))

predicted_ratings <- validation %>% left_join(movie_average, by = 'movieId') %>% pull(m_i) + mu
Second_model_result <- RMSE(predicted_ratings, validation$rating)

#Adding User Effect
user_average <- edx %>% left_join(movie_average, by = 'movieId') %>% group_by(userId) %>% summarize(u_i = mean(rating-mu-m_i))
predicted_ratings2 <- validation %>% left_join(user_average, by = 'userId') %>% left_join(movie_average, by = 'movieId') %>% mutate(pred = mu + u_i + m_i) %>% pull(pred)
Third_model_result <- RMSE(predicted_ratings2,validation$rating)

#Checking the significance of the genre effect
length(unique(edx$genres))
edx %>% group_by(genres) %>% summarise(g_i = mean(rating)) %>% filter(n()>=1)%>% ggplot(aes(g_i)) + geom_histogram(bins = 30)
 


#Adding Genre Effect
genre_average <- edx %>% left_join(user_average, by = 'userId') %>% left_join(movie_average, by = 'movieId') %>% group_by(genres) %>% summarize(g_ui = mean(rating-mu-m_i-u_i))
predicted_ratings3 <- validation %>% left_join(user_average, by = 'userId') %>% left_join(movie_average, by = 'movieId') %>% left_join(genre_average, by = 'genres') %>% mutate(pred2 = mu + u_i + m_i + g_ui) %>% pull(pred2)
Fourth_model <- RMSE(predicted_ratings3,validation$rating)


#Regularisation

#top 10 and worst 10
validation %>% left_join(movie_average, by ='movieId') %>% mutate(residual = rating - (mu+m_i)) %>% arrange(desc(abs(residual))) %>% slice(1:10) %>% pull(title)

movie_titles <- edx %>% select(movieId,title) %>% distinct()
movie_average %>% left_join(movie_titles, by = 'movieId') %>% arrange(desc(m_i)) %>% slice(1:10) %>% pull(title)
movie_average %>% left_join(movie_titles, by = 'movieId') %>% arrange((m_i)) %>% slice(1:10) %>% pull(title)

#How many times they have been rated
edx %>% count(movieId) %>% left_join(movie_average, by = 'movieId') %>%
  left_join(movie_titles, by = 'movieId') %>% arrange(desc(m_i)) %>% slice(1:10) %>% pull(n)

edx %>% count(movieId) %>% left_join(movie_average, by = 'movieId') %>%
  left_join(movie_titles, by = 'movieId') %>% arrange((m_i)) %>% slice(1:10) %>% pull(n)

#calculating values of m_i (penalised estimates)
lambda <- 2.5

movie_reg_averages <- edx %>% group_by(movieId) %>% summarize(m_i = sum(rating-mu)/(n()+lambda), n_i = n())

tibble(original = movie_average$m_i, regularised = movie_reg_averages$m_i, n = movie_reg_averages$n_i) %>% ggplot(aes(original, regularised, size = sqrt(n))) + geom_point(shape=1, alpha=.5)

#top 10 based on m_i
edx %>% count(movieId) %>% left_join(movie_reg_averages, by = 'movieId') %>% left_join(movie_titles, by = 'movieId') %>% arrange(desc(m_i)) %>% slice(1:10) %>% pull(title)

#Regularisation  based on m_i

predicted_ratings4 <- validation %>% left_join(movie_reg_averages, by = 'movieId') %>% mutate(pred = mu + m_i) %>% pull(pred)

Fourth_model_result <- RMSE(predicted_ratings4, validation$rating)

m_i <- edx %>% group_by(movieId) %>% summarise(m_i = sum(rating-mu)/(n()+2.5))
u_i <- edx %>% left_join(m_i, by = 'movieId') %>% group_by(userId) %>% summarise(u_i = sum(rating-m_i-mu)/(n()+2.5))
g_i <- edx %>% left_join(m_i, by = 'movieId') %>% left_join(u_i, by = 'userId') %>% group_by(genres) %>% summarise(g_i = sum(rating-m_i-u_i-mu)/(n()+2.5))

predicted_ratings5 <- validation %>% left_join(m_i, by = 'movieId') %>% left_join(u_i, by = 'userId') %>% left_join(g_i, by = 'genres') %>% mutate(pred = mu + m_i + u_i + g_i) %>% pull(pred)
 
fifth_model_result <- RMSE(predicted_ratings5, validation$rating)

#RESULTS

Results <- tibble('MODEL' =c('MEAN','USER_EFFECTS','MOVIE_EFFECTS', 'REGULARISED_MOVIE','REGULARISED_MOVIE_USER_GENRE'), 'RESULTS' = c(First_model_result, Second_model_result, Third_model_result, Fourth_model_result,fifth_model_result))
