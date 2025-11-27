##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(languageserver)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Save edx and holdout sets
write.csv(edx, file = "data/edx.csv")
write.csv(final_holdout_test, file = "data/final_holdout_test.csv")

### Q1 : nrows in edx data set

nrow(edx)
ncol(edx)

### Q2 how many zeroes as ratings

edx$rating %>% min()

sum(edx$rating == 0)

sum(edx$rating == 3)

# Q3 

edx %>% group_by(movieId) %>% summarise(n()) %>% nrow()

edx$movieId %>% unique() %>% length()

# Q4 how many different users in the dataset

edx$userId %>% unique() %>% length()

# Genre counts
selected_genres <- c("Drama", "Comedy", "Thriller", "Romance")

for (genre in selected_genres) {
  genre_count <- edx %>%
    filter(str_detect(genres, genre)) %>%
    nrow()
  
  cat(genre, "rating count:", genre_count, "\n")
}


# Which movie has greatest number of ratings

edx %>% group_by(title) %>% 
  summarise(n_ratings = n()) %>% arrange(-n_ratings)


# Which grades are most given

edx %>% group_by(rating) %>% summarise(n = n()) %>% arrange(-n)

# Are half star ratings less common?

edx %>% mutate(rating_half_whole = ifelse(round(rating)==rating, "whole", "half")) %>%
  group_by(rating_half_whole) %>% summarise(n())


