# Installing / Loading stuff
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(dplyr)
library(ggplot2)
library(caret)

# Setting seed
set.seed(42)  # for reproducibility of our analysis

####
#### Movielens Project - Harvard Data Science Capstone
####

# Description:
# This scripts aims at developping a ML algorithm to predict user ratings on the movielens dataset

### Important Note: This script assumes that the user has dataframes edx and final_holdout_set available in the current environment.
### If this is not the case, we invite you to uncomment and run the following script:

# library(languageserver)
#
# dl <- "ml-10M100K.zip"
# if(!file.exists(dl))
#   download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
# 
# ratings_file <- "ml-10M100K/ratings.dat"
# if(!file.exists(ratings_file))
#   unzip(dl, ratings_file)
# 
# movies_file <- "ml-10M100K/movies.dat"
# if(!file.exists(movies_file))
#   unzip(dl, movies_file)
# 
# ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
#                          stringsAsFactors = FALSE)
# colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
# ratings <- ratings %>%
#   mutate(userId = as.integer(userId),
#          movieId = as.integer(movieId),
#          rating = as.numeric(rating),
#          timestamp = as.integer(timestamp))
# 
# movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
#                         stringsAsFactors = FALSE)
# colnames(movies) <- c("movieId", "title", "genres")
# movies <- movies %>%
#   mutate(movieId = as.integer(movieId))
# 
# movielens <- left_join(ratings, movies, by = "movieId")
# 
# # Final hold-out test set will be 10% of MovieLens data
# set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# # set.seed(1) # if using R 3.5 or earlier
# test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
# edx <- movielens[-test_index,]
# temp <- movielens[test_index,]
# 
# # Make sure userId and movieId in final hold-out test set are also in edx set
# final_holdout_test <- temp %>% 
#   semi_join(edx, by = "movieId") %>%
#   semi_join(edx, by = "userId")
# 
# # Add rows removed from final hold-out test set back into edx set
# removed <- anti_join(temp, final_holdout_test)
# edx <- rbind(edx, removed)
# 
# rm(dl, ratings, movies, test_index, temp, movielens, removed)
# 
# # Save edx and holdout sets
# write.csv(edx, file = "data/edx.csv")
# write.csv(final_holdout_test, file = "data/final_holdout_test.csv")
#
# Loading data
# edx = read.csv("data/edx.csv")
# final_holdout_test = read.csv("data/final_holdout_test.csv")
# 



# Briefly look at the data

head(edx)
str(edx)

#
# EXPLORATORY ANALISIS
#

# Hypothesis: Users that give more reviews are more critical. Let's test this
# Check if number of reviews per user impacts average grade given

# 1. Compute per-user stats
user_stats <- edx %>%
  group_by(userId) %>%
  summarise(
    number_of_reviews = n(),
    average_rating    = mean(rating)
  )

# 2. Get the quartiles
qs <- quantile(user_stats$number_of_reviews, probs = c(0, .25, .5, .75, 1))

# 3. Cut into bins based on those breaks
user_stats <- user_stats %>%
  mutate(
    review_bin = cut(
      number_of_reviews,
      breaks = qs,
      include.lowest = TRUE,
      labels = c(
        paste0("≤",   qs[2]),     # ≤1st Qu.
        paste0(qs[2]+1, "–", qs[3]),  # 1Q+1 to Median
        paste0(qs[3]+1, "–", qs[4]),  # Median+1 to 3Q
        paste0(">",   qs[4])       # >3rd Qu.
      )
    )
  )

# 4. Summarise by bin
bin_summary <- user_stats %>%
  group_by(review_bin) %>%
  summarise(
    users_in_bin  = n(),
    mean_of_means = mean(average_rating)
  )

print(bin_summary)

# 5. Plot it
ggplot(bin_summary, aes(x = review_bin, y = mean_of_means)) +
  geom_col() +
  labs(
    x = "Review-count bin",
    y = "Average rating given",
    title = "Average Movie Rating by User Activity Quartile"
  ) +
  theme_minimal()

# No clear relationship visible


# Scatterplot with smoothing

# 1. Pick, say, 5,000 random users
sample_users <- edx %>%
  distinct(userId) %>%
  slice_sample(n = 5000) %>%
  pull(userId)

# 2. Filter to just those users and compute per-user stats
user_stats_samp <- edx %>%
  filter(userId %in% sample_users) %>%
  group_by(userId) %>%
  summarise(
    number_of_reviews = n(),
    average_rating    = mean(rating),
    .groups = "drop"
  )

# 3. Scatterplot with LOESS (log x-axis)
ggplot(user_stats_samp, aes(x = number_of_reviews, y = average_rating)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_log10() +
  labs(
    x     = "Number of Reviews (log scale)",
    y     = "Average Rating",
    title = "Sampled Users: Review Activity vs. Mean Rating"
  ) +
  theme_minimal()

# There is perhaps a relationships for large number of reviews


### Analyzing movie genres

# 1. Pull out the full list of genres
all_genres <- unique(unlist(strsplit(edx$genres, "\\|")))

# 2. For each genre, grepl() in the original string
for(g in all_genres) {
  # make a syntactically valid column name
  col_name <- make.names(g)
  edx[[col_name]] <- grepl(g, edx$genres, fixed = TRUE)
}


# 1. Identify original columns so we can exclude them
orig_cols <- c("userId","movieId","rating","timestamp","title","genres")

# 2. All other columns in edx must be the genre flags
genre_cols <- setdiff(names(edx), orig_cols)

# 3. Proportion of all ratings that are in each genre
prop_by_genre <- sapply(genre_cols, function(g) {
  mean(edx[[g]], na.rm = TRUE)
})

# 4. Average rating and count for each genre
mean_rating_by_genre <- sapply(genre_cols, function(g) {
  mean(edx$rating[edx[[g]]], na.rm = TRUE)
})
count_by_genre <- sapply(genre_cols, function(g) {
  sum(edx[[g]], na.rm = TRUE)
})

# 5. Combine into a summary data.frame
genre_summary <- data.frame(
  genre        = genre_cols,
  prop_ratings = prop_by_genre,
  mean_rating  = mean_rating_by_genre,
  count        = count_by_genre,
  row.names    = NULL
)

# 6. View it
print(genre_summary)

# Genre can perhaps be used as a predictor



#
# MODELLING
#



#### Naive model Y = mu + error

# 1. Global mean on edx
mu <- mean(edx$rating)

# 2. Naive predictions = mu for every row
naive_pred <- rep(mu, nrow(edx))

# 3. RMSE on edx
train_naive_rmse <- sqrt(mean((edx$rating - naive_pred)^2))

# 4. Print results
print(mu)               # the baseline constant prediction
print(train_naive_rmse) # RMSE of that predictor on the training data



# RMSE of 1.06, not great but that's a good starting point


### Add movie bias: Y = mu + move_bias + error

# 1. Global mean
mu <- mean(edx$rating)

# 2. Movie biases (b_i)
bi <- edx %>%
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu), .groups = "drop")

# 3. Predictions with movie effect only
pred_movie <- edx %>%
  left_join(bi, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)

# 4. Compute training‐set RMSE
rmse_movie <- sqrt(mean((edx$rating - pred_movie)^2))
print(rmse_movie)

# That is better, with a training RMSE of 0.94

### Add user bias: Y = mu + movie_bias + user_bias + error

# 1. User biases (b_u) accounting for movie bias
bu <- edx %>%
  left_join(bi, by = "movieId") %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu - b_i), .groups = "drop")

# 2. Make predictions μ + b_i + b_u and compute RMSE
pred_um <- edx %>%
  left_join(bi, by = "movieId") %>%
  left_join(bu, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

rmse_um <- sqrt(mean((edx$rating - pred_um)^2))
print(rmse_um)



### Regularize biases -> shrink them when little data is available for movie or user

# 1) Define a grid of lambda values to try
lambdas <- seq(0, 10, by = 0.5)

# 2) Function to compute training RMSE for a given lambda
rmse_train <- function(lambda) {
  mu <- mean(edx$rating)
  
  # movie biases with regularization
  bi <- edx %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu) / (n() + lambda), .groups="drop")
  
  # user biases with regularization
  bu <- edx %>%
    left_join(bi, by = "movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - mu - b_i) / (n() + lambda), .groups="drop")
  
  # assemble predictions on the full training set
  preds <- edx %>%
    left_join(bi, by = "movieId") %>%
    left_join(bu, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  # compute RMSE
  sqrt(mean((edx$rating - preds)^2))
}

# 3) Loop over lambdas and collect RMSEs
train_rmses <- sapply(lambdas, rmse_train)

# 4) Combine into a data.frame and print
train_summary <- data.frame(lambda = lambdas, train_rmse = train_rmses)
print(train_summary)

# 5) Retrieve lambda with lower train RMSE
lambda_fit <- train_summary$lambda[which.min(train_summary$train_rmse)]

# 6) Print training RMSE for this lambda
train_summary$train_rmse %>% min()



####
#### Further exploration : Try a recommentation system library
####

# The idea is to model the ratings as the inner product of two matrices:
# Q: The item factors
# P: The user factors
# Under the hood, recosystems uses an alternating minimization / coordinate descent approach
# It finds the P and Q that minimize the regularized squared-error over all known ratings.


if (!requireNamespace("recosystem", quietly = TRUE)) {
  install.packages("recosystem")
}
library(recosystem)

edx %>% str()

# 1. Prepare the in‐memory train set for recosystem (according to package documentation)
train_data <- data_memory(
  user_index = edx$userId,
  item_index = edx$movieId,
  rating     = edx$rating
)

# 2. Create and train the Reco model (we will pick a set or default hyperparameters, no hyperparameter tuning for the moment)
r <- Reco()
r$train(
  train_data,
  opts = list(
    dim       = 10,
    lrate     = 0.1,
    costp_l2  = 0.01,
    costq_l2  = 0.01,
    niter     = 10,
    verbose   = FALSE
  )
)

# 3. Predict back on the full training set
train_preds <- r$predict(train_data, out_memory())

# 4. Compute training RMSE
train_rmse <- RMSE(train_preds, edx$rating)
print(train_rmse) 


# That's much better, we can now calculate the RMSE on the test set.

####
#### Testing error
####

# 1. Prepare the test data for recosystem (no ratings needed for predict)
test_data <- data_memory(
  user_index = final_holdout_test$userId,
  item_index = final_holdout_test$movieId
)

# 2. Generate predictions on the test set
test_preds <- r$predict(test_data, out_memory())

# 3. Compute and print the RMSE on final_holdout_test
test_rmse <- RMSE(test_preds, final_holdout_test$rating)
print(test_rmse)
