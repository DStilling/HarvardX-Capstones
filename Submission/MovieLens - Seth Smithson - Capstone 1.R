## ----Library Load, echo=TRUE, message=FALSE, results='hide'------------------------------------------------------------------------------------------------------
# Clean the R environment
rm(list = ls())

# Trigger garbage collection and free up some memory
gc(TRUE, TRUE, TRUE)

# Time to check if the prerequisite packages are installed.  If they are not, R will download them.
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(h2o)) install.packages("h2o", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(Rcpp)) install.packages("Rcpp", repos = "http://cran.us.r-project.org")
if(!require(DataExplorer)) install.packages("DataExplorer", repos = "http://cran.us.r-project.org")

# Time to load the libraries.
library(tidyverse) # the swiss army knife for everything
library(caret) # A library that makes modeling easier
library(h2o) # A library that makes modeling much faster and use less memory
library(lubridate) # A helpful library for formatting data
library(data.table) # loading for fread function in the data loading
library(Rcpp) # contains functions useful for splitting up and assigning data
library(DataExplorer) # contains a function useful for EDA plotting



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Let's display the system info
sessionInfo()


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
dl <- tempfile()
options(timeout = 300, download.file.method="libcurl", url.method="libcurl") # Added to try and work around the download timeout
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later versions:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId), title = as.character(title), genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")

# if using R 3.6 or earlier:
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId], title = as.character(title), genres = as.character(genres))


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Display the very top 10 rows of the data table
head(movielens, 10)
head(movies, 10)
head(ratings, 10)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Display the Column Names, data type, and the initial values.
str(movielens)
str(movies)
str(ratings)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Before we move to much on, let's check the data tables for any missing data.
sum(is.na(movielens))
sum(is.na(movies))
sum(is.na(ratings))


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Check the number of rows before
sum(duplicated(movielens))
sum(duplicated(movies))
sum(duplicated(ratings))


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Summary - Displays the Mean, Median, 25th Quartile, 75th Quartile, Min, Max
summary(movielens)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Let's find the number of unique movies
movielens$movieId %>% unique() %>% length()


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Let's find the number of unique users
movielens$userId %>% unique() %>% length()


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Let's find the number of unique movie genres
movielens$genres %>% unique() %>% length()


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Let's find the number of movies within each genre
genres <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime","Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery","Romance", "SciFi", "Thriller", "War", "Western")
sapply(genres, function(g) {sum(str_detect(movielens$genres, g))})


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Let's find the top ten movies with the most ratings
movielens %>% group_by(movieId) %>%
  summarise(n_ratings=n(), title=first(title)) %>%
  top_n(10, n_ratings)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Let's find the frequency of ratings overall
movielens %>% group_by(rating) %>%
  summarise(n_ratings=n()) %>%
  top_n(10, n_ratings) %>%
  arrange(desc(n_ratings))


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Let's graph the occurrence of each rating
movielens %>%
    group_by(rating) %>%
    summarize(count = n()) %>%
    ggplot(aes(x = rating, y = count)) +
    geom_line() +
    ggtitle("Number of Occurence of each Rating")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Let's see the number of ratings for each movie
movielens %>%
  group_by(movieId) %>%
  summarise(n_reviews=n()) %>%
  ggplot(aes(n_reviews)) +
  geom_histogram(color="black") +
  scale_x_log10() +
  ggtitle("Histogram of Number of Reviews for each Movie")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Let's see the number of ratings made by each user
movielens %>%
  group_by(userId) %>%
  summarise(n_reviews=n()) %>%
  ggplot(aes(n_reviews)) +
  geom_histogram(color="black") +
  scale_x_log10() +
  ggtitle("Histogram of number of reviews made by each user")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Let's see the correlations between variables
plot_correlation(movielens)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Formatting the timestamps
ratings <- ratings %>% mutate(timestamp=as_datetime(timestamp)) #converting
ratings %>% rename(rating_datetime = timestamp) # rename the timestamp column to be more representative

movielens <- movielens %>% mutate(timestamp=as_datetime(timestamp))
movielens %>% rename(rating_datetime = timestamp) # rename the timestamp column to be more representative

# Check the results
head(ratings)
head(movielens)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
#extracting the premier date from the movies data frame
movies$premier_date <- stringi::stri_extract(movies$title, regex = "(\\d{4})", comments = TRUE ) %>% as.numeric()

#extracting the premier date from the movielens data frame
movielens$premier_date <- stringi::stri_extract(movielens$title, regex = "(\\d{4})", comments = TRUE ) %>% as.numeric()

# Check the results
head(movies)
head(movielens)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Let's one-hot encode the movies data frame
movies_one_hot <- movies %>% separate_rows(genres,sep = "\\|") %>% mutate(value=1)
movies_one_hot <- movies_one_hot %>% spread(genres, value, fill=0) %>% select(-"(no genres listed)")

# Let's one-hot encode the movielens data frame
movielens_one_hot <- movielens %>% separate_rows(genres,sep = "\\|") %>% mutate(value=1)
movielens_one_hot <- movielens_one_hot %>% spread(genres, value, fill=0) %>% select(-"(no genres listed)")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
# There are some errors that may occur that can only be fixed by installing and loading the Rcpp package in the current instance of R.
# install.packages("Rcpp")
# library(Rcpp) # Reloading the Rcpp package to address any potential error message
# Rcpp::Rcpp_precious_remove()

# Time to proceed with the edx R script to split the data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE) # The only caret function utilized (for splitting data)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Time to proceed with the edx R script to split the data
test_index_one_hot <- createDataPartition(y = movielens_one_hot$rating, times = 1, p = 0.1, list = FALSE) # The only caret function utilized (for splitting data)
edx_one_hot <- movielens_one_hot[-test_index,]
temp_one_hot <- movielens_one_hot[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation_one_hot <- temp %>% 
  semi_join(edx_one_hot, by = "movieId") %>%
  semi_join(edx_one_hot, by = "userId")

# Add rows removed from validation set back into edx set
removed_one_hot <- anti_join(temp_one_hot, validation_one_hot)
edx_one_hot <- rbind(edx_one_hot, removed_one_hot)

# Remove data no longer needed
rm(dl, ratings, movies, test_index, temp, movielens, removed, genres)


## ----initiate h2o------------------------------------------------------------------------------------------------------------------------------------------------
## Initialize H2O
h2o.init(
    nthreads = -1, # Specifying to use all processor cores available
    max_mem_size = "64G" # Specifying to use 64GB of RAM.
)


## ----convert data to h2o format----------------------------------------------------------------------------------------------------------------------------------
## Convert current dataframes to H2O dataframes
## This might also use a large amount of memory, so one data frame will be converted and removed at a time.

validation_one_hot_h2o <- as.h2o(validation_one_hot) # Convert current dataframe to h2o dataframe
validation_one_hot_h2o[c("rating", "userId", "movieId", "premier_date")] <- as.factor(validation_one_hot_h2o[c("rating", "userId", "movieId", "premier_date")]) #  Changing target column type to factor for classification work in h2o
# rm(validation_one_hot) # Remove old data file to preserve memory

edx_one_hot_h2o <- as.h2o(edx_one_hot) # Convert current dataframe to h2o dataframe
edx_one_hot_h2o[c("rating", "userId", "movieId", "premier_date")] <- as.factor(edx_one_hot_h2o[c("rating", "userId", "movieId", "premier_date")]) # Changing target column type to factor for classification work in h2o
# rm(edx_one_hot) # Remove old data file to preserve memory


## ----assign the target feature and training features-------------------------------------------------------------------------------------------------------------
# Assign the training and target variables.
y <- "rating" # target feature
x <- setdiff(names(edx_one_hot_h2o), y) # all the training features minus the target feature


## ----GLM model creation, echo=TRUE, message=FALSE, results='hide'------------------------------------------------------------------------------------------------

# Set hyperparameter grid for GLM:
glm_hyper_grid.h2o <- list(
  alpha = seq(0, 1, by = 0.05)
)

# Create a Random Discrete Grid Search
glm_search_criteria <- list(
    strategy = "RandomDiscrete", # Selecting Random Search of the hyperparameter grid
    stopping_metric = "RMSE", # The metric being optimized for in model training
    seed = 42, # Setting the seed to the answer to life, the universe, and everything.
    max_runtime_secs = 120*60 # Two hour run time
)

# Model 3:  Generalized Linear Model (GLM)
glm_random_grid <- h2o.grid(
    algorithm = "glm", # Selecting the Algorithm to build the model
    grid_id = "glm_grid", # Naming the Grid
    x = x, # Specifying the training data labels
    y = y, # Specifying the target attribute label
    nfolds = 5, # K-Fold Cross Validation
    training_frame = edx_one_hot_h2o, # Training Data selection
    hyper_params = glm_hyper_grid.h2o, # Specifying the hyperparameter grid to try
    search_criteria = glm_search_criteria # Specifying the search criteria to utilize
)



## ----GLM model evaluation----------------------------------------------------------------------------------------------------------------------------------------

# Collect the results and sort by our models: 
glm_grid_perf <- h2o.getGrid(
    grid_id = "glm_grid", # Grid of models
    sort_by = "RMSE", # Sorting the leaderboard by the desired metric
    decreasing = FALSE # Sort by Ascending/Increasing Values
)
print(glm_grid_perf)

# Select the best model
best_glm_model <- h2o.getModel(glm_grid_perf@model_ids[[1]])
print(best_glm_model@model[["model_summary"]])

# Retrieve the variable importance
glm_varimp <- h2o.varimp(best_glm_model)
print(glm_varimp)

# Check model performance
glm_perf <- h2o.performance(best_glm_model, validation_one_hot_h2o)
glm_rmse <- h2o.rmse(glm_perf)
print(glm_rmse)



## ----GBM model creation, echo=TRUE, message=FALSE, results='hide'------------------------------------------------------------------------------------------------

# Set hyperparameter grid:
gbm_hyper_grid.h2o <- list(
    ntrees = c(10, 50, 100, 150, 200),
    max_depth = c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21),
    learn_rate = c(0.001, 0.01, 0.05, 0.1, 0.15, 0.2)
)

# Create a Random Discrete Grid Search
gbm_search_criteria <- list(
    strategy = "RandomDiscrete", # Selecting Random Search of the hyperparameter grid
    stopping_metric = "RMSE", # The metric being optimized for in model training
    seed = 42, # Setting the seed to the answer to life, the universe, and everything.
    max_runtime_secs = 120*60 # Specifying a max run-time for this command in seconds (two hours)
)

# Model 3:  Gradient Boosting Machine modeling
gbm_random_grid <- h2o.grid(
    algorithm = "gbm", # Selecting the Algorithm to build the model
    grid_id = "gbm_grid", # Naming the grid
    x = x, # Specifying the training data labels
    y = y, # Specifying the target attribute label
    nfolds = 5, # K-Fold Cross Validation
    training_frame = edx_one_hot_h2o, # Training Data selection
    hyper_params = gbm_hyper_grid.h2o, # Specifying the hyperparameter grid to try
    search_criteria = gbm_search_criteria # Specifying the search criteria to utilize
)


## ----GBM model evaluation----------------------------------------------------------------------------------------------------------------------------------------

# Collect the results and sort by our models: 
gbm_grid_perf <- h2o.getGrid(
    grid_id = "gbm_grid", # Grid of models
    sort_by = "RMSE", # Sorting the leaderboard by the desired metric
    decreasing = FALSE # Sort by Ascending/Increasing Values
)
print(gbm_grid_perf)

# Best model: 
best_gbm_model <- h2o.getModel(gbm_grid_perf@model_ids[[1]])
print(best_gbm_model@model[["model_summary"]])

# Retrieve the variable importance
gbm_varimp <- h2o.varimp(best_gbm_model)
print(gbm_varimp)

# Check model performance
gbm_perf <- h2o.performance(best_gbm_model, validation_one_hot_h2o)
gbm_rmse <- h2o.rmse(gbm_perf)
print(gbm_rmse)



## ----Naive Bayes model creation, echo=TRUE, message=FALSE, results='hide'----------------------------------------------------------------------------------------

# Model 3:  Naive Bayes

# Set hyperparameter grid:
hyper_params <- list(
    laplace = seq(0, 1, by = 0.1)
)

# Create a Random Discrete Grid Search
nb_search_criteria <- list(
    strategy = "RandomDiscrete", # Selecting Random Search of the hyperparameter grid
    stopping_metric = "RMSE", # The metric being optimized for in model training
    seed = 42, # Setting the seed to the answer to life, the universe, and everything.
    max_runtime_secs = 120*60 # Specifying a max run-time for this command in seconds (two hours)
)

# Parameters for Naieve Bayes model
nb_random_grid <- h2o.grid(
    algorithm = "naivebayes", # Selecting the Algorithm to build the model
    grid_id = "nb_grid", # Naming the grid
    x = x, # Specifying the training data labels
    y = y, # Specifying the target attribute label
    nfolds = 5, # K-Fold Cross Validation
    training_frame = edx_one_hot_h2o, # Training Data selection
    hyper_params = hyper_params, # Specifying the hyperparameter grid to try
    search_criteria = nb_search_criteria # Specifying the search criteria to utilize
)



## ----Naive Bayes model evaluation--------------------------------------------------------------------------------------------------------------------------------

# Collect the results and sort by our models: 
nb_grid_perf <- h2o.getGrid(
    grid_id = "nb_grid", # Grid of models
    sort_by = "RMSE", # Sorting the leaderboard by the desired metric
    decreasing = FALSE # Sort by Ascending/Increasing Values
)
print(nb_grid_perf)

# Best model: 
best_nb_model <- h2o.getModel(nb_grid_perf@model_ids[[1]])
print(best_nb_model@model[["model_summary"]])

# Check model performance
nb_perf <- h2o.performance(
  best_nb_model, 
  newdata = validation_one_hot_h2o)
nb_rmse <- h2o.rmse(nb_perf)
print(nb_rmse)



## ----h2o Random Forest model creation, echo=TRUE, message=FALSE, results='hide'----------------------------------------------------------------------------------

# Set hyperparameter grid for RF:
rf_hyper_grid <- list(
    ntrees = seq(1, 100, by = 3),
    mtries = seq(1, 100, by = 3),
    max_depth = seq(1, 25, by = 3),
    min_rows = seq(1, 10, by = 1),
    nbins = seq(1, 1000, by = 25),
    sample_rate = seq(0.05, 0.95, by = 0.1)
)

# Create a Random Discrete Grid Search
rf_search_criteria <- list(
    strategy = "RandomDiscrete", # Selecting Random Search of the hyperparameter grid
    stopping_metric = "RMSE", # The metric being optimized for in model training
    max_runtime_secs = 120*60, # Specifying a max run-time for this command in seconds (two hours)
    stopping_tolerance = 0.005, # stop if improvement is < 0.5%
    stopping_rounds = 10 # Over the last 10 models
)

# Parameters for Random Forest Model
random_grid <- h2o.grid(
    algorithm = "randomForest", # Selecting the Algorithm to build the model
    grid_id = "rf_grid", # Naming the grid
    x = x, # Specifying the training data labels
    y = y, # Specifying the target attribute label
    seed = 42, # Setting the seed to the answer to life, the universe, and everything.
    nfolds = 5, # K-Fold Cross Validation
    training_frame = edx_one_hot_h2o, # Training Data selection
    hyper_params = rf_hyper_grid, # Specifying the hyperparameter grid to try
    search_criteria = rf_search_criteria # Specifying the search criteria to utilize
)



## ----h2o Random Forest model creation and evaluate---------------------------------------------------------------------------------------------------------------

# Collect the results and sort by our models: 
rf_grid_perf <- h2o.getGrid(
    grid_id = "rf_grid", # Grid of models
    sort_by = "RMSE", # Sorting the leaderboard by the desired metric
    decreasing = FALSE # Sort by Ascending/Increasing Values
)
print(rf_grid_perf)

# Best model: 
best_rf_model <- h2o.getModel(rf_grid_perf@model_ids[[1]])
print(best_rf_model)

# Retrieve the variable importance
rf_varimp <- h2o.varimp(best_rf_model)
print(rf_varimp)

# Check Model performance
rf_perf <- h2o.performance(best_rf_model, validation_one_hot_h2o)
rf_rmse <- h2o.rmse(rf_perf)
print(rf_rmse)



## ----h2o Neural Network model creation, echo=TRUE, message=FALSE, results='hide'---------------------------------------------------------------------------------

# Set hyperparameter grid:
nn_hyper_grid <- list(
    hidden = list(
      c(23), # One hidden layer with 23 nodes since there are 23 features/variables
      c(23,23), # two hidden layers
      c(23, 23, 23), # three hidden layers
      c(23, 23, 23, 23), # four hidden layers
      c(23, 23, 23, 23, 23) # five hidden layers
    ),
    input_dropout_ratio = seq(0, 0.1, by = 0.01),
    rate = seq(0, 0.05, by = 0.01),
    rate_annealing = c(1e-10,1e-9,1e-8,1e-7,1e-6,1e-5)
)

# Create a Random Discrete Grid Search
nn_search_criteria <- list(
    strategy = "RandomDiscrete", # Selecting Random Search of the hyperparamter grid
    stopping_metric = "RMSE", # The metric being optimized for in model training
    seed = 42, # Setting the seed to the answer to life, the universe, and everything.
    max_runtime_secs = 120*60 # Specifying a max run-time for this command in seconds (two hours)
)

# Parameters for Gradient Boost Machine
nn_random_grid <- h2o.grid(
    algorithm = "deeplearning", # Selecting the Algorithm to build the model
    grid_id = "nn_grid", # naming the grid
    x = x, # Feature  variable selection
    y = y, # Target variable selection
    nfolds = 5, # K-Fold Cross Validation
    training_frame = edx_one_hot_h2o, # Training Data selection
    hyper_params = nn_hyper_grid, # Specifying the hyperparameter grid to try
    search_criteria = nn_search_criteria # Specifying the search criteria to utilize
)



## ----h2o Neural Network model evaluation-------------------------------------------------------------------------------------------------------------------------

# Collect the results and sort by our models: 
nn_grid_perf <- h2o.getGrid(
    grid_id = "nn_grid", # Grid of models
    sort_by = "RMSE", # Sorting the leaderboard by the desired metric
    decreasing = FALSE # Sort by Ascending/Increasing Values
)
print(nn_grid_perf)

# Best model: 
best_nn_model <- h2o.getModel(nn_grid_perf@model_ids[[1]])
print(best_nn_model@model[["model_summary"]])

# Check model performance
nn_perf <- h2o.performance(
  best_nn_model, 
  newdata = validation_one_hot_h2o)
nn_rmse <- h2o.rmse(nn_perf)
print(nn_rmse)


## ----AutoML model creation, echo=TRUE, message=FALSE, results='hide'---------------------------------------------------------------------------------------------

# Setup the Auto Machine Learning Modeling
aml2 <- h2o.automl(
    x = x, # Specifying the training data labels
    y = y, # Specifying the target attribute label
    training_frame = edx_one_hot_h2o, # Specifying the data to be used for training the model
    validation_frame = validation_one_hot_h2o, # Specifying the data to be used for evaluating the model
    stopping_metric = c("RMSE"), # The metric being optimized for in model training
    stopping_rounds = 5, # Specifying that each algorithm-iteration should be judged against the moving average of the last five models
    sort_metric = c("RMSE"), # Sorting the leaderboard by the desired metric
    nfolds = 0, # Normally n-folds would be != 0 in order to use cross-validation.  However, for AutoML, when n-folds !=0, the validation_frame is ignored
    seed = 42, # Setting the seed for reproducibility and the answer to the universe, life, and everything
    max_runtime_secs = 120*60, # Specifying a max run-time for this command in seconds (two hours)
)



## ----AutoML model evaluation, echo=TRUE, message=FALSE, results='hide'-------------------------------------------------------------------------------------------

# Best model: 
best_aml2_model <- h2o.get_best_model(aml2)
print(best_aml2_model)

aml <- h2o.get_best_model(aml2)
preds <- h2o.predict(aml, validation_one_hot_h2o)
aml@model$validation_metrics@metrics$max_criteria_and_metric_scores
print(aml@model$validation_metrics@metrics$max_criteria_and_metric_scores)

perf <- h2o.performance(model = aml, newdata = validation_one_hot_h2o)
autoML_rmse <- h2o.rmse(perf)
print(autoML_rmse)


## ----Summary Results of Models-----------------------------------------------------------------------------------------------------------------------------------

## Display a table summarizing the RSME generated by models
rmse_results <- c(glm_rmse, gbm_rmse, nb_rmse, rf_rmse, nn_rmse, autoML_rmse) # Column of metric results
rmse_names <-  c("GLM", "GBM", "NB", "RF", "NN", "AutoML") # Label column for the corresponding metrics
rmse_df <- base::data.frame(rmse_names, rmse_results) 
 # Create a dataframe of results and sort in ascending order
print(rmse_df %>% dplyr::arrange(rmse_df$rmse_results)) # Display the sorted dataframe



## ----Closing the h2o Cluster-------------------------------------------------------------------------------------------------------------------------------------

# Close the h2o cluster
# h2o.shutdown(prompt=F)


