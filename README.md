# MovieLens
OVERVIEW

The MovieLens 10M dataset is a dataset that contains ratings and movie information provided by users on the MovieLens website. The dataset includes 10 million ratings and 100,000 tag applications applied to 10,677 movies by 69878 users. The ratings are on a scale from 0.5 to 5. The key steps that were performed to achieve this goal include:

1. Loading and cleaning of the data from the ratings and movies files in the dataset by reading in the data from the files, splitting the data, and setting appropriate column names. The data is then transformed so that the userId and movieId variables are integers and the rating variable is numeric.

2. Joining the ratings and movies data to create a single data frame, movielens by using a left_join function, which combines the two data frames on the movieId variable. A data frame is created that includes all the information from both the ratings and movies data frames.

3. Splitting the MovieLens data frame into two sets, edx and final_holdout_test, where final_holdout_test will be used to evaluate the performance of the recommendation algorithm and edx data set will be used for training the algorithm. The data is split using createDataPartition function with a seed of 1, and 10% of the data is selected as final_holdout_test set.

4. Creating an algorithm to predict the ratings for movies in the final_holdout_test set using the edx set and calculating the Root Mean Squared Error (RMSE) to evaluate the performance of the algorithm.
