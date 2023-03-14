##########################################################
# Create edx and final_holdout_test sets 
##########################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(recommenderlab)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(Matrix)) install.packages("reshape2", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(ggplot2)
library(caret)
library(reshape2)
library(recommenderlab)
library(Matrix)

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

# rating vs count 
b<-edx %>% group_by(rating) %>% summarize(count = n())
b %>% ggplot(aes(rating,count))+geom_point() +geom_line(color="red")

#number of ratings  vs movies
a<-edx %>% group_by(movieId) %>% summarize(count = n())
a %>% ggplot(aes(count))+geom_histogram(bins = 40,color="black")+scale_x_log10()+xlab("Number of ratings")+ylab("Number of movies")

#number of ratings vs Average rating
best<-edx %>% group_by(movieId) %>% summarise(Avg_rating=mean(rating),num_ratings=n()) %>% arrange(desc(num_ratings))
best %>% ggplot(aes(num_ratings,Avg_rating))+geom_point()+scale_x_log10()

#Genre 
gen<-edx %>% group_by(genres) %>% summarise(Avg_rating=mean(rating),num_ratings=n())
gen %>% arrange(Avg_rating) %>% print(n=10)

gen %>% arrange(desc(Avg_rating)) %>% print(n=10)
sing_gen<-gen%>%filter(str_count(genres, "\\|") == 0)

#genres vs number of ratings
sing_gen<-sing_gen %>% arrange(desc(num_ratings))
plot1<-sing_gen %>% ggplot(aes(y = genres, x = num_ratings)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Genres") +
  ylab("Number of ratings") +
  ggtitle("Number of ratings by genres") 

#naive RMSE
mu<-mean(edx$rating)
naive_rmse<-RMSE(final_holdout_test$rating,mu)
result_rmse<-data_frame(Method="Mean Movie rating",RMSE=naive_rmse)
result_rmse %>% knitr::kable()

#Movie effect
Avg_movies<- edx %>% group_by(movieId) %>% summarize(diff=mean(rating-mu))
Avg_movies %>% ggplot(aes(diff)) +geom_histogram(bins=20,color="black")+labs(y="Number of movies") # movie bias vs number of movies
predicted_ratings <- mu +  final_holdout_test %>%
  left_join(Avg_movies, by='movieId') %>%
  pull(diff)
RMSE_movie<-RMSE(predicted_ratings,final_holdout_test$rating)
result_rmse<-bind_rows(result_rmse,data_frame(Method="Movie effect",RMSE=RMSE_movie))
result_rmse %>% knitr::kable()

#Movie and user effect
Avg_Users<-edx %>% group_by(userId) %>%left_join(Avg_movies,by="movieId")%>% filter(n()>=128) %>% summarize(diff_u=mean(rating-mu-diff))
Avg_Users %>% ggplot(aes(diff_u))+geom_histogram(bins=25,color="black")  # diff_u vd count
Avg_Users<-edx %>% group_by(userId) %>%left_join(Avg_movies,by="movieId") %>% summarize(diff_u=mean(rating-mu-diff))
Avg_Users %>% print(n=5)
predicted_ratings<-mu+ final_holdout_test %>% left_join(Avg_movies,by="movieId")%>%left_join(Avg_Users,by="userId")%>% mutate(pred=diff+diff_u) %>% pull(pred)
RMSE_movieUser=RMSE(predicted_ratings,final_holdout_test$rating)
result_rmse<-bind_rows(result_rmse,data_frame(Method="Movie and User effect",RMSE=RMSE_movieUser))
result_rmse %>% knitr::kable()

#Movie and user lambda
lambdas<-seq(1,10,0.25)
rmses<-sapply(lambdas, function(x){
  #mu<-mean(edx$rating)
  diff<-edx %>% 
    group_by(movieId)%>%
    summarize(diff=sum(rating-mu)/(n()+x))
  diff_u<-edx %>%
    group_by(userId)%>%
    left_join(diff,by="movieId")%>%
    summarize(diff_u=sum(rating-diff-mu)/(n()+x))
  predicted_ratings<-mu+final_holdout_test%>%
    left_join(diff,by="movieId")%>%
    left_join(diff_u,by="userId")%>%
    mutate(pred=diff+diff_u) %>% 
    pull(pred)
  return(RMSE(predicted_ratings,final_holdout_test$rating))
})
qplot(lambdas,rmses) # lambda vs RMSEs
RMSE_lambda<-rmses[which.min(rmses)]
result_rmse<-bind_rows(result_rmse,data_frame(Method="Movie and User effect(optimal lambda)",RMSE=RMSE_lambda))
result_rmse %>%knitr::kable()

#Movie,user and genre effect
Avg_genre<-edx %>% group_by(genres)%>%left_join(Avg_movies,by="movieId")%>%left_join(Avg_Users,by="userId")%>%summarize(diff_g=mean(rating-mu-diff-diff_u))
Avg_genre %>% ggplot(aes(diff_g))+geom_histogram(bins=25,color="black") #diff_g vs count
Avg_genre %>% print(n=5)
predicted_ratings<-mu+ final_holdout_test %>% left_join(Avg_movies,by="movieId")%>%left_join(Avg_Users,by="userId")%>%left_join(Avg_genre,by="genres")%>% mutate(pred=diff+diff_u+diff_g) %>% pull(pred)
RMSE_MovieGenre<-RMSE(predicted_ratings,final_holdout_test$rating)
result_rmse<-bind_rows(result_rmse,data_frame(Method="Movie,User and genre effect",RMSE=RMSE_MovieGenre))
result_rmse %>%knitr::kable()





