
ml100k <- read_table2("datasets/ml-100k/u.data", #<---dataset
                      col_names = FALSE)

ml100k <- ml100k[,-4]

colnames(ml100k) <-  c("user", "item", "score")

movie_categories <- read_delim("datasets/ml-100k/u.item", #<----categories' file
                               ";", escape_double = FALSE, col_names = FALSE,      trim_ws = TRUE)


colnames(movie_categories) <- c("movieID", "movie title", "release date", "video release date", "IMDb URL", "unknown", "Action", "Adventure", "Animation",              "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy",              "Film-Noir", "Horror", "Musical", "Mystery", "Romance", "Sci-Fi",        "Thriller", "War", "Western")


movie_categories <-movie_categories %>%
  mutate_at(.vars = 6:24, funs(as.numeric(.))) %>%
  select_at(.vars = c(1,6:24))
movie_categories

movie_categories <- movie_categories %>%
  arrange(movieID)

