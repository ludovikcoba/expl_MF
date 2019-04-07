#### Requirements
if (!require(Rcpp)) install.packages("Rcpp")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(readr)) install.packages("readr")


#### Parameters
Neigh <- 10
Shrinkage <- 10 # damping on similarity computation.
explThreshold <- 3 # threshold for considering the rating on an item explainable.

# Import
#eval methods 
source("src/evalSplit.R") # load the splitting function. Stratified splitting of the dataset in tran/test, given a splitting ratio.
#similaryti 
sourceCpp("src/compute_similarity.cpp")
source("src/ALG_similarity.R")
# explainability 
source("src/ALG_Explainability.R")
# novelty 
source("src/ALG_Novelty.R")
# NEMF
sourceCpp("src/NEMFupdater.cpp")

#### Read Data

source("src/readML100K.R") # will load ml100k and movie_categories in the environment.


d <- evalSplit(ml100k, 0.25) # split train/test
d$train # train set
d$test # test set

#### Computing item's explainability.

#normalization
temp <- d$train

temp <- temp %>% group_by(user) %>% 
  summarise(offset = mean(score)) 

temp <- inner_join(temp, d$train) %>% 
  mutate(score = score - offset) %>% select(-offset)

# similarity compute
knnUsr <- similarity(temp, shrinkage = Shrinkage, by = c("user", "item", "score"))
knnUsr <- getKNN(knnUsr, Neigh)

head(knnUsr)

# Explainability 

Expl <- getExplainability(d$train, knnUsr)

# Novelty

Nvl <- Novelty(d$train, movie_categories) # not so efficient.
