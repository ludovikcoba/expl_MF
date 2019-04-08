#### Requirements
if (!require(Rcpp)) install.packages("Rcpp")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(readr)) install.packages("readr")

#######################
#######################
#######################
###### Edit Here ######
#######################
#######################
#######################

#### Parameters
Neigh <- 10
Shrinkage <- 10 # damping on similarity computation.
explThreshold <- 3 # threshold for considering the rating on an item explainable.
learningRate <- 0.001
regCoef <- 0.001
regCoefExplain <- 0.1
regCoefNovelty <- 0.1
nrfeat <- 80 #nr latent features
steps <- 100 # number of iterations
reg <- 2 # 1 MF, 2 L2 regulariztion, 3 L1 regularization

topN <- 10

# Read Data
source("src/readML100K.R") # will load ml100k and movie_categories in the environment.

source("src/evalSplit.R") # load the splitting function. Stratified splitting of the dataset in tran/test, given a splitting ratio.
d <- evalSplit(ml100k, 0.25) # split train/test

#######################
#######################
#######################
##### DO NOT EDIT #####
#######################
#######################
#######################

#### Computing item's similarity
#normalization
temp <- d$train
temp <- temp %>% 
  group_by(user) %>% 
  summarise(offset = mean(score)) 

temp <- inner_join(temp, d$train) %>% 
  mutate(score = score - offset) %>% select(-offset)

# similarity compute
sourceCpp("src/compute_similarity.cpp")
source("src/ALG_similarity.R")

knnUsr <- similarity(temp, shrinkage = Shrinkage, by = c("user", "item", "score"))
knnUsr <- getKNN(knnUsr, Neigh)

#### Explainability 
source("src/ALG_Explainability.R")
Expl <- getExplainability(d$train, knnUsr)

#### Novelty
source("src/ALG_Novelty.R")
Nvl <- Novelty(d$train, movie_categories) # not so efficient.

#### Train
sourceCpp("src/NEMFupdater.cpp")

train <- left_join(d$train, Expl, by = c("user", "item"))
train <- left_join(train, Nvl, by = c("user", "item"))

train$Explainability[is.na(train$Explainability)] <- 0;
train$Novelty[is.na(train$Novelty)] <- 0;


features <- NESVDupdater(
  as.matrix(train),
  learningRate, 
  regCoef,
  regCoefExplain,
  regCoefNovelty,
  nrfeat, # the total number of features.
  steps,
  reg # 1 MF, 2 L2 regulariztion, 3 L1 regularization
)

usrFeatures <- cbind(features$uID,features$U)
colnames(usrFeatures) <- c("user", paste0("f",1:nrfeat))
usrFeatures <- as.data.frame(usrFeatures)

itmFeatures <- cbind(features$iID, features$V)
colnames(itmFeatures) <- c("item", paste0("f",1:nrfeat))
itmFeatures <-as.data.frame(itmFeatures)

#### Recommend
source("src/ALG_Recommend.R")
rec <- Recommend(train, usrFeatures, itmFeatures, topN)

#### Evaluate 
