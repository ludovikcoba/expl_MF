Novelty <- function(data, categories){

  users<- inner_join(data, categories, by = c("item"="movieID") )
  
  # count how many times a user has seen a category
  nvl <- users %>%
    select(-item, -score) %>%
    group_by(user) %>%
    summarise_all(funs(sum))
  

  # group user items and categories
  nvl <- nvl %>% group_by(user) %>% 
    nest(.key = "novelty") 
  
  
  users <- users %>% 
    group_by(user) %>% 
    select(-score) %>% 
    nest(.key = "rated")
  
  users <- inner_join(users, nvl)
  
  nvlCmp <- function(rated, novelty){

    r <- as.matrix(rated[,-1])
    novelty <-1/(r %*% t(novelty)) # asuming that each item belong to a category we and based on how many times a user has seen a cetegory, we consider the novelty to be the inverse of the weight of the category to the user.
    
    data.frame(items = rated$item, novelty)
    
  }
  
  # compute and return novelty 
  users %>% 
    mutate(novelty = map2(rated, novelty, nvlCmp)) %>% 
    select(user, novelty) %>% 
    unnest()
  
  
}