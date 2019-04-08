Novelty <- function(data, categories){

  users<- inner_join(data, categories, by = c("item"="movieID") )
  
  # count how many times a user has seen a category
  nvl <- users %>%
    select(-item, -score) %>%
    group_by(user) %>%
    summarise_all(funs(sum))
  

  # group user items and categories
  nvl <- nvl %>% group_by(user) %>% 
    nest(.key = "usrNov") 
  
  
  users <- users %>% 
    group_by(user) %>% 
    select(-score) %>% 
    nest(.key = "rated")
  
  users <- inner_join(users, nvl)
  
  nvlCmp <- function(rated, usrNov){

    r <- as.matrix(rated[,-1])
    usrNov <-1/(r %*% t(usrNov)) # asuming that each item belong to a category we and based on how many times a user has seen a cetegory, we consider the usrNov to be the inverse of the weight of the category to the user.
    
    data.frame(item = rated$item, Novelty = usrNov)
    
  }
  
  # compute and return novelty 
  users %>% 
    mutate(Novelty = map2(rated, usrNov, nvlCmp)) %>% 
    select(user, Novelty) %>% 
    unnest()
  
  
}