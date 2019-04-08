evalRec <- function(rec, test, topN = 3, positiveThreshold = 3){

  test <- test %>% filter(score >= positiveThreshold)
  
  nrUsr <- length(unique(test$user))
  
  Hits <- semi_join(rec, test, by = c("user", "item")) 
  
  #### Precision
  
  
  TPcount <- Hits %>% 
    group_by(user) %>%
    summarise(TP = n())
  FPcount <- anti_join(rec, test, by = c("user", "item")) %>% 
    group_by(user) %>%
    summarise(FP = n())
  
  precUsr <- full_join(TPcount, FPcount)
  precUsr[is.na(precUsr)] <- 0
  
  precUsr <- precUsr %>% mutate(precision = TP/(TP + FP))
  
  # mean precision
  precision <- sum(precUsr$precision)/nrUsr

  #### nDCG

  nDCGusr <- Hits %>% 
    select(user,rank) %>%
    group_by(user) %>%
    nest() %>%
    mutate(val = map2(data, topN, eval_nDCG)) %>% 
    select(user, val) %>% unnest()
  
  # mean nDCG
  nDCG <- sum(nDCGusr$val) / nrUsr
  
  
  #### MEP
  browser()
  
}

eval_nDCG <- function(rank, topN){
  
  idcg <- getiDCG(topN)
  rank <- rank$rank
  if(1 %in% as.vector(rank)){
    dcg <- 1/log2(rank[-1])
    dcg <- 1 + sum(dcg)
  }else{
    dcg <- sum(1/log2(rank))
  }
  
  dcg/idcg
}

getiDCG <- function(n){
  
  idcg <- 1
  
  if(n > 1){
    idcg <- idcg + sum(1/log2(2:n))
  }
  
  idcg
}
