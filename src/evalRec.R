evalRec <- function(rec, test, topN = 3, positiveThreshold = 3){
  browser()
  test <- test %>% filter(score >= positiveThreshold)
  
  nrUsr <- length(unique(test$user))
  
  #### Precision
  TPcount <- semi_join(rec, test, by = c("user", "item")) %>% 
    group_by(user) %>%
    summarise(TP = n())
  FPcount <- anti_join(rec, test, by = c("user", "item")) %>% 
    group_by(user) %>%
    summarise(FP = n())
  
  prec <- full_join(TPcount, FPcount)
  prec[is.na(prec)] <- 0
  
  prec <- prec %>% mutate(precision = TP/(TP + FP))
  
  # mean precision
  precision <- sum(prec$precision)/nrUsr

  #### nDCG
  
  
  
}



