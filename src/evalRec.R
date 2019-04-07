evalRec <- function(r, test, topN = 3, topNGen = F, positiveThreshold, alpha = 10,file, nItems, ...){
            
            if (r@data@binary) {
              if (!is.null(positiveThreshold)) {
                cat("NOTE: The \"positiveThreshold\" attribute is not needed for binary dataset.\n")
              }
              positiveThreshold <- 1
            }
            
            if (missing(positiveThreshold)) {
              positiveThreshold <- r@data@minimum
            }
            
            
            results <- data.frame(
              alg = c(),
              TP = c(), 
              FP = c(), 
              TN = c(), 
              FN = c(), 
              precision = c(), 
              recall = c(), 
              F1 = c(),
              nDCG = c(), 
              rankscore = c(), 
              item_coverage = c(),
              user_coverage = c()
            )
            
  
            
            if(missing(nItems)) nItems <- ncol(r@data)
            
            test <- test %>% 
              group_by(user) %>%
              nest(.key = testSet)
            
            computeMetrics <- function(){

              getResults <- function(x,y){
                
                if( is.null(x) || is.null(y)) {
                  res <- data.frame(
                    TP = 0, 
                    FP = 0, 
                    TN = 0, 
                    FN = 0, 
                    precision = 0, 
                    recall = 0, 
                    F1 = 0,
                    nDCG = 0, 
                    rankscore = 0
                  )
                  
                  return(res)
                  
                }
                relevantItems <- y %>% filter(score >= positiveThreshold)
                
                bingo <- inner_join(x,relevantItems, by = "item") # :)
                TP <- nrow(bingo)
                FN <- nrow(relevantItems) - TP
                FP <- nrow(x) - TP
                TN <- nItems - TP - FN -FP
                precision <- ifelse(nrow(x) != 0,  TP/nrow(x), 0)
                recall <- ifelse(TP + FN != 0, TP/(TP + FN), 0)
                F1 <- ifelse( (precision + recall) != 0, 2*precision*recall/(precision + recall), 0)
                nDCG <- eval_nDCG(x$item, y$item)
                rankScore <- rankScore(x$item, y$item, alpha)
                
                res <- data.frame(
                  TP = TP, 
                  FP = FP, 
                  TN = TN, 
                  FN = FN, 
                  precision = precision, 
                  recall = recall, 
                  F1 = F1,
                  nDCG = nDCG, 
                  rankscore = rankScore
                )
                
                res
              }

              
              for (top_n in topN) {

                 recom <- rec %>% 
                  group_by(user) %>%
                  slice(1:top_n) 
                
                item_coverage <- length(unique(recom$item))/nItems
                user_coverage <- length(unique(recom$user))/nrow(r@data)
                
                recom <- recom %>% 
                  group_by(user) %>%
                  nest()
                
                recom <- left_join(test, recom, by = "user")
                
                res <- recom %>% 
                  mutate(res = map2(data, testSet, getResults)) %>% 
                  select(user, res)
                
                res <- res %>% 
                  unnest()
                
                #results####
                
                res <- res %>% summarise_at(2:10, mean)
                
                config <- sapply(1:length(r@parameters), function(x) paste(names(r@parameters)[x], " = ", r@parameters[[x]]))
                config <- paste(config, collapse = "; ")
                results <<- rbind(results, cbind(r@alg, top_n, res, item_coverage, user_coverage, config))
                
              }
            }
            
            
            if(class(r)%in% c("UBclass", "IBclass")){
              
              neighborhood <- r@neigh
              
              if(length(r@neigh)>1){#because in this case the neighborhood is not filtered and ordered

                sim <- r@sim
                
                for (neigh in neighborhood) {
                  r@neigh <- neigh
                  r@sim <- getKNN(sim, neigh)
                  r@parameters <- list(simFunct = "cos", neigh = neigh)
                  rec <- recommend(r, max(topN), topNGen, positiveThreshold, ...)
                  computeMetrics()
                  if(!missing(file)) write.csv(results, file)
                  
                }
              }else{
                rec <- recommend(r, max(topN), topNGen, positiveThreshold, ...)
                computeMetrics()
                if(!missing(file)) write.csv(results, file)
              }
              
              
            }else{
              rec <- recommend(r, max(topN), topNGen, positiveThreshold, ...)
              computeMetrics()
              if(!missing(file)) write.csv(results, file)
            }
            
            
            results
          }



