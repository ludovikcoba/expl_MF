// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <unordered_map>

using namespace Rcpp;
const int USER = 0;
const int ITEM = 1;
const int SCORE = 2;
const int EXPL = 3;
const int NVL = 4;

template <class T>
inline int
  sgn(T v) {
    return (v > T(0)) - (v < T(0));
  }

// [[Rcpp::export]]
List NESVDupdater(
    NumericMatrix sparseRatingMat,
    double learningRate, 
    double regCoef,
    double regCoefExplain,
    double regCoefNovelty,
    int nrfeat, // the total number of features.
    int steps,
    bool eucl_manh,
    int nr_users,
    int nr_items
)
{
  
  std::unordered_map<int, NumericVector> user_features;
  std::unordered_map<int, NumericVector> item_features;
  
  
  NumericMatrix U(nr_users, nrfeat);
  NumericMatrix V(nr_items, nrfeat);
  
  double error;
  
  for(int i = 0; i < nr_users; i++){
    for(int j = 0; j < nrfeat ; j++){
      U(i,j) = R::runif(0,1) * sqrt(0.5f/nrfeat);
    }
  }
  
  for(int i = 0; i <  nr_items; i++){
    for(int j = 0; j < nrfeat ; j++){
      V(i,j) = R::runif(0,1) * sqrt(0.5f/nrfeat);
    }
  }
  
  double eij, deltaUif, deltaVjf, pred, novelty_ij, expl_ij;
  
  int i, j;
  
  for(int ss = 0; ss < steps; ss++){
    error = 0;
    //for every rating
    for(int k = 0; k < sparseRatingMat.nrow(); k++){
      
      pred = 0;
      // -1 to change the index according to C++.
      //user index.
      i = sparseRatingMat(k,USER) - 1;
      //item index.
      j = sparseRatingMat(k,ITEM) - 1;
      
      novelty_ij = sparseRatingMat(k,NVL);
      expl_ij = sparseRatingMat(k,EXPL);
      
      //compute paiwise multiplication on the features.
      for(int l = 0; l < nrfeat; l++){
        pred += U(i,l) * V(j,l);
      }
      
      
      //compute error
      eij = sparseRatingMat(k,SCORE) - pred;
      
      error += abs(eij);
      
      for(int feat = 0; feat < nrfeat; feat++){
        
        if(eucl_manh){
          deltaVjf = learningRate * (2 * eij * U(i,feat) - regCoef * V(j,feat) + regCoefExplain * (U(i,feat) - V(j,feat)) * expl_ij - sgn(U(i,feat) - V(j,feat)) * regCoefNovelty * novelty_ij);
          deltaUif = learningRate * (2 * eij * V(j,feat) - regCoef * U(i,feat) - regCoefExplain * (U(i,feat) - V(j,feat)) * expl_ij - sgn(U(i,feat) - V(j,feat)) * regCoefNovelty * novelty_ij);
        }else{
          
          //item feature 
          //deltaVjf = learningRate * (eij * U(i,feat) - regCoef * V(j,feat) + regCoefExplain * expl_ij + regCoefNovelty * V(j,feat) * novelty_ij * novelty_ij);
          deltaVjf = learningRate * (2*eij * U(i,feat) - regCoef * V(j,feat) - sgn(U(i,feat) - V(j,feat)) * (regCoefExplain * expl_ij + regCoefNovelty * novelty_ij));
          
          // user feature 
          //deltaUif = learningRate * (eij * V(j,feat) - regCoef * U(i,feat) - regCoefExplain * expl_ij);
          deltaUif = learningRate * (2*eij * V(j,feat) - regCoef * U(i,feat) - sgn(U(i,feat) - V(j,feat)) * (regCoefExplain * expl_ij + regCoefNovelty * novelty_ij));
          
          //update
        }

        V(j,feat) += deltaVjf;
        U(i,feat) += deltaUif;
      }
      
    }
    
    if((ss % 5) == 0) {
      Rcout << "At step:" << ss << " the objective error is: "<< error <<"\n";
    }
  }
  
  
  
  
  List ret;
  ret["U"] = U;
  ret["V"] = V;
  
  return ret;
  
}




