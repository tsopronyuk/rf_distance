#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
// 
  DataFrame getDistCPP(DataFrame df) {
    Rcpp::DataFrame orig(df);
    
    Rcpp::NumericVector myv, mynv, nodeID, g(2), gT,empty;
    Rcpp::NumericVector x,y,dist,li,lj;
    
    Rcpp::NumericVector vec=orig["contPT"]; 
    
    
    int i=0,k;
    NumericVector::iterator it;
    
    // create myv and mynv 
    for(it = vec.begin() ; it != vec.end(); ++it,++i) 
      if ( internal::Rcpp_IsNA((*it)) || 
           internal::Rcpp_IsNaN(*it)) 
      {
        mynv.push_back(i);
        myv.push_back(1);
      }
    else myv.push_back(0);
    
    int size=mynv.size();
    
    //  nodeID <- x$nodeID[mynv]
    vec=orig["nodeID"];
    nodeID=empty;
    
    for( i=0; i<size; i++)
    {
      k=vec[mynv[i]];
      nodeID.push_back(k);
    }
    
    
    // myil <- list()
    // for (i in 2:length(myv)) 
      List myil;
    g[0]=g[1]=0;
    int j;
    
    // for( i=1; i< myv.size() ; i++) { 
      for( i=1; i< myv.size() ; i++) { 
        
        gT=empty;
        
        if (myv[i - 1]) {
          
          // myil <- append(myil, list(g)) 
          myil.push_back(g); 
          // g <- c(g[length(g)], g[2:(length(g) - 1)])
          gT.push_back(g[g.size()-1]);        
          j=1;    
          while(j<g.size()-1)gT.push_back(g[j++]); 
          g=gT;
          
        } else {
          
          gT.push_back(g[0]+1);
          j=1;    
          while(j<g.size())gT.push_back(g[j++]);
          gT.push_back(g[0]+1);
          g=gT;     
        }
        
      }
      
      myil.push_back(g); 
      
      
      //   length: (length(mynv)^2 - length(mynv)) / 2; 
      //    int l=(size*size-size) /2;   
      int el;
      
      for( i=0; i<size-1; i++) {
        for( j=i+1; j<size; j++){ 
          
          //id <- which.min(myil[[i]][2:length(myil[[i]])] %in% myil[[j]][2:length(myil[[j]])])
          li=myil[i];
          lj=myil[j];      
          int id=5, flag;
          for( int i1=1; i1<li.size(); i1++) {
            el=li[i1];
            flag=0;
            id=1;
            
            for( int i2=1; i2<lj.size(); i2++)
              if (el == lj[i2]) {flag=1;break;}
            
            if (!flag) {id = i1; break;}
          }
          
          
          el=nodeID[i];
          x.push_back(el);
          
          el=nodeID[j];
          y.push_back(el);
          
          //myil[[i]][1] + myil[[j]][1] - 2 * myil[[i]][2:length(myil[[i]])][id] + 2
          
          el=li[0] + lj[0] - 2 * li[id] + 2;
          dist.push_back(el);
        }
      }
      
      /*
        return Rcpp::List::create(
          Rcpp::Named("myv") = myv,
          Rcpp::Named("mynv") = mynv,
          Rcpp::Named("nodeID") = nodeID,
          Rcpp::Named("l") = l,
          Rcpp::Named("g") = g,
          Rcpp::Named("x") = x,
          Rcpp::Named("y") = y,
          Rcpp::Named("dist") = dist,
          Rcpp::Named("myil") = myil);
      
      */
        
        return DataFrame::create(_["x"]= x, _["y"]= y, _["dist"]=dist);
    }
