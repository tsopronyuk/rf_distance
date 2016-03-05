#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
using namespace RcppParallel;

#include <map>
#include <vector>
#include <utility>
#include <algorithm>
using namespace std;

typedef pair<pair <int, int>, double>  TElementOne;
typedef std::vector < TElementOne > TVectorOne;

TVectorOne getTryForOne(DataFrame df) {
    Rcpp::DataFrame copy(df);
    Rcpp::NumericMatrix orig=internal::convert_using_rfunction(copy, "as.matrix");
    
    Rcpp::NumericVector myv, mynv, nodeID, g(2), gT,empty;
    Rcpp::NumericVector x,y,dist,li,lj;
    
    Rcpp::NumericVector vec=orig(3,_); 
    //Rcpp::Rcout<<vec;
    
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
        vec=orig(1,_);
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
        TVectorOne rows;
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
                
                
                //el=nodeID[i];
                //x.push_back(el);
                
                //el=nodeID[j];
                //y.push_back(el);
                
                //myil[[i]][1] + myil[[j]][1] - 2 * myil[[i]][2:length(myil[[i]])][id] + 2
                
                el=li[0] + lj[0] - 2 * li[id] + 2;
                //dist.push_back(el);
                
                TElementOne row;
                
                row.first.first=nodeID[i];
                row.first.second=nodeID[j];
                row.second=el;
                rows.push_back(row);
                  
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
        
        return rows;
}

typedef pair<pair <int, int>, vector <double> > TElement;
typedef std::map <pair <int, int>,  vector <double> >  TMap;



DataFrame getDataFrame(TMap all, int nTree) {
    
    Rcpp::NumericVector x, y, dist, null;
    
    Rcpp::CharacterVector namevec;
    List lists(nTree+2);
    
    std::vector<double> v(nTree,-1);
    std::vector<vector <double> > dists(nTree); 
    
    
    namevec.push_back("x");
    namevec.push_back("y");
    
    std::string namestem = "tree_";
    for(TMap::const_iterator it1 = all.begin(); it1 != all.end(); ++it1){
        x.push_back(it1->first.first);
        y.push_back(it1->first.second);
        
        for(int n=0;n<nTree;n++){
            dists[n].push_back(it1->second[n]);
        }
        
    }
    
    lists[0] = x; 
    lists[1] = y; 
    
    for(int n=0;n<nTree;n++){
        namevec.push_back(namestem + std::to_string(n+1));
        lists[n+2]=dists[n];
    }
    
    lists.attr("names") = namevec;
    Rcpp::DataFrame dfout(lists);
    return dfout;
}

   
TMap getMap(DataFrame df, int nTree) {
    TMap all;
    TVectorOne one;
    Rcpp::DataFrame obj(df);
   
    Rcpp::NumericVector treeID, x, y, dist,null;
    List lists(nTree+2);
   
    Rcpp::DataFrame empty;
    Rcpp::NumericMatrix mat=internal::convert_using_rfunction(df, "as.matrix");
    Rcpp::DataFrame treeAgeID;  
 
  NumericVector::iterator  it2; 
  
  std::vector<double> v(nTree,-1);
  std::vector<vector <double> > dists(nTree); 
  
    int i=0;
    treeID = obj["treeID"];
   for (int n = 1; n<=nTree;  n++){
       
        for (i=0,it2 = treeID.begin(); it2 != treeID.end(); i++, ++it2){
            if(*it2 == n){
                treeAgeID.push_back(mat(i,_)); 
            }
        }
         
    one=getTryForOne(treeAgeID);
    treeAgeID=empty;    
        
        
        for( i=0; i<one.size();i++){
            TElement  node;
            node.first=one[i].first;
            
            if ( all.find(node.first) == all.end() ){
                    
                 node.second=v;
                 all.insert(node); 
                 all.at(node.first)[n-1]=one[i].second;
            }
            else{
                all.at(node.first)[n-1]=one[i].second;
            }
                
         }
    }
   return all;
}



// [[Rcpp::export]]
// 
DataFrame getDistForTreesCPP(DataFrame df, int nTree) {
    TMap all=getMap(df,nTree);
    return getDataFrame(all,nTree);
}
    
// case 1, case 2, index of the tree (from 0)
double getGijt(TMap all, int currentX, int currentY, int t) {
    
    TElement  node;
    
    node.first.first=currentX;
    node.first.second=currentY;
    if (all.find(node.first) == all.end() ) return -1.;
    double d=all.at(node.first)[t];
    return d;
}

// Calculate matrix with distances

// [[Rcpp::export]]
// 
NumericMatrix getMatixDistances(DataFrame df,  DataFrame member, int w=2){
    DataFrame memb(member); 
    int nTree=memb.length();
    TMap all=getMap(df,nTree);
    
    //nTree=3;
    Rcpp::NumericMatrix mat=internal::convert_using_rfunction(memb, "as.matrix");
    
    int size=memb.nrows();
    
    NumericMatrix mDist(size,size);
    
    for(int i=0; i<size-1; i++) {
    for(int j=i+1; j<size; j++){ 
    
    NumericMatrix::Row row1 = mat.row(i);
    NumericMatrix::Row row2 = mat.row(j);
    
    double sum=0.0, d;
    for(int t=0; t<nTree; t++){ 
    
        if(row1[t]< row2[t]) 
            d=getGijt(all, row1[t], row2[t], t);
        else
            d=getGijt(all, row2[t], row1[t], t);
    
        if(d>0) sum+=1. / exp(w * d);
    
    //if(i<3)
    //Rcpp::Rcout<<"row1[t] "<<row1[t]<<" row2[t]"<< row2[t]<< " t"<< t 
    //           << " d"<< d <<" sum"<<sum <<"\n";
            }
    
            mDist(i,j)=mDist(j,i)=1.-sum/nTree;
        }
    }
    return mDist;
    }

struct JsDistance : public Worker {
    
    // input matrix to read from
    //const RMatrix<double> mTree;
    const RMatrix<double> mMember;
    const std::size_t nTree;
    const std::size_t w;
    const TMap all;
    
    // output matrix to write to
    RMatrix<double> mDist;
    
    // initialize from Rcpp input and output matrixes (the RMatrix class
    // can be automatically converted to from the Rcpp matrix type)
    JsDistance(const NumericMatrix& mMember, 
               const std::size_t nTree, const TMap& all, NumericMatrix mDist, const  std::size_t w=2)
        : mMember(mMember), nTree(nTree), w(w), all(all), mDist(mDist) {}
    
    // function call operator that work for the specified range (begin/end)
    void operator()(std::size_t begin, std::size_t end) {
        
        for (std::size_t i = begin; i < end; i++) {
            for (std::size_t j = 0; j < i; j++) {      
                
                // rows we will operate on
                RMatrix<double>::Row row1 = mMember.row(i);
                RMatrix<double>::Row row2 = mMember.row(j);
                
                double sum=0.0, d;
                for(int t=0; t<nTree; t++){ 
                    
                    if(row1[t]< row2[t]) 
                        d=getGijt(all, row1[t], row2[t], t);
                    else
                        d=getGijt(all, row2[t], row1[t], t);
                    
                    if(d>0) sum+=1. / exp(w * d);
                    
                }
                
                mDist(i,j)=mDist(j,i)=1.-sum/nTree;
            }
        }
        
    }
};
//Now that we have the JsDistance function object we can pass it to parallelFor, specifying an iteration range based on the number of rows in the input matrix:

// [[Rcpp::export]]
NumericMatrix getParallelMatixDistances(DataFrame df,  DataFrame member, int w=2){
    Rcpp::DataFrame memb(member);
    Rcpp::NumericMatrix matMember=internal::convert_using_rfunction(memb, "as.matrix");
    //Rcpp::NumericMatrix matTree=internal::convert_using_rfunction(df, "as.matrix");
    std::size_t nTree=memb.length(), wPar=w;
    TMap all=getMap(df,nTree);
    
    std::size_t size=memb.nrows();
    
    // allocate the matrix we will return
    NumericMatrix mDist(size, size);
    
    
    // create the worker
    JsDistance jsDistance(matMember, nTree, all,  mDist, wPar);
    
    // call it with parallelFor
    parallelFor(0, matMember.nrow(), jsDistance);
    
    return mDist;
}

/*** R

m<-getParallelMatixDistances(treeAgeT, membership)
*/


