g <-function(disbord){
  DT = disbord[1]
  GL = disbord[2]
  B = disbord[3]
  WIND = 2.18
  
  tad=0
  for ( i in 1:(10-DT) ){
    tad<-dnorm(i,3,1)*dnorm(DT+i,3,1)+tad
  }
  tad = tad^0.2
  K = 0.00310
  b1 = 0.6713
  b2 = 0.1680
  D = 2.7552
  kk = 2.2242
  
    
  if (GL<=D){return(K*tad*exp(-b1*GL+kk*WIND))}
  else{return(K*tad*exp(-b1*D-b2*(GL-D)+kk*WIND))}
}
