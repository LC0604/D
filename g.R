g<-function(disbord){
  DT = disbord[1]
  GL = disbord[2]
  B = disbord[3]
  WD = 218
  asd = 2.1524
  
  cz = 0.7359
  D = 100
  
  tad=0
  for ( i in 1:(10-DT) ){
    tad<-dnorm(i,3,1)*dnorm(DT+i,3,1)+tad
  }
  
  vv<-rep(0,D)
  for (r in 1:D){
    
    if(r>=GL)
    {hh<-acos(GL/r)
    jj=acos((GL-B)/r)-hh
    
    vv[r]<-(2*r*hh)/(2*pi*r-2*r*jj)}
    else
    {vv[r]<-0}}
  dcau<-rep(0,D)
  for (i in 1:D){
    dcau[i]<-dcauchy(i,11.23672*cz*tad+(WD)*0+B*cz,0.4219011*asd)}
  
  rt<-c(asd*vv%*%dcau)
  return(rt)
}