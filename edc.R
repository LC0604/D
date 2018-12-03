
Dis<- function(cord,polygon2){
  nr <- nrow(cord %>% data.frame)
  gr <- polygon2@data %>% nrow
  
  bpoints <- data.frame(datediff = rep(0,nr), D = rep(0,nr), BF =  rep(0,nr))
  for (i in 1 : nr){
    
    ngps <- data.frame(x = rep(cord@coords[i,1],gr),y = rep(cord@coords[i,2],gr), 
                       x2 = rep(0,gr),y2 = rep(0,gr), date = rep(0,gr), D = rep(0,gr))
    
    for(j in 1:gr){
      
      p11 <-  cord@coords[i,-3]
      
      p22 <-  gNearestPoints(cord[i,], polygon2[j,])@coords[2,]
      ngps[j,] <-  c(p11, p22, polygon2@data$date[j]%>%as.character(), lens(p11,p22))
    }
    
    mmp <- ngps[which.min(ngps$D),]
    
    
    bxline <- matrix(c(mmp[,1:2],mmp[,3:4])%>%as.numeric(),ncol=2,byrow=T) %>% Line()
    bx <- SpatialLines(list(Lines(bxline, ID="a")))
    proj4string(bx) <- "+proj=utm +zone=51 ellps=WGS84"
    
    if (gIntersects(bx, polygon1[cord@coords[,3][i]]) == T) {
      ds <- gIntersection(bx,polygon1[cord@coords[,3][i]], byid=F)  %>% linelen()
    }
    else{ds <- 0}
    
    daydiff <- difftime(strptime(polygon1@data$date, format = "%Y-%m-%d"),
                        strptime(mmp$date, format = "%Y-%m-%d"),units="days")%>%abs()
    
    
    bpoints[i,] <- c(daydiff,as.numeric(mmp[6]) %>% round(0),ds %>% round(0))
    #bpoints[i,] <- mmp
  }
  return(bpoints)
}