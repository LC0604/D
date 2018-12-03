

nbpoints <- function(polygon1,polygon2,gridsize){
  

  linelen <- function(x){
    cods <-  x@lines[[1]]@Lines[[1]]@coords
    return( ((cods[2,]-cods[1,])^2 %>%sum())^0.5 )}
  
  lens <- function(a,b){((a-b)^2 %>%sum())^0.5}  
  gridn <- round(abs(polygon1@bbox[,1]-polygon1@bbox[,2])/gridsize,0)
 
  rasunit <- raster(ncol=gridn[1], nrow=gridn[2], crs = "+proj=utm +zone=51 ellps=WGS84")
  extent(rasunit) <- extent(polygon1)
  cord <- rasterize(polygon1, rasunit) %>% rasterToPoints() %>% SpatialPoints()
  crs(cord) = "+proj=utm +zone=51 ellps=WGS84"
 
  nr <- nrow(cord %>% data.frame)
  gr <- polygon2@data %>% nrow
  #bpoints <- data.frame(x = rep(0,nr),y = rep(0,nr), x2 = rep(0,nr),y2 = rep(0,nr), date = rep(0,nr), D = rep(0,nr), ds =  rep(0,nr))
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
    
    
    bpoints[i,] <- c(daydiff,as.numeric(mmp[6]),ds)
    #bpoints[i,] <- mmp
  }
  
  
  
    lss<- list(distable=bpoints,
               weight=rep(1,nr))
    #print(lss)
    return(lss)     
  }
