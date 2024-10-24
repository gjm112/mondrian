  #Chage seed to get different random image
  seed <- 12345
  set.seed(seed)
  #png(paste0("./mondrian_",seed,".png"), res = 300, h = 12, w = 12, units = "in")
  #linewidth 
  lwd <- 3
  #Number of boxes
  nbox <- 500
  #Number of boxes of each color
  #Sum must be less than or equal to nbox
  num <- list(red = 100,blue = 10,yellow = 4)
  
  par(mar = c(0,0,0,0))
  plot(0,0,col = rgb(0,0,0,0), frame.plot= FALSE, yaxt='n', xaxt='n',ylab = "", xlab = "", ylim = c(0,10), xlim = c(0,10), asp = 1)
  points(c(0,10,10,0,0,0),c(0,0,10,10,0,10),type = "l", lwd = lwd,lend = 1)
  
  rect <- list()
  rect[[1]] <- c(0,10,0,10)
  
  #Randomly sample a rectangle
  for (q in 1:nbox){
    i <- sample(1:length(rect),1)
    temp <- rect[[i]]
    
    x <- runif(1,temp[1],temp[2])
    y <- runif(1,temp[3],temp[4])
    
    if (runif(1) < 0.5){
      points(c(temp[1],temp[2]),c(y,y),type ="l", lwd = lwd)
      rect[[i]] <- NULL
      rect <- append(rect,list(c(temp[1],temp[2],y,temp[4])))
      rect <- append(rect,list(c(temp[1],temp[2],temp[3],y)))
    } else {
      points(c(x,x),c(temp[3],temp[4]),type ="l", lwd = lwd)
      rect[[i]] <- NULL
      rect <- append(rect,list(c(temp[1],x,temp[3],temp[4])))
      rect <- append(rect,list(c(x,temp[2],temp[3],temp[4])))
    }
  }
  
  #Now color squares
  for (color in names(num)){
  for (q in 1:num[[color]]){
    i <- sample(1:length(rect),1)
    temp <- rect[[i]]
    rect[[i]] <- NULL
    polygon(temp[c(1,2,2,1)],temp[c(3,3,4,4)], col = color, lwd=lwd, lend = 1)
    }
  }
  
  #dev.off()
