
#Answer1
pollutantmean <- function(x,y,z){
  file_full <- list.files(x,full.names = TRUE)#combines csv files into 1 array
  dat <- data.frame() #empty data frame

  for(i in z){
    
    dat <- rbind(dat,read.csv(file_full[i]))#puts needed files under directory into one array
  }
  
  mean(dat[,y],na.rm = TRUE) #takes mean 
}

#Answer2

complete <- function(x,y){
  file_full<- list.files(x,full.names = TRUE)
  dat2 <- data.frame() #Empty
  dat3<- data.frame() #Empty
  
  for(i in y){
    
    #Creates data frame of number of completed observations per csv file
    dat2 <- rbind(dat2, sum(complete.cases(read.csv(file_full[i]))))
    dat3 <- rbind(dat3, i) #Creates dataframe of id's
  }

    dat4 <-cbind(dat3,dat2) #Combines dataframes made in loop
    names(dat4) <- c("id", "nobs") #Names columns in dat_4
    
    dat4#has to have this line because otherwise it will "return" the last line
}

#Answer 3

  #calculate complete observations for each file in directory
  correlation <- function(x,y){
    file_full <- list.files(x,full.names = TRUE)
    corr <- vector()

    
    for(i in seq_along(file_full)){
      
      #Creates vector of number of completed observations per csv file
      if(sum(complete.cases(read.csv(file_full[i])))>y){

        dat<- na.omit(read.csv((file_full[i]),header = TRUE))
        corr <- c(corr,cor(dat$sulfate,dat$nitrate)) #Array of Correlations  
        
      }

    }
corr
}

  