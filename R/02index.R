makeIndex <- function(logdata){
  SAMPLE_NAME <- rownames(logdata[[1]][[2]])
  n <- length(SAMPLE_NAME)
  index <- rep(NA, n)
  type <- c("HOx1", "HOx2", "ASO","C1", "other")
  for(i in 1:n){
    index[i] <- menu(type, title = paste("choose sample type of",SAMPLE_NAME[i]))
    if(index[i]==0) index[i] <- 5
  }
  ST <- type[index]
  sample_index <- data.frame(SAMPLE_NAME = SAMPLE_NAME, SAMPLE_TYPE = ST,type_num = index)
  sample_index$SAMPLE_NAME <- as.character(sample_index$SAMPLE_NAME)
  sample_index$SAMPLE_TYPE <- as.character(sample_index$SAMPLE_TYPE)
  return(sample_index)
}

correctIndex <- function(sample_index){
  type <- c("HOx1", "HOx2", "ASO","C1", "other")
  smp_name <- sample_index$SAMPLE_NAME
  smp <- menu(smp_name,
              title = "Choose the sample name for which you want to correct the sample type"
  )
  if(smp!=0){
    index <- menu(type,
                  title = paste("choose sample type of",sample_index$SAMPLE_NAME[smp])
    )
    sample_index[smp,2] <- type[index]
    sample_index[smp,3] <- index
  }
  if(smp==0) return(sample_index)

  while(smp!=0){
    smp <- menu(sample_index$SAMPLE_NAME,
                title = "Choose the sample name for which you want to correct the sample type"
    )
    if(smp!=0){
      index <- menu(type,
                    title = paste("choose sample type of",sample_index$SAMPLE_NAME[smp])
      )
      sample_index[smp,2] <- type[index]
      sample_index[smp,3] <- index
    }
  }
  return(sample_index)
}
