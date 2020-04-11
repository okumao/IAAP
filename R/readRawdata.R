readRawdata <- function(path){
  hd <- getwd()
  setwd(path)
  FN <- list.files(pattern = "[[:digit:]]{6}")
  n_folder <- length(FN)
  PRIMARY_DATA <- list()

  for(i in 1:n_folder){
    setwd(FN[i])
    n_c14res <- length(dir(pattern="c14res"))
    fn <- paste(sprintf("%01d",seq(n_c14res)),".c14res",sep="")
    CONDITION <- list()
    SAMPLE_NAME <- rep(NA,n_c14res)
    RESULTS <- data.frame(matrix(rep(NA,18), nrow=1))[numeric(0), ]
    BLOCK_DATA <- list()
    MASTER_SPECTRUM <- list()

    for(j in 1:n_c14res){
      ##############################read CONDITION##############################
      date_temp <- paste(read.table(fn[j], skip = 1, nrows = 1)[5][[1]])
      if(nchar(date_temp)==9) date_temp <- paste(0,date_temp,sep="")
      md <- as.Date(date_temp,format="%m/%d/%y")
      mt <- paste(read.table(fn[[j]], skip = 1, nrows = 1)[6][[1]])
      Measuring_date_time <- as.POSIXct(paste(md,mt))
      if(paste(read.table(fn[j], skip = 1, nrows = 1)[7][[1]])=="PM"){
        Measuring_date_time <- Measuring_date_time + 12*60*60
      }
      condition_temp <- list(
        Measuring_date_time = Measuring_date_time,
        Files = paste(read.table(fn[j], skip = 2, nrows = 1)[[3]]),
        Sample_name = paste(read.table(fn[j], skip = 3, nrows = 1)[[4]]),
        Sample_position = read.table(fn[j], skip = 4, nrows = 1)[[4]],
        Sample_description = paste(read.table(fn[j], skip = 5, nrows = 1)[[4]]),
        Operator = paste(read.table(fn[j], skip = 6, nrows = 1)[[3]]),
        Sample_Moving = paste(read.table(fn[j], skip = 7, nrows = 1)[[4]]),
        C14_window = as.numeric(read.table(fn[j], skip = 8, nrows = 1)[c(4,6)]),
        C13parC12_window = as.numeric(read.table(fn[j], skip = 9, nrows = 1)[c(6,8)]),
        Stop_condition_blocks = read.table(fn[j], skip = 10, nrows = 1)[[4]],
        Blocks_in_file = read.table(fn[j], skip = 11, nrows = 1)[[5]],
        Chopper_Correction = read.table(fn[j], skip = 12, nrows = 1)[[4]],
        Block_time = read.table(fn[j], skip = 13, nrows = 1)[[4]],
        Charge_State = read.table(fn[j], skip = 14, nrows = 1)[[4]]
      )

      CONDITION <- c(CONDITION,list(condition_temp))
      SAMPLE_NAME[j] <- condition_temp$Sample_name

      ##############################read RESULTS##############################
      results_temp <- data.frame(
        Measurement_time = read.table(fn[j], skip = 16, nrows = 1)[[4]],
        Run_time = read.table(fn[j], skip = 17, nrows = 1)[[4]],
        Initial_Target_Current_mA = read.table(fn[j], skip = 18, nrows = 1)[[6]],
        Final_Target_Current_mA = read.table(fn[j], skip = 19, nrows = 1)[[6]],
        C14_counts = read.table(fn[j], skip = 20, nrows = 1)[[4]],
        Detector_counts = read.table(fn[j], skip = 21, nrows = 1)[[4]],
        C14_counts_par_sec = read.table(fn[j], skip = 22, nrows = 1)[[4]],
        C14_statistical_error_parcent = read.table(fn[j], skip = 23, nrows = 1)[[6]],
        C13_average_current = read.table(fn[j], skip = 24, nrows = 1)[[5]],
        C13_rel_standard_deviation = read.table(fn[j], skip = 25, nrows = 1)[[6]],
        C12_average_current = read.table(fn[j], skip = 26, nrows = 1)[[5]],
        C12_rel_standard_deviation = read.table(fn[j], skip = 27, nrows = 1)[[6]],
        C14_par_C13_ratio = read.table(fn[j], skip = 28, nrows = 1)[[6]],
        C14_par_C12_ratio = read.table(fn[j], skip = 29, nrows = 1)[[6]],
        C13_par_C12_ratio = read.table(fn[j], skip = 30, nrows = 1)[[6]],
        C14_par_C13_rel_std_deviation = read.table(fn[j], skip = 31, nrows = 1)[[8]],
        C14_par_C12_rel_std_deviation = read.table(fn[j], skip = 32, nrows = 1)[[8]],
        C13_par_C12_rel_std_deviation = read.table(fn[j], skip = 33, nrows = 1)[[8]]
      )
      RESULTS <- rbind(RESULTS,results_temp)

      ##############################read BLOCK DATA##############################
      block_data_temp <- read.table(fn[j], skip = 35, header = TRUE, nrows = condition_temp$Blocks_in_file)
      BLOCK_DATA <- c(BLOCK_DATA,list(block_data_temp))

      ##############################read MASTER SPECTRUM##############################
      master_spectrum_temp <- as.vector(read.table(fn[j], sep = "\n", skip = 37+ condition_temp$Blocks_in_file, header = FALSE, row.names=NULL)[,1])
      MASTER_SPECTRUM <- c(MASTER_SPECTRUM, list(master_spectrum_temp))
    }

    names(CONDITION) <- SAMPLE_NAME
    rownames(RESULTS) <- SAMPLE_NAME
    names(BLOCK_DATA) <- SAMPLE_NAME
    names(MASTER_SPECTRUM) <- SAMPLE_NAME
    primary_data_temp <- list(CONDITION=CONDITION,RESULTS=RESULTS,BLOCK_DATA=BLOCK_DATA,MASTER_SPECTRUM=MASTER_SPECTRUM)

    PRIMARY_DATA <- c(PRIMARY_DATA,list(primary_data_temp))
    setwd("../")
  }

  output_fn <- strsplit(path,split="/")[[1]][length(strsplit(path,split="/")[[1]])]
  property <- list(rawfolder_name = output_fn, rawpath = path, read_date = Sys.Date())
  PRIMARY_DATA <- c(list(property),PRIMARY_DATA)
  names(PRIMARY_DATA) <- c("property",foldernames)
  setwd(hd)
  return(PRIMARY_DATA)
}
