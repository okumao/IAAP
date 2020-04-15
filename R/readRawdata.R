#' Read Raw Logfiles and Convert to List
#'
#' \code{readRawdata} returns a list which includes all data in logfiles.
#'
#' @param path A string that indicates the path to the log file folder.
#' @param index_path A string that indicates the path to the index csv file.
#' If not, the index file is assumed to be in the folder indicated by @param path.
#' @export
#'



readRawdata <- function(path, index_path = NULL) {
  hd <- getwd()
  setwd(path)
  folname <- list.files(pattern = "[[:digit:]]{6}")
  FN <- paste(path, "/", folname, sep = "")
  temp <- lapply(FN, function(FN) {
    setwd(FN)
    fn <- paste(sprintf("%01d", seq(length(dir(pattern = "c14res")))), ".c14res", sep = "")
    con <- lapply(fn, function(fn) {
      list(
        Measuring_date_time = paste(read.table(fn, skip = 1, nrows = 1,
                                               colClasses = rep("character", 7))[5:7]),
        Files = paste(read.table(fn, skip = 2, nrows = 1)[[3]]),
        Sample_name = paste(read.table(fn, skip = 3, nrows = 1)[[4]]),
        Sample_position = read.table(fn, skip = 4, nrows = 1)[[4]],
        Sample_description = paste(read.table(fn, skip = 5, nrows = 1)[[4]]),
        Operator = paste(read.table(fn, skip = 6, nrows = 1)[[3]]),
        Sample_Moving = paste(read.table(fn, skip = 7, nrows = 1)[[4]]),
        C14_window = as.numeric(read.table(fn, skip = 8, nrows = 1)[c(4, 6)]),
        C13parC12_window = as.numeric(read.table(fn, skip = 9, nrows = 1)[c(6, 8)]),
        Stop_condition_blocks = read.table(fn, skip = 10, nrows = 1)[[4]],
        Blocks_in_file = read.table(fn, skip = 11, nrows = 1)[[5]],
        Chopper_Correction = read.table(fn, skip = 12, nrows = 1)[[4]],
        Block_time = read.table(fn, skip = 13, nrows = 1)[[4]],
        Charge_State = read.table(fn, skip = 14, nrows = 1)[[4]]
      )
    })
    res <- as.data.frame(t(do.call(cbind, lapply(fn, function(x) {
      read.table(x, skip = 16, nrows = 18, sep = ":", row.names = 1)}))))
    bd <- lapply(fn, function(fn)read.table(fn, skip = 35,
                                            header = TRUE, nrows = con[[1]]$Blocks_in_file))
    ms <-  do.call(data.frame, lapply(fn, function(fn) {
      as.vector(read.table(fn, sep = "\n", skip = 37 + con[[1]]$Blocks_in_file,
                           header = FALSE, row.names = NULL)[, 1])
    }))
    smp_name <- unlist(con)[grep("Sample_name", names(unlist(con)))]
    colnames(res) <- sub(" {2,}", "", names(res))
    names(con) <- rownames(res) <- names(bd) <- names(ms) <- smp_name
    list(CONDITION = con, RESULTS = res, BLOCK_DATA = bd, MASTER_SPECTRUM = ms)
  })
  if (is.null(index_path)) index_path <- "index.csv"
  index <- read.csv(paste(path, "/", index_path, sep = ""))
  temp <- c(list(list(rawfolder_path = path, read_date = Sys.Date(),
                      run_num = length(temp), index = index)), temp)
  names(temp) <- c("property", folname)
  setwd(hd)
  return(temp)
}
