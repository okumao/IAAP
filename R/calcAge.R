.editWindow <- function(logdata, edit_C14window = FALSE) {
  C14window <- do.call(rbind, lapply(logdata[-1], function(x) x$CONDITION[[1]]$C14_window))
  colnames(C14window) <- c("14C window lower limit", "14C window upper limit")
  if (edit_C14window) C14window <- edit(C14window)
  log_summary <- list()
  for (i in seq(logdata[[1]]$run_num)) {
    n <- apply(logdata[[i + 1]]$MASTER_SPECTRUM[(C14window[i, 1] + 1):C14window[i, 2], ], 2, sum) + 1
    Rs <- signif(5.34059e-21 *
        (n / (logdata[[i + 1]]$RESULTS$`C12 average current` * logdata[[i + 1]]$RESULTS$`Measurement time`)))
    R13 <- signif(0.01111111 *
          logdata[[i + 1]]$RESULTS$`C13 average current` / logdata[[i + 1]]$RESULTS$`C12 average current`)
    log_summary[[i]] <- data.frame(Rs = Rs, n = n, R13 = R13)
  }
  names(log_summary) <- names(logdata[-1])
  return(log_summary)
}


.calc <- function(logdata, x) {
  type <- toupper(logdata$property$index$type)

  ###14C age calculation###

  #Rs error
  dRs <- signif(x$Rs / sqrt(x$n))

  #isotope fractionation correction
  R13HOx2 <- signif(mean(x$R13[type == "HOX2"]))
  rPDB <- signif(R13HOx2 / (-17.8 / 1000 + 1))
  d13C <- signif(1000 * (x$R13 / rPDB - 1))
  Rsn <- signif(x$Rs * ((1 - 25 / 1000) / (1 + d13C / 1000))^2)
  dRsn <- signif(dRs * ((1 - 25 / 1000) / (1 + d13C / 1000))^2)

  #background correction
  Rbgn <- signif(mean(Rsn[type == "ASO"]))
  dRbgn <- signif(sd(Rsn[type == "ASO"]))
  Rsn_bc <- signif(Rsn - Rbgn)
  dRsn_bc <- signif(sqrt(dRsn^2 + dRbgn^2))

  #Aon
  Aon <- signif(0.7459 * mean(Rsn_bc[type == "HOX2"]))
  dAon <- signif(0.7459 * sd(Rsn_bc[type == "HOX2"]))

  f <- signif(Rsn_bc / Aon)
  f_e <- signif(sqrt(((Rsn_bc / Aon^2) * dAon)^2 + (dRsn_bc / Aon)^2))

  #14C age
  t <- signif(-8033 * suppressWarnings(log(Rsn_bc / Aon)))
  dt <- signif(8033 * sqrt((dAon / Aon)^2 + (dRsn_bc / Rsn_bc)^2))

  #no background correction 14C age
  t_nbc <- signif(-8033 * log(Rsn / Aon))
  dt_nbc <- signif(8033 * sqrt((dAon / Aon)^2 + (dRsn / Rsn)^2))

  ###data summary###
  y <- data.frame(
    sample_name = rownames(x),
    sample_type = type,
    Rs = x$Rs,
    Rs_error = dRs,
    delta13C = d13C,
    Rsn = Rsn,
    Rsn_error = dRsn,
    Rsn_bc = Rsn_bc,
    Rsn_bc_error = dRsn_bc,
    Asn_par_Aon = f,
    Asn_Aon_error = f_e,
    age = round(t),
    age_1sd = round(dt),
    no_bg_corr_age = round(t_nbc),
    no_bg_corr_age_1sd = round(dt_nbc)
  )

  Basic_information <- list(rPDB, Aon, dAon, Rbgn, dRbgn)
  names(Basic_information) <- c("C13/C12_PDB", "Aon", "Aon_error", " Rbgn", "Rbgn_error")
  return(list(Basic_information = Basic_information, data = y))
}





#' Calculate 14C age
#'
#' \code{calcAge} returns a list which includes 14C age and other values used for calculation
#'
#' @param logdata A list generated with \code{readRawdata}.
#' @param edit_C14window A logical scalar.
#' If TRUE, the editor is displayed and the C14 window can be edited.
#'
#' @export
#'

calcAge <- function(logdata, edit_C14window = FALSE) {
  log_summary <- .editWindow(logdata, edit_C14window)
  detail <- list()
  age_table <- age_weight <- nbc_table <- nbc_weight <- rep(NA, nrow(log_summary[[1]]))

  for (i in 1:(length(logdata) - 1)) {
    temp <- .calc(logdata, log_summary[[i]])
    detail <- c(detail, list(temp))
    age_table <- cbind(age_table, temp$data$age)
    age_weight <- cbind(age_weight, 1 / temp$data$age_1sd^2)
    nbc_table <- cbind(nbc_table, temp$data$no_bg_corr_age)
    nbc_weight <- cbind(nbc_weight, 1 / temp$data$no_bg_corr_age_1sd^2)
  }

  age_table <- age_table[, -1]
  age_weight <- age_weight[, -1]
  nbc_table <- nbc_table[, -1]
  nbc_weight <- nbc_weight[, -1]

  age <- round(apply(age_table * age_weight, 1, sum) / apply(age_weight, 1, sum))
  age_sd1 <- round(sqrt(1 / apply(age_weight, 1, sum)))
  nbc <- round(apply(nbc_table * nbc_weight, 1, sum) / apply(nbc_weight, 1, sum))
  nbc_sd1 <- round(sqrt(1 / apply(nbc_weight, 1, sum)))

  names(detail) <- names(logdata[-1])
  z <- data.frame(sample_type = logdata$property$index$type, age, age_error = age_sd1,
                  no_bg_corr_age = nbc, no_bg_corr_age_error = nbc_sd1)
  rownames(z) <- rownames(log_summary[[1]])
  return(list(summary = z, values_for_calc = log_summary, detail = detail))
}
