#' \pkg{datavolley}
#'
#' Provides basic functions for parsing Datavolley scouting files. Datavolley is a software package used for scouting and summarizing volleyball matches.
#' 
#' The example data files provided with the datavolley package came from \url{http://www.odbojka.si/}.
#'
#' @name datavolley
#' @author Ben Raymond \email{ben@@untan.gl}
#' @docType package
#' @import assertthat
#' @importFrom plyr ddply join ldply mapvalues rbind.fill
#' @importFrom data.table fread
#' @importFrom digest digest
#' @importFrom graphics lines par plot text
#' @importFrom readr read_csv2 locale
#' @importFrom stringi stri_trans_general stri_enc_detect2
#' @importFrom stringr str_match str_trim regex fixed str_replace_all str_to_title str_locate str_sub
#' @importFrom stats na.omit runif setNames
#' @importFrom tibble tibble
#' @importFrom utils adist read.csv read.table tail

NULL


