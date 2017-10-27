#' \pkg{datavolley}
#'
#' Provides basic functions for parsing Datavolley scouting files. Datavolley is a software package used for scouting and summarizing volleyball matches.
#' 
#' The example data files provided with the datavolley package came from \url{http://www.odbojka.si/}:
#' \itemize{
#'   \item{\code{system.file("extdata/example_data.dvw",package="datavolley")}: the January 2015 junior women's final between Braslovče and Nova KBM Branik}
#'   \item{\code{system.file("extdata/PM06.dvw",package="datavolley")}: the December 2012 men's Slovenian national championship semifinal between ACH Volley and Maribor}
#' }
#'
#' @name datavolley
#' @author Ben Raymond \email{ben@@untan.gl}
#' @docType package
#' @import plyr assertthat
#' @importFrom digest digest
#' @importFrom stringi stri_trans_general stri_enc_detect2
#' @importFrom stringr str_match str_trim regex fixed str_replace_all str_to_title str_locate str_sub
#' @importFrom data.table fread
#' @importFrom stats na.omit
#' @importFrom utils adist read.csv read.table

NULL


