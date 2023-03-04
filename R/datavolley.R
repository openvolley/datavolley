#' \pkg{datavolley}
#'
#' Provides basic functions for parsing Datavolley scout files. Datavolley is a software package used for scouting and summarizing volleyball matches.
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
#' @importFrom dplyr %>% as_tibble .data bind_rows case_when left_join mutate tibble tribble
#' @importFrom graphics lines par plot text
#' @importFrom stringi stri_trans_general stri_enc_detect2
#' @importFrom stringr fixed regex str_locate str_match str_match_all str_replace_all str_split str_sub str_to_title str_trim
#' @importFrom stats na.omit runif setNames
#' @importFrom utils adist capture.output head packageVersion read.csv read.table tail
#' @importFrom vscoututils dv_create_file_meta dv_create_meta_attack_combos dv_create_meta_comments dv_create_meta_match dv_create_meta_match_id dv_create_meta_more dv_create_meta_result dv_create_meta_setter_calls dv_create_meta_teams dv_create_meta_video dv_default_scouting_table dv_green_codes dv_default_winning_symbols dv_update_meta
NULL


