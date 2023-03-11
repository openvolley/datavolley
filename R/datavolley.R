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
#' @importFrom data.table fread
#' @importFrom digest digest
#' @importFrom dplyr %>% across all_of as_tibble .data bind_cols bind_rows case_when distinct if_else group_by lag lead left_join mutate pull slice tibble tribble ungroup
#' @importFrom graphics lines par plot text
#' @importFrom stringi stri_trans_general stri_enc_detect2
#' @importFrom stringr fixed regex str_locate str_match str_match_all str_replace_all str_split str_sub str_to_title str_trim
#' @importFrom stats approx na.omit runif setNames
#' @importFrom utils adist capture.output head packageVersion read.csv read.table tail
#' @importFrom vscoututils dv_create_file_meta dv_create_meta dv_create_meta_attack_combos dv_create_meta_comments dv_create_meta_match dv_create_meta_match_id dv_create_meta_more dv_create_meta_players dv_create_meta_result dv_create_meta_setter_calls dv_create_meta_teams dv_create_meta_video dv_decode_evaluation dv_decode_num_players dv_decode_skill_subtype dv_decode_skill_type dv_decode_special_code dv_default_attack_combos dv_default_scouting_table dv_default_winning_symbols dv_expand_rally_codes dv_file_type dv_green_codes dv_insert_technical_timeouts dv_update_meta
#' @importFrom xml2 read_xml xml_find_all xml_name xml_text
NULL


