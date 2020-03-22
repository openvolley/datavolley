# Read the point by point rotation, and update it if necessary with new entries
#' Get team rotations point by point
#'
#' @param x datavolley or data.frame: a datavolley object as returned by \code{read_dv}, or the plays component of that object
#' @param team string: team name
#' @param point_ids vector: vector of pointids for which to return the rotation
#' @param new_rotation vector: vector of player number, positionned from 1 to 6.
#'
#' @return list of 2 data.frames. Current data.frame, without changes, and updated data.frame, with new player rotation. 
#'
#' @seealso \code{\link{read_dv}}
#'
#' @examples
#' \dontrun{
#'   x <- read_dv(dv_example_file(), insert_technical_timeouts=FALSE)
#'   rotations(x)
#' }
#' @export
rotations <- function(x, team, point_ids, new_rotation = NULL){
    
    
    if(missing(team)) teamSelect = home_team(x) else teamSelect = team
    
    if(missing(point_ids)) point_ids = unique(x$plays$point_id[x$plays$set_number == 1])
    
    if(teamSelect == home_team(x)) player_table <- dplyr::select(x$meta$players_h,.data$number, .data$player_id, .data$special_role, .data$role)
    if(teamSelect == visiting_team(x)) player_table <- dplyr::select(x$meta$players_v,.data$number, .data$player_id, .data$special_role, .data$role)
    
    # Point id may not uniquely identify rotation, because substitutions will affect a point id as well. So we need to create our own unique ids. 
    # Say, when the skill is equal to serve, or Timeout
    
    if(teamSelect == home_team(x)) x_tmp = dplyr::distinct(dplyr::filter(dplyr::select(x$plays, .data$point_id,.data$skill, tidyselect::starts_with("home_player_id")), .data$point_id %in% point_ids, .data$skill %in% c("Serve", "Timeout")))
    
    if(teamSelect == visiting_team(x)) x_tmp = dplyr::distinct(dplyr::filter(dplyr::select(x$plays, .data$point_id,.data$skill, tidyselect::starts_with("visiting_player_id")), .data$point_id %in% point_ids, .data$skill %in% c("Serve", "Timeout")))
    
    if(teamSelect == home_team(x)){
        x_tmp_long <- tidyr::pivot_longer(x_tmp, tidyselect::starts_with("home_player_id"), names_to = "position", values_to = "home_player_id")
        x_tmp_long$position <- stringr::str_remove(x_tmp_long$position, "home_player_id")
    }
    if(teamSelect == visiting_team(x)){
        x_tmp_long <- tidyr::pivot_longer(x_tmp, tidyselect::starts_with("visiting_player_id"), names_to = "position", values_to = "visiting_player_id")
        x_tmp_long$position <- stringr::str_remove(x_tmp_long$position, "visiting_player_id")
    }
    if(teamSelect == home_team(x)){
        x_tmp_long <- dplyr::left_join(x_tmp_long, dplyr::rename(dplyr::select(player_table, .data$number, .data$player_id), "home_p" = "number", "home_player_id" = "player_id"), by = "home_player_id")
        x_tmp_wide <- tidyr::pivot_wider(x_tmp_long,id_cols = "point_id", names_from = "position", values_from = c("home_player_id","home_p"), names_sep = "")
    }
    if(teamSelect == visiting_team(x)){
        x_tmp_long <- dplyr::left_join(x_tmp_long, dplyr::rename(dplyr::select(player_table, .data$number, .data$player_id), "visiting_p" = "number", "visiting_player_id" = "player_id"), by = "visiting_player_id")
        x_tmp_wide <- tidyr::pivot_wider(x_tmp_long,id_cols = "point_id", names_from = "position", values_from = c("visiting_player_id","visiting_p"), names_sep = "")
    }
        x_tmp_wide_new = NULL
    if(!is.null(new_rotation)){
        
        new_rotation = as.character(new_rotation)
        
        if(sum(new_rotation %in% player_table$number) < 6){
            stop("Not all players are on the team list. Please update.")
        }
        
        if(teamSelect == home_team(x)){
            starting_rotation = stringr::str_c("\\b", as.character(x_tmp_long$home_p[1:6]), "\\b", collapse="|")
            replaceRot = function(rot){
                new_rotation[which(rot == as.character(x_tmp_long$home_p[1:6]))]
            }
            x_tmp_long$new_p <- stringr::str_replace_all(x_tmp_long$home_p, starting_rotation, replaceRot)
            x_tmp_long$new_p <- as.numeric(x_tmp_long$new_p)
            x_tmp_long_new <- dplyr::rename(dplyr::select(dplyr::left_join(x_tmp_long, dplyr::select(player_table, .data$number, .data$player_id), by = c("new_p" = "number")),
                                    .data$point_id, .data$position, .data$new_p, .data$player_id), "home_p" = "new_p", "home_player_id" = "player_id")
            x_tmp_wide_new <- tidyr::pivot_wider(x_tmp_long_new,id_cols = "point_id", names_from = "position", values_from = c("home_player_id","home_p"), names_sep = "")
        }
        if(teamSelect == visiting_team(x)){
            starting_rotation = stringr::str_c("\\b", as.character(x_tmp_long$visiting_p[1:6]), "\\b", collapse="|")
            replaceRot = function(rot){
                new_rotation[which(rot == as.character(x_tmp_long$visiting_p[1:6]))]
            }
            x_tmp_long$new_p <- stringr::str_replace_all(x_tmp_long$visiting_p, starting_rotation, replaceRot)
            x_tmp_long$new_p <- as.numeric(x_tmp_long$new_p)
            x_tmp_long_new <- dplyr::rename(dplyr::select(dplyr::left_join(x_tmp_long, dplyr::select(player_table, .data$number, .data$player_id), by = c("new_p" = "number")),
                                                          .data$point_id, .data$position, .data$new_p, .data$player_id), "home_p" = "new_p", "visiting_player_id" = "player_id")
            x_tmp_wide_new <- tidyr::pivot_wider(x_tmp_long_new,id_cols = "point_id", names_from = "position", values_from = c("visiting_player_id","home_p"), names_sep = "")
        }
    }
    return(list(current_rotation = x_tmp_wide, new_rotation = x_tmp_wide_new))
}
