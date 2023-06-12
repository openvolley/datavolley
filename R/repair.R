find_duplicate_player_numbers <- function(x) {
    hpn <- cbind(hv = "home", team_id = home_team_id(x), x$meta$players_h[, c("number", "player_id")])
    hpn <- hpn[hpn$number %in% hpn$number[duplicated(hpn$number)], ]
    vpn <- cbind(hv = "visiting", team_id = visiting_team_id(x), x$meta$players_v[, c("number", "player_id")])
    vpn <- vpn[vpn$number %in% vpn$number[duplicated(vpn$number)], ]
    pnums <- rbind(hpn, vpn)
}

find_duplicate_player_ids <- function(x) {
    pids <- c(x$meta$players_h$player_id, x$meta$players_v$player_id)
    dpids <- pids[duplicated(pids)]
}

#' Attempt to repair a datavolley object
#'
#' Currently an attempt will be made to repair these issues:
#' * if multiple players on the same team have the same jersey number, players with that number (on that team) who did not take to the court will be removed from their team roster. In this situation, whether or not a player took to the court is determined from the match metadata only
#' * if multiple players have the same player ID but different jersey numbers, players with that ID who did not take to the court will be removed from their team roster. In this situation, whether or not a player took to the court is determined from the match metadata and the play-by-play data
#'
#' @param x datavolley: a datavolley object as returned by [dv_read()]
#'
#' @return A modified copy of `x`. If problems exist and cannot be repaired, an error will be thrown
#'
# @examples
#'
#' @export
dv_repair <- function(x) {
    msgs <- c()
    ## duplicated jersey numbers (team-specific)
    pnums <- find_duplicate_player_numbers(x)
    if (nrow(pnums) > 0) {
        for (j in seq_len(nrow(pnums))) {
            this_hv <- pnums$hv[j]
            if (this_hv == "home") {
                #this_team <- home_team(x)
                #this_team_id <- home_team_id(x)
                this_roster <- x$meta$players_h
            } else {
                #this_team <- visiting_team(x)
                #this_team_id <- visiting_team_id(x)
                this_roster <- x$meta$players_v
            }
            possible_rows <- which(this_roster$number %eq% pnums$number[j])
            if (length(possible_rows) > 0) {
                can_remove <- vapply(possible_rows, function(rn) {
                    this_starting <- this_roster[rn, intersect(paste0("starting_position_set", 1:5), names(this_roster))]
                    all(is.na(unlist(this_starting)))
                }, FUN.VALUE = TRUE)
                if (any(can_remove)) {
                    can_remove <- possible_rows[which(can_remove)]
                    msgs <- c(msgs, paste0(if (this_hv == "home") paste0("Home team ", home_team(x)) else paste0("Visiting team ", visiting_team(x)), " player ", this_roster$name[can_remove], " has a duplicate jersey number (", this_roster$number[can_remove] , ") but did not take to the court, removing from roster"))
                    this_roster <- this_roster[setdiff(seq_len(nrow(this_roster)), can_remove), , drop = FALSE]
                    if (this_hv == "home") {
                        x$meta$players_h <- this_roster
                    } else {
                        x$meta$players_v <- this_roster
                    }
                } else {
                    ## can't fix
                }
            }
        }
    }

    ## duplicate player IDs (either team)
    dpids <- find_duplicate_player_ids(x)
    if (length(dpids) > 0) {
        for (this_hv in c("home", "visiting")) {
            if (this_hv == "home") {
                this_team_id <- home_team_id(x)
                this_roster <- x$meta$players_h
            } else {
                this_team_id <- visiting_team_id(x)
                this_roster <- x$meta$players_v
            }
            for (this_player_id in dpids) {
                possible_rows <- which(this_roster$player_id %eq% this_player_id)
                if (length(possible_rows) > 0) {
                    can_remove <- vapply(possible_rows, function(rn) {
                        this_starting <- this_roster[rn, intersect(paste0("starting_position_set", 1:5), names(this_roster))]
                        all(is.na(unlist(this_starting))) && !any(x$plays$team_id %eq% this_team_id & x$plays$player_number %eq% this_roster$number[rn])
                    }, FUN.VALUE = TRUE)
                    if (any(can_remove)) {
                        can_remove <- possible_rows[which(can_remove)]
                        msgs <- c(msgs, paste0(if (this_hv == "home") paste0("Home team ", home_team(x)) else paste0("Visiting team ", visiting_team(x)), " player ", this_roster$name[can_remove], " has a duplicate player ID (", this_roster$player_id[can_remove] , ") but did not take to the court, removing from roster"))
                        this_roster <- this_roster[setdiff(seq_len(nrow(this_roster)), can_remove), , drop = FALSE]
                        if (this_hv == "home") {
                            x$meta$players_h <- this_roster
                        } else {
                            x$meta$players_v <- this_roster
                        }
                    } else {
                        ## can't fix
                    }
                }
            }
        }
    }
    ## now re-check for duplicates and if they were not able to be fixed, throw an error
    tryCatch(pnums <- find_duplicate_player_numbers(x), error = function(e) {
        stop("players with duplicate jersey numbers were not able to be fixed")
    })
    if (nrow(pnums) > 0) stop("players with duplicate jersey numbers were not able to be fixed")
    tryCatch(dpids <- find_duplicate_player_ids(x), error = function(e) {
        stop("players with duplicate player IDs were not able to be fixed")
    })
    if (length(dpids) > 0) stop("players with duplicate player IDs were not able to be fixed")
    if (length(msgs) > 0) {
        ## append to x$messages
        x$messages <- rbind(x$messages, data.frame(file_line_number = NA_integer_, video_time = NA_integer_, message = msgs, file_line = NA_character_))
    }
    x
}
