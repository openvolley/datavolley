#' Additional validation checks on a DataVolley file
#'
#' This function is automatically run as part of \code{dv_read} if \code{extra_validation} is greater than zero.
#' The current validation messages/checks are:
#' \itemize{
#'   \item message "The total of the [home|visiting] team scores in the match result summary (x$meta$result) does not match the total number of points recorded for the [home|visiting] team in the plays data"
#'   \item message "Players xxx and yyy have the same player ID": player IDs should be unique, and so duplicated IDs will be flagged here
#'   \item message "The listed player is not on court in this rotation": the player making the action is not part of the current rotation. Libero players are ignored for this check
#'   \item message "Back-row player made an attack from a front-row zone": an attack starting from zones 2-4 was made by a player in the back row of the current rotation
#'   \item message "Front-row player made an attack from a back-row zone (legal, but possibly a scouting error)": an attack starting from zones 1,5-9 was made by a player in the front row of the current rotation
#'   \item message "Quick attack by non-middle player"
#'   \item message "Middle player made a non-quick attack"
#'   \item message "Block by a back-row player"
#'   \item message "Winning serve not coded as an ace"
#'   \item message "Non-winning serve was coded as an ace"
#'   \item message "Serving player not in position 1"
#'   \item message "Player designated as libero was recorded making a [serve|attack|block]"
#'   \item message "Attack (which was blocked) does not have number of blockers recorded"
#'   \item message "Attack (which was followed by a block) has 'No block' recorded for number of players"
#   \item message "End zone of attack does not match the end zone implied by the end coordinate"
#'   \item message "Repeated row with same skill and evaluation_code for the same player"
#'   \item message "Consecutive actions by the same player"
#'   \item message "Point awarded to incorrect team following error (or \"error\" evaluation incorrect)"
#'   \item message "Point awarded to incorrect team (or [winning play] evaluation incorrect)"
#'   \item message "Scores do not follow proper sequence": one or both team scores change by more than one point at a time
#'   \item message "Visiting/Home team rotation has changed incorrectly"
#'   \item message "Player lineup did not change after substitution: was the sub recorded incorrectly?"
#'   \item message "Player lineup conflicts with recorded substitution: was the sub recorded incorrectly?"
#   \item message "End zone of serve does not match the end zone implied by the end coordinate"
#'   \item message "Reception type does not match serve type": the type of reception (e.g. "Jump-float serve reception" does not match the serve type (e.g. "Jump-float serve")
#'   \item message "Reception start zone does not match serve start zone"
#'   \item message "Reception end zone does not match serve end zone"
#'   \item message "Reception end sub-zone does not match serve end sub-zone"
#'   \item message "Attack type ([type]) does not match set type ([type])": the type of attack (e.g. "Head ball attack") does not match the set type (e.g. "High ball set")
#'   \item message "Block type ([type]) does not match attack type ([type])": the type of block (e.g. "Head ball block") does not match the attack type (e.g. "High ball attack")
#'   \item message "Dig type ([type]) does not match attack type ([type])": the type of dig (e.g. "Head ball dig") does not match the attack type (e.g. "High ball attack")
#'   \item message "Multiple serves in a single rally"
#'   \item message "Multiple receptions in a single rally"
#'   \item message "Serve (that was not an error) did not have an accompanying reception"
#'   \item message "Rally had ball contacts but no serve"
#' }
#' 
#' @param x datavolley: datavolley object as returned by \code{dv_read}
#' @param validation_level numeric: how strictly to check? If 0, perform no checking; if 1, only identify major errors; if 2, also return any issues that are likely to lead to misinterpretation of data; if 3, return all issues (including minor issues such as those that might have resulted from selective post-processing of compound codes)
#' @param options list: named list of options that control optional validation behaviour. Valid entries are:
#' \itemize{
#'   \item setter_tip_codes character: vector of attack codes that represent setter tips (or other attacks that a back-row player can validly make from a front-row position). If you code setter tips as attacks, and don't want such attacks to be flagged as an error when made by a back-row player in a front-row zone, enter the setter tip attack codes here. e.g. \code{options=list(setter_tip_codes=c("PP","XY"))}
#' }
#' @param file_type string: "indoor" or "beach"
#'
#' @return data.frame with columns message (the validation message), file_line_number (the corresponding line number in the DataVolley file), video_time, and file_line (the actual line from the DataVolley file).
#'
#' @seealso \code{\link{dv_read}}
#'
#' @examples
#' \dontrun{
#'   x <- dv_read(dv_example_file(), insert_technical_timeouts = FALSE)
#'   xv <- validate_dv(x)
#'
#'   ## specifying "PP" as the setter tip code
#'   ## front-row attacks (using this code) by a back-row player won't be flagged as errors
#'   xv <- validate_dv(x, options = list(setter_tip_codes = c("PP"))) 
#' }
#'
#' @export
validate_dv <- function(x, validation_level = 2, options = list(), file_type = "indoor") {
    assert_that(is.numeric(validation_level) && validation_level %in% 0:3)
    assert_that(is.list(options))
    assert_that(is.string(file_type))
    file_type <- match.arg(tolower(file_type), c("indoor", "beach"))

    team_player_num <- if (grepl("beach", file_type)) 1:2 else 1:6

    out <- data.frame(file_line_number=integer(), video_time=numeric(), message=character(), file_line=character(), severity=numeric(), stringsAsFactors=FALSE)
    mt2nachar <- function(z) if (length(z) < 1) NA_character_ else z
    chk_df <- function(chk, msg, severity = 2) {
        vt <- video_time_from_raw(x$raw[chk$file_line_number])
        if (length(vt) < 1) vt <- NA_integer_
        data.frame(file_line_number = chk$file_line_number, video_time = vt, message = msg, file_line = mt2nachar(x$raw[chk$file_line_number]), severity = severity, stringsAsFactors = FALSE)
    }
    if (validation_level<1) return(out)

    ## metadata checks
    ## check for duplicate player IDs across both teams
    ph <- x$meta$players_h
    ph$team <- home_team(x)
    ph$hv <- "home"
    pv <- x$meta$players_v
    pv$team <- visiting_team(x)
    pv$hv <- "visiting"
    plyrs <- rbind(ph, pv)
    dpids <- plyrs$player_id[duplicated(plyrs$player_id)]
    for (dpid in unique(dpids)) {
        idx <- plyrs$player_id %eq% dpid
        msg <- paste0("Players have the same player ID (", dpid, "): ")
        this_players <- paste0(plyrs$name[idx], " (", plyrs$hv[idx], " team ", plyrs$team[idx], " #", plyrs$number[idx], ")", collapse=", ")
        out <- rbind(out, data.frame(file_line_number = NA, video_time = NA, message = paste0(msg, this_players), file_line = NA_character_, severity = 3, stringsAsFactors = FALSE))
    }

    if (file_type == "indoor") {
        ## check for missing player roles
        for (py in c("players_h", "players_v")) {
            plyrs <- x$meta[[py]]
            idx <- which(is.na(plyrs$role))
            ## of these, the players who appear in the plays data or have a special role (libero, captain)
            ##        idx1 <- idx[vapply(idx, function(z) any(x$plays$player_id %eq% plyrs$player_id[z]) || (!is.na(plyrs$special_role[z]) && nzchar(plyrs$special_role[z])), FUN.VALUE = TRUE, USE.NAMES = FALSE)]
            ## can't decide whether to treat players who appear in the plays data differently to those who do not: for now treat the same
            idx1 <- idx
            if (length(idx1) > 0) {
                this_players <- paste0(plyrs$name[idx1], collapse=", ")
                msg <- if (py == "players_h") paste0("Home team (", home_team(x), ")") else paste0("Visiting team (", visiting_team(x), ")")
                if (length(idx1) > 1) {
                    wd1 <- " players "
                    wd2 <- " have "
                } else {
                    wd1 <- " player "
                    wd2 <- " has "
                }
                msg <- paste0(msg, wd1, this_players, wd2, "no position (opposite/outside/etc) assigned in the players list")
                out <- rbind(out, data.frame(file_line_number = NA, video_time = NA, message = msg, file_line = NA_character_, severity = 3, stringsAsFactors = FALSE))
            }
            ##        ## players who do not appear in the plays data - these missing roles might be less important, but we'll flag them at the same level of severity (just with a different message)
            ##        idx2 <- setdiff(idx, idx1)
            ##        if (length(idx2) > 0) {
            ##            this_players <- paste0(plyrs$name[idx2], collapse=", ")
            ##            msg <- if (py == "players_h") paste0("Home team (", home_team(x), ")") else paste0("Visiting team (", visiting_team(x), ")")
            ##            if (length(idx2) > 1) {
            ##                wd1 <- " players "
            ##                wd2 <- " have "
            ##            } else {
            ##                wd1 <- " player "
            ##                wd2 <- " has "
            ##            }
            ##            msg <- paste0(msg, wd1, this_players, wd2, "no position (opposite/outside/etc) assigned in the players list. Note that these players do not appear in the plays data, so probably did not take the court during the match")
            ##            out <- rbind(out, data.frame(file_line_number = NA, video_time = NA, message = msg, file_line = NA_character_, severity = 3, stringsAsFactors = FALSE))
            ##        }
        }
    }
    plays <- plays(x)
    if (nrow(plays) > 0) {
        ## check that points-won in the plays component match the points in the meta$result component
        if ("point" %in% names(x$plays)) { ## not in peranavolley files (yet)
            chk <- sum(x$meta$result$score_home_team, na.rm = TRUE) == sum(x$plays$team == x$plays$home_team & x$plays$point, na.rm = TRUE)
            if (!chk) {
                msg <- "The total of the home team scores in the match result summary (x$meta$result) does not match the total number of points recorded for the home team in the plays data"
                out <- rbind(out, data.frame(file_line_number = NA, video_time = NA, message = msg, file_line = NA_character_, severity = 3, stringsAsFactors = FALSE))
            }
            chk <- sum(x$meta$result$score_visiting_team, na.rm = TRUE) == sum(x$plays$team == x$plays$visiting_team & x$plays$point, na.rm = TRUE)
            if (!chk) {
                msg <- "The total of the visiting team scores in the match result summary (x$meta$result) does not match the total number of points recorded for the visiting team in the plays data"
                out <- rbind(out, data.frame(file_line_number = NA, video_time = NA, message = msg, file_line = NA_character_, severity = 3, stringsAsFactors = FALSE))
            }
        }

        ## at most one serve and one reception per rally
        pid <- plays %>% dplyr::filter(.data$skill == "Serve") %>% dplyr::count(.data$point_id) %>% dplyr::filter(.data$n > 1) %>% pull(.data$point_id)
        chk <- plays$skill %eq% "Serve" & plays$point_id %in% pid
        if (any(chk)) out <- rbind(out, data.frame(file_line_number = plays$file_line_number[chk], video_time = video_time_from_raw(x$raw[plays$file_line_number[chk]]), message = "Multiple serves in a single rally", file_line = mt2nachar(x$raw[plays$file_line_number[chk]]), severity = 3, stringsAsFactors = FALSE))

        pid <- plays %>% dplyr::filter(.data$skill == "Reception") %>% dplyr::count(.data$point_id) %>% dplyr::filter(.data$n > 1) %>% pull(.data$point_id)
        chk <- plays$skill %eq% "Reception" & plays$point_id %in% pid
        if (any(chk)) out <- rbind(out, data.frame(file_line_number = plays$file_line_number[chk], video_time = video_time_from_raw(x$raw[plays$file_line_number[chk]]), message = "Multiple receptions in a single rally", file_line = mt2nachar(x$raw[plays$file_line_number[chk]]), severity = 3, stringsAsFactors = FALSE))

        ## no reception coded, but there was a serve and it wasn't an error, and there wasn't a rotation error
        pid <- plays %>% group_by(.data$point_id) %>% dplyr::summarize(not_ok = !any(.data$skill %eq% "Reception") && any(.data$skill %eq% "Serve") && !any(.data$skill %eq% "Serve" & .data$evaluation %eq% "Error") && !any(.data$skill %eq% "Rotation error")) %>% dplyr::filter(.data$not_ok) %>% pull(.data$point_id)
        chk <- plays$skill %eq% "Serve" & plays$point_id %in% pid
        if (any(chk)) out <- rbind(out, data.frame(file_line_number = plays$file_line_number[chk], video_time = video_time_from_raw(x$raw[plays$file_line_number[chk]]), message = "Serve (that was not an error) did not have an accompanying reception", file_line = mt2nachar(x$raw[plays$file_line_number[chk]]), severity = 3, stringsAsFactors = FALSE))

        ## rally had actions but not a serve
        pid <- plays %>% group_by(.data$point_id) %>% dplyr::summarize(not_ok = any(.data$skill %in% c("Reception", "Set", "Attack", "Block", "Dig", "Freeball")) && !any(.data$skill %eq% "Serve")) %>% dplyr::filter(.data$not_ok) %>% pull(.data$point_id)
        if (length(pid)) {
            chk <- sapply(pid, function(thispid) head(which(plays$point_id == thispid & !is.na(plays$skill)), 1))
            if (length(chk)) out <- rbind(out, data.frame(file_line_number = plays$file_line_number[chk], video_time = video_time_from_raw(x$raw[plays$file_line_number[chk]]), message = "Rally had ball contacts but no serve", file_line = mt2nachar(x$raw[plays$file_line_number[chk]]), severity = 3, stringsAsFactors = FALSE))
        }

        ## receive type must match serve type
        idx <- which(plays$skill %eq% "Reception" & lag(plays$skill) %eq% "Serve")
        idx2 <- idx[plays$skill_type[idx] != paste0(plays$skill_type[idx-1], " reception") & (plays$skill_type[idx] != "Unknown serve reception type" & plays$skill_type[idx-1] != "Unknown serve type")]
        if (length(idx2)>0)
            out <- rbind(out, chk_df(plays[idx2, ], paste0("Reception type (", plays$skill_type[idx2], ") does not match serve type (", plays$skill_type[idx2-1], ")")))
        if (validation_level > 2) {
            ## should this be validation level > 2??
            ##idx <- which(plays$skill %eq% "Serve" & !is.na(plays$end_zone) & !is.na(plays$end_coordinate))
            ##if (length(idx) > 0) {
            ##    zz <- dv_xy2zone(plays$end_coordinate[idx])
            ##    chk <- which(plays$end_zone[idx] != zz)
            ##    if (length(chk) > 0) {
            ##        out <- rbind(out, data.frame(file_line_number = plays$file_line[idx[chk]], video_time = plays$video_time[idx[chk]], message = paste0("End zone of serve (", plays$end_zone[idx[chk]], ") does not match the end zone implied by the end coordinate (", zz[chk], ")"), file_line = mt2nachar(x$raw[plays$file_line[idx[chk]]]), severity = 3, stringsAsFactors = FALSE))
            ##    }
            ##}
            ## reception zones must match serve zones
            idx <- which(plays$skill %eq% "Reception" & lag(plays$skill) %eq% "Serve")
            ## start zones mismatch, but ignore any missing
            idx2 <- idx[(!plays$start_zone[idx] %eq% plays$start_zone[idx-1]) & !is.na(plays$start_zone[idx]) & !is.na(plays$start_zone[idx-1])]
            if (length(idx2)>0)
                out <- rbind(out,chk_df(plays[idx2,],paste0("Reception start zone (",plays$start_zone[idx2],") does not match serve start zone (",plays$start_zone[idx2-1],")"),severity=1))
            ## end zones mismatch, but ignore any missing
            idx2 <- idx[(!plays$end_zone[idx] %eq% plays$end_zone[idx-1]) & !is.na(plays$end_zone[idx]) & !is.na(plays$end_zone[idx-1])]
            if (length(idx2)>0)
                out <- rbind(out,chk_df(plays[idx2,],paste0("Reception end zone (",plays$end_zone[idx2],") does not match serve end zone (",plays$end_zone[idx2-1],")"),severity=1))
            ## end zones mismatch, but ignore any missing
            idx2 <- idx[(!plays$end_subzone[idx] %eq% plays$end_subzone[idx-1]) & !is.na(plays$end_subzone[idx]) & !is.na(plays$end_subzone[idx-1])]
            if (length(idx2)>0)
                out <- rbind(out,chk_df(plays[idx2,],paste0("Reception end sub-zone (",plays$end_subzone[idx2],") does not match serve end sub-zone (",plays$end_subzone[idx2-1],")"),severity=1))

            ## attack type must match set type
            ## but only from same team, so that e.g. attacks on over-sets don't get flagged here
            idx <- which(plays$skill %eq% "Attack" & lag(plays$skill) %eq% "Set" & plays$team %eq% lag(plays$team))
            idx <- idx[plays$skill_type[idx] != gsub(" set"," attack",plays$skill_type[idx-1])]
            if (length(idx)>0)
                out <- rbind(out,chk_df(plays[idx,],paste0("Attack type (",plays$skill_type[idx],") does not match set type (",plays$skill_type[idx-1],")")))

            ## block type must match attack type
            idx <- which(plays$skill %eq% "Block" & lag(plays$skill) %eq% "Attack")
            idx <- idx[plays$skill_type[idx] != gsub(" attack"," block",plays$skill_type[idx-1])]
            if (length(idx)>0)
                out <- rbind(out,chk_df(plays[idx,],paste0("Block type (",plays$skill_type[idx],") does not match attack type (",plays$skill_type[idx-1],")")))

            ## dig type must match attack type
            idx <- which(plays$skill %eq% "Dig" & lag(plays$skill) %eq% "Attack")
            idx <- idx[plays$skill_type[idx]!=gsub(" attack"," dig",plays$skill_type[idx-1])]
            if (length(idx)>0)
                out <- rbind(out,chk_df(plays[idx,],paste0("Dig type (",plays$skill_type[idx],") does not match attack type (",plays$skill_type[idx-1],")")))
        }

        if (file_type == "indoor") {
            ## front-row attacking player isn't actually in front row
            ## find front-row players for each attack
            ignore_codes <- options$setter_tip_codes
            if (!is.null(ignore_codes)) {
                if (!is.character(ignore_codes)) ignore_codes <- NULL
            }
            if (!is.null(ignore_codes)) ignore_codes <- na.omit(ignore_codes)
            attacks <- plays[plays$skill %eq% "Attack", ]
            if (nrow(attacks) > 0) {
                for (p in 1:6) attacks[, paste0("attacker_", p)] <- NA_integer_
                idx <- attacks$home_team %eq% attacks$team
                attacks[idx, paste0("attacker_", 1:6)] <- attacks[idx, paste0("home_p", 1:6)]
                attacks[!idx, paste0("attacker_", 1:6)] <- attacks[!idx, paste0("visiting_p", 1:6)]
                chk <- attacks[which(attacks$start_zone %in% c(2, 3, 4) & (!attacks$attack_code %in% ignore_codes) & (attacks$player_number == attacks$attacker_1 | attacks$player_number == attacks$attacker_5 | attacks$player_number == attacks$attacker_6)), ]
                if (nrow(chk)>0) out <- rbind(out,chk_df(chk,"Back-row player made an attack from a front-row zone",severity=3))

                ## and vice-versa: attack starting from back row by a front-row player
                chk <- attacks[which(attacks$start_zone %in% c(5, 6, 7, 8, 9, 1) & (attacks$player_number == attacks$attacker_2 | attacks$player_number == attacks$attacker_3 | attacks$player_number == attacks$attacker_4)), ]
                if (nrow(chk)>0)
                    out <- rbind(out,chk_df(chk,"Front-row player made an attack from a back-row zone (legal, but possibly a scouting error)",severity=2))
                ## those are probably less of an issue than a back-row player making a front row attack. A front-row player making a back row attack is not illegal, just inconsistent

                ## quick attacks by non-middles
                idx <- attacks$team %eq% attacks$home_team
                attacks$player_role <- NA_character_
                temp_roles <- left_join(attacks[idx, ], dplyr::distinct(x$meta$players_h[, c("player_id", "role")], .data$player_id, .keep_all = TRUE), by = "player_id")$role
                attacks$player_role[idx] <- temp_roles
                idx <- attacks$team %eq% attacks$visiting_team
                temp_roles <- left_join(attacks[idx, ], dplyr::distinct(x$meta$players_v[, c("player_id", "role")], .data$player_id, .keep_all = TRUE), by = "player_id")$role
                attacks$player_role[idx] <- temp_roles
                ## first-tempo attack by non-middle
                chk <- attacks[!attacks$player_role %in% c(NA_character_, "middle") & grepl("^Quick", attacks$skill_type), ]
                if (nrow(chk) > 0) {
                    out <- rbind(out, chk_df(chk, "Quick attack by non-middle player", severity = 2))
                }
                ## middle attack not a quick ball
                chk <- attacks[attacks$player_role %eq% "middle" & !grepl("^(Quick|Other)", attacks$skill_type), ]
                if (nrow(chk) > 0) {
                    out <- rbind(out, chk_df(chk, "Middle player made a non-quick attack", severity = 2))
                }
            }
            ## back row player blocking
            chk <- (plays$skill %eq% "Block") &
                (((plays$team %eq% plays$home_team) & (plays$player_number %eq% plays$home_p5 | plays$player_number %eq% plays$home_p6 | plays$player_number %eq% plays$home_p1)) |
                 ((plays$team %eq% plays$visiting_team) & (plays$player_number %eq% plays$visiting_p5 | plays$player_number %eq% plays$visiting_p6 | plays$player_number %eq% plays$visiting_p1)))
            if (any(chk)) {
                out <- rbind(out,data.frame(file_line_number = plays$file_line_number[chk], video_time = video_time_from_raw(x$raw[plays$file_line_number[chk]]), message = "Block by a back-row player", file_line = mt2nachar(x$raw[plays$file_line_number[chk]]), severity = 3, stringsAsFactors = FALSE))
            }

            ## for the next two, not sure if we should assume rotation errors should be aces or not
            ##  so the default rotation_error_is_ace for each is set to not warn when rotation errors are present
            ## serves that should be coded as aces, but were not
            find_should_be_aces <- function(rally, rotation_error_is_ace = FALSE) {
                sv <- which(rally$skill == "Serve")
                if (length(sv) == 1) {
                    was_ace <- (rally$team[sv] %eq% rally$point_won_by[sv]) && (!"Reception" %in% rally$skill || (sum(rally$skill %eq% "Reception") == 1 && rally$evaluation[rally$skill %eq% "Reception"] %eq% "Error")) && (rotation_error_is_ace | !rally$skill[sv + 1] %eq% "Rotation error")
                    if (!rotation_error_is_ace && (rally$skill[sv + 1] %eq% "Rotation error")) was_ace <- FALSE ## to avoid warnings
                    ## also skip this check if the next skill not reception (or rotation error), since that's likely to affect this
                    if (!is.na(rally$skill[sv + 1]) && (!rally$skill[sv + 1] %in% c("Rotation error","Reception"))) was_ace <- FALSE
                    if (was_ace & !identical(rally$evaluation[sv], "Ace")) {
                        list(point_id = rally$point_id[1], should_be_ace = TRUE)
                    } else {
                        list(point_id = rally$point_id[1], should_be_ace = FALSE)
                    }
                } else {
                    list(point_id = rally$point_id[1], should_be_ace = NA)
                }
            }
            pid <- bind_rows(lapply(split(plays, plays$point_id), find_should_be_aces)) %>% na.omit %>% dplyr::filter(.data$should_be_ace) %>% pull(.data$point_id) %>% sort

            chk <- plays$skill %eq% "Serve" & plays$point_id %in% pid
            if (any(chk))
                out <- rbind(out,data.frame(file_line_number=plays$file_line_number[chk],video_time=video_time_from_raw(x$raw[plays$file_line_number[chk]]),message="Winning serve not coded as an ace",file_line=mt2nachar(x$raw[plays$file_line_number[chk]]),severity=3,stringsAsFactors=FALSE))
            ## and vice-versa: serves that were coded as aces, but should not have been
            find_should_not_be_aces <- function(rally,rotation_error_is_ace=TRUE) {
                ## by default assume rotation errors should be aces here (opposite to above)
                ## so that warnings won't be issued when rotation errors present
                sv <- which(rally$skill=="Serve")
                if (length(sv)==1) {
                    was_ace <- (rally$team[sv] %eq% rally$point_won_by[sv]) && (!"Reception" %in% rally$skill || (sum(rally$skill %eq% "Reception") == 1 && rally$evaluation[rally$skill %eq% "Reception"] %eq% "Error")) && (rotation_error_is_ace | !rally$skill[sv + 1] %eq% "Rotation error")
                    if (rotation_error_is_ace && (rally$skill[sv + 1] %eq% "Rotation error")) was_ace <- TRUE ## to avoid warnings
                    if (!was_ace & identical(rally$evaluation[sv],"Ace")) {
                        list(point_id = rally$point_id[1], should_not_be_ace = TRUE)
                    } else {
                        list(point_id = rally$point_id[1], should_not_be_ace = FALSE)
                    }
                } else {
                    list(point_id = rally$point_id[1], should_not_be_ace = NA)
                }
            }
            pid <- bind_rows(lapply(split(plays, plays$point_id), find_should_not_be_aces)) %>% na.omit %>% dplyr::filter(.data$should_not_be_ace) %>% pull(.data$point_id) %>% sort
            chk <- plays$skill %eq% "Serve" & plays$point_id %in% pid
            if (any(chk))
                out <- rbind(out,data.frame(file_line_number=plays$file_line_number[chk],video_time=video_time_from_raw(x$raw[plays$file_line_number[chk]]),message="Non-winning serve was coded as an ace",file_line=mt2nachar(x$raw[plays$file_line_number[chk]]),severity=3,stringsAsFactors=FALSE))

            ## server not in position 1
            chk <- (plays$skill %eq% "Serve") & (((plays$team %eq% plays$home_team) & (!plays$player_number %eq% plays$home_p1)) | ((plays$team %eq% plays$visiting_team) & (!plays$player_number %eq% plays$visiting_p1)))
            if (any(chk))
                out <- rbind(out,data.frame(file_line_number=plays$file_line_number[chk],video_time=video_time_from_raw(x$raw[plays$file_line_number[chk]]),message="Serving player not in position 1",file_line=mt2nachar(x$raw[plays$file_line_number[chk]]),severity=3,stringsAsFactors=FALSE))
        }

        ## number of blockers (for an attack) should be >=1 if it is followed by a block
        idx <- which(plays$skill %eq% "Attack" & lead(plays$skill) %eq% "Block" & !plays$team %eq% lead(plays$team))
        idx2 <- idx[is.na(plays$num_players[idx])]
        if (length(idx2)>0)
            out <- rbind(out,chk_df(plays[idx2,],"Attack (which was blocked) does not have number of blockers recorded",severity=1))

        idx2 <- idx[plays$num_players[idx] %eq% "No block"]
        if (length(idx2)>0)
            out <- rbind(out,chk_df(plays[idx2,],"Attack (which was followed by a block) has \"No block\" recorded for number of players",severity=3))

        ## winning attack not coded as such
        ## this one seems to be too problematic: e.g. can have attack that was hit out, but block net touch, so attack should NOT be coded as winning attack
                                        #idx <- which(plays$skill %eq% "Attack") ## +1 to be on the next skill
                                        #idx2 <- idx[!plays$evaluation[idx] %eq% "Winning attack" & !plays$team[idx] %eq% plays$team[idx+1] & plays$evaluation[idx+1] %eq% "Error" & !is.na(plays$skill[idx+1])]
                                        #if (length(idx2)>0)
                                        #    out <- rbind(out,chk_df(plays[idx2,],"Winning attack was not recorded as such",severity=3))

        if (FALSE) {
            ## this not exposed yet pending further testing
            ## cones or zones?
            if (x$meta$match$zones_or_cones %eq% "C") {
                idx <- which(plays$skill %eq% "Attack" & !is.na(plays$end_cone) & !is.na(plays$end_coordinate) & !is.na(plays$start_zone))
                if (length(idx) > 0) {
                    cc <- dv_xy2cone(plays$end_coordinate[idx], start_zones = plays$start_zone[idx])
                    chk <- which(plays$end_cone[idx] != cc)
                    if (length(chk) > 0) {
                        out <- rbind(out, data.frame(file_line_number = plays$file_line[idx[chk]], video_time = plays$video_time[idx[chk]], message = paste0("Attack cone (", plays$end_cone[idx[chk]], ") does not match the cone implied by the end coordinate (", cc[chk], ")"), file_line = mt2nachar(x$raw[plays$file_line[idx[chk]]]), severity = 3, stringsAsFactors = FALSE))
                    }
                }
            } else if (x$meta$match$zones_or_cones %eq% "Z") {
                idx <- which(plays$skill %eq% "Attack" & !is.na(plays$end_zone) & !is.na(plays$end_coordinate))
                if (length(idx) > 0) {
                    zz <- dv_xy2zone(plays$end_coordinate[idx])
                    chk <- which(plays$end_zone[idx] != zz)
                    if (length(chk) > 0) {
                        out <- rbind(out, data.frame(file_line_number = plays$file_line[idx[chk]], video_time = plays$video_time[idx[chk]], message = paste0("End zone of attack (", plays$end_zone[idx[chk]], ") does not match the end zone implied by the end coordinate (", zz[chk], ")"), file_line = mt2nachar(x$raw[plays$file_line[idx[chk]]]), severity = 3, stringsAsFactors = FALSE))
                    }
                }
            } else {
                ## cones/zones ambiguous
                ## check this before enabling
                ##fln <- grep("[3MATCH]", x$raw, fixed = TRUE)
                ##if (length(fln) != 1) fln <- NA_integer_
                ##out <- rbind(out, data.frame(file_line_number = fln, video_time = NA_real_, message = "The file does not indicate whether attacks were scouted with cones or zones", file_line = mt2nachar(ifelse(is.na(fln), NA_character_, x$raw[fln])), severity = 3, stringsAsFactors = FALSE)))
            }
        }

        ## player not in recorded rotation making a play (other than by libero)
        liberos_v <- x$meta$players_v$number[grepl("L", x$meta$players_v$special_role)]
        liberos_h <- x$meta$players_h$number[grepl("L", x$meta$players_h$special_role)]
        pp <- plays[plays$skill %in% c("Serve", "Attack", "Block", "Dig", "Freeball", "Reception", "Set") & !is.na(plays$player_number) & !plays$player_name %eq% "Unknown player", ]
        if (nrow(pp) > 0) {
            idx <- pp$team %eq% pp$home_team
            temp <- setNames(pp[, paste0("visiting_p", team_player_num)], paste0("player", team_player_num))
            temp[idx, ] <- setNames(pp[idx, paste0("home_p", team_player_num)], paste0("player", team_player_num))
            temp <- as.data.frame(temp, stringsAsFactors = FALSE)
            rownames(temp) <- NULL
            chk <- rep(NA, nrow(pp))
            chk[idx] <- vapply(which(idx), function(z) !pp$player_number[z] %in% liberos_h && (!pp$player_number[z] %in% temp[z, ]), FUN.VALUE = TRUE)
            chk[!idx] <- vapply(which(!idx), function(z) !pp$player_number[z] %in% liberos_v && (!pp$player_number[z] %in% temp[z, ]), FUN.VALUE = TRUE)
            if (any(chk)) {
                out <- rbind(out, data.frame(file_line_number = pp$file_line_number[chk], video_time = video_time_from_raw(x$raw[pp$file_line_number[chk]]), message = "The listed player is not on court in this rotation", file_line = mt2nachar(x$raw[pp$file_line_number[chk]]), severity = 3, stringsAsFactors = FALSE))
            }
        }

        if (file_type == "indoor") {
            ## liberos doing stuff they oughtn't be doing
            if (length(liberos_h)>0) {
                chk <- (plays$skill %in% c("Serve", "Attack", "Block")) & (plays$home_team %eq% plays$team) & (plays$player_number %in% liberos_h)
                if (any(chk)) {
                    out <- rbind(out, data.frame(file_line_number = plays$file_line_number[chk], video_time = video_time_from_raw(x$raw[plays$file_line_number[chk]]), message = paste0("Player designated as libero was recorded making a", ifelse(grepl("^a", tolower(plays$skill[chk])), "n ", " "), tolower(plays$skill[chk])), file_line = mt2nachar(x$raw[plays$file_line_number[chk]]), severity = 3, stringsAsFactors = FALSE))
                }
            }
            if (length(liberos_v)>0) {
                chk <- (plays$skill %in% c("Serve","Attack","Block")) & (plays$visiting_team %eq% plays$team) & (plays$player_number %in% liberos_v)
                if (any(chk))
                    out <- rbind(out, data.frame(file_line_number = plays$file_line_number[chk], video_time = video_time_from_raw(x$raw[plays$file_line_number[chk]]), message = paste0("Player designated as libero was recorded making a", ifelse(grepl("^a", tolower(plays$skill[chk])), "n ", " "), tolower(plays$skill[chk])), file_line = mt2nachar(x$raw[plays$file_line_number[chk]]), severity = 3, stringsAsFactors = FALSE))
            }
            ## TO DO, perhaps: check for liberos making a front-court set that is then attacked
        }
        
        ## duplicate entries with same skill and evaluation code for the same player
        idx <- which((plays$evaluation_code[-1] %eq% plays$evaluation_code[-nrow(plays)]) &
                     (plays$skill[-1] %eq% plays$skill[-nrow(plays)]) &
                     (plays$team[-1] %eq% plays$team[-nrow(plays)]) &
                     (plays$player_number[-1] %eq% plays$player_number[-nrow(plays)])
                     )+1
        if (length(idx) > 0)
            out <- rbind(out,data.frame(file_line_number=plays$file_line_number[idx],video_time=video_time_from_raw(x$raw[plays$file_line_number[idx]]),message="Repeated row with same skill and evaluation_code for the same player",file_line=mt2nachar(x$raw[plays$file_line_number[idx]]),severity=3,stringsAsFactors=FALSE))

        ## consecutive actions by the same player
        ## be selective about which skill sequences count here, because some scouts might record duplicate skills for the same player (e.g. reception and set) for one physical action
        ## also don't bother picking up illegal skill sequences, they will be picked up elsewhere
        idx0 <- seq_len(nrow(plays)-1); idx0_next <- seq_len(nrow(plays))[-1]
        idx <- which(plays$player_id[idx0] %eq% plays$player_id[idx0_next] &
                     ((plays$skill[idx0] %eq% "Reception" & plays$skill[idx0_next] %in% c("Attack")) |
                      (plays$skill[idx0] %eq% "Set" & plays$skill[idx0_next] %eq% "Block")))
        if (length(idx)>0)
            out <- rbind(out,chk_df(plays[idx+1,],"Consecutive actions by the same player",severity=3))

        ## every point (with any actual skill) should have a winning action or error
        tmp_any_skill <- !is.na(plays$skill) & !plays$skill %in% c("Timeout", "Technical timeout", "Substitution")
        tmp_win_err <- (plays$skill %eq% "Block" & plays$evaluation %eq% "Invasion") | grepl("Error", plays$evaluation) | grepl("Ace|Winning", plays$evaluation)
        chk <- unlist(lapply(unique(plays$point_id), function(pid) {
            idx <- plays$point_id %eq% pid
            if (any(tmp_any_skill[idx]) & !any(tmp_win_err[idx])) max(which(idx)) else NULL
            }))

        if (length(chk) > 0) {
            out <- rbind(out, data.frame(file_line_number = plays$file_line_number[chk], video_time = video_time_from_raw(x$raw[plays$file_line_number[chk]]), message = "Rally does not include a winning or losing action", file_line = mt2nachar(x$raw[plays$file_line_number[chk]]), severity = 3, stringsAsFactors = FALSE))
        }

        ## point not awarded to right team following error
        chk <- (plays$evaluation_code %eq% "=" | (plays$skill %eq% "Block" & grepl("invasion", plays$evaluation, ignore.case=TRUE))) &  ## error or block Invasion
            (plays$team %eq% plays$point_won_by)
        if (any(chk))
            out <- rbind(out,data.frame(file_line_number=plays$file_line_number[chk],video_time=video_time_from_raw(x$raw[plays$file_line_number[chk]]),message="Point awarded to incorrect team following error (or \"error\" evaluation incorrect)",file_line=mt2nachar(x$raw[plays$file_line_number[chk]]),severity=3,stringsAsFactors=FALSE))

        ## point not awarded to right team following win
        chk <- (plays$skill %in% c("Serve","Attack","Block") & plays$evaluation_code %eq% "#") &  ## ace or winning attack or block
            (!plays$team %eq% plays$point_won_by)
        if (any(chk))
            out <- rbind(out,data.frame(file_line_number=plays$file_line_number[chk],video_time=video_time_from_raw(x$raw[plays$file_line_number[chk]]),message=paste0("Point awarded to incorrect team (or \"",plays$evaluation[chk],"\" evaluation incorrect)"),file_line=mt2nachar(x$raw[plays$file_line_number[chk]]),severity=3,stringsAsFactors=FALSE))

        ## check scores columns against point_won_by entries
        temp <- plays[!is.na(plays$set_number) & !is.na(plays$point_won_by), ]
        temp <- do.call(rbind, lapply(sort(unique(temp$point_id)), function(pid) {
            tail(temp[which(temp$point_id == pid), c("point_id", "set_number", "home_team", "visiting_team", "home_team_score", "visiting_team_score", "point_won_by", "file_line_number")], 1)
        }))
        if (!is.null(temp) && nrow(temp) > 0) {
            temp$won_by_home <- temp$point_won_by==temp$home_team
            ## dplyr call: temp <- plays %>% group_by(point_id) %>% do(.[,c("set_number","home_team","visiting_team","home_team_score","visiting_team_score","point_won_by")] %>% filter(!is.na(set_number)) %>% distinct()) %>% ungroup %>% filter(!is.na(point_won_by)) %>% arrange(point_id) %>% mutate(won_by_home=point_won_by==home_team)
            temp$home_team_diff <- diff(c(0, temp$home_team_score))
            temp$visiting_team_diff <- diff(c(0, temp$visiting_team_score))
            temp$point_lost_by <- temp$home_team
            idx <- temp$point_won_by %eq% temp$home_team
            temp$point_lost_by[idx] <- temp$visiting_team[idx]
            temp$ok <- rep(TRUE, nrow(temp))
            idx <- temp$won_by_home
            temp$ok[idx] <- temp$home_team_diff[idx]==1 & temp$visiting_team_diff[idx]==0
            idx <- !temp$won_by_home
            temp$ok[idx] <- temp$visiting_team_diff[idx]==1 & temp$home_team_diff[idx]==0
            ## these won't be valid for first point of each set other than first set
            for (ss in seq_len(max(temp$set_number, na.rm = TRUE))[-1]) {
                idx <- temp$set_number==ss
                if (any(idx)) {
                    idx <- min(which(idx)) ## first point of set ss
                    temp$ok[idx] <- (temp$won_by_home[idx] && temp$home_team_score[idx]==1 && temp$visiting_team_score[idx]==0) || (!temp$won_by_home[idx] && temp$home_team_score[idx]==0 && temp$visiting_team_score[idx]==1)
                }
            }
            if (any(is.na(temp$point_won_by))) {
                ## should not see this
                ## just assume were ok
                temp$ok[is.na(temp$point_won_by)] <- TRUE
            }
            temp <- temp[!temp$ok,]
            if (nrow(temp)>0) {
                for (chk in seq_len(nrow(temp))) {
                    out <- rbind(out,data.frame(file_line_number=temp$file_line_number[chk],video_time=video_time_from_raw(x$raw[temp$file_line_number[chk]]),message=paste0("Point assigned to incorrect team or scores incorrect (point was won by ",temp$point_won_by[chk]," but score was incremented for ",temp$point_lost_by[chk],")"),file_line=mt2nachar(x$raw[temp$file_line_number[chk]]),severity=3,stringsAsFactors=FALSE))
                }
            }
        }

        ## scores not in proper sequence
        ## a team's score should never increase by more than 1 at a time. May be negative (change of sets)
        temp <- plays[, c("home_team_score","visiting_team_score","set_number")]
        temp <- apply(temp,2,diff)
        ## either score increases by more than one, or (decreases and not end of set)
        chk <- which((temp[,1]>1 | temp[,2]>1) | ((temp[,1]<0 | temp[,2]<0) & !(is.na(temp[,3]) | temp[,3]>0)))
        ##chk <- diff(plays$home_team_score)>1 | diff(plays$visiting_team_score)>1
        ##if (any(chk))
        if (length(chk)>0) {
            chk <- chk+1 
           out <- rbind(out,data.frame(file_line_number=plays$file_line_number[chk],video_time=video_time_from_raw(x$raw[plays$file_line_number[chk]]),message="Scores do not follow proper sequence (note that the error may be in the point before this one)",file_line=mt2nachar(x$raw[plays$file_line_number[chk]]),severity=3,stringsAsFactors=FALSE))
        }

        ## check for incorrect rotation changes
        rotleft <- function(x) if (is.data.frame(x)) { out <- cbind(x[, -1], x[, 1]); names(out) <- names(x); out} else c(x[-1], x[1])
        isrotleft <- function(x, y) all(as.numeric(rotleft(x)) == as.numeric(y)) ## is y a left-rotated version of x?
        for (tm in c("*", "a")) {
            rot_cols <- if (tm == "a") paste0("visiting_p", team_player_num) else paste0("home_p", team_player_num)
            temp <- plays[!tolower(plays$skill) %eq% "technical timeout", ]
            rx <- temp[, rot_cols]
            idx <- unname(c(FALSE, rowSums(abs(apply(rx, 2, diff))) > 0)) ## rows where rotation changed from previous
            idx[is.na(idx)] <- FALSE ## end of set, etc, ignore
            idx[temp$substitution] <- FALSE ## subs, ignore these here and they will be checked in the next section
            ## ignore any changes at the very start of the set, since lineups can be modified here multiple times fairly arbitrarily
            nsk <- rep(NA_integer_, nrow(temp))
            skcount <- 0L
            for (ii in seq_along(nsk)) {
                if (temp$end_of_set[ii]) skcount <- 0L ## reset
                if (!is.na(temp$skill[ii])) skcount <- skcount + 1L
                nsk[ii] <- skcount
            }
            idx[nsk < 1L] <- FALSE
            for (k in which(idx)) {
                if (!isrotleft(as.numeric(rx[k - 1, ]), as.numeric(rx[k, ])))
                    out <- rbind(out, data.frame(file_line_number = temp$file_line_number[k], video_time = video_time_from_raw(x$raw[temp$file_line_number[k]]), message = paste0(if (tm == "a") "Visiting" else "Home"," team rotation has changed incorrectly"), file_line = mt2nachar(x$raw[temp$file_line_number[k]]), severity = 3, stringsAsFactors = FALSE))
            }
        }

        ## check that players changed correctly on substitution
        ## e.g. *c02:01 means player 2 replaced by player 1
        idx <- which(plays$substitution & grepl("^.c",plays$code))
        idx <- idx[idx>2 & idx<(nrow(plays)-1)] ## discard anything at the start or end of the file
        rot_errors <- list()
        for (k in idx) {
            rot_cols <- if (grepl("^a",plays$code[k])) paste0("visiting_p", team_player_num) else paste0("home_p", team_player_num)
            prev_rot <- plays[k-1, rot_cols]
            if (any(is.na(prev_rot))) {
                if (k>2) prev_rot <- plays[k-2, rot_cols]
            }
            if (any(is.na(prev_rot))) next ## could perhaps search further backwards, but should not need to
            ## have only seen one NA rot row in sequence in files so far

            new_rot <- plays[k+1,rot_cols]
            if (any(is.na(new_rot))) {
                if (k<(nrow(plays)-2)) new_rot <- plays[k+2,rot_cols]
            }
            if (any(is.na(new_rot))) next
            if (all(new_rot == prev_rot)) {
                ## players did not change
                rot_errors[[length(rot_errors)+1]] <- data.frame(file_line_number = plays$file_line_number[k], video_time = video_time_from_raw(x$raw[plays$file_line_number[k]]), message = paste0("Player lineup did not change after substitution: was the sub recorded incorrectly?"), file_line = mt2nachar(x$raw[plays$file_line_number[k]]), severity = 3, stringsAsFactors = FALSE)
                next
            }
            if (!grepl("^.c[[:digit:]]+:[[:digit:]]+", plays$code[k])) {
                rot_errors[[length(rot_errors) + 1]] <- data.frame(file_line_number = plays$file_line_number[k], video_time = video_time_from_raw(x$raw[plays$file_line_number[k]]), message = paste0("The substitution code (", plays$code[k], ") does not follow the expected format"), file_line = mt2nachar(x$raw[plays$file_line_number[k]]), severity = 3, stringsAsFactors = FALSE)
                next
            } else {
                subtxt <- strsplit(sub("^.c", "", plays$code[k]), ":")[[1]]
                sub_out <- as.numeric(subtxt[1]) ## outgoing player
                sub_in <- as.numeric(subtxt[2]) ## incoming player
                if (!sub_out %in% prev_rot || !sub_in %in% new_rot || sub_in %in% prev_rot || sub_out %in% new_rot) {
                    rot_errors[[length(rot_errors)+1]] <- data.frame(file_line_number=plays$file_line_number[k],video_time=video_time_from_raw(x$raw[plays$file_line_number[k]]),message=paste0("Player lineup conflicts with recorded substitution: was the sub recorded incorrectly?"),file_line=mt2nachar(x$raw[plays$file_line_number[k]]),severity=3,stringsAsFactors=FALSE)
                    next
                }
            }
        }
        if (length(rot_errors)>0) out <- rbind(out,do.call(rbind,rot_errors))
    } ## checking plays data
    out <- out[(4 - out$severity) <= validation_level, ]
    if (nrow(out) > 0) out <- dplyr::arrange(out, .data$file_line_number)
    out[, setdiff(names(out), "severity")]
}
