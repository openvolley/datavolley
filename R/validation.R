#' Additional validation checks on a DataVolley file
#'
#' This function is automatically run as part of \code{dv_read} if \code{extra_validation} is greater than zero.
#' The current validation messages/checks are:
#'   * message "The total of the home/visiting team scores in the match result summary (x$meta$result) does not match the total number of points recorded for the home/visiting team in the plays data"
#'   * message "Home/Visiting team roster is empty": the home or visiting team roster has not been entered
#'   * message "Players xxx and yyy have the same player ID": player IDs should be unique, and so duplicated IDs will be flagged here
#'   * message "Players xxx and yyy have the same jersey number": players on the same team should not have the same jersey number
#'   * message "The listed player is not on court in this rotation": the player making the action is not part of the current rotation. Libero players are ignored for this check
#'   * message "Back-row player made an attack from a front-row zone": an attack starting from zones 2-4 was made by a player in the back row of the current rotation
#'   * message "Front-row player made an attack from a back-row zone (legal, but possibly a scouting error)": an attack starting from zones 1,5-9 was made by a player in the front row of the current rotation
#'   * message "Quick attack by non-middle player"
#'   * message "Middle player made a non-quick attack"
#'   * message "Block by a back-row player"
#'   * message "Winning serve not coded as an ace"
#'   * message "Non-winning serve was coded as an ace"
#'   * message "Serving player not in position 1"
#'   * message "Player designated as libero was recorded making a serve/attack/block"
#'   * message "Attack (which was blocked) does not have number of blockers recorded"
#'   * message "Attack (which was followed by a block) has 'No block' recorded for number of players"
#   * message "End zone of attack does not match the end zone implied by the end coordinate"
#'   * message "Repeated row with same skill and evaluation_code for the same player"
#'   * message "Consecutive actions by the same player"
#'   * message "Point awarded to incorrect team following error (or \"error\" evaluation incorrect)"
#'   * message "Point awarded to incorrect team (or winning play evaluation incorrect)"
#'   * message "Scores do not follow proper sequence": one or both team scores change by more than one point at a time
#'   * message "Visiting/Home team rotation has changed incorrectly"
#'   * message "Player lineup did not change after substitution: was the sub recorded incorrectly?"
#'   * message "Player lineup conflicts with recorded substitution: was the sub recorded incorrectly?"
#   * message "End zone of serve does not match the end zone implied by the end coordinate"
#'   * message "Reception type does not match serve type": the type of reception (e.g. "Jump-float serve reception" does not match the serve type (e.g. "Jump-float serve")
#'   * message "Reception start zone does not match serve start zone"
#'   * message "Reception end zone does not match serve end zone"
#'   * message "Reception end sub-zone does not match serve end sub-zone"
#'   * message "Attack type does not match set type": the type of attack (e.g. "Head ball attack") does not match the set type (e.g. "High ball set")
#'   * message "Block type does not match attack type": the type of block (e.g. "Head ball block") does not match the attack type (e.g. "High ball attack")
#'   * message "Dig type does not match attack type": the type of dig (e.g. "Head ball dig") does not match the attack type (e.g. "High ball attack")
#'   * message "Multiple serves in a single rally"
#'   * message "Multiple receptions in a single rally"
#'   * message "Serve (that was not an error) did not have an accompanying reception"
#'   * message "Rally had ball contacts but no serve"
#'   * message "Replacement of home/visiting setter: the team is in rotation X but the replacement setter is not in that position"
#'   * message "Set on perfect/good reception made by a player other than the designated setter (might indicate an error with the rotation/designated setter)"
#'   * message "Setter call on a set made by a player other than the designated setter (might indicate an error with the rotation/designated setter)"
#'   * "Setter call on negative reception"
#'   * message "Set by the home/visiting team was in between a dig/reception and attack by the other team (was the set assigned to the correct team?)"
#'
#' @param x datavolley: datavolley object as returned by \code{dv_read}
#' @param validation_level numeric: how strictly to check? If 0, perform no checking; if 1, only identify major errors; if 2, also return any issues that are likely to lead to misinterpretation of data; if 3, return all issues (including minor issues such as those that might have resulted from selective post-processing of compound codes)
#' @param options list: named list of options that control optional validation behaviour. Valid entries are:
#' \itemize{
#'   \item setter_tip_codes character: vector of attack codes that represent setter tips (or other attacks that a back-row player can validly make from a front-row position). If you code setter tips as attacks, and don't want such attacks to be flagged as an error when made by a back-row player in a front-row zone, enter the setter tip attack codes here. e.g. \code{options=list(setter_tip_codes=c("PP","XY"))}
#' }
#' @param file_type string: "indoor" or "beach". If not provided, will be taken from the \code{x$file_meta$file_format} entry
#'
#' @return data.frame with columns message (the validation message), file_line_number (the corresponding line number in the DataVolley file), video_time, and file_line (the actual line from the DataVolley file).
#'
#' @seealso \code{\link{dv_read}}
#'
#' @examples
#' \dontrun{
#'   x <- dv_read(dv_example_file(), insert_technical_timeouts = FALSE)
#'   xv <- dv_validate(x)
#'
#'   ## specifying "PP" as the setter tip code
#'   ## front-row attacks (using this code) by a back-row player won't be flagged as errors
#'   xv <- dv_validate(x, options = list(setter_tip_codes = c("PP")))
#' }
#'
#' @export
dv_validate <- function(x, validation_level = 2, options = list(), file_type) {
    assert_that(is.numeric(validation_level) && validation_level %in% 0:3)
    assert_that(is.list(options))
    if (missing(file_type)) file_type <- if (isTRUE(grepl("beach", x$file_meta$file_type))) "beach" else "indoor"
    assert_that(is.string(file_type))
    file_type <- match.arg(tolower(file_type), c("indoor", "beach"))

    team_player_num <- if (grepl("beach", file_type)) 1:2 else 1:6

    out <- data.frame(file_line_number = integer(), video_time = numeric(), message = character(), file_line = character(), severity = numeric(), stringsAsFactors = FALSE)
    ## internal note, severity level 1 = minor, 2 = intermediate, 3 = major
    mt2nachar <- function(z) if (length(z) < 1) NA_character_ else z
    chk_df <- function(chk, msg, severity = 2) {
        vt <- video_time_from_raw(x$raw[chk$file_line_number])
        if (length(vt) < 1) vt <- NA_integer_
        data.frame(file_line_number = chk$file_line_number, video_time = vt, message = msg, file_line = mt2nachar(x$raw[chk$file_line_number]), severity = severity, stringsAsFactors = FALSE)
    }
    if (validation_level<1) return(out)

    ## metadata checks
    if (is.null(x$meta$players_h) || nrow(x$meta$players_h) < 1) {
        ## home team player list is empty
        flnm <- grep("[3PLAYERS-H]", x$raw, fixed = TRUE)
        if (length(flnm) != 1) flnm <- NA_integer_
        out <- rbind(out, data.frame(file_line_number = flnm, video_time = NA_integer_, message = "Home team roster is empty",
                                     file_line = if (!is.na(flnm)) x$raw[flnm] else NA_character_, severity = 3, stringsAsFactors = FALSE))
    }
    if (is.null(x$meta$players_v) || nrow(x$meta$players_v) < 1) {
        ## visiting team player list is empty
        flnm <- grep("[3PLAYERS-V]", x$raw, fixed = TRUE)
        if (length(flnm) != 1) flnm <- NA_integer_
        out <- rbind(out, data.frame(file_line_number = flnm, video_time = NA_integer_, message = "Visiting team roster is empty",
                                     file_line = if (!is.na(flnm)) x$raw[flnm] else NA_character_, severity = 3, stringsAsFactors = FALSE))
    }

    ## check for duplicate player IDs across both teams
    ph <- x$meta$players_h
    ph$team <- home_team(x)
    ph$hv <- "home"
    pv <- x$meta$players_v
    pv$team <- visiting_team(x)
    pv$hv <- "visiting"
    plyrs <- tryCatch(rbind(ph, pv), error = function(e) bind_rows(ph, pv)) ## rbind fails when ph, pv have different columns (can happen with pv files); bind_rows fails when cols have different types
    dpids <- plyrs$player_id[duplicated(plyrs$player_id)]
    for (dpid in unique(dpids)) {
        idx <- plyrs$player_id %eq% dpid
        msg <- paste0("Players have the same player ID (", dpid, "): ")
        this_players <- paste0(plyrs$name[idx], " (", plyrs$hv[idx], " team ", plyrs$team[idx], " #", plyrs$number[idx], ")", collapse=", ")
        out <- rbind(out, data.frame(file_line_number = NA, video_time = NA, message = paste0(msg, this_players), file_line = NA_character_, severity = 3, stringsAsFactors = FALSE))
    }

    ## check for duplicate jersey numbers within a team
    chk <- find_duplicate_player_numbers(x)
    if (!is.null(chk) && nrow(chk) > 0) {
        chk <- chk %>% group_by(.data$hv, .data$number) %>% dplyr::summarize(player_ids = paste(.data$player_id, collapse = ", ")) %>%
            ungroup %>% mutate(msg = paste0("Players have the same jersey number ", .data$number, ": ", .data$hv, " team ", ifelse(.data$hv == "home", home_team(x), visiting_team(x)), " player IDs ", .data$player_ids))
        out <- rbind(out, data.frame(file_line_number = NA, video_time = NA, message = chk$msg, file_line = NA_character_, severity = 3, stringsAsFactors = FALSE))
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
                msg <- "The total of the home team scores in the match result summary does not match the total number of points recorded for the home team in the plays data"
                out <- rbind(out, data.frame(file_line_number = NA, video_time = NA, message = msg, file_line = NA_character_, severity = 3, stringsAsFactors = FALSE))
            }
            chk <- sum(x$meta$result$score_visiting_team, na.rm = TRUE) == sum(x$plays$team == x$plays$visiting_team & x$plays$point, na.rm = TRUE)
            if (!chk) {
                msg <- "The total of the visiting team scores in the match result summary does not match the total number of points recorded for the visiting team in the plays data"
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
                ## first-tempo attack by non-middle (but allow slides, because e.g. the opposite might run a slide, albeit unusual)
                chk <- attacks[!attacks$player_role %in% c(NA_character_, "middle") & grepl("^Quick", attacks$skill_type), ]
                if (nrow(chk) > 0) out <- rbind(out, chk_df(chk, "Quick attack by non-middle player", severity = 2))
                ## middle attack not a quick, slide, or other (e.g. overpass PR) ball
                chk <- attacks[attacks$player_role %eq% "middle" & !grepl("^(Quick|Other|Slide)", attacks$skill_type), ]
                if (nrow(chk) > 0) out <- rbind(out, chk_df(chk, "Middle player made a non-quick attack", severity = 2))
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

            ## checking some setter and setter-call related issues
            ## identify designated setter on court and add player roles
            plays <- mutate(plays, home_setter_id = case_when(.data$home_setter_position == 1 ~ .data$home_player_id1,
                                                              .data$home_setter_position == 2 ~ .data$home_player_id2,
                                                              .data$home_setter_position == 3 ~ .data$home_player_id3,
                                                              .data$home_setter_position == 4 ~ .data$home_player_id4,
                                                              .data$home_setter_position == 5 ~ .data$home_player_id5,
                                                              .data$home_setter_position == 6 ~ .data$home_player_id6),
                            visiting_setter_id = case_when(.data$visiting_setter_position == 1 ~ .data$visiting_player_id1,
                                                           .data$visiting_setter_position == 2 ~ .data$visiting_player_id2,
                                                           .data$visiting_setter_position == 3 ~ .data$visiting_player_id3,
                                                           .data$visiting_setter_position == 4 ~ .data$visiting_player_id4,
                                                           .data$visiting_setter_position == 5 ~ .data$visiting_player_id5,
                                                           .data$visiting_setter_position == 6 ~ .data$visiting_player_id6),
                            setter_id = case_when(.data$team_id == .data$home_team_id ~ .data$home_setter_id,
                                                  .data$team_id == .data$visiting_team_id ~ .data$visiting_setter_id))

            ## when a setter is replaced, check that the jersey number matches that of the player in home_setter_position or visiting_setter_position
            ## there is a complication in that there can be a *Pnn code (with things on that line being inconsistent) followed by *zN to the correct position
            ## and further complications if multiple subs are involved. So let's do the check at the next serve: everything should be correct by then
            ## Also need to cope with multiple setter replacement codes in a block (e.g. *P1 something *P2 serve, the *P1 is redundant but can be ignored)
            srvidx <- which(plays$skill == "Serve")
            chk <- bind_rows(lapply(c("\\*", "a"), function(tmcode) {
                rsidx <- which(grepl(paste0("^", tmcode, "P[[:digit:]]+"), plays$code) & !grepl(">LUp", plays$code, ignore.case = TRUE)) ## setter replacements but not lineup rows
                if (length(rsidx) > 0) {
                    chk <- bind_rows(lapply(rsidx, function(i) { ## for each setter replacement line
                        ## take the code from the setter replacement (rsidx) line and the remainder from the first serve after that
                        temp <- srvidx[srvidx > i]
                        if (length(temp) < 1 || (any(rsidx > i & rsidx < temp[1]))) {
                            ## no serve after this OR there is another setter replacement code in between this one and the next serve
                            NULL
                        } else {
                            plays[temp[1], ] %>% mutate(code = plays$code[i])
                        }
                    }))
                } else {
                    NULL
                }
            }))
            if (nrow(chk) > 0) {
                chk <- chk %>%
                    mutate(declared_setter_num = as.numeric(stringr::str_match(.data$code, "^[a\\*]P([[:digit:]]+)")[, 2]),
                           expected_setter_num = case_when(grepl("^a", .data$code) ~ case_when(.data$visiting_setter_position == 1 ~ .data$visiting_p1,
                                                                                               .data$visiting_setter_position == 2 ~ .data$visiting_p2,
                                                                                               .data$visiting_setter_position == 3 ~ .data$visiting_p3,
                                                                                               .data$visiting_setter_position == 4 ~ .data$visiting_p4,
                                                                                               .data$visiting_setter_position == 5 ~ .data$visiting_p5,
                                                                                               .data$visiting_setter_position == 6 ~ .data$visiting_p6),
                                                           TRUE ~ case_when(.data$home_setter_position == 1 ~ .data$home_p1,
                                                                            .data$home_setter_position == 2 ~ .data$home_p2,
                                                                            .data$home_setter_position == 3 ~ .data$home_p3,
                                                                            .data$home_setter_position == 4 ~ .data$home_p4,
                                                                            .data$home_setter_position == 5 ~ .data$home_p5,
                                                                            .data$home_setter_position == 6 ~ .data$home_p6))) %>%
                    dplyr::filter(.data$expected_setter_num != .data$declared_setter_num)
            }
            if (nrow(chk) > 0) {
                temp <- paste0("Replacement of ", ifelse(grepl("^a", chk$code), "visiting", "home"), " setter: the team is in rotation ",
                               ifelse(grepl("^a", chk$code), chk$visiting_setter_position, chk$home_setter_position),
                               " but the replacement setter is not in that position")
                out <- rbind(out, chk_df(chk, temp), severity = 3)
            }
            ## TODO perhaps check outgoing sub of the designated setter, is there a replacement setter code before the next serve?

            ## expect that any set made in reception phase on a perfect/good reception should be made by the designated setter
            chk <- plays %>% dplyr::filter(.data$skill == "Set", lag(.data$skill) == "Reception", .data$team == lag(.data$team), grepl("^(Perfect|Good|Positive)", lag(.data$evaluation)), (.data$player_id != .data$setter_id))
            if (nrow(chk) > 0) out <- rbind(out, chk_df(chk, "Set on perfect/good reception made by a player other than the designated setter (might indicate an error with the rotation/designated setter)", severity = 2))
            ## depending on the scout, we might not expect setter calls to be included on sets made by a player other than the designated setter
            chk <- plays %>% dplyr::filter(!is.na(.data$set_code), (.data$player_id != .data$setter_id))
            if (nrow(chk) > 0) out <- rbind(out, chk_df(chk, "Setter call on a set made by a player other than the designated setter (might indicate an error with the rotation/designated setter)", severity = 1))
            ## or on negative reception
            chk <- plays %>% dplyr::filter(!is.na(.data$set_code), lag(.data$skill == "Reception"), grepl("^(Poor|Negative)", lag(.data$evaluation)), .data$team == lag(.data$team))
            if (nrow(chk) > 0) out <- rbind(out, chk_df(chk, "Setter call on negative reception", severity = 1))
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

        ## look for aR -> *E -> aA or similar, suggesting that the set was assigned to the wrong team
        chk <- plays %>% dplyr::filter(.data$skill == "Set",
                                       ## preceded by reception or dig by the other team (but not D/ or R/)
                                       lag(.data$team) != .data$team, lag(.data$skill) %in% c("Reception", "Dig") & !lag(.data$evaluation) %in% c("Poor, no attack", "Ball directly back over net"),
                                       ## and followed by an attack by the other team
                                       lead(.data$skill) == "Attack", lead(.data$team) != .data$team)
        if (nrow(chk) > 0) {
            tm <- case_when(chk$team == chk$home_team ~ "by the home team",
                            chk$team == chk$visiting_team ~ "by the visiting team",
                            TRUE ~ "")
            out <- rbind(out, chk_df(chk, paste0("Set ", tm, " was in between a dig/reception and attack by the other team (was the set assigned to the correct team?)"), severity = 3))
        }

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
            tail(temp[which(temp$point_id == pid), c("point_id", "set_number", "home_team", "visiting_team", "home_team_score", "visiting_team_score", "point_won_by", "file_line_number", "serving_team")], 1)
        }))
        is_sideout_scoring <- grepl("sideout", x$meta$match$regulation)
        if (!is.null(temp) && nrow(temp) > 0) {
            temp$home_team_diff <- diff(c(0, temp$home_team_score))
            temp$visiting_team_diff <- diff(c(0, temp$visiting_team_score))
            temp$ok <- rep(TRUE, nrow(temp))
            idx <- temp$point_won_by %eq% temp$home_team
            if (is_sideout_scoring) idx <- idx & temp$serving_team %eq% temp$home_team
            ## expect point increment for home team on those rows
            temp$ok[idx] <- temp$home_team_diff[idx] == 1 & temp$visiting_team_diff[idx] == 0
            idx <- temp$point_won_by %eq% temp$visiting_team
            if (is_sideout_scoring) idx <- idx & temp$serving_team %eq% temp$visiting_team
            temp$ok[idx] <- temp$visiting_team_diff[idx] == 1 & temp$home_team_diff[idx] == 0
            ## these won't be valid for first point of each set other than first set
            for (ss in seq_len(max(temp$set_number, na.rm = TRUE))[-1]) {
                idx <- temp$set_number == ss
                if (any(idx)) {
                    idx <- min(which(idx)) ## first point of set ss
                    temp$ok[idx] <-
                        (temp$point_won_by[idx] %eq% temp$home_team[idx] && (is_sideout_scoring && temp$serving_team[idx] %eq% temp$home_team[idx] && temp$home_team_score[idx] == 1 && temp$visiting_team_score[idx] == 0)) ||
                        (temp$point_won_by[idx] %eq% temp$home_team[idx] && (is_sideout_scoring && !temp$serving_team[idx] %eq% temp$home_team[idx] && temp$home_team_score[idx] == 0 && temp$visiting_team_score[idx] == 0)) ||
                        (temp$point_won_by[idx] %eq% temp$home_team[idx] && (!is_sideout_scoring && temp$home_team_score[idx] == 1 && temp$visiting_team_score[idx] == 0)) ||

                        (temp$point_won_by[idx] %eq% temp$visiting_team[idx] && (is_sideout_scoring && temp$serving_team[idx] %eq% temp$visitingteam[idx] && temp$home_team_score[idx] == 0 && temp$visiting_team_score[idx] == 1)) ||
                        (temp$point_won_by[idx] %eq% temp$visiting_team[idx] && (is_sideout_scoring && !temp$serving_team[idx] %eq% temp$visiting_team[idx] && temp$home_team_score[idx] == 0 && temp$visiting_team_score[idx] == 0)) ||
                        (temp$point_won_by[idx] %eq% temp$visiting_team[idx] && (!is_sideout_scoring && temp$home_team_score[idx] == 0 && temp$visiting_team_score[idx] == 1))
                }
            }
            if (any(is.na(temp$point_won_by))) {
                ## should not see this
                ## just assume were ok
                temp$ok[is.na(temp$point_won_by)] <- TRUE
            }
            temp <- temp[!temp$ok, ]
            if (nrow(temp)>0) {
                for (chk in seq_len(nrow(temp))) {
                    out <- rbind(out,data.frame(file_line_number = temp$file_line_number[chk], video_time = video_time_from_raw(x$raw[temp$file_line_number[chk]]), message = "Point assigned to incorrect team or scores incorrect", file_line = mt2nachar(x$raw[temp$file_line_number[chk]]), severity = 3, stringsAsFactors = FALSE))
                }
            }
        }

        ## scores not in proper sequence
        ## a team's score should never increase by more than 1 at a time. May be negative (change of sets)
        temp <- plays[, c("home_team_score", "visiting_team_score", "end_of_set")]
        temp <- apply(temp, 2, diff)
        ## either score increases by more than one, or (decreases and not end of set)
        chk <- which((temp[, 1] > 1 | temp[, 2] > 1) | ((temp[, 1] < 0 | temp[, 2] < 0) & !(is.na(temp[, 3]) | temp[, 3] != 0)))
        if (length(chk) > 0) {
            chk <- chk + 1L
           out <- rbind(out, data.frame(file_line_number = plays$file_line_number[chk], video_time = video_time_from_raw(x$raw[plays$file_line_number[chk]]), message = "Scores do not follow proper sequence (note that the error may be in the point before this one)", file_line = mt2nachar(x$raw[plays$file_line_number[chk]]), severity = 3, stringsAsFactors = FALSE))
        }

        ## check for incorrect rotation changes
        rotleft <- function(x) if (is.data.frame(x)) { out <- cbind(x[, -1], x[, 1]); names(out) <- names(x); out} else c(x[-1], x[1])
        isrotleft <- function(x, y) all(as.numeric(rotleft(x)) == as.numeric(y)) ## is y a left-rotated version of x?
        plays_no_tt <- plays[!tolower(plays$skill) %eq% "technical timeout", ]
        out_of_block_idx <- which(plays_no_tt$skill %in% c("Serve", "Reception", "Set", "Attack", "Block", "Dig", "Freeball") | plays_no_tt$end_of_set | grepl(">LUp", plays_no_tt$code, ignore.case = TRUE))
        ## we will ignore any change in an end-of-rally code block that contains a substitution, these are checked in the next section
        eor_sub_idx <- unlist(lapply(which(plays_no_tt$substitution), function(k) {
            tryCatch(seq(tail(out_of_block_idx[out_of_block_idx < k], 1) + 1, head(out_of_block_idx[out_of_block_idx > k], 1) - 1), error = function(e) NULL)
        })) ## so these are the rows that are part of end-of-rally code blocks with subs
        for (tm in c("*", "a")) {
            rot_cols <- if (tm == "a") paste0("visiting_p", team_player_num) else paste0("home_p", team_player_num)
            rx <- plays_no_tt[, rot_cols]
            idx <- unname(c(FALSE, rowSums(abs(apply(rx, 2, diff))) > 0)) ## rows where rotation changed from previous
            idx[is.na(idx)] <- FALSE ## end of set, etc, ignore
            ## ignore any changes at the very start of the set, since lineups can be modified here multiple times fairly arbitrarily
            nsk <- rep(NA_integer_, nrow(plays_no_tt))
            skcount <- 0L
            for (ii in seq_along(nsk)) {
                if (isTRUE(plays_no_tt$end_of_set[ii])) skcount <- 0L ## reset
                if (!is.na(plays_no_tt$skill[ii])) skcount <- skcount + 1L
                nsk[ii] <- skcount
            }
            idx[nsk < 1L] <- FALSE
            idx <- setdiff(which(idx), eor_sub_idx) ## excluding entries that are in end-of-rally code blocks with subs
            for (k in idx) {
                ## if this wasn't a single leftwards rotation, flag it. Note that e.g. a double-sub or 6-2/4-2 setter switch will not be a rotation by 1 position, but will be valid - but these are handled in the substitution section below
                if (!isrotleft(rx[k - 1, ], rx[k, ])) ## rx[k, ] should be a left-rotated version of rx[k - 1, ]
                    out <- rbind(out, data.frame(file_line_number = plays_no_tt$file_line_number[k], video_time = video_time_from_raw(x$raw[plays_no_tt$file_line_number[k]]), message = paste0(if (tm == "a") "Visiting" else "Home"," team rotation has changed incorrectly"), file_line = mt2nachar(x$raw[plays_no_tt$file_line_number[k]]), severity = 3, stringsAsFactors = FALSE))
            }
        }

        ## check that players changed correctly on substitution
        ## e.g. *c02:01 means player 2 replaced by player 1
        ## this is much more unpleasant than it should be, because some files (looking at you, VS) don't record their substitutions correctly. They often change the players in the rotation columns on the line before the actual rotation code (e.g. if the preceding line is a setter position code). In a multiple-sub situation, all changes might be made before any actual sub codes are encountered. To be correct, the lineups should change on the same row as the corresponding sub code. BUT in order to reduce the number of warnings in the output, so long as the overall rotation ends up correct, we won't flag these as errors because they have little if any impact on analyses
        ## To cope with VS's issues the best way seems to be to check blocks of codes together: any end-of-rally codes that include one or more subs is treated as a block and we check the rotation before that block against the one just after that block
        sub_code <- grepl("^.c[[:digit:]]+:[[:digit:]]+$", plays_no_tt$code)
        idx <- which(plays_no_tt$substitution & sub_code)
        idx <- idx[idx > 2 & idx < (nrow(plays_no_tt) - 1)] ## discard anything at the start or end of the file
        setter_pos_code <- grepl("^[a\\*]z[[:digit:]]+$", plays_no_tt$code)
        rot_errors <- list()
        for (k in idx) {
            rot_cols <- if (grepl("^a",plays_no_tt$code[k])) paste0("visiting_p", team_player_num) else paste0("home_p", team_player_num)
            ## take the rotation on the first skill row after the sub row
            by_block <- TRUE; block_start_idx <- NA_integer_; block_end_idx <- NA_integer_
            block_end_idx <- head(out_of_block_idx[out_of_block_idx > k], 1) - 1L ## last row of code block before skill row
            new_rot <- plays_no_tt[block_end_idx + 1L, rot_cols]
            if (nrow(new_rot) < 1 || any(is.na(new_rot))) {
                ## that didn't work, fall back to the old method (which is likely to have false positives with VS files in particular)
                by_block <- FALSE
                new_rot <- plays_no_tt[k, rot_cols] ## the rotation on the sub line, which should reflect the new lineup
                if (any(is.na(new_rot))) new_rot <- plays_no_tt[k + 1, rot_cols]
            }
            if (any(is.na(new_rot))) next
            block_start_idx <- tail(out_of_block_idx[out_of_block_idx < k], 1) + 1L ## first row of code block
            prev_rot <- plays_no_tt[block_start_idx - 1L, rot_cols]
            if (nrow(prev_rot) < 1 || any(is.na(prev_rot))) {
                by_block <- FALSE
                prev_rot <- plays_no_tt[k - 1, rot_cols]
                if (any(is.na(prev_rot)) || (all(new_rot == prev_rot) && setter_pos_code[k - 1])) {
                    ## the second condition is for dvw files created by VS, which have a consistent error with subs as noted above. This method won't pick up all situations (multiple subs, or changes of setter are probably missed here, but that's why we use the block-based method if we can)
                    prev_rot <- plays_no_tt[k - 2, rot_cols]
                }
            }
            if (any(is.na(prev_rot))) next
            if (all(new_rot == prev_rot)) {
                ## players did not change
                rot_errors[[length(rot_errors) + 1L]] <- data.frame(file_line_number = plays_no_tt$file_line_number[k], video_time = video_time_from_raw(x$raw[plays_no_tt$file_line_number[k]]), message = paste0("Player lineup did not change after substitution: was the sub recorded incorrectly?"), file_line = mt2nachar(x$raw[plays_no_tt$file_line_number[k]]), severity = 3, stringsAsFactors = FALSE)
            } else if (!grepl("^.c[[:digit:]]+:[[:digit:]]+$", plays_no_tt$code[k])) {
                rot_errors[[length(rot_errors) + 1]] <- data.frame(file_line_number = plays_no_tt$file_line_number[k], video_time = video_time_from_raw(x$raw[plays_no_tt$file_line_number[k]]), message = paste0("The substitution code (", plays_no_tt$code[k], ") does not follow the expected format"), file_line = mt2nachar(x$raw[plays_no_tt$file_line_number[k]]), severity = 3, stringsAsFactors = FALSE)
            } else {
                if (by_block && isTRUE(block_start_idx <= block_end_idx)) {
                    ## figure out what we expect the rotation to be at the end of this code block
                    expected_rot <- prev_rot ## start with this
                    tm <- substr(plays_no_tt$code[k], 1, 1) ## the team of the sub row
                    for (cd in plays_no_tt$code[block_start_idx:block_end_idx]) {
                        if (grepl("^[a\\*]c[[:digit:]]+", cd) && isTRUE(substr(cd, 1, 1) == tm)) {
                            ## this is a sub code for the same team as row k
                            subtxt <- strsplit(sub("^.c", "", cd), ":")[[1]]
                            suppressWarnings(sub_out <- as.numeric(subtxt[1])) ## outgoing player
                            suppressWarnings(sub_in <- as.numeric(subtxt[2])) ## incoming player
                            if (!is.na(sub_out) && !is.na(sub_in)) expected_rot[expected_rot == sub_out] <- sub_in
                        }
                    }
                    ## at this point, new_rot should be identical to expected_rot or a once-rotated-left version of it
                    if (!(isTRUE(all(new_rot == expected_rot)) || isTRUE(isrotleft(expected_rot, new_rot)))) { ## new rot is same or left-rotated version of expected_rot
                        ## report the rotation error as the end of the block, so it only gets reported once per block
                        rot_errors[[length(rot_errors) + 1L]] <- data.frame(file_line_number = plays_no_tt$file_line_number[block_end_idx], video_time = video_time_from_raw(x$raw[plays_no_tt$file_line_number[block_end_idx]]), message = paste0(if (tm == "a") "Visiting" else "Home"," team rotation has changed incorrectly"), file_line = mt2nachar(x$raw[plays_no_tt$file_line_number[block_end_idx]]), severity = 3, stringsAsFactors = FALSE)
                    }
                } else {
                    subtxt <- strsplit(sub("^.c", "", plays_no_tt$code[k]), ":")[[1]]
                    suppressWarnings(sub_out <- as.numeric(subtxt[1])) ## outgoing player
                    suppressWarnings(sub_in <- as.numeric(subtxt[2])) ## incoming player
                    if (!sub_out %in% prev_rot || !sub_in %in% new_rot || sub_in %in% prev_rot || sub_out %in% new_rot) {
                        rot_errors[[length(rot_errors) + 1L]] <- data.frame(file_line_number=plays_no_tt$file_line_number[k],video_time=video_time_from_raw(x$raw[plays_no_tt$file_line_number[k]]),message=paste0("Player lineup conflicts with recorded substitution: was the sub recorded incorrectly?"),file_line=mt2nachar(x$raw[plays_no_tt$file_line_number[k]]),severity=3,stringsAsFactors=FALSE)
                    }
                }
            }
        }
        if (length(rot_errors) > 0) out <- rbind(out, unique(do.call(rbind, rot_errors)))
    } ## checking plays data
    out <- out[(4 - out$severity) <= validation_level, ]
    if (nrow(out) > 0) out <- dplyr::arrange(out, .data$file_line_number)
    out[, setdiff(names(out), "severity")]
}

#' @rdname dv_validate
#' @export
validate_dv <- dv_validate
