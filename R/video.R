## helper functions for syncing scout files with video

#' Synchronize video times
#'
#' Warning: experimental! This function uses the scouted clock time of each serve and some simple rules to align each scouted contact with its (approximately correct) time in the corresponding match video.
#'
#' When a match is scouted live, the clock time of each serve will usually be correct because the scout can enter the serve code at the actual time of serve. But the remainder of the touches in the rally might not be at the correct relative times, if the scout can't keep up with the live action. This function uses some simple rules to better synchronize the scouted contacts and corresponding match video.
#'
#' The \code{config} object contains a set of times (in seconds), which you can adjust to suit your scouting style and level of play. If you have an already-synchronized dvw file, the \code{\link{dv_sync_summary}} function can provide some guidance as to what these values should be. The \code{config} object contains the following entries:
#' \itemize{
#'   \item S_R - the time between serve and reception
#'   \item S_E - the time between serve and opposition first set (but see also \code{SQ} and \code{SM}, below)
#'   \item E_A - the time between set and attack (but see also \code{EQ} and \code{EH}, below)
#'   \item A_B - the time between attack and block
#'   \item A_A - the time between attacks in the rally (i.e. attack to counter-attack)
#'   \item D_E - the time between dig and set
#'   \item RDov - the time between reception or dig overpass and the next touch by the opposition
#'   \item END - the time between the last contact and end-of-rally marker
#'   \item SQ - S_E adjustment for jump serves (this value gets added to S_E for jump serves)
#'   \item SM - S_E adjustment for jump-float serves (this value gets added S_E for jump-float serves)
#'   \item EQ - E_A adjustment for quick sets (this value gets added to E_A for quick sets)
#'   \item EH - E_A adjustment for high sets (this value gets added to E_A for high sets)
#' }
#'
#' @param x datavolley: a single datavolley object as returned by \code{\link{dv_read}}
#' @param first_serve_time numeric or string: the time of the first serve in the video. This can be a numeric value giving the time in seconds, or a string of the form "MM:SS" (minutes and seconds) or "HH:MM:SS" (hours, minutes and seconds)
#' @param freeball_digs_accurate logical: if \code{TRUE}, treat the clock time of each freeball dig as being accurate (in the same way that serve times are treated as being accurate). Obviously this requires that the scout has been accurate in their timing when entering freeball digs, but assuming that is the case then setting \code{freeball_digs_accurate} to \code{TRUE} will improve the synchronization of rallies with freeballs (which otherwise tends to be poor, because the play is messy and less predictable compared to in-system rallies)
#' @param config list: a set of parameters that control the syncing process. See Details, below
#'
#' @return A modified copy of \code{x}
#' @seealso \code{\link{dv_sync_summary}}
#'
#' @examples
#' x <- dv_read(dv_example_file())
#' ## first serve was at 54s in the video
#' x <- dv_sync_video(x, first_serve_time = 54)
#'
#' ## with a custom configuration
#' my_config <- dv_sync_video_config()
#' my_config$S_A <- 4 ## change entries as necessary
#' ## first serve was at 3:35 in the video
#' x <- dv_sync_video(x, first_serve_time = "3:35", config = my_config)
#'
#' @export
dv_sync_video <- function(x, first_serve_time, freeball_digs_accurate = FALSE, config = dv_sync_video_config()) {
    if (is.string(first_serve_time)) {
        temp <- str_trim(strsplit(first_serve_time, ":")[[1]])
        temp <- suppressWarnings(as.numeric(temp))
        if (length(temp) > 3 || length(temp) < 1 || any(is.na(temp))) stop("first_serve_time is in an unexpected format")
        first_serve_time <- sum(c(rep(0, 3 - length(temp)), temp) * c(3600, 60, 1))
    }
    if (is.na(first_serve_time) || !is.numeric(first_serve_time)) stop("first_serve_time is not numeric")
    if (first_serve_time < 0) stop("first_serve_time cannot be negative")

    px <- plays(x)
    if (all(is.na(px$time))) stop("px does not have any `time` column entries")

    ## equality with NAs considered false
    `%eq%` <- function(x,y) x == y & !is.na(x) & !is.na(y)
    `%neq%` <- function(x,y) x != y & !is.na(x) & !is.na(y)

    vt <- rep(NA_integer_, nrow(px)) ## new video times, to be populated

    config$S_A <- config$S_E + config$E_A
    config$A_D <- config$A_A - config$D_E - config$E_A
    if (config$A_D < 0) warning("config possibly inconsistent: time between attack and dig is ", config$A_D)

    ## "Freeball" skill can be used both for sending a freeball to the opposition as well as receiving one, so disambiguate these usages
    px <- mutate(px, .FREEBALL_OVER = .data$skill %eq% "Freeball" &
                         lag(.data$match_id) %eq% .data$match_id & lag(.data$point_id) %eq% .data$point_id &
                         ((!is.na(lead(.data$team)) & !is.na(lead(.data$skill)) & lead(.data$team) != .data$team) | lag(.data$team) %eq% .data$team))

    ## pre-locate a bunch of things so we don't need to repeat these operations multiple times
    skill_idx <- !is.na(px$skill) & !grepl("timeout|rotation", px$skill, ignore.case = TRUE)

    s_idx <- px$skill %eq% "Serve"
    ## clock times within-rally are assumed not to be reliable, except for serves and (optionally) freeball digs
    if (any(diff(px$time[s_idx]) < 0, na.rm = TRUE)) warning("at least one serve has a `time` entry that is out of order (`time` goes backwards from one serve to the next), resync results might be poor")

    sq_idx <- px$skill %eq% "Jump serve"
    sm_idx <- px$skill %eq% "Jump-float serve"

    r_idx <- px$skill %eq% "Reception" & lag(s_idx)

    e_idx <- px$skill %eq% "Set"
    f_idx <- px$skill %eq% "Freeball"
    fo_idx <- f_idx & px$.FREEBALL_OVER
    fd_idx <- f_idx & !px$.FREEBALL_OVER
    d_idx <- px$skill %eq% "Dig" | fd_idx
    b_idx <- px$skill %eq% "Block"

    if (freeball_digs_accurate) {
        if (any(diff(px$time[s_idx | fd_idx]) < 0, na.rm = TRUE)) warning("at least one serve/freeball dig has a `time` entry that is out of order (`time` goes backwards from one action to the next), resync results might be poor")
    }

    a_idx <- px$skill %eq% "Attack"
    aq_idx <- a_idx & grepl("Quick|Slide", px$skill_type)
    ah_idx <- px$skill_type %eq% "High ball attack"

    if (sum(s_idx) < 1) stop("could not find any serves in px")
    ## difference of all serve clock times to the first serve clock time
    clock_time_diff <- difftime(px$time[s_idx], px$time[which(s_idx)[1]], units = "secs")
    ## align each serve time to this
    vt[s_idx] <- first_serve_time + clock_time_diff
    if (freeball_digs_accurate) {
        ## do the same with freeball digs
        clock_time_diff <- difftime(px$time[fd_idx], px$time[which(s_idx)[1]], units = "secs")
        vt[fd_idx] <- first_serve_time + clock_time_diff
    }

    ## serve - reception
    vt[which(r_idx)] <- vt[which(r_idx) - 1] + config$S_R

    ## first attack or reception-phase freeball over
    ## serve followed by opp attack 2 contacts later (no set scouted, but assumed that there actually was one)
    ## TODO allow for P2 attacks
    a1_idx <- lag(s_idx, 2) & lag(r_idx) & (a_idx | fo_idx) & lag(px$team) %eq% px$team
    vt[which(a1_idx)] <- vt[which(a1_idx) - 2] + config$S_A
    ## adjust for serve type Q and M
    vt[which(a1_idx & lag(sq_idx, 2))] <- vt[which(a1_idx & lag(sq_idx, 2))] + config$SQ
    vt[which(a1_idx & lag(sm_idx, 2))] <- vt[which(a1_idx & lag(sm_idx, 2))] + config$SM
    ## adjust for attack types
    vt[which(a1_idx & aq_idx)] <- vt[which(a1_idx & aq_idx)] + config$EQ
    vt[which(a1_idx & ah_idx)] <- vt[which(a1_idx & ah_idx)] + config$EH

    ## first attack with set scouted
    a1_idx <- lag(s_idx, 3) & lag(r_idx, 2) & lag(e_idx) & (a_idx | fo_idx) & lag(px$team, 2) %eq% px$team & lag(px$team) %eq% px$team
    vt[which(a1_idx)] <- vt[which(a1_idx) - 3] + config$S_A
    ## adjust for serve type Q and M
    vt[which(a1_idx & lag(sq_idx, 3))] <- vt[which(a1_idx & lag(sq_idx, 3))] + config$SQ
    vt[which(a1_idx & lag(sm_idx, 3))] <- vt[which(a1_idx & lag(sm_idx, 3))] + config$SM
    ## adjust for attack types
    vt[which(a1_idx & aq_idx)] <- vt[which(a1_idx & aq_idx)] + config$EQ
    vt[which(a1_idx & ah_idx)] <- vt[which(a1_idx & ah_idx)] + config$EH

    ## next touch after overpass on reception
    ov_idx <- is.na(vt) & lag(s_idx, 2) & lag(r_idx) & lag(px$evaluation_code %eq% "/") & skill_idx & lag(px$team) %neq% px$team
    vt[which(ov_idx)] <- vt[which(ov_idx) - 2] + config$S_R + config$RDov
    ## adjust for serve type
    vt[which(ov_idx & lag(sq_idx, 2))] <- vt[which(ov_idx & lag(sq_idx, 2))] + config$SQ
    vt[which(ov_idx & lag(sm_idx, 2))] <- vt[which(ov_idx & lag(sm_idx, 2))] + config$SM
    ## next touch after overpass on dig
    ov_idx <- is.na(vt) & lag(d_idx) & lag(px$evaluation_code %eq% "/") & skill_idx & lag(px$team) %neq% px$team
    vt[which(ov_idx)] <- vt[which(ov_idx) - 1] + config$RDov

    ## set after dig or reception
    this <- is.na(vt) & e_idx & lag(d_idx | r_idx) & px$team %eq% lag(px$team)
    vt[which(this)] <- vt[which(this) - 1] + config$D_E

    ## attack after set, necessary in case there was no reception-phase attack but a first transition dig/freeball dig was scouted
    this <- is.na(vt) & a_idx & lag(e_idx) & px$team %eq% lag(px$team)
    vt[which(this)] <- vt[which(this) - 1] + config$E_A
    vt[which(this & aq_idx)] <- vt[which(this & aq_idx)] + config$EQ
    vt[which(this & ah_idx)] <- vt[which(this & ah_idx)] + config$EH

    ## attack to counter-attack by opposition
    last_attack_time <- NA_real_
    last_attack_team <- NA_character_
    last_point_id <- -99L
    last_team_touch_id <- -99L
    for (i in which(a_idx)) {
        if (isTRUE(is.na(vt[i]) && !is.na(last_attack_time) &&
                   last_point_id == px$point_id[i] && (last_team_touch_id == px$team_touch_id[i] - 1) && last_attack_team != px$team[i])) {
            vt[i] <- last_attack_time + config$A_A
        }
        last_attack_time <- vt[i]
        last_attack_team <- px$team[i]
        last_point_id <- px$point_id[i]
        last_team_touch_id <- px$teamtouch_id[i]
    }

    ## we have to run the transition sync process multiple times, because each time through it will add some video times to some rows and then
    ##  that will allow some other rows to be populated on the next pass

    trans_sync <- function(vt) {
        ## set preceding attack
        this <- is.na(vt) & e_idx & lead(a_idx) & px$team %eq% lead(px$team)
        vt[which(this)] <- vt[which(this) + 1] - config$E_A
        vt[which(this & lead(aq_idx))] <- vt[which(this & lead(aq_idx))] - config$EQ
        vt[which(this & lead(ah_idx))] <- vt[which(this & lead(ah_idx))] - config$EH

        ## digs
        this <- which(is.na(vt) & d_idx & lag(a_idx))
        vt[this] <- vt[this - 1] + config$A_D
        ## or with block touch
        this <- which(is.na(vt) & d_idx & lag(b_idx) & lag(a_idx, 2))
        vt[this] <- vt[this - 2] + config$A_D

        ## other digs following an opposition touch, e.g. freeball digs not already picked up
        this <- which(is.na(vt) & d_idx & lag(px$team) %neq% px$team)
        vt[this] <- vt[this - 1] + config$A_D

        ## set after dig (maybe not preceding attack, e.g. preceding a freeball over)
        this <- which(is.na(vt) & e_idx & lag(d_idx) & px$team %eq% lag(px$team))
        vt[this] <- vt[this - 1] + config$D_E

        ## dig (inc freeball dig) preceding set
        this <- which(is.na(vt) & d_idx & lead(e_idx))
        vt[this] <- vt[this + 1] - config$D_E

        ## attack following set
        this <- is.na(vt) & a_idx & lag(e_idx)
        vt[which(this)] <- vt[which(this) - 1] + config$E_A
        vt[which(this & aq_idx)] <- vt[which(this & aq_idx)] - config$EQ
        vt[which(this & ah_idx)] <- vt[which(this & ah_idx)] - config$EH

        ## attack following own team touch that wasn't scouted as a set (e.g. P2)
        this <- which(is.na(vt) & a_idx & px$team %eq% lag(px$team))
        vt[this] <- vt[this - 1] + config$E_A

        ## blocks
        this <- which(is.na(vt) & b_idx & lag(a_idx))
        vt[this] <- vt[this - 1] + config$A_B
        vt
    }
    for (loop in 1:10) vt <- trans_sync(vt)

    ## at this point we should have most of the ball contacts synced. Anything left over should be things that don't fit our expected patterns above
    ## so just do our best to patch them up

    ## any un-synced touch following an opposition touch
    this <- which(is.na(vt) & px$team %neq% lag(px$team) & px$point_id %eq% lag(px$point_id))
    vt[this] <- vt[this - 1] + config$A_D
    for (loop in 1:10) vt <- trans_sync(vt)

    ## interpolate all remaining skill times
    s_synced <- !is.na(vt) & skill_idx
    s_unsynced <- is.na(vt) & skill_idx
    for (pid in na.omit(unique(px$point_id[which(s_unsynced)]))) {
        ridx <- px$point_id == pid
        ridx_synced <- which(ridx & s_synced)
        vt[which(ridx)] <- approx(ridx_synced, vt[ridx_synced], which(ridx))$y
    }

    ## end of rally
    is_point <- grepl("^[a\\*]p", px$code)
    is_green_code <- grepl("^[a\\*]\\$\\$&H[#=]", px$code) ## green codes [a*]$$&H[#=]
    last_point_id <- -1
    last_skill_vt <- NA_integer_
    for (i in seq_len(nrow(px))) {
        if (px$point_id[i] %eq% last_point_id && !is.na(last_skill_vt) && is.na(vt[i])) {
            if (is_green_code[i]) vt[i] <- last_skill_vt
            if (is_point[i]) vt[i] <- last_skill_vt + config$END
        }
        if (isTRUE(skill_idx[i])) last_skill_vt <- vt[i]
        last_point_id <- px$point_id[i]
    }

    ## timeouts
    to_idx <- grepl("timeout", px$skill, ignore.case = TRUE)
    this <- to_idx & is.na(vt)
    vt[which(this)] <- vt[which(this) - 1]

    ## non-touch rows
    non_skill <- is.na(px$skill) | to_idx
    this <- which(is.na(vt) & non_skill)
    for (i in setdiff(this, 1)) vt[i] <- vt[i - 1]

    ## do we need a final pass to fill anything left over?

    px$video_time <- round(vt) ## video times have to be integer
    plays(x) <- px[, setdiff(names(px), c(".FREEBALL_OVER"))]
    x
}

#' @rdname dv_sync_video
#' @export
dv_sync_video_config <- function() list(S_R = 0, ## time between serve and reception
                                     S_E = 3, ## serve to set
                                     E_A = 1, ## set to attack
                                     A_B = 0, ## attack to block
                                     A_A = 3.5, ## attack to counter-attack
                                     D_E = 2, ## dig to set
                                     RDov = 3, ## reception or dig overpass to next touch by opposition
                                     END = 3, ## last action to end-of-rally marker
                                     SQ = 2, ## S_E adjustment for jump serves (add this to S_E for jump serves)
                                     SM = 1, ## S_E adjustment for jump-float serves (add this S_E for jump-float serves)
                                     EQ = -0.5, ## E_A adjustment for quick set (add this to E_A for quick sets)
                                     EH = 1) ## E_A adjustment for high set (add this to E_A for high sets)



#' Summarize the video sync times in a dvw file
#'
#' This function will generate a summary of various video time differences in a dvw file. Apply this to a file that you have synchronized to video, and the results can be used to tweak the behaviour of \code{\link{dv_sync_video}}.
#'
#' @param x datavolley: a single datavolley object as returned by \code{\link{dv_read}}, or the \code{plays} component of one
#'
#' @return A data.frame with columns \code{type}, \code{N}, \code{mean}, \code{most_common}, \code{min}, \code{max}
#'
#' @seealso \code{\link{dv_sync_video}}
#'
#' @examples
#' x <- dv_read(dv_example_file(3))
#' dv_sync_summary(x)
#'
#' @export
dv_sync_summary <- function(x) {
    if (inherits(x, "datavolley") || (is.list(x) && all(c("plays", "meta") %in% names(x)))) x <- plays(x)
    tsm <- function(label, z) {
        z <- z[!is.na(z)]
        if (length(z) < 1) {
            data.frame(type = label, N = 0L, mean = NA_real_, most_common = NA_real_, min = NA_real_, max = NA_real_)
        } else {
            data.frame(type = label, N = length(z), mean = mean(z), most_common = most_common_value(z), min = min(z), max = max(z))
        }
    }

    x <- mutate(x, t_lead_1 = lead(.data$video_time, 1), t_lead_2 = lead(.data$video_time, 2), t_lead_3 = lead(.data$video_time, 3),
                 t_lag_1 = lag(.data$video_time, 1), t_lag_2 = lag(.data$video_time, 2), t_lag_3 = lag(.data$video_time, 3),
                 is_touch = !is.na(.data$skill) & !grepl("timeout|rotation", .data$skill, ignore.case = TRUE),
                 .FREEBALL_OVER = .data$skill %eq% "Freeball" &
                     lag(.data$match_id) %eq% .data$match_id & lag(.data$point_id) %eq% .data$point_id &
                     ((!is.na(lead(.data$team)) & !is.na(lead(.data$skill)) & lead(.data$team) != .data$team) | lag(.data$team) %eq% .data$team))
    ## time between serve and reception
    S_R <- x %>% dplyr::filter(.data$skill == "Serve", lead(.data$skill) == "Reception") %>% dplyr::reframe(tsm("Serve to receive time", .data$t_lead_1 - .data$video_time))
    S_E <- x %>% dplyr::filter(.data$skill == "Serve", lead(.data$skill) == "Reception", lead(.data$skill, 2) == "Set", lead(.data$team, 2) != .data$team) %>%
        mutate(skill_type = case_when(.data$skill_type %in% c("Jump serve", "Jump-float serve") ~ .data$skill_type,
                                      TRUE ~ "Other serve types")) %>%
        group_by(.data$skill_type) %>% dplyr::reframe(tsm(paste0("Serve to set time (", .data$skill_type[1], ")"), .data$t_lead_2 - .data$video_time))

    E_A <- x %>% dplyr::filter(.data$skill == "Set", lead(.data$skill) == "Attack", lead(.data$team) == .data$team) %>%
        mutate(skill_type = case_when(grepl("^Quick|High", .data$skill_type) ~ .data$skill_type,
                                      TRUE ~ "Other set types")) %>%
        group_by(.data$skill_type) %>% dplyr::reframe(tsm(paste0("Set to attack time (", .data$skill_type[1], ")"), .data$t_lead_1 - .data$video_time))

    A_B <- x %>% dplyr::filter(.data$skill == "Attack", lead(.data$skill) == "Block") %>% dplyr::reframe(tsm("Attack to block time", .data$t_lead_1 - .data$video_time))

    D_E <- x %>% dplyr::filter(.data$skill == "Dig", lead(.data$skill) == "Set", lead(.data$team) == .data$team) %>%
        dplyr::reframe(tsm("Dig to set time", .data$t_lead_1 - .data$video_time))

    A_A <- x %>% dplyr::filter(.data$skill == "Attack") %>% group_by(.data$match_id, .data$point_id) %>%
        mutate(A_A_time = if_else(.data$team != lag(.data$team) & (.data$team_touch_id == lag(.data$team_touch_id) + 1), .data$video_time - lag(.data$video_time), NA_real_)) %>%
        ungroup %>% dplyr::filter(!is.na(.data$A_A_time)) %>% dplyr::reframe(tsm("Attack to counter-attack time", .data$A_A_time))

    RDov <- x %>% dplyr::filter(.data$skill %in% c("Dig", "Reception"), .data$evaluation_code == "/", lead(.data$team) != .data$team) %>%
        dplyr::reframe(tsm("Overpass to next touch time", .data$t_lead_1 - .data$video_time))

    END <- x %>% group_by(.data$point_id) %>% dplyr::summarize(was_rally = any(.data$is_touch), last_touch_time = qmax(.data$video_time[.data$is_touch]),
                                                                end_rally_time = qmin(.data$video_time[grepl("^a\\*[pP]", .data$code)]),
                                                                end_rally_time = case_when(is.na(.data$end_rally_time) | is.infinite(.data$end_rally_time) ~ qmax(.data$video_time),
                                                                                           TRUE ~ .data$end_rally_time),
                                                                END = .data$end_rally_time - .data$last_touch_time) %>% ungroup %>%
                                                      dplyr::filter(!is.infinite(.data$END)) %>%
                                                      dplyr::reframe(tsm("Last touch to end of rally", .data$END))
    out <- bind_rows(S_R, S_E, E_A, A_B, D_E, A_A, RDov, END) %>% dplyr::select(-"skill_type")
    ##     for (i in seq_len(nrow(out))) {
    ##         if (i > 1 && sub("\\(.*", "", out$type[i]) != sub("\\(.*", "", out$type[i - 1])) cat("\n")
    ##         if (out$N[i] < 1) {
    ##             cat(out$type[i], " - no data\n", sep = "")
    ##         } else {
    ##             cat(out$type[i], " - N: ", out$N[i], ", mean: ", out$mean[i], ", most common value: ", out$mode[i], ", range: ", out$min[i], " to ", out$max[i], "\n", sep = "")
    ##         }
    ##     }
    out
}
