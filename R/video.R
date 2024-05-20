## helper functions for synchronization scout files with video

#' Synchronize video times
#'
#' This function uses the time of each serve and some rules to align the other contacts in a rally with their (approximately correct) times in the corresponding match video. Warning: experimental!
#'
#' When a match is scouted live, the clock time of each serve will usually be correct because the scout can enter the serve code at the actual time of serve. But the remainder of the touches in the rally might not be at their correct times if the scout can't keep up with the live action. This function makes some assumptions about typical contact-to-contact times to better synchronize the scouted contacts with the corresponding match video.
#'
#' The clock time of each serve will be used as the reference time for each rally (unless the user specifies \code{times_from = "video"}). If clock times are not present in the file, the video time of each serve will be used instead. If those are also missing, the function will fail.
#'
#' Freeball digs can optionally be treated in the same way as serves, with their scouted times used directly in the synchronization process. Obviously this only makes sense if the scout has actually been consistent in their timing when entering freeball digs, but assuming that is the case then setting the \code{freeball_dig_time_offset} to a non-NA value will improve the synchronization of rallies with freeballs. These rallies otherwise tend to synchronize poorly, because the play is messy and less predictable compared to in-system rallies.
#'
#' Note that synchronization from clock times relies on the serve clock times in the file being consistent, and so it will only work if the match has been scouted in a single sitting (either live, or from video playback but without pausing/rewinding/fast-forwarding the video). If your clock times are not consistent but the video time of each serve is correct, then you can use the video time of each serve as the reference time instead.
#'
#' The synchronization is a two-step process. In the first step, the video time of each scouted contact is estimated (i.e. the actual time that the player made contact with the ball). In the second step, skill-specific offsets are added to those contact times. (This is important if your video montage software uses the synchronized video times directly, because you will normally want a video clip to start some seconds before the actual contact of interest).
#'
#' The \code{contact_times} object contains a set of times (in seconds), which you can adjust to suit your scouting style and level of play. If you have an already-synchronized dvw file, the \code{\link{dv_sync_summary}} function can provide some guidance as to what these values should be. The \code{contact_times} object contains the following entries:
#' \itemize{
#'   \item SQ - time between the scouted serve time and actual serve contact for jump serves
#'   \item SM - time between the scouted serve time and actual serve contact for jump-float serves
#'   \item SO - time between the scouted serve time and actual serve contact for all other serves
#'   \item SQ_R, SM_R, SO_R - the time between serve contact and reception contact for jump, jump-float, and other serves
#'   \item R_E - the time between reception contact and set contact
#'   \item EQ_A - the time between set contact and attack contact for quick sets
#'   \item EH_A - the time between set contact and attack contact for high sets
#'   \item EO_A - the time between set contact and attack contact for all other sets
#'   \item A_B - the time between attack contact and block contact
#'   \item A_D - the time between attack contact and dig contact (no intervening block touch)
#'   \item A_B_D - the time between attack contact and dig contact (with block touch)
#'   \item D_E - the time between dig contact and set contact
#'   \item RDov - the time between reception or dig overpass contact and the next touch by the opposition
#'   \item END - the time between the last contact and end-of-rally marker
#' }
#'
#' The \code{offsets} object defines the offset (in seconds) to be added to each contact time in the second pass of the synchronization process. It contains the entries "S" (serve), "R" (reception), "E" (set), "A" (attack), "D", (dig), "B" (block), and "F" (freeball).
#'
#' Note that the entries in \code{contact_times} and \code{offsets} can be fractions. The actual video time entries in the returned file are required to be integers and so the final values will be rounded, but using fractional values (particularly for the \code{contact_times} entries) can give better accuracy in the intermediate calculations.
#'
#' @param x datavolley: a single datavolley object as returned by \code{\link{dv_read}}
#' @param first_serve_contact numeric or string: the video time of the first serve contact. This can be a numeric value giving the time in seconds from the start of the video, or a string of the form "MM:SS" (minutes and seconds) or "HH:MM:SS" (hours, minutes and seconds)
#' @param freeball_dig_time_offset numeric: if non-NA, the clock times of freeball digs will be used directly in the synchronization process. Freeball digs will be aligned using their clock times relative to the first serve contact clock time, with this \code{freeball_dig_time_offset} value (in seconds) added. So if when scouting live you typically enter freeball digs one second after they happen, use \code{freeball_dig_time_offset = -1}. If \code{freeball_dig_time_offset} is NA, which is the default, the clock times of freeball digs will not be used in the synchronization process
#' @param contact_times list: a set of parameters that control the synchronization process. See Details, below
#' @param offsets list: a list set of offsets to be added to each contact time in the second step of the synchronization process. See Details, below. If \code{offsets} is NULL or an empty list, no offsets are applied
#' @param times_from string: either "clock" or "video": take the serve times (and freeball dig times, if \code{freeball_dig_time_offset} is non-NA) from clock or video times. By default, clock times are used unless they are all missing
#' @param enforce_order logical: the estimated contact times will always be time-ordered (the contact time of a given touch cannot be prior to the contact time of a preceding touch). But the offsets can be different for different skills, leading to final video times that are not time ordered. These will be fixed if \code{enforce_order} is TRUE
#' @param ... : name-value pairs of elements to override the defaults in \code{dv_sync_contact_times} and \code{dv_sync_offsets}
#'
#' @return A copy of \code{x} with modified \code{video_time} values in its \code{plays} component
#' @seealso \code{\link{dv_sync_summary}}
#'
#' @examples
#' x <- dv_read(dv_example_file())
#' ## first serve contact was at 54s in the video
#' x <- dv_sync_video(x, first_serve_contact = 54)
#'
#' ## with a custom configuration
#' my_contact_times <- dv_sync_contact_times(SQ = 3) ## override default entries as necessary
#' ## first serve contact was at 3:35 in the video
#' x <- dv_sync_video(x, first_serve_contact = "3:35", contact_times = my_contact_times)
#'
#' @export
dv_sync_video <- function(x, first_serve_contact, freeball_dig_time_offset = NA, contact_times = dv_sync_contact_times(), offsets = dv_sync_offsets(), times_from, enforce_order = TRUE) {
    if (is.string(first_serve_contact)) {
        temp <- str_trim(strsplit(first_serve_contact, ":")[[1]])
        temp <- suppressWarnings(as.numeric(temp))
        if (length(temp) > 3 || length(temp) < 1 || any(is.na(temp))) stop("first_serve_contact is in an unexpected format")
        first_serve_contact <- sum(c(rep(0, 3 - length(temp)), temp) * c(3600, 60, 1))
    }
    if (is.na(first_serve_contact) || !is.numeric(first_serve_contact)) stop("first_serve_contact is not numeric")
    if (first_serve_contact < 0) stop("first_serve_contact cannot be negative")

    px <- plays(x)

    s_idx <- px$skill %eq% "Serve"

    if (missing(times_from) || is.null(times_from) || is.na(times_from)) {
        ## figure out whether to use clock times (preferred) or video times as reference times
        time_col <- "time" ## use clock times as reference
        chk <- px$time[s_idx]
        if (all(is.na(chk))) {
            time_col <- "video_time"
            chk <- px$video_time[s_idx]
        }
        if (all(is.na(chk))) {
            stop("the plays component of `x` is missing all of the `time` (clock time) and `video_time` entries on serves")
        }
        ## this is likely to lead to confusing behaviour with files that have partially-missing clock and video times
        ## if (any(is.na(chk))) {
        ##     ## choose whichever has the least missing?
        ##     time_col <- if (sum(is.na(px$time[s_idx])) <= sum(is.na(px$video_time[s_idx]))) "time" else "video_time"
        ## }
        if (time_col == "video_time") warning("using serve video time (not clock time) as the reference time for each rally")
    } else {
        times_from <- tolower(times_from)
        time_col <- if (match.arg(times_from, c("clock", "video")) == "clock") "time" else "video_time"
    }

    ref_times <- px[[time_col]]
    if (any(is.na(ref_times))) {
        warning("the plays component of `x` is missing at least one `", time_col, "` entry on serves")
    }

    if (inherits(ref_times, "POSIXt")) {
        ## were clock times
        ref_times <- as.numeric(ref_times)
    }

    contact_times$A_A <- contact_times$A_D + contact_times$D_E + contact_times$EO_A

    vt <- rep(NA_integer_, nrow(px)) ## new video times, to be populated

    ## "Freeball" skill can be used both for sending a freeball to the opposition as well as receiving one, so disambiguate these usages
    px <- mutate(px, .FREEBALL_OVER = .data$skill %eq% "Freeball" &
                         lag(.data$match_id) %eq% .data$match_id & lag(.data$point_id) %eq% .data$point_id &
                         ((!is.na(lead(.data$team)) & !is.na(lead(.data$skill)) & lead(.data$team) != .data$team) | lag(.data$team) %eq% .data$team))

    ## pre-locate a bunch of things so we don't need to repeat these operations multiple times
    skill_idx <- !is.na(px$skill) & !grepl("timeout|rotation", px$skill, ignore.case = TRUE)

    ## clock times within-rally are assumed not to be reliable, except for serves and (optionally) freeball digs
    if (any(diff(ref_times[s_idx]) < 0, na.rm = TRUE)) warning("at least one serve has a `time` entry that is out of order (`time` goes backwards from one serve to the next), resync results might be poor")

    sq_idx <- px$skill_type %eq% "Jump serve"
    sm_idx <- px$skill_type %eq% "Jump-float serve"

    r_idx <- px$skill %eq% "Reception" & lag(s_idx)

    e_idx <- px$skill %eq% "Set"
    f_idx <- px$skill %eq% "Freeball"
    fo_idx <- f_idx & px$.FREEBALL_OVER
    fd_idx <- f_idx & !px$.FREEBALL_OVER
    d_idx <- px$skill %eq% "Dig" | fd_idx
    b_idx <- px$skill %eq% "Block"

    if (!is.na(freeball_dig_time_offset)) {
        if (any(diff(ref_times[s_idx | fd_idx]) < 0, na.rm = TRUE)) warning("at least one serve/freeball dig has a `time` entry that is out of order (`time` goes backwards from one action to the next), resync results might be poor")
    }

    a_idx <- px$skill %eq% "Attack"
    aq_idx <- a_idx & grepl("Quick|Slide", px$skill_type)
    ah_idx <- px$skill_type %eq% "High ball attack"

    if (sum(s_idx) < 1) stop("could not find any serves in px")
    ## difference of all serve clock times to the first serve clock time
    clock_time_diff <- ref_times[s_idx] - ref_times[which(s_idx)[1]]
    ## align each scouted serve time to this
    vt[s_idx] <- first_serve_contact + clock_time_diff
    ## then adjust to serve contact time according to serve type
    vt[sq_idx] <- vt[sq_idx] + contact_times$SQ
    vt[sm_idx] <- vt[sm_idx] + contact_times$SM
    vt[s_idx & !sq_idx & !sm_idx] <- vt[s_idx & !sq_idx & !sm_idx] + contact_times$SO
    if (!is.na(freeball_dig_time_offset)) {
        ## align freeball digs by clock time relative to serve contact time
        clock_time_diff <- ref_times[fd_idx] - ref_times[which(s_idx)[1]] ## time difference relative to first serve contact clock time
        vt[fd_idx] <- first_serve_contact + clock_time_diff + freeball_dig_time_offset
    }

    ## serve - reception
    vt[which(r_idx & lag(sq_idx))] <- vt[which(r_idx & lag(sq_idx)) - 1] + contact_times$SQ_R
    vt[which(r_idx & lag(sm_idx))] <- vt[which(r_idx & lag(sm_idx)) - 1] + contact_times$SM_R
    vt[which(r_idx & !lag(sq_idx) & !lag(sm_idx))] <- vt[which(r_idx & !lag(sq_idx) & !lag(sm_idx)) - 1] + contact_times$SO_R

    ## reception-phase set
    e1_idx <- e_idx & lag(r_idx) & lag(px$team) == px$team
    vt[which(e1_idx)] <- vt[which(e1_idx) - 1L] + contact_times$R_E

    ## reception-phase attack
    ## with set scouted
    a1_idx <- lag(r_idx, 2) & lag(e_idx) & (a_idx | fo_idx) & lag(px$team, 2) == px$team & lag(px$team) == px$team
    ## adjust for set types
    vt[which(a1_idx & aq_idx)] <- vt[which(a1_idx & aq_idx) - 1L] + contact_times$EQ_A
    vt[which(a1_idx & ah_idx)] <- vt[which(a1_idx & ah_idx) - 1L] + contact_times$EH_A
    vt[which(a1_idx & !aq_idx & !ah_idx)] <- vt[which(a1_idx & !aq_idx & !ah_idx) - 1L] + contact_times$EO_A
    ## without set scouted. This assumes that there was a set, it just wasn't scouted
    ## TODO allow for P2 attacks
    a1_idx <- lag(r_idx, 1) & (a_idx | fo_idx) & lag(px$team) == px$team
    vt[which(a1_idx & aq_idx)] <- vt[which(a1_idx & aq_idx) - 1L] + contact_times$EQ_A + contact_times$D_E
    vt[which(a1_idx & ah_idx)] <- vt[which(a1_idx & ah_idx) - 1L] + contact_times$EH_A + contact_times$D_E
    vt[which(a1_idx & !aq_idx & !ah_idx)] <- vt[which(a1_idx & !aq_idx & !ah_idx) - 1L] + contact_times$EO_A + contact_times$D_E

    ## next touch after overpass on reception
    ov_idx <- is.na(vt) & lag(r_idx) & lag(px$evaluation_code %eq% "/") & skill_idx & lag(px$team) != px$team ## optionally remove the "/" condition?
    vt[which(ov_idx)] <- vt[which(ov_idx) - 1] + contact_times$RDov

    ## blocks
    this <- which(is.na(vt) & b_idx & lag(a_idx))
    vt[this] <- vt[this - 1] + contact_times$A_B

    ## attack after set, necessary in case there was no reception-phase attack but a first transition dig/freeball dig was scouted
    this <- is.na(vt) & a_idx & lag(e_idx) & px$team %eq% lag(px$team)
    vt[which(this & aq_idx)] <- vt[which(this & aq_idx) - 1L] + contact_times$EQ_A
    vt[which(this & ah_idx)] <- vt[which(this & ah_idx) - 1L] + contact_times$EH_A
    vt[which(this & !aq_idx & !ah_idx)] <- vt[which(this & !aq_idx & !ah_idx) - 1L] + contact_times$EO_A

    trans_sync <- function(vt) {
        ## set preceding attack
        this <- is.na(vt) & e_idx & lead(a_idx) & px$team == lead(px$team)
        vt[which(this & lead(aq_idx))] <- vt[which(this & lead(aq_idx))] - contact_times$EQ_A
        vt[which(this & lead(ah_idx))] <- vt[which(this & lead(ah_idx))] - contact_times$EH_A
        vt[which(this & !lead(aq_idx) & !lead(ah_idx))] <- vt[which(this & !lead(aq_idx) & !lead(ah_idx)) + 1L] - contact_times$EO_A

        ## digs
        this <- which(is.na(vt) & d_idx & lag(a_idx))
        vt[this] <- vt[this - 1] + contact_times$A_D
        ## or with block touch
        this <- which(is.na(vt) & d_idx & lag(b_idx) & lag(a_idx, 2))
        vt[this] <- vt[this - 2] + contact_times$A_B_D

        ## next touch after dig overpass
        ov_idx <- is.na(vt) & lag(d_idx) & skill_idx & lag(px$team) != px$team
        vt[which(ov_idx)] <- vt[which(ov_idx) - 1] + contact_times$RDov

        ## freeballs over
        ov_idx <- is.na(vt) & lag(fo_idx) & skill_idx & lag(px$team) != px$team
        vt[which(ov_idx)] <- vt[which(ov_idx) - 1] + contact_times$RDov

        ## other digs following an opposition touch, e.g. freeball digs not already picked up
        this <- which(is.na(vt) & d_idx & lag(px$team) != px$team)
        vt[this] <- vt[this - 1] + contact_times$A_D

        ## set after dig (maybe not preceding attack, e.g. preceding a freeball over)
        this <- which(is.na(vt) & e_idx & lag(d_idx) & px$team == lag(px$team))
        vt[this] <- vt[this - 1] + contact_times$D_E

        ## dig (inc freeball dig) preceding set
        this <- which(is.na(vt) & d_idx & lead(e_idx))
        vt[this] <- vt[this + 1] - contact_times$D_E

        ## attack following set
        this <- is.na(vt) & a_idx & lag(e_idx)
        vt[which(this & aq_idx)] <- vt[which(this & aq_idx) - 1L] + contact_times$EQ_A
        vt[which(this & ah_idx)] <- vt[which(this & ah_idx) - 1L] + contact_times$EH_A
        vt[which(this & !aq_idx & !ah_idx)] <- vt[which(this & !aq_idx & !ah_idx) - 1L] + contact_times$EO_A

        ## this is problematic if we aren't scouting transition sets, because we can't differentiate P2 from regular dig-set-attack
        ## attack following own team touch that wasn't scouted as a set (e.g. P2)
        this <- which(is.na(vt) & (a_idx | fo_idx) & px$team == lag(px$team))
        vt[this] <- vt[this - 1] + contact_times$EO_A + contact_times$D_E

        ## blocks
        this <- which(is.na(vt) & b_idx & lag(a_idx))
        vt[this] <- vt[this - 1] + contact_times$A_B
        vt
    }
    for (loop in 1:10) vt <- trans_sync(vt)

    ## attack to counter-attack by opposition
    last_attack_time <- NA_real_
    last_attack_team <- NA_character_
    last_point_id <- -99L
    last_team_touch_id <- -99L
    for (i in which(a_idx)) {
        if (isTRUE(is.na(vt[i]) && !is.na(last_attack_time) &&
                   last_point_id == px$point_id[i] && (last_team_touch_id == px$team_touch_id[i] - 1) && last_attack_team != px$team[i])) {
            vt[i] <- last_attack_time + contact_times$A_A
        }
        last_attack_time <- vt[i]
        last_attack_team <- px$team[i]
        last_point_id <- px$point_id[i]
        last_team_touch_id <- px$team_touch_id[i]
    }

    ## at this point we should have most of the ball contacts synced. Anything left over should be things that don't fit our expected patterns above
    ## so just do our best to patch them up

    ## any un-synced touch following an opposition touch
    this <- which(is.na(vt) & skill_idx & lag(skill_idx) & px$team != lag(px$team) & px$point_id == lag(px$point_id))
    vt[this] <- vt[this - 1] + contact_times$A_D
    for (loop in 1:5) vt <- trans_sync(vt)

    ## interpolate all remaining skill times
    s_synced <- !is.na(vt) & skill_idx
    s_unsynced <- is.na(vt) & skill_idx
    for (pid in na.omit(unique(px$point_id[which(s_unsynced)]))) {
        ridx <- px$point_id == pid
        ridx_synced <- which(ridx & s_synced)
        if (length(ridx_synced) > 0) vt[which(ridx)] <- approx(ridx_synced, vt[ridx_synced], which(ridx))$y
    }

    ## find timeouts
    to_idx <- grepl("timeout", px$skill, ignore.case = TRUE)

    ## non-touch rows including timeouts, but not green codes or point markers
    is_point <- grepl("^[a\\*]p", px$code)
    is_green_code <- grepl("^[a\\*]\\$\\$&H[#=]", px$code) ## green codes [a*]$$&H[#=]
    non_skill <- (is.na(px$skill) | to_idx) & !is_point & !is_green_code
    this <- which(is.na(vt) & non_skill)
    for (i in setdiff(this, 1)) vt[i] <- vt[i - 1]

    ## timeouts
    this <- to_idx & is.na(vt)
    vt[which(this)] <- vt[which(this) - 1]
    ## note though that this probably has not worked, because the time preceding a timeout is likely to be the last rally point assignment code
    ## but timeouts are processed again after applying offsets

    px$contact_time <- vt

    ## step 2: apply offsets
    char2skill <- list(S = "Serve", R = "Reception", E = "Set", A = "Attack", B = "Block", D = "Dig", F = "Freeball")
    align_to_prior_skill <- function(vt, target_idx, prior_skill_char) {
        if (is.logical(target_idx)) target_idx <- which(target_idx)
        prior_skill <- unname(unlist(char2skill[strsplit(prior_skill_char, "")[[1]]]))
        if (length(prior_skill) < 1) {
            warning("cannot match offset to prior skill '", prior_skill_char, "'")
            return(vt) ## do nothing
        }
        for (i in target_idx) {
            prior_sk <- which(px$skill %in% prior_skill & px$point_id == px$point_id[i])
            prior_sk <- tail(prior_sk[prior_sk %in% c(i - 1, i - 2)], 1) ## preceding two rows only, or tail(prior_sk[prior_sk < i], 1) ## for any preceding row
            if (length(prior_sk) == 1) vt[i] <- vt[prior_sk]
        }
        vt
    }

    if (length(offsets) > 0) {
        offsets_idx <- list(S = s_idx, R = r_idx, E = e_idx, A = a_idx, B = b_idx, D = d_idx, F = fo_idx) ## d_idx includes freeball digs
        for (pass in 1:2) {
            ## do numeric offsets on first pass, then character
            for (skill_char in names(offsets)) {
                if (!is.null(offsets[[skill_char]]) && !is.na(offsets[[skill_char]])) {
                    if (pass == 1 && is.numeric(offsets[[skill_char]])) {
                        vt[offsets_idx[[skill_char]]] <- vt[offsets_idx[[skill_char]]] + offsets[[skill_char]]
                    } else if (pass == 2 && is.character(offsets[[skill_char]])) {
                        if (skill_char == "S") {
                            warning("S offset cannot be character, ignoring")
                        } else {
                            vt <- align_to_prior_skill(vt, target_idx = offsets_idx[[skill_char]], prior_skill_char = offsets[[skill_char]])
                        }
                    }
                }
            }
        }
    }

    ## end of rally, end of set, green codes, substitutions, setter positions
    ## give timeouts the most recent non-missing video time
    ## do these after offsets have been applied
    last_point_id <- -1
    last_skill_vt <- NA_integer_
    last_non_missing_vt <- NA_integer_
    is_sub_or_pos <- grepl("^[a\\*][zZcC]", px$code) | grepl("^\\*\\*[[:digit:]]set", px$code)
    for (i in seq_len(nrow(px))) {
        if (to_idx[i] && !is.na(last_non_missing_vt)) vt[i] <- last_non_missing_vt
        if (px$point_id[i] %eq% last_point_id && !is.na(last_skill_vt) && is.na(vt[i])) {
            if (is_green_code[i]) {
                vt[i] <- last_skill_vt
            } else if (is_point[i]) {
                vt[i] <- last_skill_vt + contact_times$END
            }
        }
        if (i > 1 && isTRUE(is_sub_or_pos[i]) && is.na(vt[i])) vt[i] <- vt[i - 1] + 1L - isTRUE(is_sub_or_pos[i - 1])
        if (isTRUE(skill_idx[i])) last_skill_vt <- vt[i]
        if (!is.na(vt[i])) last_non_missing_vt <- vt[i]
        last_point_id <- px$point_id[i]
    }

    if (isTRUE(enforce_order)) {
        last_vt <- -1
        for (i in seq_along(vt)) {
            if (!is.na(vt[i])) {
                if (vt[i] < last_vt) vt[i] <- last_vt
                last_vt <- vt[i]
            }
        }
    }

    px$video_time <- round(vt) ## video times have to be integer
    plays(x) <- px[, setdiff(names(px), c(".FREEBALL_OVER"))]
    x
}

#' @rdname dv_sync_video
#' @export
dv_sync_contact_times <- function(...) {
    defaults <- list(SQ = 4, ## time between the scouted serve time and actual serve contact for jump serves
                     SM = 2, ## time between the scouted serve time and actual serve contact for jump-float serves
                     SO = 1, ## time between the scouted serve time and actual serve contact for all other serves
                     SQ_R = 1, ## time between serve contact and reception contact (jump serves)
                     SM_R = 1.5, ## time between serve contact and reception contact (jump-float serves)
                     SO_R = 2, ## time between serve contact and reception contact (all other serves)
                     R_E = 3, ## time between reception contact and set
                     EQ_A = 1, ## time between set and attack for quick sets
                     EH_A = 2, ## time between set and attack for high sets
                     EO_A = 1.5, ## time between set and attack for all other sets
                     A_B = 0, ## time between attack and block
                     A_D = 1, ## time between attack and dig (no block touch)
                     A_B_D = 1, ## time between attack and dig (with block touch)
                     D_E = 3, ## time between dig and set
                     RDov = 3, ## reception or dig overpass to next touch by opposition
                     END = 3) ## last action to end-of-rally marker
    out <- list(...)
    non <- setdiff(names(out), names(defaults))
    if (length(non) > 0) warning("unrecognized element", if (length(non) > 1) "s", " ", paste(non, collapse = ", "), ", ignoring")
    out <- out[names(out) %in% names(defaults)]
    c(out, defaults[!names(defaults) %in% names(out)])
}

#' @rdname dv_sync_video
#' @export
dv_sync_offsets <- function(...) {
    defaults <- list(S = -5, R = "S", E = -8, A = -5, B = -5, D = -6, F = -5)
    out <- list(...)
    non <- setdiff(names(out), names(defaults))
    if (length(non) > 0) warning("unrecognized element", if (length(non) > 1) "s", " ", paste(non, collapse = ", "), ", ignoring")
    out <- out[names(out) %in% names(defaults)]
    c(out, defaults[!names(defaults) %in% names(out)])
}

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
    S_R <- x %>% dplyr::filter(.data$skill == "Serve", lead(.data$skill) == "Reception") %>%
        mutate(skill_type = case_when(.data$skill_type %in% c("Jump serve", "Jump-float serve") ~ .data$skill_type,
                                      TRUE ~ "Other serve types")) %>%
        group_by(.data$skill_type) %>% dplyr::reframe(tsm(paste0("Serve to reception time (", .data$skill_type[1], ")"), .data$t_lead_1 - .data$video_time))

    R_E <- x %>% dplyr::filter(.data$skill == "Reception", lead(.data$skill) == "Set", lead(.data$team) == .data$team) %>%
        dplyr::reframe(tsm("Reception to set time", .data$t_lead_2 - .data$video_time))

    E_A <- x %>% dplyr::filter(.data$skill == "Set", lead(.data$skill) == "Attack", lead(.data$team) == .data$team) %>%
        mutate(skill_type = case_when(grepl("^Quick|High", .data$skill_type) ~ .data$skill_type,
                                      TRUE ~ "Other set types")) %>%
        group_by(.data$skill_type) %>% dplyr::reframe(tsm(paste0("Set to attack time (", .data$skill_type[1], ")"), .data$t_lead_1 - .data$video_time))

    A_B <- x %>% dplyr::filter(.data$skill == "Attack", lead(.data$skill) == "Block") %>% dplyr::reframe(tsm("Attack to block time", .data$t_lead_1 - .data$video_time))

    A_D <- x %>% mutate(dig_block = .data$skill == "Dig" & lag(.data$skill) == "Block" & lag(.data$skill, 2) == "Attack" & lag(.data$team, 2) != .data$team,
                        dig_no_block = .data$skill == "Dig" & lag(.data$skill) == "Attack" & lag(.data$team) != .data$team) %>%
        dplyr::filter(.data$skill == "Dig", .data$dig_no_block | .data$dig_block) %>%
        mutate(skill_type = if_else(.data$dig_block, "with block touch", "no block touch")) %>%
        group_by(.data$skill_type) %>% dplyr::reframe(tsm(paste0("Attack to dig time (", .data$skill_type[1], ")"), .data$video_time - if_else(.data$dig_block, .data$t_lag_2, .data$t_lag_1)))

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
    out <- bind_rows(S_R, R_E, E_A, A_B, A_D, D_E, A_A, RDov, END) %>% dplyr::select(-"skill_type")
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
