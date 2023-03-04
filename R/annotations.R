#' Figure out the phase of play associated with each point
#'
#' Phase is either "Serve", "Reception" (serve reception and the set and attack immediately following it, as well as the opposition block on that attack),
#' or "Transition" (all play actions after that)
#'
#' @param x datavolleyplays: the plays component of a datavolley object as returned by \code{dv_read}
#' @param method string: "default" (uses the \code{team_touch_id} and \code{skill} values to figure out phase), or "alt" (uses the sequences of \code{skill} values only. This is slower and probably less reliable, but will be more likely to give correct results in some situations (e.g. if the DataVolley file has been scouted in practice mode, and all actions have been assigned to the one team)
#' @seealso \code{\link{dv_read}} \code{\link{plays}}
#' @return character vector
#'
#' @examples
#' \dontrun{
#'   x <- dv_read(dv_example_file(), insert_technical_timeouts = FALSE)
#'   px <- plays(x)
#'   px$phase <- play_phase(px)
#' }
#'
#' @export
play_phase <- function(x, method = "default") {
    method <- match.arg(tolower(method), c("default", "alt"))
    phase <- rep(NA_character_, nrow(x))
    skill <- x$skill
    if (method == "default") {
        phase[!is.na(skill)] <- "Transition" # default to this
        phase[skill %eq% "Serve"] <- "Serve"
        ## serve reception, along with other actions with the same team_touch_id as a reception, all count as "Reception" phase
        temp <- paste(x$match_id, x$set_number, x$team_touch_id, sep = "/")
        phase[temp %in% temp[skill %eq% "Reception"]] <- "Reception"
    } else {
        phase[skill %eq% "Serve"] <- "Serve"
        phase[skill %eq% "Reception"] <- "Reception"
        reception_touches <- NA_integer_
        for (i in seq_len(nrow(x))[-1]) {
            if (is.na(skill[i]) || skill[i] %in% c("Serve", "Reception", "Timeout", "Technical timeout", "Rotation error")) {
                reception_touches <- if (skill[i] %eq% "Reception") 1 else NA_character_
                next
            }
            if (skill[i] %eq% "Dig") {
                reception_touches <- NA_integer_
                phase[i] <- "Transition"
            } else if (phase[i - 1] %eq% "Reception" && skill[i] %in% c("Set", "Attack", "Freeball") && (!is.na(reception_touches) && reception_touches < 3)) {
                ## these might not be reception phase, if the ball crossed the net before 3 reception touches were used
                ## but we can't tell if this is the case (?)
                reception_touches <- reception_touches + 1
                phase[i] <- "Reception"
            } else {
                reception_touches <- NA_integer_
                phase[i] <- "Transition"
            }
        }
    }
    ## also blocks against reception attack should be reception phase
    idx1 <- which(skill %eq% "Block")
    idx2 <- which(skill %eq% "Attack" & phase %eq% "Reception") + 1
    phase[intersect(idx1, idx2)] <- "Reception"
    phase
}

#' Point phase
#'
#' Point phase as defined by DataVolley: either "Sideout" or "Breakpoint", assigned only to winning or losing actions (including green codes). Note that the point phase is inferred for the winning action (i.e. the point phase value for both the winning and losing action is "Sideout" if the winning team was receiving).
#'
#' @param x datavolleyplays: the plays component of a datavolley object as returned by [dv_read()]
#'
#' @return Character vector
#'
#' @export
dv_point_phase <- function(x) {
    if (nrow(x) < 1) return(character())
    out <- rep(NA_character_, nrow(x))
    lidx <- (!is.na(x$skill) & x$evaluation %in% c("Error", "Blocked", "Invasion")) | (x$code %in% c("*$$&H=", "a$$&H="))
    widx <- (!is.na(x$skill) & x$evaluation %in% c("Ace", "Winning attack", "Winning block")) | (x$code %in% c("*$$&H#", "a$$&H#"))
    if (any(lidx)) {
        out[lidx & x$team %eq% x$serving_team] <- "Sideout"
        out[lidx & !x$team %eq% x$serving_team] <- "Breakpoint"
    }
    if (any(widx)) {
        out[widx & x$team %eq% x$serving_team] <- "Breakpoint"
        out[widx & !x$team %eq% x$serving_team] <- "Sideout"
    }
    out
}

#' Attack phase
#'
#' Attack phase as defined by DataVolley: either "Reception", "Transition sideout" or "Transition breakpoint", assigned only to attack actions.
#'
#' @param x datavolleyplays: the plays component of a datavolley object as returned by [dv_read()]
#'
#' @return Character vector
#'
#' @export
dv_attack_phase <- function(x) {
    if (nrow(x) < 1) return(character())
    if (!"phase" %in% names(x)) x$phase <- play_phase(x)
    out <- rep(NA_character_, nrow(x))
    out[x$skill %eq% "Attack" & x$phase %eq% "Reception"] <- "Reception"
    out[x$skill %eq% "Attack" & x$team %eq% x$serving_team & x$phase %eq% "Transition"] <- "Transition breakpoint"
    out[x$skill %eq% "Attack" & !x$team %eq% x$serving_team & x$phase %eq% "Transition"] <- "Transition sideout"
    out
}

