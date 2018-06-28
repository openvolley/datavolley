#' Figure out the phase of play associated with each point
#'
#' Phase is either "Serve", "Reception" (serve reception and the attack immediately following it),
#' or "Transition" (play not associated with serve or reception)
#'
#' @param x datavolleyplays: the plays component of a datavolley object as returned by \code{read_dv}
#' @seealso \code{\link{read_dv}} \code{\link{plays}}
#' @return character vector
#'
#' @examples
#' \dontrun{
#'   x <- read_dv(dv_example_file(), insert_technical_timeouts=FALSE)
#'   px <- plays(x)
#'   px$phase <- play_phase(px)
#' }
#'
#' @export
play_phase <- function(x) {
    phase <- as.character(rep(NA, nrow(x)))
    phase[!is.na(x$skill)] <- "Transition" # default to this
    phase[x$skill %eq% "Serve"] <- "Serve"
    ##phase[x$skill %eq% "Reception"] <- "Reception"
    ## serve reception, along with other actions with the same team_touch_id as a reception, all count as "Reception" phase
    temp <- paste(x$match_id, x$set_number, x$team_touch_id, sep="/")
    phase[temp %in% temp[x$skill %eq% "Reception"]] <- "Reception"
    ## also blocks against reception attack should be reception phase
    idx1 <- which(x$skill %eq% "Block")
    idx2 <- which(x$skill %eq% "Attack" & phase %eq% "Reception") + 1
    phase[intersect(idx1, idx2)] <- "Reception"
    phase
}
