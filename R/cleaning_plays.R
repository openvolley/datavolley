## #' Find gaps in timing that might indicate missing data
## #'
## #' @param x data.frame: the plays component of a datavolley object, as returned by \code{dv_read()}
## #'
## #' @return logical vector indicating entries where the timing looks suspicious
## #'
## #' @seealso \code{\link{dv_read}} \code{\link{plays}}
## #'
## #' @examples
## #'
## #' @export
## find_timing_errors=function(x) {
##     if (inherits(x$time,"character")) {
##         x$time <- strptime(x$time,format=c(time="%H.%M.%S"))
##     }
## }



#' Find aces that might not be marked as such
#'
#' Some DataVolley files do not indicate serve aces with the skill evaluation "Ace". This function will search for winning serves, either with no reception or a reception error, and change their evaluation value to "Ace"
#'
#' @param x datavolley: a datavolley object as returned by \code{dv_read}, or list of such objects
#' @param rotation_error_is_ace logical: should a rotation error on reception by the receiving team be counted as an ace?
#' @param verbose logical: print progress to console?
#'
#' @return datavolley object or list of such with updated evaluation values
#'
#' @seealso \code{\link{dv_read}}
#'
#' @export
fix_ace_evaluations <- function(x, rotation_error_is_ace = FALSE, verbose = TRUE) {
    if (!(inherits(x, "datavolley") | (is.list(x) && all(sapply(x, function(z) inherits(z, "datavolley")))))) stop("x must be a datavolley object or list of such objects")
    was_list <- TRUE
    if (inherits(x, "datavolley")) {
        x <- list(x)
        was_list <- FALSE
    }
    find_should_be_aces <- function(rally) {
        sv <- which(rally$skill == "Serve")
        if (length(sv) == 1) {
            ## there was one serve
            ## change it to ace if the serving team won AND (there was no reception or there was a reception error) AND there was not a rotation error on reception AND it's not already marked as an ace
            if ((rally$team[sv] %eq% rally$point_won_by[sv]) & (!"Reception" %in% rally$skill || rally$evaluation[rally$skill %eq% "Reception"] %eq% "Error") & (rotation_error_is_ace | !rally$skill[sv+1] %eq% "Rotation error") & !identical(rally$evaluation[sv], "Ace"))
                data.frame(point_id = rally$point_id[1], evaluation = rally$evaluation[sv])
        } else if (length(sv) > 1) {
            stop("multiple serves in single rally (point_id: ", rally$point_id[1], ")")
        }
    }
    for (k in seq_along(x)) {
        seval <- na.omit(bind_rows(lapply(split(x[[k]]$plays, x[[k]]$plays$point_id), find_should_be_aces)))
        ## now have data.frame of point_id's that need serve evaluation updated to "Ace"
        if (verbose) message(paste0("Changing ", nrow(seval), " serve evaluations to \"Ace\" in match_id ", x[[k]]$meta$match_id))
        x[[k]]$plays$evaluation[x[[k]]$plays$point_id %in% seval$point_id & x[[k]]$plays$skill %eq% "Serve"] <- "Ace"
    }
    if (!was_list) {
        x[[1]]
    } else {
        x
    }
}
