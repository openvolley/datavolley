#' Find serve win points
#'
#' Find points in which the serving team wins the point. Serve win rate is the fraction of serves won by the serving team.
#' 
#' @param x data.frame: the plays component of a datavolley object, as returned by \code{read_dv()}
#' @param return_id logical: include the match_id and point_id of all serve win points in the returned object?
#'
#' @return named list with components "ix" (logical indices of serves corresponding to serve win points in the x object), "n" (number of serve win points in x), "rate" (serve win rate from x). If \code{return_id} is TRUE, also return a component "id" (a data.frame containing the match_id and point_id of all serve win points)
#'
#' @seealso \code{\link{read_dv}}
#'
#' @examples
#' \dontrun{
#'   x <- read_dv(system.file("extdata/example_data.dvw",package="datavolley"))
#'   serve_win_points(subset(x$plays,team=="Kamnik"))
#' }
#'
#' @export
serve_win_points=function(x,return_id=FALSE) {
    assert_that(is.flag(return_id))
    out <- list(ix=x$skill=="Serve" & x$point_won_by==x$team)
    out$ix[is.na(out$ix)] <- FALSE
    out$n <- sum(out$ix)
    out$rate <- out$n/sum(x$skill=="Serve",na.rm=TRUE)
    if (return_id) {
        out$id <- x[out$ix,c("match_id","point_id")]
    }
    out
}

