#' Calculate break points
#'
#' A break point in one in which the serving team wins the point. Break point rate is the fraction of points won by the serving team.
#' 
#' @param x data.frame: the plays component of a datavolley object, as returned by \code{read_dv()}
#'
#' @return named list with components "ix" (logical indices of break points in the x object), "n" (number of break points in x), "rate" (break point rate from x)
#'
#' @seealso \code{\link{read_dv}}
#'
#' @examples
#' \dontrun{
#'   x <- read_dv(system.file("extdata/example_data.dvw",package="datavolley"))
#'   break_points(subset(x$plays,team=="Kamnik"))
#' }
#'
#' @export
break_points=function(x) {
    out <- list(ix=x$skill=="Serve" & !is.na(x$skill) & x$point_won_by==x$team)
    out$n <- sum(out$ix,na.rm=TRUE)
    out$rate <- out$n/sum(x$skill=="Serve",na.rm=TRUE)
    out
}

