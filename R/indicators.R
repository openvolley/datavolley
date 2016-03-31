#' Find serves
#' 
#' @param x data.frame: the plays component of a datavolley object, as returned by \code{read_dv()}
#'
#' @return a logical vector, giving the indices of the rows of x that correspond to serves
#'
#' @seealso \code{\link{read_dv}}
#'
#' @examples
#' \dontrun{
#' x <- read_dv(system.file("extdata/example_data.dvw",package="datavolley"))
#' serve_idx <- find_serves(x$plays)
#' ## number of serves by team
#' table(x$plays$team[serve_idx])
#' }
#'
#' @export
find_serves=function(x) {
    x$skill %eq% "Serve"
}

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
#' x <- read_dv(system.file("extdata/example_data.dvw",package="datavolley"))
#' serve_idx <- find_serves(x$plays)
#' swp <- serve_win_points(x$plays)
#' ## number of serves by team
#' table(x$plays$team[serve_idx])
#' ## number of points won on serve by team
#' table(x$plays$team[serve_idx & swp$ix])
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



#' Find first attacks by the receiving team (i.e. attacks associated with a serve reception)
#'
#' @param x data.frame: the plays component of a datavolley object, as returned by \code{read_dv()}
# #' @param return_id logical: include the match_id and point_id of all first attacks in the returned object?
#'
#' @return named list with components "ix" (logical indices into the x object where the row corresponds to a first attack in a rally), "n" (number of receptions for which there was a first attack by the receiving team), "n_win" (the number of winning first attacks), "win_rate" (number of winning first attacks as a proportion of the total number of first attacks).
# #' If \code{return_id} is TRUE, also return a component "id" (a data.frame containing the match_id and point_id of all first attacks)
#'
#' @seealso \code{\link{read_dv}}
#'
#' @examples
#' \dontrun{
#' x <- read_dv(system.file("extdata/example_data.dvw",package="datavolley"))
#' ## first attack win rate, by team
#' by(x$plays,x$plays$team,function(z)find_first_attack(z)$win_rate)
#' }
#' @export
find_first_attack=function(x) {
    ## add row identifier for convenience
    x$my_row_id <- 1:nrow(x)
    faw_idx <- function(z) {
        ## given a rally z, did the receiving team make a first attack?
        ## if no, return NA NA; if yes, return the my_row_id of the attack, and TRUE/FALSE that it was a winning attack
        out <- c(NA,NA)
        if (sum(z$skill %eq% "Reception")==1) {
            ## one reception (i.e. no serve error). Was the attack from the same team_touch_id as the reception a winning one?
            this_attack_idx <- z$skill %eq% "Attack" & z$team_touch_id==z$team_touch_id[z$skill %eq% "Reception"]
            if (sum(this_attack_idx,na.rm=TRUE)==1) out <- c(z$my_row_id[this_attack_idx],z$winning_attack[this_attack_idx])
        }
        data.frame(row_id=out[1],won=out[2])
    }
    this <- na.omit(ddply(x,.(match_id,point_id),faw_idx))
    ix <- rep(FALSE,nrow(x))
    ix[x$my_row_id %in% this$row_id] <- TRUE
    out <- list(ix=ix,n=nrow(this),n_win=sum(this$won),win_rate=mean(this$won))
    #if (return_id) out$id <- this[,-3] ## drop the row_id column, since that was an internal addition
    out
}

