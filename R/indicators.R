#' Find serves
#' 
#' @param x data.frame: the plays component of a datavolley object, as returned by \code{dv_read()}
#'
#' @return a logical vector, giving the indices of the rows of x that correspond to serves
#'
#' @seealso \code{\link{dv_read}} \code{\link{plays}}
#'
#' @examples
#' \dontrun{
#' x <- dv_read(dv_example_file(), insert_technical_timeouts=FALSE)
#' serve_idx <- find_serves(plays(x))
#' ## number of serves by team
#' table(plays(x)$team[serve_idx])
#' }
#'
#' @export
find_serves=function(x) {
    if (!inherits(x,"datavolleyplays")) stop("x must be a datavolleyplays object")
    x$skill %eq% "Serve"
}

#' Find serve win points
#'
#' Find points in which the serving team wins the point. Serve win rate is the fraction of serves won by the serving team.
#' 
#' @param x data.frame: the plays component of a datavolley object, as returned by \code{dv_read()}
#' @param return_id logical: include the match_id and point_id of all serve win points in the returned object?
#'
#' @return named list with components "ix" (logical indices of serves corresponding to serve win points in the x object), "n" (number of serve win points in x), "rate" (serve win rate from x). If \code{return_id} is TRUE, also return a component "id" (a data.frame containing the match_id and point_id of all serve win points)
#'
#' @seealso \code{\link{dv_read}} \code{\link{plays}}
#'
#' @examples
#' \dontrun{
#' x <- dv_read(dv_example_file(), insert_technical_timeouts=FALSE)
#' serve_idx <- find_serves(plays(x))
#' swp <- serve_win_points(plays(x))
#' ## number of serves by team
#' table(plays(x)$team[serve_idx])
#' ## number of points won on serve by team
#' table(plays(x)$team[serve_idx & swp$ix])
#' }
#'
#' @export
serve_win_points=function(x,return_id=FALSE) {
    if (!inherits(x,"datavolleyplays")) stop("x must be a datavolleyplays object")
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
#' @param x data.frame: the plays component of a datavolley object, as returned by \code{dv_read()}
# #' @param return_id logical: include the match_id and point_id of all first attacks in the returned object?
#'
#' @return named list with components "ix" (logical indices into the x object where the row corresponds to a first attack in a rally), "n" (number of receptions for which there was a first attack by the receiving team), "n_win" (the number of winning first attacks), "win_rate" (number of winning first attacks as a proportion of the total number of first attacks).
# #' If \code{return_id} is TRUE, also return a component "id" (a data.frame containing the match_id and point_id of all first attacks)
#'
#' @seealso \code{\link{dv_read}} \code{\link{plays}}
#'
#' @examples
#' \dontrun{
#' x <- dv_read(dv_example_file(), insert_technical_timeouts=FALSE)
#' ## first attack win rate, by team
#' by(plays(x),plays(x)$team,function(z)find_first_attack(z)$win_rate)
#' }
#' @export
find_first_attack <- function(x) {
    ## add row identifier for convenience
    x$my_row_id <- seq_len(nrow(x))
    faw_idx <- function(z) {
        ## given a rally z, did the receiving team make a first attack?
        ## if no, return NA NA; if yes, return the my_row_id of the attack, and TRUE/FALSE that it was a winning attack
        out <- c(NA,NA)
        if (sum(z$skill %eq% "Reception")==1) {
            ## one reception (i.e. no serve error). Was the attack from the same team_touch_id as the reception a winning one?
            this_attack_idx <- z$skill %eq% "Attack" & z$team_touch_id==z$team_touch_id[z$skill %eq% "Reception"]
            if (sum(this_attack_idx,na.rm=TRUE)==1) out <- c(z$my_row_id[this_attack_idx],z$winning_attack[this_attack_idx])
        }
        data.frame(match_id = z$match_id[1], point_id = z$point_id[1], row_id = out[1], won = out[2])
    }
    this <- na.omit(bind_rows(lapply(split(x, ~match_id + point_id), faw_idx)))

    ix <- rep(FALSE,nrow(x))
    ix[x$my_row_id %in% this$row_id] <- TRUE
    out <- list(ix=ix,n=nrow(this),n_win=sum(this$won),win_rate=mean(this$won))
    #if (return_id) out$id <- this[,-3] ## drop the row_id column, since that was an internal addition
    out
}


#' Generate information about runs of events
#'
#' Find runs of events within a match. Typically, this function would be passed a subset of \code{plays(x)}, such as rows
#' corresponding to serves. Runs that are terminated by the end of a set are not assigned a \code{run_length}.
#'
#' @param x data.frame: a subset of the plays component of a datavolley object, as returned by \code{dv_read()}
#' @param idvars character: string or character vector of variabe names to use to identify the entity doing the events
#' @param within_set logical: only consider runs within a single set? If FALSE, runs that span sets will be treated as a single run
#'
#' @return A data.frame the same number of rows as \code{x}, and with columns \code{run_id} (the identifier of the run to which each row belongs), \code{run_length} (the length of the run), and \code{run_position} (the position of this row in its associated run).
#'
#' @seealso \code{\link{dv_read}} \code{\link{plays}}
#'
#' @examples
#' \dontrun{
#' ## find runs of serves
#' x <- dv_read(dv_example_file(), insert_technical_timeouts = FALSE)
#' serve_idx <- find_serves(plays(x))
#' serve_run_info <- find_runs(plays(x)[serve_idx,])
#' ## distribution of serve run lengths
#' table(unique(serve_run_info[,c("run_id","run_length")])$run_length)
#' }
#'
#' @export
find_runs=function(x,idvars="team",within_set=TRUE) {
    nna <- rep(NA,nrow(x))
    out <- data.frame(run_length=nna,run_position=nna,run_id=nna)
    trsi <- 1 ## this run start index
    run_id <- 1
    for (k in 2:nrow(x)) {
        if (identical(x$match_id[k],x$match_id[k-1]) & (!within_set || identical(x$set_number[k],x$set_number[k-1]))) {
            ## same set and match as previous row
            ##            if (!identical(x$team[k],x$team[k-1])) {
            if (!isTRUE(all.equal(x[k,idvars],x[k-1,idvars],check.attributes=FALSE))) {
                ## same set and match, but team has changed, so run has ended
                out$run_length[trsi:(k-1)] <- k-trsi
                out$run_position[trsi:(k-1)] <- 1:(k-trsi)
                out$run_id[trsi:(k-1)] <- run_id
                ## new run starts at k
                trsi <- k; run_id <- run_id + 1
                if (k==nrow(x)) {
                    out$run_position[nrow(out)] <- 1
                    out$run_id[nrow(out)] <- run_id
                }
            } else {
                ## team is the same, run continues
            }
        } else {
            ## run has ended but because of the end of the set, so record the run_position but not the run_length
            out$run_position[trsi:(k-1)] <- 1:(k-trsi)
            out$run_id[trsi:(k-1)] <- run_id            
            trsi <- k; run_id <- run_id + 1
            if (k==nrow(x)) {
                out$run_position[nrow(out)] <- out$run_position[nrow(out)-1]+1
                out$run_id[nrow(out)] <- out$run_id[nrow(out)-1]
            }
        }
    }
    out
}
