## main reader function

#' Read a datavolley file
#'
#' @references \url{http://www.dataproject.com/IT/en/Volleyball}
#' @param filename string: file name to read
#' @param dowarn logical: should we issue warnings about the contents of the file as we read it?
#' @param encodings character: vector of text encodings to try
#'
#' @return named list with meta and plays components. meta provides match metadata, plays is the main point-by-point data in the form of a data.frame
#'
#' @examples
#' \dontrun{
#' 
#' }
#' @export
read_dv <- function(filename,dowarn=TRUE,encodings=c("latin2","windows-1252","iso885913")) {    
    out <- list()
    ## read raw lines in
    dv <- readLines(filename,warn=dowarn)
    ## try to guess encoding based on the first few lines of the file
    ## badchars indicate characters that we don't expect to see, so the presence of any of these indicates that we've got the wrong file encoding
    badchars <- utf8ToInt(paste0(iconv("\xf9",from="latin2"),iconv("\xb3\xa3",from="iso885913")))
    enctest <- sapply(encodings,function(tryenc)sum(sapply(sapply(iconv(dv[1:100],from=tryenc),utf8ToInt),function(z)any(z %in% badchars))))
    #if (abs(diff(enctest))<1 & any(enctest>0)) {
    #    cat(str(enctest))
    #    temp=iconv(dv[1:60],from="latin2",to="utf-8")
    #    print(temp[grepl(badchars,temp)])
    #    temp=iconv(dv[1:60],from="iso885913",to="utf-8")
    #    print(temp[grepl(badchars,temp)])
    #    stop("ambiguous encoding in ",filename)
    #}
    encfrom <- encodings[which.min(enctest)]
    dv <- iconv(dv,from=encfrom,to="utf-8") ## convert encoding to utf-8
    if (!dowarn) {
        suppressWarnings(out$meta <- read_meta(dv))
    } else {
        out$meta <- read_meta(dv)
    }
    out$meta$filename <- filename
    if (!dowarn) {
        suppressWarnings(this_main <- read_main(filename))
    } else {
        this_main <- read_main(filename)
    }
    if (dowarn) {
        out$plays <- parse_code(this_main$code,out$meta)
    } else {
        suppressWarnings(out$plays <- parse_code(this_main$code,out$meta))
    }
    ## post-process plays data
    ##add the recognised columns from main to plays (note that we are discarding a few columns from main here)
    out$plays <- cbind(this_main[,c("time","video_time")],out$plays,this_main[,c("home_p1","home_p2","home_p3","home_p4","home_p5","home_p6","visiting_p1","visiting_p2","visiting_p3","visiting_p4","visiting_p5","visiting_p6")])
    ## add set number
    out$plays$set_number <- NA
    temp <- c(0,which(out$plays$end_of_set))
    for (si in 2:length(temp)) {
        out$plays$set_number[(temp[si-1]+1):(temp[si]-1)] <- (si-1)
    }
    ## find technical timeouts at points 8 and 16 in sets 1-4
    for (this_set in 1:4) {
        for (thisp in c(8,16)) {
            idx <- which((out$plays$home_team_score==thisp | out$plays$visiting_team_score==thisp) & out$plays$set_number==this_set)
            if (length(idx)>0) {
                idx <- idx[1]
                ##cat(sprintf("Inserting technical timeout at row %d\n",idx))
                out$plays <- rbind.fill(out$plays[1:idx,],data.frame(skill="Technical timeout",timeout=TRUE,set_number=this_set,point=FALSE,end_of_set=FALSE,substitution=FALSE),out$plays[(idx+1):nrow(out$plays),])
            }
        }
    }
    ## add match_id
    out$plays$match_id <- out$meta$match_id    
    ##out$plays$time=hms(out$plays$time)
    
    ## add play_id
    pid <- 0
    out$plays$play_id <- NA
    out$plays$play_id[1] <- 0    
    for (k in 2:nrow(out$plays)) {
        ##if ((!is.na(out$plays$skill[k]) && out$plays$skill[k]=="Serve") | out$plays$timeout[k]) { ## does not handle sanctions
        if (out$plays$point[k-1] | out$plays$timeout[k] | out$plays$timeout[k-1]) { ## timeout[k-1] otherwise the following play does not start with a new play_id
            pid <- pid+1
        }
        out$plays$play_id[k] <- pid
    }
    ## touch_id (row number)
    out$plays$touch_id <- 1:nrow(out$plays)
    ## team name
    home_team <- out$meta$teams$team_name[out$meta$teams$team=="*"]
    visiting_team <- out$meta$teams$team_name[out$meta$teams$team=="a"]
    temp <- rep(as.character(NA),nrow(out$plays))
    temp[out$plays$team=="*"] <- home_team
    temp[out$plays$team=="a"] <- visiting_team
    out$plays$team <- temp
    out$plays$home_team <- home_team
    out$plays$visiting_team <- visiting_team
    ## keep track of who won each point
    temp <- ddply(out$plays,.(play_id),function(z)data.frame(point_won_by <- if (any(z$point)) { z$team[z$point] } else { as.character(NA) } ))
    suppressMessages(out$plays <- join(out$plays,temp))
    ## catch any that we missed
    ##dudidx <- is.na(out$plays$point_won_by) & !out$plays$skill %in% c(NA,"Timeout","Technical timeout")
    ## not sure how to deal with these!
    
    ## winning attacks
    ## A followed by D with "Error" evaluation, or A with "Winning attack" evaluation
    temp1 <- out$plays[-nrow(out$plays),]
    temp2 <- out$plays[-1,]
    out$plays$winning_attack <- FALSE
    out$plays$winning_attack[1:(nrow(out$plays)-1)] <- temp1$skill=="Attack" & (temp1$evaluation=="Winning attack" | ((temp2$skill=="Dig" | temp2$skill=="Block") & temp2$evaluation=="Error"))
    ## note the block error is not actually needed there, since such attacks are recorded as winning ones anyway
    out$plays$winning_attack[is.na(out$plays$winning_attack)] <- FALSE
    ## fill in scores
    scores <- ddply(out$plays,.(play_id),function(z)na.omit(z[,c("play_id","home_team_score","visiting_team_score")]))
    scores <- suppressMessages(join(out$plays[,"play_id",drop=FALSE],scores))
    ## double-check
    if (any(na.omit(out$plays$home_team_score-scores$home_team_score) != 0) | any(na.omit(out$plays$visiting_team_score-scores$visiting_team_score) != 0)) stop("error in scores")
    out$plays$home_team_score <- scores$home_team_score
    out$plays$visiting_team_score <- scores$visiting_team_score
    ## will still have NA scores for timeouts and technical timeouts, patch NAs where we can
    for (k in 2:nrow(out$plays)) {
        if (is.na(out$plays$home_team_score[k]) & !out$plays$point[k]) {
            out$plays$home_team_score[k] <- out$plays$home_team_score[k-1]
        }
        if (is.na(out$plays$visiting_team_score[k]) & !out$plays$point[k]) {
            out$plays$visiting_team_score[k] <- out$plays$visiting_team_score[k-1]
        }
    }
    class(out) <- c("datavolley",class(out))
    out
}


#' Print a simple summary of a datavolley object
#'
#' @param object datavolley: datavolley object as returned by read_dv()
#' @param ... : additional arguments (currently these have no effect)
#'
#' @return print summary to console, and return the summary object invisibly
#'
#' @seealso \code{\link{read_dv}}
#'
#' @export
summary.datavolley <- function(object,...) {
    out <- sprintf("Match summary:\nDate: %s\nLeague: %s\n",object$meta$match$date,object$meta$match$league)
    out <- sprintf("%sTeams: %s (%s/%s)\n       vs\n       %s (%s/%s)\n",out,object$meta$teams$team_name[1],object$meta$teams$coach[1],object$meta$teams$assistant[1],object$meta$teams$team_name[2],object$meta$teams$coach[2],object$meta$teams$assistant[2])
    temp <- object$meta$result$score_home_team>object$meta$result$score_visiting_team
    out <- sprintf("%sResult: %d-%d (%s)\n",out,sum(temp),sum(!temp),paste(object$meta$result$score,collapse=", "))
    out <- sprintf("%sDuration: %d minutes\n",out,sum(object$meta$result$duration))
    cat(out)
    invisible(out)
}


dvlist_summary <- function(z) {
    cat(sprintf("Number of matches: %d\n",length(z)))
    temp <- range(ldply(z,function(q)as.Date(q$meta$match$date))$V1)
    cat(sprintf("Match dates: %s to %s\n",temp[1],temp[2]))
    temp <- as.character(sapply(z,function(q) q$meta$teams$team_name))
    teams <- as.data.frame(table(temp))
    names(teams) <- c("team_name","played")
    temp <- ldply(z,function(q)q$meta$teams[,c("team_name","won_match")])
    teams <- suppressMessages(join(teams,ddply(temp,.(team_name),function(q)data.frame(won=sum(q$won_match)))))
    teams$win_rate <- teams$won/teams$played
    names(teams)[1] <- "team"
    teams <- arrange(teams,team)
    print(teams)
    invisible(teams)
}

