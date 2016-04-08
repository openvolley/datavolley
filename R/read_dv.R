## main reader function

#' Read a datavolley file
#'
#' The \code{do_transliterate} option may be helpful when trying to work with multiple files from the same competition, since different text encodings may be used on different files. This can lead to e.g. multiple versions of the same team name. Transliterating can help avoid this, at the cost of losing e.g. diacriticals. Transliteration is applied after converting from the specified text encoding to UTF-8. Common encodings used with DataVolley files include "windows-1252" (western European), "windows-1250" (central European), "iso88592" (central/eastern European), "iso885913" (Baltic languages)
#' 
#' @references \url{http://www.dataproject.com/IT/en/Volleyball}
#' @param filename string: file name to read
#' @param insert_technical_timeouts logical: should we insert technical timeouts at points 8 and 16 of sets 1--4?
#' @param do_warn logical: should we issue warnings about the contents of the file as we read it?
#' @param do_transliterate logical: should we transliterate all text to ASCII? See details
#' @param encoding character: text encoding to use. Text is converted from this encoding to UTF-8. A vector of multiple encodings can be provided, and this function will attempt to choose the best (experimental)
#'
#' @return named list with \code{meta} and \code{plays} components. \code{meta} provides match metadata, \code{plays} is the main point-by-point data in the form of a data.frame
#'
#' @examples
#' \dontrun{
#'   x <- read_dv(system.file("extdata/example_data.dvw",package="datavolley"),
#'     insert_technical_timeouts=FALSE)
#'   summary(x)
#' }
#' @export
read_dv <- function(filename,insert_technical_timeouts=TRUE,do_warn=FALSE,do_transliterate=FALSE,encoding) {
    assert_that(is.flag(insert_technical_timeouts))
    assert_that(is.flag(do_warn))
    assert_that(is.flag(do_transliterate))
    
    out <- list()
    ## read raw lines in
    dv <- readLines(filename,warn=do_warn)
    if (!missing(encoding)) {
        assert_that(is.character(encoding))
        if (length(encoding)>1) {
            ## try to guess encoding based on the first few lines of the file
            ## badchars indicate characters that we don't expect to see, so the presence of any of these indicates that we've got the wrong file encoding
            ## surely there is a better way to do this ...
            badchars <- utf8ToInt(paste0(iconv("\xf9",from="latin2"),iconv("\xb3\xa3",from="iso885913"),"\u008a","\u008e","\u009a"))
            enctest <- sapply(encoding,function(tryenc)sum(sapply(sapply(iconv(dv[1:100],from=tryenc),utf8ToInt),function(z)any(z %in% badchars))))
                                        #if (abs(diff(enctest))<1 & any(enctest>0)) {
                                        #    cat(str(enctest))
                                        #    temp=iconv(dv[1:60],from="latin2",to="utf-8")
                                        #    print(temp[grepl(badchars,temp)])
                                        #    temp=iconv(dv[1:60],from="iso885913",to="utf-8")
                                        #    print(temp[grepl(badchars,temp)])
                                        #    stop("ambiguous encoding in ",filename)
                                        #}
            encoding <- encoding[which.min(enctest)]
        }
        dv <- iconv(dv,from=encoding,to="utf-8") ## convert to utf-8
    }
    if (do_transliterate) {
        dv <- iconv(dv,from="utf-8",to="ascii//TRANSLIT")
    }
    if (!do_warn) {
        suppressWarnings(out$meta <- read_meta(dv))
    } else {
        out$meta <- read_meta(dv)
    }
    out$meta$filename <- filename
    if (!do_warn) {
        suppressWarnings(this_main <- read_main(filename))
    } else {
        this_main <- read_main(filename)
    }
    if (do_warn) {
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
    if (insert_technical_timeouts) {
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
    }
    ## add match_id
    out$plays$match_id <- out$meta$match_id
    
    ## turn plays times (character) into POSIXct
    temp <- paste(format(as.Date(out$meta$match$date)),out$plays$time,sep=" ")
    temp[out$plays$time=="" | is.na(out$plays$time)] <- NA
    out$plays$time <- ymd_hms(temp)
    
    ## add point_id - an identifier of each point. One point may consist of multiple attacks or other actions. Timeouts get assigned to their own "point", but other non-play rows may get assigned as part of a point.
    pid <- 0
    out$plays$point_id <- NA
    out$plays$point_id[1] <- pid
    for (k in 2:nrow(out$plays)) {
        ##if ((!is.na(out$plays$skill[k]) && out$plays$skill[k]=="Serve") | out$plays$timeout[k]) { ## does not cope with sanctions
        if (out$plays$point[k-1] | out$plays$timeout[k] | out$plays$timeout[k-1]) { ## timeout[k-1] otherwise the following play does not start with a new point_id
            pid <- pid+1
        }
        out$plays$point_id[k] <- pid
    }

    ## add team_touch_id - an identifier of consecutive touches by same team in same point - e.g. a dig-set-attack sequence by one team is a "team touch"
    tid <- 0
    out$plays$team_touch_id <- NA
    out$plays$team_touch_id[1] <- tid
    for (k in 2:nrow(out$plays)) {
        if (!identical(out$plays$team[k],out$plays$team[k-1]) | !identical(out$plays$point_id[k],out$plays$point_id[k-1])) {
            tid <- tid+1
        }
        out$plays$team_touch_id[k] <- tid
    }
    
    ## team name
    home_team <- out$meta$teams$team[out$meta$teams$home_away_team=="*"]
    visiting_team <- out$meta$teams$team[out$meta$teams$home_away_team=="a"]
    temp <- rep(as.character(NA),nrow(out$plays))
    temp[out$plays$team=="*"] <- home_team
    temp[out$plays$team=="a"] <- visiting_team
    out$plays$team <- temp
    out$plays$home_team <- home_team
    out$plays$visiting_team <- visiting_team
    ## keep track of who won each point
    temp <- ddply(out$plays,c("point_id"),function(z)data.frame(point_won_by=if (any(z$point)) { z$team[z$point] } else { as.character(NA) } ))
    suppressMessages(out$plays <- join(out$plays,temp))
    ## catch any that we missed
    ##dud_point_id <- unique(out$plays$point_id[is.na(out$plays$point_won_by) & !out$plays$skill %in% c(NA,"Timeout","Technical timeout")])
    ##for (dpi in dud_point_id) {
    ##    tail(na.omit(out$plays[out$plays$point_id<dpi,c("home_team_score","visiting_team_score","point_won_by")]),1)
    ##    head(na.omit(out$plays[out$plays$point_id>dpi,c("home_team_score","visiting_team_score","point_won_by")]),1)
    ##}
    #### not sure how to deal with these!
    
    ## winning attacks
    ## A followed by D with "Error" evaluation, or A with "Winning attack" evaluation
    temp1 <- out$plays[-nrow(out$plays),]
    temp2 <- out$plays[-1,]
    out$plays$winning_attack <- FALSE
    out$plays$winning_attack[1:(nrow(out$plays)-1)] <- temp1$skill=="Attack" & (temp1$evaluation=="Winning attack" | ((temp2$skill=="Dig" | temp2$skill=="Block") & temp2$evaluation=="Error"))
    ## note the block error is not actually needed there, since such attacks are recorded as winning ones anyway
    out$plays$winning_attack[is.na(out$plays$winning_attack)] <- FALSE
    ## fill in scores
    scores <- ddply(out$plays,c("point_id"),function(z)na.omit(z[,c("point_id","home_team_score","visiting_team_score")]))
    scores <- suppressMessages(join(out$plays[,"point_id",drop=FALSE],scores))
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
    class(out$plays) <- c("datavolleyplays",class(out$plays))
    out
}


#' A simple summary of a volleyball match
#'
#' @param object datavolley: datavolley object as returned by \code{read_dv}
#' @param ... : additional arguments (currently these have no effect)
#'
#' @return list of summary items
#'
#' @seealso \code{\link{read_dv}}
#' @examples
#' \dontrun{
#'   x <- read_dv(system.file("extdata/example_data.dvw",package="datavolley"),
#'     insert_technical_timeouts=FALSE)
#'   summary(x)
#' }
#'
#' @method summary datavolley
#' @export
summary.datavolley <- function(object,...) {
    out <- list(date=object$meta$match$date,league=object$meta$match$league)
    out$teams <- object$meta$teams[,c("team","coach","assistant","sets_won")] ## data.frame(team=object$meta$teams$team,coach=object$meta$teams$coach,assistant=object$meta$teams$assistant,sets_won=object$meta$teams$sets_won,stringsAsFactors=FALSE)
    temp <- object$meta$result$score_home_team>object$meta$result$score_visiting_team
    out$set_scores <- object$meta$result[,c("score_home_team","score_visiting_team")]
    ## make extra sure that set_scores has home team assigned correctly
    if (object$meta$teams$home_away_team[1]!="*") { out$set_scores <- out$set_scores[,2:1] }
    out$set_scores <- na.omit(out$set_scores)
    out$duration <- sum(object$meta$result$duration,na.rm=TRUE)
    class(out) <- "summary.datavolley"
    out
}

#' Print method for summary.datavolley
#'
#' @param x summary.datavolley: a summary.datavolley object as returned by \code{summary.datavolley}
#' @param ... : additional arguments (currently these have no effect)
#' @seealso \code{\link{summary.datavolley}}
#' @method print summary.datavolley
#' @export
print.summary.datavolley <- function(x,...) {
    out <- sprintf("Match summary:\nDate: %s\nLeague: %s\n",x$date,x$league)
    out <- sprintf("%sTeams: %s (%s/%s)\n       vs\n       %s (%s/%s)\n",out,x$teams$team[1],x$teams$coach[1],x$teams$assistant[1],x$teams$team[2],x$teams$coach[2],x$teams$assistant[2])
    out <- sprintf("%sResult: %d-%d (%s)\n",out,x$teams$sets_won[1],x$teams$sets_won[2],paste(x$set_scores[,1],x$set_scores[,2],sep="-",collapse=", "))
    out <- sprintf("%sDuration: %d minutes\n",out,x$duration)
    cat(out)
    invisible(out)
}

    


#' Summarize a list of volleyball matches
#'
#' @param z list: list of datavolley objects as returned by \code{read_dv}
#'
#' @return named list with various summary indicators, including a competition ladder
#'
#' @seealso \code{\link{read_dv}}
#'
#' @examples
#' \dontrun{
#'   x <- read_dv(system.file("extdata/example_data.dvw",package="datavolley"),
#'     insert_technical_timeouts=FALSE)
#'   dvlist_summary(list(x,x)) ## same match duplicated twice, just for illustration purposes
#' }
#'
#' @export
dvlist_summary=function(z) {
    out <- list(number_of_matches=length(z))
    out$date_range <- range(ldply(z,function(q)as.Date(q$meta$match$date))$V1)
    temp <- as.character(sapply(z,function(q) q$meta$teams$team))
    teams <- as.data.frame(table(temp))
    names(teams) <- c("team","played")
    temp <- ldply(z,function(q)q$meta$teams[,c("team","won_match")])
    teams <- suppressMessages(join(teams,ddply(temp,c("team"),function(q)data.frame(won=sum(q$won_match)))))
    teams$win_rate <- teams$won/teams$played
    temp <- ldply(z,function(q){ s <- summary(q); s$sc <- colSums(s$set_scores); data.frame(team=s$teams$team,points_for=s$sc,points_against=s$sc[2:1])})
    teams <- suppressMessages(join(teams,ddply(temp,c("team"),function(q)data.frame(points_for=as.integer(sum(q$points_for)),points_against=as.integer(sum(q$points_against))))))
    teams$points_ratio <- teams$points_for/teams$points_against
    teams <- arrange(teams,team)
    out$ladder <- teams
    class(out) <- "summary.datavolleylist"
    out
}


#' Print method for summary.datavolleylist
#'
#' @param x summary.datavolleylist: a summary.datavolleylist object, as returned by \code{dvlist_summary}
#' @param ... : additional arguments (currently these have no effect)
#' @seealso \code{\link{dvlist_summary}}
#' @method print summary.datavolleylist
#' @export
print.summary.datavolleylist <- function(x,...) {
    cat(sprintf("Number of matches: %d\n",x$number_of_matches))
    cat(sprintf("Match dates: %s to %s\n",x$date_range[1],x$date_range[2]))
    print(x$ladder)
}


#' Extract the plays component from a datavolley object
#'
#' @param x datavolley: a datavolley object as returned by \code{read_dv}
#'
#' @return The plays component of x (a data.frame)
#'
#' @seealso \code{\link{read_dv}}
#'
#' @examples
#' \dontrun{
#'   x <- read_dv(system.file("extdata/example_data.dvw",package="datavolley"),
#'     insert_technical_timeouts=FALSE)
#'   inspect(plays(x))
#' }
#' @export
plays=function(x) {
    if (!(inherits(x,"datavolley"))) stop("x must be a datavolley object")
    x$plays
}


#' Convenience function for inspecting the plays component of a datavolley object
#'
#' @param x datavolleyplays: the plays component of a datavolley object as returned by \code{read_dv}
#' @param vars string: which variables to print? "minimal" set or "all"
#' @param maxrows numeric: maximum number of rows to print
#'
#' @seealso \code{\link{read_dv}} \code{\link{plays}}
#'
#' @examples
#' \dontrun{
#'   x <- read_dv(system.file("extdata/example_data.dvw",package="datavolley"),
#'     insert_technical_timeouts=FALSE)
#'   inspect(plays(x))
#' }
#'
#' @export
inspect <- function(x,vars="minimal",maxrows=100) {
    ##if (!(inherits(x,"datavolleyplays"))) stop("x must be a datavolleyplays object")
    vars <- match.arg(vars,c("minimal","all"))
    cols_to_show <- if (vars=="all") names(x) else c("time","code","team","player_number","player_name","skill","skill_type","evaluation")##,"match_id","set_number")
    print(x[1:min(nrow(x),maxrows),cols_to_show])
}
