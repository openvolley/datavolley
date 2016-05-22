#' Change team names
#'
#' A team name can sometimes be spelled incorrectly, particularly if there are character encoding issues. This can be a particular problem when combining data from multiple files. A team name matching the \code{from} entry in a row in \code{remap} is renamed to the corresponding \code{to} value.
#'
#' @param x datavolley: a datavolley object as returned by \code{read_dv}, or list of such objects
#' @param remap data.frame: data.frame of strings with columns from and to
#' @param fixed logical: treat the \code{to} and \code{from} entries as fixed string? (Treat as regexps if \code{fixed} is \code{FALSE})
#'
#' @return datavolley object or list with corresponding team names changed
#'
#' @seealso \code{\link{read_dv}}
#'
#' @examples
#' \dontrun{
#'   x <- read_dv(system.file("extdata/example_data.dvw",package="datavolley"),
#'     insert_technical_timeouts=FALSE)
#'   summary(x)
#'   x <- remap_team_names(x,data.frame(from="Nova KBM Branik",to="NKBM Branik"))
#'   summary(x)
#' }
#' @export
remap_team_names=function(x,remap,fixed=TRUE) {
    if (!(inherits(x,"datavolley") | (is.list(x) && all(sapply(x,function(z)inherits(z,"datavolley"))))))
        stop("x must be a datavolley object or list of such objects")
    assert_that(is.data.frame(remap))
    assert_that(is.flag(fixed))
    assert_that(has_name(remap,"from"),has_name(remap,"to"))
    if (is.factor(remap$from)) remap$from <- as.character(remap$from)
    if (is.factor(remap$to)) remap$to <- as.character(remap$to)
    if (length(remap$from)!=length(unique(remap$from))) stop("elements in from must be unique")
    was_list <- TRUE
    if (inherits(x,"datavolley")) {
        x <- list(x)
        was_list <- FALSE
    }
    for (k in 1:length(x)) {
        for (t in 1:2) {
            this_team <- x[[k]]$meta$teams$team[t]
            if (fixed)
                idx <- remap$from==this_team
            else
                idx <- !is.na(str_locate(this_team,team_remaps$from)[,1])
            if (sum(idx)==1) {
                team_should_be <- remap$to[which(idx)]
                x[[k]]$meta$teams$team[t] <- team_should_be
                x[[k]]$plays$team <- str_replace_all(x[[k]]$plays$team,fixed(this_team),team_should_be)
                x[[k]]$plays$point_won_by <- str_replace_all(x[[k]]$plays$point_won_by,fixed(this_team),team_should_be)
                x[[k]]$plays$home_team <- str_replace_all(x[[k]]$plays$home_team,fixed(this_team),team_should_be)
                x[[k]]$plays$visiting_team <- str_replace_all(x[[k]]$plays$visiting_team,fixed(this_team),team_should_be)
            } else if (sum(idx)>1) {
                stop(sprintf("ambiguous team name remapping: %s matches more than one 'from' entry",this_team))
            }
        }
    }
    if (!was_list) {
        x[[1]]
    } else {
        x
    }
}



#' Check for similar player names
#'
#' Player names can sometimes be spelled incorrectly, particularly if there are character encoding issues. This can be a particular problem when combining data from multiple files. This function checks for similar names that might possibly be multiple variants on the same name.
#'
#' @param x datavolley: a datavolley object as returned by \code{read_dv}, or list of such objects
#' @param distance_threshold numeric: if two names differ by an amount less than this threshold, they will be returned as possible matches
#'
#' @return data.frame
#'
#' @seealso \code{\link{read_dv}}, \code{\link{adist}}
#'
#' @export
check_player_names=function(x,distance_threshold=4) {
    if (!(inherits(x,"datavolley") | (is.list(x) && all(sapply(x,function(z)inherits(z,"datavolley"))))))
        stop("x must be a datavolley object or list of such objects")
    if (inherits(x,"datavolley")) x <- list(x)

    names_ok <- ldply(x,function(z)rbind(data.frame(team=z$meta$teams$team[z$meta$teams$home_away_team=="*"],player_name=z$meta$players_h$name,stringsAsFactors=FALSE),data.frame(team=z$meta$teams$team[z$meta$teams$home_away_team=="a"],player_name=z$meta$players_v$name,stringsAsFactors=FALSE)))
    blah <- ddply(names_ok,c("team","player_name"),summarise,table(player_name))
    names(blah)[3] <- "count"
    blah$count <- as.integer(blah$count) ## else it inherits class 'table'
    ## similarity of adjacent names
    ndist <- adist(blah$player_name)
    mask <- lower.tri(ndist)
    mask[!mask] <- NA
    ndist <- ndist*mask
    susidx <- which(!is.na(ndist) & ndist<distance_threshold,arr.ind=TRUE)
    susout <- data.frame()
    if (nrow(susidx)>0) {
        for (k in 1:nrow(susidx)) {
            susout <- rbind(susout,data.frame(team1=blah$team[susidx[k,1]],player1=blah$player_name[susidx[k,1]],count1=blah$count[susidx[k,1]],team2=blah$team[susidx[k,2]],player2=blah$player_name[susidx[k,2]],count2=blah$count[susidx[k,2]],stringsAsFactors=FALSE))
        }
    }
    susout
}


#' Change player names
#'
#' A player name can sometimes be spelled incorrectly, particularly if there are character encoding issues. This can be a particular problem when combining data from multiple files. A player matching the \code{team} and \code{from} entries in a row in \code{remap} is renamed to the corresponding \code{to} value.
#'
#' @param x datavolley: a datavolley object as returned by \code{read_dv}, or list of such objects
#' @param remap data.frame: data.frame of strings with columns team, from, and to
#'
#' @return datavolley object or list with corresponding player names changed
#'
#' @seealso \code{\link{read_dv}}, \code{\link{check_player_names}}, \code{\link{find_player_name_remapping}}
#'
#' @examples
#' \dontrun{
#'   x <- read_dv(system.file("extdata/example_data.dvw",package="datavolley"),
#'     insert_technical_timeouts=FALSE)
#'   x <- remap_player_names(x,data.frame(team=c("Kamnik","Br.Maribor"),
#'                             from=c("Ana Katarina HRIBAR","Marina CVETANOVIC"),
#'                             to=c("Ana-Katarina HRIBAR","Marina CVETANOVIĆ")))
#' }
#' @export
remap_player_names=function(x,remap) {
    if (!(inherits(x,"datavolley") | (is.list(x) && all(sapply(x,function(z)inherits(z,"datavolley"))))))
        stop("x must be a datavolley object or list of such objects")
    assert_that(is.data.frame(remap))
    assert_that(has_name(remap,"team"),has_name(remap,"from"),has_name(remap,"to"))
    if (is.factor(remap$team)) remap$team <- as.character(remap$team)
    if (is.factor(remap$from)) remap$from <- as.character(remap$from)
    if (is.factor(remap$to)) remap$to <- as.character(remap$to)
    was_list <- TRUE
    if (inherits(x,"datavolley")) {
        x <- list(x)
        was_list <- FALSE
    }
    for (k in 1:length(x)) {
        ## apply to team lists
        t <- "*"
        this_team <- x[[k]]$meta$teams$team[x[[k]]$meta$teams$home_away_team==t]
        this_to_change <- subset(remap,team==this_team)
        x[[k]]$meta$players_h$name <- mapvalues(x[[k]]$meta$players_h$name,this_to_change$from,this_to_change$to,warn_missing=FALSE)
        t <- "a"
        this_team <- x[[k]]$meta$teams$team[x[[k]]$meta$teams$home_away_team==t]
        this_to_change <- subset(remap,team==this_team)
        x[[k]]$meta$players_v$name <- mapvalues(x[[k]]$meta$players_v$name,this_to_change$from,this_to_change$to,warn_missing=FALSE)
        ## and to plays dataframe
        for (ti in 1:nrow(remap)) {
            x[[k]]$plays$player_name[x[[k]]$plays$team==remap$team[ti] & x[[k]]$plays$player_name==remap$from[ti]] <- remap$to[ti]
        }
    }
    if (!was_list) {
        x[[1]]
    } else {
        x
    }
}



#' Attempt to build a player name remapping table
#'
#' A player name can sometimes be spelled incorrectly, particularly if there are character encoding issues. This can be a particular problem when combining data from multiple files. This function will attempt to find names that have been misspelled and create a remapping table suitable to pass to \code{\link{remap_player_names}}. Player names will only be compared within the same team. Note that this function is unlikely to get perfect results: use its output with care.
#'
#' @param x datavolley: a datavolley object as returned by \code{read_dv}, or list of such objects
#' @param distance_threshold numeric: if two names differ by an amount less than this threshold, they will be treated as the same name
#' @param verbose logical: print progress to console as we go? Note that warnings will also be issued regardless of this setting
#'
#' @return data.frame with columns team, from, to
#'
#' @seealso \code{\link{remap_player_names}}, \code{\link{check_player_names}}
#'
#' @examples
#' \dontrun{
#'   x <- read_dv(system.file("extdata/example_data.dvw",package="datavolley"),
#'     insert_technical_timeouts=FALSE)
#'   remap <- find_player_name_remapping(x)
#' }
#' @export
find_player_name_remapping=function(x,distance_threshold=3,verbose=TRUE) {
    assert_that(is.flag(verbose))
    if (!(inherits(x,"datavolley") | (is.list(x) && all(sapply(x,function(z)inherits(z,"datavolley"))))))
        stop("x must be a datavolley object or list of such objects")
    if (inherits(x,"datavolley")) x <- list(x)

    all_names <- ldply(x,function(z)rbind(data.frame(team=z$meta$teams$team[z$meta$teams$home_away_team=="*"],player_name=z$meta$players_h$name,stringsAsFactors=FALSE),data.frame(team=z$meta$teams$team[z$meta$teams$home_away_team=="a"],player_name=z$meta$players_v$name,stringsAsFactors=FALSE)))
    all_teams <- unique(unlist(lapply(x,function(z)z$meta$teams$team)))

    names_to_change <- data.frame()
    for (t in all_teams) {
        this_names <- subset(all_names,team==t)$player_name ##sort(unique(
        this_names_count <- as.data.frame(table(subset(all_names,team==t)$player_name))
        names(this_names_count) <- c("name","count")
        ##this_names_count <- arrange(this_names_count,name)
        ## check for suspect names by transliteration (removing diacriticals) and by text distance
        names_translit <- iconv(this_names,from="utf-8",to="ascii//TRANSLIT")
        names_map <- rep(0,length(this_names))
        for (n in 1:length(this_names)) {
            p <- this_names[n]
            this_is_translit <- p==names_translit[n] ## this name does not have diacriticals
            if (this_is_translit) {
                ## does the non-transliterated form exist? allow for fuzzy matches
                nmdist <- adist(names_translit[n],names_translit)
                this_exists_not_translit <- sum(nmdist<distance_threshold)>1
                if (this_exists_not_translit) {
                    name_should_be <- setdiff(which(nmdist<distance_threshold),n)
                    if (length(name_should_be)<1) {
                        warning("problem remapping player name ",p,", skipping")
                        break
                    }
                    if (length(name_should_be)>1) {
                        warning("ambiguous name: ",p," could be ",paste(this_names[name_should_be],collapse=" or "),". Choosing first")
                        name_should_be <- name_should_be[1]
                    }
                    if (names_map[name_should_be]==n) {
                        ## have already mapped the reverse, so don't make circular ...
                        ##cat(sprintf(" (name %s already mapped to %s, so not mapping the reverse)\n",this_names[name_should_be],p))
                    } else {
                        names_map[n] <- name_should_be
                        name_should_be <- this_names[name_should_be]
                        if (verbose) cat(sprintf("Team %s: mapping player name %s to %s\n",t,p,name_should_be))
                        names_to_change <- rbind(names_to_change,data.frame(team=t,from=p,to=name_should_be,stringsAsFactors=FALSE))
                    }
                }
            }
        }
    }
    names_to_change
}

