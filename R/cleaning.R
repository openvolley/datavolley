#' Change team names
#'
#' A team name can sometimes be spelled incorrectly, particularly if there are character encoding issues. This can be a particular problem when combining data from multiple files. Any team name in \code{to} is replaced with the corresponding entry in \code{from}.
#'
#' @param x datavolley: a datavolley object as returned by \code{read_dv}, or list of such objects
#' @param from string: vector of team names to change from
#' @param to string: vector of team names to change to
#'
#' @return datavolley object with corresponding team names changed
#'
#' @seealso \code{\link{read_dv}}
#'
#' @examples
#' \dontrun{
#'   x <- read_dv(system.file("extdata/example_data.dvw",package="datavolley"))
#'   summary(x)
#'   x <- remap_team_names(x,from="Kamnik",to="OK Kamnik")
#'   summary(x)
#' }
#' @export
remap_team_names=function(x,from,to) {
    assert_that(is.character(from))
    assert_that(is.character(to))
    assert_that(are_equal(length(from),length(to)))
    if (length(from)!=length(unique(from))) stop("elements in from must be unique")
    if (!(inherits(x,"datavolley") | (is.list(x) && all(sapply(x,function(z)inherits(z,"datavolley"))))))
        stop("x must be a datavolley object or list of such objects")
    was_list <- TRUE
    if (inherits(x,"datavolley")) {
        x <- list(x)
        was_list <- FALSE
    }
    for (k in 1:length(x)) {
        for (t in 1:2) {
            this_team <- x[[k]]$meta$teams$team[t]
            if (this_team %in% from) {
                team_should_be <- to[from==this_team]
                x[[k]]$meta$teams$team[t] <- team_should_be
                x[[k]]$plays$team <- str_replace_all(x[[k]]$plays$team,fixed(this_team),team_should_be)
                x[[k]]$plays$point_won_by <- str_replace_all(x[[k]]$plays$point_won_by,fixed(this_team),team_should_be)
                x[[k]]$plays$home_team <- str_replace_all(x[[k]]$plays$home_team,fixed(this_team),team_should_be)
                x[[k]]$plays$visiting_team <- str_replace_all(x[[k]]$plays$visiting_team,fixed(this_team),team_should_be)
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
    was_list <- TRUE
    if (inherits(x,"datavolley")) {
        x <- list(x)
        was_list <- FALSE
    }

    names_ok <- ldply(x,function(z)rbind(data.frame(team=z$meta$teams$team[z$meta$teams$home_away_team=="*"],player_name=z$meta$players_h$name,stringsAsFactors=FALSE),data.frame(team=z$meta$teams$team[z$meta$teams$home_away_team=="a"],player_name=z$meta$players_v$name,stringsAsFactors=FALSE)))
    blah <- ddply(names_ok,c("team","player_name"),summarise,table(player_name))
    names(blah)[3] <- "count"
    ## similarity of adjacent names
    ndist <- adist(blah$player_name)
    mask <- lower.tri(ndist)
    mask[!mask] <- NA
    ndist <- ndist*mask
    susidx <- which(!is.na(ndist) & ndist<distance_threshold,arr.ind=TRUE)
    susout <- data.frame()
    if (length(susidx)>0) {
        for (k in 1:nrow(susidx)) {
            susout <- rbind(susout,data.frame(team1=blah$team[susidx[k,1]],player1=blah$player_name[susidx[k,1]],count1=blah$count[susidx[k,1]],team2=blah$team[susidx[k,2]],player2=blah$player_name[susidx[k,2]],count2=blah$count[susidx[k,2]],stringsAsFactors=FALSE))
        }
    }
    susout
}


