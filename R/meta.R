## functions for reading match metadata (top part of the dv file)

## match details
read_match <- function(txt) {
    idx <- grep("[3MATCH]",txt,fixed=TRUE)
    p <- read.table(text=txt[idx+1],sep=";",quote="",stringsAsFactors=FALSE,header=FALSE)
    names(p)[1] <- "date"
    names(p)[2] <- "time"
    names(p)[3] <- "season"
    names(p)[4] <- "league"
    temp <- suppressWarnings(mdy_hms(paste(p$date,p$time,sep=" "),truncated=3))
    if (is.na(temp)) {
        ## try dmy
        temp <- suppressWarnings(dmy_hms(paste(p$date,p$time,sep=" "),truncated=3))
    }
    p$date <- temp
    p$time <- suppressWarnings(hms(p$time))
    p
}

read_result <- function(txt) {
    txt <- text_chunk(txt,"[3SET]")
    suppressWarnings(tryCatch({ p <- data.table::fread(txt,data.table=FALSE,sep=";") },error=function(e) { stop("input file could not be read: is the encoding argument supplied to read_dv correct?") }))
    names(p)[1] <- "played"
    names(p)[2] <- "score_intermediate1"
    names(p)[3] <- "score_intermediate2"
    names(p)[4] <- "score_intermediate3"
    names(p)[5] <- "score"
    names(p)[6] <- "duration"
    p$score <- gsub("\\s+","",p$score)
    temp <- str_match(p$score,"(\\d+)\\-(\\d+)")
    p$score_home_team <- as.numeric(temp[,2])
    p$score_visiting_team <- as.numeric(temp[,3])
    p <- p[p$score!="",]
    p
}

## teams
read_teams <- function(txt) {
    txt <- text_chunk(txt,"[3TEAMS]")
    suppressWarnings(tryCatch({ p <- data.table::fread(txt,data.table=FALSE,sep=";") },error=function(e) { stop("input file could not be read: is the encoding argument supplied to read_dv correct?") }))
    names(p)[1] <- "team_id"
    names(p)[2] <- "team"
    names(p)[3] <- "sets_won"
    names(p)[4] <- "coach"
    names(p)[5] <- "assistant"
    p$home_away_team <- c("*","a")
    p
}

## players
read_players <- function(txt,team,surname_case) {
    if (missing(surname_case)) surname_case <- "asis"
    if (missing(team)) team <- "home"
    txt <- switch(tolower(team),
        home=text_chunk(txt,"[3PLAYERS-H]"),
        text_chunk(txt,"[3PLAYERS-V]")
                  )
    suppressWarnings(tryCatch({ p <- data.table::fread(txt,data.table=FALSE,sep=";") },error=function(e) { stop("input file could not be read: is the encoding argument supplied to read_dv correct?") }))
    names(p)[2] <- "number"
    names(p)[4] <- "starting_position_set1"
    names(p)[5] <- "starting_position_set2"
    names(p)[6] <- "starting_position_set3"
    names(p)[7] <- "starting_position_set4"
    names(p)[8] <- "starting_position_set5"
    names(p)[9] <- "player_id"
    names(p)[10] <- "lastname"
    names(p)[11] <- "firstname"
    names(p)[13] <- "special_role"
    names(p)[14] <- "role"
    if (is.character(surname_case)) {
        p$lastname <- switch(tolower(surname_case),
                             upper=toupper(p$lastname),
                             lower=tolower(p$lastname),
                             title=str_to_title(p$lastname),
                             p$lastname)
    } else if (is.function(surname_case)) {
        p$lastname <- surname_case(p$lastname)
    }
    p$name <- paste(p$firstname,p$lastname,sep=" ")
    p$role <- plyr::mapvalues(p$role,from=1:5,to=c("libero","pass-hitter","opposite","middle","setter","unknown"),warn_missing=FALSE)
    p
}

## attack codes
read_attacks <- function(txt) {
    txt <- text_chunk(txt,"[3ATTACKCOMBINATION]")
    if (str_trim(txt)=="") {
        NULL
    } else {
        ##tryCatch({ p <- data.table::fread(txt,data.table=FALSE,sep=";") },error=function(e) { stop("input file could not be read: is the encoding argument supplied to read_dv correct?") })
        tryCatch({ p <- read.table(text=txt,sep=";",quote="",stringsAsFactors=FALSE) },error=function(e) { stop("input file could not be read: is the encoding argument supplied to read_dv correct?") })
        ## X2;2;L;Q;veloce dietro;;65280;4868;C;;
        names(p)[1] <- "code"
        names(p)[3] <- "side"
        names(p)[4] <- "type" ## tempo, codes match attack skill types Q,M,T, etc
        names(p)[5] <- "description"
        names(p)[9] <- "set_type" ## some sort of set_type code, guessing S=setter dump, C=centre, B=backset, F=frontrow, P=pipe
        p
    }
}

read_setter_calls <- function(txt) {
    txt <- text_chunk(txt,"[3SETTERCALL]")
    if (str_trim(txt)=="") {
        NULL
    } else {
        suppressWarnings(tryCatch({ p <- data.table::fread(txt,data.table=FALSE,sep=";") },error=function(e) { stop("input file could not be read: is the encoding argument supplied to read_dv correct?") }))
        names(p)[1] <- "code"
        names(p)[3] <- "description"
        p
    }
}

read_meta <- function(txt,surname_case) {
    out <- list()
    out$match <- read_match(txt)
    out$result <- read_result(txt)
    out$teams <- read_teams(txt)
    if (diff(out$teams$sets_won)<0) {
        out$teams$won_match <- c(TRUE,FALSE)
    } else {
        out$teams$won_match <- c(FALSE,TRUE)
    }
    out$players_h <- read_players(txt,"home",surname_case)
    out$players_v <- read_players(txt,"visiting",surname_case)
    out$attacks <- read_attacks(txt)
    out$sets <- read_setter_calls(txt)
    temp <- out$match
    temp$home_team <- out$teams$team[out$teams$home_away_team=="*"]
    temp$visiting_team <- out$teams$team[out$teams$home_away_team=="a"]
    out$match_id <- digest(temp)
    out
}


get_player_name <- function(team,number,meta) {
    if (!all(team=="*" | team=="a")) stop("team must be * or a")
    out <- rep(as.character(NA),length(number))
    idx <- team=="*"
    if (any(idx)) {
        out[idx] <- mapvalues(number[idx],from=meta$players_h$number,to=meta$players_h$name,warn_missing=FALSE)
        invalid_number <- out[idx]==number[idx]
        out[invalid_number] <- "unknown player"
        ##if (any(invalid_number)) stop("invalid home team player number")
    }
    idx <- team=="a"
    if (any(idx)) {
        out[idx] <- mapvalues(number[idx],from=meta$players_v$number,to=meta$players_v$name,warn_missing=FALSE)
        invalid_number <- out[idx]==number[idx]
        out[invalid_number] <- "unknown player"
        ##if (any(invalid_number)) stop("invalid visiting team player number")
    }        
    out
}


## #' Provide descriptions for attack codes
## #'
## #' DataVolley files generally contain a set of attack codes and corresponding descriptions (in Italian).
## #' If you use these codes differently, or wish to change the descriptions, you can pass a customised
## #' attack_code_describe function to \code{\link{read_dv}}
## #'
## #' @param code string: two-character attack code
## #' @param translation string: description of the corresponding attack
## #' @param show_map logical: if TRUE, return the whole table being used to map codes to descriptions
## #'
## #' @return string giving the description of that attack code
## #'
## #' @seealso \code{\link{read_dv}}
## #' @examples
## #' attack_code_describe("X2")
## #' attack_code_describe(show_map=TRUE)
## #'
## #' @export
attack_code_describe <- function(code,show_map=FALSE,stop_on_unknown=FALSE) {  
    dtbl <- read.table(text="code^description
X2^Quickball behind
X1^Quickball
XM^Veloce in punto 3
XG^7-1 Gun
XC^Quickball shifted (B-quick)
XD^DoppiaC
X7^Sette Davanti
XS^Sette Dietro
XO^Fast behind Opp.
XF^Fast opposite
PP^Setter dump
X9^Half davanti dopo
XT^Half from position 4
X3^Half from position 2
X4^Half dietro C.A.
XQ^Half Dietro C.D.
XB^Pipe between 6 and 1
XP^Pipe
XR^Pipe between 6 and 5
X5^Tip to position 4
X0^Tip to position 5
X6^Tip to position 2
X8^Tip to position 1
CD^Fast close to the setter
CB^Fast shifted from the setter
CF^Fast away from the setter
C5^Super to position 4
C0^Super to position 5
C6^Super to position 2
C8^Super to position 1
V5^High to position 4
V0^High to position 5
V6^High to position 2
V8^High to position 1
VB^High pipe between 6 and 1
VP^High pipe
VR^High pipe between 6 and 5
V3^High to position 3
P2^Secondo tocco di  lr
PR^Rigore",sep="^",header=TRUE,comment.char="",stringsAsFactors=FALSE)

    assert_that(is.logical(show_map))
    if (show_map) return(dtbl)

    #assert_that(is.string(code),nchar(code)==2)
    #this_desc <- dtbl$description[dtbl$code==code]
    #if (length(this_desc)<1) {
    #    if (stop_on_unknown) stop("unknown attack code: ",code)
    #    "unknown attack code"
    #} else {
    #    this_desc
                                        #}
    mapvalues(code,dtbl$code,dtbl$description,warn_missing=FALSE)
}



#' Get team names from datavolley object
#'
#' @param x datavolley: a datavolley object as returned by \code{read_dv}
#'
#' @return character vector of team names
#'
#' @seealso \code{\link{read_dv}}
#' 
#' @examples
#' \dontrun{
#'   x <- read_dv(system.file("extdata/example_data.dvw",package="datavolley"),
#'     insert_technical_timeouts=FALSE)
#'   teams(x)
#' }
#' @export
teams <- function(x) x$meta$teams$team

