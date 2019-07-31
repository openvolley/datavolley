## functions for reading match metadata (top part of the dv file)

## match details
read_match <- function(txt) {
    idx <- grep("[3MATCH]",txt,fixed=TRUE)
    tryCatch(p <- read.table(text=txt[idx+1],sep=";",quote="",stringsAsFactors=FALSE,header=FALSE),
             error=function(e) { stop("could not read the [3MATCH] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to read_dv is incorrect?") })
    names(p)[1] <- "date"
    names(p)[2] <- "time"
    names(p)[3] <- "season"
    names(p)[4] <- "league"
    names(p)[9] <- "text_encoding"
    names(p)[11] <- "zones_or_cones" ## C or Z, e.g. 12/08/2018;;;;;;;;1;1;Z;0;
    msgs <- list()
    if (is.na(p$date)) {
        msgs <- collect_messages(msgs,"Match information is missing the date",idx+1,txt[idx+1],severity=2)
        date_was_missing <- TRUE
    } else {
        ## date can be in various formats
        temp <- manydates(p$date)
        if (length(temp)<1) {
            ## no recognizable date
            temp <- as.Date(NA)
        } else if (length(temp)>1) {
            ## ambiguous date format
            msgs <- collect_messages(msgs,"Ambiguous date, using DMY format",idx+1,txt[idx+1],severity=2)
            temp <- temp[1] ##** can we do better here?
        }
        p$date <- temp
        if (is.na(p$date)) {
            msgs <- collect_messages(msgs,"Cannot parse the date in the match information",idx+1,txt[idx+1],severity=2)
        } else {
            if (p$date<(as.Date(lubridate::now(tzone="UTC"))-365*10)) {
                ## date is more than ten years ago!
                msgs <- collect_messages(msgs,paste0("The date of the match (",format(p$date),") is more than 10 years ago, is it correct?"),idx+1,txt[idx+1],severity=2)
            }
        }
    }
    suppressWarnings(p$time <- lubridate::hms(p$time)) ## don't warn on time, because the plays object has it anyway
    list(match=p,messages=msgs)
}

read_more <- function(txt) {
    idx <- grep("[3MORE]",txt,fixed=TRUE)
    tryCatch(p <- read.table(text = txt[idx+1], sep = ";", quote = "", stringsAsFactors = FALSE, header = FALSE),
             error=function(e) stop("could not read the [3MORE] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to read_dv is incorrect?"))
    for (k in c(1, 4:6)) p[, k] <- as.character(p[, k])
    names(p)[1] <- "referees"
    names(p)[4] <- "city"
    names(p)[5] <- "arena"
    names(p)[6] <- "scout"
    p
}

read_result <- function(txt) {
    txt <- text_chunk(txt,"[3SET]")
    suppressWarnings(tryCatch({ p <- data.table::fread(txt, data.table=FALSE, sep=";", header=FALSE, na.strings="NA", logical01=FALSE) },error=function(e) { stop("could not read the [3SET] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to read_dv is incorrect?") }))
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
    idx <- grep("[3TEAMS]", txt, fixed = TRUE)
    txt <- text_chunk(txt, "[3TEAMS]")
    msgs <- list()
    suppressWarnings(tryCatch({ p <- data.table::fread(txt, data.table=FALSE,sep=";", header=FALSE, na.strings="NA", logical01=FALSE) },error=function(e) { stop("could not read the [3TEAMS] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to read_dv is incorrect?") }))
    names(p)[1] <- "team_id"
    names(p)[2] <- "team"
    names(p)[3] <- "sets_won"
    names(p)[4] <- "coach"
    names(p)[5] <- "assistant"
    p$home_away_team <- c("*","a")
    p$team_id <- as.character(p$team_id) ## force to be char
    suppressWarnings(p$sets_won <- as.integer(p$sets_won))
    ## check for identical team names
    if (p$team[1] %eq% p$team[2]) {
        msgs <- collect_messages(msgs, "The two team names are identical. They will be modified here but this may still cause problems", idx+1, txt[idx+1], severity = 1)
        p$team[1] <- paste0(p$team[1]," (home)")
        p$team[2] <- paste0(p$team[2]," (visiting)")
    }
    if (p$team_id[1] %eq% p$team_id[2]) {
        msgs <- collect_messages(msgs, "The two team IDs are identical. They will be modified here but this may still cause problems", idx+1, txt[idx+1], severity = 1)
        p$team_id[1] <- paste0(p$team_id[1]," (home)")
        p$team_id[2] <- paste0(p$team_id[2]," (visiting)")
    }
    list(teams = p, messages = msgs)
}

## players
read_players <- function(txt,team,surname_case) {
    if (missing(surname_case)) surname_case <- "asis"
    if (missing(team)) team <- "home"
    chnkmarker <- if (tolower(team)=="home") "[3PLAYERS-H]" else "[3PLAYERS-V]"
    txt <- text_chunk(txt,chnkmarker)##switch(tolower(team),
        ##home=text_chunk(txt,"[3PLAYERS-H]"),
        ##text_chunk(txt,"[3PLAYERS-V]")
        ##          )
    suppressWarnings(tryCatch({ p <- data.table::fread(txt, data.table=FALSE, sep=";", header=FALSE, na.strings="NA", logical01=FALSE) },error=function(e) { stop("could not read the ",chnkmarker," section of the input file: either the file is missing this section or perhaps the encoding argument supplied to read_dv is incorrect?") }))
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
                             upper = toupper(p$lastname),
                             lower = tolower(p$lastname),
                             title = str_to_title(p$lastname),
                             p$lastname)
    } else if (is.function(surname_case)) {
        p$lastname <- surname_case(p$lastname)
    }
    p$firstname[is.na(p$firstname)] <- ""
    p$lastname[is.na(p$lastname)] <- ""
    p$name <- str_trim(paste(p$firstname, p$lastname, sep = " "))
    ## fallback for un-named players
    idx <- which(!nzchar(p$name))
    if (length(idx) > 0) p$name[idx] <- paste0("Unnamed player ", seq_along(idx))
    p$role <- plyr::mapvalues(p$role, from = 1:6, to = c("libero", "outside", "opposite", "middle", "setter", "unknown"), warn_missing = FALSE)
    p$role[p$role %in% c("0")] <- NA_character_
    p$player_id <- as.character(p$player_id)
    p$starting_position_set1 <- as.character(p$starting_position_set1)
    p$starting_position_set2 <- as.character(p$starting_position_set2)
    p$starting_position_set3 <- as.character(p$starting_position_set3)
    p$starting_position_set4 <- as.character(p$starting_position_set4)
    p$starting_position_set5 <- as.character(p$starting_position_set5)
    p
}

## attack codes
read_attacks <- function(txt) {
    txt <- text_chunk(txt,"[3ATTACKCOMBINATION]")
    if (str_trim(txt)=="") {
        NULL
    } else {
        ##tryCatch({ p <- data.table::fread(txt, data.table=FALSE, sep=";", header=FALSE, na.strings="NA", logical01=FALSE) },error=function(e) { stop("input file could not be read: is the encoding argument supplied to read_dv correct?") })
        tryCatch({ p <- read.table(text=txt,sep=";",quote="",stringsAsFactors=FALSE) },error=function(e) { stop("could not read the [3ATTACKCOMBINATION] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to read_dv is incorrect?") })
        ## X2;2;L;Q;veloce dietro;;65280;4868;C;;
        names(p)[1] <- "code"
        names(p)[2] <- "attacker_position"
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
        suppressWarnings(tryCatch({ p <- data.table::fread(txt, data.table=FALSE, sep=";", header=FALSE, na.strings="NA", logical01=FALSE) },error=function(e) { stop("could not read the [3SETTERCALL] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to read_dv is incorrect?") }))
        names(p)[1] <- "code"
        names(p)[3] <- "description"
        p
    }
}

read_meta <- function(txt,surname_case) {
    out <- list()
    msgs <- list()
    temp <- read_match(txt)
    out$match <- temp$match
    msgs <- join_messages(msgs, temp$messages)
    out$more <- read_more(txt)
    tryCatch(out$result <- read_result(txt),
             error=function(e) warning("could not read the [3SET] section of the input file")) ## not fatal: summary method will fail if this is not parsed, but we will have issued a warning message
    tryCatch(tempteams <- read_teams(txt), error = function(e) stop("could not read the [3TEAMS] section of the input file")) ## fatal, because we need this info later
    out$teams <- tempteams$teams
    msgs <- join_messages(msgs, tempteams$messages)
    if (any(is.na(out$teams$sets_won))) {
        ## hmm, can we fill this in from the out$result section?
        try({
            temp <- out$result[which(out$result$played), ]
            temp$home_won <- ifelse(temp$score_home_team > temp$score_visiting_team, 1, 0)
            out$teams$sets_won <- c(sum(temp$home_won), nrow(temp) - sum(temp$home_won))
        }, silent = TRUE)
    }
    temp <- tryCatch(diff(out$teams$sets_won), error = function(e) NA)
    if (is.na(temp)) {
        out$teams$won_match <- c(NA, NA)
    } else if (temp < 0) {
        out$teams$won_match <- c(TRUE, FALSE)
    } else {
        out$teams$won_match <- c(FALSE, TRUE)
    }

    tryCatch(out$players_h <- read_players(txt,"home",surname_case),
             error=function(e) stop("could not read the [3PLAYERS-H] section of the input file")) ## fatal
    tryCatch(out$players_v <- read_players(txt,"visiting",surname_case),
             error=function(e) stop("could not read the [3PLAYERS-V] section of the input file")) ## fatal
    tryCatch(out$attacks <- read_attacks(txt),
             error=function(e) stop("could not read the [3ATTACKCOMBINATION] section of the input file")) ## fatal
    tryCatch(out$sets <- read_setter_calls(txt),
             error=function(e) stop("could not read the [3SETTERCALL] section of the input file")) ## fatal
    temp <- out$match
    temp$home_team <- out$teams$team[out$teams$home_away_team=="*"]
    temp$visiting_team <- out$teams$team[out$teams$home_away_team=="a"]
    out$match_id <- digest(temp)
    if (length(msgs)>0) {
        msgs <- ldply(msgs, as.data.frame)
    } else {
        msgs <- data.frame(file_line_number=integer(),video_time=integer(),message=character(),file_line=character())
    }
    list(meta=out,messages=msgs)
}


get_player_name <- function(team,number,meta) {
    out <- rep(as.character(NA),length(number))
    idx <- team %eq% "*"
    if (any(idx)) {
        out[idx] <- mapvalues(number[idx],from=meta$players_h$number,to=meta$players_h$name,warn_missing=FALSE)
        invalid_number <- idx & out==number
        out[invalid_number] <- "unknown player"
        ##if (any(invalid_number)) stop("invalid home team player number")
    }
    idx <- team %eq% "a"
    if (any(idx)) {
        out[idx] <- mapvalues(number[idx],from=meta$players_v$number,to=meta$players_v$name,warn_missing=FALSE)
        invalid_number <- idx & out==number
        out[invalid_number] <- "unknown player"
        ##if (any(invalid_number)) stop("invalid visiting team player number")
    }
    out
}

get_player_id <- function(team,number,meta) {
    out <- rep("unknown player",length(number))
    idx <- team %eq% "*"
    if (any(idx)) {
        for (pn in which(meta$players_h$number %in% number[idx])) {
            out[idx & number %eq% meta$players_h$number[pn]] <- meta$players_h$player_id[pn]
        }
        ## this does not work if player_id is same as player_number
        ##out[idx] <- mapvalues(number[idx],from=meta$players_h$number,to=meta$players_h$player_id,warn_missing=FALSE)
        ##invalid_number <- idx & out==number
        ##out[invalid_number] <- "unknown player"
    }
    idx <- team %eq% "a"
    if (any(idx)) {
        for (pn in which(meta$players_v$number %in% number[idx])) {
            out[idx & number %eq% meta$players_v$number[pn]] <- meta$players_v$player_id[pn]
        }
        ##out[idx] <- mapvalues(number[idx],from=meta$players_v$number,to=meta$players_v$player_id,warn_missing=FALSE)
        ##invalid_number <- idx & out==number
        ##out[invalid_number] <- "unknown player"
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



#' Get team names and IDs from datavolley object
#'
#' @param x datavolley or data.frame: a datavolley object as returned by \code{read_dv}, or the plays component of that object
#'
#' @return character vector of team names or IDs
#'
#' @seealso \code{\link{read_dv}}
#' 
#' @examples
#' \dontrun{
#'   x <- read_dv(dv_example_file(), insert_technical_timeouts=FALSE)
#'   teams(x)
#'   home_team_id(x)
#' }
#' @export
teams <- function(x) {
    if ("meta" %in% names(x)) {
        x$meta$teams$team
    } else {
        na.omit(unique(x$team))
    }
}

#' @rdname teams
#' @export
home_team <- function(x) {
    if ("meta" %in% names(x)) {
        x$meta$teams$team[x$meta$teams$home_away_team %eq% "*"]
    } else {
        na.omit(unique(x$home_team))
    }
}

#' @rdname teams
#' @export
home_team_id <- function(x) {
    if ("meta" %in% names(x)) {
        x$meta$teams$team_id[x$meta$teams$home_away_team %eq% "*"]
    } else {
        if (is.null(x$home_team_id)) NA_character_ else na.omit(unique(x$home_team_id))
    }
}

#' @rdname teams
#' @export
visiting_team <- function(x) {
    if ("meta" %in% names(x)) {
        x$meta$teams$team[x$meta$teams$home_away_team %eq% "a"]
    } else {
        na.omit(unique(x$visiting_team))
    }
}

#' @rdname teams
#' @export
visiting_team_id <- function(x) {
    if ("meta" %in% names(x)) {
        x$meta$teams$team_id[x$meta$teams$home_away_team %eq% "a"]
    } else {
        if (is.null(x$visiting_team_id)) NA_character_ else na.omit(unique(x$visiting_team_id))
    }
}
