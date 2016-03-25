## functions for reading match metadata (top part of the dv file)

## match details
read_match <- function(txt) {
    idx <- grep("[3MATCH]",txt,fixed=TRUE)
    p <- read.table(text=txt[idx+1],sep=";",stringsAsFactors=FALSE,header=FALSE)
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
    p$time <- hms(p$time)
    p
}

read_result <- function(txt) {
    txt <- text_chunk(txt,"[3SET]")
    p <- data.table::fread(txt,data.table=FALSE,sep=";")
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
    p <- data.table::fread(txt,data.table=FALSE,sep=";")
    names(p)[2] <- "team_name"
    names(p)[3] <- "sets_won"
    names(p)[4] <- "coach"
    names(p)[5] <- "assistant"
    p$team <- c("*","a")
    p
}

## players
read_players <- function(txt,team) {
    if (missing(team)) team <- "home"
    txt <- switch(tolower(team),
        home=text_chunk(txt,"[3PLAYERS-H]"),
        text_chunk(txt,"[3PLAYERS-V]")
        )
    p <- data.table::fread(txt,data.table=FALSE)
    names(p)[2] <- "number"
    names(p)[4] <- "starting_position_set1"
    names(p)[5] <- "starting_position_set1"
    names(p)[6] <- "starting_position_set1"
    names(p)[7] <- "starting_position_set1"
    names(p)[8] <- "starting_position_set1"
    names(p)[10] <- "lastname"
    names(p)[11] <- "firstname"
    names(p)[13] <- "special_role"
    p$name <- paste(p$firstname,p$lastname,sep=" ")
    p
}

## attack codes
read_attacks <- function(txt) {
    txt <- text_chunk(txt,"[3ATTACKCOMBINATION]")
    if (str_trim(txt)=="") {
        NULL
    } else {
        p <- data.table::fread(txt,data.table=FALSE,sep=";")
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
        p <- data.table::fread(txt,data.table=FALSE,sep=";")
        names(p)[1] <- "code"
        names(p)[3] <- "description"
        p
    }
}

read_meta <- function(txt) {
    out <- list()
    out$match <- read_match(txt)
    out$result <- read_result(txt)
    out$teams <- read_teams(txt)
    if (diff(out$teams$sets_won)<0) {
        out$teams$won_match <- c(TRUE,FALSE)
    } else {
        out$teams$won_match <- c(FALSE,TRUE)
    }
    out$players_h <- read_players(txt,"home")
    out$players_v <- read_players(txt,"visiting")
    out$attacks <- read_attacks(txt)
    out$sets <- read_setter_calls(txt)
    temp <- out$match
    temp$home_team <- out$teams$team_name[out$teams$team=="*"]
    temp$visiting_team <- out$teams$team_name[out$teams$team=="a"]
    out$match_id <- digest(temp)
    out
}


get_player_name <- function(team,number,meta) {
    team <- match.arg(team,c("a","*"))
    if (team=="*") {
        meta$players_h$name[meta$players_h$number==number]
    } else {
        meta$players_v$name[meta$players_v$number==number]
    }        
}


