## functions for reading match metadata (top part of the dv file)

roles_int2str <- function(x) {
    out <- rep(NA_character_, length(x))
    out[x %eq% 1] <- "libero"
    out[x %eq% 2] <- "outside"
    out[x %eq% 3] <- "opposite"
    out[x %eq% 4] <- "middle"
    out[x %eq% 5] <- "setter"
    out[x %eq% 6] <- "unknown"
    out
}

##roles_int2str <- function(x) {
##    out <- rep(NA_character_, length(x))
##    idx <- x %in% 1:6
##    out[idx] <- c("libero", "outside", "opposite", "middle", "setter", "unknown")[x[idx]]
##    out
##}

roles_str2int <- function(x) {
    out <- rep(0L, length(x))
    out[x %eq% "libero"] <- 1L
    out[x %eq% "outside"] <- 2L
    out[x %eq% "opposite"] <- 3L
    out[x %eq% "middle"] <- 4L
    out[x %eq% "setter"] <- 5L
    out[x %eq% "unknown"] <- 6L
    out
}

read_semi_text <- function(txt, sep = ";", fallback = "fread", ...) {
    suppressWarnings(tryCatch({
        ## quote = "" because no text in the dvw should be fully quoted, but might have quotes within team names
        if (length(txt) == 1 && !grepl("\n", txt)) txt <- paste0(txt, "\n") ## to force read_delim to treat it as literal text
        suppressWarnings(suppressMessages(out <- readr::read_delim(I(txt), delim = sep, col_names = FALSE, quote = "", locale = readr::locale(encoding = "UTF-8"), progress = FALSE, ...)))
        ## strip the extra attributes that readr adds, and convert from spec_tbl_df back to plain old tbl_df
        attr(out, "spec") <- NULL
        as_tibble(out)
    }, error = function(e) {
        if (fallback == "fread") {
            data.table::fread(txt, data.table = FALSE, sep = sep, header = FALSE, na.strings = "NA", logical01 = FALSE) ## seems to cope with embedded quotes
        } else {
            read.table(text = txt, sep = sep, quote = "", stringsAsFactors = FALSE, header = FALSE)
        }
    }))
}

## match details
read_match <- function(txt, date_format = NULL) {
    idx <- grep("[3MATCH]", txt, fixed = TRUE)
    ##tryCatch(p <- read.table(text=txt[idx+1],sep=";",quote="",stringsAsFactors=FALSE,header=FALSE),
    ##         error=function(e) { stop("could not read the [3MATCH] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to dv_read is incorrect?") })
    tryCatch(p <- read_semi_text(txt[idx + 1], fallback = "read.table"), error = function(e) stop("could not read the [3MATCH] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to dv_read is incorrect?"))
    names(p)[1] <- "date"
    names(p)[2] <- "time"
    names(p)[3] <- "season"
    names(p)[4] <- "league"
    names(p)[5] <- "phase"
    names(p)[6] <- "home_away"
    names(p)[7] <- "day_number"
    names(p)[8] <- "match_number"
    names(p)[9] <- "text_encoding"
    names(p)[10] <- "regulation" ## 0 = indoor sideout, 1 = indoor rally point, 2 = beach rally point
    names(p)[11] <- "zones_or_cones" ## C or Z, e.g. 12/08/2018;;;;;;;;1;1;Z;0;
    msgs <- list()
    ## readr will treat e.g. 001 as character not numeric
    c2n <- function(z) if (is.character(z) && !is.na(z) && !is.na(as.numeric(z))) as.numeric(z) else z
    p$day_number <- c2n(p$day_number)
    p$match_number <- c2n(p$match_number)
    if (is.na(p$date)) {
        msgs <- collect_messages(msgs, "Match information is missing the date", idx + 1, txt[idx + 1], severity = 2)
        date_was_missing <- TRUE
    } else {
        ## date can be in various formats
        temp <- manydates(p$date, preferred = date_format)
        if (length(temp) < 1) {
            ## no recognizable date
            temp <- as.Date(NA)
        } else if (length(temp) > 1) {
            ## ambiguous date format
            msgs <- collect_messages(msgs, "Ambiguous date, using DMY format", idx + 1, txt[idx + 1], severity = 2)
            temp <- temp[1] ##** can we do better here?
        }
        p$date <- temp
        if (is.na(p$date)) {
            msgs <- collect_messages(msgs, "Cannot parse the date in the match information", idx + 1, txt[idx + 1], severity = 2)
        } else {
            if (p$date < (as.Date(lubridate::now(tzone = "UTC")) - 365 * 10)) {
                ## date is more than ten years ago!
                msgs <- collect_messages(msgs, paste0("The date of the match (", format(p$date), ") is more than 10 years ago, is it correct?"), idx + 1, txt[idx + 1], severity = 2)
            }
        }
    }
    suppressWarnings(p$time <- lubridate::hms(p$time)) ## don't warn on time, because the plays object has it anyway
    if (p$regulation %eq% 0) {
        p$regulation <- "indoor sideout"
    } else if (p$regulation %eq% 1) {
        p$regulation <- "indoor rally point"
    } else if (p$regulation %eq% 2) {
        p$regulation <- "beach rally point"
    }
    list(match = p, messages = msgs)
}

read_more <- function(txt) {
    idx <- grep("[3MORE]",txt,fixed=TRUE)
    ##tryCatch(p <- read.table(text = txt[idx+1], sep = ";", quote = "", stringsAsFactors = FALSE, header = FALSE),
    ##         error=function(e) stop("could not read the [3MORE] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to dv_read is incorrect?"))
    tryCatch(p <- read_semi_text(txt[idx+1], fallback = "read.table"), error = function(e) stop("could not read the [3MORE] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to dv_read is incorrect?"))
    for (k in c(1, 4:6)) p[[k]] <- as.character(p[[k]])
    names(p)[1:6] <- c("referees", "spectators", "receipts", "city", "arena","scout")
    p
}

read_result <- function(txt) {
    txt <- text_chunk(txt,"[3SET]")
    ##suppressWarnings(tryCatch({ p <- data.table::fread(txt, data.table=FALSE, sep=";", header=FALSE, na.strings="NA", logical01=FALSE) },error=function(e) { stop("could not read the [3SET] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to dv_read is incorrect?") }))
    tryCatch(p <- read_semi_text(txt), error = function(e) stop("could not read the [3SET] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to dv_read is incorrect?"))
    names(p)[1] <- "played"
    names(p)[2] <- "score_intermediate1"
    names(p)[3] <- "score_intermediate2"
    names(p)[4] <- "score_intermediate3"
    names(p)[5] <- "score"
    names(p)[6] <- "duration"
    p$score_intermediate1 <- gsub("[[:space:]]+", "", p$score_intermediate1)
    p$score_intermediate2 <- gsub("[[:space:]]+", "", p$score_intermediate2)
    p$score_intermediate3 <- gsub("[[:space:]]+", "", p$score_intermediate3)
    p$score <- gsub("\\s+","",p$score)
    temp <- str_match(p$score,"(\\d+)\\-(\\d+)")
    p$score_home_team <- as.numeric(temp[,2])
    p$score_visiting_team <- as.numeric(temp[,3])
    p <- p[p$score!="",]
    p[rowSums(is.na(p)) < ncol(p), ] ## discard all-NA rows
}

## teams
read_teams <- function(txt) {
    idx <- grep("[3TEAMS]", txt, fixed = TRUE)
    txt0 <- txt
    txt <- text_chunk(txt, "[3TEAMS]")
    msgs <- list()
    ##suppressWarnings(tryCatch({ p <- data.table::fread(txt, data.table=FALSE,sep=";", header=FALSE, na.strings="NA", logical01=FALSE) },error=function(e) { stop("could not read the [3TEAMS] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to dv_read is incorrect?") }))
    tryCatch(p <- read_semi_text(txt), error = function(e) stop("could not read the [3TEAMS] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to dv_read is incorrect?"))
    names(p)[1] <- "team_id"
    names(p)[2] <- "team"
    names(p)[3] <- "sets_won"
    names(p)[4] <- "coach"
    names(p)[5] <- "assistant"
    if (ncol(p) > 5) names(p)[6] <- "shirt_colour"
    try(p$shirt_colour <- dv_int2rgb(p$shirt_colour), silent = TRUE)
    p$home_away_team <- c("*","a")
    p$team_id <- str_trim(as.character(p$team_id)) ## force to be char
    p$team <- str_trim(p$team)
    suppressWarnings(p$sets_won <- as.integer(p$sets_won))
    ## check for missing team names
    if (is.na(p$team[1]) || !nzchar(p$team[1])) {
        msgs <- collect_messages(msgs, "The home team name is missing", idx+1, txt0[idx + 1], severity = 1)
        p$team[1] <- "Unknown team"
    }
    if (is.na(p$team[2]) || !nzchar(p$team[2])) {
        msgs <- collect_messages(msgs, "The visiting team name is missing", idx+2, txt0[idx + 2], severity = 1)
        p$team[2] <- "Unknown team"
    }
    ## check for identical team names
    if (p$team[1] %eq% p$team[2]) {
        msgs <- collect_messages(msgs, "The two team names are identical. They will be modified here but this may still cause problems", idx+1, txt0[idx+1], severity = 1)
        p$team[1] <- paste0(p$team[1]," (home)")
        p$team[2] <- paste0(p$team[2]," (visiting)")
    }
    if (p$team_id[1] %eq% p$team_id[2]) {
        msgs <- collect_messages(msgs, "The two team IDs are identical. They will be modified here but this may still cause problems", idx+1, txt0[idx+1], severity = 1)
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
    txt <- text_chunk(txt, chnkmarker)
##    suppressWarnings(tryCatch({ p <- data.table::fread(txt, data.table=FALSE, sep=";", header=FALSE, na.strings="NA", logical01=FALSE) },error=function(e) { stop("could not read the ",chnkmarker," section of the input file: either the file is missing this section or perhaps the encoding argument supplied to dv_read is incorrect?") }))
    tryCatch(p <- read_semi_text(txt), error = function(e) stop("could not read the ",chnkmarker," section of the input file: either the file is missing this section or perhaps the encoding argument supplied to dv_read is incorrect?"))
    if (ncol(p) < 1) p <- as_tibble(setNames(as.data.frame(matrix(nrow = 0, ncol = 18)), paste0("X", 1:18)))
    names(p)[c(2, 4:15)] <- c("number", "starting_position_set1", "starting_position_set2", "starting_position_set3", "starting_position_set4", "starting_position_set5", "player_id", "lastname", "firstname", "nickname", "special_role", "role", "foreign")
    if (is.character(surname_case)) {
        p$lastname <- switch(tolower(surname_case),
                             upper = toupper(p$lastname),
                             lower = tolower(p$lastname),
                             title = str_to_title(p$lastname),
                             p$lastname)
    } else if (is.function(surname_case)) {
        p$lastname <- surname_case(p$lastname)
    }
    p$nickname <- str_trim(p$nickname)
    p$nickname[is.na(p$nickname)] <- ""
    p$firstname[is.na(p$firstname)] <- ""
    p$lastname[is.na(p$lastname)] <- ""
    p$firstname <- str_trim(p$firstname)
    p$lastname <- str_trim(p$lastname)
    p$name <- str_trim(paste(p$firstname, p$lastname, sep = " "))
    ## fallback for un-named players
    idx <- which(!nzchar(p$name))
    if (length(idx) > 0) p$name[idx] <- paste0("Unnamed player ", seq_along(idx))
    ##p$role <- plyr::mapvalues(p$role, from = 1:6, to = c("libero", "outside", "opposite", "middle", "setter", "unknown"), warn_missing = FALSE)
    ##p$role[p$role %in% c("0")] <- NA_character_
    p$role <- roles_int2str(p$role)
    p$player_id <- str_trim(as.character(p$player_id))
    p$starting_position_set1 <- str_trim(as.character(p$starting_position_set1))
    p$starting_position_set2 <- str_trim(as.character(p$starting_position_set2))
    p$starting_position_set3 <- str_trim(as.character(p$starting_position_set3))
    p$starting_position_set4 <- str_trim(as.character(p$starting_position_set4))
    p$starting_position_set5 <- str_trim(as.character(p$starting_position_set5))
    p$foreign[is.na(p$foreign)] <- FALSE
    p$number <- as.integer(p$number)
    p
}

## attack codes
read_attacks <- function(txt) {
    txt <- text_chunk(txt,"[3ATTACKCOMBINATION]")
    if (str_trim(txt)=="") {
        NULL
    } else {
        ##tryCatch({ p <- read.table(text=txt,sep=";",quote="",header=FALSE,stringsAsFactors=FALSE) },error=function(e) { stop("could not read the [3ATTACKCOMBINATION] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to dv_read is incorrect?") })
        tryCatch(p <- read_semi_text(txt, fallback = "read.table"), error = function(e) stop("could not read the [3ATTACKCOMBINATION] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to dv_read is incorrect?"))
        ## X2;2;L;Q;veloce dietro;;65280;4868;C;;
        names(p)[1:9] <- c("code", "attacker_position", "side", "type", "description", "X6", "colour", "start_coordinate", "set_type")
        try(p$colour <- dv_int2rgb(p$colour))
        p
    }
}

read_setter_calls <- function(txt) {
    txt <- text_chunk(txt,"[3SETTERCALL]")
    if (!nzchar(str_trim(txt))) {
        NULL
    } else {
        ## with read_semi_text, need to force col 9 to be char (it's a comma-separated string of ints) else it gets parsed into a single number
        tryCatch(p <- read_semi_text(txt, col_types = "c?c??iiic??"), error = function(e) stop("could not read the [3SETTERCALL] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to dv_read is incorrect?"))
        names(p)[1:10] <- c("code", "X2", "description", "X4", "colour", "start_coordinate", "mid_coordinate", "end_coordinate", "path", "path_colour")
        ## V9 is a comma-separated list of indices that give a path
        try(p$colour <- dv_int2rgb(p$colour))
        try(p$path_colour <- dv_int2rgb(p$path_colour))
        p
    }
}

read_video <- function(txt) {
    txt <- text_chunk(txt,"[3VIDEO]")
    p <- data.frame(camera = character(), file = character(), stringsAsFactors = FALSE)
    if (nzchar(str_trim(txt))) {
        p <- tryCatch(
            suppressWarnings({
                p <- read_semi_text(txt, sep = "=", fallback = "read.table")
                if (ncol(p) > 2) {
                    ## grrr, video file (url?) had an = in it
                    txt <- strsplit(txt, "\n")[[1]] ## re-split
                    txt <- stringr::str_match(txt, "^([^=]*)=(.*)$")
                    p <- data.frame(camera = txt[, 2], file = txt[, 3], stringsAsFactors = FALSE)
                } else {
                    colnames(p) <- c("camera", "file")
                }
                p
            }), error = function(e) {
            warning("could not read the [3VIDEO] section of the input file")
        })
    }
    p
}

read_winning_symbols <- function(txt) {
    txt <- str_trim(text_chunk(txt,"[3WINNINGSYMBOLS]"))
    if (nzchar(txt)) tryCatch(winning_symbols_df(txt), error = function(e) "") else ""
}

winning_symbols_df <- function(txt) {
    if (!is.character(txt) || length(txt) != 1 || nchar(txt) != 56) stop("unexpected format for winning_symbols string")
    parts <- as.list(stringr::str_sub(txt, seq(1, 7*8, by = 8), seq(8, 7 * 8, by = 8)))
    names(parts) <- c("S", "R", "A", "B", "D", "E", "F")
    f <- function(z, start, end) strsplit(gsub("~", "", substr(z, start, end), ""), "")[[1]]
    do.call(rbind, lapply(names(parts), function(nm) {
        l <- f(parts[[nm]], 1, 4)
        w <- f(parts[[nm]], 5, 8)
        if (length(l) < 1 && length(w) < 1) return(NULL)
        tibble(skill = nm, win_lose = c(rep("L", length(l)), rep("W", length(w))), code = c(l, w))
    }))
}

winning_symbols_df2txt <- function(x) {
    if (!is.data.frame(x) || !setequal(names(x), c("skill", "win_lose", "code")) || !all(c("S", "R", "A", "B", "D", "E", "F") %in% x$skill)) stop("input in unexpected format")
    tildepad <- function(z) paste0(paste0(z, collapse = ""), paste0(rep("~", 4 - length(z)), collapse = ""))
    out <- vapply(c("S", "R", "A", "B", "D", "E", "F"), function(z) {
        paste0(tildepad(x$code[x$skill == z & x$win_lose == "L"]), tildepad(x$code[x$skill == z & x$win_lose == "W"]))
    }, FUN.VALUE = "", USE.NAMES = FALSE)
    paste0(out, collapse = "")
}

read_comments <- function(txt) {
    txt <- text_chunk(txt,"[3COMMENTS]")
    ## default to NA comments
    p <- setNames(as.data.frame(rep(list(NA_character_), 5)), paste0("comment_", 1:5))
    if (nzchar(str_trim(txt))) {
        p <- tryCatch(suppressWarnings({
            tmp <- read_semi_text(txt, fallback = "read.table")
            setNames(tmp, paste0("comment_", seq_len(ncol(tmp))))
        }), error = function(e) {
            warning("could not read the [3COMMENTS] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to dv_read is incorrect?")
        })
    }
    p
}

read_meta <- function(txt, surname_case, date_format = NULL) {
    out <- list()
    msgs <- list()
    temp <- read_match(txt, date_format = date_format)
    out$match <- temp$match
    msgs <- join_messages(msgs, temp$messages)
    out$more <- read_more(txt)
    out$comments <- read_comments(txt)
    tryCatch(out$result <- read_result(txt),
             error=function(e) warning("could not read the [3SET] section of the input file")) ## not fatal: summary method will fail if this is not parsed, but we will have issued a warning message
    tryCatch(tempteams <- read_teams(txt), error = function(e) stop("could not read the [3TEAMS] section of the input file")) ## fatal, because we need this info later
    out$teams <- tempteams$teams
    msgs <- join_messages(msgs, tempteams$messages)
    if (any(is.na(out$teams$sets_won))) {
        ## hmm, can we fill this in from the out$result section?

        regn <- out$match$regulation
        if (!is.null(regn)) {
            last_set_min_score <- 15L
            if (regn %in% "indoor rally point") {
                min_score <- 25L
                last_set <- 5L
            } else if (regn %in% "indoor sideout") {
                min_score <- 25L
                last_set <- 5L
            } else if (grepl("beach", regn)) {
                min_score <- 21L
                last_set <- 3L
            } else {
                regn <- NULL
            }
        }
        try({
            temp <- out$result[which(out$result$played), ]
            temp$set_number <- seq_len(nrow(temp))
            ## home team won
            temp$home_won <- ifelse(temp$score_home_team > (temp$score_visiting_team + 1L) & (temp$score_home_team >= min_score | (temp$set_number == last_set & temp$score_home_team >= last_set_min_score)), 1L, NA_integer_)
            ## visiting team won
            temp$home_won <- ifelse(temp$score_visiting_team > (temp$score_home_team + 1L) & (temp$score_visiting_team >= min_score | (temp$set_number == last_set & temp$score_visiting_team >= last_set_min_score)), 0L, temp$home_won)
            out$teams$sets_won <- c(sum(temp$home_won == 1L, na.rm = TRUE), sum(temp$home_won == 0L, na.rm = TRUE))
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
    tryCatch(out$winning_symbols <- read_winning_symbols(txt), error = function(e) warning("could not read the [3WINNINGSYMBOLS] section of the input file")) ## not fatal
    out$match_id <- dv_create_meta_match_id(out)
    if (length(msgs) > 0) {
        msgs <- bind_rows(msgs)
    } else {
        msgs <- data.frame(file_line_number = integer(), video_time = numeric(), message = character(), file_line = character())
    }
    out$video <- read_video(txt)
    list(meta=out,messages=msgs)
}

get_player_name <- function(team,number,meta) {
    out <- rep(NA_character_, length(number))
    idx <- team %eq% "*"
    if (any(idx)) {
        out[idx] <- mapvalues(number[idx],from=meta$players_h$number,to=meta$players_h$name,warn_missing=FALSE)
        invalid_number <- idx & out==number
        out[invalid_number] <- "unknown player"
    }
    idx <- team %eq% "a"
    if (any(idx)) {
        out[idx] <- mapvalues(number[idx],from=meta$players_v$number,to=meta$players_v$name,warn_missing=FALSE)
        invalid_number <- idx & out==number
        out[invalid_number] <- "unknown player"
    }
    out
}

get_player_id <- function(team, number, meta) {
    out <- rep("unknown player", length(number))
    idx <- team %eq% "*"
    if (any(idx)) {
        for (pn in which(meta$players_h$number %in% number[idx])) {
            out[idx & number %eq% meta$players_h$number[pn]] <- meta$players_h$player_id[pn]
        }
    }
    idx <- team %eq% "a"
    if (any(idx)) {
        for (pn in which(meta$players_v$number %in% number[idx])) {
            out[idx & number %eq% meta$players_v$number[pn]] <- meta$players_v$player_id[pn]
        }
    }
    out
}


## #' Provide descriptions for attack codes
## #'
## #' DataVolley files generally contain a set of attack codes and corresponding descriptions (in Italian).
## #' If you use these codes differently, or wish to change the descriptions, you can pass a customised
## #' attack_code_describe function to \code{\link{dv_read}}
## #'
## #' @param code string: two-character attack code
## #' @param translation string: description of the corresponding attack
## #' @param show_map logical: if TRUE, return the whole table being used to map codes to descriptions
## #'
## #' @return string giving the description of that attack code
## #'
## #' @seealso \code{\link{dv_read}}
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
#' @param x datavolley or data.frame: a datavolley object as returned by \code{dv_read}, or the plays component of that object
#'
#' @return character vector of team names or IDs
#'
#' @seealso \code{\link{dv_read}}
#'
#' @examples
#' \dontrun{
#'   x <- dv_read(dv_example_file(), insert_technical_timeouts = FALSE)
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

## these functions defined identically in the ovideo package

#' Get or set the video metadata in a datavolley object
#'
#' @param x datavolley: a datavolley object as returned by [datavolley::dv_read()]
#' @param value string or data.frame: a string containing the path to the video file, or a data.frame with columns "camera" and "file"
#'
#' @return For `dv_meta_video`, the existing video metadata. For `dv_meta_video<-`, the video metadata value in `x` is changed
#'
#' @examples
#' x <- dv_read(dv_example_file())
#' dv_meta_video(x) ## empty dataframe
#' dv_meta_video(x) <- "/path/to/my/videofile"
#' dv_meta_video(x)
#'
#' @export
dv_meta_video <- function(x) {
    x$meta$video
}

#' @rdname dv_meta_video
#' @export
`dv_meta_video<-` <- function(x, value) {
    if (is.character(value)) {
        x$meta$video <- data.frame(camera = seq_along(value) - 1L, file = value, stringsAsFactors = FALSE)
    } else if (is.data.frame(value)) {
        if (!"file" %in% names(value)) stop("expecting 'file' column in the new value")
        x$meta$video <- value
    } else {
        stop("unexpected input value format")
    }
    x
}
