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

withNames <- function(w, nms = c()) {
    if (length(nms) < length(w)) nms <- c(nms, paste0("X", seq_len(length(w) - length(nms)) + length(nms)))
    if (length(nms) > length(w)) nms <- nms[seq_along(w)]
    setNames(w, nms)
}
mt2na <- function(z) {
    z[!nzchar(z)] <- NA_character_
    if (all(is.na(z))) as.logical(z) else z ## for backwards compatibility, all-NA columns to be of type logical
}
safe_as <- function(z, fun) tryCatch(suppressWarnings(fun(z)), error = function(e) z)
safe_as_num <- function(z) safe_as(z, as.numeric)
safe_as_condnum <- function(z) { ## conditional conversion to numeric
    if (all(is.na(z))) {
        ## don't convert all-NAs
        z
    } else {
        temp <- safe_as(z, as.numeric)
        if (all(is.na(temp) == (is.na(z) | !nzchar(z)))) temp else z
    }
}
## safe_as_condnum(c(" 1", "a"))
## safe_as_condnum(c(" 1", "2"))
## safe_as_condnum(c(" 1", ""))
safe_as_log <- function(z) safe_as(z, as.logical)
safe_as_int <- function(z) safe_as(z, as.integer)
safe_as_char <- function(z) safe_as(z, as.character)

read_semi_text2 <- function(txt, types = NULL, nms = NULL) {
    ## types can be "n" (numeric), "i" (integer), "l" (logical), "Cn" (conditional numeric: only convert if can be made non-NA numeric)
    temp <- bind_rows(lapply(stringi::stri_split(txt, regex = "[\r\n]+")[[1]], function(z) withNames(as.list(str_trim(stringi::stri_split(z, fixed = ";")[[1]])), nms = nms)))
    temp <- temp %>% mutate(across(everything(), mt2na))
    if (length(types) > 0) types <- types[names(types) %in% names(temp)]
    if (length(types) > 0) {
        if (!all(types %in% c("n", "l", "i", "Cn", "c"))) stop("unexpected type(s): ", setdiff(types, c("n", "l", "i", "Cn", "c")))
        if (any(types == "n")) temp <- temp %>% mutate(across(all_of(names(types)[types == "n"]), safe_as_num))
        if (any(types == "Cn")) temp <- temp %>% mutate(across(all_of(names(types)[types == "Cn"]), safe_as_condnum))
        if (any(types == "l")) temp <- temp %>% mutate(across(all_of(names(types)[types == "l"]), safe_as_log))
        if (any(types == "i")) temp <- temp %>% mutate(across(all_of(names(types)[types == "i"]), safe_as_int))
        if (any(types == "c")) temp <- temp %>% mutate(across(all_of(names(types)[types == "c"]), safe_as_char))
    }
    temp
}

## older, slower version that is only now used in read_video
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

find_section_idx <- function(section, txt, required = TRUE) {
    idx <- grep(section, txt, fixed = TRUE)
    if (required && length(idx) < 1) stop("could not read the ", section, " section of the input file, it is missing from the input file")
    if (length(idx) > 1) {
        if (required) stop("could not read the ", section, " section of the input file, there are multiple section headers in the input file")
        ## otherwise use the first?
        idx <- integer()
    }
    idx
}
## match details
read_match <- function(txt, date_format = NULL) {
    idx <- find_section_idx("[3MATCH]", txt)
    msgs <- list()
    tryCatch(p2 <- read_semi_text2(txt[idx + 1], types = c(match_number = "Cn", day_number = "Cn", regulation = "n"), nms = c("date", "time", "season", "league", "phase", "home_away", "day_number", "match_number", "text_encoding", "regulation", "zones_or_cones")), error = function(e) stop("could not read the [3MATCH] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to dv_read is incorrect?"))
    ## arguably season, league, phase, and home_away might also be type "Cn" because previously they were left to readr and would have been type numeric if they were numbers
    p2 <- process_dv_utf8(p2, from = 13:15, to = c("league", "phase", "home_away")) ## use UTF8 columns if available
    ## backwards compatibility, text encoding was numeric if it was a number
    temp <- suppressWarnings(as.numeric(p2$text_encoding))
    if (nzchar(p2$text_encoding) && tryCatch(isTRUE(temp == p2$text_encoding), error = function(e) FALSE)) p2$text_encoding <- temp
    c2n <- function(z) if (is.character(z) && !is.na(z) && !is.na(suppressWarnings(as.numeric(z)))) as.numeric(z) else z
    if (is.na(p2$date)) {
        msgs <- collect_messages(msgs, "Match information is missing the date", idx + 1, txt[idx + 1], severity = 2)
        date_was_missing <- TRUE
    } else {
        ## date can be in various formats
        temp <- manydates(p2$date, preferred = date_format)
        if (length(temp) < 1) {
            ## no recognizable date
            temp <- as.Date(NA)
        } else if (length(temp) > 1) {
            ## ambiguous date format
            msgs <- collect_messages(msgs, "Ambiguous date, using DMY format", idx + 1, txt[idx + 1], severity = 2)
            temp <- temp[1] ##** can we do better here?
        }
        p2$date <- temp
        if (is.na(p2$date)) {
            msgs <- collect_messages(msgs, "Cannot parse the date in the match information", idx + 1, txt[idx + 1], severity = 2)
        }
    }
    suppressWarnings(p2$time <- lubridate::hms(p2$time)) ## don't warn on time, because the plays object has it anyway
    if (p2$regulation %eq% 0) {
        p2$regulation <- "indoor sideout"
    } else if (p2$regulation %eq% 1) {
        p2$regulation <- "indoor rally point"
    } else if (p2$regulation %eq% 2) {
        p2$regulation <- "beach rally point"
    }
    if (isTRUE(p2$date < (as.Date(lubridate::now(tzone = "UTC")) - 365 * 10)) && !grepl("sideout", p2$regulation)) {
        ## date is more than ten years ago!
        msgs <- collect_messages(msgs, paste0("The date of the match (", format(p2$date), ") is more than 10 years ago, is it correct?"), idx + 1, txt[idx + 1], severity = 2)
    }
    list(match = p2, messages = msgs)
}

read_more <- function(txt) {
    idx <- find_section_idx("[3MORE]", txt)
    tryCatch(p2 <- read_semi_text2(txt[idx + 1], types = c(spectators = "Cn", receipts = "Cn", referees = "c", city = "c", arena = "c", scout = "c"), nms = c("referees", "spectators", "receipts", "city", "arena", "scout")), error = function(e) stop("could not read the [3MORE] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to dv_read is incorrect?"))
    process_dv_utf8(p2, from = 7:10, to = c("referees", "city", "arena", "scout"), na_is = NA_character_) ## use UTF8 columns if available
}

read_result <- function(txt) {
    find_section_idx("[3SET]", txt) ## just to check that it can be found
    txt <- text_chunk(txt, "[3SET]")
    tryCatch(p2 <- read_semi_text2(txt, types = c(played = "l", duration = "n"), nms = c("played", "score_intermediate1", "score_intermediate2", "score_intermediate3", "score", "duration")), error = function(e) stop("could not read the [3SET] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to dv_read is incorrect?"))
    p2 <- p2 %>% mutate(score_intermediate1 = gsub("[[:space:]]+", "", .data$score_intermediate1), score_intermediate2 = gsub("[[:space:]]+", "", .data$score_intermediate2), score_intermediate3 = gsub("[[:space:]]+", "", .data$score_intermediate3))
    p2$score <- gsub("\\s+","",p2$score)
    temp <- str_match(p2$score,"(\\d+)\\-(\\d+)")
    p2$score_home_team <- suppressWarnings(as.numeric(temp[, 2]))
    p2$score_visiting_team <- suppressWarnings(as.numeric(temp[, 3]))
    p2 <- p2[nzchar(p2$score) & !is.na(p2$score), ]
    p2[rowSums(is.na(p2)) < ncol(p2), ] ## discard all-NA rows
}

## teams
read_teams <- function(txt) {
    idx <- find_section_idx("[3TEAMS]", txt)
    txt0 <- txt
    txt <- text_chunk(txt, "[3TEAMS]")
    msgs <- list()
    tryCatch(p2 <- read_semi_text2(txt, types = c(sets_won = "i", shirt_colour = "i"), nms = c("team_id", "team", "sets_won", "coach", "assistant", "shirt_colour")), error = function(e) stop("could not read the [3TEAMS] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to dv_read is incorrect?"))
    try(p2$shirt_colour <- dv_int2rgb(p2$shirt_colour), silent = TRUE)
    p2 <- process_dv_utf8(p2, from = 7:9, to = c("team", "coach", "assistant")) ## use UTF8 columns if available
    p2$home_away_team <- c("*", "a")
    ## check for missing team names
    if (is.na(p2$team[1]) || !nzchar(p2$team[1])) {
        msgs <- collect_messages(msgs, "The home team name is missing", idx+1, txt0[idx + 1], severity = 1)
        p2$team[1] <- "Unknown team"
    }
    if (is.na(p2$team[2]) || !nzchar(p2$team[2])) {
        msgs <- collect_messages(msgs, "The visiting team name is missing", idx+2, txt0[idx + 2], severity = 1)
        p2$team[2] <- "Unknown team"
    }
    ## check for identical team names
    if (p2$team[1] %eq% p2$team[2]) {
        msgs <- collect_messages(msgs, "The two team names are identical. They will be modified here but this may still cause problems", idx+1, txt0[idx+1], severity = 1)
        p2$team[1] <- paste0(p2$team[1]," (home)")
        p2$team[2] <- paste0(p2$team[2]," (visiting)")
    }
    if (p2$team_id[1] %eq% p2$team_id[2]) {
        msgs <- collect_messages(msgs, "The two team IDs are identical. They will be modified here but this may still cause problems", idx+1, txt0[idx+1], severity = 1)
        p2$team_id[1] <- paste0(p2$team_id[1]," (home)")
        p2$team_id[2] <- paste0(p2$team_id[2]," (visiting)")
    }
    list(teams = p2, messages = msgs)
}

## players
read_players <- function(txt,team,surname_case) {
    if (missing(surname_case)) surname_case <- "asis"
    if (missing(team)) team <- "home"
    chnkmarker <- if (tolower(team)=="home") "[3PLAYERS-H]" else "[3PLAYERS-V]"
    find_section_idx(chnkmarker, txt)
    txt <- text_chunk(txt, chnkmarker)
    tryCatch(p2 <- read_semi_text2(txt, types = c(foreign = "l", number = "i", starting_position_set1 = "c", starting_position_set2 = "c", starting_position_set3 = "c", starting_position_set4 = "c", starting_position_set5 = "c"), nms = c("X1", "number", "X3", "starting_position_set1", "starting_position_set2", "starting_position_set3", "starting_position_set4", "starting_position_set5", "player_id", "lastname", "firstname", "nickname", "special_role", "role", "foreign")), error = function(e) stop("could not read the ", chnkmarker, " section of the input file: either the file is missing this section or perhaps the encoding argument supplied to dv_read is incorrect?"))
    if (ncol(p2) < 2) {
        p2 <- as_tibble(setNames(as.data.frame(matrix(nrow = 0, ncol = 18)), paste0("X", 1:18)))
        names(p2)[c(2, 4:15)] <- c("number", "starting_position_set1", "starting_position_set2", "starting_position_set3", "starting_position_set4", "starting_position_set5", "player_id", "lastname", "firstname", "nickname", "special_role", "role", "foreign")
        p2 <- p2 %>% mutate(p2, across(all_of(c("starting_position_set1", "starting_position_set2", "starting_position_set3", "starting_position_set4", "starting_position_set5", "player_id", "lastname", "firstname", "nickname", "special_role", "role")), as.character),
                            foreign = character())
    }
    p2 <- process_dv_utf8(p2, from = 18:20, to = c("lastname", "firstname", "nickname"), na_is = NA_character_) ## use UTF8 columns if available before manipulating names
    if (is.character(surname_case)) {
        p2$lastname <- switch(tolower(surname_case),
                              upper = toupper(p2$lastname),
                              lower = tolower(p2$lastname),
                              title = str_to_title(p2$lastname),
                              p2$lastname)
    } else if (is.function(surname_case)) {
        p2$lastname <- surname_case(p2$lastname)
    }
    p2$nickname[is.na(p2$nickname)] <- ""
    p2$firstname[is.na(p2$firstname)] <- ""
    p2$lastname[is.na(p2$lastname)] <- ""
    p2$name <- str_trim(paste(p2$firstname, p2$lastname, sep = " "))
    ## fallback for un-named players
    idx <- which(!nzchar(p2$name))
    if (length(idx) > 0) p2$name[idx] <- paste0("Unnamed player ", seq_along(idx))
    p2$role <- roles_int2str(p2$role)
    p2$foreign[is.na(p2$foreign) | !nzchar(p2$foreign)] <- FALSE
    p2
}

## attack codes
read_attacks <- function(txt) {
    msgs <- list()
    txt <- text_chunk(txt, "[3ATTACKCOMBINATION]")
    if (!nzchar(str_trim(txt))) {
        list(attacks = NULL, messages = NULL)
    } else {
        tryCatch(p2 <- read_semi_text2(txt, types = c(attacker_position = "n", start_coordinate = "i", colour = "i"), nms = c("code", "attacker_position", "side", "type", "description", "X6", "colour", "start_coordinate", "set_type")), error = function(e) stop("could not read the [3ATTACKCOMBINATION] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to dv_read is incorrect?"))
        ## X2;2;L;Q;veloce dietro;;65280;4868;C;;
        p2 <- dplyr::distinct(p2)
        if (any(duplicated(p2$code))) {
            msgs <- collect_messages(msgs, "At least one attack combination code in the [3ATTACKCOMBINATION] section is duplicated, ignoring duplicate entries", severity = 2)
            p2 <- p2[!duplicated(p2$code), ]
        }
        try(p2$colour <- dv_int2rgb(p2$colour))
        list(attacks = p2, messages = msgs)
    }
}

read_setter_calls <- function(txt) {
    msgs <- list()
    txt <- text_chunk(txt, "[3SETTERCALL]")
    if (!nzchar(str_trim(txt))) {
        list(sets = NULL, messages = NULL)
    } else {
        tryCatch(p2 <- read_semi_text2(txt, types = c(code = "c", description = "c", path = "c", start_coordinate = "i", mid_coordinate = "i", end_coordinate = "i", path_colour = "i", colour = "i"), nms = c("code", "X2", "description", "X4", "colour", "start_coordinate", "mid_coordinate", "end_coordinate", "path", "path_colour")), error = function(e) stop("could not read the [3SETTERCALL] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to dv_read is incorrect?"))
        p2 <- dplyr::distinct(p2)
        if (any(duplicated(p2$code))) {
            msgs <- collect_messages(msgs, "At least one setter call code in the [3SETTERCALL] section is duplicated, ignoring duplicate entries", severity = 2)
            p2 <- p2[!duplicated(p2$code), ]
        }
        ## 'path' column is a comma-separated list of indices that give a path
        try(p2$colour <- dv_int2rgb(p2$colour))
        try(p2$path_colour <- dv_int2rgb(p2$path_colour))
        list(sets = p2, messages = msgs)
    }
}

read_video <- function(txt) {
    txt <- text_chunk(txt, "[3VIDEO]")
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
    txt <- str_trim(text_chunk(txt, "[3WINNINGSYMBOLS]"))
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
    if (!is.data.frame(x) || !setequal(names(x), c("skill", "win_lose", "code"))) stop("input in unexpected format")
    tildepad <- function(z) paste0(paste0(z, collapse = ""), paste0(rep("~", 4 - length(z)), collapse = ""))
    out <- vapply(c("S", "R", "A", "B", "D", "E", "F"), function(z) {
        paste0(tildepad(x$code[x$skill == z & x$win_lose == "L"]), tildepad(x$code[x$skill == z & x$win_lose == "W"]))
    }, FUN.VALUE = "", USE.NAMES = FALSE)
    paste0(out, collapse = "")
}

read_comments <- function(txt) {
    txt <- text_chunk(txt, "[3COMMENTS]")
    ## default to NA comments
    p2 <- setNames(as.data.frame(rep(list(NA_character_), 5)), paste0("comment_", 1:5))
    if (nzchar(str_trim(txt))) {
        p2 <- tryCatch(read_semi_text2(txt, nms = paste0("comment_", 1:10)), error = function(e) warning("could not read the [3COMMENTS] section of the input file: either the file is missing this section or perhaps the encoding argument supplied to dv_read is incorrect?"))
        ## collapse multiple lines into single
        if (nrow(p2) > 1) {
            p2 <- p2 %>% dplyr::summarize(across(everything(), function(z) paste(Filter(Negate(is.na), z), collapse = "\n")))
        }
        p2[!nzchar(p2)] <- NA_character_
    }
    p2
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

    tryCatch(out$players_h <- read_players(txt, "home", surname_case), error = function(e) stop("could not read the [3PLAYERS-H] section of the input file")) ## fatal
    tryCatch(out$players_v <- read_players(txt, "visiting", surname_case), error = function(e) stop("could not read the [3PLAYERS-V] section of the input file")) ## fatal
    tryCatch(temp <- read_attacks(txt), error = function(e) stop("could not read the [3ATTACKCOMBINATION] section of the input file")) ## fatal
    out$attacks <- temp$attacks
    msgs <- join_messages(msgs, temp$messages)
    tryCatch(temp <- read_setter_calls(txt), error = function(e) stop("could not read the [3SETTERCALL] section of the input file")) ## fatal
    out$sets <- temp$sets
    msgs <- join_messages(msgs, temp$messages)
    tryCatch(out$winning_symbols <- read_winning_symbols(txt), error = function(e) warning("could not read the [3WINNINGSYMBOLS] section of the input file")) ## not fatal
    out$match_id <- dv_create_meta_match_id(out)
    if (length(msgs) > 0) {
        msgs <- bind_rows(msgs)
    } else {
        msgs <- data.frame(file_line_number = integer(), video_time = numeric(), message = character(), file_line = character())
    }
    out$video <- read_video(txt)
    list(meta = out, messages = msgs)
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
P2^Secondo tocco di la
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
