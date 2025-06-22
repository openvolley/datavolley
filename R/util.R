## Accumulate messages for later display
## Internal function, not exported
## severity: 1 = critical
##           2 = informative, may lead to misinterpretation of data
##           3 = minor, esp. those that might have resulted from selective post-processing of combo codes
collect_messages <- function(msgs, msg_text, line_nums, raw_lines, severity, fatal = FALSE, xraw) {
    if (missing(line_nums)) line_nums <- NULL
    if (missing(severity)) severity <- NULL
    if (is.data.frame(msg_text)) {
        if (is.null(line_nums) || all(is.na(line_nums))) line_nums <- msg_text$line_number
        if (is.null(severity) || all(is.na(severity))) severity <- msg_text$severity
        msg_text <- msg_text$message
    }
    n <- length(msg_text)
    if (is.null(line_nums)) line_nums <- rep(NA_integer_, n)
    if (missing(raw_lines)) {
        raw_lines <- rep(NA_character_, n)
        if (!missing(xraw)) {
            raw_lines[!is.na(line_nums)] <- xraw[line_nums[!is.na(line_nums)]]
        }
    }
    if (is.null(severity)) severity <- NA_integer_
    if (length(severity) == 1 & n > 1) severity <- rep(severity, n)
    vt <- rep(NA_real_, n)
    if (!all(is.na(raw_lines))) vt <- video_time_from_raw(raw_lines)
    raw_lines[is.na(raw_lines)] <- "[unknown]"
    if (fatal) {
        lnt <- as.character(line_nums)
        lnt[is.na(lnt)] <- "[unknown]"
        txt <- paste0("line ", lnt, ": ", msg_text, " (line in file is: \"", raw_lines, "\")")
        if (fatal) stop(paste(txt, collapse = " / "))
    } else {
        msgs[[length(msgs)+1]] <- list(file_line_number = line_nums, video_time = vt, message = msg_text, file_line = raw_lines, severity = severity)
    }
    msgs
}

## messages stored as attributes of an object
get_dvmsg <- function(x) attr(x, "dvmessages", exact = TRUE)
set_dvmsg <- function(x, msg) {
    attr(x, "dvmessages") <- msg
    x
}
has_dvmsg <- function(x) !is.null(get_dvmsg(x)) && nrow(get_dvmsg(x)) > 0
clear_dvmsg <- function(x) set_dvmsg(x, NULL)

## video time from raw line, for datavolley format
raw_vt_dv <- function(z) {
    tryCatch({
        if (!is.null(z) && is.character(z) && nzchar(z) && !is.na(z)) {
            as.numeric(strsplit(z, ";")[[1]][13])
        } else {
            NA_integer_
        }},
        error = function(e) NA_integer_)
}

## video time from raw line, for peranavolley format
pv_pparse <- function(z, df = TRUE) {
    temp <- sub("^[A-Z]+~", "", z)
    if (grepl("^\\(?null\\)?", temp, ignore.case = TRUE)) {
        if (df) tibble() else NULL
    } else {
        jsonlite::fromJSON(temp)
    }
}
raw_vt_pv <- function(z) {
    tryCatch({
        if (!is.null(z) && is.character(z) && nzchar(z) && !is.na(z)) {
            as.numeric(pv_pparse(z)$videoDuration)
        } else {
            NA_integer_
        }
    }, error = function(e) NA_integer_)
}

## video time from raw line, vsm format
## can't guarantee that "lines" in vsm are valid json, because we somewhat-arbitrarily split the single-line input file
raw_vt_vsm <- function(z) {
    tryCatch({
        temp <- str_match_all(z, "\"time\"[[:space:]]*:[[:space:]]*([^,\\}\\]]+)")[[1]][, 2]
        if (length(temp) == 1) round(as.numeric(temp) / 10) else NA_integer_
    }, error = function(e) NA_integer_)
}

raw_vt_xml <- function(z) {
    tryCatch({
        temp <- str_match_all(z, "<start>([[:digit:]\\.]+)</start>")[[1]][, 2]
        if (length(temp) == 1) round(as.numeric(temp)) else NA_integer_
    }, error = function(e) NA_integer_)
}

video_time_from_raw <- function(raw_lines) {
    out <- tryCatch(vapply(raw_lines, function(z) {
        if (grepl("~{", z, fixed = TRUE)) raw_vt_pv(z) else if (grepl("\"_id\":", z, fixed = TRUE)) raw_vt_vsm(z) else if (grepl("<start>[[:digit:]\\.]+</start>", z)) raw_vt_xml(z) else raw_vt_dv(z)
    }, FUN.VALUE = 1.0, USE.NAMES = FALSE), error = function(e) rep(NA_real_, length(raw_lines)))
    if (length(out) < 1) out <- NA_real_
    out
}

join_messages <- function(msgs1,msgs2) {
    if (length(msgs2)>0) {
        msgs1 <- c(msgs1,msgs2)
    }
    msgs1
}

# Extract text chunks from datavolley file. Internal function, not exported for users.
#
# @param txt: dv text
# @param token1: starting token, e.g. "[3SET]"
# @param token2: ending token
#
# @return string
text_chunk <- function(txt, token1, token2) {
    idx1 <- grep(token1, txt, fixed = TRUE)
    if (length(idx1) < 1) return("")
    if (missing(token2)) {
        ## find next section starting with "["
        idx2 <- grep("^\\[", txt)
        idx2 <- idx2[idx2 > idx1][1]
    } else {
        idx2 <- grep(token2, txt, fixed = TRUE)
    }
    if (idx2 == (idx1 + 1)) {
        ""
    } else {
        out <- txt[(idx1 + 1):(idx2 - 1)]
        out <- out[sapply(out, is.notempty.string)]
        paste(out, collapse = "\n")
    }
}

is.notempty.string <- function(x) {
    (is.character(x) && length(x)==1) && !is.na(x) && nchar(x)>0
}

single_value_or <- function(x, or = as(NA, class(x))) if (length(x) == 1) x else or

#' Find each entry in y that follows each entry in x
#'
#' @param x numeric: vector 
#' @param y numeric: vector 
#'
#' @return vector, each entry is the value in y that is next-largest to each corresponding entry in x
#'
#' @examples
#' findnext(c(1,5,10),c(1,2,3,7,8,9))
#' 
#' @export
findnext <- function(x,y) {
    ## return the number in y that comes after each of x
    sapply(x,function(z){
        temp <- y-z
        temp <- temp[temp>0]
        if (length(temp)<1)
            NA
        else
            min(temp)+z
    })
}

##findnext <- function(these,after) {
##    ## return the number in after that comes after each of these
##    sapply(these,function(z){
##        temp <- after-z
##        temp <- temp[temp>0]
##        if (length(temp)<1)
##            NA
##        else
##            min(temp)+z
##    })
##}


#' Find each entry in y that precedes each entry in x
#'
#' @param x numeric: vector 
#' @param y numeric: vector 
#'
#' @return vector, each entry is the value in y that is next-smallest to each corresponding entry in x
#'
#' @examples
#' findprev(c(1,5,10),c(1,2,3,7,8,9))
#' 
#' @export
findprev <- function(x,y) {
    ## return the number in y that comes before each of x
    sapply(x,function(z){
        temp <- z-y
        temp <- temp[temp>0]
        if (length(temp)<1)
            NA
        else
            z-min(temp)
    })
}


##findprev <- function(these,prev) {
##    ## return the number in prev that comes before each of these
##    sapply(these,function(z){
##        temp <- z-prev
##        temp <- temp[temp>0]
##        if (length(temp)<1)
##            NA
##        else
##            z-min(temp)
##    })
##}


## equality with NAs considered false
`%eq%` <- function(x,y) x==y & !is.na(x) & !is.na(y)

## convenience function to replace NAs
na.replace <- function(x,replace_with) {x[is.na(x)] <- replace_with; x}


#' Find a particular match in a list of datavolley objects
#'
#' @param match_id string: match_id to find
#' @param x list: list of datavolley objects as returned by \code{dv_read} 
#'
#' @return numeric index of the match in the list
#'
#' @seealso \code{\link{dv_read}}
#'
#' @export
find_match <- function(match_id,x) {
    which(sapply(x,function(z)z$meta$match_id==match_id))
}


most_common_value <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

## preferred can have more than one entry, treated in order of preference
## parse datetimes and dates, removing any that resolve to future
parse_dt <- function(dstr, format, allow_future_dates = FALSE) {
    out <- unique(na.omit(c(lubridate::parse_date_time(dstr, format))))
    if (!allow_future_dates) out <- out[difftime(Sys.time(), out, units = "days") > -5] ## not more than 5 days into the future
    out
}

parse_d <- function(dstr, pfun, allow_future_dates = FALSE) {
    out <- pfun(dstr)
    if (!allow_future_dates) out <- out[difftime(Sys.time(), out, units = "days") > -5] ## not more than 5 days into the future
    out
}

manydates <- function(z, preferred = NULL, official_date = NA) {
    z <- z[!is.na(z) & nzchar(z)]
    if (length(z) < 1) return(as.Date(integer(), origin = "1970-01-01"))
    suppressWarnings(
        tries <- list(ymd = unique(as.Date(na.omit(c(parse_d(z, lubridate::ymd), parse_dt(z, "Ymd HMS"))))),
                      dmy = unique(as.Date(na.omit(c(parse_d(z, lubridate::dmy), parse_dt(z, "dmY HMS"))))),
                      mdy = unique(as.Date(na.omit(c(parse_d(z, lubridate::mdy), parse_dt(z, "mdY HMS"))))))
    )
    if (!is.na(official_date)) {
        check <- sapply(tries, function(z) isTRUE(abs(as.numeric(z - official_date)) <= 1))
        if (sum(check, na.rm = TRUE) == 1) {
            ## we have one date within 1 day of the federation date, use that
            return(tries[[which(check)]])
        }
    }
    if (!is.null(preferred)) {
        preferred <- tolower(preferred)
        for (pref in preferred) {
            if (length(tries[[pref]]) > 0) return(tries[[pref]])
        }
    }
    if (length(tries$ymd) < 1 && length(tries$dmy) < 1 && length(tries$mdy) < 1) return(as.Date(integer(), origin = "1970-01-01"))
    unique(c(tries$ymd, tries$dmy, tries$mdy))
}

manydatetimes <- function(z, preferred = NULL) {
    z <- z[!is.na(z) & nzchar(z)]
    if (length(z) < 1) return(as.POSIXct(integer(), origin = "1970-01-01"))
    ## don't use lubridate::ymd_hms etc here, because they will fall foul of e.g.
    ##  https://github.com/tidyverse/lubridate/issues/552
    suppressWarnings(tries <- list(ymd = parse_dt(z, "Ymd HMS"), dmy = parse_dt(z, "dmY HMS"), mdy = parse_dt(z, "mdY HMS")))
    if (!is.null(preferred)) {
        preferred <- tolower(preferred)
        for (pref in preferred) {
            if (length(tries[[pref]]) > 0) return(tries[[pref]])
        }
    }
    if (length(tries$ymd) < 1 && length(tries$dmy) < 1 && length(tries$mdy) < 1) return(as.POSIXct(integer(), origin = "1970-01-01"))
    unique(c(tries$ymd, tries$dmy, tries$mdy))
}

unambiguous_datetime <- function(z) {
    suppressWarnings(tries <- list(ymd = parse_dt(z, format = "Ymd HMS"), dmy = parse_dt(z, format = "dmY HMS"), mdy = parse_dt(z, format = "mdY HMS")))
    ## do we have an unambiguous date? i.e. only one format gives a valid date
    unambiguous <- Filter(length, tries)
    unambiguous <- unique(data.frame(format = names(unambiguous), date = as.Date(as.numeric(unambiguous), origin = "1970-01-01"), stringsAsFactors = FALSE))
    if (nrow(unambiguous) == 1) unambiguous$format else NULL
}

#' Generate a short, human-readable text summary of one or more actions
#'
#' @param x data.frame or tibble: one or more rows from a datavolleyplays object as returned by \code{\link{dv_read}}
#' @param verbosity integer: 1 = least verbose, 2 = more verbose. Currently ignored
#'
#' @return character vector
#'
#' @examples
#' x <- dv_read(dv_example_file())
#' dv_action2text(plays(x)[27:30, ])
#'
#' @export
dv_action2text <- function(x, verbosity = 1) {
    vapply(seq_len(nrow(x)), function(i) do_action2text(x[i, ], verbosity = verbosity), FUN.VALUE = "", USE.NAMES = FALSE)
}

do_action2text <- function(x, verbosity) {
    if (x$skill %in% c("Serve", "Reception", "Set", "Attack", "Block", "Dig", "Freeball")) {
        out <- paste0(skill_extra(x), " by ", x$player_name, " (",
                      action_extra(x),
                      x$evaluation, ")")
    } else if (x$skill %in% c("Timeout")) {
        paste0("Timeout (", x$team, ")")
    } else if (x$skill %in% c("Technical timeout")) {
        "Technical timeout"
    } else {
        NA_character_
    }
}

skill_extra <- function(x) {
    if (x$skill %in% c("Attack", "Serve") && !is.na(x$skill_type)) x$skill_type else x$skill
}
action_extra <- function(x) {
    switch(x$skill,
           "Attack" = if (!is.na(x$attack_code)) paste0(x$attack_code, " - ") else "",
           "")
}

## replacement for enc::read_lines_enc now that enc has been archived Feb 2024
read_lines_enc <- function(path, file_encoding = "UTF-8") {
    con <- file(path, encoding = file_encoding)
    on.exit(close(con), add = TRUE)
    lines <- readLines(con, warn = FALSE, n = -1L, ok = TRUE, skipNul = FALSE, encoding = "UTF-8")
    Encoding(lines) <- "UTF-8"
    stringi::stri_enc_toutf8(lines)
}

detect_encodings <- function(filename) {
    ## pass a filename or already-read file contents
    file_text <- if (length(filename) > 1) filename else readLines(filename, warn = FALSE)
    ## test from [3TEAMS] section to end of [3SETTERCALL], just before [3WINNINGSYMBOLS]
    idx1 <- suppressWarnings(grep("[3MATCH]", file_text, fixed = TRUE)) + 1L
    idx2 <- suppressWarnings(grep("[3WINNINGSYMBOLS]", file_text, fixed = TRUE)) - 1L ## click and scout doesn't have this, nor does it have attack combos or setter calls
    ## fallback
    if (length(idx1) < 1 || is.na(idx1)) idx1 <- 10
    if (length(idx2) < 1 || is.na(idx2)) idx2 <- 90
    if (!isTRUE(idx2 > idx1)) { idx1 <- 10; idx2 <- 90; }
    tst <- sub("\u000f.+", "", file_text[idx1:idx2], useBytes = TRUE) ## throw away the encoded utf8-strings, they can throw off stri_enc_detect?
    tst <- paste(tst, collapse = "")
    enclist <- stri_enc_detect(tst)[[1]]
    enclist <- enclist$Encoding[enclist$Confidence > 0.8]
    ## if shift-jis appears in the suggested list of encodings, add cp932 because sometimes the former is detected but not the latter, even though the latter is more likely to have been used in our context. TODO may even need to apply this before applying the 0.8 cutoff
    enclist <- unlist(lapply(enclist, function(z) c(z, if (tolower(z) %in% c("shift-jis", "shift_jis")) "cp932")))
    ## extract the embedded text encoding as well
    embedded <- tryCatch({
        idx <- suppressWarnings(grep("[3MATCH]", file_text, fixed = TRUE))
        if (length(idx) == 1) {
            setdiff(stringi::stri_split(file_text[idx + 1], fixed = ";")[[1]][9], "1") ## 1 seems to be used to indicate the default locale encoding, which doesn't help us
        } else {
            NULL
        }
    }, error = function(e) NULL)
    ## resolve to an actual encoding
    embedded_resolved <- if (length(embedded) == 1) head(intersect(paste0(c("windows-", "cp"), tolower(embedded)), tolower(iconvlist())), 1) else NULL
    list(likely = enclist, embedded = embedded, embedded_resolved = embedded_resolved)
}

enc_from_embedded_utf8 <- function(file_text, encodings_to_test) {
    tryCatch({
        ## extract the reference text
        reftxt <- tibble()
        idx <- suppressWarnings(grep("[3MATCH]", file_text, fixed = TRUE))
        if (length(idx) == 1) {
            temp <- stringi::stri_split(file_text[idx + 1], fixed = ";")
            reftxt <- bind_rows(lapply(temp, function(z) list(utf8 = z[13:15], encoded = z[4:6])))
        }
        idx <- suppressWarnings(grep("[3MORE]", file_text, fixed = TRUE))
        if (length(idx) == 1) {
            temp <- stringi::stri_split(file_text[idx + 1], fixed = ";")
            reftxt <- bind_rows(reftxt, lapply(temp, function(z) list(utf8 = z[7:10], encoded = z[c(1, 4:6)])))
        }
        idx <- suppressWarnings(grep("[3TEAMS]", file_text, fixed = TRUE))
        if (length(idx) == 1) {
            temp <- stringi::stri_split(file_text[(idx + 1):(idx + 2)], fixed = ";")
            reftxt <- bind_rows(reftxt, lapply(temp, function(z) list(utf8 = z[7:9], encoded = z[c(2, 4, 5)])))
        }
        idx1 <- suppressWarnings(grep("[3PLAYERS-H]", file_text, fixed = TRUE))
        idx2 <- suppressWarnings(grep("[3PLAYERS-V]", file_text, fixed = TRUE))
        if (length(idx1) == 1 && length(idx2) == 1) {
            temp <- stringi::stri_split(file_text[(idx1 + 1):(idx2 - 1)], fixed = ";")
            reftxt <- bind_rows(reftxt, lapply(temp, function(z) list(utf8 = z[18:20], encoded = z[10:12])))
        }
        idx1 <- idx2
        idx2 <- suppressWarnings(grep("[3ATTACKCOMBINATION]", file_text, fixed = TRUE)) ## NB will fail for files without attack combo table?
        if (length(idx1) == 1 && length(idx2) == 1) {
            temp <- stringi::stri_split(file_text[(idx1 + 1):(idx2 - 1)], fixed = ";")
            reftxt <- bind_rows(reftxt, lapply(temp, function(z) list(utf8 = z[18:20], encoded = z[10:12])))
        }
        reftxt$utf8 <- decode_dv_utf8(reftxt$utf8)
        ## discard empty strings
        reftxt <- reftxt[nzchar(reftxt$utf8) & nzchar(reftxt$encoded), ]
        ## unbelievably, it seems possible that some files contain INCORRECTLY ENCODED TEXT in the embedded UTF8 strings
        ok <- sum(sapply(reftxt$utf8, badscore), na.rm = TRUE) < 1 ## ok if no errors in the encoded text
        ## if (!ok) warning("errors in embedded utf8")
        ## now we need to check that our reference text is actually informative. It is possible that a file can contain all ASCII in the above sections but still otherwise-encoded text in the attack combo descriptions and/or setter call descriptions, and these do not have UTF8 info in them
        if (ok) try({
            chk <- str_trim(stri_trans_general(reftxt$encoded, "latin-ascii"))
            if (sum(sapply(seq_along(chk), function(i) adist(chk[i], reftxt$utf8[i])), na.rm = TRUE) < 1) {
                ok <- FALSE
                ## warning("embedded utf8 is uninformative")
            }
        })
        if (!ok) return(NULL)
        get_best_encodings(encodings_to_test, reftext = reftxt)
    }, error = function(e) NULL)
}

.enc_score_data <- list(## badchars/badwords indicate characters/words that we don't expect to see, so the presence of any of these indicates that we've got the wrong file encoding
    badchars = c(1328:7499, utf8ToInt("\ub3\ua3\u008a\u008e\u009a\u00b3"), 960, ## armenian through to music, then some isolated ones
                 ## allow 1025:1327 - cyrillic
                 ## may need to consider removing penalty on armenian/arabic chars too
                 ## selected elements from 0x2000 to 0x206f (general punctuation) likely wrong, 0x01-0x07 are control characters we don't expect to see
                 0x2000:0x200f, 0x2016:0x2017, 0x2020:0x2031, 0x203b, 0x203d:0x2043, 0x2045:0x2046, 0x204a:0x206f, 0x00:0x07,
                 utf8ToInt("\u253c\ud7\u3ad"), ##?? \u44d\u42d
                 0x2500:0x25ff, ## box-drawing characters (seen with Japanese misidentified as Korean)
                 0x80:0xa0, utf8ToInt("\ua4\ua6\ua8\ub6\ua2"), ## various symbols from the 0x80-0xff block that are unlikely to be used legitimately
                 0xffef:0xffff, 0xffdd:0xffdf), ## unicode specials like the question mark replacement character
    badwords = tolower(c("S\u159RENSEN","S\u159gaard","S\u159ren","M\u159LLER","Ish\u159j","Vestsj\u107lland","KJ\u107R","M\u159rk","Hj\u159rn","\u139rhus", ## these from windows-1252 (or ISO-8859-1) wrongly guessed as windows-1250
                       "\ud9ukas", "Pawe\uf9", "\ud9omacz", paste0("Mo\ufd", "d\ufdonek"), "W\uf9odarczyk", ## these from windows-1257/ISO-8859-13 wrongly guessed as windows-1252
                       "\u3a9ukas", "Pawe\u3c9", "\u3a9omacz", paste0("Mo\u3cd", "d\u3cdonek"), "W\u3c9odarczyk", ## these from windows-1257/ISO-8859-13 wrongly guessed as windows-1253
                       "\uc4\u15a", "\u139\u2dd", ## utf-8 wrongly guessed as windows-1250
                       "\uc4\u8d", "\uc5\ubd", "\uc4\u152", ## utf-8 wrongly guessed as windows-1252
                       "Nicol\u148", ## windows-1252/iso-8859-1 wrongly guessed as windows-1250
                       "\u6c\u2116", "\u6a\u2116", "\u77\u2116", "\u64\u2116\u62", "\u45a\u77", "\u45a\u6c", "\u63\u7a\u43a", ## windows-1250 wrongly guessed as windows-1251
                       "\u6a\u446\u72", "\u68\u446\u68", "\u76\u446\u6c", "\u6d\u44c\u6c", "\u72\u44c\u67", "\u70\u446\u68", "\u6b\u44c\u68", ## 1250 as 1251
                       "\uc2\ue4\u77", "\uf1\u7b", "\ue5\ue4", "\ue5\ue3", ## japanese SHIFT-JIS wrongly guessed as macintosh
                       "\u102\u104\u7a", "\u102\u104\u73", "\u102\u2c7\u7a", "\u102\u2c7\u73",
                       "\u192\u56", "\u192\u2021", "\u192\u67", "\u192\u6f", "\u192\u62", "\u192\u4e", "\u2018\u4f", "\u192\u70", "\u192\u43", "\u192\u76",
                       "\uf7\u119\uee", "\u2d9\u119\uee", "\uf7\u155\u111", "\uf7\u10d\ued", "\uc2\ueb\u155\ue4\u10d", "\u10f\u10d\uee", "\u10f\u13a\u111", "\u155\u148\uf7", ## russian 1251 wrongly detected as 1250
                       "\ue8\ueb\ueb", "\ueb\uea", "\ue4\ue0", "\ue0\ue2", "\ue0\ue5", "\ue5\ue2", "\ued\uee", "\uee\uef", "\udf\uea", "\u1fb\uef\ue8", "\uee\ued\ue0", "\uee\uf1\uf1", "\uf1\uf1\ue8", ## russian 1251 wrongly detected as 1258
                       "\u101\ue5", "\ue5\u101", "\u107\u10d", "\ud7\ue5", "\u122\u10d", ## russian 1251 wrongly detected as 1257
                       ## "\u10f\u17c\u2dd", "\u43f\u457\u405", "\u3bf\u38f\ubd", "\u5df\ubf\ubd", ## this is the unicode replacement char EF BF BD in windows-1250, 1251, 1253 (the encoding should be UTF-8). Note that 1252, 1254 seem to represent this as EF BF BD, so we can't detect that?? Doesn't seem to be reliable anyway
                       "\u56\u49\u444", "\u56\u49\uc6", ## windows-1252 wrongly detected as KOI8-R
                       "\u171\ufc", "\u8e\u52", ## cp932 wrongly detected as ISO-8859-2 (there are heaps here)
                       "\uc9\u57\uc9", "\ue2\u2122", ## cp932 wrongly detected as macintosh
                       "\ufd\u79", ## windows-1254 wrongly detected as 1250
                       "\u6e\u434\u45a\u69\u434\u45a", "\u76\u69\u434\u45a", ## "ncic" and "vic" but c with caron (Serbian/Czech/etc) wrongly detected as cyrillic
                       "\u79\u69\uf0\u69\u74", "\u75\u6c\u61\ufe", "\u64\u6f\uf0\u61\u6e", "\u70\u6f\uf0\u6c\u75", "\u6e\ufd\u6c\ufc\u66\u65\u72", ## 1252
                         "\u64\u6f\u111\u61\u6e", "\u75\u6c\u61\u163", ## 1250
                         "\u62\u44c\u6c", "\u44c\u6d\u69\u74", "\u62\u65\u6c\u65\u64\u44d\u79", "\u6e\u44d\u6c\u44c\u66\u65\u72", ## 1251
                         "\u62\u65\u6c\u65\u64\u169\u79", "\u6e\u169\u6c\ufc\u66\u65\u72")), ## Turkish iso-8859-9 wrongly detected as windows-1252 or 1250 or 1251 or other. Need to be a little careful because some characters (e.g. \uf0, \ufe) are valid in e.g. Icelandic and Faroese
    badwords_trans = c("oooo", "ouuoo", "oouoo", "uuou", "uuoo")) ## badwords after transliteration, e.g. wrongly-detected cyrillic
## to add entries here, get the \uxx numbers from paste0("\\u", sprintf("%x", utf8ToInt(tolower("dodgy"))), collapse = "")

badscore <- function(z) {
    sum(utf8ToInt(z) %in% .enc_score_data$badchars) +
        10 * sum(sapply(.enc_score_data$badwords, grepl, tolower(z), fixed = TRUE)) +
        10 * (sum(sapply(.enc_score_data$badwords_trans, grepl, stri_trans_general(tolower(z), "latin-ascii"), fixed = TRUE)) - sum(sapply(.enc_score_data$badwords_trans, grepl, tolower(z), fixed = TRUE)))
    ## the latter term penalizes text that matches badwords_trans when transliterated to ASCII but doesn't match when not transliterated. Which will generally be streams of gibberish accented characters (e.g. Cyrillic processed as something else)
}

txt2utf8 <- function(txt, from) {
    tryCatch({
        suppressWarnings(sub_u_spaces(stringi::stri_encode(txt, from = from, to = "utf-8")))
    }, error = function(e) NULL)
}

get_best_encodings <- function(encodings_to_test, reftext, filename, read_from = 10, read_to = 90, file_text, expect_tildes = TRUE) {
    ## provide either `reftext` (a data frame with columns `encoded` and `utf8`, and we will find the encodings that best reproduce the `utf8` entries from the `encoded` entries
    ## OR provide `filename`, `read_from`, `read_to`, `expect_tildes` in which case we will read from that file and find the encodings that score best when evaluated against our custom rules (characters and word fragments that we do not expect to see)
    ## OR provide `file_text` instead of `filename` to use this text instead of reading from the file each time. `file_text` should be the whole file contents. Still need `read_from` and `read_to`
    if (!missing(reftext)) {
        if (!all(c("encoded", "utf8") %in% names(reftext))) stop("reftext input is in unexpected format")
        encerrors <- sapply(encodings_to_test, function(enc) {
            ## convert from the specified encoding to utf8
            chk <- txt2utf8(reftext$encoded, from = enc)
            if (is.null(chk)) {
                NA_real_
            } else {
                ## compare to the actual utf8
                chk <- str_trim(sub_u_spaces(chk))
                sum(sapply(seq_along(chk), function(i) adist(chk[i], reftext$utf8[i])))
            }
        })
        idx <- which(encerrors == suppressWarnings(min(encerrors, na.rm = TRUE)))
        if (length(idx) < 1) {
            list(encodings = character(), error_score = NA_integer_)
        } else {
            list(encodings = encodings_to_test[idx], error_score = min(encerrors, na.rm = TRUE))
        }
    } else {
        ## use either filename, read_from, and read_to OR file_text
        if (read_to < read_from) stop("read_to cannot be less than read_from")
        if (!missing(filename)) {
            ## read from the (whole) file with the specified encoding
            read_with_enc <- function(filename, enc_to_test) {
                ## read with specified encoding and convert to UTF8
                tryCatch(read_lines_enc(filename, file_encoding = enc_to_test), warning = function(w) NA_character_, error = function(e) NA_character_)
            }
            testtxt <- lapply(encodings_to_test, read_with_enc, filename = filename)
            suppressWarnings({
                encerrors <- sapply(testtxt, function(z) {
                    z <- paste(z[read_from:read_to], collapse = "")
                    if (is.na(z)) Inf else badscore(z)
                })
            })
            if (expect_tildes) {
                ## also check whether we get ~'s in the 3SCOUT section (will not get any if CP932 incorrectly detected as SHIFT-JIS, for example)
                tilde_count <- sapply(testtxt, function(z) if (all(is.na(z))) 0L else sum(stringi::stri_count(z, fixed = "~")))
                encerrors[which(tilde_count < 1)] <- encerrors[which(tilde_count < 1)] + 20L
            }
        } else {
            ## use file_text, just re-encode to utf-8 assuming that the original text was in the encoding to test
            testtxt <- paste(file_text[read_from:read_to], collapse = "")
            file_text <- paste(file_text, collapse = "")
            encerrors <- sapply(encodings_to_test, function(enc) {
                convtext <- txt2utf8(testtxt, from = enc)
                if (is.null(convtext)) {
                    Inf
                } else {
                    out <- badscore(convtext)
                    if (expect_tildes) {
                        tilde_count <- tryCatch(sum(stringi::stri_count(file_text, fixed = "~"), na.rm = TRUE), error = function(e) 0L)
                        if (tilde_count < 1) out <- out + 20L
                    }
                    out
                }
            })
        }
        idx <- encerrors == min(encerrors)
        if (!any(idx)) {
            list(encodings = character(), error_score = NA_integer_)
        } else {
            list(encodings = encodings_to_test[idx], error_score = min(encerrors))
        }
    }
}

#' Convert integer colour to RGB
#'
#' DataVolley files use an integer representation of colours. These functions convert to and from hex colour strings as used in R.
#'
#' @param z integer: vector of integers
#' @param x integer: vector of hex colour strings
#'
#' @return Character vector of hex RGB colour strings
#'
#' @examples
#' dv_int2rgb(c(255, 16711680))
#'
#' @export
dv_int2rgb <- function(z) {
    r <- floor(z / 256^2)
    g <- floor((z - r * (256^2)) / 256)
    b <- z - floor(r * (256^2) + g * 256)
    out <- apply(cbind(r, g, b), 1, function(z) sprintf("#%02X%02X%02X", z[1], z[2], z[3]))
    out[z < 0 | z > 16777215 | is.na(z)] <- NA_character_
    out
}

#' @export
#' @rdname dv_int2rgb
dv_rgb2int <- function(x) {
    out <- grDevices::col2rgb(x)
    out <- suppressWarnings(as.integer(apply(out, 2, function(z) z[1] * 256 * 256 + z[2] * 256 + z[3])))
    out[is.na(x)] <- NA_integer_
    out
}

camel_to_underscore <- function(x) {
    s <- gsub("([a-z0-9])([A-Z])", "\\1_\\L\\2", x, perl = TRUE)
    sub("^(.[a-z])", "\\L\\1", s, perl = TRUE) # make 1st char lower case if second is lower
}

lead0 <- function(x, width = 2, pad = "0", na = NULL) {
    out <- character(length(x))
    naidx <- rep(FALSE, length(x))
    if (!is.null(na)) {
        naidx <- is.na(x) | !nzchar(x)
        out[naidx] <- as.character(na)
    }
    out[!naidx] <- stringr::str_pad(as.numeric(x[!naidx]), width = width, pad = pad)
    out
}

rotpos <- function(p, by = 1L, n = 6L) (p - 1L - by) %% n + 1L
rot_lup <- function(z, by = 1L, n = 6L) {
    ## z is a lineup vector
    z[rotpos(seq_along(z), by = by, n = n)]
}

## mapvalues from plyr, MIT license
mapvalues <- function (x, from, to, warn_missing = TRUE) {
    if (length(from) != length(to)) {
        stop("`from` and `to` vectors are not the same length.")
    }
    if (!is.atomic(x) && !is.null(x)) {
        stop("`x` must be an atomic vector or NULL.")
    }
    if (is.factor(x)) {
        levels(x) <- mapvalues(levels(x), from, to, warn_missing)
        return(x)
    }
    mapidx <- match(x, from)
    mapidxNA <- is.na(mapidx)
    from_found <- sort(unique(mapidx))
    if (warn_missing && length(from_found) != length(from)) {
        message("The following `from` values were not present in `x`: ", 
            paste(from[!(1:length(from) %in% from_found)], collapse = ", "))
    }
    x[!mapidxNA] <- to[mapidx[!mapidxNA]]
    x
}

single_unique_value_or_na_int <- function(x) {
    u <- unique(na.omit(x))
    if (length(u) == 1) u else NA_integer_
}

mean_nna <- function(...) mean(..., na.rm = TRUE)
qmax <- function(...) suppressWarnings(max(..., na.rm = TRUE))
qmin <- function(...) suppressWarnings(min(..., na.rm = TRUE))

## deal with DV's UTF8-encoded text
## their encoded text starts with \u000f
is_dv_utf8 <- function(z) grepl("^\u000f", z, useBytes = TRUE) ## one element per element of z
any_dv_utf8 <- function(z) any(grepl("\u000f", z, useBytes = TRUE), na.rm = TRUE) ## TRUE if anything in z is encoded

## decode a vector of text
decode_dv_utf8 <- function(z, na_is = NA, warn = FALSE) {
    if (length(z) > 1) return(unname(sapply(z, decode_dv_utf8)))
    if (!is_dv_utf8(z)) {
        if (warn) warning("text is not UTF8-encoded")
        return(z)
    }
    ## string starts with \u000f then 2 or 4 for 2-byte or 4-byte encoding
    if (nchar(z) < 3) return(na_is) ## empty string, but in general if we were reading from the delimited text this would be read as logical NA, so return that here for consistency. Can be overridden with the na_is parm
    nbytes <- as.numeric(substr(z, 2, 2))
    if (nbytes <= 1 || (nbytes %% 2 != 0)) {
        if (warn) warning("could not decode text")
        return(z)
    }
    str_trim(sub_u_spaces(intToUtf8(strtoi(stringi::stri_sub(z, seq(3, nchar(z), by = nbytes), length = nbytes), 16))))
}

## unicode includes a bunch of space character that aren't necessarily detected by the regexp [[:space:]] (e.g. the \ua0 non-breaking space is not)
sub_u_spaces <- function(z) {
    spcs <- utf8ToInt(c("\u00a0\u180e\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200a\u200b\u202f\u205f\u3000\ufeff"))
    zi <- lapply(z, utf8ToInt)
    zi <- lapply(zi, function(x) { x[x %in% spcs] <- 32; x }) ## standard space
    unlist(lapply(zi, intToUtf8))
}

## replace un-encoded columns of data.frame p with their encoded copies
process_dv_utf8 <- function(p, from, to, na_is = NA) {
    ## from and to are vectors of column numbers or names
    if (length(from) != length(to)) {
        warning("from and to are different lengths")
        return(p)
    }
    if (is.character(from)) {
        ## convert to column numbers
        from <- vapply(from, function(nm) if (nm %in% names(p)) which(names(p) == nm)[1] else NA_integer_, FUN.VALUE = 1L, USE.NAMES = FALSE)
    }
    todrop <- rep(FALSE, length(from))
    for (i in seq_along(from)) {
        if (!is.na(from[i]) && ncol(p) >= from[i] && any_dv_utf8(p[[from[i]]])) {
            try({
                p[[to[i]]] <- decode_dv_utf8(p[[from[i]]], na_is = na_is)
                todrop[i] <- TRUE
            })
        }
    }
    if (any(todrop)) p[, setdiff(seq_len(ncol(p)), from[todrop])] else p
}

## for debugging timings
options(dvtiming = list())
mark_timing <- function(label) {
    opt <- getOption("dvtiming")
    opt[[label]] <- proc.time()
    options(dvtiming = opt)
}
show_timing <- function(label) {
    message(label, " execution time: ", round((proc.time() - getOption("dvtiming")[[label]])[3] * 1000), " ms")
}
