## Accumulate messages for later display
## Internal function, not exported
## severity: 1=critical, 2=informative, may lead to misinterpretation of data, 3=minor, esp. those that might have resulted from selective post-processing of combo codes
collect_messages <- function(msgs,msg_text,line_nums,raw_lines,severity,fatal=FALSE) {
    if (missing(line_nums)) line_nums <- NA
    if (missing(raw_lines)) raw_lines <- "[unknown]"
    if (missing(severity)) severity <- NA
    vt <- rep(NA_real_,length(line_nums))
    if (!missing(raw_lines)) vt <- video_time_from_raw(raw_lines)
    if (fatal) {
        lnt <- as.character(line_nums)
        lnt[is.na(lnt)] <- "[unknown]"
        txt <- paste0("line ",lnt,": ",msg_text," (line in file is: \"",raw_lines,"\")")
        if (fatal) stop(paste(txt,collapse=" / "))
    } else {
        msgs[[length(msgs)+1]] <- list(file_line_number=line_nums,video_time=vt,message=msg_text,file_line=raw_lines,severity=severity)
    }
    msgs
}

## video time from raw line, for datavolley format
raw_vt_dv <- function(z) {
    tryCatch({
        if (!is.null(z) && is.character(z) && nzchar(z) && !is.na(z)) {
            as.numeric(read.csv(text = z, sep = ";", header = FALSE, stringsAsFactors = FALSE)[1,13])
        } else {
            NA_real_
        }},
        error = function(e) NA_real_)
}

## video time from raw line, for peranavolley format
raw_vt_pv <- function(z) {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
        NA_real_
    } else {
        pparse <- function(z, df = TRUE) {
            temp <- sub("^[A-Z]+~", "", z)
            if (grepl("^\\(?null\\)?", temp, ignore.case = TRUE)) {
                if (df) tibble() else NULL
            } else {
                jsonlite::fromJSON(temp)
            }
        }
        tryCatch({
            if (!is.null(z) && is.character(z) && nzchar(z) && !is.na(z)) {
                as.numeric(pparse(z)$videoDuration)
            } else {
                NA_real_
            }},
            error = function(e) NA_real_)
    }
}

video_time_from_raw <- function(raw_lines) {
    tryCatch(vapply(raw_lines,function(z) if (grepl("~{", z, fixed = TRUE)) raw_vt_pv(z) else raw_vt_dv(z), FUN.VALUE = 1.0, USE.NAMES = FALSE), error = function(e) rep(NA_real_, length(raw_lines)))
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
text_chunk <- function(txt,token1,token2) {
    idx1 <- grep(token1,txt,fixed=TRUE)
    if (length(idx1)<1) return("")
    if (missing(token2)) {
        ## find next section starting with "["
        idx2=grep("^\\[",txt)
        idx2=idx2[idx2>idx1][1]
    } else {
        idx2=grep(token2,txt,fixed=TRUE)
    }
    if (idx2==(idx1+1)) {
        ""
    } else {
        out <- txt[(idx1+1):(idx2-1)]
        out <- out[sapply(out,is.notempty.string)]##!is.na(out)]
        paste(out,collapse="\n")
    }
}

is.notempty.string <- function(x) {
    (is.character(x) && length(x)==1) && !is.na(x) && nchar(x)>0
}



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
#' @param x list: list of datavolley objects as returned by \code{read_dv} 
#'
#' @return numeric index of the match in the list
#'
#' @seealso \code{\link{read_dv}}
#'
#' @export
find_match <- function(match_id,x) {
    which(sapply(x,function(z)z$meta$match_id==match_id))
}


most_common_value <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

manydates <- function(z, preferred = NULL) {
    suppressWarnings(
        tries <- list(ymd = unique(as.Date(na.omit(c(lubridate::ymd(z), lubridate::parse_date_time(z, "Ymd HMS"))))),
                      dmy = unique(as.Date(na.omit(c(lubridate::dmy(z), lubridate::parse_date_time(z, "dmY HMS"))))),
                      mdy = unique(as.Date(na.omit(c(lubridate::mdy(z), lubridate::parse_date_time(z, "mdY HMS"))))))
    )
    if (!is.null(preferred)) {
        preferred <- tolower(preferred)
        chk <- tries[[tolower(preferred)]]
        if (!is.null(chk) && length(chk) > 0) return(chk)
    }
    unique(c(tries$ymd, tries$dmy, tries$mdy))
}

manydatetimes <- function(z, preferred = NULL) {
    ## don't use lubridate::ymd_hms etc here, because they will fall foul of e.g.
    ##  https://github.com/tidyverse/lubridate/issues/552
    suppressWarnings(
        tries <- list(ymd = unique(na.omit(c(lubridate::parse_date_time(z, "Ymd HMS")))),
                      dmy = unique(na.omit(c(lubridate::parse_date_time(z, "dmY HMS")))),
                      mdy = unique(na.omit(c(lubridate::parse_date_time(z, "mdY HMS")))))
    )
    if (!is.null(preferred)) {
        preferred <- tolower(preferred)
        chk <- tries[[tolower(preferred)]]
        if (!is.null(chk) && length(chk) > 0) return(chk)
    }
    unique(c(tries$ymd, tries$dmy, tries$mdy))
}

#' Generate a short, human-readable text summary of one or more actions
#'
#' @param x data.frame or tibble: one or more rows from a datavolleyplays object as returned by \code{\link{read_dv}}
#' @param verbosity integer: 1 = least verbose, 2 = more verbose. Currently ignored
#'
#' @return character vector
#'
#' @examples
#' x <- read_dv(dv_example_file())
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

get_best_encodings <- function(encodings_to_test, filename, read_from = 10, read_to = 90) {
    if (read_to < read_from) stop("read_to cannot be less than read_from")
    ## badchars/badwords indicate characters/words that we don't expect to see, so the presence of any of these indicates that we've got the wrong file encoding
    badchars <- c(1328:7499, utf8ToInt("\ub3\ua3\u008a\u008e\u009a\u00b3"), 960) ## armenian through to music, then some isolated ones
    ## allow 1025:1327 - cyrillic
    ## may need to consider removing penalty on armenian/arabic chars too
    ## 0x2000 to 0x206f (general punctuation) likely wrong, 0x01-0x07 are control characters we don't expect to see
    badchars <- c(badchars,0x2000:0x206f, 0x00:0x07)
    badchars <- c(badchars, "\u253c")
    badchars <- c(badchars, 0x2500:0x25ff) ## box-drawing characters (seen with Japanese misidentified as Korean)
    badwords <- tolower(c("S\u159RENSEN","S\u159gaard","S\u159ren","M\u159LLER","Ish\u159j","Vestsj\u107lland","KJ\u107R","M\u159rk","Hj\u159rn","\u139rhus")) ## these from windows-1252 (or ISO-8859-1) wrongly guessed as windows-1250
    badwords <- c(badwords,tolower(c("\ud9ukas","Pawe\uf9","\ud9omacz",paste0("Mo\ufd","d\ufdonek"),"W\uf9odarczyk"))) ## these from windows-1257/ISO-8859-13 wrongly guessed as windows-1252
    badwords <- c(badwords,tolower(c("\u3a9ukas","Pawe\u3c9","\u3a9omacz",paste0("Mo\u3cd","d\u3cdonek"),"W\u3c9odarczyk"))) ## these from windows-1257/ISO-8859-13 wrongly guessed as windows-1253
    badwords <- c(badwords,tolower(c("\uc4\u15a","\u139\u2dd"))) ## utf-8 wrongly guessed as windows-1250
    badwords <- c(badwords, tolower(c("\uc4\u8d", "\uc5\ubd", "\uc4\u152"))) ## utf-8 wrongly guessed as windows-1252
    badwords <- c(badwords,tolower(c("Nicol\u148"))) ## windows-1252/iso-8859-1 wrongly guessed as windows-1250
    badwords <- c(badwords, tolower(c("\u6a\u446\u72", "\u68\u446\u68", "\u76\u446\u6c", "\u6d\u44c\u6c", "\u72\u44c\u67", "\u70\u446\u68", "\u6b\u44c\u68"))) ## 1250 as 1251
    badwords <- c(badwords, tolower(c("\uc2\ue4\u77", "\uf1\u7b", "\ue5\ue4", "\ue5\ue3"))) ## japanese SHIFT-JIS wrongly guessed as macintosh
    badwords <- c(badwords, tolower("\u102\u104\u7a"), tolower("\u102\u104\u73"), tolower("\u102\u2c7\u7a"), tolower("\u102\u2c7\u73"))
    badwords <- c(badwords, tolower(c(intToUtf8(c(8222, 162)))))
    badwords <- c(badwords, tolower(c("\u192\u56", "\u192\u2021", "\u192\u67", "\u192\u6f", "\u192\u62", "\u192\u4e", "\u2018\u4f", "\u192\u70", "\u192\u43", "\u192\u76")))
    badwords <- c(badwords, tolower(c("\uf7\u119\uee", "\u2d9\u119\uee", "\uf7\u155\u111", "\uf7\u10d\ued", "\uc2\ueb\u155\ue4\u10d"))) ## russian 1251 wrongly detected as 1250
    badwords <- c(badwords, tolower(c("\ue8\ueb\ueb", "\ueb\uea", "\ue4\ue0", "\ue0\ue2", "\ue0\ue5", "\ue5\ue2", "\ued\uee", "\uee\uef", "\udf\uea"))) ## russian 1251 wrongly detected as 1258
    badwords <- c(badwords, tolower(c("\u101\ue5", "\ue5\u101", "\u107\u10d", "\ud7\ue5", "\u122\u10d"))) ## russian 1251 wrongly detected as 1257
    ##badwords <- c(badwords, tolower(c("\u10f\u17c\u2dd", "\u43f\u457\u405", "\u3bf\u38f\ubd", "\u5df\ubf\ubd"))) ## this is the unicode replacement char EF BF BD in windows-1250, 1251, 1253 (the encoding should be UTF-8). Note that 1252, 1254 seem to represent this as EF BF BD, so we can't detect that?? Doesn't seem to be reliable anyway
    badwords <- c(badwords, tolower(c("\u56\u49\u444", "\u56\u49\uc6"))) ## windows-1252 wrongly detected as KOI8-R
    badwords <- c(badwords, tolower(c("\u171\ufc", "\u8e\u52"))) ## cp932 wrongly detected as ISO-8859-2 (there are heaps here)
    ## get the \uxx numbers from sprintf("%x",utf8ToInt(dodgy_string_or_char)) or paste0("\\u", sprintf("%x", utf8ToInt("dodgy")), collapse = "")
    test_with_enc <- function(enc_to_test) {
        con <- file(filename, encoding = enc_to_test)
        on.exit(close(con))
        ## warn = FALSE (covers NULLs and missing final EOL, but not invalid text chars from mis-specified encoding)
        tryCatch(paste(readLines(con, n = read_to, warn = FALSE)[read_from:read_to], collapse = ""), warning = function(w) NA_character_, error = function(e) NA_character_)
    }
    enctest <- sapply(encodings_to_test, test_with_enc)
    encerrors <- sapply(enctest, function(z)if (is.na(z)) Inf else sum(utf8ToInt(z) %in% badchars)+10*sum(sapply(badwords,grepl,tolower(z),fixed=TRUE)))
    ## what badwords are matching a given encoding?
    ##cat("bw:\n")
    ##print(lapply(enctest[names(enctest) == "windows-1250"], function(z) if (is.na(z)) Inf else which(sapply(badwords,grepl,tolower(z),fixed=TRUE))))
    ## errors per encodings_to_test
    ##cat("encerrors:\n"); print(sort(encerrors))
    idx <- encerrors==min(encerrors)
    if (!any(idx)) {
        list(encodings = character(), error_score = NA_integer_)
    } else {
        list(encodings = encodings_to_test[idx], error_score = min(encerrors))
    }
}
