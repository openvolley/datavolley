#' Write a datavolley object to dvw file
#'
#' Note that this is really rather experimental, and you probably shouldn't use it yet. Once complete, this function will allow a datavolley file to be read in via \code{\link{dv_read}}, modified by the user, and then rewritten back to a datavolley file.
#' At this stage, most modifications to the datavolley object should make it back into the rewritten file. However, the scouted code (in the \code{code} column) is NOT yet updated to reflect changes that might have been made to other columns in the datavolley object.
#'
#' @param x datavolley: a datavolley object as returned by \code{\link{dv_read}}
#' @param file string: the filename to write to. If not supplied, no file will be written but the dvw content will be returned
#' @param text_encoding string: the text encoding to use
#'
#' @return The dvw file contents as a character vector (invisibly)
#'
#' @seealso \code{\link{dv_read}}
#'
#' @examples
#' \dontrun{
#'   x <- dv_read(dv_example_file())
#'   outfile <- tempfile()
#'   dv_write(x, outfile)
#' }
#' @export
dv_write <- function(x, file, text_encoding = "UTF-8") {
    if (!inherits(x, "datavolley")) stop("x must be a datavolley object")
    dateformat <- if (length(x$file_meta$preferred_date_format) > 0) {
                      if (tolower(x$file_meta$preferred_date_format[1]) %eq% "mdy") {
                          "%m/%d/%Y"
                      } else if (tolower(x$file_meta$preferred_date_format[1]) %eq% "dmy") {
                          "%d/%m/%Y"
                      } else if (tolower(x$file_meta$preferred_date_format[1]) %eq% "ymd") {
                          "%Y/%m/%d"
                      } else {
                          "%d/%m/%Y"
                      }
                  } else {
                      "%d/%m/%Y"
                  }
    out <- c(dvw_header(x, text_encoding = text_encoding, date_format = dateformat),
             dvw_match(x, text_encoding = text_encoding, date_format = dateformat),
             dvw_teams(x, text_encoding = text_encoding),
             dvw_more(x, text_encoding = text_encoding),
             dvw_comments(x, text_encoding = text_encoding),
             dvw_set(x, text_encoding = text_encoding),
             dvw_players_h(x, text_encoding = text_encoding),
             dvw_players_v(x, text_encoding = text_encoding),
             dvw_attack_combos(x, text_encoding = text_encoding),
             dvw_setter_calls(x, text_encoding = text_encoding),
             dvw_winning_symbols(x, text_encoding = text_encoding),
             "[3RESERVE]",
             dvw_video(x, text_encoding = text_encoding),
             dvw_scout(x, text_encoding = text_encoding))
    if (!missing(file)) {
        outf <- file(file, "wb") ## encoding??
        writeLines(out, outf, sep = "\n")
        close(outf)
    }
    invisible(out)
}

#' @rdname dv_write
#' @export
write_dv <- dv_write

not_null_or <- function(z, or) if (is.null(z)) or else z

df2txt <- function(z) {
    ## change any logical cols to char "True" "False"
    logical2char <- function(w) {
        out <- as.character(w)
        out[which(w)] <- "True"
        out[which(!w)] <- "False"
        out
    }
    findlogicalcols <- function(w) {
        vapply(seq_len(ncol(w)), function(ci) is.logical(w[[ci]]) & !all(is.na(w[[ci]])), FUN.VALUE = TRUE)
    }
    for (lc in which(findlogicalcols(z))) z[[lc]] <- logical2char(z[[lc]])
    ## convert period cols to text
    findperiodcols <- function(w) vapply(seq_len(ncol(w)), function(ci) lubridate::is.period(w[[ci]]), FUN.VALUE = TRUE)
    ldz <- function(nn, width = 2) formatC(nn, flag = "0", width = width) ## leading zeros
    for (pc in which(findperiodcols(z))) {
        nnaidx <- which(!is.na(z[[pc]]))
        temp <- rep("", nrow(z))
        temp[nnaidx] <- paste0(ldz(lubridate::hour(z[[pc]][nnaidx])), ".", ldz(lubridate::minute(z[[pc]][nnaidx])), ".", ldz(lubridate::second(z[[pc]][nnaidx])))
        z[[pc]] <- temp
    }
    capture.output(data.table::fwrite(z, sep = ";", col.names = FALSE, row.names = FALSE, quote = FALSE, na = ""))
}

## helper for simple sections that just require a dump of the existing section
sect2txt <- function(sect, component, outhdr, trailing_col = FALSE) {
    if (is.null(sect)) {
        warning("missing the ", component, " component of the input object")
        outhdr
    } else {
        if (trailing_col) sect[, ncol(sect)+1] <- NA_character_
        c(outhdr, df2txt(sect))
    }
}

dvw_video <- function(x, text_encoding) {
    mm <- x$meta$video
    if (is.null(mm)) {
        ## this section isn't always present
        NULL
    } else if (nrow(mm) < 1) {
        ## just the section header but nothing more
        "[3VIDEO]"
    } else {
        ## a row like Camera0=C:\\Video\\A vs B.mp4
        ## presumably can be multiple rows?
        c("[3VIDEO]", paste(mm$camera, mm$file, sep = "="))
    }
}

## metadata sections
dvw_winning_symbols <- function(x, text_encoding) {
    mm <- x$meta$winning_symbols
    if (is.null(mm)) stop("missing the meta$winning_symbols component of the input object")
    if (is.data.frame(mm)) mm <- winning_symbols_df2txt(mm)
    c("[3WINNINGSYMBOLS]", mm)
}

dvw_setter_calls <- function(x, text_encoding) {
    tmp <- x$meta$sets
    if (is.null(tmp)) {
        warning("missing the meta$sets component of the input object")
        "[3SETTERCALL]"
    } else {
        ## force coords to be 4-digit ints
        tmp$start_coordinate <- sprintf("%04d", tmp$start_coordinate)
        tmp$mid_coordinate <- sprintf("%04d", tmp$mid_coordinate)
        tmp$end_coordinate <- sprintf("%04d", tmp$end_coordinate)
        if (is.character(tmp$colour)) tmp$colour <- dv_rgb2int(tmp$colour)
        if ("path_colour" %in% names(tmp) && is.character(tmp$path_colour)) tmp$path_colour <- dv_rgb2int(tmp$path_colour)
        sect2txt(tmp, "meta$sets", "[3SETTERCALL]")
    }
}

dvw_attack_combos <- function(x, text_encoding) {
    tmp <- x$meta$attacks
    if (is.data.frame(tmp) && "set_type" %in% names(tmp)) {
        tmp$set_type[tmp$set_type %in% c(NA_character_, "")] <- "-"
    }
    if (is.character(tmp$colour)) tmp$colour <- dv_rgb2int(tmp$colour)
    sect2txt(tmp, "meta$attacks", "[3ATTACKCOMBINATION]")
}

dvw_players_v <- function(x, text_encoding) {
    tmp <- x$meta$players_v[, setdiff(names(x$meta$players_v), "name")]
    tmp$role <- as.character(roles_str2int(tmp$role))
    tmp$role[tmp$role == "0"] <- ""
    sect2txt(tmp, "meta$players_v", "[3PLAYERS-V]")
}

dvw_players_h <- function(x, text_encoding) {
    tmp <- x$meta$players_h[, setdiff(names(x$meta$players_h), "name")]
    tmp$role <- as.character(roles_str2int(tmp$role))
    tmp$role[tmp$role == "0"] <- ""
    sect2txt(tmp, "meta$players_h", "[3PLAYERS-H]")
}

dvw_set <- function(x, text_encoding) {
    tmp <- sect2txt(x$meta$result[, setdiff(names(x$meta$result), c("score_home_team", "score_visiting_team"))], "meta$result", "[3SET]")
    ## note that input might have unplayed sets, e.g.
    ## True;;;;;;
    ## as extra lines, but these have been stripped during dv_read
    c(tmp, rep("True;;;;;;", 6-length(tmp)))
}

dvw_comments <- function(x, text_encoding) sect2txt(x$meta$comments, "meta$comments", "[3COMMENTS]")

dvw_more <- function(x, text_encoding) c(sect2txt(x$meta$more, "meta$more ", "[3MORE]"), ";0;0;")

dvw_teams <- function(x, text_encoding) {
    tmp <- x$meta$teams[, setdiff(names(x$meta$teams), c("home_away_team", "won_match"))]
    if (is.character(tmp$shirt_colour)) tmp$shirt_colour <- dv_rgb2int(tmp$shirt_colour)
    sect2txt(tmp, "meta$teams", "[3TEAMS]")
}

dvw_match <- function(x, text_encoding, date_format) {
    ## [3MATCH]
    ## @@match_date@@;@@match_time@@;@@season@@;@@league@@;;;;;UTF-8;1;@@cones_zones@@;0;
    ## ;;12345;;;;;;
    ## not sure what the final line is. The 12345 is a dummy value, obviously
    mm <- x$meta$match
    if (is.null(mm)) stop("missing the meta$match component of the input object")
    if (!missing(text_encoding)) mm$text_encoding <- text_encoding
    if (!is.na(mm$date)) {
        if (!is.character(mm$date)) try(mm$date <- format(mm$date, date_format), silent = TRUE)
    }
    if (mm$regulation %eq% "indoor sideout") {
        mm$regulation <- 0L
    } else if (mm$regulation %eq% "beach rally point") {
        mm$regulation <- 2L
    } else {
        ## if "indoor rally point", but also treat as the default
        mm$regulation <- 1L
    }
    c("[3MATCH]", df2txt(mm), ";;12345;;;;;;")
}

dvw_header <- function(x, text_encoding, date_format) {
    fm <- x$file_meta
    if (is.null(fm)) stop("missing file_meta component of the input object")
    tdnow <- Sys.time()
    tdformat <- function(z) {
        if (is.character(z)) {
            z ## as-is
        } else {
            tryCatch(format(z, paste0(date_format, " %H.%M.%S")), error = function(e) NULL)
        }
    }
    c("[3DATAVOLLEYSCOUT]",
      paste0("FILEFORMAT: ", not_null_or(fm$fileformat, "2.0")),
      paste0("GENERATOR-DAY: ", not_null_or(tdformat(fm$generator_day), tdformat(tdnow))),
      paste0("GENERATOR-IDP: ", not_null_or(fm$generator_idp, "")),
      paste0("GENERATOR-PRG: ", not_null_or(fm$generator_prg, "")),
      paste0("GENERATOR-REL: ", not_null_or(fm$generator_rel, "")),
      paste0("GENERATOR-VER: ", not_null_or(fm$generator_ver, "")),
      paste0("GENERATOR-NAM: ", not_null_or(fm$generator_nam, "")),
      paste0("LASTCHANGE-DAY: ", tdformat(tdnow)),
      paste0("LASTCHANGE-IDP: datavolley"),
      paste0("LASTCHANGE-PRG: datavolley-R"),
      paste0("LASTCHANGE-REL: ", packageVersion("datavolley")),
      paste0("LASTCHANGE-VER: "),
      paste0("LASTCHANGE-NAM: "))
}

dvw_scout <- function(x, text_encoding = text_encoding) {
    xp <- plays(x)
    if (is.null(xp)) stop("missing the plays component of the input object")
    if (nrow(xp) < 1) return("[3SCOUT]")
    ## drop TTOs
    xp <- xp[!xp$skill %eq% "Technical timeout", ]
    ## if the data have been modified then the original code may need changing
    xp$code_modified <- vapply(seq_len(nrow(xp)), function(z) line2code(xp[z, ]), FUN.VALUE = "")
    xp$na_col <- NA_character_ ## for unused output columns
    ## some reformatting
    xp$time <- format(xp$time, "%H.%M.%S")
    ## map some changed values back to DV's encoding
    ## col 2 is "point/sideout"; "p" = winning attack in breakpoint, "s" = winning attack in sideout
    this <- xp$point_phase
    this[this %eq% "Breakpoint"] <- "p"
    this[this %eq% "Sideout"] <- "s"
    xp$point_phase <- this
    this <- xp$attack_phase
    this[this %eq% "Transition breakpoint"] <- "p"
    this[this %eq% "Transition sideout"] <- "s"
    this[this %eq% "Reception"] <- "r"
    xp$attack_phase <- this
    ## setter position uses 0 or 5 when unknown
    xp$home_setter_position[is.na(xp$home_setter_position)] <- 5
    xp$visiting_setter_position[is.na(xp$visiting_setter_position)] <- 5
    nms <- c("code_modified", "point_phase", "attack_phase", "na_col", ## cols 1-4
             "start_coordinate", "mid_coordinate", "end_coordinate", ## cols 5-7
             "time", ## col 8, HH.MM.SS format
             "set_number", ## col 9
             "home_setter_position", ## 10
             "visiting_setter_position", # 11
             "video_file_number", ## 12
             "video_time", ## 13
             "na_col", ## 14 need to check this one
             paste("home_p", 1:6, sep = ""), ## 15-20, home team, court positons 1-6, entries are player numbers
             paste("visiting_p", 1:6, sep = ""), ## 21-26, same for visiting team
             "na_col" ## always a trailing ;
             )
    ## if beach, xp won't have home_p3 etc
    to_add <- setdiff(nms, names(xp))
    if (length(to_add) > 0) xp[, to_add] <- NA
    c("[3SCOUT]", df2txt(xp[, nms]))
}

## rebuild the scouted code from the plays dataframe row
## for now, just use the existing code
line2code <- function(z) {
    z$code
}

