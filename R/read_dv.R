## main reader function

#' Read a datavolley file
#'
#' The \code{do_transliterate} option may be helpful when trying to work with multiple files from the same competition, since different text encodings may be used on different files. This can lead to e.g. multiple versions of the same team name. Transliterating can help avoid this, at the cost of losing e.g. diacriticals. Transliteration is applied after converting from the specified text encoding to UTF-8. Common encodings used with DataVolley files include "windows-1252" (western Europe), "windows-1250" (central Europe), "iso-8859-1" (western Europe and Americas), "iso-8859-2" (central/eastern Europe), "iso-8859-13" (Baltic languages)
#'
#' @references \url{http://www.dataproject.com/IT/en/Volleyball}
#' @param filename string: file name to read
#' @param insert_technical_timeouts logical or list: should we insert technical timeouts? If TRUE, technical timeouts are inserted at points 8 and 16 of sets 1--4 (for indoor files) or when the team scores sum to 21 in sets 1--2 (beach). Otherwise a two-element list can be supplied, giving the scores at which technical timeouts will be inserted for sets 1--4, and  set 5.
#' @param do_warn logical: should we issue warnings about the contents of the file as we read it?
#' @param extra_validation numeric: should we run some extra validation checks on the file? 0=no extra validation, 1=check only for major errors, 2=somewhat more extensive, 3=the most extra checking
#' @param validation_options list: additional options to pass to the validation step. See \code{help('validate_dv')} for details
#' @param do_transliterate logical: should we transliterate all text to ASCII? See details
#' @param encoding character: text encoding to use. Text is converted from this encoding to UTF-8. A vector of multiple encodings can be provided, and this function will attempt to choose the best. If encoding is "guess", the encoding will be guessed
#' @param date_format string: the expected date format (one of "ymd", "mdy", or "dmy") or "guess". If \code{date_format} is something other than "guess", that date format will be preferred where dates are ambiguous
#' @param surname_case string or function: should we change the case of player surnames? If \code{surname_case} is a string, valid values are "upper","lower","title", or "asis"; otherwise \code{surname_case} may be a function that will be applied to the player surname strings
#' @param skill_evaluation_decode function or string: if \code{skill_evaluation_decode} is a string, it can be either "default" (use the default DataVolley conventions for dvw or vsm files), "volleymetrics" (to follow the scouting conventions used by VolleyMetrics), "german" (same as "default" but with B/ and B= swapped), or "guess" (use volleymetrics if it looks like a VolleyMetrics file, otherwise default). If \code{skill_evaluation_decode} is a function, it should convert skill evaluation codes into meaningful phrases. See \code{\link{skill_evaluation_decoder}}
#' @param custom_code_parser function: function to process any custom codes that might be present in the datavolley file. This function takes one input (the \code{datavolley} object) and should return a list with two named components: \code{plays} and \code{messages}
#' @param metadata_only logical: don't process the plays component of the file, just the match and player metadata
#' @param verbose logical: if TRUE, show progress
#' @param edited_meta list: [very much experimental] if supplied, will be used in place of the metadata present in the file itself. This makes it possible to, for example, read a file, edit the metadata, and re-parse the file but using the modified metadata
#'
#' @return A named list with several elements. \code{meta} provides match metadata, \code{plays} is the main play-by-play data in the form of a data.frame. \code{raw} is the line-by-line content of the datavolley file. \code{messages} is a data.frame describing any inconsistencies found in the file.
#'
#' @seealso \code{\link{skill_evaluation_decoder}} \code{\link{validate_dv}}
#' @examples
#' \dontrun{
#'   ## to read the example file bundled with the package
#'   myfile <- dv_example_file()
#'   x <- dv_read(myfile, insert_technical_timeouts=FALSE)
#'   summary(x)
#'
#'   ## or to read your own file:
#'   x <- dv_read("c:/some/path/myfile.dvw", insert_technical_timeouts=FALSE)
#'
#'   ## Insert a technical timeout at point 12 in sets 1 to 4:
#'   x <- dv_read(myfile, insert_technical_timeouts=list(c(12),NULL))
#'
#'   ## to read a VolleyMetrics file
#'   x <- dv_read(myfile, skill_evaluation_decode = "volleymetrics")
#' }
#' @export
read_dv <- function(filename, insert_technical_timeouts=TRUE, do_warn=FALSE, do_transliterate=FALSE, encoding="guess", date_format = "guess", extra_validation=2, validation_options=list(), surname_case="asis", skill_evaluation_decode="default", custom_code_parser, metadata_only=FALSE, verbose=FALSE, edited_meta) {
    assert_that(is.string(filename))
    if (nchar(filename) < 1) stop("filename was specified as an empty string (\"\")")
    if (!file.exists(filename)) stop("specified input file (", filename, ") does not exist")
    assert_that(is.flag(insert_technical_timeouts) || is.list(insert_technical_timeouts))
    assert_that(is.flag(do_warn), !is.na(do_warn))
    assert_that(is.flag(do_transliterate), !is.na(do_transliterate))
    assert_that(is.flag(metadata_only), !is.na(metadata_only))
    assert_that(is.flag(verbose), !is.na(verbose))
    assert_that(is.string(date_format))
    date_format <- match.arg(tolower(date_format), c("guess", "dmy", "ymd", "mdy"))
    date_format_suggested <- NULL
    assert_that(is.numeric(extra_validation),extra_validation %in% 0:3)
    assert_that(is.list(validation_options))
    assert_that(is.string(surname_case) || is.function(surname_case))


    ft <- dv_file_type(filename) ## dvw, vsm, psvb, hxml
    if (ft == "psvb") {
        stop("for perana/vbstats files use `pv_read` from the peranavolley package")
    } else if (ft == "unknown") {
        ## treat as dvw
        ft <- "dvw"
    } else if (!ft %in% c("dvw", "hxml", "vsm")) {
        stop("unknown or unsupported file type")
    }
    if (ft == "dvw") dvlines <- stringi::stri_trans_general(readLines(filename, warn = FALSE, n = 100L), "latin-ascii")
    ## figure out the scouting conventions to follow
    if (is.string(skill_evaluation_decode)) {
        skill_evaluation_decode <- match.arg(tolower(skill_evaluation_decode), c("default", "volleymetrics", "guess", "german"))
        if (skill_evaluation_decode == "guess") {
            ## if file type is vsm or hxml, pass the guessing responsibility to the specific handler
            if (ft == "dvw") {
                ## otherwise for dvw, guess here
                is_vm <- tryCatch(any(grepl("volleymetric", dvlines, ignore.case = TRUE)), error = function(e) FALSE)
                skill_evaluation_decode <- if (is_vm) "volleymetrics" else "default"
            } else if (ft == "psvb") {
                skill_evaluation_decode <- "default"
            }
            if (ft == "dvw" && skill_evaluation_decode %eq% "volleymetrics" && is.null(date_format_suggested)) date_format_suggested <- "mdy"
        }
        if (ft == "dvw") skill_evaluation_decode <- skill_evaluation_decoder(style = skill_evaluation_decode)
    }
    ## pass non-dvw readers off to their dedicated read functions
    if (ft == "vsm") {
        return(dv_read_vsm(filename, skill_evaluation_decode = skill_evaluation_decode, insert_technical_timeouts = insert_technical_timeouts, do_transliterate = do_transliterate, extra_validation = extra_validation, validation_options = validation_options, verbose = verbose))
    } else if (ft == "hxml") {
        return(dv_read_hxml(filename, skill_evaluation_decode = skill_evaluation_decode, insert_technical_timeouts = insert_technical_timeouts, do_transliterate = do_transliterate, extra_validation = extra_validation, validation_options = validation_options, verbose = verbose))
    }
    assert_that(is.function(skill_evaluation_decode))
    if (is.null(date_format_suggested)) {
        ## check if the decoder function is the volleymetrics one
        is_vm <- tryCatch(identical(get("dtbl", envir = environment(skill_evaluation_decode)), get("dtbl", envir = environment(skill_evaluation_decoder(style = "volleymetrics")))), error = function(e) FALSE)
        if (is_vm) date_format_suggested <- "mdy"
    }
    ## note that we use a preferred date format of mdy for volleymetrics files here, but if a user has opened and re-saved the file with different date format, this will be wrong ...
    if (!missing(edited_meta)) {
        assert_that(is.list(edited_meta))
        ## test for names? c("match", "more", "result", "teams", "players_h", "players_v", "attacks", "sets", "match_id", "filename")
    }
    if (!missing(custom_code_parser)) assert_that(is.function(custom_code_parser) || is.null(custom_code_parser))
    ## don't do this unless FIVB rules change?
    ##    if (missing(insert_technical_timeouts)) warning("the current default value insert_technical_timeouts=TRUE will change to FALSE in a forthcoming release")
    out <- list()
    ## read raw lines in
    file_text <- readLines(filename,warn=FALSE)
    assert_that(is.character(encoding))
    if (length(encoding)>1 || identical(tolower(encoding), "guess")) {
        ## try to guess encoding based on the first few lines of the file
        ## test from [3TEAMS] section to end of [3PLAYERS-V] (just before [3ATTACKCOMBINATION])
        idx1 <- suppressWarnings(grep("[3MATCH]",file_text,fixed=TRUE))
        idx2 <- suppressWarnings(grep("[3SETTERCALL]",file_text,fixed=TRUE))+1
        ## fallback
        if (length(idx1)<1 || is.na(idx1)) idx1 <- 10
        if (length(idx2)<1 || is.na(idx2)) idx2 <- 90
        tst <- paste(file_text[idx1:idx2],collapse="")
        if (identical(tolower(encoding),"guess")) {
            ## first try using the embedded encoding info in the 3MATCH section
            textenc <- tryCatch(suppressWarnings({
                idx <- suppressWarnings(grep("[3MATCH]", file_text, fixed=TRUE))
                setdiff(as.character(read.table(text=file_text[idx+1],sep=";",quote="",stringsAsFactors=FALSE,header=FALSE)$V9), "1") ## 1 seems to be used to indicate the default locale encoding, which doesn't help us
            }), error=function(e) NULL)
            encoding <- stri_enc_detect2(tst)[[1]]$Encoding
            if (!is.null(textenc)) {
                enclist <- intersect(paste0(c("windows-", "cp"), tolower(textenc)), tolower(iconvlist()))
                enclist <- intersect(enclist, tolower(encoding)) ## don't use the embedded encoding if it isn't the suggested list from stri_enc_detect2
                if (length(enclist)>0) {
                    try({
                        out <- read_dv(filename=filename, insert_technical_timeouts=insert_technical_timeouts, do_warn=do_warn, do_transliterate=do_transliterate, encoding=enclist[1], date_format = date_format, extra_validation=extra_validation, validation_options=validation_options, surname_case=surname_case, skill_evaluation_decode=skill_evaluation_decode, custom_code_parser=custom_code_parser, metadata_only=metadata_only, verbose=verbose, edited_meta=edited_meta)
                        ## TODO: need to check that this actually worked, because there are files with the wrong encoding specified in their metadata
                        ## some files also appear to use different encodings for the home/visiting player lists
                        if (verbose) message(sprintf("Using text encoding: %s", enclist[1]))
                        return(out)
                    }, silent=TRUE)
                    ## if that fails, we'll drop through to our previous guessing code
                }
            }
            ## stri might return "x-iso*" encodings, but iconvlist() doesn't have them. Can these be treated just as iso*?
            ##xiso_idx <- grepl("^x\\-iso",encoding,ignore.case=TRUE)
            ##if (any(xiso_idx))
            ##    encoding <- c(encoding,gsub("^x\\-iso","iso",encoding[xiso_idx]))
            ## add common ones
            encoding <- c(encoding, c("windows-1252", "iso-8859-2", "windows-1250", "US-ASCII", "UTF-8", "SHIFT-JIS", "CP932", "windows-1251")) ## windows-1252 should be used in preference to "iso-8859-1", see https://en.wikipedia.org/wiki/ISO/IEC_8859-1
            encoding <- encoding[tolower(encoding) %in% tolower(iconvlist())]
            ##if (length(encoding)<=1) encoding <- iconvlist()
        }
        encoding <- unique(encoding)
        expect_tildes <- tryCatch(!any(grepl("PRG: Essential Stats", dvlines)), error = function(e) FALSE)
        encoding <- get_best_encodings(encoding, filename = filename, read_from = idx1, read_to = idx2, expect_tildes = expect_tildes)
        if (length(encoding$encodings) < 1) stop("error in guessing text encoding")
        if (encoding$error_score > 0) {
            ## haven't found an encoding with zero error score, but we have relied on stri_enc_detect2
            ## now just brute force it over all possible encodings (will be slow)
            encoding_brute <- get_best_encodings(iconvlist(), filename = filename, read_from = idx1, read_to = idx2)
            if (length(encoding_brute$encodings) > 0 && encoding_brute$error_score < encoding$error_score) encoding <- encoding_brute
        }
        encoding <- encoding$encodings
        ## so now we have a list of possible encodings
        ## in order of preference: a windows encoding, then UTF-8, then US-ASCII, then just whatever was first
        other_enc <- encoding
        if (any(grepl("^windows",tolower(encoding)))) {
            encoding <- encoding[grepl("^windows",tolower(encoding))][1]
        } else if (any(tolower(encoding) %in% c("utf-8", "utf8"))) {
            encoding <- encoding[tolower(encoding) %in% c("utf-8", "utf8")][1]
        } else if (any(tolower(encoding) %in% c("us-ascii"))) {
            encoding <- encoding[tolower(encoding) %in% c("us-ascii")][1]
        } else {
            encoding <- encoding[1]
        }
        other_enc <- setdiff(other_enc, encoding)
        if (verbose) {
            message(sprintf("Using text encoding: %s", encoding))
            if (length(other_enc) > 0)
                message(sprintf(" (Other possible options: %s)", paste(other_enc, collapse = ", ")))
        }
    }
    ## look for the "Secondo tocco di  la" with odd encoding on the trailing a
    ## this seems to be part of the default dv-generated file structure, so it's a common problem
    file_text <- enc::read_lines_enc(filename, file_encoding = encoding)
    file_text <- gsub("Secondo tocco di[[:space:]]+l.;","Secondo tocco di la;", file_text)

##    file_text <- iconv(file_text,from=encoding,to="utf-8") ## convert to utf-8
    ## so we got to here, either by reading the file, or using the supplied file_text
    out$raw <- file_text
    if (do_transliterate) {
        ##if (missing(encoding)) warning("transliteration may not work without an encoding specified")
        file_text <- stri_trans_general(file_text,"latin-ascii") ##file_text <- iconv(file_text,from="utf-8",to="ascii//TRANSLIT")
    }
    ## file metadata
    if (!do_warn) {
        suppressWarnings(temp <- read_filemeta(file_text, date_format = if (date_format == "guess") date_format_suggested else date_format))
    } else {
        temp <- read_filemeta(file_text, date_format = if (date_format == "guess") date_format_suggested else date_format)
    }
    ## if the last-change date was unambiguous, the match date should follow this (it doesn't always, but it's often true)
    ## at this point: date_format is the date_format specified by the user (or default "guess")
    ## date_format_suggested is either NULL or "mdy" if we have suggested the volleymetrics format
    date_format <- c(setdiff(date_format, "guess"), temp$lastchange_date_format, date_format_suggested) ## in order of preference, some of those might be NULL
    if (length(date_format) < 1) date_format <- NULL
    ## insert preferred date_format so it can be used if this object gets passed to dv_write
    ##  this isn't the ideal solution, because they actual date format in the file might not actually follow the preferred date_format
    ##  but it's a start
    temp$file_meta$preferred_date_format <- if (length(date_format) > 0) date_format[1] else NULL
    out$file_meta <- temp$file_meta
    out$messages <- temp$messages
    if (is.null(out$messages)) out$messages <- data.frame(file_line_number=integer(), video_time=numeric(), message=character(), file_line=character(), stringsAsFactors=FALSE)
    ## match metadata
    if (missing(edited_meta)) {
        if (!do_warn) {
            suppressWarnings(temp <- read_meta(file_text, surname_case, date_format = date_format))
        } else {
            temp <- read_meta(file_text, surname_case, date_format = date_format)
        }
        out$meta <- temp$meta
        out$meta$filename <- filename
    } else {
        out$meta <- edited_meta
    }
    file_type <- "indoor"
    if ((nrow(out$meta$players_h) == 2 && nrow(out$meta$players_h) == 2) || (!is.null(out$meta$match$regulation) && grepl("beach", out$meta$match$regulation))) {
        ## assume is a beach volley file
        file_type <- "beach"
    }
    out$file_meta$file_type <- file_type
    if (!is.null(temp$messages) && nrow(temp$messages)>0) {
        temp$messages$file_line <- as.character(temp$messages$file_line)
        out$messages <- bind_rows(out$messages, temp$messages)
    }
    if (metadata_only) return(out)
    this_main <- NULL
    thismsg <- NULL
    tryCatch({
        ## try using the already-read file text first, because it has been read with the proper encoding
        try({
            idx <- grep("[3SCOUT]", file_text, fixed = TRUE)
            if (length(idx) == 1 && idx < length(file_text)) {
                tmp <- file_text[idx:length(file_text)]
                if (!do_warn) {
                    suppressWarnings(this_main <- read_main(file_text = tmp))
                } else {
                    this_main <- read_main(file_text = tmp)
                }
            }
        }, silent = TRUE)
        if (is.null(this_main)) {
            if (!do_warn) {
                this_main <- suppressWarnings(read_main(filename))
            } else {
                this_main <- read_main(filename)
            }
        }
    }, error=function(e) {
        ## if file ends with 0x00, fread from the file will fail
        ## but already have the file contents as file_text, so use that
        if (grepl("Expected sep (';') but new line, EOF (or other non printing character) ends field 0", e$message, fixed = TRUE)) {
            thismsg <<- data.frame(file_line_number = length(file_text), video_time = NA_real_, message = "File ends with null line", file_line = "", severity = 3, stringsAsFactors = FALSE)
            idx <- grep("[3SCOUT]", file_text, fixed = TRUE)
            if (length(idx) == 1 && idx < length(file_text)) {
                tmp <- file_text[idx:length(file_text)]
                if (!do_warn) {
                    suppressWarnings(this_main <<- read_main(file_text = tmp))
                } else {
                    this_main <<- read_main(file_text = tmp)
                }
            } else {
                stop("could not read file (error message was: ", e$message, ")")
            }
        } else {
            stop("could not read file (error message was: ", e$message, ")")
        }
    })
    if (is.null(this_main)) stop("could not read dv file (unspecified error)")
    ##if (!is.null(thismsg)) mymsgs <- bind_rows(mymsgs,thismsg)
    ## don't actually issue this warning, for now at least

    ## count line numbers: where do codes start from?
    suppressWarnings(cln <- grep("[3SCOUT]",file_text,fixed=TRUE))
    if (length(cln)==1) {
        cln <- (cln+1):length(file_text)
        ## file_text may have empty lines at end, which won't show up in codes from read_main
        cln <- cln[1:min(length(cln),length(this_main$code))]
    } else {
        cln <- NULL
    }
    temp <- parse_code(code = this_main$code, meta = out$meta, evaluation_decoder = skill_evaluation_decode, code_line_num = cln, full_lines = if (is.null(cln)) NULL else file_text[cln], file_type = file_type)
    out$plays <- temp$plays
    if (!is.null(temp$messages) && nrow(temp$messages)>0) {
        temp$messages$file_line <- as.character(temp$messages$file_line)
        out$messages <- bind_rows(out$messages, temp$messages)
    }
    ## post-process plays data
    ##add the recognised columns from main to plays (note that we are discarding a few columns from main here)
    team_player_num <- if (grepl("beach", file_type)) 1:2 else 1:6
    out$plays <- cbind(this_main[, c("time", "video_file_number", "video_time")], out$plays, this_main[, c(paste0("home_p", team_player_num), paste0("visiting_p", team_player_num), "start_coordinate", "mid_coordinate", "end_coordinate", "point_phase", "attack_phase")])
    ## tidy up coordinates, and rescale to match zones and our ggcourt dimensions
    cxy <- dv_index2xy() ## grid of coords in our ggcourt space
    temp <- out$plays$start_coordinate
    ##temp[temp %in% c("-1-1", "")] <- NA_real_
    temp[grepl("-", temp, fixed = TRUE) | !nzchar(temp)] <- NA_real_ ## missing is usually "-1-1", but in some cases (DV bug?) can be e.g. "80-1", so turf out anything with "-" (or empty string)
    temp <- as.numeric(temp)
    temp[temp<1] <- NA_real_
    out$plays$start_coordinate <- temp
    out$plays$start_coordinate_x <- cxy[out$plays$start_coordinate, 1]
    out$plays$start_coordinate_y <- cxy[out$plays$start_coordinate, 2]
    temp <- out$plays$mid_coordinate
    temp[grepl("-", temp, fixed = TRUE) | !nzchar(temp)] <- NA_real_
    temp <- as.numeric(temp)
    temp[temp<1] <- NA_real_
    out$plays$mid_coordinate <- temp
    out$plays$mid_coordinate_x <- cxy[out$plays$mid_coordinate, 1]
    out$plays$mid_coordinate_y <- cxy[out$plays$mid_coordinate, 2]
    temp <- out$plays$end_coordinate
    temp[grepl("-", temp, fixed = TRUE) | !nzchar(temp)] <- NA_real_
    temp <- as.numeric(temp)
    temp[temp<1] <- NA_real_
    out$plays$end_coordinate <- temp
    out$plays$end_coordinate_x <- cxy[out$plays$end_coordinate, 1]
    out$plays$end_coordinate_y <- cxy[out$plays$end_coordinate, 2]
    ## add player_id values for home_p1 etc
    for (thisp in team_player_num)
        out$plays[,paste0("home_player_id",thisp)] <- get_player_id(rep("*",nrow(out$plays)),out$plays[,paste0("home_p",thisp)],out$meta)
    for (thisp in team_player_num)
        out$plays[,paste0("visiting_player_id",thisp)] <- get_player_id(rep("a",nrow(out$plays)),out$plays[,paste0("visiting_p",thisp)],out$meta)

    ## add set number
    if (!any(out$plays$end_of_set)) {
        out$messages <- bind_rows(out$messages, data.frame(file_line_number = NA_integer_, video_time = NA_real_, message = "Could not find any end-of-set markers (e.g. \"**1set\") in the input file", file_line = NA_character_, stringsAsFactors = FALSE))
        out$plays$set_number <- 1L
    } else {
        temp <- c(0L, which(out$plays$end_of_set))
        out$plays$set_number <- sapply(seq_len(nrow(out$plays)), function(z) sum(z >= temp))
        ## now fix the last **Xset entry in the file
        if (tail(temp, 1) == nrow(out$plays)) out$plays$set_number[nrow(out$plays)] <- out$plays$set_number[nrow(out$plays)] - 1L
        out$plays$home_team_score[which(out$plays$end_of_set)] <- NA_integer_
        out$plays$visiting_team_score[which(out$plays$end_of_set)] <- NA_integer_
    }

    ## technical timeouts
    if (is.flag(insert_technical_timeouts)) {
        if (insert_technical_timeouts) {
            insert_technical_timeouts <- if (grepl("beach", file_type)) list(function(s1, s2) (s1 + s2) == 21, NULL) else list(c(8,16),NULL)
        } else {
            insert_technical_timeouts <- list(NULL,NULL)
        }
    }
    set_sets <- if (grepl("beach", file_type)) list(1:2, 3) else list(1:4, 5)
    for (si in 1:2) {
        if (!is.null(insert_technical_timeouts[[si]])) {
            if (is.numeric(insert_technical_timeouts[[si]])) {
                ## find technical timeouts at e.g. points 8 and 16 in sets 1-4
                for (this_set in set_sets[[si]]) {
                    for (thisp in insert_technical_timeouts[[si]]) {
                        idx <- which((out$plays$home_team_score==thisp | out$plays$visiting_team_score==thisp) & out$plays$set_number==this_set)
                        if (length(idx) > 0) {
                            idx <- idx[1]
                            ##cat(sprintf("Inserting technical timeout at row %d (set %d, score %d)\n",idx,this_set,thisp))
                            out$plays <- bind_rows(out$plays[1:idx, ], data.frame(skill = "Technical timeout", timeout = TRUE, set_number = this_set, point = FALSE, end_of_set = FALSE, substitution = FALSE), out$plays[(idx+1):nrow(out$plays), ])
                        }
                    }
                }
            } else if (is.function(insert_technical_timeouts[[si]])) {
                ## function that is TRUE when a TTO should occur
                ## note this only allows one such TTO per set
                for (this_set in set_sets[[si]]) {
                    idx <- which(insert_technical_timeouts[[si]](out$plays$home_team_score, out$plays$visiting_team_score) & out$plays$set_number==this_set)
                    if (length(idx) > 0) {
                        idx <- idx[1]
                        out$plays <- bind_rows(out$plays[1:idx, ], data.frame(skill = "Technical timeout", timeout = TRUE, set_number = this_set, point = FALSE, end_of_set = FALSE, substitution = FALSE), out$plays[(idx+1):nrow(out$plays), ])
                    }
                }
            }
        }
    }

    ## add match_id
    out$plays$match_id <- out$meta$match_id

    ## turn plays times (character) into POSIXct
    temp <- paste(format(as.Date(out$meta$match$date)),out$plays$time,sep=" ")
    temp[out$plays$time=="" | is.na(out$plays$time)] <- NA
    suppressWarnings(out$plays$time <- lubridate::ymd_hms(temp))
    ##if (all(is.na(out$plays$time))) {
    ##    ## maybe match date failed to parse? try
    ##    suppressWarnings(out$plays$time <- lubridate::hms(temp))
    ## except this gives objects of class period, not POSIXct - hold off on this for now
    ##}

    ## add point_id - an identifier of each point. One point may consist of multiple attacks or other actions. Timeouts get assigned to their own "point", but other non-play rows may get assigned as part of a point.
    pid <- 0
    temp_point_id <- rep(NA,nrow(out$plays))
    temp_point_id[1] <- pid
    temp_point <- out$plays$point
    temp_timeout <- out$plays$timeout
    for (k in 2:nrow(out$plays)) {
        ##if ((!is.na(out$plays$skill[k]) && out$plays$skill[k]=="Serve") | out$plays$timeout[k]) { ## does not cope with sanctions
        if (temp_point[k-1] || temp_timeout[k] || temp_timeout[k-1]) { ## timeout[k-1] otherwise the following play does not start with a new point_id
            pid <- pid+1
        }
        temp_point_id[k] <- pid
    }
    out$plays$point_id <- temp_point_id
    ## fill in setter position
    temp_eos <- out$plays$end_of_set
    eos_idx <- which(temp_eos)
    out$plays$home_setter_position[eos_idx] <- NA_integer_
    out$plays$visiting_setter_position[eos_idx] <- NA_integer_
    temp_home_setter_position <- out$plays$home_setter_position
    temp_visiting_setter_position <- out$plays$visiting_setter_position
    hsp <- temp_home_setter_position[1]
    vsp <- temp_visiting_setter_position[1]
    for (k in 2:nrow(out$plays)) {
        if (isTRUE(temp_eos[k])) {
            ## start of new set, don't let current setter positions flow from last set
            hsp <- NA_integer_
            vsp <- NA_integer_
        }
        if (!is.na(temp_home_setter_position[k]))
            hsp <- temp_home_setter_position[k]
        temp_home_setter_position[k] <- hsp
        if (!is.na(temp_visiting_setter_position[k]))
            vsp <- temp_visiting_setter_position[k]
        temp_visiting_setter_position[k] <- vsp
    }
    out$plays$home_setter_position <- temp_home_setter_position
    out$plays$visiting_setter_position <- temp_visiting_setter_position

    ## add team_touch_id - an identifier of consecutive touches by same team in same point - e.g. a dig-set-attack sequence by one team is a "team touch"
    tid <- 0
    temp_ttid <- rep(NA, nrow(out$plays))
    temp_ttid[1] <- tid
    temp_team <- out$plays$team
    temp_ptid <- out$plays$point_id
    temp_skill <- out$plays$skill
    ## keep track of attacks - if a file has been scouted with only attacks, we need to account for that
    had_attack <- length(temp_skill) > 0 && temp_skill[1] %eq% "Attack" ## had_attack = had an attack already in this touch sequence
    for (k in seq_len(nrow(out$plays))[-1]) {
        if (!identical(temp_team[k], temp_team[k-1]) || !identical(temp_ptid[k], temp_ptid[k-1]) || (temp_skill[k] %eq% "Attack" && had_attack))  {
            tid <- tid+1
        }
        temp_ttid[k] <- tid
        had_attack <- temp_skill[k] %eq% "Attack"
    }
    out$plays$team_touch_id <- temp_ttid

    ## team name and ID
    idx <- out$meta$teams$home_away_team %eq% "*"
    home_team <- out$meta$teams$team[idx]
    home_team_id <- out$meta$teams$team_id[idx]
    idx <- out$meta$teams$home_away_team %eq% "a"
    visiting_team <- out$meta$teams$team[idx]
    visiting_team_id <- out$meta$teams$team_id[idx]
    ## replace * and a with team name
    temp <- temp_id <- rep(NA_character_, nrow(out$plays))
    idx <- out$plays$team %eq% "*"
    temp[idx] <- home_team
    temp_id[idx] <- home_team_id
    idx <- out$plays$team %eq% "a"
    temp[idx] <- visiting_team
    temp_id[idx] <- visiting_team_id
    out$plays$home_team <- home_team
    out$plays$visiting_team <- visiting_team
    out$plays$home_team_id <- home_team_id
    out$plays$visiting_team_id <- visiting_team_id
    out$plays$team <- temp
    out$plays$team_id <- temp_id

    ## keep track of who won each point
    temp <- as.data.frame(setNames(unique(out$plays[which(out$plays$point), c("point_id", "team")]), c("point_id", "point_won_by")))
    out$plays <- left_join(out$plays, temp, by = "point_id")
    ## catch any that we missed
    ##dud_point_id <- unique(out$plays$point_id[is.na(out$plays$point_won_by) & !out$plays$skill %in% c(NA,"Timeout","Technical timeout")])
    ##for (dpi in dud_point_id) {
    ##    tail(na.omit(out$plays[out$plays$point_id<dpi,c("home_team_score","visiting_team_score","point_won_by")]),1)
    ##    head(na.omit(out$plays[out$plays$point_id>dpi,c("home_team_score","visiting_team_score","point_won_by")]),1)
    ##}
#### not sure how to deal with these!

    ## winning attacks
    ## A followed by D with "Error" evaluation, or A with "Winning attack" evaluation
    temp1 <- out$plays[-nrow(out$plays),]
    temp2 <- out$plays[-1,]
    out$plays$winning_attack <- FALSE
    out$plays$winning_attack[1:(nrow(out$plays)-1)] <- temp1$skill=="Attack" & (temp1$evaluation=="Winning attack" | ((temp2$skill=="Dig" | temp2$skill=="Block") & temp2$evaluation=="Error"))
    ## note the block error is not actually needed there, since such attacks are recorded as winning ones anyway
    out$plays$winning_attack[is.na(out$plays$winning_attack)] <- FALSE
    ## fill in scores, so that all lines have a score
    scores <- unique(na.omit(out$plays[, c("point_id", "home_team_score", "visiting_team_score")]))
    scores <- left_join(out$plays[, "point_id", drop = FALSE], scores, by = "point_id")
    ## double-check
    if (any(na.omit(out$plays$home_team_score-scores$home_team_score) != 0) | any(na.omit(out$plays$visiting_team_score-scores$visiting_team_score) != 0)) stop("error in scores")

    temp_home_team_score <- scores$home_team_score
    temp_visiting_team_score <- scores$visiting_team_score
    ## will still have NA scores for timeouts and technical timeouts, patch NAs where we can
    temp_pt <- out$plays$point
    for (k in 2:nrow(out$plays)) {
        if (is.na(temp_home_team_score[k]) & !temp_pt[k]) {
            temp_home_team_score[k] <- temp_home_team_score[k-1]
        }
        if (is.na(temp_visiting_team_score[k]) & !temp_pt[k]) {
            temp_visiting_team_score[k] <- temp_visiting_team_score[k-1]
        }
    }
    out$plays$home_team_score <- temp_home_team_score
    out$plays$visiting_team_score <- temp_visiting_team_score

    ##out$plays$home_team_score <- scores$home_team_score
    ##out$plays$visiting_team_score <- scores$visiting_team_score
    #### will still have NA scores for timeouts and technical timeouts, patch NAs where we can
    ##for (k in 2:nrow(out$plays)) {
    ##    if (is.na(out$plays$home_team_score[k]) & !out$plays$point[k]) {
    ##        out$plays$home_team_score[k] <- out$plays$home_team_score[k-1]
    ##    }
    ##    if (is.na(out$plays$visiting_team_score[k]) & !out$plays$point[k]) {
    ##        out$plays$visiting_team_score[k] <- out$plays$visiting_team_score[k-1]
    ##    }
    ##}

    ## enforce some columns to be integer
    ints <- intersect(names(out$plays), c("player_number", "start_zone", "end_zone", "end_cone", "home_team_score", "visiting_team_score", "home_setter_position", "visiting_setter_position", "set_number"))
    for (i in ints) out$plays[,i] <- as.integer(out$plays[,i])

    ## add serving_team info
    who_served <- unique(out$plays[out$plays$skill %eq% "Serve", c("match_id", "point_id", "team")])
    who_served <- setNames(as.data.frame(who_served[!duplicated(who_served$point_id), ]), c("match_id", "point_id", "serving_team"))

    out$plays <- left_join(out$plays, dplyr::distinct(who_served, .data$match_id, .data$point_id, .keep_all = TRUE), by = c("match_id", "point_id"))
    out$plays$serving_team <- as.character(out$plays$serving_team) ## to be sure is not factor

    class(out) <- c("datavolley",class(out))
    class(out$plays) <- c("datavolleyplays",class(out$plays))

    ## add play phase
    out$plays$phase <- play_phase(out$plays)

    ## add scores at START of point
    out$plays$home_score_start_of_point <- ifelse(out$plays$point_won_by %eq% out$plays$home_team, as.integer(out$plays$home_team_score - 1L), as.integer(out$plays$home_team_score))
    out$plays$visiting_score_start_of_point <- ifelse(out$plays$point_won_by %eq% out$plays$visiting_team, as.integer(out$plays$visiting_team_score - 1L), as.integer(out$plays$visiting_team_score))

    ## now call custom code parser, if it was provided
    if (!missing(custom_code_parser) && !is.null(custom_code_parser)) {
        cx <- custom_code_parser(out)
        out$plays <- cx$plays
        if (!is.null(cx$messages) && nrow(cx$messages)>0) {
            cx$messages$file_line <- as.character(cx$messages$file_line)
            out$messages <- bind_rows(out$messages,cx$messages)
        }
    }

    ## ensure column ordering
    cls <- c("match_id", "point_id", "time", "video_file_number", "video_time", "code", "team", "player_number", "player_name", "player_id", "skill", "skill_type", "evaluation_code", "evaluation", "attack_code", "attack_description", "set_code", "set_description", "set_type", "start_zone", "end_zone", "end_subzone", "end_cone", "skill_subtype", "num_players", "num_players_numeric", "special_code", "timeout", "end_of_set", "substitution", "point", "home_team_score", "visiting_team_score", "home_setter_position", "visiting_setter_position", "custom_code", "file_line_number", paste0("home_p", 1:6), paste0("visiting_p", 1:6), "start_coordinate", "mid_coordinate", "end_coordinate", "point_phase", "attack_phase", "start_coordinate_x", "start_coordinate_y", "mid_coordinate_x", "mid_coordinate_y", "end_coordinate_x", "end_coordinate_y", paste0("home_player_id", 1:6), paste0("visiting_player_id", 1:6), "set_number", "team_touch_id", "home_team", "visiting_team", "home_team_id", "visiting_team_id", "team_id", "point_won_by", "winning_attack", "serving_team", "phase", "home_score_start_of_point", "visiting_score_start_of_point")
    out$plays <- out$plays[, c(intersect(cls, names(out$plays)), setdiff(names(out$plays), cls))]

    out$messages <- out$messages[,setdiff(names(out$messages),"severity")]
    ## apply additional validation
    if (extra_validation > 0) {
        moreval <- validate_dv(out, validation_level = extra_validation, options = validation_options, file_type = file_type)
        if (!is.null(moreval) && nrow(moreval) > 0) {
            moreval$file_line <- as.character(moreval$file_line)
            out$messages <- bind_rows(out$messages, moreval)
        }
    }
    if (is.null(out$messages)) out$messages <- data.frame(file_line_number=integer(), video_time=numeric(), message=character(), file_line=character(), stringsAsFactors=FALSE) ## should not happen, but just to be sure
    if (!is.null(out$messages) && nrow(out$messages)>0) {
        out$messages$file_line_number <- as.integer(out$messages$file_line_number)
        out$messages <- out$messages[order(out$messages$file_line_number, na.last = FALSE),]
        row.names(out$messages) <- NULL
    }

    ## some final fixes
    ## VS-exported dvw's can have an incorrect end location on reception if serve does not have an end location, but the set has a location
    idx <- out$plays$skill == "Reception" & lag(out$plays$skill) == "Serve" & lead(out$plays$skill) == "Set" & !is.na(out$plays$end_zone) & is.na(lag(out$plays$end_zone)) & out$plays$end_zone == lead(out$plays$end_zone)
    if (any(idx, na.rm = TRUE)) {
        out$plays <- mutate(out$plays, end_zone = case_when(idx ~ NA_integer_, TRUE ~ .data$end_zone),
                            end_subzone = case_when(idx ~ NA_character_, TRUE ~ .data$end_subzone))
        ## and remove the end zone/subzone from the codes on those rows
        temp <- out$plays$code[which(idx)]
        try({
            substr(temp, 11, 12) <- "~~"
            out$plays$code[which(idx)] <- sub("~+$", "", temp)
        })
    }
    if (do_warn) {
        ## spit the messages out
        for (k in 1:nrow(out$messages)) {
            vt <- if (is.na(out$messages$video_time[k])) "" else paste0(" (video time ",out$messages$video_time[k],")")
            cat(paste0("line ",out$messages$file_line_number[k],vt,": ",out$messages$message[k]," (line in file is: \"",out$messages$file_line[k],"\")"),"\n")
        }
    }
    out
}

#' @rdname read_dv
#' @export
dv_read <- read_dv

#' A simple summary of a volleyball match
#'
#' @param object datavolley: datavolley object as returned by \code{dv_read}
#' @param ... : additional arguments (currently these have no effect)
#'
#' @return list of summary items
#'
#' @seealso \code{\link{dv_read}}
#' @examples
#' x <- dv_read(dv_example_file(), insert_technical_timeouts=FALSE)
#' summary(x)
#'
#' @method summary datavolley
#' @export
summary.datavolley <- function(object,...) {
    out <- list(date=object$meta$match$date,league=object$meta$match$league)
    out$teams <- object$meta$teams[,c("team","coach","assistant","sets_won")] ## data.frame(team=object$meta$teams$team,coach=object$meta$teams$coach,assistant=object$meta$teams$assistant,sets_won=object$meta$teams$sets_won,stringsAsFactors=FALSE)
    temp <- object$meta$result$score_home_team>object$meta$result$score_visiting_team
    out$set_scores <- object$meta$result[,c("score_home_team","score_visiting_team")]
    ## make extra sure that set_scores has home team assigned correctly
    if (object$meta$teams$home_away_team[1]!="*") { out$set_scores <- out$set_scores[,2:1] }
    out$set_scores <- na.omit(out$set_scores)
    out$duration <- sum(object$meta$result$duration, na.rm = FALSE)
    class(out) <- "summary.datavolley"
    out
}

#' Print method for summary.datavolley
#'
#' @param x summary.datavolley: a summary.datavolley object as returned by \code{summary.datavolley}
#' @param ... : additional arguments (currently these have no effect)
#' @seealso \code{\link{summary.datavolley}}
#' @method print summary.datavolley
#' @export
print.summary.datavolley <- function(x,...) {
    out <- paste0("Match summary:\nDate: ", x$date, "\nLeague: ", x$league, "\n")
    coaches1 <- paste(Filter(Negate(is.na), c(x$teams$coach[1], x$teams$assistant[1])), collapse = "/")
    coaches1 <- if (length(coaches1) > 0 && nzchar(coaches1)) paste0(" (", coaches1, ")") else ""
    coaches2 <- paste(Filter(Negate(is.na), c(x$teams$coach[2], x$teams$assistant[2])), collapse = "/")
    coaches2 <- if (length(coaches2) > 0 && nzchar(coaches2)) paste0(" (", coaches2, ")") else ""
    out <- paste0(out, "Teams: ", x$teams$team[1], coaches1, "\n       vs\n       ", x$teams$team[2], coaches2, "\n")
    out <- paste0(out, "Result: ", x$teams$sets_won[1], "-", x$teams$sets_won[2], " (", paste(x$set_scores$score_home_team, x$set_scores$score_visiting_team, sep = "-", collapse = ", "), ")\n")
    out <- if (is.na(x$duration)) paste0(out, "Duration: unknown\n") else paste0(out, "Duration: ", x$duration, " minutes\n")
    cat(out)
    invisible(out)
}


#' Summarize a list of volleyball matches
#'
#' @param z list: list of datavolley objects as returned by \code{dv_read}
#'
#' @return named list with various summary indicators, including a competition ladder
#'
#' @seealso \code{\link{dv_read}}
#'
#' @examples
#' \dontrun{
#'   x <- dv_read(dv_example_file(), insert_technical_timeouts=FALSE)
#'   dvlist_summary(list(x,x)) ## same match duplicated twice, just for illustration purposes
#' }
#'
#' @export
dvlist_summary <- function(z) {
    out <- list(number_of_matches = length(z), number_of_sets = sum(vapply(z, function(z) as.integer(sum(z$meta$teams$sets_won)), FUN.VALUE = 1L, USE.NAMES = FALSE)))
    out$date_range <- range(as.Date(bind_rows(lapply(z, function(q) q$meta$match[, "date"]))$date))
    teams <- bind_rows(lapply(z, function(q) q$meta$teams[, c("team", "won_match")])) %>%
        group_by(.data$team) %>% dplyr::summarize(played = dplyr::n(), won = sum(.data$won_match), win_rate = .data$won / .data$played)

    temp <- bind_rows(lapply(z, function(q) {
        temp <- q$meta$teams[, c("team", "sets_won")]
        temp$sets_played <- sum(q$meta$teams$sets_won)
        temp
    })) %>% group_by(.data$team) %>%
        dplyr::summarize(sets_played = sum(.data$sets_played), sets_won = sum(.data$sets_won), set_win_rate = .data$sets_won / .data$sets_played)
    teams <- left_join(teams, temp, by = "team")

    temp <- bind_rows(lapply(z, function(q) {
        s <- summary(q)
        s$sc <- colSums(s$set_scores)
        data.frame(team = s$teams$team, points_for = s$sc, points_against = s$sc[2:1])
    }))
    teams <- left_join(teams,
                       temp %>% group_by(.data$team) %>% dplyr::summarize(points_for = as.integer(sum(.data$points_for)), points_against = as.integer(sum(.data$points_against))),
                       by = "team") %>%
        mutate(points_ratio = .data$points_for / .data$points_against) %>%
        dplyr::arrange(.data$team)

    out$ladder <- teams
    class(out) <- "summary.datavolleylist"
    out
}


#' Print method for summary.datavolleylist
#'
#' @param x summary.datavolleylist: a summary.datavolleylist object, as returned by \code{dvlist_summary}
#' @param ... : additional arguments (currently these have no effect)
#' @seealso \code{\link{dvlist_summary}}
#' @method print summary.datavolleylist
#' @export
print.summary.datavolleylist <- function(x,...) {
    cat(sprintf("Number of matches: %d\n",x$number_of_matches))
    cat(sprintf("Match dates: %s to %s\n",x$date_range[1],x$date_range[2]))
    cat(sprintf("Number of sets: %d\n",x$number_of_sets))
    print(x$ladder)
}


#' Extract the plays component from a datavolley object, or assign a new one
#'
#' @param x datavolley: a datavolley object as returned by \code{dv_read}
#' @param value datavolleyplays: new data
#'
#' @return The plays component of x (a data.frame), or a modified version of x with the new plays component inserted
#'
#' @seealso \code{\link{dv_read}}
#'
#' @examples
#' \dontrun{
#'   x <- dv_read(dv_example_file(), insert_technical_timeouts=FALSE)
#'   inspect(plays(x))
#'
#'   p2 <- plays(x)
#'   plays(x) <- p2
#' }
#' @export
plays <- function(x) {
    if ("plays" %in% names(x)) x$plays else stop("input has no plays component")
}

#' @rdname plays
#' @export
`plays<-` <- function(x, value) {
    x$plays <- value
    x
}

#' Convenience function for inspecting the plays component of a datavolley object
#'
#' @param x datavolleyplays: the plays component of a datavolley object as returned by \code{dv_read}
#' @param vars string: which variables to print? "minimal" set or "all"
#' @param maxrows numeric: maximum number of rows to print
#' @param extra character: names of any extra columns to include in the output
#'
#' @seealso \code{\link{dv_read}} \code{\link{plays}}
#'
#' @examples
#' \dontrun{
#'   x <- dv_read(dv_example_file(), insert_technical_timeouts=FALSE)
#'   inspect(plays(x))
#' }
#'
#' @export
inspect <- function(x,vars="minimal",maxrows=100,extra) {
    ##if (!(inherits(x,"datavolleyplays"))) stop("x must be a datavolleyplays object")
    vars <- match.arg(vars,c("minimal","all"))
    cols_to_show <- if (vars=="all") names(x) else c("time","code","team","player_number","player_name","skill","skill_type","evaluation")##,"match_id","set_number")
    if (!missing(extra)) {
        extra <- intersect(extra,names(x))
        cols_to_show <- c(cols_to_show,extra)
    }
    print(x[1:min(nrow(x),maxrows),cols_to_show])
}
