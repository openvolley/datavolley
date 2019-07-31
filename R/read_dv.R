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
#' @param encoding character: text encoding to use. Text is converted from this encoding to UTF-8. A vector of multiple encodings can be provided, and this function will attempt to choose the best (experimental). If encoding=="guess", the encoding will be guessed (really experimental)
#' @param surname_case string or function: should we change the case of player surnames? If \code{surname_case} is a string, valid values are "upper","lower","title", or "asis"; otherwise \code{surname_case} may be a function that will be applied to the player surname strings
#' @param skill_evaluation_decode function: function to use to convert skill evaluation codes into meaningful phrases. See \code{\link{skill_evaluation_decoder}}
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
#'   myfile <- example_dv_file()
#'   x <- read_dv(myfile, insert_technical_timeouts=FALSE)
#'   summary(x)
#'
#'   ## or to read your own file:
#'   x <- read_dv("c:/some/path/myfile.dvw", insert_technical_timeouts=FALSE)
#' 
#'   ## Insert a technical timeout at point 12 in sets 1 to 4:
#'   x <- read_dv(myfile, insert_technical_timeouts=list(c(12),NULL))
#' }
#' @export
read_dv <- function(filename, insert_technical_timeouts=TRUE, do_warn=FALSE, do_transliterate=FALSE, encoding="guess", extra_validation=2, validation_options=list(), surname_case="asis", skill_evaluation_decode=skill_evaluation_decoder(), custom_code_parser, metadata_only=FALSE, verbose=FALSE, edited_meta) {
    assert_that(is.flag(insert_technical_timeouts) || is.list(insert_technical_timeouts))
    assert_that(is.flag(do_warn), !is.na(do_warn))
    assert_that(is.flag(do_transliterate), !is.na(do_transliterate))
    assert_that(is.flag(metadata_only), !is.na(metadata_only))
    assert_that(is.flag(verbose), !is.na(verbose))
    assert_that(is.numeric(extra_validation),extra_validation %in% 0:3)
    assert_that(is.list(validation_options))
    assert_that(is.string(surname_case) || is.function(surname_case))
    assert_that(is.function(skill_evaluation_decode))
    if (!missing(edited_meta)) {
        assert_that(is.list(edited_meta))
        ## test for names? c("match", "more", "result", "teams", "players_h", "players_v", "attacks", "sets", "match_id", "filename")
    }
    if (!missing(custom_code_parser)) assert_that(is.function(custom_code_parser) || is.null(custom_code_parser))
    assert_that(is.string(filename))
    if (nchar(filename)<1)
        stop("filename was specified as an empty string (\"\")")
    if (!file.exists(filename))
        stop("specified input file (",filename,") does not exist")
    ## don't do this unless FIVB rules change?
    ##    if (missing(insert_technical_timeouts)) warning("the current default value insert_technical_timeouts=TRUE will change to FALSE in a forthcoming release")
    out <- list()
    ## read raw lines in
    file_text <- readLines(filename,warn=FALSE)
    assert_that(is.character(encoding))
    if (length(encoding)>1 || identical(tolower(encoding),"guess")) {
        ## try to guess encoding based on the first few lines of the file
        ## test from [3TEAMS] section to end of [3PLAYERS-V] (just before [3ATTACKCOMBINATION])
        idx1 <- suppressWarnings(grep("[3MATCH]",file_text,fixed=TRUE))
        idx2 <- suppressWarnings(grep("[3ATTACKCOMBINATION]",file_text,fixed=TRUE))+1
        ## fallback
        if (length(idx1)<1 || is.na(idx1)) idx1 <- 10
        if (length(idx2)<1 || is.na(idx2)) idx2 <- 80
        tst <- paste(file_text[idx1:idx2],collapse="")
        if (identical(tolower(encoding),"guess")) {
            ## first try using the embedded encoding info in the 3MATCH section
            textenc <- tryCatch(suppressWarnings({
                idx <- suppressWarnings(grep("[3MATCH]", file_text, fixed=TRUE))
                setdiff(as.character(read.table(text=file_text[idx+1],sep=";",quote="",stringsAsFactors=FALSE,header=FALSE)$V9), "1") ## 1 seems to be used to indicate the default locale encoding, which doesn't help us
            }), error=function(e) NULL)
            if (!is.null(textenc)) {
                enclist <- intersect(paste0(c("windows-", "cp"), tolower(textenc)), tolower(iconvlist()))
                if (length(enclist)>0) {
                    try({
                        out <- read_dv(filename=filename, insert_technical_timeouts=insert_technical_timeouts, do_warn=do_warn, do_transliterate=do_transliterate, encoding=enclist[1], extra_validation=extra_validation, validation_options=validation_options, surname_case=surname_case, skill_evaluation_decode=skill_evaluation_decode, custom_code_parser=custom_code_parser, metadata_only=metadata_only, verbose=verbose, edited_meta=edited_meta)
                        if (verbose) message(sprintf("Using text encoding: %s", enclist[1]))
                        return(out)
                    }, silent=TRUE)
                    ## if that fails, we'll drop through to our previous guessing code
                }
            }
            encoding <- stri_enc_detect2(tst)[[1]]$Encoding
            ## stri might return "x-iso*" encodings, but iconvlist() doesn't have them. Can these be treated just as iso*?
            ##xiso_idx <- grepl("^x\\-iso",encoding,ignore.case=TRUE)
            ##if (any(xiso_idx))
            ##    encoding <- c(encoding,gsub("^x\\-iso","iso",encoding[xiso_idx]))
            ## add common ones
            encoding <- c(encoding, c("windows-1252", "iso-8859-2", "windows-1250", "US-ASCII", "UTF-8", "SHIFT-JIS")) ## windows-1252 should be used in preference to "iso-8859-1", see https://en.wikipedia.org/wiki/ISO/IEC_8859-1
            encoding <- encoding[tolower(encoding) %in% tolower(iconvlist())]
            ##if (length(encoding)<=1) encoding <- iconvlist()
        }
        ##cat(encoding,"\n")
        ## badchars/badwords indicate characters/words that we don't expect to see, so the presence of any of these indicates that we've got the wrong file encoding
        badchars <- c(1025:7499,utf8ToInt("\ub3\ua3\u008a\u008e\u009a\u00b3")) ## cyrillic through to music, then some isolated ones
        ## not sure that next one should be considered bad char
        ##badchars <- c(badchars,utf8ToInt("\uf9"))##iconv("\xf9",from="latin2")))
        ## 0x2000 to 0x206f (general punctuation) likely wrong
        badchars <- c(badchars,0x2000:0x206f)
        badwords <- tolower(c("S\u159RENSEN","S\u159gaard","S\u159ren","M\u159LLER","Ish\u159j","Vestsj\u107lland","KJ\u107R","M\u159rk","Hj\u159rn","\u139rhus")) ## these from windows-1252 (or ISO-8859-1) wrongly guessed as windows-1250
        badwords <- c(badwords,tolower(c("\ud9ukas","Pawe\uf9","\ud9omacz",paste0("Mo\ufd","d\ufdonek"),"W\uf9odarczyk"))) ## these from windows-1257/ISO-8859-13 wrongly guessed as windows-1252
        badwords <- c(badwords,tolower(c("\u3a9ukas","Pawe\u3c9","\u3a9omacz",paste0("Mo\u3cd","d\u3cdonek"),"W\u3c9odarczyk"))) ## these from windows-1257/ISO-8859-13 wrongly guessed as windows-1253
        ##badwords <- c(badwords,tolower(c("\uc4\u15a\u49\uc4\u15a","SOR\uc4\u15a"))) ## utf-8 wrongly guessed as windows-1250
        badwords <- c(badwords,tolower(c("\uc4\u15a","\u139\u2dd"))) ## utf-8 wrongly guessed as windows-1250
        badwords <- c(badwords,tolower(c("Nicol\u148"))) ## windows-1252/iso-8859-1 wrongly guessed as windows-1250
        badwords <- c(badwords, tolower(c("\uc2\ue4\u77", "\uf1\u7b", "\ue5\ue4", "\ue5\ue3"))) ## japanese SHIFT-JIS wrongly guessed as macintosh
        ## get the \uxx numbers from sprintf("%x",utf8ToInt(dodgy_string_or_char))
        enctest <- sapply(encoding, function(tryenc)iconv(tst,from=tryenc))
        encerrors <- sapply(enctest, function(z)if (is.na(z)) Inf else sum(utf8ToInt(z) %in% badchars)+10*sum(sapply(badwords,grepl,tolower(z),fixed=TRUE)))
        encerr2 <- vapply(encoding, function(tryenc)tryCatch({blah <- read_match(iconv(file_text[idx1:idx2],from=tryenc)); TRUE},error=function(e) FALSE),FUN.VALUE=TRUE)
        encerrors[!encerr2] <- 999e3
        ##cat(str(sort(encerrors)),"\n")
        idx <- encerrors==min(encerrors)
        if (!any(idx)) stop("error in guessing text encoding")
        enctest <- enctest[idx]
        encoding <- encoding[idx]
                                        #cat(str(encoding))
                                        #cat(str(encerrors[idx]))
        ##cat(enctest,"\n\n\n")
        ##cat(utf8ToInt(substr(enctest,946,946)),"\n")
        other_enc <- c()
        if (FALSE) {##(any(duplicated(enctest))) {
            ## pick from the ones that give the most common output
            un <- unique(enctest)
            ui <- sapply(enctest,function(z)which(z==un))
            tmp <- as.data.frame(table(ui),stringsAsFactors=FALSE)
            umax <- as.numeric(tmp$ui[which.max(tmp$Freq)])
            ## want encoding that has ui==umax
            rset <- encoding[ui==umax]
            ## pick windows- if there is one, else first
            if (any(grepl("^windows",tolower(rset))))
                encoding <- rset[grepl("^windows",tolower(rset))][1]
            else
                encoding <- rset[1]
            other_enc <- setdiff(encoding,rset)
        } else {
            ## pick the first windows- encoding if there is one, else just pick first
            other_enc <- encoding
            if (any(grepl("^windows",tolower(encoding)))) {
                encoding <- encoding[grepl("^windows",tolower(encoding))][1]
            } else {
                encoding <- encoding[1]
            }
            other_enc <- setdiff(other_enc,encoding)
        }
        if (verbose) {
            message(sprintf("Using text encoding: %s",encoding))
            if (length(other_enc)>0)
                message(sprintf(" (Other possible options: %s)",paste(other_enc,collapse=", ")))
        }
    }
    ## look for the "Secondo tocco di  la" with odd encoding on the trailing a
    ## this seems to be part of the default dv-generated file structure, so it's a common problem
    file_text <- gsub("Secondo tocco di[[:space:]]+l\xe0;","Secondo tocco di la;",file_text)

    file_text <- iconv(file_text,from=encoding,to="utf-8") ## convert to utf-8
    ## so we got to here, either by reading the file, or using the supplied file_text
    out$raw <- file_text
    if (do_transliterate) {
        ##if (missing(encoding)) warning("transliteration may not work without an encoding specified")
        file_text <- stri_trans_general(file_text,"latin-ascii") ##file_text <- iconv(file_text,from="utf-8",to="ascii//TRANSLIT")
    }
    ## file metadata
    if (!do_warn) {
        suppressWarnings(temp <- read_filemeta(file_text))
    } else {
        temp <- read_filemeta(file_text)
    }
    out$file_meta <- temp$file_meta
    out$messages <- temp$messages
    if (is.null(out$messages)) out$messages <- data.frame(file_line_number=integer(), video_time=numeric(), message=character(), file_line=character(), stringsAsFactors=FALSE)
    ## match metadata
    if (missing(edited_meta)) {
        if (!do_warn) {
            suppressWarnings(temp <- read_meta(file_text, surname_case))
        } else {
            temp <- read_meta(file_text, surname_case)
        }
        out$meta <- temp$meta
        out$meta$filename <- filename
    } else {
        out$meta <- edited_meta
    }
    file_type <- "indoor"
    if (nrow(out$meta$players_h) == 2 && nrow(out$meta$players_h) == 2) {
        ## assume is a beach volley file
        file_type <- "beach"
    }
    out$file_meta$file_type <- file_type
    if (!is.null(temp$messages) && nrow(temp$messages)>0) out$messages <- rbind.fill(out$messages, temp$messages)
    this_main <- NULL
    tryCatch({
        thismsg <- NULL
        if (!do_warn) {
            this_main <- suppressWarnings(read_main(filename))
        } else {
            this_main <- read_main(filename)
        }},
        error=function(e) {
            ## if file ends with 0x00, fread from the file will fail
            ## but already have the file contents as file_text, so use that
            if (grepl("Expected sep (';') but new line, EOF (or other non printing character) ends field 0",e$message,fixed=TRUE)) {
                thismsg <<- data.frame(file_line_number=length(file_text),video_time=NA_integer_,message="File ends with null line",file_line="",severity=3,stringsAsFactors=FALSE)
                suppressWarnings(tmp <- file_text[grep("[3SCOUT]",file_text,fixed=TRUE):length(file_text)])
                if (!do_warn) {
                    suppressWarnings(this_main <<- read_main(paste0(tmp,collapse="\n")))
                } else {
                    this_main <<- read_main(paste0(tmp,collapse="\n"))
                }
            } else {
                stop("could not read file (error message was: ",e$message,")")
            }
        })
    if (is.null(this_main)) stop("could not read dv file (unspecified error)")
    ##if (!is.null(thismsg)) mymsgs <- rbind.fill(mymsgs,thismsg)
    ## don't actually issue this warning, for now at least

    if (metadata_only) return(out)
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
    if (!is.null(temp$messages) && nrow(temp$messages)>0) out$messages <- rbind.fill(out$messages, temp$messages)
    ## post-process plays data
    ##add the recognised columns from main to plays (note that we are discarding a few columns from main here)
    team_player_num <- if (grepl("beach", file_type)) 1:2 else 1:6
    out$plays <- cbind(this_main[, c("time", "video_time")], out$plays, this_main[, c(paste0("home_p", team_player_num), paste0("visiting_p", team_player_num), "start_coordinate", "mid_coordinate", "end_coordinate")])
    ## tidy up coordinates, and rescale to match zones and our ggcourt dimensions
    cxy <- dv_index2xy() ## grid of coords in our ggcourt space
    temp <- out$plays$start_coordinate
    temp[temp %in% c("-1-1", "")] <- NA_real_
    temp <- as.numeric(temp)
    temp[temp<1] <- NA_real_
    out$plays$start_coordinate <- temp
    out$plays$start_coordinate_x <- cxy[out$plays$start_coordinate, 1]
    out$plays$start_coordinate_y <- cxy[out$plays$start_coordinate, 2]
    temp <- out$plays$mid_coordinate
    temp[temp %in% c("-1-1", "")] <- NA_real_
    temp <- as.numeric(temp)
    temp[temp<1] <- NA_real_
    out$plays$mid_coordinate <- temp
    out$plays$mid_coordinate_x <- cxy[out$plays$mid_coordinate, 1]
    out$plays$mid_coordinate_y <- cxy[out$plays$mid_coordinate, 2]
    temp <- out$plays$end_coordinate
    temp[temp %in% c("-1-1", "")] <- NA_real_
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
        out$messages <- rbind.fill(out$messages, data.frame(file_line_number = NA_integer_, video_time = NA_real_, message = "Could not find any end-of-set markers (e.g. \"**1set\") in the input file", file_line = NA_character_, stringsAsFactors = FALSE))
        out$plays$set_number <- 1L
    } else {
        out$plays$set_number <- NA
        temp <- c(0L, which(out$plays$end_of_set))
        for (si in 2:length(temp)) {
            out$plays$set_number[(temp[si-1]+1):(temp[si]-1)] <- (si-1)
        }
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
                            out$plays <- rbind.fill(out$plays[1:idx, ], data.frame(skill = "Technical timeout", timeout = TRUE, set_number = this_set, point = FALSE, end_of_set = FALSE, substitution = FALSE), out$plays[(idx+1):nrow(out$plays), ])
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
                        out$plays <- rbind.fill(out$plays[1:idx, ], data.frame(skill = "Technical timeout", timeout = TRUE, set_number = this_set, point = FALSE, end_of_set = FALSE, substitution = FALSE), out$plays[(idx+1):nrow(out$plays), ])
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
    temp_home_setter_position <- out$plays$home_setter_position
    temp_visiting_setter_position <- out$plays$visiting_setter_position
    hsp <- temp_home_setter_position[1]
    vsp <- temp_visiting_setter_position[1]
    for (k in 2:nrow(out$plays)) {
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
    for (k in seq_len(nrow(out$plays))[-1]) {
        if (!identical(temp_team[k], temp_team[k-1]) || !identical(temp_ptid[k], temp_ptid[k-1]))  {
            tid <- tid+1
        }
        temp_ttid[k] <- tid
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
    temp <- ddply(out$plays,c("point_id"),function(z)if (any(z$point)) z$team[z$point] else NA_character_)
    names(temp) <- c("point_id","point_won_by")
    suppressMessages(out$plays <- join(out$plays,temp))
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
    scores <- unique(na.omit(out$plays[,c("point_id","home_team_score","visiting_team_score")]))
    scores <- suppressMessages(join(out$plays[,"point_id",drop=FALSE],scores))
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
    ints <- intersect(names(out$plays), c("player_number", "start_zone", "end_zone", "end_cone", "home_team_score", "visiting_team_score", "home_setter_position", "visiting_setter_position", "set_number")) ## , "video_time" should probably be int as well, but need to check first that this won't break anything
    for (i in ints) out$plays[,i] <- as.integer(out$plays[,i])

    ## add serving_team info
    who_served <- ddply(out$plays,c("match_id","point_id"),function(z)data.frame(serving_team=na.omit(z$team[z$skill %eq% "Serve"])[1],stringsAsFactors=FALSE))
    out$plays <- plyr::join(out$plays,who_served,by=c("match_id","point_id"),match="first")
    out$plays$serving_team <- as.character(out$plays$serving_team) ## to be sure is not factor
    
    class(out) <- c("datavolley",class(out))
    class(out$plays) <- c("datavolleyplays",class(out$plays))

    ## add play phase
    out$plays$phase <- play_phase(out$plays)
    
    ## now call custom code parser, if it was provided
    if (!missing(custom_code_parser) && !is.null(custom_code_parser)) {
        cx <- custom_code_parser(out)
        out$plays <- cx$plays
        if (!is.null(cx$messages) && nrow(cx$messages)>0) out$messages <- rbind.fill(out$messages,cx$messages)
    }

    out$messages <- out$messages[,setdiff(names(out$messages),"severity")]
    ## apply additional validation
    if (extra_validation > 0) {
        moreval <- validate_dv(out, validation_level = extra_validation, options = validation_options, file_type = file_type)
        if (!is.null(moreval) && nrow(moreval) > 0) {
            out$messages <- rbind.fill(out$messages, moreval)
        }
    }
    if (is.null(out$messages)) out$messages <- data.frame(file_line_number=integer(), video_time=numeric(), message=character(), file_line=character(), stringsAsFactors=FALSE) ## should not happen, but just to be sure
    if (!is.null(out$messages) && nrow(out$messages)>0) {
        out$messages$file_line_number <- as.integer(out$messages$file_line_number)
        out$messages <- out$messages[order(out$messages$file_line_number, na.last = FALSE),]
        row.names(out$messages) <- NULL
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


#' A simple summary of a volleyball match
#'
#' @param object datavolley: datavolley object as returned by \code{read_dv}
#' @param ... : additional arguments (currently these have no effect)
#'
#' @return list of summary items
#'
#' @seealso \code{\link{read_dv}}
#' @examples
#' x <- read_dv(dv_example_file(), insert_technical_timeouts=FALSE)
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
#' @param z list: list of datavolley objects as returned by \code{read_dv}
#'
#' @return named list with various summary indicators, including a competition ladder
#'
#' @seealso \code{\link{read_dv}}
#'
#' @examples
#' \dontrun{
#'   x <- read_dv(dv_example_file(), insert_technical_timeouts=FALSE)
#'   dvlist_summary(list(x,x)) ## same match duplicated twice, just for illustration purposes
#' }
#'
#' @export
dvlist_summary=function(z) {
    out <- list(number_of_matches=length(z),number_of_sets=sum(vapply(z,function(z)sum(z$meta$teams$sets_won),FUN.VALUE=as.integer(1),USE.NAMES=FALSE)))
    out$date_range <- range(ldply(z,function(q)as.Date(q$meta$match$date))$V1)
    temp <- as.character(sapply(z,function(q) q$meta$teams$team))
    teams <- as.data.frame(table(temp))
    names(teams) <- c("team","played")
    temp <- ldply(z,function(q)q$meta$teams[,c("team","won_match")])
    teams <- suppressMessages(join(teams,ddply(temp,c("team"),function(q)data.frame(won=sum(q$won_match)))))
    teams$win_rate <- teams$won/teams$played

    temp <- ddply(ldply(z,function(q){ temp <- q$meta$teams[,c("team","sets_won")]; temp$sets_played <- sum(q$meta$teams$sets_won); temp}),c("team"),function(z)data.frame(sets_played=sum(z$sets_played),sets_won=sum(z$sets_won)))##summarise,sets_played=sum(sets_played),sets_won=sum(sets_won))
    temp$set_win_rate <- temp$sets_won/temp$sets_played
    teams <- suppressMessages(join(teams,temp))
    
    temp <- ldply(z,function(q){ s <- summary(q); s$sc <- colSums(s$set_scores); data.frame(team=s$teams$team,points_for=s$sc,points_against=s$sc[2:1])})
    teams <- suppressMessages(join(teams,ddply(temp,c("team"),function(q)data.frame(points_for=as.integer(sum(q$points_for)),points_against=as.integer(sum(q$points_against))))))
    teams$points_ratio <- teams$points_for/teams$points_against
    teams <- teams[order(teams$team),] ##arrange(teams,team)
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


#' Extract the plays component from a datavolley object
#'
#' @param x datavolley: a datavolley object as returned by \code{read_dv}
#'
#' @return The plays component of x (a data.frame)
#'
#' @seealso \code{\link{read_dv}}
#'
#' @examples
#' \dontrun{
#'   x <- read_dv(dv_example_file(), insert_technical_timeouts=FALSE)
#'   inspect(plays(x))
#' }
#' @export
plays <- function(x) {
    if ("plays" %in% names(x)) x$plays else stop("input has no plays component")
}


#' Convenience function for inspecting the plays component of a datavolley object
#'
#' @param x datavolleyplays: the plays component of a datavolley object as returned by \code{read_dv}
#' @param vars string: which variables to print? "minimal" set or "all"
#' @param maxrows numeric: maximum number of rows to print
#' @param extra character: names of any extra columns to include in the output
#'
#' @seealso \code{\link{read_dv}} \code{\link{plays}}
#'
#' @examples
#' \dontrun{
#'   x <- read_dv(dv_example_file(), insert_technical_timeouts=FALSE)
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
