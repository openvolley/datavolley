## functions for dealing with the main data part of the datavolley file
skill_decode <- function(skill, code, full_line, line_num) {
    mymsgs <- list()
    if (!skill %in% c("S", "R", "A", "B", "D", "E", "F"))
        mymsgs <- collect_messages(mymsgs, paste0("Unexpected skill: ", skill), line_num, full_line, severity = 2)
    list(decoded = switch(EXPR = skill,
                          S = "Serve",
                          R = "Reception",
                          A = "Attack",
                          B = "Block",
                          D = "Dig",
                          E = "Set",
                          F = "Freeball",
                          paste0("Unknown skill: ", skill)),
         messages = mymsgs)
    }

attack_map <- function(type,skill) {
    switch(type,
           H=paste0("High ball ",skill),
           M=paste0("Half ball ",skill),
           Q=paste0("Quick ball ",skill),
           T=paste0("Head ball ",skill),
           U=paste0("Super ball ",skill),
           F=paste0("Fast ball ",skill),
           N=paste0("Slide ball ",skill),
           O=paste0("Other ",skill),
           paste0("Unknown ",skill," type"))
}

serve_map <- function(type, skill, file_type = "indoor") {
    if (grepl("beach", file_type)) {
        switch(type,
               Q=paste0("Jump ",skill),
               T=paste0("Jump-float ",skill),
               H=paste0("Standing ",skill),
               paste0("Unknown ",skill," type"))
    } else {
        switch(type,
               H=paste0("Float ",skill), ## volleymetrics use this for float far from the service line
               M=paste0("Jump-float ",skill),
               Q=paste0("Jump ",skill),
               T=paste0("Topspin ",skill), ## volleymetrics use this for float from the service line
               paste0("Unknown ",skill," type"))
    }
}

skill_type_decode <- function(skill, type, full_line, line_num, file_type = "indoor") {
    mymsgs <- list()
    this_allowed_types <- if (skill %in% c("S","R")) c("H", "M", "Q", "T") else c("H", "M", "Q", "T", "U", "F", "O", "N")
    if (!any(type == this_allowed_types))
        mymsgs <- collect_messages(mymsgs, paste0("Unexpected skill type: ", type, " for skill: ", skill), line_num, full_line, severity = 1)
    list(decoded = switch(EXPR = skill,
             S = serve_map(type, "serve", file_type = file_type),
             R = serve_map(type, "serve reception", file_type = file_type),
             A = attack_map(type, "attack"),
             B = attack_map(type, "block"),
             D = attack_map(type, "dig"),
             E = attack_map(type, "set"),
             F = "Unknown freeball type",
             paste0("Unknown skill type: ", type)),
         messages = mymsgs)
}



#' Translate skill evaluation codes into meaningful summary phrases
#'
#' If your DataVolley files use evaluation codes differently to those coded here, you will need to supply a custom
#' skill_evaluation_decode function to \code{\link{read_dv}}
#'
#' @param style string: currently "default" (following the standard definitions described in the DataVolley manual) or "volleymetrics" (per the conventions that VolleyMetrics use)
#' @return function. This function takes arguments skill, evaluation_code, and show_map and returns a string giving the interpretation of that skill evaluation code
#'
#' @seealso \code{\link{read_dv}}
#' @examples
#' sd <- skill_evaluation_decoder()
#' sd("S","#")
#' sd(show_map=TRUE)
#'
#' @export skill_evaluation_decoder
# @param skill string: one-character skill code, either "S"erve, "R"eception, "A"ttack, "B"lock, "D"ig, s"E"t, or "F"reeball
# @param evaluation_code string: one-character evaluation code, generally one of =/-+#!
# @param show_map logical: if TRUE, return the whole table being used to map evaluation codes to summary phrases

skill_evaluation_decoder <- function(style = "default") {
    style <- match.arg(tolower(style), c("default", "volleymetrics", "german"))
    ## reading this table is slow, so do it once and return the function
    dtbl <- read.table(text="skill^evaluation_code^evaluation
S^=^Error
S^/^Positive, no attack
S^-^Negative, opponent free attack
S^+^Positive, opponent some attack
S^#^Ace
S^!^OK, no first tempo possible
R^=^Error
R^/^Poor, no attack
R^-^Negative, limited attack
R^+^Positive, attack
R^#^Perfect pass
R^!^OK, no first tempo possible
A^=^Error
A^/^Blocked
A^-^Poor, easily dug
A^!^Blocked for reattack
A^+^Positive, good attack
A^#^Winning attack
B^=^Error
B^/^Invasion
B^-^Poor, opposition to replay
B^+^Positive, block touch
B^#^Winning block
B^!^Poor, opposition to replay
D^=^Error
D^/^Ball directly back over net
D^-^No structured attack possible
D^#^Perfect dig
D^+^Good dig
D^!^OK, no first tempo possible
E^=^Error
E^-^Poor
E^/^Poor
E^+^Positive
E^#^Perfect
E^!^OK
F^=^Error
F^/^Poor
F^!^OK, no first tempo possible
F^-^OK, only high set possible
F^+^Good
F^#^Perfect",sep="^",header=TRUE,comment.char="",stringsAsFactors=FALSE)
    ## customizations, currently not documented
    if (style=="volleymetrics") {
        dtbl$evaluation[dtbl$skill=="B" & dtbl$evaluation_code=="/"] <- "Poor, opposition to replay"
        ## B! is used for negative block, unplayable to our side
        dtbl$evaluation[dtbl$skill=="B" & dtbl$evaluation_code=="!"] <- "Poor, blocking team cannot recover" 
        ## B- is negative block touch, either back to opposition or poor on our side
        dtbl$evaluation[dtbl$skill=="B" & dtbl$evaluation_code=="-"] <- "Poor block" 
        ## B+ is used for positive block touch, either to our defense or difficult for opposition
        dtbl$evaluation[dtbl$skill=="B" & dtbl$evaluation_code=="+"] <- "Positive block"
        ## D/ is block cover that gives attacking team a chance to re-attack
        dtbl$evaluation[dtbl$skill=="D" & dtbl$evaluation_code=="/"] <- "Positive block cover"
        ## D! is block cover that does not give attacking team a chance to re-attack or is an error
        dtbl$evaluation[dtbl$skill=="D" & dtbl$evaluation_code=="!"] <- "Poor block cover"
        ## E/ is a reach
        dtbl$evaluation[dtbl$skill=="E" & dtbl$evaluation_code=="/"] <- "Error (reach over net)"
    } else if (style == "german") {
        ## swap B= Error and B/ Invasion
        dtbl$evaluation[dtbl$skill=="B" & dtbl$evaluation_code=="/"] <- "Error"
        dtbl$evaluation[dtbl$skill=="B" & dtbl$evaluation_code=="="] <- "Invasion"
    }
    ## extract the columns as vectors: faster
    dtbl_evaluation <- dtbl$evaluation
    dtbl_skill <- dtbl$skill
    dtbl_evaluation_code <- dtbl$evaluation_code
    function(skill,evaluation_code,show_map=FALSE) {  
        if (!is.logical(show_map)) show_map <- FALSE
        if (show_map) return(dtbl)

        skill <- as.character(skill)
        evaluation_code <- as.character(evaluation_code)
        if (nchar(skill)!=1) return(paste0("Unknown skill: ",skill))
        if (nchar(evaluation_code)!=1) return(paste0("Unknown skill evaluation code: ",evaluation_code))

        if (!any(skill==dtbl_skill)) return(paste0("Cannot decode evaluation for unknown skill ",skill))
        this_eval <- dtbl_evaluation[dtbl_skill==skill & dtbl_evaluation_code==evaluation_code]
        if (length(this_eval)<1) {
            full_skill_name <- switch(EXPR=skill,
                                      S="serve",
                                      R="pass",
                                      A="attack",
                                      B="block",
                                      D="dig",
                                      E="set",
                                      F="freeball")
            paste0("Unknown ",full_skill_name," evaluation")
        } else {
            this_eval
        }
    }
}

read_with_readr <- function(filename, file_text) {
    if (!missing(file_text)) {
        temp <- file_text
    } else {
        temp <- readLines(filename)
    }
    skip <- which(temp == "[3SCOUT]")
    if (length(skip) == 1 && skip < length(temp)) {
        ## previously we read direct from file, skipping the required number of lines. Note that this fails if we have embedded single-quotes in the file, because quoted newlines aren't counted in the line-skipping
        out <- suppressWarnings(suppressMessages(readr::read_delim(paste(temp[seq(skip+1, length(temp), by = 1L)], collapse = "\n"), delim = ";", progress = FALSE, col_names = FALSE, locale = readr::locale(encoding = "UTF-8"))))
        ## note that this can fail if non-standard-ascii chars have been used in the comments section of an input code
        attr(out, "problems") <- NULL
        attr(out, "spec") <- NULL
        out <- as.data.frame(out, stringsAsFactors = FALSE) ## so that we don't get caught by e.g. tibble column indexing differences to data.frames
        ## strip trailing all-NA rows, which would have come from trailing empty lines in the original file
        chk <- rle(rev(!nzchar(out$X1)))
        if (chk$values[1]) {
            out <- out[seq(from = 1, to = nrow(out)-chk$lengths[1], by = 1), ]
        }
        out
    } else {
        NULL
    }
}

read_main <- function(filename, file_text) {
    x <- tryCatch({ if (!missing(file_text)) read_with_readr(file_text = file_text) else read_with_readr(filename = filename) },
                  error = function(e) stop("could not read the [3SCOUT] section of the file, the error message was: ", conditionMessage(e)))

    if (is.null(x)) stop("could not read the [3SCOUT] section of the file")
    if (nrow(x) == 1 && ncol(x) == 1) {
        ## this happens if file has no scout data!
        stop("file has no scouted data (the [3SCOUT] section of the file is empty)")
    }
    names(x)[1] <- "code"
    ## col 2 is "point/sideout"; "p" = winning attack in breakpoint, "s" = winning attack in sideout
    names(x)[2] <- "point_phase"
    this <- x$point_phase
    this[this %eq% "p"] <- "Breakpoint"
    this[this %eq% "s"] <- "Sideout"
    x$point_phase <- this
    ## col 3 values "s" = transition attack during in sideout, "p" = transition attack in breakpoint, "r" = reception attack
    names(x)[3] <- "attack_phase"
    this <- x$attack_phase
    this[this %eq% "p"] <- "Transition breakpoint"
    this[this %eq% "s"] <- "Transition sideout"
    this[this %eq% "r"] <- "Reception"
    x$attack_phase <- this
    names(x)[5] <- "start_coordinate"
    names(x)[6] <- "mid_coordinate"
    names(x)[7] <- "end_coordinate"
    names(x)[8] <- "time"
    ## note these next 3 not yet used in actual file output, set number and setter positions are calculated directly
    names(x)[9] <- "set"
    names(x)[10] <- "home_rot"
    names(x)[11] <- "visiting_rot"
    ##names(x)[12] <- ## seems to also be set number?
    names(x)[13] <- "video_time"
    names(x)[15:20] <- paste("home_p",1:6,sep="") ## home team, court positons 1-6, entries are player numbers
    names(x)[21:26] <- paste("visiting_p",1:6,sep="") ## visiting team
    x$code <- as.character(x$code)
    x
}

parse_code <- function(code, meta, evaluation_decoder, code_line_num, full_lines, file_type = "indoor") {
    if (missing(code_line_num)) code_line_num <- NULL
    if (missing(full_lines)) full_lines <- code ## default to codes, if full lines not supplied
    using_cones <- tolower(meta$match$zones_or_cones) %eq% "c"
    msgs <- list()##text=c(),line=c())
    in_code <- code
    N <- length(code)
    if (is.null(code_line_num)) code_line_num <- rep(NA, N)
    out_team <- rep(NA_character_, N)
    out_player_number <- rep(NA, N)
    out_player_name <- rep(NA_character_, N)
    out_player_id <- rep(NA_character_, N)
    out_skill <- rep(NA_character_, N)
    out_skill_type <- rep(NA_character_, N)
    out_evaluation_code <- rep(NA_character_, N)
    out_evaluation <- rep(NA_character_, N)
    out_attack_code <- rep(NA_character_, N)
    out_attack_description <- rep(NA_character_, N)
    out_set_code <- rep(NA_character_, N)
    out_set_description <- rep(NA_character_, N)
    out_set_type <- rep(NA_character_, N)
    out_start_zone <- rep(NA, N)
    out_end_zone <- rep(NA, N)
    out_end_cone <- rep(NA_integer_, N)
    out_end_subzone <- rep(NA_character_, N)
    out_skill_subtype <- rep(NA_character_, N)
    out_num_players <- rep(NA, N)
    out_num_players_numeric <- rep(NA, N)
    out_special_code <- rep(NA_character_, N)
    out_custom_code <- rep(NA_character_, N)
    out_timeout <- rep(FALSE,N)
    out_end_of_set <- rep(FALSE,N)
    out_substitution <- rep(FALSE,N)
    out_point <- rep(FALSE,N)
    out_home_team_score <- rep(NA, N)
    out_visiting_team_score <- rep(NA, N)
    out_home_setter_position <- rep(NA, N)
    out_visiting_setter_position <- rep(NA, N)
    ## vectorised end-of-set handling
    done <- grepl("\\*\\*\\dset",in_code) ## end-of-set markers
    out_end_of_set[done] <- TRUE

    ## custom codes
    ## not sure if numbers are always 2 digits??
    ##out_custom_code <- substr(in_code,16,9999)
    temp <- sub("^.\\d+","",in_code) ## drop leading [a*] and digits
    out_custom_code <- tryCatch(substr(temp, 13, 9999),
                                error = function(e) {
                                    ## if that failed, it's likely that there are improperly-encoded characters
                                    ## but this section of the dv file shouldn't have any (only in the custom code?)
                                    in_code <<- stri_trans_general(in_code, "latin-ascii")
                                    temp <- sub("^.\\d+","",in_code) ## drop leading [a*] and digits
                                    substr(temp, 13, 9999)
                                })
    ## rotation errors
    ## ">ROT<" ">ROTAZ" ">ROTAZIONE" ">ROT" ">FALLOROT" ">FORMAZIONE"
    ## plusliga files: ROTE in custom notes
    thisidx <- grepl("^.\\$\\$R",in_code) | grepl("^>(ROT|FALLOROT|FORMAZIONE)",in_code) | grepl("ROT",out_custom_code)
    ## these lines are followed by a $$& line, so don't do anything here for the time being
    out_skill[thisidx] <- "Rotation error"
    out_evaluation[thisidx] <- "Error"
    done[thisidx] <- TRUE
    ## but do assign team here where possible
    out_team[thisidx & grepl("^a",in_code)] <- "a"
    out_team[thisidx & grepl("^\\*",in_code)] <- "*"
    
    ## sanctions that look like ">RED"
    ## actually anything starting with ">", are sanctions, rotation errors (dealt with above), etc
    ## from Italian league files:
    ##">RED<"  ">RED"  ">ROSSO"
    ## ">CHECK_RIPETE"    ">FALLO"   ">CONTESA"  ">CHECK_CONTESA" look like challenge notes
    ## ">SECONDI"   ??
    idx <- !done & (grepl("^>",in_code) | grepl("RED",out_custom_code))
    done[idx] <- TRUE
    
    ## team handling
    tm <- substr(in_code[!done],1,1)
    oktm <- tm=="a" | tm=="*"
    if (!all(oktm)) {
        myidx <- which(!done)[!oktm]
        msgs <- collect_messages(msgs,"Team entries not starting with * or a",code_line_num[myidx],full_lines[myidx],severity=2)
        tm[!oktm] <- "unknown"
    }
    out_team[!done] <- tm
    ## handle "automatic" codes
    thisidx <- grepl("^.p",in_code)
    ## point for the associated team
    out_point[thisidx] <- TRUE
    ## scores are given as .pX:Y where X=home team score, Y=visiting team score
    temp <- str_match(in_code[thisidx],"^.p(\\d+):(\\d+)")
        #cat(str(temp))
        #cat(str(in_code))
        #cat(str(out_home_team_score))
    out_home_team_score[thisidx] <- as.numeric(temp[,2])
    out_visiting_team_score[thisidx] <- as.numeric(temp[,3])
    done[thisidx] <- TRUE

    thisidx <- grepl("^\\*z",in_code)
    if (any(thisidx)) {
        ## identifying the position of the setter, *=home team
        temp <- str_match(in_code[thisidx],"^\\*z(\\d+)")
        out_home_setter_position[thisidx] <- as.numeric(temp[,2])
        done[thisidx] <- TRUE
    }
    thisidx <- grepl("^az",in_code)
    if (any(thisidx)) {
        ## identifying the position of the setter, a=visiting team
        temp <- str_match(in_code[thisidx],"^az(\\d+)")
        out_visiting_setter_position[thisidx] <- as.numeric(temp[,2])
        done[thisidx] <- TRUE
    }
    
    thisidx <- grepl("^.[PC]",in_code)
    ## substitution of setter (P) or other player (C)
    out_substitution[thisidx] <- TRUE
    done[thisidx] <- TRUE
    
    thisidx <- grepl("^.\\$\\$&",in_code)
    ## green code: win or loss of a point in an undefined way
    ## team marker here says which team played the ball
    ## but doesn't say which team won the point
    ## so don't set out_point here
    done[thisidx] <- TRUE

    thisidx <- grepl("^.\\$\\$[SE]",in_code)
    ## sanction
    ## not handled yet
    ## e.g.
    ##a$$SQ-;;;;;;;16.45.22;4;1;4;1;7138;;16;15;9;6;7;8;2;18;8;10;7;1;
    ##a$$EH=~~~~~~~~~RED;s;;;;;;16.45.22;4;1;4;1;7138;;16;15;9;6;7;8;2;18;8;10;7;1;
    if (any(thisidx)) {
        myidx <- which(thisidx)
        ## don't issue a message ...
        ##msgs <- collect_messages(msgs,"Unhandled code, likely a sanction",code_line_num[myidx],full_lines[myidx],severity=1)
    }
    done[thisidx] <- TRUE

    ## occasionally see green codes but without the "&"
    thisidx <- grepl("^.\\$\\$[^&]",in_code) & !done
    if (any(thisidx)) {
        thisidx <- which(thisidx)
        msgs <- collect_messages(msgs,"Unrecognized code (if this is a green code (\"$$\") it should be \"$$&\")",code_line_num[thisidx],full_lines[thisidx],severity=2)
        done[thisidx] <- TRUE
    }
    
    thisidx <- grepl("^.T",in_code)
    ## timeout by this team
    out_timeout[thisidx] <- TRUE
    out_skill[thisidx] <- "Timeout"
    done[thisidx] <- TRUE

    thisidx <- grepl("^.c",in_code)
    ## substitution
    out_substitution[thisidx] <- TRUE
    done[thisidx] <- TRUE    

## moved earlier    
##    ## custom codes
##    ## not sure if numbers are always 2 digits??
##    ##out_custom_code <- substr(in_code,16,9999)
##    temp <- sub("^.\\d+","",in_code) ## drop leading [a*] and digits
##    out_custom_code <- substr(temp,13,9999)
            
    notdone <- which(!done)
    for (ci in notdone) {
        code <- in_code[ci]
        thischar <- substr(code,2,2)
        ##player_number <- str_match(code,".(\\d+)") ##player_number <- as.numeric(player_number[2])
        tmp <- regexpr("^.(\\d+)",code)
        if (tmp!=1) {
            msgs <- collect_messages(msgs,"Player number should start at the second character",code_line_num[ci],full_lines[ci],severity=2)
            player_number <- NA
        } else {
            player_number <- as.numeric(substr(code,2,attr(tmp,"match.length")[1]))
            if (is.na(player_number)) {
                msgs <- collect_messages(msgs,"Could not read numeric player number",code_line_num[ci],full_lines[ci],severity=2)
            }
        }
        out_player_number[ci] <- player_number
        fullcode <- code
        code <- sub("^.\\d+","",code)
        skill <- substr(code,1,1)
        tmp <- skill_decode(skill,fullcode,full_lines[ci],code_line_num[ci])
        out_skill[ci] <- tmp$decoded
        msgs <- join_messages(msgs,tmp$messages)
        hit_type <- substr(code,2,2)
        tmp <- skill_type_decode(skill, hit_type, full_lines[ci], code_line_num[ci], file_type = file_type)
        out_skill_type[ci] <- tmp$decoded
        msgs <- join_messages(msgs,tmp$messages)
        skill_eval <- substr(code,3,3)
        out_evaluation_code[ci] <- skill_eval
        if (nchar(skill_eval)<1) {
            msgs <- collect_messages(msgs,"Missing evaluation code",code_line_num[ci],full_lines[ci],severity=1)
        } else {
            out_evaluation[ci] <- evaluation_decoder(skill,skill_eval)
            if (is.na(out_evaluation[ci])) {
                msgs <- collect_messages(msgs,paste0("Unknown evaluation code: ",skill_eval),code_line_num[ci],full_lines[ci],severity=2)
            }
            if (grepl("unknown",out_evaluation[ci],ignore.case=TRUE)) {
                ## out_evaluation[ci] will be something like "unknown dig evaluation"
                ## make it more informative
                temp <- paste0(out_evaluation[ci],": ",skill_eval)
                temp <- paste0(toupper(substr(temp,1,1)),substr(temp,2,nchar(temp)))
                msgs <- collect_messages(msgs,temp,code_line_num[ci],full_lines[ci],severity=2)
            }
        }
        ## extract the next few characters:
        ## for attacks, next 2 chars are the attack code from the metadata$attacks table, and similarly for sets
        ##attack_code <- substr(code,4,5)
        ##set_type <- substr(code,6,6)
        ##start_zone <- substr(code,7,7)
        ##end_zone <- substr(code,8,8)
        ##end_subzone <- substr(code,9,9)
        ##skill_subtype <- substr(code,10,10)
        ##num_players <- substr(code,11,11)
        ##special_code <- substr(code,12,12)
        some_codes <- str_sub(code,start=c(4,6,7,8,9,10,11,12),end=c(5,6,7,8,9,10,11,12))
        attack_code <- some_codes[1]##substr(code,4,5)
        if (!any(attack_code==c("","~~"))) {
            if (skill=="A") {
                out_attack_code[ci] <- attack_code
                if (!any(attack_code==meta$attacks$code)) {
                    msgs <- collect_messages(msgs,paste0("Unmatched attack code: ",attack_code),code_line_num[ci],full_lines[ci],severity=1)
                    descr <- "unknown attack code"
                } else {
                    descr <- meta$attacks$description[meta$attacks$code==attack_code]
                }
                out_attack_description[ci] <- descr
            } else if (skill=="E") {
                out_set_code[ci] <- attack_code
                if (!any(attack_code==meta$sets$code)) {
                    msgs <- collect_messages(msgs,paste0("Unmatched set code: ",attack_code),code_line_num[ci],full_lines[ci],severity=1)
                    descr <- "unknown set code"
                } else {
                    descr <- meta$sets$description[meta$sets$code==attack_code]
                }
                out_set_description[ci] <- descr
            } else {
                msgs <- collect_messages(msgs,paste0("Unexpected non-null attack code: ",attack_code," in non-attack code"),code_line_num[ci],full_lines[ci],severity=1)
            }
        }
        set_type <- some_codes[2]##substr(code,6,6)
        if (!any(set_type==c("","~"))) {
            if (skill == "E") {
                out_set_type[ci] <- set_type
                if (!is.na(set_type) && !set_type %in% c("F", "B", "C", "P", "S")) {
                    msgs <- collect_messages(msgs,paste0("Set type (attack target) should be F, B, C, P, or S, but is: ",set_type),code_line_num[ci],full_lines[ci],severity=1)
                }
            } else {
                msgs <- collect_messages(msgs,paste0("Unexpected set type: ",set_type),code_line_num[ci],full_lines[ci],severity=1)
            }
        }
        start_zone <- some_codes[3]##substr(code,7,7)
        if (!any(start_zone==c("","~"))) {
            out_start_zone[ci] <- as.numeric(start_zone)
            if ((skill=="R" || skill=="S") && !any(start_zone==c(1,9,6,7,5))) {
                msgs <- collect_messages(msgs,paste0("Unexpected serve/reception start zone: ",start_zone),code_line_num[ci],full_lines[ci],severity=2)
            }
        }
        end_zone <- some_codes[4]##substr(code,8,8)
        if (!any(end_zone==c("", "~"))) {
            if (skill %eq% "A" && using_cones) {
                out_end_cone[ci] <- as.integer(end_zone)
                ## NOT YET
                ##if ((start_zone %in% c(4, 7, 5, 2, 9, 1) && !out_end_cone[ci] %in% 1:7) || (start_zone %in% c(3, 8) && !out_end_cone[ci] %in% 1:8)) {
                ##    msgs <- collect_messages(msgs, paste0("Unexpected attack cone: ", end_zone), code_line_num[ci], full_lines[ci], severity = 2)
                ##}
            } else {
                out_end_zone[ci] <- as.integer(end_zone)
                if (skill=="B" && !out_end_zone[ci] %in% c(2, 3, 4)) {
                    msgs <- collect_messages(msgs, paste0("Unexpected block end zone: ", end_zone), code_line_num[ci], full_lines[ci], severity = 2)
                }
            }
        }
        end_subzone <- some_codes[5]##substr(code,9,9)
        if (!any(end_subzone==c("","~"))) {
            out_end_subzone[ci] <- end_subzone
            if (!any(end_subzone==c("A","B","C","D"))) {
                msgs <- collect_messages(msgs,paste0("Unexpected end subzone: ",end_subzone),code_line_num[ci],full_lines[ci],severity=1)
            }
        }
        ## skill sub type ("TYPE OF HIT", p32)
        skill_subtype <- some_codes[6]##substr(code,10,10)
        if (!any(skill_subtype==c("","~"))) {
            if (skill=="A") {
                if (!any(skill_subtype==c("H","P","T"))) {
                    msgs <- collect_messages(msgs,paste0("Unexpected attack subtype: ",skill_subtype),code_line_num[ci],full_lines[ci],severity=1)
                }
                out_skill_subtype[ci] <- switch(skill_subtype,
                                                H="Hard spike",
                                                P="Soft spike/topspin",
                                                T="Tip",
                                                paste0("Unknown ",skill_subtype))
            } else if (skill=="B") {
                if (!any(skill_subtype==c("A","T","P"))) {
                    msgs <- collect_messages(msgs,paste0("Unexpected block subtype: ",skill_subtype),code_line_num[ci],full_lines[ci],severity=1)
                }
                out_skill_subtype[ci] <- switch(skill_subtype,
                                                A="Block assist",
                                                T="Block attempt",
                                                P="Block on soft spike",
                                                paste0("Unknown ",skill_subtype))
            } else if (skill=="R") {
                if (!any(skill_subtype==c("L","R","W","O","M"))) {
                    msgs <- collect_messages(msgs,paste0("Unexpected reception subtype: ",skill_subtype),code_line_num[ci],full_lines[ci],severity=1)
                }
                out_skill_subtype[ci] <- switch(skill_subtype,
                                                L="On left",
                                                R="On right",
                                                W="Low",
                                                O="Overhand",
                                                M="Middle line",
                                                paste0("Unknown ",skill_subtype))
            } else if (skill == "E") {
                ## set subtypes were added in DV4
                ## O and U are custom codes used by some beach volley scouts prior to DV4
                if (!any(skill_subtype == c("1", "2", "3", "4", "5", "O", "U"))) {
                    msgs <- collect_messages(msgs, paste0("Unexpected set subtype: ", skill_subtype), code_line_num[ci], full_lines[ci], severity = 1)
                }
                out_skill_subtype[ci] <- switch(EXPR = skill_subtype,
                                                "1" = "1 hand set",
                                                "2" = "2 hands set",
                                                "3" = "Bump set",
                                                "4" = "Other set",
                                                "5" = "Underhand set",
                                                "O" = "Hand set",
                                                "U" = "Bump set",
                                                paste0("Unknown set subtype ", skill_subtype))
            } else if (skill=="D") {
                if (!any(skill_subtype==c("S","C","B","E","T","P"))) {
                    msgs <- collect_messages(msgs,paste0("Unexpected dig subtype: ",skill_subtype),code_line_num[ci],full_lines[ci],severity=1)
                }
                out_skill_subtype[ci] <- switch(EXPR=skill_subtype,
                                                S="On spike",
                                                C="Spike cover",
                                                B="After block",
                                                E="Emergency",
                                                T="Tip",
                                                P="Soft spike",
                                                paste0("Unknown dig subtype ",skill_subtype))
            } else {
                out_skill_subtype[ci] <- paste0("Unknown ",skill_subtype)
            }
        }
        ## number of people ("PLAYERS", p33)
        num_players <- some_codes[7]##substr(code,11,11)
        if (!any(num_players==c("","~"))) {
            out_num_players_numeric[ci] <- as.numeric(num_players)
            if (skill=="A") {
                if (!num_players %in% as.character(0:4)) {
                    msgs <- collect_messages(msgs,paste0("Unexpected number of players: ",num_players),code_line_num[ci],full_lines[ci],severity=2)
                }
                if (grepl("beach", file_type)) {
                    out_num_players[ci] <- switch(num_players,
                                                  "0"="No block",
                                                  "1"="Line block",
                                                  "2"="Crosscourt block",
                                                  "3"="Block jumps to line",
                                                  "4"="Block jumps to crosscourt",
                                                  paste0("Unexpected ",num_players))
                } else {
                    out_num_players[ci] <- switch(num_players,
                                                  "0"="No block",
                                                  "1"="1 player block",
                                                  "2"="2 player block",
                                                  "3"="3 player block",
                                                  "4"="Hole block",
                                                  paste0("Unexpected ",num_players))
                }
            } else if (skill=="B") {
                if (!num_players %in% as.character(0:4)) {
                    msgs <- collect_messages(msgs,paste0("Unexpected number of players: ",num_players),code_line_num[ci],full_lines[ci],severity=2)
                }
                if (grepl("beach", file_type)) {
                    out_num_players[ci] <- switch(num_players,
                                                  "0"="No block",
                                                  "1"="Line block",
                                                  "2"="Crosscourt block",
                                                  "3"="Block jumps to line",
                                                  "4"="Block jumps to crosscourt",
                                                  paste0("Unexpected ",num_players))
                } else {
                    out_num_players[ci] <- switch(num_players,
                                                  "0"="No block",
                                                  "1"="1 player block",
                                                  "2"="2 player block",
                                                  "3"="3 player block",
                                                  "4"="Hole block",
                                                  paste0("Unexpected ",num_players))
                }
            } else if (skill=="R") {
                if (!any(num_players==1:9)) {
                    msgs <- collect_messages(msgs,paste0("Unexpected number of players: ",num_players),code_line_num[ci],full_lines[ci],severity=2)
                }
                out_num_players[ci] <- switch(num_players,
                                              "1"="Two players receiving, the player on left receives",
                                              "2"="Two players receiving, the player on right receives",
                                              "3"="Three players receiving, the player on left receives",
                                              "4"="Three players receiving, the player in center receives",
                                              "5"="Three players receiving, the player on right receives",
                                              "6"="Four players receiving, the player on left receives",
                                              "7"="Four players receiving, the player on center-left receives",
                                              "8"="Four players receiving, the player on center-right receives",
                                              "9"="Four players receiving, the player on right receives",
                                              paste0("Unexpected ",num_players))
            } else {
                out_num_players[ci] <- paste0("Unexpected ",num_players)
            }
        }
        ## special ("SPECIAL CODES", p33)
        special_code <- some_codes[8]##substr(code,12,12)
        if (!any(special_code==c("","~"))) {
            if (skill=="A") {
                if (out_evaluation_code[ci] %eq% "=") { ##if (out_evaluation[ci]==evaluation_decoder("A","=")) {
                    ## error
                    if (!any(special_code==c("S","O","N","I","Z","A"))) {
                        msgs <- collect_messages(msgs,paste0("Unexpected special code: ",special_code," for attack evaluation \"",out_evaluation_code[ci],"\""),code_line_num[ci],full_lines[ci],severity=1)
                    }
                    out_special_code[ci] <- switch(special_code,
                                                   "S"="Attack out - side",
                                                   "O"="Attack out - long",
                                                   "N"="Attack in net",
                                                   "I"="Net contact",
                                                   "Z"="Referee call",
                                                   "A"="Antenna",
                                                   paste0("Unexpected ",special_code))
                } else if (out_evaluation_code[ci] %eq% "#") { ##} else if (out_evaluation[ci]==evaluation_decoder("A","#")) {
                    ## point ("Winning attack")
                    if (!any(special_code==c("S","O","F","X","N"))) {
                        msgs <- collect_messages(msgs,paste0("Unexpected special code: ",special_code," for attack evaluation \"",out_evaluation_code[ci],"\""),code_line_num[ci],full_lines[ci],severity=1)
                    }
                    out_special_code[ci] <- switch(special_code,
                                                   "S"="Block out - side",
                                                   "O"="Block out - long",
                                                   "F"="Block on floor",
                                                   "X"="Direct on floor",
                                                   "N"="Let",
                                                   paste0("Unexpected ",special_code))
                } else if (out_evaluation_code[ci] %in% c("+","-","!","/")) {
                ##} else if (any(out_evaluation[ci]==c(evaluation_decoder("A","+"),evaluation_decoder("A","-"),evaluation_decoder("A","!"),evaluation_decoder("A","/")))) {
                    ## continue
                    ## for A/ - continue codes applied to blocked attack - DV4 allows this
                    if (!any(special_code==c("C","N"))) {
                        msgs <- collect_messages(msgs,paste0("Unexpected special code: ",special_code," for attack evaluation \"",out_evaluation_code[ci],"\""),code_line_num[ci],full_lines[ci],severity=1)
                    }
                    out_special_code[ci] <- switch(special_code,
                                                   "C"="Block control",
                                                   "N"="Let",
                                                   paste0("Unexpected ",special_code))
                } else {
                    ## not expecting special code for this attack evaluation outcome
                    msgs <- collect_messages(msgs,paste0("Unexpected special code: ",special_code," for attack evaluation \"",out_evaluation_code[ci],"\""),code_line_num[ci],full_lines[ci],severity=1)
                }
            } else if (skill == "B") {
                if (!any(special_code == c("S", "O", "F", "X", "N", "I", "A", "P", "T", "Z"))) {
                    msgs <- collect_messages(msgs, paste0("Unexpected special code: ", special_code, " for block"), code_line_num[ci], full_lines[ci], severity = 1)
                }
                out_special_code[ci] <- switch(special_code,
                                               "S" = "Ball out - side",
                                               "O" = "Ball out - long",
                                               "F" = "Ball on floor",
                                               "X" = "Between hands",
                                               "N" = "Hands - net",
                                               "I" = "Net contact",
                                               "A" = "Antenna",
                                               "P" = "No jump",
                                               "T" = "Position error",
                                               "Z" = "Referee call",
                                               paste0("Unexpected ", special_code))
            } else if (skill=="R") {
                ## dig, reception, freeball NOT all the same in dv4, but were in dv3 (see p16 of DV v3 manual)
                if (!any(special_code==c("U","X","P","Z","E"))) {
                    msgs <- collect_messages(msgs,paste0("Unexpected special code: ",special_code," for reception"),code_line_num[ci],full_lines[ci],severity=1)
                }
                out_special_code[ci] <- switch(EXPR=special_code,
                                               "U"="Unplayable",
                                               "X"="Body error",
                                               "P"="Position error",
                                               "Z"="Referee call",
                                               "E"="Lack of effort",
                                               paste0("Unexpected ",special_code))
            } else if (skill=="F") {
                if (!any(special_code==c("U","X","P","Z"))) {
                    msgs <- collect_messages(msgs,paste0("Unexpected special code: ",special_code," for freeball"),code_line_num[ci],full_lines[ci],severity=1)
                }
                out_special_code[ci] <- switch(EXPR=special_code,
                                               "U"="Unplayable",
                                               "X"="Body error",
                                               "P"="Position error",
                                               "Z"="Referee call",
                                               paste0("Unexpected ",special_code))
            } else if (skill=="D") {
                if (!any(special_code==c("U","X","P","Z","F","O","E"))) {
                    msgs <- collect_messages(msgs,paste0("Unexpected special code: ",special_code," for dig"),code_line_num[ci],full_lines[ci],severity=1)
                }
                out_special_code[ci] <- switch(EXPR=special_code,
                                               "U"="Unplayable",
                                               "X"="Body error",
                                               "P"="Position error",
                                               "Z"="Referee call",
                                               "F"="Ball on floor",
                                               "O"="Ball out",
                                               "E"="Lack of effort",
                                               paste0("Unexpected ",special_code))
            } else if (skill=="E") {
                if (!any(special_code==c("U","I","Z"))) {
                    msgs <- collect_messages(msgs,paste0("Unexpected special code: ",special_code," for set"),code_line_num[ci],full_lines[ci],severity=1)
                }
                out_special_code[ci] <- switch(special_code,
                                               "U"="Cannot be hit",
                                               "I"="Net touch",
                                               "Z"="Referee call",
                                               paste0("Unexpected ",special_code))
            } else if (skill=="S") {
                if (out_evaluation_code[ci] %eq% "=") { ##if (out_evaluation[ci]==evaluation_decoder("S","=")) {
                    ## error
                    if (!any(special_code==c("O","L","R","N","Z"))) {
                        msgs <- collect_messages(msgs,paste0("Unexpected special code: ",special_code," for serve evaluation \"",out_evaluation_code[ci],"\""),code_line_num[ci],full_lines[ci],severity=1)
                    }
                    out_special_code[ci] <- switch(special_code,
                                                   "O"="Ball out - long",
                                                   "L"="Ball out - left",
                                                   "R"="Ball out - right",
                                                   "N"="Ball in net",
                                                   "Z"="Referee call",
                                                   paste0("Unexpected ",special_code))
                } else if (out_evaluation_code[ci] %eq% "#") { ##} else if (out_evaluation[ci]==evaluation_decoder("S","#")) {
                    ## point (ace)
                    if (special_code!="N") {
                        msgs <- collect_messages(msgs,paste0("Unexpected special code: ",special_code," for serve evaluation \"",out_evaluation_code[ci],"\""),code_line_num[ci],full_lines[ci],severity=1)
                    }
                    out_special_code[ci] <- switch(special_code,
                                                   "N"="Let",
                                                   paste0("Unexpected ",special_code))
                } else if (out_evaluation_code[ci] %in% c("/","-","+","!")) { ##} else if (any(out_evaluation[ci]==c(evaluation_decoder("S","/"),evaluation_decoder("S","-"),evaluation_decoder("S","+"),evaluation_decoder("S","!")))) {
                    ## continue
                    if (special_code!="N") {
                        msgs <- collect_messages(msgs,paste0("Unexpected special code: ",special_code," for serve evaluation \"",out_evaluation_code[ci],"\""),code_line_num[ci],full_lines[ci],severity=1)
                    }
                    out_special_code[ci] <- switch(special_code,
                                                   "N"="Let",
                                                   paste0("Unexpected ",special_code))
                } else {
                    ## not expecting special code for this attack evaluation outcome
                    msgs <- collect_messages(msgs,paste0("Unexpected special code: ",special_code," for serve evaluation \"",out_evaluation_code[ci],"\""),code_line_num[ci],full_lines[ci],severity=1)
                }
            }
        }
    }
    ## fill in player_name and player_id from player_number
    idx <- !is.na(out_player_number)
    out_player_name[idx] <- get_player_name(out_team[idx],out_player_number[idx],meta)
    out_player_id[idx] <- get_player_id(out_team[idx],out_player_number[idx],meta)
    dudidx <- (!is.na(out_player_number) & is.na(out_player_name)) | grepl("unknown player",out_player_name,ignore.case=TRUE)
    if (any(dudidx))
        msgs <- collect_messages(msgs,paste0("Player number ",out_player_number[dudidx]," could not be resolved to a player name/id"),code_line_num[dudidx],full_lines[dudidx],severity=2)

    ## order messages by line number
    #if (length(msgs$text)>0) {
    #    idx <- order(msgs$line)
    #    msgs <- msgs$text[idx]
    #}
    if (length(msgs)>0) {
        msgs <- ldply(msgs,as.data.frame)
        ##msgs <- arrange(msgs,file_line_number)
    } else {
        msgs <- data.frame(file_line_number=integer(),video_time=numeric(),message=character(),file_line=character())
    }

    list(plays = data.frame(code = in_code, team = out_team, player_number = out_player_number, player_name = out_player_name, player_id = out_player_id, skill = out_skill, skill_type = out_skill_type, evaluation_code = out_evaluation_code, evaluation = out_evaluation, attack_code = out_attack_code, attack_description = out_attack_description, set_code = out_set_code, set_description = out_set_description, set_type = out_set_type, start_zone = out_start_zone, end_zone = out_end_zone, end_subzone = out_end_subzone, end_cone = out_end_cone, skill_subtype = out_skill_subtype, num_players = out_num_players, num_players_numeric = out_num_players_numeric, special_code = out_special_code, timeout = out_timeout, end_of_set = out_end_of_set, substitution = out_substitution, point = out_point, home_team_score = out_home_team_score, visiting_team_score = out_visiting_team_score, home_setter_position = out_home_setter_position, visiting_setter_position = out_visiting_setter_position, custom_code = out_custom_code, file_line_number = as.integer(code_line_num), stringsAsFactors = FALSE), messages = msgs)
}

## single parenthesised capture
strget1 <- function(s,re,cast_to) {
    tmp <- regexec(re,s)
    out <- substr(s,tmp[[1]][2],tmp[[1]][2]+attr(tmp[[1]],"match.length")[2]-1)
    if (!missing(cast_to)) cast_to(out) else out
}

## two parenthesised captures
strget2 <- function(s,re,cast_to) {
    tmp <- regexec(re,s)
    out <- c(substr(s,tmp[[1]][2],tmp[[1]][2]+attr(tmp[[1]],"match.length")[2]-1),
             substr(s,tmp[[1]][3],tmp[[1]][3]+attr(tmp[[1]],"match.length")[3]-1))
    if (!missing(cast_to)) cast_to(out) else out
}
