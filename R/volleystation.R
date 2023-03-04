vs_int2role <- function(x) {
    r <- c("libero", "outside", "opposite", "middle", "setter", "unknown") ## zero-based
    out <- rep(NA_character_, length(x))
    idx <- x %in% 0:5
    out[idx] <- r[x[idx] + 1L]
    out
}

vs_reformat_players <- function(jx, which = "home") {
    jt <- if (which %in% "home") jx$team$home else jx$team$away
    px <- tibble(X1 = 0L,
                 number = as.integer(jt$players$shirtNumber),
                 X3 = seq_len(nrow(jt$players)) + if (which %in% "home") 0L else nrow(jx$team$home$players),
                 starting_position_set1 = NA_character_,
                 starting_position_set2 = NA_character_,
                 starting_position_set3 = NA_character_,
                 starting_position_set4 = NA_character_,
                 starting_position_set5 = NA_character_,
                 player_id = jt$players$code,
                 lastname = jt$players$lastName,
                 firstname = jt$players$firstName,
                 nickname = "",
                 special_role = NA_character_,
                 role = vs_int2role(jt$players$position),
                 foreign = FALSE,
                 X16 = NA)
    px$name <- paste(px$firstname, px$lastname)
    px$role[is.na(px$role) & px$number %in% jt$libero] <- "libero"
    px$special_role[px$role %in% "libero"] <- "L"
    ## starting positions
    sx <- jx$scout$sets$startingLineup[[if (which %in% "home") "home" else "away"]]$positions
    if (isTRUE(ncol(sx) %in% c(2, 6))) { ## presumably beach is 2?
        for (si in seq_len(nrow(sx))) {
            this <- rep(NA_character_, nrow(px))
            for (i in seq_len(ncol(sx))) this[which(px$number == sx[si, i])] <- i
            px[[paste0("starting_position_set", si)]] <- this
        }
    } else {
        warning("expecting a 2- or 6-column data frame for ", which, " team starting positions, ignoring")
    }
    px
}

dv_read_vsm <- function(filename, skill_evaluation_decode, insert_technical_timeouts = TRUE, extra_validation = 2, validation_options=list(), ...) {
    ## do_warn=FALSE, do_transliterate=FALSE,
    ## surname_case="asis", skill_evaluation_decode="default", custom_code_parser, metadata_only=FALSE, verbose=FALSE, edited_meta
    x <- list(raw = readLines(filename, warn = FALSE))
    jx <- jsonlite::fromJSON(x$raw)
    x$raw <- str_split(str_replace_all(x$raw, fixed("{\"_id"), "\n{\"_id"), fixed("\n"))[[1]]
    raw_id <- str_match(x$raw, "\"_id\":\"([^\"]+)\"")[, 2]
    idlnum <- setNames(as.list(seq_along(raw_id)), raw_id) ## line numbers, named by their corresponding _id
    ## there should not be duplicate IDs, but they will cause problems so make sure
    idlnum <- idlnum[!is.na(raw_id) & !raw_id %in% raw_id[duplicated(raw_id)]]
    px_lnum <- function(idx) {
        ## raw line numbers associated with the rows idx in px
        if (length(idx) < 1) return(integer())
        ids <- px$`_id`[idx]
        out <- rep(NA_integer_, length(idx))
        oidx <- ids %in% names(idlnum)
        out[oidx] <- idlnum[ids[oidx]]
        unlist(out)
    }
    msgs <- list()
    file_type <- jx$gameType
    pseq <- if (isTRUE(grepl("beach", file_type))) 1:2 else 1:6
    x$file_meta <- dv_create_file_meta(generator_day = as.POSIXct(NA), generator_idp = "DVW", generator_prg = "VolleyStation",
                                      generator_release = "", generator_version = NA_character_, generator_name = "", file_type = file_type)
    x$messages <- data.frame()

    mx <- list(match = dv_create_meta_match(date = lubridate::ymd_hms(jx$startDate),
                                            regulation = if (isTRUE(jx$gameType == "indoor")) "indoor rally point" else stop("unexpected gameType: ", jx$gameType),
                                            zones_or_cones = "Z"), ## cones not supported in VS
               more = dv_create_meta_more()) ## TODO surely the scout name at least is in the json
    if ("comment" %in% names(jx)) {
        ## e.g. "---- summary ----\nno comments\n\n---- match description ----\n\n\n---- coach home ----\n\n\n---- coach away ----\n"
        cx <- strsplit(jx$comment, "\n\\-\\-\\-\\-[[:space:]]*")[[1]]
        cx <- sub("\n+$", "", sub(".*\\-\\-\\-\\-\n", "", cx))
        cx <- c(cx, rep(NA_character_, 4 - length(cx)))
        mx$comments = dv_create_meta_comments(summary = cx[1], match_description = cx[2], home_coach_comments = cx[3], visiting_coach_comments = cx[4])
    } else {
        mx$comments = dv_create_meta_comments()
    }
    mx$result <- dv_create_meta_result(home_team_scores = jx$scout$sets$score$home, visiting_team_scores = jx$scout$sets$score$away) ## will be further populated by dv_update_meta below
    c1 <- paste(jx$team$home$staff$coach$firstName, jx$team$home$staff$coach$lastName)
    if (length(c1) < 1) c1 <- NA_character_
    c2 <- paste(jx$team$away$staff$coach$firstName, jx$team$away$staff$coach$lastName)
    if (length(c2) < 1) c2 <- NA_character_
    a1 <- paste(paste(jx$team$home$staff$assistantCoach$firstName, jx$team$home$staff$assistantCoach$lastName), paste(jx$team$home$staff$assistantCoach2$firstName, jx$team$home$staff$assistantCoach2$lastName), collapse = " / ")
    if (length(a1) < 1) a1 <- NA_character_
    a2 <- paste(paste(jx$team$away$staff$assistantCoach$firstName, jx$team$away$staff$assistantCoach$lastName), paste(jx$team$away$staff$assistantCoach2$firstName, jx$team$away$staff$assistantCoach2$lastName), collapse = " / ")
    if (length(a2) < 1) a2 <- NA_character_
    mx$teams <- dv_create_meta_teams(team_ids = c(jx$team$home$code, jx$team$away$code),
                                     teams = c(jx$team$home$name, jx$team$away$name),
                                     coaches = c(c1, c2),
                                     assistants = c(a1, a2))

    mx$players_h <- vs_reformat_players(jx, "home")
    mx$players_v <- vs_reformat_players(jx, "visiting")

    ax <- dv_create_meta_attack_combos(code = jx$attackCombinations$code,
                                       start_zone = jx$attackCombinations$start$zone,
                                       side = NA_character_, ## R,L,C,
                                       tempo = jx$attackCombinations$hitType,
                                       description = jx$attackCombinations$name,
                                       start_coordinate = datavolley::dv_xy2index(datavolley::dv_xy(zones = jx$attackCombinations$start$zone, subzones = jx$attackCombinations$start$subZone, end = "lower")),
                                       target_attacker = jx$attackCombinations$targetAttacker)
    if (has_dvmsg(ax)) {
        idx <- head(grep("attackCombinations", x$raw), 1)
        if (length(idx) < 1) idx <- NA_integer_
        msgs <- collect_messages(msgs, get_dvmsg(ax)$message, line_nums = idx + 1L, raw_lines = if (is.na(idx)) NA_character_ else x$raw[idx + 1L], severity = 3)
    }
    mx$attacks <- clear_dvmsg(ax)

    mx$sets <- dv_create_meta_setter_calls(code = jx$setterCalls$code, description = jx$setterCalls$name) ## jx$setterCalls$area seems to be zone and subzone of where the middle runs
    mx$winning_symbols <- dv_default_winning_symbols() ## TODO can this vary from vsm file to vsm file?
    mx$match_id <- dv_create_meta_match_id(mx)

    vf <- if (length(jx$scout$video) < 1 || length(jx$scout$video$path) < 1) character() else if (length(jx$scout$video$path) > 1) { warning("multiple video files not yet supported, using only the first"); jx$scout$video$path[1] } else jx$scout$video$path[1]
    mx$video <- dv_create_meta_video(vf)

    mx$filename <- filename
    x$meta <- mx
    ## meta component done

    ## plays
    temp_pid <- 0L
    prev_time <- NA_real_
    px <- bind_rows(lapply(seq_along(jx$scout$sets$events), function(si) {
        thisev <- jx$scout$sets$events[[si]]
        thisex <- thisev$exchange
        this_point_ids <- temp_pid + seq_len(nrow(thisex))
        temp_pid <<- max(this_point_ids)
        pwb <- tibble(point_won_by = thisex$point, point_id = this_point_ids)
        thisto <- if (is.data.frame(thisev$timeout)) thisev$timeout %>% mutate(code = paste0(.data$team, "T"), point_id = this_point_ids, skill = "Timeout", timeout = TRUE) %>% dplyr::filter(!is.na(.data$team)) else tibble(code = character(), point_id = integer(), team = character(), skill = character(), timeout = logical())
        ## sub is [a*]c[OUT]:[IN] (or P for sub of setter) ^^^TODO P
        thissub <- if (is.data.frame(thisev$substitution)) {
                       thisev$substitution %>% mutate(point_id = this_point_ids) %>% dplyr::filter(!is.na(.data$team)) %>% mutate(code = paste0(.data$team, "c", lead0(.data$playerOut, na = "00"), ":", lead0(.data$playerIn, na = "00")), substitution = TRUE) ##%>% dplyr::select(-"playerIn", -"playerOut")
                   } else {
                       tibble(point_id = integer(), team = character(), code = character(), substitution = logical())
                   }
        ## scores
        this_pts <- thisev$score %>% mutate(point_id = this_point_ids) %>% dplyr::rename(home_team_score = "home", visiting_team_score = "away") %>%
            mutate(code = case_when(dplyr::row_number() == 1 & .data$home_team_score > 0 ~ "*",
                                    dplyr::row_number() == 1 & .data$visiting_team_score > 0 ~ "a",
                                    .data$home_team_score != dplyr::lag(.data$home_team_score) ~ "*",
                                    .data$visiting_team_score != dplyr::lag(.data$visiting_team_score) ~ "a",
                                    TRUE ~ "~"), ## ~ denoting no score change, so no *p/ap code to add
                   ## note that negative score changes also count as "points", they will appear in score correction lines
                   ## only insert p-code when scores change (e.g. not subs)
                   code = case_when(code != "~" ~ paste0(.data$code, "p", lead0(.data$home_team_score, na = "99"), ":", lead0(.data$visiting_team_score, na = "99"))),
                   point = !is.na(.data$code), team = substr(.data$code, 1, 1))
        ## columns to preserve when adding new rows to the dataframe
        keepcols <- c("point_id", "time", "home_team_score", "visiting_team_score", "point_won_by", "set_number",
                      "home_setter_position", "visiting_setter_position", "home_p1", "home_p2", "home_p3", "home_p4", "home_p5", "home_p6",
                      "visiting_p1", "visiting_p2", "visiting_p3", "visiting_p4", "visiting_p5", "visiting_p6")
        ## row-bind the plays, timeouts, subs, and points, with care to ensure row ordering is correct
        thisex <- bind_rows(lapply(seq_along(thisex$plays), function(j) {
            if (!is.null(thisex$plays[[j]])) {
                temp <- thisex$plays[[j]] %>% mutate(point_id = this_point_ids[j], point = FALSE) %>% dplyr::select(-"originalTime")
                if ("travelPath" %in% names(temp)) {
                    tp <- bind_rows(lapply(temp$travelPath, function(z) {
                        if (is.null(z)) {
                            tibble(start_coordinate_x = NA_integer_, start_coordinate_y = NA_integer_,
                                   end_coordinate_x = NA_integer_, end_coordinate_y = NA_integer_)
                        } else {
                            if (nrow(z) != 2) stop("travelPath not 2 rows")
                            ## convert to dv grid conventions
                            z$x <- z$x / 293.3333 + 0.447159
                            z$y <- z$y / 297 + 0.469697
                            tibble(start_coordinate_x = z$x[1], start_coordinate_y = z$y[1],
                                   end_coordinate_x = z$x[2], end_coordinate_y = z$y[2])
                        }
                    }))
                    cbind(temp %>% dplyr::select(-"travelPath"), tp)
                } else {
                    temp
                }
            } else {
                NULL
            }
        }))
        thisex <- bind_rows(thisto %>% dplyr::group_by(.data$point_id) %>% mutate(sort_order = dplyr::row_number() / 10e7) %>% dplyr::ungroup() %>% mutate(sort_order = .data$point_id + .data$sort_order),
                            thissub %>% dplyr::group_by(.data$point_id) %>% mutate(sort_order = dplyr::row_number() / 10e5) %>% dplyr::ungroup() %>% mutate(sort_order = .data$point_id + .data$sort_order),
                            thisex %>% dplyr::group_by(.data$point_id) %>% mutate(sort_order = dplyr::row_number() / 10e3) %>% dplyr::ungroup() %>% mutate(sort_order = .data$point_id + .data$sort_order),
                            this_pts %>% dplyr::filter(.data$point) %>% dplyr::select(-"home_team_score", -"visiting_team_score") %>% dplyr::group_by(.data$point_id) %>% mutate(sort_order = dplyr::row_number() / 10e1) %>% dplyr::ungroup() %>% mutate(sort_order = .data$point_id + .data$sort_order)) %>%
            dplyr::arrange(.data$sort_order) %>% dplyr::select(-"sort_order") %>%
            mutate(substitution = case_when(is.na(.data$substitution) ~ FALSE, TRUE ~ .data$substitution),
                   timeout = case_when(is.na(.data$timeout) ~ FALSE, TRUE ~ .data$timeout),
                   point = case_when(is.na(.data$point) ~ FALSE, TRUE ~ .data$point))
        ## fill in missing times
        tms <- thisex %>% dplyr::group_by(.data$point_id) %>%
            dplyr::summarize(this_start_time = if (!all(is.na(.data$time))) min(.data$time, na.rm = TRUE) else NA_integer_,
                             this_end_time = if (!all(is.na(.data$time))) max(.data$time, na.rm = TRUE) else NA_integer_) %>%
            mutate(prev_time = dplyr::lag(.data$this_end_time), next_time = dplyr::lead(.data$this_start_time),
                   this_start_time = case_when(!is.na(.data$this_start_time) ~ .data$this_start_time, TRUE ~ pmin(.data$prev_time, .data$this_end_time, .data$next_time, na.rm = TRUE)),
                   this_end_time = case_when(!is.na(.data$this_end_time) ~ .data$this_end_time, TRUE ~ pmax(.data$prev_time, .data$this_end_time, .data$next_time, na.rm = TRUE)),
                   prev_time = case_when(!is.na(.data$prev_time) ~ .data$prev_time, TRUE ~ .data$this_start_time))
        thisex <- left_join(thisex, tms, by = "point_id") %>% mutate(time = case_when(!is.na(.data$time) ~ .data$time,
                                                                                      .data$substitution ~ .data$this_start_time,
                                                                                      .data$point ~ .data$this_end_time,
                                                                                      .data$timeout ~ .data$prev_time)) %>%
            dplyr::select(-"this_start_time", -"this_end_time", -"prev_time", -"next_time")

        names(thisex) <- camel_to_underscore(names(thisex))
        ## point won by
        thisex <- left_join(thisex, pwb, by = "point_id") %>% mutate(set_number = si)
        ## scores
        thisex <- left_join(thisex, this_pts %>% dplyr::select("point_id", "home_team_score", "visiting_team_score"), by = "point_id")
        ## setter positions
        this_htl <- tibble(home_setter_position = thisev$lineup$home$setterAt, point_id = this_point_ids)
        thisex <- left_join(thisex, this_htl, by = "point_id")
        this_vtl <- tibble(visiting_setter_position = thisev$lineup$away$setterAt, point_id = this_point_ids)
        thisex <- left_join(thisex, this_vtl, by = "point_id")
        ## home team lineup
        this_htl <- thisev$lineup$home$positions %>% mutate(point_id = this_point_ids) %>%
            dplyr::rename(home_p1 = "1", home_p2 = "2", home_p3 = "3", home_p4 = "4", home_p5 = "5", home_p6 = "6")
        thisex <- left_join(thisex, this_htl, by = "point_id")
        ## visiting team lineup
        this_vtl <- thisev$lineup$away$positions %>% mutate(point_id = this_point_ids) %>%
            dplyr::rename(visiting_p1 = "1", visiting_p2 = "2", visiting_p3 = "3", visiting_p4 = "4", visiting_p5 = "5", visiting_p6 = "6")
        thisex <- left_join(thisex, this_vtl, by = "point_id")

        fix_rally <- function(rx, last_hsp, last_vsp, last_hts, last_vts, meta) {
            ## rx is a data.frame
            ## last_hsp, last_vsp = last known setter positions
            ## last_hts, last_vts = last team scores
            if (nrow(rx) < 1) return(rx)
            ## TODO ensure no missing times
            is_sub <- is_to <- is_sc <- is_point <- FALSE
            if (any(rx$substitution)) {
                stopifnot(nrow(rx) == 1)
                is_sub <- TRUE
            } else if (any(rx$timeout)) {
                stopifnot(nrow(rx) == 1)
                is_to <- TRUE
            } else if (nrow(rx) == 1 && isTRUE(rx$point)) {
                is_sc <- TRUE
            } else if (any(rx$point)) {
                is_point <- TRUE
            }
            ## if this was this a substitution, did the setter get substituted?
            if (is_sub) {
                if (rx$team %eq% "*") {
                    setter_num <- tryCatch(rx[[paste0("home_p", rx$home_setter_position)]], error = NA_integer_)
                } else if (rx$team %eq% "a") {
                    setter_num <- tryCatch(rx[[paste0("visiting_p", rx$visiting_setter_position)]], error = NA_integer_)
                } else {
                    stop("sub for unknown team")
                }
                if (is.na(setter_num)) stop("unknown setter")
                if (rx$player_out == setter_num) substr(rx$code, 2, 2) <- "P" ## make the sub code a setter sub
            }
            ## $point indicates that the score changed, either through a genuine rally or via a score correction
            if (is_sc) {
                ## score correction, add pc rows if needed, there may need to be more than one
                ## also discard the existing point code, because it will need to be re-created in sequence here
                last_scores <- c(last_hts, last_vts)
                hts_delta <- rx$home_team_score - last_scores[1] ## change in home team score
                vts_delta <- rx$visiting_team_score - last_scores[2] ## change in visiting team score
                n_adj <- abs(hts_delta) + abs(vts_delta) ## the number of score adjustments we'll insert
                newcodes <- newscores_h <- newscores_v <- c()
                ## note that we don't try and keep track of setter positions through this and enter the appropriate *z/az codes. Just enter the first one (code block below)
                if (hts_delta != 0) {
                    for (sci in (seq_len(abs(hts_delta)) * sign(hts_delta))) {
                        thiscodes <- dv_green_codes(paste0("*p", lead0(last_scores[1] + sci), ":", lead0(last_scores[2])), meta)
                        newscores_h <- c(newscores_h, rep(last_scores[1] + sci, length(thiscodes)))
                        newcodes <- c(newcodes, thiscodes)
                    }
                    newscores_v <- rep(last_scores[2], length(newscores_h))
                    last_scores[1] <- rx$home_team_score
                }
                if (vts_delta != 0) {
                    for (sci in (seq_len(abs(vts_delta)) * sign(vts_delta))) {
                        thiscodes <- dv_green_codes(paste0("ap", lead0(last_scores[1]), ":", lead0(last_scores[2] + sci)), meta)
                        newscores_v <- c(newscores_v, rep(last_scores[2] + sci, length(thiscodes)))
                        newcodes <- c(newcodes, thiscodes)
                        newscores_h <- c(newscores_h, rep(last_scores[1], length(thiscodes)))
                    }
                }
                if (length(newcodes) != (n_adj * 3)) stop("adjustment length wrong?")
                ## TO CHECK if a point is being subtracted, is it still given a # green code ("point win") for that team?
                temp_point_ids <- rx$point_id
                if (n_adj > 1) temp_point_ids <- temp_point_ids + rep(seq_len(abs(hts_delta) + abs(vts_delta)), each = 3) / (n_adj + 1L)
                rx <- rx[rep(1L, length(newcodes)), keepcols] %>% mutate(team = substr(newcodes, 1, 1), point_id = temp_point_ids, point = substr(newcodes, 2, 2) == "p", code = newcodes, home_team_score = newscores_h, visiting_team_score = newscores_v)
            } else if (is_point) {
                ## add green codes - reconstruct codes first, codes to dv_green_codes need to be real ones
                if (!all(rx$team %in% c("a", "*"))) { print(dplyr::glimpse(rx)); stop("tm") }
                ss1 <- function(x) case_when(!is.na(x) & nzchar(x) ~ substr(x, 1, 1), TRUE ~ "~")
                temp_codes <- paste0(ss1(rx$team), lead0(rx$player, na = "00"), ss1(rx$skill), ss1(rx$hit_type), ss1(rx$effect))
                temp_codes[which(rx$point)] <- rx$code[which(rx$point)] ## retain the original point code
                codes2 <- dv_green_codes(temp_codes, meta)
                ## the last element of codes2 will be the point code pc, any other extras need to be injected
                if (length(codes2) > nrow(rx)) {
                    newcodes <- codes2[seq(from = nrow(rx), to = length(codes2) - 1L, by = 1L)] ## add these codes
                    rx <- bind_rows(head(rx, -1), ## not the last row
                                    rx[rep(nrow(rx) - 1L, length(newcodes)), keepcols] %>% mutate(team = substr(newcodes, 1, 1), point = FALSE, code = newcodes),
                                    tail(rx, 1))
                }
            }
            ## insert setter position codes, when setter has changed position (and not on first point, those come in the >LUp codes - so for the first point, make sure that the last_hsp and last_vsp are passed as their starting values)
            spcodes <- c()
            if (!rx$home_setter_position[1] %eq% last_hsp) {
                spcodes <- paste0("*z", rx$home_setter_position[1])
            }
            if (!rx$visiting_setter_position[1] %eq% last_vsp) {
                spcodes <- c(spcodes, paste0("az", rx$visiting_setter_position[1]))
            }
            if (length(spcodes) > 0) {
                ## if this was a sub, the sub code should appear before the spcodes
                rx <- bind_rows(if (is_sub) rx, rx[rep(1, length(spcodes)), keepcols] %>% mutate(team = substr(spcodes, 1, 1), code = spcodes), if (!is_sub) rx)
            }
            rx
        }
        ## update each rally, deal with score adjustment, update setter positions, etc
        temp <- list()
        last_hts <- last_vts <- 0L
        last_hsp <- thisex$home_setter_position[1]
        last_vsp <- thisex$visiting_setter_position[1]
        for (pid in unique(thisex$point_id)) {
            this <- thisex[thisex$point_id == pid, ]
            this <- fix_rally(this, last_hsp = last_hsp, last_vsp = last_vsp, last_hts = last_hts, last_vts = last_vts, meta = mx)
            temp[[pid]] <- this
            last_hts <- tail(this$home_team_score, 1)
            last_vts <- tail(this$visiting_team_score, 1)
            last_hsp <- tail(this$home_setter_position, 1)
            last_vsp <- tail(this$visiting_setter_position, 1)
        }

        thisex <- bind_rows(temp)
        ## insert >LUp codes at start of set
        hs <- tryCatch(unlist(thisex[1, paste0("home_p", pseq)])[thisex$home_setter_position[1]], error = function(e) 0L)
        vs <- tryCatch(unlist(thisex[1, paste0("visiting_p", pseq)])[thisex$visiting_setter_position[1]], error = function(e) 0L)
        lineup_codes <- paste0(c(paste0("*P", lead0(hs)), paste0("*z", thisex$home_setter_position[1]), paste0("aP", lead0(vs)), paste0("az", thisex$visiting_setter_position[1])), ">LUp")
        thisex <- bind_rows(thisex[rep(1, 4), keepcols] %>% mutate(code = lineup_codes, team = c("*", "*", "a", "a")), thisex)
        ## **Xset code at end
        bind_rows(thisex, thisex[nrow(thisex), keepcols] %>% mutate(code = paste0("**", si, "set"), end_of_set = TRUE, point_won_by = NA_character_))
    }))

    px$end_of_set[is.na(px$end_of_set)] <- FALSE

    ## re-create the proper scout code for skill rows, otherwise if we dv_write this and dv_read it, it will not work
    px$code <- vsm_row2code(px)

    idx <- which(!px$skill %in% c("S", "R", "E", "A", "B", "D", "F", "Timeout", "Technical timeout", NA_character_))
    if (length(idx) > 0) {
        msgs <- collect_messages(msgs, paste0("Unexpected skill: ", px$skill[idx]), line_nums = px_lnum(idx), xraw = x$raw, severity = 2)
    }
    temp_skill <- px$skill
    px$skill <- case_when(temp_skill == "S" ~ "Serve",
                          temp_skill == "R" ~ "Reception",
                          temp_skill == "E" ~ "Set",
                          temp_skill == "A" ~ "Attack",
                          temp_skill == "B" ~ "Block",
                          temp_skill == "D" ~ "Dig",
                          temp_skill == "F" ~ "Freeball",
                          TRUE ~ temp_skill)

    names(px)[names(px) == "end_sub_zone"] <- "end_subzone"
    idx <- which(!is.na(px$hit_type))
    px$skill_subtype_code <- px$skill_type ## we use subtype for what volleystation is calling type
    if (FALSE) {
        out_skill_type <- rep(NA_character_, nrow(px))
        for (ii in idx) {
            tmp <- skill_type_decode(temp_skill[ii], px$hit_type[ii], NA_character_, NA_integer_, file_type = file_type)
            out_skill_type[ii] <- tmp$decoded
            msgs <- join_messages(msgs, tmp$messages)
        }
        px$skill_type <- out_skill_type
    } else {
        temp <- dv_decode_skill_type(px$skill, px$hit_type)
        if (has_dvmsg(temp)) {
            msgs <- collect_messages(msgs, get_dvmsg(temp), xraw = x$raw, severity = 1)
        }
        px$skill_type <- clear_dvmsg(temp)
    }

    out_evaluation <- rep(NA_character_, nrow(px))
    px <- px %>% dplyr::rename(evaluation_code = "effect")
    if (FALSE) {
        idx <- which(!is.na(px$evaluation_code)) ## evaluation code
        for (ii in idx) {
            if (is.na(temp_skill[ii])) {
                msgs <- collect_messages(msgs, "Missing skill", NA_character_, NA_integer_, severity = 1)
            } else {
                out_evaluation[ii] <- skill_evaluation_decode(temp_skill[ii], px$evaluation_code[ii])
                if (is.na(out_evaluation[ii])) {
                    msgs <- collect_messages(msgs, paste0("Unknown evaluation code: ", px$evaluation_code[ii]), NA_integer_, NA_character_, severity = 2)
                }
                if (grepl("unknown", out_evaluation[ii], ignore.case = TRUE)) {
                    ## out_evaluation[ii] will be something like "unknown dig evaluation"
                    ## make it more informative
                    temp <- paste0(out_evaluation[ii],": ", px$evaluation_code[ii])
                    temp <- paste0(toupper(substr(temp, 1, 1)), substr(temp, 2, nchar(temp)))
                    msgs <- collect_messages(msgs, temp, NA_integer_, NA_character_, severity = 2)
                }
            }
        }
        px$evaluation <- out_evaluation
    } else {
        temp <- dv_decode_evaluation(px$skill, px$evaluation_code)
        if (has_dvmsg(temp)) {
            msgs <- collect_messages(msgs, get_dvmsg(temp), xraw = x$raw, severity = 1)
        }
        px$evaluation <- clear_dvmsg(temp)
    }

    ## attack combination codes and setter calls
    ## TODO check known attack types
    px <- px %>% mutate(attack_code = case_when(.data$skill == "Attack" ~ .data$combination))
    px <- left_join(px, mx$attacks %>% dplyr::select(attack_code = "code", attack_description = "description"), by = "attack_code")

    px <- px %>% mutate(set_code = case_when(.data$skill == "Set" ~ .data$combination))
    px <- left_join(px, mx$sets %>% dplyr::select(set_code = "code", set_description = "description"), by = "set_code")

    px <- px %>% dplyr::rename(set_type = "target_attacker")
    idx <- which(!px$set_type %in% c("F", "B", "C", "P", "S", NA_character_))
    if (length(idx) > 0) {
        msgs <- collect_messages(msgs, paste0("Set type (attack target) should be F, B, C, P, or S but is : ", px$set_type[idx]), line_nums = px_lnum(idx), xraw = x$raw, severity = 2)
    }

    ## start and end zones
    px$start_zone <- as.integer(px$start_zone)
    idx <- which(px$skill %in% c("Serve", "Reception") & !px$start_zone %in% c(1, 9, 6, 7, 5, NA_integer_))
    if (length(idx) > 0) {
        msgs <- collect_messages(msgs, paste0("Unexpected serve/reception start zone: ", px$start_zone[idx]), line_nums = px_lnum(idx), xraw = x$raw, severity = 2)
    }
    idx <- which(!px$start_zone %in% c(1:9, NA_integer_))
    if (length(idx) > 0) {
        msgs <- collect_messages(msgs, paste0("Unexpected start zone: ", px$start_zone[idx]), line_nums = px_lnum(idx), xraw = x$raw, severity = 2)
    }
    px$end_zone <- as.integer(px$end_zone)
    idx <- which(px$skill %in% c("Block") & !px$end_zone %in% c(2:4, NA_integer_))
    if (length(idx) > 0) {
        msgs <- collect_messages(msgs, paste0("Unexpected block end zone: ", px$end_zone[idx]), line_nums = px_lnum(idx), xraw = x$raw, severity = 2)
    }
    idx <- which(!px$end_zone %in% c(1:9, NA_integer_))
    if (length(idx) > 0) {
        msgs <- collect_messages(msgs, paste0("Unexpected end zone: ", px$end_zone[idx]), line_nums = px_lnum(idx), xraw = x$raw, severity = 2)
    }
    px$end_subzone <- toupper(as.character(px$end_subzone))
    idx <- which(!px$end_subzone %in% c("A", "B", "C", "D", NA_character_))
    if (length(idx) > 0) {
        msgs <- collect_messages(msgs, paste0("Unexpected end subzone: ", px$end_subzone[idx]), line_nums = px_lnum(idx), xraw = x$raw, severity = 2)
    }

    ## skill sub type ("TYPE OF HIT", p32)
    subtype_map <- tribble(~skill, ~skill_subtype_code, ~skill_subtype,
                           "Attack", "H", "Hard spike",
                           "Attack", "P", "Soft spike/topspin",
                           "Attack", "T", "Tip",
                           "Block", "A", "Block assist",
                           "Block", "T", "Block attempt",
                           "Block", "P", "Block on soft spike",
                           "Reception", "L", "On left",
                           "Reception", "R", "On right",
                           "Reception", "W", "Low",
                           "Reception", "O", "Overhand",
                           "Reception", "M", "Middle line",
                           "Set", "1", "1 hand set",
                           "Set", "2", "2 hands set",
                           "Set", "3", "Bump set",
                           "Set", "4", "Other set",
                           "Set", "5", "Underhand set",
                           "Set", "O", "Hand set",
                           "Set", "U", "Bump set",
                           "Dig", "S", "On spike",
                           "Dig", "C", "Spike cover",
                           "Dig", "B", "After block",
                           "Dig", "E", "Emergency",
                           "Dig", "T", "Tip",
                           "Dig", "P", "Soft spike")

    px <- left_join(px, subtype_map, by = c("skill", "skill_subtype_code")) %>%
        mutate(skill_subtype = case_when(is.na(.data$skill_subtype_code) & .data$skill == "Dig" & grepl("(Positive|Poor) block cover", .data$evaluation) ~ "Spike cover", ## volleymetrics scouted block cover D/ or D!
                                         is.na(.data$skill_subtype) & !is.na(.data$skill_subtype_code) ~ paste0("Unknown ", tolower(.data$skill), " subtype: ", .data$skill_subtype_code),
                                         TRUE ~ .data$skill_subtype)) %>%
        dplyr::select(-"skill_subtype_code")
    ## TODO messages for unknown skill subtype

    ## number of people ("PLAYERS", p33)
    px <- dplyr::rename(px, num_players_numeric = "players") %>%
        mutate(num_players_numeric = as.integer(.data$num_players_numeric))
    n_players_map <- if (grepl("beach", file_type)) {
                         tribble(~skill, ~num_players_numeric, ~num_players,
                                 "Attack", 0L, "No block",
                                 "Attack", 1L, "Line block",
                                 "Attack", 2L, "Crosscourt block",
                                 "Attack", 3L, "Block jumps to line",
                                 "Attack", 4L, "Block jumps to crosscourt",
                                 "Block", 0L, "No block",
                                 "Block", 1L, "Line block",
                                 "Block", 2L, "Crosscourt block",
                                 "Block", 3L, "Block jumps to line",
                                 "Block", 4L, "Block jumps to crosscourt")
                     } else {
                         tribble(~skill, ~num_players_numeric, ~num_players,
                                 "Attack", 0L, "No block",
                                 "Attack", 1L, "1 player block",
                                 "Attack", 2L, "2 player block",
                                 "Attack", 3L, "3 player block",
                                 "Attack", 4L, "Hole block",
                                 "Block", 0L, "No block",
                                 "Block", 1L, "1 player block",
                                 "Block", 2L, "2 player block",
                                 "Block", 3L, "3 player block",
                                 "Block", 4L, "Hole block",
                                 "Reception", 1L, "Two players receiving, the player on left receives",
                                 "Reception", 2L, "Two players receiving, the player on right receives",
                                 "Reception", 3L, "Three players receiving, the player on left receives",
                                 "Reception", 4L, "Three players receiving, the player in center receives",
                                 "Reception", 5L, "Three players receiving, the player on right receives",
                                 "Reception", 6L, "Four players receiving, the player on left receives",
                                 "Reception", 7L, "Four players receiving, the player on center-left receives",
                                 "Reception", 8L, "Four players receiving, the player on center-right receives",
                                 "Reception", 9L, "Four players receiving, the player on right receives")
                     }

    px <- left_join(px, n_players_map, by = c("skill", "num_players_numeric")) %>%
        mutate(num_players = case_when(is.na(.data$num_players) & !is.na(.data$num_players_numeric) ~ paste0("Unexpected ", .data$num_players),
                                       TRUE ~ .data$num_players))

    ## TODO messages for unknown num players

    ## special codes
    px <- mutate(px, special_code = case_when(.data$skill == "Attack" & .data$evaluation == "Error" ~ case_when(.data$special == "S" ~ "Attack out - side",
                                                                                                                .data$special == "O" ~ "Attack out - long",
                                                                                                                .data$special == "N" ~ "Attack in net",
                                                                                                                .data$special == "I" ~ "Net contact",
                                                                                                                .data$special == "Z" ~ "Referee call",
                                                                                                                .data$special == "A" ~ "Antenna",
                                                                                                                !is.na(.data$special) ~ paste0("Unexpected special code ", .data$special, " for attack error")),
                                              .data$skill == "Attack" & .data$evaluation == "Winning attack" ~ case_when(.data$special == "S" ~ "Block out - side",
                                                                                                                         .data$special == "O" ~ "Block out - long",
                                                                                                                         .data$special == "F" ~ "Block on floor",
                                                                                                                         .data$special == "X" ~ "Direct on floor",
                                                                                                                         .data$special == "N" ~ "Let",
                                                                                                                         !is.na(.data$special) ~ paste0("Unexpected special code ", .data$special, " for attack kill")),
                                              ## for A/ - continue codes applied to blocked attack - DV4 allows this
                                              .data$skill == "Attack" ~ case_when(.data$special == "C" ~ "Block control",
                                                                                  .data$special == "N" ~ "Let",
                                                                                  !is.na(.data$special) ~ paste0("Unexpected special code ", .data$special, " for attack")),
                                              .data$skill == "Block" ~ case_when(.data$special == "S" ~ "Ball out - side",
                                                                                 .data$special == "O" ~ "Ball out - long",
                                                                                 .data$special == "F" ~ "Ball on floor",
                                                                                 .data$special == "X" ~ "Between hands",
                                                                                 .data$special == "N" ~ "Hands - net",
                                                                                 .data$special == "I" ~ "Net contact",
                                                                                 .data$special == "A" ~ "Antenna",
                                                                                 .data$special == "P" ~ "No jump",
                                                                                 .data$special == "T" ~ "Position error",
                                                                                 .data$special == "Z" ~ "Referee call",
                                                                                 !is.na(.data$special) ~ paste0("Unexpected special code ", .data$special, " for block")),
                                              ## NB DV doesn't define 'Lack of effort' for freeball
                                              .data$skill %in% c("Reception", "Freeball") ~ case_when(.data$special == "U" ~ "Unplayable",
                                                                                                      .data$special == "X" ~ "Body error",
                                                                                                      .data$special == "P" ~ "Position error",
                                                                                                      .data$special == "Z" ~ "Referee call",
                                                                                                      .data$special == "E" ~ "Lack of effort",
                                                                                                      !is.na(.data$special) ~ paste0("Unexpected special code ", .data$special, " for ", tolower(.data$skill))),
                                              .data$skill == "Dig" ~ case_when(.data$special == "U" ~ "Unplayable",
                                                                               .data$special == "X" ~ "Body error",
                                                                               .data$special == "P" ~ "Position error",
                                                                               .data$special == "Z" ~ "Referee call",
                                                                               .data$special == "F" ~ "Ball on floor",
                                                                               .data$special == "O" ~ "Ball out",
                                                                               .data$special == "E" ~ "Lack of effort",
                                                                               !is.na(.data$special) ~ paste0("Unexpected special code ", .data$special, " for dig")),
                                              .data$skill == "Set" ~ case_when(.data$special == "U" ~ "Cannot be hit",
                                                                               .data$special == "I" ~ "Net touch",
                                                                               .data$special == "Z" ~ "Referee call",
                                                                               !is.na(.data$special) ~ paste0("Unexpected special code ", .data$special, " for set")),
                                              .data$skill == "Serve" & .data$evaluation == "Error" ~ case_when(.data$special == "O" ~ "Ball out - long",
                                                                                                               .data$special == "L" ~ "Ball out - left",
                                                                                                               .data$special == "R" ~ "Ball out - right",
                                                                                                               .data$special == "N" ~ "Ball in net",
                                                                                                               .data$special == "Z" ~ "Referee call",
                                                                                                               !is.na(.data$special) ~ paste0("Unexpected special code ", .data$special, " for serve error")),
                                              .data$skill == "Serve" ~ case_when(.data$special == "N" ~ "Let",
                                                                                 !is.na(.data$special) ~ paste0("Unexpected special code ", .data$special, " for serve"))))

    ## fill in player_name and player_id from player_number
    px <- dplyr::rename(px, player_number = "player") %>%
        mutate(player_name = case_when(.data$player_number == "$$" ~ "unknown player"), ## $$ in the player number slot is used to indicate an unknown player, not sure if this is used in VS
               player_id = NA_character_,
               player_number = case_when(.data$player_number == "$$" ~ NA_integer_,
                                         TRUE ~ as.integer(.data$player_number)))

    idx <- !is.na(px$player_number)
    px$player_name[idx] <- get_player_name(px$team[idx], px$player_number[idx], mx)
    px$player_id[idx] <- get_player_id(px$team[idx], px$player_number[idx], mx)
    dudidx <- (!is.na(px$player_number) & is.na(px$player_name)) | grepl("unknown player", px$player_name, ignore.case = TRUE)
    if (any(dudidx)) {
        msgs <- collect_messages(msgs, paste0("Player number ", px$player_number[dudidx], " could not be resolved to a player name/id"), line_nums = px_lnum(idx), xraw = x$raw, severity = 2)
    }


    ## add scores at START of point
    px$home_score_start_of_point <- ifelse(px$point_won_by %eq% "*", as.integer(px$home_team_score - 1L), as.integer(px$home_team_score))
    px$visiting_score_start_of_point <- ifelse(px$point_won_by %eq% "a", as.integer(px$visiting_team_score - 1L), as.integer(px$visiting_team_score))


    ## technical timeouts
    ## ^^ TODO add video_time
    if (is.flag(insert_technical_timeouts)) {
        if (isTRUE(insert_technical_timeouts)) {
            insert_technical_timeouts <- if (grepl("beach", file_type)) list(function(s1, s2) (s1 + s2) == 21, NULL) else list(c(8, 16), NULL)
        } else {
            insert_technical_timeouts <- list(NULL, NULL)
        }
    }
    set_sets <- if (grepl("beach", file_type)) list(1:2, 3) else list(1:4, 5)
    for (si in 1:2) {
        if (!is.null(insert_technical_timeouts[[si]])) {
            if (is.numeric(insert_technical_timeouts[[si]])) {
                ## find technical timeouts at e.g. points 8 and 16 in sets 1-4
                for (this_set in set_sets[[si]]) {
                    for (thisp in insert_technical_timeouts[[si]]) {
                        idx <- which((px$home_score_start_of_point == thisp | px$visiting_score_start_of_point == thisp) & px$set_number == this_set)
                        if (length(idx) > 0) {
                            idx <- idx[1]
                            px <- bind_rows(px[seq_len(idx - 1L), ],
                                            ##tibble(skill = "Technical timeout", point_id = px$point_id[idx] - 0.5, timeout = TRUE, set_number = this_set, point = FALSE, end_of_set = FALSE, substitution = FALSE),
                                            px[idx, c("set_number", "home_team_score", "visiting_team_score", "home_setter_position", "visiting_setter_position", paste0("home_p", pseq), paste0("visiting_p", pseq), "home_score_start_of_point", "visiting_score_start_of_point")] %>% mutate(skill = "Technical timeout", point_id = px$point_id[idx] - 0.5, timeout = TRUE, point = FALSE, end_of_set = FALSE, substitution = FALSE),
                                            px[setdiff(seq_len(nrow(px)), seq_len(idx - 1L)), ])
                        }
                    }
                }
            } else if (is.function(insert_technical_timeouts[[si]])) {
                ## function that is TRUE when a TTO should occur
                ## note this only allows one such TTO per set
                for (this_set in set_sets[[si]]) {
                    idx <- which(insert_technical_timeouts[[si]](px$home_score_start_of_point, px$visiting_score_start_of_point) & px$set_number == this_set)
                    if (length(idx) > 0) {
                        idx <- idx[1]
                        px <- bind_rows(px[seq_len(idx - 1L), ],
                                        ##tibble(skill = "Technical timeout", point_id = px$point_id[idx] - 0.5, timeout = TRUE, set_number = this_set, point = FALSE, end_of_set = FALSE, substitution = FALSE),
                                        px[idx, c("set_number", "home_team_score", "visiting_team_score", "home_setter_position", "visiting_setter_position", "home_p1", "home_p2", "home_p3", "home_p", "home_p5", "home_p6", "visiting_p1", "visiting_p2", "visiting_p3", "visiting_p4", "visiting_p5", "visiting_p6", "home_score_start_of_point", "visiting_score_start_of_point")] %>% mutate(skill = "Technical timeout", point_id = px$point_id[idx] - 0.5, timeout = TRUE, point = FALSE, end_of_set = FALSE, substitution = FALSE),
                                        px[setdiff(seq_len(nrow(px)), seq_len(idx - 1L)), ])
                    }
                }
            }
        }
    }
    ## renumber point_id to integer values
    px$point_id <- as.integer(as.factor(px$point_id)) - 1L ## zero based for compatibility

    ## add team_touch_id - an identifier of consecutive touches by same team in same point - e.g. a dig-set-attack sequence by one team is a "team touch"
    tid <- 0L
    temp_ttid <- rep(NA_integer_, nrow(px))
    temp_ttid[1] <- tid
    ## keep track of attacks - if a file has been scouted with only attacks, we need to account for that
    had_attack <- length(px$skill) > 0 && px$skill[1] %eq% "Attack" ## had_attack = had an attack already in this touch sequence
    for (k in seq_len(nrow(px))[-1]) {
        if (!identical(px$team[k], px$team[k - 1]) || !identical(px$point_id[k], px$point_id[k - 1]) || (px$skill[k] %eq% "Attack" && had_attack))  {
            tid <- tid + 1L
        }
        temp_ttid[k] <- tid
        had_attack <- px$skill[k] %eq% "Attack"
    }
    px$team_touch_id <- temp_ttid

    ## add player_id values for home_p1 etc
    for (thisp in pseq) {
        px[, paste0("home_player_id", thisp)] <- get_player_id(rep("*", nrow(px)), px[[paste0("home_p", thisp)]], mx)
        px[, paste0("visiting_player_id", thisp)] <- get_player_id(rep("a", nrow(px)), px[[paste0("visiting_p", thisp)]], mx)
    }

    ## team name and ID
    idx <- mx$teams$home_away_team %eq% "*"
    ht <- mx$teams$team[idx]
    ht_id <- mx$teams$team_id[idx]
    idx <- mx$teams$home_away_team %eq% "a"
    vt <- mx$teams$team[idx]
    vt_id <- mx$teams$team_id[idx]
    px <- mutate(px, home_team = ht, visiting_team = vt, home_team_id = ht_id, visiting_team_id = vt_id,
                 team_id = case_when(.data$team == "*" ~ ht_id, .data$team == "a" ~ vt_id),
                 team = case_when(.data$team == "*" ~ ht, .data$team == "a" ~ vt),
                 point_won_by = case_when(.data$point_won_by == "*" ~ ht, .data$point_won_by == "a" ~ vt))

    ## winning attacks, just for backwards compatibility
    ## A followed by D with "Error" evaluation, or A with "Winning attack" evaluation
    px <- mutate(px, winning_attack = .data$skill %eq% "Attack" & (.data$evaluation %eq% "Winning attack" | (dplyr::lead(.data$skill %in% c("Dig", "Block")) & dplyr::lead(.data$evaluation %eq% "Error"))))

    ## serving team
    px <- left_join(px,
                    px %>% dplyr::filter(.data$skill == "Serve") %>% dplyr::distinct(.data$point_id, serving_team = .data$team) %>% dplyr::filter(!duplicated(.data$point_id)),
                    by = "point_id")

    ## add play phase
    px$match_id <- mx$match_id
    px$phase <- play_phase(px)

    for (vr in c("start_coordinate", "mid_coordinate", "end_coordinate")) if (!vr %in% names(px)) { px[[vr]] <- NA_integer_ }
    for (vr in c("start_coordinate_x", "start_coordinate_y", "mid_coordinate_x", "mid_coordinate_y", "end_coordinate_x", "end_coordinate_y")) if (!vr %in% names(px)) { px[[vr]] <- NA_real_ }

    ## use _ids to infer line numbers (though these are really only line numbers in x$raw, because the original file is a single line)
    tempid <- tibble(`_id` = names(idlnum), file_line_number = unname(unlist(idlnum)))
    px <- left_join(px, tempid, by = "_id")
    ## fill in gaps, because subs and TOs didn't have _ids attached so they won't have line numbers, and neither will green codes or point adjustments
    try({
        idx <- is.na(px$file_line_number)
        px$file_line_number[idx] <- round(approx(which(!idx), px$file_line_number[!idx], which(idx))$y)
    })
    ## these interpolated file line numbers won't be exact, but close enough to be (hopefully) useful

    ##x$px <- px ## temporarily
    x$plays <- px %>% mutate(video_time = round(.data$time / 10),
                             time = as.POSIXct(NA), video_file_number = if (nrow(mx$video) > 0) 1L else NA_integer_,
                             end_cone = NA_integer_) %>%
        dplyr::select("match_id", ##"_id",
                      "point_id", "time", "video_file_number", "video_time", "code", "team", "player_number", "player_name", "player_id",
                      "skill", "skill_type", "evaluation_code", "evaluation", "attack_code", "attack_description", "set_code", "set_description", "set_type",
                      "start_zone", "end_zone", "end_subzone", "end_cone", "skill_subtype", "num_players", "num_players_numeric", "special_code", "timeout",
                      "end_of_set", "substitution", "point", "home_team_score", "visiting_team_score", "home_setter_position", "visiting_setter_position",
                      ## custom_code = ,
                      "file_line_number", "home_p1", "home_p2", "home_p3", "home_p4", "home_p5", "home_p6",
                      "visiting_p1", "visiting_p2", "visiting_p3", "visiting_p4", "visiting_p5", "visiting_p6",
                      "start_coordinate", "mid_coordinate", "end_coordinate",
                      ## point_phase = ,
                      ## attack_phase = ,
                      "start_coordinate_x", "start_coordinate_y", "mid_coordinate_x", "mid_coordinate_y", "end_coordinate_x", "end_coordinate_y",
                      "home_player_id1", "home_player_id2", "home_player_id3", "home_player_id4", "home_player_id5", "home_player_id6",
                      "visiting_player_id1", "visiting_player_id2", "visiting_player_id3", "visiting_player_id4", "visiting_player_id5", "visiting_player_id6",
                      "set_number", "team_touch_id", "home_team", "visiting_team", "home_team_id", "visiting_team_id", "team_id", "point_won_by",
                      "winning_attack", "serving_team", "phase", "home_score_start_of_point", "visiting_score_start_of_point")
    class(x$plays) <- c("datavolleyplays", class(x$plays))
    class(x) <- c("datavolley", class(x))

    ## update the set durations, subs, etc in the metadata
    x <- dv_update_meta(x)
    x$messages <- dplyr::select(bind_rows(msgs), -"severity")
    ## apply additional validation
    if (extra_validation > 0) {
        moreval <- validate_dv(x, validation_level = extra_validation, options = validation_options, file_type = file_type)
        if (!is.null(moreval) && nrow(moreval) > 0) x$messages <- bind_rows(x$messages, moreval)
    }
    if (is.null(x$messages)) x$messages <- data.frame(file_line_number=integer(), video_time=numeric(), message=character(), file_line=character(), stringsAsFactors=FALSE) ## should not happen, but just to be sure
    if (!is.null(x$messages) && nrow(x$messages) > 0) {
        x$messages$file_line_number <- as.integer(x$messages$file_line_number)
        x$messages <- x$messages[order(x$messages$file_line_number, na.last = FALSE),]
        row.names(x$messages) <- NULL
    }
    x
}

## convert skill rows of the vsm exchanges data.frame to proper codes
vsm_row2code <- function(x) {
    default_scouting_table <- dv_default_scouting_table()
    if (nrow(x) < 1) return(character())
    na2t <- function(z, width = 1) {
        default <- stringr::str_flatten(rep("~", width))
        if (is.null(z)) default else case_when(!is.na(z) & nchar(z) > 0 ~ stringr::str_pad(substr(z, 1, width), width, side = "right", pad = "~"), TRUE ~ default)
    }
    ##na2t(c("dsjfldjldk", "a", "", NA_character_, 99), 5) ==> "dsjfl" "a~~~~" "~~~~~" "~~~~~" "99~~~"
    out <- x$code
    ## only updating skill codes
    idx <- x$skill %in% c("S", "R", "E", "A", "B", "D", "F")
    if (!all(x$team[idx] %in% c("a", "*"))) {
        warning("at least one missing team code for a skill row")
        out[idx & !x$team %in% c("a", "*")] <- NA_character_
    }
    idx <- which(idx & x$team %in% c("a", "*"))
    if (length(idx) < 1) return(out)
    temp <- x[idx, ]
    pnum <- lead0(case_when(is.na(temp$player) | temp$player %in% c("Unknown", "Unknown player") ~ "00", TRUE ~ temp$player))
    pnum[nchar(pnum) > 2 | grepl("^\\-", pnum)] <- "00" ## illegal numbers, treat as unknown
    ## defaults
    if (is.null(default_scouting_table)) {
        for (ski in seq_len(nrow(default_scouting_table))) {
            temp$hit_type[temp$skill == default_scouting_table$skill[ski] & (is.na(temp$hit_type) | temp$hit_type == "~")] <- default_scouting_table$skill_type[ski]
            temp$effect[temp$skill == default_scouting_table$skill[ski] & (is.na(temp$effect) | temp$effect == "~")] <- default_scouting_table$evaluation_code[ski]
        }
    }
    out[idx] <- sub("~+$", "", paste0(temp$team, ## team code
                                      pnum, ## zero-padded player number
                                      temp$skill, ## skill
                                      temp$hit_type, ## skill_type (tempo)
                                      temp$effect, ## evaluation_code
                                      na2t(temp$combination, 2), ## attack combo code or setter call
                                      na2t(temp$target_attacker), ## target attacker
                                      na2t(temp$start_zone), na2t(temp$end_zone), na2t(temp$end_sub_zone), ## start, end, end subzone
                                      na2t(case_when((is.na(temp$skill_type) | temp$skill_type == "~") & temp$skill == "A" ~ "H", TRUE ~ temp$skill_type)), ## skill_subtype
                                      na2t(temp$players), ## num_players
                                      na2t(temp$special), na2t(temp$custom, 5)))
    out
}
