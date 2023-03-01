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
    jx <- jsonlite::fromJSON(filename)

    msgs <- list()
    file_type <- jx$gameType
    team_player_num <- if (isTRUE(grepl("beach", file_type))) 1:2 else 1:6
    x <- list(file_meta = data.frame(fileformat = "2.0",
                                     generator_day = as.POSIXct(NA),
                                     generator_idp = "DVW",
                                     generator_prg = "VolleyStation",
                                     generator_rel = "",
                                     generator_ver = NA_character_,
                                     generator_nam = "",
                                     lastchange_day = as.POSIXct(NA),
                                     lastchange_idp = "DVW",
                                     lastchange_prg = "VolleyStation",
                                     lastchange_rel = "",
                                     lastchange_ver = NA_character_,
                                     lastchange_nam = "",
                                     preferred_date_format = "ymd",
                                     file_type = file_type),
              messages = data.frame())

    temp <- lubridate::ymd_hms(jx$startDate)
    mx <- list(match = tibble(date = as.Date(temp),
                              time = tryCatch(lubridate::period(format(temp, "%HH %MM %SS")), error = function(e) lubridate::as.period(NA)),
                              season = NA, league = NA, phase = NA, home_away = NA, day_number = NA, match_number = NA,
                              text_encoding = "UTF-8",
                              regulation = if (isTRUE(jx$gameType == "indoor")) "indoor rally point" else stop("unexpected gameType: ", jx$gameType),
                              zones_or_cones = "Z", ## cones not supported in VS
                              X12 = NA, X13 = NA, X14 = NA, X15 = NA, X16 = NA),
               more = tibble(referees = NA_character_, spectators = NA_character_, receipts = NA, city = NA_character_, arena = NA_character_,
                             scout = NA_character_, X7 = NA, X8 = NA, X9 = NA, X10 = NA, X11 = NA), ## TODO surely the scout name at least is in the json
               comments = tibble(comment_1 = NA, comment_2 = NA, comment_3 = NA, comment_4 = NA, comment_5 = NA))
    mx$result <- tibble(played = TRUE, score_intermediate1 = NA_character_, score_intermediate2 = NA_character_, score_intermediate3 = NA_character_,
                        score = paste0(jx$scout$sets$score$home, "-", jx$scout$sets$score$away),
                        duration = NA_real_, ## update below
                        X7 = NA,
                        score_home_team = jx$scout$sets$score$home,
                        score_visiting_team = jx$scout$sets$score$away)
    c1 <- paste(jx$team$home$staff$coach$firstName, jx$team$home$staff$coach$lastName)
    if (length(c1) < 1) c1 <- NA_character_
    c2 <- paste(jx$team$away$staff$coach$firstName, jx$team$away$staff$coach$lastName)
    if (length(c2) < 1) c2 <- NA_character_
    a1 <- paste(paste(jx$team$home$staff$assistantCoach$firstName, jx$team$home$staff$assistantCoach$lastName), paste(jx$team$home$staff$assistantCoach2$firstName, jx$team$home$staff$assistantCoach2$lastName), collapse = " / ")
    if (length(a1) < 1) a1 <- NA_character_
    a2 <- paste(paste(jx$team$away$staff$assistantCoach$firstName, jx$team$away$staff$assistantCoach$lastName), paste(jx$team$away$staff$assistantCoach2$firstName, jx$team$away$staff$assistantCoach2$lastName), collapse = " / ")
    if (length(a2) < 1) a2 <- NA_character_
    mx$teams <- tibble(team_id = c(jx$team$home$code, jx$team$away$code), ## could also be shortCode
                       team = c(jx$team$home$name, jx$team$away$name),
                       sets_won = c(NA_integer_, NA_integer_), ## update below
                       coach = c(c1, c2),
                       assistant = c(a1, a2),
                       shirt_colour = NA_character_,
                       X7 = NA, X8 = NA, X9 = NA, X10 = NA,
                       home_away_team = c("*", "a"),
                       won_match = if (mean(mx$result$score_home_team > mx$result$score_visiting_team, na.rm = TRUE) > 0.5) {
                                       c(TRUE, FALSE)
                                   } else if (mean(mx$result$score_home_team > mx$result$score_visiting_team, na.rm = TRUE) < 0.5) {
                                       c(FALSE, TRUE)
                                   } else {
                                       NA
                                   })

    mx$players_h <- vs_reformat_players(jx, "home")
    mx$players_v <- vs_reformat_players(jx, "visiting")

    ax <- jx$attackCombinations
    if (any(duplicated(ax$code))) {
        warning("ignoring duplicate attack combination codes: ", paste0(unique(ax$code[duplicated(ax$code)]), collapse = ", "))
        ax <- ax[!duplicated(ax$code), , drop = FALSE]
    }
    temp <- datavolley::dv_xy(zones = ax$start$zone, subzones = ax$start$subZone, end = "lower")
    mx$attacks <- tibble(code = ax$code,
                         attacker_position = ax$start$zone,
                         side = NA, ## R,L,C,
                         type = ax$hitType,
                         description = ax$name,
                         X6 = NA,
                         colour = NA_character_,
                         start_coordinate = datavolley::dv_xy2index(temp),
                         set_type = ax$targetAttacker,
                         X10 = NA, X11 = NA)
    sx <- jx$setterCalls
    if (any(duplicated(sx$code))) {
        warning("ignoring duplicate setter calls: ", paste0(unique(sx$code[duplicated(sx$code)]), collapse = ", "))
        sx <- sx[!duplicated(sx$code), , drop = FALSE]
    }
    ## sx$area seems to be zone and subzone of where the middle runs
    mx$sets <- tibble(code = sx$code, X2 = NA, description = sx$name, X4 = NA,
                      colour = NA_character_,
                      start_coordinate = NA_integer_, mid_coordinate = NA_integer_, end_coordinate = NA_integer_,
                      path = NA_character_,  path_colour = NA_character_, X11 = NA)

    mx$winning_symbols <- dv_default_winning_symbols() ## TODO can this vary from vsm file to vsm file?
    mx$match_id <- make_match_id(mx)

    vf <- if (length(jx$scout$video) < 1 || length(jx$scout$video$path) < 1) character() else if (length(jx$scout$video$path) > 1) { warning("multiple video files not yet supported, using only the first"); jx$scout$video$path[1] } else jx$scout$video$path[1]
    mx$video <- data.frame(camera = if (length(vf) < 1) character() else "Camera0", file = vf)



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
        if (FALSE) {
            thisex <- bind_rows(lapply(seq_along(thisex$plays), function(j) {
                next_time <- if (j < length(thisex$plays) && is.data.frame(thisex$plays[[j + 1]]) && "plays" %in% names(thisex$plays[[j + 1]])) min(thisex$plays[[j + 1]]$time, na.rm = TRUE) else prev_time
                thisp <- if (!is.null(thisex$plays[[j]])) thisex$plays[[j]] %>% mutate(point_id = this_point_ids[j], point = FALSE) else NULL
                this_start_time <- if (is.data.frame(thisp) && "time" %in% names(thisp)) min(thisp$time, na.rm = TRUE) else prev_time ## TO CHECK
                this_end_time <- if (is.data.frame(thisp) && "time" %in% names(thisp)) max(thisp$time, na.rm = TRUE) else next_time ## TO CHECK
                ## if this "exchange" was a timeout, then thisp is likely NULL here, but deal with possibility that it isn't
                ## bind timeouts to start of this plays df
                if (this_point_ids[j] %in% thisto$point_id) {
                    thisp <- bind_rows(thisto %>% dplyr::filter(.data$point_id == this_point_ids[j]) %>% mutate(time = prev_time), if (!is.null(thisp)) thisp)
                }
                ## similarly insert sub rows, with NA skill
                if (this_point_ids[j] %in% thissub$point_id) {
                    thisp <- bind_rows(thissub %>% dplyr::filter(.data$point_id == this_point_ids[j]) %>% mutate(time = this_start_time), if (!is.null(thisp)) thisp)
                }
                ## do we need setter position codes? (not on first point, those come in the >LUp codes)
                spcodes <- c()
                if (j > 1 && (!thisev$lineup$home$setterAt[j] %eq% thisev$lineup$home$setterAt[j - 1])) {
                    spcodes <- paste0("*z", thisev$lineup$home$setterAt[j])
                }
                if (j > 1 && (!thisev$lineup$away$setterAt[j] %eq% thisev$lineup$away$setterAt[j - 1])) {
                    spcodes <- c(spcodes, paste0("az", thisev$lineup$away$setterAt[j]))
                }
                if (length(spcodes) > 0) {
                    thisp <- bind_rows(tibble(team = substr(spcodes, 1, 1), point_id = this_point_ids[j], time = this_start_time, point = FALSE, code = spcodes), thisp)
                }
                ## ^^^ TODO check if setter was substituted
                score_changed <- j == 1 || (!thisev$score$home[j] %eq% thisev$score$home[j - 1]) || (!thisev$score$away[j] %eq% thisev$score$away[j - 1])
                ## TODO can these blocks be consolidated?
                if (!is.na(thisex$point[j])) {
                    ## point code
                    pc <- paste0(thisex$point[j], "p", lead0(thisev$score$home[j], na = "99"), ":", lead0(thisev$score$away[j], na = "99"))
                    ## add green codes - codes to dv_green_codes need to be real ones
                    if (!all(thisp$team %in% c("a", "*"))) { print(dplyr::glimpse(thisp)); stop("tm") }
                    ss1 <- function(x) case_when(!is.na(x) & nzchar(x) ~ substr(x, 1, 1), TRUE ~ "~")
                    temp_codes <- c(paste0(ss1(thisp$team), lead0(thisp$player, na = "00"), ss1(thisp$skill), ss1(thisp$hitType), ss1(thisp$effect)), pc)
                    codes2 <- dv_green_codes(temp_codes, mx)
                    ## the last element of codes2 will be the point code pc, any other extras need to be injected
                    if (length(codes2) > (nrow(thisp) + 1L)) {
                        newcodes <- codes2[seq(from = nrow(thisp) + 1L, to = length(codes2) - 1L, by = 1L)] ## add these codes
                        thisp <- bind_rows(thisp,
                                           tibble(team = substr(newcodes, 1, 1), point_id = this_point_ids[j], time = this_end_time, point = FALSE, code = newcodes))
                    }
                    thisp <- bind_rows(thisp, tibble(team = thisex$point[j], point_id = this_point_ids[j], time = this_end_time, point = TRUE, code = pc)) ## add the point code
                } else if (score_changed) {
                    ## can have a score correction, which will have a score but an empty plays frame, no timeout, no sub, and assuming NA point
                    ## so add pc rows if needed, there may need to be more than one
                    last_scores <- if (j == 1) c(0, 0) else as.numeric(thisev$score[j - 1, ]) ## TODO ensure not missing
                    hts_delta <- thisev$score$home[j] - last_scores[1] ## change in home team score
                    vts_delta <- thisev$score$away[j] - last_scores[2] ## change in visiting team score
                    newcodes <- newscores_h <- newscores_v <- c()
                    temptm <- c() ## the team being assigned each of these points, also use this to keep track of the number of points to insert, use to adjust point_id
                    if (hts_delta != 0) {
                        for (sci in (seq_len(abs(hts_delta)) * sign(hts_delta))) {
                            newcodes <- c(newcodes, dv_green_codes(paste0("*p", lead0(last_scores[1] + sci), ":", lead0(last_scores[2])), mx))
                            newscores_h <- c(newscores_h, last_scores[1] + sci)
                        }
                        newscores_v <- rep(last_scores[2], length(newscores_h))
                        temptm <- c(temptm, rep("*", abs(hts_delta) * 3))
                        last_scores[1] <- thisev$score$home[j]
                    }
                    if (vts_delta != 0) {
                        for (sci in (seq_len(abs(vts_delta)) * sign(vts_delta))) {
                            newcodes <- c(newcodes, dv_green_codes(paste0("ap", lead0(last_scores[1]), ":", lead0(last_scores[2] + sci)), mx))
                            newscores_v <- c(newscores_v, last_scores[2] + sci)
                        }
                        temptm <- c(temptm, rep("a", abs(vts_delta) * 3))
                        newscores_h <- c(newscores_h, rep(last_scores[1], length(newscores_v) - length(newscores_h)))
                    }
                    ## ^^^ TODO also adjust the home_setter_position and visiting_setter_position so that those *zX and azX codes get entered (?)
                    ##   and in that case also change the lineups?
                    ## TO CHECK if a point is being subtracted, is it still given a # green code ("point win") for that team?
                    temp_point_ids <- this_point_ids[j] + rep(seq_len(abs(hts_delta) + abs(vts_delta)), each = 3) / (abs(hts_delta) + abs(vts_delta) + 1L)
                    ## ^^^ TODO add home_setter_position and visiting_setter_position cols here, lineups, scores
                    thisp <- bind_rows(thisp, tibble(team = temptm, point_id = temp_point_ids, time = this_end_time, point = TRUE, code = newcodes, home_team_score_corrected = rep(newscores_h, each = 3), visiting_team_score_corrected = rep(newscores_v, each = 3)))
                }
                if ("time" %in% names(thisp) && !all(is.na(thisp$time))) prev_time <<- max(thisp$time, na.rm = TRUE)
                thisp
            }))
            if (!"home_team_score_corrected" %in% names(thisex)) thisex$home_team_score_corrected <- NA_integer_
            if (!"visiting_team_score_corrected" %in% names(thisex)) thisex$visiting_team_score_corrected <- NA_integer_
            if (!"substitution" %in% names(thisex)) thisex$substitution <- FALSE
            if (!"timeout" %in% names(thisex)) thisex$timeout <- FALSE
            thisex <- left_join(thisex, pwb, by = "point_id") %>% mutate(set_number = si)
            this_pts <- thisev$score %>% mutate(point_id = this_point_ids) %>% dplyr::rename(home_team_score = "home", visiting_team_score = "away")
            thisex <- left_join(thisex, this_pts, by = "point_id") %>%
                mutate(home_team_score = case_when(!is.na(.data$home_team_score_corrected) ~ as.integer(.data$home_team_score_corrected), TRUE ~ as.integer(.data$home_team_score)),
                       visiting_team_score = case_when(!is.na(.data$visiting_team_score_corrected) ~ as.integer(.data$visiting_team_score_corrected), TRUE ~ as.integer(.data$visiting_team_score))) %>%
                dplyr::select(-"home_team_score_corrected", -"visiting_team_score_corrected")
            ## home team lineup
            this_htl <- tibble(home_setter_position = thisev$lineup$home$setterAt, point_id = this_point_ids)
            thisex <- left_join(thisex, this_htl, by = "point_id")
            this_htl <- thisev$lineup$home$positions %>% mutate(point_id = this_point_ids) %>%
                dplyr::rename(home_p1 = "1", home_p2 = "2", home_p3 = "3", home_p4 = "4", home_p5 = "5", home_p6 = "6")
            thisex <- left_join(thisex, this_htl, by = "point_id")
            ## visiting team lineup
            this_vtl <- tibble(visiting_setter_position = thisev$lineup$away$setterAt, point_id = this_point_ids)
            thisex <- left_join(thisex, this_vtl, by = "point_id")
            this_vtl <- thisev$lineup$away$positions %>% mutate(point_id = this_point_ids) %>%
                dplyr::rename(visiting_p1 = "1", visiting_p2 = "2", visiting_p3 = "3", visiting_p4 = "4", visiting_p5 = "5", visiting_p6 = "6")
            thisex <- left_join(thisex, this_vtl, by = "point_id")
            thisex$substitution[is.na(thisex$substitution)] <- FALSE
            thisex$timeout[is.na(thisex$timeout)] <- FALSE
            ## insert >LUp codes at start of set
            hs <- tryCatch(unlist(thisex[1, paste0("home_p", team_player_num)])[thisex$home_setter_position[1]], error = function(e) 0L)
            vs <- tryCatch(unlist(thisex[1, paste0("visiting_p", team_player_num)])[thisex$visiting_setter_position[1]], error = function(e) 0L)
            lineup_codes <- paste0(c(paste0("*P", lead0(hs)), paste0("*z", thisex$home_setter_position[1]), paste0("aP", lead0(vs)), paste0("az", thisex$visiting_setter_position[1])), ">LUp")
            lineup_df <- tibble(point_id = thisex$point_id[1], code = lineup_codes, team = c("*", "*", "a", "a"), substitution = FALSE, timeout = FALSE, point = FALSE, set_number = si)
            lineup_df <- dplyr::bind_cols(lineup_df, thisex[1, c("point_won_by", "home_team_score", "visiting_team_score", paste0("home_p", team_player_num), paste0("visiting_p", team_player_num))])
            thisex <- bind_rows(lineup_df, thisex)
            ## **Xset code at end
            eos_df <- tibble(point_id = max(thisex$point_id, na.rm = TRUE) + 1L, code = paste0("**", si, "set"), set_number = si, timeout = FALSE, substitution = FALSE, end_of_set = TRUE, point = FALSE, time = max(thisex$time, na.rm = TRUE))
            eos_df <- dplyr::bind_cols(eos_df, thisex[nrow(thisex), c("home_team_score", "visiting_team_score", paste0("home_p", team_player_num), paste0("visiting_p", team_player_num))])
            thisex <- bind_rows(thisex, eos_df)
            names(thisex) <- camel_to_underscore(names(thisex))
        } else {
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
            if (FALSE) {
                ## the filter-within-loop is very slow
                thisex <- bind_rows(lapply(seq_along(thisex$plays), function(j) {
                    next_time <- if (j < length(thisex$plays) && is.data.frame(thisex$plays[[j + 1]]) && "plays" %in% names(thisex$plays[[j + 1]])) min(thisex$plays[[j + 1]]$time, na.rm = TRUE) else prev_time
                    thisp <- if (!is.null(thisex$plays[[j]])) thisex$plays[[j]] %>% mutate(point_id = this_point_ids[j], point = FALSE) %>% dplyr::select(-"_id", -"originalTime") else NULL
                    this_start_time <- if (is.data.frame(thisp) && "time" %in% names(thisp)) min(thisp$time, na.rm = TRUE) else prev_time ## TO CHECK
                    this_end_time <- if (is.data.frame(thisp) && "time" %in% names(thisp)) max(thisp$time, na.rm = TRUE) else next_time ## TO CHECK
                    thisp <- bind_rows(thisto %>% dplyr::filter(.data$point_id == this_point_ids[j]) %>% mutate(time = prev_time),
                                       thissub %>% dplyr::filter(.data$point_id == this_point_ids[j]) %>% mutate(time = this_start_time),
                                       thisp,
                                       this_pts %>% dplyr::filter(.data$point, .data$point_id == this_point_ids[j]) %>% dplyr::select(-"home_team_score", -"visiting_team_score") %>% mutate(time = this_end_time))
                    ## (don't add home_team_score and visiting_team_score on the last row here, otherwise it's missing from other rows in the rally). left-join below
                    if ("time" %in% names(thisp) && !all(is.na(thisp$time))) prev_time <<- max(thisp$time, na.rm = TRUE)
                    thisp
                })) %>% mutate(substitution = case_when(is.na(.data$substitution) ~ FALSE, TRUE ~ .data$substitution),
                               timeout = case_when(is.na(.data$timeout) ~ FALSE, TRUE ~ .data$timeout),
                               point = case_when(is.na(.data$point) ~ FALSE, TRUE ~ .data$point))
            } else {
                ## faster - row-bind the plays, timeouts, subs, and points, with care to ensure row ordering is correct
                thisex <- bind_rows(lapply(seq_along(thisex$plays), function(j) {
                    if (!is.null(thisex$plays[[j]])) thisex$plays[[j]] %>% mutate(point_id = this_point_ids[j], point = FALSE) %>% dplyr::select(-"_id", -"originalTime") else NULL
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
            }
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
            hs <- tryCatch(unlist(thisex[1, paste0("home_p", team_player_num)])[thisex$home_setter_position[1]], error = function(e) 0L)
            vs <- tryCatch(unlist(thisex[1, paste0("visiting_p", team_player_num)])[thisex$visiting_setter_position[1]], error = function(e) 0L)
            lineup_codes <- paste0(c(paste0("*P", lead0(hs)), paste0("*z", thisex$home_setter_position[1]), paste0("aP", lead0(vs)), paste0("az", thisex$visiting_setter_position[1])), ">LUp")
            thisex <- bind_rows(thisex[rep(1, 4), keepcols] %>% mutate(code = lineup_codes, team = c("*", "*", "a", "a")), thisex)
            ## **Xset code at end
            thisex <- bind_rows(thisex, thisex[nrow(thisex), keepcols] %>% mutate(code = paste0("**", si, "set"), end_of_set = TRUE, point_won_by = NA_character_))
        }
        thisex
    }))

    px$end_of_set[is.na(px$end_of_set)] <- FALSE

    ## re-create the proper scout code for skill rows, otherwise if we dv_write this and dv_read it, it will not work
    px$code <- vsm_row2code(px)

    idx <- which(!px$skill %in% c("S", "R", "E", "A", "B", "D", "F", "Timeout", "Technical timeout", NA_character_))
    if (length(idx) > 0) {
        msgs <- collect_messages(msgs, paste0("Unexpected skill: ", px$skill[idx]), line_nums = NA_integer_, raw_lines = NA_character_, severity = 2)
    }
    temp_skill <- px$skill
    ##temp_skill[idx] <- NA_character_
    px$skill <- plyr::mapvalues(temp_skill, from = c("S", "R", "E", "A", "B", "D", "F"), to = c("Serve", "Reception", "Set", "Attack", "Block", "Dig", "Freeball"), warn_missing = FALSE)

    names(px)[names(px) == "end_sub_zone"] <- "end_subzone"
    idx <- which(!is.na(px$hit_type))
    out_skill_type <- out_evaluation <- rep(NA_character_, nrow(px))
    for (ii in idx) {
        tmp <- skill_type_decode(temp_skill[ii], px$hit_type[ii], NA_character_, NA_integer_, file_type = file_type)
        out_skill_type[ii] <- tmp$decoded
        msgs <- join_messages(msgs, tmp$messages)
    }
    px$skill_subtype_code <- px$skill_type ## we use subtype for what volleystation is calling type
    px$skill_type <- out_skill_type

    px <- px %>% dplyr::rename(evaluation_code = "effect")
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

    ## attack combination codes and setter calls
    ## TODO check known attack types
    px <- px %>% mutate(attack_code = case_when(.data$skill == "Attack" ~ .data$combination))
    px <- left_join(px, mx$attacks %>% dplyr::select(attack_code = "code", attack_description = "description"), by = "attack_code")

    px <- px %>% mutate(set_code = case_when(.data$skill == "Set" ~ .data$combination))
    px <- left_join(px, mx$sets %>% dplyr::select(set_code = "code", set_description = "description"), by = "set_code")

    px <- px %>% dplyr::rename(set_type = "target_attacker")
    idx <- which(!px$set_type %in% c("F", "B", "C", "P", "S", NA_character_))
    if (length(idx) > 0) {
        msgs <- collect_messages(msgs, paste0("Set type (attack target) should be F, B, C, P, or S but is : ", paste(unique(px$set_type[idx]), collapse = ", ")), line_nums = NA_integer_, raw_lines = NA_character_, severity = 2)
    }

    ## start and end zones
    px$start_zone <- as.integer(px$start_zone)
    idx <- which(px$skill %in% c("Serve", "Reception") & !px$start_zone %in% c(1, 9, 6, 7, 5, NA_integer_))
    if (length(idx) > 0) {
        msgs <- collect_messages(msgs,paste0("Unexpected serve/reception start zone: ", paste(unique(px$start_zone[idx]), collapse = ", ")), NA_integer_, NA_character_, severity = 2)
    }
    idx <- which(!px$start_zone %in% c(1:9, NA_integer_))
    if (length(idx) > 0) {
        msgs <- collect_messages(msgs,paste0("Unexpected start zone: ", paste(unique(px$start_zone[idx]), collapse = ", ")), NA_integer_, NA_character_, severity = 2)
    }
    px$end_zone <- as.integer(px$end_zone)
    idx <- which(px$skill %in% c("Block") & !px$end_zone %in% c(2:4, NA_integer_))
    if (length(idx) > 0) {
        msgs <- collect_messages(msgs,paste0("Unexpected block end zone: ", paste(unique(px$end_zone[idx]), collapse = ", ")), NA_integer_, NA_character_, severity = 2)
    }
    idx <- which(!px$end_zone %in% c(1:9, NA_integer_))
    if (length(idx) > 0) {
        msgs <- collect_messages(msgs,paste0("Unexpected end zone: ", paste(unique(px$end_zone[idx]), collapse = ", ")), NA_integer_, NA_character_, severity = 2)
    }
    px$end_subzone <- toupper(as.character(px$end_subzone))
    idx <- which(!px$end_subzone %in% c("A", "B", "C", "D", NA_character_))
    if (length(idx) > 0) {
        msgs <- collect_messages(msgs,paste0("Unexpected end subzone: ", paste(unique(px$end_subzone[idx]), collapse = ", ")), NA_integer_, NA_character_, severity = 2)
    }

    ## skill sub type ("TYPE OF HIT", p32)
    subtype_map <- tribble(~skill, ~skill_subtype_code, ~skill_subtype,
                           "Attack", "H", "Hard spike",
                           "Attack", "P", "Soft spike/topspin",
                           "Attack", "T", "Tip",
                           "BlocK", "A", "Block assist",
                           "BlocK", "T", "Block attempt",
                           "BlocK", "P", "Block on soft spike",
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
    if (any(dudidx))
        msgs <- collect_messages(msgs, paste0("Player number ", px$player_number[dudidx], " could not be resolved to a player name/id"), NA_integer_, NA_character_, severity = 2)


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
                                            px[idx, c("set_number", "home_team_score", "visiting_team_score", "home_setter_position", "visiting_setter_position", paste0("home_p", team_player_num), paste0("visiting_p", team_player_num), "home_score_start_of_point", "visiting_score_start_of_point")] %>% mutate(skill = "Technical timeout", point_id = px$point_id[idx] - 0.5, timeout = TRUE, point = FALSE, end_of_set = FALSE, substitution = FALSE),
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
    for (thisp in team_player_num) {
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

    ##x$px <- px ## temporarily
    x$plays <- px %>% mutate(video_time = round(.data$time / 10),
                             time = as.POSIXct(NA), video_file_number = if (nrow(mx$video) > 0) 1L else NA_integer_,
                             end_cone = NA_integer_,
                             start_coordinate = NA_integer_, mid_coordinate = NA_integer_, end_coordinate = NA_integer_,
                             start_coordinate_x = NA_real_, start_coordinate_y = NA_real_,
                             mid_coordinate_x = NA_real_, mid_coordinate_y = NA_real_,
                             end_coordinate_x = NA_real_, end_coordinate_y = NA_real_,
                             file_line_number = NA_integer_
                             ) %>%
        dplyr::select("match_id", "point_id", "time", "video_file_number", "video_time", "code", "team", "player_number", "player_name", "player_id",
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

    ## apply additional validation
    if (extra_validation > 0) {
        moreval <- validate_dv(x, validation_level = extra_validation, options = validation_options, file_type = file_type)
        if (!is.null(moreval) && nrow(moreval) > 0) x$messages <- bind_rows(x$messages, moreval)
    }
    if (is.null(x$messages)) x$messages <- data.frame(file_line_number=integer(), video_time=numeric(), message=character(), file_line=character(), stringsAsFactors=FALSE) ## should not happen, but just to be sure
    ##if (!is.null(x$messages) && nrow(x$messages) > 0) {
    ##    x$messages$file_line_number <- as.integer(x$messages$file_line_number)
    ##    x$messages <- x$messages[order(x$messages$file_line_number, na.last = FALSE),]
    ##    row.names(x$messages) <- NULL
    ##}
    x
}

## convert skill rows of the vsm exchanges data.frame to proper codes
vsm_row2code <- function(x) {
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
    out[idx] <- sub("~+$", "", paste0(temp$team, ## team code
                                      pnum, ## zero-padded player number
                                      temp$skill, ## skill
                                      case_when(is.na(temp$hit_type) | temp$hit_type == "~" ~ tryCatch(default_scouting_table$skill_type[default_scouting_table$skill == temp$skill], error = function(e) "~"), TRUE ~ temp$hit_type), ## skill_type (tempo)
                                      case_when(is.na(temp$effect) | temp$effect == "~" ~ tryCatch(default_scouting_table$evaluation_code[default_scouting_table$skill == temp$skill], error = function(e) "~"), TRUE ~ temp$effect), ## evaluation_code
                                      na2t(temp$combination, 2), ## attack combo code or setter call
                                      na2t(temp$target_attacker), ## target attacker
                                      na2t(temp$start_zone), na2t(temp$end_zone), na2t(temp$end_sub_zone), ## start, end, end subzone
                                      na2t(case_when((is.na(temp$skill_type) | temp$skill_type == "~") & temp$skill == "A" ~ "H", TRUE ~ temp$skill_type)), ## skill_subtype
                                      na2t(temp$players), ## num_players
                                      na2t(temp$special), na2t(temp$custom, 5)))
    out
}
