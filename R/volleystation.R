vs_int2role <- function(x) {
    r <- c("libero", "outside", "opposite", "middle", "setter", "unknown") ## zero-based
    out <- rep(NA_character_, length(x))
    idx <- x %in% 0:5
    out[idx] <- r[x[idx] + 1L]
    out
}

vs_reformat_players <- function(jx, which = "home") {
    jt <- if (which == "home") jx$team$home else jx$team$away
    px <- tibble(X1 = if (which == "home") 0L else 1L,
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
    px %>% dplyr::arrange(.data$number) %>% mutate(X3 = dplyr::row_number() + if (which %in% "home") 0L else nrow(jx$team$home$players))
}

dv_read_vsm <- function(filename, skill_evaluation_decode, insert_technical_timeouts = TRUE, do_transliterate = FALSE, extra_validation = 2, validation_options=list(), verbose = FALSE, ...) {
    ## do_warn=FALSE, do_transliterate=FALSE, surname_case="asis", custom_code_parser, metadata_only=FALSE, edited_meta
    if (is.function(skill_evaluation_decode)) stop("providing a function to skill_evaluation_decode is not supported for vsm files")
    skill_evaluation_decode <- match.arg(skill_evaluation_decode, c("default", "german", "guess", "volleymetrics")) ## used as the 'style' parm to dv_decode_* and dv_default_*

    x <- list(raw = readLines(filename, warn = FALSE))
    ## detect vm files imported into VS and saved as vsm the same way we do with dvw
    if (skill_evaluation_decode == "guess") {
        skill_evaluation_decode <- if (tryCatch(any(grepl("volleymetric", x$raw, ignore.case = TRUE)), error = function(e) FALSE)) "volleymetrics" else "default"
    }
    jx <- jsonlite::fromJSON(if (isTRUE(do_transliterate)) stri_trans_general(x$raw, "latin-ascii") else x$raw)
    x$raw <- str_split(str_replace_all(x$raw, fixed("{\"_id"), "\n{\"_id"), fixed("\n"))[[1]]
    raw_id <- str_match(x$raw, "\"_id\":\"([^\"]+)\"")[, 2]
    idlnum <- setNames(as.list(seq_along(raw_id)), raw_id) ## line numbers, named by their corresponding _id
    ## there should not be duplicate IDs, but they will cause problems so make sure
    idlnum <- idlnum[!is.na(raw_id) & !raw_id %in% raw_id[duplicated(raw_id)]]
    px_lnum <- function(idx) {
        if (is.logical(idx)) idx <- which(idx)
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
    if (length(file_type) < 1) {
        if (is.data.frame(jx$scout$sets$startingLineup$home$positions) && ncol(jx$scout$sets$startingLineup$home$positions) == 6) {
            file_type <- "indoor"
        } else {
            stop("gameType is missing and could not be inferred")
        }
    }

    if (!file_type %in% c("indoor", "beach")) stop("file type: ", file_type, " is not supported yet, please contact the package authors or submit an issue via <", utils::packageDescription("datavolley")$BugReports, ">")
    if (file_type == "beach") warning("beach files have not been tested yet")
    pseq <- seq_len(if (file_type == "beach") 2 else 6)
    x$file_meta <- dv_create_file_meta(generator_day = as.POSIXct(NA), generator_idp = "DVW", generator_prg = "VolleyStation",
                                      generator_release = "", generator_version = NA_character_, generator_name = "", file_type = file_type)

    ## note that the vsm appears to include the startDate time as UTC, and so it can differ from the time in the same file exported to dvw
    mx <- list(match = dv_create_meta_match(date = lubridate::ymd_hms(jx$startDate),
                                            regulation = if (file_type == "indoor") "indoor rally point" else if (file_type == "beach") "beach rally point" else stop("unexpected game type: ", file_type),
                                            zones_or_cones = "Z"), ## cones not supported in VS
               more = dv_create_meta_more()) ## TODO surely the scout name at least is in the json
    if ("comment" %in% names(jx)) {
        ## e.g. "---- summary ----\nno comments\n\n---- match description ----\n\n\n---- coach home ----\n\n\n---- coach away ----\n"
        cx <- strsplit(jx$comment, "\n\\-\\-\\-\\-[[:space:]]*")[[1]]
        cx <- str_trim(sub(".*\\-\\-\\-\\-\n", "", cx))
        cx <- c(cx, rep(NA_character_, 4 - length(cx)))
        mx$comments = dv_create_meta_comments(summary = cx[1], match_description = cx[2], home_coach_comments = cx[3], visiting_coach_comments = cx[4])
    } else {
        mx$comments = dv_create_meta_comments()
    }
    mx$result <- dv_create_meta_result(home_team_scores = jx$scout$sets$score$home, visiting_team_scores = jx$scout$sets$score$away) ## will be further populated by dv_update_meta below
    c1 <- paste(str_trim(jx$team$home$staff$coach$firstName), str_trim(jx$team$home$staff$coach$lastName))
    if (length(c1) < 1) c1 <- NA_character_
    c2 <- paste(str_trim(jx$team$away$staff$coach$firstName), str_trim(jx$team$away$staff$coach$lastName))
    if (length(c2) < 1) c2 <- NA_character_
    a1 <- paste(paste(str_trim(jx$team$home$staff$assistantCoach$firstName), str_trim(jx$team$home$staff$assistantCoach$lastName)), paste(str_trim(jx$team$home$staff$assistantCoach2$firstName), str_trim(jx$team$home$staff$assistantCoach2$lastName)), collapse = " / ")
    if (length(a1) < 1) a1 <- NA_character_
    a2 <- paste(paste(str_trim(jx$team$away$staff$assistantCoach$firstName), str_trim(jx$team$away$staff$assistantCoach$lastName)), paste(str_trim(jx$team$away$staff$assistantCoach2$firstName), str_trim(jx$team$away$staff$assistantCoach2$lastName)), collapse = " / ")
    if (length(a2) < 1) a2 <- NA_character_
    tx <- dv_create_meta_teams(team_ids = c(jx$team$home$code, jx$team$away$code),
                                     teams = c(jx$team$home$name, jx$team$away$name),
                                     coaches = c(c1, c2),
                                     assistants = c(a1, a2))
    if (has_dvmsg(tx)) msgs <- collect_messages(msgs, get_dvmsg(tx), xraw = x$raw)
    mx$teams <- clear_dvmsg(tx)

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

    sx <- dv_create_meta_setter_calls(code = jx$setterCalls$code, description = jx$setterCalls$name) ## jx$setterCalls$area seems to be zone and subzone of where the middle runs
    if (has_dvmsg(sx)) {
        idx <- head(grep("setterCalls", x$raw), 1)
        if (length(idx) < 1) idx <- NA_integer_
        msgs <- collect_messages(msgs, get_dvmsg(sx)$message, line_nums = idx + 1L, raw_lines = if (is.na(idx)) NA_character_ else x$raw[idx + 1L], severity = 3)
    }
    mx$sets <- clear_dvmsg(sx)

    mx$winning_symbols <- dv_default_winning_symbols(data_type = file_type, style = skill_evaluation_decode) ## this can vary from vsm file to vsm file, but it's not saved anywhere
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
        ## sub is [a*]c[OUT]:[IN]
        thissub <- if (is.data.frame(thisev$substitution)) {
                       thisev$substitution %>% mutate(point_id = this_point_ids) %>% dplyr::filter(!is.na(.data$team)) %>% mutate(code = paste0(.data$team, "c", lead0(.data$playerOut, na = "00"), ":", lead0(.data$playerIn, na = "00")), substitution = TRUE) ##%>% dplyr::select(-"playerIn", -"playerOut")
                   } else {
                       tibble(point_id = integer(), team = character(), code = character(), substitution = logical())
                   }
        ## scores
        this_pts <- thisev$score %>% mutate(point_id = this_point_ids) %>% dplyr::rename(home_team_score = "home", visiting_team_score = "away") %>%
            left_join(pwb %>% dplyr::rename(team = "point_won_by"), by = "point_id") %>%
            mutate(code = case_when(!is.na(.data$team) ~ paste0(.data$team, "p", lead0(.data$home_team_score, na = "99"), ":", lead0(.data$visiting_team_score, na = "99"))),
                   point = !is.na(.data$team))
        ## columns to preserve when adding new rows to the dataframe
        keepcols <- c("point_id", "time", "home_team_score", "visiting_team_score", "point_won_by", "set_number",
                      "home_setter_position", "visiting_setter_position", paste0("home_p", pseq), paste0("visiting_p", pseq))
        ## row-bind the plays, timeouts, subs, and points, with care to ensure row ordering is correct
        thisex <- bind_rows(lapply(seq_along(thisex$plays), function(j) {
            if (!is.null(thisex$plays[[j]])) {
                temp <- thisex$plays[[j]] %>% mutate(point_id = this_point_ids[j], point = FALSE)
                if ("originalTime" %in% names(temp)) temp <- dplyr::select(temp, -"originalTime")
                if ("travelPath" %in% names(temp)) {
                    tp <- bind_rows(lapply(temp$travelPath, function(z) {
                        if (is.null(z)) {
                            tibble(start_coordinate_x = NA_integer_, start_coordinate_y = NA_integer_,
                                   end_coordinate_x = NA_integer_, end_coordinate_y = NA_integer_)
                        } else {
                            if (nrow(z) < 2 || nrow(z) > 3) stop("travelPath not 2 or 3 rows")
                            ## convert to dv grid conventions
                            z$x <- z$x / 293.3333 + 0.447159
                            z$y <- z$y / 297 + 0.469697
                            if (nrow(z) == 2) {
                                tibble(start_coordinate_x = z$x[1], start_coordinate_y = z$y[1], end_coordinate_x = z$x[2], end_coordinate_y = z$y[2])
                            } else {
                                tibble(start_coordinate_x = z$x[1], start_coordinate_y = z$y[1], mid_coordinate_x = z$x[2], mid_coordinate_y = z$y[2], end_coordinate_x = z$x[3], end_coordinate_y = z$y[3])
                            }
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
            mutate(prev_time = lag(.data$this_end_time), next_time = lead(.data$this_start_time),
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
        ## update each rally, deal with score adjustment, update setter positions, etc
        temp <- list()
        last_hts <- last_vts <- 0L
        last_hsp <- thisex$home_setter_position[1]
        last_vsp <- thisex$visiting_setter_position[1]
        pids <- unique(thisex$point_id)
        for (pidi in seq_along(pids)) {
            this <- thisex[thisex$point_id == pids[pidi], ]
            ## TODO convert warnings from dv_expand_rally_codes and dv_green_codes to messages to be captured here
            this <- dv_expand_rally_codes(this %>% dplyr::rename(evaluation_code = "effect", player_number = "player", skill_type_code = "hit_type"),
                                          last_home_setter_position = last_hsp, last_visiting_setter_position = last_vsp,
                                          last_home_team_score = last_hts, last_visiting_team_score = last_vts, keepcols = keepcols, meta = mx) %>%
                dplyr::rename(effect = "evaluation_code", player = "player_number", hit_type = "skill_type_code")
            temp[[pidi]] <- this
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

    px <- mutate(px, substitution = case_when(is.na(.data$substitution) ~ FALSE, TRUE ~ .data$substitution),
                 timeout = case_when(is.na(.data$timeout) ~ FALSE, TRUE ~ .data$timeout),
                 point = case_when(is.na(.data$point) ~ FALSE, TRUE ~ .data$point),
                 end_of_set = case_when(is.na(.data$end_of_set) ~ FALSE, TRUE ~ .data$end_of_set))

    ## ensure some columns are present, these are known to be missing in some files
    req <- list(special = NA_character_, custom = NA_character_,
                start_coordinate_x = NA_real_, mid_coordinate_x = NA_real_, end_coordinate_x = NA_real_,
                start_coordinate_y = NA_real_, mid_coordinate_y = NA_real_, end_coordinate_y = NA_real_,
                start_sub_zone = NA_character_)
    for (rc in names(req)) if (!rc %in% names(px)) { px[[rc]] <- req[[rc]] }

    ## check that all expected columns are present
##    expctd <- c("point_id", "time", "home_team_score", "visiting_team_score", "point_won_by", "set_number", "home_setter_position", "visiting_setter_position", paste0("home_p", pseq), paste0("visiting_p", pseq), "code", "team", "skill", "timeout", "player_in", "player_out", "substitution", "_id", "player", "hit_type", "effect", "start_zone", "end_zone", "end_sub_zone", "combination", "target_attacker", "skill_type", "players", "point", "end_of_set")
##    ## plus start/mid/end coordinate x,y, special, custom, start_sub_zone but we've already ensured these
##    for (rc in setdiff(expctd, names(px))) px[[rc]] <- NA ## might run into type problems later with these

    ## other bits that need to be done before rebuilding the scout code
    names(px)[names(px) == "end_sub_zone"] <- "end_subzone"
    names(px)[names(px) == "start_sub_zone"] <- "start_subzone"
    px$start_zone <- as.integer(px$start_zone)
    px$end_zone <- as.integer(px$end_zone)
    px$end_subzone <- toupper(as.character(px$end_subzone))
    px$start_subzone <- toupper(as.character(px$start_subzone))

    ## vsm's use start and end locations relative to the skill on that row, but dvw convention is to use e.g. reception start the same as serve start
    pairs <- ((px$skill == "R" & lag(px$skill) == "S") |
              (px$skill == "D" & lag(px$skill) == "A")) & (lag(px$team) != px$team)
    px <- mutate(px, start_zone = case_when(pairs ~ lag(.data$start_zone), TRUE ~ .data$start_zone),
                 start_zone = case_when(pairs ~ lag(.data$start_zone), TRUE ~ .data$start_zone),
                 end_zone = case_when(pairs ~ lag(.data$end_zone), TRUE ~ .data$end_zone),
                 end_subzone = case_when(pairs ~ lag(.data$end_subzone), TRUE ~ .data$end_subzone),
                 start_coordinate_x = case_when(pairs ~ lag(.data$start_coordinate_x), TRUE ~ .data$start_coordinate_x),
                 start_coordinate_y = case_when(pairs ~ lag(.data$start_coordinate_y), TRUE ~ .data$start_coordinate_y),
                 mid_coordinate_x = case_when(pairs ~ lag(.data$mid_coordinate_x), TRUE ~ .data$mid_coordinate_x),
                 mid_coordinate_y = case_when(pairs ~ lag(.data$mid_coordinate_y), TRUE ~ .data$mid_coordinate_y),
                 end_coordinate_x = case_when(pairs ~ lag(.data$end_coordinate_x), TRUE ~ .data$end_coordinate_x),
                 end_coordinate_y = case_when(pairs ~ lag(.data$end_coordinate_y), TRUE ~ .data$end_coordinate_y))

    ## vsm's use the start zone/subzone for the set and block location, but dvw convention is to put these into end zone/subzone
    px <- mutate(px, end_zone = case_when(.data$skill %in% c("B", "E") ~ .data$start_zone, TRUE ~ .data$end_zone),
                 end_subzone = case_when(.data$skill %in% c("B", "E") ~ .data$start_subzone, TRUE ~ .data$end_subzone),
                 start_zone = case_when(.data$skill %in% c("B", "E") ~ NA_integer_, TRUE ~ .data$start_zone),
                 start_subzone = case_when(.data$skill %in% c("B", "E") ~ NA_character_, TRUE ~ .data$start_subzone))

    ## convert x,y coords back to single-index coords
    px$start_coordinate <- dv_xy2index(px$start_coordinate_x, px$start_coordinate_y)
    px$mid_coordinate <- dv_xy2index(px$mid_coordinate_x, px$mid_coordinate_y)
    px$end_coordinate <- dv_xy2index(px$end_coordinate_x, px$end_coordinate_y)

    ## other cols needed before re-creating code
    ## attack combination codes and setter calls
    ## TODO check known attack types?
    px <- px %>% mutate(attack_code = case_when(.data$skill == "A" ~ .data$combination), set_code = case_when(.data$skill == "E" ~ .data$combination))
    mxa <- mx$attacks %>% dplyr::select(attack_code = "code", attack_description = "description", target_attacker = "set_type")
    ## target_attacker might or might not already be in the px data frame, depending on the vsm file
    if ("target_attacker" %in% names(px)) mxa <- dplyr::select(mxa, -"target_attacker")
    px <- left_join(px, mxa, by = "attack_code")
    px <- left_join(px, mx$sets %>% dplyr::select(set_code = "code", set_description = "description"), by = "set_code")

    ## re-create the proper scout code for skill rows, otherwise if we dv_write this and dv_read it, it will not work
    px$code <- vsm_row2code(px, data_type = file_type, style = skill_evaluation_decode)

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
        temp <- dv_decode_skill_type(px$skill, px$hit_type, data_type = file_type, style = skill_evaluation_decode)
        if (has_dvmsg(temp)) {
            msgs <- collect_messages(msgs, get_dvmsg(temp), xraw = x$raw)
        }
        px$skill_type <- clear_dvmsg(temp)
    }

    out_evaluation <- rep(NA_character_, nrow(px))
    px <- px %>% dplyr::rename(evaluation_code = "effect")
    if (skill_evaluation_decode %eq% "volleymetrics") {
        idx <- which(px$skill == "Attack" & lead(px$skill) == "Block" & lead(px$evaluation_code) == "/")
        if (length(idx) > 0) {
            px$evaluation_code[idx] <- "!"
            temp <- px$code[idx]
            substr(temp, 6, 6) <- "!"
            px$code[idx] <- temp
        }
    }
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
        temp <- dv_decode_evaluation(px$skill, px$evaluation_code, data_type = file_type, style = skill_evaluation_decode)
        if (has_dvmsg(temp)) {
            msgs <- collect_messages(msgs, get_dvmsg(temp), xraw = x$raw)
        }
        px$evaluation <- clear_dvmsg(temp)
    }

    px <- px %>% dplyr::rename(set_type = "target_attacker")
    idx <- which(!px$set_type %in% c("F", "B", "C", "P", "S", NA_character_))
    if (length(idx) > 0) {
        msgs <- collect_messages(msgs, paste0("Set type (attack target) should be F, B, C, P, or S but is : ", px$set_type[idx]), line_nums = px_lnum(idx), xraw = x$raw, severity = 2)
    }

    ## start and end zones
    idx <- which(px$skill %in% c("Serve", "Reception") & !px$start_zone %in% c(1, 9, 6, 7, 5, NA_integer_))
    if (length(idx) > 0) {
        msgs <- collect_messages(msgs, paste0("Unexpected serve/reception start zone: ", px$start_zone[idx]), line_nums = px_lnum(idx), xraw = x$raw, severity = 2)
    }
    idx <- which(!px$start_zone %in% c(1:9, NA_integer_))
    if (length(idx) > 0) {
        msgs <- collect_messages(msgs, paste0("Unexpected start zone: ", px$start_zone[idx]), line_nums = px_lnum(idx), xraw = x$raw, severity = 2)
    }
    idx <- which(px$skill %in% c("Block") & !px$end_zone %in% c(2:4, NA_integer_))
    if (length(idx) > 0) {
        msgs <- collect_messages(msgs, paste0("Unexpected block end zone: ", px$end_zone[idx]), line_nums = px_lnum(idx), xraw = x$raw, severity = 2)
    }
    idx <- which(!px$end_zone %in% c(1:9, NA_integer_))
    if (length(idx) > 0) {
        msgs <- collect_messages(msgs, paste0("Unexpected end zone: ", px$end_zone[idx]), line_nums = px_lnum(idx), xraw = x$raw, severity = 2)
    }
    idx <- which(!px$end_subzone %in% c("A", "B", "C", "D", NA_character_))
    if (length(idx) > 0) {
        msgs <- collect_messages(msgs, paste0("Unexpected end subzone: ", px$end_subzone[idx]), line_nums = px_lnum(idx), xraw = x$raw, severity = 2)
    }

    ## skill sub type ("TYPE OF HIT", p32)
    temp <- dv_decode_skill_subtype(px$skill, skill_subtype_code = px$skill_subtype_code, evaluation = px$evaluation, data_type = file_type, style = skill_evaluation_decode)
    if (has_dvmsg(temp)) {
        msgs <- collect_messages(msgs, get_dvmsg(temp), xraw = x$raw)
    }
    px$skill_subtype <- clear_dvmsg(temp)

    ## number of people ("PLAYERS", p33)
    px <- dplyr::rename(px, num_players_numeric = "players") %>%
        mutate(num_players_numeric = as.integer(.data$num_players_numeric))
    temp <- dv_decode_num_players(px$skill, num_players_code = px$num_players_numeric, data_type = file_type, style = skill_evaluation_decode)
    if (has_dvmsg(temp)) {
        msgs <- collect_messages(msgs, get_dvmsg(temp), xraw = x$raw)
    }
    px$num_players <- clear_dvmsg(temp)

    ## special codes
    if (all(is.na(px$special))) {
        px$special_code <- NA_character_
    } else {
        temp <- dv_decode_special_code(px$skill, special_code = px$special, evaluation = px$evaluation, data_type = file_type, style = skill_evaluation_decode)
        if (has_dvmsg(temp)) {
            msgs <- collect_messages(msgs, get_dvmsg(temp), xraw = x$raw)
        }
        px$special_code <- clear_dvmsg(temp)
    }

    ## fill in player_name and player_id from player_number
    px <- dplyr::rename(px, player_number = "player") %>%
        mutate(player_name = case_when(.data$player_number == "$$" ~ "unknown player"), ## $$ in the player number slot is used to indicate an unknown player, not sure if this is used in VS
               player_id = NA_character_,
               player_number = case_when(.data$player_number == "$$" ~ NA_integer_,
                                         TRUE ~ as.integer(.data$player_number)))

    idx <- !is.na(px$player_number)
    px$player_name[idx] <- get_player_name(px$team[idx], px$player_number[idx], mx)
    px$player_id[idx] <- get_player_id(px$team[idx], px$player_number[idx], mx)
    dudidx <- which((!is.na(px$player_number) & is.na(px$player_name)) | grepl("unknown player", px$player_name, ignore.case = TRUE))
    if (length(dudidx) > 0) {
        msgs <- collect_messages(msgs, paste0("Player number ", px$player_number[dudidx], " could not be resolved to a player name/id"), line_nums = px_lnum(dudidx), xraw = x$raw, severity = 2)
    }

    ## add scores at START of point
    px$home_score_start_of_point <- ifelse(px$point_won_by %eq% "*", as.integer(px$home_team_score - 1L), as.integer(px$home_team_score))
    px$visiting_score_start_of_point <- ifelse(px$point_won_by %eq% "a", as.integer(px$visiting_team_score - 1L), as.integer(px$visiting_team_score))

    ## technical timeouts
    if (isTRUE(insert_technical_timeouts)) px <- dv_insert_technical_timeouts(px, data_type = file_type)

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
    px <- mutate(px, winning_attack = .data$skill %eq% "Attack" & (.data$evaluation %eq% "Winning attack" | (lead(.data$skill %in% c("Dig", "Block")) & lead(.data$evaluation %eq% "Error"))))

    ## serving team
    px <- left_join(px,
                    px %>% dplyr::filter(.data$skill == "Serve") %>% dplyr::distinct(.data$point_id, serving_team = .data$team) %>% dplyr::filter(!duplicated(.data$point_id)),
                    by = "point_id")

    ## add play phase
    px$match_id <- mx$match_id
    px$phase <- play_phase(px)
    ## and the DV point_phase and attack_phase
    px$point_phase <- dv_point_phase(px)
    px$attack_phase <- dv_attack_phase(px)

    ## use _ids to infer line numbers (though these are really only line numbers in x$raw, because the original file is a single line)
    tempid <- tibble(`_id` = names(idlnum), file_line_number = unname(unlist(idlnum)))
    px <- left_join(px, tempid, by = "_id")
    ## fill in gaps, because subs and TOs didn't have _ids attached so they won't have line numbers, and neither will green codes or point adjustments
    try({
        idx <- is.na(px$file_line_number)
        px$file_line_number[idx] <- round(approx(which(!idx), px$file_line_number[!idx], which(idx))$y)
    })
    ## these interpolated file line numbers won't be exact, but close enough to be (hopefully) useful

    x$plays <- px %>% mutate(video_time = round(round(.data$time / 10)), ## must be integer
                             time = as.POSIXct(NA), video_file_number = if (nrow(mx$video) > 0) 1L else NA_integer_,
                             end_cone = NA_integer_) %>%
        dplyr::select("match_id", ##"_id",
                      "point_id", "time", "video_file_number", "video_time", "code", "team", "player_number", "player_name", "player_id",
                      "skill", "skill_type", "evaluation_code", "evaluation", "attack_code", "attack_description", "set_code", "set_description", "set_type",
                      "start_zone", ##"start_subzone", ## maybe
                      "end_zone", "end_subzone", "end_cone", "skill_subtype", "num_players", "num_players_numeric", "special_code", "timeout",
                      "end_of_set", "substitution", "point", "home_team_score", "visiting_team_score", "home_setter_position", "visiting_setter_position",
                      custom_code = "custom", "file_line_number",
                      paste0("home_p", pseq), paste0("visiting_p", pseq),
                      "start_coordinate", "mid_coordinate", "end_coordinate", "point_phase", "attack_phase",
                      "start_coordinate_x", "start_coordinate_y", "mid_coordinate_x", "mid_coordinate_y", "end_coordinate_x", "end_coordinate_y",
                      paste0("home_player_id", pseq), paste0("visiting_player_id", pseq),
                      "set_number", "team_touch_id", "home_team", "visiting_team", "home_team_id", "visiting_team_id", "team_id", "point_won_by",
                      "winning_attack", "serving_team", "phase", "home_score_start_of_point", "visiting_score_start_of_point")
    class(x$plays) <- c("datavolleyplays", class(x$plays))
    class(x) <- c("datavolley", class(x))

    ## update the set durations, subs, etc in the metadata
    x <- dv_update_meta(x)
    msgs <- bind_rows(msgs)
    x$messages <- msgs[, setdiff(names(msgs), c("severity"))]

    ## some fixes before validation
    for (tm in c("home", "visiting")) {
        ## find rows where the lineup changes on the line previous to the actual substitution code
        lup <- function(i) apply(apply(x$plays[i, paste0(tm, "_p", pseq)], 1, sort), 2, paste, collapse = "|")
        idx <- which(x$plays$substitution & x$plays$team == x$plays[[paste0(tm, "_team")]])
        idx <- idx[idx > 3]
        thiscols <- paste0(tm, c("_setter_position", paste0("_p", pseq), paste0("_player_id", pseq)))
        for (j in idx) {
            if (lup(j - 1) == lup(j) & lup(j - 1) != lup(j - 2)) {
                x$plays[j - 1L, thiscols] <- x$plays[j - 2L, thiscols]
            }
        }
    }

    ## apply additional validation
    if (extra_validation > 0) {
        moreval <- validate_dv(x, validation_level = extra_validation, options = validation_options, file_type = file_type)
        if (!is.null(moreval) && nrow(moreval) > 0) x$messages <- bind_rows(x$messages, moreval)
    }
    if (is.null(x$messages) || ncol(x$messages) < 1) x$messages <- tibble(file_line_number = integer(), video_time = integer(), message = character(), file_line = character())
    if (nrow(x$messages) > 0) {
        x$messages$file_line_number <- as.integer(x$messages$file_line_number)
        x$messages$video_time <- as.integer(x$messages$video_time)
        x$messages <- x$messages[order(x$messages$file_line_number, na.last = FALSE), ]
        row.names(x$messages) <- NULL
    }
    x
}

## convert skill rows of the vsm exchanges data.frame to proper codes
vsm_row2code <- function(x, data_type, style) {
    default_scouting_table <- dv_default_scouting_table(data_type = data_type, style = style)
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
    if (!is.null(default_scouting_table)) {
        for (ski in seq_len(nrow(default_scouting_table))) {
            temp$hit_type[temp$skill == default_scouting_table$skill[ski] & (is.na(temp$hit_type) | temp$hit_type == "~")] <- default_scouting_table$skill_type[ski]
            temp$effect[temp$skill == default_scouting_table$skill[ski] & (is.na(temp$effect) | temp$effect == "~")] <- default_scouting_table$evaluation_code[ski]
        }
    }
    out[idx] <- sub("~+$", "", paste0(temp$team, ## team code
                                      pnum, ## zero-padded player number
                                      temp$skill, ## skill
                                      temp$hit_type, ## skill_type (tempo)
                                      na2t(temp$effect), ## evaluation_code
                                      na2t(temp$combination, 2), ## attack combo code or setter call
                                      na2t(temp$target_attacker), ## target attacker
                                      na2t(temp$start_zone), na2t(temp$end_zone), na2t(temp$end_subzone), ## start, end, end subzone
                                      na2t(case_when((is.na(temp$skill_type) | temp$skill_type == "~") & temp$skill == "A" ~ "H", TRUE ~ temp$skill_type)), ## skill_subtype
                                      na2t(temp$players), ## num_players
                                      na2t(temp$special), na2t(temp$custom, 5)))
    out
}
