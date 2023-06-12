## functions to convert from (original) long text back to short codes
h_skill_code <- function(x) {
    ## remap "Save" skills first, which are somewhat ambiguous
    x %>% mutate(skill = case_when(.data$skill == "Save" ~ case_when(
                                                      ## save after oppo team touch is either a dig or a freeball dig
                                                      lag(.data$team) != .data$team & lag(.data$skill) %in% c("Ballover", "Freeball") ~ "Freeball", ## freeball dig
                                                      lag(.data$team) != .data$team ~ "Dig",

                                                      ## save after our team touch, and followed by our team touch = Set
                                                      lag(.data$team) == .data$team & lead(.data$team) == .data$team ~ "Set",

                                                      ## save after our team touch, and followed by other team touch = Freeball over
                                                      lag(.data$team) == .data$team & lead(.data$team) != .data$team ~ "Freeball",

                                                      ## save after our team touch, with no following touch (end of rally)
                                                      lag(.data$team, 2) == .data$team & lag(.data$team) == .data$team ~ "Freeball", ## our third touch, treat as freeball over
                                                      lag(.data$team) == .data$team ~ "Set", ## our second touch, treat as Set
                                                      TRUE ~ "Dig"),
                                TRUE ~ .data$skill),
                 sc = case_when(.data$skill == "Serve" ~ "S",
                                .data$skill == "Receive" ~ "R",
                                .data$skill == "Set" ~ "E",
                                .data$skill == "Attack" ~ "A",
                                .data$skill == "Freeball" ~ "F",
                                .data$skill == "Dig" ~ "D",
                                .data$skill == "Block" ~ "B",
                                .data$skill == "Down Ball" ~ "A", ## attack
                                .data$skill == "Cover" ~ "D", ## dig
                                .data$skill == "Ballover" ~ "F")) %>% ## freeball
    pull(.data$sc)
}

## skill here is our translated skill types, so Reception not Receive
h_skill_type_code <- function(x) {
    case_when(x$skill == "Serve" & x$serve_type == "Jump Spin" ~ "Q",
              x$skill == "Serve" & x$serve_type == "Jump Float" ~ "M",
              x$skill == "Serve" & x$serve_type == "Hybrid" ~ "N",
              x$skill == "Serve" & x$serve_type == "Float Near" ~ "T",
              x$skill == "Serve" & x$serve_type == "Float Far" ~ "H",
              x$skill == "Serve" & x$serve_type == "Underhand" ~ "U",
              x$skill == "Reception" & x$serve_type == "Jump Spin" ~ "Q",
              x$skill == "Reception" & x$serve_type == "Jump Float" ~ "M",
              x$skill == "Reception" & x$serve_type == "Hybrid" ~ "N",
              x$skill == "Reception" & x$serve_type == "Float Near" ~ "T",
              x$skill == "Reception" & x$serve_type == "Float Far" ~ "H",
              x$skill == "Reception" & x$serve_type == "Underhand" ~ "U",
              x$skill == "Set" ~ "H",
              x$skill == "Attack" ~ "H",
              x$skill == "Freeball" ~ "H",
              x$skill == "Dig" ~ "H",
              x$skill == "Block" ~ "H",
              x$skill == "Save" ~ "O", ## TODO check new logic
              x$skill == "Down Ball" ~ "O", ## attack
              x$skill == "Cover" ~ "O", ## dig
              x$skill == "Ballover" ~ "O") ## freeball
}

## attack combos
h_attack_code <- function(x, style = "default") {
    style <- match.arg(style, c("default", "volleymetrics", "usa"))
    on2 <- if (style == "usa") "C" else "Z"
    ## adjust for second-ball attacks, differentiating attack after block from attack after rec/dig/freeball
    x %>% mutate(attack_type = case_when(.data$skill == "Attack" & lag(.data$skill) %in% c("Reception", "Dig", "Freeball") & .data$team == lag(.data$team) ~ "Second Touch",
                                         .data$skill == "Attack" & lag(.data$skill) == "Block"  ~ "After Block",
                                         TRUE ~ .data$attack_type),
                 attack_code = case_when(.data$skill == "Attack" & lag(.data$skill) == "Freeball" & .data$team != lag(.data$team) ~ "XX", ## attack on opponent freeball
                                         .data$skill == "Attack" & (is.na(.data$attack_location) | is.na(.data$attack_type)) ~ "~~",
                                         .data$skill == "Attack" ~ paste0(case_when(.data$attack_type == "In System" ~ "X",
                                                                                    .data$attack_type == "Out of System" ~ "V",
                                                                                    .data$attack_type == "Second Touch" ~ on2,
                                                                                    .data$attack_type == "After Block" ~ "L",
                                                                                    .data$attack_type == "Quick" ~ "P",
                                                                                    TRUE ~ "~"),
                                                                          case_when(grepl("^Net Point [12345]", .data$attack_location) ~ sub("Net Point ", "", .data$attack_location),
                                                                                    TRUE ~ "~")
                                                                          )),
                 attack_code = if_else(grepl("~", .data$attack_code), NA_character_, .data$attack_code)) %>%
        pull(.data$attack_code)
}

h_skill_subtype_code <- function(x, style = "default") {
    style <- match.arg(style, c("default", "volleymetrics", "usa"))
    if (style == "usa") {
        hand_set <- "3"; bump_set <- "2"
    } else {
        hand_set <- "2"; bump_set <- "3"
    }
    case_when(x$skill == "Serve" ~ NA_character_,
              ## reception posture
              x$skill == "Reception" & x$receive_side == "Middle" ~ "M",
              x$skill == "Reception" & x$receive_side == "Overhead" ~ "O",
              x$skill == "Reception" & x$receive_side == "Left" ~ "L",
              x$skill == "Reception" & x$receive_side == "Right" ~ "R",
              x$skill == "Reception" & x$receive_side == "Low" ~ "W",
              x$skill == "Set" & x$set_subtype == "Hand" ~ hand_set,
              x$skill == "Set" & x$set_subtype == "Bump" ~ bump_set,
              x$skill == "Set" ~ NA_character_,
              ## attack type
              x$skill == "Attack" & x$attack_style == "Shot" ~ "P",
              x$skill == "Attack" & x$attack_style == "Pokey" ~ "T",
              x$skill == "Attack" ##& x$attack_style == "Hard"
                                  ~ "H") ## default to this (?)
}

h_num_players <- function(x, style = "default") {
    style <- match.arg(style, c("default", "volleymetrics", "usa"))
    drop_block <- if (style == "usa") 0L else 3L
    case_when(x$skill == "Attack" & x$attack_block_type == "Line" ~ 1L,
              x$skill == "Attack" & x$attack_block_type == "Angle" ~ 2L,
              x$skill == "Attack" & x$attack_block_type == "Peel" ~ drop_block,
              TRUE ~ NA_integer_)
}

h_evaluation_code <- function(x) {
    case_when(x$grade == "Error" ~ "=",
              x$grade == "Incomplete" ~ "/",
              x$grade == "Poor" ~ "-",
              x$grade == "Average" ~ "!",
              x$grade == "Positive" ~ "+",
              x$grade == "Perfect" ~ "#")
}

## convert the 16 x 16 grid of zones to our x, y coordinates
h_zone2dvxy <- function(z) (z - 1) * 3 / 16 + 19/32
## 16 x 16 grid of zones to standard zones
h_uv2zone <- function(u, v, as_for_serve = FALSE) {
    out <- rep(NA_integer_, length(u))
    idx <- !is.na(u) & !is.na(v)
    out[idx] <- dv_xy2zone(h_zone2dvxy(u[idx]), h_zone2dvxy(v[idx]), as_for_serve = as_for_serve)
    out
}

## 16 x 16 grid of zones to standard zone + subzone
h_uv2subzone <- function(u, v) {
    out <- tibble(end_zone = rep(NA_integer_, length(u)), end_subzone = rep(NA_character_, length(u)))
    idx <- !is.na(u)
    out[idx, ] <- dv_xy2subzone(h_zone2dvxy(u[idx]), h_zone2dvxy(v[idx]))
    out
}

## convert rows to full scout codes
h_row2code <- function(x, data_type, style) {
    default_scouting_table <- dv_default_scouting_table(data_type = data_type, style = style)
    if (nrow(x) < 1) return(character())
    na2t <- function(z, width = 1) {
        default <- stringr::str_flatten(rep("~", width))
        if (is.null(z)) default else case_when(!is.na(z) & nchar(z) > 0 ~ stringr::str_pad(substr(z, 1, width), width, side = "right", pad = "~"), TRUE ~ default)
    }
    ##na2t(c("dsjfldjldk", "a", "", NA_character_, 99), 5) ==> "dsjfl" "a~~~~" "~~~~~" "~~~~~" "99~~~"
    out <- x$code
    msgs <- c()
    ## only updating skill codes
    idx <- x$skill %in% c("S", "R", "E", "A", "B", "D", "F")
    if (!all(x$team[idx] %in% c("a", "*"))) {
        ## warning("at least one missing team code for a skill row")
        ## do this from the main function, so it can be captured in x$messages
        out[idx & !x$team %in% c("a", "*")] <- NA_character_
    }
    idx <- which(idx & x$team %in% c("a", "*"))
    if (length(idx) < 1) return(out)
    temp <- x[idx, ]
    pnum <- lead0(temp$player_number, na = "00")
    pnum[nchar(pnum) > 2 | grepl("^\\-", pnum)] <- "00" ## illegal numbers, treat as unknown
    ## defaults
    if (!is.null(default_scouting_table)) {
        for (ski in seq_len(nrow(default_scouting_table))) {
            temp$skill_type_code[temp$skill == default_scouting_table$skill[ski] & (is.na(temp$skill_type_code) | temp$skill_type_code == "~")] <- default_scouting_table$skill_type[ski]
            temp$evaluation_code[temp$skill == default_scouting_table$skill[ski] & (is.na(temp$evaluation_code) | temp$evaluation_code == "~")] <- default_scouting_table$evaluation_code[ski]
        }
    }
    out[idx] <- sub("~+$", "", paste0(temp$team, ## team code
                                      pnum, ## zero-padded player number
                                      temp$skill,
                                      temp$skill_type_code, ## skill_type (tempo)
                                      na2t(temp$evaluation_code),
                                      na2t(case_when(temp$skill == "A" ~ temp$attack_code, temp$skill == "E" ~ temp$set_code, TRUE ~ "~~"), 2), ## attack combo code or setter call
                                      na2t(temp$set_type), ## target attacker
                                      na2t(temp$start_zone), na2t(temp$end_zone), na2t(temp$end_subzone), ## start, end, end subzone
                                      na2t(case_when((is.na(temp$skill_subtype_code) | temp$skill_subtype_code == "~") & temp$skill == "A" ~ "H", TRUE ~ temp$skill_subtype_code)),
                                      na2t(temp$num_players_numeric),
                                      na2t(temp$special_code), na2t(temp$custom_code, 5)))
    out
}

dv_read_hxml <- function(filename, insert_technical_timeouts = TRUE, do_transliterate = FALSE, skill_evaluation_decode = "volleymetrics", extra_validation = 2, validation_options=list(), verbose = FALSE, ...) {
    ##  do_warn=FALSE, surname_case="asis", custom_code_parser, metadata_only=FALSE, edited_meta
    if (is.function(skill_evaluation_decode)) stop("providing a function to skill_evaluation_decode is not supported for xml files")
    skill_evaluation_decode <- match.arg(skill_evaluation_decode, c("default", "german", "guess", "volleymetrics"))
    if (skill_evaluation_decode %in% c("default", "guess")) {
        ## as of March 2023, treat "default" as the default for the file type, so for hxml that's vm
        skill_evaluation_decode <- "volleymetrics"
    }
    ## check for beach vs indoor
    chk <- readLines(filename, n = 200L, warn = FALSE)
    if (any(grepl("Rotation", chk, fixed = TRUE))) stop("this appears to be an indoor file - not yet supported. Please contact the package authors or submit an issue via <", utils::packageDescription("datavolley")$BugReports, ">")

    xml <- read_xml(filename)
    ## find the instances of interest to us
    alli <- xml_find_all(xml, "ALL_INSTANCES/instance[.//code/text()[contains(.,'Serve') or contains(.,'Receive') or contains(.,'Set') or contains(.,'Attack') or contains(.,'Block') or contains(.,'Dig') or contains(.,'Ballover') or contains(.,'Cover') or contains(.,'Save') or contains(.,'Rally')]]")

    if (length(alli) < 1) stop("xml file has unexpected format")

    ## pull out the id, start, end, and code elements of each instance
    i1 <- xml_find_all(alli, "(id|start|end|code)")
    i1 <- tibble(nm = xml_name(i1), val = xml_text(i1))
    ## convert to wide format, one row per id
    i1 <- tibble(id = i1$val[i1$nm == "id"],
                 start = i1$val[i1$nm == "start"],
                 end = i1$val[i1$nm == "end"],
                 code = i1$val[i1$nm == "code"])

    i2 <- xml_find_all(alli, "(id|label/group)") ## all id and label group elements
    i2 <- tibble(nm = xml_name(i2), val = xml_text(i2))
    i3 <- xml_find_all(alli, "(id|label/text)") ## all id and label text elements
    i3 <- tibble(nm = xml_name(i3), val = xml_text(i3))
    if (isTRUE(do_transliterate)) i3$val <- stri_trans_general(i3$val, "latin-ascii")
    i3$val <- str_trim(i3$val)
    ## check for indoor
    idx <- grep("player name$", i2$val, ignore.case = TRUE)
    if (length(unique(i3$val[idx])) > 4) stop("this appears to be an indoor file - not yet supported. Please contact the package authors or submit an issue via <", utils::packageDescription("datavolley")$BugReports, ">")

    ## find the "id"'s in i2 and i3, each of these is the start of a row group corresponding to a single touch
    i2i <- c(which(i2$nm == "id"), nrow(i2) + 1)
    i3i <- c(which(i3$nm == "id"), nrow(i3) + 1)
    if (!isTRUE(all.equal(i2i, i3i))) stop("mismatch in label group and text")

    rally_ids <- i1$id[which(i1$code == "Rally")]
    i23 <- bind_rows(lapply(head(seq_along(i2i), -1), function(j) {
        idx <- (i2i[j] + 1) : (i2i[j + 1] - 1) ## rows in this group
        thisid <- i2$val[i2i[j]]
        ## 'rally' codes can have repeated columns which causes all manner of problems, and we don't need anything from these anyway
        if (##as.integer(
            thisid##)
            %in% rally_ids) c(id = thisid) else c(id = thisid, setNames(i3$val[idx], i2$val[idx])) ## the id then all the other label text elements, each named by their corresponding label group
    }))
    msgs <- list()
    px <- dplyr::left_join(i1, i23, by = "id")
    px <- mutate(px, point_id = cumsum(!is.na(.data$code) & .data$code == "Rally"),
                 ## but each Rally code should be the end of the rally, not the start of the next one
                 point_id = case_when(.data$code == "Rally" ~ lag(.data$point_id), TRUE ~ .data$point_id),
                 Set = case_when(.data$code == "Rally" ~ lag(.data$Set), TRUE ~ .data$Set))

    ## check some columns
    if (!"YYYY-MM-DD" %in% names(px)) {
        ## file is missing match date, can we get it from the file name?
        temp <- tryCatch(suppressWarnings(lubridate::ymd(sub("^&", "", basename(filename)))), error = function(e) NA)
        if (!is.na(temp) && temp > as.Date("1970-01-01") && temp < (Sys.Date() + 10L)) {
            ## use date from filename but warn
            msgs <- collect_messages(msgs, msg_text = paste0("File is missing match date column, using date '", format(temp, "%Y-%m-%d"), "' from file name"), severity = 3)
            px$`YYYY-MM-DD` <- temp
        } else {
            msgs <- collect_messages(msgs, msg_text = "File is missing match date column", severity = 2)
            px$`YYYY-MM-DD` <- NA_character_
        }
    }
    chk <- setdiff(c("Team Name", "Player Name", "Player Jersey", "Skill", "Grade", "Set"), names(px))
    if (length(chk) > 0) stop("missing column", if (length(chk) > 1) "s", ": ", paste(chk, collapse = ", "))
    ## "Rally Won" is dealt with below
    ## expect two score columns plus "Score Status", "Score Phase", "Score Difference"
    chk <- setdiff(names(px)[grepl("^Score[[:space:]]", names(px))], c("Score Status", "Score Phase", "Score Difference"))
    if (length(chk) < 2) stop("missing one or both team score columns")
    ## expected but not critical
    req <- c("Serve Type", "Zone X", "Zone Y", ##"From Zone X", "From Zone Y"
             "To Zone X", "To Zone Y", "Tournament - Location", "Tournament - Event", "Tournament - Phase",
             "Conditions - Weather", "Conditions - Wind", "Conditions - Light",
             "Attack Location", "Attack Style", "Receive Side", "Attack Block Type", "Attack Type", "Set Type")
    for (cl in req) if (!cl %in% names(px)) px[[cl]] <- NA_character_

    ## teams
    tms <- unique(na.omit(px$`Team Name`))
    if (length(tms) != 2 || any(is.na(tms))) stop("could not extract team names")
    ## home team is listed first in the column names
    t1idx <- which(names(px) == paste0("Score ", tms[1]))
    t2idx <- which(names(px) == paste0("Score ", tms[2]))
    if (length(t1idx) == 1 && length(t2idx) == 1 && t2idx < t1idx) tms <- rev(tms)

    names(px)[names(px) == paste0("Score ", tms[1])] <- "home_score_start_of_point"
    names(px)[names(px) == paste0("Score ", tms[2])] <- "visiting_score_start_of_point"
    names(px) <- tolower(gsub("[[:space:]\\-]+", "_", names(px)))

    px$team <- case_when(px$team_name == tms[1] ~ "*", px$team_name == tms[2] ~ "a")
    px <- dplyr::rename(px, set_number = "set", player_number = "player_jersey", h_code = "code") %>%
        mutate(across(all_of(c("home_score_start_of_point", "visiting_score_start_of_point", "set_number", "player_number", "zone_x", "zone_y", ##"from_zone_x", "from_zone_y",
                               "to_zone_x", "to_zone_y")), as.integer))
    ## fill in gaps in score
    px <- mutate(px,
                 home_score_start_of_point = if_else(.data$set_number == lag(.data$set_number) & is.na(.data$home_score_start_of_point),
                                                     lag(.data$home_score_start_of_point), .data$home_score_start_of_point),
                 visiting_score_start_of_point = if_else(.data$set_number == lag(.data$set_number) & is.na(.data$visiting_score_start_of_point),
                                                         lag(.data$visiting_score_start_of_point), .data$visiting_score_start_of_point))

    ## change team names that only have 3 characters
    players <- px %>% dplyr::filter(!is.na(.data$team)) %>% dplyr::select("team", "player_name", number = "player_number") %>% distinct %>% mutate(lastname = sub(",.*", "", .data$player_name), firstname = case_when(grepl(",", .data$player_name) ~ sub(".*,[[:space:]]*", "", .data$player_name), TRUE ~ ""), number = as.integer(.data$number)) %>%
        dplyr::arrange(.data$team, .data$number)
    if (nchar(tms[1]) == 3) tms[1] <- paste(tms[1], paste0(substr(players$lastname[players$team == "*"], 1, 3), collapse = "/"))
    if (nchar(tms[2]) == 3) tms[2] <- paste(tms[2], paste0(substr(players$lastname[players$team == "a"], 1, 3), collapse = "/"))

    if (!"rally_won" %in% names(px)) {
        temp <- px %>% group_by(.data$point_id) %>% slice(1L) %>% group_by(.data$set_number) %>%
            mutate(point_won_by = case_when(lead(.data$point_id) == (.data$point_id + 1L) & lead(.data$home_score_start_of_point) > .data$home_score_start_of_point ~ "*",
                                            is.na(lead(.data$point_id)) & .data$home_score_start_of_point > .data$visiting_score_start_of_point ~ "*", ## end of set
                                            lead(.data$point_id) == (.data$point_id + 1L) & lead(.data$visiting_score_start_of_point) > .data$visiting_score_start_of_point ~ "a",
                                            is.na(lead(.data$point_id)) & .data$visiting_score_start_of_point > .data$home_score_start_of_point ~ "a")) %>% ## end of set
            ungroup
        if (any(is.na(temp$point_won_by))) stop("'Rally Won' is missing from the source file")
    } else {
        temp <- px %>% group_by(.data$point_id) %>% slice(1L) %>% ungroup %>%
            mutate(point_won_by = case_when((.data$team == "*" & .data$rally_won == "Won") | (.data$team == "a" & .data$rally_won == "Lost") ~ "*",
            (.data$team == "a" & .data$rally_won == "Won") | (.data$team == "*" & .data$rally_won == "Lost") ~ "a"))
    }
    temp <- temp %>% mutate(home_team_score = case_when(.data$point_won_by == "*" ~ .data$home_score_start_of_point + 1L,
                                                        TRUE ~ .data$home_score_start_of_point),
                            visiting_team_score = case_when(.data$point_won_by == "a" ~ .data$visiting_score_start_of_point + 1L,
                                                            TRUE ~ .data$visiting_score_start_of_point))
    px <- left_join(px, temp %>% dplyr::select("point_id", "point_won_by", "home_team_score", "visiting_team_score"), by = "point_id")
    ## TODO check any missing scores

    x <- list(raw = paste0("<instance>", str_trim(strsplit(gsub(">\n[[:space:]]*<", "><", as.character(xml)), "<instance>")[[1]])))
    raw_id <- str_match(x$raw, "<id>([^<]+)</id>")[, 2]
    idlnum <- setNames(as.list(seq_along(raw_id)), raw_id) ## line numbers, named by their corresponding _id
    ## there should not be duplicate IDs, but they will cause problems so make sure
    idlnum <- idlnum[!is.na(raw_id) & !raw_id %in% raw_id[duplicated(raw_id)]]
    px_lnum <- function(idx) {
        if (is.logical(idx)) idx <- which(idx)
        ## raw line numbers associated with the rows idx in px
        if (length(idx) < 1) return(integer())
        ids <- px$id[idx]
        out <- rep(NA_integer_, length(idx))
        oidx <- ids %in% names(idlnum)
        out[oidx] <- idlnum[ids[oidx]]
        unlist(out)
    }

    temp <- px %>% dplyr::filter(!is.na(.data$team_name), !is.na(.data$player_name)) %>% group_by(.data$team_name) %>% dplyr::summarize(n_players = dplyr::n_distinct(.data$player_name))
    file_type <- NA
    if (nrow(temp) == 2) {
        if (isTRUE(all(temp$n_players == 2))) {
            file_type <- "beach"
        } else if (all(temp$n_players >= 6)) {
            file_type <- "indoor"
        }
    }
    if (is.na(file_type)) stop("could not determine file type")
    pseq <- seq_len(if (file_type == "beach") 2 else 6)
    x$file_meta <- dv_create_file_meta(generator_day = as.POSIXct(NA), generator_idp = "DVW", generator_prg = "Hudl",
                                       generator_release = "", generator_version = NA_character_, generator_name = "", file_type = file_type)
    first_unique <- function(x, what, warn = TRUE) {
        x <- unique(na.omit(x))
        if (length(x) > 1) {
            msgs <<- collect_messages(msgs, msg_text = paste0("Multiple '", what, "' values detected, using the first one ('", x[1], "')"), severity = 2)
            x[1]
        } else if (length(x) < 1) {
            msgs <<- collect_messages(msgs, msg_text = paste0("No '", what, "' values found"), severity = 2)
            NA_character_
        } else {
            x
        }
    }
    conds <- px %>% dplyr::filter(!is.na(.data$set_number)) %>% group_by(.data$set_number) %>%
        dplyr::summarize(weather = paste0(na.omit(unique(.data$conditions_weather)), collapse = ", "),
                         wind = paste0(na.omit(unique(.data$conditions_wind)), collapse = ", "),
                         light = paste0(na.omit(unique(.data$conditions_light)), collapse = ", ")) %>%
        group_by(.data$weather, .data$wind, .data$light) %>%
        dplyr::summarize(conditions = paste0("Set", if (dplyr::n() > 1) "s", " ", paste0(.data$set_number, collapse = ", "), " -",
                                             if (!is.na(.data$weather[1]) && nchar(.data$weather[1]) > 0) paste0(" Weather: ", .data$weather[1], "."),
                                             if (!is.na(.data$wind[1]) && nchar(.data$wind[1]) > 0) paste0(" Wind: ", .data$wind[1], "."),
                                             if (!is.na(.data$light[1]) && nchar(.data$light[1]) > 0) paste0(" Light: ", .data$light[1]))) %>%
        ungroup

    mx <- list(match = dv_create_meta_match(date = lubridate::ymd(first_unique(px$yyyy_mm_dd, "match date", warn = FALSE)),
                                            regulation = if (file_type == "indoor") "indoor rally point" else if (file_type == "beach") "beach rally point" else stop("unexpected game type: ", file_type),
                                            league = first_unique(px$tournament_event, "Tournament - Event"),
                                            phase = first_unique(px$tournament_phase, "Tournament - Phase"),
                                            zones_or_cones = "Z"),
               more = dv_create_meta_more(city = first_unique(px$tournament_location, "Tournament - Location")),
               comments = dv_create_meta_comments(summary = paste0(conds$conditions, collapse = " | "))
               )
    set_scores <- px %>% dplyr::filter(!is.na(.data$set_number)) %>% group_by(.data$set_number) %>% dplyr::summarize(home_team_score = max(.data$home_team_score, na.rm = TRUE), visiting_team_score = max(.data$visiting_team_score, na.rm = TRUE))

    mx$result <- dv_create_meta_result(home_team_scores = set_scores$home_team_score, visiting_team_scores = set_scores$visiting_team_score)
    ## will be further populated by dv_update_meta below

    tx <- dv_create_meta_teams(team_ids = tms, teams = tms)
    if (has_dvmsg(tx)) msgs <- collect_messages(msgs, get_dvmsg(tx), xraw = x$raw)
    mx$teams <- clear_dvmsg(tx)

    mx$players_h <- dv_create_meta_players(players %>% dplyr::filter(.data$team == "*"))
    mx$players_v <- dv_create_meta_players(players %>% dplyr::filter(.data$team == "a"))

    ## get player_id into px and change player_name to that in the players meta
    temp <- bind_rows(mx$players_h %>% mutate(team = "*"), mx$players_v %>% mutate(team = "a")) %>%
        dplyr::select("team", player_number = "number", "player_id", player_name = "name")
    px <- px %>% dplyr::select(-"player_name") %>% left_join(temp, by = c("team", "player_number"))
    ## TODO check that starting positions are updated by dv_update_meta

    x$meta <- dv_create_meta(match = mx$match, more = mx$more, comments = mx$comments, result = mx$result, teams = mx$teams, players_h = mx$players_h, players_v = mx$players_v,
                             video = dv_create_meta_video(),
                             ##attacks = dv_default_attack_combos(data_type = file_type, style = skill_evaluation_decode),
                             ##setter_calls = dv_default_setter_calls(data_type = file_type, style = skill_evaluation_decode),
                             ##winning_symbols = dv_default_winning_symbols(style = skill_evaluation_decode),
                             data_type = file_type, style = skill_evaluation_decode)
    x$meta$filename <- filename

    px$skill_code <- h_skill_code(px)
    px <- mutate(px, skill_original = .data$skill, skill = case_when(.data$skill_code == "S" ~ "Serve",
                                                                     .data$skill_code == "R" ~ "Reception",
                                                                     .data$skill_code == "E" ~ "Set",
                                                                     .data$skill_code == "A" ~ "Attack",
                                                                     .data$skill_code == "B" ~ "Block",
                                                                     .data$skill_code == "D" ~ "Dig",
                                                                     .data$skill_code == "F" ~ "Freeball"))

    ## zone_x, zone_y is the location of the skill (i.e. start), to_zone_* is the end location (from_zone_* is the location of the preceding skill, don't need this)
    px <- mutate(px, zone_y = case_when(.data$skill == "Serve" & is.na(.data$zone_y) ~ 0L, TRUE ~ .data$zone_y),
                 ## TODO check this one to_zone_y = case_when(.data$skill == "Serve" & is.na(.data$to_zone_y) ~ 0L, TRUE ~ .data$to_zone_y))
                 start_zone = case_when(.data$skill %in% c("Serve", "Reception") ~ h_uv2zone(.data$zone_x, .data$zone_y, as_for_serve = TRUE),
                                        TRUE ~ h_uv2zone(.data$zone_x, .data$zone_y, as_for_serve = FALSE)),
                 start_subzone = NA_character_)
    px <- bind_cols(px, h_uv2subzone(px$to_zone_x, px$to_zone_y))
    ## set and block need start subzone
    idx <- px$skill %in% c("Block", "Set")
    if (any(idx)) px$start_subzone[idx] <- h_uv2subzone(px$zone_x[idx], px$zone_y[idx])$end_subzone

    ## fix zones
    pairs <- ((px$skill == "Reception" & lag(px$skill) == "Serve") |
              (px$skill == "Dig" & lag(px$skill) == "Attack")) & (lag(px$team) != px$team)
    px <- mutate(px, start_zone = case_when(pairs ~ lag(.data$start_zone), TRUE ~ .data$start_zone),
                 start_zone = case_when(pairs ~ lag(.data$start_zone), TRUE ~ .data$start_zone),
                 end_zone = case_when(pairs ~ lag(.data$end_zone), TRUE ~ .data$end_zone),
                 end_subzone = case_when(pairs ~ lag(.data$end_subzone), TRUE ~ .data$end_subzone)##,
                 ##start_coordinate_x = case_when(pairs ~ lag(.data$start_coordinate_x), TRUE ~ .data$start_coordinate_x),
                 ##start_coordinate_y = case_when(pairs ~ lag(.data$start_coordinate_y), TRUE ~ .data$start_coordinate_y),
                 ##mid_coordinate_x = case_when(pairs ~ lag(.data$mid_coordinate_x), TRUE ~ .data$mid_coordinate_x),
                 ##mid_coordinate_y = case_when(pairs ~ lag(.data$mid_coordinate_y), TRUE ~ .data$mid_coordinate_y),
                 ##end_coordinate_x = case_when(pairs ~ lag(.data$end_coordinate_x), TRUE ~ .data$end_coordinate_x),
                 ##end_coordinate_y = case_when(pairs ~ lag(.data$end_coordinate_y), TRUE ~ .data$end_coordinate_y)
                 )

    ## start zone/subzone have been used for the set and block location, but dvw convention is to put these into end zone/subzone
    px <- mutate(px, end_zone = case_when(.data$skill %in% c("Block", "Set") ~ .data$start_zone, TRUE ~ .data$end_zone),
                 end_subzone = case_when(.data$skill %in% c("Block", "Set") ~ .data$start_subzone, TRUE ~ .data$end_subzone),
                 start_zone = case_when(.data$skill %in% c("Block", "Set") ~ NA_integer_, TRUE ~ .data$start_zone),
                 start_subzone = case_when(.data$skill %in% c("Block", "Set") ~ NA_character_, TRUE ~ .data$start_subzone))

    ## convert x,y coords back to single-index coords
    ##px$start_coordinate <- dv_xy2index(px$start_coordinate_x, px$start_coordinate_y)
    ##px$mid_coordinate <- dv_xy2index(px$mid_coordinate_x, px$mid_coordinate_y)
    ##px$end_coordinate <- dv_xy2index(px$end_coordinate_x, px$end_coordinate_y)

    if ("set_type" %in% names(px)) px <- dplyr::rename(px, set_subtype = "set_type")

    ## combo codes
    px$attack_code <- h_attack_code(px, style = skill_evaluation_decode)
    px <- left_join(px, x$meta$attacks %>% dplyr::select(attack_code = "code", "set_type", skill_type_code = "type", attack_description = "description"), by = "attack_code")
    px$set_type[px$set_type %in% "-"] <- NA_character_

    ## no setter calls for beach
    if (file_type == "beach") {
        px$set_code <- px$set_type <- px$set_description <- NA_character_
    } else {
        stop("setter calls not coded yet")
    }
    ## patch in the remaining skill types
    idx <- is.na(px$skill_type_code) | px$skill_type_code == "~"
    if (any(idx)) px$skill_type_code[idx] <- h_skill_type_code(px[idx, ])
    ## enforce skill_type (tempo) code of a set, block, or dig to match that of the corresponding attack
    px <- mutate(px, skill_type_code = case_when(.data$skill == "Set" & lead(.data$skill) == "Attack" & .data$team == lead(.data$team) ~ lead(.data$skill_type_code),
                                                 .data$skill == "Block" & lag(.data$skill) == "Attack" & .data$team != lag(.data$team) ~ lag(.data$skill_type_code),
                                                 .data$skill == "Dig" & lag(.data$skill) == "Attack" & .data$team != lag(.data$team) ~ lag(.data$skill_type_code),
                                                 .data$skill == "Dig" & lag(.data$skill) == "Block" & lag(.data$skill, 2) == "Attack" & .data$team != lag(.data$team, 2) ~ lag(.data$skill_type_code, 2),
                                                 TRUE ~ .data$skill_type_code))
    px$skill_type <- dv_decode_skill_type(px$skill, px$skill_type_code, data_type = file_type, style = skill_evaluation_decode)

    px$evaluation_code <- h_evaluation_code(px)
    ## other adjustments
    if (skill_evaluation_decode %eq% "volleymetrics") {
        px <- mutate(px, evaluation_code = case_when(.data$skill == "Attack" & lead(.data$skill) == "Block" & lead(.data$evaluation_code) == "/" ~ "!",
                                                     TRUE ~ .data$evaluation_code))
    }
    px$evaluation <- dv_decode_evaluation(px$skill, px$evaluation_code, data_type = file_type, style = skill_evaluation_decode)

    px$skill_subtype_code <- h_skill_subtype_code(px, style = skill_evaluation_decode)
    px$skill_subtype <- dv_decode_skill_subtype(px$skill, px$skill_subtype_code, px$evaluation, data_type = file_type, style = skill_evaluation_decode)

    px$num_players_numeric <- h_num_players(px, style = skill_evaluation_decode)
    px$num_players <- dv_decode_num_players(px$skill, px$num_players_numeric, data_type = file_type, style = skill_evaluation_decode)

    req <- list(special_code = NA_character_, custom_code = NA_character_,
                start_coordinate = NA_integer_, mid_coordinate = NA_integer_, end_coordinate = NA_integer_,
                start_coordinate_x = NA_real_, mid_coordinate_x = NA_real_, end_coordinate_x = NA_real_,
                start_coordinate_y = NA_real_, mid_coordinate_y = NA_real_, end_coordinate_y = NA_real_,
                start_sub_zone = NA_character_)
    for (rc in names(req)) px[[rc]] <- req[[rc]]

    ## serving team
    temp <- px %>% dplyr::filter(.data$skill == "Serve") %>% distinct(.data$point_id, .data$team) %>%
        dplyr::add_count(.data$point_id) %>% dplyr::filter(.data$n == 1)
    px <- left_join(px, temp %>% dplyr::select("point_id", serving_team = "team"), by = "point_id")
    ## TODO check for missing serving team, fill gaps

    ## add cols to px
    ## point
    px <- mutate(px, code = case_when(.data$h_code == "Rally" ~ paste0(.data$point_won_by, "p", lead0(.data$home_team_score, na = "99"), ":", lead0(.data$visiting_team_score, na = "99"))),
                 team = case_when(.data$h_code == "Rally" ~ .data$point_won_by, TRUE ~ .data$team), point = .data$h_code %eq% "Rally")
    ## timeout
    px$timeout <- FALSE ## TODO, do we even have timeout info?
    ## lineups
    if (file_type == "beach") {
        ## beach: each set, serving team starts in with first-serving player in position 1, receiving team with that player in 2
        lups <- px %>% dplyr::filter(.data$skill == "Serve") %>% group_by(.data$set_number) %>% slice(1L) %>% ungroup %>%
            mutate(home_p1 = case_when(.data$serving_team == "*" ~ .data$player_number,
                                       .data$serving_team == "a" ~ 3L - .data$player_number), ## only works with 2 players numbered 1 and 2
                   visiting_p1 = case_when(.data$serving_team == "a" ~ .data$player_number,
                                           .data$serving_team == "*" ~ 3L - .data$player_number),
                   home_p2 = 3L - .data$home_p1, visiting_p2 = 3L - .data$visiting_p1) %>%
            dplyr::select("point_id", "home_p1", "home_p2", "visiting_p1", "visiting_p2")
        ## populate first point of each set in px
        px <- px %>% left_join(lups, by = "point_id") %>%
            ## arbitrarily call player 1 the setter on each team
            mutate(home_setter_position = case_when(.data$home_p1 == 1L ~ 1L, .data$home_p2 == 1L ~ 2L),
                   visiting_setter_position = case_when(.data$visiting_p1 == 1L ~ 1L, .data$visiting_p2 == 1L ~ 2L))

        temp <- px %>% dplyr::distinct(.data$point_id, .data$serving_team, .data$point_won_by, .data$home_setter_position, .data$visiting_setter_position,
                                       .data$home_p1, .data$home_p2, .data$visiting_p1, .data$visiting_p2)
        hsp <- temp$home_setter_position
        vsp <- temp$visiting_setter_position
        hlup <- matrix(nrow = nrow(temp), ncol = 2)
        hlup[1, ] <- as.numeric(temp[1, c("home_p1", "home_p2")])
        vlup <- matrix(nrow = nrow(temp), ncol = 2)
        vlup[1, ] <- as.numeric(temp[1, c("visiting_p1", "visiting_p2")])
        for (i in seq_len(nrow(temp))[-1]) {
            rh <- rv <- FALSE
            if (is.na(hsp[i])) {
                if (isTRUE(temp$serving_team[i - 1] == "a" & temp$point_won_by[i - 1] == "*")) {
                    hsp[i] <- rotpos(hsp[i - 1], n = 2L)
                    rh <- TRUE
                } else {
                    hsp[i] <- hsp[i - 1]
                }
                if (isTRUE(temp$serving_team[i - 1] == "*" & temp$point_won_by[i - 1] == "a")) {
                    vsp[i] <- rotpos(vsp[i - 1], n = 2L)
                    rv <- TRUE
                } else {
                    vsp[i] <- vsp[i - 1]
                }
            }
            hlup[i, ] <- if (rh) rot_lup(hlup[i - 1, ], n = 2L) else hlup[i - 1, ]
            vlup[i, ] <- if (rv) rot_lup(vlup[i - 1, ], n = 2L) else vlup[i - 1, ]
        }
        temp$home_setter_position <- hsp
        temp$visiting_setter_position <- vsp
        temp[, c("home_p1", "home_p2")] <- hlup
        temp[, c("visiting_p1", "visiting_p2")] <- vlup
        px <- px %>% dplyr::select(-"home_setter_position", -"home_p1", -"home_p2", -"visiting_setter_position", -"visiting_p1", -"visiting_p2") %>%
            left_join(temp %>% dplyr::select(-"point_won_by", -"serving_team"), by = "point_id")
    } else {
        stop("not coded")
    }
    ## home_player_id1 etc
    for (ii in pseq) {
        temp <- x$meta$players_h %>% dplyr::select("player_id", "number")
        temp <- setNames(temp, c(paste0(c("home_player_id", "home_p"), ii)))
        px <- left_join(px, temp, by = paste0("home_p", ii))
        temp <- x$meta$players_v %>% dplyr::select("player_id", "number")
        temp <- setNames(temp, c(paste0(c("visiting_player_id", "visiting_p"), ii)))
        px <- left_join(px, temp, by = paste0("visiting_p", ii))
    }

    if (file_type == "beach") {
        px$substitution <- FALSE
    } else {
        ## for non-beach: add substitution, player_in and player_out columns
        stop("substitutions not coded")
    }

    ## full scout code
    px$code <- h_row2code(px %>% dplyr::select(-"skill", skill = "skill_code"), data_type = file_type, style = skill_evaluation_decode)

    ## time and video_time
    ## times in the xml are presumably clip start and end times around the skill in question
    ## take start as initial time for serve and reception, and (start + end) / 2 for other skills, then tweak
    px <- mutate(px, start = as.numeric(.data$start), end = as.numeric(.data$end),
                 start = case_when(.data$skill == "Serve" & is.na(.data$start) & lead(.data$skill) == "Reception" ~ lead(.data$start),
                                   TRUE ~ .data$start),
                 video_time = case_when(.data$skill %in% c("Serve", "Reception") ~ round(.data$start),
                                        TRUE ~ round((.data$start + .data$end) / 2)),
                 ## noe adjust
                 video_time = case_when(.data$skill == "Reception" & lag(.data$skill) == "Serve" ~ lag(.data$video_time),
                                        .data$skill == "Block" & lag(.data$skill) == "Attack" ~ lag(.data$video_time),
                                        .data$skill == "Dig" & lag(.data$skill) == "Attack" & .data$video_time <= lag(.data$video_time) ~ lag(.data$video_time) + 1L,
                                        .data$skill == "Dig" & lag(.data$skill, 2) == "Attack" & .data$video_time <= lag(.data$video_time, 2) ~ lag(.data$video_time, 2) + 1L,
                                        .data$skill == "Set" & lead(.data$skill) == "Attack" & .data$video_time <= lead(.data$video_time) ~ lead(.data$video_time) - 1L,
                                        TRUE ~ .data$video_time),
                 time = as.POSIXct(NA))
    ## then just enforce non-decreasing video times on whatever we ended up with
    vt <- px$video_time
    for (i in seq_along(vt)[-1]) if (vt[i] < vt[i - 1]) vt[i] <- vt[i - 1]
    px$video_time <- as.integer(vt)

    ## columns to preserve when adding new rows to the dataframe
    keepcols <- c("point_id", "time", "video_time", "home_team_score", "visiting_team_score", "point_won_by", "set_number",
                  "home_setter_position", "visiting_setter_position", paste0("home_p", pseq), paste0("visiting_p", pseq),
                  paste0("home_player_id", pseq), paste0("visiting_player_id", pseq))
    ## expand rally codes, by set
    ## TODO check for any missing set_number, point_id entries
    temp <- list()
    for (si in sort(unique(na.omit(px$set_number)))) {
        thispx <- px %>% dplyr::filter(.data$set_number == si)
        pids <- unique(thispx$point_id)
        temp2 <- vector("list", length(pids))
        last_hts <- last_vts <- 0L
        last_hsp <- thispx$home_setter_position[1]
        last_vsp <- thispx$visiting_setter_position[1]
        for (pidi in seq_along(pids)) {
            this <- thispx[thispx$point_id == pids[pidi], ]
            ## suppress warnings here, any issues should be captured by later validation checks
            this <- dv_expand_rally_codes(this, last_home_setter_position = last_hsp, last_visiting_setter_position = last_vsp,
                                          last_home_team_score = last_hts, last_visiting_team_score = last_vts,
                                          keepcols = keepcols, meta = x$meta, rebuild_codes = FALSE, do_warn = FALSE)
            temp2[[pidi]] <- this
            last_hts <- tail(this$home_team_score, 1)
            last_vts <- tail(this$visiting_team_score, 1)
            last_hsp <- tail(this$home_setter_position, 1)
            last_vsp <- tail(this$visiting_setter_position, 1)
        }
        thispx <- bind_rows(temp2)
        ## insert >LUp codes at start of set
        hs <- tryCatch(unlist(thispx[1, paste0("home_p", pseq)])[thispx$home_setter_position[1]], error = function(e) 0L)
        vs <- tryCatch(unlist(thispx[1, paste0("visiting_p", pseq)])[thispx$visiting_setter_position[1]], error = function(e) 0L)
        lineup_codes <- paste0(c(paste0("*P", lead0(hs)), paste0("*z", thispx$home_setter_position[1]), paste0("aP", lead0(vs)), paste0("az", thispx$visiting_setter_position[1])), ">LUp")
        thispx <- bind_rows(thispx[rep(1, 4), keepcols] %>% mutate(code = lineup_codes, team = c("*", "*", "a", "a")), thispx)
        ## **Xset code at end
        temp[[si]] <- bind_rows(thispx, thispx[nrow(thispx), keepcols] %>% mutate(code = paste0("**", si, "set"), end_of_set = TRUE, point_won_by = NA_character_))
    }
    px <- bind_rows(temp) %>% mutate(end_of_set = case_when(is.na(.data$end_of_set) ~ FALSE, TRUE ~ .data$end_of_set),
                                     point = case_when(is.na(.data$point) ~ FALSE, TRUE ~ .data$point),
                                     timeout = case_when(is.na(.data$timeout) ~ FALSE, TRUE ~ .data$timeout),
                                     substitution = case_when(is.na(.data$substitution) ~ FALSE, TRUE ~ .data$substitution),
                                     match_id = x$meta$match_id)

    ## technical timeouts
    if (isTRUE(insert_technical_timeouts)) px <- dv_insert_technical_timeouts(px, data_type = file_type)

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

    ## team name and ID
    idx <- x$meta$teams$home_away_team %eq% "*"
    ht <- x$meta$teams$team[idx]
    ht_id <- x$meta$teams$team_id[idx]
    idx <- x$meta$teams$home_away_team %eq% "a"
    vt <- x$meta$teams$team[idx]
    vt_id <- x$meta$teams$team_id[idx]
    px <- mutate(px, home_team = ht, visiting_team = vt, home_team_id = ht_id, visiting_team_id = vt_id,
                 team_id = case_when(.data$team == "*" ~ ht_id, .data$team == "a" ~ vt_id),
                 team = case_when(.data$team == "*" ~ ht, .data$team == "a" ~ vt),
                 point_won_by = case_when(.data$point_won_by == "*" ~ ht, .data$point_won_by == "a" ~ vt),
                 serving_team = case_when(.data$serving_team == "*" ~ ht, .data$serving_team == "a" ~ vt))

    ## winning attacks, just for backwards compatibility
    ## A followed by D with "Error" evaluation, or A with "Winning attack" evaluation
    px <- mutate(px, winning_attack = .data$skill %eq% "Attack" & (.data$evaluation %eq% "Winning attack" | (lead(.data$skill %in% c("Dig", "Block")) & lead(.data$evaluation %eq% "Error"))))

    px$phase <- play_phase(px)
    ## and the DV point_phase and attack_phase
    px$point_phase <- dv_point_phase(px)
    px$attack_phase <- dv_attack_phase(px)

    ## use _ids to infer line numbers (though these are really only line numbers in x$raw, because the original file is a single line)
    tempid <- tibble(id = names(idlnum), file_line_number = unname(unlist(idlnum)))
    px <- left_join(px, tempid, by = "id")
    ## fill in gaps, because subs and TOs didn't have _ids attached so they won't have line numbers, and neither will green codes or point adjustments
    try({
        idx <- is.na(px$file_line_number)
        px$file_line_number[idx] <- round(approx(which(!idx), px$file_line_number[!idx], which(idx))$y)
    })
    ## these interpolated file line numbers won't be exact, but close enough to be (hopefully) useful


    ## some checks
    ## fix any wildly mismatched net zones (against zone x)
    idx <- grep("^[CZXVLP][12345]", px$attack_code)
    if (length(idx) > 0) {
        temp <- px[idx, c("file_line_number", "attack_code", "zone_x", "zone_y")] %>%
            ## we have 16 x-zones, but 5 net zones, so it's not a 1:1 correspondence. Expect e.g. net zone 1 to be x-zone 1:3, but allow one extra x-zone
            mutate(netz = substr(.data$attack_code, 2, 2), netz_ok = case_when(.data$netz == "1" ~ .data$zone_x < 5,
                                                                               .data$netz == "2" ~ .data$zone_x %in% 3:7,
                                                                               .data$netz == "3" ~ .data$zone_x %in% 7:10,
                                                                               .data$netz == "4" ~ .data$zone_x %in% 10:14,
                                                                               .data$netz == "5" ~ .data$zone_x > 12,
                                                                               TRUE ~ TRUE))
        if (!all(temp$netz_ok)) {
            temp <- mutate(temp, netz2 = case_when(.data$netz_ok ~ .data$netz,
                                                   .data$zone_x < 4 ~ "1",
                                                   .data$zone_x %in% 4:6 ~ "2",
                                                   .data$zone_x %in% 7:10 ~ "3",
                                                   .data$zone_x %in% 11:13 ~ "4",
                                                   .data$zone_x > 13 ~ "5",
                                                   TRUE ~ .data$netz))
            substr(temp$attack_code, 2, 2) <- temp$netz2
            px$attack_code[idx] <- temp$attack_code
            ##print(temp %>% dplyr::filter(.data$netz2 != .data$netz))
            ##stop(sum(!temp$netz_ok))
            temp <- temp %>% dplyr::filter(.data$netz2 != .data$netz)
            msgs <- collect_messages(msgs, msg_text = paste0("The scouted attack net zone (", temp$netz, ") does not match the start zone (", temp$zone_x, "), replacing with net zone ", temp$netz2), line_nums = temp$file_line_number, severity = 2, xraw = x$raw)
        }
    }

    idx <- x$skill %in% c("Serve", "Reception", "Set", "Attack", "Block", "Dig", "Freeball", "Timeout") & is.na(x$team)
    if (any(idx)) {
        msgs <- collect_messages(msgs, msg_text = "Skill code has missing or invalid team identifier", line_nums = px$file_line_number[idx], severity = 1, xraw = x$raw)
    }

    x$plays <- px %>% mutate(video_file_number = if (nrow(x$meta$video) > 0) 1L else NA_integer_, end_cone = NA_integer_) %>%
        dplyr::select("match_id", "point_id", "time", "video_file_number", "video_time", "code", "team", "player_number", "player_name", "player_id",
                      "skill", "skill_type", "evaluation_code", "evaluation", "attack_code", "attack_description", "set_code", "set_description", "set_type",
                      "start_zone", "end_zone", "end_subzone", "end_cone", "skill_subtype", "num_players", "num_players_numeric", "special_code", "timeout",
                      "end_of_set", "substitution", "point", "home_team_score", "visiting_team_score", "home_setter_position", "visiting_setter_position",
                      "custom_code", "file_line_number",
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
    ## apply additional validation
    if (extra_validation > 0) {
        moreval <- validate_dv(x, validation_level = extra_validation, options = validation_options, file_type = file_type)
        if (!is.null(moreval) && nrow(moreval) > 0) x$messages <- bind_rows(x$messages, moreval)
    }
    if (is.null(x$messages) || ncol(x$messages) < 1) x$messages <- tibble(file_line_number = integer(), video_time = numeric(), message = character(), file_line = character())
    if (nrow(x$messages) > 0) {
        x$messages$file_line_number <- as.integer(x$messages$file_line_number)
        x$messages$video_time <- as.integer(x$messages$video_time)
        x$messages <- x$messages[order(x$messages$file_line_number, na.last = FALSE), ]
        row.names(x$messages) <- NULL
    }
    x
}

