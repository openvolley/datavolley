## some helper functions for working with attack codes and setter calls

#' Extract the setter calls table from a datavolley object
#'
#' @details
#' Note that some columns are placeholders (named e.g. `X2`, `X4` and generally filled with NAs). These are kept for compatibility reasons.
#'
#' @param x datavolley: as returned by [dv_read()]
#'
#' @return The setter calls table from the metadata section of `x`
#'
#' @examples
#' x <- dv_read(dv_example_file())
#'
#' ## the setter calls in this file
#' dv_meta_setter_calls(x)
#'
#' @export
dv_meta_setter_calls <- function(x) x$meta$sets

#' Extract the attack codes table from a datavolley object
#'
#' @details
#' Note that some columns are placeholders (named e.g. `X6`, `X10` and generally filled with NAs). These are kept for compatibility reasons.
#'
#' @param x datavolley: as returned by [dv_read()]
#'
#' @return The attack codes table from the metadata section of `x`
#'
#' @examples
#' x <- dv_read(dv_example_file())
#'
#' ## the attack codes in this file
#' dv_meta_attack_codes(x)
#'
#' @export
dv_meta_attack_codes <- function(x) x$meta$attacks

#' Remap attack codes to new values
#'
#' @details
#' If a given value of `to_code` does not exist in the attacks metadata table in `x`, we will just rename `from_code` to `to_code` without changing any other details. If `to_code` does exist in the attacks metadata table, we will rename the code and also change the attack description, attack and set tempo (`skill_type` column) and set target (`set_type`), if those details are different for the `to_code`. The `start_zone` is not updated in either case.
#'
#' If a given value of `to_code` is NA or an empty string (""), the corresponding `from_code` will be removed, so long as `allow_remove` is TRUE. It is not entirely clear how useful this removal functionality is, so the extra `allow_remove` parameter must be set in order to avoid inadvertant removals.
#'
#' @param x datavolley: as returned by [dv_read()]
#' @param from_codes character: one or more attack codes to change
#' @param to_codes character: one or more replacement attack codes. `to_codes` must either be the same length as `from_codes` (in which case each element of `from_codes` will be changed to its corresponding element in `to_codes`), or `to_codes` must be a single string (in which case each element of `from_codes` will be changed to this code)
#' @param allow_remove logical: if a given value of `to_code` is NA or an empty string (""), the corresponding `from_code` will be removed, so long as `allow_remove` is TRUE. It is not entirely clear how useful this removal functionality is, so the extra `allow_remove` parameter must be set in order to avoid inadvertant removals
#'
#' @return A modified copy of `x`
#'
#' @examples
#' x <- dv_read(dv_example_file())
#'
#' ## the attack codes in this file
#' dv_meta_attack_codes(x)
#'
#' ## and their usage in the play-by-play data
#' table(plays(x)$attack_code)
#'
#' ## Example 1
#' ## relabel V5 and V6 as Z5 and Z6
#' ## (leaving their other details unchanged, because Z5 and Z6
#' ##  do not exist in the attack codes table)
#' x2 <- dv_remap_attack_codes(x, from_codes = c("V5", "V6"), to_codes = c("Z5", "Z6"))
#'
#' ## check that the table has changed
#' dv_meta_attack_codes(x2)
#' ## and their usage in the play-by-play data
#' table(plays(x2)$attack_code)
#'
#' ## Example 2
#' ## relabel "C5" and "JJ" to "X5" (they are all fast sets to position 4)
#' ac <- dv_meta_attack_codes(x)
#' ac[ac$code %in% c("X5", "C5", "JJ"), ]
#'
#' x2 <- dv_remap_attack_codes(x, from_codes = c("C5", "JJ"), to_codes = "X5")
#' ac2 <- dv_meta_attack_codes(x2)
#' ac2[ac2$code %in% c("X5", "C5", "JJ"), ]
#' table(plays(x2)$attack_code)
#'
#' @export
dv_remap_attack_codes <- function(x, from_codes, to_codes, allow_remove = FALSE) {
    assert_that(is.character(from_codes))
    assert_that(is.character(to_codes) || all(is.na(to_codes)))
    assert_that(is.flag(allow_remove))
    if (length(from_codes) > 1 && length(to_codes) == 1) to_codes <- rep(to_codes, length(from_codes))
    if (length(from_codes) < 1) stop("from_codes must have at least one entry")
    if (length(to_codes) < 1) stop("to_codes must have at least one entry")
    if (length(from_codes) != length(to_codes)) stop("from_codes and to_codes must be the same length, or to_codes must be a single string")

    if (any(is.na(to_codes) | !nzchar(to_codes)) && !isTRUE(allow_remove)) {
        ## warn once, not on every iteration of the loop below
        warning("one or more to_codes entries is NA or an empty string: the `allow_remove` parameter must be set to TRUE to allow attack codes to be removed")
    }

    for (ii in seq_along(from_codes)) {
        if ((is.na(to_codes[ii]) || !nzchar(to_codes[ii])) && !isTRUE(allow_remove)) {
            next
        }
        x <- do_remap_attack_code(x, from_codes[ii], to_codes[ii])
    }
    x
}

do_remap_attack_code <- function(dv, from_code, to_code) {
    assert_that(is.string(from_code), nchar(from_code) == 2, msg = "`from_code` must be a two-character string")
    assert_that(is.string(to_code) || is.na(to_code), msg = "`to_code` must be a string or NA")
    if (!nzchar(to_code)) to_code <- NA_character_ ## empty string "" to NA
    do_del <- is.na(to_code) ## are we deleting it?
    if (!do_del) assert_that(nchar(to_code) == 2, msg = "`to_code` must be a two-character string")
    ## e.g. from_code = "C5", to_code "X5"
    ## - if to_code does not exist in the dv$meta$attacks table, we will just rename from_code to to_code without changing any other details
    ## - if to_code does exist in the dv$meta$attacks table, we will rename the code but also change the attack description, attack and set tempo and set target, if those details are different for the to_code. The start zone is not updated regardless
    if (!any(dv$meta$attacks$code == from_code, na.rm = TRUE)) {
        if (!any(dv$plays$attack_code == from_code, na.rm = TRUE)) {
            ## nothing to do
            return(dv)
        } else {
            ## hmm, the attack code is not defined in the metadata but it is used in the play-by-play data
            ## warn, unless we are deleting it because in that case it doesn't really matter if it's in the metadata or not
            if (!do_del) warning("attack code '", from_code, "' is not defined in the metadata but is used in the play-by-play data.\n",
                                 "I will change it in the play-by-play data to '", to_code, "' but cannot add it to the metadata table or update play-by-play details like tempo etc")
        }
    }
    m_idx <- which(dv$meta$attacks$code == to_code)
    if (length(m_idx) > 1) m_idx <- m_idx[1] ## should not happen but only use first if multiple
    replace_details <- FALSE ## are we also updating skill_type etc?
    if (length(m_idx) > 0) {
        ## the to_code does exist in our attack codes table
        replace_details <- TRUE
        to_tempo <- dv$meta$attacks$type[m_idx]
        to_description <- dv$meta$attacks$description[m_idx]
        to_set_type <- dv$meta$attacks$set_type[m_idx]
    } else{
        to_tempo <- to_description <- to_set_type <- NA_character_
    }
    px <- dv$plays
    ## find the relevant attacks
    idx <- px$skill == "Attack" & px$attack_code == from_code
    ## and the set preceding each of those attacks
    eidx <- which(px$skill == "Set" & lead(idx) & px$team == lead(px$team) & px$match_id == lead(px$match_id) & px$point_id == lead(px$point_id))
    idx <- which(idx)

    if (replace_details) {
        px$attack_description[idx] <- to_description
        px$skill_type[idx] <- attack_map(to_tempo, "attack") ## attack tempo
        px$set_type[eidx] <- to_set_type ## set target
        px$skill_type[eidx] <- attack_map(to_tempo, "set") ## set tempo should match attack tempo
    } else if (do_del) {
        px$attack_description[idx] <- NA_character_
    }
    px$attack_code[idx] <- to_code ## replace the value in the attack_code column (NA if we are deleting it)
    substr(px$code[idx], 7, 8) <- if (do_del) "~~" else to_code ## also replace the relevant characters in the scout code
    dv$plays <- px
    if (replace_details || do_del) {
        ## drop the old attack code from the metadata
        dv$meta$attacks <- dv$meta$attacks[!dv$meta$attacks$code %in% from_code, ]
    } else {
        ## relabel the old attack codes in the metadata with the new code
        dv$meta$attacks$code[which(dv$meta$attacks$code == from_code)] <- to_code
    }
    dv
}
