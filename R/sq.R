#' Read a team roster (*.sq) file
#'
#' @param filename string: file name to read
#' @param do_transliterate logical: should we transliterate all text to ASCII?
#' @param encoding character: text encoding to use. Text is converted from this encoding to UTF-8. A vector of multiple encodings can be provided, and this function will attempt to choose the best. If encoding is "guess", the encoding will be guessed
#' @param date_format string: the expected date format (used for dates of birth). One of "ymd", "mdy", "dmy", or "guess". If \code{date_format} is something other than "guess", that date format will be preferred where dates are ambiguous
#' @param surname_case string or function: should we change the case of player surnames? If \code{surname_case} is a string, valid values are "upper","lower","title", or "asis"; otherwise \code{surname_case} may be a function that will be applied to the player surname strings
#' @param verbose logical: if \code{TRUE}, show progress
#'
#' @return A list with two components: "team" and "players", both of which are data frames
#'
#' @examples
#' \dontrun{
#'   x <- dv_read_sq("/path/to/my/roster_file")
#' }
#' @export
dv_read_sq <- function(filename, do_transliterate = FALSE, encoding = "guess", date_format = "guess", surname_case = "asis", verbose = FALSE) {
    assert_that(is.flag(do_transliterate), !is.na(do_transliterate))
    assert_that(is.character(encoding))
    assert_that(is.flag(verbose), !is.na(verbose))
    assert_that(is.string(date_format))
    date_format <- match.arg(tolower(date_format), c("guess", "dmy", "ymd", "mdy"))
    if (date_format %eq% "guess") date_format <- NULL
    assert_that(is.string(surname_case) || is.function(surname_case))
    file_text <- readLines(filename, warn = FALSE)
    if (length(encoding)>1 || identical(tolower(encoding), "guess")) {
        ## try to guess encoding
        if (identical(tolower(encoding), "guess")) {
            ## first try using the embedded encoding info
            textenc <- tryCatch(read.table(text = file_text[2], sep = "\t")[[6]], error = function(e) NULL)
            if (!is.null(textenc)) {
                enclist <- intersect(paste0(c("windows-", "cp"), tolower(textenc)), tolower(iconvlist()))
                if (length(enclist)>0) {
                    try({
                        out <- dv_read_sq(filename = filename, do_transliterate = do_transliterate, encoding = enclist[1], date_format = date_format, surname_case = surname_case, verbose = verbose)
                        ## TODO: check that this actually worked
                        if (verbose) message(sprintf("Using text encoding: %s", enclist[1]))
                        return(out)
                    }, silent=TRUE)
                    ## if that fails, we'll drop through to our previous guessing code
                }
            }
            encoding <- stri_enc_detect2(file_text)[[1]]$Encoding
            ## add common ones
            encoding <- c(encoding, c("windows-1252", "iso-8859-2", "windows-1250", "US-ASCII", "UTF-8", "SHIFT-JIS", "CP932")) ## windows-1252 should be used in preference to "iso-8859-1", see https://en.wikipedia.org/wiki/ISO/IEC_8859-1
            encoding <- encoding[tolower(encoding) %in% tolower(iconvlist())]
        }
        encoding <- get_best_encodings(encoding, filename = filename, read_from = 1, read_to = length(file_text), expect_tildes = FALSE)
        if (length(encoding$encodings) < 1) stop("error in guessing text encoding")
        if (encoding$error_score > 0) {
            ## haven't found an encoding with zero error score, but we have relied on stri_enc_detect2
            ## now just brute force it over all possible encodings (will be slow)
            encoding_brute <- get_best_encodings(iconvlist(), filename = filename, read_from = 1, read_to = length(file_text))
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
    file_text <- enc::read_lines_enc(filename, file_encoding = encoding)
    if (do_transliterate) {
        file_text <- stri_trans_general(file_text, "latin-ascii")
    }

    ## player info
    x <- data.table::fread(text = file_text[seq_along(file_text)[-1:-2]], data.table = FALSE, header = FALSE)
    ## set column names, allowing for varying numbers of columns that might actually be in x
    nms <- paste0("V", seq_len(ncol(x)))
    temp <- seq_len(min(15, ncol(x)))
    nms[temp] <- c("number", "player_id", "lastname", "date_of_birth", "height", "V6", "special_role", "V8", "firstname", "role", "nickname", "transferred", "foreign", "picture", "V15")[temp]
    x <- setNames(x, nms)
    if (is.character(surname_case)) {
        x$lastname <- switch(tolower(surname_case),
                             upper = toupper(x$lastname),
                             lower = tolower(x$lastname),
                             title = str_to_title(x$lastname),
                             x$lastname)
    } else if (is.function(surname_case)) {
        x$lastname <- surname_case(x$lastname)
    }
    x$role <- roles_int2str(x$role)
    ## dates
    temp <- lapply(seq_len(nrow(x)), function(i) {
        out <- manydates(x$date_of_birth[i], preferred = date_format)
        if (length(out) < 1) NA else out[1] ## defaults to ymd if multiple and ymd works
    })
    x$date_of_birth <- as.Date(unlist(temp), origin = "1970-01-01")
    x$transferred[is.na(x$transferred)] <- FALSE
    x$foreign[is.na(x$foreign)] <- FALSE

    ## team info
    tx <- data.table::fread(text = paste0(file_text[2], "\n"), data.table = FALSE, header = FALSE)
    nms <- paste0("V", seq_len(ncol(tx)))
    temp <- seq_len(min(6, ncol(tx)))
    nms[temp] <- c("team_id", "team", "coach", "assistant", "team_abbrev", "text_encoding")[temp]
    tx <- setNames(tx, nms)
    list(team = tx, players = x)
}
