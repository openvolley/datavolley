## Accumulate messages for later display
## Internal function, not exported
## severity: 1=critical, 2=informative, may lead to misinterpretation of data, 3=minor, esp. those that might have resulted from selective post-processing of combo codes
collect_messages <- function(msgs,msg_text,line_nums,raw_lines,severity,fatal=FALSE) {
    if (missing(line_nums)) line_nums <- NA
    if (missing(raw_lines)) raw_lines <- "[unknown]"
    if (missing(severity)) severity <- NA
    vt <- rep(NA_integer_,length(line_nums))
    if (!missing(raw_lines)) vt <- video_time_from_raw(raw_lines)
    if (fatal) {
        lnt <- as.character(line_nums)
        lnt[is.na(lnt)] <- "[unknown]"
        txt <- paste0("line ",lnt,": ",msg_text," (line in file is: \"",raw_lines,"\")")
        if (fatal) stop(paste(txt,collapse=" / "))
    } else {
        msgs[[length(msgs)+1]] <- list(file_line_number=line_nums,video_time=vt,message=msg_text,file_line=raw_lines,severity=severity)
    }
    msgs
}

video_time_from_raw <- function(raw_lines) {
    tryCatch(vapply(raw_lines,function(z)tryCatch(if (!is.null(z) && is.character(z) && nzchar(z) && !is.na(z)) as.numeric(read.csv(text=z,sep=";",header=FALSE,stringsAsFactors=FALSE)[1,13]) else NA_integer_,error=function(e)NA_integer_),FUN.VALUE=1,USE.NAMES=FALSE),error=function(e)rep(NA_integer_,length(raw_lines)))
}

join_messages <- function(msgs1,msgs2) {
    if (length(msgs2)>0) {
        msgs1 <- c(msgs1,msgs2)
    }
    msgs1
}

# Extract text chunks from datavolley file. Internal function, not exported for users.
#
# @param txt: dv text
# @param token1: starting token, e.g. "[3SET]"
# @param token2: ending token
#
# @return string
text_chunk <- function(txt,token1,token2) {
    idx1 <- grep(token1,txt,fixed=TRUE)
    if (length(idx1)<1) return("")
    if (missing(token2)) {
        ## find next section starting with "["
        idx2=grep("^\\[",txt)
        idx2=idx2[idx2>idx1][1]
    } else {
        idx2=grep(token2,txt,fixed=TRUE)
    }
    if (idx2==(idx1+1)) {
        ""
    } else {
        out <- txt[(idx1+1):(idx2-1)]
        out <- out[sapply(out,is.notempty.string)]##!is.na(out)]
        paste(out,collapse="\n")
    }
}

is.notempty.string <- function(x) {
    (is.character(x) && length(x)==1) && !is.na(x) && nchar(x)>0
}



#' Find each entry in y that follows each entry in x
#'
#' @param x numeric: vector 
#' @param y numeric: vector 
#'
#' @return vector, each entry is the value in y that is next-largest to each corresponding entry in x
#'
#' @examples
#' findnext(c(1,5,10),c(1,2,3,7,8,9))
#' 
#' @export
findnext <- function(x,y) {
    ## return the number in y that comes after each of x
    sapply(x,function(z){
        temp <- y-z
        temp <- temp[temp>0]
        if (length(temp)<1)
            NA
        else
            min(temp)+z
    })
}

##findnext <- function(these,after) {
##    ## return the number in after that comes after each of these
##    sapply(these,function(z){
##        temp <- after-z
##        temp <- temp[temp>0]
##        if (length(temp)<1)
##            NA
##        else
##            min(temp)+z
##    })
##}


#' Find each entry in y that precedes each entry in x
#'
#' @param x numeric: vector 
#' @param y numeric: vector 
#'
#' @return vector, each entry is the value in y that is next-smallest to each corresponding entry in x
#'
#' @examples
#' findprev(c(1,5,10),c(1,2,3,7,8,9))
#' 
#' @export
findprev <- function(x,y) {
    ## return the number in y that comes before each of x
    sapply(x,function(z){
        temp <- z-y
        temp <- temp[temp>0]
        if (length(temp)<1)
            NA
        else
            z-min(temp)
    })
}


##findprev <- function(these,prev) {
##    ## return the number in prev that comes before each of these
##    sapply(these,function(z){
##        temp <- z-prev
##        temp <- temp[temp>0]
##        if (length(temp)<1)
##            NA
##        else
##            z-min(temp)
##    })
##}


## equality with NAs considered false
`%eq%` <- function(x,y) x==y & !is.na(x) & !is.na(y)

## convenience function to replace NAs
na.replace <- function(x,replace_with) {x[is.na(x)] <- replace_with; x}


#' Find a particular match in a list of datavolley objects
#'
#' @param match_id string: match_id to find
#' @param x list: list of datavolley objects as returned by \code{read_dv} 
#'
#' @return numeric index of the match in the list
#'
#' @seealso \code{\link{read_dv}}
#'
#' @export
find_match <- function(match_id,x) {
    which(sapply(x,function(z)z$meta$match_id==match_id))
}


most_common_value <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

manydates <- function(z) suppressWarnings(unique(as.Date(na.omit(c(lubridate::ymd(z), lubridate::dmy(z), lubridate::mdy(z), lubridate::ymd_hms(z), lubridate::dmy_hms(z), lubridate::mdy_hms(z))))))

manydatetimes <- function(z) suppressWarnings(unique(na.omit(c(lubridate::ymd_hms(z), lubridate::dmy_hms(z), lubridate::mdy_hms(z)))))
