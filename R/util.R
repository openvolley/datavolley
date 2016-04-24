# Extract text chunks from datavolley file. Internal function, not exported for users.
#
# @param txt: dv text
# @param token1: starting token, e.g. "[3SET]"
# @param token2: ending token
#
# @return string
text_chunk=function(txt,token1,token2) {
    idx1=grep(token1,txt,fixed=TRUE)
    if (missing(token2)) {
        ## find next section starting with "["
        idx2=grep("^\\[",txt)
        idx2=idx2[idx2>idx1][1]
    } else {
        idx2=grep(token2,txt,fixed=TRUE)
    }
    if (idx2==idx1+1) {
        ""
    } else {
        paste(txt[(idx1+1):(idx2-1)],collapse="\n")
    }
}


findnext <- function(these,after) {
    ## return the number in after that comes after each of these
    sapply(these,function(z){
        temp <- after-z
        temp <- temp[temp>0]
        if (length(temp)<1)
            NA
        else
            min(temp)+z
    })
}

findprev <- function(these,prev) {
    ## return the number in prev that comes before each of these
    sapply(these,function(z){
        temp <- z-prev
        temp <- temp[temp>0]
        if (length(temp)<1)
            NA
        else
            z-min(temp)
    })
}


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

