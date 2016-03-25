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
