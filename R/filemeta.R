##[3DATAVOLLEYSCOUT]
##FILEFORMAT: 2.0
##GENERATOR-DAY: 2015/10/30 17.24.59
##GENERATOR-IDP: DVW
##GENERATOR-PRG: Data Volley
##GENERATOR-REL: Release 3.7.7
##GENERATOR-VER: Professional
##GENERATOR-NAM: PLUSLIGA - Trefl Gdansk
##LASTCHANGE-DAY: 11/20/2015 20.54.57
##LASTCHANGE-IDP: DVW
##LASTCHANGE-PRG: Data Volley
##LASTCHANGE-REL: Release 3.7.7
##LASTCHANGE-VER: Professional
##LASTCHANGE-NAM: KLUB SPORTOWY JASTRZEBSKI WEGIEL S.A.

read_filemeta <- function(txt) {
    msgs <- list()
    txt <- text_chunk(txt,"[3DATAVOLLEYSCOUT]")
    ok <- FALSE
    try({
        p <- read.table(text=txt,sep=":",quote="",stringsAsFactors=FALSE,header=FALSE)
        nms <- tolower(gsub("-", "_", p[, 1], fixed=TRUE))
        p <- as.data.frame(t(p[, 2]), stringsAsFactors=FALSE)
        colnames(p) <- nms
        ok <- TRUE
    }, silent=TRUE)
    if (!ok) {
        msgtxt <- "Could not read the [3DATAVOLLEYSCOUT] section of the input file: the filemeta information will be missing" 
        msgs <- collect_messages(msgs, msgtxt, NA_integer_, "", severity=3)
        warning(msgtxt)
        return(list(file_meta=data.frame(), messages=do.call(rbind, lapply(msgs, as.data.frame))))
    }

    if ("generator_day" %in% names(p)) {
        dt <- manydatetimes(p$generator_day)
        if (length(dt)==1) p$generator_day <- dt
    }
    if ("lastchange_day" %in% names(p)) {
        dt <- manydatetimes(p$lastchange_day)
        if (length(dt)==1) p$lastchange_day <- dt
    }
    list(file_meta=p, messages=do.call(rbind, lapply(msgs, as.data.frame)))
}