#' Additional validation checks on a datavolley object
#'
#' This function is automatically run as part of \code{read_dv} if \code{do_extra_validation} is TRUE
#' 
#' @param x datavolley: datavolley object as returned by \code{read_dv}
#'
#' @return data.frame
#'
#' @seealso \code{\link{read_dv}}
#'
#' @examples
#' \dontrun{
#'   x <- read_dv(system.file("extdata/example_data.dvw",package="datavolley"),
#'     insert_technical_timeouts=FALSE)
#'   xv <- validate_dv(x)
#' }
#'
#' @export
validate_dv <- function(x) {
    plays <- plays(x)
    out <- data.frame(file_line_number=integer(),message=character(),file_line=character(),stringsAsFactors=FALSE)
    ## find front-row players for each attack
    attacks <- subset(plays,skill %eq% "Attack")
    temp <- ldply(1:nrow(attacks),function(z) {fcols <- if (attacks$home_team[z]==attacks$team[z]) c("home_p2","home_p3","home_p4") else c("visiting_p2","visiting_p3","visiting_p4"); data.frame(attacker2=attacks[z,fcols[1]],attacker3=attacks[z,fcols[2]],attacker4=attacks[z,fcols[3]])})
    attacks$attacker2 <- temp$attacker2
    attacks$attacker3 <- temp$attacker3
    attacks$attacker4 <- temp$attacker4

    ## front-row attacking player isn't actually in front row
    chk <- subset(attacks,start_zone %in% c(2,3,4) & player_number!=attacker2 & player_number!=attacker3 & player_number!=attacker4)
    if (nrow(chk)>0)
        out <- rbind(out,data.frame(file_line_number=chk$file_line_number,message="player making a front row attack is not in the front row",file_line=x$raw[chk$file_line_number],stringsAsFactors=FALSE))
    out

    ## player not in recorded rotation making a play (other than by libero)
    liberos_v <- subset(x$meta$players_v,grepl("L",special_role))$number
    liberos_h <- subset(x$meta$players_h,grepl("L",special_role))$number
    pp <- subset(plays,skill %in% c("Serve","Attack","Block","Dig","Freeball","Reception","Set"))
    pp$labelh <- "home_team"
    pp$labelv <- "visiting_team"
    temp <- ldply(1:nrow(pp),function(z) {fcols <- if (pp$home_team[z] %eq% pp$team[z]) c("home_p1","home_p2","home_p3","home_p4","home_p5","home_p6","labelh") else c("visiting_p1","visiting_p2","visiting_p3","visiting_p4","visiting_p5","visiting_p6","labelv"); out <- pp[z,fcols]; names(out) <- c(paste0("player",1:6),"which_team"); out })
    chk <- sapply(1:nrow(pp),function(z) !pp$player_number[z] %in% (if (temp$which_team[z]=="home_team") liberos_h else liberos_v) & (!pp$player_number[z] %in% temp[z,]))
    if (any(chk))
        out <- rbind(out,data.frame(file_line_number=pp$file_line_number[chk],message="the listed player is not on court in this rotation",file_line=x$raw[pp$file_line_number[chk]],stringsAsFactors=FALSE))
    out
}


