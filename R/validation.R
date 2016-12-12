#' Additional validation checks on a DataVolley file
#'
#' This function is automatically run as part of \code{read_dv} if \code{do_extra_validation} is TRUE.
#' The current validation messages/checks are:
#' \itemize{
#'   \item message "the listed player is not on court in this rotation": the player making the action is not part of the current rotation. Libero players are ignored for this check
#'   \item message "player making a front row attack is not in the front row": an attack starting from zones 2-4 was made by a player not in the front row of the current rotation
#'   \item message "point awarded to incorrect team following error (or \"error\" evaluation incorrect)"
#'   \item message "point awarded to incorrect team (or [winning play] evaluation incorrect)"
#' }
#' 
#' @param x datavolley: datavolley object as returned by \code{read_dv}
#'
#' @return data.frame with columns message (the validation message), file_line_number (the corresponding line number in the DataVolley file), and file_line (the actual line from the DataVolley file).
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
    attacks <- plays[plays$skill %eq% "Attack",]
    temp <- ldply(1:nrow(attacks),function(z) {fcols <- if (attacks$home_team[z]==attacks$team[z]) c("home_p2","home_p3","home_p4") else c("visiting_p2","visiting_p3","visiting_p4"); data.frame(attacker2=attacks[z,fcols[1]],attacker3=attacks[z,fcols[2]],attacker4=attacks[z,fcols[3]])})
    attacks$attacker2 <- temp$attacker2
    attacks$attacker3 <- temp$attacker3
    attacks$attacker4 <- temp$attacker4

    ## front-row attacking player isn't actually in front row
    chk <- attacks[attacks$start_zone %in% c(2,3,4) & attacks$player_number!=attacks$attacker2 & attacks$player_number!=attacks$attacker3 & attacks$player_number!=attacks$attacker4,]
    
    if (nrow(chk)>0)
        out <- rbind(out,data.frame(file_line_number=chk$file_line_number,message="player making a front row attack is not in the front row",file_line=x$raw[chk$file_line_number],stringsAsFactors=FALSE))

    ## player not in recorded rotation making a play (other than by libero)
    liberos_v <- x$meta$players_v$number[grepl("L",x$meta$players_v$special_role)] ##subset(x$meta$players_v,grepl("L",special_role))$number
    liberos_h <- x$meta$players_h$number[grepl("L",x$meta$players_h$special_role)] ##subset(x$meta$players_h,grepl("L",special_role))$number
    pp <- plays[plays$skill %in% c("Serve","Attack","Block","Dig","Freeball","Reception","Set"),]
    pp$labelh <- "home_team"
    pp$labelv <- "visiting_team"
    temp <- ldply(1:nrow(pp),function(z) {fcols <- if (pp$home_team[z] %eq% pp$team[z]) c("home_p1","home_p2","home_p3","home_p4","home_p5","home_p6","labelh") else c("visiting_p1","visiting_p2","visiting_p3","visiting_p4","visiting_p5","visiting_p6","labelv"); out <- pp[z,fcols]; names(out) <- c(paste0("player",1:6),"which_team"); out })
    chk <- sapply(1:nrow(pp),function(z) !pp$player_number[z] %in% (if (temp$which_team[z]=="home_team") liberos_h else liberos_v) & (!pp$player_number[z] %in% temp[z,]))
    if (any(chk))
        out <- rbind(out,data.frame(file_line_number=pp$file_line_number[chk],message="the listed player is not on court in this rotation",file_line=x$raw[pp$file_line_number[chk]],stringsAsFactors=FALSE))

    ## point not awarded to right team following error
    chk <- (plays$evaluation_code %eq% "=" | (plays$skill %eq% "Block" & plays$evaluation_code %eq% "/")) &  ## error or block Invasion
            (plays$team %eq% plays$point_won_by)
    if (any(chk))
        out <- rbind(out,data.frame(file_line_number=plays$file_line_number[chk],message="point awarded to incorrect team following error (or \"error\" evaluation incorrect)",file_line=x$raw[plays$file_line_number[chk]],stringsAsFactors=FALSE))
    
    ## point not awarded to right team following win
    chk <- (plays$skill %in% c("Serve","Attack","Block") & plays$evaluation_code %eq% "#") &  ## ace or winning attack or block
            (!plays$team %eq% plays$point_won_by)
    if (any(chk))
        out <- rbind(out,data.frame(file_line_number=plays$file_line_number[chk],message=paste0("point awarded to incorrect team (or \"",plays$evaluation[chk],"\" evaluation incorrect)"),file_line=x$raw[plays$file_line_number[chk]],stringsAsFactors=FALSE))
    out
}


