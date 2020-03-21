# Read the point by point rotation, and update it if necessary with new entries
#' Get team rotations point by point
#'
#' @param x datavolley or data.frame: a datavolley object as returned by \code{read_dv}, or the plays component of that object
#' @param team string: team name
#' @param point_ids vector: vector of pointids for which to return the rotation
#' @param new_rotation vector: vector of player number, positionned from 1 to 6.
#'
#' @return list of 2 data.frames. Current data.frame, without changes, and updated data.frame, with new player rotation. 
#'
#' @seealso \code{\link{read_dv}}
#'
#' @examples
#' \dontrun{
#'   x <- read_dv(dv_example_file(), insert_technical_timeouts=FALSE)
#'   rotations(x)
#' }
#' @export
rotations <- function(x, team, point_ids, new_rotation = NULL){
    
}