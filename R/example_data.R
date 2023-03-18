#' Example DataVolley files provided as part of the datavolley package
#'
#' @references The example data files came from \url{http://www.odbojka.si/}
#' @param choice numeric: which data file to return?
#' \itemize{
#'   \item{1 - the January 2015 Slovenian junior women's final between Braslovče and Nova KBM Branik}
#'   \item{2 - the December 2012 men's Slovenian national championship semifinal between ACH Volley and Maribor}
#'   \item{3 - Nicaragua vs Cuba women from the Pan American Cup, August 2022 (vsm format, courtesy Christophe Elek)}
#' }
#' @return path to the file
#'
#' @seealso \code{\link{dv_read}}
#'
#' @examples
#' myfile <- dv_example_file()
#' x <- dv_read(myfile, insert_technical_timeouts = FALSE)
#' summary(x)
#'
#' @export
dv_example_file <- function(choice=1) {
    assert_that(is.numeric(choice))
    switch(as.character(choice),
           "1" = system.file("extdata/example_data.dvw", package = "datavolley"),
           "2" = system.file("extdata/PM06.dvw", package = "datavolley"),
           "3" = system.file("extdata/2022-08-23_NCA-CUB.vsm", package = "datavolley"),
           stop("unrecognized 'choice' value (", choice, ")")
           )
}


#' Fake coordinate data
#'
#' Generates fake coordinate data. The DataVolley software has the capability to accurately record court locations associated with each action. However, not all files contain this information (it can be time consuming to enter). This function generates fake coordinate data that can be used for demonstration purposes.
#'
#' @param skill string: the skill type to generate positions for (only "serve" is implemented so far)
#' @param evaluation character: vector of evaluations (as returned in the \code{evalution} column of a datavolleyplays object)
#'
#' @return data.frame of coordinates with columns "start_coordinate", "start_coordinate_x", "start_coordinate_y", "end_coordinate", "end_coordinate_x", "end_coordinate_y". The returned data.frame will have as many rows as the length of the \code{evaluation} vector
#'
#' @seealso \code{\link{dv_xy}}
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' ## read example data file
#' x <- dv_read(dv_example_file(), insert_technical_timeouts = FALSE)
#'
#' ## take just the serves from the play-by-play data
#' xserves <- subset(plays(x), skill=="Serve")
#'
#' ## if the file had been scouted with coordinate included, we could plot them directly
#' ## this file has no coordinates, so we'll fake some up for demo purposes
#' coords <- dv_fake_coordinates("serve", xserves$evaluation)
#' xserves[, c("start_coordinate", "start_coordinate_x", "start_coordinate_y",
#'             "end_coordinate", "end_coordinate_x", "end_coordinate_y")] <- coords
#'
#' ## now we can plot these
#' xserves$evaluation[!xserves$evaluation %in% c("Ace", "Error")] <- "Other"
#' 
#' ggplot(xserves, aes(start_coordinate_x, start_coordinate_y,
#'        xend=end_coordinate_x, yend=end_coordinate_y, colour=evaluation))+
#'     geom_segment() + geom_point() +
#'     scale_colour_manual(values=c(Ace="limegreen", Error="firebrick", Other="dodgerblue")) +
#'     ggcourt(labels=c("Serving team", "Receiving team"))
#' }
#'
#' @export
dv_fake_coordinates <- function(skill, evaluation) {
    assert_that(is.string(skill))
    skill <- match.arg(tolower(skill), c("serve"))
    assert_that(is.character(evaluation))
    evaluation <- tolower(evaluation)
    N <- length(evaluation)
    intsample <- function(N, from, to) sample.int(to-from+1, size=N, replace=TRUE)+from-1
    if (skill %eq% "serve") {
        ## default to legal starting and ending locations
        s_x <- intsample(N, 11, 90)
        s_y <- intsample(N, 1, 8)
        e_x <- intsample(N, 11, 90)
        e_y <- intsample(N, 55, 91)
        ## serve errors
        temp <- runif(N)
        ## make 1/5 of them foot faults (illegal starting location)
        idx <- evaluation %eq% "error" & temp>0.8
        s_y[idx] <- intsample(sum(idx), 11, 13)
        ## and the remainder an illegal ending location
        idx <- evaluation %eq% "error" & temp<=0.1 ## net fault
        e_y[idx] <- intsample(sum(idx), 50, 51)
        idx <- evaluation %eq% "error" & temp>0.1 & temp<=0.8 ## out of bounds
        e_y[idx] <- intsample(sum(idx), 92, 101)
    }
    ## s_x etc are grid bin numbers
    ## calculate the corresponding grid index
    s_i <- s_x+(s_y-1)*100
    e_i <- e_x+(e_y-1)*100
    ## and convert s_x etc to actual court coordinates for plotting
    temp <- dv_index2xy(s_i)
    s_x <- temp[, 1]
    s_y <- temp[, 2]
    temp <- dv_index2xy(e_i)
    e_x <- temp[, 1]
    e_y <- temp[, 2]
    data.frame(start_coordinate=s_i, start_coordinate_x=s_x, start_coordinate_y=s_y,
               end_coordinate=e_i, end_coordinate_x=e_x, end_coordinate_y=e_y)
}






