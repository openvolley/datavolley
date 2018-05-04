#' Example DataVolley files provided as part of the datavolley package
#'
#' @references The example data files came from \url{http://www.odbojka.si/}
#' @param choice numeric: which data file to return?
#' \itemize{
#'   \item{1 - the January 2015 Slovenian junior women's final between Braslovče and Nova KBM Branik}
#'   \item{2 - the December 2012 men's Slovenian national championship semifinal between ACH Volley and Maribor}
#' }
#' @return path to the file
#'
#' @seealso \code{\link{read_dv}}
#'
#' @examples
#' myfile <- dv_example_file()
#' x <- read_dv(myfile, insert_technical_timeouts=FALSE)
#' summary(x)
#' 
#' @export
dv_example_file <- function(choice=1) {
    assert_that(is.numeric(choice))
    switch(as.character(choice),
           "1"=system.file("extdata/example_data.dvw",package="datavolley"),
           "2"=system.file("extdata/PM06.dvw",package="datavolley"),
           stop("unrecognized 'choice' value (", choice, ")")
           )
}





