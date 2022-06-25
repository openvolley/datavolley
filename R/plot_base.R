#' Plot a volleyball court diagram
#'
#' Volleyball court schematic suitable for adding to a figure
#'
#' The datavolley package uses the following dimensions and coordinates for plotting:
#' \itemize{
#'   \item the court is shown such that the sidelines are oriented vertically and the net is oriented horizontally
#'   \item the intersection of the left-hand sideline and the bottom baseline is at (0.5, 0.5)
#'   \item the intersection of the right-hand sideline and the top baseline is at (3.5, 6.5)
#'   \item the net intersects the sidelines at (0.5, 3.5) and (3.5, 3.5)
#'   \item the zones 1-9 (as defined in the DataVolley manual) on the lower half of the court are located at:
#'     \enumerate{
#'       \item (3, 1)
#'       \item (3, 3)
#'       \item (2, 3)
#'       \item (1, 3)
#'       \item (1, 1)
#'       \item (2, 1)
#'       \item (1, 2)
#'       \item (2, 2)
#'       \item (3, 2)
#'     }
#'   \item the zones 1-9 (as defined in the DataVolley manual) on the upper half of the court are located at:
#'     \enumerate{
#'       \item (1, 6)
#'       \item (1, 4)
#'       \item (2, 4)
#'       \item (3, 4)
#'       \item (3, 6)
#'       \item (2, 6)
#'       \item (3, 5)
#'       \item (2, 5)
#'       \item (1, 5)
#'     }
#' }
#'
#' To get a visual depiction of this, try: \code{ ggplot() + ggcourt() + theme_bw()}
#'
#' @param plot_package string: either "base" or "ggplot2". If "ggplot2", the \code{\link{ggcourt}} function is used
#' @param court string: "full" (show full court) or "lower" or "upper" (show only the lower or upper half of the court)
#' @param show_zones logical: add numbers indicating the court zones (3m squares)?
#' @param labels string: labels for the lower and upper court halves (pass NULL for no labels)
#' @param as_for_serve logical: if TRUE and \code{show_zones} is TRUE, show zones as for serving. Only zones 1,5,6,7,9 are meaningful in this case
#' @param show_zone_lines logical: if FALSE, just show the 3m line. If TRUE, also show the 3m x 3m zones
#' @param show_minor_zones logical: add lines for the subzones (1.5m squares)?
#' @param grid_colour string: colour to use for court sidelines, 3m line, and net
#' @param zone_colour string: colour to use for zone lines and labels
#' @param minor_zone_colour string: colour to use for minor zone grid lines
#' @param fixed_aspect_ratio logical: if TRUE, coerce the plotted court to be square (for a half-court plot) or a 2:1 rectangle (full court plot). Prior to package version 0.5.3 this was not TRUE by default
#' @param zone_font_size numeric: the font size of the zone labels
#' @param ... : additional parameters passed to \code{ggplot2::theme_classic(...)}
#'
#' @return NULL
#'
#' @seealso \code{\link{ggcourt}} for a \code{ggplot2} equivalent function; \code{\link{dv_xy}}, \code{\link{dv_xy2index}}, \code{\link{dv_index2xy}}, \code{\link{dv_flip_xy}}
#'
#' @examples
#' \dontrun{
#' x <- dv_read(dv_example_file(), insert_technical_timeouts=FALSE)
#'
#' library(dplyr)
#'
#' ## Example: attack frequency by zone, per team
#'
#' attack_rate <- plays(x) %>% dplyr::filter(skill == "Attack") %>%
#'   group_by(team, start_zone) %>% dplyr::summarize(n_attacks = n()) %>%
#'   mutate(rate = n_attacks/sum(n_attacks)) %>% ungroup
#'
#' ## add columns "x" and "y" for the x,y coordinates associated with the zones
#' attack_rate <- cbind(attack_rate, dv_xy(attack_rate$start_zone, end = "lower"))
#'
#' ## for team 2, these need to be on the top half of the diagram
#' tm2 <- attack_rate$team == teams(x)[2]
#' attack_rate[tm2, c("x", "y")] <- dv_xy(attack_rate$start_zone, end = "upper")[tm2, ]
#'
#' ## plot it
#' dv_heatmap(attack_rate[, c("x", "y", "rate")], legend_title = "Attack rate")
#'
#' ## add the court diagram
#' dv_court(labels = teams(x))
#' }
#'
#' @export
dv_court <- function(plot_package = "base", court="full", show_zones=TRUE, labels=c("Serving team","Receiving team"), as_for_serve=FALSE, show_zone_lines=TRUE, show_minor_zones=FALSE, grid_colour="black", zone_colour="grey70", minor_zone_colour="grey80", fixed_aspect_ratio=TRUE, zone_font_size = 10, ...) {
    assert_that(is.string(plot_package))
    plot_package <- match.arg(tolower(plot_package), c("base", "ggplot", "ggplot2")) ##"rbokeh"
    if (plot_package %in% c("ggplot", "ggplot2")) return(ggcourt(court = court, show_zones = show_zones, labels = labels, as_for_serve = as_for_serve, show_zone_lines = show_zone_lines, show_minor_zones = show_minor_zones, grid_colour = grid_colour, zone_colour = zone_colour, minor_zone_colour = minor_zone_colour, fixed_aspect_ratio = fixed_aspect_ratio, zone_font_size = zone_font_size, ...))

    court <- match.arg(tolower(court),c("full","lower","upper"))
    assert_that(is.flag(show_zones),!is.na(show_zones))
    assert_that(is.flag(show_minor_zones),!is.na(show_minor_zones))
    assert_that(is.flag(as_for_serve),!is.na(as_for_serve))
    assert_that(is.flag(fixed_aspect_ratio),!is.na(fixed_aspect_ratio))
    ## horizontal grid lines
    hl <- data.frame(x=c(0.5,3.5),y=c(0.5,0.5,2.5,2.5,3.5,3.5,4.5,4.5,6.5,6.5),id=c(1,1,2,2,3,3,4,4,5,5))
    hl <- switch(court,
                 lower=hl[hl$y<4,],
                 upper=hl[hl$y>3,],
                 hl)
    plot_lines <- function(x, y, group, col = "black", ...) {
        if (missing(group)) {
            group <- rep(1, length(x))
        }
        invisible(sapply(unique(group), function(id) lines(x[group == id], y[group == id], col = col, ...)))
    }
    plot_lines(hl$x, hl$y, hl$id, col = grid_colour)
    hlz <- data.frame(x=c(0.5,3.5),y=c(1.5,1.5,5.5,5.5),id=c(6,6,7,7))
    hlz <- switch(court,
                 lower=hlz[hlz$y<4,],
                 upper=hlz[hlz$y>3,],
                 hlz)
    plot_lines(hlz$x, hlz$y, hlz$id, col = zone_colour)
    ## vertical grid lines
    vl <- data.frame(y=c(0.5,6.5),x=c(0.5,0.5,3.5,3.5),id=c(1,1,2,2))
    vl$y <- switch(court,
                   lower=mapvalues(vl$y,6.5,3.5),
                   upper=mapvalues(vl$y,0.5,3.5),
                   vl$y)
    plot_lines(vl$x, vl$y, vl$id, col = grid_colour)
    vlz <- data.frame(y=c(0.5,6.5),x=c(1.5,1.5,2.5,2.5),id=c(3,3,4,4))
    vlz$y <- switch(court,
                   lower=mapvalues(vlz$y,6.5,3.5),
                   upper=mapvalues(vlz$y,0.5,3.5),
                   vlz$y)
    plot_lines(vlz$x, vlz$y, vlz$id, col = zone_colour)
    ## minor grid lines
    if (show_minor_zones) {
        hlm <- data.frame(x=c(0.5,3.5),y=c(1,1,1.5,1.5,2,2,2.5,2.5,3,3,4,4,4.5,4.5,5,5,5.5,5.5,6,6),id=c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10))
        hlm <- switch(court,
                      lower=hlm[hlm$y<4,],
                      upper=hlm[hlm$y>3,],
                      hlm)
        plot_lines(hlm$x, hlm$y, hlm$id, col = minor_zone_colour)
        vlm <- data.frame(y=c(0.5,6.5),x=c(1,1,1.5,1.5,2,2,2.5,2.5,3,3),id=c(1,1,2,2,3,3,4,4,5,5))
        vlm$y <- switch(court,
                        lower=mapvalues(vlm$y,6.5,3.5),
                        upper=mapvalues(vlm$y,0.5,3.5),
                        vlm$y)
        plot_lines(vlm$x, vlm$y, vlm$id, col = minor_zone_colour)
    }
    ## net
    plot_lines(c(0.25, 3.75), c(3.5, 3.5), col = grid_colour, lwd = 2)
    if (!is.null(labels)) {
        if (court %in% c("full", "lower")) {
            ly <- if (as_for_serve) 0.1 else 0.4
            text(2, ly, labels[1])
        }
        if (court %in% c("full", "upper")) {
            lb <- if (court=="full") {
                     labels[2]
                 } else {
                     if (length(labels) == 2) labels[2] else labels[1]
                 }
            ly <- if (as_for_serve) 6.9 else 6.6
            text(2, ly, lb)
        }
    }
    if (show_zones) {
        xoff <- if (as_for_serve) 0.5 else 0.4
        szx <- if (as_for_serve) c(3.2, 0.8, 2.0, 1.4, 2.6) + xoff else c(3, 3, 2, 1, 1, 2, 1, 2, 3)
        szy <- if (as_for_serve) c(1, 1, 1, 1, 1) - 0.25 else c(1, 3, 3, 3, 1, 1, 2, 2, 2)
        ezx <- 4 - szx
        ezy <- 3 + 4 - szy
        lb <- if (as_for_serve) c(1, 5, 6, 7, 9) else 1:9
        ## these need to be added one by one, otherwise doesn't work with e.g. facet_wrap plots
        if (court %in% c("full","lower")) {
            text(x = szx - xoff, y = szy - 0.4, labels = lb, col = zone_colour, adj = c(0.5, 0.5), font = 3, cex = 0.6)
        }
        if (court %in% c("full","upper")) {
            text(x = ezx - xoff, y = ezy - 0.4, labels = lb, col = zone_colour, adj = c(0.5, 0.5), font = 3, cex = 0.6)
        }
    }
    invisible(NULL)
}


#' Create a new plot page for base graphics plotting
#'
#' The plot will be set up as either a full- or half-court plot, depending on the inputs. The extent can be specified via the \code{court} argument (values either "full", "lower", or "upper"), or via the \code{x} and \code{y} arguments. If the latter, provide either separate \code{x} and \code{y} numeric vectors, or as a single \code{x} \code{RasterLayer} object. If no extent is specified by any of these methods, a full-court plot is assumed.
#'
#' @param x numeric or RasterLayer: x-coordinates of the data to plot, or a \code{RasterLayer} layer defining the extent of the data
#' @param y numeric: y-coordinates of the data to plot. Not needed if \code{x} is a \code{RasterLayer} object
#' @param legend logical: if \code{TRUE}, leave space for a legend
#' @param court string: either "full", "lower", or "upper"
#' @param margins numeric: vector of four values to use as margins (bottom, left, top, right). Values are as a proportion of the plot size
#' @param par_args list: parameters to pass to \code{\link{par}}
#' @param ... : additional parameters passed to \code{\link{plot.window}}
#'
#' @return NULL
#'
#' @seealso \code{\link{dv_court}}, \code{\link{dv_heatmap}}
#'
#' @examples
#' dv_plot_new()
#' ## show an attack from position 4 to position 6
#' from <- dv_xy(4, end = "lower")
#' to <- dv_xy(6, end = "upper")
#' lines(c(from[1], to[1]), c(from[2], to[2]), col = "green")
#' ## add the court diagram
#' dv_court(labels = c("Attacking team", "Defending team"))
#'
#' @export
dv_plot_new <- function(x, y, legend, court, margins, par_args, ...) {
    if (missing(x)) x <- NULL
    if (missing(y)) y <- NULL
    if (missing(legend)) {
        legend <- inherits(x, "RasterLayer") ## default to TRUE when given a raster layer
    }
    xlims <- c(0, 4)
    ylims <- c(3.5, 3.5)
    if (!missing(court)) {
        assert_that(is.string(court))
        court <- match.arg(tolower(court), c("full", "lower", "upper"))
        ylims <- switch(court,
                        upper = c(3.5, 7),
                        lower = c(0, 3.5),
                        c(0, 7))
    }
    if (!is.null(x) && inherits(x, "RasterLayer")) {
        check_raster_available()
        xy <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
        y <- xy[, 2]
        x <- xy[, 1]
    }
    if (!is.null(x)) {
        ## expand xlims to accommodate
        x_xlims <- c(floor(min(x, na.rm = TRUE)*2)/2, ceiling(max(x, na.rm = TRUE)*2)/2)
        xlims <- c(min(xlims[1], x_xlims[1]), max(xlims[2], x_xlims[2]))
    }
    if (!is.null(y)) {
        ## expand ylims to accommodate
        if (any(y > 3.5)) ylims[2] <- max(ylims[2], 7)
        if (any(y < 3.5)) ylims[1] <- min(ylims[1], 0)
    }
    ## if we got this far and we still haven't set the ylims, default to full court
    if (abs(diff(ylims)) < 0.01) ylims <- c(0, 7)
##    cat(str(xlims))
##    cat(str(ylims))
    ##opar <- par()
    if (!missing(par_args)) do.call(par, par_args)
    graphics::plot.new()
    if (par()$page) par(oma = c(0, 0, 0, 0))
        ## figure if full or half court, so that margin can be set appropriately
    fsz <- par("fin") ## figure size in inches, width and height
    if (missing(margins)) {
        mai <- c(0, 0, 0, 0)
        if (legend && diff(range(ylims, na.rm = TRUE)) <= 3.5) {
            mai[4] <- fsz[2]*0.1
        }
    } else {
        ## margins supplied in normalized units, convert to inches
        mai <- margins*fsz[c(2, 1, 2, 1)]
    }
    par(mai = mai)
    graphics::plot.window(xlim = xlims, ylim = ylims, asp = 1, ...)##, xaxt = "n", yaxt = "n")
    #par(opar)
}

check_raster_available <- function() if (!requireNamespace("raster", quietly = TRUE)) stop("the raster package is required for heatmaps in base graphics. Either install it with \"install.packages('raster')\" or use ggplot") else invisible(TRUE)

#' Plot a court heatmap, using base graphics
#'
#' See \code{link{ggcourt}} for a \code{ggplot2}-based court diagram, which can be used to plot heatmaps with e.g. \code{ggplot2::geom_tile}.
#' 
#' Data can be provided either as separate \code{x}, \code{y}, and \code{z} objects, or as a single \code{RasterLayer} or \code{data.frame} object. If a \code{data.frame}, the first three columns are used (and assumed to be in the order \code{x}, \code{y}, \code{z}).
#'
#' @param x numeric, RasterLayer or data.frame: x-coordinates of the data to plot, or a \code{RasterLayer} layer or \code{data.frame} containing the data (x, y, and z together)
#' @param y numeric: y-coordinates of the data to plot
#' @param z numeric: values of the data to plot
#' @param col character: a vector of colours to use
#' @param zlim numeric: the minimum and maximum z values for which colors should be plotted, defaulting to the range of the finite values of z
#' @param legend logical: if \code{TRUE}, plot a legend
#' @param legend_title string: title for the legend
#' @param legend_title_font numeric: 1 = normal, 2 = bold, 3 = italic
#' @param legend_title_cex numeric: size scaling of legend title
#' @param legend_cex numeric: size scaling of legend text
#' @param legend_pos numeric: position of the legend (xmin, xmax, ymin, ymax) - in normalized units
#' @param res numeric: size of the heatmap cells. This parameter should only be needed in cases where the input data are sparse, when the automatic algorithm can't work it out. Values are given in metres, so \code{res} is 3 when showing zones, or 1.5 when showing subzones
#' @param add logical: if \code{TRUE}, add the heatmap to an existing plot
#'
#' @return NULL
#'
#' @seealso \code{\link{dv_court}}, \code{\link{dv_plot_new}}
#'
#' @examples
#' \dontrun{
#' x <- dv_read(dv_example_file(), insert_technical_timeouts = FALSE)
#'
#' library(dplyr)
#'
#' ## Example: attack frequency by zone, per team
#'
#' attack_rate <- plays(x) %>% dplyr::filter(skill == "Attack") %>%
#'   group_by(team, start_zone) %>% dplyr::summarize(n_attacks = n()) %>%
#'   mutate(rate = n_attacks/sum(n_attacks)) %>% ungroup
#'
#' ## add columns "x" and "y" for the x,y coordinates associated with the zones
#' attack_rate <- cbind(attack_rate, dv_xy(attack_rate$start_zone, end = "lower"))
#'
#' ## for team 2, these need to be on the top half of the diagram
#' tm2 <- attack_rate$team == teams(x)[2]
#' attack_rate[tm2, c("x", "y")] <- dv_xy(attack_rate$start_zone, end="upper")[tm2, ]
#'
#' ## plot it
#' dv_heatmap(attack_rate[, c("x", "y", "rate")], legend_title = "Attack rate")
#'
#' ## or, controlling the z-limits
#' dv_heatmap(attack_rate[, c("x", "y", "rate")], legend_title = "Attack rate", zlim = c(0, 1))
#'
#' ## add the court diagram
#' dv_court(labels = teams(x))
#'
#' ## sometimes you may need more control over the plot layout
#' ## set up a plot with 10% bottom/top margins and 20% left/right margins
#' ## showing the lower half of the court only
#' dv_plot_new(margins = c(0.05, 0.1, 0.05, 0.1), court = "lower")
#' ## add the heatmap
#' dv_heatmap(attack_rate[1:6, c("x", "y", "rate")], add = TRUE)
#' ## and the court diagram
#' dv_court(court = "lower")
#' 
#' }
#'
#' @export
dv_heatmap <- function(x, y, z, col, zlim, legend = TRUE, legend_title = NULL, legend_title_font = 1, legend_title_cex = 0.7, legend_cex = 0.7, legend_pos = c(0.8, 0.85, 0.25, 0.75), res, add = FALSE) {
    check_raster_available()
    assert_that(is.flag(add), !is.na(add))
    assert_that(is.flag(legend), !is.na(legend))
    if (missing(col)) col <- grDevices::colorRampPalette(c("#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#08519C"))(21)
    if (missing(zlim)) zlim <- NULL
    if (!missing(res)) {
        assert_that(is.numeric(res), res > 0)
        if (length(res) == 1) res <- c(res, res)
        ## res specified in m, but we work in plot units
        res <- res/3
    } else {
        res <- c(NA_real_, NA_real_)
    }
    noplot <- FALSE
    if (!inherits(x, "RasterLayer")) {
        if (!missing(y) && !missing(z)) {
            if (length(x) < 1) {
                noplot <- TRUE
                x <- raster::raster(matrix(1))
            } else {
                x <- raster::rasterFromXYZ(cbind(x, y, z), res = res)
            }
        } else {
            if (is.null(x) || nrow(x) < 1) {
                noplot <- TRUE
                x <- raster::raster(matrix(1))
            } else {
                x <- raster::rasterFromXYZ(x, res = res)
            }
        }
    }
    if (noplot) {
        if (!add) dv_plot_new(legend = legend)
    } else {
        if (!add) dv_plot_new(x, legend = legend)
        raster::plot(x, col = col, legend = FALSE, interpolate = FALSE, add = TRUE, zlim = zlim)
    }
    if (legend && (!noplot || !is.null(zlim))) {
        raster::plot(x, legend.only = TRUE, interpolate = FALSE,
                     col = col, zlim = zlim,
                     legend.args = list(text = legend_title, cex = legend_title_cex, side = 3, font = legend_title_font, line = 1, adj = 0), ## adj 0 = left-align
                     axis.args = list(cex.axis = legend_cex),
                     smallplot = legend_pos)
    }
}
