#' ggplot volleyball court
#' 
#' Volleyball court schematic suitable for adding to a ggplot
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
#' @param label_font_size numeric: the font size of the labels
#' @param label_colour string: colour to use for labels
#' @param court_colour string: colour to use for the court. If \code{NULL}, the court is only plotted with lines (no colour fill) and so the \code{figure_colour} will show through. The string "indoor" can be used as a shortcut to set the court colour to orange, figure colour to blue, and lines and labels to white (similar to the typical indoor court colour scheme)
#' @param figure_colour string: colour to set the figure background to. If \code{NULL}, the background colour of the theme will be used (white, by default)
#' @param background_only logical: if \code{TRUE}, only plot the background elements (including general plot attributes such as the theme)
#' @param foreground_only logical: if \code{TRUE}, only plot the foreground elements (grid lines, labels, etc)
#' @param ... : additional parameters passed to \code{ggplot2::theme_classic(...)}
#'
#' @return ggplot layer
#'
#' @seealso \code{\link{dv_xy}}, \code{\link{dv_xy2index}}, \code{\link{dv_index2xy}}, \code{\link{dv_flip_xy}}
#'
#' @examples
#' \dontrun{
#' x <- read_dv(dv_example_file(), insert_technical_timeouts=FALSE)
#'
#' library(ggplot2)
#' library(dplyr)
#' 
#' ## Example 1: attack frequency by zone, per team
#' 
#' attack_rate <- plays(x) %>% dplyr::filter(skill == "Attack") %>%
#'   group_by(team, start_zone) %>% dplyr::summarize(n_attacks=n()) %>%
#'   mutate(rate=n_attacks/sum(n_attacks)) %>% ungroup
#' 
#' ## add columns "x" and "y" for the x,y coordinates associated with the zones
#' attack_rate <- cbind(attack_rate, dv_xy(attack_rate$start_zone, end = "lower"))
#'
#' ## for team 2, these need to be on the top half of the diagram
#' tm2 <- attack_rate$team == teams(x)[2]
#' attack_rate[tm2, c("x", "y")] <- dv_xy(attack_rate$start_zone, end = "upper")[tm2, ]
#'
#' ## plot this
#' ggplot(attack_rate, aes(x, y, fill = rate)) + geom_tile() + ggcourt(labels = teams(x)) +
#'   scale_fill_gradient2(name = "Attack rate")
#'
#'
#' ## Example 2: controlling layering
#' ## use the background_only and foreground_only parameters to control the
#' ##   order of layers in a plot
#'
#' ggplot(attack_rate, aes(x, y, fill=rate)) +
#'   ## add the background court colours
#'   ggcourt(court_colour = "indoor", background_only = TRUE) +
#'   ## now the heatmap
#'   geom_tile() +
#'   ## and finally the grid lines and labels
#'   ggcourt(labels = teams(x), foreground_only = TRUE, court_colour = "indoor")
#'
#'
#' ## Example 3: map of starting and ending zones of attacks using arrows
#'
#' ## first tabulate attacks by starting and ending zone
#' attack_rate <- plays(x) %>% dplyr::filter(team == teams(x)[1] & skill == "Attack") %>%
#'   group_by(start_zone, end_zone) %>% tally() %>% ungroup
#'
#' ## convert counts to rates
#' attack_rate$rate <- attack_rate$n/sum(attack_rate$n)
#'
#' ## discard zones with zero attacks or missing location information
#' attack_rate <- attack_rate %>% dplyr::filter(rate>0 & !is.na(start_zone) & !is.na(end_zone))
#'
#' ## add starting x,y coordinates
#' attack_rate <- cbind(attack_rate, dv_xy(attack_rate$start_zone, end = "lower",
#'                                         xynames = c("sx","sy")))
#'
#' ## and ending x,y coordinates
#' attack_rate <- cbind(attack_rate, dv_xy(attack_rate$end_zone, end = "upper",
#'                                         xynames = c("ex","ey")))
#'
#' ## plot in reverse order so largest arrows are on the bottom
#' attack_rate <- attack_rate %>% dplyr::arrange(desc(rate))
#'
#' p <- ggplot(attack_rate, aes(x, y, col = rate)) + ggcourt(labels = c(teams(x)[1], ""))
#' for (n in 1:nrow(attack_rate))
#'     p <- p + geom_path(data = data.frame(x = c(attack_rate$sx[n], attack_rate$ex[n]),
#'                                          y = c(attack_rate$sy[n], attack_rate$ey[n]),
#'                                          rate = attack_rate$rate[n]),
#'                        aes(size = rate), lineend = "round",
#'                        arrow = arrow(length = unit(2, "mm"), type = "closed",
#'                                      angle = 20, ends = "last"))
#' p + scale_colour_gradient(name = "Attack rate") + guides(size = "none")
#' }
#' 
#' @export
ggcourt <- function(court = "full", show_zones = TRUE, labels = c("Serving team", "Receiving team"), as_for_serve = FALSE, show_zone_lines = TRUE, show_minor_zones = FALSE, grid_colour = "black", zone_colour = "grey70", minor_zone_colour = "grey80", fixed_aspect_ratio = TRUE, zone_font_size = 10, label_font_size = 12, label_colour = "black", court_colour = NULL, figure_colour = NULL, background_only = FALSE, foreground_only = FALSE, ...) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("The ggplot2 package needs to be installed for ggcourt to be useful")
    }
    court <- match.arg(tolower(court),c("full","lower","upper"))
    assert_that(is.flag(show_zones),!is.na(show_zones))
    assert_that(is.flag(show_minor_zones),!is.na(show_minor_zones))
    assert_that(is.flag(as_for_serve),!is.na(as_for_serve))
    assert_that(is.flag(fixed_aspect_ratio),!is.na(fixed_aspect_ratio))
    if (is.null(court_colour)) court_colour <- "none"
    if (is.null(figure_colour)) figure_colour <- "none"
    assert_that(is.string(court_colour))
    assert_that(is.string(figure_colour))
    assert_that(is.flag(background_only),!is.na(background_only))
    assert_that(is.flag(foreground_only),!is.na(foreground_only))
    if (tolower(court_colour) %eq% "indoor") {
        court_colour <- "#D98875"
        figure_colour <- "#26A9BD"
        grid_colour <- label_colour <- "white"
    }
    if (tolower(court_colour) %eq% "none") {
        cfill <- NULL
    } else {
        cfill <- ggplot2::annotate(geom = "rect", xmin = 0.5, xmax = 3.5, ymin = switch(court, upper = 3.5, 0.5), ymax = switch(court, lower = 3.5, 6.5), fill = court_colour)
    }
    ## horizontal grid lines
    hl <- data.frame(x=c(0.5,3.5),y=c(0.5,0.5,2.5,2.5,3.5,3.5,4.5,4.5,6.5,6.5),id=c(1,1,2,2,3,3,4,4,5,5))
    hl <- switch(court,
                 lower=hl[hl$y<4,],
                 upper=hl[hl$y>3,],
                 hl)
    hl <- ggplot2::geom_path(data=hl,ggplot2::aes_string(x="x",y="y",group="id"),colour=grid_colour,inherit.aes=FALSE)
    hlz <- data.frame(x=c(0.5,3.5),y=c(1.5,1.5,5.5,5.5),id=c(6,6,7,7))
    hlz <- switch(court,
                 lower=hlz[hlz$y<4,],
                 upper=hlz[hlz$y>3,],
                 hlz)
    hlz <- ggplot2::geom_path(data=hlz,ggplot2::aes_string(x="x",y="y",group="id"),colour=zone_colour,inherit.aes=FALSE)
    ## vertical grid lines
    vl <- data.frame(y=c(0.5,6.5),x=c(0.5,0.5,3.5,3.5),id=c(1,1,2,2))
    vl$y <- switch(court,
                   lower=mapvalues(vl$y,6.5,3.5),
                   upper=mapvalues(vl$y,0.5,3.5),
                   vl$y)
    vl <- ggplot2::geom_path(data=vl,ggplot2::aes_string(x="x",y="y",group="id"),colour=grid_colour,inherit.aes=FALSE)
    vlz <- data.frame(y=c(0.5,6.5),x=c(1.5,1.5,2.5,2.5),id=c(3,3,4,4))
    vlz$y <- switch(court,
                   lower=mapvalues(vlz$y,6.5,3.5),
                   upper=mapvalues(vlz$y,0.5,3.5),
                   vlz$y)
    vlz <- ggplot2::geom_path(data=vlz,ggplot2::aes_string(x="x",y="y",group="id"),colour=zone_colour,inherit.aes=FALSE)
    ## minor grid lines
    if (show_minor_zones) {
        hlm <- data.frame(x=c(0.5,3.5),y=c(1,1,1.5,1.5,2,2,2.5,2.5,3,3,4,4,4.5,4.5,5,5,5.5,5.5,6,6),id=c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10))
        hlm <- switch(court,
                      lower=hlm[hlm$y<4,],
                      upper=hlm[hlm$y>3,],
                      hlm)
        hlm <- ggplot2::geom_path(data=hlm,ggplot2::aes_string(x="x",y="y",group="id"),colour=minor_zone_colour,inherit.aes=FALSE)
        vlm <- data.frame(y=c(0.5,6.5),x=c(1,1,1.5,1.5,2,2,2.5,2.5,3,3),id=c(1,1,2,2,3,3,4,4,5,5))
        vlm$y <- switch(court,
                        lower=mapvalues(vlm$y,6.5,3.5),
                        upper=mapvalues(vlm$y,0.5,3.5),
                        vlm$y)
        vlm <- ggplot2::geom_path(data=vlm,ggplot2::aes_string(x="x",y="y",group="id"),colour=minor_zone_colour,inherit.aes=FALSE)
    }
    net <- ggplot2::geom_path(data=data.frame(x=c(0.25,3.75),y=c(3.5,3.5)),ggplot2::aes_string(x="x",y="y"),colour=grid_colour,size=2,inherit.aes=FALSE) ## net
    thm <- ggplot2::theme_classic(...)
    thm2 <- ggplot2::theme(axis.line = ggplot2::element_blank(),axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(),axis.ticks = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank())
    thm3 <- if (!tolower(figure_colour) %eq% "none") ggplot2::theme(panel.background = ggplot2::element_rect(fill = figure_colour)) else NULL
    out <- if (!foreground_only) list(cfill, thm, thm2, thm3) else list()
    if (!background_only) out <- c(out, list(net))
    if (fixed_aspect_ratio && !foreground_only) out <- c(out, list(ggplot2::coord_fixed()))
    if (show_minor_zones && !background_only) out <- c(out, list(hlm, vlm))
    if (show_zone_lines && !background_only) out <- c(out, list(hlz, vlz))
    if (!background_only) out <- c(out, list(hl,vl))
    if (!is.null(labels) && !background_only) {
        if (court %in% c("full","lower")) {
            ly <- if (as_for_serve) 0.1 else 0.4
            out <- c(out,ggplot2::annotate("text", x = 2, y = ly, label = labels[1], size = label_font_size*0.35278, colour = label_colour))
        }
        if (court %in% c("full","upper")) {
            lb <- if (court=="full") {
                     labels[2]
                 } else {
                     if (length(labels)==2) labels[2] else labels[1]
                 }
            ly <- if (as_for_serve) 6.9 else 6.6
            out <- c(out, ggplot2::annotate("text", x = 2, y = ly, label = lb, size = label_font_size*0.35278, colour = label_colour))
        }
    }
    if (show_zones && !background_only) {
        xoff <- if (as_for_serve) 0.5 else 0.4
        szx <- if (as_for_serve) c(3,1,2,1.5,2.5)+0.5 else c(3,3,2,1,1,2,1,2,3)
        szy <- if (as_for_serve) c(1,1,1,1,1)-0.25 else c(1,3,3,3,1,1,2,2,2)
        ezx <- 4-szx
        ezy <- 3+4-szy
        lb <- if (as_for_serve) c(1,5,6,7,9) else 1:9
        ## these need to be added one by one, otherwise doesn't work with e.g. facet_wrap plots
        if (court %in% c("full","lower")) {
            for (ii in seq_len(length(lb))) {
                out <- c(out, ggplot2::annotate("text", x=szx[ii]-xoff, y=szy[ii]-0.4, label=lb[ii], vjust="center", hjust="middle", fontface="italic", color=zone_colour, size = zone_font_size*0.35278))
            }
        }
        if (court %in% c("full","upper")) {
            for (ii in seq_len(length(lb))) {
                out <- c(out, ggplot2::annotate("text", x=ezx[ii]+xoff, y=ezy[ii]+0.4, label=lb[ii], vjust="center", hjust="middle", fontface="italic", color=zone_colour, size = zone_font_size*0.35278))
            }
        }
    }
    out
}

#' Grid index to x,y coordinate and vice-versa
#'
#' DataVolley uses a grid to represent positions on court (values in columns "start_coordinate", "mid_coordinate", and "end_coordinate" in the play-by-play data frame). These functions convert grid index values to x, y coordinates suitable for plotting, and vice-versa. For a description of the court dimensons and coordinates see \code{\link{ggcourt}}.
#' 
#' @param index integer: vector of grid indices. If missing, the entire grid will be returned. The row numbers match the grid indices
#' @param x numeric: x-coordinate. For \code{dv_index2xy} this can be a two-column matrix or data.frame containing x and y
#' @param y numeric: y-coordinate
#'
#' @return for dv_index2xy, a data.frame with columns "x" and "y"; for dv_xy2index a vector of integer values
#'
#' @seealso \code{\link{ggcourt}}, \code{\link{dv_xy}}, \code{\link{dv_flip_xy}}
#'
#' @examples
#'
#' ## positions (zones) 1 and 3 are at x, y coordinates c(3, 1) and c(2, 3) respectively
#'
#' ## their grid indices:
#' dv_xy2index(c(3, 2), c(1, 3))
#'
#' @export
dv_index2xy <- function(index) {
    ##cxy <- expand.grid(x=seq(from=3*(1-10.5)/79+0.5, to=3*(100-10.5)/79+0.5, length.out=100), y=seq(from=3*(1-10.5)/40.5+0.5, to=3*(101-10.5)/40.5+0.5, length.out=101), KEEP.OUT.ATTRS=FALSE)
    binx <- dv_xy_xbins()
    biny <- dv_xy_ybins()
    ## need to add half a bin to get cell centres
    binx <- binx+(diff(binx[1:2])/2)
    biny <- biny+(diff(biny[1:2])/2)
    cxy <- expand.grid(x=binx, y=biny, KEEP.OUT.ATTRS=FALSE)
    if (missing(index)) {
        cxy
    } else {
        assert_that(is.numeric(index))
        index[index<1] <- NA
        cxy[index,]
    }
}

## internal functions
## these give the lower-left coordinate of the grid cells in the xy grid
dv_xy_xbins <- function() 0.5+(seq_len(100)-11)/80*3.0
dv_xy_ybins <- function() 0.5+(seq_len(101)-11)/81*6.0


#' @rdname dv_index2xy
#' @export
dv_xy2index <- function(x, y) {
    if (missing(y) && ncol(x)>1) {
        y <- x[,2]
        x <- x[,1]
    }
    assert_that(is.numeric(x))
    assert_that(is.numeric(y))
    ## define cells (these are LEFT edges) in plot x, y space
    ##binx <- seq(from=3*(1-10.5)/79+0.5, to=3*(100-10.5)/79+0.5, length.out=100)
    binx <- dv_xy_xbins()
    binx[1] <- -Inf ## so that anything to the left of the first cell is put in the first cell
    binx <- c(binx, Inf) ## and anything beyond the last cell is put into the last cell
    ##biny <- seq(from=3*(1-10.5)/40.5+0.5, to=3*(101-10.5)/40.5+0.5, length.out=101)
    biny <- dv_xy_ybins()
    biny[1] <- -Inf
    biny <- c(biny, Inf)
    xi <- .bincode(x, binx, right=FALSE)
    yi <- .bincode(y, biny, right=FALSE)
    as.integer(xi+(yi-1)*(length(binx)-1))
}

#' Attack cones to x, y coordinates
#'
#' @param start_zones integer: starting zone of attack
#' @param end_cones integer: cone of attack
#' @param end string: use the "lower" or "upper" part of the figure
#' @param xynames character: names to use for the x and y columns of the returned data.frame
#' @param as string: either "points" or "polygons" (see Value, below)
#' @param force_center_zone logical: a vector indicating the attacks that should be treated as center zone attacks regardless of their start_zone value (e.g. by the setter). If \code{FALSE}, the start_zone value will be used. If provided as a single scalar value, this will be applied to all attacks
#'
#' @return a tibble (NOT a data.frame) with columns "x" and "y" (or other names if specified in \code{xynames}). If \code{as} is "polygons", the columns will be lists, because each polygon will have four x- and y-coordinates
#'
#' @seealso \code{\link{ggcourt}}, \code{\link{dv_flip_xy}}, \code{\link{dv_xy2index}}, \code{\link{dv_index2xy}}, \code{\link{dv_xy}}
#'
#' @examples
#' \dontrun{
#' ## attacks from left side (zone 4) to cones 1-7
#' 
#' ## plot as line segments
#' cxy <- dv_cone2xy(4, 1:7)
#' ## add starting coordinate for zone 4
#' cxy <- cbind(dv_xy(4), cxy)
#' ggplot(cxy, aes(x, y, xend=ex, yend=ey)) + geom_segment() + ggcourt()
#'
#' ## plot as polygons
#' cxy <- dv_cone2xy(4, 1:7, as = "polygons")
#'
#' ## this returns coordinates as list columns, unpack these to use with ggplot
#' ##  also add an identifier for each polygon
#' cxy <- data.frame(x = unlist(cxy$ex), y = unlist(cxy$ey),
#'                   id = unlist(lapply(1:nrow(cxy), rep, 4)))
#' ggplot(cxy, aes(x, y, group = id, fill = as.factor(id))) + geom_polygon() +
#'    ggcourt()
#' }
#' @export
dv_cone2xy <- function(start_zones, end_cones, end = "upper", xynames = c("ex", "ey"), as = "points", force_center_zone = FALSE) {
    end <- match.arg(tolower(end), c("lower", "upper"))
    as <- match.arg(tolower(as), c("points", "polygons"))
    as_points <- as == "points"
    if (as_points) {
        outx <- outy <- rep(NA_real_, length(start_zones))
    } else {
        outx <- outy <- vector("list", length(start_zones))
    }
    if (length(force_center_zone) == 1) force_center_zone <- rep(force_center_zone, length(start_zones))
    idxL <- start_zones %in% c(4, 7, 5) & end_cones %in% 1:7
    idxR <- start_zones %in% c(2, 9, 1) & end_cones %in% 1:7
    idxC <- (start_zones %in% c(3, 8, 6) | force_center_zone) & end_cones %in% 1:8

    if (any(idxL)) {
        ex2 <- c(0.85, 1.35, 1.85, 2.85, 3.5, 3.5, 3.5)
        ex1 <- c(0.5, ex2[1:6])
        if (as_points) {
            ex <- c((ex1[1:4]+ex2[1:4])/2, 3.5, 3.5, 3.5)
            ey <- c(rep(6.5, 5), 5.3, 4.15)
            if (end == "lower") {
                temp <- dv_flip_xy(ex, ey)
                ex <- temp$x
                ey <- temp$y
            }
            outx[idxL] <- ex[end_cones[idxL]]
            outy[idxL] <- ey[end_cones[idxL]]
        } else {
            ## polygons
            sx1 <- c(0.5, rep(0.9, 6))
            sx2 <- c(rep(0.9, 4), 3.5, 0.9, 0.9)
            sy1 <- rep(3.5, 7)
            sy2 <- c(rep(3.5, 4), 5.65, 3.5, 3.5)
            ey1 <- c(rep(6.5, 5), 5.65, 4.8)
            ey2 <- c(rep(6.5, 5), 4.8, 3.5)
            if (end == "lower") {
                temp <- dv_flip_xy(sx1, sy1)
                sx1 <- temp$x; sy1 <- temp$y
                temp <- dv_flip_xy(sx2, sy2)
                sx2 <- temp$x; sy2 <- temp$y
                temp <- dv_flip_xy(ex1, ey1)
                ex1 <- temp$x; ey1 <- temp$y
                temp <- dv_flip_xy(ex2, ey2)
                ex2 <- temp$x; ey2 <- temp$y
            }
            outx[idxL] <- lapply(end_cones[idxL], function(ec) c(sx1[ec], ex1[ec], ex2[ec], sx2[ec]))
            outy[idxL] <- lapply(end_cones[idxL], function(ec) c(sy1[ec], ey1[ec], ey2[ec], sy2[ec]))
        }
    }
    ## idxR
    if (any(idxR)) {
        ## do as for L, then flip
        temp <- dv_cone2xy(start_zones = 4, end_cones = end_cones[idxR], end = end, as = as, force_center_zone = FALSE)
        outy[idxR] <- temp$ey
        if (as_points) {
            outx[idxR] <- dv_flip_x(temp$ex)
        } else {
            outx[idxR] <- lapply(temp$ex, dv_flip_x)
        }
    }
    ## idxC
    if (any(idxC)) {
        if (as_points) {
            ex <- c(1, 0.5+3/5/2+3/5*0:4, 3, 2)
            ey <- c(4.5, rep(5.85, 5), 4.5, 4.75)
            if (end == "lower") {
                temp <- dv_flip_xy(ex, ey)
                ex <- temp$x
                ey <- temp$y
            }
            outx[idxC] <- ex[end_cones[idxC]]
            outy[idxC] <- ey[end_cones[idxC]]
        } else {
            sx1 <- c(0.5, 0.5, 1.1, 1.7, 2.3, 2.9, 2.0, 2.0)
            sx2 <- c(2.0, 1.1, 1.7, 2.3, 2.9, 3.5, 3.5, 2.0)
            ex1 <- c(0.5, 0.5, 1.1, 1.7, 2.3, 2.9, 2.3, 1.7)
            ex2 <- c(1.7, 1.1, 1.7, 2.3, 2.9, 3.5, 3.5, 2.3)
            sy1 <- c(3.5, rep(5.15, 5), 3.5, 3.5)
            sy2 <- sy1
            ey1 <- c(5.15, rep(6.5, 5), 5.15, 5.15)
            ey2 <- ey1
            if (end == "lower") {
                temp <- dv_flip_xy(sx1, sy1)
                sx1 <- temp$x; sy1 <- temp$y
                temp <- dv_flip_xy(sx2, sy2)
                sx2 <- temp$x; sy2 <- temp$y
                temp <- dv_flip_xy(ex1, ey1)
                ex1 <- temp$x; ey1 <- temp$y
                temp <- dv_flip_xy(ex2, ey2)
                ex2 <- temp$x; ey2 <- temp$y
            }
            outx[idxC] <- lapply(end_cones[idxC], function(ec) c(sx1[ec], ex1[ec], ex2[ec], sx2[ec]))
            outy[idxC] <- lapply(end_cones[idxC], function(ec) c(sy1[ec], ey1[ec], ey2[ec], sy2[ec]))
        }
    }
    if (as_points) {
        setNames(tibble(x = outx, y = outy), xynames)
    } else {
        idxNA <- !idxL & !idxR & !idxC
        outx[idxNA] <- list(rep(NA_real_, 4))
        outy[idxNA] <- list(rep(NA_real_, 4))
        setNames(tibble(x = outx, y = outy), xynames)
    }
}

#' Court zones to x, y coordinates
#' 
#' Generate x and y coordinates for plotting, from DataVolley numbered zones 
#'
#' For a description of the court dimensions and coordinates used for plotting, see \code{\link{ggcourt}}
#' 
#' @param zones numeric: zones numbers 1-9 to convert to x and y coordinates
#' @param end string: use the "lower" or "upper" part of the figure
#' @param xynames character: names to use for the x and y columns of the returned data.frame
#' @param as_for_serve logical: if TRUE, treat positions as for serving. Only zones 1,5,6,7,9 are meaningful in this case
#' @param subzones character: if supplied, coordinates will be adjusted for subzones. Values other than "A" to "D" will be ignored
#'
#' @return data.frame with columns "x" and "y" (or other names if specified in \code{xynames})
#'
#' @seealso \code{\link{ggcourt}}, \code{\link{dv_flip_xy}}, \code{\link{dv_xy2index}}, \code{\link{dv_index2xy}}, \code{\link{dv_cone2xy}}
#'
#' @examples
#' \dontrun{
#' x <- read_dv(dv_example_file(), insert_technical_timeouts = FALSE)
#'
#' library(ggplot2)
#' library(dplyr)
#' 
#' ## Example 1: attack frequency by zone, per team
#' 
#' attack_rate <- plays(x) %>% dplyr::filter(skill == "Attack") %>%
#'   group_by(team, start_zone) %>% dplyr::summarize(n_attacks = n()) %>%
#'   mutate(rate = n_attacks/sum(n_attacks)) %>% ungroup
#' 
#' ## add columns "x" and "y" for the x, y coordinates associated with the zones
#' attack_rate <- cbind(attack_rate, dv_xy(attack_rate$start_zone, end = "lower"))
#'
#' ## for team 2, these need to be on the top half of the diagram
#' tm2 <- attack_rate$team == teams(x)[2]
#' attack_rate[tm2, c("x", "y")] <- dv_xy(attack_rate$start_zone, end = "upper")[tm2, ]
#'
#' ## plot this
#' ggplot(attack_rate, aes(x, y, fill = rate)) + geom_tile() + ggcourt(labels = teams(x)) +
#'   scale_fill_gradient2(name = "Attack rate")
#' 
#'
#' ## Example 2: map of starting and ending zones of attacks using arrows
#'
#' ## first tabulate attacks by starting and ending zone
#' attack_rate <- plays(x) %>% dplyr::filter(team == teams(x)[1] & skill == "Attack") %>%
#'   group_by(start_zone, end_zone) %>% tally() %>% ungroup
#'
#' ## convert counts to rates
#' attack_rate$rate <- attack_rate$n/sum(attack_rate$n)
#'
#' ## discard zones with zero attacks or missing location information
#' attack_rate <- attack_rate %>% dplyr::filter(rate>0 & !is.na(start_zone) & !is.na(end_zone))
#'
#' ## add starting x,y coordinates
#' attack_rate <- cbind(attack_rate,
#'      dv_xy(attack_rate$start_zone, end = "lower", xynames = c("sx","sy")))
#'
#' ## and ending x,y coordinates
#' attack_rate <- cbind(attack_rate,
#'      dv_xy(attack_rate$end_zone, end = "upper", xynames = c("ex","ey")))
#'
#' ## plot in reverse order so largest arrows are on the bottom
#' attack_rate <- attack_rate %>% dplyr::arrange(desc(rate))
#'
#' p <- ggplot(attack_rate,aes(x,y,col = rate)) + ggcourt(labels = c(teams(x)[1],""))
#' for (n in 1:nrow(attack_rate))
#'     p <- p + geom_path(data = data.frame(x = c(attack_rate$sx[n], attack_rate$ex[n]),
#'                                        y = c(attack_rate$sy[n],attack_rate$ey[n]),
#'                                        rate = attack_rate$rate[n]),
#'         aes(size = rate), lineend = "round", arrow = arrow(ends = "last", type = "closed"))
#' p + scale_fill_gradient(name = "Attack rate") + guides(size = "none")
#' }
#' @export
dv_xy <- function(zones, end = "lower", xynames = c("x", "y"), as_for_serve = FALSE, subzones) {
    end <- match.arg(tolower(end), c("lower", "upper"))
    use_subz <- FALSE
    if (!missing(subzones) && !is.null(subzones)) {
        assert_that(is.character(subzones))
        use_subz <- TRUE
        subzones <- toupper(subzones)
        subzones[!subzones %in% c("A", "B", "C", "D")] <- NA_character_
    }
    ## define zones and their corresponding coordinates
    start_zones <- 1:9 ## lower part of figure
    ##szx <- if (as_for_serve) c(3, NA, NA, NA, 1, 2, 1.5, NA, 2.5) else c(3, 3, 2, 1, 1, 2, 1, 2, 3)
    szx <- if (as_for_serve) c(3.2, NA, NA, NA, 0.8, 2, 1.4, NA, 2.6) else c(3, 3, 2, 1, 1, 2, 1, 2, 3)
    szy <- if (as_for_serve) c(1, NA, NA, NA, 1, 1, 1, NA, 1)-0.5 else c(1, 3, 3, 3, 1, 1, 2, 2, 2)
    end_zones <- 1:9 ## upper part of figure
    ezx <- 4-szx
    ezy <- 3+4-szy

    zones[!zones %in% 1:9] <- NA
    out <- switch(end,
                  lower = data.frame(x = mapvalues(zones, start_zones, szx, warn_missing = FALSE), y = mapvalues(zones, start_zones, szy, warn_missing = FALSE)),
                  upper = data.frame(x = mapvalues(zones, end_zones, ezx, warn_missing = FALSE), y = mapvalues(zones, end_zones, ezy, warn_missing = FALSE)),
                  stop("unexpected end, should be \"lower\" or \"upper\"")
                  )
    if (use_subz) {
        adj <- rep(0, nrow(out))
        adj[subzones %in% c("A", "B")] <- 0.25
        adj[subzones %in% c("C", "D")] <- -0.25
        out$x <- if (end %eq% "lower") out$x + adj else out$x - adj
        adj <- rep(0, nrow(out))
        adj[subzones %in% c("A", "D")] <- -0.25
        adj[subzones %in% c("B", "C")] <- 0.25
        out$y <- if (end %eq% "lower") out$y + adj else out$y - adj
    }
    names(out) <- xynames
    out
}


#' @title Court zones to x, y coordinates
#' @description Generate x and y coordinates for plotting, from DataVolley numbered zones 
#' @param zones numeric: zones numbers 1-9 to convert to x and y coordinates
#' @param end string: use the "lower" or "upper" part of the figure
#' @param xynames character: names to use for the x and y columns of the returned data.frame
#' @param as_for_serve logical: if TRUE, treat positions as for serving. Only zones 1,5,6,7,9 are meaningful in this case
#' @return data.frame with columns "x" and "y" (or other names if specified in \code{xynames})
#' @name ggxy-deprecated
#' @usage ggxy(zones, end="lower", xynames=c("x", "y"), as_for_serve=FALSE)
#' @keywords internal
#' @seealso \code{\link{datavolley-deprecated}}
NULL



#' @rdname datavolley-deprecated
#' @section \code{ggxy}:
#' For \code{ggxy}, use \code{\link{dv_xy}}.
#'
#' @export
ggxy <- function(zones, end="lower", xynames=c("x", "y"), as_for_serve=FALSE) {
    .Deprecated("dv_xy", package="datavolley")
    dv_xy(zones, end=end, xynames=xynames, as_for_serve=as_for_serve)
}

#' Flip the x,y court coordinates
#'
#' This is a convenience function that will transform coordinates from the top half of the court to the bottom, or vice-verse.
#' 
#' @param x numeric: x-coordinate. For \code{dv_flip_xy} this can be a two-column matrix or data.frame containing x and y
#' @param y numeric: y-coordinate
#' @param index integer: grid index value
#'
#' @return transformed coordinates or grid index
#'
#' @seealso \code{\link{ggcourt}}, \code{\link{dv_xy}}, \code{\link{dv_xy2index}}, \code{\link{dv_index2xy}}
#'
#' @examples
#' \dontrun{
#'  x <- read_dv(dv_example_file(), insert_technical_timeouts=FALSE)
#'  library(ggplot2)
#'  library(dplyr)
#'
#' ## attack rate by zone (both teams combined)
#' attack_rate <- plays(x) %>% dplyr::filter(skill=="Attack") %>%
#'    group_by(team, start_zone) %>% dplyr::summarize(n_attacks=n()) %>%
#'    mutate(rate=n_attacks/sum(n_attacks)) %>% ungroup
#'
#' ## add columns "x" and "y" for the x,y coordinates associated with the zones
#' attack_rate <- cbind(attack_rate, dv_xy(attack_rate$start_zone, end="lower"))
#'
#' ## plot this
#' ggplot(attack_rate, aes(x, y, fill=rate)) + geom_tile() + ggcourt(labels=teams(x)) +
#'      scale_fill_gradient2(name="Attack rate")
#'
#' ## or, plot at the other end of the court
#' attack_rate <- attack_rate %>% mutate(x=dv_flip_x(x), y=dv_flip_y(y))
#'
#' ggplot(attack_rate, aes(x, y, fill=rate)) + geom_tile() + ggcourt(labels=teams(x)) +
#'      scale_fill_gradient2(name="Attack rate")
#' }
#'
#' @export
dv_flip_xy <- function(x, y) {
    if (missing(y) && ncol(x)>1) {
        y <- x[,2]
        x <- x[,1]
    }
    data.frame(x=4-x, y=7-y)
}

#' @rdname dv_flip_xy
#' @export
dv_flip_x <- function(x) 4-x

#' @rdname dv_flip_xy
#' @export
dv_flip_y <- function(y) 7-y

#' @rdname dv_flip_xy
#' @export
dv_flip_index <- function(index) 10101-index


#' The polygon coordinates for attack cones
#'
#' @param zone string: one of "L", "R", "M"
#' @param end string: use the "lower" or "upper" part of the figure
#' @param extended logical: if \code{FALSE}, the polygons will only cover the actual court area; if \code{TRUE}, they will be extended to cover the court periphery as well
#'
#' @return A data.frame with columns \code{cone_number}, \code{x}, \code{y}
#'
#' @examples
#' \dontrun{
#'  library(ggplot2)
#'  cxy <- dv_cone_polygons("M")
#'  ggplot(cxy, aes(x, y, group = cone_number, fill = as.factor(cone_number))) +
#'    geom_polygon() + ggcourt()
#' }
#' @export
dv_cone_polygons <- function(zone, end = "upper", extended = FALSE) {
    if (is.numeric(zone)) {
        if (zone %in% c(2, 9, 1)) {
            zone <- "L"
        } else if (zone %in% c(3, 8, 6)) {
            zone <- "M"
        } else if (zone %in% c(4, 7, 5)) {
            zone <- "L"
        }
    }
    zone <- match.arg(toupper(zone), c("L", "M", "C", "R"))
    end <- match.arg(tolower(end), c("lower", "upper"))
    assert_that(is.flag(extended), !is.na(extended))
    xmax <- 3.5 + extended*2.5
    xmin <- 0.5 - extended*2.5
    ymax <- 6.5 + extended*2.5
    if (zone == "L") {
        xextra <- function(x0) x0 + extended*(x0-0.9)/3*(xmax-3.5)
        yextra <- function(y0) y0 + extended*(y0-3.5)/(3.5-0.9)*(xmax-3.5)
        ex2 <- c(xextra(0.85), xextra(1.35), xextra(1.85), xextra(2.85), xmax, xmax, xmax)
        ex1 <- c(xmin, ex2[1:6])
        ## polygons
        sx1 <- c(xmin, rep(0.9, 6))
        sx2 <- c(rep(0.9, 4), xmax, 0.9, 0.9)
        sy1 <- rep(3.5, 7)
        sy2 <- c(rep(3.5, 4), yextra(5.65), 3.5, 3.5)
        ey1 <- c(rep(ymax, 5), yextra(5.65), yextra(4.8))
        ey2 <- c(rep(ymax, 5), 4.8, 3.5)
        outx <- lapply(1:7, function(ec) c(sx1[ec], ex1[ec], ex2[ec], sx2[ec]))
        outy <- lapply(1:7, function(ec) c(sy1[ec], ey1[ec], ey2[ec], sy2[ec]))
    } else if (zone == "R") {
        ## do as for L, then flip
        temp <- dv_cone_polygons(zone = "L", end = end, extended = extended)
        temp$x <- dv_flip_x(temp$x)
        return(temp)
    } else if (zone %in% c("M", "C")) {
        sx1 <- c(xmin, xmin, 1.1, 1.7, 2.3, 2.9, 2.0, 2.0)
        sx2 <- c(2.0, 1.1, 1.7, 2.3, 2.9, xmax, xmax, 2.0)
        ex1 <- c(xmin, xmin, 1.1, 1.7, 2.3, 2.9, 2.3, 1.7)
        ex2 <- c(1.7, 1.1, 1.7, 2.3, 2.9, xmax, xmax, 2.3)
        sy1 <- c(3.5, rep(5.15, 5), 3.5, 3.5)
        sy2 <- sy1
        ey1 <- c(5.15, rep(ymax, 5), 5.15, 5.15)
        ey2 <- ey1
        outx <- lapply(1:8, function(ec) c(sx1[ec], ex1[ec], ex2[ec], sx2[ec]))
        outy <- lapply(1:8, function(ec) c(sy1[ec], ey1[ec], ey2[ec], sy2[ec]))
    }
    out <- do.call(rbind, lapply(seq_along(outx), function(ci) data.frame(cone_number = ci, x = outx[[ci]], y = outy[[ci]])))
    if (end == "lower") {
        out$x <- dv_flip_x(out$x)
        out$y <- dv_flip_y(out$y)
    }
    out
}



#' Convert x, y coordinates to cones
#'
#' @param x numeric: the x coordinate
#' @param y numeric: the y coordinate. If \code{y} is \code{NULL}, \code{x} will be treated as a grid index (see \code{\link{dv_index2xy}})
#' @param start_zones numeric or character: the starting zone of each row (values 1-9, or "L", "M", "R")
#' @param force_center_zone logical: a vector indicating the rows that should be treated as center zone attacks regardless of their start_zone value (e.g. attacks by the setter). If \code{FALSE}, the start_zone value will be used. If provided as a single scalar value, this will be applied to all attacks
#'
#' @return A numeric vector giving the cone number
#'
#' @seealso \code{\link{dv_xy2index}}, \code{\link{dv_index2xy}}, \code{\link{dv_cone2xy}}
#'
#' @examples
#' \dontrun{
#'
#' ## a bunch of random points on and around the court
#' idx <- round(runif(100, min = 1, max = 10000))
#'
#' ## convert to cones, assuming a start_zone of "L"
#' cn <- dv_xy2cone(x = idx, start_zones = "M")
#'
#' ## generate the cone polygons for reference
#' cxy <- dv_cone_polygons("M")
#' cxyl <- dv_cone_polygons("M", end = "lower")
#'
#' ## plot
#' ggplot(cxy, aes(x, y, group = cone_number, fill = as.factor(cone_number))) +
#'   ## the cone polygons
#'   geom_polygon() + geom_polygon(data = cxyl) +
#'   ggcourt(labels = NULL) +
#'   ## and our points
#'   geom_point(data = dv_index2xy(idx) %>% mutate(cone_number = cn), shape = 21,
#'              colour = "black", size = 2)
#'
#' ## the points shoud be coloured the same as the cone polygons
#' }
#'
#' @export
dv_xy2cone <- function(x, y = NULL, start_zones, force_center_zone = FALSE) {
    if (is.null(y)) {
        xy <- dv_index2xy(x)
        x <- xy$x
        y <- xy$y
    }
    if (is.numeric(start_zones)) {
        temp <- rep(NA_character_, length(start_zones))
        temp[start_zones %in% c(2, 9, 1)] <- "R"
        temp[start_zones %in% c(3, 8, 6)] <- "M"
        temp[start_zones %in% c(4, 7, 5)] <- "L"
        start_zones <- temp
    }
    assert_that(is.character(start_zones))
    start_zones <- toupper(start_zones)
    assert_that(all(start_zones %in% c("R", "L", "M", "C", NA_character_)))
    out <- rep(NA_integer_, length(x))
    ## do point-in-polygon test for each polygon
    for (end in c("lower", "upper")) {
        for (z in c("L", "R", "M")) {
            idx <- which(is.na(out) & start_zones %eq% z)
            if (length(idx) > 0) {
                this_cpp <- dv_cone_polygons(z, end = end, extended = TRUE)
                for (ppi in unique(this_cpp$cone_number)) {
                    this_pp <- this_cpp[this_cpp$cone_number %eq% ppi, ]
                    this_pp <- rbind(this_pp, this_pp[1, ])
                    cidx <- sp::point.in.polygon(x[idx], y[idx], pol.x = this_pp$x, pol.y = this_pp$y)
                    cidx <- which(cidx > 0)
                    out[idx[cidx]] <- ppi
                }
            }
        }
    }
    out
}


##library(dplyr)
##cxy <- bind_rows(lapply(c("L", "M", "R"), function(z) dv_cone_polygons(z) %>% mutate(end = "upper", zone = z))) %>%
##    bind_rows(bind_rows(lapply(c("L", "M", "R"), function(z) dv_cone_polygons(z, end = "lower") %>% mutate(end = "lower", zone = z))))
##ggplot(cxy, aes(x, y, group = cone_number, fill = as.factor(cone_number))) + geom_polygon() + ggcourt() + facet_wrap(~end + zone)
