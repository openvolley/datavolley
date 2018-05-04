#' Add volleyball court schematic to a ggplot
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
#' @param ... : additional parameters passed to \code{ggplot2::theme_classic(...)}
#'
#' @return ggplot layer
#'
#' @seealso \code{\link{ggxy}}
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
#' attack_rate <- plays(x) %>% dplyr::filter(skill=="Attack") %>%
#'   group_by(team, start_zone) %>% dplyr::summarize(n_attacks=n()) %>%
#'   mutate(rate=n_attacks/sum(n_attacks)) %>% ungroup
#' 
#' ## add columns "x" and "y" for the x,y coordinates associated with the zones
#' attack_rate <- cbind(attack_rate, ggxy(attack_rate$start_zone, end="lower"))
#'
#' ## for team 2, these need to be on the top half of the diagram
#' tm2 <- attack_rate$team==teams(x)[2]
#' attack_rate[tm2, c("x", "y")] <- ggxy(attack_rate$start_zone, end="upper")[tm2, ]
#'
#' ## plot this
#' ggplot(attack_rate, aes(x, y, fill=rate)) + geom_tile() + ggcourt(labels=teams(x)) +
#'   scale_fill_gradient2(name="Attack rate")
#' 
#'
#' ## Example 2: map of starting and ending zones of attacks using arrows
#'
#' ## first tabulate attacks by starting and ending zone
#' attack_rate <- plays(x) %>% dplyr::filter(team==teams(x)[1] & skill=="Attack") %>%
#'   group_by(start_zone, end_zone) %>% tally() %>% ungroup
#'
#' ## convert counts to rates
#' attack_rate$rate <- attack_rate$n/sum(attack_rate$n)
#'
#' ## discard zones with zero attacks or missing location information
#' attack_rate <- attack_rate %>% dplyr::filter(rate>0 & !is.na(start_zone) & !is.na(end_zone))
#'
#' ## add starting x,y coordinates
#' attack_rate <- cbind(attack_rate, ggxy(attack_rate$start_zone, end="lower", xynames=c("sx","sy")))
#'
#' ## and ending x,y coordinates
#' attack_rate <- cbind(attack_rate, ggxy(attack_rate$end_zone, end="upper", xynames=c("ex","ey")))
#'
#' ## plot in reverse order so largest arrows are on the bottom
#' attack_rate <- attack_rate %>% dplyr::arrange(desc(rate))
#'
#' p <- ggplot(attack_rate,aes(x,y,col=rate)) + ggcourt(labels=c(teams(x)[1],""))
#' for (n in 1:nrow(attack_rate))
#'     p <- p + geom_path(data=data.frame(x=c(attack_rate$sx[n], attack_rate$ex[n]),
#'                                        y=c(attack_rate$sy[n],attack_rate$ey[n]),
#'                                        rate=attack_rate$rate[n]),
#'         aes(size=rate), lineend="round", arrow=arrow(ends="last", type="closed"))
#' p + scale_fill_gradient(name="Attack rate") + guides(size="none")
#' }
#' @export
ggcourt <- function(court="full", show_zones=TRUE, labels=c("Attacking team","Receiving team"), as_for_serve=FALSE, show_zone_lines=TRUE, show_minor_zones=FALSE, grid_colour="black", zone_colour="grey70", minor_zone_colour="grey80", ...) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("The ggplot2 package needs to be installed for ggcourt to be useful")
    }    
    court <- match.arg(tolower(court),c("full","lower","upper"))
    assert_that(is.flag(show_zones),!is.na(show_zones))
    assert_that(is.flag(show_minor_zones),!is.na(show_minor_zones))
    assert_that(is.flag(as_for_serve),!is.na(as_for_serve))
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
    thm2 <- ggplot2::theme(axis.line=ggplot2::element_blank(),axis.text.x=ggplot2::element_blank(), axis.text.y=ggplot2::element_blank(),axis.ticks=ggplot2::element_blank(), axis.title.x=ggplot2::element_blank(), axis.title.y=ggplot2::element_blank())
    out <- list(net,thm,thm2)
    if (show_minor_zones) out <- c(out, list(hlm, vlm))
    if (show_zone_lines) out <- c(out, list(hlz, vlz))
    out <- c(out, list(hl,vl))
    if (!is.null(labels)) {
        if (court %in% c("full","lower")) {
            ly <- if (as_for_serve) 0.1 else 0.4
            out <- c(out,ggplot2::annotate("text",x=2,y=ly,label=labels[1]))
        }
        if (court %in% c("full","upper")) {
            lb <- if (court=="full") {
                     labels[2]
                 } else {
                     if (length(labels)==2) labels[2] else labels[1]
                 }
            ly <- if (as_for_serve) 6.9 else 6.6
            out <- c(out,ggplot2::annotate("text",x=2,y=ly,label=lb))
        }
    }
    if (show_zones) {
        xoff <- if (as_for_serve) 0.5 else 0.4
        szx <- if (as_for_serve) c(3,1,2,1.5,2.5)+0.5 else c(3,3,2,1,1,2,1,2,3)
        szy <- if (as_for_serve) c(1,1,1,1,1)-0.25 else c(1,3,3,3,1,1,2,2,2)
        ezx <- 4-szx
        ezy <- 3+4-szy
        lb <- if (as_for_serve) c(1,5,6,7,9) else 1:9
        if (court %in% c("full","lower"))        
            out <- c(out,ggplot2::annotate("text",x=szx+xoff*rep(-1,length(lb)),y=szy+0.4*rep(-1,length(lb)),label=lb,vjust="center",hjust="middle",fontface="italic",color=zone_colour))
        if (court %in% c("full","upper"))
            out <- c(out,ggplot2::annotate("text",x=ezx+xoff*rep(1,length(lb)),y=ezy+0.4*rep(1,length(lb)),label=lb,vjust="center",hjust="middle",fontface="italic",color=zone_colour))
    }
    out
}


#' Create x and y coordinates for plotting, from DataVolley numbered zones 
#'
#' @param zones numeric: zones numbers 1-9 to convert to x and y coordinates
#' @param end string: use the "lower" or "upper" part of the figure
#' @param xynames character: names to use for the x and y columns of the returned data.frame
#' @param as_for_serve logical: if TRUE, treat positions as for serving. Only zones 1,5,6,7,9 are meaningful in this case
#'
#' @return data.frame with x and y components
#'
#' @seealso \code{\link{ggcourt}}
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
#' attack_rate <- plays(x) %>% dplyr::filter(skill=="Attack") %>%
#'   group_by(team, start_zone) %>% dplyr::summarize(n_attacks=n()) %>%
#'   mutate(rate=n_attacks/sum(n_attacks)) %>% ungroup
#' 
#' ## add columns "x" and "y" for the x,y coordinates associated with the zones
#' attack_rate <- cbind(attack_rate, ggxy(attack_rate$start_zone, end="lower"))
#'
#' ## for team 2, these need to be on the top half of the diagram
#' tm2 <- attack_rate$team==teams(x)[2]
#' attack_rate[tm2, c("x", "y")] <- ggxy(attack_rate$start_zone, end="upper")[tm2, ]
#'
#' ## plot this
#' ggplot(attack_rate, aes(x, y, fill=rate)) + geom_tile() + ggcourt(labels=teams(x)) +
#'   scale_fill_gradient2(name="Attack rate")
#' 
#'
#' ## Example 2: map of starting and ending zones of attacks using arrows
#'
#' ## first tabulate attacks by starting and ending zone
#' attack_rate <- plays(x) %>% dplyr::filter(team==teams(x)[1] & skill=="Attack") %>%
#'   group_by(start_zone, end_zone) %>% tally() %>% ungroup
#'
#' ## convert counts to rates
#' attack_rate$rate <- attack_rate$n/sum(attack_rate$n)
#'
#' ## discard zones with zero attacks or missing location information
#' attack_rate <- attack_rate %>% dplyr::filter(rate>0 & !is.na(start_zone) & !is.na(end_zone))
#'
#' ## add starting x,y coordinates
#' attack_rate <- cbind(attack_rate, ggxy(attack_rate$start_zone, end="lower", xynames=c("sx","sy")))
#'
#' ## and ending x,y coordinates
#' attack_rate <- cbind(attack_rate, ggxy(attack_rate$end_zone, end="upper", xynames=c("ex","ey")))
#'
#' ## plot in reverse order so largest arrows are on the bottom
#' attack_rate <- attack_rate %>% dplyr::arrange(desc(rate))
#'
#' p <- ggplot(attack_rate,aes(x,y,col=rate)) + ggcourt(labels=c(teams(x)[1],""))
#' for (n in 1:nrow(attack_rate))
#'     p <- p + geom_path(data=data.frame(x=c(attack_rate$sx[n], attack_rate$ex[n]),
#'                                        y=c(attack_rate$sy[n],attack_rate$ey[n]),
#'                                        rate=attack_rate$rate[n]),
#'         aes(size=rate), lineend="round", arrow=arrow(ends="last", type="closed"))
#' p + scale_fill_gradient(name="Attack rate") + guides(size="none")
#' }
#' @export
ggxy <- function(zones, end="lower", xynames=c("x", "y"), as_for_serve=FALSE) {
    end <- match.arg(tolower(end),c("lower","upper"))
    ## define zones and their corresponding coordinates
    start_zones <- 1:9 ## lower part of figure
    szx <- if (as_for_serve) c(3,NA,NA,NA,1,2,1.5,NA,2.5) else c(3,3,2,1,1,2,1,2,3)
    szy <- if (as_for_serve) c(1,NA,NA,NA,1,1,1,NA,1)-0.5 else c(1,3,3,3,1,1,2,2,2)
    end_zones <- 1:9 ## upper part of figure
    ezx <- 4-szx
    ezy <- 3+4-szy

    zones[!zones %in% 1:9] <- NA
    out <- switch(end,
                  lower=data.frame(x=mapvalues(zones,start_zones,szx,warn_missing=FALSE),y=mapvalues(zones,start_zones,szy,warn_missing=FALSE)),
                  upper=data.frame(x=mapvalues(zones,end_zones,ezx,warn_missing=FALSE),y=mapvalues(zones,end_zones,ezy,warn_missing=FALSE)),
                  stop("unexpected end, should be \"lower\" or \"upper\"")
                  )
    names(out) <- xynames
    out
}
