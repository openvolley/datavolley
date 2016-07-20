#' Add volleyball court schematic to a ggplot
#'
#' @param court string: "full" (show full court) or "attack" or "defence" (show only the attacking or defending half of the court)
#' @param show_zones logical: add numbers indicating the court zones?
#' @param labels string: labels for the lower and upper court halves (pass NULL for no labels)
#'
#' @return ggplot layer
#'
#' @seealso \code{\link{ggxy}}
#'
#' @examples
#' \dontrun{
#' x <- read_dv(system.file("extdata/example_data.dvw",package="datavolley"),
#'     insert_technical_timeouts=FALSE)
#'
#' library(ggplot2)
#' ## calculate attack frequency by zone, per team
#' attack_rate <- ddply(subset(plays(x),skill=="Attack"),.(team),
#'   function(z)ddply(z,.(start_zone),function(w)data.frame(rate=nrow(w)/nrow(z))))
#' ## add x,y coordinates associated with the zones
#' attack_rate <- cbind(attack_rate,ggxy(attack_rate$start_zone,end="lower"))
#' ## for team 2, these need to be on the top half of the diagram
#' tm2 <- attack_rate$team==x$meta$teams$team[2]
#' attack_rate[tm2,c("x","y")] <- ggxy(attack_rate$start_zone,end="upper")[tm2,]
#' ggplot(attack_rate,aes(x,y,fill=rate))+geom_tile()+ggcourt(labels=x$meta$teams$team)+
#'   scale_fill_gradient2(name="Attack rate")
#' 
#' ## show map of starting and ending zones of attacks using arrows
#' ## tabulate attacks by starting and ending zone
#' attack_rate <- as.data.frame(xtabs(~start_zone+end_zone,
#'   data=subset(plays(x),skill=="Attack" & team==x$meta$teams$team[1],
#'   stringsAsFactors=FALSE)
#' attack_rate$start_zone <- as.numeric(attack_rate$start_zone)
#' attack_rate$end_zone <- as.numeric(attack_rate$end_zone)
#' attack_rate$rate <- attack_rate$Freq/sum(attack_rate$Freq)
#' attack_rate <- attack_rate[attack_rate$Freq>0,]
#' ## starting x,y coordinates
#' temp <- ggxy(attack_rate$start_zone,end="lower")
#' names(temp) <- c("sx","sy")
#' attack_rate <- cbind(attack_rate,temp)
#' ## ending x,y coordinates
#' temp <- ggxy(attack_rate$end_zone,end="upper")
#' names(temp) <- c("ex","ey")
#' attack_rate <- cbind(attack_rate,temp)
#' ## plot in reverse order so largest arrows are on the bottom
#' attack_rate <- attack_rate[order(attack_rate$rate,decreasing=TRUE),]
#' p <- ggplot(attack_rate,aes(x,y,col=rate))+ggcourt(labels=c(x$meta$teams$team[1],""))
#' for (n in 1:nrow(attack_rate))
#'     p <- p+geom_path(data=data.frame(
#'             x=c(attack_rate$sx[n],attack_rate$ex[n]),
#'             y=c(attack_rate$sy[n],attack_rate$ey[n]),
#'             rate=attack_rate$rate[n]),
#'         aes(size=rate),lineend="round",arrow=arrow(ends="last",type="closed"))
#' p+scale_fill_gradient(name="Attack rate")+guides(size="none")
#' }
#' @export
ggcourt <- function(court="full",show_zones=TRUE,labels=c("Attacking team","Receiving team")) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("The ggplot2 package needs to be installed for ggcourt to be useful")
    }    
    court <- match.arg(tolower(court),c("full","attack","defence"))
    ## horizontal grid lines
    hl <- data.frame(x=c(0.5,3.5),y=c(0.5,0.5,1.5,1.5,2.5,2.5,3.5,3.5,4.5,4.5,5.5,5.5,6.5,6.5),id=c(1,1,2,2,3,3,4,4,5,5,6,6,7,7))
    hl <- switch(court,
                 attack=hl[hl$y<4,],
                 defence=hl[hl$y>3,],
                 hl)
    ## vertical grid lines
    vl <- data.frame(y=c(0.5,6.5),x=c(0.5,0.5,1.5,1.5,2.5,2.5,3.5,3.5),id=c(1,1,2,2,3,3,4,4))
    vl$y <- switch(court,
                   attack=mapvalues(vl$y,6.5,3.5),
                   defence=mapvalues(vl$y,0.5,3.5),
                   vl$y)
    hl <- ggplot2::geom_path(data=hl,ggplot2::aes_string(x="x",y="y",group="id"),colour="black",inherit.aes=FALSE)
    vl <- ggplot2::geom_path(data=vl,ggplot2::aes_string(x="x",y="y",group="id"),colour="black",inherit.aes=FALSE) 
                     
    net <- ggplot2::geom_path(data=data.frame(x=c(0.25,3.75),y=c(3.5,3.5)),ggplot2::aes_string(x="x",y="y"),colour="black",size=2,inherit.aes=FALSE) ## net
    thm <- ggplot2::theme_classic()
    thm2 <- ggplot2::theme(axis.line=ggplot2::element_blank(),axis.text.x=ggplot2::element_blank(), axis.text.y=ggplot2::element_blank(),axis.ticks=ggplot2::element_blank(), axis.title.x=ggplot2::element_blank(), axis.title.y=ggplot2::element_blank())
    out <- list(hl,vl,net,thm,thm2)
    if (!is.null(labels)) {
        if (court %in% c("full","attack"))
            out <- c(out,ggplot2::annotate("text",x=2,y=0.4,label=labels[1]))
        if (court %in% c("full","defence"))
            out <- c(out,ggplot2::annotate("text",x=2,y=6.6,label=labels[2]))
    }
    if (show_zones) {
        szx <- c(3,3,2,1,1,2,1,2,3)
        szy <- c(1,3,3,3,1,1,2,2,2)
        ezx <- 4-szx
        ezy <- 3+4-szy
        if (court %in% c("full","attack"))        
            out <- c(out,ggplot2::annotate("text",x=szx+0.4*rep(-1,9),y=szy+0.4*rep(-1,9),label=1:9,vjust="center",hjust="middle",fontface="italic"))
        if (court %in% c("full","defence"))
            out <- c(out,ggplot2::annotate("text",x=ezx+0.4*rep(1,9),y=ezy+0.4*rep(1,9),label=1:9,vjust="center",hjust="middle",fontface="italic"))
    }
    out
}


#' Create x and y coordinates for plotting, from DataVolley numbered zones 
#'
#' @param zones numeric: zones numbers 1-9 to convert to x and y coordinates
#' @param end string: use the "lower" or "upper" part of the figure
#'
#' @return data.frame with x and y components
#'
#' @seealso \code{\link{ggcourt}}
#'
#' @examples
#' \dontrun{
#' x <- read_dv(system.file("extdata/example_data.dvw",package="datavolley"),
#'     insert_technical_timeouts=FALSE)
#'
#' library(ggplot2)
#' ## calculate attack frequency by zone, per team
#' attack_rate <- ddply(subset(plays(x),skill=="Attack"),.(team),
#'   function(z)ddply(z,.(start_zone),function(w)data.frame(rate=nrow(w)/nrow(z))))
#' ## add x,y coordinates associated with the zones
#' attack_rate <- cbind(attack_rate,ggxy(attack_rate$start_zone,end="lower"))
#' ## for team 2, these need to be on the top half of the diagram
#' tm2 <- attack_rate$team==x$meta$teams$team[2]
#' attack_rate[tm2,c("x","y")] <- ggxy(attack_rate$start_zone,end="upper")[tm2,]
#' ggplot(attack_rate,aes(x,y,fill=rate))+geom_tile()+ggcourt(labels=x$meta$teams$team)+
#'   scale_fill_gradient2(name="Attack rate")
#' 
#' ## show map of starting and ending zones of attacks using arrows
#' ## tabulate attacks by starting and ending zone
#' attack_rate <- as.data.frame(xtabs(~start_zone+end_zone,
#'   data=subset(plays(x),skill=="Attack" & team==x$meta$teams$team[1],
#'   stringsAsFactors=FALSE)
#' attack_rate$start_zone <- as.numeric(attack_rate$start_zone)
#' attack_rate$end_zone <- as.numeric(attack_rate$end_zone)
#' attack_rate$rate <- attack_rate$Freq/sum(attack_rate$Freq)
#' attack_rate <- attack_rate[attack_rate$Freq>0,]
#' ## starting x,y coordinates
#' temp <- ggxy(attack_rate$start_zone,end="lower")
#' names(temp) <- c("sx","sy")
#' attack_rate <- cbind(attack_rate,temp)
#' ## ending x,y coordinates
#' temp <- ggxy(attack_rate$end_zone,end="upper")
#' names(temp) <- c("ex","ey")
#' attack_rate <- cbind(attack_rate,temp)
#' ## plot in reverse order so largest arrows are on the bottom
#' attack_rate <- attack_rate[order(attack_rate$rate,decreasing=TRUE),]
#' p <- ggplot(attack_rate,aes(x,y,col=rate))+ggcourt(labels=c(x$meta$teams$team[1],""))
#' for (n in 1:nrow(attack_rate))
#'     p <- p+geom_path(data=data.frame(
#'             x=c(attack_rate$sx[n],attack_rate$ex[n]),
#'             y=c(attack_rate$sy[n],attack_rate$ey[n]),
#'             rate=attack_rate$rate[n]),
#'         aes(size=rate),lineend="round",arrow=arrow(ends="last",type="closed"))
#' p+scale_fill_gradient(name="Attack rate")+guides(size="none")
#' }
#' @export
ggxy <- function(zones,end="lower") {
    end <- match.arg(tolower(end),c("lower","upper"))
    ## define zones and their corresponding coordinates
    start_zones <- 1:9 ## lower part of figure
    szx <- c(3,3,2,1,1,2,1,2,3)
    szy <- c(1,3,3,3,1,1,2,2,2)
    end_zones <- 1:9 ## upper part of figure
    ezx <- 4-szx
    ezy <- 3+4-szy

    zones[!zones %in% 1:9] <- NA
    switch(end,
           lower=data.frame(x=mapvalues(zones,start_zones,szx,warn_missing=FALSE),y=mapvalues(zones,start_zones,szy,warn_missing=FALSE)),
           upper=data.frame(x=mapvalues(zones,end_zones,ezx,warn_missing=FALSE),y=mapvalues(zones,end_zones,ezy,warn_missing=FALSE)),
           stop("unexpected end, should be \"lower\" or \"upper\"")
           )
}
