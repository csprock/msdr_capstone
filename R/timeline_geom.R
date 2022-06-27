
 draw_panel <- function(data, panel_scales, coord) {

   data_trans <- coord$transform(data, panel_scales)

   circles <- grid::circleGrob(
     x=data_trans$x,
     y=data_trans$y,
     r=map_to_interval(data_trans$size, x_min=0, x_max=10, u=0.05, l=0),
     gp=grid::gpar(
       col=data_trans$color,
       fill=data_trans$fill,
       alpha=data_trans$alpha
     )
   )

   return(grid::gTree(children=grid::gList(circles)))
}

GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::GeomPoint,
                        required_aes = c("x","y", "size", "fill"),
                        default_aes = ggplot2::aes(color="grey", alpha=0.75),
                        draw_key = ggplot2::draw_key_point,
                        draw_panel = draw_panel)


geom_timeline <- function(
  mapping=NULL,
  data=NULL,
  stat="identity",
  position="identity",
  na.rm=TRUE,
  show.legend=NA,
  inherit.aes=TRUE, ...) {
  ggplot2::layer(
    geom=GeomTimeline,
    mapping=mapping,
    data=data,
    stat=stat,
    position=position,
    show.legend=show.legend,
    inherit.aes = inherit.aes,
    params=list(na.rm=na.rm, ...)
  )
}
