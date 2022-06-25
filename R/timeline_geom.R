



create_timeline_grob <- function(data, panel_scales, coord) {


  #data_trans <- data.frame(
  #  x = data[["x"]],
  #  y = data[["y"]]
  #) %>% coord$transform(panel_scales)

  data_trans <- coord$transform(data, panel_scales)

  y_pos <- data_trans[["y"]][1]
  base_horizontal_line <- linesGrob(x=c(0, 1), y=c(y_pos, y_pos))


  circles <- circleGrob(
    x=data_trans$x,
    y=data_trans$y,
    r=data_trans$size,
    gp=gpar(
      col="black",
      fill=data_trans$fill,
      alpha=0.5
    )
  )


  return(gTree(children=gList(circles, base_horizontal_line)))

  return(grobs)
}


draw_panel <- function(data, panel_scales, coord) {
  if (isFALSE("y" %in% colnames(data))) {
    data$y <- 0.5
  }

  timeline_grob <- create_timeline_grob(data, panel_scales, coord)
  return(timeline_grob)
}

GeomTimeline <- ggproto("GeomTimeline", GeomPoint,
                        required_aes = c("x", "size", "fill"),
                        default_aes = aes(color="grey", alpha=0.75),
                        draw_key = draw_key_point,
                        draw_panel = draw_panel)


geom_timeline <- function(
  mapping=NULL,
  data=NULL,
  stat="identity",
  position="identity",
  na.rm=TRUE,
  show.legend=NA,
  inherit.aes=TRUE, ...) {
  layer(
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


### test
ca_data %>%
  ggplot(mapping=aes(x=Year, size=Mag, fill=Deaths)) +
  geom_timeline() +
  scale_fill_continuous() +
  labs(fill="#deaths", size="Magnitude") +
  scale_radius(range=c(0, 0.1))


