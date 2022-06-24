
GeomTimeline <- ggproto("GeomTimeline", Geom,
                         required_aes = c("x", "r"),
                         default_aes = aes(color="grey", alpha=0.75, fill="blue"),
                         draw_key = draw_key_polygon,
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



create_timeline_grob <- function(data, panel_scales, coord) {


  data_trans <- data.frame(
    x = map_to_interval(data[["x"]], x_min=min(data$x), x_max=max(data$x), u=0.9, l=0.1),
    y = data[["y"]],
    radius = map_to_interval(data[["r"]], x_min=0, x_max=10, u=0.04, l=0.005)
  ) #%>% coord$transform(panel_scales)

  y_pos <- data_trans[["y"]][1]
  base_horizontal_line <- linesGrob(x=c(0.1, 0.9), y=c(y_pos, y_pos))
  print(y_pos)
  # create circles
  circles <- list()
  for (i in 1:nrow(data_trans)) {
    print(data_trans[["x"]][i])
    circles[[i]] <- circleGrob(
      x=data_trans[["x"]][i],
      y=y_pos,
      r=data_trans[["radius"]][i],
      gp=gpar(
        col="black",
        fill="blue",
        alpha=0.5
      )
    )
  }

  circles[[length(circles)+1]] <- base_horizontal_line

  grobs <- gTree(children=do.call(gList, circles))

  return(grobs)
}


draw_panel <- function(data, panel_scales, coord) {
  if (isFALSE("y" %in% colnames(data))) {
    data$y <- 0.5
  }

  timeline_grob <- create_timeline_grob(data, panel_scales, coord)
  return(timeline_grob)
}

### test
ca_data %>%
  ggplot(mapping=aes(x=Year, r=Mag)) +
  geom_timeline()


