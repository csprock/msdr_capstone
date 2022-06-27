
draw_panel_timeline <- function(data, panel_scales, coord) {

  # filter to the top 5 for each country
  data <- data %>%
    dplyr::group_by(y) %>%
    dplyr::top_n(x, n=data$n_max[1])

  data_trans <- coord$transform(data, panel_scales)

  data_trans$y0 <- data_trans$y + 0.05


  text_labels <- grid::textGrob(
    label = data_trans$label,
    x=data_trans$x,
    y=data_trans$y0,
    just="left",
    rot=45,
    check.overlap = TRUE,
    gp=grid::gpar(fontsize=10)
  )

  segments <- grid::segmentsGrob(
    x0=data_trans$x,
    x1=data_trans$x,
    y0=data_trans$y0,
    y=data_trans$y,
    default.units="npc"
  )

  return(grid::gTree(children=grid::gList(segments, text_labels)))
}

GeomLabelTimeline <- ggplot2::ggproto("GeomLabelTimeline", ggplot2::Geom,
                        required_aes = c("x","y", "label"),
                        default_aes = c(n_max=5),
                        draw_key = ggplot2::draw_key_point,
                        draw_panel = draw_panel_timeline)


geom_label_timeline <- function(
  mapping=NULL,
  data=NULL,
  stat="identity",
  position="identity",
  na.rm=TRUE,
  show.legend=NA,
  inherit.aes=TRUE, ...) {
  ggplot2::layer(
    geom=GeomLabelTimeline,
    mapping=mapping,
    data=data,
    stat=stat,
    position=position,
    show.legend=show.legend,
    inherit.aes = inherit.aes,
    params=list(na.rm=na.rm, ...)
  )
}
