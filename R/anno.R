#' Draw a curve
#'
#' @return Current annotated figure as a grob
#' @export
anno_curve <- function() {
  message("Select start point:")
  pos1 <- grid_locate()
  message("Select end point:")
  pos2 <- grid_locate()
  curvature <- readline(prompt = "curvature [left = -1, straight = 0, right = 1]: ")
  line_annotation <- grid::curveGrob(
    x1 = pos1$x,
    x2 = pos2$x,
    y1 = pos1$y,
    y2 = pos2$y,
    curvature = as.numeric(curvature),
    arrow = grid::arrow(length = .anno_unit)
  )
  line_annotation <- ggfx::with_outer_glow(line_annotation, "white", sigma = 0.01, expand = 2)
  push_anno_storage(line_annotation)
  grid::grid.draw(line_annotation)
  invisible(snapshot_anno())
}

#' Draw a bezier curve
#'
#' @return Current annotated figure as a grob
#' @export
anno_bezier <- function() {
  n <- message("Pick 4 control points: ")
  dots <- vector("list", 4)
  for (i in 1:4) {
    cat("Point", i, "\n")
    dots[[i]] <- grid_locate()
  }
  bezier_annotation <- grid::bezierGrob(
    name = "anno",
    x = lapply(dots, `[[`, "x"),
    y = lapply(dots, `[[`, "y"),
    arrow = grid::arrow(length = .anno_unit)
  )
  bezier_annotation <- ggfx::with_outer_glow(bezier_annotation, "white", sigma = 0.01, expand = 2)
  push_anno_storage(bezier_annotation)
  grid::grid.draw(bezier_annotation)
  invisible(snapshot_anno())
}

#' Write text
#'
#' @return Current annotated figure as a grob
#' @export
anno_text <- function() {
  message("Select position:")
  pos <- grid_locate()
  label <- readline(prompt = "label: ")
  hjust <- readline(prompt = "align [left = 0, right = 1]: ")
  size <- readline(prompt = "size: ")
  text_annotation <- grid::textGrob(
    name = "anno",
    label = label,
    x = pos$x,
    y = pos$y,
    hjust = as.numeric(hjust),
    vjust = 0.5,
    gp = grid::gpar(size = as.numeric(size), fontfamily = "Inter")
  )
  text_annotation <- ggfx::with_outer_glow(text_annotation, "white", sigma = 0.01, expand = 2)
  push_anno_storage(text_annotation)
  grid::grid.draw(text_annotation)
  invisible(snapshot_anno())
}
