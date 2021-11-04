resolve_anno <- function(anno) {
  outlined <- ggfx::with_outer_glow(anno, "white", sigma = 0.01, expand = 2)
  push_anno_storage(outlined)
  grid::grid.draw(outlined)
  invisible(snapshot_anno())
}

#' Draw a line
#'
#' @return Current annotated figure as a grob
#' @export
anno_line <- function() {
  message("Select start point:")
  pos1 <- grid_locate()
  message("Select end point:")
  pos2 <- grid_locate()
  line_annotation <- grid::linesGrob(
    name = "anno_line",
    x = c(pos1$x, pos2$x),
    y = c(pos1$y, pos2$y)
  )
  resolve_anno(line_annotation)
}

#' Draw a vertical or horizontal line
#'
#' @return Current annotated figure as a grob
#' @export
anno_vhline <- function() {
  message("Select starting reference point:")
  pos1 <- grid_locate()
  message("Select vertical or horizontal end point:")
  pos2 <- grid_locate()
  run <- abs(as.numeric(pos1$x) - as.numeric(pos2$x))
  rise <- abs(as.numeric(pos1$y) - as.numeric(pos2$y))
  if (abs(atan2(rise, run)) < atan(1)) {
    pos2$y <- pos1$y
  } else {
    pos2$x <- pos1$x
  }
  line_annotation <- grid::linesGrob(
    name = "anno_line",
    x = c(pos1$x, pos2$x),
    y = c(pos1$y, pos2$y)
  )
  resolve_anno(line_annotation)
}

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
  curve_annotation <- grid::curveGrob(
    name = "anno_line",
    x1 = pos1$x,
    x2 = pos2$x,
    y1 = pos1$y,
    y2 = pos2$y,
    curvature = as.numeric(curvature),
    arrow = grid::arrow(length = .anno_unit)
  )
  resolve_anno(curve_annotation)
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
    name = "anno_bezier",
    x = lapply(dots, `[[`, "x"),
    y = lapply(dots, `[[`, "y"),
    arrow = grid::arrow(length = .anno_unit)
  )
  resolve_anno(bezier_annotation)
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
    name = "anno_text",
    label = label,
    x = pos$x,
    y = pos$y,
    hjust = as.numeric(hjust),
    vjust = 0.5,
    gp = grid::gpar(size = as.numeric(size), fontfamily = "Helvetica")
  )
  resolve_anno(text_annotation)
}

#' Draw a rectangle outline
#'
#' @return Current annotated figure as a grob
#' @export
anno_rect <- function() {
  message("Select top-left corner:")
  pos1 <- grid_locate()
  message("Select bottom-right corner:")
  pos2 <- grid_locate()
  width <- pos2$x - pos1$x
  height <- pos1$y - pos2$y
  if (min(as.numeric(width), as.numeric(height)) < 0) {
    stop("Invalid corner point coordinate specifications.")
  }
  rect_annotation <- grid::rectGrob(
    x = pos1$x + width / 2,
    y = pos2$y + height / 2,
    width = width,
    height = height
  )
  resolve_anno(rect_annotation)
}
