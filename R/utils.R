#' Quickly draw a grob or a list of grobs
#' @param g A grob or a list of grobs
#' @param new Whether to draw on a new page
#'
#' @return Current annotated figure as a grob
#' @export
qdraw <- function(g, new = TRUE) {
  if (new) {
    grid::grid.newpage()
    vp <- grid::viewport()
  } else {
    vp <- NULL
  }
  if (is.list(g)) {
    grid::grid.draw(grid::gTree(children = do.call(grid::gList, g), vp = vp))
  } else {
    grid::grid.draw(grid::gTree(children = grid::gList(g), vp = vp))
  }
  invisible(snapshot_anno())
}
