.anno_store <- function() {
  annos <- list()
  cur_idx <- 0
  list(
    get = function() { annos },
    push = function(anno) {
      if (cur_idx != length(annos)) { annos <<- annos[seq_len(cur_idx)] }
      if (length(gnames()) <= 1) { annos <<- list() }
      annos <<- c(annos, list(anno))
      cur_idx <<- length(annos)
    },
    clear = function() { annos <<- list() },
    up = function() { cur_idx <<- min(cur_idx + 1, length(annos)) },
    down = function() { cur_idx <<- max(cur_idx - 1, 0) },
    pos = function() { cur_idx }
  )
}
.anno_storage <- .anno_store()
push_anno_storage <- function(g) { .anno_storage$push(g) }

#' Get annotation history
#'
#' @return A list of grobs
#' @export
get_anno_storage <- function() { .anno_storage$get() }

#' Clear annotation history
#'
#' @rdname get_anno_storage
#' @return A list of grobs
#' @export
clear_anno_storage <- function() { .anno_storage$clear() }

#' Undo annotation
#'
#' @return Current annotated figure as a grob
#' @export
undo_anno <- function() {
  annos <- gnames()[-1]
  if (length(annos) == 0) { stop("No annotation to remove") }
  last <- paste0("^", annos[length(annos)], "$")
  popped <- grid::grid.get(last, grep = TRUE)
  .anno_storage$down()
  grid::grid.remove(last, grep = TRUE)
  invisible(snapshot_anno())
}

#' Redo annotation
#'
#' @rdname undo_anno
#' @return Current annotated figure as a grob
#' @export
redo_anno <- function() {
  if (.anno_storage$pos() == length(get_anno_storage())) { stop("No annotation to redo") }
  .anno_storage$up()
  anno <- get_anno_storage()[[.anno_storage$pos()]]
  grid::grid.draw(anno)
}

#' Get names of first-level grobs in current viewport
#'
#' @return character
#' @export
gnames <- function() {
  grid::grid.ls(print = function(...) { })[1]$name
}

#' Snapshot the current grid output
#'
#' @return Current annotated figure as a grob
#' @export
snapshot_anno <- function() {
  grid::grid.grab(wrap.grobs = TRUE)
}

#' Interactively locate a point
#'
#' @return List of x and y positions in normalized parent coordinates, rounded to 3 digits
#' @export
grid_locate <- function() {
  lapply(grid::grid.locator(unit = "npc"), round, 3)
}

.anno_unit <- grid::unit(0.03, "npc")
