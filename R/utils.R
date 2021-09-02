.wk_grid <- function(x, max_dim = 1024) {
  ## expect x is a vapour raster_info list
  rat <- x$dimXY[1]/x$dimXY[2]
  dm <- c(max_dim * rat, max_dim)[order(x$dimXY)]

  .data <- array(dim = c(dm, 0))
  bb <- x$extent[c(1, 3, 2, 4)]

  wk::grd_rct(.data, bbox = wk::rct(xmin = bb[1L], ymin = bb[2L], xmax = bb[3L], ymax = bb[4L], crs = x$projection))
}
