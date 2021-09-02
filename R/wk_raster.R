#' Read raster data with a wk grid
#'
#' Read any GDAL accessible source into a wk grd_rct object.
#'
#' The argument 'grid' is used to provide an extent, dimension, and projection.
#'
#' If argument 'grid' is NULL a heuristic is used to produce a smallish version of the source.
#'
#' @param x  a GDAL source (a character string) file name, URL, SDS string, data base connection string
#' @param grid a grd_rct object (can be NULL)
#' @param ... arguments passed to [vapour::vapour_warp_raster()], particularly 'resample'
#' @param max_dim default is 1024, ignored if 'grid' is not `NULL`
#'
#' @return grd_rct
#' @export
#'
#' @examples
#' ## some sources
#' dsn <- paste0("WMTS:https://basemap.nationalmap.gov/arcgis/rest/services/USGSTopo/",
#'                  "MapServer/WMTS/1.0.0/WMTSCapabilities.xml,layer=USGSTopo,tilematrixset=default028mm")
#' vearth <- '<GDAL_WMS> <Service name="VirtualEarth">
#' <ServerUrl>http://a${server_num}.ortho.tiles.virtualearth.net/tiles/a${quadkey}.jpeg?g=90</ServerUrl></Service>
#'     <MaxConnections>4</MaxConnections>    <Cache/>    </GDAL_WMS>'
#'
#' g <- wk_image(dsn)
#' plot(g)
#'
#' op <- par(mar = rep(0, 4))
#' dm <- dev.size("px")
#'
#' b <- 1e5
#'
#' grid <- wk::grd(bbox = wk::rct(-b, -b, b, b, crs = "+proj=laea +lon_0=-63 +lat_0=45"), nx = dm[1], ny = dm[2])
#' g1 <- wk_image(dsn, grid, resample = "cubic")
#' plot(g1)
#' im <- wk_image(vearth, grid, resample = "cubic")
#' plot(im)
#'

#' gridt <- wk::grd(bbox = wk::rct(-b, -b, b, b, crs = "+proj=laea +lon_0=147 +lat_0=-42"), nx = dm[1], ny = dm[2])
#' g2 <- wk_image(vearth, gridt, resample = "cubic")
#' plot(g2)
#'
#' aws.elev <- paste0(
#'    readLines("https://raw.githubusercontent.com/hypertidy/gdalwebsrv/master/inst/awswmsxml/elevation-tiles-prod.xml"),
#'    collapse = "")
#' dem <- wk_raster(aws.elev, grid)
#'
#'
#' #f <- raadtools::topofile("gebco_19")
#' #r <- wk_raster(f, gridt)
#' #plot(r)
wk_image <- function(x, grid = NULL, ..., max_dim = 1024) {
  ri <- vapour::vapour_raster_info(x)

  if (is.null(grid)) grid <- .wk_grid(ri, max_dim = max_dim)
  v <- vapour::vapour_warp_raster_hex(x, band = seq_len(min(c(ri$bands, 4L))),
                                      extent = as.numeric(wk::wk_bbox(grid))[c(1L, 3, 2, 4)],
                                      dimension = dim(grid)[2:1],
                                      wkt = as.character(wk::wk_crs(grid)), ...)

  grid$data <- as.raster(matrix(v, dim(grid)[1L], dim(grid)[2L], byrow = TRUE))
  grid
}

#' @export
#' @name wk_image
wk_raster <- function(x, grid = NULL, ..., max_dim = 1024) {
  ri <- vapour::vapour_raster_info(x)

  if (is.null(grid)) grid <- .wk_grid(ri, max_dim = max_dim)
  v <- vapour::vapour_warp_raster(x,
                                      extent = as.numeric(wk::wk_bbox(grid))[c(1L, 3, 2, 4)],
                                      dimension = dim(grid)[2:1],
                                      wkt = as.character(wk::wk_crs(grid)), ...)[[1L]]  ## we're ignoring multiple bands ...

  grid$data <- matrix(v, dim(grid)[1L], dim(grid)[2L], byrow = TRUE)
  grid
}
