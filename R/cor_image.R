#' @title Graphical display of a correlation matrix
#' @description
#'   The function shows an `image()` of a correlation matrix, where row-and column names
#'   are use as labels.
#' @importFrom grDevices hcl.colors
#' @importFrom graphics axis image text
#' @param cor_mat A matrix of pairwise correlations
#' @param srt A numeric controlling the angle of the text
#' @param cex,cex.axis
#'   Numerics indicating the font size for text- and axis labels
#'
#' @return A graphical representation of the correlation matrix
#' @export
#' @author Lukas Dargel
#' @examples
#' cor_mat <- cor(matrix(rnorm(40),ncol = 4))
#' cor_image(cor_mat)
cor_image <- function(cor_mat, cex = .7, srt = 45, cex.axis = .7) {

  cor_mat[!is.finite(cor_mat)] <- NA
  tol <- sqrt(.Machine$double.eps)
  assert(all(c(diag(cor_mat) == 1, abs(cor_mat) - tol <= 1),na.rm = TRUE),
         "Make sure to provide a valid correlation matrix.
         All elements musst be in the interval from -1 to 1
         and the diagonal elements musst be 1!")

  if (is.null(rownames(cor_mat)))
    rownames(cor_mat) <- colnames(cor_mat)

  if (is.null(colnames(cor_mat)))
    colnames(cor_mat) <- rownames(cor_mat)


  nc_c <- max(c(nchar(colnames(cor_mat))^(.85),1.3))
  nc_r <- max(c(nchar(rownames(cor_mat))^(.85),1.3))
  opts <- par(mar = c(1, nc_r / 1.3, nc_c / 1.3, 1))
  cor_mat_rev <- cor_mat[,rev(seq(ncol(cor_mat)))]

  # color
  ncolor <- 200
  col_breaks <- seq(from = -1, to = 1, length.out = ncolor + 1)
  col_breaks <- sign(col_breaks) * sqrt(abs(col_breaks))
  col_pal <- hcl.colors(n = ncolor, palette = "Blue-Red 3",rev = T)

  # text and labels
  pos_xy <- seq(from = 0, to = 1, length.out = ncol(cor_mat))
  pos_x <- rep(pos_xy, ncol(cor_mat))
  pos_y <- rep(pos_xy, each = ncol(cor_mat))
  text_cor <- as.vector(cor_mat_rev)
  text_cor <- round(text_cor,2)
  text_col <- rep("white", length(text_cor))
  text_col[abs(text_cor) < .5] <- "black"
  top_labels <- rev(colnames(cor_mat_rev))
  left_labels <- rev(rownames(cor_mat_rev))
  backup_labels <- paste0("V", seq(ncol(cor_mat_rev)))

  dont_show <- sort(abs(col_breaks))[2]
  text_filter <- abs(text_cor) > dont_show


  image.default(
    z = cor_mat_rev,
    useRaster = FALSE,
    axes = FALSE,
    col = col_pal,
    breaks = col_breaks)
  axis(side = 2, # left
       at = seq(from = 0, to = 1, length.out = ncol(cor_mat)),
       labels = left_labels %||% backup_labels,
       las = 2,
       cex.axis = cex.axis)
  axis(side = 3, # top
       at = seq(from = 0, to = 1, length.out = ncol(cor_mat)),
       labels = top_labels %||% backup_labels,
       las = 2,
       cex.axis = cex.axis)
  text(x = pos_x[text_filter],
       y = pos_y[text_filter],
       labels = text_cor[text_filter],
       col = text_col[text_filter],
       cex = cex,
       srt = srt)
  par(opts)
}
