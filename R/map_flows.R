#' Geographic representation of origin-destination flows
#'
#' The graphic combines a graphical representation of origin-destination
#' flows with bar charts that represent the total inflow and outflow per site.
#' By default only the largest 25% of flows are shown.
#'
#' @param y A numeric vector of flows
#' @param index_o
#'   A factor/character vector of origin ids
#'   (should be the same length as y)
#' @param index_d
#'   A factor/character vector of destination ids
#'   (should be the same length as y)
#' @param coords_s
#'   A matrix or data.frame, containing coordinates for each (unique) origin and
#'   destination in `index_o` and `index_d`.
#'   The rownames musts be given and must correspond with the identifiers
#'   of the origins and destinations.
#' @param color_palette
#'   A character vector of colors. Should have the same length and ordering as
#'   the coordinates in `coords_s`
#' @param add A logical, controlling whether a new graphic window may be opened
#' @param max_lwd
#'   A numeric, indicating the value of the maximum width for the lines
#'   connecting the origins with the destinations
#' @param filter_lowest
#'   A numeric, that gives the value of the highest quantile to represent
#'   (e.g.: if `filter_lowest = .75`, only the largest 25% of flows are shown)
#' @param max_bar
#'   A numeric, indicating the height of the largest bar-chart shown
#' @param legend_position
#'   A character, indicating the legend position. Should be one of
#'   `c("none", "bottomright", "bottom", "bottomleft", "left",`
#'      `"topleft", "top", "topright", "right", "center")`
#' @param decimal_points
#'    A numeric, indicating the number of decimal points shown in in the legend
#' @param add_labels
#'    A logical, controlling whether the site names should be printed
#' @param remove_intra
#'    A logical that sets the intra flow to null values
#' @inheritParams spflow_control
#'
#' @return Creates a graphical representation of origin-destination flows
#' @export
#' @importFrom grDevices colors
#' @importFrom graphics arrows legend lines polygon text
#'
#' @examples
#'
#' library(spflow)
#' library(sf)
#' data("paris10km_commuteflows")
#' data("paris10km_municipalities")
#'
#'
#' # use only the 20 central aggrandisements of Paris
#' two_letters <- function(x, var) substr(x[[var]], 1 , 2)
#'
#' paris_arron_commuteflows <- paris10km_commuteflows[
#'   two_letters(paris10km_commuteflows, "ID_DEST") == "75" &
#'     two_letters(paris10km_commuteflows, "ID_ORIG") == "75",]
#'
#' paris_aggrandisements <- paris10km_municipalities[
#'   two_letters(paris10km_municipalities, "ID_MUN") == "75" ,]
#'
#' # create the matrix with the coordinates
#' coords_xy <- st_coordinates(st_centroid(paris_aggrandisements))
#' rownames(coords_xy) <- paris_aggrandisements[["ID_MUN"]]
#'
#' map_flows(paris_arron_commuteflows[, "COMMUTE_FLOW"],
#'           paris_arron_commuteflows[, "ID_ORIG"],
#'           paris_arron_commuteflows[, "ID_DEST"],
#'           coords_s = coords_xy,
#'           legend_position = "topright",
#'           color_palette = colors(distinct = TRUE)[20:41],
#'           add_labels = TRUE,
#'           remove_intra = TRUE)
#'
map_flows <- function(
  y,
  index_o,
  index_d,
  coords_s,
  color_palette = sample(colors(), size = nrow(coords_s), replace = nrow(coords_s) > length(colors())),
  add = FALSE,
  max_lwd = 1,
  filter_lowest = 0.75,
  max_bar = 1,
  legend_position = "none",
  decimal_points = 0,
  add_labels = FALSE,
  remove_intra = FALSE,
  na_rm = TRUE) {

  # verification
  # size of the vectors
  stopifnot(length(y) == length(index_o),
            length(index_o) == length(index_d))


  valid_y <- is.finite(y)
  if (!all(valid_y)) {
    assert(na_rm, "NA/NaN/Inf in y!")
    y <- y[valid_y]
    index_o <- index_o[valid_y]
    index_d <- index_d[valid_y]
    }

  stopifnot(legend_position %in% c("none", "bottomright", "bottom", "bottomleft",
            "left", "topleft", "top", "topright", "right", "center"))

  ############### Initialisation
  # number of flows
  N <- length(y)
  # index of the origin
  O <- unique(as.character(index_o))
  n_o <- length(O)
  D <- unique(as.character(index_d))
  n_d <- length(D)
  # number of unique site in S
  S <- union(O, D)
  n <- length(S)

  # remove intra or not ?
  if (remove_intra) {
    y[index_o == index_d] <- 0
  }


  # Check on the spatial coordinates data
  stopifnot(all(S %in% rownames(coords_s)))
  coords_s <- coords_s[rownames(coords_s) %in% S, , drop = FALSE]
  site_s <- rownames(coords_s)

  # initialisation
  rownames(coords_s) <- site_s
  names(color_palette) <- site_s

  # matricial form
  outflows <- numeric(n)
  names(outflows) <- site_s
  inflows <- numeric(n)
  names(inflows) <- site_s

  # outflows / Inflows
  for (k in 1:N) {
    outflows[as.character(index_o)[k]] <- outflows[as.character(index_o)[k]] + y[k]
    inflows[as.character(index_d)[k]] <- inflows[as.character(index_d)[k]] + y[k]
  }

  # fonction qui créé un arc entre deux points
  my_arc <- function(xA, yA, xB, yB) {
    xC <- (xA + xB)/2
    yC <- (yA + yB)/2
    R <- sqrt((yB - yA) ^ 2 + (xB - xA)^2)
    slope <- (yB - yA)/(xB - xA)
    s <- -1 / slope
    k <- sqrt(3) / 2 * R
    if (xB > xA) {
      xO <- xC - sqrt(k^2 / (s^2 + 1))
      yO <- yC - k * s / sqrt(s^2 + 1)
    } else {
      xO <- xC + sqrt(k^2 / (s^2 + 1))
      yO <- yC + k * s / sqrt(s^2 + 1)
    }
    my_x <- seq(xA, xB, length.out = 100)
    if (yO < min(yA,yB)) {
      my_y <- yO + sqrt(R^2 - (my_x - xO)^2)
    } else {
      if (yO > max(yA, yB)) {
        my_y <- yO - sqrt(R^2 - (my_x - xO)^2)
      } else {
        if(xA > xB) {
          my_x_1 <- seq(xA, xO - R, length.out = 100)
          my_x_2 <- seq(xO - R, xB, length.out = 100)
        } else {
          my_x_1 <- seq(xA, xO + R, length.out = 100)
          my_x_2 <- seq(xO + R, xB, length.out = 100)
        }

        cond_positiv <- (R^2 - (my_x_1 - xO)^2 > 0) & (R^2 - (my_x_2 - xO)^2 > 0)
        if(yA > yO) {
          my_y_1 <- yO + sqrt(R^2 - (my_x_1[cond_positiv] - xO)^2)
          my_y_2 <- yO - sqrt(R^2 - (my_x_2[cond_positiv] - xO)^2)
        } else {
          my_y_1 <- yO - sqrt(R^2 - (my_x_1[cond_positiv] - xO)^2)
          my_y_2 <- yO + sqrt(R^2 - (my_x_2[cond_positiv] - xO)^2)
        }
        my_y<-c(my_y_1, my_y_2)
        my_x<-c(my_x_1[cond_positiv], my_x_2[cond_positiv])
      }
    }
    my_na <- is.na(my_y)
    return(cbind(my_x[!my_na], my_y[!my_na]))
  }

  # initialisation
  max_lwd <- max_lwd * 7
  max_bar <- max_bar * 1.8
  # width of the flows
  max_lwd_flows <- max_lwd * y / max(y, na.rm = T)
  # vector of colors for the flows
  my_col_flow <- color_palette[as.numeric(factor(index_o, levels = site_s))]
  my_col_bar <- color_palette[as.numeric(factor(O, levels = site_s))]

  # parameter to shift the destination barplot
  shift <- 1 / 50

  # define the coordinates of origin site: it corresponds to the coordinates
  # of s in S, slightly shifted
  shift_coords_x <- diff(range(coords_s[, 1])) * shift / 2
  shift_coords_y <- diff(range(coords_s[, 2])) * shift / 12

  xy_origin <- coords_s[O, ]
  xy_origin[, 1] <- xy_origin[, 1] - shift_coords_x
  xy_origin[, 2] <- xy_origin[, 2] - shift_coords_y

  xy_dest <- coords_s[D, ]
  xy_dest[, 1] <- xy_dest[, 1] + shift_coords_x
  xy_dest[, 2] <- xy_dest[, 2] + shift_coords_y

  # shift of the two bar
  shift_bar <- diff(range(xy_origin[, 1])) * shift / 3

  # maximum height for the bars
  max_bar <- shift_bar * 6 * max_bar
  bar_out <-  max_bar * outflows[O] / max(c(inflows, outflows), na.rm = T)
  bar_in <- max_bar * inflows[D] / max(c(inflows, outflows), na.rm = T)

  if (!add) {
   plot(coords_s[, 1], coords_s[, 2], type = "n", xaxt = "n", yaxt = "n",
        xlab = "", ylab = "", frame = F, asp = 1)
  }

  # plot the highest flows
  ind_biggest <- which(y > quantile(y, filter_lowest, na.rm = TRUE))
  for(i in ind_biggest) {
      A <- xy_origin[as.character(index_o[i]), ]
      B <- xy_dest[as.character(index_d[i]), ]
      xA <- A[[1]]
      yA <- A[[2]]
      xB <- B[[1]]
      yB <- B[[2]]
      my_arc_don <- my_arc(xA, yA, xB, yB)
      lines(my_arc_don[, 1], my_arc_don[, 2],
            lwd = max_lwd_flows[i], col = my_col_flow[i])
  }

  bar_1x <- cbind(xy_origin[,1] - shift_bar, xy_origin[,1] + shift_bar,
                    xy_origin[,1] + shift_bar, xy_origin[,1] - shift_bar,
                    xy_origin[,1] - shift_bar)
  bar_2x <- cbind(xy_dest[,1] - shift_bar, xy_dest[,1] + shift_bar,
                    xy_dest[,1] + shift_bar, xy_dest[,1] - shift_bar,
                    xy_dest[,1] - shift_bar)

  # outflows bar
  for(k in 1:n_o) {
      bar_1y <- cbind(xy_origin[k, 2], xy_origin[k, 2],
                      xy_origin[k, 2] + bar_out[k],
                      xy_origin[k, 2] + bar_out[k],
                      xy_origin[k, 2])
      polygon(bar_1x[k, ], bar_1y, col = my_col_bar[k])
  }

  # redefine the y_coordinates of the barplot of the inflows origin = destination
  xy_dest[, 2] <- xy_dest[, 2] - 2 * shift_coords_y

  # inflows bar
  for(k in 1:n_d) {
    local_inflow <- numeric(n_o)
      for (i in 1:n_o) {
        does_ind_exist <- which(index_o == O[i] & index_d == D[k])
        if (length(does_ind_exist) == 1)
          local_inflow[i] <- y[does_ind_exist]
      }
      my_cum_sum <- xy_dest[k, 2] + c(0, bar_in[k] * cumsum(local_inflow) / sum(local_inflow))

      for (j in 1:n_o) {
        bar_2y <- cbind(my_cum_sum[j], my_cum_sum[j],
                        my_cum_sum[j+1], my_cum_sum[j+1],
                        my_cum_sum[j])

        polygon(bar_2x[k,], bar_2y, col = my_col_bar[j], border = my_col_bar[j])
      }
      polygon(bar_2x[k, ], cbind(my_cum_sum[1], my_cum_sum[1],
                                 my_cum_sum[n_o + 1], my_cum_sum[n_o + 1],
                                 my_cum_sum[1]))
   }

  # print labels
  if (add_labels) {
   bar_out_S <-  max_bar * outflows / max(c(inflows, outflows), na.rm = T)
   bar_in_S <- max_bar * inflows / max(c(inflows, outflows), na.rm = T)
   text(coords_s[site_s, 1] ,
        coords_s[site_s, 2] + apply(cbind(bar_out_S, bar_in_S), 1, max),
        site_s, adj = c(0.5, 0.), cex = 0.6)
  }

  # plot the legend
  if (legend_position != "none") {
    # plot the legend of the flows
    flows_legend <- round(quantile(
      y, seq(filter_lowest, 1, length.out = 4) ^ c(1, 0.5, 0.5, 0.5)),
      decimal_points)
    xy_leg <- legend(legend_position,
           legend = flows_legend,
           lty = 1,
           lwd = max_lwd * flows_legend / max(y, na.rm = T),
           cex = 0.6,
           title = "Flow size",
           box.lwd = 0)

    x_left <- xy_leg$rect$left + xy_leg$rect$w / 4
    if(legend_position %in% c("left", "topleft", "top",
                              "topright", "right", "center")) {
      y_bottom <- xy_leg$rect$top - 2 * xy_leg$rect$h
    } else {
      y_bottom <- xy_leg$rect$top + xy_leg$rect$h / 4
    }
    # max of the outflows
    polygon(cbind(x_left - shift_bar,
                  x_left + shift_bar,
                  x_left + shift_bar,
                  x_left - shift_bar,
                  x_left - shift_bar),
            cbind(y_bottom, y_bottom,
                  y_bottom + max(bar_out, bar_in),
                  y_bottom + max(bar_out, bar_in),
                  y_bottom))

    # max of the inflows
    polygon(cbind(x_left - shift_bar + diff(range(xy_origin[, 1])) * shift,
                  x_left + shift_bar + diff(range(xy_origin[, 1])) * shift,
                  x_left + shift_bar + diff(range(xy_origin[, 1])) * shift,
                  x_left - shift_bar + diff(range(xy_origin[, 1])) * shift,
                  x_left - shift_bar + diff(range(xy_origin[, 1])) * shift),
            cbind(y_bottom, y_bottom,
                  y_bottom + max(bar_out, bar_in) / 2,
                  y_bottom + max(bar_out, bar_in) / 2,
                  y_bottom))

    # Print out and In
    text(x_left, y_bottom + max(bar_out, bar_in), "Out / In", cex = 0.5, pos = 3)
    # text(x_left + diff(range(xy_origin[, 1])) * shift, y_bottom, "In", cex = 0.5, pos = 1)

    # Print the arrows
    arrows(x_left + 2 * diff(range(xy_origin[, 1])) * shift,
           y_bottom, x_left + 2 * diff(range(xy_origin[, 1])) * shift,
           y_bottom + max(bar_out, bar_in),
           length = 0.1)

    # Print the values
    text(x_left + 2.1 * diff(range(xy_origin[, 1])) * shift,
         y_bottom, "0", cex = 0.5, pos = 4)
    text(x_left + 2.1 * diff(range(xy_origin[, 1])) * shift,
         y_bottom + max(bar_out, bar_in),
         round(max(outflows, inflows), decimal_points),
         cex = 0.5, pos = 4)
    text(x_left + 2.1 * diff(range(xy_origin[, 1])) * shift,
         y_bottom + max(bar_out, bar_in) / 2,
         round(max(outflows, inflows) / 2, decimal_points),
         cex = 0.5, pos = 4)
  }
}
