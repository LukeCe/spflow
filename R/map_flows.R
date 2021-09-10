map_flows <- function(y, index_o, index_d, coords_s, site_s,
                      q4 = sample(colors(), size = nrow(coords_s)),
                      add = F, maxlwd = 1, alpha.q = 0.75, max_bar = 1, 
                      add.legend = T, round.values = 0) {
  
  # verification
  # size of the vectors
  stopifnot(length(y) == length(index_o),
            length(index_o) == length(index_d))
 
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
 
  # Check on the spatial coordinates data 
  stopifnot(all(S %in% site_s))

  # initialisation
  rownames(coords_s) <- site_s
  names(q4) <- site_s
  
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
  maxlwd <- maxlwd * 7
  max_bar <- max_bar * 1.8
  # width of the flows
  maxlwd <- maxlwd * y / max(y, na.rm = T)
  # vector of colors for the flows
  my_col_flow <- q4[as.numeric(factor(index_o, levels = site_s))] 
  my_col_bar <- q4[as.numeric(factor(O, levels = site_s))]
    
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
  ind_biggest <- which(y > quantile(y, alpha.q))
  for(i in ind_biggest) {
      A <- xy_origin[as.character(index_o[i]), ]
      B <- xy_dest[as.character(index_d[i]), ]
      xA <- A[1]
      yA <- A[2]
      xB <- B[1]
      yB <- B[2]
      my_arc_don <- my_arc(xA, yA, xB, yB)
      lines(my_arc_don[, 1], my_arc_don[, 2], 
            lwd = maxlwd[i], col = my_col_flow[i])
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
    
  # plot the legend
  if (add.legend) {
    # max of the outflows
    polygon(cbind(par()$xaxp[1] - shift_bar, 
                  par()$xaxp[1] + shift_bar, 
                  par()$xaxp[1] + shift_bar, 
                  par()$xaxp[1] - shift_bar,
                  par()$xaxp[1] - shift_bar),
            cbind(par()$yaxp[1], par()$yaxp[1], 
                  par()$yaxp[1] + max(bar_out), 
                  par()$yaxp[1] + max(bar_out),
                  par()$yaxp[1]))
    
    # max of the inflows
    polygon(cbind(par()$xaxp[1] - shift_bar + diff(range(xy_origin[, 1])) * shift, 
                  par()$xaxp[1] + shift_bar + diff(range(xy_origin[, 1])) * shift,
                  par()$xaxp[1] + shift_bar + diff(range(xy_origin[, 1])) * shift, 
                  par()$xaxp[1] - shift_bar + diff(range(xy_origin[, 1])) * shift,
                  par()$xaxp[1] - shift_bar + diff(range(xy_origin[, 1])) * shift),
            cbind(par()$yaxp[1], par()$yaxp[1], 
                  par()$yaxp[1] + max(bar_in), 
                  par()$yaxp[1] + max(bar_in),
                  par()$yaxp[1]))
    
    # Print out and In
    text(par()$xaxp[1], par()$yaxp[1], "Out", cex = 0.6, pos = 1)
    text(par()$xaxp[1] + diff(range(xy_origin[, 1])) * shift, par()$yaxp[1], "In", cex = 0.6, pos = 1)
    
    # Print the arrows 
    arrows(par()$xaxp[1] + 2 * diff(range(xy_origin[, 1])) * shift,   
           par()$yaxp[1], par()$xaxp[1] + 2 * diff(range(xy_origin[, 1])) * shift, 
           par()$yaxp[1] + max(bar_out, bar_in),
           length = 0.1)
    
    # Print the values  
    text(par()$xaxp[1] + 2.1 * diff(range(xy_origin[, 1])) * shift,
         par()$yaxp[1], round.values, cex = 0.5, pos = 4)
    text(par()$xaxp[1] + 2.1 * diff(range(xy_origin[, 1])) * shift, 
         par()$yaxp[1] + max(bar_out, bar_in), 
         round(max(outflows, inflows), round.values), 
         cex = 0.5, pos = 4)
    text(par()$xaxp[1] + 2.1 * diff(range(xy_origin[, 1])) * shift,
         par()$yaxp[1] + max(bar_out, bar_in) / 2, 
         round(max(outflows, inflows) / 2, round.values), 
         cex = 0.5, pos = 4)
    legend(par()$xaxp[2], par()$yaxp[2],
           legend = round(quantile(y, c(0.5, 0.95, 0.99, 0.999)), round.values)[-1], 
           lty = 1,
           lwd = quantile(maxlwd, c(0.5, 0.95, 0.99, 0.999))[-1],
           cex = 0.6)
  }
}
