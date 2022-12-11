
# == title
# Draw horizon chart along the spiral
#
# == param
# -x X-locations of the data points.
# -y Y-locations of the data points.
# -y_max Maximal absolute value on y-axis.
# -n_slices Number of slices.
# -slice_size Size of the slices. The final number of sizes is ``ceiling(max(abs(y))/slice_size)``.
# -pos_fill Colors for positive values. 
# -neg_fill Colors for negative values.
# -negative_from_top Should negative distribution be drawn from the top?
# -track_index Index of the track. 
#
# == details
# Since the track height is very small in the spiral, horizon chart visualization is a efficient way to visualize
# distribution-like graphics.
#
# == value
# A list of the following objects:
# 
# - a color mapping function for colors.
# - a vector of intervals that split the data.
#
# == example
# \donttest{
# df = readRDS(system.file("extdata", "global_temperature.rds", package = "spiralize"))
# df = df[df$Source == "GCAG", ]
# spiral_initialize_by_time(xlim = range(df$Date), unit_on_axis = "months", period = "year",
#     period_per_loop = 20, polar_lines_by = 360/20, 
#     vp_param = list(x = unit(0, "npc"), just = "left"))
# spiral_track()
# spiral_horizon(df$Date, df$Mean, use_bar = TRUE)
# }
circos.horizon = function(x, y, y_max = max(abs(y)), n_slices = 4, slice_size, 
	pos_fill = "#D73027", neg_fill = "#313695",
	negative_from_top = FALSE, 
	sector.index = get.current.sector.index(),
	track.index = get.current.track.index()) {

	ylim = get.cell.meta.data("ylim", sector.index = sector.index, track.index = track.index)
	if(!(ylim[1] == 0 & ylim[2] == 1)) {
		stop_wrap("The track must have 'ylim = c(0, 1)'.")
	}

	if(missing(slice_size)) {
		slice_size = y_max/n_slices
	}
	n_slices = ceiling(y_max/slice_size)

	if(n_slices == 0) {
		return(invisible(NULL))
	}

	n = length(x)

	l = is.na(x)
	x = x[!l]
	y = y[!l]

	l = is.na(y)
	y[l] = 0

	od = order(x)
	x = x[od]
	y = y[od]

	if(all(y >= 0)) {
		y_type = "positive"
	} else if(all(y <= 0)) {
		y_type = "negative"
	} else {
		y_type = "both"
	}

	pos_col_fun = colorRamp2(c(0, n_slices), c("white", pos_fill))
	neg_col_fun = colorRamp2(c(0, n_slices), c("white", neg_fill))
	if(y_type %in% c("positive", "both")) {
		for(i in seq_len(n_slices)) {
			l1 = y >= (i-1)*slice_size & y < i*slice_size
			l2 = y < (i-1)*slice_size
			l3 = y >= i*slice_size
			if(any(l1)) {
				x2 = x
				y2 = y
				y2[l1] = y2[l1] - slice_size*(i-1)
				y2[l3] = slice_size
				x2[l2] = NA
				y2[l2] = NA

				add_horizon_polygons(x2, y2, slice_size = slice_size, 
						col = pos_col_fun(i), border = NA, sector.index = sector.index, track.index = track.index)
			}
		}
	}
	if(y_type %in% c("negative", "both")) {
		y = -y
		for(i in seq_len(n_slices)) {
			l1 = y >= (i-1)*slice_size & y < i*slice_size
			l2 = y < (i-1)*slice_size
			l3 = y >= i*slice_size
			if(any(l1)) {
				x2 = x
				y2 = y
				y2[l1] = y2[l1] - slice_size*(i-1)
				y2[l3] = slice_size
				x2[l2] = NA
				y2[l2] = NA

				add_horizon_polygons(x2, y2, slice_size = slice_size, from_top = negative_from_top, 
					col = neg_col_fun(i), border = NA, sector.index = sector.index, track.index = track.index)
				
			}
		}
	}

	interval = 0:n_slices*slice_size
	if(y_type == "positive") {
		col_fun = colorRamp2(c(0, n_slices*slice_size), c("white", pos_fill))
		return(invisible(list(col_fun = col_fun, interval = interval)))
	} else if(y_type == "negative") {
		col_fun = colorRamp2(c(-n_slices*slice_size, 0), c(neg_fill, "white"))
		return(invisible(list(col_fun = col_fun, interval = -rev(interval))))
	} else {
		col_fun = colorRamp2(c(-n_slices*slice_size, 0, n_slices*slice_size), c(neg_fill, "white", pos_fill))
		return(invisible(list(col_fun = col_fun, interval = seq(-n_slices, n_slices)*slice_size)))
	}
}

add_horizon_polygons = function(x, y, slice_size = NULL, from_top = FALSE, ...) {
	ltx = split_vec_by_NA(x)
	lty = split_vec_by_NA(y)

	for(i in seq_along(ltx)) {
		x0 = ltx[[i]]
		y0 = lty[[i]]
		if(from_top) {
			x0 = c(x0[1], x0, x0[length(x0)], x0[1])
			y0 = c(slice_size, slice_size - y0, slice_size, slice_size)
		} else {
			x0 = c(x0[1], x0, x0[length(x0)], x0[1])
			y0 = c(0, y0, 0, 0)
		}
		circos.polygon(x0, y0/slice_size, ...)
	}
}

# https://stat.ethz.ch/pipermail/r-help/2010-April/237031.html
split_vec_by_NA = function(x) {
	idx = 1 + cumsum(is.na(x))
	not.na = !is.na(x)
	split(x[not.na], idx[not.na])
}
