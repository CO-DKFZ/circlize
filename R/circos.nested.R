# == title
# Nested zooming with two circular plots
#
# == param
# -f1 a self-defined function with no argument for making the first circular plot
# -f2 a self-defined function with no argument for making the second circular plot
# -correspondance a six-column data frame which contains correspondance between the
#               coordinates in two circular plots
# -connection_height the height of the connections, measured as the percent to the radius of the unit circle.
#         The value can be specified by `uh` or `convert_height` with absolute units.
# -connection_col filled color for the connection polygons. The value can be a vector with same length as number of rows of ``correspondance``
# -connection_border border color for the connection polygons
# -connection_lty line style for the connection polygon borders
# -connection_lwd connection polygon border width
# -adjust_start_degree If ``start.degree`` is not set in ``f2()``, the start degree for the second
#       circular plot will be adjusted to make the distance of sectors between the two plots to the minimal.
#
# == details
# The function visualizes zoomings by combining two circular plots into one page where
# one is the normal circular plot and the other one only contains regions need to be zoomed.
# This function automatically arranges the two plots to make it easy to correspond between
# the original and the zoomed plots.
#
# Since the function needs to know the information of the two circular plots, please do not call
# `circos.clear` in either ``f1()`` or ``f2()``. It will be called internally in `circos.nested`.
#
# If ``adjust_start_degree`` is set to ``TRUE``, ``start.degree`` should not be set in ``f2()``.
# Also ``canvas.xlim`` and ``canvas.ylim`` are reset in ``f2()``, they should not be set in ``f2()``
# either.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
circos.nested = function(f1, f2, correspondance, connection_height = convert_height(5, "mm"),
	connection_col = NA, connection_border = "black",
	connection_lty = par("lty"), connection_lwd = par("lwd"),
	adjust_start_degree = TRUE) {

	on.exit(circos.clear())

	pdf(NULL)
	oe = try({
		f2()
		if(!is.circos.initialized()) {
			stop("Do not call `circos.clear()` in `f2`.")
		}
		param2 = circos.par()
		correspondance[[1]] = as.vector(correspondance[[1]])
		correspondance[[4]] = as.vector(correspondance[[4]])
		all_sn = get.all.sector.index()
		l = correspondance[[4]] %in% all_sn
		correspondance = correspondance[l, , drop = FALSE]
		if(nrow(correspondance) == 0) {
			stop("Cannot map `correspondance` to sectors in `f2()`.")
		}
		if(length(connection_col) > 1) connection_col = connection_col[l]
		if(length(connection_border) > 1) connection_border = connection_border[l]
		if(length(connection_lty) > 1) connection_lty = connection_lty[l]
		if(length(connection_lwd) > 1) connection_lwd = connection_lwd[l]
		for(i in seq_len(nrow(correspondance))) {
		    correspondance[i, "c2_theta1"] = circlize(correspondance[i, 5], 1, sector.index = correspondance[i, 4])[1, 1]
		    correspondance[i, "c2_theta2"] = circlize(correspondance[i, 6], 1, sector.index = correspondance[i, 4])[1, 1]
		}
		if(!identical(c(-1, 1), param2$canvas.xlim)) {
			stop("Do not modify circos.par('canvas.xlim') in `f2()`.")
		}
		if(!identical(c(-1, 1), param2$canvas.ylim)) {
			stop("Do not modify circos.par('canvas.ylim') in `f2()`.")
		}
		if(adjust_start_degree) {
			if(!identical(0, param2$start.degree)) {
				stop("Do not modify circos.par('start.degree') when adjust_start_degree = TRUE in `f2()`.")
			}
		}
	})
	circos.clear()
	dev.off()

	circos.par(points.overflow.warning = FALSE)
	f1()
	if(!is.circos.initialized()) {
		stop("Do not call `circos.clear()` in `f1`.")
	}
	param1 = circos.par()
	if(abs(abs(param1$canvas.xlim[1]) - abs(param1$canvas.xlim[2])) > 1e-6) {
		stop("`canvas.xlim` should be symmetric to zero in `f1()`.")
	}
	if(abs(abs(param1$canvas.ylim[1]) - abs(param1$canvas.ylim[2])) > 1e-6) {
		stop("`canvas.ylim` should be symmetric to zero in `f1()`.")
	}
	if(abs(abs(param1$canvas.xlim[1]) - abs(param1$canvas.ylim[1])) > 1e-6) {
		stop("Canvas coordinate should be square in `f1()`.")
	}
	all_sn = get.all.sector.index()
	l = correspondance[[1]] %in% all_sn
	correspondance = correspondance[l, , drop = FALSE]
	if(nrow(correspondance) == 0) {
		stop("Cannot map `correspondance` to sectors in `f1()`.")
	}
	if(length(connection_col) > 1) connection_col = connection_col[l]
	if(length(connection_border) > 1) connection_border = connection_border[l]
	if(length(connection_lty) > 1) connection_lty = connection_lty[l]
	if(length(connection_lwd) > 1) connection_lwd = connection_lwd[l]
	for(i in seq_len(nrow(correspondance))) {
	    correspondance[i, "c1_theta1"] = circlize(correspondance[i, 2], 1, sector.index = correspondance[i, 1])[1, 1]
	    correspondance[i, "c1_theta2"] = circlize(correspondance[i, 3], 1, sector.index = correspondance[i, 1])[1, 1]
	}
	r1 = get_most_inside_radius()
	r2 = r1 - connection_height

	## adjust start degree of the second circular plot
	if(adjust_start_degree) {
		t1 = rowMeans(correspondance[, 7:8])
		t2 = rowMeans(correspondance[, 9:10])

		sum_diff = numeric(360)
		for(offset in 0:359) {
			diff_degree = abs(t1 - t2 + offset)
			diff_degree = ifelse(diff_degree > 180, diff_degree - 180, diff_degree)
			sum_diff[offset + 1] = sum(diff_degree)
		}
		offset = which.min(sum_diff) - 1
		correspondance[, "c2_theta1"] = correspondance[, "c2_theta1"] + offset
		correspondance[, "c2_theta2"] = correspondance[, "c2_theta2"] + offset
	}

	# the coordinate convert is applied in the first circular plot
	# connection is from the top to the bottom of this track
	make_connection = function(chr, c1_theta1, c1_theta2, c2_theta1, c2_theta2,
		col, border, lty, lwd) {
		cell.top.radius = get.cell.meta.data("cell.top.radius")
		cell.bottom.radius = get.cell.meta.data("cell.bottom.radius")
		df = reverse.circlize(c(c1_theta1, c1_theta2, c2_theta1, c2_theta2), 
			c(cell.top.radius, cell.top.radius, cell.bottom.radius, cell.bottom.radius))

	    x21 = df[1, 1]
	    x22 = df[2, 1]
	    y21 = df[1, 2]
	    y22 = df[2, 2]
	    x11 = df[3, 1]
	    x12 = df[4, 1]
	    y11 = df[3, 2]
	    y12 = df[4, 2]
	    circos.polygon(c(x11, x11, x21, x21, x22, x22, x12, x12, x11),
	                   c(y11, (y21 - y11)/3, (y21 - y11)/3*2, y21, y22, (y22 - y12)/3*2, (y22 - y12)/3, y12, y11),
	                   col = col, border = border, lty = lty, lwd = lwd)
	}

	nr = nrow(correspondance)
	if(length(connection_col) == 1) {
		connection_col = rep(connection_col, nr)
	}
	if(length(connection_border) == 1) {
		connection_border = rep(connection_border, nr)
	}
	if(length(connection_lty) == 1) {
		connection_lty = rep(connection_lty, nr)
	}
	if(length(connection_lwd) == 1) {
		connection_lwd = rep(connection_lwd, nr)
	}

	# make the connections 
	circos.track(ylim = c(0, 1), track.height = connection_height, panel.fun = function(x, y) {

	    l = correspondance[[1]] == CELL_META$sector.index
	    if(sum(l) == 0) {
	    	return(NULL)
	    }
	    
	    for(i in which(l)) {
	    	make_connection(correspondance[i, 1], correspondance[i, "c1_theta1"], correspondance[i, "c1_theta2"],
	    		correspondance[i, "c2_theta1"], correspondance[i, "c2_theta2"],
	    		connection_col[i], connection_border[i], connection_lty[i], connection_lwd[i])
	    }
	    
	}, track.margin = c(0, 0), cell.padding = c(0, 0, 0, 0), bg.border = NA)

	circos.clear()

	op = par("new")
	par(new = TRUE)
	
	if(adjust_start_degree) {
		circos.par(start.degree = offset)
	}

	circos.par(canvas.xlim = c(-param1$canvas.xlim[2]/r2, param1$canvas.xlim[2]/r2), 
		       canvas.ylim = c(-param1$canvas.xlim[2]/r2, param1$canvas.xlim[2]/r2))
	f2()
	circos.clear()
	par(new = op)
}