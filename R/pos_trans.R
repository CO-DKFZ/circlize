
# == title
# Genomic position transformation function
#
# == param
# -region Genomic positions at a single chromosome. It is a data frame with two
#     columns which are start position and end position.
# -... other arguments
#
# == details
# The default position transformation functions transforms position to be equally distributed
# along the chromosome. If users want to define their own transformation function, the requirement
# is that the returned value should be a data frame with two columns: transformed start position
# and transformed end position. The returned value should have same number of rows as the input one.
#
# For details why need to use position transformation, please refer to `circos.genomicPosTransformLines`.
posTransform.default = function(region, ...) {
	xlim = get.cell.meta.data("xlim")
	segment = seq(xlim[1], xlim[2], length.out = nrow(region) + 1)
	return(data.frame(start = segment[-length(segment)], end = segment[-1]))
}

# == title
# Genomic position transformation function specifically for text
#
# == param
# -region Genomic positions at a single chromosome. It is a data frame with two
#     columns which are start position and end position.
# -y positions of texts
# -labels text labels
# -cex  text size
# -font  text font style
# -sector.index sector index
# -track.index track index
# -padding padding of text
# -extend extend to allow labels to be put in an region which is wider than the current chromosome.
#    The value should be a proportion value and the length is either one or two.
# -... other arguments
#
# == details
# This position transformation function is designed specifically for text.
# Under the transformation, texts will be as close as possible to the original positions.
posTransform.text = function(
	region, 
	y, 
	labels, 
	cex = 1, 
	font = par("font"),
	sector.index = get.cell.meta.data("sector.index"),
	track.index = get.cell.meta.data("track.index"), 
	padding = 0, 
	extend = 0, 
	...) {

	if(length(y) == 1) y = rep(y, nrow(region))
	if(length(labels) == 1) labels = rep(labels, nrow(region))

	if(length(extend) == 1) extend = rep(extend, 2)
	if(length(extend) > 2) extend = extend[1:2]

	od = order(region[, 1] + region[, 2])
	od_back = NULL
	od_back[od] = seq_along(od)
	region = region[od, ,drop = FALSE]
	y = y[od]
	labels = labels[od]

	text_height = strheight(labels, cex = cex, font = font)*(1+padding)
	
	d = circlize( (region[[1]] + region[[2]])/2, y, sector.index = sector.index, track.index = track.index)
	alpha1 = d[, "theta"] + as.degree(atan(text_height/2/d[, "rou"]))
	alpha2 = d[, "theta"] - as.degree(atan(text_height/2/d[, "rou"]))
	x1 = reverse.circlize(alpha1, d[, "rou"], sector.index = sector.index, track.index = track.index)[, "x"]
	x2 = reverse.circlize(alpha2, d[, "rou"], sector.index = sector.index, track.index = track.index)[, "x"]
	
	xlim = get.cell.meta.data("cell.xlim", sector.index = sector.index, track.index = track.index)
	xrange = xlim[2] - xlim[1]
	xlim = c(xlim[1] - extend[1]*xrange, xlim[2] + extend[2]*xrange)

	x1_new = x1
	x2_new = x2
	l = x2 - x1 >= xlim[2] - xlim[1]; x1_new[l] = xlim[1]; x2_new[l] = xlim[2]
	l = x1 < xlim[1]; x1_new[l] = xlim[1]; x2_new[l] = x2[l] + xlim[1] - x1[l]
	l = x2 > xlim[2]; x1_new[l] = x1[l] - (x2[l] - xlim[2]); x2_new[l] = xlim[2]
	df = smartAlign(x1_new, x2_new, xlim = xlim)

	return(df[od_back, ,drop = FALSE])
}



# == title
# Add genomic position transformation lines between tracks
#
# == param
# -data A data frame containing genomic data.
# -track.height Height of the track.
# -posTransform Genomic position transformation function, see `posTransform.default` for an example.
# -horizontalLine Whether to draw horizontal lines which indicate region width .
# -track.margin Margin of tracks.
# -direction Type of the transformation. ``inside`` means position transformed track are located inside 
#       and ``outside`` means position transformed track are located outside.
# -col Color of lines, can be length of one or nrow of ``data``.
# -lwd Width of lines.
# -lty Style of lines.
# -... Pass to `circos.trackPlotRegion`.
#
# == details
# There is one representative situation when such position transformation needs to be applied. 
# For example, there are two sets of regions in a chromosome in which regions in one set regions are
# quite densely to each other and regions in other set are far from others. Heatmap or text is going
# to be drawn on the next track. If there is no position transformation, heatmap or text for those
# dense regions would be overlapped and hard to identify, also ugly to visualize. Thus, a way
# to transform original positions to new positions would help for the visualization. 
circos.genomicPosTransformLines = function(
	data, 
	track.height = 0.1, 
	posTransform = NULL, 
	horizontalLine = c("none", "top", "bottom", "both"), 
	track.margin = c(0, 0),
	direction = c("inside", "outside"), 
	col = "black", 
	lwd = par("lwd"),
    lty = par("lty"), 
    ...) {
	
	horizontalLine = match.arg(horizontalLine)[1]

	data = validate_data_frame(data)
	data = normalizeToDataFrame(data)
	
	if(is.dataFrameList(data)) {
		stop_wrap("`data` can not be list of regions.")
	}
	
	nr = nrow(data)
	if(length(col) == 1) {
		col = rep(col, nr)
	}
	if(length(lwd) == 1) {
		lwd = rep(lwd, nr)
	}
	if(length(lty) == 1) {
		lty = rep(lty, nr)
	}

	o.track.margin = circos.par("track.margin")
	circos.par(track.margin = track.margin)
	
	if(direction[1] == "default") direction = "outside"
	if(direction[1] == "reverse") direction = "inside"
	direction = match.arg(direction)[1]
	
	if(direction == "inside") {
		circos.genomicTrackPlotRegion(data, ylim = c(0, 1), bg.border = NA, track.height = track.height, panel.fun = function(region, value, ...) {
			chr = get.current.chromosome()
			l = data[[1]] == chr
			if(!is.null(posTransform)) {
				if(is.function(posTransform)) {
					args = as.list(posTransform)
					if(length(args) == 2) {
						region_new = posTransform(region)
					} else if(length(args) == 3) {
						region_new = posTransform(region, value)
					}
				}
			} else {
				region_new  = region
			}
			
			# for(i in seq_len(nrow(region))) {
			# 	if(horizontalLine == "both" || horizontalLine == "top") {
			# 		circos.lines(c(region[i, 1], region[i, 2]), c(1, 1), col = col[l][i], lwd = lwd[l][i], lty = lty[l][i])
			# 	}
			# 	if(horizontalLine == "both" || horizontalLine == "bottom") {
			# 		circos.lines(c(region[i, 1], region[i, 2]), c(0, 0), col = col[l][i], lwd = lwd[l][i], lty = lty[l][i])
			# 	}
			# 	mid = (region[i, 1] + region[i, 2])/2
			# 	mid_new = (region_new[i, 1] + region_new[i, 2])/2
			# 	circos.lines(c(mid, mid, mid_new, mid_new), c(1, 2/3, 1/3, 0), col = col[l][i], lwd = lwd[l][i], lty = lty[l][i])
			# }
			nr = nrow(region)
			if(horizontalLine == "both" || horizontalLine == "top") {
				circos.segments(region[, 1], rep(1, nr), region[, 2], rep(1, nr), col = col[l], lwd = lwd[l], lty = lty[l])
			}
			if(horizontalLine == "both" || horizontalLine == "bottom") {
				circos.segments(region[, 1], rep(0, nr), region[, 2], rep(0, nr), col = col[l], lwd = lwd[l], lty = lty[l])
			}
			mid = (region[, 1] + region[, 2])/2
			mid_new = (region_new[, 1] + region_new[, 2])/2
			circos.segments(mid, rep(1, nr), mid, rep(2/3, nr), col = col[l], lwd = lwd[l], lty = lty[l])
			circos.segments(mid, rep(2/3, nr), mid_new, rep(1/3, nr), col = col[l], lwd = lwd[l], lty = lty[l])
			circos.segments(mid_new, rep(1/3, nr), mid_new, rep(0, nr), col = col[l], lwd = lwd[l], lty = lty[l])
		}, ...)
	} else {
		circos.genomicTrackPlotRegion(data, ylim = c(0, 1), bg.border = NA, track.height = track.height, panel.fun = function(region, value, ...) {
			chr = get.current.chromosome()
			l = data[[1]] == chr
			region_subset = data[l, , drop = FALSE]
			if(!is.null(posTransform)) {
				if(is.function(posTransform)) {
					args = as.list(posTransform)
					if(length(args) == 2) {
						region_new = posTransform(region)
					} else if(length(args) == 3) {
						region_new = posTransform(region, value)
					}
				}
			} else {
				region_new  = region
			}
			
			# for(i in seq_len(nrow(region))) {
			# 	if(horizontalLine == "both" || horizontalLine == "bottom") {
			# 		circos.lines(c(region[i, 1], region[i, 2]), c(0, 0), col = col[l][i], lwd = lwd[l][i], lty = lty[l][i])
			# 	}
			# 	if(horizontalLine == "both" || horizontalLine == "top") {
			# 		circos.lines(c(region[i, 1], region[i, 2]), c(1, 1), col = col[l][i], lwd = lwd[l][i], lty = lty[l][i])
			# 	}
			# 	mid = (region[i, 1] + region[i, 2])/2
			# 	mid_new = (region_new[i, 1] + region_new[i, 2])/2
			# 	circos.lines(c(mid, mid, mid_new, mid_new), c(0, 1/3, 2/3, 1), col = col[l][i], lwd = lwd[l][i], lty = lty[l][i])
			# }
			nr = nrow(region)
			if(horizontalLine == "both" || horizontalLine == "bottom") {
				circos.segments(region[, 1], rep(0, nr), region[, 2], rep(0, nr), col = col[l], lwd = lwd[l], lty = lty[l])
			}
			if(horizontalLine == "both" || horizontalLine == "top") {
				circos.segments(region[, 1], rep(1, nr), region[, 2], rep(1, nr), col = col[l], lwd = lwd[l], lty = lty[l])
			}
			mid = (region[, 1] + region[, 2])/2
			mid_new = (region_new[, 1] + region_new[, 2])/2
			circos.segments(mid, rep(0, nr), mid, rep(1/3, nr), col = col[l], lwd = lwd[l], lty = lty[l])
			circos.segments(mid, rep(1/3, nr), mid_new, rep(2/3, nr), col = col[l], lwd = lwd[l], lty = lty[l])
			circos.segments(mid_new, rep(2/3, nr), mid_new, rep(1, nr), col = col[l], lwd = lwd[l], lty = lty[l])
		}, ...)
	}
	
	circos.par(track.margin = o.track.margin)
}


# == title
# Adjust positions of text
#
# == param
# -x1 Position which corresponds to the top of the text.
# -x2 Position which corresponds to the bottom of the text.
# -xlim Ranges on x-axis.
#
# == details
# used internally
smartAlign = function(x1, x2, xlim) {

	ncluster.before = -1
	ncluster = length(x1)
	while(ncluster.before != ncluster) {
		ncluster.before = ncluster
		cluster = rep(0, length(x1))
		i_cluster = 1
		cluster[1] = i_cluster
		for(i in seq_along(x1)[-1]) {
			# overlap with previous one
			if(x1[i] <= x2[i-1]) {
				cluster[i] = i_cluster
			} else {
				i_cluster = i_cluster + 1
				cluster[i] = i_cluster
			}
		}
		ncluster = length(unique(cluster))
		
		if(ncluster.before == ncluster) break
		
		# tile intervals in each cluster and re-assign x1 and x2
		new_x1 = numeric(length(x1))
		new_x2 = numeric(length(x2))
		for(i_cluster in unique(cluster)) {
			index = which(cluster == i_cluster)
			len = x2[index] - x1[index]
			total_len = sum(len)
			mid = (min(x1[index]) + max(x2[index]))/2
			if(total_len > xlim[2] - xlim[1]) {
				tp = c(0, cumsum(len)) + (xlim[1] + xlim[2])/2 - total_len/2
			} else if(mid - total_len/2 < xlim[1]) {
				tp = c(0, cumsum(len)) + xlim[1]
			} else if(mid + total_len/2 > xlim[2]) {
				tp = xlim[2] - total_len + c(0, cumsum(len))
			} else {
				tp = c(0, cumsum(len)) + mid - total_len/2
			}
			new_x1[index] = tp[-length(tp)]
			new_x2[index] = tp[-1]
		}
		
		x1 = new_x1
		x2 = new_x2
	}
	
	return(data.frame(start = x1, end = x2))
}
