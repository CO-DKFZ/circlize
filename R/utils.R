

# expand breakpoints in two points to draw an arc
# x and y are transformed and re-mapped points
lines.expand = function(x, y, sector.index = get.current.sector.index(), track.index = get.current.track.index()) {
    sector.data = get.sector.data(sector.index)
	cell.data = get.cell.data(sector.index, track.index)
    nx = x[1]
    ny = y[1]
	
    for(i in seq_along(x)) {
        if(i == 1) {
            next   
        }
        if(is.na(x[i]) || is.na(y[i])) {
        	nx = c(nx, NA)
        	ny = c(ny, NA)
        	next
        }
        if(is.na(x[i-1]) || is.na(y[i-1])) {
        	next
        }
	
		td = cbind(c(x[i-1], x[i]), c(y[i-1], y[i]))
        td = td[order(td[, 1]), ]
		td2 = circlize(td[, 1], td[, 2], sector.index = sector.index, track.index = track.index)
		
		a = as.radian(abs(td2[1, 1] - td2[2, 1]))
		b = abs(td2[1, 2] - td2[2, 2])
		l = sqrt(a^2 + b^2)
		
		ncut = l/ (2*pi/circos.par("unit.circle.segments"))
        ncut = floor(ncut)

		if(ncut) {
			j = seq_len(ncut) / (ncut + 1)
				
			nx = c(nx, x[i-1] + (x[i] - x[i-1])*j, x[i])
			ny = c(ny, y[i-1] + (y[i] - y[i-1])*j, y[i])
		} else {
            nx = c(nx, x[i])
            ny = c(ny, y[i])
        }
    }
   
    d = cbind(nx, ny)
    return(d)
}

# if x has same length as levels of factors
# the order of x is same as the order of levels
recycle.with.factors = function(x, factors) {
    le = levels(factors)
    if(length(x) == 1) {
        x = rep(x, length(factors))
    } else if(length(x) == length(le)) {
        b = factors
        levels(b) = x
        x = as.vector(b)
    }
    return(x)
}

recycle.with.levels = function(x, levels) {
    
    if(length(x) == 1) {
        x = rep(x, length(levels))
    } 
    return(x)
}

check.track.position = function(track.index, track.start, track.height) {

    track.margin = circos.par("track.margin")
    if(track.start - track.height - track.margin[2] < 0 ||
       track.start - track.height < 0 ||
       track.start < 0) {
        stop_wrap(paste("not enough space for cells at track index '", track.index, "'.\n", sep = ""))
    }
    if(track.start - track.margin[1] - track.height - track.margin[2] < 0) {
        stop_wrap(paste("not enough space for bottom margin of cells at track index '", track.index, "'.\n", sep = ""))
    }
    
    if(track.index > 1) {
        
        if(track.start > get.cell.meta.data("cell.bottom.radius", track.index = track.index - 1)) {
            stop_wrap("Track overlaps with previous track.\n")
        }
    }
}

check.points.position = function(x, y, sector.index = get.cell.meta.data("sector.index"),
	track.index = get.cell.meta.data("track.index")) {
        
    cell.xlim = get.cell.meta.data("cell.xlim", sector.index, track.index)
    cell.ylim = get.cell.meta.data("cell.ylim", sector.index, track.index)
    
    l = is.na(x) | is.na(y)
    x = x[!l]
    y = y[!l]

    xrange = cell.xlim[2] - cell.xlim[1]
    yrange = cell.ylim[2] - cell.ylim[1]
    l1 = x < cell.xlim[1] | x > cell.xlim[2]
    l2 = y < cell.ylim[1] | y > cell.ylim[2]
    l = l1 | l2
    if(sum(l) && circos.par("points.overflow.warning")) {
        message_wrap(paste("Note: ", sum(l), " point", ifelse(sum(l) == 1, " is", "s are"), " out of plotting region in sector '", sector.index, "', track '", track.index, "'.\n", sep = ""))
    }

    return(invisible(NULL))
}

as.radian = function(degree) {
	return(degree/180*pi)
}

as.degree = function(radian) {
	return(radian/pi*180)
}

# will be considered in the future
circos.approx = function(x, y, resolution = 0.1, sector.index = get.cell.meta.data("sector.index"),
	track.index = get.cell.meta.data("track.index"),
	approxFun = function(x) sample(x, 1)) {
	
	od = order(x)
	x = x[od]
	y = y[od]
	
	xplot = get.cell.meta.data("xplot", sector.index = sector.index, track.index = track.index)
	cell.xlim = get.cell.meta.data("cell.xlim", sector.index = sector.index, track.index = track.index)
	
	window.size = resolution/(xplot[1] - xplot[2])*(cell.xlim[2] - cell.xlim[1])
	window = seq(cell.xlim[1], cell.xlim[2], by = window.size)
	
	newx = rep(NA, length(x))
	newy = rep(NA, length(y))
	
	for(i in seq_along(window)[-1]) {
		l = x >= window[i-1] & x < window[i]
		# if there are points in current window
		if(sum(l)) {
			newx[i] = (window[i-1] + window[i])/2
			newy[i] = approxFun(y[l])
		}
	}
	
	newx = newx[!is.na(newx)]
	newy = newy[!is.na(newy)]
	
	return(list(x = newx, y = newy))
}

# == title
# Add transparency to colors
#
# == param
# -col A vector of colors.
# -transparency Transparency, numeric value between 0 and 1.
#
# == value
# A vector of colors.
#
# == example
# add_transparency("red", 0.5)
# add_transparency(1, 0.5)
# add_transparency("#FF000080", 0.2)
add_transparency = function (col, transparency = 0) {
    rgb(t(col2rgb(col)/255), alpha = 1 - transparency)
}


# == title
# Convert adjacency list to an adjacency matrix
#
# == param
# -lt A data frame which contains adjacency list.
# -square Should the returned matrix be a square matrix?
#
# == example
# set.seed(123)
# df = data.frame(from = sample(letters, 10, replace = TRUE), 
#                 to = sample(letters, 10, replace = TRUE), 
#                 value = 1:10)
# adjacencyList2Matrix(df)
# adjacencyList2Matrix(df, square = TRUE)
adjacencyList2Matrix = function(lt, square = FALSE) {
	lt = as.data.frame(lt)
	if(ncol(lt) == 2) {
		lt = cbind(lt, rep(1, nrow(lt)))
	}
	if(ncol(lt) < 3) {
		stop_wrap("`lt` should be a data frame with three columns")
	}

	if(!is.numeric(lt[[3]])) {
		stop_wrap("Third column in `lt` should be numeric.")
	}

	lt[[1]] = as.vector(lt[[1]])
	lt[[2]] = as.vector(lt[[2]])

	rn = unique(lt[[1]])
	cn = unique(lt[[2]])

	if(square) {
		nm = union(rn, cn)
		mat = matrix(0, ncol = length(nm), nrow = length(nm))
		rownames(mat) = nm
		colnames(mat) = nm
	} else {
		mat = matrix(0, ncol = length(cn), nrow = length(rn))
		rownames(mat) = rn
		colnames(mat) = cn
	}

	for(i in seq_len(nrow(lt))) {
		mat[lt[i, 1], lt[i, 2]] = lt[i, 3]
	}

	return(mat)
}



stop_wrap = function(...) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    stop(x, call. = FALSE)
}

warning_wrap = function(...) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    warning(x, call. = FALSE)
}

message_wrap = function(...) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    message(x)
}


dev.off2 = function () {
    i1 = dev.prev()
    i2 = dev.cur()
    if (i1 == 2) {
        dev.set(i1)
    }
    else if (i1 > 2) {
        i11 = dev.prev(i1)
        if (names(i11) == "RStudioGD") {
            dev.set(i11)
        }
        else {
            dev.set(i1)
        }
    }
    dev.off(i2)
}

strwrap2 = function(x) {
    paste(strwrap(x), collapse = "\n")
}


validate_data_frame = function(x) {
    if(inherits(x, "data.frame")) {
        return(as.data.frame(x))
    } else if(inherits(x, "GRanges")) {
        x = as.data.frame(x)
        return(x[, -(4:5), drop = FALSE])
    } else {
        oe = try(x <- as.data.frame(x))
        if(inherits(oe, "try-error")) {
            stop_wrap("The input should be a data frame or an object that can be converted to a data frame.")
        }
        return(x)
    }
}



is.dataFrameList = function(data) {
    is.list(data) && all(sapply(data, is.data.frame))
}


normalizeToDataFrame = function(data, sort = FALSE) {

    all.chr = get.all.sector.index()

    if(is.data.frame(data)) {
        data = as.data.frame(data)
        if(ncol(data) < 3) {
            stop_wrap("Your data frame is less than 3 column!.")
        }
        data = data[data[[1]] %in% all.chr, , drop = FALSE]
        if(sort) {
            data = data[order(data[[1]], data[[2]]), , drop = FALSE]
        }
        return(data)
    } else if(is.list(data) && all(sapply(data, is.data.frame))) {
        df = lapply(data, function(gr) {
            if(ncol(gr) < 3) {
                stop_wrap("Your data frame is less than 3 column!.")
            }
            gr = gr[gr[[1]] %in% all.chr, , drop = FALSE]
            if(sort) {
                gr = gr[order(gr[[1]], gr[[2]]), ]
            }
            return(gr)
        })
        return(df)
    } else if(inherits(df, "GRanges")) {
        df = as.data.frame(df)
        return(df)
    } else {
        stop_wrap("The format of `data` should only be a data frame or a list of data frames.")
    }
}


.normalizeGraphicalParam = function(x, nc, nr, name) {
    if(nc == 1) {
        if(!(length(x) == 1 || length(x) == nr)) {
            stop_wrap("The length of `", name, "` (", length(x), ") should be equal to 1 or the number of your regions (", nr, ").")
        } else if(length(x) == 1) {
            x = rep(x, nr)
        }
    } else {
        if(!(length(x) == 1 || length(x) == nc)) {
            stop_wrap("The length of `", name, "` (", length(x), ") should be equal to 1 or the number of your data column (", nc, ").")
        } else if(length(x) == 1) {
            x = rep(x, nc)
        }
    }
    return(x)
}


## code copied from circos.genomicLabels()
refer_to_one_sector = function(sectors, x) {

    bed = data.frame(sectors = sectors, x = x, order = seq_along(x))
    od = order(bed[[1]], bed[[2]])
    bed = bed[od, , drop = FALSE]

    if(!circos.par$xaxis.clock.wise) {
        all_chr_vec = as.character(bed[, 1])
        bed[, 2] = .CIRCOS.ENV$.SECTOR.DATA[all_chr_vec, "max.value"] - bed[, 2] + .CIRCOS.ENV$.SECTOR.DATA[all_chr_vec, "min.value"]
        bed[, 3] = .CIRCOS.ENV$.SECTOR.DATA[all_chr_vec, "max.value"] - bed[, 3] + .CIRCOS.ENV$.SECTOR.DATA[all_chr_vec, "min.value"]
        bed[, 2:3] = bed[, 3:2]
        circos.par(xaxis.clock.wise = TRUE)
        on.exit(circos.par(xaxis.clock.wise = FALSE))
    }

    chr = get.all.sector.index()[1]
    sector_data = get.sector.data(chr)
    
    all_chr = unique(bed[, 1])
    rho = NULL
    for(cr in all_chr) {
        sub_bed = bed[bed[, 1] == cr, ]
        rho = c(rho, circlize(sub_bed[, 2], y = rep(1, nrow(sub_bed)), sector.index = cr)[, 1])
    }
    if(length(rho) == 0) {
        anchor = ((sector_data["start.degree"] + sector_data["end.degree"])/2 + 180) %% 360
    } else if(length(rho) == 1) {
        anchor = (rho + 180) %% 360
    } else {
        rho = sort(rho)
        rho = c(rho, rho[1] + 360)
        i = which.max(diff(rho))
        anchor = ((rho[i] + rho[i+1])/2) %% 360
    }

    # map all other chromosomes to the first chromosome
    chr_width = sector_data["start.degree"] - sector_data["end.degree"]
    # extend = (360 - chr_width)/chr_width
    # extend = c(0, extend)
    extend = numeric(2)
    s1 = sector_data["start.degree"] %% 360
    s2 = sector_data["end.degree"] %% 360
    # if the anchor in inside the first sector
    if(s1 < s2) { # the first sector go across theta = 0
        s1 = s1 + 360
    } 

    if(anchor >= s2 && anchor <= s1) { # anchor inside sector
        if(s1 - s2 > 180) {
            extend[1] = (abs(s1 - anchor) %% 360)/chr_width
            extend[2] = -(abs(s2 - anchor) %% 360)/chr_width
        } else {
            extend = (360 - chr_width)/chr_width
            extend = c(0, extend)
        }
    } else {
        extend[1] = ((anchor - s1) %% 360)/chr_width  # goes reverse clockwise
        extend[2] = ((s2 - anchor) %% 360)/chr_width  # goes clockwise
    }
    extend = abs(extend)

    if(0) segments(0, 0, cos(anchor/180*pi), sin(anchor/180*pi))

    # start.degree is always larger than end.degree
    new_chr_range = c(sector_data["start.degree"] + chr_width*extend[1], sector_data["end.degree"] - chr_width*extend[2])

    all_chr = unique(bed[, 1])
    bed2 = NULL
    for(cr in all_chr) {
        sub_bed = bed[bed[, 1] == cr, ]
        if(cr != chr) {
            dfx1 = circlize(sub_bed[, 2], y = rep(1, nrow(sub_bed)), sector.index = cr)
            
            l = dfx1[, 1] > new_chr_range[1] | dfx1[, 1] < new_chr_range[2]
            if(any(l)) dfx1[l, 1] = (dfx1[l, 1] - new_chr_range[2]) %% 360 + new_chr_range[2]
            x1 = reverse.circlize(dfx1, sector.index = chr)[, 1]
            
            sub_bed[, 2] = data.frame(start = x1)
        }
        bed2 = rbind(bed2, sub_bed)
    }
    bed2[, 1] = chr
    bed2
}
