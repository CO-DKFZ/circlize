
# before initialization, .SECTOR.DATA is NULL
is.circos.initialized = function() {
	.SECTOR.DATA = get(".SECTOR.DATA", envir = .CIRCOS.ENV)
	return(! is.null(.SECTOR.DATA))
}

# == title
# Initialize the circular layout
#
# == param
# -sectors A `factor` variable or a character vector which represent data categories
# -factors The same as ``sectors``. It will be removed in future versions. 
# -x       Data on x-axes, a vector
# -xlim    Ranges for values on x-axes, see "details" section for explanation of the format
# -sector.width Width for each sector. The length of the vector should be either 1 which means
#          all sectors have same width or as same as the number of sectors. Values for
#          the vector are relative, and they will be scaled by dividing their summation.
#          By default, it is ``NULL`` which means the width of sectors correspond to the data
#          range in sectors.
# -ring Whether the sector represented as a ring. If yes, there should only be one sector in the circle.
#
# == details
# The function allocates the sectors according to the values on x-axis.
# The number of sectors are determined by the ``factors`` and the order
# of sectors are determined by the levels of factors. In this function,
# the start and end position for each sector on the circle (measured by degree)
# are calculated according to the values on x-axis or by ``xlim``.
#
# If ``x`` is set, the length of ``x`` must be equal to the length of ``factors``.
# Then the data range for each sector are calculated from ``x`` by splitting ``factors``.
#
# If ``xlim`` is set, it should be a vector containing two numbers or a matrix with 2 columns.
# If ``xlim`` is a 2-element vector, it means all sector share the same ``xlim``.
# If ``xlim`` is a 2-column matrix, the number of rows should be equal to the number of categories
# identified by ``factors``, then each row of ``xlim`` corresponds to the data range for each sector
# and the order of rows is corresponding to the order of levels of ``factors``. If ``xlim`` is a matrix
# for which row names cover all sector names, ``xlim`` is automatically adjusted.
#
# Normally, width of sectors will be calculated internally according to the data range in sectors. But you can
# still set the width manually. However, it is not always a good idea to change the default sector width since
# the width can reflect the range of data in sectors. However, in some cases, it is useful to manually set
# the width such as you want to zoom some part of the sectors.
#
# The function finally calls `graphics::plot` with enforing aspect ratio to be 1 and be ready for adding graphics.
#
# == seealso
# https://jokergoo.github.io/circlize_book/book/circular-layout.html
circos.initialize = function(
	sectors = NULL, 
	x = NULL,
	xlim = NULL,
	sector.width = NULL,
	offset = NULL,
	factors = sectors,
	ring = FALSE) {

    resetGlobalVariable()

	.SECTOR.DATA = get(".SECTOR.DATA", envir = .CIRCOS.ENV)
	.CELL.DATA = get(".CELL.DATA", envir = .CIRCOS.ENV)

	if(is.null(factors)) {
		if(is.matrix(xlim) || is.data.frame(xlim)) {
			if(is.null(rownames(xlim))) {
				stop_wrap("Since `sectors` is not specified, row names of `xlim` are taken as `sectors`, thus `xlim` should be a two-column matrix with row names.")
			} else {
				factors = rownames(xlim)
			}
		} else {
			stop_wrap("Since `sectors` is not specified, row names of `xlim` are taken as `sectors`, thus `xlim` should be a two-column matrix with row names.")
		}
	}

	if(is.numeric(factor)) {
		warning_wrap("Your `sectors` is numeric, it will be converted to characters internally.")
	}

	if(any(factors == "")) {
		stop_wrap("`sectors` cannot contain empty strings.")
	}

    if(! is.factor(factors)) {
        if(length(factors) == length(unique(factors))) {
           factors = factor(factors, levels = factors)
        } else {
            factors = factor(factors)
        }
    }
    factors = factor(as.character(factors), intersect(levels(factors), as.character(factors)))
    le = levels(factors)

    if(ring) {
    	if(length(le) != 1) {
    		stop_wrap("There should be only one sector under 'ring' mode.")
    	}
    	circos.par$ring = TRUE
    	circos.par$gap.degree = 0
    	circos.par$cell.padding = c(circos.par$cell.padding[1], 0, circos.par$cell.padding[3], 0)
    	circos.par$points.overflow.warning = FALSE
    } else {
    	circos.par$ring = FALSE
    }

    if(!is.null(x)) {
    	x = as.numeric(x)
    }

    # initialize .SECTOR.DATA
    # you can think it as the global x axis configuration
    # calculate min and max value for each sectors
    # there are several ways
	# xlim is prior than x
    if(is.vector(xlim)) {
        if(length(xlim) != 2) {
            stop_wrap("Since `xlim` is vector, it should have length of 2.")
        }

        xlim = as.numeric(xlim)

        min.value = rep(xlim[1], length(le))
        max.value = rep(xlim[2], length(le))
    } else if(is.matrix(xlim) || is.data.frame(xlim)) {
        if(dim(xlim)[1] != length(le) || dim(xlim)[2] != 2) {
            stop_wrap("Since `xlim` is a matrix, it should have same number of rows as the length of the level of `sectors` and number of columns of 2.")
        }

        if(!is.null(rownames(xlim))) {
        	if(length(setdiff(le, rownames(xlim))) == 0) {
        		xlim = xlim[le, ,drop = FALSE]
        	}
        }
        if(is.data.frame(xlim)) xlim = as.matrix(xlim)
        xlim2 = as.numeric(xlim)
    	dim(xlim2) = dim(xlim)
    	dimnames(xlim2) = dimnames(xlim)
    	xlim = xlim2

        min.value = apply(xlim, 1, function(x) x[1])
        max.value = apply(xlim, 1, function(x) x[2])
    } else if(is.vector(x)) {

        if(length(x) != length(factors)) {
            stop_wrap("Length of `x` and length of `sectors` differ.")
        }
        min.value = tapply(x, factors, min)
        max.value = tapply(x, factors, max)
    } else {
		stop_wrap("You should specify either `x` or `xlim`.")
	}

    cell.padding = circos.par("cell.padding")

	# range for sectors
    sector.range = max.value - min.value
    n.sector = length(le)

    sector = vector("list", 7)
	# for each sector, `start.degree always referto `min.value` and `end.degree` always
	# refer to `max.value` in a reverse clockwise fasion. So here `start.degree` and
	# `end.degree` also correspond to the direction.
	# So in the polar coordinate, `start.degree` would be larger than `end.degree`
    names(sector) = c("factor", "min.value", "max.value", "start.degree", "end.degree", "min.data", "max.data")
    sector[["factor"]] = le
	sector[["min.data"]] = min.value
	sector[["max.data"]] = max.value

    gap.degree = circos.par("gap.degree")
    if(!is.null(names(gap.degree))) {
    	if(length(setdiff(le, names(gap.degree))) == 0) {
    		gap.degree = gap.degree[le]
    	}
    }

	if(length(gap.degree) == 1) {
		gap.degree = rep(gap.degree, n.sector)
	} else if(length(gap.degree) != n.sector) {
		stop_wrap("Since `gap.degree` parameter has length larger than 1, it should have same length as the number of sectors.")
	}

	start.degree = circos.par("start.degree")
	clock.wise = circos.par("clock.wise")

    if(360 - sum(gap.degree) <= 0) {
		stop_wrap("Maybe your `gap.degree` is too large so that there is no space to allocate sectors.")
	}

    if(is.null(sector.width)) {
		# degree per data
		unit = (360 - sum(gap.degree)) / sum(sector.range)

		for(i in seq_len(n.sector)) {

			if(sector.range[i] == 0) {
				stop_wrap("Range of the sector ('", le[i] ,"') cannot be 0. You might need to set the second and the fourth values in `circos.par$cell.padding` to 0.")
			}

			# only to ensure value are always increasing or decreasing with the absolute degree value
			if(clock.wise) {
				sector[["start.degree"]][i] = ifelse(i == 1, start.degree, sector[["end.degree"]][i-1] - gap.degree[i-1])
				sector[["end.degree"]][i] =  sector[["start.degree"]][i] - sector.range[i]*unit
			} else {
				sector[["end.degree"]][i] = ifelse(i == 1, start.degree, sector[["start.degree"]][i-1] + gap.degree[i-1])
				sector[["start.degree"]][i] = sector[["end.degree"]][i] + sector.range[i]*unit
			}
		}
	} else {
		if(length(sector.width) == 1) {
			sector.width = rep(sector.width, n.sector)
		} else if(length(sector.width) != n.sector) {
			stop_wrap("Since you manually set the width for each sector, the length of `sector.width` should be either 1 or as same as the number of sectors.")
		}

		sector.width.percentage = sector.width / sum(sector.width)
		degree.per.sector = (360 - sum(gap.degree)) * sector.width.percentage

		if(any(degree.per.sector <= 0)) {
			stop_wrap("Maybe your `gap.degree` is too large so that there is no space to allocate sectors.")
		}

		for(i in seq_len(n.sector)) {

			if(sector.range[i] == 0) {
				stop_wrap("Range of the sector (", le[i] ,") cannot be 0.")
			}


			# only to ensure value are always increasing or decreasing with the absolute degree value
			if(clock.wise) {
				sector[["start.degree"]][i] = ifelse(i == 1, start.degree, sector[["end.degree"]][i-1] - gap.degree[i-1])
				sector[["end.degree"]][i] =  sector[["start.degree"]][i] - degree.per.sector[i]
			} else {
				sector[["end.degree"]][i] = ifelse(i == 1, start.degree, sector[["start.degree"]][i-1] + gap.degree[i-1])
				sector[["start.degree"]][i] = sector[["end.degree"]][i] + degree.per.sector[i]
			}
		}
	}
	# from start.degree, degree is increasing in a reverse-clock wise fasion
	# so, if circos is created clock wise, the forward sector would have large degrees
	# if circos is created reverse clock wise, the forward sector would have small degrees
	# just for goodlooking for the degree
	if(clock.wise) {
		sector[["start.degree"]] = sector[["start.degree"]] + 360
		sector[["end.degree"]] = sector[["end.degree"]] + 360
	}

	if(any(cell.padding[2] + cell.padding[4] >= sector[["start.degree"]] - sector[["end.degree"]])) {
		stop_wrap("Summation of cell padding on x-direction are larger than the width for some sectors. You can e.g. set 'circos.par(cell.padding = c(0.02, 0, 0.02, 0))' or remove tiny sectors.")
	}

	min.value = min.value - cell.padding[2]/(sector[["start.degree"]] - sector[["end.degree"]] - cell.padding[2] - cell.padding[4])*sector.range  # real min value
    max.value = max.value + cell.padding[4]/(sector[["start.degree"]] - sector[["end.degree"]] - cell.padding[2] - cell.padding[4])*sector.range  # real max value
    sector[["min.value"]] = min.value
    sector[["max.value"]] = max.value

    sector = as.data.frame(sector, stringsAsFactors = FALSE)
	offset2 = rep(0, n.sector)
	names(offset2) = le
	if(!is.null(offset)) {
		if(!is.null(names(offset))) {
			nm = intersect(names(offset2), names(offset))
			if(length(nm)) offset2[nm] = offset[nm]
		} else {
			if(length(offset) == 1) {
				offset2 = rep(offset, n.sector)
				names(offset2) = le
			} else {
				if(length(offset) != n.sector) {
					stop_wrap("If `offset` is set as an unnamed vector, the length should be the same as the number of sectors.")
				} else {
					offset2 = offset
					names(offset2) = le
				}
			}
		}
	}
	sector$offset = offset2
    
    .SECTOR.DATA = sector

    # initialize .CELL.DATA which contains information of each cell
    # if content of that cell has been created, it means that the
    # plotteing region for that cell has been created.
    .CELL.DATA = vector("list", length = length(le))
    names(.CELL.DATA) = le
    for(i in seq_along(.CELL.DATA)) {
        .CELL.DATA[[ le[i] ]] = vector("list", length = 0)
    }

	assign(".SECTOR.DATA", .SECTOR.DATA, envir = .CIRCOS.ENV)
	assign(".CELL.DATA", .CELL.DATA, envir = .CIRCOS.ENV)
	assign(".CURRENT.SECTOR.INDEX", .SECTOR.DATA[1, "factor"], envir = .CIRCOS.ENV)

    circos.par("__omar__" = FALSE)
	if(identical(par("mar"), c(5.1, 4.1, 4.1, 2.1))) {
		circos.par("__omar__" = TRUE)
		par(mar = c(1, 1, 1, 1))
	}
    # draw everything in a unit circle
	plot(circos.par("canvas.xlim"), circos.par("canvas.ylim"), type = "n", ann = FALSE, axes = FALSE, asp = 1)

	# all the information of cells would be visited through `get.cell.meta.data`
	return(invisible(NULL))
}

# == title
# Reset the circular layout parameters
#
# == details
# Because there are several
# parameters for the circular plot which can only be set before `circos.initialize`. So before you draw the next
# circular plot, you need to reset all these parameters.
#
# If you meet some errors when re-drawing the circular plot, try running this function and it will solve most of the problems.
circos.clear = function() {

	resetGlobalVariable()
	if(circos.par("__omar__")) {
		circos.par("__omar__" = FALSE)
		par(mar = c(5.1, 4.1, 4.1, 2.1))
	}
	tmpdir = circos.par("__tempdir__")
	circos.par(RESET = TRUE)
	circos.par("__tempdir__" = tmpdir)

	empty_env(circos.par("__tempenv__"))
	circos.par("__tempenv__" = new.env(parent = emptyenv()))

    return(invisible(NULL))
}

empty_env = function(env) {
	obj = ls(envir = env, all.names = TRUE)
	if(length(obj)) rm(list = obj, envir = env)
}
