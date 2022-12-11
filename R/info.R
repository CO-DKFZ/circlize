
# == title
# Get index for all sectors
#
# == details
# It simply returns a vector of all sector index.
get.all.sector.index = function() {
	.SECTOR.DATA = get(".SECTOR.DATA", envir = .CIRCOS.ENV)
	if(is.null(.SECTOR.DATA)) {
		return(character(0))
	} else {
		return(as.vector(.SECTOR.DATA$factor))
	}
}

# == title
# Get index for all tracks
#
# == details
# It simply returns a vector of all track index.
get.all.track.index = function() {
	.CELL.DATA = get(".CELL.DATA", envir = .CIRCOS.ENV)
	if(is.null(.CELL.DATA)) {
		return(integer(0))
	} else if(all(sapply(.CELL.DATA, length) == 0)) {
		return(integer(0))
	} else {
		return(seq_len(max(sapply(.CELL.DATA[ which(sapply(.CELL.DATA, length) > 0) ], function(x) length(x)))))
	}
}

get.sector.data = function(sector.index = get.current.sector.index()) {
	sector.index = as.character(sector.index)
	.SECTOR.DATA = get(".SECTOR.DATA", envir = .CIRCOS.ENV)
    sector.data = as.vector(as.matrix(.SECTOR.DATA[.SECTOR.DATA[[1]] == sector.index, -1]))
    names(sector.data) = colnames(.SECTOR.DATA)[-1]
    return(sector.data)
}

# == title
# Get current track index
#
# == value
# Simply returns the numeric index for the current track.
get.current.track.index = function() {
	.CURRENT.TRACK.INDEX = get(".CURRENT.TRACK.INDEX", envir = .CIRCOS.ENV)
    return(.CURRENT.TRACK.INDEX)
}

set.current.track.index = function(x) {
	.CURRENT.TRACK.INDEX = x
	assign(".CURRENT.TRACK.INDEX", .CURRENT.TRACK.INDEX, envir = .CIRCOS.ENV)
    return(invisible(NULL))
}

# == title
# Get current sector index
#
# == value
# Simply returns the name of current sector
get.current.sector.index = function() {
	.CURRENT.SECTOR.INDEX = get(".CURRENT.SECTOR.INDEX", envir = .CIRCOS.ENV)
    return(.CURRENT.SECTOR.INDEX)
}

set.current.sector.index = function(x) {
	.CURRENT.SECTOR.INDEX = get(".CURRENT.SECTOR.INDEX", envir = .CIRCOS.ENV)
	if(!x %in% get.all.sector.index()) {
		stop_wrap(paste0("Cannot find ", x, " in all available sector names.\n"))
	}
    .CURRENT.SECTOR.INDEX = x
	assign(".CURRENT.SECTOR.INDEX", .CURRENT.SECTOR.INDEX, envir = .CIRCOS.ENV)
    return(invisible(NULL))
}

# == title
# Set flag to current cell
#
# == param
# -sector.index sector index
# -track.index track index
#
# == details
# After setting the current cell, all functions which need ``sector.index`` and ``track.index``
# arguments and are applied to the current cell do not need to specify the two arguments explicitly.
#
# == example
# pdf(NULL)
# circos.initialize(letters[1:8], xlim = c(0, 1))
# circos.track(ylim = c(0, 1))
# circos.info()
# set.current.cell("b", 1)
# circos.info()
# circos.clear()
# dev.off()
set.current.cell = function(sector.index, track.index) {
	sector.index = as.character(sector.index)
	set.current.sector.index(sector.index)
	set.current.track.index(track.index)
}

get.cell.data = function(sector.index = get.current.sector.index(), track.index = get.current.track.index()) {
	sector.index = as.character(sector.index)
	.CELL.DATA = get(".CELL.DATA", envir = .CIRCOS.ENV)
    .CELL.DATA[[sector.index]][[track.index]]
}

set.cell.data = function(sector.index = get.current.sector.index(), track.index = get.current.track.index(), ...) {
	sector.index = as.character(sector.index)
	
	.CELL.DATA = get(".CELL.DATA", envir = .CIRCOS.ENV)
    .CELL.DATA[[sector.index]][[track.index]] = list(...)
	assign(".CELL.DATA", .CELL.DATA, envir = .CIRCOS.ENV)
    return(invisible(NULL))
}

# whether cell in sector.index, track.index exists?
has.cell = function(sector.index, track.index) {
	sector.index = as.character(sector.index)
	
	.CELL.DATA = get(".CELL.DATA", envir = .CIRCOS.ENV)
    if(sector.index %in% names(.CELL.DATA) &&
       track.index <= length(.CELL.DATA[[sector.index]]) &&
       !is.null(.CELL.DATA[[sector.index]][[track.index]])) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

# == title
# Get information of the circular plot
#
# == param
# -sector.index Which sectors you want to look at? It can be a vector.
# -track.index  Which tracks you want to look at? It can be a vector.
# -plot         Whether to add information on the plot.
#
# == details
# It tells you the basic parameters for sectors/tracks/cells. If both ``sector.index``
# and ``track.index`` are set to ``NULL``, the function would print index for
# all sectors and all tracks. If ``sector.index`` and/or ``track.index`` are set,
# the function would print ``xlim``, ``ylim``, ``cell.xlim``, ``cell.ylim``,
# ``xplot``, ``yplot``, ``cell.width``, ``cell.height``, ``track.margin`` and ``cell.padding`` for every cell in specified sectors and tracks.
# Also, the function will print index of your current sector and current track.
#
# If ``plot`` is set to ``TRUE``, the function will plot the index of the sector and the track
# for each cell on the figure.
#
# == seealso
# https://jokergoo.github.io/circlize_book/book/circular-layout.html#circos-info-and-circos-clear
circos.info = function(sector.index = NULL, track.index = NULL, plot = FALSE) {
	sectors = get.all.sector.index()
	tracks = get.all.track.index()

	if(plot) {
		for(i in seq_along(sectors)) {
			for(j in seq_along(tracks)) {
				cell.xlim = get.cell.meta.data("cell.xlim", sector.index = sectors[i], track.index = j)
				cell.ylim = get.cell.meta.data("cell.ylim", sector.index = sectors[i], track.index = j)
				circos.text(mean(cell.xlim), mean(cell.ylim), labels = paste(sectors[i], j, sep = ":"),
					sector.index = sectors[i], track.index = j, facing = "downward")
			}
		}
	} else {
		# just print the name and xlim for each sector
		if(is.null(sector.index) && is.null(track.index)) {
			if(length(sectors)) {
				cat("All your sectors:\n")
				print(sectors)
			} else {
				cat("No sector has been created\n")
			}
			cat("\n")
			if(length(tracks)) {
				cat("All your tracks:\n")
				print(tracks)
			} else {
				cat("No track has been created\n")
			}
			cat("\n")

		} else {
			if(is.null(track.index)) {
				track.index = tracks
			} else if(is.null(sector.index)) {
				sector.index = sectors
			}
			for(i in seq_along(sector.index)) {
				for(j in seq_along(track.index)) {
					cat("sector index: '", sector.index[i], "'\n", sep = "")
					cat("track index: ", track.index[j], "\n", sep = "")
					xlim = get.cell.meta.data('xlim', sector.index[i], track.index[j])
					ylim = get.cell.meta.data('ylim', sector.index[i], track.index[j])
					cell.xlim = get.cell.meta.data("cell.xlim", sector.index[i], track.index[j])
					cell.ylim = get.cell.meta.data("cell.ylim", sector.index[i], track.index[j])
					xplot = get.cell.meta.data("xplot", sector.index[i], track.index[j])
					yplot = get.cell.meta.data("yplot", sector.index[i], track.index[j])
				    cell.width = get.cell.meta.data("cell.width", sector.index[i], track.index[j])
				    cell.height = get.cell.meta.data("cell.height", sector.index[i], track.index[j])
				    track.margin = get.cell.meta.data("track.margin", sector.index[i], track.index[j])
				    cell.padding = get.cell.meta.data("cell.padding", sector.index[i], track.index[j])
					cat("xlim: [", xlim[1], ", ", xlim[2], "]\n", sep = "")
					cat("ylim: [", ylim[1], ", ", ylim[2], "]\n", sep = "")
					cat("cell.xlim: [", cell.xlim[1], ", ", cell.xlim[2], "]\n", sep = "")
					cat("cell.ylim: [", cell.ylim[1], ", ", cell.ylim[2], "]\n", sep = "")
					cat("xplot (degree): [", xplot[1], ", ", xplot[2], "]\n", sep = "")
					cat("yplot (radius): [", yplot[1], ", ", yplot[2], "]\n", sep = "")
					cat("cell.width (degree): ", cell.width, "\n", sep = "")
					cat("cell.height (radius): ", cell.height, "\n", sep = "")
					cat("track.margin: c(", track.margin[1], ", ", track.margin[2], ")\n", sep = "")
					cat("cell.padding: c(", cell.padding[1], ", ", cell.padding[2], ", ", cell.padding[3], ", ", cell.padding[4], ")\n", sep = "")
					cat("\n")
				}
			}

		}

		if(length(get.current.sector.index())) cat("Your current sector.index is ", get.current.sector.index(), "\n", sep = "")
		if(get.current.track.index() > 0) cat("Your current track.index is ", get.current.track.index(), "\n", sep = "")
	}

}


# == title
# Label the sector index and the track index on each cell
#
# == details
# This function is deprecated, please use `circos.info` instead.
show.index = function() {
	circos.info(plot = TRUE)
	warning_wrap("`show.index` is deprecated, please use `circos.info` instead.")
}

# == title
# Get the meta data of a cell
#
# == param
# -name         Only support one name at a time, see "details" section
# -sector.index Index of the sector
# -track.index  Index of the track
#
# == details
# The following meta information for a cell can be obtained:
#
# -``sector.index``         The name (index) for the sector
# -``sector.numeric.index`` Numeric index for the sector
# -``track.index``          Numeric index for the track
# -``xlim``                 Minimal and maximal values on the x-axis
# -``ylim``                 Minimal and maximal values on the y-axis
# -``xrange``               Range of ``xlim``. It equals to ``xlim[2] - xlim[1]``
# -``yrange``               Range of ``ylim``
# -``xcenter``              Center of x-axis. It equals to ``(xlim[2] + xlim[1])/2``
# -``ycenter``              Center of y-axis
# -``cell.xlim``            Minimal and maximal values on the x-axis extended by cell paddings
# -``cell.ylim``            Minimal and maximal values on the y-axis extended by cell paddings
# -``xplot``                Degrees for right and left borders of the cell. The values ignore the direction of the circular layout (i.e. whether it is clock wise or not).
# -``yplot``                Radius for top and bottom borders of the cell.
# -``cell.width``           Width of the cell, in degrees.
# -``cell.height``          Height of the cell, simply ``yplot[2] - yplot[1]``
# -``cell.start.degree``    Same as ``xplot[1]``
# -``cell.end.degree``      Same as ``xplot[2]``
# -``cell.bottom.radius``   Same as ``yplot[1]``
# -``cell.top.radius``      Same as ``yplot[2]``
# -``track.margin``         Margin for the cell
# -``cell.padding``         Padding for the cell
#
# The function is useful when using ``panel.fun`` in `circos.track` to
# get detailed information of the current cell.
#
# == seealso
# `CELL_META` is a short version of `get.cell.meta.data`.
#
# == example
# sectors = letters[1:4]
# circos.initialize(sectors, xlim = c(0, 1))
# circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
#     print(get.cell.meta.data("xlim"))
# })
# print(get.cell.meta.data("xlim", sector.index = "a", track.index = 1))
# circos.clear()
get.cell.meta.data = function(name, sector.index = get.current.sector.index(),
                              track.index = get.current.track.index()) {
	
	sector.index = as.character(sector.index)

	if(length(sector.index) == 0) {
		stop_wrap("It seems the circular plot has not been initialized.")
	}
	if(length(track.index) == 0) {
		stop_wrap("It seems the track has not been created.")
	}
	if(length(sector.index) != 1) {
		stop_wrap("Length of `sector.index` should only be 1.")
	}
	if(length(track.index) != 1) {
		stop_wrap("Length of `track.index` should only be 1.")
	}
	if(track.index == 0) {
		stop_wrap("It seems the track has not been created.")
	}
	if(!any(sector.index %in% get.all.sector.index())) {
		stop_wrap("Cannot find sector: ", sector.index, ".")
	}
	if(!any(track.index %in% get.all.track.index())) {
		stop_wrap("Cannot find track: ", track.index, ".")
	}

	current.sector.data = get.sector.data(sector.index)
	current.cell.data = get.cell.data(sector.index, track.index)
	cell.padding = current.cell.data$cell.padding

	if(length(name) != 1) {
		stop_wrap("``name`` should only have length of 1.")
	}

	if(name == "xlim") {
		return(current.cell.data$xlim)
	} else if(name == "ylim") {
		return(current.cell.data$ylim)
	} else if(name == "xrange") {
		xlim = current.cell.data$xlim
		return(xlim[2] - xlim[1])
	} else if(name == "yrange") {
		ylim = current.cell.data$ylim
		return(ylim[2] - ylim[1])
	} else if(name == "xcenter") {
		xlim = current.cell.data$xlim
		return((xlim[2] + xlim[1])/2)
	} else if(name == "ycenter") {
		ylim = current.cell.data$ylim
		return((ylim[2] + ylim[1])/2)
	} else if(name == "cell.xlim") {
		return(current.cell.data$cell.xlim)
	} else if(name == "cell.ylim") {
		return(current.cell.data$cell.ylim)
	} else if(name == "sector.numeric.index") {
		return(which(get.all.sector.index() == sector.index))
	} else if(name == "sector.index") {
		return(sector.index)
	} else if(name == "track.index") {
		return(track.index)
	} else if(name == "xplot") {
		x = current.sector.data[c("start.degree", "end.degree")]
		names(x) = NULL
		return(x)
	} else if(name == "yplot") {
		return(c(current.cell.data$track.start - current.cell.data$track.height, current.cell.data$track.start))
	} else if(name == "cell.width") {
		x = current.sector.data[c("start.degree", "end.degree")]
		return((x[1] - x[2]))
	} else if(name == "cell.height") {
		y = c(current.cell.data$track.start - current.cell.data$track.height, current.cell.data$track.start)
		return(y[2] - y[1])
	} else if(name == "track.margin") {
		return(current.cell.data$track.margin)
	} else if(name == "cell.padding") {
		return(current.cell.data$cell.padding)
	} else if(name == "cell.start.degree") {
		x = current.sector.data["start.degree"]
		names(x) = NULL
		return(x)
	} else if(name == "cell.end.degree") {
		x = current.sector.data["end.degree"]
		names(x) = NULL
		return(x)
	} else if(name == "cell.bottom.radius") {
		return(current.cell.data$track.start - current.cell.data$track.height)
	} else if(name == "cell.top.radius") {
		return(current.cell.data$track.start)
	} else if(name == "bg.col") {
		return(current.cell.data$bg.col)
	} else if(name == "bg.border") {
		return(current.cell.data$bg.border)
	} else if(name == "bg.lty") {
		return(current.cell.data$bg.lty)
	} else if(name == "bg.lwd") {
		return(current.cell.data$bg.lwd)
	} else if(name == "track.height") {
		return(current.cell.data$track.height)
	} else {
		env = circos.par("__tempenv__")
		if(!is.null(env$track.meta.data)) {
			track.index = as.character(track.index)
			if(!is.null(env$track.meta.data[[track.index]])) {
				if(name %in% names(env$track.meta.data[[track.index]])) {
					return(env$track.meta.data[[track.index]][[name]])
				}
			}
		}
		if(!is.null(env$sector.meta.data)) {
			if(!is.null(env$sector.meta.data[[sector.index]])) {
				if(name %in% names(env$sector.meta.data[[sector.index]])) {
					return(env$sector.meta.data[[sector.index]][[name]])
				}
			}
		}
	}

	return(NULL)
}

add.track.meta.data = function(name, value, track.index = get.current.track.index()) {
	env = circos.par("__tempenv__")
	if(is.null(env$track.meta.data)) env$track.meta.data = list()
	track.index = as.character(track.index)
	if(is.null(env$track.meta.data[[track.index]])) env$track.meta.data[[track.index]] = list()
	env$track.meta.data[[track.index]][[name]] = value
}

add.sector.meta.data = function(name, value, sector.index = get.current.sector.index()) {
	sector.index = as.character(sector.index)
	env = circos.par("__tempenv__")
	if(is.null(env$sector.meta.data)) env$sector.meta.data = list()
	if(is.null(env$sector.meta.data[[sector.index]])) env$sector.meta.data[[sector.index]] = list()
	env$sector.meta.data[[sector.index]][[name]] = value
}

# == title (variable:CELL_META)
# Easy way to get meta data in the current cell
#
# == details
# The variable `CELL_META` can only be used to get meta data of the "current" cell.
# Basically you can simply replace  e.g. ``get.cell.meta.data("sector.index")`` to ``CELL_META$sector.index``.
#
# == seealso
# `get.cell.meta.data`
#
# == example
# pdf(NULL)
# circos.initialize("a", xlim = c(0, 1))
# circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
#     print(CELL_META$sector.index)
#     print(CELL_META$xlim)
# })
# print(names(CELL_META))
# dev.off()
CELL_META = "don't use me directly"
class(CELL_META) = "CELL_META"

# == title
# Names of all meta data in the current cell
#
# == param
# -x use `CELL_META`.
#
# == example
# names(CELL_META)
names.CELL_META = function(x) {
	sector.index = get.current.sector.index()
    track.index = get.current.track.index()

	nm = c("xlim", "ylim", "xrange", "yrange", "xcenter", "ycenter", "cell.xlim", "cell.ylim",
     "sector.numeric.index", "sector.index", "track.index", "xplot", "yplot", "cell.width", "cell.height", "track.margin", "cell.padding",
     "cell.start.degree", "cell.end.degree", "cell.bottom.radius", "cell.top.radius", "bg.col", "bg.border",
     "bg.lty", "bg.lwd", "track.height")

	env = circos.par("__tempenv__")
	if(track.index > 0) {
		if(!is.null(env$track.meta.data)) {
			track.index = as.character(track.index)
			if(!is.null(env$track.meta.data[[track.index]])) {
				nm = c(nm, names(env$track.meta.data[[track.index]]))
			}
		}
	}
	if(!is.null(sector.index)) {
		if(!is.null(env$sector.meta.data)) {
			if(!is.null(env$sector.meta.data[[sector.index]])) {
				nm = c(nm, names(env$sector.meta.data[[sector.index]]))
			}
		}
	}

	return(nm)
}

# == title
# Easy to way to get meta data in the current cell
#
# == param
# -x name of the variable should be "CELL_META"
# -name name of the cell meta name
#
# == details
# The variable `CELL_META` can only be used to get meta data of the "current" cell.
# Basically you can simply replace  e.g. ``get.cell.meta.data("sector.index")`` to ``CELL_META$sector.index``.
#
# == seealso
# `get.cell.meta.data`
"$.CELL_META" = function(x, name) {
	get.cell.meta.data(name)
}

# == title
# Print CELL_META
#
# == param
# -x input
# -... additional parameters
#
print.CELL_META = function(x, ...) {
	cat(paste(strwrap("Please use in a form of `CELL_META$name` where `name` should be supported in `get.cell.meta.data()`. Type `names(CELL_META)` for supported names.\n"), collapse = "\n"), "\n")
}


# == title
# The .DollarNames method for the CELL_META class
#
# == param
# -x A ``CELL_META`` object.
# -pattern pattern, please ignore it.
#
# == details
# This makes the option object looks like a list that it allows
# option name completion after ``$``.
#
.DollarNames.CELL_META = function(x, pattern = "") {
	names(x)
}

# == title
# Get the inside radius of the most inner track
#
get_most_inside_radius = function(sector.index = NULL) {
	tracks = get.all.track.index()
	if(length(tracks) == 0) {
	    1
	} else {
	    n = length(tracks)
        if(is.null(sector.index)) {
            all_le = get.all.sector.index()
    	    	min(sapply(all_le, function(s) {
                get.cell.meta.data("cell.bottom.radius", sector.index = s, track.index = tracks[n]) - get.cell.meta.data("track.margin", track.index = tracks[n])[1] - circos.par("track.margin")[2]
            }))
        } else {
            get.cell.meta.data("cell.bottom.radius", sector.index = sector.index, track.index = tracks[n]) - get.cell.meta.data("track.margin", track.index = tracks[n])[1] - circos.par("track.margin")[2]
        }
	}
}

