# == title
# Convert to polar coordinate system
#
# == param
# -x            Data points on x-axis. The value can also be a two-column matrix/data frame if you put x and y data points into one variable.
# -y            Data points on y-axis.
# -sector.index Index for the sector to convert the coordinates.
# -track.index  Index for the track to convert the coordinates.
#
# == details
# This is the core function in the package. It transform data points from data coordinate system (in a specific cell) to the polar coordinate system.
#
# == values
# A matrix with two columns (``theta`` and ``rou``). ``rou`` is measured in degree.
#
# == example
# pdf(NULL)
# sectors = c("a", "b")
# circos.initialize(sectors, xlim = c(0, 1))
# circos.track(ylim = c(0, 1))
# # x = 0.5, y = 0.5 in sector a and track 1
# circlize(0.5, 0.5, sector.index = "a", track.index = 1)
# circos.clear()
# dev.off()
circlize = function(
    x, y, 
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index()) {

    if(missing(y)) {
        if(ncol(x) >= 2) {
            y = x[, 2]
            x = x[, 1]
        }
    }
    
    sector.data = get.sector.data(sector.index)
       
    theta = sector.data["start.degree"] - (x - sector.data["min.value"]) / (sector.data["max.value"] - sector.data["min.value"]) *
            abs(sector.data["start.degree"] - sector.data["end.degree"])

    if(!circos.par("xaxis.clock.wise")) {
        theta = sector.data["start.degree"] - theta + sector.data["end.degree"]
    }
    
	if(track.index == 0) {
		rou = rep(1, length(theta))
	} else {
		cell.data = get.cell.data(sector.index, track.index)
		cell.ylim = get.cell.meta.data("cell.ylim", sector.index, track.index)  
		y.range = cell.ylim[2] - cell.ylim[1] 
		rou = cell.data$track.start - (cell.ylim[2] - y) / y.range * cell.data$track.height
    }
	
    m = cbind(theta, rou)
    colnames(m) = c("theta", "rou")
    rownames(m) = NULL
    
    return(m)
}

# == title
# Convert to data coordinate system
#
# == param
# -x   degree values. The value can also be a two-column matrix/data frame if you put x and y data points into one variable.
# -y   distance to the circle center (the radius)
# -sector.index Index for the sector where the data coordinate is used
# -track.index  Index for the track where the data coordinate is used
#
# == details
# This is the reverse function of `circlize`. It transform data points from polar coordinate system to a specified data coordinate system.
#
# == values
# A matrix with two columns (``x`` and ``y``)
# 
# == example
# pdf(NULL)
# sectors = letters[1:4]
# circos.initialize(sectors, xlim = c(0, 1))
# circos.trackPlotRegion(ylim = c(0, 1))
# reverse.circlize(c(30, 60), c(0.9, 0.8))
# reverse.circlize(c(30, 60), c(0.9, 0.8), sector.index = "d", track.index = 1)
# reverse.circlize(c(30, 60), c(0.9, 0.8), sector.index = "a", track.index = 1)
# circos.clear()
# dev.off()
reverse.circlize = function(
    x, y, 
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index()) {

    if(missing(y)) {
        if(ncol(x) >= 2) {
            y = x[, 2]
            x = x[, 1]
        }
    }

    theta = x
    rou = y

	sector.data = get.sector.data(sector.index)
    if(track.index > 0) {
        cell.data = get.cell.data(sector.index, track.index)
    	cell.ylim = get.cell.meta.data("cell.ylim", sector.index, track.index)
	}

	x = (sector.data["start.degree"] - theta) / abs(sector.data["end.degree"] - sector.data["start.degree"]) *
	    (sector.data["max.value"] - sector.data["min.value"]) + sector.data["min.value"]

    if(!circos.par("xaxis.clock.wise")) {
        x = sector.data["max.value"] - x + sector.data["min.value"]
    }

	if(track.index > 0) {
        y = (cell.data$track.height - (cell.data$track.start - rou)) / cell.data$track.height * (cell.ylim[2] - cell.ylim[1]) + cell.ylim[1]
	} else {
        y = rep(1, length(x))
    }
	m = cbind(x, y)
	colnames(m) = c("x", "y")
	rownames(m) = NULL
	return(m)
}

polar2Cartesian = function(d) {
    theta = as.radian(d[, 1])
    rou = d[, 2]
    x = rou * cos(theta)
    y = rou * sin(theta)
    return(cbind(x, y))
}

# theta in degree
cartesian2Polar = function(d) {
    r = sqrt(d[, ]^2 + d[, 2]^2)
    theta = atan(d[, 2]/d[, 1])
    theta = as.degree(theta)
    theta = ifelse(d[, 1] > 0 & d[, 2] < 0, 360 + theta, 
            ifelse(d[, 1] < 0 & d[, 2] < 0, 180 + theta,
            ifelse(d[, 1] < 0 & d[, 2] > 0, 180 + theta, theta)))
    data.frame(theta = theta, rou = r)
}


as.radian = function(degree) {
    return(degree/180*pi)
}

as.degree = function(radian) {
    return(radian/pi*180)
}

# == title
# Convert coordinates from one cell to another cell
#
# == param
# -x X-coordinates in the origin cell.
# -y Y-coordinates in the origin cell.
# -sector.index.from Sector index of the origin cell.
# -track.index.from Track index of the origin call.
# -sector.index.to Sector index of the target cell.
# -track.index.to  Track index of the target cell.
#
# == example
# circos.initialize(letters[1:8], xlim = c(0, 1))
# circos.track(ylim = c(0, 1))
# # first draw a point in sector "a"
# circos.points(0.3, 0.6, pch = 1, cex = 2, sector.index = "a")
# # next convert to sector "d"
# df = convert.coord.between.cells("a", 1, 0.3, 0.6, "d", 1)
# circos.points(df[1, 1], df[1, 2], pch = 1, cex = 0.5, col = "red", sector.index = "d")
# circos.clear()
convert.coord.between.cells = function(x, y,
    sector.index.from, track.index.from,
    sector.index.to, track.index.to = track.index.from) {

    df = circlize(x, y, sector.index = sector.index.from, track.index = track.index.from)
    df2 = reverse.circlize(df, sector.index = sector.index.to, track.index = track.index.to)
    return(df2)
}

# == title
# Convert coordinates from canvas to a cell
#
# == param
# -x X-coordinates in the canvas coordinate system.
# -y Y-coordinates in the canvas coordinate system.
# -sector.index.to Sector index of the target cell.
# -track.index.to  Track index of the target cell.
#
# == example
# circos.initialize(letters[1:8], xlim = c(0, 1))
# circos.track(ylim = c(0, 1))
# points(0.5, 0.3, pch = 1, cex = 2)
# # map to sector "e"
# df = covnert.coord.from.canvas.to.cell(0.5, 0.3, "e", 1)
# circos.points(df[1, 1], df[1, 2], pch = 1, cex = 0.5, col = "red", sector.index = "e")
# circos.clear()
covnert.coord.from.canvas.to.cell = function(x, y, sector.index.to, track.index.to) {
    
    df = cartesian2Polar(data.frame(x, y))
    df2 = reverse.circlize(df, sector.index = sector.index.to, track.index = track.index.to)
    return(df2)
}

# == title
# Convert coordinates from cell to the canvas
#
# == param
# -x X-coordinates in the origin cell.
# -y Y-coordinates in the origin cell.
# -sector.index Sector index of the origin cell.
# -track.index Track index of the origin call.
#
# == example
# circos.initialize(letters[1:8], xlim = c(0, 1))
# circos.track(ylim = c(0, 1))
# # first draw a point in sector "a"
# circos.points(0.3, 0.6, pch = 1, cex = 2, sector.index = "a")
# df = convert.coord.from.cell.to.canvas(0.3, 0.6, "a", 1)
# points(df[1, 1], df[1, 2], pch = 1, col = "red")
# circos.clear()
convert.coord.from.cell.to.canvas = function(x, y, sector.index, track.index) {
    df = circlize(x, y, sector.index = sector.index, track.index = track.index)
    polar2Cartesian(df)
}


