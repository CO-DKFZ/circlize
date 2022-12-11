parse_unit = function(str) {
  if(grepl("^(-?\\d+(\\.\\d+)?)\\s*(mm|cm|inche|inches)$", str)) {
    m = regexpr("^(-?\\d+(\\.\\d+)?)", str)
    v = regmatches(str, m)
    m = regexpr("(mm|cm|in|inche|inches)$", str)
    u = regmatches(str, m)
    return(list(value = as.numeric(v), unit = u))
  } else {
    stop_wrap("Format of the unit is incorrect. It should be like '2mm', '-2.1 inches'.")
  }
}


# == title
# Convert units
#
# == param
# -x a numeric vector
# -unit supported units, only "mm", "cm", "inches".
#
# == details
# This function coverts mm/cm/inches units to units measured in the canvas coordinate,
# e.g. how much is it in the canvas coordinate for 1 mm/cm/inches.
#
# Since in the circular plot, the aspect ratio is always 1, it does not matter this conversion
# is applied on x direction or y direction.
#
# This function is mainly used in the radical direction.
#
# == seealso
# `convert_x` and `convert_y` convert absolute units into a data coordinate in a specified cell.
#
# https://jokergoo.github.io/circlize_book/book/circular-layout.html#convert-functions
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# sectors = letters[1:10]
# circos.par(cell.padding = c(0, 0, 0, 0), track.margin = c(0, 0))
# circos.initialize(sectors, xlim = cbind(rep(0, 10), runif(10, 0.5, 1.5)))
# circos.track(ylim = c(0, 1), track.height = mm_h(5))
# circos.par(track.margin = c(0, mm_h(2)))
# circos.track(ylim = c(0, 1), track.height = cm_h(1))
# circos.par(track.margin = c(0, mm_h(5)))
# circos.track(ylim = c(0, 1), track.height = inch_h(1))
# circos.clear()
convert_length = function(x, unit = c("mm", "cm", "inches")) {

  pin = par("pin")
  usr = par("usr")

  unit = match.arg(unit)

    pt_per_inche1 = (usr[2] - usr[1])/pin[1]
  pt_per_inche2 = (usr[4] - usr[3])/pin[2]

    if(abs(pt_per_inche1 - pt_per_inche2) > 1e-3) {
        warning_wrap("`convert_length()` only works when aspect of the coordinate is 1.")
    }

  inche_per_mm = 0.0393700787401575
    # length in the data coordinate
  if(unit == "inches") {
    len = x * pt_per_inche1
  } else if(unit == "mm") {
    len = x * pt_per_inche1 * inche_per_mm
  } else if(unit == "cm") {
    len = x * pt_per_inche1 * inche_per_mm * 10
  }

    return(len)
}

# == title
# Convert units
#
# == param
# -... pass to `convert_length`
#
# == details
# This function is same as `convert_length`. The reason for naming this function
# is `convert_length` is mostely used for defining the height of tracks and track margins.
#
# == seealso
# For pre-defined units, users can use `cm_h`, `mm_h` and `inches_h`.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# # see example in `convert_length` page
# NULL
convert_height = function(...) {
    convert_length(...)
}

# == title
# Convert units
#
# == param
# -... pass to `convert_length`.
#
# == details
# Please do not use this function. Use `mm_h`/`cm_h`/inches_h` instead.
#
uh = function(...) {
    convert_length(...)
}

convert_unit_in_data_coordinate = function(x, unit = c("mm", "cm", "inches", "canvas"),
    direction = c("x", "y"),
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index(),
    h = get.cell.meta.data("ycenter", sector.index = sector.index, 
        track.index = track.index)) {

    pin = par("pin")
    usr = par("usr")

    unit = match.arg(unit)

    pt_per_inche1 = (usr[2] - usr[1])/pin[1]
    pt_per_inche2 = (usr[4] - usr[3])/pin[2]

    if(abs(pt_per_inche1 - pt_per_inche2) > 1e-3) {
        warning_wrap("`convert_unit_in_data_coordinate()` only works when aspect of the coordinate is 1.")
    }

    direction = match.arg(direction)

    inche_per_mm = 0.0393700787401575
    # length in the data coordinate
    if(unit == "inches") {
        len = x * pt_per_inche1
    } else if(unit == "mm") {
        len = x * pt_per_inche1 * inche_per_mm
    } else if(unit == "cm") {
        len = x * pt_per_inche1 * inche_per_mm * 10
    } else if(unit == "canvas") {
        len = x
    }

    xlim = get.cell.meta.data("xlim", sector.index = sector.index, track.index = track.index)
    ylim = get.cell.meta.data("ylim", sector.index = sector.index, track.index = track.index)

    
    if(direction == "x") {
        if(length(h) == 1) {
            h = rep(h, length(x))
        }
        w_in_data = xlim[2] - xlim[1]
        w_in_canvas = numeric(length(x))
        for(i in seq_along(x)) {
            coor_polar = circlize(xlim, c(h[i], h[i]), sector.index = sector.index, track.index = track.index)
            w_in_canvas[i] = abs(coor_polar[1, 1] - coor_polar[2, 1])/180*pi* coor_polar[1, 2]
        }
    } else {
        coor_polar = circlize(c(xlim[1], xlim[1]), ylim, sector.index = sector.index, track.index = track.index)
        w_in_data = ylim[2] - ylim[1]
        w_in_canvas = abs(coor_polar[2, 2] - coor_polar[1, 2])
    }

    len = len * abs(w_in_data/w_in_canvas)
    return(len)
}

# == title
# Convert unit on x direction in data coordinate
#
# == param
# -x a numeric vector.
# -unit supported units, only "mm", "cm", "inches".
# -sector.index index for the sector where the conversion is applied.
# -track.index index for the track where the conversion is applied.
# -h since the width of the cell is not identical from the top to the bottom in the cell, the position on
#   y direction needs to be specified. By default it is at the middle point on y-axis.
#
# == value
# A vector of numeric values which are measured in the specified data coordinate.
#
# == seealso
# For pre-defined units, users can use `cm_x`, `mm_x` and `inches_x`.
#
# `convert_y` converts on y direction.
#
# https://jokergoo.github.io/circlize_book/book/circular-layout.html#convert-functions
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# sectors = letters[1:10]
# circos.par(cell.padding = c(0, 0, 0, 0), track.margin = c(0, 0))
# circos.initialize(sectors, xlim = cbind(rep(0, 10), runif(10, 0.5, 1.5)))
# circos.track(ylim = c(0, 1), track.height = mm_h(5),
#     panel.fun = function(x, y) {
#         circos.lines(c(0, 0 + mm_x(5)), c(0.5, 0.5), col = "blue")
#     })
# circos.par(track.margin = c(0, mm_h(2)))
# circos.track(ylim = c(0, 1), track.height = cm_h(1),
#     panel.fun = function(x, y) {
#         xcenter = get.cell.meta.data("xcenter")
#         circos.lines(c(xcenter, xcenter), c(0, cm_y(1)), col = "red")
#     })
# circos.par(track.margin = c(0, mm_h(5)))
# circos.track(ylim = c(0, 1), track.height = inch_h(1),
#     panel.fun = function(x, y) {
#         line_length_on_x = cm_x(1*sqrt(2)/2)
#         line_length_on_y = cm_y(1*sqrt(2)/2)
#         circos.lines(c(0, line_length_on_x), c(0, line_length_on_y), col = "orange")
#     })
# circos.clear()
convert_x = function(
    x, 
    unit = c("mm", "cm", "inches"),
    sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"),
    h = get.cell.meta.data("ycenter", sector.index = sector.index, 
        track.index = track.index)) {
    convert_unit_in_data_coordinate(x, unit = unit, sector.index = sector.index, 
        track.index = track.index, h = h, direction = "x")
}

# == title
# Convert unit on x direction in data coordinate
#
# == param
# -... pass to `convert_x`.
#
# == details
# Please do not use this function. Use `mm_x`/`cm_x`/inches_x` instead.
#
ux = function(...) {
    convert_x(...)
}

# == title
# Convert unit on y direction in data coordinate
#
# == param
# -x a numeric vector
# -unit supported units, only "mm", "cm", "inches"
# -sector.index index for the sector where the conversion is applied
# -track.index index for the track where the conversion is applied
#
# == value
# A vector of numeric values which are measured in the specified data coordinate
#
# == seealso
# For pre-defined units, users can use `cm_y`, `mm_y` and `inches_y`.
#
# `convert_x` converts on x direction.
#
# https://jokergoo.github.io/circlize_book/book/circular-layout.html#convert-functions
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# # see example on `convert_x` page 
# NULL
convert_y = function(
    x, 
    unit = c("mm", "cm", "inches"),
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index()) {
    convert_unit_in_data_coordinate(x, unit = unit, sector.index = sector.index, 
        track.index = track.index, direction = "y")
}

# == title
# Convert unit on y direction in data coordinate
#
# == param
# -... pass to `convert_y`.
#
# == details
# Please do not use this function. Use `mm_y`/`cm_y`/inches_y` instead.
#
uy = function(...) {
    convert_y(...)
}

convert_unit_in_canvas_coordinate = function(x, unit = c("mm", "cm", "inches")) {

    pin = par("pin")
    usr = par("usr")

    unit = match.arg(unit)

    pt_per_inche1 = (usr[2] - usr[1])/pin[1]
    pt_per_inche2 = (usr[4] - usr[3])/pin[2]

    if(abs(pt_per_inche1 - pt_per_inche2) > 1e-3) {
        warning_wrap("`convert_unit_in_data_coordinate()` only works when aspect of the coordinate is 1.")
    }

    inche_per_mm = 0.0393700787401575
    # length in the data coordinate
    if(unit == "inches") {
        len = x * pt_per_inche1
    } else if(unit == "mm") {
        len = x * pt_per_inche1 * inche_per_mm
    } else if(unit == "cm") {
        len = x * pt_per_inche1 * inche_per_mm * 10
    }
    return(len)
}

# == title
# Convert unit on x direction in data coordinate
#
# == param
# -x The x-value in numeric.
# -sector.index Index of sector.
# -track.index Index of track.
# -... Pass to `convert_x`.
#
# == details
# See explanations in `convert_x` page.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# # see examples in `convert_x` page
# NULL
cm_x = function(x, sector.index = get.current.sector.index(),
    track.index = get.current.track.index(), ...) {
    convert_x(x, unit = "cm", sector.index = sector.index, track.index = track.index, ...)
}

# == title
# Convert unit on y direction in data coordinate
#
# == param
# -y The y-value in numeric.
# -sector.index Index of sector.
# -track.index Index of track.
#
# == details
# See explanations in `convert_y` page.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# # see examples in `convert_y` page
# NULL
cm_y = function(y, sector.index = get.current.sector.index(),
    track.index = get.current.track.index()) {
    convert_y(y, unit = "cm", sector.index = sector.index, track.index = track.index)
}

# == title
# Convert units
#
# == param
# -h The height in numeric.
#
# == details
# See explanations in `convert_length` page.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# # see examples in `convert_length` page
# NULL
cm_h = function(h) {
    convert_length(h, unit = "cm")
}

# == title
# Convert unit on x direction in data coordinate
#
# == param
# -x The x-value in numeric.
# -sector.index Index of sector.
# -track.index Index of track.
# -... Pass to `convert_x`.
#
# == details
# See explanations in `convert_x` page.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# # see examples in `convert_x` page
# NULL
mm_x = function(x, sector.index = get.current.sector.index(),
    track.index = get.current.track.index(), ...) {
    convert_x(x, unit = "mm", sector.index = sector.index, track.index = track.index, ...)
}

# == title
# Convert unit on y direction in data coordinate
#
# == param
# -y The y-value in numeric.
# -sector.index Index of sector.
# -track.index Index of track.
#
# == details
# See explanations in `convert_y` page.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# # see examples in `convert_y` page
# NULL
mm_y = function(y, sector.index = get.current.sector.index(),
    track.index = get.current.track.index()) {
    convert_y(y, unit = "mm", sector.index = sector.index, track.index = track.index)
}

# == title
# Convert units
#
# == param
# -h The height in numeric.
#
# == details
# See explanations in `convert_length` page.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# # see examples in `convert_length` page
# NULL
mm_h = function(h) {
    convert_length(h, unit = "mm")
}

# == title
# Convert unit on x direction in data coordinate
#
# == param
# -x The x-value in numeric.
# -sector.index Index of sector.
# -track.index Index of track.
# -... Pass to `convert_x`.
#
# == details
# See explanations in `convert_x` page.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# # see examples in `convert_x` page
# NULL
inches_x = function(x, sector.index = get.current.sector.index(),
    track.index = get.current.track.index(), ...) {
    convert_x(x, unit = "inches", sector.index = sector.index, track.index = track.index, ...)
}

# == title
# Convert unit on y direction in data coordinate
#
# == param
# -y The y-value in numeric.
# -sector.index Index of sector.
# -track.index Index of track.
#
# == details
# See explanations in `convert_y` page.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# # see examples in `convert_y` page
# NULL
inches_y = function(y, sector.index = get.current.sector.index(),
    track.index = get.current.track.index()) {
    convert_y(y, unit = "inches", sector.index = sector.index, track.index = track.index)
}

# == title
# Convert units
#
# == param
# -h The height in numeric.
#
# == details
# See explanations in `convert_length` page.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# # see examples in `convert_length` page
# NULL
inches_h = function(h) {
    convert_length(h, unit = "inches")
}

# == title
# Convert unit on x direction in data coordinate
#
# == param
# -... pass to `inches_x`.
#
# == details
# This function is the same as `inches_x`.
inch_x = function(...) {
    inches_x(...)
}
# == title
# Convert unit on y direction in data coordinate
#
# == param
# -... pass to `inches_y`
#
# == details
# This function is the same as `inches_y`.
inch_y = function(...) {
    inches_y(...)
}

# == title
# Convert units
#
# == param
# -... pass to `inches_h`
#
# == details
# This function is the same as `inches_h`.
inch_h = function(...) {
    inches_h(...)
}

# circos.unit(2, "mm")
# circos.unit(2, "native") / circos.unit(2, "data")
# circos.unit(1, "canvas")
# circos.unit(strwidth(), "canvas")
circos.unit = function(x, unit) {
    structure(x, unit = unit)
}