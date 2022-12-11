# this file contains variables and functions related to
# global variables.

.CIRCOS.ENV = new.env()

resetGlobalVariable = function() {
	assign(".SECTOR.DATA", NULL, envir = .CIRCOS.ENV)
	assign(".CELL.DATA", NULL, envir = .CIRCOS.ENV)
	assign(".CURRENT.TRACK.INDEX", 0, envir = .CIRCOS.ENV)
	assign(".CURRENT.SECTOR.INDEX", NULL, envir = .CIRCOS.ENV)
}

resetGlobalVariable()

# == title
# Parameters for the circular layout
#
# == param
# -... Arguments for the parameters, see "details" section
# -RESET reset to default values
# -READ.ONLY please ignore
# -LOCAL please ignore
# -ADD please ignore
#
# == details
# Global parameters for the circular layout. Currently supported parameters are:
#
# -``start.degree``            The starting degree from which the circle begins to draw. Note this degree is measured
#     in the standard polar coordinate which means it is always reverse-clockwise.
# -``gap.degree``             Gap between two neighbour sectors. It can be a single value or a vector. If it is a vector,
#                          the first value corresponds to the gap after the first sector.
# -``gap.after`` identical to ``gap.degree`` option, but a more understandable name. Modifying this option will also affect ``gap.degree``.
# -``track.margin``            Like ``margin`` in Cascading Style Sheets (CSS), it is the blank area
#     out of the plotting region, also outside of the borders. Since left and right margin are controlled
#     by ``gap.degree``, only bottom and top margin need to be set. And all cells in a same track share the same margins, and
#     that's why this parameter is called ``track.margin``. The value for the ``track.margin``
#     is the percentage according to the radius of the unit circle. `convert_height` can be used to set to an absolute unit (e.g cm/inche).
# -``unit.circle.segments``    Since curves are simulated by a series of straight lines,
#     this parameter controls the amount of segments to represent a curve. The minimal length
#     of the line segmentation is the length of the unit circle (``2pi``) divided by ``unit.circoe.segments``.
#     More segments means better approximation for the curves while larger size if you generate figures as PDF format.
# -``cell.padding``            Padding of the cell. Like ``padding`` in Cascading Style Sheets
#    (CSS), it is the blank area around the plotting regions, but within the borders.
#     The parameter has four values, which controls the bottom, left, top and right paddings
#     respectively. The first and the third padding
#     values are the percentages according to the radius of the unit circle and the second and
#     fourth values are degrees. Similar as ``track.margin`` option, the first and the third value
#     can be set by `convert_height` to an absolute unit.
# -``track.height``    The default height of tracks. It is the percentage according to the radius
#     of the unit circle. The height includes the top and bottom cell paddings but not the margins.
#     `convert_height` can be used to set the height to an absolute unit.
# -``points.overflow.warning`` Since each cell is in fact not a real plotting region but only
#     an ordinary rectangle, it does not eliminate points that are plotted out of
#     the region. So if some points are out of the plotting region, ``circlize`` would continue drawing the points and printing warnings. In some
#     cases, draw something out of the plotting region is useful, such as draw
#     some legend or text. Set this value to ``FALSE`` to turn off the warnings.
# -``circle.margin``  Margin in the horizontal and vertical direction. The value should be a positive numeric vector
#    and the length of it should be either 1, 2, or 4. When it has length of 1, it controls the margin on the four sides of the circle.
#    When it has length of 2, the first value controls the margin on the left and right, and the second value controls
#    the margin on the bottom and top side. When it has length of 4, the four values controls the margins on the left, right, bottom and top sides
#    of the circle. So A value of ``c(x1, x2, y1, y2)`` means ``circos.par(canvas.xlim = c(-(1+x1), 1+x2), canvas.ylim = c(-(1+y1), 1+y2))``.
# -``canvas.xlim``              The coordinate for the canvas. Because ``circlize`` draws everything (or almost everything) inside the unit circle,
#     the default ``canvas.xlim`` and ``canvas.ylim`` for the canvas would be all ``c(-1, 1)``. However, you can set it to a more broad
#     interval if you want to draw other things out of the circle. By choosing proper
#     ``canvas.xlim`` and ``canvas.ylim``, you can draw part of the circle. E.g. setting
#     ``canvas.xlim`` to ``c(0, 1)`` and ``canvas.ylim`` to ``c(0, 1)`` would only draw
#     circle in the region of (0, pi/2).
# -``canvas.ylim``              The coordinate for the canvas. By default it is ``c(-1, 1)``
# -``clock.wise``               The direction for adding sectors. Default is ``TRUE``.
# -``xaxis.clock.wise``    The direction in the x-axes for all sectors. Default is ``TRUE``.
#
# Similar as `graphics::par`, you can get the parameter values by specifying the
# names of parameters and you can set the parameter values by specifying a
# named list which contains the new values.
#
# ``gap.degree``, ``start.degree``, ``canvas.xlim``, ``canvas.ylim`` and ``clock.wise``
# only be set before the initialization of the circular layout
# (i.e. before calling `circos.initialize`) because these values will not be changed after
# adding sectors on the circle. The left and right padding for ``cell.padding`` will also be
# ignored after the initialization because all cells in a sector would share the same
# left and right paddings.
#
# == seealso
# https://jokergoo.github.io/circlize_book/book/circular-layout.html#graphic-parameters
#
# == example
# circos.par
circos.par = function(..., RESET = FALSE, READ.ONLY = NULL, LOCAL = FALSE, ADD = FALSE) {}
circos.par = setGlobalOptions(
	start.degree = list(
		.value = 0,
		.length = 1,
		.class = "numeric",
		.filter = function(x) {
			if(is.circos.initialized()){
				warning_wrap("'start.degree' can only be modified before `circos.initialize`, or maybe you forgot to call `circos.clear` in your last plot.")
			}
			return(x)
		}),
	gap.degree = list(
		.value = 1,
		.class = "numeric",
		.validate = function(x) {
			all(x >= 0 & x < 360)
		},
		.filter = function(x) {
			if(is.circos.initialized()){
				warning_wrap("'gap.degree' can only be modified before `circos.initialize`, or maybe you forgot to call `circos.clear` in your last plot.")
			}
			return(x)
		}
		),
	gap.after = list(.synonymous = "gap.degree"),
	track.margin = list(
		.value = c(0.01, 0.01),  # top margin and bottom margin, percentage
		.length = 2,
		.class = "numeric"
		),
	unit.circle.segments = 500,   #to simulate smooth curve
	cell.padding = list(
		.value = c(0.02, 1, 0.02, 1),  # percentage
		.length = c(2, 4),
		.class = "numeric",
		.filter = function(x) {
			if(length(x) == 2) x = c(x, x)
			o.cell.padding = circos.par("cell.padding")
			if(is.circos.initialized()){
				return(c(x[1], o.cell.padding[2], x[3], o.cell.padding[4]))
			} else {
				return(x)
			}
		}),
	default.track.height = list(
		.value = 0.2,
		.visible = FALSE,
		.filter = function(x) {
			warning_wrap("`default.track.height` is replaced by `track.height`, ignore this setting.")
			return(x)
		}),
	track.height = 0.2,
	points.overflow.warning = TRUE,
	circle.margin = list(
		.value = c(0, 0, 0, 0),
		.filter = function(x) {
			if(is.circos.initialized()){
				warning_wrap("'circle.margin' can only be modified before `circos.initialize`, or maybe you forgot to call `circos.clear` in your last plot.")
			}
			if(any(x <= 0)) {
				stop_wrap("The value of `circle.margin` should be positive.")
			}
			if(length(x) == 1) {
				x = rep(x, 4)
			} else if(length(x) == 2) {
				x = rep(x, each = 2)
			} else if(length(x) == 4) {

			} else {
				stop_wrap("Length of `circle.margin` can only be 1, 2, or 4.")
			}
			return(x)
		}
	),
	canvas.xlim = list(
		.value = function() {
			c(-(1 + .v$circle.margin[1]), (1 + .v$circle.margin[2]))
		},
		.filter = function(x) {
			if(is.circos.initialized()){
				warning_wrap("'canvas.xlim' can only be modified before `circos.initialize`, or maybe you forgot to call `circos.clear` in your last plot.")
			}
			return(x)
		}),
	canvas.ylim = list(
		.value = function() {
			c(-(1 + .v$circle.margin[3]), (1 + .v$circle.margin[4]))
		},
		.filter = function(x) {
			if(is.circos.initialized()){
				warning_wrap("'canvas.ylim' can only be modified before `circos.initialize`, or maybe you forgot to call `circos.clear` in your last plot.")
			}
			return(x)
		}),
	major.by.degree = 10,
	clock.wise = list(
		.value = TRUE,
		.filter = function(x) {
			if(is.circos.initialized()){
				warning_wrap("'clock.wise' can only be modified before `circos.initialize`, or maybe you forgot to call `circos.clear` in your last plot.")
			}
			return(x)
		}),
	xaxis.clock.wise = list(
		.value = TRUE,
		.filter = function(x) {
			if(is.circos.initialized() && environmentName(topenv()) != "circlize"){
				stop_wrap("'xaxis.clock.wise' can only be modified before `circos.initialize`, or maybe you forgot to call `circos.clear` in your last plot.")
			}
			return(x)
		}),
	lend = list(
		.value = NULL,
		.visible = FALSE,
		.private = TRUE),
	ljoin = list(
		.value = NULL,
		.visible = FALSE,
		.private = TRUE),
	'__tempdir__' = list(
		.value = ".",
		.private = TRUE,
		.filter = function(x) {dir.create(x, showWarnings = FALSE); return(x)},
		.visible = FALSE),
	'__omar__' = list(   # is par("mar") the default value?
		.value = FALSE,
		.private = TRUE,
		.visible = FALSE),
	'__tempenv__' = list(
		.value = new.env(parent = emptyenv()),
		.private = TRUE,
		.visible = FALSE),
	message = TRUE,
	help = list(.synonymous = "message"),
	ring = list(
		.value = FALSE,
		.private = TRUE,
		.visible = FALSE
	),
	offset = list(
		.value = NULL
	)
)

