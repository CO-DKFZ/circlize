

# == title
# Add circular dendrograms
#
# == param
# -dend A `stats::dendrogram` object.
# -facing Is the dendromgrams facing inside to the circle or outside?
# -max_height Maximum height of the dendrogram. This is important if more than one dendrograms
#             are drawn in one track and making them comparable. The height of a dendrogram
#             can be obtained by ``attr(dend, "height")``.
# -use_x_attr Whether use the ``x`` attribute to determine node positions in the dendrogram, used internally.
# -sector.index Index of sector.
# -track.index Index of track.
#
# == details
# Assuming there are ``n`` nodes in the dendrogram, the positions for leaves on x-axis are always ``0.5, 1.5, ..., n - 0.5``.
# So you must be careful with ``xlim`` when you initialize the cirular layout.
#
# You can use the ``dendextend`` package to render the dendrograms.
#
# == seealso
# https://jokergoo.github.io/circlize_book/book/high-level-plots.html#phylogenetic-trees
#
# == example
# load(system.file(package = "circlize", "extdata", "bird.orders.RData"))
#
# labels = hc$labels  # name of birds
# ct = cutree(hc, 6)  # cut tree into 6 pieces
# n = length(labels)  # number of bird species
# dend = as.dendrogram(hc)
#
# circos.par(cell.padding = c(0, 0, 0, 0))
# circos.initialize(sectors = "a", xlim = c(0, n)) # only one sector
# max_height = attr(dend, "height")  # maximum height of the trees
# circos.trackPlotRegion(ylim = c(0, 1), bg.border = NA, track.height = 0.3,
#     panel.fun = function(x, y) {
#         for(i in seq_len(n)) {
#             circos.text(i-0.5, 0, labels[i], adj = c(0, 0.5),
#                 facing = "clockwise", niceFacing = TRUE,
#                 col = ct[labels[i]], cex = 0.7)
#         }
# })
#
# suppressPackageStartupMessages(require(dendextend))
# dend = color_branches(dend, k = 6, col = 1:6)
#
# circos.trackPlotRegion(ylim = c(0, max_height), bg.border = NA,
#     track.height = 0.4, panel.fun = function(x, y) {
#         circos.dendrogram(dend, max_height = max_height)
# })
# circos.clear()
circos.dendrogram = function(
    dend,
    facing = c("outside", "inside"),
    max_height = NULL,
    use_x_attr = FALSE,
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index()) {

    os = get.current.sector.index()
    ot = get.current.track.index()
    set.current.cell(sector.index, track.index)
    on.exit(set.current.cell(os, ot))
    
    facing = match.arg(facing)[1]

    if(is.null(max_height)) {
        max_height = attr(dend, "height")
    }

    is.leaf = function(object) {
        leaf = attr(object, "leaf")
        if(is.null(leaf)) {
            FALSE
        } else {
            leaf
        }
    }

    use_x_attr = use_x_attr

    lines_par = function(col = par("col"), lty = par("lty"), lwd = par("lwd"), ...) {
        return(list(col = col, lty = lty, lwd = lwd))
    }

    points_par = function(col = par("col"), pch = par("pch"), cex = par("cex"), ...) {
        return(list(col = col, pch = pch, cex = cex))
    }

    draw.d = function(dend, max_height, facing = "outside", max_width = 0) {
        leaf = attr(dend, "leaf")
        height = attr(dend, "height")
        midpoint = attr(dend, "midpoint")
        n = length(dend)

        xl = numeric(n)
        yl = numeric(n)
        for(i in seq_len(n)) {
            if(use_x_attr) {
                xl[i] = attr(dend[[i]], "x")
            } else {
                if(is.leaf(dend[[i]])) {
                    xl[i] = x[as.character(attr(dend[[i]], "label"))]
                } else {
                    xl[i] = attr(dend[[i]], "midpoint") + x[as.character(labels(dend[[i]]))[1]]
                }
            }
            yl[i] = attr(dend[[i]], "height")
        }

        # graphic parameter for current branch
        # only for lines, there are lwd, col, lty
        edge_par_lt = vector("list", n)
        for(i in seq_len(n)) {
            edge_par_lt[[i]] = do.call("lines_par", as.list(attr(dend[[i]], "edgePar")))  # as.list to convert NULL to list()
        }
        node_par = attr(dend, "nodePar")
        if(!is.null(node_par)) node_par = do.call("points_par", as.list(attr(dend, "nodePar")))

        # plot the connection line
        if(facing == "outside") {
            if(n == 1) {
                circos.lines(c(xl[1], xl[1]), max_height - c(yl[1], height), 
                    col = edge_par_lt[[1]]$col, lty = edge_par_lt[[1]]$lty, lwd = edge_par_lt[[1]]$lwd, straight = TRUE)
            } else {
                circos.lines(c(xl[1], xl[1]), max_height - c(yl[1], height), 
                    col = edge_par_lt[[1]]$col, lty = edge_par_lt[[1]]$lty, lwd = edge_par_lt[[1]]$lwd, straight = TRUE)
                circos.lines(c(xl[1], (xl[1]+xl[2])/2), max_height - c(height, height), 
                    col = edge_par_lt[[1]]$col, lty = edge_par_lt[[1]]$lty, lwd = edge_par_lt[[1]]$lwd)
                if(n > 2) {
                    for(i in seq(2, n-1)) {
                        circos.lines(c(xl[i], xl[i]), max_height - c(yl[i], height), 
                            col = edge_par_lt[[i]]$col, lty = edge_par_lt[[i]]$lty, lwd = edge_par_lt[[i]]$lwd, straight = TRUE)
                        circos.lines(c((xl[i-1]+xl[i])/2, (xl[i]+xl[i+1])/2), max_height - c(height, height), 
                            col = edge_par_lt[[i]]$col, lty = edge_par_lt[[i]]$lty, lwd = edge_par_lt[[i]]$lwd)
                    }
                }
                circos.lines(c(xl[n], xl[n]), max_height - c(yl[n], height), 
                    col = edge_par_lt[[n]]$col, lty = edge_par_lt[[n]]$lty, lwd = edge_par_lt[[n]]$lwd, straight = TRUE)
                circos.lines(c(xl[n], (xl[n]+xl[n-1])/2), max_height - c(height, height), 
                    col = edge_par_lt[[n]]$col, lty = edge_par_lt[[n]]$lty, lwd = edge_par_lt[[n]]$lwd)
            }
            if(!is.null(node_par)) {
                circos.points(mean(xl)/2, max_height - height, col = node_par$col, pch = node_par$pch, cex = node_par$cex)
            }
        } else if(facing == "inside") {
            if(n == 1) {
                circos.lines(c(xl[1], xl[1]), c(yl[1], height), 
                    col = edge_par_lt[[1]]$col, lty = edge_par_lt[[1]]$lty, lwd = edge_par_lt[[1]]$lwd, straight = TRUE)
            } else {
                circos.lines(c(xl[1], xl[1]), c(yl[1], height), 
                    col = edge_par_lt[[1]]$col, lty = edge_par_lt[[1]]$lty, lwd = edge_par_lt[[1]]$lwd, straight = TRUE)
                circos.lines(c(xl[1], (xl[1]+xl[2])/2), c(height, height), 
                    col = edge_par_lt[[1]]$col, lty = edge_par_lt[[1]]$lty, lwd = edge_par_lt[[1]]$lwd)
                if(n > 2) {
                    for(i in seq(2, n-1)) {
                        circos.lines(c(xl[i], xl[i]), c(yl[i], height), 
                            col = edge_par_lt[[i]]$col, lty = edge_par_lt[[i]]$lty, lwd = edge_par_lt[[i]]$lwd, straight = TRUE)
                        circos.lines(c((xl[i-1]+xl[i])/2, (xl[i]+xl[i+1])/2), c(height, height), 
                            col = edge_par_lt[[i]]$col, lty = edge_par_lt[[i]]$lty, lwd = edge_par_lt[[i]]$lwd)
                    }
                }
                circos.lines(c(xl[n], xl[n]), c(yl[n], height), 
                    col = edge_par_lt[[n]]$col, lty = edge_par_lt[[n]]$lty, lwd = edge_par_lt[[n]]$lwd, straight = TRUE)
                circos.lines(c(xl[n], (xl[n]+xl[n-1])/2), c(height, height), 
                    col = edge_par_lt[[n]]$col, lty = edge_par_lt[[n]]$lty, lwd = edge_par_lt[[n]]$lwd)
            }
            if(!is.null(node_par)) {
                circos.points(mean(xl)/2, height, col = node_par$col, pch = node_par$pch, cex = node_par$cex)
            }
        }

        # do it recursively
        for(i in seq_len(n)) {
            if(is.leaf(dend[[i]])) {
                node_par = attr(dend[[i]], "nodePar")
                if(!is.null(node_par)) node_par = do.call("points_par", as.list(attr(dend[[i]], "nodePar")))
                if(facing == "outside") {
                    if(!is.null(node_par)) {
                        circos.points(xl[i], max_height, col = node_par$col, pch = node_par$pch, cex = node_par$cex)
                    }
                } else if(facing == "inside") {
                    if(!is.null(node_par)) {
                        circos.points(xl[i], 0, col = node_par$col, pch = node_par$pch, cex = node_par$cex)
                    }
                }
            } else {
                draw.d(dend[[i]], max_height, facing, max_width)
            }
        }
    }

    labels = as.character(labels(dend))
    x = seq_along(labels) - 0.5

    names(x) = labels
    n = length(labels)

    if(!is.leaf(dend)) draw.d(dend, max_height, facing, max_width = n)
}


circos.dendrogram2 = function(dend, col = 1, lty = 1, lwd = 1, ...) {
    lt = spiralize:::construct_dend_segments(dend, list(col = col, lty = lty, lwd = lwd))
        
    yrange = get.cell.meta.data("ylim", ...)[2]

    ymax2 = max(lt$y0, lt$y1)

    lt$y0 = lt$y0/ymax2 * yrange
    lt$y1 = lt$y1/ymax2 * yrange
   
    circos.segments(lt$x0, lt$y0, lt$x1, lt$y1, lwd = lt$lwd, lty = lt$lty, col = lt$col, ...)   
}
