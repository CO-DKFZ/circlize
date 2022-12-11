
# == title
# Add a stacked text track
#
# == param
# -sectors A vector of sector names.
# -x  A vector of x-coordinates.
# -text  A vector of texts.
# -bg.border  Background color.
# -bg.col  Colors for borders.
# -niceFacing Current not supported.
# -side Side of the track.
# -col Text colors.
# -font Text fontfaces.
# -cex Font sizes.
# -family Font families.
#
# == details
# The height of the track is not fixed, so you may need to manually adjust the track height.
#
# == example
# \dontrun{
# circos.par$circle.margin = 0.5
# circos.par$cell.padding = rep(0, 4)
# circos.par$track.margin = rep(0, 2)
#
# circos.initialize(sectors = letters[1:4], xlim = c(0, 1))
#
# sectors = sample(letters[1:4], 40, replace = TRUE)
# x = runif(40)
# text = sapply(letters[sample(26, 40, replace = TRUE)], function(x) strrep(x, sample(4:6, 1)))
# circos.stackedText(sectors, x, text, bg.col = "#EEEEEE")
#
# circos.track(ylim = c(0, 1))
# circos.clear()
#
# #### genome plot
# circos.par$track.margin = rep(0, 2)
# circos.par$cell.padding = rep(0, 4)
#
# circos.initializeWithIdeogram(plotType = NULL)
# bed = generateRandomBed(50)
# text = sapply(
#     letters[sample(26, nrow(bed), replace = TRUE)], 
#     function(x) strrep(x, sample(4:6, 1))
# )
# bed$text = text
#
# circos.stackedText(bed[, 1], bed[, 2], bed$text, cex = 0.7)
# circos.genomicIdeogram()
# circos.clear()
#
# }
circos.stackedText = function(sectors, x, text, 
    col = par("col"), font = par("font"), cex = par("cex"), family = par("family"),
    bg.border = "black", bg.col = "#FF8080",
    niceFacing = FALSE, 
    side = c("outside", "inside")) {

    side = match.arg(side)[1]

    if(niceFacing) {
        stop_wrap("Current `niceFacing` is not supported.")
    }

    n = length(x)

    if(length(bg.border) == 1) {
        bg.border = rep(bg.border, n)
    }
    if(length(bg.col) == 1) {
        bg.col = rep(bg.col, n)
    }
    if(length(col) == 1) {
        col = rep(col, n)
    }
    if(length(font) == 1) {
        font = rep(font, n)
    }
    if(length(cex) == 1) {
        cex = rep(cex, n)
    }

    op = circos.par("points.overflow.warning")
    circos.par("points.overflow.warning" = FALSE)

    circos.track(ylim = c(0, 1), bg.border = NA)

    df = refer_to_one_sector(sectors, x)
    set.current.sector.index(df[1, 1])

    x = df$x
    text = text[df$order]

    tw = circos.strwidth(text, cex = cex, font = font, family = family)
    th = circos.strheight(text, cex = cex, font = font, family = family)
    ta = circos.strAscent(text, cex = cex, font = font, family = family)
    td = circos.strDescent(text, cex = cex, font = font, family = family)

    lt = stack_text(x, text, strwidth = circos.strwidth, strheight = circos.strheight, margin = 0.2,
        cex = cex, font = font, family = family)

    if(side == "outside") {
        y = mm_y(4.5)
    } else {
        y = 1 - mm_y(4.5)
    }

    for(i in seq_along(lt)) {
        ind = lt[[i]]
        if(side == "outside") {
            circos.segments(x[ind], 0, x[ind], y, col = bg.border)
            y = y + mm_y(0.5) + max(td[ind] + ta[ind]) + mm_y(0.5) + mm_y(0.5)
        } else {
            circos.segments(x[ind], 1, x[ind], y, col = bg.border)
            y = y - mm_y(0.5) - max(td[ind] + ta[ind]) - mm_y(0.5) - mm_y(0.5)
        }
        
    }

    if(side == "outside") {
        y = mm_y(4.5)
    } else {
        y = 1 - mm_y(4.5)
    }

    for(i in seq_along(lt)) {
        ind = lt[[i]]
        if(side == "outside") {
            tw = circos.strwidth(text, h = y, cex = cex, font = font, family = family)
            circos.rect(x[ind] - tw[ind]/2 - mm_x(0.5), 
                y, 
                x[ind] + tw[ind]/2 + mm_x(0.5), 
                y + mm_y(0.5) + td[ind] + ta[ind] + mm_y(0.5), 
                col = bg.col[ind], border = bg.border[ind],
                radius = mm_h(1))
            circos.text(x[ind], y + mm_y(0.5) + td[ind] + th[ind]/2, 
                text[ind], facing = "bending.inside", niceFacing = niceFacing, 
                col = col[ind], cex = cex[ind], font = font[ind], family = family)
            y = y + mm_y(0.5) + max(td[ind] + ta[ind]) + mm_y(0.5) + mm_y(0.5)
        } else {
            tw = circos.strwidth(text, h = y, cex = cex, font = font, family = family)
            circos.rect(x[ind] - tw[ind]/2 - mm_x(0.5), 
                y - mm_y(0.5) - td[ind] - ta[ind] - mm_y(0.5),
                x[ind] + tw[ind]/2 + mm_x(0.5), 
                y, 
                col = bg.col[ind], border = bg.border[ind],
                radius = mm_h(1))
            circos.text(x[ind], y - mm_y(0.5) - (ta[ind] - th[ind]/2), 
                text[ind], facing = "bending.inside", niceFacing = niceFacing,
                col = col[ind], cex = cex[ind], font = font[ind], family = family)
            y = y - mm_y(0.5) - max(td[ind] + ta[ind]) - mm_y(0.5) - mm_y(0.5)
        }
    }

    circos.par("points.overflow.warning" = op)
}



stack_text = function(x, text, margin = 0.2, 
    strwidth = strwidth, strheight = strheight, ...) {

    tw = sapply(text, strwidth, ...)
    th = sapply(text, strheight, ...)

    e = min(c(tw, th))*margin

    tw = tw + e

    x1 = x - tw/2
    x2 = x + tw/2
    ind = seq_along(x)

    od = order(x1)
    tw = tw[od]
    x1 = x1[od]
    x2 = x2[od]
    ind = ind[od]

    lt = list()

    i_lt = 1
    while(length(x1)) {
        v = ind[1]
        x_right = x2[1]

        ind_used = 1
        for(i in seq_along(x1)[-1]) {
            if(x1[i] >= x_right) {
                v = c(v, ind[i])
                x_right = x2[i]
                ind_used = c(ind_used, i)
            }
        }
        x1 = x1[-ind_used]
        x2 = x2[-ind_used]
        ind = ind[-ind_used]

        lt[[i_lt]] = v

        i_lt = i_lt + 1

    }

    lt
}


circos.strwidth = function(text, ..., 
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index(),
    h = get.cell.meta.data("ylim", sector.index = sector.index, track.index = track.index)[1]) {

    chars = strsplit(text, "")
        
    nlabel = length(text)
    strw = lapply(chars, strwidth, ...) # width of every character in text
    
    strw = sapply(strw, function(w) {
        w2 = convert_unit_in_data_coordinate(w, unit = "canvas", direction = "x",
            sector.index = sector.index, track.index = track.index, h = h)
        sum(w2)
    })

    strw
}

circos.strheight = function(text, ..., 
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index()) {

    strh = lapply(text, strheight, ...)
    
    strh = sapply(strh, function(w) {
        w2 = convert_unit_in_data_coordinate(w, unit = "canvas", direction = "y",
            sector.index = sector.index, track.index = track.index)
        max(w2)
    })

    strh
}

circos.strAscent = function(text, cex = par("cex"), font = par("font"), family = par("family"),
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index()) {

    sapply(text, function(t) {
        gb = textGrob(t, gp = gpar(cex = cex, fontface = font, fontfamily = family))
        h = grobAscent(gb)
        h = convertHeight(h, "mm", valueOnly = TRUE)
        mm_y(h, sector.index = sector.index, track.index = track.index)
    })
}

circos.strDescent = function(text, cex = par("cex"), font = par("font"), family = par("family"),
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index()) {

    sapply(text, function(t) {
        gb = textGrob(t, gp = gpar(cex = cex, fontface = font, fontfamily = family))
        h = grobDescent(gb)
        h = convertHeight(h, "mm", valueOnly = TRUE)
        mm_y(h, sector.index = sector.index, track.index = track.index)
    })
}
