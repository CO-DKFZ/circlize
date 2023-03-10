\name{circos.genomicLines}
\alias{circos.genomicLines}
\title{
Add lines to a plotting region, specifically for genomic graphics
}
\description{
Add lines to a plotting region, specifically for genomic graphics
}
\usage{
circos.genomicLines(
    region,
    value,
    numeric.column = NULL,
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index(),
    posTransform = NULL,
    col = ifelse(area, "grey", "black"),
    lwd = par("lwd"),
    lty = par("lty"),
    type = "l",
    area = FALSE,
    area.baseline = NULL,
    border = "black",
    baseline = "bottom",
    pt.col = par("col"),
    cex = par("cex"),
    pch = par("pch"),
    ...)
}
\arguments{

  \item{region}{A data frame contains 2 column which correspond to start positions and end positions.}
  \item{value}{A data frame contains values and other information.}
  \item{numeric.column}{Which column in \code{value} data frame should be taken as y-value. If it is not defined, the whole numeric columns in \code{value} will be taken.}
  \item{sector.index}{Index of sector.}
  \item{track.index}{Index of track.}
  \item{posTransform}{Self-defined function to transform genomic positions, see \code{\link{posTransform.default}} for explaination.}
  \item{col}{col of lines/areas. If there are more than one numeric column, the length of \code{col} can be either one or number of numeric columns. If there is only one numeric column and type is either \code{segment} or \code{h},  the length of \code{col} can be either one or number of rows of \code{region}. pass to \code{\link{circos.lines}}}
  \item{lwd}{Settings are similar as \code{col}. Pass to \code{\link{circos.lines}}.}
  \item{lty}{Settings are similar as \code{col}. Pass to \code{\link{circos.lines}}.}
  \item{type}{There is an additional option \code{segment} which plot segment lines from start position to end position. Settings are similar as \code{col}. Pass to \code{\link{circos.lines}}. }
  \item{area}{Settings are similar as \code{col}. Pass to \code{\link{circos.lines}}.}
  \item{area.baseline}{Deprecated, use \code{baseline} instead.}
  \item{baseline}{Settings are similar as \code{col}. Pass to \code{\link{circos.lines}}.}
  \item{border}{Settings are similar as \code{col}. Pass to \code{\link{circos.lines}}.}
  \item{pt.col}{Settings are similar as \code{col}. Pass to \code{\link{circos.lines}}.}
  \item{cex}{Settings are similar as \code{col}. Pass to \code{\link{circos.lines}}.}
  \item{pch}{Settings are similar as \code{col}. Pass to \code{\link{circos.lines}}.}
  \item{...}{Mysterious parameters.}

}
\details{
The function is a low-level graphical function and usually is put in \code{panel.fun} when using \code{\link{circos.genomicTrack}}.

The function behaviours differently from different formats of input, see the examples in 
the "Examples" Section or go to \url{https://jokergoo.github.io/circlize_book/book/modes-of-input.html} for more details.
}
\examples{
\donttest{
### test bed
circos.par("track.height" = 0.1)
circos.initializeWithIdeogram(plotType = NULL)

bed = generateRandomBed(nr = 100)
circos.genomicTrack(bed, panel.fun = function(region, value, ...) {
    circos.genomicLines(region, value, type = "l", ...)
})

bed1 = generateRandomBed(nr = 100)
bed2 = generateRandomBed(nr = 100)
bed_list = list(bed1, bed2)

circos.genomicTrack(bed_list, panel.fun = function(region, value, ...) {
    i = getI(...)
    circos.genomicLines(region, value, col = i, ...)
})

circos.genomicTrack(bed_list, stack = TRUE, 
    panel.fun = function(region, value, ...) {
    i = getI(...)
    circos.genomicLines(region, value, col = i, ...)
})

bed = generateRandomBed(nr = 100, nc = 4)
circos.genomicTrack(bed, panel.fun = function(region, value, ...) {
    circos.genomicLines(region, value, col = 1:4, ...)
})

circos.genomicTrack(bed, stack = TRUE, panel.fun = function(region, value, ...) {
    i = getI(...)
    circos.genomicLines(region, value, col = i, ...)
})

bed = generateRandomBed(nr = 100)
circos.genomicTrack(bed, panel.fun = function(region, value, ...) {
    circos.genomicLines(region, value, type = "segment", lwd = 2, ...)
})

circos.clear()
}
}
