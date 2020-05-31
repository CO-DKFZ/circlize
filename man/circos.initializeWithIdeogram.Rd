\name{circos.initializeWithIdeogram}
\alias{circos.initializeWithIdeogram}
\title{
Initialize the circular layout with an ideogram
}
\description{
Initialize the circular layout with an ideogram
}
\usage{
circos.initializeWithIdeogram(
    cytoband = system.file(package = "circlize", "extdata", "cytoBand.txt"),
    species = NULL,
    sort.chr = TRUE,
    chromosome.index = usable_chromosomes(species),
    major.by = NULL,
    plotType = c("ideogram", "axis", "labels"),
    track.height = NULL,
    ideogram.height = convert_height(2, "mm"),
    ...)
}
\arguments{

  \item{cytoband}{A path of the cytoband file or a data frame that already contains cytoband data. By default it is cytoband for hg19.
  \item{species}{Abbreviations of species. e.g. hg19 for human, mm10 for mouse. If this
  \item{chromosome.index}{subset of chromosomes, also used to reorder chromosomes.
  \item{sort.chr}{Whether chromosome names should be sorted (first sort by numbers then by letters).
  \item{major.by}{Increment of major ticks. Pass to \code{\link{circos.genomicInitialize}}.
  \item{plotType}{Which tracks should be drawn. \code{ideogram} for ideogram rectangle, \code{axis} for genomic axis and \code{labels} for chromosome names.
  \item{track.height}{Height of the track which contains "axis" and "labels".
  \item{ideogram.height}{Height of the ideogram track
  \item{...}{Pass to \code{\link{circos.genomicInitialize}}.

}
\details{
The function will initialize the circular plot in which each sector corresponds to a chromosome. You can control the order of 
chromosomes by \code{chromosome.index} or by \code{sort.chr}, or by setting a special format of \code{cytoband} (please refer to \code{\link{read.cytoband}} 
to find out how to control a proper \code{cytoband}).

The function finally pass data to \code{\link{circos.genomicInitialize}} to initialize the circular plot.

The style of ideogram is almost fixed, but you can customize it with your self-sefined code. Refer to vignette for demonstration.
}
\seealso{
\url{https://jokergoo.github.io/circlize_book/book/initialize-genomic-plot.html#initialize-cytoband}
}
\examples{
\donttest{
circos.initializeWithIdeogram()

cytoband.file = system.file(package = "circlize",
    "extdata", "cytoBand.txt")
circos.initializeWithIdeogram(cytoband.file)

cytoband.df = read.table(cytoband.file, colClasses = c("character", "numeric",
    "numeric", "character", "character"), sep = "\t")
circos.initializeWithIdeogram(cytoband.df)

circos.initializeWithIdeogram(species = "hg18")

circos.initializeWithIdeogram(species = "mm10")

circos.initializeWithIdeogram(chromosome.index = c("chr1", "chr2"))

cytoband = read.table(cytoband.file, colClasses = c("character", "numeric",
    "numeric", "character", "character"), sep = "\t")
circos.initializeWithIdeogram(cytoband, sort.chr = FALSE)

cytoband[[1]] = factor(cytoband[[1]], levels = paste0("chr", c(22:1, "X", "Y")))
circos.initializeWithIdeogram(cytoband, sort.chr = FALSE)

cytoband = read.table(cytoband.file, colClasses = c("character", "numeric",
    "numeric", "character", "character"), sep = "\t")
circos.initializeWithIdeogram(cytoband, sort.chr = TRUE)

circos.initializeWithIdeogram(plotType = c("axis", "labels"))

circos.initializeWithIdeogram(plotType = NULL)

circos.par("start.degree" = 90)
circos.initializeWithIdeogram()
circos.clear()

circos.par("gap.degree" = rep(c(2, 4), 12))
circos.initializeWithIdeogram()
circos.clear()
}
}