\name{circos.initialize}
\alias{circos.initialize}
\title{
Initialize the circular layout
}
\description{
Initialize the circular layout
}
\usage{
circos.initialize(
    sectors = NULL,
    x = NULL,
    xlim = NULL,
    sector.width = NULL,
    factors = sectors,
    ring = FALSE)
}
\arguments{

  \item{sectors}{A \code{\link{factor}} variable or a character vector which represent data categories}
  \item{factors}{The same as \code{sectors}. It will be removed in future versions. }
  \item{x}{Data on x-axes, a vector}
  \item{xlim}{Ranges for values on x-axes, see "details" section for explanation of the format}
  \item{sector.width}{Width for each sector. The length of the vector should be either 1 which means all sectors have same width or as same as the number of sectors. Values for the vector are relative, and they will be scaled by dividing their summation. By default, it is \code{NULL} which means the width of sectors correspond to the data range in sectors.}
  \item{ring}{Whether the sector represented as a ring. If yes, there should only be one sector in the circle.}

}
\details{
The function allocates the sectors according to the values on x-axis.
The number of sectors are determined by the \code{factors} and the order
of sectors are determined by the levels of factors. In this function,
the start and end position for each sector on the circle (measured by degree)
are calculated according to the values on x-axis or by \code{xlim}.

If \code{x} is set, the length of \code{x} must be equal to the length of \code{factors}.
Then the data range for each sector are calculated from \code{x} by splitting \code{factors}.

If \code{xlim} is set, it should be a vector containing two numbers or a matrix with 2 columns.
If \code{xlim} is a 2-element vector, it means all sector share the same \code{xlim}.
If \code{xlim} is a 2-column matrix, the number of rows should be equal to the number of categories
identified by \code{factors}, then each row of \code{xlim} corresponds to the data range for each sector
and the order of rows is corresponding to the order of levels of \code{factors}. If \code{xlim} is a matrix
for which row names cover all sector names, \code{xlim} is automatically adjusted.

Normally, width of sectors will be calculated internally according to the data range in sectors. But you can
still set the width manually. However, it is not always a good idea to change the default sector width since
the width can reflect the range of data in sectors. However, in some cases, it is useful to manually set
the width such as you want to zoom some part of the sectors.

The function finally calls \code{\link[graphics:plot.default]{plot}} with enforing aspect ratio to be 1 and be ready for adding graphics.
}
\seealso{
\url{https://jokergoo.github.io/circlize_book/book/circular-layout.html}
}
\examples{
# There is no example
NULL

}
