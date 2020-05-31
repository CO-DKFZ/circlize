\name{circos.par}
\alias{circos.par}
\title{
Parameters for the circular layout
}
\description{
Parameters for the circular layout
}
\usage{
circos.par(..., RESET = FALSE, READ.ONLY = NULL, LOCAL = FALSE, ADD = FALSE)
}
\arguments{

  \item{...}{Arguments for the parameters, see "details" section
  \item{RESET}{reset to default values
  \item{READ.ONLY}{please ignore
  \item{LOCAL}{please ignore
  \item{ADD}{please ignore

}
\details{
Global parameters for the circular layout. Currently supported parameters are:

\describe{
  \item{\code{start.degree}}{The starting degree from which the circle begins to draw. Note this degree is measured
  \item{\code{gap.degree}}{Gap between two neighbour sectors. It can be a single value or a vector. If it is a vector,
  \item{\code{gap.after}}{identical to \code{gap.degree} option, but a more understandable name. Modifying this option will also affect \code{gap.degree}.
  \item{\code{track.margin}}{Like \code{margin} in Cascading Style Sheets (CSS), it is the blank area
  \item{\code{unit.circle.segments}}{Since curves are simulated by a series of straight lines,
  \item{\code{cell.padding}}{Padding of the cell. Like \code{padding} in Cascading Style Sheets
  \item{\code{track.height}}{The default height of tracks. It is the percentage according to the radius
  \item{\code{points.overflow.warning}}{Since each cell is in fact not a real plotting region but only
  \item{\code{canvas.xlim}}{The coordinate for the canvas. Because \code{circlize} draws everything (or almost everything) inside the unit circle,
  \item{\code{canvas.ylim}}{The coordinate for the canvas. By default it is \code{c(-1, 1)}
  \item{\code{clock.wise}}{The direction for adding sectors. Default is \code{TRUE}.
}

Similar as \code{\link[graphics]{par}}, you can get the parameter values by specifying the
names of parameters and you can set the parameter values by specifying a
named list which contains the new values.

\code{gap.degree}, \code{start.degree}, \code{canvas.xlim}, \code{canvas.ylim} and \code{clock.wise}
only be set before the initialization of the circular layout
(i.e. before calling \code{\link{circos.initialize}}) because these values will not be changed after
adding sectors on the circle. The left and right padding for \code{cell.padding} will also be
ignored after the initialization because all cells in a sector would share the same
left and right paddings.
}
\examples{
# There is no example
NULL

}