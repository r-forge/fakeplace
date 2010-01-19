\name{recurSplit}
\alias{recurSplit}

\title{
Recursively split a line segment.
}
\description{
Create a random fractal line by recursive splitting of segments.
}
\usage{
recurSplit(x0, y0, x1, y1, n, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x0}{
x coordinate of start point
}
  \item{y0}{
y coordinate of start point
}
  \item{x1}{
x coordinate of finish point
}
  \item{y1}{
y coordinate of finish point
}
  \item{n}{
number of recursive divisions
}
  \item{\dots}{
arguments to be passed to the underlying splitting algorithm - currently
not used.
}
}
\details{
  Call slpitSegMid to get a midpoint and generate two segments.
  Then call splitSegMid again on those. Repeat n times.
}
\value{
A two-column matrix of coordinates. The first and last will be the input
coordinates.

}

\author{
Barry Rowlingson
}
\examples{
recurSplit(0,0,1,1,3)
}

\keyword{ datagen }
