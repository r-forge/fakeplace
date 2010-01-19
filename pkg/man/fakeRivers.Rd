\name{fakeRivers}
\alias{fakeRivers}

\title{
Generate fake river networks.
}
\description{
Produce some wiggly lines that form a tree structure.
}
\usage{
fakeRivers(n = 30, pts = cbind(runif(n), runif(n)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
If this is the only argument, start with n points in a unit square.
}
  \item{pts}{
The points to use for initial seeds.
}
}
\details{
  Use the initial points to create Voronoi tiles. Wiggle the edges. Then
use the distance between nodes as edge weight to create a minimum
spanning tree. Return the edges of the tree.
}
\value{
A list of 2-column matrix line segments.
}
\author{
Barry Rowlingson
}
\examples{
rivers = fakeRivers()
\dontrun{plot(c(0,1),c(0,1),type='n')}
\dontrun{lapply(rivers,lines)}
  }

\keyword{datagen}


