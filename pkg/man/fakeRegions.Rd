\name{fakeRegions}
\alias{fakeRegions}
\title{
Create fake geographic regions.
}
\description{
  Using an initial set of points, create some wiggly boundaries that
  look a bit like natural regional boundaries.
}
\usage{
fakeRegions(n = 30, pts = cbind(runif(n), runif(n)), boundary = pts[chull(pts), ], clipmode = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
The number of initial points. If only this argument is given, then n
points on a unit square are the initial points.
}
  \item{pts}{
The initial seed points.
}
  \item{boundary}{
A boundary for clipping or cutting as a gpc.poly object.
}
  \item{clipmode}{
How to clip the polygons to the bounday. See details.
}
}
\details{
The process is as follows. Create Voronoi tiles using the points as
seeds. Take all the finite edges and wiggle them. Create polygons from
those wiggly edges. Clip the polygons to the boundary polygon object.

If clipmode is 0, no clipping is done and all polygons are returned.

If clipmode = 1, any polygons that are partly within the boundary are
returned.

If clipmode = 2, polygons are clipped to the boundary polygon.

If clipmode = 3, only polygons that are fully within the boundary are
returned.

}
\value{
  A list with the following components:

\item{polys}{A list of GPC Polygon objects for the regions.}
\item{bounds}{The clipping polygon used}
\item{points}{The seed points used}
\item{box}{The bounding box of the polygons}

}
\author{
Barry Rowlingson
}

\examples{
places = fakeRegions(clipmode=2)
\dontrun{ plot(places$box,type="n")}

\dontrun{
lapply(places$polys,function(x){plot(x,add=TRUE,poly.args=list(border="blue"))})
}
}

\keyword{datagen}


