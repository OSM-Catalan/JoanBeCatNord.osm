\name{tesaurus_comarques}
\alias{tesaurus_comarques}
\docType{data}
\title{
Tesaurus de comarques de Catalunya Nord
}
\description{
Correspondències entre els municipis de les revisions de topònims dels cadastres feta per Joan Becat i OpenStreetMap.
}
\usage{tesaurus_comarques}
\format{
  Un «data frame» amb 8 observacions de les següents 4 variables.
  \describe{
    \item{\code{becat_nom}}{Nom de la comarca segons la revisió de topònims de Joan Becat.}
    \item{\code{osm_name}}{Nom de la comarca a OpenStreetMap.}
    \item{\code{osm_type}}{Tipus de l'objecte d'OSM que representa la comarca.}
    \item{\code{osm_id}}{Identificador de l'objecte d'OSM que representa la comarca.}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
\file{data-raw/tesaurus_comarques.R}
}
\examples{
str(tesaurus_comarques)
}
\seealso{
Altres dades relacionades:
\code{\link{tesaurus_municipis}},
}
\concept{dades}
\keyword{datasets}
