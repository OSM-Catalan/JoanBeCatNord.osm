\name{tesaurus_municipis}
\alias{tesaurus_municipis}
\docType{data}
\title{
Tesaurus de municipis de Catalunya Nord
}
\description{
Correspondències entre els municipis de les revisions de topònims dels cadastres feta per Joan Becat i OpenStreetMap.
}
\usage{tesaurus_municipis}
\format{
  Un «data frame» amb 223 observacions de les següents 7 variables.
  \describe{
    \item{\code{becat_nom}}{Nom del municipi segons la revisió de topònims de Joan Becat.}
    \item{\code{becat_comarca}}{Nom de la comarca segons la revisió de topònims de Joan Becat.}
    \item{\code{osm_name:ca}}{Nom del municipi en català a OpenStreetMap.}
    \item{\code{osm_name}}{Nom del municipi a OpenStreetMap.}
    \item{\code{osm_comarca}}{a character vector}
    \item{\code{osm_type}}{Tipus de l'objecte d'OSM que representa el municipi.}
    \item{\code{osm_id}}{Identificador de l'objecte d'OSM que representa el municipi.}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
\file{data-raw/tesaurus_municipis.R}
}
\examples{
str(tesaurus_municipis)

discrepancies <- tesaurus_municipis[
  tesaurus_municipis$becat_nom != tesaurus_municipis$`osm_name:ca` &
    tesaurus_municipis$osm_comarca != "Fenolledès",
  c("becat_nom", "osm_name:ca", "osm_name", "osm_comarca")
]
discrepancies
}
\seealso{
Altres dades relacionades:
\code{\link{tesaurus_comarques}}
}
\keyword{datasets}
