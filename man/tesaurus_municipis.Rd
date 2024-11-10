\name{tesaurus_municipis}
\alias{tesaurus_municipis}
\docType{data}
\title{
Tesaurus de municipis de Catalunya Nord
}
\description{
Correspondències entre les comarques de les revisions de topònims dels cadastres feta per Joan Becat i OpenstreetMap.
}
\usage{tesaurus_municipis}
\format{
  Un «data frame» amb 223 observacions de les següents 6 variables.
  \describe{
    \item{\code{becat_nom}}{Nom del municipi segons la revisió de topònims de Joan Becat.}
    \item{\code{becat_comarca}}{Nom de la comarca segons la revisió de topònims de Joan Becat.}
    \item{\code{osm_name}}{Nom del municipi a OpenstreetMap.}
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
}
\keyword{datasets}
