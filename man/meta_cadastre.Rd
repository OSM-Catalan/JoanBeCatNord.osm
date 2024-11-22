\name{meta_cadastre}
\alias{meta_cadastre}
\docType{data}
\title{
Metadades de les revisions dels cadastres
}
\description{
Informació sistematitzada extreta de \code{\link{intro_cadastre}}.
}
\usage{data("meta_cadastre")}
\format{
  A data frame with 223 observations on the following 6 variables.
  \describe{
    \item{\code{municipi}}{Nom del municipi.}
    \item{\code{corregit}}{Vector logic que indica si el cadastre ha estat corregit.}
    \item{\code{atles_CatNord}}{Vector logic que indica si els topònims apareixen a l’Atles toponímic de Catalunya Nord, ed. Terra Nostra, Prada, 2015, 2 volums, 970p.}
    \item{\code{ign_geoportail}}{Vector logic que indica si els topònims han estat corregits al GEOPORTAIL de l'IGN.}
    \item{\code{ign_mapes}}{a character vector}
    \item{\code{catala_pdf}}{Vector logic que indica si el fitxer pdf és en català (\code{TRUE}) o en francès (\code{FALSE}).}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Joan Becat \url{https://joanbecat.cat}. Extracció de les dades a \file{data-raw/metadades_becat_cadastre.R}.
}
\examples{
data(meta_cadastre)
## maybe str(meta_cadastre) ; plot(meta_cadastre) ...
}
\keyword{datasets}
