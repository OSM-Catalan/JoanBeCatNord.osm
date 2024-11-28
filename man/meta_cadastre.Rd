\name{meta_cadastre}
\alias{meta_cadastre}
\docType{data}
\title{
Metadades de les revisions dels cadastres
}
\description{
Informació sistematitzada extreta de \code{\link{intro_cadastre}}.
}
\usage{meta_cadastre}
\format{
  Un «data frame» amb 223 observacions de les següents 7 variables.
  \describe{
    \item{\code{municipi}}{Nom del municipi.}
    \item{\code{corregit}}{Vector logic que indica si el cadastre ha estat corregit.}
    \item{\code{atles_CatNord}}{Vector logic que indica si els topònims apareixen a l’Atles toponímic de Catalunya Nord, ed. Terra Nostra, Prada, 2015, 2 volums, 970p.}
    \item{\code{ign_geoportail}}{Vector logic que indica si els topònims han estat corregits al GEOPORTAIL de l'IGN.}
    \item{\code{ign_mapes}}{Vector de tipus caràcter indicant si l'IGN a corregit els maps o no, o si només parcialment.}
    \item{\code{catala_pdf}}{Vector logic que indica si el fitxer pdf és en català (\code{TRUE}) o en francès (\code{FALSE}).}
    \item{\code{dues_taules}}{Vector logic que indica si el municipi està dividit en dues taules.}
  }
}
\details{
Els municipis amb \code{dues_taules == TRUE}, tenen dues taules a la llista \code{becat_cadastre} amb el nom del
municipi seguit de \code{_CADASTRE}, \code{_PROPOSAT}, \code{_CORREGIT} o \code{_NO CORREGIT}.
}
\source{
Joan Becat \url{https://joanbecat.cat}. Extracció de les dades a \file{data-raw/metadades_becat_cadastre.R}.
}
\examples{
str(meta_cadastre)
}
\seealso{
Altres dades relacionades:
\code{\link{intro_cadastre}},
\code{\link{becat_cadastre}},
\code{\link{fitxers_cadastre}},
\code{\link{comarques}}
}
\keyword{datasets}
