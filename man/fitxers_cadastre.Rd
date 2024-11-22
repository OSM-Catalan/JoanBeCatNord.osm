\name{fitxers_cadastre}
\alias{fitxers_cadastre}
\docType{data}
\title{
Descripció dels fitxers pdf originals
}
\description{
Informació extreta de l'apartat de la web amb revisions dels topònims dels cadastres dels municipis de Catalunya Nord.
}
\usage{fitxers_cadastre}
\format{
  Un «data frame» amb 224 observacions de les següents 4 variables.
  \describe{
    \item{\code{municipi}}{Nom del municipi.}
    \item{\code{url}}{URL de la pàgina del municipi.}
    \item{\code{url_pdf}}{URL del fitxer en format pdf de la revisió de topònims del municipi.}
    \item{\code{md5sum}}{Valor dels resums del fitxer pdf. Útil per detectar actualitzacions dels fitxers.}
  }
}
\details{
L'enllaç del pdf de Vilafranca de Conflent apunta a la url del poble i no hi ha pdf disponible.
La url del pdf de la web de Corbera la Cabanal és incorrecte (li falta la `f` final).
No hi ha disponibles en català els pdf de Calce, Espirà de l’Aglí i Jújols i s'usa la versió en francés.
}
\source{
Joan Becat \url{https://joanbecat.cat}. Extracció de les dades a \file{data-raw/fitxers_cadastre.R}.
}
\examples{
str(fitxers_cadastre)
}
\seealso{
Altres dades relacionades:
\code{\link{becat_cadastre}},
\code{\link{meta_cadastre}},
\code{\link{intro_cadastre}},
\code{\link{comarques}}
}
\keyword{datasets}
