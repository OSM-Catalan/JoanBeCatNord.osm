\name{intro_cadastre}
\alias{intro_cadastre}
\docType{data}
\title{
Introduccions de les revisions dels topònims dels cadastres dels municipis de Catalunya Nord feta per Joan Becat
}
\description{
Contingut de la introducció dels fitxers en prosa.
}
\usage{intro_cadastre}
\format{
  El format és una llista de 223 elements, corresponents a la revisió del cadastre de cada municipi. Per cada municipi
  hi ha un vector de caràcters amb el contingut de la introducció del fitxers.
}
\details{
Les introduccions contenen informació sobre l'estat del cadastre i de GEOPORTAIL (revisat o no), data en què es va fer
la revisió, participants en la revisió i la referència a l'«Atles toponímic de Catalunya Nord - Atlas toponymique de
Catalogne Nord, ed. Terra Nostra».
}
\source{
Joan Becat \url{https://joanbecat.cat}. Extracció de les dades a \file{data-raw/becat_cadastre.R}.
}
\examples{
str(intro_cadastre)
}
\seealso{
Altres dades relacionades:
\code{\link{becat_cadastre}},
\code{\link{meta_cadastre}},
\code{\link{fitxers_cadastre}},
\code{\link{comarques}}
}
\concept{dades}
\keyword{datasets}
