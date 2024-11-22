\name{becat_cadastre}
\alias{becat_cadastre}
\docType{data}
\title{
Revisió dels topònims dels cadastres dels municipis de Catalunya Nord feta per Joan Becat
}
\description{
Inclou els topònims anteriors i els corregits així com el topònim que apareix al GEOPORTAIL de l'IGN.
}
\usage{becat_cadastre}
\format{
  El format és una llista de 235 elements, corresponents a la revisió del cadastre de cada municipi. Per cada municipi
  hi ha un «data frame» amb tantes observacions com topònims i 5 variables:
  \describe{
    \item{\code{TOPÒNIMS I FULLS CADASTRALS}}{Referència del full cadastral del topònim.}
    \item{\code{NOM*}}{Aquesta variable no és igual en tots els municipis i pren 3 formes diferents:
      \code{NOM DEL CADASTRE ANTERIOR I SOBRE EL CADASTRE ACTUAL}, \code{NOM SOBRE EL CADASTRE ACTUAL} o
      \code{NOM SOBRE EL CADASTRE ANTERIOR}.}
    \item{\code{NOM*}}{Aquesta variable no és igual en tots els municipis i pren 3 formes diferents:
      \code{NOM A CORREGIR SOBRE EL CADASTRE}, \code{NOM CORREGIT SOBRE EL CADASTRE ACTUAL},
      \code{NOM PROPOSAT PEL BATLLE I NO CORREGIT PER LA DGI} o \code{NOM SOBRE EL CADASTRE ACTUAL CORREGIT}.}
    \item{\code{NOM SOBRE GÉOPORTAIL (portal cartogràfic de l’IGN)}}{Topònim que apareix a
      \url{https://www.geoportail.gouv.fr}.}
    \item{\code{tipus}}{Tipus de l'element associat al topònim.}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Joan Becat \url{https://joanbecat.cat}. Extracció de les dades a \file{data-raw/becat_cadastre.R}.
}
\examples{
str(becat_cadastre[1])

# Diversitat de capçaleres
unique(lapply(becat_cadastre, names))

# Municipis inclosos
names(becat_cadastre)
}
\seealso{
Altres dades relacionades:
\code{\link{meta_cadastre}},
\code{\link{intro_cadastre}},
\code{\link{fitxers_cadastre}},
\code{\link{comarques}}
}
\keyword{datasets}
