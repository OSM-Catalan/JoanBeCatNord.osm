\name{comarques}
\alias{comarques}
\docType{data}
\title{
Comarques dels municipis de Catalunya Nord
}
\description{
Noms dels municipis i comarques extrets dels fitxers del cadastre
}
\usage{comarques}
\format{
  Un «data frame» amb 223 observacions de les següents 5 variables.
  \describe{
    \item{\code{municipi}}{a character vector}
    \item{\code{comarca}}{a character vector}
    \item{\code{municipi_pdf}}{a character vector}
    \item{\code{comarca_pdf}}{a character vector}
    \item{\code{nom_fitxer}}{a character vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Joan Becat \url{https://joanbecat.cat}. Extracció de les dades a \file{data-raw/becat_cadastre.R}.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
str(comarques)
}
\seealso{
Altres dades relacionades:
\code{\link{becat_cadastre}},
\code{\link{meta_cadastre}},
\code{\link{intro_cadastre}},
\code{\link{fitxers_cadastre}}
}
\keyword{datasets}
