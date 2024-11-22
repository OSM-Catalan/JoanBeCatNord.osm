load("data/comarques.rda", verbose = TRUE) # comarques

municipis_becat <- comarques[, c("municipi", "comarca")]
names(municipis_becat) <- c("becat_nom", "becat_comarca")

municipis_osm <- monitorOSM::municipis[monitorOSM::municipis$regio == "CatNord", ]
municipis_osm <- municipis_osm[, c("name:ca", "comarca", "osm_type", "osm_id")]
names(municipis_osm) <- gsub("^name:ca$", "osm_name", names(municipis_osm))
names(municipis_osm) <- gsub("^comarca$", "osm_comarca", names(municipis_osm))


tesaurus_municipis <- cbind(
  municipis_becat,
  matrix(NA, nrow = nrow(municipis_becat), ncol = ncol(municipis_osm), dimnames = list(NULL, c(names(municipis_osm))))
)
tesaurus_municipis

sel_mun_osm <- sapply(tesaurus_municipis$becat_nom, function(x) {
  sel <- grep(paste0("^", x, "$"), municipis_osm$osm_name)
  if (length(sel) == 0) {
    sel <- grep(x, municipis_osm$osm_name)
  }
  if (length(sel) == 0) {
    sel <- agrep(paste0("^", x, "$"), municipis_osm$osm_name)
  }
  if (length(sel) == 0) {
    sel <- agrep(x, municipis_osm$osm_name)
  }
  if (length(sel) == 0) {
    sel <- NA
  }

  if (length(sel) > 1) {
    warning(x, " amb >1 candidat: ", paste(paste(sel, municipis_osm$osm_name[sel]), collapse = ", "))
  }
  sel
})

## Selecció manual
sel_mun_osm$Baó <- 93
sel_mun_osm$Castell <- 35
sel_mun_osm$Corbera <- 109
sel_mun_osm$`l'Albera` <- 185
sel_mun_osm$Pi <- 66
sel_mun_osm$`Vilanova de Raó` <- 176
sel_mun_osm$Viran <- NA

table(sapply(sel_mun_osm, length))
sel_mun_osm <- unlist(sel_mun_osm)

tesaurus_municipis[, names(municipis_osm)] <- municipis_osm[sel_mun_osm, ]
tesaurus_municipis[is.na(tesaurus_municipis$osm_name), ]

sel_mun_osm_pendents <- sapply(tesaurus_municipis$becat_nom[is.na(tesaurus_municipis$osm_name)], function(x) {
  sel <- grep(paste0("^", x, "$"), municipis_osm$osm_name)
  if (length(sel) == 0) {
    sel <- grep(x, municipis_osm$osm_name)
  }
  if (length(sel) == 0) {
    sel <- agrep(paste0("^", x, "$"), municipis_osm$osm_name)
  }
  if (length(sel) == 0) {
    sel <- agrep(x, municipis_osm$osm_name)
  }

  parts <- strsplit(x, " ")[[1]]
  regex_parts <- paste(parts[nchar(parts) > 3], collapse = "|")
  if (length(sel) == 0) {
    sel <- grep(regex_parts, municipis_osm$osm_name)
  }
  if (length(sel) == 0) {
    sel <- agrep(regex_parts, municipis_osm$osm_name)
  }
  if (length(sel) == 0) {
    sel <- NA
  }
  if (length(sel) > 1) {
    warning(x, " amb >1 candidat: ", paste(paste(sel, municipis_osm$osm_name[sel]), collapse = ", "))
  }
  sel
})

## Selecció manual
sel_mun_osm_pendents$`Argelers de la Marenda` <- 88
sel_mun_osm_pendents$`Corbera la Cabana` <- 121
sel_mun_osm_pendents$`Eus i Coma` <- grep("Eus", municipis_osm$osm_name)
sel_mun_osm_pendents$`la Torre del Bisbe` <- 123
sel_mun_osm_pendents$`Pesilhan de Conflent` <- NA
sel_mun_osm_pendents$`Sant Joan Pla de Corts` <- 193
sel_mun_osm_pendents$`Sant Martin` <- NA
sel_mun_osm_pendents$`Sant Pau de Fenolhet` <- NA
sel_mun_osm_pendents$Viran <- NA

table(sapply(sel_mun_osm_pendents, length))
sel_mun_osm_pendents <- unlist(sel_mun_osm_pendents)

tesaurus_municipis[match(names(sel_mun_osm_pendents), tesaurus_municipis$becat_nom), names(municipis_osm)] <-
  municipis_osm[sel_mun_osm_pendents, ]

tesaurus_municipis[tesaurus_municipis$becat_nom == "Sansà", names(municipis_osm)] <-
  municipis_osm[municipis_osm$osm_name == "Censà", ]

tesaurus_municipis[tesaurus_municipis$becat_comarca == "Fenolhedés", ]
tesaurus_municipis[tesaurus_municipis$becat_comarca == "Fenolhedés", names(municipis_osm)] <- NA

## Per REPASSAR:
tesaurus_municipis[
  which(tesaurus_municipis$osm_name != tesaurus_municipis$becat_nom),
  c("osm_name", "becat_nom", "osm_comarca", "becat_comarca")
]
# CONCLUSIONS: corregir Ralleu / Ral -> Real / Ral

tesaurus_municipis[tesaurus_municipis$becat_nom == "Ral", names(municipis_osm)] <-
  municipis_osm[municipis_osm$osm_name == "Real", ]


tesaurus_municipis[is.na(tesaurus_municipis$osm_name), ] ## sense municipi a OSM o Fenolleda
candidats_osm <- lapply(tesaurus_municipis$becat_nom[is.na(tesaurus_municipis$osm_name)], function(x) {
  osmdata::getbb(paste0(x, ", Pirineus Orientals"), format_out = "data.frame")
})
names(candidats_osm) <- tesaurus_municipis$becat_nom[is.na(tesaurus_municipis$osm_name)]
candidats_osm_sel <- do.call(rbind, lapply(candidats_osm, function(x) {
  x[1, c("name", "osm_type", "osm_id", "addresstype", "class", "type")]
}))
colnames(candidats_osm_sel)[1] <- "osm_name"

municipis_pendents <- data.frame(
  osm_name = candidats_osm_sel[, "osm_name", drop = FALSE],
  comarca = rep(NA, nrow(candidats_osm_sel)),
  candidats_osm_sel[, c("osm_type", "osm_id")]
)

municipis_pendents_osm <- osmdata::opq_osm_id(
  id = municipis_pendents$osm_id, type = municipis_pendents$osm_type, out = "tags"
) |> osmdata::osmdata_data_frame()

municipis_pendents_osm[, grep("^name", names(municipis_pendents_osm))]

tesaurus_municipis[is.na(tesaurus_municipis$osm_name), names(municipis_osm)] <- municipis_pendents
tesaurus_municipis[is.na(tesaurus_municipis$osm_comarca), ]
tesaurus_municipis$osm_comarca[tesaurus_municipis$becat_comarca == "Fenolhedés"] <- "Fenolledès"
municipis_osm[!municipis_osm$osm_name %in% tesaurus_municipis$osm_name, ]
## TODO: REPORT sense dades de Becat per Sallagosa, Vilafranca de Conflent i Centernac (forat al mapa del Fenolledès)


## Duplicats?
apply(tesaurus_municipis[, c("becat_nom", "osm_name", "osm_id")], 2, function(x) any(duplicated(na.omit(x))))


## Comprova comarques ----
unique(tesaurus_municipis[, c("osm_comarca", "becat_comarca")])
## CONCLUSIONS: hi ha discrepancies

library(osmdata)
library(sf)

municipis_sf <- opq_osm_id(id = na.omit(tesaurus_municipis$osm_id), type = na.omit(tesaurus_municipis$osm_type)) |>
  osmdata_sf()

municipis_sf_becat <- st_as_sf(merge(tesaurus_municipis, municipis_sf$osm_multipolygons, by = "osm_id"))

mapa <- municipis_sf_becat[, c("becat_nom", "osm_name", "osm_comarca", "becat_comarca")]
comarques_becat <- mapview::mapview(mapa, zcol = "becat_comarca")
mapview::mapshot2(comarques_becat, url = "inst/comarques_becat.html")
comarques_osm <- mapview::mapview(mapa, zcol = "osm_comarca")
mapview::mapshot2(comarques_osm, url = "inst/comarques_osm.html")
## CONCLUSIONS: algunes discrepàncies

## Comprova municipis ----

tesaurus_municipis[
  tesaurus_municipis$becat_nom != tesaurus_municipis$osm_name,
  c("becat_nom", "osm_name", "becat_comarca", "osm_comarca")
]
discrepancies <- tesaurus_municipis[
  tesaurus_municipis$becat_nom != tesaurus_municipis$osm_name &
    tesaurus_municipis$osm_comarca != "Fenolledès",
  c("becat_nom", "osm_name", "becat_comarca", "osm_comarca")
]

openxlsx::write.xlsx(
  discrepancies,
  file = "data-raw/discrepàncies-municipis_osm-becat.xlsx", rowNames = FALSE, borders = "surrounding",
  colWidths = "auto", firstRow = TRUE, headerStyle = openxlsx::createStyle(textDecoration = "BOLD")
)


## Desa ----

usethis::use_data(tesaurus_municipis, overwrite = TRUE)

openxlsx::write.xlsx(
  tesaurus_municipis,
  file = "data-raw/tesaurus_municipis.xlsx", rowNames = FALSE, borders = "surrounding",
  colWidths = "auto", firstRow = TRUE, headerStyle = openxlsx::createStyle(textDecoration = "BOLD")
)

load("data/tesaurus_municipis.rda", verbose = TRUE) # tesaurus_municipis
