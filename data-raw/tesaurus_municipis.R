load("data/comarques.rda", verbose = TRUE) # comarques

municipis_becat <- comarques[, c("municipi", "comarca")]
names(municipis_becat) <- c("becat_nom", "becat_comarca")

municipis_osm <- monitorOSM::municipis[monitorOSM::municipis$regio == "CatNord", ]
municipis_osm <- municipis_osm[, c("name:ca", "name", "comarca", "osm_type", "osm_id")]
names(municipis_osm) <- gsub("^name$", "osm_name", names(municipis_osm))
names(municipis_osm) <- gsub("^name:ca$", "osm_name:ca", names(municipis_osm))
names(municipis_osm) <- gsub("^comarca$", "osm_comarca", names(municipis_osm))


tesaurus_municipis <- cbind(
  municipis_becat,
  matrix(NA, nrow = nrow(municipis_becat), ncol = ncol(municipis_osm), dimnames = list(NULL, c(names(municipis_osm))))
)
tesaurus_municipis

sel_mun_osm <- sapply(tesaurus_municipis$becat_nom, function(x) {
  sel <- grep(paste0("^", x, "$"), municipis_osm$`osm_name:ca`)
  if (length(sel) == 0) {
    sel <- grep(x, municipis_osm$`osm_name:ca`)
  }
  if (length(sel) == 0) {
    sel <- agrep(paste0("^", x, "$"), municipis_osm$`osm_name:ca`)
  }
  if (length(sel) == 0) {
    sel <- agrep(x, municipis_osm$`osm_name:ca`)
  }
  if (length(sel) == 0) {
    sel <- grep(paste0("^", x, "$"), municipis_osm$osm_name)
  }
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
    warning(x, " amb >1 candidat: ", paste(paste(sel, municipis_osm$`osm_name:ca`[sel]), collapse = ", "))
  }
  sel
})

## Selecció manual
sel_mun_osm$Baó <- 119
sel_mun_osm$Castell <- 33
sel_mun_osm$Corbera <- 135
sel_mun_osm$Fenolhet <- 94
sel_mun_osm$`l'Albera` <- 147
sel_mun_osm$Pi <- 65
sel_mun_osm$`Vilanova de Raó` <- 205
sel_mun_osm$Viran <- NA

table(sapply(sel_mun_osm, length))
sel_mun_osm <- unlist(sel_mun_osm)

tesaurus_municipis[, names(municipis_osm)] <- municipis_osm[sel_mun_osm, ]
tesaurus_municipis[is.na(tesaurus_municipis$`osm_name:ca`), ]


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
sel_mun_osm_pendents$`Eus i Coma` <- grep("Eus", municipis_osm$osm_name)
sel_mun_osm_pendents$`Pesilhan de Conflent` <- 101


table(sapply(sel_mun_osm_pendents, length))
sel_mun_osm_pendents <- unlist(sel_mun_osm_pendents)

tesaurus_municipis[match(names(sel_mun_osm_pendents), tesaurus_municipis$becat_nom), names(municipis_osm)] <-
  municipis_osm[sel_mun_osm_pendents, ]


## Omple manualment
tesaurus_municipis[is.na(tesaurus_municipis$`osm_name:ca`), ]
municipis_osm[!municipis_osm$osm_id %in% tesaurus_municipis$osm_id, ]

tesaurus_municipis[tesaurus_municipis$becat_nom == "Ansinhan", names(municipis_osm)] <-
  municipis_osm[municipis_osm$`osm_name:ca` %in% "Ansinyà", ]
tesaurus_municipis[tesaurus_municipis$becat_nom == "Argelers de la Marenda", names(municipis_osm)] <-
  municipis_osm[municipis_osm$`osm_name:ca` %in% "Argelers", ]
tesaurus_municipis[tesaurus_municipis$becat_nom == "Camporsin", names(municipis_osm)] <-
  municipis_osm[municipis_osm$`osm_name:ca` %in% "Campossí", ]
tesaurus_municipis[tesaurus_municipis$becat_nom == "Cassanhas", names(municipis_osm)] <-
  municipis_osm[municipis_osm$`osm_name:ca` %in% "Cassanyes", ]
tesaurus_municipis[tesaurus_municipis$becat_nom == "la Roca de l'Albera", names(municipis_osm)] <-
  municipis_osm[municipis_osm$`osm_name:ca` %in% "la Roca d'Albera", ]
tesaurus_municipis[tesaurus_municipis$becat_nom == "Pontellà i Nyils", names(municipis_osm)] <-
  municipis_osm[municipis_osm$`osm_name:ca` %in% "Pontellà", ]
tesaurus_municipis[tesaurus_municipis$becat_nom == "Prunhanas", names(municipis_osm)] <-
  municipis_osm[municipis_osm$`osm_name:ca` %in% "Prunyanes", ]
tesaurus_municipis[tesaurus_municipis$becat_nom == "Sansà", names(municipis_osm)] <-
  municipis_osm[municipis_osm$`osm_name:ca` %in% "Censà", ]
tesaurus_municipis[tesaurus_municipis$becat_nom == "Saorra i Toren", names(municipis_osm)] <-
  municipis_osm[municipis_osm$`osm_name:ca` %in% "Saorra", ]
tesaurus_municipis[tesaurus_municipis$becat_nom == "Sautó i Fetges", names(municipis_osm)] <-
  municipis_osm[municipis_osm$`osm_name:ca` %in% "Sautó", ]
tesaurus_municipis[tesaurus_municipis$becat_nom == "Soanyes i Marians", names(municipis_osm)] <-
  municipis_osm[municipis_osm$`osm_name:ca` %in% "Soanyes", ]
tesaurus_municipis[tesaurus_municipis$becat_nom == "Sornian", names(municipis_osm)] <-
  municipis_osm[municipis_osm$`osm_name:ca` %in% "Sornià", ]
tesaurus_municipis[tesaurus_municipis$becat_nom == "Trilhan", names(municipis_osm)] <-
  municipis_osm[municipis_osm$`osm_name:ca` %in% "Trillà", ]



tesaurus_municipis[tesaurus_municipis$becat_comarca == "Fenolhedés", ]



## Per REPASSAR:
tesaurus_municipis[
  which(tesaurus_municipis$`osm_name:ca` != tesaurus_municipis$becat_nom),
  c("osm_name:ca", "becat_nom", "osm_name", "osm_comarca", "becat_comarca")
]
# CONCLUSIONS: corregir Ralleu / Ral -> Real / Ral; la Cabanassa / Corbera la Cabana -> ??;
# Torrelles de la Salanca / la Torre del Bisbe -> ??; Taurinyà / Maurin -> ??; Brullà / Viran -> ??;
municipis_osm[!municipis_osm$osm_id %in% tesaurus_municipis$osm_id, ]
dbTools::duplicatedPK(tesaurus_municipis, pk = "osm_id")

tesaurus_municipis[tesaurus_municipis$becat_nom == "Ral", names(municipis_osm)] <-
  municipis_osm[municipis_osm$`osm_name:ca` == "Real", ]

tesaurus_municipis[tesaurus_municipis$becat_nom == "Corbera la Cabana", names(municipis_osm)] <-
  municipis_osm[municipis_osm$`osm_name:ca` == "la Cabana de Corbera", ]
tesaurus_municipis[tesaurus_municipis$becat_nom == "Maurin", names(municipis_osm)] <-
  municipis_osm[municipis_osm$`osm_name:ca` == "Maurí", ]
tesaurus_municipis[tesaurus_municipis$becat_nom == "Viran", names(municipis_osm)] <-
  municipis_osm[municipis_osm$`osm_name:ca` == "Virà", ]
tesaurus_municipis[tesaurus_municipis$becat_nom == "la Torre del Bisbe", names(municipis_osm)] <-
  municipis_osm[municipis_osm$`osm_name:ca` == "la Torre d'Elna", ]

tesaurus_municipis[is.na(tesaurus_municipis$`osm_name:ca`), ] ## sense municipi

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
# tesaurus_municipis$osm_comarca[tesaurus_municipis$becat_comarca == "Fenolhedés"] <- "Fenolledès"
municipis_osm[!municipis_osm$osm_name %in% tesaurus_municipis$osm_name, ]
## TODO: #2 sense dades de Becat per Sallagosa, Vilafranca de Conflent i Centernac (forat al mapa del Fenolledès)


## Duplicats?
apply(tesaurus_municipis[, c("becat_nom", "osm_name", "osm_id")], 2, function(x) any(duplicated(na.omit(x))))


## Comprova comarques ----

unique(tesaurus_municipis[, c("osm_comarca", "becat_comarca")])
dif_comarques <- tesaurus_municipis[tesaurus_municipis$becat_comarca != tesaurus_municipis$osm_comarca, ]
dif_comarques$osm_comarca <- gsub("^Fenolledès$", "Fenolhedés", dif_comarques$osm_comarca)
dif_comarques$osm_comarca <- gsub("^Alta Cerdanya$", "Cerdanya", dif_comarques$osm_comarca)
dif_comarques <- dif_comarques[dif_comarques$becat_comarca != dif_comarques$osm_comarca, ]
dif_comarques[, c("becat_nom", "becat_comarca", "osm_comarca")]
## CONCLUSIONS: discrepàncies resoltes però el municipi de Prunet i Bellpuig s'hauria de dividir entre Rosselló (Prunet)
# i Vallespir (Bellpuig)

library(osmdata)
library(sf)

municipis_sf <- opq_osm_id(id = na.omit(tesaurus_municipis$osm_id), type = na.omit(tesaurus_municipis$osm_type)) |>
  osmdata_sf()

municipis_sf_becat <- st_as_sf(merge(tesaurus_municipis, municipis_sf$osm_multipolygons, by = "osm_id"))

mapa <- municipis_sf_becat[, c("becat_nom", "osm_name", "osm_comarca", "becat_comarca")]
comarques_becat <- mapview::mapview(mapa, zcol = "becat_comarca", map.types = "OpenStreetMap.CAT")
mapview::mapshot2(comarques_becat, url = "inst/comarques_becat.html")
comarques_osm <- mapview::mapview(mapa, zcol = "osm_comarca", map.types = "OpenStreetMap.CAT")
mapview::mapshot2(comarques_osm, url = "inst/comarques_osm.html")
## CONCLUSIONS: algunes discrepàncies


## Comprova municipis ----

tesaurus_municipis[
  tesaurus_municipis$becat_nom != tesaurus_municipis$`osm_name:ca`,
  c("becat_nom", "osm_name:ca", "osm_name", "becat_comarca", "osm_comarca")
]
discrepancies <- tesaurus_municipis[
  tesaurus_municipis$becat_nom != tesaurus_municipis$`osm_name:ca` &
    tesaurus_municipis$osm_comarca != "Fenolledès",
  c("becat_nom", "osm_name:ca", "osm_name", "osm_comarca")
]

openxlsx::write.xlsx(
  discrepancies,
  file = "inst/discrepàncies-municipis_osm-becat.xlsx", rowNames = FALSE, borders = "surrounding",
  colWidths = "auto", firstRow = TRUE, headerStyle = openxlsx::createStyle(textDecoration = "BOLD")
)


## Desa ----

tesaurus_municipis <- tesaurus_municipis[order(tesaurus_municipis$becat_comarca, tesaurus_municipis$becat_nom), ]
tesaurus_municipis <- unique(tesaurus_municipis)
rownames(tesaurus_municipis) <- NULL
usethis::use_data(tesaurus_municipis, overwrite = TRUE)

openxlsx::write.xlsx(
  tesaurus_municipis,
  file = "data-raw/tesaurus_municipis.xlsx", rowNames = FALSE, borders = "surrounding",
  colWidths = "auto", firstRow = TRUE, headerStyle = openxlsx::createStyle(textDecoration = "BOLD")
)

load("data/tesaurus_municipis.rda", verbose = TRUE) # tesaurus_municipis
