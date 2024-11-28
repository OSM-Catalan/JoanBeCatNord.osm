## Sistematitza el text de les introduccions en una taula.
load("data/intro_cadastre.rda", verbose = TRUE) # intro_cadastre
table(unlist(sapply(intro_cadastre, stringi::stri_trans_isnfc)))


## Classifica línies ----

linies <- list()
linies$corregit <- lapply(intro_cadastre, function(x) {
  out <- grep("corregit|corrigé", x, value = TRUE, ignore.case = TRUE)
  grep("GEOPORTAIL", out, value = TRUE, ignore.case = TRUE, invert = TRUE)
})
linies$atles_CatNord <- lapply(intro_cadastre, function(x) {
  grep("Atles", x, value = TRUE, ignore.case = TRUE)
})
linies$geoportail <- lapply(intro_cadastre, function(x) {
  grep("GEOPORTAIL", x, value = TRUE, ignore.case = TRUE)
})
linies$recull_preliminar <- lapply(intro_cadastre, function(x) {
  grep("preliminar", x, value = TRUE, ignore.case = TRUE)
})
linies$notes <- lapply(intro_cadastre, function(x) {
  grep("Notes", x, value = TRUE, ignore.case = TRUE)
})
num <- list()
num$corregit <- lapply(intro_cadastre, function(x) {
  # x <- rev(x)
  out <- grep("corregit|corrigé", x, ignore.case = TRUE)
  intersect(out, grep("GEOPORTAIL", x, ignore.case = TRUE, invert = TRUE))
})
num$atles_CatNord <- lapply(intro_cadastre, function(x) {
  x <- rev(x)
  grep("Atles", x, ignore.case = TRUE)
})
num$geoportail <- lapply(intro_cadastre, function(x) {
  x <- rev(x)
  grep("GEOPORTAIL", x, ignore.case = TRUE)
})
lapply(num, function(x) table(unlist(x)))

n_linies <- lapply(linies, sapply, length)
lapply(n_linies, table)

linies_0 <- lapply(n_linies, function(n) {
  intro_cadastre[n == 0]
})

municipis_linies_0 <- lapply(linies_0, names)
comarques[comarques$municipi %in% unique(unlist(municipis_linies_0)), ]
## CONCLUSIONS: els municipis sense referències a l'Atles toponímic de Catalunya Nord o GEOPORTAIL són del Fenolhedés


linies_2 <- mapply(function(l, n) {
  l[n == 2]
}, l = linies, n = n_linies)
linies_2

# CONCLUSIONS: Bages amb 2 revisions del cadastre (1990 i 2012) -> paste
# 2 línies amb GEOPORTAIL a Calmella (duplicat amb canvi d'ordre) -> agafa el primer
# 2 línies amb GEOPORTAIL a Vilanova de la Ribera (corregit en un mapa i no en un altre) -> paste
linies$corregit$Bages <- paste(linies$corregit$Bages, collapse = " ")
linies$geoportail$Calmella <- linies$geoportail$Calmella[1]
linies$geoportail$`Vilanova de la Ribera` <- paste(linies$geoportail$`Vilanova de la Ribera`, collapse = " ")

## CORREGEIX línia partida del bloc de recull_preliminar
linies$recull_preliminar$Montferrer <- paste(
  linies$recull_preliminar$Montferrer,
  grep("^ROSSIGNOL, J", intro_cadastre$Montferrer, value = TRUE)
)


### Analitza descartats ----

descartats_num <- mapply(function(x, loc) {
  linies_sel <- lapply(linies, function(sel) sel[[loc]])
  linia_descartada <- which(!x %in% linies_sel)

  if (length(linia_descartada) == 0) {
    return(integer())
  }

  # x[linia_descartada]
  c(which(!x %in% linies_sel), ultima_línia = length(x))
}, x = intro_cadastre, loc = names(intro_cadastre))
descartats_num <- descartats_num[sapply(descartats_num, length) > 0]
descartats_num


descarta_ultima <- sapply(descartats_num, function(x) setNames(x[length(x)] == x[length(x) - 1], NULL))
table(descarta_ultima)
intro_cadastre[names(which(descarta_ultima))]
## CONCLUSIONS: mirant el pdf, sembla correcte l'última línia de Felhuns. No s'ha perdut res.

## busca salts en els descartats
salta <- sapply(descartats_num, function(x) {
  x <- x[-length(x)]
  rang <- range(x)
  !all(x %in% rang[1]:rang[2])
})
table(salta)
## CONCLUSIONS: no hi ha salts.


#### Text dels descartats ----

descartats <- mapply(function(x, loc) {
  linies_sel <- lapply(linies, function(sel) sel[[loc]])
  grep(
    paste0(
      "GEOPORTAIL|corregit|",
      "^ROSSIGNOL, J. PEYTAVI i consellers municipals. Llista aprovada pel ple del consell municipal; batllessa: Madeleine DENAMIEL.$"
    ),
    setdiff(x, linies_sel),
    value = TRUE, invert = TRUE
  ) # elimina els casos modificats
}, x = intro_cadastre, loc = names(intro_cadastre))
descartats <- descartats[sapply(descartats, length) > 0]
descartats
c(casos = length(unlist(descartats)), unics = length(unique(unlist(descartats))))
sort(table(unlist(descartats)))


# FET: Potser es pot agrupar en recull preliminar i la resta que pot anar amb el camp corregit.
# table(sel_preliminar <- sapply(descartats, function(x) sum(grepl("preliminar", x))))
# table(sel_preliminar_ultim <- sapply(descartats[which(sel_preliminar > 0)], function(x) grep("preliminar", x) == length(x)))
# descartats[which(sel_preliminar > 0)][!sel_preliminar_ultim]
#
## CONCLUSIONS: Tots els reculls preliminars apareixen al final de les descartades
# Montferrer amb línia acabada en punt que no acaba la frase -> paste
# la Roca de l'Albera amb Notes: al final de la introducció. FET: linies$notes$`la Roca de l'Albera`


## CONCLUSIONS: les línies descartades corresponen als autors, col·laboradors i data de la revisió.
# en els pdf, formen un mateix paràgraf amb linies$corregit

linies$atribucions <- vector("list", length = length(intro_cadastre))
names(linies$atribucions) <- names(intro_cadastre)
linies$atribucions[names(descartats)] <- descartats
linies$atribucions[sapply(linies$atribucions, is.null)] <- lapply(
  linies$atribucions[sapply(linies$atribucions, is.null)], function(x) character(0L)
)
# save(linies, file = "data/part/linies_intro_clas.RData", compress = "xz")


## Extreu metadades ----

load("data/part/linies_intro_clas.RData", verbose = TRUE) # linies


## Idioma
catala_pdf <- sapply(linies$corregit, grepl, pattern = "corregit")
which(!catala_pdf)

## Corregit
table(corr_estandard <- sapply(linies$corregit, function(x) {
  grepl("^ *Cadastre (corregit|corrigé)|(no és corregit|pas corrigé)\\.$", x)
}))
linies$corregit[!corr_estandard]

corregit <- sapply(linies$corregit, function(x) grepl("^ *Cadastre (corregit|corrigé)", x))


## Atles toponímic de Catalunya Nord
unique(lapply(linies$atles_CatNord, function(x) {
  gsub("(topònims d[e'’]).+( amb la seua grafia catalana)", "\\1 XXX \\2", x)
}))
## CONCLUSIONS: expressió regular recull tots els casos en què el municipi apareix a l'Atles

atles_CatNord_estandard <- sapply(linies$atles_CatNord, function(x) {
  if (length(x) == 1) {
    grepl("((topònims|toponymes) d[e'’]).+( amb la seua grafia catalana|avec leur graphie catalane)", x)
  } else {
    FALSE
  }
})
unique(linies$atles_CatNord[!atles_CatNord_estandard])
## CONCLUSIONS: Alenyà, Baó i Sureda tenen redactat diferent. No diu «amb la seua grafia catalana correcte»
# Suposo que és equivalent
atles_CatNord <- sapply(linies$atles_CatNord, function(x) {
  if (length(x) == 1) {
    TRUE
  } else {
    FALSE
  }
})


### Geoportail ----

linies$geoportail
unique(geo_patro <- lapply(linies$geoportail, function(x) {
  gsub("(.+toponímia|toponymie)[ de'’]+.+((sobre el seu|sur son portail).+)\\.", "\\1 XXX \\2", x)
}))
table(patro_OK <- sapply(geo_patro, function(x) length(x) == 0 || grepl(x, pattern = "XXX")))
geo_patro[!patro_OK]

table(mapa_OK <- sapply(linies$geoportail, function(x) length(x) == 0 || grepl(x, pattern = "1:25\\.000")))
geo_patro[!mapa_OK]

table(parts_OK <- sapply(linies$geoportail, function(x) {
  length(x) == 0 || (grepl(x, pattern = " (seus* map(a|es)|sa carte)") & grepl(x, pattern = " (seu portal|sa carte)"))
}))
geo_patro[!parts_OK]

## CONCLUSIONS: expressió regular recull tots els casos.
# Diversitat en les opcions: corregit o no a GEOPORTAIL + corregit o no sobre els mapes 1:25000
# linies$geoportail[!catala_pdf]

ign_geoportail <- sapply(linies$geoportail, function(x) {
  if (length(x) == 0) {
    return(FALSE)
  }
  if (grepl("\\(IGN, Par[íi]s\\) (ha corregit la toponímia.+sobre el seu portal|a corrigé la toponymie.+sur son portail) internet GEOPORTAIL", x)) {
    TRUE
  } else if (grepl("\\(IGN, Par[íi]s\\) (no ha corregit la toponímia.+sobre el seu portal|n'a pas corrigé la toponymie.+sur son portail) internet GEOPORTAIL", x)) {
    FALSE
  } else {
    NA
  }
})
linies$geoportail[is.na(ign_geoportail)]

ign_mapes <- sapply(linies$geoportail, function(x) {
  if (length(x) == 0) {
    return(FALSE)
  }
  x <- gsub(" i i ", " i ", x)
  if (grepl("GEOPORTAIL (i sobre els* seus* map(a|es)|et sur sa carte)", x) ||
    grepl("\\(IGN, Par[íi]s\\) (ha corregit la toponímia.+sobre el seu mapa a|a corrigé la toponymie.+sur sa carte) 1:25.000", x)) {
    "corregit"
  } else if (grepl("GEOPORTAIL,* ((ni|i no l’ha corregida) sobre els* seus* map(a|es)|et ne l’a pas corrigée sur sa carte)", x) ||
    grepl("\\(IGN, Par[íi]s\\) (no ha corregit la toponímia.+sobre el seu mapa a|n'a pas corrigé la toponymie.+sur sa carte) 1:25.000", x)) {
    "no corregit"
  } else if (grepl("(sobre|per) (la|una) part .*del* (municipi en el )*.*seu mapa", x)) {
    "parcialment corregit"
  } else {
    NA
  }
})
linies$geoportail[is.na(ign_mapes)]


### Taula de metainformació dels fitxers de revisió de cadastres ----

meta_cadastre <- data.frame(
  municipi = names(linies$corregit), corregit, atles_CatNord, ign_geoportail, ign_mapes, catala_pdf, row.names = NULL
)


## Municipis amb dues taules de topònims (corregits i no corregits) ----

municipis_partits_becat_cadastre <- grep("_", names(becat_cadastre), value = TRUE)
municipis_partits <- unique(gsub("_.+$", "", municipis_partits_becat_cadastre))


meta_municipis_partits <- lapply(municipis_partits, function(x) {
  list(
    becat_cadastre = grep(paste0("^", x, "_.+$"), names(becat_cadastre), value = TRUE),
    meta = meta_cadastre[meta_cadastre$municipi == x, ],
    corregit = linies$corregit[[x]],
    geoportail = linies$geoportail[[x]]
  )
})
names(meta_municipis_partits) <- municipis_partits
meta_municipis_partits

meta_cadastre[meta_cadastre$municipi %in% municipis_partits, ]
municipis_partits_no_corregit <- meta_cadastre$municipi[meta_cadastre$municipi %in% municipis_partits & meta_cadastre$ign_mapes != "corregit"]
meta_municipis_partits[municipis_partits_no_corregit]
intro_cadastre[municipis_partits_no_corregit]
## CONCLUSIONS: els municipis amb dues taules (corregit i proposat) consten com a corregits segons les metadades.
# IGN mapes i geoportail també corregit segons metadades excepte per Mosset i Orellà
# Afegeix columna per municipis amb dues taules
meta_cadastre$dues_taules <- ifelse(meta_cadastre$municipi %in% municipis_partits, TRUE, FALSE)
sum(meta_cadastre$dues_taules) == length(municipis_partits)

capçaleres_partides <- lapply(becat_cadastre[municipis_partits_becat_cadastre], \(x) names(x)[2:3])
unique(capçaleres_partides)
capçaleres_partides[c("la Roca de l'Albera_CORREGIT", "la Roca de l'Albera_NO CORREGIT")]


## Desa ----

usethis::use_data(meta_cadastre, overwrite = TRUE)

openxlsx::write.xlsx(
  meta_cadastre,
  file = "data-raw/meta_cadastre.xlsx", rowNames = FALSE, borders = "surrounding", colWidths = "auto",
  firstRow = TRUE, headerStyle = openxlsx::createStyle(textDecoration = "BOLD")
)
readODS::write_ods(meta_cadastre, path = "data-raw/meta_cadastre.ods", row_names = FALSE)
