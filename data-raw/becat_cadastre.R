## https://joanbecat.cat/ca/catalunya-nord/municipis/

fitxers <- dir("data-raw/pdf", "\\.pdf$", full.names = TRUE)

cadastres <- lapply(fitxers, pdftools::pdf_text) # equivalent a `pdftotext -layout data-raw/pdf/*.pdf`
sapply(cadastres, length)
names(cadastres) <- gsub("^data-raw/pdf/|\\.pdf$", "", fitxers)

table(unlist(sapply(cadastres, Encoding)))
table(unlist(sapply(cadastres, stringi::stri_trans_isnfc)))
table(unlist(lapply(cadastres, function(x) {
  unique(do.call(rbind, lapply(stringi::stri_enc_detect(x), function(y) y[1, "Encoding"])))
})))
table(unlist(lapply(cadastres, function(x) stringi::stri_enc_isutf8(x))))
# lapply(cadastres, function(x) unique(do.call(rbind, lapply(stringi::stri_enc_detect(x), function(y) y[1, ]))))
# CONCLUSIÓ: Tot codificat a UTF-8 i normalitzat a NFC (W3C Specifications recommend using NFC for all content)

files <- lapply(cadastres, lapply, function(x) {
  # x <- gsub("(\\n)+[0-9]+", "", x) # treu números de pàgina (no afecta ncol i es pot usar de nom d'element)
  # x <- gsub("^\\s+[A-Z]\\n", "", x) # treu posició de l'abecedari
  # x <- gsub("[ ]{2,}", "\t", x) # afegeix \t com a separadors de columnes

  if (length(x) == 0 || x %in% c(NA, "", "\\n")) {
    return(character())
  }

  # read.table(text = x, header = FALSE, sep = "\t", row.names = NULL, fill = TRUE, encoding = "UTF-8")
  read.delim(text = x, header = FALSE, row.names = NULL, fill = TRUE, encoding = "UTF-8")
})

ncols <- sapply(files, sapply, ncol)
table(unlist(ncols))
files <- lapply(files, function(x) {
  x <- lapply(x, function(p) p[[1]])
  setNames(x, paste0("pag", seq_len(length(x))))
})


## Números de pàgina ----

## Extreu dels documents
tmp <- sapply(files, sapply, function(x) {
  # pag <- apply(x, 2, function(y) grep("^[0-9]+$", y, value = TRUE))
  # unlist(pag)
  pag <- gsub("\\s+", "", x[1])
})
tmp
all(mapply(function(val, nom) all(gsub("^pag", "", nom) == val), val = tmp, nom = lapply(tmp, names)))
## CONCLUSIONS: els números extrets coincideixen amb els noms ja afegits

## Elimina
unique(sapply(files, sapply, function(x) gsub("\\s+", "", x[1])))
## CONCLUSIÓ: elimina totes les primeres files (només contenen números)

files <- lapply(files, function(x) {
  sapply(x, function(p) {
    if (any(grepl("^\\s+[0-9]+$", p[1]))) {
      p <- p[-1]
    }
    return(p)
  }, simplify = FALSE)
})


## Peus de pàgina ----

sapply(files, sapply, function(p) p[length(p)])
peus <- sapply(files, function(x) unique(sapply(x, function(p) gsub("^\\s+", "", p[length(p)]))))
peus

## CONCLUSIONS: tot correcte amb un únic valor per cada fitxer. Elimina l'últim element.

files <- lapply(files, function(x) {
  sapply(x, function(p) p[-length(p)], simplify = FALSE)
})


## Extreu introducció ----

lapply(files, function(x) {
  x[[1]]
})


### Comarques ----
comarques <- lapply(files, function(x) {
  if (grepl("[A-ZÀ-Ú]$", x[[1]][2])) { # municipi > 1 línia
    if (grepl("[A-ZÀ-Ú]$", x[[1]][3])) { # municipi de 3 línies
      data.frame(comarca_pdf = x[[1]][4], municipi_pdf = paste(x[[1]][1:3], collapse = " "))
    } else { # municipi de 2 línies
      data.frame(comarca_pdf = x[[1]][3], municipi_pdf = paste(x[[1]][1:2], collapse = " "))
    }
  } else { # municipi d'una línia
    data.frame(comarca_pdf = x[[1]][2], municipi_pdf = x[[1]][1])
  }
})
comarques <- do.call(rbind, comarques)

table(comarques$comarca_pdf, useNA = "ifany")
comarques$comarca_pdf <- gsub("^ |\\.$", "", comarques$comarca_pdf)
comarques$municipi_pdf <- trimws(comarques$municipi_pdf)
comarques$nom_fitxer <- rownames(comarques)
rownames(comarques) <- NULL

# corregeix municipis
comarques$municipi <- sapply(strsplit(comarques$municipi_pdf, " "), function(x) {
  out <- paste(toupper(substring(x, 1, 1)), tolower(substring(x, 2)), sep = "", collapse = " ")
  out <- gsub("’", "'", out)
  out <- gsub(" I ", " i ", out)
  out <- gsub(" De ", " de ", out)
  out <- gsub(" Del ", " del ", out)
  out <- gsub(" Dels ", " dels ", out)
  out <- gsub("La ", "la ", out)
  out <- gsub("Les ", "les ", out)
  out <- gsub("^El ", "el ", out)
  out <- gsub("^Els ", "els ", out)
  out <- gsub("^L'a", "l'A", out)
  out <- gsub("^L'e", "l'E", out)
  out <- gsub(" \\(nom Correct\\)$", "", out)
  out <- gsub("^.+ \\(nom Correct: |\\)$", "", out)
  out <- gsub(" L'a", " l'A", out)
  out <- gsub(" D'a", " d'A", out)
  out <- gsub(" D'i", " d'I", out)
  # out <- gsub("D'amunt", "d'Amunt", out)
})
grep(" ", comarques$municipi, value = TRUE)
# Tesaurus: Le Vivièr (francès) -> El Viver, Pesilhan de Conflent (occità) -> Pesillà de Conflent
comarques[grep("Vivièr|Pesilhan", comarques$municipi), ]
tesaurus_municipis[grep("Vivièr|Pesilhan", tesaurus_municipis$becat_nom), ]

print(comarques[toupper(comarques$municipi_pdf) != toupper(comarques$nom_fitxer), ], quote = TRUE)


## Corregeix comarques
comarques$comarca <- gsub("^Comarca del* ", "", comarques$comarca_pdf)
comarques$comarca <- gsub("^Région historique du Roussillon", "Rosselló", comarques$comarca)
comarques$comarca <- gsub("^Région historique du Conflent", "Conflent", comarques$comarca)
comarques$comarca <- gsub("^Comarques del ", "", comarques$comarca)
comarques$comarca <- gsub(" i del ", " i ", comarques$comarca)
table(comarques$comarca)

## CONCLUSIONS: Algunes comarques mal agafades en municipis que ocupen > 1 línies. Corregit

comarques <- comarques[, c("municipi", "comarca", "municipi_pdf", "comarca_pdf", "nom_fitxer")]

compareDF::view_html(compareDF::compare_df(
  comarques,
  JoanBeCatNord.osm::comarques # , group_col = "osm_id"
))
identical(comarques, JoanBeCatNord.osm::comarques)
sapply(comarques, Encoding)
sapply(JoanBeCatNord.osm::comarques, Encoding)

usethis::use_data(comarques, overwrite = TRUE)

openxlsx::write.xlsx(
  comarques,
  file = "data-raw/comarques.xlsx", rowNames = FALSE, borders = "surrounding", colWidths = "auto",
  firstRow = TRUE, headerStyle = openxlsx::createStyle(textDecoration = "BOLD")
)


### Altres dades (data, autors, estat a l'IGN) ----

#### TODO: REPORT Calce, Espirà de l’Aglí i Jújols no tenen la capçalera en català (error a l'enllaç de la web o fitxer no disponible) ----
regex_comarca <- paste0("(", paste0(gsub("\\(.+", "", unique(comarques$comarca_pdf)), collapse = "|"), ")")
intro_cadastre <- lapply(files, function(x) {
  intro_cadastre <- x[[1]]

  inici <- grep(regex_comarca, x[[1]])
  pag_inici <- 1

  fi <- grep("TOPÒNIMS|TOPONYMES", x[[1]])
  pag_fi <- 1

  if (length(fi) == 0) {
    pag_fi <- 2
    intro_cadastre <- c(intro_cadastre, x[[2]])
    fi <- grep("TOPÒNIMS|TOPONYMES", intro_cadastre)
  }

  if (length(inici) > 0 && length(fi) > 0) {
    intro_cadastre <- intro_cadastre[(inici[1] + 1):(fi[1] - 1)]
  } else {
    warning(
      "Falta algun límit a ", paste(intro_cadastre[1:max(c(inici, 1))], collapse = " "), ": inici=", inici, "; fi=", fi
    )
  }

  return(intro_cadastre)
})


#### Uneix salts de línia ----

# Fitxers amb text en 2 columnes
multicolumnes <- lapply(intro_cadastre, function(x) grep("\\s{2,}", x, value = TRUE))
multicolumnes <- multicolumnes[sapply(multicolumnes, length) > 2]
multicolumnes

intro_cadastre <- lapply(intro_cadastre, function(x) {
  if (any(grepl("\\s{2,}", x))) { # multicolumnes (únic cas amb 2 columnes)
    inicis_columnes <- gregexec("^\\s*([0-9A-Za-zÀ-Úà-ú-]).+\\s{2,}([A-Za-zÀ-Úà-ú-])", x)
    inicis_columnes <- inicis_columnes[sapply(inicis_columnes, function(fila) fila[[1]] != -1)]
    mat_inicis <- sapply(inicis_columnes, function(y) y[, 1])
    if (length(mat_inicis) > 0) {
      inici_columnes <- apply(mat_inicis, 1, min)
    } else {
      inici_columnes <- NA
    }
    x <- c(
      trimws(substr(x, 1, inici_columnes[3] - 1)), # columna 1
      trimws(substr(x, inici_columnes[3], nchar(x))) # columna 2
    )
    x <- x[!x %in% ""]
  }

  sel_punt <- grep("\\.$", x)
  out <- character(length(sel_punt))
  for (i in seq_along(sel_punt)) {
    fi <- sel_punt[i]
    inici <- if (i == 1) 1 else sel_punt[i - 1] + 1
    out[i] <- paste(x[inici:fi], collapse = " ")
  }
  if (length(x) > fi) warning("parts omeses al final: ", x[fi:length(x)])

  out
})

table(sapply(intro_cadastre, length))
sort(table(unlist(intro_cadastre)))


##### Ajunta salt de línia amb punt final per abreviació de «ed. Terra Nostra» ----
# elimina espais dobles
sel_espais <- sapply(intro_cadastre, function(x) any(grepl("  ", x)))
table(sel_espais)
intro_cadastre[sel_espais] <- lapply(intro_cadastre[sel_espais], function(x) gsub("  ", " ", x))

sel_ed <- sapply(intro_cadastre, function(x) any(grepl("[eé]d\\.$", x)))
table(sel_ed)
ed <- sapply(intro_cadastre[sel_ed], function(x) {
  l <- c(x[length(x) - 1], x[length(x)])
  l <- paste(l, collapse = " ")
  ed_ok <- grepl("Atlas toponymique de +Catalogne Nord, [eé]d\\. +Terra Nostra, Prad(a|es), 2015, 2 volum(e)*s", l)
  if (!ed_ok) {
    warning(l)
    return(NULL)
  } else {
    return(l)
  }
})
ed <- ed[!sapply(ed, is.null)]
ed
## CONCLUSIONS: penúltima línia acaba amb «.» per l'abreviació de «ed.». -> paste
intro_cadastre[names(ed)] <- lapply(intro_cadastre[names(ed)], function(x) {
  uneix <- paste(x[length(x) - 1], x[length(x)])
  x <- x[-length(x)]
  x[length(x)] <- uneix
  x
})


### Canvia noms dels municipis pels del pdf corregits ----

load("data/comarques.rda", verbose = TRUE) # comarques

lapply(comarques[, c("municipi", "municipi_pdf", "nom_fitxer")], function(x) table(names(intro_cadastre) %in% x))
setdiff(names(intro_cadastre), comarques$nom_fitxer)

noms_corregits <- comarques$municipi[match(gsub("_.+", "", names(intro_cadastre)), comarques$nom_fitxer)]
tmp <- data.frame(noms_corregits, names(intro_cadastre))
tmp[tmp[[1]] != tmp[[2]], ]

names(intro_cadastre) <- noms_corregits


### Desa intro_cadastre ----

identical(intro_cadastre, JoanBeCatNord.osm::intro_cadastre)
sapply(intro_cadastre, Encoding)
sapply(JoanBeCatNord.osm::intro_cadastre, Encoding)

usethis::use_data(intro_cadastre, overwrite = TRUE)
load("data/intro_cadastre.rda", verbose = TRUE)


### Elimina introducció ----

files <- lapply(files, function(x) {
  fi_intro <- grep("TOPÒNIMS|TOPONYMES", x[[1]])

  if (length(fi_intro) > 0) {
    x$pag1 <- x[[1]][fi_intro:length(x[[1]])]
  } else { # Topònims comencen a la pàg. 2
    x <- x[-1]
    fi_intro <- grep("TOPÒNIMS|TOPONYMES", x[[1]])
    x$pag2 <- x[[1]][fi_intro:length(x[[1]])]
  }
  x
})


## Elimina pàgines buides ----
files <- lapply(files, function(x) x[sapply(x, length) > 0])


## Separa columnes per nchar. Diferent per cada pàgina ----
tipus <- sort(table(trimws(unlist(files))), decreasing = TRUE)
sel_tipus <- tipus[tipus > 2 & !grepl("(\\(|\\)|-|TOPÒNIMS I)", names(tipus))]

names(sel_tipus)

nom_tipus <- grep("^[A-Za-z]{2,}( [A-Za-z]{2,})*$", unlist(files), value = TRUE)
sort(table(nom_tipus), decreasing = TRUE)
n_nom_tipus <- sort(table(tolower(nom_tipus)), decreasing = TRUE)
sort(n_nom_tipus, decreasing = TRUE)
n_nom_tipus[n_nom_tipus > 1]


cerca_inici_columnes <- function(inicis) {
  inicis <- inicis[sapply(inicis, function(fila) fila[[1]] != -1)]
  mat_inicis <- sapply(inicis, function(y) y[, 1])
  if (length(mat_inicis) > 0) {
    inici_columnes <- apply(mat_inicis, 1, min)
  } else {
    inici_columnes <- NA
  }

  return(inici_columnes)
}


taula <- lapply(files, function(x) {
  out_pag <- lapply(x, function(p) {
    inicis_columnes <- gregexec(
      "^\\s*([0-9A-Za-zÀ-Úà-ú-]).+\\s{2,}([A-Za-zÀ-Úà-ú-]).+\\s{2,}([A-Za-zÀ-Úà-ú-]).+\\s{2,}([A-Za-zÀ-Úà-ú-])",
      p
    )
    # inici_columnes <- regexec(
    #   "^\\s*([A-Za-zÀ-Úà-ú-]).+\\s{2,}([A-Za-zÀ-Úà-ú-]).+\\s{2,}([A-Za-zÀ-Úà-ú-]).+\\s{2,}([A-Za-zÀ-Úà-ú-])",
    #   p
    # )
    inici_columnes <- cerca_inici_columnes(inicis_columnes)

    if (is.na(inici_columnes[1])) { # menys de 4 columnes
      inicis_columnes <- gregexec("^\\s*([0-9A-Za-zÀ-Úà-ú-]).+\\s{2,}([A-Za-zÀ-Úà-ú-]).+\\s{2,}([A-Za-zÀ-Úà-ú-])", p)
      inici_columnes <- cerca_inici_columnes(inicis_columnes)
      if (is.na(inici_columnes[1])) { # menys de 3 columnes
        inicis_columnes <- gregexec("^\\s*([0-9A-Za-zÀ-Úà-ú-]).+\\s{2,}([A-Za-zÀ-Úà-ú-])", p)
        inici_columnes <- cerca_inici_columnes(inicis_columnes)
        if (is.na(inici_columnes[1])) { # 1 columna
          out <- data.frame(c1. = p)
        } else { # 2 columnes
          out <- data.frame(list(
            c1. = substr(p, 1, inici_columnes[3] - 1),
            c2. = substr(p, inici_columnes[3], nchar(p))
          ))
        }
      } else { # 3 columnes
        out <- data.frame(list(
          c1. = substr(p, 1, inici_columnes[3] - 1),
          c2. = substr(p, inici_columnes[3], inici_columnes[4] - 1),
          c3. = substr(p, inici_columnes[4], nchar(p))
        ))
      }
    } else { # 4 columnes
      out <- data.frame(list(
        c1. = substr(p, 1, inici_columnes[3] - 1),
        c2. = substr(p, inici_columnes[3], inici_columnes[4] - 1),
        c3. = substr(p, inici_columnes[4], inici_columnes[5] - 1),
        c4. = substr(p, inici_columnes[5], nchar(p))
      ))
    }

    return(out)
  })

  out_pag
})


## Afegeix columnes buides i uneix pàgines ----
lapply(taula, sapply, ncol)
table(unlist(lapply(taula, sapply, ncol)))

taules_menys_4cols <- lapply(taula, function(x) x[sapply(x, ncol) < 4])
taules_menys_4cols <- taules_menys_4cols[sapply(taules_menys_4cols, length) > 0]

lapply(taules_menys_4cols, names)
lapply(taules_menys_4cols, sapply, ncol)
## CONCLUSIONS: alguns fitxers amb cel·les buides sense «-».
# Afegir columna al final per casos amb ncol = 3, i 1 + col + 2 per casos amb ncol = 1 (el Barquerès)

taula <- lapply(taula, function(x) {
  out <- lapply(x, function(p) {
    if (ncol(p) == 4) {
      return(p)
    }
    if (ncol(p) == 3) {
      return(cbind(p, c4. = character(nrow(p))))
    }
    if (ncol(p) == 1) {
      return(data.frame(c1. = character(nrow(p)), c2. = p[[1]], c3. = character(nrow(p)), c4. = character(nrow(p))))
    }
  })
  do.call(rbind, out)
})


## Extreu noms de columnes de les primeres files ----

unique(lapply(taula, function(x) data.frame(lapply(x[1:5, ], trimws))))

unique(lapply(taula, function(x) grep("(Noms de|Seccions|Sections)", x[, 1])[1]))
## CONCLUSIONS: capçaleres ben situades a les línies 1:3 però amb certa variabilitat.
# Acaben a les línies 4:5|6 amb nom de la secció.

taula <- lapply(taula, function(x) {
  fi_capçalera <- grep("(Noms de|Seccions|Sections)", x[1:10, 1])[1]
  capçalera <- x[1:(fi_capçalera - 1), ]
  x <- x[fi_capçalera:nrow(x), ]

  names(x) <- sapply(capçalera, function(y) trimws(paste(trimws(y), collapse = " ")))

  return(x)
})
unique(lapply(taula, names))
lapply(taula, head)


# FET! Punt segur ----

taula0 <- taula
# save(taula0, file = "data/part/taula0.RData", compress = "xz")
load("data/part/taula0.RData", verbose = TRUE) # taula0
taula <- taula0


## Arregla les files amb múltiples línies ----
## NOTA: els espais al principi i final de columna NO són útils. Depenen de si hi ha contingut o no a les columnes
# posteriors.
## línia amb "^\\s+$" a la primera columna. Tb hi ha alguns valors multilínia a la primera columna que comencen per
# "^\\s+/" o que la línia anterior acaba amb "/\\s+$"
lapply(taula, function(x) {
  segona_línia <- grep("(^\\s+$|^\\s*/)", x[, 1])
  multilínia <- grep("/\\s*$", x[, 1])

  multilínia <- sort(unique(c(multilínia, segona_línia - 1)))
  segona_línia <- multilínia + 1
  x[sort(unique(c(multilínia, segona_línia))), ]
})


# x <- taula$Alenyà # Cap línia doble
# x <- taula[[which(sapply(taula, nrow) == 268)]] # 4 línies en una fila
# x <- taula[[which(sapply(taula, nrow) == 272)]] #
# x <- taula[[which(sapply(taula, nrow) == 200)]] # 5 línies en una fila
# x <- taula[[which(sapply(taula, nrow) == 34)]] # només 2 línies dobles
# x <- taula[[which(sapply(taula, nrow) == 311)]] # 5 línies en una fila
# x <- taula$Talteüll # secció partida en fila multilínia sense / (taula$Talteüll[grep("pag12\\.", rownames(taula$Talteüll)), ])
taula <- lapply(taula, function(x) {
  segona_línia <- grep("(^\\s+$|^\\s*/)", x[, 1])
  multilínia <- grep("/\\s*$", x[, 1])

  multilínia <- sort(unique(c(multilínia, segona_línia - 1)))
  segona_línia <- multilínia + 1
  línia_simple <- setdiff(seq_len(nrow(x)), c(multilínia, segona_línia))

  if (length(segona_línia) == 0) {
    return(x)
  }

  reps2 <- c(ifelse(diff(segona_línia) == 1, 2, 1), 1)
  reps3 <- c(ifelse(diff(segona_línia, lag = 2) == 2, 3, 1), 1, 1)
  if (length(segona_línia) > 2) {
    reps4 <- c(ifelse(diff(segona_línia, lag = 3) == 3, 4, 1), 1, 1, 1)
  } else {
    reps4 <- rep(1, length(segona_línia))
  }
  if (length(segona_línia) > 3) {
    reps5 <- c(ifelse(diff(segona_línia, lag = 4) == 4, 5, 1), 1, 1, 1, 1)
  } else {
    reps5 <- rep(1, length(segona_línia))
  }
  reps <- ifelse(reps5 == 5, 5, ifelse(reps4 == 4, 4, ifelse(reps3 == 3, 3, reps2)))

  # tmp<- data.frame(segona_línia, reps, reps2, reps3, reps3, reps5); tmp
  if (any(reps == 5)) {
    reps <- reps[-(which(reps == 5) + rep(1:4, each = sum(reps == 5)))]
  }
  if (any(reps == 4)) {
    reps <- reps[-(which(reps == 4) + rep(1:3, each = sum(reps == 4)))]
  }
  if (any(reps == 3)) {
    reps <- reps[-(which(reps == 3) + rep(1:2, each = sum(reps == 3)))]
  }
  if (any(reps == 2)) {
    reps <- reps[-(which(reps == 2) + 1)]
  }

  grups <- rep(seq_along(reps), times = reps)
  # data.frame(segona_línia, grups)
  fila_multilínia <- split(segona_línia, grups)

  if (length(fila_multilínia)) {
    for (i in seq_along(fila_multilínia)) {
      linies <- c(fila_multilínia[[i]][1] - 1, fila_multilínia[[i]])
      x[linies, ] <- apply(x[linies, ], 2, function(y) {
        y[1] <- paste(gsub("^\\s+|\\s+$", "", y), collapse = " ")
        y[1] <- gsub("\\s+/|/\\s+", "/", y[1])
        y[1] <- gsub(" -([A-Za-zÀ-Úà-ú])", "-\\1", y[1])
        y[1] <- gsub("([A-Za-zÀ-Úà-ú])- ", "\\1-", y[1])
        y[-1] <- NA_character_
        y
      })
    }
  }

  return(x)
})
print(taula, quote = TRUE)


## Uneix tipus en múltiples línies ----

taula <- lapply(taula, function(x) {
  sel_tipus <- which(apply(x[, -1], 1, function(y) all(grepl("^\\s*$", y))))
  # diff(sel_tipus) == 1
  # x[sel_tipus, 1]
  reps2 <- c(ifelse(diff(sel_tipus) == 1, 2, 1), 1)
  reps3 <- c(ifelse(diff(sel_tipus, lag = 2) == 2, 3, 1), 1, 1)
  if (length(sel_tipus) > 2) {
    reps4 <- c(ifelse(diff(sel_tipus, lag = 3) == 3, 4, 1), 1, 1, 1)
  } else {
    reps4 <- rep(1, length(sel_tipus))
  }
  if (length(sel_tipus) > 3) {
    reps5 <- c(ifelse(diff(sel_tipus, lag = 4) == 4, 5, 1), 1, 1, 1, 1)
  } else {
    reps5 <- rep(1, length(sel_tipus))
  }
  reps <- ifelse(reps5 == 5, 5, ifelse(reps4 == 4, 4, ifelse(reps3 == 3, 3, reps2)))

  # tmp<- data.frame(sel_tipus, reps, reps2, reps3, reps3, reps5); tmp
  if (any(reps == 5)) {
    reps <- reps[-(which(reps == 5) + rep(1:4, each = sum(reps == 5)))]
  }
  if (any(reps == 4)) {
    reps <- reps[-(which(reps == 4) + rep(1:3, each = sum(reps == 4)))]
  }
  if (any(reps == 3)) {
    reps <- reps[-(which(reps == 3) + rep(1:2, each = sum(reps == 3)))]
  }
  if (any(reps == 2)) {
    reps <- reps[-(which(reps == 2) + 1)]
  }

  grups <- rep(seq_along(reps), times = reps)
  # data.frame(sel_tipus, grups)
  fila_multilínia <- split(sel_tipus, grups)
  fila_multilínia <- fila_multilínia[sapply(fila_multilínia, length) > 1]
  # x[unlist(fila_multilínia), ]

  if (length(fila_multilínia)) {
    for (i in seq_along(fila_multilínia)) {
      linies <- fila_multilínia[[i]]
      x[linies, ] <- apply(x[linies, ], 2, function(y) {
        y[1] <- paste(gsub("^\\s+|\\s+$", "", y), collapse = " ")
        y[1] <- gsub("\\s+/|/\\s+", "/", y[1])
        y[-1] <- NA_character_
        y
      })
    }
  }

  # return(x[unlist(fila_multilínia), ])
  return(x)
})


tipus_errors <- lapply(taula, function(x) {
  sel_tipus <- which(apply(x[, -1], 1, function(y) all(grepl("^\\s*$", y))))
  grep("/|[0-9]|^\\s*[A-Z]\\s*$|^\\s*[A-Z] [A-Za-z]", x[sel_tipus, 1], value = TRUE)
})
tipus_errors <- tipus_errors[sapply(tipus_errors, length) > 0]
tipus_errors
## CONCLUSIONS: errors degut a seccions sensals partides en més d'una línia sense acabar o que comença amb / i cap altra
# columna amb més d'una línia

taula[names(tipus_errors)] <- lapply(taula[names(tipus_errors)], function(x) {
  err <- which(
    grepl("/|[0-9]|^\\s*[A-Z]\\s*$|^\\s*[A-Z] [A-Za-z]", x[, 1]) &
      apply(x[, -1], 1, function(y) all(grepl("^\\s*$", y)))
  )
  while (length(err) > 0) {
    if (length(err) > 1 && any(diff(err) == 1)) {
      stop("Errors de amb més d'una línia")
    }

    # x[sort(c(err - 1, err, err + 1)), ]
    fusiona <- data.frame(multilínia = trimws(x[err - 1, 1]), err = trimws(x[err, 1]))
    fusiona <- apply(fusiona, 2, function(y) {
      y[is.na(y)] <- ""
      y
    }, simplify = FALSE)
    línia <- paste0(fusiona[[1]], fusiona[[2]])
    x[err - 1, 1] <- sapply(strsplit(línia, " "), function(y) y[1])
    x[err, 1] <- sapply(strsplit(trimws(x[err, 1]), " "), function(y) paste(y[-1], collapse = " "))

    err <- which(
      grepl("/|[0-9]|^\\s*[A-Z]\\s*$|^\\s*[A-Z] [A-Za-z]", x[, 1]) &
        apply(x[, -1], 1, function(y) all(grepl("^\\s*$", y) | is.na(y)))
    )
  }

  # sel <- which(apply(x[, -1], 1, function(y) all(grepl("^\\s*$", y))))
  # sel <- sort(c(sel, sel + 1, sel - 1))
  # return(x[sel, ])
  return(x)
})


## Línies pendents que tenien la primera columna multilínia i alguna altre també. Buscar valors acabats en -\\s*$ (e.g.
# taula$$`Vilallonga de la Salanca`["pag3.23", ])
multilínia_pendent <- lapply(taula, function(x) {
  multilínia <- which(apply(x[, -1], 1, function(y) any(grepl("[A-ZÀ-Úa-zà-ú]-\\s*$", y))))
  x[sort(c(multilínia, multilínia + 1)), ]
})
multilínia_pendent <- multilínia_pendent[sapply(multilínia_pendent, nrow) > 0]
print(multilínia_pendent, quote = TRUE)

### TODO: REPORT guio final sense sentit a l'original ----
taula$Oceja["pag7.3", "NOM SOBRE GEOPORTAIL (portal cartogràfic de l’IGN)"]
taula$Oceja["pag7.3", "NOM SOBRE GEOPORTAIL (portal cartogràfic de l’IGN)"] <-
  gsub("-$", "", taula$Oceja["pag7.3", "NOM SOBRE GEOPORTAIL (portal cartogràfic de l’IGN)"])

corregit <- paste(
  trimws(taula$`Vilallonga de la Salanca`[c("pag3.22", "pag3.23"), "TOPÒNIMS I FULLS CADASTRALS"]),
  collapse = ""
)
corregit
taula$`Vilallonga de la Salanca`["pag3.22", "TOPÒNIMS I FULLS CADASTRALS"] <- corregit

corregit <- paste(
  trimws(taula$`Vilallonga de la Salanca`[c("pag3.22", "pag3.23"), "NOM SOBRE EL CADASTRE ANTERIOR"]),
  collapse = ""
)
corregit
taula$`Vilallonga de la Salanca`["pag3.22", "NOM SOBRE EL CADASTRE ANTERIOR"] <- corregit

taula$`Vilallonga de la Salanca`[c("pag3.22", "pag3.23"), ]
taula$`Vilallonga de la Salanca`["pag3.23", ] <- NA

### CONCLUSIONS: TODO poden quedar més casos de línies múltiples emmascarades per valors múltiples en la primera i altres columnes ----


## Noms de seccions cadastrals ----

### Multilínia fusionats ----

codis <- lapply(taula, function(x) {
  sel <- intersect(which(is.na(x[, 1])) - 1, which(!is.na(x[, 1])))
  sel <- intersect(sel, grep("/", x[, 1]))
  out <- x[sel, 1]
  unlist(strsplit(out, "/"))
})
codisU <- trimws(unique(unlist(codis)))
codisU[order(nchar(codisU))]


#### Correccions nchar(codis) == 1 ----

codis1 <- unique(codisU[nchar(codisU) == 1])
regexp <- paste0("^(", paste(codis1, collapse = "|"), ")$")
codis_1L <- lapply(codis, function(x) unique(grep(regexp, trimws(x), value = TRUE)))
codis_1L <- codis_1L[sapply(codis_1L, length) > 0]
codis_1L


x <- mapply(function(x, codi) {
  sel <- grep(paste0("(^|\\s+|/)", codi, "(\\s+|/|$)"), x[, 1])
  sel <- sort(unique(c(sel, sel - 1, sel + 1, sel + 2)))
  x[sel, ]
}, x = taula[names(codis_1L)], codi = codis_1L, SIMPLIFY = FALSE)
x

x$Montesquiu
sel <- grep("/D$", taula$Montesquiu[, 1])
corregit <- trimws(paste(trimws(taula$Montesquiu[sel, ]), trimws(taula$Montesquiu[sel + 2, ])))
corregit[1] <- gsub(" ", "", corregit[1])
corregit

taula$Montesquiu[sel + 2, ] <- NA_character_
taula$Montesquiu[sel, ] <- corregit


x$Perpinyà
sel <- grep("/E\\s+$", taula$Perpinyà[, 1])
corregit <- trimws(paste(trimws(taula$Perpinyà[sel, ]), trimws(taula$Perpinyà[sel + 1, ])))
corregit[1] <- gsub(" ", "", corregit[1])
corregit

taula$Perpinyà[sel + 1, ] <- NA_character_
taula$Perpinyà[sel, ] <- corregit


x$`Pesillà de la Ribera`
sel <- grep("/A\\s+$", taula$`Pesillà de la Ribera`[, 1])
corregit <- trimws(paste(trimws(taula$`Pesillà de la Ribera`[sel, ]), trimws(taula$`Pesillà de la Ribera`[sel + 1, ])))
corregit[1] <- gsub(" ", "", corregit[1])
corregit

taula$`Pesillà de la Ribera`[sel + 1, ] <- NA_character_
taula$`Pesillà de la Ribera`[sel, ] <- corregit


x$`Sant Llorenç de la Salanca`
sel <- grep("/A\\s+$", taula$`Sant Llorenç de la Salanca`[, 1])
corregit <- trimws(paste(
  trimws(taula$`Sant Llorenç de la Salanca`[sel, ]), trimws(taula$`Sant Llorenç de la Salanca`[sel + 1, ])
))
corregit[1] <- gsub(" ", "", corregit[1])
corregit

taula$`Sant Llorenç de la Salanca`[sel + 1, ] <- NA_character_
taula$`Sant Llorenç de la Salanca`[sel, ] <- corregit

sel <- grep("/B\\s+$", taula$`Sant Llorenç de la Salanca`[, 1])
taula$`Sant Llorenç de la Salanca`[sort(sel + 0:1), ]
corregit <- trimws(paste(
  trimws(taula$`Sant Llorenç de la Salanca`[sel, ]), trimws(taula$`Sant Llorenç de la Salanca`[sel + 1, ])
))
corregit[1] <- gsub(" ", "", corregit[1])
corregit

taula$`Sant Llorenç de la Salanca`[sel + 1, ] <- NA_character_
taula$`Sant Llorenç de la Salanca`[sel, ] <- corregit


x$Sornian # correcte


#### Correccions nchar(codis) > 3 ----

codis45 <- unique(codisU[nchar(codisU) > 3])
regexp <- paste0("^(", paste(codis45, collapse = "|"), ")$")
codis_45L <- lapply(codis, function(x) unique(grep(regexp, trimws(x), value = TRUE)))
codis_45L <- codis_45L[sapply(codis_45L, length) > 0]
codis_45L


x <- mapply(function(x, codi) {
  sel <- grep(paste0("(^|\\s+|/)", paste(codi, collapse = "|"), "(\\s+|/|$)"), x[, 1])
  sel <- sort(unique(c(sel, sel - 1, sel + 1, sel + 2)))
  x[sel, ]
}, x = taula[names(codis_45L)], codi = codis_45L, SIMPLIFY = FALSE)
x

x$Aiguatèbia ## Correcte


x$Cotlliure ## TODO: REPORT error al fitxer ATAV -> AT/AV
taula$Cotlliure[, 1] <- gsub("ATAV", "AT/AV", taula$Cotlliure[, 1])


x$Elna ## TODO: REPORT error al fitxer ATAV -> AT/AV
taula$Elna[, 1] <- gsub("ATAV", "AT/AV", taula$Elna[, 1])


x$Estavar ## TODO: REPORT error al fitxer ADA1 -> AD/A1
taula$Estavar[, 1] <- gsub("ADA1", "AD/A1", taula$Estavar[, 1])


x$`la Guingueta d’Ix` ## TODO: REPORT error al fitxer ABAE AIA1; CORRECTE: 31B1 31A1
taula$`la Guingueta d’Ix`[, 1] <- gsub("ABAE", "AB/AE", taula$`la Guingueta d’Ix`[, 1])
taula$`la Guingueta d’Ix`[, 1] <- gsub("AIA1", "AI/A1", taula$`la Guingueta d’Ix`[, 1])


x$`Morellàs i les Illes` ## CORRECTE: codis_45L$`Morellàs i les Illes`


x$Oceja ## TODO: REPORT error al fitxer B1B1 -> B1
taula$Oceja[, 1] <- gsub("B1B1", "B1", taula$Oceja[, 1])


x$Paretstortes ## TODO: REPORT error al fitxer ABA2 -> AB/A2
taula$Paretstortes[, 1] <- gsub("ABA2", "AB/A2", taula$Paretstortes[, 1])


x$Perpinyà ## TODO: REPORT error al fitxer DZEI -> DZ/EI
taula$Perpinyà[, 1] <- gsub("DZEI", "DZ/EI", taula$Perpinyà[, 1])


x$Queixàs ## TODO: REPORT error al fitxer C1A3 -> C1/A3
taula$Queixàs[, 1] <- gsub("C1A3", "C1/A3", taula$Queixàs[, 1])


x$`Òpol i Perellós` ## TODO: REPORT error al fitxer A1D1 -> A1/D1
taula$`Òpol i Perellós`[, 1] <- gsub("A1D1", "A1/D1", taula$`Òpol i Perellós`[, 1])



### Tots els noms de seccions sensals ----
# Útil per detectar multilínies pendents! La majoria de codis són [A-Z0-9]{2}

codis <- lapply(taula, function(x) {
  sel <- grep("[A-Z0-9]", x[, 1])
  # sel <- intersect(sel, grep("[a-z]", x[, 1], invert = TRUE))
  # sel <- intersect(sel, grep("/", x[, 1]))
  out <- x[sel, 1]
  unlist(strsplit(out, "/"))
})
codisU <- setdiff(trimws(unique(unlist(codis))), "")

minuscules <- grep("[a-z]", codisU, value = TRUE)
minuscules[order(nchar(minuscules))]
## CONCLUSIONS: totes els valors amb minúscules són correctes o són tipus. Treu-los
codisU <- grep("[a-z]", codisU, value = TRUE, invert = TRUE)

codisU[order(nchar(codisU))]
codisU[nchar(codisU) > 2]

sospitosos <- grep("[^a-z^A-Z^0-9]", codisU, value = TRUE)
sospitosos

## CONCLUSIONS: Taules amb capçalera a mitja taula. Separadors de seccions amb - (no /) a taula$Terrats (TODO: REPORT).
# Problemes amb columnes (solució: calcular punts de tall usant files de 3 i 4 columnes) a taula$`Santa Maria la Mar`

## CONCLUSIONS:
regexp <- paste0("^(", paste(gsub("\\.", "\\\\.", sospitosos), collapse = "|"), ")$")
cerca <- lapply(codis, function(x) grep(regexp, trimws(x), value = TRUE))
cerca <- cerca[sapply(cerca, length) > 0]
cerca


#### Correccions ----
taula$Salses[, 1] <- gsub("\\.c1", "C1", taula$Salses[, 1]) # TODO: REPORT
taula$`el Voló`[, 1] <- gsub("\\.a1", "A1", taula$`el Voló`[, 1]) # TODO: REPORT
taula$Terrats[, 1] <- gsub("-", "/", taula$Terrats[, 1])
taula$`Òpol i Perellós`[, 1] <- gsub("-E1", "E1", taula$`Òpol i Perellós`[, 1]) # TODO: REPORT
taula$`Sant Llorenç de la Salanca`[, 1] <- gsub("\\.B", "B", taula$`Sant Llorenç de la Salanca`[, 1]) # TODO: REPORT
taula$`Angostrina i Vilanova de les Escaldes`[, 1] <-
  gsub("\\.D3", "D3", taula$`Angostrina i Vilanova de les Escaldes`[, 1]) # TODO: REPORT
taula$Baixàs[, 1] <- gsub("\\.A3", "A3", taula$Baixàs[, 1]) # TODO: REPORT
taula$Cortsaví[, 1] <- gsub("úC2", "C2", taula$Cortsaví[, 1]) # TODO: REPORT
# taula$Cortsaví[, 1] <- gsub("A·", "A?", taula$Cortsaví[, 1]) # TODO: REPORT No és clar si cau a A3 o a A2
taula$Matamala[, 1] <- gsub("C!", "C1", taula$Matamala[, 1]) # TODO: REPORT


#### multilínia en capçaleres intermitges ----
capçaleres <- lapply(taula, function(x) {
  sel <- grep("TOPÒNIMS", x[, 1])
  x[sort(c(sel, sel + 1, sel + 2, sel + 3)), ]
})
capçaleres <- capçaleres[sapply(capçaleres, nrow) > 0]
capçaleres

taula[names(capçaleres)] <- lapply(taula[names(capçaleres)], function(x) {
  inici <- grep("TOPÒNIMS", x[, 1])
  fi <- grep("CADASTRALS", x[, 1])

  x[inici, ] <- apply(x[inici:fi, ], 2, function(y) {
    paste(trimws(y), collapse = " ")
  })
  x[(inici + 1):fi, ] <- NA_character_

  # x[inici:fi, ]
  x
})


#### Partició de columnes a taula$`Santa Maria la Mar`[3:6] ----
x <- taula$`Santa Maria la Mar`[3:6, ]
n_caracters <- sapply(taula$`Santa Maria la Mar`[3:6, ], nchar)

corregit <- data.frame(
  c1 = substr(x[, 1], 1, n_caracters[, 1] - 1),
  c2 = trimws(paste0(substr(x[, 1], n_caracters[, 1], n_caracters[, 1]), substr(x[, 2], 1, n_caracters[, 2] - 2))),
  c3 = trimws(paste0(substr(x[, 2], n_caracters[, 2] - 2, n_caracters[, 2]), substr(x[, 3], 1, n_caracters[, 3]))),
  c4 = character(nrow(x))
)
corregit

taula$`Santa Maria la Mar`[3:6, ] <- corregit


#### Correccions nchar(codis) == 1 ----

codis <- lapply(taula, function(x) {
  sel <- grep("[A-Z0-9]", x[, 1])
  # sel <- intersect(sel, grep("[a-z]", x[, 1], invert = TRUE))
  # sel <- intersect(sel, grep("/", x[, 1]))
  out <- x[sel, 1]
  unlist(strsplit(out, "/"))
})
codisU <- setdiff(trimws(unique(unlist(codis))), "")

codis1 <- unique(codisU[nchar(codisU) == 1])
regexp <- paste0("^(", paste(codis1, collapse = "|"), ")$")
codis_1L <- lapply(codis, function(x) unique(grep(regexp, trimws(x), value = TRUE)))
codis_1L <- codis_1L[sapply(codis_1L, length) > 0]
codis_1L


x <- mapply(function(x, codi) {
  sel <- grep(paste0("(^|\\s+|/)(", paste(codi, collapse = "|"), ")(\\s+|/|$)"), x[, 1])
  x[sel, ]
  x[sort(unique(c(sel, sel - 1, sel + 1, sel + 2))), ]
}, x = taula[names(codis_1L)], codi = codis_1L, SIMPLIFY = FALSE)
x


##### Generalitza seccions partides que deixen un caràcter sol ----
test <- lapply(taula, function(y) {
  multilínia <- which(grepl("(^|/|\\s+)[A-Z]\\s*$", y[, 1])[-nrow(y)] & grepl("^\\s*[0-9A-Z](^|/|\\s+)", y[, 1])[-1])
  for (i in multilínia) {
    corregit <- apply(y[c(i, i + 1), ], 2, function(z) trimws(paste(trimws(z), collapse = " ")))
    corregit[1] <- gsub(" ", "", corregit[1])
    corregit <- gsub(" /", "/", corregit)
    y[i, ] <- corregit
    y[i + 1, ] <- NA_character_
  }
  y[multilínia, ]
})
test <- test[sapply(test, nrow) > 0]
test


taula <- lapply(taula, function(x) {
  multilínia <- which(grepl("(^|/|\\s+)[A-Z]\\s*$", x[, 1])[-nrow(x)] & grepl("^\\s*[A-Z0-9](^|/|\\s+)", x[, 1])[-1])
  for (i in multilínia) {
    corregit <- apply(x[c(i, i + 1), ], 2, function(z) trimws(paste(trimws(z), collapse = " ")))
    corregit[1] <- gsub(" ", "", corregit[1])
    corregit <- gsub(" /", "/", corregit)
    x[i, ] <- corregit
    x[i + 1, ] <- NA_character_
  }
  x
})


codis <- lapply(taula, function(x) {
  sel <- grep("[A-Z0-9]", x[, 1])
  # sel <- intersect(sel, grep("[a-z]", x[, 1], invert = TRUE))
  # sel <- intersect(sel, grep("/", x[, 1]))
  out <- x[sel, 1]
  unlist(strsplit(out, "/"))
})
codisU <- setdiff(trimws(unique(unlist(codis))), "")

codis1 <- unique(codisU[nchar(codisU) == 1])
regexp <- paste0("^(", paste(codis1, collapse = "|"), ")$")
codis_1L <- lapply(codis, function(x) unique(grep(regexp, trimws(x), value = TRUE)))
codis_1L <- codis_1L[sapply(codis_1L, length) > 0]
codis_1L


x <- mapply(function(x, codi) {
  sel <- grep(paste0("(^|\\s+|/)(", paste(codi, collapse = "|"), ")(\\s+|/|$)"), x[, 1])
  x[sel, ]
  x[sort(unique(c(sel, sel - 1, sel + 1, sel + 2))), ]
}, x = taula[names(codis_1L)], codi = codis_1L, SIMPLIFY = FALSE)
x

x$Forques # Correcte segons el pdf original. Si fa referència a la capella de Sant Sebastià, A4
taula$Forques[, 1] <- gsub("^\\s+A\\s+$", "A4", taula$Forques[, 1])


x$`Morellàs i les Illes`
sel <- grep("^\\s+[12]\\s+$$", taula$`Morellàs i les Illes`[, 1])
corregit <- lapply(sel, function(i) {
  out <- apply(taula$`Morellàs i les Illes`[c(i - 1, i), ], 2, function(y) {
    paste(trimws(y), collapse = " ")
  })
  out[1] <- gsub(" ", "", out[1])
  out
})
corregit <- do.call(rbind, corregit)
corregit

taula$`Morellàs i les Illes`[sel - 1, ]
taula$`Morellàs i les Illes`[sel, ]

taula$`Morellàs i les Illes`[sel - 1, ] <- corregit
taula$`Morellàs i les Illes`[sel, ] <- NA_character_


x$Reiners # Correcte segons el pdf original

x$Sornian # Correcte segons el pdf original


#### Correccions nchar(codis) > 2 ----

codis <- lapply(taula, function(x) {
  sel <- grep("^\\s+[A-Z0-9]+{2,}\\s+$", x[, 1])
  # sel <- intersect(sel, grep("[a-z]", x[, 1], invert = TRUE))
  # sel <- intersect(sel, grep("/", x[, 1]))
  out <- x[sel, 1]
  unlist(strsplit(out, "/"))
})
codisU <- setdiff(trimws(unique(unlist(codis))), "")

codis3 <- unique(codisU[nchar(codisU) > 2])
regexp <- paste0("^(", paste(codis3, collapse = "|"), ")$")
codis_3L <- lapply(codis, function(x) unique(grep(regexp, trimws(x), value = TRUE)))
codis_3L <- codis_3L[sapply(codis_3L, length) > 0]
codis_3L


x <- mapply(function(x, codi) {
  sel <- grep(paste0("(^|\\s+|/)(", paste(codi, collapse = "|"), ")(\\s+|/|$)"), x[, 1])
  x[sel, ]
  x[sort(unique(c(sel, sel - 1, sel + 1, sel + 2))), ]
}, x = taula[names(codis_3L)], codi = codis_3L, SIMPLIFY = FALSE)
x

x$Aiguatèbia # Correcte "200A1" "200B1"
x$Canet # Correcte ACAD segons taula del pdf. Probablement AC/AD pel mapa # TODO: REPORT
x$Fenolhet # Correcte BB3 segons taula del pdf. Probablement B3 pel mapa # TODO: REPORT
x$Fontpedrosa # Correcte
x$Illa # Correcte BDE segons taula del pdf. Probablement incorrecte segons el mapa # TODO: REPORT
x$`la Guingueta d’Ix` # Correcte
x$Montboló # Correcte "AB1" "AC1" segons taula del pdf. Probablement incorrecte segons el mapa # TODO: REPORT
x$`Morellàs i les Illes` # Correcte
x$Orbanyà # Correcte
x$Planès # Correcte "AB1" segons taula del pdf. Probablement incorrecte segons el mapa # TODO: REPORT
x$Portè # Correcte "B12" segons taula del pdf. Probablement incorrecte segons el mapa # TODO: REPORT
x$`Prats de Molló` # Correcte "AB1" i "AC1" segons taula del pdf. Probablement incorrecte segons el mapa # TODO: REPORT. F10 correcte
x$Salses # Correcte "AA7G1" segons taula del pdf. Probablement incorrecte segons el mapa # TODO: REPORT.


# FET! Punt probablement segur ----
taula1 <- taula
# save(taula1, file = "data/part/taula1.RData", compress = "xz")
load("data/part/taula1.RData", verbose = TRUE) # taula1
taula <- taula1


## **TRIMWS, elimina files buides i cel·les buides com a NAs** ----

taula <- lapply(taula, function(x) {
  x[] <- apply(x, 2, function(y) {
    y[y %in% "" | grepl("^(\\s*[-]*\\s*)$", y)] <- NA
    trimws(y)
  })
  x <- x[!apply(x, 1, function(y) all(is.na(y))), ]
})


## Corregeix guions sospitosos ----

test <- lapply(taula, function(x) x[apply(x[, -1], 1, function(y) any(grepl("-$|\\s+-|-\\s", y))), ])
test <- test[sapply(test, nrow) > 0]
test
## CONCLUSIONS: algunes línies fusionades incorrectament! Algunes no tenen full cadastral i per això s'han colat

## test$`Argelers de la Marenda`
d <- taula$`Argelers de la Marenda`
d[grep("^pag13\\.2", rownames(d)), ]
sel <- grep("^pag13\\.21$", rownames(d))
corregit <- d[sel, ]
corregit <- rbind(corregit, setNames(data.frame(NA, NA, NA, "Font Andreu"), names(corregit)))
corregit[1, ] <- gsub(" -$| Font Andreu$", "", corregit[1, ])
rownames(corregit)[2] <- "pag13.22"
corregit

d_corregit <- rbind(d[1:(sel - 1), ], corregit, d[(sel + 1):nrow(d), ])
d_corregit[(sel - 3):(sel + 3), ]
d[(sel - 3):(sel + 3), ]
taula$`Argelers de la Marenda` <- d_corregit


## test$Cortsaví
# CONCLUSIONS: Full cadastral de la fila comença amb / i es fusiona amb l'anterior. TODO: REPORT
d <- taula$Cortsaví
d[grep("^pag14\\.1", rownames(d)), ]
sel <- grep("^pag14\\.11$", rownames(d))
corregit <- d[sel, ]
corregit <- rbind(corregit, corregit)
corregit[1, ] <- gsub(
  "/D1/D2$| CR dit Camí de la Torre d'Anglars$| Chemin dit de la Tourre d'en Glas$", "", corregit[1, ]
)
corregit[2, ] <- gsub(
  "^B1/B5/B6/|^Chemin dit de la Taillède et de Madeloc |^CR dit Camí de la Telleda i Madaloc ", "", corregit[2, ]
)
rownames(corregit)[2] <- "pag14.13"
corregit[, 4] <- NA_character_
corregit

d_corregit <- rbind(d[1:(sel - 1), ], corregit, d[(sel + 1):nrow(d), ])
d_corregit[(sel - 3):(sel + 3), ]
d[(sel - 3):(sel + 3), ]
taula$Cortsaví <- d_corregit


## test$Elna
# CONCLUSIONS: Fila sense full cadastral que es fusiona amb l'anterior. TODO: REPORT
d <- taula$Elna
d[grep("^pag5\\.2", rownames(d)), ]
sel <- grep("^pag5\\.26$", rownames(d))
corregit <- d[sel, ]
corregit <- rbind(corregit, corregit)
corregit[1, ] <- gsub(
  " -$| Mas Riols$", "", corregit[1, ]
)
corregit[2, ] <- gsub(
  "^AN$|^- |^Mas Ribes ", "", corregit[2, ]
)
rownames(corregit)[2] <- "pag5.27"
corregit[1, 2] <- NA_character_
corregit[2, c(1, 3)] <- NA_character_
corregit

d_corregit <- rbind(d[1:(sel - 1), ], corregit, d[(sel + 1):nrow(d), ])
d_corregit[(sel - 3):(sel + 3), ]
d[(sel - 3):(sel + 3), ]
taula$Elna <- d_corregit


## test$`Espirà de l’Aglí`
# CONCLUSIONS: Fila sense full cadastral que es fusiona amb l'anterior. TODO: REPORT
d <- taula$`Espirà de l’Aglí`
d[grep("^pag6\\.1", rownames(d)), ]
sel <- grep("^pag6\\.14$", rownames(d))
corregit <- d[sel, ]
corregit <- rbind(corregit, corregit)
corregit[1, ] <- gsub(
  " -$| CR nº8 de l'Ase$", "", corregit[1, ]
)
corregit[2, ] <- gsub(
  "^B2$|^- |^Chemin de la Juliette|^CR nº6 de la Jolieta ", "", corregit[2, ]
)
rownames(corregit)[2] <- "pag6.15"
corregit[1, 4] <- NA_character_
corregit[2, c(1, 2, 4)] <- NA_character_
corregit

d_corregit <- rbind(d[1:(sel - 1), ], corregit, d[(sel + 1):nrow(d), ])
d_corregit[(sel - 3):(sel + 3), ]
d[(sel - 3):(sel + 3), ]
taula$`Espirà de l’Aglí` <- d_corregit


## test$Estoer
# CONCLUSIONS: Fila sense full cadastral que es fusiona amb l'anterior. TODO: REPORT
d <- taula$Estoer
d[grep("^pag5\\.[0-9]$", rownames(d)), ]
sel <- grep("^pag5\\.4$", rownames(d))
corregit <- d[sel, ]
corregit <- rbind(corregit, corregit)
corregit[1, ] <- gsub(" -$", "", corregit[1, ])
corregit[2, ] <- gsub("^- ", "", corregit[2, ])
rownames(corregit)[2] <- "pag5.5"
corregit[1, 2:3] <- NA_character_
corregit[2, c(1, 2, 4)] <- NA_character_
corregit

d_corregit <- rbind(d[1:(sel - 1), ], corregit, d[(sel + 1):nrow(d), ])
d_corregit[(sel - 3):(sel + 3), ]
d[(sel - 3):(sel + 3), ]
taula$Estoer <- d_corregit


## test$Fontpedrosa
# CONCLUSIONS: Error tipogràfic amb 2 guions fora de lloc. TODO: REPORT
d <- taula$Fontpedrosa
d[grep("^pag8\\.[0-9]$", rownames(d)), ]
sel <- grep("^pag8\\.6$", rownames(d))
corregit <- d[sel, ]
corregit[, 2] <- gsub("--", "", corregit[, 2])
corregit

d[sel, ] <- corregit
taula$Fontpedrosa <- d


## test$Illa
# CONCLUSIONS: Fila sense full cadastral que es fusiona amb l'anterior. TODO: REPORT
d <- taula$Illa
d[grep("^pag7\\.1", rownames(d)), ]
sel <- grep("^pag7\\.17$", rownames(d))
corregit <- d[sel, ]
corregit <- rbind(corregit, corregit)
corregit[1, ] <- gsub(
  " -$| Mas Suzanna$", "", corregit[1, ]
)
corregit[2, ] <- gsub(
  "^AL$|^- |^Mas de l’Hospital ", "", corregit[2, ]
)
rownames(corregit)[2] <- "pag7.18"
corregit[1, 2] <- NA_character_
corregit[2, 1:3] <- NA_character_
corregit

d_corregit <- rbind(d[1:(sel - 1), ], corregit, d[(sel + 1):nrow(d), ])
d_corregit[(sel - 3):(sel + 3), ]
d[(sel - 3):(sel + 3), ]
taula$Illa <- d_corregit


## test$Matamala
# CONCLUSIONS: Fila sense full cadastral que es fusiona amb l'anterior. TODO: REPORT
d <- taula$Matamala
d[grep("^pag4\\.2", rownames(d)), ]
sel <- grep("^pag4\\.20$", rownames(d))
corregit <- d[sel, ]
corregit <- rbind(corregit, corregit)
corregit[1, ] <- gsub(
  " -$| Soula et Bac del Pleyt$| Solà del Pleit$", "", corregit[1, ]
)
corregit[2, ] <- gsub(
  "^B2$|^- |^Lou Soucarrat |^el Socarrat ", "", corregit[2, ]
)
rownames(corregit)[2] <- "pag4.21"
corregit[2, c(1, 4)] <- NA_character_
corregit

d_corregit <- rbind(d[1:(sel - 1), ], corregit, d[(sel + 1):nrow(d), ])
d_corregit[(sel - 3):(sel + 3), ]
d[(sel - 3):(sel + 3), ]
taula$Matamala <- d_corregit


## test$`Morellàs i les Illes`
# CONCLUSIONS: Fila sense full cadastral que es fusiona amb l'anterior. TODO: REPORT
d <- taula$`Morellàs i les Illes`
d[grep("^pag10\\.2", rownames(d)), ]
sel <- grep("^pag10\\.22$", rownames(d))
corregit <- d[sel, ]
corregit <- rbind(corregit, corregit)
corregit[1, ] <- gsub(
  " -$| el Rodon$", "", corregit[1, ]
)
corregit[2, ] <- gsub(
  "^163B1$|^- |^Riunoguers ", "", corregit[2, ]
)
rownames(corregit)[2] <- "pag10.23"
corregit[1, 2:3] <- NA_character_
corregit[2, 1:3] <- NA_character_
corregit

d_corregit <- rbind(d[1:(sel - 1), ], corregit, d[(sel + 1):nrow(d), ])
d_corregit[(sel - 3):(sel + 3), ]
d[(sel - 3):(sel + 3), ]
taula$`Morellàs i les Illes` <- d_corregit


## test$Oms
# CONCLUSIONS: Fila amb full cadastral començat amb / que es fusiona amb l'anterior. TODO: REPORT
d <- taula$Oms
d[grep("^pag7\\.3", rownames(d)), ]
sel <- grep("^pag7\\.30$", rownames(d))
corregit <- d[sel, ]
corregit <- rbind(corregit, corregit)
corregit[1, ] <- gsub(
  "/C1$| nom a corregir$| Ravin d'en Py$", "", corregit[1, ]
)
corregit[2, ] <- gsub(
  "^B2/|^nom a corregir |^Ravin d'en Pallary ", "", corregit[2, ]
)
rownames(corregit)[2] <- "pag7.31"
corregit[1, 4] <- NA_character_
corregit[2, 4] <- NA_character_
corregit

d_corregit <- rbind(d[1:(sel - 1), ], corregit, d[(sel + 1):nrow(d), ])
d_corregit[(sel - 3):(sel + 3), ]
d[(sel - 3):(sel + 3), ]
taula$Oms <- d_corregit


## test$Paçà
# CONCLUSIONS: Fila buida. TODO: REPORT
d <- taula$Paçà
d[grep("^pag3\\.[0-9]$", rownames(d)), ]
sel <- grep("^pag3\\.5$", rownames(d))
corregit <- d[sel, ]

corregit[1, 2:3] <- NA_character_
corregit

d[sel, ] <- corregit
taula$Paçà <- d


## test$`Prats de Molló`
# CONCLUSIONS: cel·la amb *- . TODO: REPORT
d <- taula$`Prats de Molló`
d[grep("^pag16\\.[0-9$]", rownames(d)), ]
sel <- grep("^pag16\\.6$", rownames(d))
taula$`Prats de Molló`[sel, 2]
taula$`Prats de Molló`[sel, 2] <- NA_character_


## test$`Sant Feliu d’Amunt`
# CONCLUSIONS: Fila sense full cadastral que es fusiona amb l'anterior. TODO: REPORT
d <- taula$`Sant Feliu d’Amunt`
d[grep("^pag3\\.[189]", rownames(d)), ]
sel <- grep("^pag3\\.9$", rownames(d))
corregit <- d[sel, ]
corregit <- rbind(corregit, corregit)
corregit[1, ] <- gsub(
  " -$| Agouille del Moulinas$| Agulla del Molinàs$", "", corregit[1, ]
)
corregit[2, ] <- gsub(
  "^C2$|^- |^Agouille de las Sitges |^Agulla de les Sitges ", "", corregit[2, ]
)
rownames(corregit)[2] <- "pag3.10"
corregit[1, 4] <- NA_character_
corregit[2, c(1, 4)] <- NA_character_
corregit

d_corregit <- rbind(d[1:(sel - 1), ], corregit, d[(sel + 1):nrow(d), ])
d_corregit[(sel - 3):(sel + 3), ]
d[(sel - 3):(sel + 3), ]
taula$`Sant Feliu d’Amunt` <- d_corregit


## test$Sornian
# CONCLUSIONS: Fila sense full cadastral que es fusiona amb l'anterior. TODO: REPORT
d <- taula$Sornian
d[grep("^pag9\\.[12]", rownames(d)), ]
sel <- grep("^pag9\\.19$", rownames(d))
corregit <- d[sel, ]
corregit <- rbind(corregit, corregit)
corregit[1, ] <- gsub(
  " -$| Ravin del Serrat d’en Grau$| Rèc del Serrat d’en Grau$", "", corregit[1, ]
)
corregit[2, ] <- gsub(
  "^B1/B2/F$|^- |^Ravin d’el Rial |^Rèc del Rial ", "", corregit[2, ]
)
rownames(corregit)[2] <- "pag9.20"
corregit[1, 4] <- NA_character_
corregit[2, c(1, 4)] <- NA_character_
corregit

d_corregit <- rbind(d[1:(sel - 1), ], corregit, d[(sel + 1):nrow(d), ])
d_corregit[(sel - 3):(sel + 3), ]
d[(sel - 3):(sel + 3), ]
taula$Sornian <- d_corregit


## test$Teulís
# CONCLUSIONS: Fila full cadastral començat amb / que es fusiona amb l'anterior. TODO: REPORT
d <- taula$Teulís
d[grep("^pag4\\.1", rownames(d)), ]
sel <- grep("^pag4\\.13$", rownames(d))
corregit <- d[sel, ]
corregit <- rbind(corregit, corregit)
corregit[1, ] <- gsub(
  "/A2/B1$| -$| Rivière de Saint-Marsal$| Ribera de Sant Marçal$", "", corregit[1, ]
)
corregit[2, ] <- gsub(
  "^B2/|^- |^Fontaine |^la Font ", "", corregit[2, ]
)
rownames(corregit)[2] <- "pag4.14"
corregit[, 4] <- NA_character_
corregit

d_corregit <- rbind(d[1:(sel - 1), ], corregit, d[(sel + 1):nrow(d), ])
d_corregit[(sel - 3):(sel + 3), ]
d[(sel - 3):(sel + 3), ]
taula$Teulís <- d_corregit


## test$Torrelles
# CONCLUSIONS: Files buides que es fusiona amb l'anterior. TODO: REPORT
d <- taula$Torrelles
d[grep("^pag3\\.2", rownames(d)), ]
sel <- grep("^pag3\\.21$", rownames(d))

d[sel, 4]
d[sel, 4] <- gsub(" - -$", "", d[sel, 4])
taula$Torrelles <- d


## test$Trevilhac
# CONCLUSIONS: Fila amb línies subtilment desplaçades que es fusiona amb l'anterior. TODO: REPORT
d <- taula$Trevilhac
d[grep("^pag8\\.3", rownames(d)), ]
sel <- grep("^pag8\\.3[14]$", rownames(d))
corregit <- d[sel, ]
corregit[1, 2] <- NA_character_
corregit[2, 2] <- "Chemin/CR dit de la Courtalasse"
corregit

d[sel, ]
d[sel, ] <- corregit
taula$Trevilhac <- d


## TODO: possibles errors pendents no detectables amb guions duplicats en files sense guions (cel·les buides)


## Corregeix parèntesis desaparellats ----

## Amb parèntesis
valors <- unlist(lapply(taula, function(x) {
  gsub(
    ".*(\\(.+\\))", "\\1",
    # x[apply(x, 1, function(y) any(grepl("\\(|\\)", y))), apply(x, 2, function(y) any(grepl("\\(|\\)", y)))]
    grepv("\\(|\\)", unlist(x))
  )
}))
table(valors)
grepv(")$", valors, invert = TRUE)

pars2 <- unlist(lapply(taula, function(x) { # 2 parentesis iguals
  unique(c(grepv("\\(.*\\(", unlist(x)), grepv("\\).*\\)", unlist(x))))
}))
pars2
## CONCLUSIÓ: només hi ha un cas amb més d'un grup de parèntesis i és un error a corregir
sel <- apply(taula$`Argelers de la Marenda`, 1, function(x) any(grepl("Ansa", x)))
taula$`Argelers de la Marenda`[sel, ]
val <- taula$`Argelers de la Marenda`$`NOM SOBRE GEOPORTAIL (portal cartogràfic de l’IGN)`[sel]
taula$`Argelers de la Marenda`$`NOM SOBRE GEOPORTAIL (portal cartogràfic de l’IGN)`[sel] <- gsub("^\\(", "", val)


### Parèntesis no aparellats ----

test <- lapply(taula, function(x) {
  sel_despar <- apply(x, 1, function(ch) {
    any(grepl("\\(", ch) != grepl("\\)", ch))
  })
  x[sel_despar, ]
})
test <- test[sapply(test, nrow) > 0]
test
## CONCLUSIÓ: falta tancar els parèntesis en els pdf. Corregir
# TODO: REPORT `el Barcarès` pag3.5 "Pointe de Coudalère (Punta de Codalera"
# TODO: REPORT `la Tor de Querol` pag5.10 "Riu Tartarès (Ravin"
# TODO: REPORT Portvendres pag5.5 "Mas Rovira (Mas d’en Rovira"

taula$`el Barcarès`["pag3.5", "NOM SOBRE GEOPORTAIL (portal cartogràfic de l’IGN)"]
taula$`el Barcarès`["pag3.5", "NOM SOBRE GEOPORTAIL (portal cartogràfic de l’IGN)"] <-
  paste0(taula$`el Barcarès`["pag3.5", "NOM SOBRE GEOPORTAIL (portal cartogràfic de l’IGN)"], ")")

taula$`la Tor de Querol`["pag5.10", "NOM SOBRE EL CADASTRE ACTUAL"]
taula$`la Tor de Querol`["pag5.10", "NOM SOBRE EL CADASTRE ACTUAL"] <-
  paste0(taula$`la Tor de Querol`["pag5.10", "NOM SOBRE EL CADASTRE ACTUAL"], ")")

taula$Portvendres["pag5.5", "NOM SOBRE GEOPORTAIL (portal cartogràfic de l’IGN)"]
taula$Portvendres["pag5.5", "NOM SOBRE GEOPORTAIL (portal cartogràfic de l’IGN)"] <-
  paste0(taula$Portvendres["pag5.5", "NOM SOBRE GEOPORTAIL (portal cartogràfic de l’IGN)"], ")")


## TODO: repassar caràcters especials i corregir ----

valors <- grepv("[^a-zA-ZÀÁÈÉÍÏÓÒÚÜÇàáèéíïóòúüç0-9 ()']", unlist(lapply(taula, function(x) x[, -1])))
sort(table(valors), decreasing = TRUE)[1:100]
unique(valors)

sort(table(unlist(stringr::str_extract_all(unlist(taula), "[^a-zA-ZÀÁÈÉÍÏÓÒÚÜÇàáèéíïóòúüç0-9 ()']"))))

valors <- grepv("[^a-zA-ZÀÁÈÉÍÏÓÒÚÜÇàáèéíïóòúüç0-9 ()'/º:âêëîôûùýÿ-]", unlist(lapply(taula, function(x) x[, -1])))
# sort(table(valors))
valors <- unique(valors)
valors
grepv("\\.", valors, invert = TRUE)

## Caràcters francesos inclosos (repassar per columnes)
valors <- apply(do.call(rbind, lapply(taula, as.matrix)), 2, function(x) grepv("[âêîôûùýÿ]", x))
unique(valors)


## caràcters especial a inici o fi
grepv("[^a-zA-ZÀÁÈÉÍÏÓÒÚÜÇàáèéíïóòúüç0-9 ()']$", unlist(lapply(taula, function(x) x[, -1])))
grepv("^[^a-zA-ZÀÁÈÉÍÏÓÒÚÜÇàáèéíïóòúüç0-9 ()']", unlist(lapply(taula, function(x) x[, -1])))
## CONCLUSIÓ: Elimina els que no són abreviacions

sel <- grep("Nahuja\\)\\.", taula$Naüja$`NOM SOBRE EL CADASTRE ACTUAL`)
taula$Naüja$`NOM SOBRE EL CADASTRE ACTUAL`[sel] <- gsub("\\.$", "", taula$Naüja$`NOM SOBRE EL CADASTRE ACTUAL`[sel])

sel <- grep("Pla de la Creu Verda", taula$Trasserra$`NOM SOBRE GEOPORTAIL (portal cartogràfic de l’IGN)`)
taula$Trasserra$`NOM SOBRE GEOPORTAIL (portal cartogràfic de l’IGN)`[sel] <-
  gsub("^\\^", "", taula$Trasserra$`NOM SOBRE GEOPORTAIL (portal cartogràfic de l’IGN)`[sel])



### Camps amb punt -> NA ----
test <- lapply(taula, function(x) {
  x[apply(x, 1, function(y) any(grepl("^\\.$", y))), ]
})
test <- test[sapply(test, nrow) > 0]
test
## CONCLUSIONS: són camps buits. Passa a NA
taula[names(test)] <- lapply(taula[names(test)], function(x) {
  x[] <- lapply(x, function(y) {
    y[grep("^\\.$", y)] <- NA_character_
    y
  })
  x
})


### Normalitza apòstrof ----
taula <- lapply(taula, function(x) {
  x[] <- lapply(x, function(y) gsub("[‘’]", "'", y))
  x
})


### Normalitza º ----
valors <- grepv("[°²ª]", unlist(lapply(taula, function(x) x[, -1])))
sort(table(valors), decreasing = TRUE)[1:100]
unique(valors)
# CONCLUSIONS: [Nn][°²ªº]-> nº

taula <- lapply(taula, function(x) {
  x[] <- lapply(x, function(y) gsub("[Nn][°²ª]", "nº", y))
  x
})


### Errors varis ----
test <- lapply(taula, function(x) x[apply(x, 1, function(y) any(grep("Salñ|,l'Alba|Pinosa,ou", y))), ])
test <- test[sapply(test, nrow) > 0]
test
# TODO: REPORT
# taula$Mentet["pag5.12", ] Pinosa,ou -> Pinosa, ou
# taula$Molig["pag3.4", ] -> "Salñ del Gaill" -> "Salt del Gaill"
# taula$Oms["pag5.7", ] "Mas de ,l'Alba" -> "Mas de l'Alba"

taula$Mentet["pag5.12", "NOM SOBRE EL CADASTRE ACTUAL CORREGIT"] <- gsub(
  " Pinosa,ou ",
  " Pinosa, ou ",
  taula$Mentet["pag5.12", "NOM SOBRE EL CADASTRE ACTUAL CORREGIT"]
)
taula$Molig["pag3.4", "NOM DEL CADASTRE ANTERIOR I SOBRE EL CADASTRE ACTUAL"] <- gsub(
  "Salñ del Gaill",
  "Salt del Gaill",
  taula$Molig["pag3.4", "NOM DEL CADASTRE ANTERIOR I SOBRE EL CADASTRE ACTUAL"]
)
taula$Oms["pag5.7", "NOM SOBRE GEOPORTAIL (portal cartogràfic de l’IGN)"] <- gsub(
  "Mas de ,l'Alba",
  "Mas de l'Alba",
  taula$Oms["pag5.7", "NOM SOBRE GEOPORTAIL (portal cartogràfic de l’IGN)"]
)


## Corregeix tipus multicolumna ----

tipus <- lapply(taula, function(x) x[apply(x[, -1], 1, function(y) all(is.na(y))), 1])

sort(table(unlist(tipus)))
unique(unlist(tipus))
unique(tipus)
grep("^(Carrers de Font- rabiosa|cadastrals|corregits)$", unlist(tipus), value = TRUE)

## Corregeix espai en blanc
sel <- grep("Carrers de Font- rabiosa", taula$`Font-rabiosa`[, 1])
taula$`Font-rabiosa`[sel, 1] <- "Carrers de Font-rabiosa"


test <- lapply(taula, function(x) {
  sel <- grep("^cadastrals$", x[, 1])
  x[sort(unique(c(sel, sel - 1, sel + 1))), ]
})
test <- test[sapply(test, nrow) > 0]
grep("corregit", unlist(tipus), value = TRUE)
# CONCLUSIONS: errors degut a notes en columnes 2:3 ("NOMS NO CORREGITS", "NOMS CORREGITS"). Afegir al tipus + no
# corregit/corregit
# NOMS CORREGIT inclou valors a la columna `NOM SOBRE EL CADASTRE ACTUAL CORREGIT` i NOMS NO CORREGITS només a Sant
# Esteve del Monestir

taula[names(test)] <- lapply(taula[names(test)], function(x) {
  sel <- grep("^cadastrals$", x[, 1])
  corregit <- sapply(sel, function(i) {
    out <- paste(x[c(i - 1, i), 1], collapse = " ")
    out <- paste(c(out, na.omit(as.character(x[i - 1, -1]))), collapse = " ")
  })
  x[sel - 1, 1] <- corregit
  x[sel - 1, -1] <- NA_character_
  x <- x[-sel, ]
  x
})


## Afegeix columna amb tipus ----

taula <- lapply(taula, function(x) {
  sel_tipus <- which(apply(x[, -1], 1, function(y) all(is.na(y))))
  tipus <- x[sel_tipus, 1]
  reps <- c(sel_tipus[2] - 1, diff(c(sel_tipus[-1], nrow(x) + 1)))
  # tmp <- data.frame(tipus, sel_tipus, times = reps); tmp$pos <- cumsum(tmp$times); tmp
  cbind(x, tipus = c(rep(tipus, times = reps)))
})
# lapply(taula, function(x) x[which(x[, 1] == x[, 5]), c(1, 5)])


## Elimina files de tipus (ja són a la columna)
taula <- lapply(taula, function(x) x[-which(x[, 1] == x[, 5]), ])


# FET! Punt probablement segur ----

taula2 <- taula
# save(taula2, file = "data/part/taula2.RData", compress = "xz")
load("data/part/taula2.RData", verbose = TRUE) # taula2
taula <- taula2


## Divideix taules amb capçaleres intermèdies ----
test <- lapply(taula, function(x) grepv("^TOP", x[, 1]))
test <- test[sapply(test, length) > 0]
unique(test)

capçalera <- lapply(taula, function(x) {
  x[grep("^TOPÒNIMS", x[, 1]), ]
})
capçalera <- capçalera[sapply(capçalera, nrow) > 0]
capçalera
noms_col <- lapply(capçalera, function(x) {
  matrix(c(names(x)[-5], x[, -5]), ncol = 4, byrow = TRUE, dimnames = list(c("capçalera", "intermitja"), 1:4))
})
apply(do.call(rbind, noms_col), 2, unique)

lapply(noms_col, function(x) x[, 2:3])
## CONCLUSIONS: la primera capçalera inclou noms presents als cadastres, la segona conté propostes de canvis

taules_compostes <- mapply(function(x, nom) {
  sel <- grep("^TOPÒNIMS", x[, 1])
  out <- x[(sel + 1):nrow(x), ]
  names(out) <- c(x[sel, -5], "tipus")

  out <- list(x[1:(sel - 1), ], out)
  names(out) <- paste0(nom, "_", c("CADASTRE", "PROPOSAT"))
  out
}, x = taula[names(capçalera)], nom = names(capçalera), SIMPLIFY = FALSE)
str(taules_compostes)

data.frame(names(taula), sort(names(taula)))[names(taula) != sort(names(taula)), ]
## CONCLUSIONS: l'ordre dels municipis no canvia massa

taules_partides <- do.call(c, taules_compostes)
names(taules_partides) <- sapply(strsplit(names(taules_partides), "\\."), function(x) x[2])


table(taula$`la Roca de l’Albera`$tipus)
## CONCLUSIONS: a la Roca de l'Albera tipus divideix la taula en topònims corregits i no corregits
taula_partida <- split(taula$`la Roca de l’Albera`, !grepl("NOMS CORREGITS$", taula$`la Roca de l’Albera`$tipus))
names(taula_partida) <- paste0("la Roca de l’Albera_", c("CORREGIT", "NO CORREGIT"))
taula_partida$`la Roca de l’Albera_CORREGIT`$tipus <- gsub(
  " NOMS CORREGITS$", "", taula_partida$`la Roca de l’Albera_CORREGIT`$tipus
)
taula_partida$`la Roca de l’Albera_NO CORREGIT`$tipus <- gsub(
  " NOMS NO CORREGITS$", "", taula_partida$`la Roca de l’Albera_NO CORREGIT`$tipus
)
names(taula_partida$`la Roca de l’Albera_NO CORREGIT`)[3] <- "NOM SOBRE EL CADASTRE ACTUAL NO CORREGIT"


taules_partides <- c(taules_partides, taula_partida)


taula <- c(taula[!names(taula) %in% c(names(taules_compostes), "la Roca de l’Albera")], taules_partides)
taula <- taula[order(names(taula))]


## Unifica noms de columnes ----

lapply(1:5, function(col) {
  sort(unique(unlist(lapply(taula, function(x) names(x)[col]))))
})
sort(unique(unlist(lapply(taula, function(x) names(x)[2:3]))))
sort(unique(unlist(lapply(taula, function(x) names(x)[c(1, 4)]))))
unique(lapply(taula, function(x) names(x)[2:4]))
taula <- lapply(taula, function(x) {
  names(x)[1] <- "TOPÒNIMS I FULLS CADASTRALS"

  names(x)[2] <- gsub("^NOM SUR LE CADASTRE ACTUEL$", "NOM SOBRE EL CADASTRE ACTUAL", names(x)[2])
  names(x)[2] <- gsub("^NOM SUR LE CADASTRE ANTÉRIEUR$", "NOM SOBRE EL CADASTRE ANTERIOR", names(x)[2])

  names(x)[3] <- gsub("^NOM À CORRIGER SUR LE CADASTRE$", "NOM A CORREGIR SOBRE EL CADASTRE", names(x)[3])
  names(x)[3] <- gsub("^NOM SUR LE CADASTRE ACTUEL CORRIGÉ$", "NOM SOBRE EL CADASTRE ACTUAL CORREGIT", names(x)[3])

  names(x)[4] <- "NOM SOBRE GÉOPORTAIL (portal cartogràfic de l’IGN)"
  x
})


## Canvia noms de la llista pels del pdf corregits ----

load("data/comarques.rda", verbose = TRUE) # comarques

lapply(comarques[, c("municipi", "municipi_pdf", "nom_fitxer")], function(x) table(names(taula) %in% x))
setdiff(names(taula), comarques$nom_fitxer)

noms_corregits <- comarques$municipi[match(gsub("_.+", "", names(taula)), comarques$nom_fitxer)]
sel <- grep("_", names(taula))
noms_corregits[sel] <- paste0(noms_corregits[sel], "_", sapply(strsplit(names(taula)[sel], "_"), function(x) x[2]))
tmp <- data.frame(noms_corregits, names(taula))
tmp[tmp[[1]] != tmp[[2]], ]

names(taula) <- noms_corregits


# FET! Punt probablement segur ----

taula3 <- taula
# save(taula3, file = "data/part/taula3.RData", compress = "xz")
load("data/part/taula3.RData", verbose = TRUE) # taula3
taula <- taula3


## Desa ----

becat_cadastre <- taula

identical(becat_cadastre, JoanBeCatNord.osm::becat_cadastre)
# igual nrow
table(sapply(names(becat_cadastre), function(x) {
  nrow(becat_cadastre[[x]]) == nrow(JoanBeCatNord.osm::becat_cadastre[[x]])
}))

dif_vals <- lapply(names(becat_cadastre), function(x) {
  list(
    dif1 = sort(setdiff(unlist(becat_cadastre[[x]]), unlist(JoanBeCatNord.osm::becat_cadastre[[x]]))),
    dif2 = sort(setdiff(unlist(JoanBeCatNord.osm::becat_cadastre[[x]]), unlist(becat_cadastre[[x]])))
  )
})
dif_vals[sapply(dif_vals, function(x) length(x[[1]]) != length(x[[2]]))]

dif <- lapply(names(becat_cadastre), function(x) {
  try(compareDF::compare_df(
    becat_cadastre[[x]],
    JoanBeCatNord.osm::becat_cadastre[[x]] # , group_col = "osm_id"
  ))
})
dif <- dif[sapply(dif, class) != "try-error"]
# dif
compareDF::view_html(dif[[1]])

table(unlist(sapply(becat_cadastre, sapply, Encoding)))
table(unlist(sapply(JoanBeCatNord.osm::becat_cadastre, sapply, Encoding)))


usethis::use_data(becat_cadastre, overwrite = TRUE)

becat_xlsx <- becat_cadastre
names(becat_xlsx) <- substr(names(becat_xlsx), 1, 31)
becat_xlsx <- lapply(becat_xlsx, function(x) {
  names(x) <- gsub(" ", "_", names(x))
  names(x) <- gsub("\\(|\\)", "", names(x))
  x
})

openxlsx::write.xlsx(
  becat_xlsx,
  file = "data-raw/becat_cadastre.xlsx", rowNames = TRUE, borders = "surrounding", colWidths = "auto",
  firstRow = TRUE, headerStyle = openxlsx::createStyle(textDecoration = "BOLD")
)



