## Extreu municipis, url i url dels pdf de la web ----
req <- httr2::request("https://joanbecat.cat/ca/catalunya-nord/municipis/")
resp <- httr2::req_perform(req)
html <- httr2::resp_body_html(resp)

xml2::html_structure(html)
mun <- xml2::xml_find_all(html, xpath = './/h3[@class="gdlr-core-blog-title gdlr-core-skin-title"]')
cat(as.character(mun))
url_mun <- xml2::xml_children(mun)
url <- xml2::xml_attr(url_mun, attr = "href")
municipi <- xml2::xml_text(url_mun)


url_pdf <- sapply(url, function(x) {
  html <- httr2::request(x) |> httr2::req_perform() |> httr2::resp_body_html()
  urls <- xml2::xml_find_all(html, xpath = './/a')
  url_pdf <- grep("\\.pdf", as.character(urls), value = TRUE)

  if (length(url_pdf) > 1) warning("Hi ha més d'una URL de pdf a ", x)
  if (length(url_pdf) == 0) {
    warning("No hi ha cap URL de pdf a ", x)
    return(NA_character_)
  }

  regmatches(url_pdf, gregexpr("https://joanbecat\\.cat/.+\\.pdf", url_pdf))[[1]]
})
municipi <- stringi::stri_trans_nfc(municipi)
fitxers_cadastre <- data.frame(municipi, url, url_pdf)
fitxers_cadastre <- fitxers_cadastre[order(fitxers_cadastre$municipi), ]


dup <- duplicated(fitxers_cadastre)
fitxers_cadastre[dup, ]
fitxers_cadastre <- unique(fitxers_cadastre)

fitxers_cadastre[apply(fitxers_cadastre, 1, function(x) any(is.na(x))), ]
# REPORTED: l'enllaç del pdf de Vilafranca de Conflent apunta a la url del poble ----
# REPORTED: l'enllaç al pdf és erroni ----
fitxers_cadastre$url_pdf[fitxers_cadastre$municipi == "Corbera la Cabana"] <- "https://joanbecat.cat/wp-content/uploads/2019/07/cat-Web-jbk-cadastre_Corbera_la_Cabana.pdf"

d_antic <- JoanBeCatNord.osm::fitxers_cadastre
compareDF::view_html(compareDF::compare_df(fitxers_cadastre, d_antic))


# REPORTED: pdf en francès per Calce, Espirà de l’Aglí i Jújols ----
fitxers_cadastre[grep("/fr", fitxers_cadastre$url_pdf), ]


## Descarrega pdf ----
dir.create("data-raw/pdf/nou", showWarnings = FALSE, recursive = TRUE)

for (i in seq_along(fitxers_cadastre$url_pdf)) {
  message(i, " / ", nrow(fitxers_cadastre), " ", fitxers_cadastre$municipi[i])

  fitxer <- paste0("data-raw/pdf/nou/", fitxers_cadastre$municipi[i], ".pdf")
  if (is.na(fitxers_cadastre$url_pdf[i]) | file.exists(fitxer)) next

  download.file(fitxers_cadastre$url_pdf[i], destfile = fitxer)
}


## Calcula md5sum ----

fitxers <- dir("data-raw/pdf", pattern = "\\.pdf$", full.names = TRUE)
fitxers_nous <- dir("data-raw/pdf/nou", pattern = "\\.pdf$", full.names = TRUE)
md5sum <- tools::md5sum(fitxers)
md5sum_nous <- tools::md5sum(fitxers_nous)

## Compara fitxers nous amb els antics
names(md5sum_nous) <-  gsub("/nou/", "/", names(md5sum_nous))
comp <- merge(md5sum, md5sum_nous, by = 0)
table(comp[[2]] == comp[[3]])
setdiff(names(md5sum), names(md5sum_nous))
setdiff(names(md5sum_nous), names(md5sum))

## Afegeix md5sum dels fitxers
names(md5sum) <- gsub("^data-raw/pdf/|\\.pdf$", "", names(md5sum))
fitxers_cadastre <- merge(fitxers_cadastre, md5sum, by.x = "municipi", by.y = 0, all = TRUE)
row.names(fitxers_cadastre) <- NULL

# usethis::use_data(fitxers_cadastre, overwrite = TRUE)
load("data/fitxers_cadastre.rda", verbose = TRUE) # fitxers_cadastre
