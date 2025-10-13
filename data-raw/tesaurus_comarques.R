load("data/comarques.rda", verbose = TRUE) # comarques

comarques_becat <- sort(unique(comarques$comarca))
grep(" ", comarques_becat, value = TRUE)

comarques_osm <- monitorOSM::comarques[monitorOSM::comarques$regio == "CatNord", ]
comarques_osm <- comarques_osm[, c("name:ca", "osm_type", "osm_id", "wikidata")]
names(comarques_osm) <- gsub("^name:ca$", "osm_name", names(comarques_osm))

tesaurus_comarques <- data.frame(
  becat_nom = grep(" ", comarques_becat, value = TRUE, invert = TRUE),
  comarques_osm[c(2, 1, 3:6), ]
)

pendents <- setdiff(comarques_becat, tesaurus_comarques$becat_nom)
pendents <- rep(pendents, 2)
pendents <- data.frame(becat_nom = pendents, comarques_osm[5:6, ])

tesaurus_comarques <- rbind(tesaurus_comarques, pendents)
rownames(tesaurus_comarques) <- NULL


d_antic <- JoanBeCatNord.osm::tesaurus_comarques
sel_cols <- intersect(names(tesaurus_comarques), names(d_antic))
compareDF::view_html(compareDF::compare_df(tesaurus_comarques[, sel_cols], d_antic[, sel_cols], group_col = "osm_id"))


## Desa ----

# usethis::use_data(tesaurus_comarques, overwrite = TRUE)
load("data/tesaurus_comarques.rda", verbose = TRUE) # tesaurus_comarques

openxlsx::write.xlsx(
  tesaurus_comarques,
  file = "data-raw/tesaurus_comarques.xlsx", rowNames = FALSE, borders = "surrounding", colWidths = "auto",
  firstRow = TRUE, headerStyle = openxlsx::createStyle(textDecoration = "BOLD")
)
