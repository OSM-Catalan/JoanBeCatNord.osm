load("data/comarques.rda", verbose = TRUE) # comarques

comarques_becat <- sort(unique(comarques$comarca))
grep(" ", comarques_becat, value = TRUE)

comarques_osm <- monitorOSM::comarques[monitorOSM::comarques$regio == "CatNord", ]
comarques_osm <- comarques_osm[, c("name:ca", "osm_type", "osm_id")]
names(comarques_osm) <- gsub("^name:ca$", "osm_name", names(comarques_osm))

tesaurus_comarques <- data.frame(
  becat_nom = grep(" |^Fenolhedés", comarques_becat, value = TRUE, invert = TRUE),
  comarques_osm[c(2, 1, 3:5), ]
)

pendents <- setdiff(comarques_becat, tesaurus_comarques$becat_nom)
pendents <- c(pendents, pendents[2])
pendents <- data.frame(becat_nom = pendents, rbind(rep(NA, ncol(comarques_osm)), comarques_osm[4:5, ]))

tesaurus_comarques <- rbind(tesaurus_comarques, pendents)
rownames(tesaurus_comarques) <- NULL

tesaurus_comarques[tesaurus_comarques$becat_nom == "Fenolhedés", ]
tesaurus_comarques[
  tesaurus_comarques$becat_nom == "Fenolhedés", c("osm_name", "osm_type", "osm_id")
] <- c("Fenolledès", "relation", "16780160")


## Desa ----

usethis::use_data(tesaurus_comarques, overwrite = TRUE)

load("data/tesaurus_comarques.rda", verbose = TRUE) # tesaurus_comarques
