## Chargement des packages
pacman::p_load(data.table, ggplot2, ggpubr, magrittr, descr, forcats,
               kableExtra, R.temis, stringi, viridis,
               scales, weights, modelsummary, lubridate)
theme_set(theme_minimal())
options(descr.plot = F)

## Fonctions usuelles
source("fonctions.R")

## Couleurs 
plotcolours <- magma(4, begin = 0.2, end = 0.8)
sexcolours <- magma(2, begin = 0.45, end = 0.8)

## Bases ----

path = "~/Desktop/M2/Mémoire/Data/"
repondant <- fread(paste0(path, "repondant.csv"))
histoire <- fread(paste0(path, "histoire.csv"))
rm(path)

setnames(repondant, tolower(names(repondant)))
setnames(histoire, tolower(names(histoire)))

# Passage des identifiants en caractères
repondant[, ident_men := as.character(ident_men)]
histoire[, ident_men := as.character(ident_men)]

# Création d'une pondération ramenée à l'effectif de l'échantillon
repondant[, pond := poids/mean(poids)]


################################################################################
############################# Recodages ########################################
################################################################################

## CS à la passation
repondant[, pcsr := rec_pcs(m_cs42)]

## Niveau d'études
repondant[, niv_dipl := rec_niv(ager, m_diplome, m_dipl5), by = ident_men]
histoire[repondant, niv_dipl := i.niv_dipl, on = "ident_men"]

# Age révolu en quatre classes
repondant[, ager_4cl := fcase(ager >= 25 & ager <= 34, 1L,
                              ager >= 35 & ager <= 44, 2L,
                              ager >= 45 & ager <= 54, 3L,
                              ager >= 55 & ager <= 65, 4L)]

# Rang de la relation en quatre classes
histoire[, h_nrel_4cl := fifelse(h_nrel_corr > 4, 4, h_nrel_corr)]

# Variable du nombre de relations corrigée
repondant[histoire[, .N, by = ident_men], 
          h_nbrel_corr := i.N, on = "ident_men"]

## Nombre de relations
repondant[, h_nbrel_5cl := fcase(is.na(h_nbrel_corr), 0L,
                                 h_nbrel_corr <= 4, h_nbrel_corr,
                                 h_nbrel_corr > 4, 4L)]

## Nombre de partenaires sexuel·les
repondant[, h_nbpart_4cl := fcase(h_nbpart_imp <= 1, 1,
                                  h_nbpart_imp > 1 & h_nbpart_imp <= 4, 2,
                                  h_nbpart_imp > 4 & h_nbpart_imp <= 9, 3,
                                  h_nbpart_imp >= 10, 4)]

# Date de naissance de l'enquêté·e
repondant[, date_nais := ym(paste(anaisr, mnaisr))]

# Durée de la relation en mois
histoire[, `:=`(date_deb = ym(paste(h_adeb_imp, h_mdeb_imp)),
                date_fin = fifelse(h_fin == 1, ym(paste(2014, 1)),
                                   ym(paste(h_afin_imp, h_mfin_imp))))]
histoire[, duree_m := as.numeric(as.period(interval(date_deb, date_fin)), "months")]

histoire[, duree_5cl := fcase(duree_m < 12, 1,
                              duree_m >= 12 & duree_m < 4*12, 2,
                              duree_m >= 4*12 & duree_m < 10*12, 3,
                              duree_m >= 10*12 & duree_m < 25*12, 4,
                              duree_m >= 25*12, 5)]

# Âge de l'enquêté·e en début de relation 
cols <- c("anaisr", "mnaisr")
histoire[repondant, c(cols) := mget(cols), on = "ident_men"]
histoire[, date_nais := ym(paste(anaisr, mnaisr))
         ][, ager_deb := as.integer(as.numeric(as.period(interval(date_nais, date_deb)), "years"))]

histoire[, ager_deb_4cl := fcase(ager_deb < 18, 1,
                                 ager_deb >= 18 & ager_deb <= 21, 2, 
                                 ager_deb >= 22 & ager_deb <= 27, 3,
                                 ager_deb >= 28, 4)]

# Année de naissance du ou de la conjointe corrigée et âge à la passation
histoire[, h_anaisc_corr := fcase(h_anaisc != 9999, h_anaisc,
                                  h_agec == 1, anaisr,
                                  h_agec == 2, anaisr + 3L,
                                  h_agec == 3, anaisr + 6L,
                                  h_agec == 4, anaisr - 3L,
                                  h_agec == 5, anaisr - 6L)]
histoire[, agec_deb := h_adeb_imp - h_anaisc_corr]
histoire[agec_deb <= 5, `:=`(agec_deb = NA, h_anaisc_corr = NA)]
histoire[, agec := 2014 - h_anaisc_corr]

### Passage de valeurs aberrantes en NA
histoire[ident_men == "2100069201000" & h_nrel_corr == 1,
         `:=`(agec_deb = NA, ecart_age = NA)] 
histoire[ident_men == "2300007701000" & h_nrel_corr == 2,
         `:=`(agec_deb = NA, ecart_age = NA)]
histoire[ident_men == "1100034804000" & h_nrel_corr == 3,
         `:=`(agec_deb = NA, ecart_age = NA)]

repondant[histoire, c_agec := i.agec, on = c("ident_men", h_nrel_corr_c = "h_nrel_corr")]

## Écart de taille
repondant[, ecart_taille := fifelse(is.na(c_taille) | is.na(c_taillec) |
                                      c_taille == 999 | c_taillec == 999, 
                                    NA_integer_, c_taille - c_taillec)]

# Variable de goûts amoureux
repondant[, r_plugd := fcoalesce(r_plugd10, r_plugd5)]
repondant[, r_pluptt := fcoalesce(r_pluptt10, r_pluptt5)]
cols <- paste0("r_", c("pluage", "plujeun", 
                       "plugd10", "plugd5",
                       "pluptt10", "pluptt5",
                       "plugd", "pluptt", 
                       "pludipl", "moindipl", 
                       "oppoldif"))
repondant[, paste0(cols, "_2cl") :=
            lapply(.SD, function(v) fcase(v %in% 1:2, 1L,
                                          v %in% 3, 0L,
                                          v == 9, 9L)), .SDcols = cols]

# Accepter un couple où la femme est plus grande
repondant[, femplugde := fcase(sexer == 1 & r_plugd_2cl == 1 |
                                 sexer == 2 & r_pluptt_2cl == 1, 1L, 
                               sexer == 1 & r_plugd_2cl == 0 |
                                 sexer == 2 & r_pluptt_2cl == 0, 0L,
                               default = NA_integer_)]

# Accepter un couple où la femme est plus âgée
repondant[, fempluage := fcase(sexer == 1 & r_pluage_2cl == 1 |
                                 sexer == 2 & r_plujeun_2cl == 1, 1L, 
                               sexer == 1 & r_pluage_2cl == 0 |
                                 sexer == 2 & r_plujeun_2cl == 0, 0L,
                               default = NA_integer_)]

# Ajout du sexe et de l'âge dans la base histoire
cols <- c("sexer", "ager", "ager_4cl")
histoire[repondant, c(cols) := mget(cols), on = "ident_men"]


################################################################################
################################### Formats ####################################
################################################################################
formats <- list()

formats$sexer <- c("Homme", "Femme")
formats$ager_4cl <- paste(c("25-34", "35-44", "45-54", "55-65"), "ans")

formats$pcsr <- c("Cadre", "Intermédiaire", "Petit·e indépendant·e",
                  "Employé·e", "Ouvrier·ère", "Inactif·ve")

formats$niv_dipl <- paste("Niveau", 1:4)

formats$h_nrel_4cl <- paste("Relation", c(1:3, "4 ou plus"))
formats$h_nbrel_5cl <- c("Aucune", "Une", "Deux", "Trois", "Quatre ou plus")

formats$h_nbpart_4cl <- c("0 ou 1", "2 à 4", "5 à 9", "10 ou plus")

formats$ager_deb_4cl <- paste(c("-18", "18-21", "22-26", "27+"), "ans")

formats$duree_5cl <- c("Moins de 1 ans",
                       "1-3 ans",
                       "4-9 ans",
                       "10-25 ans",
                       "25 ans et plus")

formats$h_fin <- c("En cours", "Finie")


formats$r_attiphy  <- c("Ça compte surtout quand on est jeune",
                        "Ça compte à tous les âges",
                        "Ça n'est jamais très important",
                        "Ne sait pas")

formats$r_pluage <- c("Oui, c'est justement mon cas",
                      "Oui", "Non", "Ne sait pas")

formats[paste0("r_", c("plujeun", 
                       "plugd10", "plugd5", 
                       "pluptt10", "pluptt5",
                       "plugd", "pluptt",
                       "pludipl", "moindipl",
                       "oppoldif"))] <- rep(list(formats$r_pluage), 10)

formats[paste0(c("pluage", "plujeun", 
                 "plugd10", "plugd5",
                 "pluptt10", "pluptt5",
                 "plugd", "pluptt", 
                 "pludipl", "moindipl", 
                 "oppoldif"), "_2cl")] <- rep(list(c("Non", "Oui", "Ne sait pas")), 11)

rm(cols)


