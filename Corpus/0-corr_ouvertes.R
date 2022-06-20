source("Scripts/import_epic.R")

# Importation de la base des questions ouvertes
ouvertes <- fread("Data/quest_ouvertes_rep.csv"
                  )[, setnames(.SD, names(.SD), tolower(names(.SD)))
                    ][, ident_men := as.character(ident_men)]

# Nombre de relations décrites par enquêté·e
histoire[, nbrel := .N, by = ident_men]

# Le nombre de relations dans les deux bases n'est pas toujours égal
repondant[, freq(h_nbrel)]
histoire[, freq(nbrel)]

# Nombre de relation incorrect dans la base répondant pour 197 observations.
ids <- histoire[histoire[repondant[!(h_dejarel %in% 2),], 
                         nbrel != h_nbrel, on = "ident_men"], unique(ident_men)]
histoire[repondant[ident_men %in% ids], on = "ident_men"
         ][, .(relations_theorique = paste(1:h_nbrel, collapse = ":"), 
               relations = paste(h_nrel_corr, collapse = ":")), by = ident_men]

# On crée une nouvelle variable corrigée dans la base répondant
repondant[histoire, h_nbrel_corr := nbrel, on = "ident_men"]
repondant[, table(h_nbrel, h_nbrel_corr, useNA = "ifany")]
repondant[, freq(h_nbrel_corr)]


## Corrections h_plu9 et h_plu10

ouvertes[repondant, nbrel := i.h_nbrel_corr, on = "ident_men"]
cols <- grep("h_plu", names(ouvertes), value = T)

# Quand nbrel < 9 et h_plu9 != "" et h_plu10 == "", on assigne la valeur de 
# h_plu9 à h_plu{nbrel}
ids <- ouvertes[h_plu9 != "" & h_plu10 == "" & nbrel < 9, ident_men]
cbind(ouvertes[ident_men %in% ids, lapply(.SD, `!=`, ""), .SDcols = cols], 
      ouvertes[ident_men %in% ids, nbrel])
for(id in ids) {
  col <- ouvertes[ident_men == id, cols[nbrel]]
  ouvertes[ident_men == id, c(col) := h_plu9]
  ouvertes[ident_men == id, h_plu9 := ""]
}

# Quand h_plu9 != "" et h_plu10 != "", on assigne la valeur de h_plu9 à h_plu{nbrel-1}
# et la valeur de h_plu10 à h_plu{nbrel}
ids <- ouvertes[h_plu9 != "" & h_plu10 != "", ident_men]
cbind(ouvertes[ident_men %in% ids, lapply(.SD, `!=`, ""), .SDcols = cols], 
      ouvertes[ident_men %in% ids, nbrel])
for(id in ids) {
  col <- ouvertes[ident_men == id, cols[nbrel-1]]
  ouvertes[ident_men == id, c(col) := h_plu9]
  ouvertes[ident_men == id, h_plu9 := ""]
  col <- ouvertes[ident_men == id, cols[nbrel]]
  ouvertes[ident_men == id, c(col) := h_plu10]
  ouvertes[ident_men == id, h_plu10 := ""]
}

# Quand h_plu9 == "" & h_plu10 != "", on assigne la valeur de h_plu10 à h_plu{nbrel}
ids <- ouvertes[h_plu10 != "", ident_men]
cbind(ouvertes[ident_men %in% ids, lapply(.SD, `!=`, ""), .SDcols = cols], 
      ouvertes[ident_men %in% ids, nbrel])
for(id in ids) {
  col <- ouvertes[ident_men == id, cols[nbrel]]
  ouvertes[ident_men == id, c(col) := h_plu10]
  ouvertes[ident_men == id, h_plu10 := ""]
}

# Après correction, une réponse ou plus est vide pour 144 enquêté·es (1,9%)
ouvertes[!is.na(nbrel), freq(rowSums(.SD != "") != nbrel), .SDcols = cols]

# Exportation d'une nouvelle base corrigée
# ouvertes[, h_plu10 := NULL]
# fwrite(ouvertes, "Data/ouvertes_corr.csv")


