library(data.table)
library(R.temis)
library(tidyverse)
library(stringi)

# Importation de la base des questions ouvertes
ouvertes <- fread("Data/ouvertes_corr.csv")[, ident_men := as.character(ident_men)]
setnames(ouvertes, names(ouvertes), tolower(names(ouvertes)))

# Importation de la base lexicale lexique 3
lexique3 <- fread("Data/Lexique383.tsv")
lexique3[, ortho_sans := stri_trans_general(lexique3$ortho, id = "Latin-ASCII")]

# Création du corpus à partir des variables "h_plu3" à "h_plu9"
vars <- paste0("h_plu", 3:9)
dt_plu <- ouvertes[, setNames(.SD, c("doc_id", vars)), 
                   .SDcols = c("ident_men", vars)] %>%
  melt(measure.vars = vars, value.name = "text")
dt_plu[, text := tolower(stri_trans_general(text, id = "Latin-ASCII"))] 

corpus <- VCorpus(DataframeSource(dt_plu[, .(doc_id, text, variable)]), 
                  readerControl = list(language = "fr"))
meta(corpus, type = "corpus", tag = "language") <- "fr"

# Création du tableau lexical et du lexique
dtm <- build_dtm(corpus, remove_numbers = F)
inspect(dtm)
dict <- data.table(dictionary(dtm), keep.rownames = "Word")

# Remplacement des espaces insécables par des espaces classiques
dt_plu[grep("\u0099", text),
       text_corr := stri_replace_all(text, " ", regex = "\u0099")]

# Suppression des espaces superflus
dt_plu[, text_corr := str_squish(coalesce(text_corr, text))]


## Corrections othographiques ----

# Jointure pour trouver les mots du corpus qui ne sont pas dans lexique3
dict[lexique3, lexique3 := ortho_sans, on = .(Word = ortho_sans)]
dict[is.na(lexique3), Word]

# Fonction pour ajouter le texte corrigé au tableau dt_plu
correction <- function(mot, corr) {
  cat(crayon::bold(paste0(mot, " -> ", corr,"\n")))
  cat(paste("Fréquence :", dict[Word == mot, Occurrences], "\n"))
  corpus <- corpus[row_sums(dtm[, mot]) > 0]
  color <- crayon::red(crayon::bold("X"))
  for(i in seq_along(corpus)) {
    id <- names(corpus)[i]
    cat(crayon::bold(id), "\n")
    cat(gsub(sprintf("\\b(%s)\\b", mot), 
             sub("X", "\\\\1", color), as.character(corpus[[i]]), 
             ignore.case = TRUE))
    cat("\n")
    dt_plu[doc_id == id, 
           text_corr := stri_replace_all_regex(text_corr, 
                                               paste0("(^|[^a-z0-9])(", mot, ")([^a-z0-9]|$)"), 
                                               paste0("$1", corr, "$3"))]
    cat(paste0("-> ", dt_plu[doc_id == id, text_corr]), "\n")
  }
  cat("\n\n")
}

# Fonctions outils de vérification
f <- function(mot) concordances(corpus, dtm, mot)
fc <- function(mot) concordances(corpus_corr, dtm_corr, mot)
g <- function(mot) lexique3[ortho_sans == mot, ]

# Correspondance des mots à corriger et leur correction ----
corr <- c("130" = "cent-trente",
          "1telligent" = "intelligent",
          "20" = "vingt",
          "allurre" = "allure",
          "alui" = "a lui",
          "amandeil" = "amande il",
          "amitiee" = "amitie",
          "asurance" = "assurance",
          "athelte" = "athlete",
          "atipyque" = "atypique",
          "atititudes" = "attitudes",
          "attireepar" = "attiree par",
          "attisance" = "attirance",
          "batant" = "battant",
          "bcp" = "beaucoup",
          "besoinde" = "besoin de",
          "besu" = "beau",
          "bleux" = "bleus",
          "blle" = "belle",
          "britisch" = "british",
          "brittanique" = "britannique",
          "cad" = "cestadire",
          "caracrere" = "caractere",
          "caracter" = "caractere",
          "caratere" = "caractere",
          "carctere" = "caractere",
          "carismatique" = "charismatique",
          "carisme" = "charisme",
          "carrisme" = "charisme",
          "chariste" = "charisme",
          "charmeson" = "charme son",
          "chatant" = "chantant",
          "cheveulure" = "chevelure",
          "compation" = "compassion",
          "constancer" = "constance",
          "conviviabilite" = "convivialite",
          "coprs" = "corps",
          "cruz" = "cruise",
          "delle" = "d'elle",
          "dicret" = "discret",
          "discuteret" = "discuter et",
          "discution" = "discussion",
          "ds" = "dans",
          "dynamysme" = "dynamisme",
          "dysnamisme" = "dynamisme",
          "eizn" = "rien",
          "ensemblecomme" = "ensemble comme",
          "envi" = "envie",
          "esaaye" = "essaye",
          "eseule" = "esseule",
          "espritoo" = "esprit",
          "estun" = "est un",
          "etit" = "etait",
          "facle" = "facile",
          "femminine" = "feminine",
          "fesait" = "faisait",
          "fondeements" = "fondements",
          "fudre" = "foudre",
          "gd" = "grand",
          "generl" = "general",
          "gentilesse" = "gentillesse",
          "gentillese" = "gentillesse",
          "gentillessee" = "gentillesse",
          "gentillesseson" = "gentillesse son",
          "gossesi" = "gosses",
          "grabe" = "grave",
          "gran" = "grand",
          "gueulle" = "gueule",
          "hora" = "aura",
          "iintelligent" = "intelligent",
          "ilest" = "il est",
          "iletait" = "il etait",
          "ilfait" = "il fait",
          "impotance" = "importance",
          "intelectuel" = "intellectuel",
          "intelectuellement" = "intellectuellement",
          "inteligente" = "intelligente",
          "intelligeance" = "intelligence",
          "intereeait" = "interessait",
          "interressant" = "interessant",
          "interressante" = "interessante",
          "intriguante" = "intrigante",
          "ique" = "",
          "jentillesse" = "gentillesse",
          "juvenil" = "juvenile",
          "kest" = "est",
          "l4hulour" = "l'humour",
          "lair" = "l'air",
          "magnifiquec" = "magnifique c",
          "marante" = "marrante",
          "mignone" = "mignonne",
          "natur" = "nature",
          "nesait" = "ne sait",
          "ns" = "tout",
          "nsp" = "ne sait pas",
          "oie" = "joie",
          "omprehensif" = "comprehensif",
          "orine" = "origine",
          "pa" = "pas",
          "peins" = "pleins",
          "pepss" = "peps",
          "pers" = "personne",
          "percings" = "piercings",
          "peronne" = "personne",
          "personalite" = "personnalite",
          "pesonnalite" = "personnalite",
          "phisique" = "physique",
          "physiqhe" = "physique",
          "physiqueses" = "physique ses",
          "physiue" = "physique",
          "pitrine" = "poitrine",
          "portuguaise" = "portugaise",
          "pr" = "pour",
          "prestence" = "prestance",
          "professionelle" = "professionnelle",
          "pyhsique" = "physique",
          "quil" = "qu'il",
          "r2serve" = "reserve",
          "raprochait" = "rapprochait",
          "rein" = "rien",
          "reistait" = "resistait",
          "reparti" = "repartie",
          "resemblait" = "ressemblait",
          "riglo" = "rigolo",
          "rigolee" = "rigoler",
          "rigolotte" = "rigolote",
          "rirev" = "rire",
          "rudbyman" = "rugbyman",
          "sabeaute" = "sa beaute",
          "saforce" = "sa force",
          "savoix" = "sa voix",
          "sereinet" = "serein et",
          "serinite" = "serenite",
          "sexie" = "sexy",
          "silhoutte" = "silhouette",
          "silouette" = "silhouette",
          "so" = "son",
          "soccupait" = "s'occupait",
          "sonaccent" = "son accent",
          "sonsourire" = "son sourire",
          "sourir" = "sourire",
          "sozi" = "sosie",
          "ss" = "sa",
          "sympath" = "sympathique",
          "sympatie" = "sympathie",
          "sympatique" = "sympathique",
          "talen" = "talent",
          "tchache" = "tchatche",
          "temperamment" = "temperament",
          "touuut" = "tout",
          "tre" = "tres",
          "tupe" = "type",
          "vivire" = "vivre",
          "vivreµ" = "vivre",
          "voie" = "voix",
          "voulai" = "voulait",
          "ya" = "y a")

# Application de la fonction de correction ----
as.data.table(corr, keep.rownames = "mot"
)[, correction(mot, corr), by = mot]


# Corrections marginales (pas prise en charge par la fonction)
dt_plu[doc_id == "2400061401000", text_corr := gsub("atout", "a tout", text_corr)]
dt_plu[doc_id == "7200065401000", text_corr := gsub("sex apeal", "sexappeal", text_corr)]
dt_plu[doc_id == "2100006901000", text_corr := gsub("m.....", "merde", text_corr)]
dt_plu[doc_id == "3100042101000", text_corr := gsub("extra verti", "extraverti", text_corr)]
dt_plu[doc_id == "5200030201000", text_corr := gsub("de contracte", "decontracte", text_corr)]
dt_plu[doc_id == "9100028301000", text_corr := gsub("foutre", "foudre", text_corr)]


# Levé de l'ambiguïté plaire/plu (plus -> plu)
dt_plu[doc_id %in% c("7300001901000", "7300048301000"), 
       text_corr := gsub("plus", "plu", text_corr)]

# Suppression des interventions de l'enquêteur·ice
dt_plu[doc_id == "7200047701000", text_corr := gsub("sans precision", "", text_corr)]
dt_plu[doc_id == "7200002201000", text_corr := gsub("monsieur", "", text_corr)]



## Gestion des non-réponses ----

dt_plu[text %in% c("0", "x"), text_corr := ""]

# "ne souhaite pas répondre", etc.
dt_plu[grep("repondre", text_corr),
       .(text_corr,
         stri_replace_all(text_corr, 
                          "refus",
                          regex = "ne (veut|souhaite|desire) pas repondre"))
]
dt_plu[grep("repondre", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "refus",
                                     regex = "ne (veut|souhaite|desire) pas repondre")
]


## Ne sait pas ----

# "je ne sais pas", "ne sait pas", etc.
dt_plu[grep("sai(s|t) pas", text_corr), 
       .(text_corr,
         stri_replace_all(gsub("[[:punct:][:blank:]]+", " ", text_corr), 
                          " nesaitpas", 
                          regex = "(^| |\")(je |il |elle )?(ne )?sai(s|t) pas( trop| quoi dire)?"))
] %>% fwrite("~/Desktop/test_nesaitpas.csv") 
dt_plu[grep("sai(s|t) pas", text_corr), 
       text_corr := stri_trim(stri_replace_all(gsub("[[:punct:][:blank:]]+", " ", text_corr), 
                                               " nesaitpas", 
                                               regex = "(^| |\")(je |il |elle )?(ne )?sai(s|t) pas( trop| quoi dire)?"))] 

dt_plu[doc_id == "1100067801000", 
       text_corr := gsub("je n ai pas d idee", "nesaitpas", text_corr)]
dt_plu[doc_id == "9300102401000", 
       text_corr := gsub("je ne sais pas quoi  dire", "nesaitpas", text_corr)]


dt_plu[text == "ras", text_corr := "nesaitpas"]


## Ne peut pas dire --

dt_plu[grep("repondre", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "nepeutpasdire",
                                     regex = "(je )?ne peu(t|x) pa(s|r) repondre( en detail)?")]

dt_plu[doc_id == "5300040001000", 
       text_corr := gsub("ne peut dire", "nepeutpasdire", text_corr)]

dt_plu[doc_id == "7200048501000", 
       text_corr := gsub("nesaitpas dire", "nepeutpasdire", text_corr)]

dt_plu[doc_id == "7300022701000", 
       text_corr := gsub("nesaitpas l expliquer", "nepeutpasdire", text_corr)]

dt_plu[doc_id == "9300102801000", 
       text_corr := gsub("pas de reponse", "", text_corr)]

## Ne sait plus ----

dt_plu[doc_id == "2400070401000", 
       text_corr := gsub("je ne sais plus", "nesaitplus", text_corr)]

dt_plu[doc_id == "8200114001000", 
       text_corr := gsub("ne sait plus vraiment...", "nesaitplus", text_corr)]

dt_plu[grep("souvien(s|t) (plus|pas)", text_corr), 
       text_corr := stri_trim(stri_replace_all(text_corr,
                                               " nesaitplus",
                                               regex = "(^| )(je )?(ne )?(me |se |m en )?souvien(s|t) (plus|pas)( du tout)?"))]


## Conversion d'expressions composées de plusieurs mots en forme unique ----

# Coup de foudre
dt_plu[grep("coup de foudre", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "coupdefoudre",
                                     regex = "coup de foudre")]

# Joie de vivre
dt_plu[grep("vivre", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "joiedevivre",
                                     regex = "joie de vivre")]

# Plus âgé·e
dt_plu[grep("age(e)?", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "plus âgé",
                                     regex = "plus age(e)?")]

# Sur de lui/d'elle -> sûr de soi
dt_plu[grep("sur(e)? (de lui|d('| )elle)", text_corr), 
       text_corr := stri_trim(stri_replace_all(text_corr,
                                               "sûrdesoi",
                                               regex = "sur(e)? (de lui|d('| )elle)"))] 

# Beau parleur
dt_plu[grep("parleur", text_corr), 
       text_corr := stri_trim(stri_replace_all(text_corr,
                                               "beauparleur",
                                               regex = "beau parleur"))] 

# Tout le temps
dt_plu[grep("temps", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "toutletemps",
                                     regex = "tout le temps")]

# Rien du tout
dt_plu[grep("rien du tout", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "riendutout",
                                     regex = "rien du tout")]

# Rien de spécial
dt_plu[grep("rien de special", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "riendespecial",
                                     regex = "rien de special(e)?")]

# Rien de particulier
dt_plu[grep("rien (de|en) particulier", text_corr), 
       text_corr := stri_trim(stri_replace_all(text_corr,
                                               "riendeparticulier",
                                               regex = "rien (de|en) particulier"))]

# Rien à dire
dt_plu[grep("rien a dire", text_corr), 
       text_corr := stri_replace_all(text_corr, 
                                     "rienàdire",
                                     regex = "rien a dire")]

# Bad boy
dt_plu[grep("boy", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "badboy",
                                     regex = "bad boy")]


# En particulier
dt_plu[grep("en particulier", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "enparticulier",
                                     regex = "en particulier")]

# Tout de suite
dt_plu[grep("suite", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "toutdesuite",
                                     regex = "(tout )?de suite")]

# Comme ça
dt_plu[grep("comme ca", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "commeça",
                                     regex = "comme ca")]

# Le fait
dt_plu[grep("le fait", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "lefait",
                                     regex = "le fait")]

# Façon d'être
dt_plu[grep("etre", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "façondêtre",
                                     regex = "facon (d |d')?etre")]

# Manière d'être
dt_plu[grep("etre", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "manièredêtre",
                                     regex = "maniere (d |d')?etre")]


# Création du nouveau corpus à partir des réponses corrigées
corpus_corr <- VCorpus(DataframeSource(dt_plu[, .(doc_id, text = fifelse(is.na(text_corr), text, text_corr))]),
                       readerControl = list(language = "fr"))
meta(corpus_corr, type = "corpus", tag = "language") <- "fr"

# Création du tableau lexical et du lexique corrigés
dtm_corr <- build_dtm(corpus_corr, remove_numbers = F)
inspect(dtm_corr)
dict_corr <- data.table(dictionary(dtm_corr), keep.rownames = "Word")

# Ajout des lemmes de lexique3 par jointure, utilisation du mot d'origine comme
# lemme lorsque le mot n'est pas présent dans lexique3
lexique3_unique <- unique(lexique3[order(ortho, -freqlemfilms2)], by = "ortho_sans")
dict_corr[lexique3_unique, c("Lemme", "Cat_gram") := .(i.lemme, cgram), 
          on = .(Word = ortho_sans)]
dict_corr[is.na(Lemme), Word]
dict_corr[is.na(Lemme), Lemme := Word]





# dict1 <- fread("Data/dict_plu1_corr.csv")
# dict2 <- fread("Data/dict_plu2_corr.csv")
# dict12 <- rbind(dict1, dict2) %>% unique
# 
# dict1239 <- merge(dict_corr, dict1, all.x = T, by = "Word")

# dict1239[Lemme.x != Lemme.y, .(Word, Lemme.x, Stopword.x, Cat_gram.x,
#                                Lemme.y, Stopword.y, Cat_gram.y)]
#  
# mot_corr <- dict1239[Lemme.x != Lemme.y, .(sprintf('dict_corr[Word == "%s", `:=`(Lemme = "%s", Stopword = "%s", Cat_gram = "%s")]',
#                                                Word, Lemme.y, Stopword.y, Cat_gram.y))]
# 
# fwrite(mot_corr, "~/Desktop/corr_dict.csv")

dict_corr[Word == "accueillant", `:=`(Lemme = "accueillant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "active", `:=`(Lemme = "actif", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "affirme", `:=`(Lemme = "affirmé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "affirmee", `:=`(Lemme = "affirmé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "age", `:=`(Lemme = "âge", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "âgé", `:=`(Lemme = "âgé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "aller", `:=`(Lemme = "aller", Stopword = "Stopword", Cat_gram = "VER")]
dict_corr[Word == "amusant", `:=`(Lemme = "amusant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "attirant", `:=`(Lemme = "attirant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "attire", `:=`(Lemme = "attiré", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "attiree", `:=`(Lemme = "attiré", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "aura", `:=`(Lemme = "aura", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "aussi", `:=`(Lemme = "aussi", Stopword = "Stopword", Cat_gram = "ADV")]
dict_corr[Word == "autres", `:=`(Lemme = "autres", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "avoir", `:=`(Lemme = "avoir", Stopword = "Stopword", Cat_gram = "AUX")]
dict_corr[Word == "bel", `:=`(Lemme = "beau", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "boucles", `:=`(Lemme = "bouclé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "bronze", `:=`(Lemme = "bronzé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "ca", `:=`(Lemme = "ca", Stopword = "Stopword", Cat_gram = "PRO:dem")]
dict_corr[Word == "calme", `:=`(Lemme = "calme", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "carre", `:=`(Lemme = "carré", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "cheveux", `:=`(Lemme = "cheveux", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "cote", `:=`(Lemme = "côté", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "coupe", `:=`(Lemme = "coupe", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "courbes", `:=`(Lemme = "courbes", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "cultive", `:=`(Lemme = "cultivé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "decale", `:=`(Lemme = "décalé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "different", `:=`(Lemme = "différent", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "distingue", `:=`(Lemme = "distingué", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "distinguee", `:=`(Lemme = "distingué", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "du", `:=`(Lemme = "devoir", Stopword = "Stopword", Cat_gram = "VER")]
dict_corr[Word == "echange", `:=`(Lemme = "échanger", Stopword = "", Cat_gram = "VER")]
dict_corr[Word == "echanges", `:=`(Lemme = "échanger", Stopword = "", Cat_gram = "VER")]
dict_corr[Word == "elance", `:=`(Lemme = "élancé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "elancee", `:=`(Lemme = "élancé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "eleve", `:=`(Lemme = "élevé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "entre", `:=`(Lemme = "entre", Stopword = "Stopword", Cat_gram = "PRE")]
dict_corr[Word == "entreprenant", `:=`(Lemme = "enreprenant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "epaules", `:=`(Lemme = "épaules", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "etudes", `:=`(Lemme = "études", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "facilite", `:=`(Lemme = "facilité", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "femmes", `:=`(Lemme = "femmes", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "fesses", `:=`(Lemme = "fesses", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "formes", `:=`(Lemme = "formes", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "habille", `:=`(Lemme = "habillé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "imposant", `:=`(Lemme = "imposant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "integre", `:=`(Lemme = "intègre", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "interessant", `:=`(Lemme = "intéressant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "jambes", `:=`(Lemme = "jambes", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "manieres", `:=`(Lemme = "manières", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "maquillee", `:=`(Lemme = "maquillé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "marrons", `:=`(Lemme = "marron", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "mate", `:=`(Lemme = "mat", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "meme", `:=`(Lemme = "même", Stopword = "", Cat_gram = "ADV")]
dict_corr[Word == "memes", `:=`(Lemme = "même", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "mots", `:=`(Lemme = "mots", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "mur", `:=`(Lemme = "mûr", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "mure", `:=`(Lemme = "mûr", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "muscle", `:=`(Lemme = "musclé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "muscles", `:=`(Lemme = "musclé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "ouvert", `:=`(Lemme = "ouvert", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "ouverte", `:=`(Lemme = "ouvert", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "parce", `:=`(Lemme = "parce", Stopword = "Stopword", Cat_gram = "ADV")]
dict_corr[Word == "parcours", `:=`(Lemme = "parcours", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "parents", `:=`(Lemme = "parents", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "parfait", `:=`(Lemme = "parfait", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "parfaite", `:=`(Lemme = "parfait", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "passions", `:=`(Lemme = "passion", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "patient", `:=`(Lemme = "patient", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "peche", `:=`(Lemme = "pêche", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "peps", `:=`(Lemme = "peps", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "personne", `:=`(Lemme = "personne", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "physique", `:=`(Lemme = "physique", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "plu", `:=`(Lemme = "plaire", Stopword = "", Cat_gram = "VER")]
dict_corr[Word == "poli", `:=`(Lemme = "poli", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "pose", `:=`(Lemme = "posé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "posee", `:=`(Lemme = "posé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "prevenant", `:=`(Lemme = "prévenant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "puis", `:=`(Lemme = "puis", Stopword = "Stopword", Cat_gram = "CON")]
dict_corr[Word == "qq", `:=`(Lemme = "qq", Stopword = "Stopword", Cat_gram = "")]
dict_corr[Word == "quand", `:=`(Lemme = "quand", Stopword = "Stopword", Cat_gram = "CON")]
dict_corr[Word == "quelqu", `:=`(Lemme = "quelqu", Stopword = "Stopword", Cat_gram = "")]
dict_corr[Word == "quelque", `:=`(Lemme = "quelque", Stopword = "Stopword", Cat_gram = "ADJ:ind")]
dict_corr[Word == "quoi", `:=`(Lemme = "quoi", Stopword = "Stopword", Cat_gram = "PRO:int")]
dict_corr[Word == "raffine", `:=`(Lemme = "raffiné", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "rassurant", `:=`(Lemme = "rassurant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "rebelle", `:=`(Lemme = "rebelle", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "reflechi", `:=`(Lemme = "réfléchi", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "reflechie", `:=`(Lemme = "réfléchi", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "repartie", `:=`(Lemme = "répartie", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "reserve", `:=`(Lemme = "réservé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "reservee", `:=`(Lemme = "réservé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "rondeurs", `:=`(Lemme = "rondeurs", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "securisant", `:=`(Lemme = "sécurisant", Stopword = "", Cat_gram = "VER")]
dict_corr[Word == "seduisant", `:=`(Lemme = "séduisant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "seins", `:=`(Lemme = "seins", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "sens", `:=`(Lemme = "sens", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "si", `:=`(Lemme = "si", Stopword = "Stopword", Cat_gram = "ADV")]
dict_corr[Word == "soigne", `:=`(Lemme = "soigné", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "souriait", `:=`(Lemme = "sourire", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "souriant", `:=`(Lemme = "souriant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "sourire", `:=`(Lemme = "sourire", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "tete", `:=`(Lemme = "tête", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "tigresse", `:=`(Lemme = "tigresse", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "trempe", `:=`(Lemme = "trempé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "valeurs", `:=`(Lemme = "valeurs", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "vers", `:=`(Lemme = "vers", Stopword = "Stopword", Cat_gram = "PRE")]
dict_corr[Word == "yeux", `:=`(Lemme = "yeux", Stopword = "", Cat_gram = "NOM")]

# fwrite(dict1239[is.na(Lemme.y), .(Word, Lemme.x, Stopword.x, Cat_gram.x,
#                                   Lemme.y, Stopword.y, Cat_gram.y)],
#        "~/Desktop/dict.csv")


# dict_der <- fread("~/Desktop/dict39.csv", header = T)
# mot_corr <- dict_der[paste0(Lemme.y, Stopword.y, Cat_gram.y) != "",
#                      .(sprintf('dict_corr[Word == "%s", `:=`(Lemme = "%s", Stopword = "%s", Cat_gram = "%s")]',
#                                Word, 
#                                fcoalesce(Lemme.y, Lemme.x), 
#                                fcoalesce(Stopword.y, Stopword.x),
#                                fcoalesce(Cat_gram.y, Cat_gram.x)))]

dict_corr[Word == "ainsi", `:=`(Lemme = "", Stopword = "Stopword", Cat_gram = "")]
dict_corr[Word == "allant", `:=`(Lemme = "allant", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "allemand", `:=`(Lemme = "allemand", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "allemande", `:=`(Lemme = "allemande", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "ame", `:=`(Lemme = "âme", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "amuse", `:=`(Lemme = "amusé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "argentine", `:=`(Lemme = "Argentine", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "attention", `:=`(Lemme = "attention", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "atome", `:=`(Lemme = "atomes", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "bagou", `:=`(Lemme = "bagout", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "baraque", `:=`(Lemme = "baraqué", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "barbu", `:=`(Lemme = "barbu", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "baroudeur", `:=`(Lemme = "baroudeur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "battu", `:=`(Lemme = "battu", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "blagueur", `:=`(Lemme = "blagueur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "blond", `:=`(Lemme = "blond", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "blonde", `:=`(Lemme = "blond", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "blonds", `:=`(Lemme = "blond", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "brillantissime", `:=`(Lemme = "brillantissime", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "bronzee", `:=`(Lemme = "bronzé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "car", `:=`(Lemme = "car", Stopword = "Stopword", Cat_gram = "CON")]
dict_corr[Word == "celle", `:=`(Lemme = "", Stopword = "Stopword", Cat_gram = "")]
dict_corr[Word == "celui", `:=`(Lemme = "celui", Stopword = "Stopword", Cat_gram = "PRO:dem")]
dict_corr[Word == "charmeur", `:=`(Lemme = "charmeur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "chez", `:=`(Lemme = "chez", Stopword = "Stopword", Cat_gram = "PRE")]
dict_corr[Word == "comique", `:=`(Lemme = "comique", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "complice", `:=`(Lemme = "complice", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "compromis", `:=`(Lemme = "compromis", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "culotte", `:=`(Lemme = "culotté", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "decontracte", `:=`(Lemme = "décontracté", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "decue", `:=`(Lemme = "déçue", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "degaine", `:=`(Lemme = "dégaine", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "degourdie", `:=`(Lemme = "dégourdi", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "dejantee", `:=`(Lemme = "déjanté", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "delires", `:=`(Lemme = "délire", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "deroutant", `:=`(Lemme = "déroutant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "distingues", `:=`(Lemme = "distingué", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "divorce", `:=`(Lemme = "divorce", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "donc", `:=`(Lemme = "donc", Stopword = "Stopword", Cat_gram = "CON")]
dict_corr[Word == "dont", `:=`(Lemme = "dont", Stopword = "Stopword", Cat_gram = "PRO:int")]
dict_corr[Word == "droit", `:=`(Lemme = "droit", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "eduquee", `:=`(Lemme = "éduqué", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "effronte", `:=`(Lemme = "effronté", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "elles", `:=`(Lemme = "", Stopword = "Stopword", Cat_gram = "")]
dict_corr[Word == "enfin", `:=`(Lemme = "", Stopword = "Stopword", Cat_gram = "")]
dict_corr[Word == "envers", `:=`(Lemme = "envers", Stopword = "Stopword", Cat_gram = "PRE")]
dict_corr[Word == "etaient", `:=`(Lemme = "être", Stopword = "Stopword", Cat_gram = "VER")]
dict_corr[Word == "etais", `:=`(Lemme = "être", Stopword = "Stopword", Cat_gram = "VER")]
dict_corr[Word == "etait", `:=`(Lemme = "être", Stopword = "Stopword", Cat_gram = "VER")]
dict_corr[Word == "ete", `:=`(Lemme = "être", Stopword = "Stopword", Cat_gram = "VER")]
dict_corr[Word == "etions", `:=`(Lemme = "être", Stopword = "Stopword", Cat_gram = "VER")]
dict_corr[Word == "etonne", `:=`(Lemme = "étonné", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "etrangere", `:=`(Lemme = "étranger", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "expressivite", `:=`(Lemme = "expressivite", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "ferme", `:=`(Lemme = "ferme", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "fermee", `:=`(Lemme = "fermé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "franchement", `:=`(Lemme = "", Stopword = "Stopword", Cat_gram = "")]
dict_corr[Word == "fonceuse", `:=`(Lemme = "fonceur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "gaulee", `:=`(Lemme = "gaulé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "general", `:=`(Lemme = "général", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "generale", `:=`(Lemme = "général", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "haut", `:=`(Lemme = "haut", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "joueuse", `:=`(Lemme = "joueur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "laquelle", `:=`(Lemme = "", Stopword = "Stopword", Cat_gram = "")]
dict_corr[Word == "melange", `:=`(Lemme = "mélange", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "micro", `:=`(Lemme = "micro", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "metisse", `:=`(Lemme = "métis", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "mimi", `:=`(Lemme = "mimi", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "mitige", `:=`(Lemme = "mitigé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "paumee", `:=`(Lemme = "paumé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "percant", `:=`(Lemme = "perçant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "perdue", `:=`(Lemme = "perdu", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "plaisante", `:=`(Lemme = "plaisant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "platine", `:=`(Lemme = "platine", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "portugaise", `:=`(Lemme = "portugais", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "renfrogne", `:=`(Lemme = "renfrogné", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "sculpte", `:=`(Lemme = "sculpté", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "soignee", `:=`(Lemme = "soigné", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "soutenue", `:=`(Lemme = "soutenu", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "travailleur", `:=`(Lemme = "travailleur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "travailleuse", `:=`(Lemme = "travailleur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "the", `:=`(Lemme = "the", Stopword = "", Cat_gram = "")]
dict_corr[Word == "voile", `:=`(Lemme = "voile", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "zenitude", `:=`(Lemme = "zenitude", Stopword = "", Cat_gram = "NOM")]


## Exportation des réponses corrigées et du lexique associé
# fwrite(dt_plu[, .(variable,
#                   ident_men = doc_id,
#                   text = fifelse(is.na(text_corr), text, text_corr))],
#        "Data/plu39_corr.csv")
#
# fwrite(dict_corr[, .(Word,  Lemme, Stopword, Cat_gram, Occurrences)],
#        "Data/dict_plu39_corr.csv")
