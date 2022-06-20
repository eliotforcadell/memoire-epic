library(data.table)
library(R.temis)
library(tidyverse)
library(stringi)

# Importation de la base des questions ouvertes
ouvertes <- fread("Data/ouvertes_corr.csv")
setnames(ouvertes, names(ouvertes), tolower(names(ouvertes)))

# Importation de la base lexicale lexique 3
lexique3 <- fread("Data/Lexique383.tsv")
lexique3[, ortho_sans := stri_trans_general(lexique3$ortho, id = "Latin-ASCII")]

# Création du corpus à partir de la variable "h_plu2"
dt_plu <- ouvertes[, .(doc_id = as.character(ident_men), 
                       text = tolower(stri_trans_general(h_plu2, id = "Latin-ASCII")))]
corpus <- VCorpus(DataframeSource(dt_plu), 
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
corr <- c("0n" = "on",
          "12" = "douze",
          "15" = "quinze",
          "1er" = "premier",
          "1ere" = "premiere",
          "24" = "vingt-quatre",
          "1m72" = "un metre soixante-douze",
          "affinitee" = "affinite",
          "alluree" = "allure",
          "alure" = "allure",
          "ama" = "ame",
          "amitiee" = "amitie",
          "ampathie" = "empathie",
          "ateit" = "etait",
          "atentionne" = "attentionne",
          "atentionner" = "attentionne", 
          "atlhete" = "athlete",
          "attentione" = "attentionne",
          "attentionner" = "attentionne",
          "attirancecoup" = "attirance coup",
          "bcp" = "beaucoup",
          "beauet" = "beau et",
          "beuax" = "beaux",
          "bleux" = "bleus",
          "blon" = "blond",
          "bone" = "bonne",
          "bulde" = "builde",
          "c4est" = "c'est",
          "came" = "calme",
          "caratere" = "caractere",
          "carisme" = "charisme",
          "carractere" = "caractere",
          "carurre" = "carrure",
          "cetait" = "c'etait",
          "cetit" = "c'etait",
          "charsime" = "charisme",
          "cheuveux" = "cheveux",
          "cloney" = "clooney",
          "conformite" = "conformisme",
          "connaisssance" = "connaissance",
          "corp" = "corps",
          "cotemysterieux" = "cote mysterieux",
          "coye" = "cote",
          "croyai" = "croyais",
          "cte" = "cote",
          "curuieuse" = "curieuse",
          "daner" = "danser",
          "dattirance" = "d'attirance",
          "desprit" = "d'esprit",
          "detre" = "d'etre",
          "diffferent" = "different",
          "ds" = "dans",
          "dupoint" = "du point de",
          "dutout" = "du tout",
          "dynanisme" = "dynamisme",
          "ellle" = "elle",
          "entendat" = "entendait",
          "entousiasme" = "enthousiasme",
          "erenite" = "serenite",
          "etair" = "etait",
          "etaitinteressant" = "etait interessant",
          "etaitsympa" = "etait sympa",
          "etavec" = "et avec",
          "etsa" = "et sa",
          "exhuberant" = "exuberant",
          "expenssif" = "expensif",
          "facilete" = "facilite",
          "feelin" = "feeling",
          "fesait" = "faisait",
          "filling" = "feeling",
          "flasch" = "flash",
          "fontuite" = "fortuite",
          "franchete" = "franchise",
          "fure" = "fur et a mesure",
          "gande" = "grande",
          "genil" = "gentil",
          "genral" = "general",
          "gentilesse" = "gentillesse",
          "gentillesee" = "gentillesse",
          "gentillessebeaute" = "gentillesse beaute",
          "gentillessse" = "gentillesse",
          "gentilllesse" = "gentillesse",
          "gentissesse" = "gentillesse",
          "genyille" = "gentille",
          "georges" = "george",
          "groi" = "",
          "groupen" = "groupe",
          "gueulle" = "gueule",
          "hiumour" = "humour",
          "hora" = "aura",
          "humeure" = "humeur",
          "iintelligent" = "intelligent",
          "iletait" = "il etait",
          "inaxessible" = "inaccessible",
          "inpression" = "impression",
          "insoucience" = "insouciance",
          "instantannement" = "instantanement",
          "intelectuel" = "intellectuel",
          "intelectuelle" = "intellectuelle",
          "intelectuellement" = "intellectuellement",
          "intellignece" = "intelligence",
          "intellignete" = "intelligente",
          "intentione" = "attentionne",
          "interressant" = "interessant",
          "interresse" = "interesse",
          "intres" = "",
          "iveau" = "niveau",
          "joile" = "jolie",
          "jolieson" = "jolie son",
          "laise" = "l'aise",
          "lichelle" = "michelle",
          "loisirse" = "loisirs",
          "loock" = "look",
          "lyban" = "liban",
          "maince" = "mince",
          "manequin" = "mannequin",
          "mantien" = "maintien",
          "matcho" = "macho",
          "mediterranneen" = "mediterraneen",
          "mignone" = "mignonne",
          "naienne" = "",
          "nsp" = "ne sait pas",
          "parceque" = "parce que",
          "parraissait" = "paraissait",
          "parresseuse" = "paresseuse",
          "pcq" = "parce que",
          "perceverence" = "perseverance",
          "percing" = "piercing",
          "pers" = "personne",
          "persant" = "percant",
          "personalite" = "personnalite",
          "personnalit2" = "personnalite",
          "philosophiqyes" = "philosophiques",
          "phisique" = "physique",
          "phisyque" = "physique",
          "physisque" = "physique",
          "physiue" = "physique",
          "plysique" = "physique",
          "pp" = "par rapport",
          "pr" = "pour",
          "preestance" = "prestance",
          "presance" = "prestance",
          "presonnalite" = "personnalite",
          "prestanc" = "prestance",
          "prestence" = "prestance",
          "professionelle" = "professionnelle",
          "prsence" = "presence",
          "pyjsique" = "physique",
          "pysique" = "physique",
          "qd" = "quand",
          "qq" = "quelqu",
          "quelq" = "quelqu",
          "quelquechose" = "quelque chose",
          "quelqun" = "quelqu'un",
          "rdv" = "rendez-vous",
          "regarg" = "regard",
          "relationel" = "relationnel",
          "remrque" = "remarquee",
          "resemblait" = "ressemblait",
          "resenti" = "ressenti",
          "ressemblaita" = "ressemblait a",
          "riendu" = "rien du",
          "rigollote" = "rigolote",
          "rigolot" = "rigolo",
          "rigolotte" = "rigolote",
          "sagentillessse" = "sa gentillesse",
          "sajoie" = "sa joie",
          "santillais" = "antillais",
          "saon" = "son",
          "sauvagede" = "sauvage de",
          "sensinbilite" = "sensibilite",
          "seperation" = "separation",
          "servialbilite" = "serviabilite",
          "silhouete" = "silhouette",
          "silouette" = "silhouette",
          "silouhette" = "silhouette",
          "simplicie" = "simplicite",
          "simplicitie" = "simplicite",
          "snp" = "ne sait pas",
          "so" = "son",
          "sonhumour" = "son humour",
          "sonintelligence" =  "son intelligence",
          "souriente" = "souriante",
          "sourir" = "sourire",
          "sourrire" = "sourire",
          "spontaneitie" = "spontaneite",
          "ssa" = "sa",
          "stablee" = "stable",
          "subjuge" = "subjugue",
          "sympat" = "sympa",
          "sympatie" = "sympathie",
          "sympatique" = "sympathique",
          "sympatise" = "sympathise",
          "symphatie" = "sympathie",
          "symphatique" = "sympathique",
          "symptathique" = "sympathique",
          "tb" = "tres bien",
          "temperamment" = "temperament",
          "tiche" = "riche",
          "timdite" = "timidite",
          "timiditee" = "timidite",
          "tmidite" = "timidite",
          "toiut" = "tout",
          "tolerente" = "tolerante",
          "tou" = "tout",
          "toutl" = "tout",
          "tranquil" = "tranquille",
          "tranquilite" = "tranquillite",
          "tre" = "tres",
          "trs" = "tres",
          "tt" = "tout",
          "ueux" = "yeux",
          "vec" = "avec",
          "vestimententairement" = "vestimentairement",
          "viet" = "vietnamienne",
          "vouvoyez" = "vouvoyait",
          "xx" = "")


# Application de la fonction de correction ----
as.data.table(corr, keep.rownames = "mot"
              )[, correction(mot, corr), by = mot]


# Corrections marginales (pas prise en charge par la fonction)
dt_plu[doc_id == "5300068201000", text_corr := gsub("sex a pile", "sexappeal", text_corr)]
dt_plu[doc_id == "5300032801000", text_corr := gsub("chercheur", "chercher", text_corr)]
dt_plu[doc_id == "1100117401000", text_corr := gsub("differend", "different", text_corr)]
dt_plu[doc_id == "8300017201000", text_corr := gsub("ecoutez", "ecoute", text_corr)]
dt_plu[doc_id == "2400005203000", text_corr := gsub("1 ere", "première", text_corr)]
dt_plu[doc_id == "1100134801000", text_corr := gsub("lu,i", "lui", text_corr)]
dt_plu[doc_id == "2400067001000", text_corr := gsub("mois", "moi", text_corr)]
dt_plu[doc_id == "5200069701000", text_corr := gsub("out", "tout", text_corr)]
dt_plu[doc_id == "9300102801000", text_corr := gsub("pas de reponse", "", text_corr)]
dt_plu[doc_id == "8200009301000", text_corr := gsub("roque", "rauque", text_corr)]
dt_plu[doc_id == "9100026101000", text_corr := gsub("voie", "voix", text_corr)]


# Levé de l'ambiguïté plaire/plu (plus -> plu)
dt_plu[doc_id %in% c("9300065401000", "8200077201000"), 
       text_corr := gsub("plus", "plu", text_corr)]

# Suppression des interventions de l'enquêteur·ice
dt_plu[doc_id == "1100167901000", text_corr := "refus"]
dt_plu[doc_id == "3100032601000", text_corr := "tout nesaitplus"]


## Gestion des non-réponses ----

dt_plu[text %in% c("0", "1", "x"), text_corr := ""]

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

dt_plu[doc_id %in% c("9300101901000"), text_corr := "refus"]

## Ne sait pas ----

# "je ne sais pas", "ne sait pas", etc.
dt_plu[grep("sai(s|t) pas", text_corr), 
       .(text_corr,
         stri_replace_all(gsub("[[:punct:][:blank:]]+", " ", text_corr), 
                          " nesaitpas", 
                          regex = "(^| |\")(je |il |elle )?(ne )?sai(s|t) pas( trop)?"))
] # %>% fwrite("~/Desktop/test_nesaitpas.csv") 
dt_plu[grep("sai(s|t) pas", text_corr), 
       text_corr := stri_trim(stri_replace_all(gsub("[[:punct:][:blank:]]+", " ", text_corr), 
                                               " nesaitpas", 
                                               regex = "(^| |\")(je |il |elle )?(ne )?sai(s|t) pas( trop)?"))] 
dt_plu[grep("nesaitpas( quoi)?( vous)? repondre", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "nesaitpas",
                                     regex = "nesaitpas( quoi)?( vous)? repondre")]

dt_plu[grep("j(')? en sais rien", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "nesaitpas",
                                     regex = "j(')? en sais rien")]

dt_plu[text == "ras", text_corr := "nesaitpas"]
dt_plu[doc_id == "5300040001000", "nesaitpas"]

## Ne peut pas dire --

dt_plu[grep("(je )?(ne )?peu(t|x) (pas|rien) (vous )?dire( plus| autre chose)?", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "nepeutpasdire",
                                     regex = "(je )?(ne )?peu(t|x) (pas|rien) (vous )?dire( plus| autre chose)?")]

dt_plu[grep("(c est )?(tres )?(difficile|impossible) a dire", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "nepeutpasdire",
                                     regex = "(c est )?(tres )?(difficile|impossible) a dire")]

dt_plu[doc_id == "1100065101000", text_corr := gsub("incapable de le dire", "nepeutpasdire", text_corr)]

dt_plu[grep("explique", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "nepeutpasdire",
                                     regex = "ne peut pas expliquer")]

# Ça ne s'explique pas
dt_plu[grep("explique", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "nesexpliquepas",
                                     regex = "(ca |cela |ce )?(ne )?s explique pas( trop)?")]


## Ne sait plus ----

# "je ne sais plus", "ne me souviens pas", etc.
dt_plu[grep("sai(s|t) plus", text_corr),
       .(text_corr,
         stri_replace_all(text_corr, 
                          "nesaitplus",
                          regex = "(^| )(je )?(ne )?sai(s|t) plus( ca| trop)?"))
] #%>% fwrite("~/Desktop/test_nesaitplus.csv") 
dt_plu[grep("sai(s|t) plus", text_corr),
       text_corr := stri_trim(stri_replace_all(text_corr, 
                                               "nesaitplus",
                                               regex = "(^| )(je )?(ne )?sai(s|t) plus( ca| trop)?"))]

dt_plu[grep("souvien(s|t) (plus|pas)", text_corr), 
       .(text_corr,
         stri_trim(stri_replace_all(text_corr,
                                    " nesaitplus",
                                    regex = "(^| )(je )?(ne )?(me |se |m en )?souvien(s|t) (plus|pas)?( du tout)?")))
] #%>% fwrite("~/Desktop/test_nesaitplus.csv")
dt_plu[grep("souvien(s|t) (plus|pas)", text_corr), 
       text_corr := stri_trim(stri_replace_all(text_corr,
                                               " nesaitplus",
                                               regex = "(^| )(je )?(ne )?(me |se |m en )?souvien(s|t) (plus|pas)( du tout)?"))]

dt_plu[grep("souvenir", text_corr),
       text_corr := stri_trim(stri_replace_all(text_corr, 
                                               "nesaitplus",
                                               regex = "pas de souvenirs"))]


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
dt_plu[grep("plus age(e)?", text_corr),
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
                                     regex = "rien de special")]

# Rien de particulier
dt_plu[grep("rien (de|en) particulier", text_corr), 
       text_corr := stri_trim(stri_replace_all(text_corr,
                                               "riendeparticulier",
                                               regex = "rien (de|en) particulier"))]
dt_plu[doc_id == "2500024101000", text_corr := "riendeparticulier"]

# Rien à dire
dt_plu[grep("rien a dire", text_corr), 
       text_corr := stri_replace_all(text_corr, 
                                     "rienàdire",
                                     regex = "rien a dire")]

# Peut-être
dt_plu[grep("peut etre", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "peutêtre",
                                     regex = "peut etre")]

# Bad boy
dt_plu[grep("boy", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "badboy",
                                     regex = "bad boy(s)?")]

# Bout en train
dt_plu[grep("bout", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "boutentrain",
                                     regex = "bout en train")]


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
# dict12 <- merge(dict_corr, dict1, all.x = T, by = "Word")
# 
# # dict12[Lemme.x != Lemme.y, .(Word, Lemme.x, Stopword.x, Cat_gram.x, 
# #                              Lemme.y, Stopword.y, Cat_gram.y)]
# 
# mot_corr <- dict12[Lemme.x != Lemme.y, .(sprintf('dict_corr[Word == "%s", `:=`(Lemme = "%s", Stopword = "%s", Cat_gram = "%s")]',
#                                                Word, Lemme.y, Stopword.y, Cat_gram.y))]

# fwrite(mot_corr, "~/Desktop/corr_dict.csv")

dict_corr[Word == "accueillant", `:=`(Lemme = "accueillant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "affirme", `:=`(Lemme = "affirmé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "age", `:=`(Lemme = "âge", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "âgé", `:=`(Lemme = "âgé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "aller", `:=`(Lemme = "aller", Stopword = "Stopword", Cat_gram = "VER")]
dict_corr[Word == "amusant", `:=`(Lemme = "amusant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "apaisant", `:=`(Lemme = "apaisant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "atomes", `:=`(Lemme = "atomes", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "attachant", `:=`(Lemme = "attachant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "attirant", `:=`(Lemme = "attirant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "attire", `:=`(Lemme = "attiré", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "aura", `:=`(Lemme = "aura", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "aussi", `:=`(Lemme = "aussi", Stopword = "Stopword", Cat_gram = "ADV")]
dict_corr[Word == "autres", `:=`(Lemme = "autres", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "avoir", `:=`(Lemme = "avoir", Stopword = "Stopword", Cat_gram = "AUX")]
dict_corr[Word == "blagueur", `:=`(Lemme = "blagueur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "bel", `:=`(Lemme = "beau", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "blond", `:=`(Lemme = "blond", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "blonde", `:=`(Lemme = "blond", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "blonds", `:=`(Lemme = "blond", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "boucles", `:=`(Lemme = "bouclé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "bouille", `:=`(Lemme = "bouille", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "boutentrain", `:=`(Lemme = "boutentrain", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "bronze", `:=`(Lemme = "bronzé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "ca", `:=`(Lemme = "ca", Stopword = "Stopword", Cat_gram = "PRO:dem")]
dict_corr[Word == "calme", `:=`(Lemme = "calme", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "car", `:=`(Lemme = "car", Stopword = "Stopword", Cat_gram = "CON")]
dict_corr[Word == "celui", `:=`(Lemme = "celui", Stopword = "Stopword", Cat_gram = "PRO:dem")]
dict_corr[Word == "charmeur", `:=`(Lemme = "charmeur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "cheveux", `:=`(Lemme = "cheveux", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "chez", `:=`(Lemme = "chez", Stopword = "Stopword", Cat_gram = "PRE")]
dict_corr[Word == "comique", `:=`(Lemme = "comique", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "complice", `:=`(Lemme = "complice", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "cote", `:=`(Lemme = "côté", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "coupe", `:=`(Lemme = "coupe", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "courant", `:=`(Lemme = "courant", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "courbes", `:=`(Lemme = "courbes", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "cultive", `:=`(Lemme = "cultivé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "cultivee", `:=`(Lemme = "cultivé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "decale", `:=`(Lemme = "décalé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "decontracte", `:=`(Lemme = "décontracté", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "different", `:=`(Lemme = "différent", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "donc", `:=`(Lemme = "donc", Stopword = "Stopword", Cat_gram = "CON")]
dict_corr[Word == "dont", `:=`(Lemme = "dont", Stopword = "Stopword", Cat_gram = "PRO:int")]
dict_corr[Word == "du", `:=`(Lemme = "devoir", Stopword = "Stopword", Cat_gram = "VER")]
dict_corr[Word == "echange", `:=`(Lemme = "échanger", Stopword = "", Cat_gram = "VER")]
dict_corr[Word == "echanges", `:=`(Lemme = "échanger", Stopword = "", Cat_gram = "VER")]
dict_corr[Word == "eduque", `:=`(Lemme = "éduqué", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "elancee", `:=`(Lemme = "élancé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "eleve", `:=`(Lemme = "élevé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "entre", `:=`(Lemme = "entre", Stopword = "Stopword", Cat_gram = "PRE")]
dict_corr[Word == "entreprenant", `:=`(Lemme = "enreprenant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "envers", `:=`(Lemme = "envers", Stopword = "Stopword", Cat_gram = "PRE")]
dict_corr[Word == "etaient", `:=`(Lemme = "être", Stopword = "Stopword", Cat_gram = "VER")]
dict_corr[Word == "etait", `:=`(Lemme = "être", Stopword = "Stopword", Cat_gram = "VER")]
dict_corr[Word == "etais", `:=`(Lemme = "être", Stopword = "Stopword", Cat_gram = "VER")]
dict_corr[Word == "ete", `:=`(Lemme = "être", Stopword = "Stopword", Cat_gram = "VER")]
dict_corr[Word == "etranger", `:=`(Lemme = "étranger", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "etrangere", `:=`(Lemme = "étranger", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "etudes", `:=`(Lemme = "études", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "facilite", `:=`(Lemme = "facilité", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "femmes", `:=`(Lemme = "femmes", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "fesses", `:=`(Lemme = "fesses", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "fonces", `:=`(Lemme = "foncé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "fonceuse", `:=`(Lemme = "fonceur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "formes", `:=`(Lemme = "formes", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "foutu", `:=`(Lemme = "foutu", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "frimeur", `:=`(Lemme = "frimeur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "frises", `:=`(Lemme = "frisé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "general", `:=`(Lemme = "général", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "generale", `:=`(Lemme = "général", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "habille", `:=`(Lemme = "habillé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "hommes", `:=`(Lemme = "hommes", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "important", `:=`(Lemme = "important", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "instruite", `:=`(Lemme = "instruit", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "integre", `:=`(Lemme = "intègre", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "interessant", `:=`(Lemme = "intéressant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "jambes", `:=`(Lemme = "jambes", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "marrons", `:=`(Lemme = "marron", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "mat", `:=`(Lemme = "mat", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "mecs", `:=`(Lemme = "mecs", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "meme", `:=`(Lemme = "même", Stopword = "", Cat_gram = "ADV")]
dict_corr[Word == "memes", `:=`(Lemme = "même", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "metisse", `:=`(Lemme = "métis", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "mots", `:=`(Lemme = "mots", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "mure", `:=`(Lemme = "mûr", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "muscle", `:=`(Lemme = "musclé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "muscles", `:=`(Lemme = "musclé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "nouvelle", `:=`(Lemme = "nouveau", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "ouvert", `:=`(Lemme = "ouvert", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "ouverte", `:=`(Lemme = "ouvert", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "parcours", `:=`(Lemme = "parcours", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "parents", `:=`(Lemme = "parents", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "parce", `:=`(Lemme = "parce", Stopword = "Stopword", Cat_gram = "ADV")]
dict_corr[Word == "parfait", `:=`(Lemme = "parfait", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "parfaite", `:=`(Lemme = "parfait", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "peps", `:=`(Lemme = "peps", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "personne", `:=`(Lemme = "personne", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "physique", `:=`(Lemme = "physique", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "plu", `:=`(Lemme = "plaire", Stopword = "", Cat_gram = "VER")]
dict_corr[Word == "poli", `:=`(Lemme = "poli", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "polie", `:=`(Lemme = "poli", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "porte", `:=`(Lemme = "porter", Stopword = "", Cat_gram = "VER")]
dict_corr[Word == "portugaise", `:=`(Lemme = "portugais", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "pose", `:=`(Lemme = "posé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "posee", `:=`(Lemme = "posé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "pratique", `:=`(Lemme = "pratique", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "prevenant", `:=`(Lemme = "prévenant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "puis", `:=`(Lemme = "puis", Stopword = "Stopword", Cat_gram = "CON")]
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
dict_corr[Word == "rieuse", `:=`(Lemme = "rieur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "rondeurs", `:=`(Lemme = "rondeurs", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "sacre", `:=`(Lemme = "sacré", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "securisant", `:=`(Lemme = "sécurisant", Stopword = "", Cat_gram = "VER")]
dict_corr[Word == "seduisant", `:=`(Lemme = "séduisant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "seins", `:=`(Lemme = "seins", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "sens", `:=`(Lemme = "sens", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "si", `:=`(Lemme = "si", Stopword = "Stopword", Cat_gram = "ADV")]
dict_corr[Word == "soigne", `:=`(Lemme = "soigné", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "souriait", `:=`(Lemme = "sourire", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "souriant", `:=`(Lemme = "souriant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "sourire", `:=`(Lemme = "sourire", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "tenue", `:=`(Lemme = "tenue", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "tete", `:=`(Lemme = "tête", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "travailleur", `:=`(Lemme = "travailleur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "travailleuse", `:=`(Lemme = "travailleur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "trempe", `:=`(Lemme = "trempé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "valeurs", `:=`(Lemme = "valeurs", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "vers", `:=`(Lemme = "vers", Stopword = "Stopword", Cat_gram = "PRE")]
dict_corr[Word == "vivant", `:=`(Lemme = "vivant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "vive", `:=`(Lemme = "vive", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "yeux", `:=`(Lemme = "yeux", Stopword = "", Cat_gram = "NOM")]

# fwrite(dict12[is.na(Lemme.y), .(Word, Lemme.x, Stopword.x, Cat_gram.x, 
#                                 Lemme.y, Stopword.y, Cat_gram.y)],
#        "~/Desktop/dict.csv")


# dict_der <- fread("~/Desktop/dict.csv", header = T)[, V8 := NULL]
# mot_corr <- dict_der[paste0(Lemme.y, Stopword.y, Cat_gram.y) != "",
#                      .(sprintf('dict_corr[Word == "%s", `:=`(Lemme = "%s", Stopword = "%s", Cat_gram = "%s")]',
#                                Word, Lemme.y, Stopword.y, Cat_gram.y))]

# fwrite(mot_corr, "~/Desktop/corr_dict2.csv")

dict_corr[Word == "alors", `:=`(Lemme = "", Stopword = "Stopword", Cat_gram = "")]
dict_corr[Word == "ame", `:=`(Lemme = "âme", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "arabe", `:=`(Lemme = "arabe", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "attention", `:=`(Lemme = "attention", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "bagou", `:=`(Lemme = "bagout", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "baiser", `:=`(Lemme = "baiser", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "baraque", `:=`(Lemme = "baraqué", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "barbu", `:=`(Lemme = "barbu", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "baratineur", `:=`(Lemme = "baratineur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "baroudeur", `:=`(Lemme = "baroudeur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "bosseuse", `:=`(Lemme = "bosseuse", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "bronzee", `:=`(Lemme = "bronzé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "builde", `:=`(Lemme = "buildé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "but", `:=`(Lemme = "but", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "caracteristiques", `:=`(Lemme = "caractéristiques", Stopword = "", Cat_gram = "")]
dict_corr[Word == "circonstances", `:=`(Lemme = "circonstances", Stopword = "", Cat_gram = "")]
dict_corr[Word == "codes", `:=`(Lemme = "codes", Stopword = "", Cat_gram = "")]
dict_corr[Word == "convoite", `:=`(Lemme = "convoité", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "coureur", `:=`(Lemme = "coureur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "court", `:=`(Lemme = "court", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "craquant", `:=`(Lemme = "craquant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "danses", `:=`(Lemme = "danse", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "decide", `:=`(Lemme = "décidé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "degaine", `:=`(Lemme = "dégaine", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "determinee", `:=`(Lemme = "déterminée", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "divorce", `:=`(Lemme = "divorce", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "divorces", `:=`(Lemme = "divorce", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "droit", `:=`(Lemme = "droit", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "ecorchee", `:=`(Lemme = "écorché", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "efface", `:=`(Lemme = "effacé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "enfin", `:=`(Lemme = "", Stopword = "Stopword", Cat_gram = "")]
dict_corr[Word == "engagements", `:=`(Lemme = "engageant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "expensif", `:=`(Lemme = "expensif", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "fetarde", `:=`(Lemme = "fetard", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "flache", `:=`(Lemme = "flasher", Stopword = "", Cat_gram = "VER")]
dict_corr[Word == "foutue", `:=`(Lemme = "foutu", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "impressionnee", `:=`(Lemme = "impressionné", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "invitee", `:=`(Lemme = "invité", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "jambe", `:=`(Lemme = "jambes", Stopword = "", Cat_gram = "")]
dict_corr[Word == "levres", `:=`(Lemme = "lèvres", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "masque", `:=`(Lemme = "masqué", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "matte", `:=`(Lemme = "mat", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "melange", `:=`(Lemme = "mélange", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "modele", `:=`(Lemme = "modèle", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "motivee", `:=`(Lemme = "motivé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "obstine", `:=`(Lemme = "obstiné", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "paris", `:=`(Lemme = "Paris", Stopword = "", Cat_gram = "")]
dict_corr[Word == "passionne", `:=`(Lemme = "passionné", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "passionnee", `:=`(Lemme = "passionné", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "pecheur", `:=`(Lemme = "pêcheur", Stopword = "", Cat_gram = "")]
dict_corr[Word == "percant", `:=`(Lemme = "perçant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "perturbe", `:=`(Lemme = "perturbé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "piquant", `:=`(Lemme = "piquant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "plaisante", `:=`(Lemme = "plaisant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "pourtant", `:=`(Lemme = "", Stopword = "Stopword", Cat_gram = "")]
dict_corr[Word == "precedent", `:=`(Lemme = "précédent", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "precisement", `:=`(Lemme = "", Stopword = "Stopword", Cat_gram = "")]
dict_corr[Word == "prononce", `:=`(Lemme = "prononcé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "quelques", `:=`(Lemme = "", Stopword = "Stopword", Cat_gram = "")]
dict_corr[Word == "rassuree", `:=`(Lemme = "rassuré", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "reflechit", `:=`(Lemme = "réfléchi", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "renfermee", `:=`(Lemme = "renfermé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "retenue", `:=`(Lemme = "retenue", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "seduite", `:=`(Lemme = "séduit", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "soignee", `:=`(Lemme = "soigné", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "sorte", `:=`(Lemme = "sorte", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "subjugue", `:=`(Lemme = "subjugué", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "tchache", `:=`(Lemme = "tchatche", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "tolerant", `:=`(Lemme = "tolérant", Stopword = "", Cat_gram = "ADJ")]

## Exportation des réponses corrigées et du lexique associé
# fwrite(dt_plu[, .(ident_men = doc_id,
#                   text = fifelse(is.na(text_corr), text, text_corr))],
#        "Data/plu2_corr.csv")
#
# fwrite(dict_corr[, .(Word,  Lemme, Stopword, Cat_gram, Occurrences)],
#        "Data/dict_plu2_corr.csv")
