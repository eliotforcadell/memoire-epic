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

# Création du corpus à partir de la variable "h_plu1"
dt_plu <- ouvertes[, .(doc_id = as.character(ident_men), 
                       text = tolower(stri_trans_general(h_plu, id = "Latin-ASCII")))]
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
corr <- c("10" = "dix",
          "1adorable" = "adorable",
          "1discret" = "discret",
          "1er" = "premier",
          "1ere" = "premiere",
          "1ier" = "premier",
          "1telligence" = "intelligence",
          "20" = "vingt",
          "²il" = "il",
          "2son" = "son",
          "aa" = "a",
          "abiller" = "habiller",
          "acune" = "aucune",
          "alterophile" = "halterophile",
          "aplu" = "a plu",
          "apreciait" = "appreciait", 
          "armonica" = "harmonica",
          "artisite" = "artistique",
          "atipique" = "atypique",
          "atirance" = "attirance",
          "attentione" = "attentionne",
          "attentionnne" = "attentionne",
          "attetionne" = "attentionne",
          "aus" = "aux",
          "avenanttre" = "avenante",
          "avit" = "avait",
          "babacool" = "baba cool",
          "bagou" = "bagout",
          "batant" = "battant",
          "bazane" = "basane",
          "bcp" = "beaucoup",
          "beautee" = "beaute",
          "blageur" = "blagueur",
          "bleux" = "bleus",
          "blon" = "blond",
          "boeme" = "boheme",
          "bonhommie" = "bonhomie",
          "boute" = "bout",
          "bouvles" = "boucles",
          "caisait" = "faisait",
          "caractee" = "caractere",
          "caracter" = "caractere",
          "caratere" = "caractere",
          "carctere" = "caractere",
          "carismatique" = "charismatique",
          "carisme" = "charisme", 
          "carure" = "carrure",
          "cefut" = "ce fut",
          "cetait" = "c'etait",
          "cetiat" = "c'etait",
          "charmeet" = "charme et",
          "che" = "je sais",
          "cheuveux" = "cheveux",
          "cheveus" = "cheveux",
          "cima" = "cinema",
          "cinemathographique" = "cinematographique",
          "connaissebien" = "connaissais bien",
          "connmuns" = "communs",
          "cordensensible" = "corde sensible",
          "coyte" = "cote",
          "cractere" = "caractere",
          "cuture" = "culture",
          "daffection" = "d'affection",
          "dande" = "danse",
          "dansit" = "dansait",
          "debortante" = "debordante",
          "decaractere" = "de caractere",
          "decolees" = "decollees",
          "defoudre" = "de foudre",
          "dentille" = "gentille",
          "depuuis" = "depuis",
          "desuite" = "de suite",
          "diiferents" = "different",
          "dinamisme" = "dynamisme",
          "discetion" = "discretion",
          "discretrion" = "discretion",
          "discution" = "discussion",
          "discutr" = "discuter",
          "distnguee" = "distinguee",
          "drolme" = "drole",
          "ds" = "dans",
          "dynammisme" = "dynamisme",
          "dynamyme" = "dynamisme",
          "dynanisme" = "dynamisme",
          "ecouteses" = "ecoute ses",
          "ellefaisait" = "elle faisait",
          "ellle" = "elle",
          "eloquant" = "eloquent",
          "emmanait" = "emanait",
          "emsemble" = "ensemble",
          "entedait" = "entendait",
          "entousiasme" = "enthousiasme",
          "erieux" = "serieux",
          "errudition" = "erudition",
          "estait" = "etait",
          "etaait" = "etait",
          "etaitdouce" = "etait douce",
          "etaitsympathique" = "etait sympathique",
          "eteit" = "etait",
          "etiat" = "etait",
          "etit" = "etait",
          "etla" = "et la",
          "etson" = "et son",
          "etsympa" = "et sympa", 
          "ettait" = "etait",
          "etvolontaire" = "et volontaire",
          "exentrique" = "excentrique",
          "faiiait" = "faisait",
          "faisiait" = "faisait",
          "fason" = "facon",
          "faon" = "facon",
          "femminite" = "feminite",
          "fur" = "fut",
          "fusionelle" = "fusionnelle",
          "galenterie" = "galanterie",
          "gar" = "gars",
          "garcone" = "garcon",
          "gebereux" = "genereux",
          "genrale" = "generale",
          "genthillesse" = "gentillesse",
          "gentiellesse" = "gentillesse",
          "gentilees" = "gentilles",
          "gentilesse" = "gentillesse",
          "gentillesseet" = "gentillesse et",
          "gentillessetres" = "gentillesse tres",
          "gentiltravailleur" = "gentil travailleur",
          "gentllesse" = "gentillesse",
          "gntil" = "gentil",
          "gole" = "gaule",
          "gransds" = "grands",
          "hatitude" = "attitude",
          "honetete" = "honnetete",
          "hora" = "aura",
          "hummour" = "humour",
          "humou" = "humour",
          "humouriste" = "humoriste",
          "hyppie" = "hippie",
          "identhique" = "identique",
          "iil" = "il",
          "iletait" = "il etait", 
          "ilme" = "il me",
          "in" = "il",
          "inacceccible" = "inaccessible",
          "indentique" = "identique",
          "inellectuel" = "intellectuel",
          "intelect" = "intellect",
          "intelectuel" = "intellectuel",
          "inteligent" = "intelligent",
          "inteligente" = "intelligente",
          "intellegence" = "intelligence",
          "intelligeance" = "intelligence",
          "intelligeante" = "intelligente",
          "intelligentet" = "intelligent et",
          "intellogence" = "intelligence",
          "interlligent" = "intelligent",
          "interressant" = "interessant",
          "interressantes" = "interessantes",
          "interresser" = "interessait",
          "intillenge" = "intelligence",
          "intilligent" = "intelligent",
          "intilligente" = "intelligente",
          "intrigant" = "intriguant",
          "intrigeant" = "intriguant",
          "italiene" = "italienne",
          "ja" = "je",
          "jai" = "j'ai",
          "jetter" = "jeter",
          "joile" = "jolie",
          "joili" = "jolie",
          "joilie" = "jolie",
          "joilis" = "jolis",
          "lavie" = "la vie",
          "lecoup" = "le coup",
          "londe" = "blonde",
          "longeur" = "longueur",
          "loock" = "look",
          "lson" = "son",
          "luii" = "lui",
          "maborder" = "m'aborder",
          "marant" = "marrant",
          "marante" = "marrante",
          "matchisme" = "machisme",
          "matcho" = "macho",
          "matte" = "mat",
          "matue" = "mature",
          "matur" = "mature",
          "mediterranneen" = "mediterraneen",
          "mignone" = "mignonne",
          "mobilette" = "mobylette",
          "moints" = "points",
          "mses" = "ses",
          "muscisienne" = "musicienne",
          "n4est" = "n'est",
          "nai" = "n'ai",
          "netait" = "n'etait",
          "nintelligence" = "intelligence",
          "nsp" = "ne sait pas",
          "nsplus" = "ne sait plus",
          "nvo" = "niveau",
          "originalitee" = "originalite",
          "oulant" = "voulant",
          "pa" = "pas",
          "parcequ" = "parce qu",
          "parelr" = "parler",
          "parraissait" = "paraissait",
          "pb" = "probleme",
          "peersonnalite" = "personnalite",
          "peite" = "petite",
          "peronnalite" = "personnalite",
          "pers" = "personne",
          "personalite" = "personnalite",
          "personnaliteet" = "personnalite et",
          "pertinant" = "pertinent",
          "pesonnalite" = "personnalite",
          "phisique" = "physique",
          "phisyque" = "physique",
          "phsiqye" = "physique",
          "phsysique" = "physique",
          "physiqhe" = "physique",
          "physiqque" = "physique",
          "physiqueelle" = "physique elle",
          "physqie" = "physique",
          "physqiue" = "physique",
          "physyque" = "physique",
          "pomettes" = "pommettes",
          "ponpon" = "pompon",
          "posebien" = "pose bien",
          "pot" = "pote",
          "pressetance" = "prestance",
          "prestence" = "prestance",
          "prmier" = "premier",
          "prsonnalite" = "personnalite",
          "pt" = "point",
          "pyjsique" = "physique",
          "pysique" = "physique",
          "pysiquement" = "physiquement",
          "qd" = "quand",
          "qq1" = "quelqu'un",
          "quartizer" = "quartier",
          "quelqu4un" = "quelqu'un",
          "rappel" = "rappelle",
          "rapelle" = "rappelle",
          "rebel" = "rebelle",
          "rebelion" = "rebellion",
          "refechi" = "reflechi",
          "rein" = "rien",
          "riendu" = "rien du",
          "riere" = "rire",
          "rigolot" = "rigolo",
          "rigolotte" = "rigolote",
          "rtesponsable" = "responsable",
          "rugbymann" = "rugbyman",
          "s4entendait" = "s'entendait",
          "s4interresse" = "s'interesse",
          "s7ympa" = "sympa",
          "sagentillesse" = "sa gentillesse",
          "saitplus" = "sait plus",
          "san" = "son",
          "sautea" = "saute a",
          "sc" = "s'",
          "semblai" = "semblait",
          "sensibilte" = "sensibilite",
          "sentendait" = "s'entendait",
          "sepcial" = "special",
          "serieu" = "serieux",
          "serieus" = "serieux",
          "silouette" = "silhouette",
          "silouhette" = "silhouette",
          "simplicitee" = "simplicite",
          "sinteressait" = "s'interessait",
          "snp" = "ne sait pas",
          "so" = "son",
          "socialble" = "sociable",
          "som" = "son",
          "sonattitude" = "son attitude",
          "soncorps" = "son corps",
          "soncote" = "son cote",
          "sonhumour" = "son humour",
          "sourir" = "sourire",
          "sourrire" = "sourire",
          "soy0ns" = "soyons",
          "specificique" = "specifique",
          "spontaneiete" = "spontaneite",
          "spontaneiteet" = "spontaneite et",
          "spontanieite" = "spontaneite",
          "spontaniete" = "spontaneite",
          "spontannee" = "spontanee",
          "sson" = "son",
          "symatique" = "sympathique",
          "sympathiaque" = "sympathique",
          "sympati" = "sympathie",
          "sympatie" = "sympathie",
          "sympatique" = "sympathique",
          "sympha" = "sympa",
          "symphatie" = "sympathie",
          "symphatique" = "sympathique",
          "symptathie" = "sympathie",
          "sympthie" = "sympathie",
          "taptience" = "patience",
          "tb" = "tres",
          "tchache" = "tchatche",
          "temperamment" = "temperament",
          "tgenerale" = "generale",
          "timiditesa" = "timidite sa",
          "tjrs" = "toujours",
          "tjs" = "toujours",
          "toujous" = "toujours",
          "tps" = "temps",
          "tranquilite" = "tranquillite",
          "travaillvait" = "travaillait",
          "trs" = "tres",
          "ts" = "tous",
          "tt"= "tout",
          "ttes" = "toutes",
          "unbeau" = "un beau",
          "uper" = "super",
          "vait" = "avait",
          "vebu" = "venu",
          "veutr" = "veut",
          "viisage" = "visage",
          "vire" = "vivre",
          "vivire" = "vivre",
          "vtalite" = "vitalite",
          "xx" = "",
          "xxrefus" = "refus",
          "yatout" = "y a tout",
          "yeus" = "yeux",
          "yoeux" = "yeux",
          "zetbonne" = "et bonne",
          "zson" = "son")


# Application de la fonction de correction ----
as.data.table(corr, keep.rownames = "mot"
              )[, correction(mot, corr), by = mot]


# Corrections marginales (pas prise en charge par la fonction)
dt_plu[doc_id == "2300043101000", text_corr := "beau attirance"]
dt_plu[doc_id == "2300012701000", text_corr := "son fair-play et son experience de voyage"]
dt_plu[doc_id == "5200016501000", text_corr := "elle etait tres naturelle, pas \"pin-up\""]
dt_plu[doc_id == "5400021501000", text_corr := "bonne entente entre nous"]


# Levé de l'ambiguïté plaire/plu (plus -> plu)
dt_plu[doc_id == "5300037801000", text_corr := "ne sait pas c'etait l'ensemble m'a bien plu c'est tout  / le flash"]
dt_plu[doc_id == "4100035301000", text_corr := "il m'a plu tout de suite il etait beau sur de lui"]
dt_plu[doc_id == "2400002802000", text_corr := "rien ne m'a plu chez lui"]

# Suppression des interventions de l'enquêteur·ice
dt_plu[doc_id == "3100015801000", text_corr := "ne sait plus"]
dt_plu[doc_id == "2600007701000", text_corr := "son physique ne sait pas"]
dt_plu[doc_id == "4100044001000", text_corr := "ne se souvient plus"]
dt_plu[doc_id == "7200012201000", text_corr := "sa gentillesse et quel cretin!!!"]


## Refus ----

dt_plu[text %in% c("0", "1", "?", "??", "????", "x"), text_corr := ""]

# "refus de repondre" -> "refus"
dt_plu[grep("refus de repondre", text_corr), text_corr := "refus"]
dt_plu[doc_id == "9300092501000", text_corr := "nesaitplus refus"]

# "ne répond pas"
dt_plu[grep("ne repond pas", text_corr), text_corr := "refus"]

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

# "ne veut pas dire", "ne veut rien dire"
dt_plu[grep("ne veut (pas|rien) dire", text_corr),
       text_corr := stri_replace_all(text_corr, 
                          "refus",
                          regex = "ne veut (pas|rien) dire")]


dt_plu[doc_id %in% c("2100056001000",
                     "8200080501000",
                     "9100054901000",
                     "4100012701000",
                     "8300005801000",
                     "9300011401000",
                     "2300028901000",
                     "5300017201000"), 
       text_corr := "refus"]


## Ne sait pas ----

# "je ne sais pas", "ne sait pas", etc.
dt_plu[grep("sai(s|t) pas", text_corr), 
       .(text_corr,
         stri_replace_all(gsub("[[:punct:][:blank:]]+", " ", text_corr), 
                          " nesaitpas", 
                          regex = "(^| |\")(je |il |elle )?(ne )?sai(s|t) pas( trop)?"))
       ] #%>% fwrite("~/Desktop/test_nesaitpas.csv") 
dt_plu[grep("sai(s|t) pas", text_corr), 
       text_corr := stri_trim(stri_replace_all(gsub("[[:punct:][:blank:]]+", " ", text_corr), 
                                               " nesaitpas", 
                                               regex = "(^| |\")(je |il |elle )?(ne )?sai(s|t) pas( trop)?"))] 

dt_plu[grep("aucune idee", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "nesaitpas",
                                     regex = "aucune idee")]

dt_plu[text == "n s p", text_corr := "nesaitpas"]
dt_plu[text == "ras", text_corr := "nesaitpas"]
dt_plu[doc_id == "8200040401000", text_corr := "nesaitpas"]

dt_plu[doc_id == "5300067201000", text_corr := "nesaitpas nepeutpasdire ca c est fait comme ca"]

# "je n'en sais rien", etc.
dt_plu[doc_id %in% c("5200070201000",
                     "7200039901000",
                     "8200041701000",
                     "9300104001000",
                     "7200020801000"), text_corr := "nesaitpas"]


## Ne peut pas dire --

dt_plu[grep("(je )?(ne )?peu(t|x) (pas|rien) (vous )?dire", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "nepeutpasdire",
                                     regex = "(je )?(ne )?peu(t|x) (pas|rien) (vous )?dire")]

dt_plu[grep("je ne saurais pas", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "nepeutpasdire",
                                     regex = "je ne saurais pas( dire| comment decrire ca)?")]

dt_plu[grep("repondre", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "nepeutpasdire",
                                     regex = "(je )?ne peu(t|x) pa(s|r) repondre( en detail)?")]

dt_plu[doc_id %in% c("9300014301000",
                     "5200044701000"), text_corr := "nepeutpasdire"]
dt_plu[doc_id == "2100032901000", text_corr := "un tout nepeutpasdire"]
dt_plu[grep("(c est )?(tres )?(difficile|impossible) a dire", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "nepeutpasdire",
                                     regex = "(c est )?(tres )?(difficile|impossible) a dire")]

dt_plu[grep("explique", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "nepeutpasdire",
                                     regex = "(je |ca )?ne (sais|peut) pas expliquer")]
dt_plu[doc_id == "7200048501000", text_corr := "son charme nepeutpasdire"]


# Ça ne s'explique pas
dt_plu[grep("explique", text_corr),
       text_corr := stri_replace_all(text_corr, 
                          "nesexpliquepas",
                          regex = "(ca |cela )?(ne )?s explique pas( trop)?")]
dt_plu[grep("explique", text_corr),
       text_corr := stri_replace_all(text_corr, 
                          "nesexpliquepas",
                          regex = "ca ne peut pas s expliquer")]



## Ne sait plus ----

dt_plu[doc_id == "7200047701000", text_corr := "nesaitplus. attirance physique."]

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


dt_plu[grep("rappelle", text_corr),
       .(text_corr,
         stri_replace_all(text_corr, 
                          " nesaitplus",
                          regex = "(^| )(je )?(ne )?(me |se )?rappelle plus"))
] #%>% fwrite("~/Desktop/test_nesaitplus.csv") 
dt_plu[grep("rappelle", text_corr),
       text_corr := stri_trim(stri_replace_all(text_corr, 
                                               " nesaitplus",
                                               regex = "(^| )(je )?(ne )?(me |se )?rappelle plus"))]

dt_plu[grep("souvenir", text_corr),
       text_corr := stri_trim(stri_replace_all(text_corr, 
                                               "nesaitplus",
                                               regex = "aucun(s)? souvenir(s)?"))]


dt_plu[doc_id %in% c("2600034701000",
                     "2500006801000",
                     "7200004801000",
                     "1100182001000",
                     "3100021501000",
                     "7200065201000",
                     "5200063601000",
                     "8200039901000"), 
       text_corr := "nesaitplus"]


## Conversion d'expressions composées de plusieurs mots en forme unique ----

# Coup de foudre
dt_plu[grep("coup de foudre", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "coupdefoudre",
                                     regex = "coup de foudre")]

# Coup de coeur
dt_plu[grep("coup", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "coupdecoeur",
                                     regex = "coup de coeur")]

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
dt_plu[grep("sur", text_corr), 
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
dt_plu[grep("tout", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "riendutout",
                                     regex = "rien du tout")]

# Rien de spécial
dt_plu[grep("rien de special", text_corr),
       text_corr := stri_replace_all(text_corr, 
                                     "riendespecial",
                                     regex = "rien de special(e)?")]

# Rien de particulier
dt_plu[doc_id %in% c("2100068301000",
                     "2300016501000"), text_corr := "riendeparticulier"]
dt_plu[doc_id == "1100064501000", text_corr := "une impression d'ensemble riendeparticulier"]
dt_plu[grep("rien (de|en) particulier", text_corr), 
       text_corr := stri_trim(stri_replace_all(text_corr,
                                               "riendeparticulier",
                                               regex = "rien (de|en) particulier"))]


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
                                     regex = "bad boy")]

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


# Correction manuelle des lemmes et catégories grammaticales :  

dict_corr[Word == "accueillant", `:=`(Lemme = "accueillant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "active", `:=`(Lemme = "actif", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "affirme", `:=`(Lemme = "affirmé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "affirmee", `:=`(Lemme = "affirmé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "age", `:=`(Lemme = "âge", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "âgé", `:=`(Lemme = "âgé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "alcoolique", `:=`(Lemme = "alcoolique", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "aller", `:=`(Lemme = "aller", Stopword = "Stopword", Cat_gram = "VER")]
dict_corr[Word == "amusant", `:=`(Lemme = "amusant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "apaisant", `:=`(Lemme = "apaisant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "apprete", `:=`(Lemme = "apprêté", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "arrange", `:=`(Lemme = "arrangé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "arrangee", `:=`(Lemme = "arrangé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "atomes", `:=`(Lemme = "atomes", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "attachant", `:=`(Lemme = "attachant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "attention", `:=`(Lemme = "attention", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "attirant", `:=`(Lemme = "attirant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "attire", `:=`(Lemme = "attiré", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "attiree", `:=`(Lemme = "attiré", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "aura", `:=`(Lemme = "aura", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "aussi", `:=`(Lemme = "aussi", Stopword = "Stopword", Cat_gram = "ADV")]
dict_corr[Word == "autres", `:=`(Lemme = "autres", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "avoir", `:=`(Lemme = "avoir", Stopword = "Stopword", Cat_gram = "AUX")]
dict_corr[Word == "basane", `:=`(Lemme = "basané", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "basse", `:=`(Lemme = "basse", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "bel", `:=`(Lemme = "beau", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "bilingue", `:=`(Lemme = "bilingue", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "blagueur", `:=`(Lemme = "blagueur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "blond", `:=`(Lemme = "blond", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "blonde", `:=`(Lemme = "blond", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "blonds", `:=`(Lemme = "blond", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "boite", `:=`(Lemme = "boîte", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "boucles", `:=`(Lemme = "bouclé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "bouille", `:=`(Lemme = "bouille", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "bourre", `:=`(Lemme = "bourré", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "boutentrain", `:=`(Lemme = "boutentrain", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "bronze", `:=`(Lemme = "bronzé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "ca", `:=`(Lemme = "ca", Stopword = "Stopword", Cat_gram = "PRO:dem")]
dict_corr[Word == "calin", `:=`(Lemme = "câlin", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "calme", `:=`(Lemme = "calme", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "car", `:=`(Lemme = "car", Stopword = "Stopword", Cat_gram = "CON")]
dict_corr[Word == "carre", `:=`(Lemme = "carré", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "catalan", `:=`(Lemme = "catalan", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "celui", `:=`(Lemme = "celui", Stopword = "Stopword", Cat_gram = "PRO:dem")]
dict_corr[Word == "chaque", `:=`(Lemme = "chaque", Stopword = "Stopword", Cat_gram = "ADJ:ind")]
dict_corr[Word == "charmeur", `:=`(Lemme = "charmeur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "charmeurs", `:=`(Lemme = "charmeur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "cheveu", `:=`(Lemme = "cheveux", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "cheveux", `:=`(Lemme = "cheveux", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "chez", `:=`(Lemme = "chez", Stopword = "Stopword", Cat_gram = "PRE")]
dict_corr[Word == "comique", `:=`(Lemme = "comique", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "complice", `:=`(Lemme = "complice", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "complete", `:=`(Lemme = "compléter", Stopword = "", Cat_gram = "VER")]
dict_corr[Word == "conformiste", `:=`(Lemme = "conformiste", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "cote", `:=`(Lemme = "côté", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "coupe", `:=`(Lemme = "coupe", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "courant", `:=`(Lemme = "courant", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "courbes", `:=`(Lemme = "courbes", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "cours", `:=`(Lemme = "cours", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "cultive", `:=`(Lemme = "cultivé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "cultivee", `:=`(Lemme = "cultivé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "decale", `:=`(Lemme = "décalé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "decontracte", `:=`(Lemme = "décontracté", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "decontractee", `:=`(Lemme = "décontracté", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "devant", `:=`(Lemme = "devant", Stopword = "Stopword", Cat_gram = "ADV")]
dict_corr[Word == "different", `:=`(Lemme = "différent", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "directive", `:=`(Lemme = "directif", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "directrice", `:=`(Lemme = "directrice", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "distingue", `:=`(Lemme = "distingué", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "distinguee", `:=`(Lemme = "distingué", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "donc", `:=`(Lemme = "donc", Stopword = "Stopword", Cat_gram = "CON")]
dict_corr[Word == "dont", `:=`(Lemme = "dont", Stopword = "Stopword", Cat_gram = "PRO:int")]
dict_corr[Word == "du", `:=`(Lemme = "devoir", Stopword = "Stopword", Cat_gram = "VER")]
dict_corr[Word == "echange", `:=`(Lemme = "échanger", Stopword = "", Cat_gram = "VER")]
dict_corr[Word == "echanges", `:=`(Lemme = "échanger", Stopword = "", Cat_gram = "VER")]
dict_corr[Word == "eclatant", `:=`(Lemme = "éclatant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "eduque", `:=`(Lemme = "éduqué", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "effarouche", `:=`(Lemme = "effarouché", Stopword = "", Cat_gram = "VER")]
dict_corr[Word == "effrontee", `:=`(Lemme = "effronté", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "elance", `:=`(Lemme = "élancé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "elancee", `:=`(Lemme = "élancé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "eleve", `:=`(Lemme = "élevé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "entoure", `:=`(Lemme = "entouré", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "entre", `:=`(Lemme = "entre", Stopword = "Stopword", Cat_gram = "PRE")]
dict_corr[Word == "entreprenant", `:=`(Lemme = "enreprenant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "envers", `:=`(Lemme = "envers", Stopword = "Stopword", Cat_gram = "PRE")]
dict_corr[Word == "epaules", `:=`(Lemme = "épaules", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "etai", `:=`(Lemme = "être", Stopword = "Stopword", Cat_gram = "NOM")]
dict_corr[Word == "etaient", `:=`(Lemme = "être", Stopword = "Stopword", Cat_gram = "VER")]
dict_corr[Word == "etais", `:=`(Lemme = "être", Stopword = "Stopword", Cat_gram = "VER")]
dict_corr[Word == "etait", `:=`(Lemme = "être", Stopword = "Stopword", Cat_gram = "VER")]
dict_corr[Word == "etant", `:=`(Lemme = "être", Stopword = "Stopword", Cat_gram = "VER")]
dict_corr[Word == "etc", `:=`(Lemme = "etc", Stopword = "Stopword", Cat_gram = "ADV")]
dict_corr[Word == "ete", `:=`(Lemme = "être", Stopword = "Stopword", Cat_gram = "VER")]
dict_corr[Word == "etiez", `:=`(Lemme = "être", Stopword = "Stopword", Cat_gram = "VER")]
dict_corr[Word == "etions", `:=`(Lemme = "être", Stopword = "Stopword", Cat_gram = "VER")]
dict_corr[Word == "etranger", `:=`(Lemme = "étranger", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "etrangere", `:=`(Lemme = "étranger", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "etudes", `:=`(Lemme = "études", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "etudiant", `:=`(Lemme = "étudiant", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "exterieur", `:=`(Lemme = "extérieur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "facilite", `:=`(Lemme = "facilité", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "facilites", `:=`(Lemme = "facilité", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "femmes", `:=`(Lemme = "femmes", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "fesses", `:=`(Lemme = "fesses", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "filles", `:=`(Lemme = "filles", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "fonces", `:=`(Lemme = "foncé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "fonceuse", `:=`(Lemme = "fonceur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "forcement", `:=`(Lemme = "forcément", Stopword = "", Cat_gram = "ADV")]
dict_corr[Word == "formes", `:=`(Lemme = "formes", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "foutu", `:=`(Lemme = "foutu", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "frimeur", `:=`(Lemme = "frimeur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "frises", `:=`(Lemme = "frisé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "galere", `:=`(Lemme = "galère", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "garcons", `:=`(Lemme = "garçons", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "gaule", `:=`(Lemme = "gaulé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "general", `:=`(Lemme = "général", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "generale", `:=`(Lemme = "général", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "gestuelle", `:=`(Lemme = "gestuelle", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "gorges", `:=`(Lemme = "gorges", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "habille", `:=`(Lemme = "habillé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "haut", `:=`(Lemme = "haut", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "hommes", `:=`(Lemme = "hommes", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "hum", `:=`(Lemme = "hum", Stopword = "Stopword", Cat_gram = "ONO")]
dict_corr[Word == "humoriste", `:=`(Lemme = "humoriste", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "important", `:=`(Lemme = "important", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "imposant", `:=`(Lemme = "imposant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "impressionnant", `:=`(Lemme = "impressionnant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "inconnu", `:=`(Lemme = "inconnu", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "inconnus", `:=`(Lemme = "inconnu", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "instruite", `:=`(Lemme = "instruit", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "integre", `:=`(Lemme = "intègre", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "intellectuelles", `:=`(Lemme = "intellectuel", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "intellectuels", `:=`(Lemme = "intellectuel", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "interdit", `:=`(Lemme = "interdit", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "interessant", `:=`(Lemme = "intéressant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "intriguant", `:=`(Lemme = "intriguant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "jambes", `:=`(Lemme = "jambes", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "juif", `:=`(Lemme = "juif", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "julien", `:=`(Lemme = "julien", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "jusqu", `:=`(Lemme = "jusqu", Stopword = "Stopword", Cat_gram = "PRE")]
dict_corr[Word == "lequel", `:=`(Lemme = "lequel", Stopword = "Stopword", Cat_gram = "PRO:int")]
dict_corr[Word == "lettres", `:=`(Lemme = "lettres", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "manieres", `:=`(Lemme = "manières", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "maquille", `:=`(Lemme = "maquillé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "maquillee", `:=`(Lemme = "maquillé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "marrons", `:=`(Lemme = "marron", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "mat", `:=`(Lemme = "mat", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "mate", `:=`(Lemme = "mat", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "mecs", `:=`(Lemme = "mecs", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "meme", `:=`(Lemme = "même", Stopword = "", Cat_gram = "ADV")]
dict_corr[Word == "memes", `:=`(Lemme = "même", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "menu", `:=`(Lemme = "menu", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "metisse", `:=`(Lemme = "métis", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "mimi", `:=`(Lemme = "mimi", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "mort", `:=`(Lemme = "mort", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "motive", `:=`(Lemme = "motivé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "mots", `:=`(Lemme = "mots", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "moulee", `:=`(Lemme = "moulée", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "mur", `:=`(Lemme = "mûr", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "mure", `:=`(Lemme = "mûr", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "muscle", `:=`(Lemme = "musclé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "muscles", `:=`(Lemme = "musclé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "musulmans", `:=`(Lemme = "musulman", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "nichons", `:=`(Lemme = "nichons", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "nouvelle", `:=`(Lemme = "nouveau", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "oblige", `:=`(Lemme = "obligé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "ondules", `:=`(Lemme = "ondulés", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "orange", `:=`(Lemme = "orange", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "ouvert", `:=`(Lemme = "ouvert", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "ouverte", `:=`(Lemme = "ouvert", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "parce", `:=`(Lemme = "parce", Stopword = "Stopword", Cat_gram = "ADV")]
dict_corr[Word == "parcours", `:=`(Lemme = "parcours", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "parente", `:=`(Lemme = "parenté", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "parents", `:=`(Lemme = "parents", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "parfait", `:=`(Lemme = "parfait", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "parfaite", `:=`(Lemme = "parfait", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "passer", `:=`(Lemme = "passer", Stopword = "", Cat_gram = "VER")]
dict_corr[Word == "passions", `:=`(Lemme = "passion", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "patient", `:=`(Lemme = "patient", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "peche", `:=`(Lemme = "pêche", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "penetrant", `:=`(Lemme = "pénétrant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "peps", `:=`(Lemme = "peps", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "personne", `:=`(Lemme = "personne", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "physique", `:=`(Lemme = "physique", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "plantes", `:=`(Lemme = "plantes", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "plu", `:=`(Lemme = "plaire", Stopword = "", Cat_gram = "VER")]
dict_corr[Word == "poli", `:=`(Lemme = "poli", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "polie", `:=`(Lemme = "poli", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "porte", `:=`(Lemme = "porter", Stopword = "", Cat_gram = "VER")]
dict_corr[Word == "pose", `:=`(Lemme = "posé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "posee", `:=`(Lemme = "posé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "pourquoi", `:=`(Lemme = "pourquoi", Stopword = "", Cat_gram = "ADV")]
dict_corr[Word == "pratique", `:=`(Lemme = "pratique", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "prevenant", `:=`(Lemme = "prévenant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "prise", `:=`(Lemme = "prise", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "public", `:=`(Lemme = "public", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "puis", `:=`(Lemme = "puis", Stopword = "Stopword", Cat_gram = "CON")]
dict_corr[Word == "qq", `:=`(Lemme = "qq", Stopword = "Stopword", Cat_gram = "")]
dict_corr[Word == "quand", `:=`(Lemme = "quand", Stopword = "Stopword", Cat_gram = "CON")]
dict_corr[Word == "quelqu", `:=`(Lemme = "quelqu", Stopword = "Stopword", Cat_gram = "")]
dict_corr[Word == "quelque", `:=`(Lemme = "quelque", Stopword = "Stopword", Cat_gram = "ADJ:ind")]
dict_corr[Word == "quoi", `:=`(Lemme = "quoi", Stopword = "Stopword", Cat_gram = "PRO:int")]
dict_corr[Word == "raffine", `:=`(Lemme = "raffiné", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "rassurant", `:=`(Lemme = "rassurant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "rebelle", `:=`(Lemme = "rebelle", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "recherche", `:=`(Lemme = "rechercher", Stopword = "", Cat_gram = "VER")]
dict_corr[Word == "reflechi", `:=`(Lemme = "réfléchi", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "reflechie", `:=`(Lemme = "réfléchi", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "repartie", `:=`(Lemme = "répartie", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "repere", `:=`(Lemme = "repère", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "reserve", `:=`(Lemme = "réservé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "reservee", `:=`(Lemme = "réservé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "rieuse", `:=`(Lemme = "rieur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "rondeurs", `:=`(Lemme = "rondeurs", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "roulee", `:=`(Lemme = "roulée", Stopword = "", Cat_gram = "ADJ")]
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
dict_corr[Word == "structuree", `:=`(Lemme = "structuré", Stopword = "", Cat_gram = "VER")]
dict_corr[Word == "surs", `:=`(Lemme = "sûr", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "tenue", `:=`(Lemme = "tenue", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "tete", `:=`(Lemme = "tête", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "tigresse", `:=`(Lemme = "tigresse", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "trace", `:=`(Lemme = "tracé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "travailleur", `:=`(Lemme = "travailleur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "travailleuse", `:=`(Lemme = "travailleur", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "trempe", `:=`(Lemme = "trempé", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "valeurs", `:=`(Lemme = "valeurs", Stopword = "", Cat_gram = "NOM")]
dict_corr[Word == "vantard", `:=`(Lemme = "vantard", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "vers", `:=`(Lemme = "vers", Stopword = "Stopword", Cat_gram = "PRE")]
dict_corr[Word == "violent", `:=`(Lemme = "violent", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "vivant", `:=`(Lemme = "vivant", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "vive", `:=`(Lemme = "vive", Stopword = "", Cat_gram = "ADJ")]
dict_corr[Word == "voila", `:=`(Lemme = "voilà", Stopword = "", Cat_gram = "PRE")]
dict_corr[Word == "yeux", `:=`(Lemme = "yeux", Stopword = "", Cat_gram = "NOM")]

## Exportation des réponses corrigées et du lexique associé
# fwrite(dt_plu[, .(ident_men = doc_id,
#                   text = fifelse(is.na(text_corr), text, text_corr))],
#        "Data/plu1_corr.csv")
#
# fwrite(dict_corr[, .(Word,  Lemme, Stopword, Cat_gram, Occurrences)],
#        "Data/dict_plu1_corr.csv")
