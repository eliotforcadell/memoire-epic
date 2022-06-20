# memoire_epic

Ce dépôt contient l'ensemble des codes ayant servi à la rédaction de mon mémoire de M2 « Un homme plus grand, une femme plus jeune : structuration sociale et biographique des goûts amoureux hétérosexuels ». 

Le mémoire à été rédigé au format Rmarkdown, converti en LaTex et compilé au format PDF. Chaque chapitre a été rédigé dans un script distinct, tous les scripts étant concaténés à l'aide du package R `bookdown` au moment de la compilation du document final. Le code dédié à l'importation des bases de données et au recodage des variables est contenu dans le fichier import_epic.R, et quelques fonctions usuelles sont définies dans le fichier fonctions.R, ces deux fichiers étant exécutés au début du script index.Rmd.

Les données exploitées dans ce mémoire sont celles de l'enquête *Étude des parcours individuels et conjugaux* (Ined-Insee, 2013-2014), et peuvent être obtenues sur demande auprès de l'Ined par l'intermédiaire du portail [Quetelet-Progedo-Diffusion](https://data.progedo.fr/). Les données textuelles exploitées dans le troisième chapitre ont été obtenues directement auprès des concepteur·ices de l'enquête, et les codes ayant servi au nettoyage et à la lemmatisation de ces données sont disponibles dans le dossier Corpus.

