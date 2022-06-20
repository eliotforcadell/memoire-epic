### Fonctions usuelles ###

# Recodage CS et diplôme ----

## Catégorie socioprofessionnelle
rec_pcs <- function(cs) {
  fcase(cs %in% c(23, 31, 33:38), 1,
        cs %in% c(42:48), 2, 
        cs %in% c(11:13, 21:22), 3,
        cs %in% c(50, 52:56), 4,
        cs %in% c(60, 62:65, 67:69), 5,
        cs %in% c(81, 83:87, 97, 99), 9)
}

## Niveau d'études
rec_niv <- function(age, dip, bac) {
  if(age <= 35) {
    niv <- fcase(dip %in% c(1,2,99), 1,
                 dip %in% 3:4 | bac %in% c(1,2,4), 2,
                 bac %in% c(3,5,9) | dip == 6, 3,
                 dip %in% 7:9, 4)
  } else if(age <= 50) {
    niv <- fcase(dip %in% c(1,2,99), 1,
                 dip %in% 3:4, 2, 
                 dip == 5, 3, 
                 dip %in% 6:9, 4)
  } else {
    niv <- fcase(dip %in% c(1,99), 1,
                 dip == 2, 2,
                 dip %in% 3:5, 3,
                 dip %in% 6:9, 4)
  }
  return(niv)
}



# Mise en forme ----

## Mise en forme des pourcentages
pct <- label_percent(accuracy = 0.1, decimal.mark = ",")
pct2 <- label_percent(accuracy = 1, decimal.mark = ",")
pct_latex <- function(n) {gsub("%", "\\\\%", pct(n))}

## Mise en forme des nombres
virgule <- function(x) gsub(pattern = "\\.", x = x, replacement = ",")
fmt_num <- function(x, digits = 1) virgule(format(round(x, digits), nsmall = digits))

# Mise en forme des p-values
pshort <- function(x) pvalue(x, decimal.mark = ",") 

## Tableau croisé effectifs + pourcentages en ligne
mycrosstab <- function(DT, x, y, w = "None", labx = NULL, laby = NULL) {
  x <- deparse(ensym(x))
  y <- deparse(ensym(y))
  w <- deparse(ensym(w))
  DT <- copy(data.table(DT))
  
  # Pondération
  if(w == "None") {
    DT[, pond := 1]
  } else {
    DT[, pond := get(w)]
  }
  
  # Labels des variables
  if(exists("formats")) {
    xlabs <- formats[[x]]
    ylabs <- formats[[y]]
  } else {
    xlabs <- NULL
    ylabs <- NULL
  }
  
  if(!is.null(labx)) xlabs <- labx
  if(!is.null(laby)) ylabs <- laby
  
  if(is.null(xlabs) | length(xlabs) != DT[, length(unique(get(x)))]) 
    xlabs <- DT[, levels(factor(get(x)))]
  if(is.null(ylabs) | length(ylabs) != DT[, length(unique(get(y)))]) 
    ylabs <- DT[, levels(factor(get(y)))]
  
  # Marges x
  mar_x <- DT[, .(n_mar = sum(pond)), keyby = x]
  
  # Croisement des variables
  tab <- dcast(melt(DT[, .(n = .N, n_pond = sum(pond)), keyby = c(x, y)
  ][mar_x, p := n_pond/i.n_mar, on = x
  ][, `:=`(x = factor(get(x), labels = xlabs),
           y = factor(get(y), labels = ylabs),
           n = as.character(round(n)),
           p = pct(p))],
  measure.vars = c("n", "p")),
  x + variable ~ y, value.var = "value")
  
  # Lignes d'ensemble
  tab_ens <- dcast(melt(DT[, .(n = .N,
                               p = sum(pond)/DT[, sum(pond)]), by = y
  ][, `:=`(x = rep("Ensemble", ncol(tab)-2),
           y = factor(get(y), labels = ylabs),
           n = as.character(round(n)),
           p = pct(p))],
  measure.vars = c("n", "p")),
  x + variable ~ y, value.var = "value")
  
  # Tableau final
  full_tab <- rbind(tab, tab_ens
  )[, variable := NULL
  ][, setnames(.SD, "x", ".")]
  
  # Chi2
  full_tab[, chisq := c(DT[, pshort(wtd.chi.sq(get(x), get(y), weight = pond)[3])],
                        rep("", nrow(full_tab)-1))]
  
  full_tab[]
}


# AFC sur tableau lexical agrégé ----

## Adaptation de la fonction corpus_ca du package R.temis, pour conserver les 
## noms de modalités
mycorpus_ca <- function (corpus, dtm, variables = NULL, ncp = 5, sparsity = 1, 
                         ...) {
  
  if (!isTRUE(all.equal(names(corpus), rownames(dtm)))) 
    stop("`dtm` must have one row per document in `corpus`, with the same names and in the same order.")
  if (!is.null(variables) && !all(variables %in% colnames(meta(corpus)))) 
    stop("All items of 'variables' should be meta-data variables of the corpus.")
  oldMeta <- meta <- meta(corpus)[colnames(meta(corpus)) != 
                                    "MetaID"]
  dict <- attr(dtm, "dict")
  if (sparsity < 1) 
    dtm <- removeSparseTerms(dtm, sparsity)
  invalid <- which(apply(dtm, 1, sum) == 0)
  if (length(invalid) > 0) {
    dtm <- dtm[-invalid, , drop = FALSE]
    meta <- oldMeta[-invalid, , drop = FALSE]
    corpus <- corpus[-invalid]
  }
  ndocs <- nrow(dtm)
  nterms <- ncol(dtm)
  if (ndocs <= 1 || nterms <= 1) 
    stop("Please increase the value of the 'sparsity' parameter so that at least two documents and two terms are retained.")
  if (length(invalid) > 0) {
    message(sprintf("%i documents have been skipped because they do not include any occurrence of the terms retained in the final document-term matrix. Increase the value of the 'sparsity' parameter if you want to include them. These documents are: %s.", 
                    length(invalid), paste(names(invalid), collapse = ", ")))
  }
  skippedVars1 <- character()
  skippedVars2 <- character()
  skippedLevs <- character()
  origVars <- character()
  dupLevels <- any(duplicated(unlist(lapply(meta, function(x) substr(unique(as.character(x[!is.na(x)])), 
                                                                     0, 30)))))
  varDtm <- NULL
  vars <- colnames(meta)
  vars10 <- make.unique(substr(vars, 0, 10))
  vars20 <- make.unique(substr(vars, 0, 20))
  if (ncol(meta) > 0) {
    for (i in 1:ncol(meta)) {
      var <- vars[i]
      levs <- levels(factor(meta[, i]))
      totNLevels <- nlevels(factor(oldMeta[, i]))
      if (length(levs) == 0) {
        skippedVars1 <- c(skippedVars1, var)
        next
      }
      else if (length(levs) > 100) {
        skippedVars2 <- c(skippedVars2, var)
        next
      }
      else if (length(levs) < totNLevels) {
        skippedLevs <- c(skippedLevs, var)
      }
      suppressWarnings(mat <- rollup(dtm[1:ndocs, , drop = FALSE], 
                                     1, meta[i]))
      if (totNLevels == 1 && any(is.na(meta[, i]))) 
        rownames(mat) <- vars20[i]
      else if (dupLevels || !any(is.na(suppressWarnings(as.numeric(levs))))) 
        rownames(mat) <- make.unique(paste(vars10[i], 
                                           substr(levs, 0, 30), sep = "_"))
      else rownames(mat) <- substr(levs, 0, 30)
      varDtm <- rbind(varDtm, mat)
      origVars <- c(origVars, rep(var, nrow(mat)))
    }
  }
  if (!is.null(variables) && sum(origVars %in% variables) < 
      2) 
    stop("Please select active variables so that at least two levels are present in the retained documents.")
  msg <- ""
  if (length(skippedVars1) > 0) 
    msg <- sprintf("Variable(s) %s have been skipped since they contain only missing values for retained documents.", 
                   paste(skippedVars1, collapse = ", "))
  if (length(skippedVars2) > 0) {
    msg2 <- sprintf("Variable(s %s have been skipped since they contain more than 100 levels.", 
                    paste(skippedVars2, collapse = ", "))
    if (msg != "") 
      msg <- paste0(msg, "\n\n", msg2)
    else msg <- msg2
  }
  skippedVars <- unique(c(skippedVars1, skippedVars2))
  if (length(skippedLevs) > 0) {
    msg2 <- sprintf("Some levels of variable(s %s have been skipped since they contain only missing values for retained documents.", 
                    paste(skippedLevs, collapse = ", "))
    if (msg != "") 
      msg <- paste0(msg, "\n\n", msg2)
    else msg <- msg2
  }
  if (msg != "") 
    message(msg)
  newDtm <- as.matrix(rbind(dtm, varDtm))
  #rownames(newDtm) <- make.names(rownames(newDtm))
  if (!is.null(variables)) 
    obj <- FactoMineR::CA(newDtm, row.sup = c(1:nrow(dtm), 
                                              nrow(dtm) + which(!origVars %in% variables)), ncp = ncp, 
                          graph = FALSE)
  else if (nrow(newDtm) - ndocs > 0) 
    obj <- FactoMineR::CA(newDtm, row.sup = (ndocs + 1):nrow(newDtm), 
                          ncp = ncp, graph = FALSE, ...)
  else obj <- FactoMineR::CA(newDtm, ncp = ncp, graph = FALSE, 
                             ...)
  if (nrow(newDtm) - ndocs > 0) {
    obj$rowvars <- seq.int(ndocs + 1, nrow(newDtm))
    names(obj$rowvars) <- origVars
  }
  if (!is.null(dict)) 
    attr(obj, "dict") <- dict
  obj
}
