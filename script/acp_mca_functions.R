###############################################################
# 📌 1. Librairies
###############################################################

install.packages("factoextra")
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(missMDA)
library(stringi)

###############################################################
# 📌 2. Normalisation des noms de colonnes
###############################################################
normalize_cols <- function(data) {
  clean_names <- colnames(data) %>%
    stringi::stri_trans_general("Latin-ASCII") %>% # enlever accents
    str_replace_all("[^A-Za-z0-9]", "_") %>%       # remplacer caractères spéciaux
    str_replace_all("_+", "_") %>%                 # éviter doublons
    str_trim()
  
  new_names <- paste0("Q", seq_along(clean_names))
  message("✔ Colonnes renommées automatiquement : Q1, Q2, Q3, ...")
  names(data) <- new_names
  return(data)
}

###############################################################
# 📌 3. Nettoyage des variables
###############################################################
clean_variables <- function(data, vars) {
  if (!is.data.frame(data)) stop("❌ 'data' doit être un data.frame.")
  
  if (!all(vars %in% colnames(data))) {
    stop(paste0(
      "❌ Certaines variables n'existent pas : ",
      paste(vars[!vars %in% colnames(data)], collapse=", ")
    ))
  }
  
  return(data[, vars, drop = FALSE])
}

###############################################################
# 📌 4. Recodage pour ACP (Likert → 1–4)
###############################################################
recoding <- function(x) {
  dplyr::case_when(
    str_detect(x, "Jamais|Non|0") ~ 1,
    str_detect(x, "Parfois|Plutôt non|1") ~ 2,
    str_detect(x, "Souvent|Plutôt oui|2") ~ 3,
    str_detect(x, "Toujours|Oui|3") ~ 4,
    TRUE ~ NA_real_
  )
}

###############################################################
# 📌 5. ACP automatisée
###############################################################
run_acp <- function(data, vars) {
  message("📊 Lancement de l'ACP sur : ", paste(vars, collapse=", "))
  
  df <- clean_variables(data, vars)
  
  # Recodage automatique
  df <- df %>% mutate(across(everything(), recoding))
  
  # Imputation si NA
  if (any(is.na(df))) {
    message("🔧 Données manquantes détectées → imputation (PCA)…")
    ncp_opt <- estim_ncpPCA(df, ncp.max = 5)$ncp
    df <- imputePCA(df, ncp = ncp_opt)$completeObs
  }
  
  # ACP
  res <- PCA(df, graph = FALSE)
  message("✔ ACP terminée !")
  print(fviz_pca_biplot(res))
  
  return(res)
}

###############################################################
# 📌 6. MCA automatisée
###############################################################
run_mca <- function(data, vars) {
  message("🎲 Lancement de la MCA sur : ", paste(vars, collapse=", "))
  
  df <- clean_variables(data, vars)
  
  # Conversion en facteur
  df <- df %>% mutate(across(everything(), as.factor))
  
  # Imputation MCA
  ncp_est <- estim_ncpMCA(df, ncp.max = 5)
  df <- imputeMCA(df, ncp = ncp_est$ncp)$completeObs
  
  # MCA
  res <- MCA(df, graph = FALSE)
  message("✔ MCA terminée !")
  print(fviz_mca_biplot(res))
  
  return(res)
}

###############################################################
# 📌 7. Détection automatique (ACP vs MCA)
###############################################################
run_auto <- function(data, vars) {
  df <- clean_variables(data, vars)
  
  # Test : toutes colonnes convertibles en numérique ?
  numeric_test <- all(sapply(df, function(x) all(!is.na(suppressWarnings(as.numeric(as.character(x)))))))
  
  if (numeric_test) {
    message("🔍 Variables quantitatives détectées → ACP")
    return(run_acp(data, vars))
  } else {
    message("🔍 Variables qualitatives détectées → MCA")
    return(run_mca(data, vars))
  }
}

###############################################################
# 📌 8. Exemple d’utilisation
# data <- read.csv("anon_quest_OCT.csv", sep=";", fileEncoding="latin1")
# data <- normalize_cols(data)
# run_auto(data, c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8"))
###############################################################
