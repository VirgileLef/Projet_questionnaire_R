#script : Démarrage----
#commentaire : Ce script sera l'excutable de toute les fonctions des différentes fichiers scripts que nous aurons
#Author : LEFEBVRE Virgile
#Créé le : 19/10/2025
#dernière modification : 19/10/2025
#texte defaut encoding : UTF-8
#R version R : 4.5.1 (2025-06-13 ucrt)
#install.packages("tidyverse")
#install.packages("viridis")
#install.packages("tidyverse")
#install.packages("rmarkdown")

library("tidyverse")
library(dplyr)    # v1.1.4
library(tidyr)    # v1.3.1
library(ggplot2)  # v3.5.2
library(viridis)  # v0.6.5 


data <- read.delim("input/data_test.csv", header = TRUE, sep = ",", fileEncoding = "UTF-8")

  #Visualisation des questionnaires et réponses du questionnaire
  head(data)
  #repérage des types des variables (charactere, string, integer, numeric etc)
  summary(data)
  
  #création d'une matrice pour centraliser les informations de chaque variables (moyenne, variance, ... etc)
  matrice <- matrix(nrow=0, ncol=ncol(data))  # 0 lignes car on les ajouteras au fur et à mesure, 80 colonnes car il en faut une pour chaque variables
  matrice <- rbind(matrice, data[0,])
    
  source("script/Fonction_detect_var_type.R")
  var_type <- detect_type_var(data)
  matrice <- rbind(matrice, var_type)
  
  
  source("script/Fonction_compt_des_rep.R")
  new_var_integer <- compt_des_rep(data)
  new_var_integer
  
  source("script/Fonction_tableau_d_effectif.R")
  #boucle sur toutes les variables
  for(i in names(data)) {
    tableau_d_effectif(data[i],i) 
  }
  source("script/Fonction_graph_en_bar.R")
  #boucle sur toutes les variables
  for(i in names(data)) {
    graph_en_bar(data[[i]],i)
  }
  
  source("script/Fonction_graph_camembert.R")
  #boucle sur toutes les variables
  for(i in names(data)) {
    graph_camembert(data[[i]],i)
  }
  
  # code permettant de faire une martice de corrélation à partir des données 
  matrice_cor <- matrix(nrow=0, ncol=ncol(new_var_integer))
  for(i in names(new_var_integer)) {
    # création d'un vecteur vide pour stocker les valeurs de corrélation
    vect_cor <- numeric()
    for(y in names(new_var_integer)) {
      # calcul du coefficient de corrélation 
      vect_cor <- c(vect_cor, cor(new_var_integer[[i]], new_var_integer[[y]], use = "complete.obs"))
    }
    # rajoute dans la matrice qui stock toute les données
    matrice <- rbind(matrice, vect_cor)
    # création d'une matrice de corrélation
    matrice_cor <- rbind(matrice_cor, vect_cor)
  }
  
  # code permettant d'exécuter du code que si la corrélation est au dessus d'un certain seuil 
  for(i in names(new_var_integer)) {
    vect_cor <- numeric()
    for(y in names(new_var_integer)) {
      coeff_r <- cor(new_var_integer[[i]], new_var_integer[[y]], use = "complete.obs")
      if (abs(coeff_r) > 0.60) {
        if (i < y) { # évite les corrélations entre les mêmes variables sinon la boucle retomberait plusieurs fois entre les mêmes variables 
        cat("Forte corrélation entre la variable :", i, "et la variable :", y, "avec r =", coeff_r, "\n")
        }
      }
    }
  }
  
  data_chi2 <- data
  # Initialisation de la matrice des V de Cramer
  matrice_cramer_v <- matrix(nrow = 0, ncol = ncol(data_chi2))
  colnames(matrice_cramer_v) <- names(data_chi2)
  
  # Boucle principale (ligne)
  for (i in names(data_chi2)) {
    # Vecteur pour stocker les V de Cramer de la ligne 'i'
    vect_cramer_v <- numeric()
    # Boucle interne (colonne)
    for (y in names(data_chi2)) {
      # Créer le tableau de contingence (table croisée des fréquences)
      table_contingence <- table(data_chi2[[i]], data_chi2[[y]])
      # Utilisation de tryCatch pour gérer les erreurs (ex: colonnes vides ou numériques)
      cramer_v <- NA
      # Le test Chi-2 nécessite au moins deux modalités dans chaque variable (au moins 2x2)
      if (all(dim(table_contingence) >= 2)) {
        chi2_result <- tryCatch({
          # Calcul du test du Chi-2 
          chisq.test(table_contingence, correct = TRUE)
        }, error = function(e) {
          # Si une erreur se produit (ex: trop peu de cellules > 5), le résultat est NA
          return(NULL)
        })
        # Calcul du V de Cramer si le Chi-2 a réussi
        if (!is.null(chi2_result)) {
          chi2_stat <- chi2_result$statistic
          N <- sum(table_contingence)
          R <- nrow(table_contingence) # Nombre de modalités de la variable i
          C <- ncol(table_contingence) # Nombre de modalités de la variable y
          k <- min(R, C)               # Le minimum entre R et C
          # Formule du V de Cramer
          cramer_v <- sqrt(chi2_stat / (N * (k - 1)))
        }
      }
      # Ajout du V de Cramer au vecteur de la ligne
      vect_cramer_v <- c(vect_cramer_v, cramer_v)
    }
    
    # Ajout de la ligne à la matrice
    matrice_cramer_v <- rbind(matrice_cramer_v, vect_cramer_v)
    # Nommer la nouvelle ligne
    rownames(matrice_cramer_v)[nrow(matrice_cramer_v)] <- i
  }




