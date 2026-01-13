library("tidyverse")
library(dplyr)    # v1.1.4
library(tidyr)    # v1.3.1
library(ggplot2)  # v3.5.2
library(viridis)  # v0.6.5 

# Chargement des données
data <- read.csv2("input/Questionnaire.csv", header = TRUE, fileEncoding = "ISO-8859-1",colClasses = typevar) # encodage windows nécessaire
typevar <- sapply(data, class)
# typevar[i] <- "character"
# faire la ligne si dessus pour changer une colonne spécifiquement par son numéro de colonne

# typevar["8. Vous arrive-t-il de travailler plus de 45 heures par semaine ?"] <- "Character"
# faire la ligne si dessus pour changer une colonne spécifiquement par son nom de variable

source("script/Demarrage.R")
lancement(data)














































