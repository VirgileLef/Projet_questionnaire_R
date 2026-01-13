library("tidyverse")
library(dplyr)    # v1.1.4
library(tidyr)    # v1.3.1
library(ggplot2)  # v3.5.2
library(viridis)  # v0.6.5 
library(yaml)
library(readr)

# Lecture du fichier de configuration Yaml
config <- yaml::read_yaml("input/config.yaml")

# Lecture des données
data <- read.csv2(file = config$files$input_data, fileEncoding = config$files$encoding, sep = config$files$separator, check.names = FALSE)

# Aperçu
#head(data, 2)
#str(data)

source("script/Demarrage.R")
source("script/Fonction_detect_var_type.R")
detect_type_var(data)
lancement(data)














































