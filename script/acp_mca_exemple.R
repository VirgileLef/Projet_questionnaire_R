source("script/acp_mca_functions.R")
data <- read.csv2("input/Questionnaire.csv", header = TRUE, fileEncoding = "ISO-8859-1",colClasses = typevar)

data<-data <- normalize_cols(data)
# ACP avec recodage automatique
run_acp(data, c("Q1","Q2","Q3","Q4"))

# MCA
run_mca(data, c("Q10","Q14","Q17"))

# Détection automatique ACP/MCA
run_auto(data, c("Q1","Q2","Q10"))


