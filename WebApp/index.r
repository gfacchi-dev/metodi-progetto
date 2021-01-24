rm(list=ls())
library(shiny)
library(randomForest)
df <- read.csv("../whitewines.csv", stringsAsFactors = FALSE)

# N� totale di osservazioni: 4898
# N� di variabili: 12
# fixed.acidity           --> Acidi Non Volatili
# volatile.acidity        --> Acidi Volatili
# citric.acid             --> Acido Citrico
# residual.sugar          --> Zuccheri Residui
# chlorides               --> Cloruri
# free.sulfur.dioxide     --> Anidride Solforosa Libera
# total.sulfur.dioxide    --> Anidride Solforosa Totale
# density                 --> Densit�
# pH                      --> PH
# sulphates               --> Solfati
# alcohol                 --> % Alcol
# quality                 --> Qualit�


## Fattorizzazione delle colonne
names <- c("acidi_non_volatili", 
           "acidi_volatili", 
           "acido_citrico", 
           "zuccheri_residui", 
           "cloruri", 
           "anidride_solforosa_libera", 
           "anidride_solforosa_totale", 
           "densita", 
           "ph", 
           "solfati", 
           "alcol", 
           "qualita")
colnames(df) <- names

set.seed(1) 

# Aggiunta di una nuova feature (gusto) per la procedura di CLASSIFICAZIONE dei vini
df$gusto <- ifelse(df$qualita < 6, 'cattivo', 'buono')
df$gusto[df$qualita == 6] <- 'normale'
df$gusto <- as.factor(df$gusto)

# Divisione in training e test set 
# 85% Training  --> 4163  Entries
# 15% Test      --> 735   Entries

train_length <- round(0.85 * nrow(df))
train_labels <- sample(1:nrow(df), train_length)
training_values <- df[train_labels,-12]

model <- randomForest(gusto ~ ., data = training_values, ntree=100)
#### WEB APPLICATION ####

source("server.r")
source("ui.r")

shinyApp(ui, server)

