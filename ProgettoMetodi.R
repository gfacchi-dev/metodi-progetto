###############################
#                             #
# Autore: Giuseppe Facchi     #
# Matricola: 845662           #
# Metodi Inf. Gest. Aziendale #
#                             #
###############################

# Pulizia variabili
rm(list = ls())
require(Amelia)
library(MASS)
library(plyr)
library(ggplot2)
library(knitr)
library(GGally)
library(viridis)
library(rpart)
library(rpart.plot)
library(heatmaply)
library(RColorBrewer)
library(randomForest)
library(RWeka)
library(rattle)	
library(pastecs)

# Caricamento dataset
df <- read.csv("whitewines.csv", stringsAsFactors = FALSE)
# Visualizzazione dei missing values
missmap(df)
dev.off()
# Non ci sono valori mancanti, si può proseguire con la data exploration senza intaccare l'integrità del dataset

# Primo data exploring del dataset
str(df)
head(df)
tail(df)

# N° totale di osservazioni: 4898
# N° di variabili: 12
# fixed.acidity           --> Acidi Non Volatili (g/L)
# volatile.acidity        --> Acidi Volatili (g/L)
# citric.acid             --> Acido Citrico (g/L)
# residual.sugar          --> Zuccheri Residui (g/L)
# chlorides               --> Cloruri (g/L)
# free.sulfur.dioxide     --> Anidride Solforosa Libera (mg/L)
# total.sulfur.dioxide    --> Anidride Solforosa Totale (mg/L)
# density                 --> Densità
# pH                      --> PH
# sulphates               --> Solfati
# alcohol                 --> % Alcol
# quality                 --> Qualità


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
str(df)

# Sommario delle osservazioni del dataset
kable(summary(df))
kable(stat.desc(df))

############################
#   EXPLORATORY ANALYSIS   #
############################

plotOccorrenze <- function(dfcol, xlab, main, ylim) {
  plot(factor(dfcol), 
       ylim=ylim,
       col=plasma(nlevels(factor(dfcol))),
       xlab=xlab,
       ylab="Occorrenze",
       main=main
  )
  grid()
}

plotNormalDistribution <- function(dfcol, main) {
  qqnorm(dfcol, main = main, col = "brown")
  qqline(dfcol)
}

plotBox <- function(dfcol, main, xlab) {
  boxplot(dfcol, main=main, horizontal = TRUE, col="orange", xlab=xlab)
  abline(h=0.0, lty=2, lwd=2, col="blue")
}

plotDensity <- function(dfcol, main, xlab, ylab) {
  plot(density(dfcol), 
       type = "l", 
       xlab =  xlab, 
       col = "purple", 
       lwd = 3, 
       ylab = "Densità di probabilità", 
       main = main)
}

#Mancano UDM
par(mfrow = c(1, 1))
plotOccorrenze(df$qualita, "Qualità", "Occorenze per qualità", c(0,2500))

par(mfrow = c(2, 2))
plotOccorrenze(df$acidi_non_volatili, "Acidi non volatili", "Occorenze per Acidi non volatili", c(0, 400))
plotNormalDistribution(df$acidi_non_volatili, "QQ plot Acidi non volatili")
plotBox(df$acidi_non_volatili, "Boxplot distribuzione Acidi non volatili", "Acidi non volatili")
plotDensity(df$acidi_non_volatili, "Densità di probabilità distribuzione Acidi non volatili", "Acidi non volatili", "%")


plotOccorrenze(df$acidi_volatili, "Acidi volatili", "Occorenze per Acidi volatili", c(0, 400))
plotNormalDistribution(df$acidi_volatili, "QQ plot Acidi volatili")
plotBox(df$acidi_volatili, "Boxplot distribuzione Acidi volatili", "Acidi volatili")
plotDensity(df$acidi_volatili, "Densità di probabilità distribuzione Acidi volatili", "Acidi volatili", "%")

plotOccorrenze(df$acido_citrico, "Acido citrico", "Occorenze per Acido citrico", c(0, 400))
plotNormalDistribution(df$acido_citrico, "QQ plot Acido citrico")
plotBox(df$acido_citrico, "Boxplot distribuzione Acido citrico", "Acido citrico")
plotDensity(df$acido_citrico, "Densità di probabilità distribuzione Acido citrico", "Acido citrico", "%")

plotOccorrenze(df$zuccheri_residui, "Zuccheri residui", "Occorenze per Zuccheri residui", c(0, 400))
plotNormalDistribution(df$zuccheri_residui, "QQ plot Zuccheri residui")
plotBox(df$zuccheri_residui, "Boxplot distribuzione Zuccheri residui", "Zuccheri residui")
plotDensity(df$zuccheri_residui, "Densità di probabilità distribuzione Zuccheri residui", "Zuccheri residui", "%")

plotOccorrenze(df$cloruri, "Cloruri", "Occorenze per cloruri", c(0, 400))
plotNormalDistribution(df$cloruri, "QQ plot cloruri")
plotBox(df$cloruri, "Boxplot distribuzione cloruri", "cloruri")
plotDensity(df$cloruri, "Densità di probabilità distribuzione cloruri", "cloruri", "%")

plotOccorrenze(df$anidride_solforosa_libera, "Anidride solforosa libera", "Occorenze per Anidride solforosa libera", c(0, 400))
plotNormalDistribution(df$anidride_solforosa_libera, "QQ plot Anidride solforosa libera")
plotBox(df$anidride_solforosa_libera, "Boxplot distribuzione Anidride solforosa libera", "Anidride solforosa libera")
plotDensity(df$anidride_solforosa_libera, "Densità di probabilità distribuzione Anidride solforosa libera", "Anidride solforosa libera", "%")

plotOccorrenze(df$anidride_solforosa_totale, "Anidride solforosa totale", "Occorenze per Anidride solforosa totale", c(0, 400))
plotNormalDistribution(df$anidride_solforosa_totale, "QQ plot Anidride solforosa totale")
plotBox(df$anidride_solforosa_totale, "Boxplot distribuzione Anidride solforosa totale", "Anidride solforosa totale")
plotDensity(df$anidride_solforosa_totale, "Densità di probabilità distribuzione Anidride solforosa totale", "Anidride solforosa totale", "%")

plotOccorrenze(df$densita, "densita", "Occorenze per densità", c(0, 400))
plotNormalDistribution(df$densita, "QQ plot densità")
plotBox(df$densita, "Boxplot distribuzione densità", "densità")
plotDensity(df$densita, "Densità di probabilità distribuzione densità", "densità", "%")

plotOccorrenze(df$ph, "ph", "Occorenze per pH", c(0, 200))
plotNormalDistribution(df$ph, "QQ plot pH")
plotBox(df$ph, "Boxplot distribuzione pH", "pH")
plotDensity(df$ph, "Densità di probabilità distribuzione pH", "pH", "%")

plotOccorrenze(df$solfati, "solfati", "Occorenze per solfati", c(0, 200))
plotNormalDistribution(df$solfati, "QQ plot solfati")
plotBox(df$solfati, "Boxplot distribuzione solfati", "Solfati")
plotDensity(df$solfati, "Densità di probabilità distribuzione solfati", "Solfati", "%")

plotOccorrenze(df$alcol, "alcol", "Occorenze per alcol", c(0, 400))
plotNormalDistribution(df$alcol, "QQ plot alcol")
plotBox(df$alcol, "Boxplot distribuzione alcol", "Alcol")
plotDensity(df$alcol, "Densità di probabilità distribuzione alcol", "Alcol", "%")

# Calcolo della matrice di Correlazione
cor_matrix <- cor(df)

# Heatmap di Correlazione
heatmaply_cor(
  cor_matrix,
  xlab = "Matrice di correlazione tra variabili",
  k_col = 2,
  k_row = 2
)

###########################
#   DATA PRE-PROCESSING   #
###########################
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
training_values <- df[train_labels, ]
test_values <- df[-train_labels, ]


###################
#   REGRESSIONE   #
###################

# Collezione MAE regressione
regr_acc <- c()

# Implementazione di modelli di regressione per la predizione della QUALITA' dei vini in base alle loro proprietà chimiche

# Formula iniziale che contiene tutte le variabili a disposizione (tranne il gusto aggiunto precedentemente)
init_formula <- as.formula("qualita ~ acidi_non_volatili + acidi_volatili + acido_citrico + zuccheri_residui + cloruri + anidride_solforosa_libera + anidride_solforosa_totale + densita + ph + solfati + alcol")

# Modello di Regressione lineare multipla (multivariata) iniziale
init_model <- lm(init_formula, data = training_values)
summary(init_model)

# Adjusted R squared iniziale
init_adj_rsq <- summary(init_model)$adj.r.squared

# Predizione iniziale della qualità per i valori di test
init_prediction <- predict(init_model, newdata = test_values)

# Calcolo dei residui
init_residuals <- test_values$qualita - init_prediction

# Calcolo dell'RMSE
init_mae <- mean(abs(init_residuals))
regr_acc <- rbind(regr_acc, c(paste("MLR init"), round(init_mae, 4)))

# Formula migliorata con la backward elimination scartando le variabili maggiormente correlate
improved_formula <- as.formula("qualita ~ acidi_volatili + acidi_non_volatili + zuccheri_residui + anidride_solforosa_libera + densita + ph + solfati + alcol")

# Modello di Regressione lineare multipla (multivariata) migliorato
improved_model <- lm(improved_formula, data = training_values)
summary(improved_model)

# Adjusted R squared migliorato
improved_adj_rsq <- summary(improved_model)$adj.r.squared

# Predizione migliorata della qualità per i valori di test
improved_prediction <- predict(improved_model, newdata = test_values)

# Calcolo dei residui
improved_residuals <- test_values$qualita - improved_prediction

# Calcolo dell'RMSE
improved_mae <- mean(abs(improved_residuals))
regr_acc <- rbind(regr_acc, c(paste("MLR improved"), round(improved_mae, 4)))

# Visualizzazione dei risultati
print(paste0("Adjusted R-squared iniziale: ", round(init_adj_rsq, 4)))
print(paste0("Adjusted R-squared finale: ", round(improved_adj_rsq, 4)))
print(paste0("Miglioramento di ",  round(improved_adj_rsq, 4) - round(init_adj_rsq, 4))) # Miglioramento Adj R Sq
print(paste0("MAE iniziale: ", round(init_mae, 4)))
print(paste0("MAE finale: ", round(improved_mae, 4)))
print(paste0("Miglioramento di ",  round(init_mae, 4) - round(improved_mae, 4))) # Miglioramento MAE


# ALBERO DI REGRESSIONE SEMPLICE
par(mfrow = c(1,1))
MAE <- function(actual, predicted) { mean(abs(actual - predicted))}

m.rpart <- rpart(qualita ~ . -gusto, data = training_values)
summary(m.rpart)

rpart.plot(m.rpart, digits = 2, fallen.leaves = TRUE, type = 3, extra = 101, main = "Regression Tree Model per la predizione della qualità del vino")
p.rpart <- predict(m.rpart, test_values)
regr_acc <- rbind(regr_acc, c(paste("R part"),round(MAE(p.rpart, test_values$qualita), 4) )) 

# M5P Regression Tree
m.m5p <- M5P(qualita ~ ., data = training_values)
summary(m.m5p)
p.m5p <- predict(m.m5p, test_values)
regr_acc <- rbind(regr_acc, c(paste("M5P"), round(MAE(p.m5p, test_values$qualita), 4))) 

ggplot(as.data.frame(regr_acc), aes(x=factor(regr_acc[,1]), y=factor(regr_acc[,2]))) + 
  geom_bar(stat = "identity", fill=plasma(nlevels(factor(regr_acc[,1])))) +
  xlab("Modello di predizione") +
  ylab("MAE (Mean Absolute Error)") +
  scale_fill_hue(c = 40) +
  theme(legend.position="none") +
  ggtitle("MAE nelle predizioni dei modelli di regressione multivariata (Less is better)")

#######################
#   CLASSIFICAZIONE   #
#######################

#  GUSTO VINO (Ragguppramento della qualità dei vini in 3 macro gruppi di cardinalità simile)
  # Qualità < 6 --> Cattivo Gusto
  # Qualità = 6 --> Normale Gusto
  # Qualità > 6 --> Buon Gusto

# Occorrenze di vini per gusto
table(df$gusto)

# Collezione che contiene le accuracy dei modelli di decisione
acc <- c()

# Funzione per visualizzazione della matrice di confusione
prettyConfused<-function(Actual,Predict,colors=c("white","red","dodgerblue3"),text.scl=5){
  actual = as.data.frame(table(Actual))
  names(actual) = c("Actual","ActualFreq")
  confusion = as.data.frame(table(Actual, Predict))
  names(confusion) = c("Actual","Predicted","Freq")
  confusion = merge(confusion, actual, by=c('Actual','Actual'))
  confusion$Percent = confusion$Freq/confusion$ActualFreq*100
  confusion$ColorScale<-confusion$Percent*-1
  confusion[which(confusion$Actual==confusion$Predicted),]$ColorScale<-confusion[which(confusion$Actual==confusion$Predicted),]$ColorScale*-1
  confusion$Label<-paste(round(confusion$Percent,0),"%, n=",confusion$Freq,sep="")
  tile <- ggplot() +
    geom_tile(aes(x=Actual, y=Predicted,fill=ColorScale),data=confusion, color="black",size=0.1) +
    labs(x="Classe Reale",y="Classe Predetta")
  tile = tile +
    geom_text(aes(x=Actual,y=Predicted, label=Label),data=confusion, size=text.scl, colour="black") +
    scale_fill_gradient2(low=colors[2],high=colors[3],mid=colors[1],midpoint = 0,guide='none')
}

# Modello ad albero semplice per la classificazione dei vini in base al gusto
m.rpart <- rpart(gusto ~ . - qualita, data = training_values)
summary(m.rpart)

# Plot del modello di decisisione
fancyRpartPlot(m.rpart, caption=NULL, main = "Tree Model per classificazione del gusto del vino")

# Predizione della classificazione sui dati di test
p.rpart <- predict(m.rpart, newdata = test_values, type = "class")

# Matrice di confusione 
cm <- table(p.rpart, test_values$gusto)
print(cm)
print(prettyConfused(test_values$gusto, p.rpart))

# Calcolo dell'accuracy
simpTreeAcc <- round(sum(diag(cm)) / nrow(test_values), 4)*100
acc <- rbind(acc, c("Simple", simpTreeAcc))
print(paste("Precisione:", simpTreeAcc, "%"))

# RANDOM FOREST
generateRf <- function(ntrees) {
  model <- randomForest(gusto ~ . - qualita, data = training_values, ntree=ntrees)
  plot(model, log="y", main=paste("Random Forest Model with", ntrees, "trees"))
  pred <- predict(model, newdata = test_values)
  cm <- table(pred, test_values$gusto)
  print(prettyConfused(test_values$gusto, pred))
  accuracy <- round(sum(diag(cm)) / nrow(test_values), 4)*100
  print(paste("Precisione:", accuracy, "%"))
  acc <<- rbind(acc, c(paste("RF", ntrees), accuracy))
}

# Generazione Random Forest con 100 alberi
generateRf(100)
# Generazione Random Forest con 200 alberi
generateRf(200)
# Generazione Random Forest con 300 alberi
generateRf(300)
# Generazione Random Forest con 400 alberi
generateRf(400)

# Visulizzazione delle precisioni delle predizioni di classificazione
ggplot(as.data.frame(acc), aes(x=factor(acc[,1]), y=factor(acc[,2]))) + 
  geom_bar(stat = "identity", fill=plasma(nlevels(factor(acc[,1])))) +
  xlab("Modello di predizione") +
  ylab("Precisione (%)") +
  scale_fill_hue(c = 40) +
  theme(legend.position="none") +
  ggtitle("Accuracy delle predizioni dei tree models per la classificazione del gusto (More is better)")