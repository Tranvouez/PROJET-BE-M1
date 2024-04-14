
# Chargement des données : ---- 

TH002 <- read.table("TH0002.csv",header = TRUE,sep = ";")
# Création de la colonne des probabilité de survie
TH002$qx <- c(0, 1 - (TH002[-1, 2] / TH002[1:nrow(TH002) - 1, 2]))
head(TH002)

# Extraction cohorte 
cohort_one_one <- read.table("both_years_one_x_one.txt", header = TRUE, skip = 1)
cohort_one_one <- as.data.frame(lapply(cohort_one_one, as.numeric))

### Extraction Fonction ----

extraction_cohort <- function(YEAR = 1811, PLOT = FALSE, cohort) {
  new_cohort <- cohort[cohort$Year == YEAR,]
  rownames(new_cohort) <- 0:110
  S_x <- 1 - new_cohort$qx
  if(PLOT){
    pv <- log(10000*new_cohort$qx)
    tv <- seq(0, 110, by = 1)
    plot(tv, pv, xlab = "age", ylab = "ln(10000*qx)", type = "l", xlim = c(5, 110), ylim = c(2, 9))
    title(paste("Evolution des probabilités de décés en ",YEAR))
  }
  return(list(S_x = S_x, new_cohort = new_cohort))
}


# On les mets au même format que la table TH0002
cohort_1811 <- extraction_cohort(1811, F, cohort = cohort_one_one)[[2]]
TM_1811 <- cohort_1811[c(1:104),c(2,6,4)]
cohort_1861 <- extraction_cohort(1861, F, cohort = cohort_one_one)[[2]]
TM_1861 <- cohort_1861[c(1:108),c(2,6,4)]
cohort_1911 <- extraction_cohort(1911, F, cohort = cohort_one_one)[[2]]
TM_1911 <- cohort_1911[c(1:111),c(2,6,4)]
TM_1911[111,1] <- 110

# Création des tables pour les âges physiologiques :

P_1811 <- round(readRDS("RDS/P_1811.rds"),10)
P_1861 <- round(readRDS("RDS/P_1861.rds"),10)
P_1911 <- round(readRDS("RDS/P_1911.rds"),10)
P_1811_200 <- round(readRDS("RDS/P_1811_200.rds"),10)
P_1861_200 <- round(readRDS("RDS/P_1861_200.rds"),10)
P_1911_200 <- round(readRDS("RDS/P_1911_200.rds"),10)

crea_table_bio <- function(vecteur_proba){
  th_ap <- data.frame(
    Age = c(0:200), 
    lx = rep(0, 201),
    qx = rep(0, 201)
  )
  th_ap$qx[1:200] <- vecteur_proba
  th_ap$qx[201] <- 1
  px <- 1- th_ap$qx
  th_ap$lx[1] = 1000000000
  for (i in 2:200){ 
    th_ap$lx[i] = th_ap$lx[i-1] * px[i-1]
  }
  return(th_ap)
}

th_1811_bio <- crea_table_bio(P_1811)
th_1861_bio <- crea_table_bio(P_1861)
th_1911_bio <- crea_table_bio(P_1911)
th_1811_200_bio <- crea_table_bio(P_1811_200)
th_1861_200_bio <- crea_table_bio(P_1861_200)
th_1911_200_bio <- crea_table_bio(P_1911_200)

##########################################################
              # Comparaison des primes : ----
##########################################################

# Importation des tables simulées d'âge physiologique : 

th_1811 <- read.csv2("Export/table_morta_1811.csv")[, -1]
th_1861 <- read.csv2("Export/table_morta_1861.csv")[, -1]
th_1911 <- read.csv2("Export/table_morta_1911.csv")[, -1]
th_1811_200 <- read.csv2("Export/table_morta_1811_200.csv")[, -1]
th_1861_200 <- read.csv2("Export/table_morta_1861_200.csv")[, -1]
th_1911_200 <- read.csv2("Export/table_morta_1911_200.csv")[, -1]

source("partie_1_fonctions_shiny.R") # Chargement du fichier des fonctions :

### Test sur des fonctions : ----
primes <- function(table, age) {
  df <-   data.frame( 
    dix_ans = round(prime_brute_coti_n_années(age = age, nb_années = 10, capital = 1000, theta1 = 0.05, theta2 = 0, theta3 = 0.1, table_morta = table),2),
    vingt_ans = round(prime_brute_coti_n_années(age = age, nb_années = 20, capital = 1000, theta1 = 0.05, theta2 = 0, theta3 = 0.1, table_morta = table),2),
    trente_ans = round(prime_brute_coti_n_années(age = age, nb_années = 30, capital = 1000, theta1 = 0.05, theta2 = 0, theta3 = 0.1, table_morta = table),2),
    vie =  round(prime_brute_coti_vie(age = age, capital = 1000, theta1 = 0.05, theta2 = 0, theta3 = 0.1, table_morta = table),2),
    unique = round(prime_brute_unique(age = age, capital = 1000, theta1 = 0.05, theta2 = 0, theta3 = 0.1, table_morta = table),2)
  )
  return(df)
}

age = 70
# Table des primes comparaison années : 
{
df <- rbind(primes(TM_1811, age = age), primes(th_1811, age = age), primes(th_1811_200, age = age), 
            primes(th_1811_bio, age = age), primes(th_1811_200_bio, age = age),
            c("", "", "", "", ""),
            primes(TM_1861, age = age), primes(th_1861, age = age), primes(th_1861_200, age = age), 
            primes(th_1861_bio, age = age), primes(th_1861_200_bio, age = age),
            c("", "", "", "", ""),
            primes(TM_1911, age = age), primes(th_1911, age = age), primes(th_1911_200, age = age),
            primes(th_1911_bio, age = age), primes(th_1911_200_bio, age = age),
            c("", "", "", "", ""),
            primes(TH002, age = age))
rownames(df) <- c("1811", "1811 simu" , "1811 simu adj 200", "AB 1811", "AB 1811 simu adj 200", "_",
                  "1861", "1861 simu", "1861 simu adj 200","AB 1861", "AB 1861 simu adj 200", "__",
                  "1911", "1911 simu", "1911 simu adj 200", "AB 1911", "AB 1911 simu adj 200", "___",
                  "TH0002")
cat(paste(" Table des primes 1000€ sur les bases de mortalité réelles \n","Primes pour une personne de ", age," ans : \n"))
df
}

# Statistiques et comparaisons sur les tables : 

# On va maintenant comparer les tables de mortalité biologique :

#### Annexes utiles pour les statistiques : ----

### Espérance de vie selon le modèle : ----
esp = function(matrice, n = 200) {
  e = rep(1, n)
  alpha = rep(0, n)
  alpha[1] = 1
  return (-t(alpha) %*% solve(matrice) %*% e)
}

### Chargements : ----
M_1811 <- readRDS("RDS/M_1811.rds")
M_1861 <- readRDS("RDS/M_1861.rds")
M_1911 <- readRDS("RDS/M_1911.rds")
M_1811_200 <- readRDS("RDS/M_1811_200.rds")
M_1861_200 <- readRDS("RDS/M_1861_200.rds")
M_1911_200 <- readRDS("RDS/M_1911_200.rds")
Age_1811 <- readRDS("RDS/Age_1811.rds")
Age_1861 <- readRDS("RDS/Age_1861.rds")
Age_1911 <- readRDS("RDS/Age_1911.rds")
Age_1811_200 <- readRDS("RDS/Age_1811_200.rds")
Age_1861_200 <- readRDS("RDS/Age_1861_200.rds")
Age_1911_200 <- readRDS("RDS/Age_1911_200.rds")

### Moyenne des âges réels et biologiques : -----

df_stats_age <- data.frame(
  "Année" = c("1811", "1861", "1911"),
  "Esp vie modele " = c(esp(M_1811), esp(M_1861), esp(M_1911)),
  "Esp vie modele 200 " = c(esp(M_1811_200), esp(M_1861_200), esp(M_1911_200)),
  "Esp vie réelle cohort" = c(cohort_1811$ex[1], cohort_1861$ex[1], cohort_1911$ex[1]), 
  "Moyenne age r simu" = c(mean(Age_1811$age_r), mean(Age_1861$age_r), mean(Age_1911$age_r)),
  "Moy age r simu 200" = c(mean(Age_1811_200$age_r), mean(Age_1861_200$age_r), mean(Age_1911_200$age_r)),
  "Moy age b simu " = c(mean(Age_1811$age_bio), mean(Age_1861$age_bio), mean(Age_1911$age_bio)),
  "Moy age b simu 200" = c(mean(Age_1811_200$age_bio), mean(Age_1861_200$age_bio), mean(Age_1911_200$age_bio))
)
df_stats_age <- round(t(as.data.frame(lapply(df_stats_age, as.numeric))),2)
colnames(df_stats_age) <- as.character(df_stats_age[1,])
df_stats_age <- df_stats_age[-1,]
df_stats_age 


### Fonction RMSE simple : ----
rmse <- function(y, y_hat) {
  sqrt(mean((y - y_hat)^2))
}

df_rmse <- data.frame(
  "Année" = c("1811", "1861", "1911"),
  "S&TM " = c(rmse(th_1811$qx[1:102], TM_1811$qx[1:102]), rmse(th_1861$qx[1:106], TM_1861$qx[1:106]), rmse(th_1911$qx[1:109], TM_1911$qx[1:109])),
  "S200&TM " = c(rmse(th_1811_200$qx[1:102], TM_1811$qx[1:102]), rmse(th_1861_200$qx[1:103], TM_1861$qx[1:103]), rmse(th_1911_200$qx[1:108], TM_1911$qx[1:108])),
  "Bio&TM" = c(rmse(th_1811$qx[1:102], th_1811_bio$qx[1:102]), rmse(th_1861$qx[1:103], th_1861_bio$qx[1:103]), rmse(th_1911$qx[1:108], th_1911_bio$qx[1:108])), 
  "Bio200&TM" = c(rmse(th_1811_200$qx[1:102], th_1811_bio$qx[1:102]), rmse(th_1861_200$qx[1:103], th_1861_bio$qx[1:103]), rmse(th_1911_200$qx[1:108], th_1911_bio$qx[1:108]))
  )
df_rmse

## Résumé des statistiques : ----
df
df_rmse
df_stats_age








