# ----------------------------------------------------------

## Création table de données résultats : 

max_age <- max(TH002[, "Age"])
l = max_age - 40 + 1
c = 6
df_primes <- as.data.frame(matrix(rep(0, l * c), nrow = l, ncol = c))
colnames(df_primes) <-
  c(
    "Age",
    "Paiements vie entière",
    "Paiements 10 ans",
    "Paiements 20 ans",
    "Paiements 30 ans",
    "Paiement unique"
  )

df_primes[, 1] <- c(40:max_age)

for (i in 40:max_age){
  if (i >= max_age - 10){
    df_primes[i-39,2] <- 0
    df_primes[i-39,3] <- 0 
    df_primes[i-39,4] <- 0
    df_primes[i-39,5] <- 0
    df_primes[i-39,6] <- 0
  }
  else if (i >= max_age - 20){ 
    df_primes[i-39,2] <- prime_brute_coti_vie(age = i,capital = 1000)
    df_primes[i-39,3] <- prime_brute_coti_n_années(age = i, capital = 1000, nb_années = 10)
    df_primes[i-39,4] <- 0
    df_primes[i-39,5] <- 0
    df_primes[i-39,6] <- prime_brute_unique(age = i, capital = 1000)
  }
  else if (i >= max_age - 30){ 
    df_primes[i-39,2] <- prime_brute_coti_vie(age = i,capital = 1000)
    df_primes[i-39,3] <- prime_brute_coti_n_années(age = i, capital = 1000, nb_années = 10)
    df_primes[i-39,4] <- prime_brute_coti_n_années(age = i, capital = 1000, nb_années = 20)
    df_primes[i-39,5] <- 0
    df_primes[i-39,6] <- prime_brute_unique(age = i, capital = 1000)
  }
  else{ 
    df_primes[i-39,2] <- prime_brute_coti_vie(age = i,capital = 1000)
    df_primes[i-39,3] <- prime_brute_coti_n_années(age = i, capital = 1000, nb_années = 10)
    df_primes[i-39,4] <- prime_brute_coti_n_années(age = i, capital = 1000, nb_années = 20)
    df_primes[i-39,5] <- prime_brute_coti_n_années(age = i, capital = 1000, nb_années = 30)
    df_primes[i-39,6] <- prime_brute_unique(age = i, capital = 1000)
  }
}


### Création tableau valeur de rachat et réduction : 

max_age <- max(TH002[, "Age"])
l = max_age - 40 + 1
c = 9
rachat_10ans <- as.data.frame(matrix(rep(0, l * c), nrow = l, ncol = c))
rachat_20ans <- as.data.frame(matrix(rep(0, l * c), nrow = l, ncol = c))
rachat_30ans <- as.data.frame(matrix(rep(0, l * c), nrow = l, ncol = c))
rachat_unique <- as.data.frame(matrix(rep(0, l * c), nrow = l, ncol = c))
rachat_vie_entière <- as.data.frame(matrix(rep(0, l * c), nrow = l, ncol = c))

df_init <- function(df) {
  colnames(df) <- c("Age", paste0("Année_",1:8))
  df[, 1] <- c(40:max_age)
  return(df)
}
rachat_vie_entière <- df_init(rachat_vie_entière)
rachat_unique <- df_init(rachat_unique)
rachat_10ans <- df_init(rachat_10ans)
rachat_20ans <- df_init(rachat_20ans)
rachat_30ans <- df_init(rachat_30ans)




for (i in 40:max_age){
  if (i > max_age - 10){
    rachat_vie_entière[i-39,2:9] <- rep(0,8)
    rachat_10ans[i-39,2:9] <- rep(0,8)
    rachat_20ans[i-39,2:9] <- rep(0,8)
    rachat_30ans[i-39,2:9] <- rep(0,8)
    rachat_unique[i-39,2:9] <- rep(0,8)
  }
  else if (i > max_age - 20){ 
    rachat_vie_entière[i-39,2:9] <- f_valeur_rachat_coti_vie(age = i,capital = 1000)
    rachat_10ans[i-39,2:9] <- f_valeur_rachat_coti_n_années(age = i, capital = 1000, nb_années = 10)
    rachat_20ans[i-39,2:9] <- rep(0,8)
    rachat_30ans[i-39,2:9] <- rep(0,8)
    rachat_unique[i-39,2:9] <- f_valeur_rachat_coti_unique(age = i, capital = 1000)  }
  else if (i > max_age - 30){ 
    rachat_vie_entière[i-39,2:9] <- f_valeur_rachat_coti_vie(age = i,capital = 1000)
    rachat_10ans[i-39,2:9] <- f_valeur_rachat_coti_n_années(age = i, capital = 1000, nb_années = 10)
    rachat_20ans[i-39,2:9] <- f_valeur_rachat_coti_n_années(age = i, capital = 1000, nb_années = 20)
    rachat_30ans[i-39,2:9] <- rep(0,8)
    rachat_unique[i-39,2:9] <- f_valeur_rachat_coti_unique(age = i, capital = 1000)
  }
  else{ 
    rachat_vie_entière[i-39,2:9] <- f_valeur_rachat_coti_vie(age = i,capital = 1000)
    rachat_10ans[i-39,2:9] <- f_valeur_rachat_coti_n_années(age = i, capital = 1000, nb_années = 10)
    rachat_20ans[i-39,2:9] <- f_valeur_rachat_coti_n_années(age = i, capital = 1000, nb_années = 20)
    rachat_30ans[i-39,2:9] <- f_valeur_rachat_coti_n_années(age = i, capital = 1000, nb_années = 30)
    rachat_unique[i-39,2:9] <- f_valeur_rachat_coti_unique(age = i, capital = 1000)
  }
}

max_age <- max(TH002[, "Age"])
l = max_age - 40 + 1
c = 9
réduction_10ans <- as.data.frame(matrix(rep(0, l * c), nrow = l, ncol = c))
réduction_20ans <- as.data.frame(matrix(rep(0, l * c), nrow = l, ncol = c))
réduction_30ans <- as.data.frame(matrix(rep(0, l * c), nrow = l, ncol = c))
réduction_unique <- as.data.frame(matrix(rep(0, l * c), nrow = l, ncol = c))
réduction_vie_entière <- as.data.frame(matrix(rep(0, l * c), nrow = l, ncol = c))

df_init <- function(df) {
  colnames(df) <- c("Age", paste0("Année_",1:8))
  df[, 1] <- c(40:max_age)
  return(df)
}
réduction_vie_entière <- df_init(réduction_vie_entière)
réduction_unique <- df_init(réduction_unique)
réduction_10ans <- df_init(réduction_10ans)
réduction_20ans <- df_init(réduction_20ans)
réduction_30ans <- df_init(réduction_30ans)

for (i in 40:max_age){
  if (i > max_age - 10){
    réduction_vie_entière[i-39,2:9] <- rep(0,8)
    réduction_10ans[i-39,2:9] <- rep(0,8)
    réduction_20ans[i-39,2:9] <- rep(0,8)
    réduction_30ans[i-39,2:9] <- rep(0,8)
    réduction_unique[i-39,2:9] <- rep(0,8)
  }
  else if (i > max_age - 20){ 
    réduction_vie_entière[i-39,2:9] <- f_valeur_réduction_coti_vie(age = i,capital = 1000)
    réduction_10ans[i-39,2:9] <- f_valeur_réduction_coti_n_années(age = i, capital = 1000, nb_années = 10)
    réduction_20ans[i-39,2:9] <- rep(0,8)
    réduction_30ans[i-39,2:9] <- rep(0,8)
    réduction_unique[i-39,2:9] <- f_valeur_réduction_coti_unique(age = i, capital = 1000)  }
  else if (i > max_age - 30){ 
    réduction_vie_entière[i-39,2:9] <- f_valeur_réduction_coti_vie(age = i,capital = 1000)
    réduction_10ans[i-39,2:9] <- f_valeur_réduction_coti_n_années(age = i, capital = 1000, nb_années = 10)
    réduction_20ans[i-39,2:9] <- f_valeur_réduction_coti_n_années(age = i, capital = 1000, nb_années = 20)
    réduction_30ans[i-39,2:9] <- rep(0,8)
    réduction_unique[i-39,2:9] <- f_valeur_réduction_coti_unique(age = i, capital = 1000)
  }
  else{ 
    réduction_vie_entière[i-39,2:9] <- f_valeur_réduction_coti_vie(age = i,capital = 1000)
    réduction_10ans[i-39,2:9] <- f_valeur_réduction_coti_n_années(age = i, capital = 1000, nb_années = 10)
    réduction_20ans[i-39,2:9] <- f_valeur_réduction_coti_n_années(age = i, capital = 1000, nb_années = 20)
    réduction_30ans[i-39,2:9] <- f_valeur_réduction_coti_n_années(age = i, capital = 1000, nb_années = 30)
    réduction_unique[i-39,2:9] <- f_valeur_réduction_coti_unique(age = i, capital = 1000)
  }
}


# Charger le package openxlsx
library(openxlsx)

# Supposons que vos tableaux soient déjà créés
# Par exemple, supposons que réduction_10ans, réduction_20ans, etc., sont des dataframes

# Créer un dictionnaire avec le nom de chaque tableau en tant que clé et le tableau en tant que valeur
tableaux <- list(
  df_primes = df_primes,
  rachat_10ans = rachat_10ans,
  rachat_20ans = rachat_20ans,
  rachat_30ans = rachat_30ans,
  rachat_unique = rachat_unique,
  rachat_vie_entière = rachat_vie_entière,
  réduction_10ans = réduction_10ans,
  réduction_20ans = réduction_20ans,
  réduction_30ans = réduction_30ans,
  réduction_unique = réduction_unique,
  réduction_vie_entière = réduction_vie_entière
)

# Définir le nom du fichier Excel de sortie
fichier_excel <- "df_primes.xlsx"

# Créer un workbook
classeur <- createWorkbook()

# Parcourir le dictionnaire et écrire chaque tableau dans une feuille Excel
for (nom_feuille in names(tableaux)) {
  addWorksheet(classeur, sheetName = nom_feuille)
  writeData(classeur, sheet = nom_feuille, x = tableaux[[nom_feuille]])
}

# Sauvegarder le workbook
saveWorkbook(classeur, fichier_excel)

cat("Les tableaux ont été exportés avec succès dans", fichier_excel, "\n")

