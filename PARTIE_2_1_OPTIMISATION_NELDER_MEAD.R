
#################################
# Importation des données : ---- 
#################################

## Importation cohorte :  ----

cohort_one_one <- read.table("both_years_one_x_one.txt", header = TRUE, skip = 1)
cohort_one_ten <- read.table("both_years_one_x_ten.txt", header = TRUE, skip = 1)

# Passer toutes les variables en numérique :
cohort_one_one <- as.data.frame(lapply(cohort_one_one, as.numeric))
cohort_one_ten[,-1] <- as.data.frame(lapply(cohort_one_ten[,-1], as.numeric))

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

cohort_1811 <- extraction_cohort(1811, TRUE, cohort = cohort_one_one)[[2]]
cohort_1861 <- extraction_cohort(1861, TRUE, cohort = cohort_one_one)[[2]]
cohort_1911 <- extraction_cohort(1911, TRUE, cohort = cohort_one_one)[[2]]

# La probabilité de survie : 
S_x = 1 - cohort_1811$qx

cohort_1811_one_ten <- extraction_cohort("1810-1819", TRUE, cohort = cohort_one_ten)[[2]]
cohort_1861_one_ten <- extraction_cohort("1860-1869", TRUE, cohort = cohort_one_ten)[[2]]
cohort_1911_one_ten <- extraction_cohort("1900-1909", TRUE, cohort = cohort_one_ten)[[2]]

# La probabilité de survie :
S_x_one_ten = 1 - cohort_1811_one_ten$qx

# Fonctions utiles : --- 

library(Matrix)

## Fonction de survie :  ----

S <- function(age, matrice, n = 200) {
  alpha = rep(0, n)
  alpha[1] = 1
  e = rep(1, n)
  return (t(alpha) %*% expm(matrice * age) %*% e)
}

Sx <- function(age, matrice, n = 200) {
  alpha = rep(0, n)
  alpha[age] = 1
  e = rep(1, n)
  return (t(alpha) %*% expm(matrice * age) %*% e)
}


## Proba de décès : ----- 
# les qx_chap  

proba_deces <- function(n, M) {
  a=rep(0,n)
  for(i in 1:n){
    S_i <- S(i,M)
    S_i_1 <- S(i+1,M)
    a[i]=(S_i - S_i_1)/S_i
  }
  return(a)
}

## Fonction matrice de probabilité : ----

fonction_matrice_proba <- function(x){
  lambda <- x[1]
  lambda1 <- x[2]
  lambda2 <- x[3]
  lambda3 <- x[4]
  lambda4 <- x[5]
  q1 <- x[6]
  q2 <- x[7]
  q3 <- x[8]
  q4 <- x[9]
  q <- x[10]
  a <- x[11]
  b <- x[12]
  p <- x[13]
  i_1 <- 42 # on le modifiera après 
  i_2 <- 99 # on le modifiera après
  
  # Création de la matrice A : 
  
  M_A = matrix(0, ncol = 200, nrow = 200) # matrice par bloc
  v_lambda = c(lambda1, lambda2, lambda3, lambda4)
  v_q = c(q1, q2, q3, q4)
  
  for (i in 1:200){ 
    
    if (i >= 1 & i <=4){
      M_A[i, i + 1] = v_lambda[i]
      M_A[i, i] = -(v_lambda[i] + v_q[i])
    }
    else if (i>= 5 & i<= i_1) {
      M_A[i, i + 1] = lambda
      M_A[i, i] = -(lambda + q*(i^p) + b)
    }
    else if (i > i_1 & i <= i_2) {
      M_A[i, i + 1] = lambda
      M_A[i, i] = -(lambda + q*(i^p) + a + b)
    }
    else if (i > i_2 & i < 200) {
      M_A[i, i + 1] = lambda
      M_A[i, i] = -(lambda + q*(i^p) + b)
    }
    else{
      M_A[i, i] = - 1
    }
  }
  return(M_A)
}

## Initialisation des paramètres de base : ----

param <- c(
  lambda = 2.5657,
  lambda1 = 2.4906,
  lambda2 = 0.6376,
  lambda3 = 0.7040,
  lambda4 = 0.4416,
  q1 = 0.6051,
  q2 = 0.0726,
  q3 = 0.0503,
  q4 = 0.0285,
  q = 9.3157 * (10 ^ -9),
  a = 1.9888 * (10 ^ -3),
  b = 3.1504 * (10 ^ -3),
  p = 3
)

## Fonction à optimiser : ----

library(Matrix)

fonction_optim <- function(x = NULL, index = NULL, fmsfundata = NULL) { 
  
  lambda <- x[1]
  lambda1 <- x[2]
  lambda2 <- x[3]
  lambda3 <- x[4]
  lambda4 <- x[5]
  q1 <- x[6]
  q2 <- x[7]
  q3 <- x[8]
  q4 <- x[9]
  q <- x[10]
  a <- x[11]
  b <- x[12]
  p <- x[13] # On cherchera par boucle for le meilleur 
  #p <- 3
  i_1 <- 70 # on le modifiera après 
  i_2 <- 140 # on le modifiera après
  
  # Création de la matrice A : 
  
  n = 200
  
  M_A = matrix(0, ncol = n, nrow = n) # matrice par bloc
  v_lambda = c(lambda1, lambda2, lambda3, lambda4)
  v_q = c(q1, q2, q3, q4)
  
  for (i in 1:n){ 
    
    if (i >= 1 & i <= 4){
      M_A[i, i + 1] = v_lambda[i]
      M_A[i, i] = -(v_lambda[i] + v_q[i])
    }
    else if (i>= 5 & i<= i_1) {
      M_A[i, i + 1] = lambda
      M_A[i, i] = -(lambda + q*(i^p) + b)
    }
    else if (i > i_1 & i <= i_2) {
      M_A[i, i + 1] = lambda
      M_A[i, i] = -(lambda + q*(i^p) + a + b)
    }
    else if (i > i_2 & i < n) {
      M_A[i, i + 1] = lambda
      M_A[i, i] = -(lambda + q*(i^p) + b)
    }
    else{
      if (i == n){
       # M_A[i, i] = - (q*(i^p) + b) ancienne version 
        M_A[i, i] = - 1
      }
    }
  }
  
  # Partie 2 : la fonction à optimiser : 
  
  # if(oui){
  w = which(cohort_1811$qx == 1)   # à modifier si besoin
  alpha = t(c(1, rep(0, 199)))
  e = rep(1, n)
  age = 0:w
  A = rep(1, w+1)
  S_x = 1 - cohort_1811$qx[1:w]
  
  # Nouvelle méthode :
  Sx_estime = list()
  
  for (i in 1:w+1){ 
    Sx_estime[[i]] = alpha %*% expm(M_A * age[i]) %*% e
  }
  Sx_estime[[1]] = 1 

  for (i in 1:w){
    A[i] = 1 - Sx_estime[[i+1]] / Sx_estime[[i]]
  }
  
  inf = 1
  
  Fonction = sum(S_x[inf:w] * ((cohort_1811$qx[inf:w] - A[inf:w])^2))
  
  # Les contraintes : 
  
  c <- c(1-b-q*(i_2^p)-a, 1-b-q*(n^p)) 
  # On a des contraintes sur les qi mais comme se sont des 'fonctions' 
  # croissantes il suffit de conditionner les max . 
  
   varargout <- list(f = Fonction,
                     g = c(),
                     c = c, 
                     gc = c(),
                     #A = A,
                     index = index,
                     this = list(costfargument = fmsfundata))
   
  return(varargout)
}

# Test de la fonction : ----

fonction_optim(param)

# Optimisation : ---- 

### Détermination des bornes : ---- 
borne_inf <- c(
  lambda = 2, lambda1 = 0, lambda2 = 0, lambda3 = 0, lambda4 = 0,
  q1 = 0, q2 = 0, q3 = 0, q4 = 0, q = 0, a = 0, b = 0 , p = 2)

borne_sup <- c(
  lambda = 5, lambda1 = 8,  lambda2 = 8,  lambda3 = 8,  lambda4 = 8,
  q1 = 1, q2 = 1, q3 = 1, q4 = 1, q = 1, 
  a = 1,  b = 1, p = 7)

### Vecteur initial sur les données : ----

X0 <- transpose(c(
  lambda = 5, lambda1 = 1.5,  lambda2 = 0.5, lambda3 = 0.5, lambda4 = 0.5,
  q1 = 0.5, q2 = 0.05, q3 = 0.05, q4 = 0.02, q = 0, 
  a = 0.05, b = 0.05 ,p = 3
))  # vecteur initial 

### Optimisation : ----

library(neldermead)
set.seed(123) # on fixe la racine

nm <- neldermead() # on initialise l'objet
nm <- neldermead.set(nm, 'numberofvariables', 13) # nombre de variables à optimiser
nm <- neldermead.set(nm, 'function', fonction_optim) # fonction à optimiser
nm <- neldermead.set(nm,"nbineqconst",2) # nombre de contraintes inégalités
nm <- neldermead.set(nm, 'x0', X0) # point de départ
nm <- neldermead.set(nm,'maxiter', 10) # nombre d'itérations
nm <- neldermead.set(nm,'maxfunevals',10) # nombre d'évaluations de la fonction
nm <- neldermead.set(nm,'boundsmin',borne_inf) # bornes inférieures
nm <- neldermead.set(nm,'boundsmax',borne_sup) # bornes supérieures
nm <- neldermead.set(nm,'simplex0method','pfeffer') # méthode de génération du simplex initial 
# c'est à dire les points de départ sont générés aléatoirement. 
nm <- neldermead.set(nm,'boxnbpoints', 6) # nombre de points pour la méthode de recherche
nm <- neldermead.set(nm, 'verbose', TRUE) # pas de message d'erreur
nm <- neldermead.set(nm, 'storehistory', TRUE) # on garde l'historique
# nm <- neldermead.set(nm, 'verbosetermination', FALSE) # pas de message d'erreur
nm <- neldermead.set(nm, 'method', 'box') # méthode de recherche
nm <- neldermead.search(nm) # on lance la recherche
summary(nm) # on affiche le résultat
saveRDS(nm, "RDS/test_<numéro>.rds")


# Affichage des meilleurs résultats : ---- 
P_1811 <- proba_deces(200,fonction_matrice_proba(param))

test_8 <- readRDS("RDS/test_8.rds")
# Test de résultats sur la fonction : 
fonction_optim(test_8$optbase$xopt)
p_test_8 <- proba_deces(200, fonction_matrice_proba(test_8$optbase$xopt))

# Données de la première courbe : 
tv_1811 <- seq(1, 104, by = 1)
# Tracer la première courbe
plot(
  tv_1811,
  log(cohort_1811$qx[1:104] * 10000),
  xlab = "age",
  ylab = "ln(10000*qx)",
  type = "l",
  xlim = c(5, 120),
  ylim = c(2, 9),
  col = "blue",
  main = "Evolution des probabilités de décès"
)
lines(tv_1811, log(P_1811[1:104]*10000), col = "red")
lines(tv_1811, log(p_test_8[1:104]*10000), col = "green")
legend("bottomright", legend = c("Données", "Paramètres", "Meilleure Optimisation"), col = c("blue", "red", "green"), lty = 1)









