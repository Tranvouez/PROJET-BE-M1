# Code et formules : 

## Chargement des librairies : 

library(readxl)
library(ggplot2)
library(tidyr)

## Préambule de code : 

#Importation de la table de mortalité TH0002
TH002 <- read.table("TH0002.csv",header = TRUE,sep = ";")

# Création de la colonne des probabilité de survie
TH002$qx <- c(0, 1 - (TH002[-1, 2] / TH002[1:nrow(TH002) - 1, 2]))
head(TH002)

## Fonctions initiales : 

### Fonction calcul des taux sans risque pour un nombre d'années donné. 

# La fonction permet d'établir un vecteur des coefficients d'actualisation des années 0 à n-1.
# Grâce aux valeurs de la variable période on peut choisir la période de prélèvement. 
# Période = 0 début année | Période = 0.5 milieu d'année | période = 1 fin d'année

# Entrée : 
## maturité en années : une année = 1
## taux : taux technique en vigueur, 1.75% -> taper 0.0175
## période : période de prélèvement x € [0,1]

# Sortie : 
## v_taux : sort le vecteur des taux actualisés sur la période donnée de l'année 0 à l'année - 1

f_vect_taux <- function(maturité,taux = 0.0175,période){
  v_taux <- 1/((1+taux) ** c(rep(0:(maturité-1))+période))
  return(v_taux)
}

# Exemple :
f_vect_taux(maturité = 10, taux = 0.0175, période = 0.5)

### Fonction calcul de probabilité de vivre à l'âge k + x sachant que l'on a l'âge x. 

# La fonction retourne un vecteur de probabilité de taille nb_années. 
# Elle calcule la probabilité de décès d'une personne à l'age x + i pour tout i € [1,nb_années]


# Entrée : 
## age : age de la personne aujourd'hui 
## nb_années : années en plus de l'age actuel 

# Sortie : 
## v_survie : vecteur de probabilité que l'on vive jusqu'à l'âge x + i sachant que l'on a l'âge x.
## pour i € [1,nb_années]

f_survie_age_x_plus_k <- function(age, nb_années,table_morta = TH002) {
  # Obtenir le numéro de ligne associés à l'age donnée en entrée dans la table de mortalité
  indice_age_TM = which(table_morta[, 1] == age)
  max_age = nb_années + indice_age_TM
  v_survie = c()
  v_survie = table_morta[(indice_age_TM + 1): max_age, 2] / table_morta[indice_age_TM, 2]
  return(v_survie)
}

# Exemple :
f_survie_age_x_plus_k(40, 10, table_morta = TH002)


### Fonction de récupération des probas de décès chaque année de age actuel à age + k

# On récupère directement la probabilité de décédé d'une année à l'autre de en partant de l'âge actuel 
# et jusqu'à l'âge + nb_années. 

# Entrée : 
## age : age de la personne aujourd'hui 
## nb_années : années en plus de l'age actuel 

# Sortie : 
## v_proba_décès : vecteur de la probabilité de décéder d'une année à l'autre. 

f_proba_deces <- function(age, nb_années, table_morta = TH002)
{
  indice_age_TM = which(table_morta[, 1] == age)
  v_proba_décès = table_morta[indice_age_TM:(indice_age_TM + nb_années - 1), 3]
  return(v_proba_décès)
}

# Exemple :
f_proba_deces(40, 10, table_morta = TH002)


## Fonctions associés aux formules d'annuités. 

### Annuités viagères à termes fractionnés d’avance immédiates et illimitées

# Entrée : 
## age : age de la personne aujourd'hui 
## table_morta : table de mortalité utilisée 

# Sortie : 
## a_2pts_x : Annuités viagères à termes fractionnés d’avance immédiates et illimitées


f_annuitée_viagère_fractionnés_avance_immediate_illimitee <- function(age, table_morta = TH002) {
  
  # On récupère l'année maximum du calcul : W - age 
  nb_années = max(table_morta[, 1]) - age
  
  # On récupère le vecteur de survie sur n_années sachant que l'on a un certain âge
  v_survie_age_x_plus_k <- f_survie_age_x_plus_k(age = age, nb_années = nb_années, table_morta = table_morta )
  
  #On récupère le vecteur des taux 
  v_taux <- f_vect_taux(maturité = nb_années, période = 0)
  
  #On fait la somme du produit vectoriel 
  a_2pts_x <- sum(v_survie_age_x_plus_k*v_taux)
  
  return(a_2pts_x)
}

# Exemple :
f_annuitée_viagère_fractionnés_avance_immediate_illimitee(40,table_morta = TH002)


### Annuités viagères à termes fractionnés d’avance immédiates et temporaires de n années


# Entrée : 
## age : age de la personne aujourd'hui 
## table_morta : table de mortalité utilisée 
## nb_années : année en plus de l'age actuel 

# Sortie : 
## a_2pts_xn : Annuités viagères à termes fractionnés d’avance immédiates et temporaires de n années

f_annuitée_viagère_fractionnés_avance_immediate_n_années <- function(age, nb_années, table_morta = TH002){ 
  
  # On récupère le vecteur de survie sur n_années sachant que l'on a un certain âge
  v_survie_age_x_plus_k <- f_survie_age_x_plus_k(age = age, nb_années = nb_années, table_morta = table_morta )
  
  #On récupère le vecteur des taux 
  v_taux <- f_vect_taux(maturité = nb_années, période = 0)
  
  #On fait la somme du produit vectoriel 
  a_2pts_x_n <- sum(v_survie_age_x_plus_k*v_taux)
  
  return(a_2pts_x_n)
}

f_annuitée_viagère_fractionnés_avance_immediate_n_années(age = 40, nb_années = 10, table_morta = TH002)

### Assurance décès payable immédiatement au décès à effet immédiat 


# Entrée : 
## age : age de la personne aujourd'hui 
## table_morta : table de mortalité utilisée 

# Sortie : 
# A_barre_x : Assurance décès payable immédiatement au décès à effet immédiat

f_assureur_décès_payable_immédiat <- function(age, table_morta = TH002) {
  
  # On récupère l'année maximum du calcul : W - age 
  nb_années = max(table_morta[, 1]) - age
  
  # On récupère le vecteur de proba de décès sur les n années 
  v_p_x <- f_proba_deces(age = age, nb_années = nb_années, table_morta = table_morta)
  
  # On récupère le vecteur de survie sur n_années sachant que l'on a un certain âge
  v_survie_age_x_plus_k <- f_survie_age_x_plus_k(age = age, nb_années = nb_années, table_morta = table_morta )
  
  #On récupère le vecteur des taux 
  v_taux <- f_vect_taux(maturité = nb_années, période = 0.5)
  
  # Calcul par somme et produit vectoriel : 
  A_barre_x <- sum(v_p_x*v_survie_age_x_plus_k*v_taux)
  return(A_barre_x)
  }

# Exemple :

f_assureur_décès_payable_immédiat(age = 80, table_morta = TH002)

## Fonctions de calcul des primes brutes : 

### Prime : cotisation à vie 

# Entrée :
# age : age de la personne aujourd'hui
## capital : capital que l'on souhaite recevoir
## thêta1 : frais d'acquisition (5%)
## thêta2 : frais dépendants du capital (0%)
## thêta3 : frais de gestion (10%)
## table_morta : table de mortalité choisie


#Sortie :
# prime annuelle dans le cadre d'une cotisation à vie :

prime_brute_coti_vie <-
  function(age, capital, theta1 = 0.05, theta2 = 0, theta3 = 0.1, table_morta = TH002){
    
    A_barre_x <- f_assureur_décès_payable_immédiat(age = age, table_morta = table_morta)
    a_2pts_x <- f_annuitée_viagère_fractionnés_avance_immediate_illimitee(age = age, table_morta = table_morta)
    
    prime = (capital * (A_barre_x + theta2*a_2pts_x))/(a_2pts_x*(1-theta1 - theta3))
    
    return(prime)
  }

## Exemple :
prime_brute_coti_vie( age = 60, capital = 1000, theta1 = 0.05, theta2 = 0, theta3 = 0.1, table_morta = TH002)

# # Test : 
# v_primes = c()
# for (i in 0:110){ 
#   v_primes[i+1] <- prime_brute_coti_vie(age = i, capital = 1000,theta1 = 0.05,theta2 = 0,theta3 = 0.1,table_morta = TH002)
# }
# # Analyse de la fonction prime unique : 
# x = 0:110
# y = v_primes
# plot(x,y, type = "l", col = "blue", xlab = "Age", ylab = "Prime", main = "Prime unique en fonction de l'âge")
# which(diff(y)<0)

### Prime : cotisation sur une durée de n années 

# Entrée : 
## age : age de la personne aujourd'hui 
## capital : capital que l'on souhaite recevoir 
## thêta1 : frais d'acquisition (5%) 
## thêta2 : frais dépendants du capital (0%) 
## thêta3 : frais de gestion (10%)
## table_morta : table de mortalité choisie


#Sortie : 
## prime annuelle dans le cadre d'une cotisation de n années : 

prime_brute_coti_n_années <-
  function(age,
           nb_années,
           capital,
           theta1 = 0.05,
           theta2 = 0, 
           theta3 = 0.1, 
           table_morta = TH002) {
    
    A_barre_x <- f_assureur_décès_payable_immédiat(age = age, table_morta = table_morta)
    a_2pts_x_n <- f_annuitée_viagère_fractionnés_avance_immediate_n_années(age = age, nb_années = nb_années, table_morta = table_morta)
    
    prime = (capital*(A_barre_x + theta2*a_2pts_x_n))/ ((1- theta1 - theta3)*a_2pts_x_n)
    return(prime)
  }

# Exemple : 

 prime_brute_coti_n_années( age = 40, nb_années = 10, capital = 1000, theta1 = 0.05, theta2 = 0, theta3 = 0.1, table_morta = TH002)
 
 # année = 10
 # v_primes = c()
 # for (i in 0:110){ 
 #   v_primes[i+1] <- prime_brute_coti_n_années(age = i,nb_années = année,capital = 1000,theta1 = 0.05,theta2 = 0,theta3 = 0.1,table_morta = TH002)
 # }
 # # Analyse de la fonction prime unique : 
 # x = 0:110
 # y = v_primes
 # plot(x,y, type = "l", col = "blue", xlab = "Age", ylab = "Prime", main = "Prime unique en fonction de l'âge")
 # which(diff(y)<0)


### Prime : prime unique 

# Entrée : 
## age : age de la personne aujourd'hui 
## capital : capital que l'on souhaite recevoir 
## thêta1 : frais d'acquisition (5%)
## thêta2 : frais dépendants du capital (0%)
## thêta3 : frais de gestion (10%)
## table_morta : table de mortalité choisie

#Sortie : 
## prime  dans le cadre d'une cotisation unique : 

prime_brute_unique <-
  function(age,
           capital,
           theta1 = 0.05,
           theta2 = 0, 
           theta3 = 0.1, 
           table_morta = TH002) {
    
    A_barre_x <- f_assureur_décès_payable_immédiat(age = age, table_morta = table_morta)
    prime = (capital * (A_barre_x + theta2))/ (1- theta1 - theta3)
    return(prime)
  }


# Exemple : 

 prime_brute_unique( age = 40, capital = 1000, theta1 = 0.05, theta2 = 0, theta3 = 0.1, table_morta = TH002)
 
#  v_primes = c()
#  for (i in 0:110){ 
#    v_primes[i+1] <- prime_brute_unique(age = i, capital = 1000,theta1 = 0.05,theta2 = 0,theta3 = 0.1,table_morta = TH002)
#  }
#  # Analyse de la fonction prime unique : 
#  x = 0:110
#  y = v_primes
#  plot(x,y, type = "l", col = "blue", xlab = "Age", ylab = "Prime", main = "Prime unique en fonction de l'âge")
 

 ## A revoir ## ----

 ## Valeurs de rachat : 
 
 ### Valeur de rachat : cotisation à vie 
 
 # Entrée : 
 ## age : age de la personne aujourd'hui 
 ## capital : capital que l'on souhaite recevoir 
 ## thêta1 : frais d'acquisition (5%) 
 ## thêta2 : frais dépendants du capital (0%) 
 ## thêta3 : frais de gestion (10%)
 ## alpha : pénalité que l'on accord à la valeur de rachat 
 ## k : nombre de valeurs que l'on veut
 ## table_morta : table de mortalité choisie
 
 
 # Sortie : 
 ## PMk : valeurs de rachat des k premières années
 
 f_valeur_rachat_coti_vie <-
   function(age,
            capital,
            theta1 = 0.05,
            theta2 = 0,
            theta3 = 0.1,
            alpha = 0.05, 
            k = 8, 
            table_morta = TH002) {
     PMk <- c()
     for (p in 0:k-1) {
       
       A_x <- f_assureur_décès_payable_immédiat(age = age + p)
       prime <-  prime_brute_coti_vie(age = age,capital = capital, theta1 = theta1,theta2 = theta2,theta3 = theta3, table_morta = table_morta)
       a_2pts_x <- f_annuitée_viagère_fractionnés_avance_immediate_illimitee(age = age + p, table_morta = table_morta)
       
       if (p <=10){ 
         PMk[p+1] <- ((A_x * capital + prime*(theta1+theta3)*a_2pts_x) - prime * a_2pts_x)* (1 - alpha)
       }
       else{ 
         PMk[p+1] <- ((A_x * capital + prime*(theta1+theta3)*a_2pts_x) - prime * a_2pts_x)
         }
     }
     return(PMk)
   }

 f_valeur_rachat_coti_vie(
   age = 40,
   capital = 1000 ,
   theta1 = 0.05,
   theta2= 0,
   theta3 = 0.1,
   alpha = 0.05, 
   k = 20,
   table_morta = TH002
 )
 
 
 ### Valeur de rachat : cotisation sur une durée de n années 
 
 # Entrée : 
 ## age : age de la personne aujourd'hui 
 ## capital : capital que l'on souhaite recevoir 
 ## thêta1 : frais d'acquisition (5%)
 ## thêta2 : frais dépendants du capital (0%)
 ## thêta3 : frais de gestion (10%)
 ## alpha : pénalité que l'on accord à la valeur de rachat 
 ## nb_années : correspond au nombres d'années de cotisation sur le contrat 
 ## k: nombre de valeurs que l'on veut
 ## table_morta : table de mortalité choisie
 
 
 # Sortie : 
 ## PMk : valeurs de rachat des k premières années
 
 f_valeur_rachat_coti_n_années <-
   function(age,
            capital,
            nb_années,
            theta1 = 0.05,
            theta2 = 0, 
            theta3 = 0.1,
            alpha = 0.05,
            k = 8, 
            table_morta = TH002) {
     PMk <- c()
     for (p in 0:k-1) {
       
       A_barre_x <- f_assureur_décès_payable_immédiat(age = age + p, table_morta = table_morta)
       prime <- prime_brute_coti_n_années(age = age,nb_années =  nb_années, capital = capital,
                                          theta1 = theta1,theta2 = theta2, theta3 = theta3, table_morta = table_morta)
       a_2pts_x_n <- f_annuitée_viagère_fractionnés_avance_immediate_n_années(age = age + p, nb_années = nb_années-p, table_morta = table_morta)
       if (p < 10){
         PMk[p+1] <- ((A_barre_x* capital + (theta1+theta3)* prime* a_2pts_x_n) - prime* a_2pts_x_n)* (1 - alpha)
       }
       else if (p >= 10 & p < nb_années){ 
         PMk[p+1] <- ((A_barre_x* capital + (theta1+theta3)* prime* a_2pts_x_n) - prime* a_2pts_x_n)
       }
       else { 
         PMk[p+1] <- A_barre_x * capital 
      }
     }
     return(PMk)
   }
 
  f_valeur_rachat_coti_n_années(
    age = 40,
    capital = 1000,
    nb_années = 10,
    theta1 = 0.05,
    theta2 = 0,
    theta3 = 0.1,
    alpha = 0.05, 
    k = 20, 
    table_morta = TH002
  )
 
  
 ### Valeur de rachat : prime unique 
 
 # Entrée : 
 ## age : age de la personne aujourd'hui 
 ## capital : capital que l'on souhaite recevoir 
 ## thêta1 : frais d'acquisition (5%)
 ## thêta2 : frais dépendants du capital (0%)
 ## thêta3 : frais de gestion (10%)
 ## alpha : pénalité que l'on accord à la valeur de rachat 
 ## k: nombre de valeurs que l'on veut
 
 # Sortie : 
 ## PMk : valeurs de rachat des k premières années
 
 f_valeur_rachat_coti_unique<-
   function(age,
            capital,
            theta1 = 0.05,
            theta2 = 0,
            theta3 = 0.1,
            alpha = 0.05, 
            k = 8, 
            table_morta = TH002) {
     PMk <- c()
     for (p in 0:k-1) {
       
       A_barre_x <- f_assureur_décès_payable_immédiat(age = age + p)
       prime <-  prime_brute_unique(age = age, capital = capital,theta1 = 0.05, theta2 = 0, theta3 = 0.1, table_morta = table_morta)
         
       if (p == 0){ 
         PMk[p+1] <- (A_barre_x * capital + (theta1 + theta3)*prime - prime) * (1 - alpha)
       }
       else if (p > 0 & p <10){ 
         PMk[p+1] <- (A_barre_x * capital) * (1 - alpha)
       }
       else if (p > 0 & p >= 10){ 
         PMk[p+1] <- (A_barre_x * capital)
         }
     }
     return(PMk)
   }
 
 f_valeur_rachat_coti_unique(
   age = 40,
   capital = 1000,
   theta1 = 0.05,
   theta2 = 0,
   theta3 = 0.1,
   alpha = 0.05, 
   k = 20, 
   table_morta = TH002
 )
 
 
 ## Calcul valeur de réduction : 
 
 ### Valeur de réduction : cotisation à vie 
 
 # Entrée :
 # age : age de la personne aujourd'hui
 # capital : capital que l'on souhaite recevoir
 # thêta1 : frais d'acquisition (5%)
 # thêta2 : frais dépendants du capital (0%)
 # thêta3 : frais de gestion (10%)
 # k: nombre de valeurs que l'on veut
 
 # Sortie :
 # v_valeur_red <- valeur de réduction sur les k années à venir
 
 f_valeur_réduction_coti_vie <-
   function(age, capital, 
            theta1 = 0.05,
            theta2 = 0,
            theta3 = 0.1,
            k = 8, 
            table_morta = TH002) {
     v_valeur_red <- c()
     for (p in 0:k - 1) {
       
       A_barre_x <- f_assureur_décès_payable_immédiat(age = age+p, table_morta = table_morta)
       prime <-  prime_brute_coti_vie(age = age,capital = capital,theta1 = theta1, theta2 = theta2, theta3 = theta3, table_morta = table_morta)
       a_2pts_x <- f_annuitée_viagère_fractionnés_avance_immediate_illimitee(age = age + p, table_morta = table_morta)
       
       v_valeur_red[p + 1] <- ((A_barre_x * capital + (theta1 + theta3)* prime * a_2pts_x) - prime * a_2pts_x)/ A_barre_x
     }
     return(v_valeur_red)
   }
 
  f_valeur_réduction_coti_vie(
    age = 40,
    capital = 1000,
    theta1 = 0.05,
    theta2 = 0,
    theta3 = 0.1,
    k = 8
  )
 
 ### Valeur de réduction : cotisation sur une durée de n années 
 
 # Entrée :
 # age : age de la personne aujourd'hui
 # capital : capital que l'on souhaite recevoir
 # thêta1 : frais d'acquisition (5%)
 # thêta2 : frais dépendants du capital (0%)
 # thêta3 : frais de gestion (10%)
 # nb_années : correspond au nombres d'années de cotisation sur le contrat
 # k: nombre de valeurs que l'on veut
 
 # Sortie :
 # v_valeur_red <- valeur de réduction sur les k années à venir
 
 f_valeur_réduction_coti_n_années <-
   function(age,
            capital,
            theta1 = 0.05,
            theta2 = 0,
            theta3 = 0.1,
            k = 8,
            nb_années, 
            table_morta = TH002) {
     v_valeur_red <- c()
     for (p in 0:k - 1) {
       
       A_barre_x <- f_assureur_décès_payable_immédiat(age = age + p)
       prime <- prime_brute_coti_n_années( age = age,nb_années =  nb_années, capital = capital, theta1 = theta1,theta2 = theta2,theta3 = theta3,table_morta = table_morta) 
       a_2pts_x_n <- f_annuitée_viagère_fractionnés_avance_immediate_n_années(age = age + p, nb_années = nb_années - p)
       
       if (p<nb_années){ 
         v_valeur_red[p + 1] <- (A_barre_x * capital + (theta1 + theta3)* prime* a_2pts_x_n - prime* a_2pts_x_n)/ A_barre_x
         }
       else { 
         v_valeur_red[p + 1] <- (A_barre_x * capital / A_barre_x)
         }
     }
     return(v_valeur_red)
   }
 
 f_valeur_réduction_coti_n_années(
   age = 40,
   capital = 1000,
   theta1 = 0.05,
   theta2 = 0, 
   theta3 = 0.1,
   k  = 10,
   nb_années = 1,
   table_morta = TH002
 )
 
 
 ### Valeur de réduction : cotisation sur une prime unique 
 
 # Entrée :
 # age : age de la personne aujourd'hui
 # capital : capital que l'on souhaite recevoir
 # thêta1 : frais d'acquisition (5%)
 # thêta2 : frais dépendants du capital (0%)
 # thêta3 : frais de gestion (10%)
 # nb_années : correspond au nombres d'années de cotisation sur le contrat
 # k: nombre de valeurs que l'on veut
 
 # Sortie :
 # v_valeur_red <- valeur de réduction sur les k années à venir
 
 f_valeur_réduction_coti_unique <-
   function(age,
            capital,
            theta1 = 0.05,
            theta2 = 0,
            theta3 = 0.1,
            k = 8, 
            table_morta = TH002){
     v_valeur_red <- c()
     for (p in 0:k - 1) {
       
       A_barre_x <- f_assureur_décès_payable_immédiat(age = age + p)
       prime <- prime_brute_unique(age = age, capital = capital, theta1 = theta1, theta2 = theta2, theta3 = theta3, table_morta = table_morta)
       
       if (p == 0){ 
         v_valeur_red[p+1] <- (A_barre_x*capital + (theta1 + theta3)*prime - prime)/ A_barre_x
         
       }
       else { 
         v_valeur_red[p + 1] <- capital 
         }
     } 
     return(v_valeur_red)
   }
 
 f_valeur_réduction_coti_unique(
   age = 40,
   capital = 1000,
   theta1 = 0.05,
   theta2 = 0,
   theta3 = 0.1,
   k = 8, 
   table_morta = TH002
 )
 
 
 # Partie Shiny app : ---- 
 
 ## Importation des données : ----
 dfbe <- read_excel("Export/df_primes.xlsx", sheet = 1)
 
 # Pour une bonne mise à l'échelle 
 dfbe_bis <- dfbe
 dfbe_bis[,3] <- dfbe[,3]*10
 dfbe_bis[,4] <- dfbe[,4]*20
 dfbe_bis[,5] <- dfbe[,5]*30
 
 # Convertir le tableau en format long
 data_long <- pivot_longer(dfbe[-c(which(dfbe[,1]==81):nrow(dfbe)),-6], cols = -Age, names_to = "Variables", values_to = "Valeur")
 # Avec la prime unique :
 data_long_bis <- pivot_longer(dfbe_bis[-c(which(dfbe_bis[,1]==81):nrow(dfbe_bis)),-2], cols = -Age, names_to = "Variables", values_to = "Valeur")
 
 # Filtre sur les valeurs : 
 data_long_filtered <- data_long[data_long$Valeur != 0, ]
 data_long_filtered_bis <- data_long_bis[data_long_bis$Valeur != 0, ]
 
 # Tracer le graphique ggplot
 
 ## Fonctions pour les intervalles de légendes: 
 # Fonction renvoie le multiple supérieur d'une valeur. 
 # Si multiple vaut 20 et x 21 ça renvoie 40. (19,20-->20)
 multiple_superieur <- function(x, multiple = 20) {
   resultat <- ceiling(x / multiple) * multiple
   return(resultat)
 }
 
 # Fonction renvoie le multiple inférieur d'une valeur. 
 # Si multiple vaut 20 et x 21 ça renvoie 20. (19,20-->0)
 multiple_inférieur <- function(x, multiple = 20) {
   resultat <- floor(x / multiple) * multiple
   return(resultat)
 }
 
 plot_comparaisons <- ggplot(data_long_filtered, aes(x = Age, y = Valeur, color = Variables)) +
   geom_line(size = 0.8) +
   #geom_point() +
   labs(title = "Prime mensuelle pour un versement de 1000€ en fonction de l'âge",
        x = "Âge à la souscription",
        y = "Montant de la cotisation") +
   theme_minimal()+
   scale_x_continuous(breaks = seq(min(data_long_filtered$Age), max(data_long_filtered$Age), by = 5))+
   scale_y_continuous(breaks = seq(multiple_inférieur(min(data_long_filtered$Valeur),multiple = 20), multiple_superieur(max(data_long_filtered$Valeur), multiple = 20),by = 20))+
   theme(
     text = element_text(size = 20),  # Ajustez la taille du texte
     axis.text.x = element_text(size = 20),  # Ajustez la taille du texte de l'axe des x
     axis.text.y = element_text(size = 20),  # Ajustez la taille du texte de l'axe des y
     legend.text = element_text(size = 20),  # Ajustez la taille du texte de la légende
     plot.title = element_text(size = 20, face = "bold")  # Ajustez la taille du texte du titre du graphique
   )
 
 plot_comparaisons_bis <- ggplot(data_long_filtered_bis, aes(x = Age, y = Valeur, color = Variables)) +
   geom_line(size = 0.8) +
   #geom_point() +
   labs(title = "Prime totale pour un versement de 1000€ en fonction de l'âge",
        x = "Âge à la souscription",
        y = "Montant de la cotisation") +
   theme_minimal()+
   scale_x_continuous(breaks = seq(min(data_long_filtered_bis$Age), max(data_long_filtered_bis$Age), by = 5))+
   scale_y_continuous(breaks = seq(multiple_inférieur(min(data_long_filtered_bis$Valeur),multiple = 400), multiple_superieur(max(data_long_filtered_bis$Valeur), multiple = 400),by = 400))+
   theme(
     text = element_text(size = 20),  # Ajustez la taille du texte
     axis.text.x = element_text(size = 20),  # Ajustez la taille du texte de l'axe des x
     axis.text.y = element_text(size = 20),  # Ajustez la taille du texte de l'axe des y
     legend.text = element_text(size = 20),  # Ajustez la taille du texte de la légende
     plot.title = element_text(size = 20, face = "bold")  # Ajustez la taille du texte du titre du graphique
   )+
   geom_hline(yintercept = 1000, linetype = "dashed", color = "black")  # Ajouter la ligne horizontale en pointillés à 1000
 
 ## Tableaux des différentes valeurs : ----
 
 tab_coti_vie <- function(va_age, va_capital) {
   df <- rbind(round(f_valeur_rachat_coti_vie(age = va_age, capital = va_capital),2),
               round(f_valeur_réduction_coti_vie(age= va_age, capital = va_capital),2))
   df <- apply(df, c(1, 2), function(x) paste0(x, "€"))
   rownames(df) <- c("Valeurs de rachat","Valeurs de réduction")
   colnames(df) <- paste("Année", 1:8)
   return(df)
 }
 tab_coti_unique <- function(va_age, va_capital) {
   df <- rbind(round(f_valeur_rachat_coti_unique(age = va_age, capital = va_capital),2),
               round(f_valeur_réduction_coti_unique(age= va_age, capital = va_capital),2))
   df <- apply(df, c(1, 2), function(x) paste0(x, "€"))
   rownames(df) <- c("Valeurs de rachat","Valeurs de réduction")
   colnames(df) <- paste("Année", 1:8)
   return(df)
 }
 
 tab_annees_fixe <- function(va_age, va_capital, duree) {
   df <- rbind(round(f_valeur_rachat_coti_n_années(age = va_age, capital = va_capital, nb_années = duree),2),
               round(f_valeur_réduction_coti_n_années(age= va_age, capital = va_capital,nb_années = duree),2))
   df <- apply(df, c(1, 2), function(x) paste0(x, "€"))
   rownames(df) <- c("Valeurs de rachat","Valeurs de réduction")
   colnames(df) <- paste("Année", 1:8)
   return(df)
 }
 
 f_comparaison <- function(va_age, va_capital){
   df <- rbind(
     rep("",8),
     tab_coti_vie(va_age = va_age, va_capital = va_capital), 
     rep("",8),
     tab_coti_unique(va_age = va_age, va_capital = va_capital),
     rep("",8),
     tab_annees_fixe(va_age = va_age, va_capital = va_capital, duree = 10), 
     rep("",8),
     tab_annees_fixe(va_age = va_age, va_capital = va_capital, duree = 20),
     rep("",8),
     tab_annees_fixe(va_age = va_age, va_capital = va_capital, duree = 30)
   )
   df <- cbind(rep("",15),df)
   rownames(df) <- c("Cotisation à Vie","Valeurs de rachat","Valeurs de réduction",
                     "Cotisation Unique","Valeurs de rachat","Valeurs de réduction",
                     "Cotisation 10 ans","Valeurs de rachat","Valeurs de réduction",
                     "Cotisation 20 ans","Valeurs de rachat","Valeurs de réduction",
                     "Cotisation 30 ans","Valeurs de rachat","Valeurs de réduction")
   
   colnames(df) <- c("Prime",paste("Année", 1:8))
   df[1,1] <- paste0(round(prime_brute_coti_vie(age = va_age, capital = va_capital),2),"€")
   df[4,1] <- paste0(round(prime_brute_unique(age = va_age, capital = va_capital),2),"€")
   df[7,1] <- paste0(round(prime_brute_coti_n_années(age = va_age, capital = va_capital, nb_années = 10),2),"€")
   df[10,1] <- paste0(round(prime_brute_coti_n_années(age = va_age, capital = va_capital, nb_années = 20),2),"€")
   df[13,1] <- paste0(round(prime_brute_coti_n_années(age = va_age, capital = va_capital, nb_années = 30),2),"€")           
   return(df)
 }
 
 ## Tableau valeur de rachat et de réduction sur longue période : 
 
 tab_coti_vie_r <- function(va_age, va_capital,k) {
   df <- as.data.frame(rbind(round(f_valeur_rachat_coti_vie(age = va_age, capital = va_capital, k = 8),2),
                             round(f_valeur_réduction_coti_vie(age= va_age, capital = va_capital, k = 8),2),
                             c(1:8)))
   rownames(df) <- c("Valeurs de rachat","Valeurs de réduction","Années")
   colnames(df) <- paste("Année", 1:ncol(df))
   data_long <- tidyr::pivot_longer(as.data.frame(t(df)), cols = -Années, names_to = "Type", values_to = "Valeur")
   data_long <- data_long[data_long$Valeur != 0, ]
   return(data_long)
 }
 #tab_coti_vie_r(40,100)
 
 tab_coti_unique_r <- function(va_age, va_capital) {
   df <- as.data.frame(rbind(round(f_valeur_rachat_coti_unique(age = va_age, capital = va_capital, k = 8),2),
                             round(f_valeur_réduction_coti_unique(age= va_age, capital = va_capital, k = 8),2), 
                             c(1:8)))
   rownames(df) <- c("Valeurs de rachat","Valeurs de réduction", "Années")
   colnames(df) <- paste("Année", 1:ncol(df))
   data_long <- tidyr::pivot_longer(as.data.frame(t(df)), cols = -Années, names_to = "Type", values_to = "Valeur")
   data_long <- data_long[data_long$Valeur != 0, ]
   return(data_long)
 }
 #tab_coti_unique_r(80,100)
 
 tab_annees_fixe_r <- function(va_age, va_capital, duree) {
   df <- as.data.frame(rbind(round(f_valeur_rachat_coti_n_années(age = va_age, capital = va_capital, nb_années = duree, k = 8),2),
                             round(f_valeur_réduction_coti_n_années(age= va_age, capital = va_capital,nb_années = duree, k = 8),2), 
                             c(1:8)))
   rownames(df) <- c("Valeurs de rachat","Valeurs de réduction", "Années")
   colnames(df) <- paste("Année", 1:ncol(df))
   data_long <- tidyr::pivot_longer(as.data.frame(t(df)), cols = -Années, names_to = "Type", values_to = "Valeur")
   data_long <- data_long[data_long$Valeur != 0, ]
   return(data_long)
 }
 #tab_annees_fixe_r(40,1000,30)
 
 # Graphiques des évolutions de rachat et réduction 
 plot_réduction_rachat <- function(data_long_filtered) {
   ggplot(data_long_filtered, aes(x = Années, y = Valeur, color = Type)) +
     geom_line(size = 0.8) +
     geom_point() +
     labs(title = "Évolution des valeurs de rachat et de réduction",
          x = "Années",
          y = "Valeur en €") +
     theme_minimal()+
     scale_x_continuous(breaks = seq(1, 8, by = 1))+
     scale_y_continuous(breaks = seq(multiple_inférieur(min(data_long_filtered$Valeur),multiple = 250), multiple_superieur(max(data_long_filtered$Valeur), multiple = 250),by = 250))+
     theme(
       text = element_text(size = 20),  # Ajustez la taille du texte
       axis.text.x = element_text(size = 20),  # Ajustez la taille du texte de l'axe des x
       axis.text.y = element_text(size = 20),  # Ajustez la taille du texte de l'axe des y
       legend.text = element_text(size = 20),  # Ajustez la taille du texte de la légende
       plot.title = element_text(size = 20, face = "bold")  # Ajustez la taille du texte du titre du graphique
     )
 }
 
 
 




