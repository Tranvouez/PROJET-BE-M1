# Importation des tables : -----

#lecture des objets créés 
# cohort_1811 <- readRDS("RDS/cohort_1811.rds")
# cohort_1861 <- readRDS("RDS/cohort_1861.rds")
# cohort_1911 <- readRDS("RDS/cohort_1911.rds")
# 
# M_1811 <- readRDS("RDS/M_1811.rds")
# M_1861 <- readRDS("RDS/M_1861.rds")
# M_1911 <- readRDS("RDS/M_1911.rds")
# 
# P_1811 <-  readRDS("RDS/P_1811.rds")
# P_1861 <-  readRDS("RDS/P_1861.rds")
# P_1911 <-  readRDS("RDS/P_1911.rds")

# m_evol_age_bio_1811 <- readRDS("RDS/m_evol_age_bio_1811.rds")
# m_evol_age_bio_1861 <- readRDS("RDS/m_evol_age_bio_1861.rds")
# m_evol_age_bio_1911 <- readRDS("RDS/m_evol_age_bio_1911.rds")

# mat_somme_durée_1811 <- readRDS("RDS/mat_somme_durée_1811.rds")
# mat_somme_durée_1861 <- readRDS("RDS/mat_somme_durée_1861.rds")
# mat_somme_durée_1911 <- readRDS("RDS/mat_somme_durée_1911.rds")

# age_bio_reel_1811 <- readRDS("RDS/age_bio_reel_1811.rds")
# age_bio_reel_1861 <- readRDS("RDS/age_bio_reel_1861.rds")
# age_bio_reel_1911 <- readRDS("RDS/age_bio_reel_1911.rds")

# effectif_mat_1811 <- readRDS("RDS/effectif_mat_1811.rds")
# effectif_mat_1861 <- readRDS("RDS/effectif_mat_1861.rds")
# effectif_mat_1911 <- readRDS("RDS/effectif_mat_1911.rds")

# mat_rognée_1811 <- readRDS("RDS/mat_rognée_1811.rds")
# mat_rognée_1861 <- readRDS("RDS/mat_rognée_1861.rds")
# mat_rognée_1911 <- readRDS("RDS/mat_rognée_1911.rds")


# Liste générales sur les données : 
f_liste_distribution <- function(mat_rognée) {
  l_distrib <- list()
  
  for (i in 20:100){
    l_distrib[[i+1-20]] <- list(
      age = i, 
      repartition = table(mat_rognée[, i])[-1]
    )
  }
  return(l_distrib)
}

#l_distrib_1811 <- f_liste_distribution(mat_rognée_1811)
# barplot(l_distrib_1811[[1]]$repartition)
# title(main = paste("Distribution des âges biologique pour l'âge de ", l_distrib_1811[[1]]$age, "ans"))

# Tableau des valeurs autour de la médiane 10% : ----

f_tab_med_dix <- function(mat_rognée) {
  n <- ncol(mat_rognée)
  tab_med_inf_sup_10 <- matrix(0, nrow = n, ncol = 4)
  rownames(tab_med_inf_sup_10) <- paste("age", 0:(n-1), "ans")
  colnames(tab_med_inf_sup_10) <- c("40%", "50%", "60%", "mean")
  for (i in 1:n){ 
    tab_med_inf_sup_10[i,1:3] <- quantile(mat_rognée[mat_rognée[, i] != -1, i], c(0.4, 0.5, 0.6))
    tab_med_inf_sup_10[i,4] <- round(mean(mat_rognée[mat_rognée[, i] != -1, i]))
  }
  return(tab_med_inf_sup_10)
}
#t_med_inf_sup_10_1811 <- f_tab_med_dix(mat_rognée_1811)
#t_med_inf_sup_10_1811

# Tableau des valeurs autour de la médiane 5% : ----

f_tab_med_cinq <- function(mat_rognée) {
  n <- ncol(mat_rognée)
  tab_med_inf_sup_5 <- matrix(0, nrow = n, ncol = 4)
  rownames(tab_med_inf_sup_5) <- paste("age", 0:(n-1), "ans")
  colnames(tab_med_inf_sup_5) <- c("45%", "50%", "55%","mean")
  for (i in 1:n){ 
    tab_med_inf_sup_5[i,1:3] <- quantile(mat_rognée[mat_rognée[, i] != -1, i], c(0.45, 0.5, 0.55))
    tab_med_inf_sup_5[i,4] <- round(mean(mat_rognée[mat_rognée[, i] != -1, i]))
  }
  
  return(tab_med_inf_sup_5)
}

#t_med_inf_sup_5_1811 <- f_tab_med_cinq(mat_rognée_1811)

# Tableau des vingtiles : ----

t_vingtiles <- function(mat_rognée) {
  # On fait un tableau qui regroupe tous les vingtiles pour chaque âge :
  n <- ncol(mat_rognée)
  t_vingtiles <- matrix(0, nrow = n, ncol = 21)
  rownames(t_vingtiles) <- paste("age", 0:(n-1), "ans")
  colnames(t_vingtiles) <- paste0(seq(0, 100, by = 5), "%")
  
  for (i in 1:n){ 
    t_vingtiles[i,] <- quantile(mat_rognée[mat_rognée[, i] != -1, i], probs = seq(0, 1, by = 0.05))
  }
  return(t_vingtiles)
}

# t_vingtiles_1811 <- t_vingtiles(mat_rognée_1811)
# t_vingtiles_1811

# On va faire le choix de ne prendre que les éléments compris dans l'intervalle 90% 
# L'idée est de prendre l'intervalle moyenne d'un qx comme référence d'un intervalle de confiance à 5%. C'est à dire si les valeurs de l'intervalle à 5% - 10% sont équivalente à celle d'un qx j alors on dit que l'on prend la prime j. 

# Tableau référençant les primes à prendre par vingtiles : ------ 

# C'est un vingtile mais on ne prend pas les 0-5% et les 95-100% car ils sont trop à risque 
# On créer des contrats que pour les gens de 40 à 80 ans donc on fait ce tableau que pour ces âges là
# Dans la case on met l'âge correspondant à la prime à prendre

f_t_inter_age <- function(tab_med_inf_sup_5, t_vingtiles) {
  n = 41
  tab_inter_age <- matrix(0, nrow = n, ncol = 19)
  rownames(tab_inter_age) <- paste("age", 40:80, "ans")
  colnames(tab_inter_age) <- paste0(seq(5, 95, by = 5), "%")
  
  for (i in 1:n) {
    for (j in 1:19) {
      age_réel = i + 39
      if (j <= 10){ 
        # Possibilité sans risque car on maximise la valeur de l'intervalle à 5% - 10% (on prend la borne sup)
        tab_inter_age[i, j]  = max(which(tab_med_inf_sup_5[,3] <= t_vingtiles[(age_réel + 1),j+2]))
        # Possibilité avec un peu plus de risque : (on prend la borne inf)
        #tab_inter_age[i, j] <- min(which(tab_med_inf_sup_5[,2] > t_vingtiles[(age_réel + 1),j+1])) - 1 
      } 
      else { 
        tab_inter_age[i, j] = min(which(tab_med_inf_sup_5[,2] > t_vingtiles[(age_réel + 1),j+1]))
      }
    }
  }
  return(tab_inter_age)
}

#tab_inter_age_1811 <- f_t_inter_age(t_med_inf_sup_5_1811, t_vingtiles_1811)
#tab_inter_age_1811


# Fonction qui renvoie les âges et le vecteur de proba associé : ----

# On a 19 âge de primes possible : 

f_proba_age_prime <-
  function(t_vingtiles,
           age_reel,
           age_bio,
           tab_inter_age,
           poids = 0.75) {
    # Revoir si on peut pas jouer sur le facteur :
    repartition <- (1 - poids) / 18
    proba <- rep(repartition, 19)
    if (length(which(t_vingtiles[age_reel + 1, ] <= age_bio)) == 0) {
      proba[1] <-  poids
    }
    else {
        proba[max(which(t_vingtiles[age_reel + 1, ] <= age_bio))] <-  poids
    }
    return(list(proba, tab_inter_age[age_reel - 39, ]))
  }

#f_proba_age_prime(t_vingtiles_1811,40, 10, tab_inter_age_1811)

# Résumé de toutes les fonctions : --- 
# f_liste_distribution(mat_rognée = mat_rognée_1811)
# f_tab_med_dix(mat_rognée = mat_rognée_1811)
# f_tab_med_cinq(mat_rognée = mat_rognée_1811)
# t_vingtiles(mat_rognée = mat_rognée_1811)
# f_t_inter_age(t_vingtiles = t_vingtiles_1811, tab_med_inf_sup_5 = t_med_inf_sup_5_1811)
# f_proba_age_prime(t_vingtiles = t_vingtiles_1811, age_reel = 40, age_bio = 50, tab_inter_age = tab_inter_age_1811)


# Génération des objets : ---- 

# mat_rognée_1811 <- readRDS("RDS/mat_rognée_1811.rds")
# t_med_inf_sup_5_1811 <- f_tab_med_cinq(mat_rognée_1811)
# t_vingtiles_1811 <- t_vingtiles(mat_rognée_1811)
# tab_inter_age_1811 <- f_t_inter_age(t_med_inf_sup_5_1811, t_vingtiles_1811)
# saveRDS(t_vingtiles_1811, "RDS/t_vingtiles_1811.rds")
# saveRDS(tab_inter_age_1811, "RDS/tab_inter_age_1811.rds")
# rm(mat_rognée_1811, t_med_inf_sup_5_1811, t_vingtiles_1811, tab_inter_age_1811)
# 
# mat_rognée_1861 <- readRDS("RDS/mat_rognée_1861.rds")
# t_med_inf_sup_5_1861 <- f_tab_med_cinq(mat_rognée_1861)
# t_vingtiles_1861 <- t_vingtiles(mat_rognée_1861)
# tab_inter_age_1861 <- f_t_inter_age(t_med_inf_sup_5_1861, t_vingtiles_1861)
# saveRDS(t_vingtiles_1861, "RDS/t_vingtiles_1861.rds")
# saveRDS(tab_inter_age_1861, "RDS/tab_inter_age_1861.rds")
# rm(mat_rognée_1861, t_med_inf_sup_5_1861, t_vingtiles_1861, tab_inter_age_1861)
# 
# mat_rognée_1911 <- readRDS("RDS/mat_rognée_1911.rds")
# t_med_inf_sup_5_1911 <- f_tab_med_cinq(mat_rognée_1911)
# t_vingtiles_1911 <- t_vingtiles(mat_rognée_1911)
# tab_inter_age_1911 <- f_t_inter_age(t_med_inf_sup_5_1911, t_vingtiles_1911)
# saveRDS(t_vingtiles_1911, "RDS/t_vingtiles_1911.rds")
# saveRDS(tab_inter_age_1911, "RDS/tab_inter_age_1911.rds")
# rm(mat_rognée_1911, t_med_inf_sup_5_1911, t_vingtiles_1911, tab_inter_age_1911)
# 

## Fonction calcul prime avec âge biologique : ----

prime_unique_age_bio <-
  function(age_bio,
           age_reel,
           capital,
           table_morta,
           theta1 ,
           theta2,
           theta3, 
           table_année, 
           poids) {
    if (table_année == 1811) {
      t_vingtiles <- t_vingtiles_1811
      tab_inter_age <- tab_inter_age_1811
    }
    if (table_année == 1861) {
      t_vingtiles <- t_vingtiles_1861
      tab_inter_age <- tab_inter_age_1861
    }
    if (table_année == 1911) {
      t_vingtiles <- t_vingtiles_1911
      tab_inter_age <- tab_inter_age_1911
    }
    v <-
      f_proba_age_prime(
        t_vingtiles = t_vingtiles,
        age_reel = age_reel,
        age_bio = age_bio,
        tab_inter_age = tab_inter_age, 
        poids = poids
      )
    
    primes = rep(0, 19)
    for (i in 1:19) {
      primes[i] = prime_brute_unique(age = v[[2]][i], capital = capital, table_morta = table_morta)
    }
    primes = sum(primes * v[[1]])
    return(primes)
  }

prime_brute_coti_vie_age_bio <- function(age_bio,
           age_reel,
           capital,
           table_morta,
           theta1 ,
           theta2,
           theta3, 
           table_année, 
           poids) {
    if (table_année == 1811) {
      t_vingtiles <- t_vingtiles_1811
      tab_inter_age <- tab_inter_age_1811
    }
    if (table_année == 1861) {
      t_vingtiles <- t_vingtiles_1861
      tab_inter_age <- tab_inter_age_1861
    }
    if (table_année == 1911) {
      t_vingtiles <- t_vingtiles_1911
      tab_inter_age <- tab_inter_age_1911
    }
    v <-
      f_proba_age_prime(
        t_vingtiles = t_vingtiles,
        age_reel = age_reel,
        age_bio = age_bio,
        tab_inter_age = tab_inter_age, 
        poids = poids
      )
    
    primes = rep(0, 19)
    for (i in 1:19) {
      primes[i] = prime_brute_coti_vie(age = v[[2]][i], capital = capital, table_morta = table_morta)
    }
    primes = sum(primes * v[[1]])
    return(primes)
}

prime_brute_coti_n_années_age_bio <- function(age_bio,
                                              age_reel,
                                              capital,
                                              table_morta,
                                              nb_années,
                                              theta1 ,
                                              theta2,
                                              theta3,
                                              table_année,
                                              poids
) {
  if (table_année == 1811) {
    t_vingtiles <- t_vingtiles_1811
    tab_inter_age <- tab_inter_age_1811
  }
  if (table_année == 1861) {
    t_vingtiles <- t_vingtiles_1861
    tab_inter_age <- tab_inter_age_1861
  }
  if (table_année == 1911) {
    t_vingtiles <- t_vingtiles_1911
    tab_inter_age <- tab_inter_age_1911
  }
  v <-
    f_proba_age_prime(
      t_vingtiles = t_vingtiles,
      age_reel = age_reel,
      age_bio = age_bio,
      tab_inter_age = tab_inter_age,
      poids = poids
    )
  
  primes = rep(0, 19)
  for (i in 1:19) {
    primes[i] = prime_brute_coti_n_années(
      age = v[[2]][i],
      nb_années = nb_années,
      capital = capital,
      table_morta = table_morta
    )
  }
  primes = sum(primes * v[[1]])
  return(primes)
}






