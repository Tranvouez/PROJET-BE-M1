source("PARTIE_1_1_FONCTIONS_PRIMES.R")
source("PARTIE_2_3_ETUDE_POP_MODELISATION.R")

# Importation des données cohortes et mises en forme comme la TH0002: ----

## Importation des données ---- 

cohort_one_one <- read.table("both_years_one_x_one.txt", header = TRUE, skip = 1)
# Passer toutes les variables en numérique :
cohort_one_one <- as.data.frame(lapply(cohort_one_one, as.numeric))

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

## Uniformisations à la TH0002 ----

cohort_1811 <- extraction_cohort(1811, F, cohort = cohort_one_one)[[2]]
TM_1811 <- cohort_1811[c(1:104),c(2,6,4)]
cohort_1861 <- extraction_cohort(1861, F, cohort = cohort_one_one)[[2]]
TM_1861 <- cohort_1861[c(1:108),c(2,6,4)]
cohort_1911 <- extraction_cohort(1911, F, cohort = cohort_one_one)[[2]]
TM_1911 <- cohort_1911[c(1:111),c(2,6,4)]
TM_1911[111,1] <- 110

## Importation objets modelisation age biologique : 

t_vingtiles_1811 <- readRDS("RDS/t_vingtiles_1811.rds")
t_vingtiles_1861 <- readRDS("RDS/t_vingtiles_1861.rds")
t_vingtiles_1911 <- readRDS("RDS/t_vingtiles_1911.rds")

tab_inter_age_1811 <- readRDS("RDS/tab_inter_age_1811.rds")
tab_inter_age_1861 <- readRDS("RDS/tab_inter_age_1861.rds")
tab_inter_age_1911 <- readRDS("RDS/tab_inter_age_1911.rds")

### Fonction calcul prime avec âge physio : ----
prime_brute_coti_n_années_age_bio(age_bio = 10, age_reel = 40, capital = 1000, nb_années = 10, TM_1811, 0.1, 0.1, 0.1, table_année = 1811, poids = 0.75)
prime_brute_coti_vie_age_bio(age_bio = 10, age_reel = 40, 1000, TM_1811, 0.1, 0.1, 0.1, table_année = 1811, poids = 0.75)
prime_unique_age_bio(age_bio = 10, age_reel = 40, 1000, TM_1811, 0.1, 0.1, 0.1, table_année = 1811, poids = 0.75)

### Simulation des primes : ----

prime_brute_unique(age = 40, capital = 1000, table_morta = TM_1811)
prime_brute_unique(age = 40, capital = 1000, table_morta = TM_1861)
prime_brute_unique(age = 40, capital = 1000, table_morta = TM_1911)



