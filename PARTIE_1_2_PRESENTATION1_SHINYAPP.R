library(readxl)
library(ggplot2)
library(tidyr)
library(shiny)
library(DT)
source("PARTIE_1_1_FONCTIONS_PRIMES.R")

ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "lux"),
  "Bureau d'étude",
  tabPanel(
    "Tarification d'un contrat assurance obsèques et prise en compte de l'âge physiologique.",
    titlePanel("Simulation de votre prime"),
    sidebarLayout(
      sidebarPanel(
        numericInput("Age", "Saisissez votre âge :", value = 60, min = 40, max = 80, step = 1),
        numericInput("Capital", "Quelle somme souhaitez-vous recevoir ?", value = 4500),
        selectInput("Contrat", "Choisissez un contrat :", choices = c("Cotisation à Vie","Cotisation 10 ans","Cotisation 20 ans", "Cotisation 30 ans", "Cotisation Unique")),
        textOutput("primeOutput")),
      mainPanel(
        tabsetPanel(
          id = "tabs",
          tabPanel("Valeurs de rachat et de réduction",tagList(
                    div(style = "margin-top: 40px;"),
                   DTOutput("df_rachat_réduction"),
                   div(style = "margin-top: 40px;"),
                   plotOutput("plot_evol_rachat_reduc")), 
                   div(style = "margin-top: 40px;")),
          tabPanel("Comparaison des contrats", DTOutput("df_comparaisons")),
          tabPanel("Evolution", tagList(
            div(style = "margin-top: 40px;"),
            plotOutput("plot_compa"),
            div(style = "margin-top: 40px;"),
            plotOutput("plot_compa_bis"),
            div(style = "margin-top: 40px;"))
            )
        )
      )
    )
  )
)

server <- function(input, output) {
  
  output$primeOutput <- renderText({
    if(is.na(input$Age) || is.na(input$Capital)){ 
      "Votre prime annuelle sera de :"
    }
    else if(input$Contrat == "Cotisation à Vie"){ 
      prime_calculée <- prime_brute_coti_vie(age = input$Age, capital = input$Capital)
      paste("Votre prime annuelle sera de :", round(prime_calculée,2), " euros.")
    }
    else if(input$Contrat == "Cotisation Unique"){ 
      prime_calculée <- prime_brute_unique(age = input$Age, capital = input$Capital)
      paste("Votre prime unique sera de :", round(prime_calculée,2), " euros.")
    }
    else if (input$Contrat == "Cotisation 10 ans"){ 
      prime_calculée <- prime_brute_coti_n_années(age = input$Age, capital = input$Capital, nb_années = 10 )
      paste("Votre prime annuelle sera de :", round(prime_calculée,2), " euros.")
    }
    else if (input$Contrat == "Cotisation 20 ans"){ 
      prime_calculée <- prime_brute_coti_n_années(age = input$Age, capital = input$Capital, nb_années = 20 )
      paste("Votre prime annuelle sera de :", round(prime_calculée,2), " euros.")
    }
    else if (input$Contrat == "Cotisation 30 ans"){ 
      prime_calculée <- prime_brute_coti_n_années(age = input$Age, capital = input$Capital, nb_années = 30 )
      paste("Votre prime annuelle sera de :", round(prime_calculée,2), " euros.")
    }
  })
  
  output$df_rachat_réduction <- renderDT({
    
    if(input$Contrat == "Cotisation à Vie"){
      dff <- tab_coti_vie(va_age = input$Age, va_capital = input$Capital)
    }
    else if(input$Contrat == "Cotisation Unique"){ 
      dff <- tab_coti_unique(va_age = input$Age, va_capital = input$Capital)
    }
    else if (input$Contrat == "Cotisation 10 ans"){ 
      dff <- tab_annees_fixe(va_age = input$Age, va_capital = input$Capital, duree = 10)
    }
    else if (input$Contrat == "Cotisation 20 ans"){ 
      dff <- tab_annees_fixe(va_age = input$Age, va_capital = input$Capital, duree = 20)
    }
    else if (input$Contrat == "Cotisation 30 ans"){ 
      dff <- tab_annees_fixe(va_age = input$Age, va_capital = input$Capital, duree = 30)
    }
    
    datatable(
      dff,
      options = list(
        dom = 't', # Afficher seulement la table sans la recherche, les informations de pagination, etc.
        scrollX = TRUE, # Activer la barre de défilement horizontale si le tableau est trop large
        columnDefs = list(list(width = '80px', targets = '_all')), # Définir une largeur fixe pour toutes les colonnes
        autoWidth = TRUE, # Ajuster automatiquement la largeur des colonnes en fonction de la taille de la table
        pageLength = 5, # Définir le nombre de lignes affichées par page
        lengthChange = FALSE, # Masquer l'option de changement du nombre de lignes affichées par page
        searching = FALSE, # Masquer la barre de recherche
        info = FALSE # Masquer les informations de pagination
      ),
      rownames = TRUE, # Masquer la colonne des noms de lignes
      class = 'compact', # Utiliser le style compact pour réduire la taille du tableau
      caption = paste("Valeurs de rachat et réduction d'un contrat ",input$Contrat, " pour une personne de " ,input$Age,"ans  et un capital de ", input$Capital," euros.")  # Ajouter un titre au tableau
    )
  })
  
  output$plot_evol_rachat_reduc <- renderPlot({
    
    if(input$Contrat == "Cotisation à Vie"){
      dff <- tab_coti_vie_r(va_age = input$Age, va_capital = input$Capital)
      plot_réduction_rachat(dff)
    }
    else if(input$Contrat == "Cotisation Unique"){ 
      dff <- tab_coti_unique_r(va_age = input$Age, va_capital = input$Capital)
      plot_réduction_rachat(dff)
    }
    else if (input$Contrat == "Cotisation 10 ans"){ 
      dff <- tab_annees_fixe_r(va_age = input$Age, va_capital = input$Capital, duree = 10)
      plot_réduction_rachat(dff)
    }
    else if (input$Contrat == "Cotisation 20 ans"){ 
      dff <- tab_annees_fixe_r(va_age = input$Age, va_capital = input$Capital, duree = 20)
      plot_réduction_rachat(dff)
    }
    else if (input$Contrat == "Cotisation 30 ans"){ 
      dff <- tab_annees_fixe_r(va_age = input$Age, va_capital = input$Capital, duree = 30)
      plot_réduction_rachat(dff)
    }
  })
  
  output$df_comparaisons <- renderDT({
    
    dff <- f_comparaison(va_age = input$Age, va_capital = input$Capital)
    datatable(
      dff,
      options = list(
        dom = 't', # Afficher seulement la table sans la recherche, les informations de pagination, etc.
        scrollX = TRUE, # Activer la barre de défilement horizontale si le tableau est trop large
        columnDefs = list(list(width = '80px', targets = '_all')), # Définir une largeur fixe pour toutes les colonnes
        autoWidth = TRUE, # Ajuster automatiquement la largeur des colonnes en fonction de la taille de la table
        pageLength = 20, # Définir le nombre de lignes affichées par page
        lengthChange = FALSE, # Masquer loption de changement du nombre de lignes affichées par page
        searching = FALSE, # Masquer la barre de recherche
        info = FALSE # Masquer les informations de pagination
      ),
      rownames = TRUE, # Masquer la colonne des noms de lignes
      class = 'compact', # Utiliser le style compact pour réduire la taille du tableau
      caption = paste("Comparaison des contrats pour une personne de ",input$Age," ans et un capital de ", input$Capital," euros.") # Ajouter un titre au tableau
    )
  })
  
  output$plot_compa <- renderPlot(
    plot_comparaisons
  )
  
  output$plot_compa_bis <- renderPlot(
    plot_comparaisons_bis
  )
  
}

shinyApp(ui, server)