# Script R Shiny pour l'analyse du taux de pauvreté en Tunisie
# Version avec coordonnées réelles des gouvernorats

library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)

# Interface utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "Taux de Pauvreté Tunisie"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Carte Interactive", tabName = "map", icon = icon("map")),
      menuItem("Visualisations", tabName = "viz", icon = icon("chart-bar")),
      menuItem("Données Brutes", tabName = "data", icon = icon("table")),
      menuItem("Analyse Statistique", tabName = "stats", icon = icon("calculator")),
      hr(),
      downloadButton("downloadData", "Télécharger les données"),
      hr(),
      h5("Source: Données de pauvreté par gouvernorat"),
      h5("Répertoire: C:/Users/ammon/Desktop/pauvrete")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("mean_poverty"),
                valueBoxOutput("max_poverty"),
                valueBoxOutput("min_poverty")
              ),
              fluidRow(
                box(
                  title = "Résumé des Données",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  tableOutput("summary_table")
                )
              ),
              fluidRow(
                box(
                  title = "Distribution du Taux de Pauvreté",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("histogram")
                ),
                box(
                  title = "Top 5 Gouvernorats (Taux le plus élevé)",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("top5_chart")
                )
              )
      ),
      
      tabItem(tabName = "map",
              fluidRow(
                box(
                  title = "Carte Interactive des Taux de Pauvreté",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  leafletOutput("map", height = 600)
                )
              ),
              fluidRow(
                box(
                  title = "Légende",
                  status = "info",
                  solidHeader = FALSE,
                  width = 12,
                  p("Les couleurs représentent les taux de pauvreté:"),
                  div(style = "display: flex; align-items: center;",
                      div(style = "width: 20px; height: 20px; background-color: #005a32; margin-right: 10px;"),
                      span("Faible (0-10%)")),
                  div(style = "display: flex; align-items: center;",
                      div(style = "width: 20px; height: 20px; background-color: #41ab5d; margin-right: 10px;"),
                      span("Modéré (10-20%)")),
                  div(style = "display: flex; align-items: center;",
                      div(style = "width: 20px; height: 20px; background-color: #fec44f; margin-right: 10px;"),
                      span("Élevé (20-30%)")),
                  div(style = "display: flex; align-items: center;",
                      div(style = "width: 20px; height: 20px; background-color: #d95f0e; margin-right: 10px;"),
                      span("Très élevé (30%+)"))
                )
              )
      ),
      
      tabItem(tabName = "viz",
              fluidRow(
                box(
                  title = "Taux de Pauvreté par Gouvernorat",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("bar_chart", height = 500)
                )
              ),
              fluidRow(
                box(
                  title = "Paramètres du Graphique",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 4,
                  selectInput("sort_order", "Ordre d'affichage:",
                              choices = c("Croissant", "Décroissant", "Alphabétique"),
                              selected = "Décroissant"),
                  sliderInput("num_gov", "Nombre de gouvernorats à afficher:",
                              min = 5, max = 24, value = 15)
                ),
                box(
                  title = "Boîte à Moustaches (Boxplot)",
                  status = "info",
                  solidHeader = TRUE,
                  width = 8,
                  plotlyOutput("boxplot", height = 300)
                )
              )
      ),
      
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "Données Brutes",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("raw_data")
                )
              )
      ),
      
      tabItem(tabName = "stats",
              fluidRow(
                box(
                  title = "Statistiques Descriptives",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  tableOutput("stat_table")
                ),
                box(
                  title = "Test de Normalité (Shapiro-Wilk)",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  verbatimTextOutput("normality_test"),
                  hr(),
                  p("Hypothèse nulle: les données suivent une distribution normale"),
                  p("Si p-value < 0.05, on rejette l'hypothèse nulle")
                )
              ),
              fluidRow(
                box(
                  title = "Classification des Gouvernorats par Niveau de Pauvreté",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("classification_table")
                )
              ),
              fluidRow(
                box(
                  title = "Corrélation et Distribution",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("density_plot", height = 300)
                )
              )
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Lecture des données avec coordonnées approximatives des gouvernorats tunisiens
  poverty_data <- reactive({
    data <- data.frame(
      Gouvernorat = c("Tunis", "Ariana", "Ben Arous", "Manouba", "Nabeul", 
                      "Zaghouan", "Bizerte", "Beja", "Jendouba", "Le Kef", 
                      "Siliana", "Sousse", "Monastir", "Mahdia", "Sfax", 
                      "Kairouan", "Kasserine", "Sidi Bouzid", "Gabes", 
                      "Mednine", "Tataouine", "Gafsa", "Tozeur", "Kebili"),
      Taux_de_pauvrete = c(3.5, 5.4, 4.3, 12.1, 7.4, 12.1, 17.5, 32.0, 22.4, 
                           34.2, 27.7, 16.2, 8.3, 21.1, 5.8, 34.9, 32.8, 23.1, 
                           15.8, 21.6, 15.0, 18.0, 14.6, 18.5),
      # Coordonnées approximatives (latitude, longitude) des capitales des gouvernorats
      lat = c(36.8, 36.86, 36.75, 36.81, 36.45, 
              36.4, 37.27, 36.73, 36.5, 36.18, 
              36.08, 35.83, 35.78, 35.5, 34.74, 
              35.68, 35.17, 35.04, 33.88, 33.35, 
              32.93, 34.42, 33.92, 33.7),
      lng = c(10.18, 10.19, 10.22, 10.1, 10.73, 
              10.14, 9.87, 9.18, 8.78, 8.71, 
              9.37, 10.64, 10.83, 11.06, 10.76, 
              10.1, 8.83, 9.48, 10.1, 10.49, 
              10.45, 8.78, 8.13, 8.97)
    )
    
    # Classement des gouvernorats par niveau de pauvreté
    data <- data %>%
      mutate(
        Classification = case_when(
          Taux_de_pauvrete < 10 ~ "Faible",
          Taux_de_pauvrete >= 10 & Taux_de_pauvrete < 20 ~ "Modéré",
          Taux_de_pauvrete >= 20 & Taux_de_pauvrete < 30 ~ "Élevé",
          Taux_de_pauvrete >= 30 ~ "Très élevé"
        ),
        Classification = factor(Classification, levels = c("Faible", "Modéré", "Élevé", "Très élevé"))
      )
    
    return(data)
  })
  
  # Statistiques descriptives
  output$mean_poverty <- renderValueBox({
    data <- poverty_data()
    valueBox(
      value = sprintf("%.1f%%", mean(data$Taux_de_pauvrete)),
      subtitle = "Taux moyen de pauvreté",
      icon = icon("calculator"),
      color = "blue"
    )
  })
  
  output$max_poverty <- renderValueBox({
    data <- poverty_data()
    max_gov <- data$Gouvernorat[which.max(data$Taux_de_pauvrete)]
    valueBox(
      value = sprintf("%.1f%%", max(data$Taux_de_pauvrete)),
      subtitle = paste("Maximum:", max_gov),
      icon = icon("arrow-up"),
      color = "red"
    )
  })
  
  output$min_poverty <- renderValueBox({
    data <- poverty_data()
    min_gov <- data$Gouvernorat[which.min(data$Taux_de_pauvrete)]
    valueBox(
      value = sprintf("%.1f%%", min(data$Taux_de_pauvrete)),
      subtitle = paste("Minimum:", min_gov),
      icon = icon("arrow-down"),
      color = "green"
    )
  })
  
  # Tableau récapitulatif
  output$summary_table <- renderTable({
    data <- poverty_data()
    stats <- data.frame(
      Statistique = c("Moyenne", "Médiane", "Écart-type", "Minimum", "Maximum", "Étendue", "Nombre de gouvernorats"),
      Valeur = c(
        sprintf("%.2f%%", mean(data$Taux_de_pauvrete)),
        sprintf("%.2f%%", median(data$Taux_de_pauvrete)),
        sprintf("%.2f%%", sd(data$Taux_de_pauvrete)),
        sprintf("%.1f%%", min(data$Taux_de_pauvrete)),
        sprintf("%.1f%%", max(data$Taux_de_pauvrete)),
        sprintf("%.1f%%", max(data$Taux_de_pauvrete) - min(data$Taux_de_pauvrete)),
        nrow(data)
      )
    )
    stats
  })
  
  # Histogramme
  output$histogram <- renderPlotly({
    data <- poverty_data()
    
    p <- ggplot(data, aes(x = Taux_de_pauvrete)) +
      geom_histogram(binwidth = 3, fill = "steelblue", color = "white", alpha = 0.8) +
      geom_density(aes(y = after_stat(count) * 3), color = "darkred", linewidth = 1) +
      labs(title = "Distribution du taux de pauvreté",
           x = "Taux de pauvreté (%)",
           y = "Nombre de gouvernorats") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
  })
  
  # Graphique des 5 gouvernorats avec le taux le plus élevé
  output$top5_chart <- renderPlotly({
    data <- poverty_data() %>%
      arrange(desc(Taux_de_pauvrete)) %>%
      head(5)
    
    p <- ggplot(data, aes(x = reorder(Gouvernorat, Taux_de_pauvrete), y = Taux_de_pauvrete)) +
      geom_bar(stat = "identity", fill = "#d95f0e", alpha = 0.8) +
      geom_text(aes(label = sprintf("%.1f%%", Taux_de_pauvrete)), 
                hjust = -0.1, size = 4) +
      labs(title = "Top 5: Taux de pauvreté les plus élevés",
           x = NULL,
           y = "Taux de pauvreté (%)") +
      coord_flip() +
      ylim(0, max(data$Taux_de_pauvrete) * 1.1) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
  })
  
  # Carte Leaflet avec coordonnées réelles
  output$map <- renderLeaflet({
    data <- poverty_data()
    
    # Définition des couleurs basées sur le taux de pauvreté
    getColor <- function(taux) {
      if (taux < 10) {
        return("#005a32")  # Vert foncé
      } else if (taux < 20) {
        return("#41ab5d")  # Vert
      } else if (taux < 30) {
        return("#fec44f")  # Orange
      } else {
        return("#d95f0e")  # Rouge orangé
      }
    }
    
    # Création de la carte avec les coordonnées réelles
    leaflet(data) %>%
      addTiles() %>%
      setView(lng = 9.5, lat = 35, zoom = 6) %>%
      addCircleMarkers(
        ~lng, ~lat,
        radius = ~Taux_de_pauvrete,
        color = ~sapply(Taux_de_pauvrete, getColor),
        stroke = TRUE,
        weight = 2,
        fillOpacity = 0.7,
        label = ~paste(Gouvernorat, ": ", Taux_de_pauvrete, "%"),
        popup = ~paste("<b>", Gouvernorat, "</b><br>",
                       "Taux de pauvreté: ", Taux_de_pauvrete, "%<br>",
                       "Classification: ", Classification, "<br>",
                       "Coordonnées: ", round(lat, 3), "°, ", round(lng, 3), "°")
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("#005a32", "#41ab5d", "#fec44f", "#d95f0e"),
        labels = c("Faible (0-10%)", "Modéré (10-20%)", "Élevé (20-30%)", "Très élevé (30%+)"),
        title = "Niveau de pauvreté",
        opacity = 0.8
      )
  })
  
  # Graphique à barres
  output$bar_chart <- renderPlotly({
    data <- poverty_data()
    
    # Application du tri sélectionné
    if (input$sort_order == "Croissant") {
      data <- data %>%
        arrange(Taux_de_pauvrete)
    } else if (input$sort_order == "Décroissant") {
      data <- data %>%
        arrange(desc(Taux_de_pauvrete))
    } else {
      data <- data %>%
        arrange(Gouvernorat)
    }
    
    # Limiter le nombre de gouvernorats affichés
    data_display <- head(data, input$num_gov)
    
    p <- ggplot(data_display, aes(x = reorder(Gouvernorat, Taux_de_pauvrete), 
                                  y = Taux_de_pauvrete,
                                  fill = Classification,
                                  text = paste(Gouvernorat, ": ", Taux_de_pauvrete, "%"))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("Faible" = "#005a32", 
                                   "Modéré" = "#41ab5d", 
                                   "Élevé" = "#fec44f", 
                                   "Très élevé" = "#d95f0e")) +
      labs(title = "Taux de pauvreté par gouvernorat",
           x = "Gouvernorat",
           y = "Taux de pauvreté (%)",
           fill = "Niveau") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Boxplot
  output$boxplot <- renderPlotly({
    data <- poverty_data()
    
    p <- ggplot(data, aes(y = Taux_de_pauvrete, fill = "Taux de pauvreté")) +
      geom_boxplot(alpha = 0.7) +
      geom_jitter(width = 0.2, aes(color = Classification), size = 2) +
      scale_color_manual(values = c("Faible" = "#005a32", 
                                    "Modéré" = "#41ab5d", 
                                    "Élevé" = "#fec44f", 
                                    "Très élevé" = "#d95f0e")) +
      labs(title = "Distribution des taux de pauvreté",
           y = "Taux de pauvreté (%)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none")
    
    ggplotly(p)
  })
  
  # Données brutes
  output$raw_data <- renderDT({
    datatable(poverty_data() %>% select(-lat, -lng),
              options = list(pageLength = 10,
                             language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')),
              rownames = FALSE) %>%
      formatRound(columns = 'Taux_de_pauvrete', digits = 1)
  })
  
  # Tableau de statistiques
  output$stat_table <- renderTable({
    data <- poverty_data()
    
    # Calcul des statistiques descriptives
    stats <- data.frame(
      "Statistique" = c("Moyenne", "Médiane", "Mode (approx.)", "Écart-type", 
                        "Variance", "Minimum", "Maximum", "Étendue", 
                        "1er Quartile (Q1)", "3ème Quartile (Q3)", 
                        "Intervalle interquartile", "Coefficient de variation"),
      "Valeur" = c(
        sprintf("%.2f%%", mean(data$Taux_de_pauvrete)),
        sprintf("%.2f%%", median(data$Taux_de_pauvrete)),
        sprintf("%.2f%%", as.numeric(names(sort(-table(round(data$Taux_de_pauvrete)))[1]))),
        sprintf("%.2f%%", sd(data$Taux_de_pauvrete)),
        sprintf("%.2f", var(data$Taux_de_pauvrete)),
        sprintf("%.1f%%", min(data$Taux_de_pauvrete)),
        sprintf("%.1f%%", max(data$Taux_de_pauvrete)),
        sprintf("%.1f%%", max(data$Taux_de_pauvrete) - min(data$Taux_de_pauvrete)),
        sprintf("%.2f%%", quantile(data$Taux_de_pauvrete, 0.25)),
        sprintf("%.2f%%", quantile(data$Taux_de_pauvrete, 0.75)),
        sprintf("%.2f%%", quantile(data$Taux_de_pauvrete, 0.75) - quantile(data$Taux_de_pauvrete, 0.25)),
        sprintf("%.2f%%", (sd(data$Taux_de_pauvrete) / mean(data$Taux_de_pauvrete)) * 100)
      )
    )
    stats
  })
  
  # Test de normalité
  output$normality_test <- renderPrint({
    data <- poverty_data()
    test <- shapiro.test(data$Taux_de_pauvrete)
    cat("Test de Shapiro-Wilk pour la normalité\n\n")
    cat("W = ", round(test$statistic, 4), "\n")
    cat("p-value = ", format.pval(test$p.value, digits = 4), "\n\n")
    
    if (test$p.value < 0.05) {
      cat("Conclusion: Les données ne suivent pas une distribution normale (p < 0.05)")
    } else {
      cat("Conclusion: Les données suivent une distribution normale (p >= 0.05)")
    }
  })
  
  # Table de classification
  output$classification_table <- renderDT({
    data <- poverty_data() %>%
      select(Gouvernorat, Taux_de_pauvrete, Classification) %>%
      arrange(desc(Taux_de_pauvrete))
    
    datatable(data,
              options = list(pageLength = 10,
                             language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')),
              rownames = FALSE) %>%
      formatRound(columns = 'Taux_de_pauvrete', digits = 1)
  })
  
  # Graphique de densité
  output$density_plot <- renderPlot({
    data <- poverty_data()
    
    ggplot(data, aes(x = Taux_de_pauvrete)) +
      geom_density(fill = "steelblue", alpha = 0.5) +
      geom_vline(aes(xintercept = mean(Taux_de_pauvrete)), 
                 color = "red", linetype = "dashed", linewidth = 1) +
      geom_vline(aes(xintercept = median(Taux_de_pauvrete)), 
                 color = "darkgreen", linetype = "dashed", linewidth = 1) +
      annotate("text", x = mean(data$Taux_de_pauvrete), y = 0.02, 
               label = paste("Moyenne:", round(mean(data$Taux_de_pauvrete), 1), "%"), 
               color = "red", hjust = -0.1) +
      annotate("text", x = median(data$Taux_de_pauvrete), y = 0.015, 
               label = paste("Médiane:", round(median(data$Taux_de_pauvrete), 1), "%"), 
               color = "darkgreen", hjust = 1.1) +
      labs(title = "Densité de probabilité des taux de pauvreté",
           x = "Taux de pauvreté (%)",
           y = "Densité") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Téléchargement des données
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("taux_pauvrete_tunisie_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(poverty_data(), file, row.names = FALSE)
    }
  )
}

# Exécution de l'application
shinyApp(ui = ui, server = server)