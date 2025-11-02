


library(shiny)
library(ggplot2)
library(dplyr)
library(openxlsx)
library(scales)  # Para formato con comas

# --- Paso 1: Cargar datos y generar resumen ---
datos <- read.csv("data.csv")

resumen <- datos %>%
  group_by(ramo, TIPO_CLIENTE, numero_polizas) %>%
  summarise(
    Q1 = quantile(PRIMA_MES_TRIM_SEMES_SOLES, 0.25, na.rm = TRUE),
    Q3 = quantile(PRIMA_MES_TRIM_SEMES_SOLES, 0.75, na.rm = TRUE),
    mediana = median(PRIMA_MES_TRIM_SEMES_SOLES, na.rm = TRUE),
    limite_superior = Q3 + 1.5 * (Q3 - Q1),
    total_clientes = n(),
    clientes_fuera = sum(PRIMA_MES_TRIM_SEMES_SOLES > (Q3 + 1.5 * (Q3 - Q1)), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(porcentaje_fuera = round((clientes_fuera / total_clientes) * 100, 2))

# --- Paso 2: UI ---
ui <- fluidPage(
  titlePanel("Análisis de Primas por Filtros"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("ramo", "Selecciona el ramo:", choices = unique(resumen$ramo)),
      selectInput("tipo_cliente", "Selecciona el tipo de cliente:", choices = unique(resumen$TIPO_CLIENTE)),
      selectInput("num_polizas", "Selecciona número de pólizas:", choices = unique(resumen$numero_polizas)),
      downloadButton("descargar_grafico", "Descargar gráfico"),
      downloadButton("descargar_reporte", "Descargar reporte en Excel")
    ),
    
    mainPanel(
      plotOutput("boxplot"),
      br(),
      textOutput("detalle")
    )
  )
)

# --- Paso 3: Server ---
server <- function(input, output) {
  
  datos_filtro <- reactive({
    resumen %>%
      filter(ramo == input$ramo,
             TIPO_CLIENTE == input$tipo_cliente,
             numero_polizas == input$num_polizas)
  })
  
  datos_originales <- reactive({
    datos %>%
      filter(ramo == input$ramo,
             TIPO_CLIENTE == input$tipo_cliente,
             numero_polizas == input$num_polizas)
  })
  
  # Gráfico con formato correcto
  output$boxplot <- renderPlot({
    df_resumen <- datos_filtro()
    df_original <- datos_originales()
    
    limite <- df_resumen$limite_superior
    
    ggplot(df_original, aes(x = factor(numero_polizas), y = PRIMA_MES_TRIM_SEMES_SOLES)) +
      geom_boxplot(fill = "lightblue") +
      geom_hline(yintercept = limite, color = "red", linetype = "dashed", size = 1) +
      annotate("text", x = 1, y = limite, label = paste("Límite:", comma(round(limite, 2))), color = "red", vjust = -1) +
      labs(
        title = "Distribución de Primas",
        subtitle = paste("Límite superior:", comma(round(limite, 2))),
        y = "Prima (Soles)",
        x = "Número de pólizas"
      ) +
      scale_y_continuous(labels = comma) +  # Eje Y con comas
      theme_minimal()
  })
  
  # Texto debajo del gráfico
  output$detalle <- renderText({
    df <- datos_filtro()
    paste0("Total clientes: ", format(df$total_clientes, big.mark = ","),
           " | Clientes sobre el límite: ", format(df$clientes_fuera, big.mark = ","),
           " (", df$porcentaje_fuera, "%)")
  })
  
  # Descargar gráfico
  output$descargar_grafico <- downloadHandler(
    filename = function() { paste("grafico_", input$ramo, ".png", sep = "") },
    content = function(file) {
      df_resumen <- datos_filtro()
      df_original <- datos_originales()
      limite <- df_resumen$limite_superior
      
      g <- ggplot(df_original, aes(x = factor(numero_polizas), y = PRIMA_MES_TRIM_SEMES_SOLES)) +
        geom_boxplot(fill = "lightblue") +
        geom_hline(yintercept = limite, color = "red", linetype = "dashed", size = 1) +
        annotate("text", x = 1, y = limite, label = paste("Límite:", comma(round(limite, 2))), color = "red", vjust = -1) +
        labs(
          title = "Distribución de Primas",
          subtitle = paste("Límite superior:", comma(round(limite, 2))),
          y = "Prima (Soles)",
          x = "Número de pólizas"
        ) +
        scale_y_continuous(labels = comma) +
        theme_minimal()
      
      ggsave(file, g)
    }
  )
  
  # Descargar reporte en Excel con porcentaje incluido
  output$descargar_reporte <- downloadHandler(
    filename = function() { "reporte_completo.xlsx" },
    content = function(file) {
      write.xlsx(resumen, file)
    }
  )
}

shinyApp(ui, server)