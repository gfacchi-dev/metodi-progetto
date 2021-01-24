server <- function(input, output) {
  multiple_dataset <- reactive({
    ds = c();
    
    if (!is.null(input$group_check))
      for (i in 1:length(input$group_check)){
        if (input$group_check[i] == "acidi_non_volatili")
          ds <- cbind(ds, df$acidi_non_volatili)
        else if (input$group_check[i] == "acidi_volatili")
          ds <- cbind(ds, df$acidi_volatili)
        else if (input$group_check[i] == "acido_citrico")
          ds <- cbind(ds, df$acido_citrico)
        else if (input$group_check[i] == "zuccheri_residui")
          ds <- cbind(ds, df$zuccheri_residui)
        else if (input$group_check[i] == "cloruri")
          ds <- cbind(ds, df$cloruri)
        else if (input$group_check[i] == "anidride_solforosa_libera")
          ds <- cbind(ds, df$anidride_solforosa_libera)
        else if (input$group_check[i] == "anidride_solforosa_totale")
          ds <- cbind(ds, df$anidride_solforosa_totale)
        else if (input$group_check[i] == "densita")
          ds <- cbind(ds, df$densita)
        else if (input$group_check[i] == "ph")
          ds <- cbind(ds, df$ph)
        else if (input$group_check[i] == "solfati")
          ds <- cbind(ds, df$solfati)
        else if (input$group_check[i] == "alcol")
          ds <- cbind(ds, df$alcol)
        else if (input$group_check[i] == "qualita")
          ds <- cbind(ds, df$qualita)
      }
    colnames(ds) <- names
    ds
  })
  
  
  # settore <- reactive({
  #   ds = c();
  #   if (input$group_radio == "OILGAS")
  #     ds <- cbind(stocks.compoundReturns$TEN.MI, stocks.compoundReturns$ENI.MI)
  #   else if (input$group_radio == "FINANCE")
  #     ds <- cbind(stocks.compoundReturns$EXO.MI, stocks.compoundReturns$AZM.MI)
  #   else if (input$group_radio == "BIOTECH")
  #     ds <- cbind(stocks.compoundReturns$REC.MI, stocks.compoundReturns$DIA.MI)
  #   
  #   ds <- window(ds, start = input$window[1], end = input$window[2])
  #   ds
  # })
  # 
  output$plot_map <- renderPlot({
    heatmap(cor(multiple_dataset()), main = "Heatmap della matrice di correlazione tra le variabili del dataset") 
  })
  
  output$predicted_gusto <-
    renderText({
        features <- data.frame(
          acidi_non_volatili = input$acidi_non_volatili,
          acidi_volatili = input$acidi_volatili,
          acido_citrico = input$acido_citrico,
          zuccheri_residui = input$zuccheri_residui,
          cloruri = input$cloruri,
          anidride_solforosa_libera  = input$anidride_solforosa_libera,
          anidride_solforosa_totale = input$anidride_solforosa_totale,
          densita = input$densita,
          ph = input$ph,
          solfati = input$solfati,
          alcol = input$alcol,
          row.names = NULL
      )
      paste('Gusto previsto:', predict(model, features))
    })
}