ui <- fluidPage(
  titlePanel("Progetto Metodi Informatici"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "group_check",
        label = "Selezione Stocks", 
        selected = names,
        choices = names
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "plot_map", height = "500px")
    )
  ),
  
  #####
  
  sidebarLayout(
    sidebarPanel(
      p("Classificazione gusto vino"),
      numericInput(
        'acidi_non_volatili',
        'Acidi non volatili (mg/L)',
        value = 0
      ),
      numericInput(
        'acidi_volatili',
        'Acidi volatili (mg/L)',
        value = 1,
        min = 0,
        max = 2
      ),
      numericInput(
        'acido_citrico',
        'Acido citrico',
        value = 0,
        min = 0,
        max = 2
      ),
      numericInput(
        'zuccheri_residui',
        'Zuccheri residui',
        value = 0,
        min = 0,
        max = 100
      ),
      numericInput(
        'cloruri',
        'cloruri',
        value = 0,
        min = 0,
        max = 1
      ),
      numericInput(
        'anidride_solforosa_libera',
        'Anidride solforosa libera',
        value = 0,
        min = min(0),
        max = max(400)
      ),
      numericInput(
        'anidride_solforosa_totale',
        'Anidride solforosa totale',
        value = 0,
        min = 0,
        max = 400
      ),
      numericInput(
        'densita',
        'Densità',
        value = 0,
        min = 0,
        max = 2
      ),
      numericInput(
        'ph',
        'pH',
        value = 7,
        min = 0,
        max = 14
      ),
      numericInput(
        'solfati',
        'Solfati',
        value = 0,
        min = 0,
        max = 2
      ),
      numericInput(
        'alcol',
        'Alcol',
        value = 0,
        min = 0,
        max = 20
      ),
    ),
    
    mainPanel(textOutput(outputId = "predicted_gusto"))
    )
  )
