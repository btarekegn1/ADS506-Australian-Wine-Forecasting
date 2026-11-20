# ADS-506 Week 5: Australian Wine Sales Forecasting Dashboard
# Run in RStudio: install.packages(c('shiny','fpp3','readr','dplyr','tidyr','ggplot2','DT','lubridate'))
library(shiny)
library(fpp3)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(lubridate)

raw <- read_csv('AustralianWines.csv', na = c("", "NA", "*"), show_col_types = FALSE)

# Convert wine sales columns to numeric
raw <- raw |>
  mutate(across(-Month, ~ as.numeric(gsub(",", "", as.character(.x)))))
names(raw) <- trimws(names(raw))
stopifnot('Month' %in% names(raw))
varieties <- setdiff(names(raw), 'Month')
# The CSV uses abbreviated month-year labels such as Jan-80.
raw$Month <- yearmonth(as.Date(paste0('01-', raw$Month), format = '%d-%b-%y'))
stopifnot(!anyNA(raw$Month))
wines <- raw |>
  pivot_longer(cols = all_of(varieties), names_to = 'Varietal', values_to = 'Sales') |>
  mutate(Varietal = as.character(Varietal), Sales = as.numeric(Sales)) |>
  as_tsibble(key = Varietal, index = Month)
min_date <- as.Date(min(wines$Month))
max_date <- as.Date(max(wines$Month))

ui <- fluidPage(
  titlePanel('Australian Wine Sales | Forecast Explorer'),
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput('varietals', 'Wine varieties', choices = varieties,
                  selected = c('Red', 'sparkling'), multiple = TRUE),
      dateRangeInput('dates', 'Historical date range', start = min_date,
                     end = max_date, min = min_date, max = max_date),
      sliderInput('validation', 'Validation months (holdout)', min = 6, max = 24,
                  value = 12, step = 1),
      sliderInput('horizon', 'Future forecast horizon (months)', min = 3, max = 24,
                  value = 12, step = 1),
      checkboxGroupInput('models', 'Models to display',
                         choices = c('TSLM', 'ETS', 'ARIMA'),
                         selected = c('TSLM', 'ETS', 'ARIMA')),
      helpText('Validation uses the final months of the selected historical range. Future forecasts are refitted using all selected observations.')
    ),
    mainPanel(width = 9,
      tabsetPanel(
        tabPanel('Historical Data', plotOutput('history', height = '420px'),
                 h4('Selected observations'), DTOutput('data_table')),
        tabPanel('Forecast Comparison',
                 h4('Holdout validation: observed versus predicted'),
                 plotOutput('validation_plot', height = '440px'),
                 h4('Future forecasts with 80% and 95% prediction intervals'),
                 plotOutput('future_plot', height = '440px')),
        tabPanel('Accuracy & Models',
                 h4('Validation accuracy (original sales units)'),
                 DTOutput('accuracy_table'),
                 h4('Training accuracy'), DTOutput('training_table'),
                 h4('Model specifications'), DTOutput('spec_table')),
        tabPanel('About',
                 h3('How to use this dashboard'),
                 p('Choose one or more wine varieties, a historical date range, a validation period, and a forecast horizon. Compare TSLM, ETS and ARIMA forecasts and their accuracy.'),
                 p('The data are monthly Australian wine sales by varietal, January 1980 through December 1994. Values are in the units supplied in AustralianWines.csv (thousands of liters).'),
                 p('For validation, each model is fitted on the selected period excluding its final holdout months. For future forecasts, models are refitted on the complete selected period.'),
                 p('RMSE and MAE are in original sales units. MAPE is percentage error and can be unstable for small or zero actual values.'),
                 p('The Shiny interface and model workflow were developed with assistance from ChatGPT; analysis, testing, and final submission remain the student’s responsibility.'))
      )
    )
  )
)

server <- function(input, output, session) {
  selected <- reactive({
    req(input$varietals, input$dates)
    x <- wines |>
      filter(Varietal %in% input$varietals,
             as.Date(Month) >= input$dates[1],
             as.Date(Month) <= input$dates[2])
    validate(need(nrow(x) > 0, 'Select at least one variety and a valid date range.'))
    x
  })
  split_data <- reactive({
    x <- selected()
    months <- sort(unique(x$Month))
    validate(need(length(months) >= input$validation + 36,
                  'Choose at least 36 training months plus the selected validation months.'))
    # Require contiguous monthly observations for meaningful seasonal models.
    validate(need(length(months) == as.integer(max(months) - min(months)) + 1,
                  'The selected months must form a continuous monthly sequence.'))
    cutoff <- months[length(months) - input$validation]
    list(train = x |> filter(Month <= cutoff),
         test = x |> filter(Month > cutoff), cutoff = cutoff)
  })
  fit_models <- function(data) {
    data |> model(
      TSLM = TSLM(Sales ~ trend() + season()),
      ETS = ETS(Sales),
      ARIMA = ARIMA(Sales)
    )
  }
  validation_fits <- reactive({ fit_models(split_data()$train) })
  validation_fc <- reactive({
    s <- split_data()
    validation_fits() |> forecast(new_data = s$test)
  })
  future_fits <- reactive({fit_models(selected())})
  future_fc <- reactive({future_fits() |> forecast(h = input$horizon, level = c(80, 95))})
  chosen <- reactive({req(input$models); input$models})
  output$history <- renderPlot({
    ggplot(as_tibble(selected()), aes(Month, Sales, color = Varietal)) +
      geom_line(linewidth = 0.8) + geom_point(size = 0.7) +
      labs(title = 'Monthly Australian wine sales', x = 'Month', y = 'Sales (thousands of liters)') +
      theme_minimal(base_size = 12)
  })
  output$data_table <- DT::renderDT({
    table_data <- selected() |>
      as_tibble() |>
      mutate(Month = as.character(Month)) |>
      select(Month, Varietal, Sales) |>
      as.data.frame()
    
    DT::datatable(
      table_data,
      rownames = FALSE,
      options = list(
        pageLength = 8,
        searching = TRUE,
        ordering = TRUE
      )
    )
  })
  output$validation_plot <- renderPlot({
    fc <- validation_fc() |> filter(.model %in% chosen())
    s <- split_data()
    ggplot() +
      geom_line(data = as_tibble(s$train), aes(Month, Sales), color = 'gray55') +
      geom_line(data = as_tibble(s$test), aes(Month, Sales), color = 'black', linewidth = 0.9) +
      geom_line(data = as_tibble(fc), aes(Month, .mean, color = .model), linewidth = 0.85) +
      facet_wrap(~Varietal, scales = 'free_y') +
      labs(x = 'Month', y = 'Sales (thousands of liters)', color = 'Forecast model',
           caption = 'Gray: training actuals; black: holdout actuals; colored: holdout forecasts') +
      theme_minimal(base_size = 12)
  })
  output$future_plot <- renderPlot({
    fc <- future_fc() |> filter(.model %in% chosen())
    autoplot(fc, selected(), level = 95) +
      facet_wrap(~Varietal, scales = 'free_y') +
      labs(title = 'Future forecasts (95% prediction intervals)',
           x = 'Month', y = 'Sales (thousands of liters)') +
      theme_minimal(base_size = 12)
  })
  output$accuracy_table <- renderDT({
    a <- accuracy(validation_fc(), split_data()$test) |>
      as_tibble() |> filter(.model %in% chosen()) |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      mutate(across(c(RMSE, MAE, MAPE), ~round(.x, 2))) |>
      arrange(Varietal, RMSE)
    datatable(a, rownames = FALSE, options = list(dom = 't', pageLength = 30))
  })
  output$training_table <- renderDT({
    a <- accuracy(validation_fits()) |>
      as_tibble() |> filter(.model %in% chosen()) |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      mutate(across(c(RMSE, MAE, MAPE), ~round(.x, 2)))
    datatable(a, rownames = FALSE, options = list(dom = 't', pageLength = 30))
  })
  output$spec_table <- DT::renderDT({
    
    fits <- validation_fits()
    
    specs <- data.frame(
      Varietal = character(),
      Model = character(),
      Specification = character()
    )
    
    for (i in seq_len(nrow(fits))) {
      for (model_name in c("TSLM", "ETS", "ARIMA")) {
        
        model_object <- fits[[model_name]][[i]]
        
        specs <- rbind(
          specs,
          data.frame(
            Varietal = as.character(fits$Varietal[i]),
            Model = model_name,
            Specification = as.character(
              fabletools::model_sum(model_object)
            )
          )
        )
      }
    }
    
    specs <- specs[specs$Model %in% chosen(), ]
    
    DT::datatable(
      specs,
      rownames = FALSE,
      options = list(dom = "t", pageLength = 30)
    )
    
  })
}
shinyApp(ui, server)
