library(here)
library(tidyverse)
library(tidymodels)
library(shinydashboard)
library(shiny)
library(shinythemes)
library(knitr)
library(glue)

data=read_csv(file=here('train.csv'))
model=readRDS(file=here('model'))

data=data[,-c(1,2)]

colnames(data)=lapply(colnames(data),function(x) stringr::str_replace_all(string=x,pattern=" ", repl="_")) |>  tolower()

categoricos=data |> select_if(is.character) |> colnames()
for(col in categoricos) {
    data[[col]] =as.factor(data[[col]])
}

colnames(data)
glimpse(data)
data=data|> na.omit()
data_shiny = subset(head(data,0), select = -c(satisfaction))
data_shiny |> colnames()


ui <- dashboardPage(
    skin='green',
    dashboardHeader(title ="Airline Passenger Satisfaction"),
    dashboardSidebar(disable=TRUE),
    
    dashboardBody(
        fluidRow(
            column(width= 5,offset = 1,
                   selectInput(
                       "gender",
                       label = "Gender",
                       choices =levels(data$gender),
                       multiple = FALSE,
                       selected =levels(data$gender)[1] 
                   ),
                   
                   selectInput(
                       "customer_type",
                       label = "Customer type",
                       choices =levels(data$customer_type),
                       multiple = FALSE,
                       selected = levels(data$customer_type)[1]
                   ),
                   
                   selectInput(
                       "type_of_travel",
                       label = "Type of travel",
                       choices = levels(data$type_of_travel) |> sort(),
                       selected = levels(data$type_of_travel)[1]
                   ),
                   
                   selectInput(
                       "class",
                       label = "Class",
                       choices =levels(data$class),
                       multiple = FALSE,
                       selected =levels(data$class)[1] 
                   ),
                   sliderInput(
                       'inflight_wifi_service',
                       label = 'Inflight wifi service',
                       min=min(data$inflight_wifi_service),
                       max=max(data$inflight_wifi_service),
                       value=1
                   ), 
                   sliderInput(
                     'inflight_entertainment',
                     label = 'Inflight entertainment',
                     min=min(data$inflight_entertainment),
                     max=max(data$inflight_entertainment),
                     value=mean(data$inflight_entertainment)
                   ),
                   sliderInput(
                     'inflight_service',
                     label = 'Inflight service',
                     min=min(data$leg_room_service),
                     max=max(data$leg_room_service),
                     value=mean(data$leg_room_service)
                   ),
                   sliderInput(
                     'food_and_drink',
                     label = 'Food and drink',
                     min=min(data$food_and_drink),
                     max=max(data$food_and_drink),
                     value=mean(data$food_and_drink)
                   ),
                   sliderInput(
                     'seat_comfort',
                     label = 'Seat comfort',
                     min=min(data$seat_comfort),
                     max=max(data$seat_comfort),
                     value=mean(data$seat_comfort)
                   ),
                  
                   sliderInput(
                     'on-board_service',
                     label = 'On-board service',
                     min=min(data$`on-board_service`),
                     max=max(data$`on-board_service`),
                     value=mean(data$`on-board_service`)
                   ),
                   sliderInput(
                     'leg_room_service',
                     label = 'Leg room service',
                     min=min(data$leg_room_service),
                     max=max(data$leg_room_service),
                     value=mean(data$leg_room_service)
                   ),),
            
            column(width=5,offset = 1,
                   sliderInput(
                       'age',
                       label ='Age',
                       min=18,
                       max=99,
                       value=25
                   ),
                   sliderInput(
                       'flight_distance',
                       label ='flight_distance',
                       min=min(data$flight_distance),
                       max=max(data$flight_distance),
                       value=mean(data$flight_distance) |> round(1) ,
                       step = 0.1
                   ),
                   sliderInput(
                       'departure/arrival_time_convenient',
                       label = "Departure/Arrival time convenient`",
                       min=min(data$`departure/arrival_time_convenient`),
                       max=max(data$`departure/arrival_time_convenient`),
                       value=1
                   ),
                   sliderInput(
                     'departure_delay_in_minutes',
                     label = 'Departure delay in minutes',
                     min=min(data$departure_delay_in_minutes),
                     max=max(data$departure_delay_in_minutes),
                     value=mean(data$departure_delay_in_minutes)
                   ),
                   sliderInput(
                     'arrival_delay_in_minutes',
                     label = 'Arrival delay in minutes',
                     min=min(data$arrival_delay_in_minutes),
                     max=max(data$arrival_delay_in_minutes),
                     value=mean(data$arrival_delay_in_minutes)
                   ),
                   sliderInput(
                       'ease_of_online_booking',
                       label = 'Ease of online booking',
                       min=min(data$ease_of_online_booking),
                       max=max(data$ease_of_online_booking),
                       value=mean(data$ease_of_online_booking)
                   ),
                   sliderInput(
                       'gate_location',
                       label = 'Gate location',
                       min=min(data$gate_location),
                       max=max(data$gate_location),
                       value=mean(data$gate_location)
                   ),
                   sliderInput(
                     'online_boarding',
                     label = 'Online boarding',
                     min=min(data$online_boarding),
                     max=max(data$online_boarding),
                     value=mean(data$online_boarding)
                   ),
                   
                   sliderInput(
                     'baggage_handling',
                     label = 'Baggage handling',
                     min=min(data$baggage_handling),
                     max=max(data$baggage_handling),
                     value=mean(data$baggage_handling)
                   ),
                   sliderInput(
                     'checkin_service',
                     label = 'Checkin service',
                     min=min(data$checkin_service),
                     max=max(data$checkin_service),
                     value=mean(data$checkin_service)
                   ),
                   
                   sliderInput(
                     'cleanliness',
                     label = 'Cleanliness',
                     min=min(data$cleanliness),
                     max=max(data$cleanliness),
                     value=mean(data$cleanliness)
                   ),
            ),
            
            
            fixedRow(
                
                box(align="center",
                    verbatimTextOutput('pred'), width = 12,collapsible = TRUE,collapsed = TRUE,
                    title ="Predictions",
                ),
            ))))








server <- function(input, output) {
    output$pred =renderPrint({
        
        data_shiny[1, ]=list(input$gender ,
                             input$customer_type,
                             input$age  ,
                             input$type_of_travel,
                             input$class,
                             input$flight_distance,
                             input$inflight_wifi_service,
                             input$`departure/arrival_time_convenient`,
                             input$ease_of_online_booking,
                             input$gate_location,
                             input$food_and_drink,
                             input$online_boarding,
                             input$seat_comfort,
                             input$inflight_entertainment,
                             input$`on-board_service`,
                             input$leg_room_service,
                             input$baggage_handling,
                             input$checkin_service,
                             input$inflight_service,
                             input$cleanliness,
                             input$departure_delay_in_minutes,
                             input$arrival_delay_in_minutes
        )
        
        pred = predict(model,data_shiny,type = 'prob') 
        good=pred$.pred_satisfied * 100 |>  round(2)
        
        
        print(glue('Neutral or dissatisfied:{100-good}% - Satisfied: {good}%'))
        
      
    })
}

shinyApp(ui, server)
