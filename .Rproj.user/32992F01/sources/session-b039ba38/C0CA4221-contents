library(here)
library(tidyverse)
library(tidymodels)
library(caret)

data=read_csv(here('train.csv'),)

data=data[,-c(1,2)]

colnames(data)=lapply(colnames(data),function(x) stringr::str_replace_all(string=x,pattern=" ", repl="_")) |>  tolower()

data |> glimpse()
data$gender =as.factor(data$gender)
data$customer_type =as.factor(data$customer_type)
data$type_of_travel =as.factor(data$type_of_travel)
data$class= as.factor(data$class)
data$inflight_wifi_service =as.factor(data$inflight_wifi_service)
data$`departure/arrival_time_convenient`=as.factor(data$`departure/arrival_time_convenient`)
data$ease_of_online_booking = as.factor(data$ease_of_online_booking)
data$gate_location = as.factor(data$gate_location)
data$food_and_drink = as.factor(data$food_and_drink)
data$online_boarding = as.factor(data$online_boarding)
data$seat_comfort = as.factor(data$seat_comfort)
data$inflight_entertainment = as.factor(data$inflight_entertainment)
data$`on-board_service` = as.factor(data$`on-board_service`)
data$leg_room_service = as.factor(data$leg_room_service)
data$baggage_handling = as.factor(data$baggage_handling)
data$checkin_service = as.factor(data$checkin_service)
data$inflight_service = as.factor(data$inflight_service)
data$cleanliness = as.factor(data$cleanliness)
data$satisfaction = as.factor(data$satisfaction)

data |> glimpse()


set.seed(108)

data_split = initial_split(data,prop=0.80)
data_split

data_train = training(data_split)
data_test = testing(data_split)

show_engines("rand_forest")
model=rand_forest(mode ='classification',engine = 'randomForest' )
model_fit= model |> fit(satisfaction ~., data=data_train)

new_data=data_test |> na.omit() |> select(-c(satisfaction))
real= data_test |> na.omit() |> select(satisfaction)
pred=predict(model_fit,new_data)


real |> length()
pred |> length()


pred |> nrow()
real |> nrow()
levels(pred)= c('satisfied','neutral or dissatisfied')
levels(real)= c('satisfied','neutral or dissatisfied')
levels(real)
levels(pred)
conf_mat(pred,real)
real
pred
confusionMatrix(pred$.pred_class,real$satisfaction)

# Full Model --------------------------------------------------------
treino=read_csv(here('train.csv'))
treino=treino[,-c(1,2)]
teste=read_csv(here('test.csv'))
teste = teste |> na.omit()
teste=teste[,-c(1,2)]

colnames(treino)=lapply(colnames(treino),function(x) stringr::str_replace_all(string=x,pattern=" ", repl="_")) |>  tolower()
colnames(teste)=lapply(colnames(teste),function(x) stringr::str_replace_all(string=x,pattern=" ", repl="_")) |>  tolower()

categoricos=treino |> select_if(is.character) 
to_factor=function(dados){
  for(col in colnames(categoricos)) {
    dados[[col]] =as.factor(dados[[col]])
  }
  
  return(dados)
}

treino=to_factor(treino)
teste=to_factor(teste)

model=rand_forest(mode ='classification',engine = 'randomForest' )
model_fit= model |> fit(satisfaction ~., data=treino)

treino
teste|> select(-satisfaction)
pred=predict(object = model_fit,new_data = teste |> select(-satisfaction))
pred
confusionMatrix(pred$.pred_class,teste$satisfaction)

saveRDS(model_fit,here('model'))
