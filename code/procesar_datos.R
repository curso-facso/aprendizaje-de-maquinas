library(tidyverse)
library(janitor)
library(fastDummies)
library(caret)
library(xgboost)
library(feather)

# Cargar datos de EPF
gastos <-  read_csv2("data/base-gastos-viii-epf-(formato-csv).csv")
personas <- read_csv2("data/base-personas-viii-epf-(formato-csv).csv")


# Dejar solo los hogares que tienen gasto en bencina
gastos_bencina <- gastos %>% 
  clean_names() %>% 
  filter(ccif %in% c("07.2.2.01.01", "07.2.2.01.02")) %>% 
  group_by(folio) %>% 
  slice(1) %>% 
  ungroup() 

# Trabajar base personas
personas2 <- personas %>%
  clean_names() %>% 
  select(folio, zona, sexo, edad, ing_disponible = ing_disp_hog_hd_ai, npersonas, aecise) %>% 
  filter(edad >= 0  & aecise !=  -88 & aecise != -99) %>% 
  mutate(ocupado = if_else(aecise >= 1, 1, 0) ,
         edad_tramos = case_when(
           edad < 0 ~ NA_integer_,
           edad < 10 ~ 1,
           edad < 20 ~ 2,
           edad < 30 ~ 3,
           edad < 40 ~ 4,
           edad < 50 ~ 5,
           edad < 60 ~ 6,
           edad < 70 ~ 7,
           edad < 80 ~ 8,
           T ~ 9),
         hombre = if_else(sexo == 1, 1, 0),
         mujer = if_else(sexo == 2, 1, 0),
         rm = if_else(zona == 1, 1, 0)
         ) %>% 
  dummy_cols(select_columns = 'edad_tramos')  


# Tabla final
hogares <- personas2 %>% 
  group_by(folio) %>%
  mutate_at(c("mujer", "hombre", "ocupado"), .funs = sum ) %>% 
  mutate_at(vars(starts_with("edad_tramos_")) , .funs = sum) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c("sexo", "edad", "aecise", "edad_tramos", "zona")) %>% 
  left_join(gastos_bencina %>% select(folio, glosa), by = "folio") %>% 
  mutate(bencina = if_else(!is.na(glosa), 1, 0)) %>% 
  select(-glosa)


##########
# SEPARAR 
##########


set.seed(123)
train_indices <- createDataPartition( hogares$bencina, times = 1, p = 0.8, list = FALSE)
train_set <-  hogares[train_indices, ]
test_set <-  hogares[-train_indices, ]



################
# Entrenar modelo
################


# Formato eficiente
dtrain <- xgb.DMatrix(data = train_set %>% select(-folio, -bencina) %>% as.matrix() ,
                      label = train_set$bencina)


# Entrenamiento
model_xgb <- xgboost(
  data = dtrain,
  nrounds = 40, 
  max.depth = 6, 
  objective = "binary:logistic", 
  verbose = T)

# Predecir set de entrenamiento
predict_xgb_train <- predict(model_xgb, dtrain)
predictions_train <- if_else(predict_xgb_train > 0.5, 1, 0)
acc_train <- mean(train_set$bencina == predictions_train)
print(sprintf("acc en test set: %s", round(acc_train, 3)) )

# Predecir set de testeo
predict_xgb_test <- predict(model_xgb, test_set %>% select(-bencina, -folio) %>% as.matrix()) 
predictions_test <- if_else(predict_xgb_test > 0.5, 1, 0)
acc_test <- mean(test_set$bencina == predictions_test)
print(sprintf("acc en test set: %s", round(acc_test, 3)) )


################
# Gasto división
################

total_hogares <- personas %>% 
  clean_names() %>% 
  group_by(folio) %>% 
  dplyr::slice(1) %>% 
  ungroup() %>% 
  summarise(total_exp = sum(fe))
  

gastos_clean <- gastos %>% 
  clean_names() %>% 
  filter(ccif != "04.2.1.01.01" & ccif != "04.2.2.01.01" & ccif != "04.2.2.01.02" & folio != "") %>%
  mutate(gasto_exp = gasto * fe) %>% 
  group_by(d) %>% 
  summarise(gasto_div = sum(gasto_exp)) %>% 
  ungroup() %>% 
  filter(!is.na(d)) %>% 
  mutate(prop = gasto_div / sum(gasto_div) * 100) %>% 
  mutate(total_hogares = total_hogares %>% pull(total_exp)) 


write_feather(gastos_clean, "data/estructura_gastos.feather")
