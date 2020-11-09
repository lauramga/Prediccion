
# Libraries and functions

library(readr)
library(here) # Comentar
library(tidyverse)
library(janitor) # Clean names
library(skimr) # Beautiful Summarize
library(magrittr) # Pipe operators
library(corrplot) # Correlations
library(ggcorrplot)  # Correlations
library(PerformanceAnalytics) # Correlations
library(leaps) # Model selection


# Read Data
DATOS_NBA <- read.csv("nba.csv", sep= ",",  fileEncoding="latin1")
View(DATOS_NBA)

# Variables Names
DATOS_NBA %<>% clean_names()
colnames(DATOS_NBA)

# Summarize Data
skim(DATOS_NBA)

# Data Wrangling data: process of cleaning and unifying complex data sets for 
# analysis, in turn boosting productivity within an organization.

# delete duplicate
# Remove duplicate rows of the dataframe
DATOS_NBA %<>% distinct(player,.keep_all= TRUE)

# delete NA's
DATOS_NBA %<>% drop_na()

# Summarise
skim(DATOS_NBA)

DATOS_NBA %>% 
  select_at(vars(-c("player","nba_country","tm"))) %>% 
  tidyr::gather("id", "value", 2:25) %>% 
  ggplot(., aes(y=salary, x=value))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(~id,ncol=2,scales="free_x")


DATOS_NBA %>% 
  select_at(vars(-c("player","nba_country","tm"))) %>% 
  tidyr::gather("id", "value", 2:25) %>% 
  ggplot(., aes(y=log(salary), x=value))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(~id,ncol=2,scales="free_x")



## EDA
# Log salary

log_data <- DATOS_NBA %>% mutate(salary=log(salary))

skim(log_data)

# Excluded vars (factor)

vars <- c("player","nba_country","tm")

# Correlations
corrplot(cor(log_data %>% 
               select_at(vars(-vars)), 
             use = "complete.obs"), 
         method = "circle",type = "upper")

# Other Correlations


ggcorrplot(cor(log_data %>% 
                 select_at(vars(-vars)), 
               use = "complete.obs"),
           hc.order = TRUE,
           type = "lower",  lab = TRUE)

# Other Correlations

chart.Correlation(log_data %>% 
                    select_at(vars(-vars)),
                  histogram=TRUE, pch=19)




# Empleamos la funcion lm() para generar un modelo de regresión lineal por 
# minimos cuadrados en el que la variable de respuesta son los jugadores en el
# que la variable de respuesta es..... y el predictor.....

modelo_regresion_vif <- lm(salary~.-player-nba_country-tm, data=log_data)

vif_values <- car::vif(model_vif)

#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")

#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)


knitr::kable(vif_values)

## Model Selection

nba <- log_data %>% select_at(vars(-vars))

set.seed(1234)
num_data <- nrow(nba)
num_data_test <- 10
train=sample(num_data ,num_data-num_data_test)


data_train <- nba[train,]
data_test  <-  nba[-train,]

model_select <- regsubsets(salary~. , data =data_train, method = "seqrep",nvmax=24)

model_select_summary <- summary(model_select)

data.frame(
  Adj.R2 = (model_select_summary$adjr2),
  CP = (model_select_summary$cp),
  BIC = (model_select_summary$bic)
)


model_select_summary$outmat

plot(model_select, scale = "bic", main = "BIC")


data.frame(
  Adj.R2 = which.max(model_select_summary$adjr2),
  CP = which.min(model_select_summary$cp),
  BIC = which.min(model_select_summary$bic)
)


coef(model_select,which.min(model_select_summary$adjr2))

coef(model_select,which.min(model_select_summary$cp))

coef(model_select,which.min(model_select_summary$bic))


# adjR2 model

nba_r2 <- lm(salary~ mp , data =data_train)
summary(nba_r2)

# CP model

nba_cp <- lm(salary~ nba_draft_number+age+mp+per+ts+f_tr+trb+ast+tov+usg+dws+ws_48+dbpm, data =data_train)
summary(nba_cp)

# BIC model

nba_bic <- lm(salary~ nba_draft_number+age+mp+drb, data =data_train)
summary(nba_bic)



# Prediction

# adjR2
predict_r2 <- predict(nba_r2,newdata = data_test)


exp(cbind(predict_r2,data_test$salary))


mean((data_test$salary-predict_r2)^2)
sqrt(mean((data_test$salary-predict_r2)^2))


# CP
predict_cp <- predict(nba_cp,newdata = data_test)
cbind(predict_cp,data_test$salary)

exp(cbind(predict_cp,data_test$salary))

mean((data_test$salary-predict_cp)^2)
sqrt(mean((data_test$salary-predict_cp)^2))

# BIC (Bayesiano)
predict_bic <- predict(nba_bic,newdata = data_test)
cbind(predict_bic,data_test$salary)

exp(cbind(predict_bic,data_test$salary))

mean((data_test$salary-predict_bic)^2)

sqrt(mean((data_test$salary-predict_bic)^2))




