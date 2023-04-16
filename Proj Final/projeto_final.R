#carregando os pacotes
pacotes <- c("tidyverse","randomForest","plotly","dplyr","ROSE","rpart","xgboost","caret","pROC","ROCR","MLmetrics")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#carregando as bases
applications <- read.csv("Dataset/application_record.csv")
behavior <- read.csv("Dataset/credit_record.csv")


#################################################################################
# Análise dos dados e informações
#################################################################################

#entendendo a estrutura das informações
head(applications)
head(behavior)

#Verificando quantos valores de cada status existem na base de behavior
table(behavior$STATUS)
barplot(table(behavior$STATUS), col="steelblue", main="Distribution of Status", xlab="Status", ylab="Frequency")

#Entendendo quantidade de valores possíveis para cada variável
unique_counts <- applications %>%
  summarize_all(n_distinct) %>%
  pivot_longer(everything(), names_to = "Column_Name", values_to = "Num_Unique") %>%
  arrange(Num_Unique)
unique_counts

#removendo coluna "flag_mobil" que não vai interferir no modelo já que todas as variáveis possuem o mesmo valor
applications$FLAG_MOBIL = NULL

#Base com os ids dos clientes e o primeiro mês de cada um
open_month <- behavior %>%
  group_by(ID) %>%
  summarize(begin_month = min(MONTHS_BALANCE)) %>%
  as.data.frame()


#join das bases
base_mergeada <- left_join(applications, open_month,
                           by = "ID")
applications <- base_mergeada

#Verificando população por genero
table(applications$CODE_GENDER)
barplot(table(applications$CODE_GENDER), col="steelblue", main="Distribution of Gender", xlab="Gender", ylab="Frequency")

#Verificando população por genero
table(applications$FLAG_OWN_CAR)
barplot(table(applications$FLAG_OWN_CAR), col="steelblue", main="Distribution of OWN_CAR", xlab="OWN_CAR", ylab="Frequency")

#Verificando população por OWN_REALTY
table(applications$FLAG_OWN_REALTY)
barplot(table(applications$FLAG_OWN_REALTY), col="steelblue", main="Distribution of FLAG_OWN_REALTY", xlab="FLAG_OWN_REALTY", ylab="Frequency")

#Verificando população por CNT_CHILDREN
table(applications$CNT_CHILDREN)
barplot(table(applications$CNT_CHILDREN), col="steelblue", main="Distribution of CNT_CHILDREN", xlab="CNT_CHILDREN", ylab="Frequency")

#Verificando população por NAME_INCOME_TYPE
table(applications$NAME_INCOME_TYPE)
barplot(table(applications$NAME_INCOME_TYPE), col="steelblue", main="Distribution of NAME_INCOME_TYPE", xlab="NAME_INCOME_TYPE", ylab="Frequency")

#Verificando população por NAME_EDUCATION_TYPE
table(applications$NAME_EDUCATION_TYPE)
barplot(table(applications$NAME_EDUCATION_TYPE), col="steelblue", main="Distribution of NAME_EDUCATION_TYPE", xlab="NAME_EDUCATION_TYPE", ylab="Frequency")

#Verificando população por NAME_EDUCATION_TYPE
table(applications$NAME_FAMILY_STATUS)
barplot(table(applications$NAME_FAMILY_STATUS), col="steelblue", main="Distribution of NAME_FAMILY_STATUS", xlab="NAME_FAMILY_STATUS", ylab="Frequency")

#Verificando população por NAME_HOUSING_TYPE
table(applications$NAME_HOUSING_TYPE)
barplot(table(applications$NAME_HOUSING_TYPE), col="steelblue", main="Distribution of NAME_HOUSING_TYPE", xlab="NAME_HOUSING_TYPE", ylab="Frequency")

#Verificando população por FLAG_WORK_PHONE
table(applications$FLAG_WORK_PHONE)
barplot(table(applications$FLAG_WORK_PHONE), col="steelblue", main="Distribution of FLAG_WORK_PHONE", xlab="NAME_HOUSING_TYPE", ylab="Frequency")

#Verificando população por FLAG_EMAIL
table(applications$FLAG_EMAIL)
barplot(table(applications$FLAG_EMAIL), col="steelblue", main="Distribution of FLAG_EMAIL", xlab="FLAG_EMAIL", ylab="Frequency")

#Verificando população por OCCUPATION_TYPE
table(applications$OCCUPATION_TYPE)
barplot(table(applications$OCCUPATION_TYPE), col="steelblue", main="Distribution of OCCUPATION_TYPE", xlab="OCCUPATION_TYPE", ylab="Frequency")

#Verificando população por CNT_FAM_MEMBERS
table(applications$CNT_FAM_MEMBERS)
barplot(table(applications$CNT_FAM_MEMBERS), col="steelblue", main="Distribution of CNT_FAM_MEMBERS", xlab="CNT_FAM_MEMBERS", ylab="Frequency")


#verificando se temos registros IDs duplicados
length(applications$ID)
length(unique(applications$ID))

#removendo registros duplicados
applications <- applications[!duplicated(applications$ID),]

#removendo coluna "OCCUPATION_TYPE" que tem missing values
applications$OCCUPATION_TYPE = NULL


#################################################################################
# Tratamento e organização dos dados/informações
#################################################################################

#tranformando Code gender em campo numérico
applications = applications %>% mutate(MALE = case_when(CODE_GENDER=="M" ~ 1, TRUE ~ 0))
applications$CODE_GENDER = NULL

#tranformando FLAG_OWN_CAR em campo numérico
applications = applications %>% mutate(OWN_CAR = case_when(FLAG_OWN_CAR=="Y" ~ 1, TRUE ~ 0))
applications$FLAG_OWN_CAR = NULL

#tranformando OWN_REALTY em campo numérico
applications = applications %>% mutate(OWN_REALTY = case_when(FLAG_OWN_REALTY=="Y" ~ 1, TRUE ~ 0))
applications$FLAG_OWN_REALTY = NULL

#Criando nova variárvel "WORKING" e transformando em numérico
applications = applications %>% mutate(WORKING = case_when(NAME_INCOME_TYPE=="Working"|NAME_INCOME_TYPE=="Commercial associate"|NAME_INCOME_TYPE=="State servant" ~ 1, TRUE ~ 0))

#Criando INCOME_TYPE com base em NAME_INCOME_TYPE e transformando em numérico
applications = applications %>% mutate(INCOME_TYPE = case_when(NAME_INCOME_TYPE=="Working"|NAME_INCOME_TYPE=="Commercial associate"|NAME_INCOME_TYPE=="State servant" ~ "Working", NAME_INCOME_TYPE=="Pensioner" ~ "Pensioner", TRUE ~ "Estudent"))
applications$NAME_INCOME_TYPE = NULL 
dummies <- model.matrix(~ INCOME_TYPE - 1, data = applications)
applications <- cbind(applications, dummies)
applications$INCOME_TYPE = NULL


#Criando nova variárvel "Education" com base na NAME_EDUCATION_TYPE
applications = applications %>% mutate(EDUCATION = case_when(NAME_EDUCATION_TYPE=="Secondary / secondary special"|NAME_EDUCATION_TYPE=="Lower secondary" ~ "secondary", NAME_EDUCATION_TYPE=="Higher_education"|NAME_EDUCATION_TYPE=="Incomplete higher"~ "Higher_education" ,TRUE ~ "Academic_degree"))
applications$NAME_EDUCATION_TYPE = NULL
dummies <- model.matrix(~ EDUCATION - 1, data = applications)
applications <- cbind(applications, dummies)
applications$EDUCATION = NULL

#Criando nova coluna MARRIED com base em NAME_FAMILY_STATUS  e tranformando em numérico
applications = applications %>% mutate(MARRIED = case_when(NAME_FAMILY_STATUS=="Civil marriage"|NAME_FAMILY_STATUS=="Married" ~ 1, TRUE ~ 0))
applications$NAME_FAMILY_STATUS = NULL

#Criando nova coluna HOUSE_APARTMENT e tranformando em numérico
applications = applications %>% mutate(IS_HOUSE_OR_APARTMENT = case_when(NAME_HOUSING_TYPE=="With parents" ~ 0, TRUE ~ 1))
applications$NAME_HOUSING_TYPE = NULL


#Convertendo DAYS_EMPLOYED para anos
applications$Experience <- applications$DAYS_EMPLOYED/365
applications$Experience <- ifelse(applications$Experience < 0, as.integer(applications$Experience*-1), 0)
applications$DAYS_EMPLOYED = NULL

#Convertendo DAYS_BIRTH para anos de idade
applications$Age = round((applications$DAYS_BIRTH/365)*-1)
applications$DAYS_BIRTH = NULL

other_numerical_cols <- c("Income", "Age", "Experience", "Family_Member_Count")

#Criando os boxplots para identificar outliers
fig <- plot_ly() %>% 
  add_boxplot(
    x = ~applications$AMT_INCOME_TOTAL, 
    name = "Income",
    boxmean = TRUE
  ) %>% 
  add_boxplot(
    x = ~applications$Age, 
    name = "Age",
    boxmean = TRUE
  ) %>% 
  add_boxplot(
    x = ~applications$Experience, 
    name = "Experience",
    boxmean = TRUE
  ) %>% 
  add_boxplot(
    x = ~applications$CNT_CHILDREN, 
    name = "Family Member Count",
    boxmean = TRUE
  ) %>% 
  layout(
    title = "Boxplots of Numeric Columns",
    xaxis = list(title = ""),
    yaxis = list(title = ""),
    grid = list(rows = 2, columns = 2)
  )

fig


#Utilizando técnica do z-score para retirar os outliers
calculate_z_scores <- function(df, cols) {
  for (col in cols) {
    df[paste0(col, "_z_score")] <- (df[, col] - mean(df[, col])) / sd(df[, col])
  }
  return(df)
}

# Apply the function to your data frame
df_2 <- calculate_z_scores(df = applications, cols = c("AMT_INCOME_TOTAL","Experience","CNT_CHILDREN"))

# Remove outliers
filter_2 <- abs(df_2$CNT_CHILDREN_z_score) <= 3.5
filter_3 <- abs(df_2$Experience_z_score) <= 3.5
filter_4 <- abs(df_2$AMT_INCOME_TOTAL_z_score) <= 3.5

#retirando os outliers da base
applications <- df_2[filter_2 & filter_3 & filter_4, ]
applications <- applications[, !(names(applications) %in% c("AMT_INCOME_TOTAL_z_score", "Experience_z_score", "CNT_CHILDREN_z_score"))]

#visualizando novamente a base sem os outliers
fig <- plot_ly() %>% 
  add_boxplot(
    x = ~applications$AMT_INCOME_TOTAL, 
    name = "Income",
    boxmean = TRUE
  ) %>% 
  add_boxplot(
    x = ~applications$Age, 
    name = "Age",
    boxmean = TRUE
  ) %>% 
  add_boxplot(
    x = ~applications$Experience, 
    name = "Experience",
    boxmean = TRUE
  ) %>% 
  add_boxplot(
    x = ~applications$CNT_CHILDREN, 
    name = "Family Member Count",
    boxmean = TRUE
  ) %>% 
  layout(
    title = "Boxplots of Numeric Columns",
    xaxis = list(title = ""),
    yaxis = list(title = ""),
    grid = list(rows = 2, columns = 2)
  )

fig



#Criando a variável target "bad_payer"(Mal pagador)
behavior$bad_payer <- NA

#Todos com status entre 2 e 5 são considerados bad_payers
#0: 1-29 days past due 1: 30-59 days past due 2: 60-89 days overdue 3: 90-119 days overdue 4: 120-149 days overdue 5: Overdue or bad debts, write-offs for more than 150 days C: paid off that month X: No loan for the month
behavior$bad_payer[behavior$STATUS %in% c('2', '3', '4', '5')] <- 'Yes'

#Identificando os bons pagadores (Todos que não são mals pagadores)
grouped_base <- behavior %>%
  group_by(ID) %>%
  summarize(bad_payer_count = sum(!is.na(bad_payer))) %>%
  mutate(bad_payer = ifelse(bad_payer_count > 0, 'Yes', 'No')) %>%
  select(bad_payer,ID)


# merge customer_apps (variáveis de predição) com grouped_base(target)
customer_apps <- inner_join(applications, grouped_base, by = "ID")
# Transformando target em variável numérica
customer_apps$target <- ifelse(customer_apps$bad_payer == "Yes", 1, 0)

# remove bad_payer column
customer_apps$bad_payer <- NULL

#entendendo a distribuição entre bons e mals pagadores
table(customer_apps$target)

set.seed(1)

#excluindo ID do cliente da base - variável indesejada ao modelo
X <- customer_apps[, 2:(ncol(customer_apps))]

#"embaralhando" a base para melhor aleatoriedade entre amostra treino e teste
random_order <- sample(nrow(X ))
X <- X[random_order, ]

#normalização da base
max_data <- apply(X, 2, max) 
min_data <- apply(X, 2, min)
scaled <- data.frame(scale(X,center = min_data, scale = max_data - min_data))

#Criando índice para separação das amostras de treino e teste
index = sample(1:nrow(scaled),round(0.70*nrow(scaled)))

#Selecionando a base de treino (70% da base)
train_data <- as.data.frame(scaled[index,])

#balanceando a base de treino
balanced_data <- ovun.sample(target ~ ., data = train_data, method = "under", seed = 1)$data
train_data <- balanced_data
table(train_data$target)


#Selecionando a base de teste
test_data <- as.data.frame(scaled[-index,])

#################################################################################
# Criando os modelos
#################################################################################


###########################  Logístico Binário  ################################
#criando o modelo logístico binário
logistico_binario <- glm(formula = target~., 
                         data = train_data, 
                         family = "binomial")
#aplicando stepwise3r
logistico_binario <- step(object =  logistico_binario,
                          k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

##########################  Árvores de decisão #################################

#criando o modelo de árvore
tree_model <- rpart(target~., 
                    data=train_data,
                    control=rpart.control(maxdepth = 5, cp=0), method="class")
#visualizando a árvore
paleta = scales::viridis_pal(begin=.75, end=1)(20)
rpart.plot::rpart.plot(tree_model,
                       box.palette = paleta) # Paleta de cores

########################## Random Forest #######################################

#Criando o modelo "Random Forest""
rf_model <- randomForest(target ~ ., data = train_data, ntree = 500)

############################  XGBOOST  #########################################

#Criando o modelo XgBoost
xgb_model <- xgboost(target ~ ., data = as.matrix(train_data[, -ncol(train_data)]),label = train_data$target,  nrounds = 100, objective = "binary:logistic")



#################################################################################
#Realizando as predições na base de teste  
#################################################################################

log_pred <- predict(logistico_binario, newdata = test_data, type = "response")

tree_pred <- predict(tree_model, newdata = test_data, type = "prob")
tree_pred <- tree_pred[,2]

rf_pred <- predict(rf_model, newdata = test_data)

xgb_pred <- predict(xgb_model, newdata = as.matrix(test_data[, -ncol(test_data)]))



#################################################################################
#Curva ROC para cada modelo
#################################################################################

#Cálculo da curva ROC 

roc_logistico <- roc(response = test_data$target, 
                     predictor = log_pred)

roc_tree <- roc(response = test_data$target, 
                predictor = c(tree_pred))


roc_rf <- roc(response = test_data$target, 
              predictor = rf_pred)

roc_xg <- roc(response = test_data$target, 
              predictor = xgb_pred)


#Plotando curva ROC para cada modelo



ggplotly(
  ggroc(roc_logistico, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(roc_logistico$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((roc_logistico$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

ggplotly(
  ggroc(roc_tree, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(roc_tree$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((roc_tree$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)



ggplotly(
  ggroc(roc_rf, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(roc_rf$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((roc_rf$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)


ggplotly(
  ggroc(roc_xg, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(roc_xg$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((roc_xg$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

#########################################################################################
#Análise de sensibilidade/especificidade para definição do melhor cutoff para cada modelo
#########################################################################################

predicoesLog <- prediction(predictions = log_pred, 
                           labels = test_data$target) 

predicoesTree <- prediction(predictions = tree_pred, 
                            labels = test_data$target) 

predicoesRf <- prediction(predictions = rf_pred, 
                          labels = test_data$target) 

predicoesXgb <- prediction(predictions = xgb_pred, 
                           labels = test_data$target) 


dados_curva_roc_log <- performance(predicoesLog, measure = "sens") 
dados_curva_roc_tree <- performance(predicoesTree, measure = "sens") 
dados_curva_roc_rf <- performance(predicoesRf, measure = "sens") 
dados_curva_roc_xgb <- performance(predicoesXgb, measure = "sens") 



sensitividadeLog <- dados_curva_roc_log@y.values[[1]] 
sensitividadeTree <- dados_curva_roc_tree@y.values[[1]] 
sensitividadeRf <- dados_curva_roc_rf@y.values[[1]] 
sensitividadeXgb <- dados_curva_roc_xgb@y.values[[1]] 


especificidadeLog <- performance(predicoesLog, measure = "spec") 
especificidadeTree <- performance(predicoesTree, measure = "spec")
especificidadeRf <- performance(predicoesRf, measure = "spec")
especificidadeXgb <- performance(predicoesXgb, measure = "spec")

especificidadeLog <- especificidadeLog@y.values[[1]]
especificidadeTree <- especificidadeTree@y.values[[1]]
especificidadeRf <- especificidadeRf@y.values[[1]]
especificidadeXgb <- especificidadeXgb@y.values[[1]]

cutoffsLog <- dados_curva_roc_log@x.values[[1]] 
cutoffsTree <- dados_curva_roc_tree@x.values[[1]] 
cutoffsRf <- dados_curva_roc_rf@x.values[[1]] 
cutoffsXgb <- dados_curva_roc_xgb@x.values[[1]] 

dados_plotagemLog <- cbind.data.frame(cutoffsLog, especificidadeLog, sensitividadeLog)
dados_plotagemTree <- cbind.data.frame(cutoffsTree, especificidadeTree, sensitividadeTree)
dados_plotagemRf <- cbind.data.frame(cutoffsRf, especificidadeRf, sensitividadeRf)
dados_plotagemXgb <- cbind.data.frame(cutoffsXgb, especificidadeXgb, sensitividadeXgb)

ggplotly(dados_plotagemLog %>%
           ggplot(aes(x = cutoffsLog, y = especificidadeLog)) +
           geom_line(aes(color = "Especificidade"),
                     size = 1) +
           geom_point(color = "#95D840FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffsLog, y = sensitividadeLog, color = "Sensitividade"),
                     size = 1) +
           geom_point(aes(x = cutoffsLog, y = sensitividadeLog),
                      color = "#440154FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())


ggplotly(dados_plotagemTree %>%
           ggplot(aes(x = cutoffsTree, y = especificidadeTree)) +
           geom_line(aes(color = "Especificidade"),
                     size = 1) +
           geom_point(color = "#95D840FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffsTree, y = sensitividadeTree, color = "Sensitividade"),
                     size = 1) +
           geom_point(aes(x = cutoffsTree, y = sensitividadeTree),
                      color = "#440154FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())


ggplotly(dados_plotagemRf %>% 
           ggplot(aes(x = cutoffsRf, y = especificidadeRf)) +
           geom_line(aes(color = "Especificidade"),
                     size = 1) +
           geom_point(color = "#95D840FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffsRf, y = sensitividadeRf, color = "Sensitividade"),
                     size = 1) +
           geom_point(aes(x = cutoffsRf, y = sensitividadeRf),
                      color = "#440154FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())


ggplotly(dados_plotagemXgb %>%
           ggplot(aes(x = cutoffsXgb, y = especificidadeXgb)) +
           geom_line(aes(color = "Especificidade"),
                     size = 1) +
           geom_point(color = "#95D840FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffsXgb, y = sensitividadeXgb, color = "Sensitividade"),
                     size = 1) +
           geom_point(aes(x = cutoffsXgb, y = sensitividadeXgb),
                      color = "#440154FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())


#################################################################################
#Criando a matriz confusão para cada modelo
#################################################################################


#Calculando predições com base no cutoff elencado nas análises logo acima
log_pred_threshold <- ifelse(log_pred >= 0.5, 1, 0)
tree_pred_threshold <- ifelse(tree_pred >= 0.6, 1, 0)
rf_pred_threshold <- ifelse(rf_pred >= 0.5, 1, 0)
xgb_pred_threshold <- ifelse(xgb_pred >= 0.5, 1, 0)


#Calculando as matrizes de confusão
matrizConfusaoLogisticoBinario <- confusionMatrix(data=factor(c(log_pred_threshold)), reference = factor(c(test_data$target)))

matrizConfusaoTree <- confusionMatrix(data=factor(c(tree_pred_threshold)), reference = factor(c(test_data$target)))

matrizConfusaoRandomForest <- confusionMatrix(data=factor(c(rf_pred_threshold)), reference = factor(c(test_data$target)))

matrizConfusaoXgb <- confusionMatrix(data=factor(c(xgb_pred_threshold)), reference = factor(c(test_data$target)))

#Visualizando as informações da matriz de confusão
matrizConfusaoLogisticoBinario

matrizConfusaoTree

matrizConfusaoRandomForest

matrizConfusaoXgb
0.73
0.47
#Cálculo do F1-Score

f1_score_from_cm <- function(cm) {
  VP <- cm$table[1, 1]
  FP <- cm$table[2, 1]
  FN <- cm$table[1, 2]
  
  precision <- VP / (VP + FP)
  sensitivity <- VP / (VP + FN)

  f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)
  return(f1_score)
}

#F1-Score do modelo de regressão logísitica binária
f1_score_from_cm(cm = matrizConfusaoLogisticoBinario) 

#F1-score do modelo de árvores de decisão
f1_score_from_cm(cm = matrizConfusaoTree)

#F1-score do modelo Random Forest
f1_score_from_cm(cm = matrizConfusaoRandomForest) 

#F1-score do modelo XGBoost
f1_score_from_cm(cm = matrizConfusaoXgb) 


