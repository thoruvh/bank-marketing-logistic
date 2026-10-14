library(readr)
library(openxlsx) #biblioteca para escrever arquivo em excel
library(haven)
library(readxl)
library(tidyverse)
library(yardstick) #biblioteca para calcular medidas de erro
library(car) # calcula vif
library(ggraph)
library(ggstance)
library(fastDummies)
library(lazyeval) # para a analise de interacao
library(rcompanion) # cramersV
library(caret)
library(pROC)
library(patchwork)


bank_base <- read_delim("bank.csv",
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

##A base possui diversas colunas categóricas que descrevem as características dos clientes, em nível de perfil
## de idade, sexo, educação, etc. Em um primeiro momento é interessante começar a entender como se comportam essa variáveis
## Vou iniciar fazendo algumas analises descritivas básicas para entender as categorias presentes em cada variável e sua frequência
## Até mesmo antes de entrar na visualização de cada nível das variáveis, vou entender se existem valores faltantes em alguma coluna previamente
colMeans(is.na(bank_base)) * 100

##Boas notícias, nenhuma coluna apresenta valores faltantes, agora me sinto mais seguro em inciar a avaliar cada uma das variáveis uma por vez

#####Descritivas Univariadas#####

##Começando com idade

summary(bank_base$age)

ggplot(bank_base, aes(x = age)) +
  geom_histogram()

ggplot(bank_base, aes(x = age)) + 
  geom_boxplot(color="black", fill="blue")

##Ao que tudo indica o banco tem uma concentração de clientes na faixa etária predominantemente entre os 30 a 50 anos
## os clientes mais velhos já estão concentrados como outliers

##Emprego

ggplot(bank_base, aes(x = job, fill = job)) +
  geom_bar()

##O perfil de clientes mais frequentes estão concetrados entre os clientes que possuem ocupações como gerentes, blue collar ou técnicos
## essas 3 categorias são os mais frequentes entre a ocupação dos clientes que somam quase 60% de todos os tipo de ocupações

##Estado civil

ggplot(bank_base, aes(x = marital, fill = marital)) +
  geom_bar()

##Mais de 61% dos clientes é casado, seguido por clientes solteiros com 26% de representatividades

##Educação

ggplot(bank_base, aes(x = education, fill = education)) +
  geom_bar()

##Mais de 50% dos clientes tem o nível de educação até o segundo grau de ensino, seguidos por 30% que possuem grau de instrução terciário
##cerca de 15% tem apenas o grau primário e apenas 4% não é conhecido o grau de instrução

##Default

ggplot(bank_base, aes(x = default, fill = default)) +
  geom_bar()

##Quando avaliamos o crédito com default, quase 99% dos clientes não possuem crédito em default, que indica um bom perfil de pagamentos entre os clientes ativos

##Tipo de Crédito: HABITAÇÃO

ggplot(bank_base, aes(x = housing, fill = housing)) +
  geom_bar()

##Mais de 56% dos clientes possui crédito habitacional

#Tipo de crédito : Pessoal

ggplot(bank_base, aes(x = loan, fill = loan)) +
  geom_bar()

##Quando vemos o empréstimo pessoal, mais de 84% dos clientes não possui esse tipo de crédito, o que nos leva a entender que os clientes que possue, crédito habitacional não possuem crédito pessoal

##Contato


ggplot(bank_base, aes(x = contact, fill = contact)) +
  geom_bar()

##O contato para as campanhas foram realizado majoritariamente via celular pessoal do cliente, 64% delas

##Mês de contato

ggplot(bank_base, aes(x = month, fill = month)) +
  geom_bar()

##Maio tem o maior pico de campanhas realizadas, 30% de todas as ultimos contatos feitos para os clientes ocorreu em Maio

##Quantidade de contatos

summary(bank_base$campaign)

ggplot(bank_base, aes(x = campaign)) +
  geom_histogram()

ggplot(bank_base, aes(x = campaign)) + 
  geom_boxplot(color="black", fill="blue")

##Olhando a quantidade de contatos realizados com os clientes me deixa um pouco intrigado ver clientes com uma quantidade tão alta de
## contatos, em casos únicos superando 20, até 40 contatos realizados, me pergunto se isso não pode ser um erro de inputação da base, uma
## vez que a quantidade de contatos se concentra mais fortemente em até 10 contatos realizados.

##Duração da chamada
summary(bank_base$duration)

ggplot(bank_base, aes(x = duration)) +
  geom_histogram()

ggplot(bank_base, aes(x = duration)) + 
  geom_boxplot(color="black", fill="blue")

## Variável de Interesse, Subscrição de Depósito a Prazo

ggplot(bank_base, aes(x = y, fill = y)) +
  geom_bar()

##Variável Balance

summary(bank_base$balance)

ggplot(bank_base, aes(x = balance)) +
  geom_histogram()

ggplot(bank_base, aes(x = balance)) + 
  geom_boxplot(color="black", fill="blue")

##

##Avaliando a própria variável de interesse, ou seja, o evento em que o cliente realizou ou não a subscrição de um depósito a prazo, vemos que 
## a maioria dos clientes não realizou esse evento, mais de 88% dos clientes não realizou a subscrição, ou seja, a base em si já se encontra com 
##um desbalanceamento forte entre aqueles que realizam ou não a subscrição.

#####Análise Bivariada####

##Será que tem distinção na frequencia entre os contatos e o sucesso de fazer o evento y 
## Avaliando y em relação a quantidade de contatos

ggplot(bank_base, aes(x = y, y = campaign)) + 
  geom_boxplot(color="red", fill="orange", alpha=0.2)+
  labs(x = "Subscription", y = "Campain")

wilcox.test(bank_base$campaign[bank_base$y=="yes"],bank_base$campaign[bank_base$y=="no"],paired = FALSE)

df_campaign <- bank_base %>%
  select(y, campaign) %>%
  arrange(campaign) %>%
  mutate(percentil_campaign = ntile(campaign, 30))


df_campaign <- df_campaign %>%
  mutate(y = if_else(y == "yes", 1, 0))

campaign_sum <- df_campaign %>%
  group_by(percentil_campaign) %>%
  summarize(campaign = min(campaign), n= n(), y = mean(y),
            logito_p = log(y / (1 - y)) ) %>%
  ungroup()
campaign_sum

ggplot(campaign_sum, aes(x = percentil_campaign, y = y)) +
  geom_point() +
  labs(x = "Percentil da Campanha", y = "Subscrição")

##Campaing pode apresentar uma possibilidade de ser uma variável a ser adicionada como numérica

##Será que existe alguma relação possível entre o saldo do cliente e ele ter feito ou não o evento y
##Avaliando Y em relação ao saldo do cliente

ggplot(bank_base, aes(x = y, y = balance)) + 
  geom_boxplot(color="red", fill="orange", alpha=0.2)+
  labs(x = "Subscription", y = "Balance")

## Não observamos nenhum tipo de relação expressiva entre o evento de intresse e o saldo do cliente
#H0: MEDIANA YES - MEDIANA NO = 0
#H1: MEDIANA YES - MEDIANA NO <> 0

wilcox.test(bank_base$balance[bank_base$y=="yes"],bank_base$balance[bank_base$y=="no"],paired = FALSE)

##Através do teste, temos evidência o suficiente para afirmar que os saldos bancários entre clientes que realizam o evento de interesse y é diferente dos clientes que não realizam
##Nesse caso, vale a pena entendermos se é possível existir uma relação linear entre os saldos bancários e o logito de p para o evento y

df_balance <- bank_base %>%
  select(y, balance) %>%
  arrange(balance) %>%
  mutate(percentil_balance = ntile(balance, 30))

df_balance <- df_balance %>%
  mutate(y = if_else(y == "yes", 1, 0))

balance_sum <- df_balance %>%
  group_by(percentil_balance) %>%
  summarize(balance = min(balance), n= n(), y = mean(y),
            logito_p = log(y / (1 - y)) ) %>%
  ungroup()
balance_sum

ggplot(balance_sum, aes(x = percentil_balance, y = y)) +
  geom_point() +
  labs(x = "Percentil do Saldo", y = "Subscrição")

##Será que a idade pode ser um fator condicionante para o cliente realizar y 
##Avaliando Y em relação a idade dos clientes

ggplot(bank_base, aes(x = y, y = age)) + 
  geom_boxplot(color="red", fill="orange", alpha=0.2)+
  labs(x = "Subscription", y = "age")

##Por desencargo, sabendo que a distribuição das idades não é normal, vou realizar um teste não paramétrico de comparação entre médias
#H0: MEDIANA YES - MEDIANA NO = 0
#H1: MEDIANA YES - MEDIANA NO <> 0

wilcox.test(bank_base$age[bank_base$y=="yes"],bank_base$age[bank_base$y=="no"],paired = FALSE)

## O teste evidencia que a mediana da idade para cada categoria de resposta é igual a entre elas, logo a idade também não parece ser um fator relacionado com a subscrição

##Será que a duração do contato possui relação com o evento Y 
##Avaliando Y em relação ao tempo em segundos de cada contato

ggplot(bank_base, aes(x = y, y = duration)) + 
  geom_boxplot(color="red", fill="orange", alpha=0.2)+
  labs(x = "Subscription", y = "Duration")

##BINGO! A duração do contato em minutos pode ter uma relação com o evento Y, apenas para constatar a diferença entre as medianas vou aplicar o teste novamente

wilcox.test(bank_base$duration[bank_base$y=="yes"],bank_base$duration[bank_base$y=="no"],paired = FALSE)

##OK, já observei que as medianas entre os clientes que realizaram y e não realizaram y são efetivamengte diferentes, como a duração é uma variável numérica, vale a pena
##testar se possui relação linear com o logito de y

##Subdividindo a base com base em cada tipo de duração

df_duration <- bank_base %>%
  select(y, duration) %>%
  arrange(duration) %>%
  mutate(percentil_duration = ntile(duration, 30)) 

df_duration <- df_duration %>%
  mutate(y = if_else(y == "yes", 1, 0))

duration_sum <- df_duration %>%
  group_by(percentil_duration) %>%
  summarize(duration = min(duration), n= n(), y = mean(y),
            logito_p = log(y / (1 - y)) ) %>%
  ungroup()
duration_sum

ggplot(duration_sum, aes(x = percentil_duration, y = y)) +
  geom_point() +
  labs(x = "Percentil da Duração", y = "Subscrição")

##Aparentemente quando observamos o comportamento do logito de P associado a frequencia da subscrição e o tempo da duração da chamada
##observamos que existe um comportamento linear nesse interação, ou seja, o tempo de duração da chamada pode ser inserido no modelo na forma
##numérica contínua uma vez que apresenta relação linear com o logito 

##Para examinar a possível relação entre as variáveis categóricas e o evento Y, vou utilizar o teste de associação V de Kramer

# --- Função para Calcular Associação (Chi-quadrado e Cramer's V) ---
analisar_associacao <- function(df, var_target, vars_categoricas) {
  
  map_df(vars_categoricas, function(var) {
    tab <- table(df[[var]], df[[var_target]])
    chi2 <- chisq.test(tab)
    v_cramer <- rcompanion::cramerV(tab)
    
    data.frame(
      Variavel = var,
      Chi2_p_valor = round(chi2$p.value, 4),
      Cramer_V = round(v_cramer, 4)
    )
  }) %>%
    arrange(desc(Cramer_V))
}

# --- Executando para as principais variáveis ---
vars_estudo <- c("education", "poutcome", "marital", "job", "contact", "month", "loan", "housing")
ranking_associacao <- analisar_associacao(bank_base, "y", vars_estudo)

print(ranking_associacao)

#####Montando a Base para estimação do Modelo####

##Deixando apenas as colunas que se tornarão dummies

Bank_dummies <- bank_base %>%
  select(-'pdays',-'previous')

####Agrupando variáveis Numéricas#### 

##Variável idade pode ser agrupada em 4 categorias <30, 30-40, 40-50,50> e depois verificar a frequência para saber como ficou a distribuição

##IDADE

Bank_dummies <- Bank_dummies %>%
  mutate(age_range = cut(`age`, 
                          breaks = c(0,35,55,90), 
                          right = FALSE, 
                          labels = c("<35","35-55",">55")))

ggplot(Bank_dummies, aes(x = age_range, fill = age_range)) +
  geom_bar()

##BALANCE

Bank_dummies <- Bank_dummies %>%
  mutate(balance = if_else(balance >= 444, ">444", "<444"))

ggplot(Bank_dummies, aes(x = balance, fill = balance)) +
  geom_bar()

##DIA DO MÊS

Bank_dummies <- Bank_dummies %>%
  mutate(day_range = cut(`day`, 
                         breaks = c(0,20, 32), 
                         right = FALSE, 
                         labels = c("1-20", "20-31")))

ggplot(Bank_dummies, aes(x = day_range, fill = day_range)) +
  geom_bar()

#####Agrupando Variáveis Categóricas####

##jobs - blue collar, management, technician, outros

Bank_dummies <- Bank_dummies %>%
  mutate(job = if_else(job %in% c("management", "blue-collar", "technician"), 
                       job, 
                       "others"))

ggplot(Bank_dummies, aes(x = job, fill = job)) +
  geom_bar()

##month - dividir em quarters 

Bank_dummies <- Bank_dummies %>%
  mutate(
    month = toupper(month),
    semester = case_when(
      month %in% c("JAN", "FEV", "FEB", "MAR","APR","ABR", "MAY", "MAI", "JUN") ~ "S1",
      month %in% c("JUL", "AUG", "AGO", "SEP", "SET","OCT", "OUT", "NOV", "DEC", "DEZ") ~ "S2",
      TRUE                                               ~ "Unknown"
    )
  )

ggplot(Bank_dummies, aes(x = semester, fill = semester)) +
  geom_bar()

##Retirando colunas não necessárias para a base final

Bank_dummies <- Bank_dummies %>%
  select(-'age',-'month')

##Criando todas as dummies necessárias

Bank_dummies <- dummy_columns(.data = Bank_dummies,
                              select_columns = c("job","marital","balance","education","contact","semester","age_range",
                                                 "default","housing","loan",'day_range','poutcome'),
                              remove_selected_columns = F,
                              remove_most_frequent_dummy = T)

Bank_dummies <- Bank_dummies %>%
  select(-'job',-'marital',-'education',-'contact',-'semester',-'age_range',-'default',-'housing',-'loan',-'balance',-'day',-'day_range',-'poutcome')


Bank_dummies <- Bank_dummies %>%
  mutate(y = if_else(y == "yes", 1, 0))

#####Estimando Modelos####

model_teste_1 <- glm(y ~ ., family = "binomial", data=Bank_dummies)

summary(model_teste_1)

##Selecionando as variáveis através do critério stepwise AIC

model_step <- step(model_teste_1, direction = "both")
summary(model_step)

##Selecionando variáveis através do critério stepwise p-valor

model_pv <- step(model_teste_1, k = 3.841459)
summary(model_pv)


vif(model_step)
vif(model_pv)

#####Métricas de Ajuste Para Objetivo Estimação####

#*** Metricas_AIC
bank_metricas <- Bank_dummies
#*** Coluna 'y_chapeu' sao as previsões do modelo
bank_metricas$y_chapeu <- predict(model_step, newdata = bank_metricas, type = "response")
bank_metricas <- bank_metricas %>%
  select(y, y_chapeu)

#*** Metricas_PV
bank_metricas_pv <- Bank_dummies
#*** Coluna 'y_chapeu' sao as previsões do modelo
bank_metricas_pv$y_chapeu <- predict(model_pv, newdata = bank_metricas_pv, type = "response")
bank_metricas_pv <- bank_metricas_pv %>%
  
  select(y, y_chapeu)

# Uma forma muito mais elegante de calcular e comparar:
resumo_erros <- list(
  Stepwise_AIC = bank_metricas,
  Stepwise_PV  = bank_metricas_pv
) %>%
  map_df(~{
    mse <- mean((.x$y - .x$y_chapeu)^2)
    data.frame(MSE = mse, RMSE = sqrt(mse))
  }, .id = "Modelo")

print(resumo_erros)

#####Métricas de Ajuste para objetivo Decisão######

# --- Função para Avaliação de Modelos com Corte Customizável ---
calcular_metricas_credito <- function(modelo, base, nome_modelo = "Modelo", corte = 0.5) {
  
  # 1. Predições de probabilidade
  probabilidades <- predict(modelo, newdata = base, type = "response")
  
  # 2. Decisão baseada no corte escolhido
  predicoes_binarias <- ifelse(probabilidades >= corte, 1, 0)
  
  # 3. Matriz de Confusão
  conf_matrix <- table(Real = base$y, Predito = factor(predicoes_binarias, levels = c(0, 1)))
  
  # 4. Cálculos de Performance
  precision <- conf_matrix[2,2] / sum(conf_matrix[,2])
  recall    <- conf_matrix[2,2] / sum(conf_matrix[2,])
  accuracy  <- sum(diag(conf_matrix)) / sum(conf_matrix)
  
  # 5. AUC-ROC (é a mesma independente do corte, pois avalia o ordenamento)
  roc_obj <- pROC::roc(base$y, probabilidades, quiet = TRUE)
  auc_val <- as.numeric(pROC::auc(roc_obj))
  
  # 6. Resultado organizado
  df_metricas <- data.frame(
    Modelo = nome_modelo,
    Corte = corte,
    AUC = round(auc_val, 4),
    Accuracy = round(accuracy, 4),
    Precision = round(precision, 4),
    Recall = round(recall, 4)
  )
  
  return(df_metricas)
}

# --- Gerando a Tabela Comparativa com Múltiplos Cortes ---

# Lista de cortes que queremos avaliar
cortes <- c(0.4, 0.5, 0.6)

# Criando a tabela para o Modelo Stepwise AIC
tabela_step <- map_df(cortes, ~calcular_metricas_credito(model_step, Bank_dummies, "Stepwise AIC", .x))

# Criando a tabela para o Modelo Stepwise P-Valor
tabela_pv <- map_df(cortes, ~calcular_metricas_credito(model_pv, Bank_dummies, "Stepwise P-Valor", .x))

# Consolidação Final
tabela_final <- rbind(tabela_step, tabela_pv)

# Exibindo o resultado ordenado por Modelo e Corte
tabela_final %>% arrange(Modelo, Corte)


#####Métricas de Ajuste para Objetivo Ordenação####


gerar_dados_lift <- function(df, nome_modelo) {
  # Média global de conversão (Baseline)
  media_global <- mean(as.numeric(as.character(df$y)))
  
  df %>%
    arrange(desc(y_chapeu)) %>%
    mutate(decil = ntile(row_number(), 10)) %>%
    group_by(decil) %>%
    summarise(
      taxa_resposta = mean(as.numeric(as.character(y))),
      .groups = 'drop'
    ) %>%
    mutate(
      Lift = taxa_resposta / media_global,
      Modelo = nome_modelo,
      decil = as.factor(decil * 10)
    )
}

# Gerando os dados para ambos
lift_aic <- gerar_dados_lift(bank_metricas, "Stepwise AIC")
lift_pv  <- gerar_dados_lift(bank_metricas_pv, "Stepwise P-Valor")

# Unindo as tabelas
df_lift_final <- bind_rows(lift_aic, lift_pv)

# Criando o gráfico comparativo (Facets)
ggplot(df_lift_final, aes(x = decil, y = Lift, fill = Modelo)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_text(aes(label = round(Lift, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Stepwise AIC" = "steelblue", "Stepwise P-Valor" = "orange")) +
  labs(
    title = "Comparativo de Lift por Decil",
    subtitle = "Linha vermelha representa o baseline (aleatório)",
    x = "Percentil da Base (Ordenados por Score)",
    y = "Alavancagem (Lift)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Criar um objeto ROC

roc_curve <- roc(bank_metricas$y, bank_metricas$y_chapeu)
area_curva_roc <- auc(roc_curve)


roc_curve_pv <- roc(bank_metricas_pv$y, bank_metricas_pv$y_chapeu)
area_curva_roc_pv <- auc(roc_curve_pv)


# 1. Plotar a primeira curva (Modelo Full)
plot(roc_curve, 
     main = "Comparativo de Modelos: Curva ROC", 
     col = "red", 
     lwd = 2)

# 2. Sobrepor a segunda curva (Modelo Reduzido)
plot(roc_curve_pv, 
     add = TRUE,        # O pulo do gato está aqui!
     col = "blue", 
     lwd = 2)

# 4. Criar uma legenda única para facilitar a leitura
legend("bottomright", 
       legend = c(paste("Step AIC (AUC =", round(area_curva_roc, 3), ")"),
                  paste("Step P-v (AUC =", round(area_curva_roc_pv, 3), ")")), 
       col = c("red", "blue"), 
       lty = 1, 
       lwd = 2,
       cex = 0.8)


##Calculando o Máximo KS

calcular_ks <- function(df, label_col = "y", prob_col = "y_chapeu", grupos = 10) {
  
  ks_table <- df %>%
    # Usamos sym() e !! para permitir que os nomes das colunas sejam flexíveis
    mutate(faixa = ntile(!!sym(prob_col), grupos)) %>% 
    group_by(faixa) %>%
    summarise(
      total = n(),
      bons = sum(!!sym(label_col) == 0),
      maus = sum(!!sym(label_col) == 1),
      .groups = "drop"
    ) %>%
    mutate(
      perc_bons_acum = cumsum(bons) / sum(bons),
      perc_maus_acum = cumsum(maus) / sum(maus),
      distancia = abs(perc_bons_acum - perc_maus_acum)
    )
  
  # Retorna o valor máximo do KS
  return(max(ks_table$distancia))
}

# Para a base original
ks_full <- calcular_ks(bank_metricas)

# Para a base PV
ks_pv <- calcular_ks(bank_metricas_pv)

# Exibindo os resultados
print(paste("KS Modelo AIC:", round(ks_full, 4)))
print(paste("KS Modelo PV:", round(ks_pv, 4)))
