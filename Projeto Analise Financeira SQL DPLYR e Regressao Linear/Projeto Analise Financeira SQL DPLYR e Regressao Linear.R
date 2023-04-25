setwd("D:/Envios git/Projetos R/Projeto Analise Financeira SQL DPLYR e Regressao linear")
getwd()

#O  dataset foi  gerado  a  partir  das fontes:
# https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Cost-Report/HospitalCostPUF
# https://healthdata.gov/Data 


# Atributo Descrição
# AGE Idade do paciente
# FEMALE Variável binária que indica se o paciente é do sexo feminino
# LOS(Length of stay) Tempo da internação do paciente
# RACE Raça do paciente
# TOTCHG Custo da internação
# APRDRG Grupo de diagnóstico refinado do paciente


# Abaixo as perguntas que devem ser respondidas na análise exploratória de dados com Linguagem SQL:
#   1-Quantas raças estão representadas no dataset?
#   2-Qual a idade média dos pacientes?
#   3-Qual a moda da idade dos pacientes?
#   4-Qual a variância da coluna idade?
#   5-Qual o gasto total com internações hospitalares por idade?
#   6-Qual idade gera o maior gasto total com internações hospitalares?
#   7-Qual o gasto total com internações hospitalares por gênero?
#   8-Qual a média de gasto com internações hospitalares por raça do paciente?
#   9-Para  pacientes  acima  de  10  anos,  qual  a  média  de  gasto  total  com  internações hospitalares?
#   10-Considerando o item anterior, qual idade tem média de gastos superior a 3000?

# Pacotes
library(dplyr)
library(sqldf)


# Carrega o dataset
dados <- read.csv('dataset.csv')

#Visualiza informações do dataset
View(dados)

str(dados)

summary(dados)

sum(is.na(dados))



#verificar o dado com NA
dados[!complete.cases(dados),]
sum(complete.cases(dados))


#remover o dado com infomação NA
dados_completos <- na.omit(dados)


#Transforma genero em factor
dados_completos$FEMALE <- factor(x = dados_completos$FEMALE,levels = c(0,1),
                                    labels = c("Male", "Female"))

#altera o nome da coluna FEMALE para Sex
colnames(dados_completos)[2] <- "SEX"

View(dados_completos)

#   Questão 1-Quantas raças estão representadas no dataset?

questao_1 <- sqldf("select count(DISTINCT RACE) as contagem from dados_completos")
questao_1

questao_1_dplyr <- dados_completos %>%
  summarise(total = length(unique(RACE)))
questao_1_dplyr

#remove da memoria
rm(questao_1, questao_1_dplyr)

# Questão 2-Qual a idade média dos pacientes?
questao_2 <- sqldf("select avg(AGE) from dados_completos")
questao_2

questao_2_dplyr <- dados_completos %>%
  summarise(media = mean(AGE))
questao_2_dplyr

rm(questao_2, questao_2_dplyr)



#  Questão 3-Qual a moda da idade dos pacientes?

questao_3 <- sqldf("select AGE from dados_completos group by AGE order by count(*) desc LIMIT 1")
questao_3

moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

questao_3_R <- moda(dados_completos$AGE)
questao_3_R
rm(questao_3,questao_3_R, moda)


# Questão 4-Qual a variância da coluna idade?
questao_4 <- sqldf("select  variance(AGE) from dados_completos")
questao_4

questao_4_R <- var(dados_completos$AGE)
questao_4_R

rm(questao_4,questao_4_R)


# Questão 5-Qual o gasto total com internações hospitalares por idade?
questao_5 <- sqldf("select  AGE, sum(TOTCHG)  from dados_completos group by AGE ")
questao_5

questao_5_dplyr <-dados_completos %>%
  group_by(AGE) %>%
  summarise(Gasto_total = sum(TOTCHG))
questao_5_dplyr

rm(questao_5, questao_5_dplyr)

# Questão 6-Qual idade gera o maior gasto total com internações hospitalares?
questao_6 <- sqldf("select  AGE, sum(TOTCHG)  from dados_completos group by AGE order by count(*) desc LIMIT 1")
questao_6

questao_6_dplyr <- dados_completos %>%
  group_by(AGE) %>%
  summarise(Gasto_total = sum(TOTCHG))%>%
  filter(Gasto_total == max(Gasto_total))
questao_6_dplyr

rm(questao_6, questao_6_dplyr)

# Questão 7-Qual o gasto total com internações hospitalares por gênero?
questao_7 <- sqldf("select  SEX, sum(TOTCHG)  from dados_completos group by SEX")
questao_7

questao_7_dplyr <- dados_completos %>%
  group_by(SEX) %>%
  summarise(Gasto_total = sum(TOTCHG))
questao_7_dplyr

rm(questao_7,questao_7_dplyr)

# Questão 8-Qual a média de gasto com internações hospitalares por raça do paciente?
questao_8 <- sqldf("select  RACE, avg(TOTCHG) as MEAN  from dados_completos group by RACE")
questao_8

questao_8_dplyr <- dados_completos %>%
  group_by(RACE) %>%
  summarise(Media_gasto = mean(TOTCHG))
questao_8_dplyr

rm(questao_8, questao_8_dplyr)

# Questão 9-Para  pacientes  acima  de  10  anos,  qual  a  média  de  gasto  total  com  internações hospitalares?
questao_9 <- sqldf("select  avg(TOTCHG) as MEAN  from dados_completos where AGE >10")
questao_9

questao_9_dplyr <- dados_completos %>%
  filter(AGE>10)%>%
  summarise(Media_gasto = mean(TOTCHG))
questao_9_dplyr

rm(questao_9, questao_9_dplyr)

# Questão 10-Considerando o item anterior, qual idade tem média de gastos superior a 3000?
questao_10 <- sqldf("select  AGE, avg(TOTCHG) as MEAN  from dados_completos where AGE >10 group by AGE having MEAN > 3000")
questao_10

questao_10_dplyr <- dados_completos %>%
  select(TOTCHG, AGE)%>%
  group_by(AGE)%>%
  summarise(Media_gasto = mean(TOTCHG))%>%
  filter(AGE>10 & Media_gasto>3000)
  
questao_10_dplyr

rm(questao_10, questao_10_dplyr)


#Análise de variáveis
# 1-Qual a distribuição da idade dos pacientes que frequentam o hospital?
# 2-Qual faixa etária tem o maior gasto total no hospital?
# 3-Qual grupo baseado em diagnóstico (Aprdrg) tem o maior gasto total no hospital?
# 4-A raça do paciente tem relação com o total gasto em internações no hospital?
# 5-A combinação de idade e gênero dos pacientes influencia no gasto total em internações no hospital?
# 6-Como o tempo de permanência é o fator crucial para pacientes internados, desejamos descobrir se o tempo de permanência pode ser previsto a partir de idade, gênero e raça.
# 7-Quais variáveis têm maior impacto nos custos de internação hospitalar?


#Questão 1-Qual a distribuição da idade dos pacientes que frequentam o hospital?
hist(dados_completos$AGE)

hist(dados_completos$AGE, 
     main="Distribuição idades",
     xlab = "Idade",
     border = "Black",
     col = c("light green", "dark green"),
     xlim = c(0,20),
     ylim=c(0,350)
     )
summary(dados_completos$AGE)

table(dados_completos$AGE)

#A maioria são crianças com menos de 1 ano

#Questão 2-Qual faixa etária tem o maior gasto total no hospital?
idade_gasto <- dados_completos %>%
  select(TOTCHG, AGE)%>%
  group_by(AGE)%>%
  summarise(Soma_gasto = sum(TOTCHG))%>%
  arrange(desc(Soma_gasto))

barplot(tapply(idade_gasto$Soma_gasto, 
               idade_gasto$AGE, 
               FUN = sum))
rm(idade_gasto)




#Crianças com menos de 1 ano


#Questão 3-Qual grupo baseado em diagnóstico (Aprdrg) tem o maior gasto total no hospital?
grupo_gasto <- dados_completos %>%
  select(TOTCHG, APRDRG)%>%
  group_by(APRDRG)%>%
  summarise(Soma_gasto = sum(TOTCHG))%>%
  arrange(desc(Soma_gasto))


View(grupo_gasto)
barplot(tapply(grupo_gasto$Soma_gasto, 
               grupo_gasto$APRDRG, 
               FUN = sum))

grupo_gasto[which.max(grupo_gasto$Soma_gasto),]


rm(grupo_gasto)
#O grupo 640



#Questão 4-A raça do paciente tem relação com o total gasto em internações no hospital?
cor.test(dados_completos$RACE, dados_completos$TOTCHG,method="pearson")


#Não há relação entre as variável Race e Totchg


#Questão 5-A combinação de idade e gênero dos pacientes influencia no gasto total em internações no hospital?

#Teste ANOVA
teste_anova <-  aov(TOTCHG ~ AGE+SEX, data = dados_completos)
teste_anova
summary(teste_anova)
rm(teste_anova)

#Valor p abaixo de 0.05, isso indica que AGE e SEX impactam em TOTCHG

# 6-Como o tempo de permanência é o fator crucial para pacientes internados, desejamos 
#descobrir se o tempo de permanência pode ser previsto a partir de idade, gênero e raça.
modelo <- lm(LOS ~ AGE+SEX+RACE, data = dados_completos)
summary(modelo)

teste_anova <-  aov(LOS ~ AGE+SEX+RACE, data = dados_completos)
summary(teste_anova)
rm(teste_anova, modelo)

#Valor p maior que 0.05, entãoas variáveis possuem correlação, ou seja, 
#podemos prever LOS a partir de  AGE, SEX e RACE 

# 7-Quais variáveis têm maior impacto nos custos de internação hospitalar?
teste_anova <-  aov(TOTCHG ~ ., data = dados_completos)
summary(teste_anova)

#pelo teste ANOVA, são as variáveisLOS e APRDRG. A variável AGE 
#também apresentou significancia

modelo <- lm(TOTCHG ~ ., data = dados_completos)
summary(modelo)
#pelo modelo de regressão, são as variáveis LOS, APRDRG e AGE

#Testando as melhores variáveis para verificar se impacta na melhoria do modelo
modelo <- lm(TOTCHG ~ AGE+SEX+LOS+APRDRG, data = dados_completos)
summary(modelo)

modelo <- lm(TOTCHG ~ AGE+LOS+APRDRG, data = dados_completos)
summary(modelo)

#Sem impacto na melhoria

rm(modelo, teste_anova, dados, dados_completos)
