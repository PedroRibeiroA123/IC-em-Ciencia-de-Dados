{
#install.packages("wrapr")
#install.packages("WVPlots")
library(ggplot2)
library(wrapr)
library(WVPlots)
}

#carrega dados
load("NatalRiskData.rData")
train <- sdata[sdata$ORIGRANDGROUP <= 5 , ]
test <- sdata[sdata$ORIGRANDGROUP > 5, ]

#vetores para a construção da função
complications <- c("ULD_MECO",
                   #"ULD_PRECIP",
                   "ULD_BREECH"
                   )
#riskfactors <- c("URF_DIAB","URF_CHYPER", "URF_PHYPER","URF_ECLAM")
y <- "atRisk"
x <- c("PWGT",
       "UPREVIS",
       #"CIG_REC",
       "GESTREC3",
       #"DPLURAL",
       #"DBWT",
       complications
       #riskfactors
       )

#gera a função
#neste caso função apenas significa "y em função de x" e não
#uma função matemática convencional

fmla <- mk_formula(y, x)

#gera o modelo de regressão logistica

model <- glm(fmla, data = train, family = binomial(link = "logit"))

#argumento family especifica distribuição binomial
#onde y assume dois valores dependentes de x

#gera as previsões

train$pred <- predict(model, newdata=train, type = "response")
test$pred <- predict(model, newdata=test, type="response")

#gera grafico da densidade em função do valor da previsão
#densidade significa quanto do conjunto estará presente em um dado intervalo

DoubleDensityPlot(train, "pred", "atRisk",
                  title = "Distribution of natality risk scores")

#gera a matriz de confusão

ctab.test <- table(pred = test$pred > 0.02, atRisk = test$atRisk)

#analise da signifcancia dos coeficientes
summary(model)
