library(MASS) # for menarche
library(dplyr)

rm(list = ls())

data(menarche)
head(menarche)

plot(Menarche/Total ~ Age, data = menarche, pch = 19)
# S-shape : 로지스틱 회귀모형 적합 타당

mnc_glm <- glm(cbind(Menarche, Total - Menarche) ~ Age, family = binomial, data = menarche)
summary(mnc_glm)
# Age 유의

exp(mnc_glm$coefficients["Age"])

1 - pchisq(mnc_glm$deviance, mnc_glm$df.residual)

plot(Menarche/Total ~ Age, data = menarche, pch = 19)
lines(menarche$Age, mnc_glm$fitted, type = "l", col = "blue")
title(main = "Menarche Data with Fitted Logistic Regression Line")

prediction <- ifelse(predict(mnc_glm, newdata = menarche, type = "resp") < 0.5, "No", "Yes")

menarche2 <- menarche %>% 
  mutate(ratio = ifelse(Menarche/Total < 0.5, "No", "Yes"), fit = prediction)

confm_mnc <- table(menarche2$ratio, menarche2$fit)

error <- 1 - sum(diag(confm_mnc))/sum(confm_mnc)
error
