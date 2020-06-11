library(dplyr)

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

predict(mnc_glm, newdata = menarche, type = "resp")

menarche %>% 
  mutate(fit = mnc_glm$fitted * Total)

?menarche
