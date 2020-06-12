rm(list = ls())

install.packages("klaR")
library(MASS)
library(klaR)
library(dplyr)

city <- read.csv("ex7-2.csv")
head(city)
summary(city)
str(city)

city_lda <- lda(type ~ ., data = city)
city_lda #그룹이 2개이므로 1개 선형판별함수 출력

pred_lda <- predict(city_lda, newdata = city)
names(pred_lda)
head(pred_lda$class)
head(pred_lda$posterior)
head(pred_lda$x)

confm_lda <- table(city$type, pred_lda$class)
confm_lda

error <- 1 - sum(diag(confm_lda))/sum(confm_lda)
error

# 변수선택 가능? 안되는데? 안되는게 정상이라고.
# city_forward <- greedy.wilks(type ~ ., data = city, niveau = 0.01)

# 분류함수
source("classfunc.r")
X <- city[, -1]
classfunc_result <- classfunc.lda(X, city$type)
names(classfunc_result)
classfunc_result$class.func
classfunc_result

X2 <- X %>% 
  mutate(ind = -12.7537645 + 0.3272460 * population + 0.5556657 * pollution,
         mer = -12.702302 + 0.365933 * population + 0.526113 * pollution) %>% 
  mutate(type = city$type, type2 = ifelse(ind > mer, "ind", "mer"))

confm_lda2 <- table(X2$type, X2$type2)
confm_lda2

error <- 1 - sum(diag(confm_lda2))/sum(confm_lda2)
error
