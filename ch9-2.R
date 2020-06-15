# 제5장에서 사용한 Fisher의 붓꽃 데이터를 이용하여 분류나무를 구하고자 한다. 
# 다음과 같이 분류나무를 구하시오. 

# 문제에서 제시된 처리

data(iris)
firis <- iris 
head(firis)     # 자료 확인하기 위함 
names(firis) <- c("SL","SW","PL","PW","SP") 
levels(firis$SP) <- c("st","vc","vg")  
head(firis)     # 자료 확인하기 위함 

# rpart 함수를 이용하여 나무모형을 수행한 후, 나무모형의 도형화 결과를 보이고 결과를 해석하시오. 
# 나무모형을 이용하여 분류한 결과와  원래 그룹과의 분류표를 구하고, 오분류율을 구하시오. 

library(rpart)
tree <- rpart(SP ~ ., data = firis)
tree

plot(tree, uniform = TRUE, compress = TRUE, margin = 0.1)
text(tree, use.n = TRUE, col = "blue")

prediction <- predict(tree, newdata = firis, type = "class")

confm_firis <- table(firis$SP, prediction, dnn = c("Actual", "Predicted"))
confm_firis

1 - sum(diag(confm_firis)) / sum(confm_firis)

summary(tree)

# 정지규칙에서 가지를 나누는 최소 자료의 수를 5로 하여 나무모형을 수행하고 앞의 (1)의 결과와 비교하시오

my_control <- rpart.control(minsplit = 5)
new_tree <- rpart(SP ~ ., data = firis, method = "class", control = my_control)
new_tree

plot(new_tree, uniform = TRUE, compress = TRUE, margin = 0.1)
text(new_tree, use.n = TRUE, col = "blue")

new_prediction <- predict(new_tree, newdata = firis, type = "class")

confm_firis2 <- table(firis$SP, new_prediction, dnn = c("Actual", "Predicted"))
confm_firis2

1 - sum(diag(confm_firis2)) / sum(confm_firis2)

summary(new_tree)

# 오분류율이 개선되었다.
