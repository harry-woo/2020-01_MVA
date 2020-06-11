library(dplyr)

rm(list = ls())

data(iris)
str(iris)
head(iris)

# 각 변수의 표준화한 값
install.packages("pls")
library(pls)
ziris <- cbind(stdize(as.matrix(iris[-5])),iris[5])
head(ziris)

# K-평균 군집분석방법을 이용하여 3개 군집에 대한 군집분석 실시
kmcl <- kmeans(ziris[-5], 3)
kmcl

# 군집 결과를 소속군집 산점도로 표현
pairs(ziris, col = kmcl$cluster, pch = 16)

# K-평균 군집분석의 군집결과와 붓꽃 데이터에 주어져 있는 종류와의 분할표 통해 군집분석의 성능 평가

ziris2 <- cbind(ziris, Kmcl = kmcl$cluster)
kmcl_gns <- table(ziris2$Species, ziris2$Kmcl)

ziris3 <- ziris2 %>% 
  mutate(Kmcl2 = ifelse(Kmcl == 1, "versicolor", ifelse(Kmcl == 2, "virginica", "setosa")))
kmcl_gns2 <- table(ziris3$Species, ziris3$Kmcl2)
kmcl_gns2
error <- 1 - sum(diag(kmcl_gns2))/sum(kmcl_gns2)
error
1- error

# 정분류율은 83.3%, 특히 Setosa는 완벽히 분류되었음
