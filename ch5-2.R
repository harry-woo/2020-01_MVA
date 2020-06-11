rm(list = ls())

# source("readMatrix.r")
# city <- readMatrix()
# C:/Users/S-OIL/Documents/KNOU-MVA/citydist.txt
# 2
# 10
# city_name <- scan("C:/Users/S-OIL/Documents/KNOU-MVA/cityname.txt", what = "")
# colnames(city) <- city_name
# rownames(city) <- city_name
# city

data(UScitiesD)
# https://jbhender.github.io/Stats506/F17/MatrixDecompositions.html

# 거리행렬 만들기
str(UScitiesD)
dist <- as.matrix(UScitiesD)
dist

# cmdscale 실행 (2차원, 지도상의 거리)
MDS1 <- cmdscale(dist, k = 2)

# plot(USCitiesMDS, pch='', xlab = "D1", ylab = "D2", xlim = -c(-1750, 1250))
# text(USCitiesMDS, attr(UScitiesD, "Labels"))

# Plot
MDS1 <- -1 * MDS1 # 실제 위치에 맞추기 위해 rotate
MDS1

plot(MDS1[,1], MDS1[,2], pch='', xlab = "", ylab = "", 
     xlim = c(-1750, 1250), main = "cmdscale(Auto)")
text(MDS1[,1], MDS1[,2], rownames(dist))
abline(h = 0, v = 0, lty = 3, col = "grey")

# smacofSym 활용
# install.packages("smacof")
library(smacof)

MDS2 <- smacofSym(dist, ndim = 2)
attributes(MDS2)

# Plot
MDS2$conf <- -1 * MDS2$conf # 실제 위치에 맞추기 위해 rotate
plot(MDS2$conf[,1], MDS2$conf[,2], pch="", xlab="", ylab="", xlim = c(-1.1, 0.8),
     main = "smacof(Auto)")
text(MDS2$conf[,1], MDS2$conf[,2], rownames(dist))
abline(h = 0, v = 0, lty = 3, col = "grey")

# Stress Value : 0.05~0.10 만족, 0~0.05 매우좋음
# smacof 버전의 차이로 보임, 최신 버전에는 stress.m 이 없음 (m : metric)
MDS2$stress # 매우 좋음

# Scree plot
MDS2.1 <- smacofSym(dist, ndim = 1)
MDS2.2 <- smacofSym(dist, ndim = 2)
MDS2.3 <- smacofSym(dist, ndim = 3)
MDS2.4 <- smacofSym(dist, ndim = 4)

stress_value = c(MDS2.1$stress, MDS2.2$stress, MDS2.3$stress, MDS2.4$stress)
plot(stress_value, type = "l", main = "Scree plot")
points(stress_value, cex = 0.9)

# 적합도
z01dist <- (MDS2$delta - min(MDS2$delta))/(max(MDS2$delta) - min(MDS2$delta))
plot(z01dist, MDS2$confdist,
     xlab = "Observed distance", ylab = "Configuration distance")
abline(0, 1)
