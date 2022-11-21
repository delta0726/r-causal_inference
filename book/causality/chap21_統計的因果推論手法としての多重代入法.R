# ***********************************************************************************************
# Title   : 統計的因果推論の理論と実装
# Chapter : 21 統計的因果推論手法としての多重代入法
# Date    : 2022/11/22
# Page    : P294 - P302
# URL     : https://github.com/mtakahashi123/causality
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 データ準備
# 1 ATTの真値の確認
# 2 リストワイズ除去した場合の傾向スコアマッチング
# 3 多重代入法の場合の傾向スコアマッチング
# 4 リストワイズ除去した場合の操作変数法
# 5 多重代入法の場合の操作変数法
# 6 リストワイズ除去した場合の回帰不連続デザイン
# 7 多重代入法の場合の回帰不連続デザイン


# 0 データ準備 ------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(Amelia)
library(MASS)


# データロード
df1 <- read_csv("csv/data21.csv")


# 1 ATEの真値の確認 --------------------------------------------------------------

# サマリー
df1 %>% summary()

#
mean(df1$y1t) - mean(df1$y0t)
mean(df1$y1, na.rm = TRUE) - mean(df1$y0, na.rm = TRUE)

df2 <- data.frame(y0= df1$y0, y1= df1$y1, x1= df1$x1)
n1 <- nrow(df2)
m1 <- 100
set.seed(1)
a.out <- amelia(df2, m = m1)


# ATEの推定
mi1 <- NULL
mi2 <- NULL
i <- 1
for (i in 1:m1){
  y1imp <- a.out$imputations[[i]]$y1
  y0imp <- a.out$imputations[[i]]$y0
  tauimp <- y1imp - y0imp
  mi1[i] <- mean(tauimp)
  mi2[i] <- var(tauimp) / n1
}

tauMI <- mean(mi1)
wmi <- mean(mi2)
bmi <- (1 / (m1 - 1)) * sum((mi1 - tauMI) ^ 2)
tmi <- wmi + (1 + 1 / m1) * bmi
seMI <- sqrt(tmi)

tauMI + qt(p = 0.975, df = n1 - 1) * seMI
tauMI - qt(p = 0.975, df = n1 - 1) * seMI


set.seed(1)
n1 <- nrow(df2)
m1 <- 100
rho1 <- 0.9
rho2 <- cor(df1$y3, df1$x1)
mu1 <- mean(df1$y3)
mu2 <- mean(df1$x1)
sigma2a <- var(df1$y3)
sigma2b <- var(df1$x1)

cv1 <- rho1 * sqrt(sigma2a) * sqrt(sigma2a)
cv2 <- rho2 * sqrt(sigma2a) * sqrt(sigma2b)
s2 <- matrix(c(sigma2a, cv1, cv2,
               cv1, sigma2a, cv2,
               cv2, cv2, sigma2b), nrow = 3)

prior0 <- mvrnorm(n = 100, mu = c(mu1, mu1, mu2), Sigma = s2)
prior1 <- data.frame(prior0) %>% set_colnames(c("y0", "y1", "x1"))
df3 <- rbind(df2, prior1)
a.out2 <- amelia(df3, m = m1)

tau1 <- matrix(NA, m1, n1)

for (j in 1:n1){
  for (i in 1:m1){
    y1i <- a.out2$imputations[[i]][j, 2]
    y0i <- a.out2$imputations[[i]][j, 1]
    tau1[i, j] <- y1i - y0i
  }
}

mi1 <- NULL
s1 <- NULL
for (j in 1:n1){
  mi1[j] <- mean(tau1[, j])
  s1[j] <- sd(tau1[, j])
}

UL1 <- mi1 + qt(p = 0.975, df = n1 - 1) * s1
LL1 <- mi1 - qt(p = 0.975, df = n1 - 1) * s1

cover1 <- NULL
cover1[(tau1 < LL1) == TRUE | (UL1 < tau1) == TRUE] <- 0
cover1[(tau1 > LL1) == TRUE & (UL1 > tau1) == TRUE] <- 1
mean(cover1) * 100

p1 <- 7
maxx <- max(tau1)
minx <- min(tau1)
boxplot(tau1[, p1], ylim = c(minx, maxx), horizontal = TRUE)
abline(v = mean(mi1), lty = 3, lwd = 2)
abline(v = tau1[p1], col = 8, lwd = 2)
