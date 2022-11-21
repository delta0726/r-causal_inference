# ***********************************************************************************************
# Title   : 統計的因果推論の理論と実装
# Chapter : 20 統計的因果推論委おける欠測データ
# Date    : 2022/11/21
# Page    : P281 - P286
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
library(broom)
library(MatchIt)
library(lmtest)
library(Amelia)
library(sandwich)
library(AER)
library(rdrobust)


# データロード
data20aPS <- read_csv("csv/data20aPS.csv")
data20bIV <- read_csv("csv/data20bIV.csv")
data20cRDD <- read_csv("csv/data20cRDD.csv")


# 1 ATTの真値の確認 --------------------------------------------------------------

# サマリー
data20aPS %>% summary()

# ATTの真値
# --- -1.846206
data20aPS %>%
  filter(t1 == 1) %>%
  summarise(y1t = mean(y1t),
            y0t = mean(y0t)) %>%
  mutate(ATT = y1t - y0t)


# 2 リストワイズ除去した場合の傾向スコアマッチング ------------------------------------

# ＜ポイント＞
# - 欠測データの影響によって大幅な偏りが発生している
#   --- 推定値：0.73153
#   --- 真値  ：-1.846206


# データ準備
# --- リストワイズ除去
df2 <- data20aPS %>% na.omit()

# モデル構築
# --- 傾向スコアマッチング
m.out2 <- matchit(t1 ~ x1 + x2 + x3, method = "nearest", data = df2,
                  distance = "glm", replace = TRUE)

# マッチングデータの抽出
m.data2 <- m.out2 %>% match.data()

# レコード数の確認
df2 %>% nrow()
m.data2 %>% nrow()

# 効果測定
model2 <- lm(y3 ~ t1 + x1 + x2 + x3, data = m.data2, weights = weights)

# 係数確認
# --- 推定値：0.73153
# --- 真値  ：-1.846206
model2 %>% coeftest(vcov. = vcovCL, cluster = ~weights)


# 3 多重代入法の場合の傾向スコアマッチング ------------------------------------

# ＜ポイント＞
# - 多重代入法による欠測値補完を行うと、かなり真値に近くなる
# --- 推定値：-1.79786
# --- 真値  ：-1.846206


# データ準備
df3 <- data20aPS %$% data.frame(y3, t1, x1, x2, x3)

# パラメータ設定
m1 <- 100
n1 <- nrow(df3)
k1 <- ncol(df3) - 1

# 格納用オブジェクト
tau1 <- NULL
se1 <- NULL
diff1 <- matrix(NA, m1, k1)
diff2 <- matrix(NA, m1, k1)

# 乱数シード
set.seed(1)

# 多重代入法による欠損値補完
a.out <- amelia(df3, m = m1)

# シミュレーション
i <- 1
for (i in 1:m1) {
  # データ取得
  dfimp <- a.out$imputations[[i]]

  # 傾向スコアマッチング
  m.out <- matchit(t1 ~ x1 + x2 + x3, method = "nearest", data = dfimp,
                   distance = "glm", replace = TRUE)

  # マッチングデータの取得
  m.data <- m.out %>% match.data()

  # 効果検証
  model1 <- lm(y3 ~ t1 + x1 + x2 + x3, data = m.data, weights = weights)

  tau1[i] <- coeftest(model1, vcov. = vcovCL, cluster = ~weights)[2, 1]
  se1[i] <- coeftest(model1, vcov. = vcovCL, cluster = ~weights)[2, 2]
  diff1[i,] <- rev(abs(summary(m.out)$sum.all[, 3]))
  diff2[i,] <- rev(abs(summary(m.out)$sum.matched[, 3]))
  maxx <- max(diff1, diff2)
  lab1 <- rev(rownames(summary(m.out)$sum.all))
}


# 推定値と標準誤差
# --- 推定値：-1.79786
# --- 真値  ：-1.846206
tau1bar <- mean(tau1)
w1bar <- sum(se1^2) / m1
b1bar <- sum((tau1 - mean(tau1))^2) / (m1 - 1)
se1bar <- sqrt(w1bar + (1 + 1 / m1) * b1bar)
tau1bar %>% print()
se1bar %>% print()

# 信頼区間(95%)
cit <- qt(0.975, n1 - 5)
tau1bar + cit * se1bar
tau1bar - cit * se1bar

# ラブプロット
dotchart(diff1[1,], xlim = c(0, maxx), labels = lab1, pch = 2, col = 8)
par(TRUE)
dotchart(diff2[1,], xlim = c(0, maxx), labels = lab1)
for (i in 2:m1) {
  par(new = TRUE)
  dotchart(diff1[i,], xlim = c(0, maxx), labels = lab1, pch = 2, col = 8)
  par(new = TRUE)
  dotchart(diff2[i,], xlim = c(0, maxx), labels = lab1)
}

abline(v = 0, col = 8)
abline(v = 0.1, col = 8)
abline(v = 0.05, lty = 2, col = 8)


# 4 リストワイズ除去した場合の操作変数法 -------------------------------------------------

# データ準備
df4 <- data20bIV %>% as.data.frame()

# データ確認
df4 %>% print()
df4 %>% summary()

# サマリー
df4 %>% print()

# 操作変数法
modelIV2 <- ivreg(y1 ~ x1 | z1 + z2, data = df4)

# 結果確認
# --- 推定値：1.14
# --- 真値  ：1.5
modelIV2 %>% tidy()


# 5 多重代入法の場合の操作変数法 -------------------------------------------------------

# パラメータ設定
m1 <- 100
n1 <- df4 %>% nrow()

# 格納用オブジェクト
tau2 <- NULL
se2 <- NULL

# 乱数シード
set.seed(1)

# 多重代入法による欠損値補完
a.out <- amelia(df4, m = m1)

# シミュレーション
i <- 1
for (i in 1:m1) {
  dfimp2 <- a.out$imputations[[i]]
  modelIV <- ivreg(y1 ~ x1 | z1 + z2, data = dfimp2)
  tau2[i] <- summary(modelIV)$coefficient[2, 1]
  se2[i] <- summary(modelIV)$coefficient[2, 2]
}


# 推定値と標準誤差
# --- 推定値：1.478538
# --- 真値  ：1.5
mean(tau2)
w2bar <- sum(se2^2) / m1
b2bar <- sum((tau2 - mean(tau2))^2) / (m1 - 1)
sqrt(w2bar + (1 + 1 / m1) * b2bar)


# 6 リストワイズ除去した場合の回帰不連続デザイン -----------------------------------------

# データ準備
df4 <- data20cRDD %>% as.data.frame()
df4 %>% head()

# LATEの真値
c1 <- - 1 / 3
h1 <- 0.05
bin1 <- c1 < df4$x1 & df4$x1 < c1 + h1
bin2 <- c1 - h1 < df4$x1 & df4$x1 < c1
mean(df4$y1t[bin1 | bin2]) - mean(df4$y0t[bin1 | bin2])


#
covs0 <- cbind(df4$x2)
rd0 <- rdrobust(df4$y3, df4$x1, c = c1, h = h1, covs = covs0)
covs0b <- cbind(df4$x2b)
rd0b <- rdrobust(df4$y3b, df4$x1, c = c1, h = h1, covs = covs0b)

rd0 %>% summary()
rd0b %>% summary()

m1 <- 100
set.seed(1)

y3b <- df4$y3b[bin1]
x1 <- df4$x1[bin1]
x2b <- df4$x2b[bin1]
datarddb <- data.frame(y3b, x1, x2b)
y3b <- df4$y3b[bin2]
x1 <- df4$x1[bin2]
x2b <- df4$x2b[bin2]
datarddc <- data.frame(y3b, x1, x2b)
a1 <- amelia(datarddb, m = m1)
a2 <- amelia(datarddc, m = m1)


# 7 多重代入法の場合の回帰不連続デザイン ------------------------------------------------

tau1 <- NULL
se1 <- NULL
n1 <- NULL

i <- 1
for (i in 1:m1) {
  dfimp <- rbind(a1$imputations[[i]], a2$imputations[[i]])
  covs1 <- cbind(dfimp$x2b)
  rd1 <- rdrobust(dfimp$y3b, dfimp$x1, c = c1, h = h1, covs = covs1)
  tau1[i] <- rd1$Estimate[1, 1]
  se1[i] <- rd1$Estimate[1, 3]
  n1[i] <- sum(rd1$N_h)
}

mean(tau1)
w2bar <- sum(se1^2) / m1
b2bar <- sum((tau1 - mean(tau1))^2) / (m1 - 1)
sqrt(w2bar + (1 + 1 / m1) * b2bar)
mean(n1)