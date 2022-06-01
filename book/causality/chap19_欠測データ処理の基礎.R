# ***********************************************************************************************
# Title   : 統計的因果推論の理論と実装
# Chapter : 19 欠測データ処理の基礎
# Date    : 2022/06/02
# Page    : P263 - P280
# URL     : https://github.com/mtakahashi123/causality
# ***********************************************************************************************


# ＜概要＞
# - 因果推論においては欠損値データの処理も重要な過大となる
# - 多重代入法は多くの分野で欠損データの対処方法のベストプラクティスとして用いられている


# ＜目次＞
# 0 準備
# 1 単一代入法
# 2 多重代入法
# 3 多重代入法の結果の統合方法
# 4 {mice}による多重代入法
# 5 交互作用項のある重回帰モデルにおける欠損値処理


# 0 準備 ---------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(Amelia)
library(mice)

# データロード
data19a <- read_csv("csv/data19a.csv", na = "9999")
data19b <- read_csv("csv/data19b.csv")


# 1 単一代入法 ---------------------------------------------------------------

# ＜ポイント＞
# -

# ＜データ概要＞
# - y  ： 被説明変数
# - x1 ： 説明変数（欠損なし）
# - x2 ： 説明変数（欠損あり）
# - x3 ： 説明変数（欠損値を単一代入法で補完）


# データ確認
data19a %>% print()

# モデル構築
# --- 欠損値の与える影響を確認
# --- 真値は0.8607, 欠損の場合は0.6903
lm(x1 ~ y, data = data19a)
lm(x2 ~ y, data = data19a)


# 2 多重代入法 -------------------------------------------------------------

# データ作成
# --- 単一代入法を用いた説明変数を使用
df2 <- data.frame(y = data19a$y, x2 = data19a$x2)

# 多重代入法
set.seed(1)
a.out <- df2 %>% amelia(m = 3)

# 欠損値補完後のデータ
dfimp1 <- a.out$imputation[[1]]
dfimp2 <- a.out$imputation[[2]]
dfimp3 <- a.out$imputation[[3]]

# モデル構築
model1 <- lm(y ~ x2, data = dfimp1)
model2 <- lm(y ~ x2, data = dfimp2)
model3 <- lm(y ~ x2, data = dfimp3)

# 結果確認
# --- いずれも真値の0.8607に近い
model1 %>% summary() %>% use_series(coefficient)
model2 %>% summary() %>% use_series(coefficient)
model3 %>% summary() %>% use_series(coefficient)


# 3 多重代入法の結果の統合方法 -----------------------------------------------

# 回帰係数の取得
b1 <- model1 %>% summary() %>% use_series(coefficient) %>% .[2, 1]
b2 <- model2 %>% summary() %>% use_series(coefficient) %>% .[2, 1]
b3 <- model3 %>% summary() %>% use_series(coefficient) %>% .[2, 1]

# 標準誤差の取得
se1 <- model1 %>% summary() %>% use_series(coefficient) %>% .[2, 2]
se2 <- model2 %>% summary() %>% use_series(coefficient) %>% .[2, 2]
se3 <- model3 %>% summary() %>% use_series(coefficient) %>% .[2, 2]

# 回帰係数の平均値
betabar <- (b1 + b2 + b3) / 3

# 標準誤差の平均値
wbar <- (se1^2 + se2^2 + se3^2) / 3


bbar1 <- (b1 - betabar)^2 + (b2 - betabar)^2 + (b3 - betabar)^2
bbar2 <- bbar1 / (3 - 1)

tbar <- wbar + (1 + 1/3) * bbar2
betabar
sqrt(tbar)


# 4 {mice}による多重代入法 -------------------------------------------------

# パラメータ設定
m1 <- 3

# モデル構築
m.out <- mice(df2, m = m1, seed = 1, meth = "norm", maxit = 20)

# オブジェクト生成
dfimp0 <- NULL
b0 <- NULL
se0 <- NULL

# 回帰係数等の取得
for (i in 1:m1){
  dfimp0 <- a.out$imputations[[i]]
  model0 <- lm(y ~ x2, data = dfimp0)
  b0[i] <- model0 %>% summary() %>% use_series(coefficient) %>% .[2, 1]
  se0[i] <- model0 %>% summary() %>% use_series(coefficient) %>% .[2, 2]
}

# 回帰係数の平均値
b0 %>% mean()

wbar0 <- sum(se0^2) / m1
bbar0 <- sum((b0 - mean(b0))^2) / (m1 - 1)
tbar0 <- wbar0 + (1 + 1 / m1) * bbar0
tbar0 %>% sqrt()


# 5 交互作用項のある重回帰モデルにおける欠損値処理 ------------------------------

dfimp0 <- NULL
b0 <- NULL
se0 <- NULL
m1 <- 3

m.out <- mice(df2, m = m1, seed = 1, meth = "norm", maxit = 20)
for (i in 1:m1){
  dfimp0 <- complete(m.out, i)
  model0 <- lm(y ~ x2, data = dfimp0)
  b0[i] <- model0 %>% summary() %>% use_series(coefficient) %>% .[2, 1]
  se0[i] <- model0 %>% summary() %>% use_series(coefficient) %>% .[2, 2]
}

mean(b0)
wbar0 <- sum(se0^2) / m1
bbar0 <- sum((b0 - mean(b0))^2) / (m1 - 1)
tbar0 <- wbar0 + (1 + 1 / m1) * bbar0
sqrt(tbar0)


# 例示
set.seed(1)
n1 <- 10
x1 <- rnorm(n1) %>% round(0)
x2 <- rnorm(n1) %>% round(0)
e1 <- rnorm(n1)

x1x2 <- x1 * x2

y <- round(1 + 1 * x1 + 1 * x2 + 1 * x1x2 + e1, 0)
df1 <- data.frame(y, x1, x2, x1x2)
x2[1] <- NA
x1x2[1] <- NA
df2 <- data.frame(y, x1, x2, x1x2)


df1
df2


imp1 <- mice(data = df2, m = 1, seed = 1, maxit = 1, meth = c("", "", "norm.predict", "norm.predict"))
imp1 %>% complete(1)


imp2 <- mice(data = df2, m = 1, seed = 1, maxit = 1, meth = c("", "", "norm.predict", "~I(x1*x2)"))
imp2 %>% complete(1)



summary(data19b)

data19b_2 <-
  data19b %>%
    mutate(xt0 = (x0 - mean(x0)) * (t1 - mean(t1)),
           xt1 = (x1 - mean(x1, na.rm = TRUE)) * (t1 - mean(t1)))

model1 <- lm(y3 ~ x0 + t1 + xt0, data = data19b_2)
model2 <- lm(y3 ~ x1 + t1 + xt1, data = data19b_2)


model1 %>% summary() %>% use_series(coefficient)
model2 %>% summary() %>% use_series(coefficient)

x1L <- data19b$x1 - mean(data19b$x1, na.rm = TRUE)
t1L <- data19b$t1 - mean(data19b$t1)
xt1L <- x1L * t1L
dfL <- data.frame(y3 = data19b$y3, x1 = data19b$x1, t1 = data19b$t1, xt1L)

imp01 <- mice(data=dfL, m=1, maxit=1, meth=c("", "norm.predict", "", "norm.predict"))
xlimbar <- mean(complete(imp01, 1)$x1)

x1MI <- x1 - xlimbar
xt1MI <- x1MI * t1L

dfMI <- data.frame(y3 = data19b$y3, x1 = data19b$x1, t1 = data19b$t1, xt1MI)


#

m1 <- 100
max1 <- 20
imp1 <- mice(data = dfMI, m=m1, maxit = max1, seed = 1, meth = c("", "norm", "", "norm"))
tau2m <- NULL
se2m <- NULL

for (i in 1:m1){
  dfimp1 <- complete(imp1, 1)
  modelM1 <- lm(y3 ~ x1 + t1 + xt1MI, data = dfimp1)
  tau2m[i] <- modelM1 %>% summary() %>% use_series(coefficient) %>% .[3, 1]
  se2m[i] <- modelM1 %>% summary() %>% use_series(coefficient) %>% .[3, 2]
}

tau2m %>% mean()
w1bar <- sum(se2m^2) / m1
b1bar <- sum((tau2m - mean(tau2m))^2) / (m1 - 1)
sqrt(w1bar + (1 + 1 / m1) * b1bar)


#

set.seed(1)
imp1a <- amelia(dfMI, m1)
tau2a <- NULL
se2a <- NULL

for (i in 1:m1){
  dfimp1a <- imp1a$imputation[[i]]
  modelA1 <- lm(y3 ~ x1 + t1 + xt1MI, data = dfimp1a)
  tau2a[i] <- modelA1 %>% summary() %>% use_series(coefficient) %>% .[3, 1]
  se2a[i] <- modelA1 %>% summary() %>% use_series(coefficient) %>% .[3, 2]
}

tau2a %>% mean()
w1bar <- sum(se2a^2) / m1
b1bar <- sum((tau2a - mean(tau2a))^2) / (m1 - 1)
sqrt(w1bar + (1 + 1 / m1) * b1bar)

