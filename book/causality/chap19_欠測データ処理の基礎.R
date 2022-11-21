# ***********************************************************************************************
# Title   : 統計的因果推論の理論と実装
# Chapter : 19 欠測データ処理の基礎
# Date    : 2022/06/02
# Page    : P263 - P280
# URL     : https://github.com/mtakahashi123/causality
# ***********************************************************************************************


# ＜概要＞
# - 因果推論においては欠損値データの処理も重要な課題となる
# - 多重代入法は多くの分野で欠損データの対処方法のベストプラクティスとして用いられている


# ＜欠測のメカニズム＞
# - MCAR：Xの値が何にも依存せず完全に無作為で欠損すること
# - MAR ：Xの値に依存して欠損するものの、γを条件づけたときに無作為な欠損とみなせること
# - NMAR：Xの値に依存して欠損し、かつγを条件づけたときに無作為な欠損とみなせないこと

# ＜欠測処理の方法＞
# - MCARの場合は欠測している行を削除(リストワイズ除去法)しても偏りが生じないが、MARの場合は偏りが生じる
# - MARの条件が満たされれば、欠測メカニズムを無視可能とみなすことができる


# ＜目次＞
# 0 準備
# 1 単一代入法
# 2 多重代入法による欠損値シミュレーション
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
df1 <- read_csv("csv/data19a.csv", na = "9999")
data19b <- read_csv("csv/data19b.csv")


# 1 単一代入法 ---------------------------------------------------------------

# ＜ポイント＞
# - 単一代入法は逆回帰モデルを利用して欠損値を推定する手法
#   --- 逆回帰としてモデルを考えた場合は欠測値があっても回帰係数を正しく推定することができる
#   --- 欠損以外のデータでモデル構築して予測値を用いて欠損値を補完

# ＜データ概要＞
# - y  ： 被説明変数
# - x1 ： 説明変数（欠損なし）
# - x2 ： 説明変数（欠損あり）
# - x3 ： 説明変数（欠損値を単一代入法で補完）


# データ確認
# --- NAの真値：80
# --- NAの推定値：74.1（x3は推定値で補完した系列）
df1 %>% print()

# モデル構築
# --- 逆回帰モデルを構築（xとyが逆である点に注意）
model1 <- lm(x1 ~ y, data = df1)
model2 <- lm(x2 ~ y, data = df1)
model3 <- lm(x3 ~ y, data = df1)

# 確認
# --- 真値：0.8607
# --- 欠損の場合：0.6903
# --- 欠損値補完の場合：0.6901
print(model1)
print(model2)
print(model3)

# NAの推定
model3$fitted.values[1]

# 代入法における誤差
# --- 根本的な誤差（実測値 - 推定値[モデル1]）
# --- 推定誤差（推定値[モデル1] - 推定値[モデル3]）
df1$x1[1] - model1$fitted.values[1]
model1$fitted.values[1] - model3$fitted.values[1]


# 2 多重代入法による欠損値シミュレーション -----------------------------------

# ＜ポイント＞
# - 多重代入法は推定するたびに異なる回帰モデルを推定する方法
#   --- 以降ではEMBアルゴリズムを使用（ブートストラップ法に期待値最大化法を適用）
#   --- シミュレーションにより推定不確実性を反映
#   --- 各データでモデルを構築してから推定結果を統合する点に注意


# データ作成
# --- 単一代入法を用いた説明変数を使用
df2 <- df1 %>% select(y, x2)

# 多重代入法
set.seed(1)
a.out <- df2 %>% amelia(m = 3)

# 出力確認
a.out %>% print()
a.out %>% names()
a.out %>% plot()

# 欠損値補完後のデータ
# --- シミュレーションベースで単一代入法を実行
dfimp1 <- a.out$imputation[[1]]
dfimp2 <- a.out$imputation[[2]]
dfimp3 <- a.out$imputation[[3]]

# モデル構築
# --- 各データでモデルを構築してから推定結果を統合する
model1 <- lm(y ~ x2, data = dfimp1)
model2 <- lm(y ~ x2, data = dfimp2)
model3 <- lm(y ~ x2, data = dfimp3)

# 結果確認
# --- いずれも真値の0.8607に近い
model1 %>% summary() %>% use_series(coefficient) %>% .[2, 1]
model2 %>% summary() %>% use_series(coefficient) %>% .[2, 1]
model3 %>% summary() %>% use_series(coefficient) %>% .[2, 1]


# 3 多重代入法の回帰係数の統合方法 -------------------------------------------

# ＜ポイント＞
# - 多重代入法の結果はモデル推定後の結果を用いて行う
#   --- 回帰係数は平均値
#   --- 標準誤差は二乗平均


# 回帰係数の取得
b1 <- model1 %>% summary() %>% use_series(coefficient) %>% .[2, 1]
b2 <- model2 %>% summary() %>% use_series(coefficient) %>% .[2, 1]
b3 <- model3 %>% summary() %>% use_series(coefficient) %>% .[2, 1]

# 標準誤差の取得
se1 <- model1 %>% summary() %>% use_series(coefficient) %>% .[2, 2]
se2 <- model2 %>% summary() %>% use_series(coefficient) %>% .[2, 2]
se3 <- model3 %>% summary() %>% use_series(coefficient) %>% .[2, 2]

# 回帰係数の平均値
# --- 式(19.4)
# --- 0.846083
betabar <- (b1 + b2 + b3) / 3
print(betabar)

# 代入内分散
# --- 式(19.5)
wbar <- (se1^2 + se2^2 + se3^2) / 3

# 介入間分散
# --- 式(19.6)
bbar <- ((b1 - betabar)^2 + (b2 - betabar)^2 + (b3 - betabar)^2) / (3 - 1)

# 全体分散
# --- 式(19.7)
# --- 0.2591214
tbar <- wbar + (1 + 1/3) * bbar
sqrt(tbar)


# 4 {mice}による多重代入法 -------------------------------------------------

# ＜ポイント＞
# - {mice}による計算で前述の全体分散を再現する


# データ確認
df2 %>% print()

# パラメータ設定
m1 <- 3

# モデル構築
m.out <- mice(df2, m = m1, seed = 1, meth = "norm", maxit = 20)

# オブジェクト生成
dfimp0 <- NULL
b0 <- NULL
se0 <- NULL

# 推定結果の取得
# --- シミュレーションごとの回帰係数と標準誤差の取得
i <- 1
for (i in 1:m1){
  dfimp0 <- a.out$imputations[[i]]
  model0 <- lm(y ~ x2, data = dfimp0)
  b0[i] <- model0 %>% summary() %>% use_series(coefficient) %>% .[2, 1]
  se0[i] <- model0 %>% summary() %>% use_series(coefficient) %>% .[2, 2]
}

# 回帰係数の平均値
# --- 0.846083
b0 %>% mean()

# 代入内分散
wbar0 <- sum(se0^2) / m1

# 代入間分散
bbar0 <- sum((b0 - mean(b0))^2) / (m1 - 1)

# 全体分散
# --- 0.2591214
tbar0 <- wbar0 + (1 + 1 / m1) * bbar0
tbar0 %>% sqrt()


# 5 交互作用項のある重回帰モデルにおける欠損値処理 ------------------------------

# データロード
df1 <- read_csv("csv/data19_df1.csv")
df2 <- read_csv("csv/data19_df2.csv")


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
