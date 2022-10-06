# ***********************************************************************************************
# Title   : 統計学OnePint5 欠測データ処理
# Chapter : 5 多重代入法のアルゴリズム
# Date    : 2022/10/05
# Page    : P54 - P71
# URL     : https://www.kyoritsu-pub.co.jp/book/b10003896.html
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 normによる多重代入法
# 2 データ拡大法による多重代入法
# 3 FCSアルゴリズムによる多重代入法
# 4 EMBアルゴリズムによる多重代入法


# 0 準備 -----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(norm2)
library(mice)
library(naniar)

# データ作成
df1 <- read_csv("csv/gdp.csv")

# データ確認
df1 %>% print()
df1 %>% vis_miss()


# 1 データ拡大法の準備 --------------------------------------------------------------

# ＜ポイント＞
# - データ拡大法では初期値を設定する必要があるが、EMアルゴリズムの値が推奨される


# EMアルゴリズムの実行
# --- EMアルゴリズム
set.seed(1)
emResult <- df1 %>% emNorm(iter.max = 10000)

# データ確認
emResult %>% names()

# イテレーション回数
# --- EMの収束回数の2倍にすることで保守的な推定を目指す
max1 <- emResult$iter * 2

# 確認
max1 %>% print()


# 2 データ拡大法による多重代入法 ------------------------------------------------------

# ＜ポイント＞
# - データ拡大法(DA)はマルコフ連鎖モンテカルロ法(MCMC)に基づく多重代入法の伝統的アルゴリズム
# - 前期までの値を条件としてパラメータ推定値を繰り返し置き換えることでパラメータを推定する


# 準備
M <- 3
imp.list <- as.list(NULL)

# 多重代入法の実行
# --- EMアルゴリズム
i <- 1
for (i in 1:M){
  imp.list[[i]] <-
    emResult %>%
      mcmcNorm(iter = max1) %>%
      impNorm()
}

# データ確認
imp.list[[1]]
imp.list[[2]]
imp.list[[3]]


# 3 FCSアルゴリズムによる多重代入法 ---------------------------------------------------

# ＜ポイント＞
# - 完全条件付き指定(FCS)は多変量分布を一連の条件付分布として指定する
#   --- DAアルゴリズムでは多変量分布を仮定したうえでジョイントモデルとして使用


# イテレーション回数
# --- EMアルゴリズムの結果から決定
emResult <- df1 %>% emNorm(iter.max = 10000)
max1 <- emResult$iter * 2

# 多重代入法
# --- 線形回帰モデル
imp <- df1 %>% mice(m = 3, seed = 1, meth = "norm", maxit = max1)

# データ確認
imp %>% complete(1)
imp %>% complete(2)
imp %>% complete(3)


# 4 EMBアルゴリズムによる多重代入法 -----------------------------------------------------

imp %>% glimpse()

set.seed(1)
x <- c(NA, 272, 797, 239, 415)
xboot1 <- sample(x, replace = TRUE)
xboot2 <- sample(x, replace = TRUE)
xboot3 <- sample(x, replace = TRUE)


n <- 5
x <- c(415, 415, 239, 239)
mu <- 150
expect <- NA

for (i in 1:100){
  expect[i] <- sum(x) + (n - length(x)) * mu[i]
  mu[i + 1] <- n * expect[i]
  if (mu[i+1] - mu[i] < 0.0001){
    break
  }
}

library(Amelia)

set.seed(1)
a.out <- amelia(df1, m = 3)


plot(mcmcResult$series.worst)
plot(imp, c("freedom"), layout = c(2, 1))
