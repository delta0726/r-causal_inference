# ***********************************************************************************************
# Title   : 統計的因果推論の理論と実装
# Chapter : 8 最小二乗法によるモデリングと診断2
# Date    : 2022/5/23
# Page    : P107 - P124
# URL     : https://github.com/mtakahashi123/causality
# ***********************************************************************************************


# ＜概要＞
# - 回帰分析における仮定を確認する
#   - 仮定4： 完全な多重共線性がない
#   - 仮定5： 誤差項の分散均一性
#   - 仮定6： 誤差項の正規性


# ＜目次＞
# 0 準備
# 1 完全な多重共線性がないこと(仮定4)
# 2 多重共線性を回避したモデル
# 3 誤差項の不均一分散(仮定5)
# 4 不均一分散の診断
# 5 不均一分散への対処法1(加重最小二乗法)
# 6 不均一分散への対処法2(不均一分散に頑健な標準誤差)
# 7 誤差項の正規性(仮定6)


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(psych)
library(car)
library(lmtest)
library(sandwich)
library(normtest)
library(broom)


# データロード
data08a <- read_csv("csv/data08a.csv")
data08b <- read_csv("csv/data08b.csv")
data08c <- read_csv("csv/data08c.csv")


# 1 完全な多重共線性がないこと(仮定4) -----------------------------------------------

# ＜ポイント＞
# - 完全な多重共線性というのは説明変数同士の相関が1のものをいう（禁止）
# - 多重共線性は相関係数が1ではないものの、高い相関を持つ場合をいう（禁止でないが推定に問題を起こしうる）
#   --- X1とX2の相関が強くなるにつれて標準誤差が大きくなるため信頼区間の幅が急速に広がってしまう
#   --- VIF(分散拡大要因)が10以上(決定係数が90％以上)の場合に多重共線性が発生しているとみなす
#   --- VIFが10以上の場合に自動的に問題視するのではなく、問題の背景を探りに行く姿勢が重要


# データ確認
data08a %>% print()
data08a %>% summary()

# 相関係数の確認
# --- 全体的に高相関（X2とX3は特に高相関）
data08a %>% pairs.panels()

# モデル構築
# --- x2とx3は説明変数同士でなく被説明変数であっても多重共線性は発生する
model_1a <- lm(x1 ~ x2 + x3, data = data08a)
model_1b <- lm(x2 ~ x1 + x3, data = data08a)
model_1c <- lm(x3 ~ x1 + x2, data = data08a)

# 決定係数
rj2_1a <- model_1a %>% summary() %>% use_series(r.squared)
rj2_1b <- model_1b %>% summary() %>% use_series(r.squared)
rj2_1c <- model_1c %>% summary() %>% use_series(r.squared)

# VIF
# --- 1bと1cが10を超えている
1 / (1 - rj2_1a)
1 / (1 - rj2_1b)
1 / (1 - rj2_1c)


# 2 多重共線性を回避したモデル ----------------------------------------------------

# ＜ポイント＞
# - 以下の例では｢多重共線性の排除｣と｢交絡因子のコントロール｣の関係について考察する
#   --- 多重共線性が回帰係数を推定を常に歪ませるわけではない
#   --- 変数をむやみに除外すると交絡因子のコントロールが不十分となるケースがでる

# ＜結論＞
# - 統計的因果推論の立場からは、共変量の偏回帰係数の解釈は必要ない
#   --- 多重共線性についてはそれほど気にする必要はない
#   --- コントロール変数は適切に入っていることが重要（編回帰係数の水準を気にする必要はない）


# モデル構築
# --- x2とx3が同時に含まれないようにする
model_2 <- lm(y1 ~ x1, data = data08a)
model_3 <- lm(y1 ~ x1 + x2, data = data08a)
model_4 <- lm(y1 ~ x1 + x3, data = data08a)

# 回帰係数
# --- モデル2のみx1の真値である1.3を推定できていない（交絡変数の影響が排除できていない）
model_2 %>% summary() %>% use_series(coefficients)
model_3 %>% summary() %>% use_series(coefficients)
model_4 %>% summary() %>% use_series(coefficients)

# VIF
# --- モデル3/モデル4は多重共線性なし
model_3 %>% vif()
model_4 %>% vif()


# モデル構築
# --- x2とx3を含むので多重共線性を持つ
model_5 <- lm(y1 ~ x1 + x2 + x3, data = data08a)

# 回帰係数
# --- x1の真値である1.3を推定できている
model_5 %>% summary() %>% use_series(coefficients)

# VIF
# --- x2とx3で10を超えている
model_5 %>% vif()


# 3 誤差項の不均一分散(仮定5) -------------------------------------------------------

# ＜ポイント＞
# - 不均一分散とは、誤差項の分散が特定の条件において分散が異なる状況をいう
# - 通常の最小二乗法から回帰係数を推定する目的においては取り除くべき問題
#   --- 不均一分散自体が学術的発見を示していることもある
# - 不均一分散は回帰係数の推定には影響を与えないが標準誤差に影響が出る
#   --- 最小二乗法による回帰係数の推定の不偏性には不要な仮定だが、最良な線形不偏推定量であるために必要な仮定
#   --- 最良な線形不偏推定量とは、全ての線形不偏推定量の中で標準誤差が最も小さいことを意味する


# データ確認
data08b %>% print()
data08b %>% summary()

# プロット確認
# --- 全体的に高相関（X2とX3は特に高相関）
data08b %>% pairs.panels()

# モデル構築
model_1a <- lm(y1 ~ x1, data = data08b)
model_2a <- lm(y2 ~ x1, data = data08b)

# 回帰係数の確認
# --- x1の回帰係数は違い水準だが、標準誤差は大きく異なる
# --- 標準誤差に影響が出ていることが窺える
model_1a %>% tidy()
model_2a %>% tidy()

# 残差プロット
# --- 1aは均一分散
# --- 2aは不均一分散
tibble(x1 = data08b$x1, resid_1a = residuals(model_1a)) %>% plot(ylim = c(-10, 10))
tibble(x1 = data08b$x1, resid_1a = residuals(model_2a)) %>% plot(ylim = c(-10, 10))


# 4 不均一分散の診断 -------------------------------------------------------------

# ＜ポイント＞
# - ブルーシュ・ペイガン検定を用いると誤差項の分散が均一かどうかを検定することができる
#   --- 手動計算と関数計算の両アプローチを確認


# データ確認
data08b %>% print()

# モデル構築
model_2a <- lm(y2 ~ x1, data = data08b)


# ブルーシュ・ペイガン検定(手動) ----------------------------

# データフレーム作成
# --- 残差平方の列を追加
data08b_2 <- data08b %>% mutate(resid_2b = residuals(model_2a) ^ 2)

# モデル構築
# --- 残差をx1で回帰
model_2b <- lm(resid_2b ~ x1, data = data08b_2)

# LM統計量の計算
r_squared <- model_2b %>% summary() %>% use_series(r.squared)
bp2 <- r_squared * nrow(data08b)

# p値を算出
# --- カイ二乗分布を用いてp値を算出
pchisq(bp2, 1, lower.tail = FALSE)


# ブルーシュ・ペイガン検定(関数) ----------------------------

# 検定の実施
# --- 関数による計算
model_2a %>% bptest()
model_2a %>% bptest() %>% use_series(p.value)


# 5 不均一分散への対処法1(加重最小二乗法) -------------------------------------------

# ＜ポイント＞
# - 誤差項の分散を不均一にしている変数および関数の形が分かれば加重最小二乗法により対処することができる
#   --- 今回の誤差項はexp(1.5*xi)に基づくようになっている


# モデル構築
# --- 加重最小二乗法
model_5w <- lm(y2 ~ x1, weights =  1 / exp(1.5 * x1), data = data08b)

# 回帰係数
# --- 標準誤差が小さくなっている
model_5w %>% tidy()

# データフレーム作成
data08b_3 <- data08b_2 %>% mutate(logresid2b = log(resid_2b))


# モデル構築
model_6 <- lm(logresid2b ~ x1, data = data08b_3)
hhat2 <- model_6 %>% predict() %>% exp()


# モデル構築
model_7 <- lm(y2 ~ x1, weights = 1 / hhat2, data = data08b)
model_7 %>% tidy()


# 6 不均一分散への対処法2(不均一分散に頑健な標準誤差) ----------------------------------

# 不均一分散への対処法2：不均一分散に頑健な標準誤差
model_2a <- lm(y2 ~ x1, data = data08b)
e2 <- model_2a %>% resid() %>% . ^ 2
hensa_x2 <- (data08b$x1 - mean(data08b$x1)) ^ 2
num <- sum(hensa_x2 * e2)
denom <- (sum(hensa_x2)) ^ 2
sqrt(num / denom)

model_2a %>% vcovHC(type = "HC") %>% sqrt()


# 7 誤差項の正規性(仮定6) -----------------------------------------------------------

# ＜ポイント＞
# - 誤差項の正規性はジャック・ベラの正規性検定で確認することができる


# データ確認
data08c %>% print()
data08c %>% summary()

# ジャック・ベラの正規性検定
set.seed(1)
model_1 <- lm(y1 ~ x1, data08c)
resid_1 <- model_1 %>% residuals()
resid_1 %>% jb.norm.test()

model_2 <- lm(y2 ~ x1, data08c)
resid_2 <- model_2 %>% residuals()
resid_2 %>% jb.norm.test()

resid_1 %>% hist()
resid_2 %>% hist()
