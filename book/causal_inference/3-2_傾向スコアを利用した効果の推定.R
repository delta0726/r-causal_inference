#***************************************************************************************
# Title     : 効果検証入門
# Chapter   : 3章 傾向スコアを用いた分析
# Theme     : 3-2 傾向スコアを利用した効果の推定
# Date      : 2022/06/20
# Page      : P96 - P111
# URL       : https://github.com/ghmagazine/cibook
#***************************************************************************************


# ＜概要＞
# - 回帰分析(共分散分析)は共変量の選定が重要なるものの、回帰分析の仮定を考えると非常に難しいプロセス
# - 傾向スコアマッチングは傾向スコアを用いて介入有無のグループの性質が近くなるようサンプリングする


# ＜目次＞
# 0 準備
# 1 バイアスのあるデータを作成
# 2 傾向スコアの推定
# 3 傾向スコアマッチング
# 4 逆確率重み付き推定（IPW）
# 5 より良い傾向スコアとは
# 6 回帰分析(共分散分析)と傾向スコアの比較


# 0 準備 ------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(broom)
library(MatchIt)
library(WeightIt)
library(cobalt)
library(conflicted)
library(gridExtra)


# コンフリクト解消
conflict_prefer("select", "dplyr", quiet = TRUE)
conflict_prefer("filter", "dplyr", quiet = TRUE)


# データ取り込み
email_data <- read_csv("csv/E-MailAnalytics.csv")

# データ確認
email_data %>% print()
email_data %>% glimpse()


# 1 バイアスのあるデータを作成 -----------------------------------------------------------

# データ作成
# --- 女性向けメールが配信されたデータを削除
# --- 介入を表すtreatment変数を追加
male_df <-
  email_data %>%
    filter(segment != "Womens E-Mail") %>%
    mutate(treatment = ifelse(segment == "Mens E-Mail", 1, 0))

# シード固定
set.seed(1)

# パラメータ指定
# --- 条件に反応するサンプルの量を半分にする
obs_rate_c <- 0.5
obs_rate_t <- 0.5

# バイアスのあるデータを作成
biased_data <-
  male_df %>%
    mutate(obs_rate_c = ifelse((history > 300) |
                                 (recency < 6) |
                                 (channel == "Multichannel"), obs_rate_c, 1),
           obs_rate_t = ifelse((history > 300) |
                                 (recency < 6) |
                                 (channel == "Multichannel"), 1, obs_rate_t),
           random_number = runif(n = NROW(male_df))) %>%
    filter((treatment == 0 & random_number < obs_rate_c) |
             (treatment == 1 & random_number < obs_rate_t))


# 2 傾向スコアの推定 -------------------------------------------------------------------

# ＜ポイント＞
# - 傾向スコアは分類モデルのクラス確率として定義される
#   --- 共変量が多い場合に1つの系列として情報が縮約される
#   --- 最もベーシックなのは一般化線形モデルだが、クラス確率が定義されれば他のアルゴリズムでもよい


# データ確認
biased_data %>%
  select(treatment, recency, history, channel)

# モデル構築
# --- 一般化線形モデルの傾向スコアの推定
ps_model <-
  glm(treatment ~ recency + history + channel,
      data = biased_data, family = binomial)

# 傾向スコア
ps_model$fitted.values %>% head()


# 3 傾向スコアマッチング ---------------------------------------------------------------

# ＜ポイント＞
# - 傾向スコアマッチングは傾向スコアを用いてサンプル同士をマッチングさせる手法
#   --- 介入有無のデータ群の性質を類似させる効果がある（セレクションバイアスの排除）


# データ確認
biased_data %>%
  select(treatment, recency, history, channel)

# 傾向スコアマッチング
m_near <-
  matchit(treatment ~ recency + history + channel,
          data = biased_data, method = "nearest", replace = TRUE)

# プロット作成
# --- 共変量のバランスを確認
m_near %>% love.plot(threshold = .1)

# マッチングデータの作成
# --- 31853 ⇒ 24222
matched_data <- m_near %>% match.data()

# マッチング後のデータで効果の推定
PSM_result <-
  lm(spend ~ treatment, data = matched_data) %>%
    tidy()

# 確認
# --- ATT：0.908
PSM_result %>% print()


# 4 逆確率重み付き推定（IPW） ----------------------------------------------------------

# ＜ポイント＞
# - 逆確率重み付き推定とは、傾向スコアの逆数をサンプルのウエイトとして利用する方法


# データ確認
biased_data %>%
  select(treatment, recency, history, channel)

# 重みの推定
weighting <-
  weightit(treatment ~ recency + history + channel,
           data = biased_data,
           method = "ps",
           estimand = "ATE")

# プロット作成
# --- 重み付きデータでの共変量のバランス
weighting %>% love.plot(threshold = .1)

# 重み付きデータでの効果の推定
IPW_result <-
  lm(spend ~ treatment,
     data = biased_data, weights = weighting$weights) %>%
    tidy()

# 確認
# --- ATT：0.870
IPW_result %>% print()


# 5 より良い傾向スコアとは ------------------------------------------------------------------

# ＜ポイント＞
# - 傾向スコアではデータに対する説明力が一定水準を超えることが重要とされてきた(c統計量)
# - 近年では傾向スコアを用いたマッチング後のデータで共変量のバランスが取れているかを重視（love.plot）
#   --- replace = TRUEにしないとマッチングの度合いが下がる


# 傾向スコアマッチング（再掲）
# --- replace = TRUE
m_near_true <-
  matchit(treatment ~ recency + history + channel,
          data = biased_data, method = "nearest", replace = TRUE)

# 傾向スコアマッチング（再掲）
# --- replace = FALSE
m_near_false <-
  matchit(treatment ~ recency + history + channel,
          data = biased_data, method = "nearest", replace = FALSE)

# プロット作成
# --- replaceを適用しないとAdjustの方が乖離が大きくなる
p1 <- m_near_true %>% love.plot(threshold = .1)
p2 <- m_near_false %>% love.plot(threshold = .1)
grid.arrange(p1, p2, nrow = 2)


# 6 回帰分析(共分散分析)と傾向スコアの比較 -----------------------------------------------------

# ＜回帰分析＞
# メリット
# - 非常に手軽で取り組みやすい
# - 回帰分析はOVBやSensitivity Analysisといった分析ツールが充実している

# デメリット
# - 前提が多いため入念なモデリングを行わないと期待した結果が得られない


# ＜傾向スコア＞
# メリット
# - Yに対するモデリングを必要としない
#   --- 情報を入手しやすいZ(共変量)に対するモデリングだけでよい
#   --- Yに対して正確な情報を持ち合わせない場合に重宝する（実務ではこのケースが大多数）

# デメリット
# - マッチングを行うため計算コストがかかる
