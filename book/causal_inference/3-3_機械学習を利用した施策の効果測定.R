#***************************************************************************************
# Title     : 効果検証入門
# Chapter   : 3章 傾向スコアを用いた分析
# Theme     : 3-3 機械学習を利用したメールマーケティング施策の効果測定
# Date      : 2022/11/19
# Page      : P96 - P111
# URL       : https://github.com/ghmagazine/cibook
#***************************************************************************************


# ＜概要＞
# - 機械学習システムでは傾向スコアそのものがログとして残ることがあり分析モチベーションが高いデータとなる
# - ここでは仮想的にロジスティック回帰を利用してメール配信データを作成して効果測定を行う
#   --- 傾向スコアに機械学習の予測の概念を導入する


# ＜目次＞
# 0 準備
# 1 データ作成
# 2 データ分割
# 3 傾向スコアの算出してデータセットに追加
# 4 RCTの比較
# 5 傾向スコアマッチングによる推定
# 6 逆確率ウエイトによる推定


# 0 準備 ------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(broom)
library(WeightIt)
library(cobalt)
library(Matching)
library(conflicted)

# コンフリクト解消
conflict_prefer("select", "dplyr", quiet = TRUE)
conflict_prefer("filter", "dplyr", quiet = TRUE)


# データ取り込み
email_data <- read_csv("csv/E-MailAnalytics.csv")

# データ確認
email_data %>% print()
email_data %>% glimpse()


# 1 データ作成 ------------------------------------------------------------------

# データ作成
# --- 女性向けメールが配信されたデータを削除
# --- 介入を表すtreatment変数を追加
male_df <-
  email_data %>%
    filter(segment != "Womens E-Mail") %>%
    mutate(treatment = ifelse(segment == "Mens E-Mail", 1, 0))


# 2 データ分割 --------------------------------------------------------------------

# ＜ポイント＞
# - データセットを2つに分割して｢過去データ(訓練用)｣｢未来データ(検証用)｣として使用する
#   --- 訓練データでモデル構築、検証データで予測


# 乱数シードの設定
set.seed(1)

# 抽出レコードのサンプリング
train_flag <-
  male_df %>%
    nrow() %>%
    sample(nrow(male_df) / 2, replace = FALSE)

# データ分割
# --- 訓練データ
# --- 検証データ
male_df_train <- male_df %>% slice(train_flag) %>% filter(treatment == 0)
male_df_test <- male_df %>% slice(-train_flag)


# 3 傾向スコアの算出してデータセットに追加 ---------------------------------------------

# ＜ポイント＞
# - 機械学習モデルから出力されたデータを想定して傾向スコア付のデータセットを作成する


# データ確認
male_df_train %>% select(conversion, recency, history_segment, channel, zip_code)

# モデル構築
# --- 介入有無をY
predict_model <-
  glm(conversion ~ recency + history_segment + channel + zip_code,
      data = male_df_train, family = binomial)

# 傾向スコア
# --- クラス確率
pred_cv <-
  predict_model %>%
    predict(newdata = male_df_test, type = "response")

# 確率の変換
# --- パーセントランク
pred_cv_rank <-
  pred_cv %>%
    percent_rank()

# メール配信
# --- 配信確率を元にメールの配信を決める
mail_assign <-
  pred_cv_rank %>%
    sapply(rbinom, n = 1, size = 1)

# 配信ログの作成
# --- 傾向スコア付のデータセット
ml_male_df <-
  male_df_test %>%
    mutate(mail_assign = mail_assign,
           ps = pred_cv_rank) %>%
    filter((treatment == 1 & mail_assign == 1) |
             (treatment == 0 & mail_assign == 0))


# 4 RCTの比較 ---------------------------------------------------------------------

# ＜ポイント＞
# - メールが売上が高くなりやすいユーザーに偏って配信されてセレクションバイアスが発生している


# 平均の差を確認
rct_male_lm <- lm(spend ~ treatment, data = male_df_test) %>% tidy()
ml_male_lm <- lm(spend ~ treatment, data = ml_male_df) %>% tidy()

# 確認
# --- RCTの場合：0.805
# --- セレクションバイアスあり：1.04（メールが売上の高い顧客に偏って配信された結果）
rct_male_lm %>% print()
ml_male_lm %>% print()


# 5 傾向スコアマッチングによる推定 -------------------------------------------------

# ＜ポイント＞
# - ここでは{Matching}のMatch()を使う


# モデル構築
PSM_result <-
  Match(Y = ml_male_df$spend,
        Tr = ml_male_df$treatment,
        X = ml_male_df$ps,
        estimand = "ATT")

# 推定結果の表示
PSM_result %>% summary() %>% use_series(coefficient)


# 6 逆確率ウエイトによる推定 ------------------------------------------------------

# IPWの推定
# --- 0.7015（p値が0.42もあり信頼性も疑わしい）
W.out <-
  weightit(treatment ~ recency + history_segment + channel + zip_code,
           data = ml_male_df,
           ps = ml_male_df$ps,
           method = "ps",
           estimand = "ATE")

# プロット作成
# --- 重み付けしたデータでの共変量のバランスを確認
W.out %>% love.plot(threshold = .1)

# 重みづけしたデータでの効果の分析
IPW_result <-
  ml_male_df %>%
    lm(spend ~ treatment, data = ., weights = W.out$weights) %>%
    tidy()

# 確認
IPW_result %>% print()
