#***************************************************************************************
# Title     : 効果検証入門
# Chapter   : 3章 傾向スコアを用いた分析
# Theme     : 3-4 LaLondeデータセットの分析
# Date      : 2022/6/20
# Page      : P118 - P130
# URL       : https://github.com/ghmagazine/cibook
#***************************************************************************************


# ＜概要＞
# - RCTは結果変数以外の共変量の影響がなく効果検証において最も信頼できる分析となる
#   --- RCTができない状況において信頼できる結果を得るための工夫が必要（因果推論のモチベーション）


# ＜目次＞
# 0 準備
# 1 データセットの作成
# 2 RCTによる結果確認
# 3 共分散分析による効果検証
# 4 傾向スコアマッチングを適用した効果検証
# 5 逆確率重み付き推定による効果検証


# 0 準備 ------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(broom)
library(MatchIt)
library(WeightIt)
library(cobalt)


# データ読み込み
cps1_data <- read_csv("csv/cps1_data.csv")
cps3_data <- read_csv("csv/cps3_data.csv")
nswdw_data <- read_csv("csv/nswdw_data.csv")

# データ確認
# --- 全て列が同じデータセット
cps1_data %>% glimpse()
cps3_data %>% glimpse()
nswdw_data %>% glimpse()


# 1 データセットの作成 --------------------------------------------------------------

# ＜ポイント＞
# - NSWは一定の条件を持つ人に対して行われたRCTデータ（職業訓練の効果検証データ）
# - NSWの実験で得たデータの非介入グループを削除し、実験外で得られたCPSデータを挿入する
#   --- NSW：介入データのみ
#   --- CPS1：別の調査データを非介入データとして使用
#   --- CPS3：CPS1のうち特定条件を満たさないサンプルを削除

# ＜データ項目＞
# treat    ：介入有無
# age      ：年齢
# education：教育
# black    ：黒人フラグ
# hispanic ：ヒスパニック系フラグ
# married  ：結婚有無
# nodegree ：学位有無
# re74     ：収入1
# re75     ：収入2
# re78     ：収入3


# データ作成
# --- NSWデータから介入グループだけ抽出してCPS1の介入グループとして扱う
# --- CPS1と結合
cps1_nsw_data <-
  nswdw_data %>%
    filter(treat == 1) %>%
    bind_rows(cps1_data)

# データ作成
# --- NSWデータから介入グループだけ抽出してCPS3の介入グループとして扱う
# --- CPS3と結合
cps3_nsw_data <-
  nswdw_data %>%
    filter(treat == 1) %>%
    bind_rows(cps3_data)


# 2 RCTによる結果確認 ---------------------------------------------------------------

# ＜ポイント＞
# - NSWのローデータ(nswdw_data)を用いてRCTにおける効果を測定する
#   --- 介入有無(treat)以外に交絡変数を全てモデルに入れることでRCTを実現
#   --- ATEの真値は1676（以降で真値をどれだけ再現できるかが論点）


# データ確認
nswdw_data %>% print()

# モデル構築
# --- NSWのローデータ(nswdw_data)を使用
# --- RCTにおける効果測定
nsw_cov <-
   lm(re78 ~ treat + re74 + re75 + age + education + black + hispanic + nodegree + married,
      data = nswdw_data) %>%
     tidy() %>%
     filter(term == "treat")

# 確認
# --- ATEは$1676
nsw_cov %>% print()


# 3 共分散分析による効果検証 -----------------------------------------------------------

# ＜ポイント＞
# - CPS1は非介入データを1974年の収入が観測されているデータに差替えたもの
#   --- 調査データは非失業者に限定されているわけではない
# - CPS3は


# 共変量付きの回帰分析
# --- バイアスのあるデータ（cps1_nsw_data）
cps1_reg <-
   lm(re78 ~ treat + re74 + re75 + age + education + black + hispanic + nodegree + married,
      data = cps1_nsw_data) %>%
     tidy() %>%
     filter(term == "treat")

# 共変量付きの回帰分析
# --- バイアスのあるデータ（cps3_nsw_data）
cps3_reg <-
   lm(re78 ~ treat + re74 + re75 + age + education + black + hispanic + nodegree + married,
      data = cps3_nsw_data) %>%
     tidy() %>%
     filter(term == "treat")

# 確認
# --- CPS1：ATEは$699
# --- CPS3：ATEは$1548
cps1_reg %>% print()
cps3_reg %>% print()


# 4 傾向スコアマッチングを適用した効果検証 -------------------------------------------

# ＜ポイント＞
# - 傾向スコアマッチングでNSWと近いサンプルをCPSから抜き出す
#   --- NSWに近い結果を得るためには、NSWと近いサンプルで検証する必要がある
#   --- 恣意性が働きにくいシステマティックな方法


# 傾向スコアを用いたマッチング
# --- lm()ではなくmatchit()を用いる
m_near <-
  matchit(treat ~ age + education + black + hispanic + nodegree +
          married + re74 + re75 + I(re74^2) + I(re75^2), data = cps1_nsw_data, method = "nearest")

# 共変量のバランスを確認
# --- 共変量の2群における差が小さくなっている
m_near %>% love.plot(threshold = .1)

# マッチング後のデータを作成
matched_data <- m_near %>% match.data()

# 共変量なしの回帰分析
# --- マッチング後のデータで検証
PSM_result_cps1 <-
   lm(re78 ~ treat, data = matched_data) %>%
     tidy()

# 確認
# --- ATEは$1877（ATEの真値は$1676）
# --- 比較的近い値になっている
PSM_result_cps1 %>% print()


# 5 逆確率重み付き推定による効果検証 ----------------------------------------------------

# ATE *********************************************

# 重みの推定
weighting <-
  weightit(treat ~ age + education + black + hispanic + nodegree
                   + married + re74 + re75 + I(re74^2) + I(re75^2),
           data = cps1_nsw_data,
           method = "ps",
           estimand = "ATE")

# 共変量のバランスを確認
weighting %>% love.plot(threshold = .1)

# 共変量なしの回帰分析
# --- 重み付きデータでの効果の推定
IPW_result <-
   lm(re78 ~ treat, data = cps1_nsw_data, weights = weighting$weights) %>%
     tidy()

# 確認
IPW_result %>% print()


# ATT *********************************************

# 重みの推定
weighting <-
  weightit(treat ~ age + education + black + hispanic + nodegree
           + married + re74 + re75 + I(re74^2) + I(re75^2),
           data = cps1_nsw_data,
           method = "ps",
           estimand = "ATT")

# 共変量のバランスを確認
weighting %>% love.plot(threshold = .1)

# 共変量なしの回帰分析
# --- 重み付きデータでの効果の推定
IPW_result <-
   lm(re78 ~ treat, data = cps1_nsw_data, weights = weighting$weights) %>%
     tidy()

# 確認
IPW_result %>% print()
