#***************************************************************************************
# Title     : 効果検証入門
# Chapter   : 3章 傾向スコアを用いた分析
# Date      : 2021/2/9
# Page      : P118 - P130
# URL       : https://github.com/ghmagazine/cibook
#***************************************************************************************


# ＜目次＞
# 0 準備
# 1 データ作成
# 2 回帰分析による効果検証
# 3 傾向スコアによる効果検証
# 4 逆確率重み付き推定による効果検証



# 0 準備 ------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(haven)
library(broom)
library(MatchIt)
library(WeightIt)
library(cobalt)

# データ読み込み
cps1_data <- read_dta("https://users.nber.org/~rdehejia/data/cps_controls.dta")
cps3_data <- read_dta("https://users.nber.org/~rdehejia/data/cps_controls3.dta")
nswdw_data <- read_dta("https://users.nber.org/~rdehejia/data/nsw_dw.dta")

# データ加工
cps1_data %>% glimpse()
cps3_data %>% glimpse()
nswdw_data %>% glimpse()


# 1 データ作成 ------------------------------------------------------------------

# データ作成
# --- NSWデータから介入グループだけ抽出
# --- CPS1と結合
cps1_nsw_data <-
  nswdw_data %>%
    filter(treat == 1) %>%
    bind_rows(cps1_data)

# データ作成
# --- NSWデータから介入グループだけ抽出
# --- CPS3と結合
cps3_nsw_data <-
  nswdw_data %>%
    filter(treat == 1) %>%
    bind_rows(cps3_data)


# 2 回帰分析による効果検証 -----------------------------------------------------------

# 共変量なしの回帰分析
# --- RCTデータでの分析
nsw_nocov <-
  lm(re78 ~ treat, data = nswdw_data) %>%
    tidy()

# 共変量付きの回帰分析
# --- RCTデータでの分析
nsw_cov <-
   lm(re78 ~ treat + re74 + re75 + age + education + black + hispanic + nodegree + married,
      data = nswdw_data) %>%
     tidy() %>%
     filter(term == "treat")

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


# 3 傾向スコアによる効果検証 --------------------------------------------------------

# 傾向スコアを用いたマッチング
m_near <-
  matchit(treat ~ age + education + black + hispanic + nodegree +
          married + re74 + re75 + I(re74^2) + I(re75^2), data = cps1_nsw_data, method = "nearest")

# 共変量のバランスを確認
m_near %>% love.plot(threshold = .1)

# マッチング後のデータを作成
matched_data <- m_near %>% match.data()

# 共変量なしの回帰分析
# --- マッチング後のデータ
PSM_result_cps1 <-
   lm(re78 ~ treat, data = matched_data) %>%
     tidy()


# 4 逆確率重み付き推定による効果検証 ----------------------------------------------------

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
