# ***********************************************************************************************
# Library   : ivreg
# Title     : Get Start
# Date      : 2022/06/04
# URL       : https://john-d-fox.github.io/ivreg/
# ***********************************************************************************************


# ＜概要＞
# - 以前にAERパッケージに含まれていたivreg()を独立してライブラリとして実装したもの
# - モデル診断や各種プロットなども実装している


# ＜目次＞
# 0 準備
# 1 データセットの確認
# 2 モデルで使用するデータ変換の確認
# 3 線形回帰モデル


# 0 準備 ------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(ivreg)
library(modelsummary)
library(psych)


# データロード
data("SchoolingReturns", package = "ivreg")

# 確認
SchoolingReturns %>% as_tibble()


# 1 データセットの確認 ------------------------------------------------------------------

# ＜ポイント＞
# - 賃金決定のための古典的なモデルでの収入に対する学校教育の影響を調査したデータ
# - 入門的な計量経済学の教科書で2SLS推定を説明するために使用されたもの


# データ確認
SchoolingReturns %>% glimpse()

# モデルで使用するデータ
SchoolingReturns %>%
  select(wage, education, experience, ethnicity, smsa, south) %>%
  as_tibble()

# サマリー
SchoolingReturns %>%
  select(wage, education, experience, ethnicity, smsa, south) %>%
  summary()


# 2 モデルで使用するデータ変換の確認 --------------------------------------------------

# モデルで使用する数量データ
SchoolingReturns %>%
  mutate(wage = log(wage),
         experience_poly_1 = poly(experience, 2)[,1],
         experience_poly_2 = poly(experience, 2)[,2]) %>%
  select(wage, education, experience_poly_1, experience_poly_2) %>%
  pairs.panels()

# 多項式回帰による変換
SchoolingReturns %>%
  select(experience) %>%
  mutate(experience_poly_1 = poly(experience, 2)[,1],
         experience_poly_2 = poly(experience, 2)[,2]) %>%
  as_tibble() %>%
  pairs.panels()


# 3 線形回帰モデル ---------------------------------------------------------------------

# ＜ポイント＞
# - 標準的な賃金方程式は、賃金の半対数線形回帰をOLS回帰によって定義する
# - OLSの推定では、学校への復帰について年間7.4％の推定値が得られる

# モデル構築
m_ols <- lm(log(wage) ~ education + poly(experience, 2) + ethnicity + smsa + south,
            data = SchoolingReturns)

# サマリー
m_ols %>% summary()
m_ols %>% tidy()

# 教育と賃金変化率
# --- 年間7.4％
m_ols %>%
  tidy() %>%
  filter(term == "education") %>%
  select(estimate)


# 4 2段階回帰モデル ---------------------------------------------------------------

# ＜ポイント＞
# - 上記のモデルはeducationには内生性があるため推定に問題がある
#   --- 操作変数法によるコントロールが必要となる


# モデル定義
# --- 2SLS回帰を定義するには|の後に操作変数を指定する
m_iv <- ivreg(log(wage) ~ education + poly(experience, 2) + ethnicity + smsa + south |
                          nearcollege + poly(age, 2) + ethnicity + smsa + south,
              data = SchoolingReturns)


# モデル定義（簡素化）
# ---- 外因性変数、内因性変数、追加の操作変数、に分けて記述
m_iv <- ivreg(log(wage) ~ ethnicity + smsa + south | education + poly(experience, 2) |
              nearcollege + poly(age, 2), data = SchoolingReturns)

# サマリー
m_iv %>% summary()
m_iv %>% tidy()

# 教育と賃金変化率
# --- 年間13.3％
m_iv %>%
  tidy() %>%
  filter(term == "education") %>%
  select(estimate)


# 5 モデル比較 --------------------------------------------------------------

# ＜ポイント＞
# - 2段階の最小二乗法を使用して回帰を推定するとeducaionの係数がはるかに大きくなる
# - 回帰の複数モデルの比較は{modelsummary}で手軽に確認することができる


# 複数モデルをリストに格納
m_list <- list(OLS = m_ols, IV = m_iv)

# サマリー
m_list %>% msummary()

# プロット
# --- スケールがt異なるため切片とexperienceは削除
m_list %>% modelplot(coef_omit = "Intercept|experience")
