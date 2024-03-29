# ***********************************************************************************************
# Title   : 統計学OnePint5 欠測データ処理
# Chapter : 4 多重代入法の概要
# Date    : 2022/10/04
# Page    : P38 - P53
# URL     : https://www.kyoritsu-pub.co.jp/book/b10003896.html
# ***********************************************************************************************


# ＜概要＞
# - 欠損値補完の目的は個別の値の完全な復元ではなく、母集団のパラメータ推定における偏りを排除すること
# - 多重代入法はベイズ統計額の枠組みを用いて誤差を適切に評価することができる手法


# ＜目次＞
# 0 準備
# 1 単一代入法の実体
# 2 ベイズ統計学の概要
# 3 多重代入法による代入結果の例
# 4 多重代入法による分析の流れ


# 0 準備 -----------------------------------------------------------------

# ライブラリ
library(tidyverse)


# 1 単一代入法の実体 -------------------------------------------------------

# ＜ポイント＞
# - 欠損値補完は予測行為であるため、使用するモデルが異なると代入値も変化する
#   --- 欠損値は一意に定まるわけではなく、背後に様々な可能性を持っている
#   --- 潜在的な不確実性を反映させるため複数の値を算出する（ベイズ統計学/シミュレーション）


# 2 ベイズ統計学の概要 ----------------------------------------------------

# ＜ポイント＞
# - 多重代入法はベイズ統計額の枠組みを用いて理論構築されている
#   --- ベイズ統計学は新しい情報を入手するたびに確率を更新する
#   --- データに対して知識がない場合は無情報事前分布を用いる


# 3 多重代入法による代入結果の例 --------------------------------------------

# 欠損値補完後の平均値
# --- 欠損を含むfreedomに対して3回の補完を実施
means <- c(61.9, 59.4, 61.6)
means %>% var()


gdp <- c(0.8, 3.2, 7, 11.1, 14.3, 23.5, 24.7, 26.4, 38.1, 41.5)
freedom <- c(NA, NA, 24, NA, 16, 95, 86, 83, 96, 95)
imp1 <- c(31.1, 32.9, 24, 59.8, 16, 95, 86, 83, 96, 95)
imp2 <- c(23.0, 35.4, 24, 40.3, 16, 95, 86, 83, 96, 95)
imp3 <- c(28.3, 32.0, 24, 60.6, 16, 95, 86, 83, 96, 95)
imp1 %>% var()
imp2 %>% var()
imp3 %>% var()


# 4 多重代入法による分析の流れ ----------------------------------------------

# ＜ポイント＞
# - 多重代入法は推定における潜在的な不確実性を反映させるため複数の値を算出する

# ＜3つのステップ＞
# - 代入ステージ ：欠測データの事後予測分布を構築することにより、複数の代入済データセットを用意
# - 分析ステージ ：各データセットを別々に使用して統計分析を行う
# - 統合ステージ ：Rubinの手法により複数の結果を1つに統合
