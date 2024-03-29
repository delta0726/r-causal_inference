# ***********************************************************************************************
# Title   : 統計学OnePint5 欠測データ処理
# Chapter : 3 単一代入法
# Date    : 2022/10/03
# Page    : P25 - P37
# URL     : https://www.kyoritsu-pub.co.jp/book/b10003896.html
# ***********************************************************************************************


# ＜概要＞
# - 確定的単一代入法は、代入モデルによって得られた予測値を欠測値と置き換える
#   --- モデル構築は行うが、多重化は行わない


# ＜これまで学んだ代入法＞
# - リストワイズ除去
#   --- 解析対象とされる複数の変数のどれか一つでも欠損値を持つケースを計算から除外
#   --- 効率性を下げるだけでなく、結果に偏りをもたらす
#
# - 確定的単一代入法
#   --- データの平均集計を目的とした場合、平均値で単一代入する欠損値補完は合理的


# ＜目次＞
# 0 準備
# 1 平均値の比較
# 2 確定的回帰代入法
# 3 比率代入法
# 4 平均値代入法
# 5 ホットデック法
# 6 確率的回帰代入法


# 0 準備 ------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(mice)
library(hot.deck)
library(naniar)


# データ作成
gdp <- c(0.8, 3.2, 7, 11.1, 14.3, 23.5, 24.7, 26.4, 38.1, 41.5)
freedom <- c(NA, NA, 24, NA, 16, 95, 86, 83, 96, 95)
freedomTrue <- c(25, 28, 24, 65, 16, 95, 86, 83, 96, 95)
df1 <- data.frame(freedom, gdp)

# データ確認
df1 %>% print()
df1 %>% vis_miss()


# 1 平均値の比較 -----------------------------------------------------------

# ＜ポイント＞
# - ｢NA除外の平均値｣と｢真値の平均値｣は大きく乖離している
#   --- リストワイズ除去法は偏りを生み出してしまう（分布が歪む）
#   --- リストワイズ除去法とはNAレコードを除去する処理


# 平均値の比較
# --- NAあり(70.71429)
# --- 真値(61.3)
df1$freedom %>% mean(na.rm = TRUE)
freedomTrue %>% mean()


# 2 確定的回帰代入法 --------------------------------------------------------

# ＜ポイント＞
# - 確定的回帰代入法とは回帰モデルから算出した予測値を用いて欠損値を補完する方法
#   --- 回帰代入法は欠損値を含む系列を被説明変数として回帰モデルを構築
#   --- モデルはリストワイズ除去されたデータで構築される点に注意
#   --- 補完結果は説明変数に依存したものとなる

# ＜結論＞
# - リストワイズ除去と比べて、平均値の偏りが小さくなる
# --- 真値：61.3
# --- リストワイズ：70.71429
# --- 確定的回帰代入法：56.24232

# 線形回帰の実行
# --- モデルはリストワイズ除去されたデータで構築
lm(freedom ~ gdp, data = df1)


# 欠損値補完
# --- 確定的回帰代入法
imp <- df1 %>% mice(method = "norm.predict", m = 1, maxit = 1)
imp %>% complete()

# 補完後データ
freedomImp <- imp %>% complete() %>% use_series(freedom)

# 平均値の比較
# --- 確定的回帰代入法(56.24232)
# --- 真値(61.3)
freedomImp %>% mean()
freedomTrue %>% mean()


# 3 比率代入法 -------------------------------------------------------------

# 4 平均値代入法 -----------------------------------------------------------

# ＜ポイント＞
# - 平均値代入法は系列の平均値である70.7をそのまま採用する方法
#   --- 一般的に百害あって一利なし
#   --- グループごとの平均値を使用するなどすると改善する


# 5 ホットデック法 --------------------------------------------------------

# ＜ポイント＞
# - 米国センサス局において開発されたノンパラメトリックな代入方法（モデルを仮定しない方法）
# - ホットデック法は欠測変数とは別の補助変数が類似したレコードを探して欠測変数を補完する


# データ作成
gdp <- c(0.8, 3.2, 7, 11.1, 14.3, 23.5, 24.7, 26.4, 38.1, 41.5)
freedom <- c(NA, NA, 0, NA, 0, 1, 1, 1, 1, 1)
df2 <- data.frame(freedom, gdp)
#hot.deck(data = df1, method = "best.cell", impContinuous = "HD")


# 6 確率的回帰代入法 -------------------------------------------------------

# ＜ポイント＞
# - 確定的回帰代入法は推定値のバラツキを著しく損なう（回帰直線上の値を取るため）
# - 確率的回帰代入法は回帰モデルで予測値を推定したうえで、乱数生成した誤差項を追加する
#   --- 平均値：0  分散：回帰モデルの残差分散


# データ確認
df1 %>% print()

# 欠損値補完
# --- 確率的回帰代入法
imp <- df1 %>% mice(meth = "norm.nob", m = 1, maxit = 1, seed = 1)

# 補完後データ
freedomImp <- imp %>% complete() %>% use_series(freedom)

# 平均値の比較
# --- 確率的回帰代入法(63.38306)
# --- 真値(61.3)
freedomImp %>% mean()
freedomTrue %>% mean()

# 分散
freedomImp %>% sd()
