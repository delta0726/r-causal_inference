# ***********************************************************************************************
# Title   : 統計的因果推論の理論と実装
# Chapter : 12 傾向スコアによる層別解析法と重み付け法
# Date    : 2022/06/02
# Page    : P172 - P182
# URL     : https://github.com/mtakahashi123/causality
# ***********************************************************************************************


# ＜概要＞
# - 傾向スコアマッチングではATTは推定できるが、ATEは推定することはできない
# - 傾向スコアのフレキシビリティを享受してATEを算出したい
#   --- 層別解析法と重み付け法により推定可能


# ＜目次＞
# 0 準備
# 1 データセットの概要
# 2 層別マッチングデータの作成
# 3 傾向スコアマッチングの層化解析
# 4 傾向スコアのバランシング評価
# 5 傾向スコアによる重み付け法


# 0 準備 ---------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(broom)
library(MatchIt)
library(lmtest)
library(sandwich)

# データロード
data11 <- read_csv("csv/data11.csv")


# 1 データセットの概要 ---------------------------------------------------------

# ＜ポイント＞
# - 本章の目的はATEの算出なので、潜在的結果変数から算出されるATEの真値を確認しておく


# データ確認
data11 %>% print()

# ATEの確認
# --- 真値は3.756
mean(data11$y1t) - mean(data11$y0t)


# 2 層別マッチングデータの作成 ----------------------------------------------------

# ＜ポイント＞
# - 層化解析はマッチング方法の議論であるのでmethod引数で"subclass"を選択することにより実現することができる
#   --- method = "subclass" / estimand = "ATE" と指定する

# モデル構築
m.out2 <- matchit(t1 ~ x1 + x2 + x3 + x4 + x5 + x6, data = data11,
                  distance = "glm", method = "subclass", subclass = 5,
                  estimand = "ATE", min.n = 2)

# マッチングデータの抽出
# --- マッチングデータにsubclassの列が追加される
# --- モデル
m.data2 <- m.out2 %>% match.data()
m.data2 %>% print()


# 3 傾向スコアマッチングの層化解析 ------------------------------------------------



# オブジェクト生成
# --- t1の推定値
# --- t1の標準誤差
# --- 層のレコード数
# --- クラスターに頑健な標準誤差
psp <- NULL
psvar <- NULL
nps <- NULL
robustvar = NULL

# 共分散分析(層別)
# --- t1のEstimateとStd.Errorを取得
# --- クラスターに頑健な標準誤差の取得
j = 1
for (j in 1:m.out2$info$subclass) {
  dataps <- m.data2[m.data2$subclass == j,]
  model4 <- lm(y3 ~ t1 + x1 + x2 + x3 + x4 + x5 + x6, data = dataps)
  psp[j] <- model4 %>% summary() %>% use_series(coefficients) %>% .[2, 1]
  psvar[j] <- model4 %>% summary() %>% use_series(coefficients) %>% .[2, 2] %>% .^2
  nps[j] <- dataps %>% nrow()
  robustvar[j] <- coeftest(model4, vcov = vcovCL, cluster = ~weights)[2, 2]
}


# ウエイトの算出
# --- 全体のサンプル数
# --- 層別のサンプル数から算出したウエイト
n1 <- data11 %>% nrow()
wgt1 <- nps / n1

# ATEの算出
# --- 各層内での平均処置効果(psp)をウエイト加重することにより算出
# --- 3.749（真値は3.756）
# --- かなり正確に推定することができている
tauhat <- sum(wgt1 * psp)

# 標準誤差
vartau <- sum(wgt1^2 * psvar)
setau <- vartau %>% sqrt()

# クラスターに頑健な標準誤差
robustvartau <- sum(wgt1^2 * robustvar)
robustsetau <-robustvartau %>%  sqrt()

# 信頼区間
tstar <- qt(0.975, n1 - 8)
tauhat + tstar * robustsetau
tauhat - tstar * robustsetau


# 4 傾向スコアのバランシング評価 ------------------------------------------------------

# ＜ポイント＞
# - ラブプロットで共変量の分布が処置群と統制群で類似しているかを視覚的に確認
#   --- サマリーのStd.Mean Diff / Var.Ratioを用いると定量的に評価することができる

# サマリー
m.out2 %>% summary()

# ラブプロットの作成
# ---- ライブラリ
m.out2 %>% summary() %>% plot()

# ラブプロットの作成
# ---- 手動
diff1 <- m.out2 %>% summary() %>% use_series(sum.all) %>% .[, 3] %>% abs() %>% rev()
diff2 <- m.out2 %>% summary() %>% use_series(sum.across) %>% .[, 3] %>% abs() %>% rev()
maxx <- max(diff1, diff2)
labels1 <- m.out2 %>% summary() %>% use_series(sum.all) %>% .[, 3] %>% names() %>% rev()
dotchart(diff1, xlim = c(0, maxx), labels = labels1)
abline(v = 0.00, col = 8)
abline(v = 0.10, col = 8)
abline(v = 0.05, col = 8, lty = 2)
par(new = TRUE)
dotchart(diff2, xlim = c(0, maxx), labels = labels1, pch = 16)


# 5 傾向スコアによる重み付け法 -------------------------------------------------------

# ＜ポイント＞
# - 重み付け法は傾向スコアの逆数をウエイトとしてモデルを構築する（加重ウエイトの重回帰分析）
# - マッチングデータに共変量の偏りが残っている可能性を考慮して、モデルは交絡変数ありで構築する方が良い


# ＜欠点＞
# - 重み付け法は傾向スコアが極小の場合に大きなウエイトを与えてしまうことがある
#   --- 傾向スコアの算出にエラーがあって極小値が出る場合は問題となる
#   --- 傾向スコアの予測値が正確に計算されていることが前提


# モデル構築
model1 <- glm(t1 ~ x1 + x2 + x3 + x4 + x5 + x6, data = data11, family = binomial(link = "logit"))

# 傾向スコアの取得
ps1 <- model1$fitted.values

# ウエイトの算出
# --- 傾向スコアの逆数（t1が1/0で計算が異なる点に注意）
weights1 <-
  data11 %>%
    select(t1) %>% mutate(weights1 = ifelse(t1 == 1, 1 / ps1, 1 / (1 - ps1))) %>%
    pull(weights1)

# モデル構築
# --- 交絡変数なし
# --- 交絡変数あり（推奨）
model2 <- lm(y3 ~ t1, data = data11, weights = weights1)
model3 <- lm(y3 ~ t1 + x1 + x2 + x3 + x4 + x5 + x6, data = data11, weights = weights1)

# 結果確認
# --- 交絡変数なしは3.202、交絡変数ありは3.816（真値は3.756）
# --- 交絡変数をモデルに加えたほうが良好な結果
model2 %>% summary() %>% use_series(coefficients)
model3 %>% summary() %>% use_series(coefficients)

# 標準誤差
model3 %>% coeftest(vcov = vcovCL, cluster = weights1)

# 信頼区間
model3 %>% coefci(level = 0.95, vcov. = vcovCL, cluster = weights1)
