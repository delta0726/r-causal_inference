# ***********************************************************************************************
# Title   : 統計的因果推論の理論と実装
# Chapter : 12 傾向スコアによる層別解析法と重み付け法
# Date    : 2022/06/02
# Page    : P172 - P182
# URL     : https://github.com/mtakahashi123/causality
# ***********************************************************************************************


# ＜概要＞
# - 単純な傾向スコアマッチングではATEを推定することはできない
#   --- 傾向スコアを用いた層別解析法と重み付け法により推定を試みる


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

# データ確認
data11 %>% print()

# ATEの確認
# --- 真値は3.756
mean(data11$y1t) - mean(data11$y0t)


# 2 層別マッチングデータの作成 ----------------------------------------------------

# ＜ポイント＞
# - 層化解析はマッチング方法の議論であるのでmethod引数で"subclass"を選択することにより実現することができる
# - 推定対象はATEなのでestimate引数により設定する


# パラメータ設定
# --- 層数
sub <- 5

# モデル構築
m.out2 <- matchit(t1 ~ x1 + x2 + x3 + x4 + x5 + x6, data = data11,
                  distance = "glm", method = "subclass", subclass = sub,
                  estimand = "ATE", min.n = 2)

# マッチングデータの抽出
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
for (j in 1:sub) {
  dataps <- m.data2[m.data2$subclass == j,]
  model4 <- lm(y3 ~ t1 + x1 + x2 + x3 + x4 + x5 + x6, data = dataps)
  psp[j] <- model4 %>% summary() %>% use_series(coefficients) %>% .[2, 1]
  psvar[j] <- model4 %>% summary() %>% use_series(coefficients) %>% .[2, 2] %>% .^2
  nps[j] <- dataps %>% nrow()
  robustvar[j] <- coeftest(model4, vcov = vcovCL, cluster = ~weights)[2, 2]
}


# クラスターに頑健な標準誤差
n1 <- data11 %>% nrow()
tauhat <- sum((nps / n1) * psp)
vartau <- sum((nps / n1)^2 * psvar)
setau <- vartau %>% sqrt()
robustvartau <- sum((nps / n1)^2 * robustvar)
robustsetau <-robustvartau %>%  sqrt()

# 信頼区間
tstar <- qt(0.975, n1 - 8)
tauhat + tstar * robustsetau
tauhat - tstar * robustsetau


# 4 傾向スコアのバランシング評価 ------------------------------------------------------

# サマリー
m.out2 %>% summary()

# ラブプロットの作成
# ---- 手動
diff1 <- m.out2 %>% summary() %>% use_series(sum.all) %>% .[, 3] %>% abs() %>% rev()
diff2 <- m.out2 %>% summary() %>% use_series(sum.across) %>% .[, 3] %>% abs() %>% rev()
maxx <- max(diff1, diff2)
labels0 <- m.out2 %>% summary() %>% use_series(sum.all) %>% .[, 3] %>% names()
labels1 <- labels0 %>% rev()
dotchart(diff1, xlim = c(0, maxx), labels = labels1)
abline(v = 0.00, col = 8)
abline(v = 0.10, col = 8)
abline(v = 0.05, col = 8, lty = 2)
par(new = TRUE)
dotchart(diff2, xlim = c(0, maxx), labels = labels1, pch = 16)

# ラブプロットの作成
# ---- ライブラリ
m.out2 %>% summary() %>% plot()


# 5 傾向スコアによる重み付け法 -------------------------------------------------------

# モデル構築
model1 <- glm(t1 ~ x1 + x2 + x3 + x4 + x5 + x6, data = data11, family = binomial(link = "logit"))

# 傾向スコアの取得
ps1 <- model1$fitted.values

# ウエイトの算出
# --- 傾向スコアの逆数で加重
if1 <- data11$t1 == 1
if0 <- data11$t1 == 0
weights1 <- NULL
weights1[if1] <- 1 / ps1[if1]
weights1[if0] <- 1 / (1 - ps1[if0])

# モデル構築
# --- 交絡変数なし
# --- 交絡変数あり
model2 <- lm(y3 ~ t1, data = data11, weights = weights1)
model3 <- lm(y3 ~ t1 + x1 + x2 + x3 + x4 + x5 + x6, data = data11, weights = weights1)

# 結果確認
model2 %>% summary() %>% use_series(coefficients)
model3 %>% summary() %>% use_series(coefficients)

# 標準誤差
model3 %>% coeftest(vcov = vcovCL, cluster = weights1)

# 信頼区間
model3 %>% coefci(level = 0.95, vcov. = vcovCL, cluster = weights1)
