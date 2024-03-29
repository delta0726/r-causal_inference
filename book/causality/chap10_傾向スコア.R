# ***********************************************************************************************
# Title   : 統計的因果推論の理論と実装
# Chapter : 10 傾向スコア
# Date    : 2022/05/31
# Page    : P136 - P152
# URL     : https://github.com/mtakahashi123/causality
# ***********************************************************************************************


# ＜概要＞
# - 観察研究では実験研究のように実験群と統制群を実験段階で意図的に分離することができない場合がある
# - その場合は無作為割り付けを再現するため、処置の有無以外は同じ条件となるように割付をしたい
# - ｢傾向スコア｣は無作為割り付けが行われない状況で交絡を取り除く手法
#   --- 共分散分析よりもフレキシブルで使いやすい


# ＜目次＞
# 0 準備
# 1 バランシングスコア
# 2 手動でおこなうバランシング
# 3 傾向スコアによるマッチング
# 4 傾向スコアの算出（手動計算）
# 5 傾向スコアの算出（glm）
# 6 傾向スコアの算出（Matchit）


# 0 準備 ---------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(broom)
library(psych)
library(MatchIt)


# データロード
data10a <- read_csv("csv/data10a.csv")
data10b <- read_csv("csv/data10b.csv")
data10c <- read_csv("csv/data10c.csv")
data03 <- read_csv("csv/data03.csv")



# 1 バランシングスコア ----------------------------------------------------------

# ＜ポイント＞
# - バランシングスコアとは処置群と統制群をサンプリングで類似させるために使用する統計量のことをいう
#   --- 条件付分布が処置群と統制群において同じになるようにスコアを割り当てる
#   --- 傾向スコアはバランシングスコアの1つ
#   --- バランシングしない場合にATEと結果が大きく乖離することを確認（問題提起）


# ＜データ概要＞
# y0t：潜在的結果変数
# y1t：潜在的結果変数
# t1 ：処置の有無
# y  ：観測結果
# x1 ：共変量（交絡因子）


# データ確認
data10a %>% print()
data10a %>% pairs.panels()

# 平均処置効果(ATE)
# --- 潜在的結果の差分(P18)
# --- 9.95(真値)
mean(data10a$y1t) - mean(data10a$y0t)

# 処置群の平均処置効果(ATT)
# --- 潜在的結果の差分(P18)
# --- 9.09(真値)
mean(data10a$y1t[data10a$t1 == 1]) - mean(data10a$y0t[data10a$t1 == 1])

# ナイーブな推定値
# --- 目的変数であるyを処置の有無で差分
# --- 17.39
mean(data10a$y[data10a$t1 == 1]) - mean(data10a$y[data10a$t1 == 0])

# 共変量の確認
# --- x1はt1が1の方が大きく0の場合に小さい
# --- x1は共変量の可能性がある
data10a$x1[data10a$t1 == 0] %>% summary()
data10a$x1[data10a$t1 == 1] %>% summary()

# プロット確認
data10a %>%
  select(x1, t1) %>%
    ggplot(aes(x = t1, y = x1, group = t1)) +
    geom_boxplot()


# 2 手動でおこなうバランシング -------------------------------------------------

# ＜ポイント＞
# - 共変量(X)が1つと分かっている場合は共変量(X)が類似した値における処置の有無で比較する
# - このように抽出することで処置群と統制群の間で共変量の分布を近づけることができる
# - バランシングしたペアでは処置効果がATEの真値と近いことが確認されている(P138)

# 表10.3
data10a %>%
  select(t1, y, x1) %>%
  arrange(x1)


# 3 傾向スコアによるマッチング ---------------------------------------------------------

# ＜ポイント＞
# - 共変量が多くなると直感的なバランシングが難しくなるので傾向スコアを用いる
# - 傾向スコアは共変量Xが与えられたときの処置に割り付けられる確率と定義される
# - 傾向スコアは最も粗いバランシングスコアと呼ばれる
#   --- 共変量Xを単変量e(X)に縮約するため｢粗い｣と表現される（｢単純｣と表現してもよい
#   --- 傾向スコアの似たものをペアにするなどして無作為割り当てと似た状態を作る

# ＜データ概要＞
# y0t：潜在的結果変数
# y1t：潜在的結果変数
# t1 ：処置の有無
# y  ：観測結果
# x1 ：共変量1（交絡因子）
# x2 ：共変量2（交絡因子）
# x3 ：共変量3（交絡因子）
# ps1：傾向スコア


# データ確認
# --- 全サンプルを含むデータセット
data10b %>% print()

# 処置群の平均処置効果(ATT)
# --- 17.4（ナイーブ推定値）
mean(data10b$y[data10b$t1 == 1]) - mean(data10b$y[data10b$t1 == 0])

# サマリー
# --- 各共変量は処置の有無で分布が大きく異なっている
# --- 共変量x1の処置有無
# --- 共変量x2の処置有無
# --- 共変量x3の処置有無
data10b$x1[data10a$t1 == 0] %>% summary()
data10b$x1[data10a$t1 == 1] %>% summary()
data10b$x2[data10a$t1 == 0] %>% summary()
data10b$x2[data10a$t1 == 1] %>% summary()
data10b$x3[data10a$t1 == 0] %>% summary()
data10b$x3[data10a$t1 == 1] %>% summary()

# プロット確認
# --- 共変量ごとの処置有無(1/0)の分布を確認
data10b %>%
  select(x1, x2, x3, t1) %>%
  pivot_longer(starts_with("x"), names_to = "t", values_to = "x") %>%
  mutate(t1 = factor(t1)) %>%
    ggplot(aes(x = t, y = x, fill = t1)) +
    geom_boxplot()


# データ確認
# --- 傾向スコア(ps1)でマッチング後のデータセット
data10c %>% print()

# 処置群の平均処置効果(ATT)
# --- 8.5（真値が9.09、ナイーブ推定値が17.4だったのでかなり近づいた）
mean(data10c$y[data10c$t1 == 1]) - mean(data10c$y[data10c$t1 == 0])

# サマリー
# --- いずれも分布が近くなっている
# --- 共変量x1の処置有無（傾向スコアで無作為抽出後）
# --- 共変量x2の処置有無（傾向スコアで無作為抽出後）
# --- 共変量x3の処置有無（傾向スコアで無作為抽出後）
data10c$x1[data10c$t1 == 0] %>% summary()
data10c$x1[data10c$t1 == 1] %>% summary()
data10c$x2[data10c$t1 == 0] %>% summary()
data10c$x2[data10c$t1 == 1] %>% summary()
data10c$x3[data10c$t1 == 0] %>% summary()
data10c$x3[data10c$t1 == 1] %>% summary()

# プロット確認
# --- 共変量ごとの処置有無(1/0)の分布を確認
data10c %>%
  select(x1, x2, x3, t1) %>%
  pivot_longer(starts_with("x"), names_to = "t", values_to = "x") %>%
  mutate(t1 = factor(t1)) %>%
    ggplot(aes(x = t, y = x, fill = t1)) +
    geom_boxplot()


# 4 傾向スコアの算出（手動計算） --------------------------------------------------------

# ＜ポイント＞
# - 傾向スコアがロジスティック回帰モデルの予測確率として定義されることを確認する


# データ作成
set.seed(1)
n1 <- 1000
b0 <- 0.5
b1 <- 1.1
x1 <- runif(n1, -10, 10)
e1 <- rlogis(n1, location = 0, scale = 1)
tstar <- b0 + b1 * x1 + e1
t1 <- NULL
t1[tstar > 0] <- 1
t1[tstar <= 0] <- 0

# 散布図
# --- 理論変数と回帰直線
plot(x1, tstar, col = 8)
lm(tstar ~ x1) %>% abline(lwd = 2)

# 散布図
# --- 観測変数とLOWESS曲線
plot(x1, t1, col = 8)
lines(lowess(x1, t1), lwd = 2)

# ロジスティック曲線
tt <- plogis(x1)
plot(x1, tt)

# 傾向スコアの算出
# --- 公式
num <- exp(b0 + b1 * x1)
denom <- 1 + exp(b0 + b1 * x1)
p1 <- num / denom

# 傾向スコアの算出
# --- glm()の予測確率
model2 <- glm(t1 ~ x1, family = binomial(link = "logit"))
model2 %>% summary()
p2 <- model2$fitted.values

# 結果比較
plot(p1, p2)
cor(p1, p2)


# 5 傾向スコアの算出（glm） ---------------------------------------------------------------

# ＜ポイント＞
# - 介入有無(t1)のようなカテゴリカル変数があれば傾向スコアは定義することができる
# - モデルから算出される傾向スコアは真値よりも標本データの共変量のバランシングをよくする傾向がある
#   --- 傾向スコアの予測値の方が偶然に起因する変動もバランシングしてくれるため

# ＜データ概要＞
# x1  ：入学試験
# y3  ：期末試験
# t1  ：処置の有無
# ps3 ：傾向スコア（予測値）
# ps4 ：傾向スコア（真値）


# 確認
data03 %>% print()
data03 %>% select(t1, x1)

# 傾向スコアの算出
# --- glm()を用いたロジスティック回帰
psmodel <- glm(t1 ~ x1, data = data03, family = binomial(link = "logit"))

# 傾向スコアの取得
# --- 予測確率
ps3 <- psmodel$fitted.values

# 傾向スコアの予測値
# --- 理論確率
# --- ps4は傾向スコアの真値（P149のとおり処置の割付を意図的に行っている）
ps4 <- c(rep(0.8, 5), rep(0.6, 5) ,rep(0.4, 5), rep(0.2, 5))

# データ確認
# --- x1の値が等しければps3は同じ値
# --- 予測確率と理論確率は概ね等しくなっている
df2 <- data.frame(x1 = data03$x1, y3 = data03$y3, t1 = data03$t1, ps3 = ps3, ps4 = ps4)
df2 %>% print()


# 6 傾向スコアの算出（Matchit） --------------------------------------------------

# ＜ポイント＞
# - 傾向スコア自体はglm()で計算できるが、実際のマッチングや層別解析などの処理は{MatchIt}を用いるほうが拡張性が高い
#   --- ここでは傾向スコアがglm()で算出したものと一致することのみを確認する


# データ確認
data03 %>% select(t1, x1)

# モデル構築
m.out <- matchit(t1 ~ x1, data = data03)

# 傾向スコアの取得
ps5 <- m.out$model$fitted.values

# サマリー
ps3 %>% summary()
ps5 %>% summary()

# 結果比較
cor(ps3, ps5)
