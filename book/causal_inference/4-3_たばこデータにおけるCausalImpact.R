#***************************************************************************************
# Title     : 効果検証入門
# Chapter   : 4章 差分の差分法(DID)とCausal Impact
# Theme     : 4-3 たばこデータにおけるCausal Impact
# Date      : 2022/11/19
# Page      : P147 - P160
# URL       : https://github.com/ghmagazine/cibook
#***************************************************************************************


# ＜概要＞
# - Causal Impactはさまざまな変数Xを利用してYを予測するモデルを介入が行われる前の期間のみで作成する
# - 自動的に利用する変数選択をを決定してモデル構築してくれる
# - 平行トレンド仮説のような強い前提をおく必要がなくなる


# ＜DIDの欠点＞
# - 集計済データしか持つことができない場合、DIDは最後の頼みの綱といえる手法
# - 以下の2点の欠点については吟味する必要がある
#   --- 効果測定したい変数が複数パターンで入手できている
#   --- 平行トレンド仮定を分析者の主観で決めてしまう


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 前後比較による分析
# 3 タバコの売上のトレンド
# 4 DIDのためのデータ準備
# 5 DIDによる分析
# 6 州ごとのデータでのDID分析
# 7 CaucialImapctのデータ準備
# 8 モデル構築
# 9 結果確認


# 0 準備 ---------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(broom)
library(Ecdat)
library(miceadds)
library(CausalImpact)

# データ確認
Cigar %>% as_tibble()
Cigar %>% glimpse()


# 1 データ準備 --------------------------------------------------------

# データ削除用ベクトル
# --- Common Trend Assumptionの為に分析から特定の州を外す
# --- タバコの税金が1988年以降50セント以上上がった州のリスト
skip_state <- c(3, 9, 10, 22, 21, 23, 31, 33, 48)

# データ再定義
Cigar <-
  Cigar %>%
    filter(!state %in% skip_state, year >= 70) %>%
    mutate(area = if_else(state == 5, "CA", "Rest of US")) %>%
    as_tibble()

# データ確認
Cigar %>% print()


# 2 前後比較による分析---------------------------------------------------

# テーブル作成
Cigar %>%
  mutate(period = if_else(year > 87, "after", "before"),
         state = if_else(state == 5, "CA", "Rest of US")) %>%
  group_by(period, state) %>%
  summarise(sales = sum(sales * pop16) / sum(pop16)) %>%
  spread(state, sales)

# プロット
# --- 前後比較
Cigar %>%
  mutate(period = if_else(year > 87, "after", "before"),
         state = if_else(state == 5, "CA", "Rest of US")) %>%
  group_by(period, state) %>%
  summarise(sales = sum(sales * pop16) / sum(pop16), .groups = "drop") %>%
  ggplot(aes(y = sales,
             x = period,
             shape = state,
             linetype = state)) +
  geom_point(size = 2) +
  geom_line(aes(group = state)) +
  ylim(0, NA) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_discrete(name = "Period", limits = c("before", "after"))


# 3 タバコの売上のトレンド ----------------------------------------------

# プロット作成
Cigar %>%
  mutate(state = if_else(state == 5, "CA", "Rest of US")) %>%
  group_by(year, state) %>%
  summarise(sales = sum(sales * pop16) / sum(pop16)) %>%
  ungroup() %>%
  ggplot(aes(y = sales,
             x = year,
             shape = state,
             linetype = state)) +
  geom_line() +
  geom_point(size = 2) +
  geom_vline(xintercept = 88, linetype = 4) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(1, 1, 1, 1, "cm"))


# 4 DIDのためのデータ準備 -------------------------------------------------------

# データ作成
# --- カリフォルニア州とその他という2グループのデータ
Cigar_did_sum <-
  Cigar %>%
    mutate(post = if_else(year > 87, 1, 0),
           ca = if_else(state == 5, 1, 0),
           state = factor(state),
           year_dummy = paste("D", year, sep = "_")) %>%
    group_by(post, year, year_dummy, ca) %>%
    summarise(sales = sum(sales * pop16) / sum(pop16)) %>%
    ungroup()

# データ作成
# --- カリフォルニア州とその他の州という州ごとでのデータ
Cigar_did_data <-
  Cigar %>%
    mutate(post = if_else(year > 87, 1, 0),
           ca = if_else(state == 5, 1, 0),
           state = factor(state),
           year_dummy = paste("D", year, sep = "_")) %>%
    group_by(post, ca, year, year_dummy, state) %>%
    summarise(sales = sum(sales * pop16) / sum(pop16)) %>%
    ungroup()


# 5 DIDによる分析 -------------------------------------------------------------

# (9) カリフォルニア州とその他というグループでの分析
## 2グループでのデータでの分析
Cigar_did_sum_reg <-
  lm(sales ~ ca + post + ca:post + year_dummy, data = Cigar_did_sum) %>%
    tidy() %>%
    filter(!str_detect(term, "state"),
           !str_detect(term, "year"))

## 2グループでのデータでの分析(log)
Cigar_did_sum_logreg <-
  lm(log(sales) ~ ca + post + ca:post + year_dummy, data = Cigar_did_sum) %>%
    tidy() %>%
    filter(!str_detect(term, "state"),
           !str_detect(term, "year"))

# 結果確認
Cigar_did_sum_reg %>% print()
Cigar_did_sum_logreg %>% print()


# 6 州ごとのデータでのDID分析 -------------------------------------------------------

## 州ごとのデータでの分析
Cigar_did_data_cluster <-
  Cigar_did_data %>%
    miceadds::lm.cluster(data = .,
                         sales ~ ca + state + post + ca:post + year_dummy,
                         cluster = "state") %>%
    summary()

## 結果の抽出
did_cluster_result <- Cigar_did_data_cluster[row.names(Cigar_did_data_cluster) == "ca:post",]
did_cluster_result


# 7 CaucialImapctのデータ準備 --------------------------------------------------

# 被説明変数
# --- ベクトル
Y <- Cigar %>% filter(state == 5) %>% pull(sales)

# 被説明変数
# --- 共変量として他の州の売上を抜き出し整形
X_sales <-
  Cigar %>%
    filter(state != 5) %>%
    select(state, sales, year) %>%
    spread(state, sales)

# データ結合
CI_data <-
  Y %>%
    cbind(X_sales) %>%
    select(-year) %>%
    as_tibble()


# 8 モデル構築 --------------------------------------------------------------

# 期間設定
# --- 介入が行われるデータを示す
pre_period  <- seq_len(NROW(X_sales))[X_sales$year < 88]
post_period <- seq_len(NROW(X_sales))[X_sales$year >= 88]

# モデル構築
impact <-
  CI_data %>%
    CausalImpact(pre.period = c(min(pre_period), max(pre_period)),
                 post.period = c(min(post_period), max(post_period)))


# 9 結果確認 -----------------------------------------------------------------

# 結果確認
impact %>% print()

# 結果のプロット
impact %>% plot()
