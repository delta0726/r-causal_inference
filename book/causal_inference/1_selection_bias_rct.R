#***************************************************************************************
# Title     : 効果検証入門
# Chapter   : 1章 セレクションバイアスとRCT
# Date      : 2020/8/13
# Page      : P1 - P38
# URL       : https://github.com/ghmagazine/cibook
#***************************************************************************************

# ＜概要＞
# - RCT(Randomized Controlled Trail)とは、あらゆる正y句を取り払った際に実行できる検証方法
#   --- 現実の世界ではRCTは実施することはできない（タイムマシンが必要）
#   --- そこで、RCTの結果が再現できるような検証方法を構築することを目指す


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 RCTデータ集計
# 3 有意差検定
# 4 バイアスありのデータ作成


# 0 準備 ------------------------------------------------------------------

library(tidyverse)


# データロード
# --- ECサイトのユーザーに対してRCTを適用したメールマーケティングを行ったデータ
email_data <- read_csv("csv/E-MailAnalytics.csv")

# データ概要
email_data %>% as_tibble()
email_data %>% glimpse()


# 1 データ準備 -------------------------------------------------------------

# マーケティング選択肢(介入)の確認
# --- ｢男性向けメール｣｢女性向けメール｣｢メール配信なし｣の3パターンがある
email_data$segment %>% unique()
email_data %>% count(segment)

# データ準備
# --- ｢男性向けメール｣と｢メール配信なし｣に限定したデータを作る
# --- 女性向けメールが配信されたデータを削除
male_df <-
  email_data %>%
    filter(segment != "Womens E-Mail") %>%
    mutate(treatment = ifelse(segment == "Mens E-Mail", 1, 0))

# 確認
male_df %>% as_tibble()
male_df %>% glimpse()
male_df %>% count(treatment)


# 2 RCTデータ集計 -------------------------------------------------------------

# ＜ポイント ＞
# - 男性でメールを送った人はconversion(購入者割合)もspend(購入金額)も大きいように見える
#   ---- 差が有意なものかを検定


# 集計による比較
# --- treatment: 1は｢男性向けメール｣を配信
# --- conversion: メールが配信されて2週間以内に購入したかを1/0で表示
# --- spend: 実際に購入した金額
summary_by_segment <-
  male_df %>%
    group_by(treatment) %>%
    summarise(conversion_rate = mean(conversion),
              spend_mean = mean(spend),
              count = n())

# 確認
# --- メールを配信しているグループのほうがconversionが0.677%高い（1.25%-0.573%）
# --- メールが配信されると売上が発生しやすくなる
summary_by_segment %>% print()


# 3 有意差検定 -------------------------------------------------------------

# ＜ポイント＞
# - 集計しているデータはRCTによって得られたデータ（セレクションバイアスの問題はない）
# - 有意差検定を行って帰無仮説が棄却されたら偶然の範囲の誤差かを判定
#   --- 差の検定で有意性が確認


# 有意差検定(t検定)
# --- treatmentが1/0のspendをベクトルで取得
# --- 上記の平均差に対して検定を行う
# --- ｢var.equal = T｣は等分散性を仮定している
mens_mail <- male_df %>% filter(treatment == 1) %>% pull(spend)
no_mail   <- male_df %>% filter(treatment == 0) %>% pull(spend)
rct_ttest <- t.test(mens_mail, no_mail, var.equal = T)


# 確認
# --- ｢mean of x｣｢mean of y ｣は前述の集計結果と同じ
# --- p値が0に極めて近くt値も5.3（差がゼロであることを否定する結果：統計的に有意）
rct_ttest %>% print()



# 4 バイアスありのデータ作成 -------------------------------------------------------------

# ＜仮定＞
# - RCTデータから男性のうち購買傾向が一定以上であるユーザーを抽出してメール配信（セレクションバイアス）
#   ---


# seedを固定する
set.seed(1)


# 閾値の設定
# --- 条件に反応するサンプルの量を半分にする
obs_rate_c <- 0.5
obs_rate_t <- 0.5


## バイアスのあるデータの作成
# --- 購入しそうな顧客をフィルタリング
# --- history: 昨年の購入額（多い）
# --- recency: 経過月数（短い）
# --- channel: 購入チャネル（マルチチャネル）
# --- treatment: 1(配信した) / 0(配信しなかった)
biased_data <-
  male_df %>%
    mutate(obs_rate_c = ifelse((history > 300) | (recency < 6) | (channel == "Multichannel"), obs_rate_c, 1),
           obs_rate_t = ifelse((history > 300) | (recency < 6) | (channel == "Multichannel"), 1, obs_rate_t),
           random_number = runif(n = nrow(male_df))) %>%
    filter((treatment == 0 & random_number < obs_rate_c) |
             (treatment == 1 & random_number < obs_rate_t) )


# データ確認
biased_data %>% select(history, recency, channel, obs_rate_c, obs_rate_t, random_number, treatment)
biased_data %>% glimpse()


# 集計による比較
# --- conversion: メールが配信されて2週間以内に購入したか
# --- spend: 実際に購入した金額
summary_by_segment_biased <-
  biased_data %>%
    group_by(treatment) %>%
    summarise(conversion_rate = mean(conversion),
              spend_mean = mean(spend),
              count = n())


# 確認
# --- treatmentごとの差が大きくなっている（conversion_rateで確認）
# --- セレクションバイアスが発生した可能性を示唆
summary_by_segment_biased %>% print()


# 3 t検定 -------------------------------------------------------------

# t検定を行う
# --- treatmentが1/0のspendをベクトルで取得
# --- 上記の平均差に対して検定を行う
# --- ｢var.equal = T｣は等分散性を仮定している
mens_mail_biased <- biased_data %>% filter(treatment == 1) %>% pull(spend)
no_mail_biased   <- biased_data %>% filter(treatment == 0) %>% pull(spend)
rct_ttest_biased <- t.test(mens_mail_biased, no_mail_biased, var.equal = T)


# 確認
# --- p値がさらに小さくなっている（セレクションバイアス）
rct_ttest_biased %>% print()
