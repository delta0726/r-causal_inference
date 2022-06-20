#***************************************************************************************
# Title     : 効果検証入門
# Chapter   : 2章 介入効果を測るための回帰分析
# Theme     : 2-1 回帰分析の導入
# Date      : 2022/06/21
# Page      : P40 - P66
# URL       : https://github.com/ghmagazine/cibook
#***************************************************************************************


# ＜ポイント＞
# - セレクションバイアスを取り除く最も基本的な方法は回帰分析である
#   --- 因果推論における回帰分析の使い方(視点)を確認する


# ＜目次＞
# 0 準備
# 1 バイアスありデータの作成
# 2 回帰分析の実行
# 3 回帰分析におけるバイアス
# 4 脱落変数バイアス(OVB)
# 5 OVBをmap()を使ってアプローチ
# 6 モデルに不要な変数を入れる弊害


# 0 準備 -------------------------------------------------------------------------

# ＜ポイント＞
# - ECサイトのユーザーに対してRCTを適用したメールマーケティングを行ったデータを使用（第1章と同様）

# ライブラリ
library(tidyverse)
library(broom)

# データ準備
# --- ECサイトのユーザーに対してRCTを適用したメールマーケティングを行ったデータ
email_data <- read_csv("csv/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge.csv")

# データの準備
# --- 女性向けメールが配信されたデータを削除したデータを作成
# --- 問題の単純化するため
# --- treatment: 男性でEmailを送った人
male_df <-
  email_data %>%
    filter(segment != "Womens E-Mail") %>%
    mutate(treatment = ifelse(segment == "Mens E-Mail", 1, 0))


# 1 バイアスありデータの作成 ----------------------------------------------------------

# seedを固定する
set.seed(1)

# 閾値の設定
# --- 条件に反応するサンプルの量を半分にする
obs_rate_c <- 0.5
obs_rate_t <- 0.5

# バイアスのあるデータの作成
biased_data <-
  male_df %>%
    mutate(obs_rate_c = ifelse((history > 300) | (recency < 6) | (channel == "Multichannel"), obs_rate_c, 1),
           obs_rate_t = ifelse((history > 300) | (recency < 6) | (channel == "Multichannel"), 1, obs_rate_t),
           random_number = runif(n = NROW(male_df))) %>%
    filter((treatment == 0 & random_number < obs_rate_c) |
             (treatment == 1 & random_number < obs_rate_t) )


# 2 回帰分析の実行 ---------------------------------------------------------------------

# ＜ポイント＞
# - 介入変数をダミー変数にして回帰モデルを解くことで効果を測定する
# - 因果推論における回帰では回帰係数のみに着目（予測など行わない）

# データ確認
biased_data %>% select(spend, treatment, history)

# バイアスのあるデータでの回帰分析
# --- 目的変数： spend:(購入額)
# --- 介入変数： treatment: 1(配信した) / 0(配信しなかった)
# --- 共変量  ： history: 昨年の購入額
biased_reg <- lm(spend ~ treatment + history, data = biased_data)

# 回帰パラメーターをtibbleで抽出
# --- treatmentは0.9026でp値も小さいので帰無仮説を棄却することができる
# --- メールを送信することで売上が平均0.9程度増加する
biased_reg_coef <- biased_reg %>% tidy()
biased_reg_coef %>% print()


# 3 回帰分析におけるバイアス --------------------------------------------------------------

# ＜ポイント＞
# - 回帰分析でセレクションバイアスが小さくなるような推定を行うには共変量を正しく選択する必要がある
#   --- 共変量とセレクションバイアスの関係を知る必要がある
# - バイアスを及ぼす共変量を回帰モデルに加えることでバイアスを緩和することができる
#   ---- 完全にOVBとなるわけではない


# 回帰分析の比較
# --- RCTデータ
# --- バイアスのあるデータ
rct_reg_coef <- lm(spend ~ treatment, data = male_df) %>% tidy()
nonrct_reg_coef <- lm(spend ~ treatment, biased_data) %>% tidy()

# 確認
# --- RCTデータ： 0.770
# --- バイアスのあるデータ： 0.979（セレクションバイアスで効果が過剰に推定されている）
rct_reg_coef %>% print()
nonrct_reg_coef %>% print()


# 回帰係数の追加
# --- バイアスをかけたデータを回帰係数として追加
# --- 回帰係数は0.847（RCTデータに近づいた）
nonrct_mreg_coef <- lm(spend ~ treatment + recency + channel + history, biased_data) %>% tidy()
nonrct_mreg_coef %>% print()


# 4 脱落変数バイアス(OVB) ---------------------------------------------------------------------

# ＜ポイント＞
# - ｢脱落変数｣とは本来モデルで必要だがリサーチャーの判断で脱落してしまった変数をいう
# - 統計的に有意でない共変量をモデルから除外してもOVBは発生する
#   --- 完全なモデルを構築するのは困難なので、現実にはある程度のOVBは生じる


# モデルAの評価 ***************************************

# モデルA
# --- ｢history｣抜きの回帰分析（バイアスをかけた変数を1つ見逃す）
short_coef <-
   lm(spend ~ treatment + recency + channel, data = biased_data) %>%
     tidy()

# 確認
# --- 回帰係数は0.875（RCTデータから遠ざかった）
short_coef %>% print()

# パラメータ抽出
# --- treatment(介入効果)
alpha_1 <-
  short_coef %>%
    filter(term == "treatment") %>%
    pull(estimate)


# モデルBの評価 ***************************************

# モデルB
long_coef <-
   lm(spend ~ treatment + recency + channel + history, data = biased_data) %>%
    tidy()

# 確認
# --- 回帰係数は0.847（RCTデータに近づいた）
long_coef %>% print()

# パラメータ抽出
beta_1 <- long_coef %>% filter(term == "treatment") %>% pull(estimate)
beta_2 <- long_coef %>% filter(term == "history") %>% pull(estimate)


# # モデルCの評価 ***************************************

# モデルC
# --- 脱落した変数と介入変数
omitted_coef <-
  lm(history ~ treatment + channel + recency, data = biased_data) %>%
    tidy()

# パラメータ抽出
# --- cの結果から介入変数に関するパラメーターを取り出す
gamma_1 <- omitted_coef %>% filter(term == "treatment") %>% pull(estimate)

# OVBの確認
# --- 各モデルのtreatmentの係数
alpha_1 - beta_1
beta_2 * gamma_1


# 5 OVBをmap()を使ってアプローチ ---------------------------------------------------------------

# ＜ポイント＞
# - 4でやったことをRのモダンなアプローチで実行する
#   --- 実行内容は同じだが、1つのデータフレームで完結する点がメリット


# モデル式の定義
# --- ベクトルで用意
formula_vec <-
  c(spend ~ treatment + recency + channel,
    spend ~ treatment + recency + channel + history,
    history ~ treatment + channel + recency) %>%
    set_names(paste("reg", LETTERS[1:3], sep ="_"))

# tibbleに格納
# --- モデル式のデータフレーム化
models <-
  formula_vec %>%
    enframe(name = "model_index", value = "formula")

# 回帰分析の実行
# --- purrr::map()で一括処理
df_models <-
  models %>%
    mutate(model = map(.x = formula, .f = lm, data = biased_data)) %>%
    mutate(lm_result = map(.x = model, .f = tidy))

# モデル結果の整形
df_results <-
  df_models %>%
    mutate(formula = as.character(formula)) %>%
    select(formula, model_index, lm_result) %>%
    unnest(cols = lm_result)

# パラメータ抽出
# --- 各モデルのtreatmentのパラメータを抜き出す
treatment_coef <-
  df_results %>%
    filter(term == "treatment") %>%
    pull(estimate)

# パラメータ抽出
# --- モデルBからhistoryのパラメータを抽出
history_coef <-
  df_results %>%
    filter(model_index == "reg_B", term == "history") %>%
    pull(estimate)


# OVBの確認
OVB <- history_coef * treatment_coef[3]
coef_gap <- treatment_coef[1] - treatment_coef[2]

# 出力
OVB %>% print()
coef_gap %>% print()


# 6 モデルに不要な変数を入れる弊害 ------------------------------------------------

# ＜ポイント＞
# - セレクションバイアスが減る可能性から、OVBがゼロでない変数は全てモデルに入れてよいわけではない
#   --- Post Treatment Bias


# ＜Post Treatment Bias＞
# - これを避けるには、介入よりも後のタイミングで値が決まるような変数は使ってはいけない
#   --- リサーチャーが適切に判断できるとは限らない


# 回帰分析で高相関な係数を探す
# --- treatmentと相関の高い係数を探す
# --- visitが0.144と有意に正
cor_visit_treatment <-
  lm(treatment ~ visit + channel + recency + history, data = biased_data) %>%
   tidy()

# 不要な変数を含むモデル
# --- 不要な変数： visit
bad_control_reg <-
  lm(spend ~ treatment + channel + recency + history + visit, data = biased_data) %>%
   tidy()

# 結果確認
# --- treatは0.294と大きく低下（RCTをも下回って実験イメージと合わない）
# --- 多重共線性が悪影響を及ぼしている
bad_control_reg %>% print()