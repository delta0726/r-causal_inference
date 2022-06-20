#***************************************************************************************
# Title     : 効果検証入門
# Chapter   : 1章 セレクションバイアスとRCT
# Theme     : 2-2 回帰分析におけるバイアス
# Date      : 2022/06/21
# Page      : P49 - P66
# URL       : https://github.com/ghmagazine/cibook
#***************************************************************************************


# ＜ポイント＞
# - 回帰分析でセレクションバイアスが小さくなるように推定するには、共変量を正しく選択する必要がある
#   --- 共変量とセレクションバイアスの関係性を理解する
#   --- 回帰分析アプローチの仮定の難しさを確認する


# ＜目次＞
# 0 準備
# 1 回帰分析におけるバイアス
# 2 脱落変数バイアス(OVB)
# 3 OVBをモダンなアプローチで算出
# 4 モデルに不要な変数を入れる弊害


# 0 準備 -------------------------------------------------------------------------

# ＜ポイント＞
# - ECサイトのユーザーに対してRCTを適用したメールマーケティングを行ったデータを使用（第1章と同様）


# ライブラリ
library(tidyverse)
library(broom)


# データロード
# --- E-mailのバイアスありデータ（1-4で作成）
email_data <- read_csv("csv/E-MailAnalytics.csv")
male_df <- read_csv("csv/E-MailAnalytics_male.csv")

# データ概要
email_data %>% as_tibble()
email_data %>% glimpse()

male_df %>% as_tibble()
male_df %>% glimpse()


# 1 回帰分析におけるバイアス --------------------------------------------------------------

# ＜ポイント＞
# - 回帰分析でセレクションバイアスが小さくなるような推定を行うには共変量を正しく選択する必要がある
#   --- バイアスを及ぼす共変量をモデルに加えることでバイアスを緩和することができる
#   --- 共変量による重回帰によりRCTに近づけることができる（完全ではない）


# 回帰分析の比較
# --- RCTデータ
# --- バイアスのあるデータ
rct_reg_coef <- lm(spend ~ treatment, data = male_df) %>% tidy()
nonrct_reg_coef <- lm(spend ~ treatment, data = biased_data) %>% tidy()

# 確認
# --- RCTデータ： 0.770（真値：1-4で測定した値と同じ）
# --- バイアスのあるデータ： 0.979（セレクションバイアスで効果が過剰に推定）
rct_reg_coef %>% print()
nonrct_reg_coef %>% print()

# 回帰係数の追加
# --- バイアスのあるデータに共変量を追加して推定
# --- 回帰係数は0.847（真値に近づいた）
nonrct_mreg_coef <- lm(spend ~ treatment + recency + channel + history, data = biased_data) %>% tidy()
nonrct_mreg_coef %>% print()


# 2 脱落変数バイアス(OVB) ---------------------------------------------------------------------

# ＜ポイント＞
# - セレクションバイアスを小さくするには、共変量は目的変数(Y)と介入変数(Z)に対して相関を持つ共変量を選択すべき
# - OVBとは共変量がモデルに入らないことによる機会コスト
#   --- 2つのモデルのtreatを比較することで算出可能
#   --- 脱落変数を被説明変数としたモデルなどからも算出可能


# モデルA(脱落モデル) ***************************************

# ＜ポイント＞
# - ｢history｣抜きの回帰モデルを構築（バイアスをかけた変数を1つ見逃す）


# モデルA
short_coef <-
   lm(spend ~ treatment + recency + channel, data = biased_data) %>%
     tidy()

# 確認
# --- 回帰係数は0.875（本来モデルは0.847、真値0.770から乖離）
short_coef %>% print()

# パラメータ抽出
# --- treatment(介入効果)
alpha_1 <-
  short_coef %>%
    filter(term == "treatment") %>%
    pull(estimate)


# モデルB(本来モデル) ***************************************

# ＜ポイント＞
# - ｢history｣ありの回帰モデルを構築（1と同様のモデル）


# モデルB
long_coef <-
   lm(spend ~ treatment + recency + channel + history, data = biased_data) %>%
    tidy()

# 確認
# --- 回帰係数は0.847（真値に近づいた）
long_coef %>% print()

# パラメータ抽出
beta_1 <- long_coef %>% filter(term == "treatment") %>% pull(estimate)
beta_2 <- long_coef %>% filter(term == "history") %>% pull(estimate)


# モデルCの評価 *******************************************

# ＜ポイント＞
# - 脱落変数を被説明変数としてモデル構築


# モデルC
# --- 脱落した変数と介入変数
omitted_coef <-
  lm(history ~ treatment + channel + recency, data = biased_data) %>%
    tidy()

# 確認
# --- 回帰係数は27.2
omitted_coef %>% print()

# パラメータ抽出
# --- cの結果から介入変数に関するパラメーターを取り出す
gamma_1 <- omitted_coef %>% filter(term == "treatment") %>% pull(estimate)

# OVBの確認
# --- 各モデルのtreatmentの係数
alpha_1 - beta_1
beta_2 * gamma_1


# 3 OVBをモダンなアプローチで算出 --------------------------------------------------------

# ＜ポイント＞
# - OVBの算出をRのネストを活用して行う
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


# 4 モデルに不要な変数を入れる弊害 ------------------------------------------------

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