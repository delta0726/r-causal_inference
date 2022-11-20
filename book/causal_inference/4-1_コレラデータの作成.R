#***************************************************************************************
# Title     : 効果検証入門
# Chapter   : 4章 差分の差分法(DID)とCausal Impact
# Theme     : 4-1 コレラデータの作成
# Date      : 2022/11/19
# Page      : P137 - P147
# URL       : https://github.com/ghmagazine/cibook
#***************************************************************************************


# ＜目次＞
# 0 準備
# 1 データ作成
# 2 データ保存


# 0 準備 ---------------------------------------------------------------------

library(tidyverse)


# 1 データ作成 ----------------------------------------------------------------

# Southwark and Vauxhall Company（sv）
sv1849 <- c(283, 157, 192, 249, 259, 226, 352, 97, 111, 8, 235, 92)
sv1854 <- c(371, 161, 148, 362, 244, 237, 282, 59, 171, 9, 240, 174)

# Lambeth Company & Southwark and Vauxhall Company（lsv）
lsv1849 <- c(256, 267, 312, 257, 318, 446, 143, 193, 243, 215, 544, 187, 153, 81, 113, 176)
lsv1854 <- c(113, 174, 270 ,93, 210, 388, 92, 58, 117, 49, 193, 303, 142, 48, 165, 132)

# データ結合
# --- 時系列方向
sv_death <- c(sv1849, sv1854)
lsv_death <- c(lsv1849, lsv1854)


# ラベル作成（地域）
# --- どのデータがどのエリアのものか
sv_area <- str_c("sv_", c(seq_along(sv1849), seq_along(sv1854)))
lsv_area <- str_c("lsv_", c(seq_along(lsv1849), seq_along(lsv1854)))


# ラベル作成（時点）
sv_year <- c(rep("1849", length(sv1849)), rep("1854", length(sv1854)))
lsv_year <- c(rep("1849",length(lsv1849)), rep("1854", length(lsv1854)))

# データフレームで整理（企業ごと）
# --- Southwark & Vauxhall
# --- Lambeth & Southwark and Vauxhall
sv <-
  tibble(area = sv_area,
         year = sv_year,
         death = sv_death,
         LSV = "0",
         company = "Southwark and Vauxhall")

lsv <-
  tibble(area = lsv_area,
         year = lsv_year,
         death = lsv_death,
         LSV = "1",
         company = "Lambeth & Southwark and Vauxhall")

# データフレーム作成
# --- 企業ごとのデータを結合
JS_df <-
  sv %>%
    bind_rows(lsv) %>%
    mutate(LSV =if_else(company == "Lambeth & Southwark and Vauxhall", 1, 0))


# 2 データ保存 ----------------------------------------------------------------

# データ保存
JS_df %>% write_csv("csv/JS_df.csv")
