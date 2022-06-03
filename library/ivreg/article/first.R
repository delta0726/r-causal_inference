# ***********************************************************************************************
# Library   : ivreg
# Title     : First Step
# Date      : 2022/06/04
# URL       : https://john-d-fox.github.io/ivreg/
# ***********************************************************************************************


# ＜概要＞
# - 以前にAERパッケージに含まれていたivreg()を独立してライブラリとして実装したもの
# - {broom}による操作も可能


# ＜目次＞
# 0 準備
# 1 ivreg()の実行


# 0 準備 -------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(ivreg)
library(broom)

# データ確認
Kmenta %>% as_tibble()


# 1 ivreg()の実行 ------------------------------------------------------------------

# 2段階回帰の実行
result <- ivreg(Q ~ P + D | D + F + A, data = Kmenta)

# 結果確認
result %>% print()
result %>% summary()

# {broom}による結果確認
result %>% glance()
result %>% tidy(conf.int = TRUE)
