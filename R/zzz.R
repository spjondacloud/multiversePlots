# R/zzz.R

# roxygen 导入（让 NAMESPACE 自动增加 importFrom）
#' @importFrom magrittr %>%
#' @importFrom rlang .data :=
NULL

# 避免 R CMD check 对非标准评估(NSE)符号的 NOTE
utils::globalVariables(c(
  # 在代码里作为列名/临时列出现的符号：
  ".data", "term", "vname", "level", "label_text", "level_name",
  # 你的决策变量列名（在 prep_data / heatmap 里作为列使用）
  "k1","k2","k3","k4","k5","k6","k7"
))
