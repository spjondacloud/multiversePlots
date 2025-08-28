# R/zzz.R

# roxygen imports (automatically add importFrom to NAMESPACE)
#' @importFrom magrittr %>%
#' @importFrom rlang .data :=
NULL

# Avoid R CMD check NOTE for non-standard evaluation (NSE) symbols
utils::globalVariables(c(
  # Symbols that appear in the code as column names / temporary columns:
  ".data", "term", "vname", "level", "label_text", "level_name",
  # Decision variable column names (used as columns in prep_data / heatmap)
  "k1","k2","k3","k4","k5","k6","k7"
))
