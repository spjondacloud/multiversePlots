#' Full multiverse plot (density + heatmap strips)
#'
#' Wrapper that combines density and heatmap plots.
#' Works in two modes:
#' 1) High-level: pass only use_case (calls prep_data()).
#' 2) Low-level: provide df/outcome_var/etc. explicitly.
#'
#' @param use_case Character. Either "hurricane" or "beauty". Ignored if df is supplied.
#' @param df Data frame with results (optional).
#' @param outcome_var Character name of outcome column.
#' @param outcome_var_label Label for the x-axis.
#' @param strip_vars Decision variables.
#' @param variable_labels Labels for decision variables.
#' @param save_path Optional file path to save the plot.
#'
#' @return A patchwork combined plot.
#' @examples
#' # High-level:
#' # plot_multiverse("hurricane")
#' # plot_multiverse("beauty", save_path = "beauty.png")
#'
#' # Low-level:
#' # prep <- prep_data("hurricane")
#' # plot_multiverse(df = prep[[1]], outcome_var = prep[[2]],
#' #                 outcome_var_label = prep[[3]], strip_vars = prep[[4]],
#' #                 variable_labels = prep[[5]])
#' @export
plot_multiverse <- function(use_case = c("hurricane","beauty"),
                            df = NULL,
                            outcome_var = NULL,
                            outcome_var_label = NULL,
                            strip_vars = NULL,
                            variable_labels = NULL,
                            save_path = NULL) {
  if (is.null(df)) {
    use_case <- match.arg(use_case)
    prep <- prep_data(use_case)
    df                <- prep[[1]]
    outcome_var       <- prep[[2]]
    outcome_var_label <- prep[[3]]
    strip_vars        <- prep[[4]]
    variable_labels   <- prep[[5]]
  }

  density_plot <- generate_density_plot(df, outcome_var, outcome_var_label)
  heatmap_plot <- generate_heatmap_strips(df, outcome_var, strip_vars, variable_labels)

  strip_levels <- sapply(strip_vars, function(v) nlevels(df[[v]]))
  total_strip_levels <- sum(strip_levels)
  heights_vector <- c(0.5 * total_strip_levels, strip_levels + 1)

  combined <- density_plot / heatmap_plot +
    patchwork::plot_layout(heights = heights_vector)

  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, combined, width = 11.5, height = 6.12, dpi = 300)
  }
  combined
}
