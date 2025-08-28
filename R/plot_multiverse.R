#' Plot the multiverse figure (density + heatmap strips)
#'
#' Two calling styles:
#' 1) By use case: plot_multiverse("hurricane") or plot_multiverse("beauty")
#' 2) By explicit components: plot_multiverse(df = df, outcome_var = "...",
#'    outcome_var_label = "...", strip_vars = c(...), variable_labels = c(...))
#'
#' @param use_case Optional, "hurricane" or "beauty".
#' @param output_path Optional file path to save the figure (width=11.5, height=8).
#' @param df Optional data.frame (if provided, overrides use_case mode).
#' @param outcome_var Optional string (required if df is provided).
#' @param outcome_var_label Optional string (x-axis label, required if df is provided).
#' @param strip_vars Optional character vector of decision variables (required if df is provided).
#' @param variable_labels Optional named character vector mapping strip_vars to display labels.
#' @return A patchwork/ggplot object.
#' @examples
#' \dontrun{
#' plot_multiverse("hurricane")
#' plot_multiverse("beauty")
#' }
#' @export
plot_multiverse <- function(use_case = NULL, output_path = NULL,
                            df = NULL, outcome_var = NULL, outcome_var_label = NULL,
                            strip_vars = NULL, variable_labels = NULL) {

  # Branch A: explicit components provided
  if (!is.null(df)) {
    stopifnot(!is.null(outcome_var), !is.null(outcome_var_label),
              !is.null(strip_vars), !is.null(variable_labels))
  } else {
    # Branch B: use-case mode
    if (is.null(use_case)) stop("Either provide `use_case` or explicit `df`+args.")
    prep <- prep_data(use_case = use_case)
    df                <- prep[[1]]
    outcome_var       <- prep[[2]]
    outcome_var_label <- prep[[3]]
    strip_vars        <- prep[[4]]
    variable_labels   <- prep[[5]]
  }

  if (!"significant" %in% names(df) && "p" %in% names(df)) {
    df <- df %>% dplyr::mutate(significant = .data$p < 0.05)
  }
  if (!"significant" %in% names(df) && "p_b" %in% names(df)) {
    df <- df %>% dplyr::mutate(significant = .data$p_b < 0.05)
  }
  if (!"significant" %in% names(df)) {
    stop("`df` must contain `significant` (logical) or a p-value to derive it.")
  }

  density_plot <- generate_density_plot(df, outcome_var, outcome_var_label)
  heatmap_plot <- generate_heatmap_strips(df, outcome_var, strip_vars, variable_labels)

  density_ratio <- 0.30
  heights_pair  <- c(density_ratio, 1 - density_ratio)

  combined <- density_plot / heatmap_plot + patchwork::plot_layout(heights = heights_pair)

  if (!is.null(output_path)) {
    ggplot2::ggsave(filename = output_path, plot = combined, width = 11.50, height = 8.00)
  }
  combined
}
