#' Heatmap strips for multiverse decisions
#'
#' Generates stacked heatmap strips showing the distribution of outcomes
#' across decision levels for each variable in `strip_vars`.
#'
#' @param data A data.frame containing `outcome_var` and factor columns in `strip_vars`.
#' @param outcome_var Character. Name of numeric outcome.
#' @param strip_vars Character vector of decision variables (factors).
#' @param variable_labels Named character vector: pretty labels for `strip_vars`.
#' @param n_bins Integer number of bins for the outcome (default 20).
#' @param palette_option Viridis palette option.
#'
#' @return A patchwork object (ggplot) with stacked strips.
#' @importFrom ggtext element_markdown
#' @importFrom viridis viridis
#' @importFrom dplyr mutate count group_by ungroup bind_rows filter distinct rename left_join
#' @importFrom tidyr complete
#' @importFrom rlang sym
#' @importFrom patchwork wrap_plots
#' @examples
#' # heatmap_plot <- generate_heatmap_strips(df, "edif", c("k1","k2"), c(k1="Outliers", k2="Leverage"))
#' @export
generate_heatmap_strips <- function(data, outcome_var, strip_vars, variable_labels,
                                    n_bins = 20, palette_option = "D") {
  stopifnot(all(strip_vars %in% names(data)))
  x_min <- min(data[[outcome_var]], na.rm = TRUE)
  x_max <- max(data[[outcome_var]], na.rm = TRUE)
  x_buffer <- 0.1 * (x_max - x_min)
  common_xlim <- c(x_min, x_max + x_buffer)
  breaks <- seq(x_min, x_max, length.out = n_bins + 1)

  base_colors <- viridis::viridis(length(strip_vars), option = palette_option)

  add_variable_label_row <- function(data_in, varname, x_min_loc, x_max_loc) {
    orig_levels <- levels(data_in[[varname]])
    new_levels <- rev(c("label", orig_levels))
    data_in[[varname]] <- factor(data_in[[varname]], levels = new_levels)
    label_row <- data.frame(outcome_bin = NA, prop = NA, xmin = x_min_loc, xmax = x_max_loc, level = 0)
    label_row[[varname]] <- factor("label", levels = new_levels)
    for (col in setdiff(names(data_in), names(label_row))) label_row[[col]] <- NA
    dplyr::bind_rows(label_row[, names(data_in)], data_in)
  }

  #label
  label_lookup <- do.call(dplyr::bind_rows, lapply(strip_vars, function(v) {
    tibble::tibble(var = v, level = levels(data[[v]]), label_text = "italic('Ref.')") |>
      dplyr::mutate(label_text = ifelse(level == levels(data[[v]])[1], "italic('Ref.')", "' '"))
  }))

  generate_one_strip <- function(df_in, varname, base_color, var_label) {
    var_sym <- rlang::sym(varname)
    out_sym <- rlang::sym(outcome_var)
    x_label_pos <- x_max + 0.01 * (x_max - x_min)

    heatmap_data <- df_in |>
      dplyr::mutate(outcome_bin = cut(!!out_sym, breaks = breaks, include.lowest = TRUE),
                    !!var_sym := .data[[varname]]) |>
      dplyr::count(outcome_bin, !!var_sym) |>
      dplyr::group_by(outcome_bin) |>
      dplyr::mutate(prop = n / sum(n)) |>
      dplyr::ungroup() |>
      tidyr::complete(outcome_bin, !!var_sym, fill = list(prop = 0)) |>
      dplyr::mutate(
        bin_index = as.integer(outcome_bin),
        xmin = breaks[bin_index],
        xmax = breaks[bin_index + 1],
        !!var_sym := factor(!!var_sym, levels = levels(df_in[[varname]]))
      )

    heatmap_data <- add_variable_label_row(heatmap_data, varname, x_min, x_max)

    label_data <- heatmap_data |>
      dplyr::filter(!is.na(!!var_sym), !is.na(prop)) |>
      dplyr::distinct(!!var_sym) |>
      dplyr::mutate(var = varname) |>
      dplyr::rename(level_name = !!var_sym) |>
      dplyr::left_join(label_lookup, by = c("var", "level_name" = "level")) |>
      dplyr::mutate(
        !!var_sym := factor(level_name, levels = levels(heatmap_data[[varname]]))
      )

    ggplot2::ggplot(heatmap_data, ggplot2::aes(y = !!var_sym, fill = prop)) +
      ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax,
                                      ymin = as.numeric(!!var_sym) - 0.5,
                                      ymax = as.numeric(!!var_sym) + 0.5),
                         color = NA) +
      ggplot2::geom_text(data = label_data,
                         ggplot2::aes(y = !!var_sym, label = label_text),
                         x = x_label_pos, inherit.aes = FALSE, hjust = 0, size = 3, parse = TRUE) +
      ggplot2::scale_fill_gradient(low = "white", high = base_color, na.value = NA) +
      ggplot2::scale_x_continuous(limits = c(x_min, x_max), expand = c(0, 0)) +
      ggplot2::scale_y_discrete(
        labels = function(labs) {
          first_level <- levels(df_in[[varname]])[1]
          labs <- ifelse(labs == "label", paste0("**", var_label, "**"),
                         ifelse(labs == first_level, paste0("*", labs, "*"), labs))
          labs
        },
        expand = c(0, 0)
      ) +
      ggplot2::coord_cartesian(xlim = common_xlim) +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggtext::element_markdown(),
        panel.grid = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(0, 40, 0, 120)
      )
  }

  plots <- Map(function(v, col) {
    generate_one_strip(data, v, col, variable_labels[[v]])
  }, v = strip_vars, col = base_colors)

  heights <- sapply(strip_vars, function(v) nlevels(data[[v]]) + 1)
  patchwork::wrap_plots(plots, ncol = 1, heights = heights)
}
