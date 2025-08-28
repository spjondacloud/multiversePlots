#' Heatmap strips for analytical decisions
#'
#' @param data Data frame (with factor decision vars).
#' @param outcome_var Name of outcome column.
#' @param strip_vars Character vector of factor decision variables.
#' @param variable_labels Named vector for display labels.
#' @param n_bins Number of x bins.
#' @param palette_option Viridis palette option.
#' @return A patchwork object.
#' @examples
#' \dontrun{
#' prep <- prep_data("hurricane")
#' generate_heatmap_strips(prep[[1]], prep[[2]], prep[[4]], prep[[5]])
#' }
#' @importFrom ggplot2 ggplot aes geom_rect geom_text scale_fill_gradient
#' @importFrom ggplot2 scale_x_continuous scale_y_discrete coord_cartesian
#' @importFrom ggplot2 theme_minimal theme element_blank
#' @importFrom dplyr mutate count group_by ungroup bind_rows filter distinct rename left_join
#' @importFrom tidyr complete
#' @importFrom rlang sym
#' @importFrom patchwork wrap_plots
#' @importFrom viridis viridis
#' @importFrom stats as.formula coef lm
#' @importFrom tibble enframe tibble
#' @export
generate_heatmap_strips <- function(data, outcome_var, strip_vars, variable_labels,
                                    n_bins = 20, palette_option = "D") {
  if (!length(strip_vars)) stop("`strip_vars` is empty.")
  for (sv in strip_vars) {
    if (!sv %in% names(data)) stop("`strip_var` not found: ", sv)
    if (!is.factor(data[[sv]])) data[[sv]] <- factor(data[[sv]])
  }

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

  ok_vars <- strip_vars[sapply(strip_vars, function(v) nlevels(data[[v]]) > 0)]
  if (length(ok_vars)) {
    reg_formula <- stats::as.formula(paste(outcome_var, "~", paste(ok_vars, collapse = " + ")))
    lm_model <- stats::lm(reg_formula, data = data)
    coefs <- stats::coef(lm_model)
    tidy_coefs <- tibble::enframe(coefs, name = "term", value = "estimate") %>%
      dplyr::filter(.data$term != "(Intercept)") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        var = ok_vars[which.max(sapply(ok_vars, function(v) startsWith(term, v)))],
        level = trimws(sub(paste0("^", var), "", term))
      ) %>% dplyr::ungroup()

    label_lookup <- tidy_coefs %>%
      dplyr::mutate(
        rounded = round(.data$estimate, 2),
        label_text = dplyr::case_when(
          .data$rounded > 0 ~ paste0("'+", formatC(.data$rounded, format = "f", digits = 2), "'"),
          .data$rounded < 0 ~ paste0("'",  formatC(.data$rounded, format = "f", digits = 2), "'"),
          TRUE ~ "'0.00'"
        )
      ) %>% dplyr::select(dplyr::all_of(c("var","level","label_text")))


    ref_labels <- lapply(ok_vars, function(var) {
      base_level <- levels(data[[var]])[1]
      tibble::tibble(var = var, level = base_level, label_text = "italic('Ref.')")
    }) %>% dplyr::bind_rows()

    label_lookup <- dplyr::bind_rows(label_lookup, ref_labels)
  } else {
    label_lookup <- tibble::tibble(var = character(), level = character(), label_text = character())
  }

  generate_one_strip <- function(df_in, varname, base_color, var_label) {
    var_sym <- rlang::sym(varname)
    out_sym <- rlang::sym(outcome_var)
    x_label_pos <- x_max + 0.01 * (x_max - x_min)

    heatmap_data <- df_in %>%
      dplyr::mutate(outcome_bin = cut(!!out_sym, breaks = breaks, include.lowest = TRUE),
                    !!var_sym := .data[[varname]]) %>%
      dplyr::count(.data$outcome_bin, !!var_sym) %>%
      dplyr::group_by(.data$outcome_bin) %>%
      dplyr::mutate(prop = .data$n / sum(.data$n)) %>%
      dplyr::ungroup() %>%
      tidyr::complete(.data$outcome_bin, !!var_sym, fill = list(prop = 0)) %>%
      dplyr::mutate(
        bin_index = as.integer(.data$outcome_bin),
        xmin = breaks[.data$bin_index],
        xmax = breaks[.data$bin_index + 1],
        !!var_sym := factor(!!var_sym, levels = levels(df_in[[varname]]))
      )

    heatmap_data <- add_variable_label_row(heatmap_data, varname, x_min, x_max)

    label_data <- heatmap_data %>%
      dplyr::filter(!is.na(!!var_sym), !is.na(.data$prop)) %>%
      dplyr::distinct(!!var_sym) %>%
      dplyr::mutate(var = varname) %>%
      dplyr::rename(level_name = !!var_sym) %>%
      dplyr::left_join(label_lookup, by = c("var", "level_name" = "level")) %>%
      dplyr::mutate(
        !!var_sym := factor(.data$level_name, levels = levels(heatmap_data[[varname]]))
      )

    ggplot2::ggplot(heatmap_data, ggplot2::aes(y = !!var_sym, fill = .data$prop)) +
      ggplot2::geom_rect(ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                      ymin = as.numeric(!!var_sym) - 0.5,
                                      ymax = as.numeric(!!var_sym) + 0.5),
                         color = NA) +
      ggplot2::geom_text(data = label_data,
                         ggplot2::aes(y = !!var_sym, label = .data$label_text),
                         x = x_label_pos, inherit.aes = FALSE, hjust = 0, size = 3, parse = TRUE) +
      ggplot2::scale_fill_gradient(low = "white", high = base_color, na.value = NA) +
      ggplot2::scale_x_continuous(limits = c(x_min, x_max), expand = c(0, 0)) +
      ggplot2::scale_y_discrete(
        labels = function(labs) {
          if (!length(labs)) return(labs)
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
        plot.margin = ggplot2::margin(0, 20, 0, 10)
      )
  }

  heatmap_strips <- Map(function(varname, base_color) {
    var_label <- variable_labels[[varname]]
    if (is.null(var_label)) var_label <- varname
    generate_one_strip(data, varname, base_color, var_label)
  }, varname = strip_vars, base_color = base_colors)

  strip_heights <- sapply(strip_vars, function(v) nlevels(data[[v]]) + 1)
  patchwork::wrap_plots(heatmap_strips, ncol = 1, heights = strip_heights)
}
