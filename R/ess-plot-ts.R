#' Plot a time series
#'
#' @description
#' This function configures and plots a time series dataframe downloaded from
#' ESSENCE.
#'
#' @param df A time series dataframe downloaded from ESSENCE.
#' @param title A plot title.
#' @param show_alerts Logical: Should the plot show ESSENCE alerts? Affects
#' point shape and color, as well as tooltip if `interactive = TRUE`.
#' @param interactive Logical: Should the plot be interactive (via plotly) or
#' static (via ggplot2)?
#' @param width,height The plot dimensions in pixels. Only used if `interactive
#' = TRUE`.
#' @param wrap_years Logical: If the dataset contains multiple years, wrap the
#' plot grouped by year using [ggplot2::facet_wrap()]?
#'
#' @inheritSection ess_build_url Rnssp package
#'
#' @family essence helpers
#'
#' @export
#'
#' @examples
#' # Static plot
#' df <- data.frame(
#'   date = seq(as.Date("2024-05-26"), as.Date("2024-06-14")),
#'   count = c(0, 0, 1, 3, 4, 2, 6, 9, 4, 5, 3, 2, 1, 1, 0, 0, 0, 1, 0, 0),
#'   color_id = c(0, 0, 0, 1, 2, 1, 3, 3, 2, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
#' )
#'
#' ess_plot_ts(
#'   df, title = "ED visits for unspecified syndrome, May-Jun 2024",
#'   interactive = FALSE
#' )
#'
#' # Interactive plot
#' df <- data.frame(
#'   date = c(
#'     seq(as.Date("2024-05-26"), as.Date("2024-06-14")),
#'     seq(as.Date("2025-05-26"), as.Date("2025-06-14"))
#'   ),
#'   count = c(
#'     0, 0, 1, 3, 4, 2, 6, 9, 4, 5, 3, 2, 1, 1, 0, 0, 0, 1, 0, 0,
#'     1, 0, 1, 0, 0, 0, 2, 0, 0, 1, 3, 5, 4, 5, 3, 1, 1, 0, 1, 0
#'   ),
#'   color_id = c(
#'     0, 0, 0, 1, 2, 1, 3, 3, 2, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 3, 2, 2, 1, 0, 0, 0, 0, 0
#'   )
#' )
#'
#' ess_plot_ts(
#'   df, title = "ED visits for unspecified syndrome, May-Jun 2024 & 2025",
#'   show_alerts = FALSE, wrap_years = TRUE
#' )
#'
ess_plot_ts <- function(
  df,
  title = NULL,
  show_alerts = TRUE,
  interactive = TRUE,
  width = NULL,
  height = NULL,
  wrap_years = FALSE
) {
  pkg1 <- requireNamespace("ggplot2", quietly = TRUE)

  pkg2 <- requireNamespace("scales", quietly = TRUE)

  pkg3 <- requireNamespace("plotly", quietly = TRUE)

  if (!pkg1 | !pkg2 | !pkg3) {
    stop(paste(
      "The ggplot2, scales, and plotly packages must be installed",
      "to use this function"
    ))
  }

  if (show_alerts) {
    df <- ess_config_alerts(df)
  } else {
    df$count <- as.numeric(df$count)
  }

  if (wrap_years) {
    df <- add_plot_date(df)
  }

  plot <- ess_ts_ggplot(df, alerts = show_alerts, wrap = wrap_years)

  if (interactive) {
    ess_ts_plotly(df, plot, title, show_alerts, width, height, wrap_years)
  } else {
    plot +
      ggplot2::labs(title = title) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = .5))
  }
}

add_plot_date <- function(df) {
  # Add `year` variable
  df$year <- format(df$date, "%Y")

  # Add `plot_date` variable - all dates in the same year so axes align
  yrs <- as.numeric(sort(unique(df$year)))

  # If any leap years in dataset, use 366 day year for `plot_date`
  if (any(yrs %% 4 %in% 0)) {
    yr <- yrs[yrs %% 4 %in% 0]
  } else {
    yr <- yr[1]
  }

  df$plot_date <- as.Date(paste0(yr, "-", format(df$date, "%m-%d")))

  df
}

ess_config_alerts <- function(df) {
  status <- c("Normal", "Warning", "Anomaly")

  fill <- c("#0703fc", "#f2c00a", "#ff0000")

  color <- c("#04029e", "#a17f03", "#a30202")

  shape <- c(21, 23, 24)

  df$count <- as.numeric(df$count)

  df$alert_status <- df$color_id

  df$alert_status[df$alert_status == 0] <- 1

  df$alert_status <- factor(status[df$alert_status], levels = status)

  df$alert_fill <- fill[df$alert_status]

  df$alert_color <- color[df$alert_status]

  df$alert_symbol <- shape[df$alert_status]

  df
}

ess_ts_ggplot <- function(df, alerts, wrap) {
  # Number of integer breaks for y-axis
  n_breaks <- ifelse(max(df$count) < 5, max(df$count), 5)

  # Tooltip text
  if (alerts) {
    df$tt <- paste(
      "<b>Date:</b>", format(df$date, "%b %d, %Y"),
      "<br><b>Count:</b>", df$count,
      "<br><b>Alert status:</b>", df$alert_status
    )
  } else {
    df$tt <- paste(
      "<b>Date:</b>", format(df$date, "%b %d, %Y"),
      "<br><b>Count:</b>", df$count
    )
  }

  if (wrap) {
    p <- ggplot2::ggplot(df, ggplot2::aes(
      x = .data$plot_date,
      y = .data$count,
      group = .data$year,
      text = .data$tt
    )) +
      ggplot2::facet_wrap(~year, ncol = 1) +
      ggplot2::scale_x_date(date_labels = "%b")
  } else {
    df$gp <- 1

    p <- ggplot2::ggplot(df, ggplot2::aes(
      x = .data$date,
      y = .data$count,
      group = .data$gp,
      text = .data$tt
    )) +
      ggplot2::scale_x_date(date_labels = "%b %Y")
  }

  p <- p +
    ggplot2::geom_line()

  if (alerts) {
    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(
          fill = .data$alert_fill,
          color = .data$alert_color,
          shape = .data$alert_symbol
        ),
        size = 2
      ) +
      ggplot2::scale_fill_identity() +
      ggplot2::scale_color_identity() +
      ggplot2::scale_shape_identity()
  } else {
    p <- p +
      ggplot2::geom_point()
  }

  p +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n_breaks)) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(color = "#ddd"),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#ddd"),
      panel.grid.minor.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(x = "Date", y = "Count")
}

ess_ts_plotly <- function(
  df,
  plot,
  title = NULL,
  alerts,
  width = NULL,
  height = NULL,
  wrap
) {
  plot <- plot |>
    plotly::ggplotly(
      width = width,
      height = height,
      tooltip = "text"
    ) |>
    plotly::layout(
      font = list(family = "sans-serif"),
      hoverlabel = list(
        bgcolor = "white",
        font = list(color = "black"),
        align = "left"
      )
    )

  if (!is.null(title)) {
    plot <- plot |>
      plotly::layout(
        title = list(
          text = title,
          automargin = TRUE,
          pad = list(t = 5)
        )
      )
  }

  if (wrap & !is.null(title)) {
    plot <- plot |>
      plotly::layout(margin = list(t = 60))
  }

  plot
}
