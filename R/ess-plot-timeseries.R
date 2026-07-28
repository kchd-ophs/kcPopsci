#' Plot a time series
#'
#' @description
#' This function configures and plots a time series dataframe downloaded from
#' ESSENCE.
#'
#' @param df A time series dataframe downloaded from ESSENSE.
#' @param title A plot title.
#' @param width,height The plot dimensions in pixels.
#'
#' @inheritSection ess_build_url Rnssp package
#'
#' @family essence helpers
#'
#' @returns An object of classes "plotly" and "htmlwidget".
#' @export
#'
#' @examples
#' \dontrun{
#' ess_plot_timeseries(my_ts, title = "My tremendous time series")
#' }
#'
ess_plot_timeseries <- function(
  df,
  title = NULL,
  width = NULL,
  height = NULL
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

  df <- ess_config_alerts(df)

  ess_plot_ts(df, title, width, height)
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

ess_plot_ts <- function(df, title = NULL, width = NULL, height = NULL) {
  # Tooltip text
  tt_text <- paste(
    "<b>Date:</b>", format(df$date, "%b %d, %Y"),
    "<br><b>Count:</b>", df$count,
    "<br><b>Alert status:</b>", df$alert_status
  )

  n_breaks <- ifelse(max(df$count) < 5, max(df$count), 5)

  p <- df |>
    ggplot2::ggplot(ggplot2::aes(
      x = date,
      y = count
    )) +
    ggplot2::geom_line() +
    ggplot2::geom_point(
      ggplot2::aes(
        fill = .data$alert_fill,
        color = .data$alert_color,
        shape = .data$alert_symbol
      ),
      size = 2.5,
      stroke = .5
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::scale_shape_identity() +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n_breaks)) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#ddd"),
      panel.grid.minor.y = ggplot2::element_line(color = "#ddd")
    ) +
    ggplot2::labs(x = "Date", y = "Count")

  p <- p |>
    plotly::ggplotly(
      width = width,
      height = height
    ) |>
    plotly::style(
      text = tt_text
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
    p <- p |>
      plotly::layout(
        title = list(
          text = title,
          automargin = TRUE
        )
      )
  }

  p
}
