#' Plot a time series with highcharter
#'
#' @description
#' This function configures and plots a time series using the highcharter
#' package. The output is an interactive plot with alerts and tooltips that
#' mimic the time series plot within the ESSENCE software.#'
#'
#' @param df A time series dataframe downloaded from ESSENSE.
#' @param title A plot title.
#'
#' @inheritSection ess_build_url Rnssp package
#'
#' @family essence helpers
#'
#' @returns An object of classes "highchart" and "htmlwidget".
#' @export
#'
#' @examples
#' \dontrun{
#' ess_plot_timeseries(my_ts, title = "My tremendous time series")
#' }
#'
ess_plot_timeseries <- function(df, title = NULL) {
  pkg1 <- requireNamespace("highcharter", quietly = TRUE)

  pkg2 <- requireNamespace("htmlwidgets", quietly = TRUE)

  if (!pkg1 | !pkg2) {
    stop(paste(
      "The highcharter and htmlwidgets packages must be installed",
      "to use this function"
    ))
  }

  df <- ess_config_alerts(df)

  ls <- ess_listify_ts(df)

  ess_plot_ts(ls, title)
}

# Configure time series data for plotting
ess_config_alerts <- function(df) {
  # Add alert status, color, symbol, and radius
  lvl <- c("Normal", "Warning", "Anomaly")

  fill <- c("#0703fc", "#f2c00a", "#ff0000")

  clr <- c("#04029e", "#a17f03", "#a30202")

  shp <- c("circle", "diamond", "triangle")

  df <- df |>
    dplyr::mutate(
      alert_status = dplyr::case_when(
        color_id == 0 ~ lvl[1],
        color_id == 1 ~ lvl[1],
        color_id == 2 ~ lvl[2],
        color_id == 3 ~ lvl[3]
      ),
      alert_status = factor(.data$alert_status, levels = lvl),
      alert_fill = fill[.data$alert_status],
      alert_color = clr[.data$alert_status],
      alert_symbol = shp[.data$alert_status],
      alert_radius = 5,
      alert_line = 1
    )
}

# Convert a dataframe to a list for plotting with highcharter
ess_listify_ts <- function(df) {
  df$date <- as.POSIXct(paste(df$date, "12:00:00"))

  list(
    list(
      data = lapply(1:nrow(df), \(r) {
        list(
          x = highcharter::datetime_to_timestamp(df[r, "date"]),
          y = df[r, "count"],
          color = df[r, "alert_fill"],
          marker = list(
            symbol = df[r, "alert_symbol"],
            radius = df[r, "alert_radius"],
            lineWidth = df[r, "alert_line"],
            lineColor = df[r, "alert_color"]
          ),
          alert_status = df[r, "alert_status"]
        )
      })
    )
  )
}

# Plot a time series using highcharter
ess_plot_ts <- function(ls, title) {
  plot <- highcharter::highchart() |>
    highcharter::hc_add_series_list(ls) |>
    highcharter::hc_xAxis(
      type = "datetime",
      title = list(text = "Date"),
      labels = list(format = "{value:%b %d}")
    ) |>
    highcharter::hc_yAxis(
      title = list(text = "Count")
    ) |>
    highcharter::hc_legend(enabled = FALSE) |>
    highcharter::hc_tooltip(formatter = htmlwidgets::JS(
      "function() {
        const dt = new Date(this.x);
        return dt.toDateString() + '<br>' +
        `Count: <b>${this.y}</b>` + '<br>' +
        `Alert status: <b>${this.point.alert_status}</b>`;
      }"
    ))

  if (is.null(title)) {
    plot
  } else {
    highcharter::hc_title(plot, text = title)
  }
}
