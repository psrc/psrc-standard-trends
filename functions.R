echarts4r::e_common(font_family = "Poppins")

tooltip_fmt <- function(est, fill = NULL) {
  if(est == 'number') est <- 'decimal'
  style_quoted <- paste0('"', est, '"')
  
  # adjust maximumFractionDigits
  if(est %in% c('percent', 'currency')) {
    max_fd <- 0
  } else {
    max_fd <- 2
  }
  
  if(!is.null(fill)) {
    r <- "params.seriesName + '<br>' + params.marker + ' ' + params.value[1] + ': ' + fmt.format(params.value[0])"
  } else {
    # When data is not grouped by a variable & filled with color
    r <- "params.seriesName + '<br>' + params.marker + fmt.format(params.value[1])"
  }
  
  t <- paste0("function(params, ticket, callback) {
                  let fmt = new Intl.NumberFormat('en', {style:", style_quoted, ", minimumFractionDigits:0, maximumFractionDigits:", max_fd, ", currency: 'USD'});
                  return(", r, ")","}")
}

generic_echart <- function(df,
                           category_var,
                           fill = NULL,
                           color = NULL,
                           est = "percent",
                           title = NULL,
                           subtitle = NULL,
                           legend = FALSE) {
  
  # Create the most basic chart
  if(is.null(fill)) {
    p <- df
  } else {
    p <- df |>
      dplyr::group_by(.data[[fill]]) 
  }
  
  p <- p |>
    echarts4r::e_charts_(category_var) |>
    echarts4r::e_color(color) |>
    echarts4r::e_title(title, subtitle) |>
    echarts4r::e_grid(left = '15%') |>
    echarts4r::e_x_axis(axisTick = list(show = FALSE),
                        axisLabel = list(fontSize = 14,
                                         family = "Poppins",
                                         interval = 0)) |>
    echarts4r::e_y_axis(axisTick = list(show = FALSE),
                        axisLabel = list(fontSize = 14,
                                         family = "Poppins",
                                         interval = 0)) |>
    echarts4r::e_show_loading() |>
    echarts4r::e_legend(show = legend, bottom = 0) |>
    echarts4r::e_toolbox_feature("dataView") |>
    echarts4r::e_toolbox_feature("saveAsImage") 
  
  if(est == "number") {
    p <- p |> echarts4r::e_tooltip(trigger = "item")
    
  } else {
    p <- p |>
      echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter(est))
  }
  
  p <- p |>
    echarts4r::e_tooltip(formatter =  htmlwidgets::JS(tooltip_fmt(est, fill)))
  
  return(p)
}

echart_bar_chart <- function(t,
                             x,
                             y,
                             pos = "NULL",
                             column_vs_bar = "column", ...) {
  
  p <- generic_echart(df = t, category_var = x, ...) |>
    echarts4r::e_bar_(y, stack = pos) 
  
  if(column_vs_bar == "bar") {
    p <- p |>
      echarts4r::e_flip_coords() |>
      echarts4r::e_y_axis(inverse = TRUE)
  }
  return(p)
}

echart_line_chart <- function(t,
                              x,
                              y, 
                              ...) {
  
  p <- generic_echart(df = t, category_var = x, ...) |>
    echarts4r::e_line_(y)
  
  return(p)
}

echart_pie_chart <- function(t, lab, val, color, legend=TRUE) {
  
  p <- t |>
    echarts4r::e_charts_(lab) |>
    echarts4r::e_pie_(val, radius = c("50%", "70%")) |> 
    echarts4r::e_color(color) |>
    echarts4r::e_toolbox_feature("dataView") |>
    echarts4r::e_toolbox_feature("saveAsImage") |>
    echarts4r::e_tooltip() |>
    echarts4r::e_legend(show = legend)
  
  return(p)
  
}
