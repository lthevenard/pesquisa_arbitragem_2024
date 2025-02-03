## Data wrangling ----

save_plot <- function(filename, path = "./output/2025_01_27/graficos", height = 6, width = 10, dpi = 300, ...) {
  ggsave(filename=filename, height=height, width=width, path=path, dpi=dpi, ...)
}

export_table <- function(df, filename, folder = "./output/2025_01_27/tabelas/") {
  df %>%
    write_excel_csv2(paste0(folder, filename))
}

perc_to_text <- function(perc, acc = 0.1) {
  scales::label_percent(accuracy=acc, decimal.mark=",")(perc)
}

perc_and_n_to_text <- function(n, perc, acc = 0.1) {
  paste0(perc_to_text(perc, acc), " (N = ", n, ")")
}

n_and_perc_to_text <- function(n, perc, acc = 0.1) {
  paste0(n, " (", perc_to_text(perc, acc), ")")
}

count_and_perc <- function(grouped_df, acc=0.1) {
  grouped_df %>% 
    summarise(n = n()) %>% 
    mutate(perc = n / sum(n),
           lbl_perc = perc_to_text(perc, acc),
           lbl_perc_n = perc_and_n_to_text(n, perc, acc),
           lbl_n_perc = n_and_perc_to_text(n, perc, acc))
}

calc_date_dist <- function(date_begin, date_end) {
  if (date_end %in% c("N/A", "N/D") | date_begin %in% c("N/A", "N/D")) {
    return(NA)
  } else {
    return(
      as.numeric(ymd(date_end)) - as.numeric(ymd(date_begin))
    )
  }
}

evolucao_temporal_ajuste <- function(p) {
  p +
    geom_vline(xintercept = "", size = 0.25) +
    scale_x_discrete(drop = FALSE, expand = expansion(mult = c(0.045, 0.045))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "bottom",
          axis.ticks.x = element_line(colour =c(rep("black", 22), "transparent", "black")))
}

from_df_to_summary <- function(df, groupname, metricname) {
  df %>%
    rename(var_metric=metricname, var_group=groupname) %>%
    group_by(var_group) %>%
    summarise(
      min=min(var_metric, na.rm=T),
      q1=quantile(var_metric, c(.25), na.rm=T),
      median=median(var_metric, na.rm=T),
      q3=quantile(var_metric, c(.75), na.rm=T),
      max=max(var_metric, na.rm=T),
      mean=mean(var_metric, na.rm=T),
      sd=sd(var_metric, na.rm=T)
    )
}

x_to_time_count <- function(x) {
  n_years <- floor(x/365)
  remainder <- x - (n_years*365)
  n_months <- floor(remainder/30)
  n_days <- remainder - (n_months*30)
  return(
    paste0(n_years, " years, ", n_months, " months and ", n_days, " days.")
  )
}

rename_df_from_str <- function(df, col_name, new_name) {
  col_idx <- which(names(df) == col_name)
  names(df)[[col_idx]] <- new_name
  return(df)
}

## Time Plots ----

time_prepare_interval_df <- function(df, begin_col, end_col) {
  df %>%
    rename_df_from_str(begin_col, "begin") %>%
    rename_df_from_str(end_col, "end") %>%
    mutate(time_interval = map2_dbl(begin, end, calc_date_dist)) %>%
    filter(!is.na(time_interval))
}

time_calc_year <- function(df=df_arbitragens) {
  df %>%
    mutate(year = str_extract(data_inicio, "\\d{4}") %>% as.numeric())
}

time_calc_year_end <- function(df=df_arbitragens) {
  df %>%
    mutate(year = str_extract(data_fim, "\\d{4}") %>% as.numeric())
}

time_scatter_by_year <- function(
    df,
    begin_col,
    end_col,
    first_year,
    last_year,
    time_description,
    point_color=single,
    mean_color=pal_uf[1],
    line_color="pink"
  ) {
  p <- df %>%
    time_calc_year() %>%
    time_prepare_interval_df(begin_col, end_col) %>%
    ggplot(aes(x=year, y=time_interval)) +
    scale_x_continuous(breaks=first_year:last_year) +
    geom_smooth(method="lm",
                se=F,
                color=line_color,
                linetype="dashed") +
    geom_point(color=point_color,
               alpha=0.6,
               size=3) +
    stat_summary(geom="pointrange",
                 fun=function(x) {ifelse(length(x) < 2, NA, mean(x))},
                 color=point_color,
                 shape=8) +
    labs(title=paste0("Tempo ", time_description, ", por ano de início da arbitragem"),
         subtitle=subtitle,
         x="Ano de início da arbitragem",
         y=paste0("Tempo, em dias, ", time_description))
  return(p)
}

time_scatter_by_year_of_end <- function(
    df,
    begin_col,
    end_col,
    first_year,
    last_year,
    time_description,
    point_color=single,
    mean_color=pal_uf[1],
    line_color="pink"
) {
  p <- df %>%
    time_calc_year_end() %>%
    time_prepare_interval_df(begin_col, end_col) %>%
    ggplot(aes(x=year, y=time_interval)) +
    scale_x_continuous(breaks=first_year:last_year) +
    geom_smooth(method="lm",
                se=F,
                color=line_color,
                linetype="dashed") +
    geom_point(color=point_color,
               alpha=0.6,
               size=3) +
    stat_summary(geom="pointrange",
                 fun=function(x) {ifelse(length(x) < 2, NA, mean(x))},
                 color=point_color,
                 shape=8) +
    labs(title=paste0("Tempo ", time_description, ", por ano de fim da arbitragem"),
         subtitle=subtitle,
         x="Ano de fim da arbitragem",
         y=paste0("Tempo, em dias, ", time_description))
  return(p)
}

time_n_mean_per_year <- function(df, begin_col, end_col) {
  df %>% 
    time_calc_year() %>%
    time_prepare_interval_df(begin_col, end_col) %>%
    group_by(year) %>%
    summarise(n=n(), mean=mean(time_interval)) %>%
    ungroup() %>%
    mutate(mean_description=map_chr(round(mean), x_to_time_count))
}

time_boxplot_by_var <- function(
    df,
    var_col,
    begin_col,
    end_col,
    var_label,
    time_description,
    var_description,
    n_lines=1,
    fill_color=pal_uf[3],
    line_color=single
  ) {
  
  df_renamed_with_dist <- df %>%
    rename_df_from_str(var_col, "var") %>%
    time_prepare_interval_df(begin_col, end_col)
  
  if (n_lines > 1) {
    par_sep <- "\n"
  } else {
    par_sep <- " "
  }
  
  var_n <- df_renamed_with_dist %>%
    count(var) %>%
    mutate(var_n = paste0(var, par_sep, "(N = ", n, ")")) %>%
    select(!n)
  
  df_renamed_with_dist %>%
    left_join(var_n, by="var") %>%
    ggplot(aes(x=fct_infreq(var_n), y=time_interval)) +
    geom_boxplot(color=line_color, fill=fill_color) +
    labs(title=paste0("Tempo ", time_description,", por ", var_description),
         subtitle = subtitle,
         x = var_label,
         y = paste0("Tempo, em dias, ", time_description))
}

time_reorder_var_pericia <- function(df) {
  df %>%
    mutate(
      var_n_order = case_when(
        var == "Sim" ~ 1,
        var ==  "Não" ~ 2,
        var ==  "N/A" ~ 3,
        var ==  "N/D" ~ 4,
      ),
      var_n = fct_reorder(var_n, var_n_order)
    ) %>%
    select(!var_n_order)
}

time_boxplot_by_var_reordering <- function(
    df,
    var_col,
    begin_col,
    end_col,
    var_label,
    time_description,
    var_description,
    reordering_var_n_func,
    n_lines=1,
    fill_color=pal_uf[3],
    line_color=single
) {
  
  df_renamed_with_dist <- df %>%
    rename_df_from_str(var_col, "var") %>%
    time_prepare_interval_df(begin_col, end_col)
  
  if (n_lines > 1) {
    par_sep <- "\n"
  } else {
    par_sep <- " "
  }
  
  var_n <- df_renamed_with_dist %>%
    count(var) %>%
    mutate(var_n = paste0(var, par_sep, "(N = ", n, ")")) %>%
    reordering_var_n_func() %>%
    select(!n)
  
  df_renamed_with_dist %>%
    left_join(var_n, by="var") %>%
    ggplot(aes(x=var_n, y=time_interval)) +
    geom_boxplot(color=line_color, fill=fill_color) +
    labs(title=paste0("Tempo ", time_description,", por ", var_description),
         subtitle = subtitle,
         x = var_label,
         y = paste0("Tempo, em dias, ", time_description))
}

## Other Plots ----


simple_question_bar_plot <- function(
    df,
    bar_var,
    nudge=1,
    title_label="",
    subtitle_label=subtitle,
    x_label="",
    y_label="",
    colors=c(single, "#555")
) {
  df %>%
    rename_df_from_str(bar_var, "question") %>%
    group_by(question) %>%
    count_and_perc() %>%
    mutate(question_fill = ifelse(question %in% c("N/A", "N/D"), "1", "0")) %>%
    ggplot(aes(x=question, y=n, label=lbl_n_perc, fill=question_fill, color=question_fill)) +
    geom_col() +
    geom_text(nudge_y=nudge, size=3, fontface="bold") +
    labs(
      title=title_label, subtitle=subtitle_label, x=x_label, y=y_label
    ) +
    scale_fill_manual(values=colors) +
    scale_color_manual(values=colors) +
    theme(legend.position="none")
}
