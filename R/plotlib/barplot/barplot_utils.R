prepare_df <- function(grouped_df, compute_stats, stats_var=NULL) {
  if (compute_stats) {
    if (is.null(stats_var)) {
      warning("To compute stats, you must provide a numeric varible")
      return(NULL)
    }
    grouped_df <- grouped_df %>% 
      rename(numeric_var=stats_var)
    grouped_df %>% 
      summarise(n=n(),
                mean_value=mean(numeric_var, na.rm=T),
                median_value=median(numeric_var, na.rm=T),
                min_value=min(numeric_var, na.rm=T),
                max_value=max(numeric_var, na.rm=T),
                sd_value=sd(numeric_var, na.rm=T)) %>% 
      mutate(perc=n/sum(n)) %>%
      mutate(lbl_perc=scales::label_percent(accuracy=0.1, decimal.mark=",")(perc),
             lbl_n=as.character(n),
             lbl_n_perc=paste0(n, " (", lbl_perc, ")"),
             lbl_perc_n=paste0(lbl_perc, " (N = ", n, ")"),
             lbl_mean=round(mean_value) %>% as.character(),
             lbl_median=round(median_value) %>% as.character(),
             lbl_min=as.character(min_value),
             lbl_max=as.character(max_value),
             lbl_sd=round(sd_value) %>% as.character()) %>% 
      ungroup()
  } else {
    grouped_df %>% 
      summarise(n=n()) %>% 
      mutate(perc=n/sum(n)) %>%
      mutate(lbl_perc=scales::label_percent(accuracy=0.1, decimal.mark=",")(perc),
             lbl_n=as.character(n),
             lbl_n_perc=paste0(n, " (", lbl_perc, ")"),
             lbl_perc_n=paste0(lbl_perc, " (N = ", n, ")")) %>% 
      ungroup()
  }
}

prepare_annotation_df <- function(
    df,
    has_fill,
    has_facets,
    flip,
    adjustment,
    threshold_min,
    threshold_max,
    fix_min,
    fix_min_func,
    fix_max,
    fix_max_func
  ) {
  max_var2 <- max(df$var2, na.rm=TRUE)
  if (has_fill & has_facets) {
    annotation_df <- df %>%
      group_by(fvar, var1) %>%
      mutate(lbl_pos_middle = sum(var2) - (cumsum(var2) - (var2/2)),
             lbl_pos_top = ifelse(flip,
                                  var2 + ((adjustment*max_var2)*(str_count(lvar)/2)),
                                  var2 + adjustment*max_var2))
  } else if (has_fill) {
    annotation_df <- df %>%
      group_by(var1) %>%
      mutate(lbl_pos_middle = sum(var2) - (cumsum(var2) - (var2/2)),
             lbl_pos_top = ifelse(flip,
                                  var2 + ((adjustment*max_var2)*(str_count(lvar)/2)),
                                  var2 + adjustment*max_var2))
  } else if (has_facets) {
    annotation_df <- df %>%
      group_by(fvar, var1) %>%
      mutate(lbl_pos_middle = sum(var2) - (cumsum(var2) - (var2/2)),
             lbl_pos_top = ifelse(flip,
                                  var2 + ((adjustment*max_var2)*(str_count(lvar)/2)),
                                  var2 + adjustment*max_var2))
  } else {
    annotation_df <- df %>%
      mutate(lbl_pos_middle = var2/2,
             lbl_pos_top = ifelse(flip,
                                  var2 + ((adjustment*max_var2)*(str_count(lvar)/2)),
                                  var2 + adjustment*max_var2))
  }
  if (fix_min) {
    annotation_df <- annotation_df %>%
      mutate(
        lbl_pos_top = ifelse(
          lbl_pos_top < threshold_min,
          fix_min_func(lbl_pos_top),
          lbl_pos_top
        ),
        lbl_pos_middle = ifelse(
          lbl_pos_middle < threshold_min,
          fix_min_func(lbl_pos_middle),
          lbl_pos_middle
        )
      )
  }
  if (fix_max) {
    annotation_df <- annotation_df %>%
      mutate(
        lbl_pos_top = ifelse(
          lbl_pos_top > threshold_max,
          fix_max_func(lbl_pos_top),
          lbl_pos_top
        ),
        lbl_pos_middle = ifelse(
          lbl_pos_middle > threshold_max,
          fix_max_func(lbl_pos_middle),
          lbl_pos_middle
        )
      )
  }
  return(annotation_df)
}


