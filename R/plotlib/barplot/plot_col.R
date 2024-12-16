# COL: geom_col without fill (possible annotation) ----

plot_col <- function(
    df,
    col_category,
    col_size="n",
    var_facet=NULL,
    title_label="",
    subtitle_label=subtitle,
    x_label="",
    y_label="",
    flip = FALSE,
    order_bars=TRUE,
    rescaling_func=NULL,
    scale_perc_by_coll_size=TRUE,
    fill_colors="choose",
    linecolor=NA,
    lineweight=0.2,
    theme_func=theme_minimal,
    strip_background=TRUE,
    frows=NULL,
    fcols=NULL,
    fscales="fixed",
    annotate=TRUE,
    var_label="lbl_n_perc",
    lbl_position=c("middle", "top"),
    lbl_colors="black",
    lbl_size=3,
    lbl_adjustment=0.015,
    lbl_fix_min=FALSE,
    lbl_fix_max=FALSE,
    lbl_threshold_min=0.1,
    lbl_threshold_max=0.9,
    lbl_fix_min_func=~function(x){return(NA)},
    lbl_fix_max_func=~function(x){return(NA)},
    bar_width=0.8
) {
  
  # Rename variables ----
  df <- df %>% 
    rename(var1=col_category,
           var2=col_size)
  
  if (!is.null(var_facet)) {
    df <- df %>% 
      rename(fvar = var_facet)
  }
  
  if (annotate) {
    df <- df %>% 
      rename(lvar=var_label)
  }
  
  # Choose the color ----
  if (fill_colors=="choose") {
    fill_colors <- choose_colors(1)
  }
  
  # Reorder categories ----
  if (order_bars) {
    if (flip) {
      df <- df %>%  
        mutate(var1=fct_infreq(var1) %>% fct_rev())
    } else {
      df <- df %>% 
        mutate(var1=fct_infreq(var1))
    }
  }
  
  # Generate graphing area ----
  if (flip) {
    p <- df %>% 
      ggplot(aes(y=var1, x=var2))
  } else {
    p <- df %>% 
      ggplot(aes(x=var1, y=var2))
  }
  
  
  # Plot the bars ----
  p <- p +
    geom_col(fill=fill_colors[1],
             color=linecolor,
             size=lineweight,
             width=bar_width) +
    labs(title=title_label,
         subtitle=subtitle_label,
         x=x_label,
         y=y_label) +
    theme_func()
  
  # Annotation ----
  if (annotate) {
    annotation_df <- prepare_annotation_df(
      df,
      has_fill=FALSE,
      has_facets=!is.null(var_facet),
      flip,
      lbl_adjustment,
      lbl_threshold_min,
      lbl_threshold_max,
      lbl_fix_min,
      lbl_fix_min_func,
      lbl_fix_max,
      lbl_fix_max_func
    )
    # Ploting annotation
    if (flip) {
      if (lbl_position[1]=="top") {
        p <- p +
          geom_text(data=annotation_df,
                    aes(label=lvar, x=lbl_pos_top),
                    size=lbl_size, 
                    color=lbl_colors,
                    fontface="bold")
      } else if (lbl_position[1]=="middle") {
        p <- p +
          geom_text(data=annotation_df,
                    aes(label=lvar, x=lbl_pos_middle),
                    size=lbl_size, 
                    color=lbl_colors,
                    fontface="bold")
      }
    } else {
      if (lbl_position[1]=="top") {
        p <- p +
          geom_text(data=annotation_df,
                    aes(label=lvar, y=lbl_pos_top),
                    size=lbl_size, 
                    color=lbl_colors,
                    fontface="bold")
      } else if (lbl_position[1]=="middle") {
        p <- p +
          geom_text(data=annotation_df,
                    aes(label=lvar, y=lbl_pos_middle),
                    size=lbl_size, 
                    color=lbl_colors,
                    fontface="bold")
      }
    }
  }
  
  # Rescaling axes ----
  if (col_size=="perc") {
    if (scale_perc_by_coll_size) {
      if (flip) {
        p <- p +
          scale_x_continuous(labels=scales::label_percent())
      } else {
        p <- p +
          scale_y_continuous(labels=scales::label_percent())
      }
    }
  }
  if (!is.null(rescaling_func)) {
    p <- rescaling_func(p)
  }
  
  # Add facets ----
  if (!is.null(var_facet)) {
    p <- p +
      facet_wrap(~fvar, ncol=fcols, nrow=frows, scales=fscales)
    if (strip_background) {
      p <- p +
        theme(strip.background = element_rect(color="black", linewidth = 0.2))
    }
  }
  
  # Return plot ----
  return(p)
}
