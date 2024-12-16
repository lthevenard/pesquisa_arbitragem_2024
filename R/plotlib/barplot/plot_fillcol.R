# FILLCOL: geom_col with fill (and possible annotation) ----

plot_fillcol <- function(
    df,
    col_category,
    fill_category,
    col_size="n",
    var_facet=NULL,
    title_label="",
    subtitle_label=subtitle,
    x_label="",
    y_label="",
    fill_label="",
    fill_position="dodge",
    flip = FALSE,
    order_bars=TRUE,
    order_fill=TRUE,
    fill_levels=NULL,
    reverse_legend=FALSE,
    rescaling_func=NULL,
    scale_perc_by_coll_size=TRUE,
    scale_perc_if_postion_fill=TRUE,
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
    dodge_adjustment=-0.1,
    bar_width=0.8
) {
  
  # Rename variables ----
  df <- df %>% 
    rename(var1=col_category,
           var2=col_size,
           var3=fill_category)
  
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
    n <- df$var3 %>% unique() %>% length()
    fill_colors <- choose_colors(n)
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
  if (order_fill) {
    if (is.null(fill_levels)) {
      df <- df %>% 
        mutate(var3=fct_infreq(var3))
    } else {
      df <- df %>% 
        mutate(var3=factor(var3, ordered=TRUE, levels=fill_levels))
    }
  }
  
  # Generate graphing area ----
  if (flip) {
    p <- df %>% 
      ggplot(aes(y=var1, x=var2, fill=var3))
  } else {
    p <- df %>% 
      ggplot(aes(x=var1, y=var2, fill=var3))
  }
  
  
  # Plot the bars ----
  p <- p +
    geom_col(color=linecolor,
             size=lineweight,
             position=fill_position,
             width=bar_width) +
    labs(title=title_label,
         subtitle=subtitle_label,
         x=x_label,
         y=y_label,
         fill=fill_label) +
    scale_fill_manual(values=fill_colors, guide=guide_legend(reverse=reverse_legend)) +
    theme_func()
  
  # Annotation ----
  if (annotate) {
    if (fill_position=="dodge") { # Plot annotation when position is "dodge"
      if (flip) {
        p <- p +
          geom_text(aes(label=lvar),
                    size=lbl_size, 
                    color=lbl_colors,
                    fontface="bold",
                    position=position_dodge(width=bar_width),
                    hjust=dodge_adjustment)
      } else {
        p <- p +
          geom_text(aes(label=lvar),
                    size=lbl_size, 
                    color=lbl_colors,
                    fontface="bold",
                    position=position_dodge(width=bar_width),
                    vjust=dodge_adjustment)
      }
    } else { # Plot annotation when position far all other cases
      annotation_df <- prepare_annotation_df(
        df,
        has_fill=TRUE,
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
