# FILLBAR: geom_bar with fill (no annotation) ----

plot_fillbar <- function(
    df,
    bar_category,
    fill_category,
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
    scale_perc_if_postion_fill=TRUE,
    fill_colors="choose",
    linecolor=NA,
    lineweight=0.2,
    theme_func=theme_minimal,
    strip_background=TRUE,
    frows=NULL,
    fcols=NULL,
    fscales="fixed",
    bar_width=0.8
) {
  
  # Rename variables ----
  df <- df %>% 
    rename(var1=bar_category,
           var2=fill_category)
  
  if (!is.null(var_facet)) {
    df <- df %>% 
      rename(fvar=var_facet)
  }
  
  # Choose the color ----
  if (fill_colors=="choose") {
    n <- df$var2 %>% unique() %>% length()
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
        mutate(var2=fct_infreq(var2))
    } else {
      df <- df %>% 
        mutate(var2=factor(var2, ordered=TRUE, levels=fill_levels))
    }
  }
  
  # Generate graphing area ----
  if (flip) {
    p <- df %>% 
      ggplot(aes(y=var1, fill=var2))
  } else {
    p <- df %>% 
      ggplot(aes(x=var1, fill=var2))
  }
  
  # Plot the bars ----
  p <- p +
    geom_bar(color=linecolor,
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
  
  # Rescaling axes ----
  if (fill_position=="fill") {
    if (scale_perc_if_postion_fill) {
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