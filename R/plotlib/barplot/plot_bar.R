# BAR: geom_bar without fill and no possible annotation ----

plot_bar <- function(
    df,
    bar_category,
    var_facet=NULL,
    title_label="",
    subtitle_label=subtitle,
    x_label="",
    y_label="",
    flip = FALSE,
    order_bars=TRUE,
    rescaling_func=NULL,
    fill_colors="choose",
    linecolor=NA,
    lineweight=0.2,
    theme_func=theme_minimal,
    strip_background=TRUE,
    frows=NULL,
    fcols=NULL,
    fscales="fixed",
    width=0.8
) {
  
  # Rename variables ----
  df <- df %>% 
    rename(var1 = bar_category)
  
  if (!is.null(var_facet)) {
    df <- df %>% 
      rename(fvar = var_facet)
  }
  
  # Choose the color ----
  if (fill_colors=="choose") {
    fill_colors <- choose_colors(1)
  }
  
  # Reorder categories ----
  if (order_bars) {
    if (flip) {
      df <- df %>%
        mutate(var1 = fct_infreq(var1) %>% fct_rev())
    } else {
      df <- df %>% 
        rename(var1 = bar_category) %>% 
        mutate(var1 = fct_infreq(var1))
    }
  }
  
  # Generate graphing area ----
  if (flip) {
    p <- df %>% 
      ggplot(aes(y = var1))
  } else {
    p <- df %>% 
      ggplot(aes(x = var1))
  }
  
  # Plot the bars ----
  p <- p +
    geom_bar(fill=fill_colors[1],
             color=linecolor,
             size=lineweight,
             width=bar_width) +
    labs(title=title_label,
         subtitle=subtitle_label,
         x=x_label,
         y=y_label) +
    theme_func()
  
  # Rescaling axes ----
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