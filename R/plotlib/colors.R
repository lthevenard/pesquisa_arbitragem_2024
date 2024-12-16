regnum_colors <- list(
  light_blue_logo = "#78BBE7",
  darkblue_logo = "#192E4C",
  bright_blue = "#5197F7",
  darker_blue = "#111F33",
  middle_blue = "#2E558C",
  dark_greenish = "#17252E",
  middle_greenish = "#3E6279",
  light_greenish = "#737F7D",
  lighter_greenish = "#DFF7F1",
  yellow_gray = "#6C6E72FF",
  beige = "#B3A772FF",
  darker_yellow = "#DBC761FF",
  yellow = "#FFEA46FF"
)

pal_7 <- c(
  regnum_colors$darkblue_logo,
  regnum_colors$middle_greenish,
  regnum_colors$light_blue_logo,
  regnum_colors$yellow_gray,
  regnum_colors$beige,
  regnum_colors$yellow,
  regnum_colors$lighter_greenish
)

pal_1 <- pal_7[3]
pal_2 <- pal_7[c(1, 3)]
pal_3 <- pal_7[1:3]
pal_4 <- pal_7[c(1:3, 5)]
pal_5 <- pal_7[1:5]
pal_6 <- pal_7[c(1:5, 7)]

choose_colors <- function(n) {
  if (n==1) {
    return(pal_1)
  } else if (n==2) {
    return(pal_2)
  } else if (n==3) {
    return(pal_3)
  } else if (n==4) {
    return(pal_4)
  } else if (n==5) {
    return(pal_5)
  } else if (n==6) {
    return(pal_6)
  } else if (n==7) {
    return(pal_7)
  } else if (n>7) {
    return(viridis::cividis(n+1))
  }
}