#' Plot football field
#'
#' @param df Data frame with nfl data to plot. The data frame should be filtered on the plays you want to plot
#'
#' @return faceted ggplot of selected plays
#'
#' @examples
#' plot_field(filter(train, play_id %in% sample(train$play_id, 1)))
#' 
plot_field <- function(df) {
  num_plays <- length(unique(df$play_id))
  if (num_plays > 20) stop("More than 20 plays, too many to plot")
  facet_rows <- ifelse(num_plays == 1, 1, ceiling(num_plays / 2))
  facet_cols <- ifelse(num_plays == 1, 1, 2)

  fig <- df %>%
    ggplot(aes(x = x_std, y = y_std, fill = is_on_offense)) +
    # draw endzones
    geom_rect(aes(xmin = -10, xmax = 0, ymin = 0, max = Inf), fill = "darkseagreen1") +
    geom_rect(aes(xmin = 100, xmax = 110, ymin = 0, max = Inf), fill = "darkseagreen1") +
    # draw players
    geom_point(pch = 21, size = 3) +
    geom_point(data = filter(df, is_ball_carrier), size = 4, pch = 21, fill = "black") +
    # draw lines
    geom_vline(aes(xintercept = yards_from_own_goal), colour = "blue", lty = 2) +
    geom_vline(aes(xintercept = yards_from_own_goal + yards_to_first_down), colour = "yellow3", lty = 2) +
    geom_vline(aes(xintercept = 0)) +
    geom_vline(aes(xintercept = 100)) +
    geom_vline(aes(xintercept = yards_from_own_goal + rushing_yards), colour = "red", lty = 3) +
    # settings
    scale_colour_brewer(palette = "Set2") +
    scale_x_continuous(breaks = (c(0:10) * 10), limits = c(-10, 110), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 53.3), expand = c(0, 0)) +
    labs(x = "X", y = "Y", title = "Field Position at Handoff", fill = "Offense",
         subtitle = "Blue: line of scrimmage, Yellow: first down, Red: end of play") +
    theme_bw(14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "right"
    ) +
    facet_wrap(~play_id, nrow = facet_rows, ncol = facet_cols)
  
  fig
}


#' Circle coordinates
#' 
#' A function that will create circle coordinates for plotting in ggplot with geom_polygon().
#'
#' @param center 
#' @param diameter radius * 2
#' @param npoints 
#' @param start 0 default, set to 1.5 for semi circle
#' @param end 2 default, set to 2.5 for semi circle
#'
#' @return data frame of x and y with containing coordinates for a circle
#'
#' @examples
#' circle_coords(c(0,7.625), 9.8, start=1.5, end=2.5) # draws a semi circle
circle_coords <- function(center=c(0,0), diameter=1, npoints=100, start=0, end=2) {
  tt <- seq(start*pi, end*pi, length.out=npoints)
  data.frame(x = center[1] + diameter / 2 * cos(tt), 
             y = center[2] + diameter / 2 * sin(tt))
}


#' Plot Field Area Circles
#'
#' @param df 
#'
#' @return faceted ggplot of selected plays
#'
#' @examples
#' plot_field_circles(filter(train, play_id %in% sample(train$play_id, 1)))
plot_field_circles <- function(df) {
  
  fig <- plot_field(df) 
  ball_carrier_coords <- df %>% filter(is_ball_carrier) %>% select(x_std, y_std)
  
  for (i in filter(df, is_on_offense == FALSE)$nfl_id) {
    # calculate distances
    defense_coords <- df %>% filter(nfl_id == i) %>% select(x_std, y_std, nfl_id)
    distance_x <- abs(defense_coords$x_std - ball_carrier_coords$x_std)
    distance_y <- abs(defense_coords$y_std - ball_carrier_coords$y_std)
    distance_z <- sqrt(distance_x^2 + distance_y^2)
    # calculate semi circle
    df_circle <- circle_coords(c(ball_carrier_coords$x_std, ball_carrier_coords$y_std),
                               distance_z * 2,
                               start = 1.5, # 1.5 to draw semi circle
                               end = 2.5) # 2.5 to draw demi circle
    # update plot
    fig <- fig + 
      geom_polygon(data = df_circle, aes(x, y), fill = "blue", alpha = 1/11)
  }
    
  fig
}
