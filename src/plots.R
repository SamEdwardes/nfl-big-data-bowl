#' Plot football field
#'
#' @param play_ids a vector of PlayIds to plot
#'
#' @return faceted ggplot of selected plays
#'
#' @examples
#' plot_field(filter(train, play_id %in% sample(train$play_id, 1)))
#' 
plot_field <- function(df) {
  num_plays <- length(unique(df$play_id))
  facet_rows <- ifelse(num_plays == 1, 1, ceiling(num_plays / 2))
  facet_cols <- ifelse(num_plays == 1, 1, 2)

  field_position_plot <- df %>%
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
    # settings
    scale_colour_brewer(palette = "Set2") +
    scale_x_continuous(breaks = (c(0:10) * 10), limits = c(-10, 110), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 53.3), expand = c(0, 0)) +
    labs(x = "X", y = "Y", title = "Field Position at Handoff", fill = "Offense") +
    theme_bw(14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "right"
    ) +
    facet_wrap(~play_id, nrow = facet_rows, ncol = facet_cols)
  
  field_position_plot
}
