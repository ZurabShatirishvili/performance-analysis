# =============================================================================
# R ANALYSIS SCRIPT: Performance Analysis
# Author: Zurab Shatirishvili
# Date: 17-09-2026
# Purpose: Analyze ratings, scores, and movement accuracy across three groups
# and four songs.
# =============================================================================


# =============================================================================
# 1. LOAD REQUIRED LIBRARIES
# =============================================================================

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(effectsize)


# =============================================================================
# 2. LOAD DATA
# =============================================================================

# Add the path to the dataset here.
# The dataset should contain the following variables:
# participant_id, group, song, star_rating, score,
# Missed, Okay, Good, Super, Perfect

load("PATH_TO_DATA.RData")

# View first few rows
head(all_data)


# =============================================================================
# 3. DATA CLEANING & PREPARATION
# =============================================================================

# Define the songs included in the analysis
songs_of_interest <- c(
  "Song1",
  "Song2",
  "Song3",
  "Song4"
)


# Filter data to include only relevant songs
all_data <- all_data %>%
  filter(song %in% songs_of_interest)


# Convert numeric variables to numeric
# Non-numeric values become NA
all_data <- all_data %>%
  mutate(
    star_rating = as.numeric(star_rating),
    score = as.numeric(score),
    Missed = as.numeric(Missed),
    Okay = as.numeric(Okay),
    Good = as.numeric(Good),
    Super = as.numeric(Super),
    Perfect = as.numeric(Perfect)
  )


# Make group a factor
all_data <- all_data %>%
  mutate(
    group = factor(
      group,
      levels = c("Group1", "Group2", "Group3")
    )
  )


# =============================================================================
# 4. CREATE STAR RATING DATA
# =============================================================================

# Participant-level star ratings for the first three songs
star_data <- all_data %>%
  filter(
    song %in% c(
      "Song1",
      "Song2",
      "Song3"
    ),
    !is.na(star_rating)
  )


# =============================================================================
# 5. CREATE SUMMARY DATA: TOTAL STARS BY SONG AND GROUP
# =============================================================================

star_totals <- all_data %>%
  filter(
    song %in% c(
      "Song1",
      "Song2",
      "Song3"
    )
  ) %>%
  group_by(group, song) %>%
  summarise(
    total_stars = sum(star_rating, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    song = factor(
      song,
      levels = c(
        "Song1",
        "Song2",
        "Song3"
      )
    )
  )


# =============================================================================
# 6. CREATE SUMMARY DATA: AVERAGE STAR RATING
# =============================================================================

participant_star_average <- star_data %>%
  group_by(participant_id, group) %>%
  summarise(
    mean_stars = mean(star_rating, na.rm = TRUE),
    n_songs = n(),
    .groups = "drop"
  ) %>%
  mutate(
    mean_stars = ifelse(
      is.nan(mean_stars),
      NA,
      mean_stars
    )
  )


# =============================================================================
# 7. CREATE SUMMARY DATA: SONG4 MOVEMENT RATINGS
# =============================================================================

movement_individual <- all_data %>%
  filter(song == "Song4") %>%
  mutate(
    total_movements = Missed + Okay + Good + Super + Perfect
  ) %>%
  filter(
    !is.na(total_movements),
    total_movements > 0
  ) %>%
  mutate(
    Missed_pct = Missed / total_movements * 100,
    Okay_pct = Okay / total_movements * 100,
    Good_pct = Good / total_movements * 100,
    Super_pct = Super / total_movements * 100,
    Perfect_pct = Perfect / total_movements * 100
  )


# Convert movement ratings to long format
movement_long <- movement_individual %>%
  select(
    participant_id,
    group,
    Missed_pct,
    Okay_pct,
    Good_pct,
    Super_pct,
    Perfect_pct
  ) %>%
  pivot_longer(
    cols = c(
      Missed_pct,
      Okay_pct,
      Good_pct,
      Super_pct,
      Perfect_pct
    ),
    names_to = "rating_category",
    values_to = "percentage"
  ) %>%
  mutate(
    rating_category = recode(
      rating_category,
      "Missed_pct" = "Missed",
      "Okay_pct" = "Okay",
      "Good_pct" = "Good",
      "Super_pct" = "Super",
      "Perfect_pct" = "Perfect"
    ),
    rating_category = factor(
      rating_category,
      levels = c(
        "Missed",
        "Okay",
        "Good",
        "Super",
        "Perfect"
      )
    )
  )


# Summary of movement ratings by group
movement_summary <- movement_long %>%
  group_by(group, rating_category) %>%
  summarise(
    mean_percentage = mean(percentage, na.rm = TRUE),
    sd_percentage = sd(percentage, na.rm = TRUE),
    .groups = "drop"
  )


# =============================================================================
# 8. CREATE SUMMARY DATA: SONG4 SCORES
# =============================================================================

score_data <- all_data %>%
  filter(
    song == "Song4",
    !is.na(score)
  )


# =============================================================================
# 9. DESCRIPTIVE STATISTICS
# =============================================================================

# Average star rating by group
star_descriptives <- participant_star_average %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean = mean(mean_stars, na.rm = TRUE),
    sd = sd(mean_stars, na.rm = TRUE),
    .groups = "drop"
  )

print(star_descriptives)


# Song4 score by group
score_descriptives <- score_data %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    .groups = "drop"
  )

print(score_descriptives)


# Star rating by song and group
star_song_descriptives <- star_data %>%
  group_by(group, song) %>%
  summarise(
    n = n(),
    mean = mean(star_rating, na.rm = TRUE),
    sd = sd(star_rating, na.rm = TRUE),
    .groups = "drop"
  )

print(star_song_descriptives)


# =============================================================================
# 10. STATISTICAL ANALYSIS
# =============================================================================

# -----------------------------------------------------------------------------
# 10.1 ANOVA: AVERAGE STAR RATING ACROSS GROUPS
# -----------------------------------------------------------------------------

average_star_anova <- aov(
  mean_stars ~ group,
  data = participant_star_average
)

summary(average_star_anova)


# Post-hoc pairwise comparisons
emmeans_avg <- emmeans(
  average_star_anova,
  ~ group
)

pairs(
  emmeans_avg,
  adjust = "tukey"
)


# -----------------------------------------------------------------------------
# 10.2 ANOVA: SONG4 SCORE ACROSS GROUPS
# -----------------------------------------------------------------------------

score_anova <- aov(
  score ~ group,
  data = score_data
)

summary(score_anova)


# Post-hoc pairwise comparisons
emmeans_score <- emmeans(
  score_anova,
  ~ group
)

pairs(
  emmeans_score,
  adjust = "tukey"
)


# -----------------------------------------------------------------------------
# 10.3 NORMALITY CHECK: SHAPIRO-WILK TEST
# -----------------------------------------------------------------------------

# Average star rating
shapiro_star <- by(
  participant_star_average$mean_stars,
  participant_star_average$group,
  shapiro.test
)

print(shapiro_star)


# Song4 score
shapiro_score <- by(
  score_data$score,
  score_data$group,
  shapiro.test
)

print(shapiro_score)


# -----------------------------------------------------------------------------
# 10.4 MIXED-EFFECTS MODEL:
# STAR RATINGS ACROSS SONGS AND GROUPS
# -----------------------------------------------------------------------------

# Create timepoint variable from the three repeated songs
star_time_data <- star_data %>%
  mutate(
    timepoint = factor(
      song,
      levels = c(
        "Song1",
        "Song2",
        "Song3"
      ),
      labels = c(
        "1",
        "2",
        "3"
      )
    )
  )


# Fit mixed-effects model
star_time_model <- lmer(
  star_rating ~ timepoint * group + (1 | participant_id),
  data = star_time_data
)


# ANOVA table for mixed-effects model
star_time_anova <- anova(star_time_model)

print(star_time_anova)


# Pairwise comparisons between timepoints
star_time_posthoc <- emmeans(
  star_time_model,
  pairwise ~ timepoint,
  adjust = "tukey"
)

print(star_time_posthoc)


# =============================================================================
# 11. EFFECT SIZES
# =============================================================================

# -----------------------------------------------------------------------------
# 11.1 Partial Eta-Squared for Average Star Rating ANOVA
# -----------------------------------------------------------------------------

eta_squared_avg <- eta_squared(
  average_star_anova,
  partial = TRUE
)

print(eta_squared_avg)


# -----------------------------------------------------------------------------
# 11.2 Partial Eta-Squared for Song4 Score ANOVA
# -----------------------------------------------------------------------------

eta_squared_score <- eta_squared(
  score_anova,
  partial = TRUE
)

print(eta_squared_score)


# -----------------------------------------------------------------------------
# 11.3 Cohen's d for Average Star Rating Comparisons
# -----------------------------------------------------------------------------

cohens_d_avg <- eff_size(
  emmeans_avg,
  sigma = sigma(average_star_anova),
  edf = df.residual(average_star_anova)
)

print(cohens_d_avg)


# -----------------------------------------------------------------------------
# 11.4 Cohen's d for Score Comparisons
# -----------------------------------------------------------------------------

cohens_d_score <- eff_size(
  emmeans_score,
  sigma = sigma(score_anova),
  edf = df.residual(score_anova)
)

print(cohens_d_score)


# =============================================================================
# 12. VISUALIZATION
# =============================================================================

# -----------------------------------------------------------------------------
# 12.1 Total Stars by Song and Group
# -----------------------------------------------------------------------------

total_stars_plot <- star_totals %>%
  ggplot(
    aes(
      x = song,
      y = total_stars,
      fill = group
    )
  ) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c(
      "Group1" = "#E69F00",
      "Group2" = "#009E73",
      "Group3" = "#0072B2"
    )
  ) +
  labs(
    x = "Song",
    y = "Total Stars",
    fill = "Group",
    title = "Total Stars by Song and Group"
  ) +
  theme_classic()


# -----------------------------------------------------------------------------
# 12.2 Average Star Rating Across Three Songs
# -----------------------------------------------------------------------------

average_star_plot <- participant_star_average %>%
  ggplot(
    aes(
      x = group,
      y = mean_stars,
      color = group
    )
  ) +
  geom_jitter(
    width = 0.15,
    alpha = 0.5
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 4
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15,
    linewidth = 0.8
  ) +
  scale_color_manual(
    values = c(
      "Group1" = "#E69F00",
      "Group2" = "#009E73",
      "Group3" = "#0072B2"
    )
  ) +
  labs(
    x = "Group",
    y = "Average Star Rating",
    color = "Group",
    title = "Average Star Rating Across Three Songs by Group"
  ) +
  theme_classic()


# -----------------------------------------------------------------------------
# 12.3 Song4: Movement Ratings by Group
# -----------------------------------------------------------------------------

movement_ratings_plot <- movement_long %>%
  ggplot(
    aes(
      x = rating_category,
      y = percentage,
      color = group
    )
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15,
    linewidth = 0.8
  ) +
  scale_color_manual(
    values = c(
      "Group1" = "#E69F00",
      "Group2" = "#009E73",
      "Group3" = "#0072B2"
    )
  ) +
  labs(
    x = "Movement Rating",
    y = "Percentage of Movements",
    color = "Group",
    title = "Song4: Movement Ratings by Group"
  ) +
  theme_classic()


# -----------------------------------------------------------------------------
# 12.4 Song4: Movement Rating Distribution
# -----------------------------------------------------------------------------

movement_distribution_plot <- movement_long %>%
  ggplot(
    aes(
      x = rating_category,
      y = percentage,
      fill = group
    )
  ) +
  geom_boxplot(
    position = position_dodge(width = 0.75),
    alpha = 0.5
  ) +
  scale_fill_manual(
    values = c(
      "Group1" = "#E69F00",
      "Group2" = "#009E73",
      "Group3" = "#0072B2"
    )
  ) +
  labs(
    x = "Movement Rating",
    y = "Percentage of Movements",
    fill = "Group",
    title = "Song4: Movement Rating Distribution"
  ) +
  theme_classic()


# -----------------------------------------------------------------------------
# 12.5 Song4: Perfect Movement Ratings
# -----------------------------------------------------------------------------

perfect_movement_plot <- movement_individual %>%
  ggplot(
    aes(
      x = group,
      y = Perfect_pct,
      color = group
    )
  ) +
  geom_jitter(
    width = 0.15,
    alpha = 0.5
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 4
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15,
    linewidth = 0.8
  ) +
  scale_color_manual(
    values = c(
      "Group1" = "#E69F00",
      "Group2" = "#009E73",
      "Group3" = "#0072B2"
    )
  ) +
  labs(
    x = "Group",
    y = "Perfect Movements (%)",
    color = "Group",
    title = "Song4: Perfect Movement Ratings by Group"
  ) +
  theme_classic()


# -----------------------------------------------------------------------------
# 12.6 Song4: Score by Group
# -----------------------------------------------------------------------------

score_plot <- score_data %>%
  ggplot(
    aes(
      x = group,
      y = score,
      color = group
    )
  ) +
  geom_jitter(
    width = 0.15,
    alpha = 0.5
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 4
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15,
    linewidth = 0.8
  ) +
  scale_color_manual(
    values = c(
      "Group1" = "#E69F00",
      "Group2" = "#009E73",
      "Group3" = "#0072B2"
    )
  ) +
  labs(
    x = "Group",
    y = "Score",
    color = "Group",
    title = "Song4: Score by Group"
  ) +
  theme_classic()


# -----------------------------------------------------------------------------
# 12.7 Star Ratings Across Songs by Group
# -----------------------------------------------------------------------------

star_time_plot <- star_time_data %>%
  ggplot(
    aes(
      x = timepoint,
      y = star_rating,
      group = group,
      color = group
    )
  ) +
  stat_summary(
    fun = mean,
    geom = "line",
    linewidth = 1
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.08,
    linewidth = 0.8
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Song1",
      "2" = "Song2",
      "3" = "Song3"
    )
  ) +
  scale_color_manual(
    values = c(
      "Group1" = "#E69F00",
      "Group2" = "#009E73",
      "Group3" = "#0072B2"
    )
  ) +
  labs(
    x = "Song",
    y = "Star Rating",
    color = "Group",
    title = "Star Ratings Across Songs by Group"
  ) +
  theme_classic()


# -----------------------------------------------------------------------------
# 12.8 Star Ratings by Group and Song
# -----------------------------------------------------------------------------

star_ratings_plot <- star_data %>%
  mutate(
    song = factor(
      song,
      levels = c(
        "Song1",
        "Song2",
        "Song3"
      )
    )
  ) %>%
  ggplot(
    aes(
      x = group,
      y = star_rating,
      color = group
    )
  ) +
  geom_jitter(
    width = 0.15,
    alpha = 0.5
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 4
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15,
    linewidth = 0.8
  ) +
  facet_wrap(~ song) +
  scale_color_manual(
    values = c(
      "Group1" = "#E69F00",
      "Group2" = "#009E73",
      "Group3" = "#0072B2"
    )
  ) +
  labs(
    x = "Group",
    y = "Star Rating",
    color = "Group",
    title = "Star Ratings by Group and Song"
  ) +
  theme_classic()


# -----------------------------------------------------------------------------
# 12.9 Q-Q Plot: Average Star Rating
# -----------------------------------------------------------------------------

qq_star <- ggplot(
  participant_star_average,
  aes(sample = mean_stars)
) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ group) +
  labs(
    title = "Q-Q Plot: Average Star Rating",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_classic()


# -----------------------------------------------------------------------------
# 12.10 Q-Q Plot: Song4 Scores
# -----------------------------------------------------------------------------

qq_score <- ggplot(
  score_data,
  aes(sample = score)
) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ group) +
  labs(
    title = "Q-Q Plot: Song4 Scores",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_classic()


# =============================================================================
# 13. SAVE PLOTS AND RESULTS
# =============================================================================

# Create output folder
dir.create(
  "plots",
  showWarnings = FALSE
)


# Save plots
ggsave(
  "plots/Total_Stars_by_Song_and_Group.png",
  total_stars_plot,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  "plots/Average_Star_Rating.png",
  average_star_plot,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  "plots/Movement_Ratings_by_Group.png",
  movement_ratings_plot,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  "plots/Movement_Rating_Distribution.png",
  movement_distribution_plot,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  "plots/Perfect_Movement_Ratings.png",
  perfect_movement_plot,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  "plots/Score_by_Group.png",
  score_plot,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  "plots/Star_Ratings_Across_Songs.png",
  star_time_plot,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  "plots/Star_Ratings_by_Group_and_Song.png",
  star_ratings_plot,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  "plots/QQ_Average_Star_Rating.png",
  qq_star,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  "plots/QQ_Scores.png",
  qq_score,
  width = 8,
  height = 6,
  dpi = 300
)


# =============================================================================
# 14. SAVE ANALYSIS OBJECTS
# =============================================================================

save(
  star_data,
  star_totals,
  participant_star_average,
  movement_individual,
  movement_long,
  movement_summary,
  score_data,
  average_star_anova,
  score_anova,
  shapiro_star,
  shapiro_score,
  star_time_data,
  star_time_model,
  star_time_anova,
  star_time_posthoc,
  eta_squared_avg,
  eta_squared_score,
  cohens_d_avg,
  cohens_d_score,
  total_stars_plot,
  average_star_plot,
  movement_ratings_plot,
  movement_distribution_plot,
  perfect_movement_plot,
  score_plot,
  star_time_plot,
  star_ratings_plot,
  qq_star,
  qq_score,
  file = "analysis_results.RData"
)


# =============================================================================
# 15. SAVE FINAL WORKSPACE AND HISTORY
# =============================================================================

save.image(
  "analysis_workspace.RData"
)

savehistory(
  "analysis_history.R"
)


# =============================================================================
# 16. FINAL CHECK
# =============================================================================

file.exists("analysis_results.RData")
file.exists("analysis_workspace.RData")
file.exists("analysis_history.R")

ls()


# =============================================================================
# END OF SCRIPT
# =============================================================================
