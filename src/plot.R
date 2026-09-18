library(tidyverse)

#----------------------------
# Relative Error
#----------------------------

re_ts <- res_time_series %>%
  mutate(
    BBmsy = 100 * (BBmsy_spict / BBmsy_vpa - 1),
    FFmsy = 100 * (FFmsy_spict / FFmsy_vpa - 1),
    Biomass = 100 * (B_spict / EB_vpa - 1)
  ) %>%
  select(simulation, BBmsy, FFmsy, Biomass)

re_other <- res_others %>%
  mutate(
    MSY = 100 * (msy_spict / msy_vpa - 1),
    r   = 100 * (r_spict / r_vpa - 1),
    K   = 100 * (K_spict / K_vpa - 1)
  ) %>%
  select(simulation, MSY, r, K)

# 最終年のみ
re_ts_last <- res_time_series %>%
  group_by(simulation, id) %>%
  #filter(year == max(year)) %>%
  filter(year %in% (max(year)-5):max(year)) |>
  ungroup() %>%
  mutate(
    `B/Bmsy` = 100 * (BBmsy_spict / BBmsy_vpa - 1),
    `F/Fmsy` = 100 * (FFmsy_spict / FFmsy_vpa - 1),
    Biomass  = 100 * (B_spict / EB_vpa - 1)
  ) %>%
  select(simulation, `B/Bmsy`, `F/Fmsy`, Biomass)

re_other2 <- res_others %>%
  mutate(
    MSY = 100 * (msy_spict / msy_vpa - 1)
  ) %>%
  select(simulation, MSY)

plot_dat <- bind_rows(
  pivot_longer(re_ts_last,
               -simulation,
               names_to = "Parameter",
               values_to = "RE"),
  pivot_longer(re_other2,
               -simulation,
               names_to = "Parameter",
               values_to = "RE")
)

#----------------------------
# Figure 4風
#----------------------------

ggplot(
  plot_dat,
  aes(simulation, RE, fill = simulation)
) +
  geom_hline(
    yintercept = 0,
    linetype = 2,
    colour = "grey40"
  ) +
  geom_boxplot(
    outlier.alpha = 0.3,
    width = 0.7
  ) +
  facet_wrap(
    ~Parameter,
    scales = "free_y",
    ncol = 3
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    strip.background = element_rect(
      fill = "grey90"
    )
  ) +
  labs(
    x = NULL,
    y = "Relative Error (%)"
  )


plot_dat_are <- plot_dat %>%
  mutate(ARE = abs(RE))

ggplot(
  plot_dat_are,
  aes(simulation, ARE, fill = simulation)
) +
  geom_boxplot() +
  facet_wrap(
    ~Parameter,
    scales = "free_y",
    ncol = 3
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  ) +
  labs(
    x = NULL,
    y = "Absolute Relative Error (%)"
  )
