library(stringr)
# library(reshape2)
# install.packages("reshape2")
library(tidyr)

range_data <- riskmeasures1 |>
  filter(modtype=="acli" & scenario!="baseline") |>
  select(scenario, porttype, vloss_a95t10, verc_a95t10) |>
  rename(VaR_loss = vloss_a95t10 , VaR_ERC = verc_a95t10) |>
  mutate(scenario = case_when(
    scenario == "sigma_v_lbound" ~ "sigmaV_lbound",
    scenario == "sigma_v_ubound" ~ "sigmaV_ubound",
    TRUE ~ scenario
    )) |>
  separate(scenario, c("parameter", "bound")) |>
  pivot_longer(names_to = "risk_measures", cols = c(VaR_loss, VaR_ERC)) |>
  pivot_wider(names_from = bound, values_from = value)

range_data |>
  filter(risk_measures == "VaR_loss") |>
  mutate(porttype = case_when(
    porttype == "highequity" ~ "High-Equity",
    porttype == "highfixed" ~ "High-Fixed",
    TRUE ~ porttype 
    )) |>
  ggplot(aes(x = parameter)) +
  geom_linerange(aes(ymin = lbound, ymax = ubound), size = 10, color = "gray70") +
  geom_text(aes(y = lbound, label = scales::percent(lbound)), size = 3.5) +
  geom_text(aes(y = ubound, label = scales::percent(ubound)), size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  ylab("The Range of VaR (Worst 5th percentile) Asset Loss at 10th Year") +
  xlab(NULL) +
  theme_bw() +
  facet_wrap(~ porttype, scales = "fixed",  ncol = 1)

ggsave("FigureVaRLoss.png")


range_data |>
  filter(risk_measures == "VaR_ERC") |>
  mutate(porttype = case_when(
    porttype == "highequity" ~ "High-Equity",
    porttype == "highfixed" ~ "High-Fixed",
    TRUE ~ porttype 
  )) |>
  ggplot(aes(x = parameter)) +
  geom_linerange(aes(ymin = lbound, ymax = ubound), size = 10, color = "gray70") +
  geom_text(aes(y = lbound, label = scales::percent(lbound)), size = 3.5) +
  geom_text(aes(y = ubound, label = scales::percent(ubound)), size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  ylab("The Range of VaR (Highest 95th percentile) Employer Contribution Increase at 10th Year") +
  xlab(NULL) +
  theme_bw() +
  facet_wrap(~ porttype, scales = "fixed",  ncol = 1)

ggsave("FigureVaRERC.png")

