library(here)
library(data.table)
library(ggplot2)
library(MetBrewer)
library(patchwork)

#chain operator for data.table
DT <- `[`


allqs <- data.table::fread(here("data", "availableQsbyhor.csv")) |>
  DT(, diffin_factor := paste0(diffin * 4) |>
       factor(levels = c("2", "6", "18", "19"))) |>
  DT(, forecast_origin_quarter := paste0(forecast_year, "Q", forecast_quarter)) |>
  DT(, is_present := "Quarter was queried in given edition")



plot_present <- ggplot(allqs, aes(x = diffin_factor, y = forecast_origin))  +
  scale_y_continuous(breaks = c(1999.25:2024.25),
                     labels = paste0(1999:2024, "Q1"))+
  geom_tile(aes(fill = is_present), color = "black") +
  scale_fill_met_d("Hokusai2") +
  ylab("Forecast Origin (Quarterly Frequency)") +
  xlab("Number of Quarters Ahead") +
  ggtitle("Queried Quarters in given release of EU-SPF") +
  theme_minimal() %+replace%
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

plot_num_responses <- ggplot(allqs, aes(x = diffin_factor, y = forecast_origin))  +
  scale_y_continuous(breaks = c(1999.25:2024.25),
                     labels = paste0(1999:2024, "Q1"))+
  geom_tile(aes(fill = num_respones), color = "black") +
  scale_fill_continuous(name = "Number of Responses") +
  ylab("Forecast Origin (Quarterly Frequency)") +
  xlab("Number of Quarters Ahead") +
  ggtitle("Number of responses: Queried Quarters in given release of EU-SPF") +
  theme_minimal() %+replace%
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))


pdf(here("QueriedQuarters.pdf"), width = 8, height = 12)
plot_present /
  plot_spacer() /
  plot_num_responses +
  plot_layout(heights = c(4,1,4))
dev.off()
