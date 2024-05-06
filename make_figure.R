library(dplyr)
library(readxl)
library(zoo)
library(ggplot2)
library(ggpubr)

d = read_excel(path = "policy_brief.xlsx")

d$law = na.approx(d$law)
d$sequence = na.approx(d$sequence)
d$cost = na.approx(d$cost)

inflation_ratio = 100000
color1 = "#66A182"
color2 = "#D1495B"

d$law = d$law * inflation_ratio

plt = ggplot(d, aes(x = year)) +
  geom_line(aes(y = sequence, color = "Genomes Sequenced in the US"), size = 1.2) +
  geom_line(aes(y = law, color = "Relavant Laws Established"), size = 1.2) +
  # geom_line(aes(y = cost, color = "Average Sequence Cost"), size = 1.2) +
  # scale_y_continuous(labels = scales::label_number()) +
  scale_y_continuous(labels = scales::label_number(),
                     sec.axis = sec_axis(~./inflation_ratio, name = "Relavant Laws Established", 
                                         breaks = seq(0, 10, by = 5), 
                                         labels = seq(0, 10, by = 5)  )) +
  scale_color_manual(values = c("Genomes Sequenced in the US" = color2, 
                                "Relavant Laws Established" = color1),
                     name = "") +
  labs(y = "Genomes Sequenced in the US", 
       x = "Year") +
  ggtitle(label = NULL, subtitle = "Genome Sequencing and Regulations") +
  theme_cleveland() +
  theme(text = element_text(size = 14),
        axis.title.y = element_text(color = color2),
        axis.text.y = element_text(color = color2),
        axis.title.y.right = element_text(color = color1),
        axis.text.y.right = element_text(color = color1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom"); plt

ggsave(filename = "policy_brief.png", plot = plt, device = "png", dpi = 1200,
       width = 6, height = 5)



