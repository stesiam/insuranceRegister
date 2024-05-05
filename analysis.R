# Import libraries

library(readr)
library(dplyr)
library(ggplot2)
library(glue)
library(ggtext)

# Read data

ins_companies = readr::read_csv("GRins.csv")

# Rename columns

ins_companies = ins_companies |>
  setNames(c("ID", "Name", "insCompType", "insCompCat", "insStatus", "Country", "OrigCountry",
             "Site", "Link", "Address"))


## Plot 1 - texts

title_text = glue("<b>Type of Insurance Companies</b>")
subtitle_text = glue("Based on the register of insurance comapnies which is being <br> maintained by Bank of Greece,
                     the vast majority of insurance <br> undertakings are focused on non-life perils.")
caption_text = glue("stesiam, 2024")

plot1  = ins_companies |>
  dplyr::filter(insStatus == "Active") |>
  count(insCompCat) %>%
  mutate(pct = round( (n / sum(n)) * 100, digits =1)) %>%
  ggplot(.,aes(x = reorder(insCompCat, -n), y = n, fill = insCompCat)) +
  geom_col() +
  geom_richtext(aes(x = reorder(insCompCat, -n), y = n+40, label = glue("<b>{n}</b> | {pct}%")), color = "white",
                fill = NA, label.color = NA, size = 6) +
    labs(title = title_text,
    subtitle =subtitle_text,
    caption = caption_text,
    x = "",
    y = ""
  ) +
  scale_fill_manual( values = c('#ee8080','#2b374b','#178a84')) +
  theme_light(base_size = 13) +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.title = element_markdown(color = "white"),
        plot.subtitle = element_markdown(color = "white"),
        plot.caption = element_markdown(color = "white"),
        legend.position = "none",
        plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black"),
        axis.text = element_markdown(color = "white")
  )

ggsave("images/plot1.png", plot1,
       width = 6,
       height = 4)

