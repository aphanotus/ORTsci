# A modified version of Figure 1 from Colella et al. 2021 BioScience
# https://academic.oup.com/bioscience/article/71/4/405/6030117
# Combining data from Figure 1 and Table 1, weighted by relative funding levels.

# Load packages
x <- c("tidyverse","viridis")
invisible(lapply(x, require, character.only = TRUE))

df <- read.csv("Colella.et.al.2021.data.csv")

dim(df)
head(df)

(sample.sizes <- c(with(df, by(class,class,length))))

df$research_dollars[is.na(df$research_dollars)] <- 1

open.requirements.for.agencies.journals.plot <- df %>%
  pivot_longer(cols=4:9, names_to = "data.type") %>%
  mutate(data.type = factor(data.type, levels = c("general","statement","code","software","sequences","specimens"))) %>%
  ggplot(aes(x=data.type, fill=value)) +
  theme_bw() +
  theme(legend.position="bottom",
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1) ) +
  facet_grid(.~class, scales = "free",
             labeller=labeller(
               class = c(funding = paste0("US Funding Agencies (n=",sample.sizes["funding"],")*"),
                         journal = paste0("EEBS Journals (n=",sample.sizes["journal"],")")))) +
  geom_bar(aes(weight = research_dollars), position = "fill", color = "gray10") +
  scale_fill_manual(
    name="",
    values = c("gray95",viridis(3, begin=0.2, end=0.5, option = "magma", direction = -1)),
    labels=c("no policy","encouraged","expected","required")) +
  labs(x="data type", y="frequency",
       caption = "Data from Colella et al. 2021 BioScience\n*Agency requirements weighted by relative funding levels")
open.requirements.for.agencies.journals.plot

ggsave("open.requirements.for.agencies.journals.pdf", open.requirements.for.agencies.journals.plot,
       height = 5, width = 7, scale = 1)
