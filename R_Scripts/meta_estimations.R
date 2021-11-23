
if(!require("meta")){install.packages("meta", dependencies = TRUE); require("meta")}
if(!require("scales")){install.packages("scales", dependencies = TRUE); require("scales")}
if(!require("car")){install.packages("car", dependencies = TRUE); require("car")}
if(!require("dplyr")){install.packages("dplyr", dependencies = TRUE); require("dplyr")}
if(!require("ggplot2")){install.packages("ggplot2", dependencies = TRUE); require("ggplot2")}
if(!require("ggrepel")){install.packages("ggrepel", dependencies = TRUE); require("ggrepel")}
if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
library(readxl)

metadata <- read_excel("metadata.xlsx")

metaeff.simdiff <- metadata$eff[grepl("Simple/Difficult", metadata$IAT)==TRUE]
metaeff.se.simdiff <- metadata$se[grepl("Simple/Difficult", metadata$IAT)==TRUE]
metaeff.absdrink <- metadata$eff[grepl("Abstain/Drink", metadata$IAT)==TRUE]
metaeff.se.absdrink <- metadata$se[grepl("Abstain/Drink", metadata$IAT)==TRUE]

meta.simdiff <- metagen(TE = metaeff.simdiff, seTE = metaeff.se.simdiff, sm = "MD")
summary(meta.simdiff)

meta.absdrink <- metagen(TE = metaeff.absdrink, seTE = metaeff.se.absdrink, sm = "MD")
summary(meta.absdrink)


metadata$ValenceList <-  extract_numeric(metadata$`Embedding+VL`)
view(metadata)

meta_simdiff <- metadata %>%
  filter(IAT == 'Simple/Difficult')


meta_simdiff_data <- meta_simdiff%>%
  filter(ValenceList == 9)

view(meta_simdiff)

meta_simdiff %>%
  ggplot(aes(ValenceList, eff)) + geom_point() + 
  geom_point(data = meta_simdiff_data, aes(ValenceList, eff), color = 'red', size = 3) + 
  theme_bw() + scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), label = c("Valenced 1", "Valenced 2", "Valenced 3", "Valenced 4", "Valenced 5", "Valenced 6", "Valenced 7", "Valenced 8", "Meta-Analytic Estimate"))

meta_absdrink <- metadata %>%
  filter(IAT == 'Abstain/Drink')

meta_absdrink_data <- meta_absdrink%>%
  filter(ValenceList == 9)

meta_absdrink %>%
  ggplot(aes(ValenceList, eff)) + geom_point() + 
  geom_point(data = meta_absdrink_data, aes(ValenceList, eff), color = 'red', size = 3) + 
  theme_bw() + scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), label = c("Valenced 1", "Valenced 2", "Valenced 3", "Valenced 4", "Valenced 5", "Valenced 6", "Valenced 7", "Valenced 8", "Meta-Analytic Estimate"))

