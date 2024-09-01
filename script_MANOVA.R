library(tinytex)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(car)
library(broom)
library(readxl)
library(knitr)
library(tibble)
library(magrittr)
library(mvtnorm)
library(dplyr)
library(tidyr)
library(purrr)
library(psych)
library(tables)
library(kableExtra)


base <- read_excel("C:/Users/Julien/Desktop/JULIEN/Linkedin/Travaux_Rstudio/lois_discrete_app/MANOVA/base.xlsx")
base$Groupe <- factor(base$Groupe, levels = c("Témoin", "Traitement"))

base |> head(5) |> 
  kbl(align = "c", caption = "Entête de la base de données") |> 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F, 
                position = "center") |> 
  row_spec(0, bold = TRUE, color = "white", background = "#4CAF50") |> 
  row_spec(1, background = "#f2f2f2")

stat <- base |> 
  group_by(Groupe) |> 
  get_summary_stats(HbA1c, pa_systolique, creatinine_s, type ="common")

stat[, c(1:6, 8:9)] |> kbl(caption="Statistiques descriptives", digits = 2, align = "c")|> 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F, 
                position = "center") |> 
  row_spec(0, bold = TRUE, color = "white", background = "#4CAF50") |> 
  row_spec(1, background = "#f2f2f2")

ggboxplot(
  base, x = "Groupe", y = "HbA1c", merge = TRUE, fill = "Groupe", palette = "jco", add = "jitter", ylim = c(min(base[,"HbA1c"]), max(base[,"HbA1c"])))

ggboxplot(
  base, x = "Groupe", y = "pa_systolique", merge = TRUE, fill = "Groupe", palette = "npg", add = "jitter", ylim = c(min(base[,"pa_systolique"]), max(base[,"pa_systolique"])))

ggboxplot(
  base, x = "Groupe", y = "creatinine_s", merge = TRUE, fill = "Groupe", palette = "simpsons", add = "jitter", ylim = c(min(base[,"creatinine_s"]), max(base[,"creatinine_s"])))

base |> 
  group_by(Groupe) |> 
  summarise(N = n()) |> 
  kbl(caption = "Nombre d'observations de chaque groupe") |> 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F, 
                position = "center") |> 
  row_spec(0, bold = TRUE, color = "white", background = "#4CAF50") |> 
  row_spec(1, background = "#f2f2f2")

base |> 
  group_by(Groupe) |> 
  shapiro_test(HbA1c, pa_systolique, creatinine_s) |> 
  arrange(variable) |> 
  kbl(caption = "Normalité multivariée", align = "c", digits = 5) |> 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F, 
                position = "center") |> 
  row_spec(0, bold = TRUE, color = "white", background = "#4CAF50") |> 
  row_spec(1, background = "#f2f2f2")

base |> 
  select(HbA1c, pa_systolique, creatinine_s) |> 
  mshapiro_test() |> 
  kbl(caption = "Normalité multivariée")|> 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F, 
                position = "center") |> 
  row_spec(0, bold = TRUE, color = "white", background = "#4CAF50") |> 
  row_spec(1, background = "#f2f2f2")


## Test de corrélations 

base |>  cor_test(HbA1c, pa_systolique, creatinine_s) |> 
  kbl(caption = "Corrélations", digits = 3)|> 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F, 
                position = "center") |> 
  row_spec(0, bold = TRUE, color = "white", background = "#4CAF50") |> 
  row_spec(1, background = "#f2f2f2")

## Hypothèse de linéarité 

library(GGally)
results <- base |> 
  select(HbA1c, pa_systolique, creatinine_s) |> 
  group_by(base$Groupe) |> 
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results$plots[1]

## Hypothèse de linéarité 

results$plots[2]

## Hypothèses d'homogénéité des covariances 

box_m(base[, c("HbA1c", "pa_systolique", "creatinine_s")], base$Groupe) |> 
  kbl(caption = "Résultats du test", digits = 3)|> 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F, 
                position = "center") |> 
  row_spec(0, bold = TRUE, color = "white", background = "#4CAF50") |> 
  row_spec(1, background = "#f2f2f2")

## Hypothèse d’homogénéité de la variance

base |> 
  gather(key = "variable", value = value, HbA1c, pa_systolique, creatinine_s) |> 
  group_by(variable) |> 
  levene_test(value ~ Groupe) |> 
  kbl(caption = "Test d'homogénéité de la variance", digits = 3, align = "c")|> 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F, 
                position = "center") |> 
  row_spec(0, bold = TRUE, color = "white", background = "#4CAF50") |> 
  row_spec(1, background = "#f2f2f2")

## Réalisation de MANOVA à un facteur

model <- lm(cbind(HbA1c, pa_systolique, creatinine_s) ~ Groupe, base)
modele <-  Manova(model, test.statistic = "Pillai") 
results <- data.frame(
  Term = c("Groupe"),
  Df = 1,
  `Test Stat` = 0.91976,
  `Approx F` = 213.96,
  `Num Df` = 3,
  `Den Df` = 56,
  `Pr(>F)` = "< 2.2e-16***"
)

results |> 
  kbl(align = "c", caption = "Résultats du test MANOVA - Statistique de Pillai") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F, 
                position = "center") |> 
  row_spec(0, bold = TRUE, color = "white", background = "#4CAF50") |> 
  row_spec(1, background = "#f2f2f2")

