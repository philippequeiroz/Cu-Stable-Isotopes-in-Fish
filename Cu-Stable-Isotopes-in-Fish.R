###############################################################
### Cu Stable Isotopes in European seabass (Dicentrarchus labrax)
###############################################################

############################
### 1. Packages
############################

library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(writexl)

############################
### 2. Import data
############################

dados <- read_excel("ISOPESQ_Cu_seabass.xlsx")

############################
### 3. Rename variables
############################

dados <- dados %>%
  rename(
    Estuary   = Estuary_or_Zone,
    Length    = Total_Length_cm,
    Cu_conc   = Cu_mg_kg_dw,
    Cu_burden = Cu_mg
  )

############################
### 4. Theme
############################

theme_cu <- theme_classic(base_size = 15) +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 13),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

###############################################################
### ETAPA 1 – SAMPLE CHARACTERIZATION
###############################################################

Tabela1 <- dados %>%
  group_by(Estuary) %>%
  summarise(
    
    n = n(),
    
    Mean_Age = mean(Age),
    SD_Age = sd(Age),
    Min_Age = min(Age),
    Max_Age = max(Age),
    
    Mean_Length = mean(Length),
    SD_Length = sd(Length),
    Min_Length = min(Length),
    Max_Length = max(Length),
    
    Mean_Cu = mean(Cu_conc),
    SD_Cu = sd(Cu_conc),
    Min_Cu = min(Cu_conc),
    Max_Cu = max(Cu_conc),
    
    Mean_Burden = mean(Cu_burden),
    SD_Burden = sd(Cu_burden),
    Min_Burden = min(Cu_burden),
    Max_Burden = max(Cu_burden),
    
    Mean_d65Cu = mean(d65Cu),
    SD_d65Cu = sd(d65Cu),
    Min_d65Cu = min(d65Cu),
    Max_d65Cu = max(d65Cu),
    
    .groups = "drop"
  )

Tabela1

View(Tabela1)

write_xlsx(
  Tabela1,
  "Tabelas/Tabela1_SeabassDescriptiveStatistics.xlsx"
)

###############################################################
### FIGURE S1 – Length distribution
###############################################################

fig_length <- ggplot(
  dados,
  aes(
    x = Estuary,
    y = Length,
    fill = Estuary
  )
) +
  geom_boxplot(width = 0.6) +
  labs(
    x = "Estuary",
    y = "Total length (cm)"
  ) +
  theme_cu

fig_length

ggsave(
  "Figuras/Fig_S1_Length_by_Estuary.png",
  fig_length,
  width = 16,
  height = 12,
  units = "cm",
  dpi = 600
)

###############################################################
### FIGURE S2 – Age distribution
###############################################################

fig_age_dist <- ggplot(
  dados,
  aes(
    x = factor(Age),
    fill = Estuary
  )
) +
  geom_bar(position = "dodge") +
  labs(
    x = "Age class",
    y = "Number of fish",
    fill = "Estuary"
  ) +
  theme_classic(base_size = 15)

fig_age_dist

ggsave(
  "Figuras/Fig_S2_Age_distribution.png",
  fig_age_dist,
  width = 16,
  height = 12,
  units = "cm",
  dpi = 600
)

###############################################################
### FIGURE S3 – Cu body burden
###############################################################

fig_burden <- ggplot(
  dados,
  aes(
    x = Estuary,
    y = Cu_burden,
    fill = Estuary
  )
) +
  geom_boxplot(width = 0.6) +
  labs(
    x = "Estuary",
    y = "Cu body burden (mg)"
  ) +
  theme_cu

fig_burden

ggsave(
  "Figuras/Fig_S3_Cu_body_burden.png",
  fig_burden,
  width = 16,
  height = 12,
  units = "cm",
  dpi = 600
)

###############################################################
### ETAPA 2 – DO ESTUARIES DIFFER?
###############################################################

############################
### Length
############################

modelo_length <- lm(Length ~ Estuary, data = dados)

anova(modelo_length)

summary(modelo_length)

shapiro.test(residuals(modelo_length))

leveneTest(Length ~ Estuary, data = dados)

###############################################################

############################
### Age
############################

fig_age <- ggplot(
  dados,
  aes(
    x = Estuary,
    y = Age,
    fill = Estuary
  )
) +
  geom_boxplot(width = 0.6) +
  labs(
    x = "Estuary",
    y = "Age (years)"
  ) +
  theme_cu

fig_age

modelo_age <- lm(Age ~ Estuary, data = dados)

anova(modelo_age)

summary(modelo_age)

shapiro.test(residuals(modelo_age))

leveneTest(Age ~ Estuary, data = dados)

###############################################################

############################
### Cu body burden
############################

modelo_burden <- lm(Cu_burden ~ Estuary, data = dados)

anova(modelo_burden)

summary(modelo_burden)

shapiro.test(residuals(modelo_burden))

leveneTest(Cu_burden ~ Estuary, data = dados)

###############################################################

############################
### Cu concentration
############################

fig_conc <- ggplot(
  dados,
  aes(
    x = Estuary,
    y = Cu_conc,
    fill = Estuary
  )
) +
  geom_boxplot(width = 0.6) +
  labs(
    x = "Estuary",
    y = expression(Cu~concentration~(mg~kg^{-1}~dw))
  ) +
  theme_cu

fig_conc

ggsave(
  "Figuras/Fig_S4_Cu_concentration.png",
  fig_conc,
  width = 16,
  height = 12,
  units = "cm",
  dpi = 600
)

modelo_conc <- lm(Cu_conc ~ Estuary, data = dados)

anova(modelo_conc)

summary(modelo_conc)

shapiro.test(residuals(modelo_conc))

leveneTest(Cu_conc ~ Estuary, data = dados)

if (anova(modelo_conc)$`Pr(>F)`[1] < 0.05) {
  TukeyHSD(aov(Cu_conc ~ Estuary, data = dados))
}

###############################################################

############################
### δ65Cu
############################

fig_d65Cu <- ggplot(
  dados,
  aes(
    x = Estuary,
    y = d65Cu,
    fill = Estuary
  )
) +
  geom_boxplot(width = 0.6) +
  labs(
    x = "Estuary",
    y = expression(delta^65*Cu~("\u2030"))
  ) +
  theme_cu

fig_d65Cu

ggsave(
  "Figuras/Fig_S5_d65Cu.png",
  fig_d65Cu,
  width = 16,
  height = 12,
  units = "cm",
  dpi = 600
)

modelo_d65Cu <- lm(d65Cu ~ Estuary, data = dados)

anova(modelo_d65Cu)

summary(modelo_d65Cu)

shapiro.test(residuals(modelo_d65Cu))

leveneTest(d65Cu ~ Estuary, data = dados)

if (anova(modelo_d65Cu)$`Pr(>F)`[1] < 0.05) {
  TukeyHSD(aov(d65Cu ~ Estuary, data = dados))
}

###############################################################
### ETAPA 3 – ONTOGENETIC VARIATION
###############################################################

############################
### 3.1 Cu body burden x Length
############################

fig_burden_length <- ggplot(
  dados,
  aes(
    x = Length,
    y = Cu_burden
  )
) +
  geom_point(size = 2.8) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    x = "Total length (cm)",
    y = "Cu body burden (mg)"
  ) +
  theme_cu

fig_burden_length

ggsave(
  "Figuras/Fig01_CuBurden_Length.png",
  fig_burden_length,
  width = 16,
  height = 12,
  units = "cm",
  dpi = 600
)

m1_burden <- lm(
  Cu_burden ~ Length,
  data = dados
)

summary(m1_burden)

anova(m1_burden)

par(mfrow = c(2,2))
plot(m1_burden)
par(mfrow = c(1,1))

m2_burden <- lm(
  Cu_burden ~ Length + Estuary,
  data = dados
)

m3_burden <- lm(
  Cu_burden ~ Length * Estuary,
  data = dados
)

anova(
  m1_burden,
  m2_burden,
  m3_burden
)

AIC(
  m1_burden,
  m2_burden,
  m3_burden
)

fig_burden_length_estuary <-
  ggplot(
    dados,
    aes(
      Length,
      Cu_burden,
      colour = Estuary
    )
  ) +
  geom_point(size = 3) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    x = "Total length (cm)",
    y = "Cu body burden (mg)"
  ) +
  theme_classic(base_size = 15)

fig_burden_length_estuary

ggsave(
  "Figuras/fig_burden_length_estuary.png",
  fig_burden_length_estuary,
  width = 16,
  height = 12,
  units = "cm",
  dpi = 600
)

############################
### 3.2 Cu concentration x Length
############################

fig_conc_length <- ggplot(
  dados,
  aes(
    x = Length,
    y = Cu_conc
  )
) +
  geom_point(size = 2.8) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    x = "Total length (cm)",
    y = expression(Cu~concentration~(mg~kg^{-1}~dw))
  ) +
  theme_cu

fig_conc_length

ggsave(
  "Figuras/Fig02_CuConcentration_Length.png",
  fig_conc_length,
  width = 16,
  height = 12,
  units = "cm",
  dpi = 600
)

############################
### Linear model
############################

m1_conc <- lm(
  Cu_conc ~ Length,
  data = dados
)

summary(m1_conc)

anova(m1_conc)

############################
### Diagnostics
############################

par(mfrow = c(2,2))
plot(m1_conc)
par(mfrow = c(1,1))

############################
### Length + Estuary
############################

m2_conc <- lm(
  Cu_conc ~ Length + Estuary,
  data = dados
)

############################
### Length * Estuary
############################

m3_conc <- lm(
  Cu_conc ~ Length * Estuary,
  data = dados
)

############################
### Model comparison
############################

anova(
  m1_conc,
  m2_conc,
  m3_conc
)

AIC(
  m1_conc,
  m2_conc,
  m3_conc
)

############################
### Figure by estuary
############################

fig_conc_length_estuary <-
  ggplot(
    dados,
    aes(
      Length,
      Cu_conc,
      colour = Estuary
    )
  ) +
  geom_point(size = 3) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    x = "Total length (cm)",
    y = expression(Cu~concentration~(mg~kg^{-1}~dw))
  ) +
  theme_classic(base_size = 15)

fig_conc_length_estuary

ggsave(
  "Figuras/Fig02_CuConcentration_Length_Estuary.png",
  fig_conc_length_estuary,
  width = 16,
  height = 12,
  units = "cm",
  dpi = 600
)

############################
### 3.3 δ65Cu x Length
############################

fig_d65Cu_length <- ggplot(
  dados,
  aes(
    x = Length,
    y = d65Cu
  )
) +
  geom_point(size = 2.8) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    x = "Total length (cm)",
    y = expression(delta^65*Cu~("\u2030"))
  ) +
  theme_cu

fig_d65Cu_length

ggsave(
  "Figuras/Fig03_d65Cu_Length.png",
  fig_d65Cu_length,
  width = 16,
  height = 12,
  units = "cm",
  dpi = 600
)

############################
### Linear model
############################

m1_d65Cu <- lm(
  d65Cu ~ Length,
  data = dados
)

summary(m1_d65Cu)

anova(m1_d65Cu)

############################
### Diagnostics
############################

par(mfrow = c(2,2))
plot(m1_d65Cu)
par(mfrow = c(1,1))

############################
### Length + Estuary
############################

m2_d65Cu <- lm(
  d65Cu ~ Length + Estuary,
  data = dados
)

############################
### Length * Estuary
############################

m3_d65Cu <- lm(
  d65Cu ~ Length * Estuary,
  data = dados
)

############################
### Model comparison
############################

anova(
  m1_d65Cu,
  m2_d65Cu,
  m3_d65Cu
)

AIC(
  m1_d65Cu,
  m2_d65Cu,
  m3_d65Cu
)

############################
### Figure by estuary
############################

fig_d65Cu_length_estuary <-
  ggplot(
    dados,
    aes(
      Length,
      d65Cu,
      colour = Estuary
    )
  ) +
  geom_point(size = 3) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    x = "Total length (cm)",
    y = expression(delta^65*Cu~("\u2030"))
  ) +
  theme_classic(base_size = 15)

fig_d65Cu_length_estuary

ggsave(
  "Figuras/Fig03_d65Cu_Length_Estuary.png",
  fig_d65Cu_length_estuary,
  width = 16,
  height = 12,
  units = "cm",
  dpi = 600
)

###############################################################
### ETAPA 4 – Relationships among Cu variables
###############################################################

library(tidyverse)
library(car)
library(Hmisc)

###########################################################
# ETAPA 4.1
# Correlation matrix
###########################################################

vars <- dados %>%
  dplyr::select(
    Age,
    Length,
    Cu_burden,
    Cu_conc,
    d65Cu
  )

correlation_matrix <-
  Hmisc::rcorr(
    as.matrix(vars),
    type = "pearson"
  )

correlation_matrix

###############################################
### 4.1 Matriz de correlação por estuário
###############################################

dados %>%
  group_by(Estuary) %>%
  group_modify(~{
    print(unique(.x$Estuary))
    print(Hmisc::rcorr(
      as.matrix(
        .x %>%
          dplyr::select(
            Length,
            Cu_burden,
            Cu_conc,
            d65Cu
          )
      ),
      type = "pearson"
    ))
    tibble()
  })

###########################################################
# ETAPA 4.2
# Cu concentration × Cu burden
###########################################################

fig_conc_burden <-
  ggplot(
    dados,
    aes(
      Cu_burden,
      Cu_conc
    )
  ) +
  geom_point(size = 3) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    x = "Cu body burden (mg)",
    y = expression(Cu~concentration~(mg~kg^{-1}~dw))
  ) +
  theme_cu

fig_conc_burden

ggsave(
  "Figuras/Fig04_CuConcentration_CuBurden.png",
  fig_conc_burden,
  width = 16,
  height = 12,
  units = "cm",
  dpi = 600
)

### Modelo simples

m1_conc_burden <- lm(
  Cu_conc ~ Cu_burden,
  data = dados
)

summary(m1_conc_burden)

anova(m1_conc_burden)

par(mfrow = c(2,2))
plot(m1_conc_burden)
par(mfrow = c(1,1))

# Normalidade dos resíduos
shapiro.test(residuals(m1_conc_burden))

# Homocedasticidade
car::ncvTest(m1_conc_burden)

### Modelos com estuário

m2_conc_burden <- lm(
  Cu_conc ~ Cu_burden + Estuary,
  data = dados
)

m3_conc_burden <- lm(
  Cu_conc ~ Cu_burden * Estuary,
  data = dados
)

anova(
  m1_conc_burden,
  m2_conc_burden,
  m3_conc_burden
)

AIC(
  m1_conc_burden,
  m2_conc_burden,
  m3_conc_burden
)

### Figura por estuário

fig_conc_burden_estuary <-
  ggplot(
    dados,
    aes(
      Cu_burden,
      Cu_conc,
      colour = Estuary
    )
  ) +
  geom_point(size = 3) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    x = "Cu body burden (mg)",
    y = expression(Cu~concentration~(mg~kg^{-1}~dw))
  ) +
  theme_cu

fig_conc_burden_estuary

ggsave(
  "Figuras/Fig05_CuConcentration_CuBurden_Estuary.png",
  fig_conc_burden_estuary,
  width = 16,
  height = 12,
  units = "cm",
  dpi = 600
)

###########################################################
# ETAPA 4.3
# d65Cu × Cu burden
###########################################################

fig_d65Cu_burden <-
  ggplot(
    dados,
    aes(
      Cu_burden,
      d65Cu
    )
  ) +
  geom_point(size = 3) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    x = "Cu body burden (mg)",
    y = expression(delta^65*Cu~("\u2030"))
  ) +
  theme_cu

fig_d65Cu_burden

ggsave(
  "Figuras/Fig06_d65Cu_CuBurden.png",
  fig_d65Cu_burden,
  width = 16,
  height = 12,
  units = "cm",
  dpi = 600
)

### Modelo simples

m1_d65Cu_burden <- lm(
  d65Cu ~ Cu_burden,
  data = dados
)

summary(m1_d65Cu_burden)

anova(m1_d65Cu_burden)

par(mfrow = c(2,2))
plot(m1_d65Cu_burden)
par(mfrow = c(1,1))

# Normalidade dos resíduos
shapiro.test(residuals(m1_d65Cu_burden))

# Homocedasticidade
car::ncvTest(m1_d65Cu_burden)

### Modelos com estuário

m2_d65Cu_burden <- lm(
  d65Cu ~ Cu_burden + Estuary,
  data = dados
)

m3_d65Cu_burden <- lm(
  d65Cu ~ Cu_burden * Estuary,
  data = dados
)

anova(
  m1_d65Cu_burden,
  m2_d65Cu_burden,
  m3_d65Cu_burden
)

AIC(
  m1_d65Cu_burden,
  m2_d65Cu_burden,
  m3_d65Cu_burden
)

### Figura por estuário

fig_d65Cu_burden_estuary <-
  ggplot(
    dados,
    aes(
      Cu_burden,
      d65Cu,
      colour = Estuary
    )
  ) +
  geom_point(size = 3) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    x = "Cu body burden (mg)",
    y = expression(delta^65*Cu~("\u2030"))
  ) +
  theme_cu

fig_d65Cu_burden_estuary

ggsave(
  "Figuras/Fig07_d65Cu_CuBurden_Estuary.png",
  fig_d65Cu_burden_estuary,
  width = 16,
  height = 12,
  units = "cm",
  dpi = 600
)

###########################################################
# ETAPA 4.4
# d65Cu × Cu concentration
###########################################################

fig_d65Cu_conc <-
  ggplot(
    dados,
    aes(
      Cu_conc,
      d65Cu
    )
  ) +
  geom_point(size = 3) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    x = expression(Cu~concentration~(mg~kg^{-1}~dw)),
    y = expression(delta^65*Cu~("\u2030"))
  ) +
  theme_cu

fig_d65Cu_conc

ggsave(
  "Figuras/Fig08_d65Cu_CuConcentration.png",
  fig_d65Cu_conc,
  width = 16,
  height = 12,
  units = "cm",
  dpi = 600
)

### Modelo simples

m1_d65Cu_conc <- lm(
  d65Cu ~ Cu_conc,
  data = dados
)

summary(m1_d65Cu_conc)

anova(m1_d65Cu_conc)

par(mfrow = c(2,2))
plot(m1_d65Cu_conc)
par(mfrow = c(1,1))

# Normalidade dos resíduos
shapiro.test(residuals(m1_d65Cu_conc))

# Homocedasticidade
car::ncvTest(m1_d65Cu_conc)

### Modelo com estuários

m2_d65Cu_conc <- lm(
  d65Cu ~ Cu_conc + Estuary,
  data = dados
)

m3_d65Cu_conc <- lm(
  d65Cu ~ Cu_conc * Estuary,
  data = dados
)

anova(
  m1_d65Cu_conc,
  m2_d65Cu_conc,
  m3_d65Cu_conc
)

AIC(
  m1_d65Cu_conc,
  m2_d65Cu_conc,
  m3_d65Cu_conc
)

### Figura por estuário

fig_d65Cu_conc_estuary <-
  ggplot(
    dados,
    aes(
      Cu_conc,
      d65Cu,
      colour = Estuary
    )
  ) +
  geom_point(size = 3) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    x = expression(Cu~concentration~(mg~kg^{-1}~dw)),
    y = expression(delta^65*Cu~("\u2030"))
  ) +
  theme_cu + theme(
  legend.position = "right"
)

fig_d65Cu_conc_estuary

ggsave(
  "Figuras/Fig09_d65Cu_CuConcentration_Estuary.png",
  fig_d65Cu_conc_estuary,
  width = 16,
  height = 12,
  units = "cm",
  dpi = 600
)

### Figura por estuário
#


