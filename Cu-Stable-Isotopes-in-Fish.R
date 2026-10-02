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

#### teste Git

dados %>%
  group_by(Estuary) %>%
  group_split() %>%
  lapply(function(x) {
    
    cat("\n\n============================\n")
    cat("Estuary:", unique(x$Estuary), "\n")
    cat("============================\n")
    
    Hmisc::rcorr(
      as.matrix(
        x %>%
          dplyr::select(
            Length,
            Cu_burden,
            Cu_conc,
            d65Cu
          )
      ),
      type = "pearson"
    )
    
  })

# ============================================================
# REGRESSÕES POR ESTUÁRIO
# ============================================================

library(dplyr)
library(broom)
library(writexl)

# ------------------------------------------------------------
# 1. Definir as relações a serem testadas
# ------------------------------------------------------------

modelos <- list(
  "Length_Cu_burden" = Cu_burden ~ Length,
  "Length_Cu_conc"   = Cu_conc ~ Length,
  "Length_d65Cu"     = d65Cu ~ Length,
  "Burden_Cu_conc"   = Cu_conc ~ Cu_burden,
  "Burden_d65Cu"     = d65Cu ~ Cu_burden,
  "Conc_d65Cu"       = d65Cu ~ Cu_conc
)


############################################################
### REGRESSÕES POR ESTUÁRIO
############################################################

library(dplyr)
library(writexl)

# Lista para armazenar os resultados
resultados <- list()

# Estuários
estuarios <- unique(dados$Estuary)

# Relações a testar
relacoes <- list(
  Length_Cu_burden = c("Cu_burden", "Length"),
  Length_Cu_conc   = c("Cu_conc", "Length"),
  Length_d65Cu     = c("d65Cu", "Length"),
  Burden_Cu_conc   = c("Cu_conc", "Cu_burden"),
  Burden_d65Cu     = c("d65Cu", "Cu_burden"),
  Conc_d65Cu       = c("d65Cu", "Cu_conc")
)

# Loop
for (nome_relacao in names(relacoes)) {
  
  resposta <- relacoes[[nome_relacao]][1]
  explicativa <- relacoes[[nome_relacao]][2]
  
  for (estuario in estuarios) {
    
    # Selecionar estuário
    dados_est <- dados %>%
      filter(Estuary == estuario)
    
    # Criar fórmula
    formula_modelo <- as.formula(
      paste(resposta, "~", explicativa)
    )
    
    # Modelo
    modelo <- lm(
      formula_modelo,
      data = dados_est
    )
    
    # Summary
    sum_modelo <- summary(modelo)
    coef <- sum_modelo$coefficients
    IC <- confint(modelo)
    
    # Resultado
    resultado <- data.frame(
      
      Estuary = estuario,
      Relationship = nome_relacao,
      
      Intercept = coef[1, "Estimate"],
      Intercept_SE = coef[1, "Std. Error"],
      Intercept_p = coef[1, "Pr(>|t|)"],
      
      Slope = coef[2, "Estimate"],
      Slope_SE = coef[2, "Std. Error"],
      Slope_p = coef[2, "Pr(>|t|)"],
      
      Slope_CI95_lower = IC[2, 1],
      Slope_CI95_upper = IC[2, 2],
      
      R2 = sum_modelo$r.squared,
      Adjusted_R2 = sum_modelo$adj.r.squared,
      
      F_value = sum_modelo$fstatistic[1],
      
      Model_p = pf(
        sum_modelo$fstatistic[1],
        sum_modelo$fstatistic[2],
        sum_modelo$fstatistic[3],
        lower.tail = FALSE
      ),
      
      N = nobs(modelo)
    )
    
    resultados[[length(resultados) + 1]] <- resultado
  }
}

# Juntar resultados
tabela_regressoes <- bind_rows(resultados)

# Organizar
tabela_regressoes <- tabela_regressoes %>%
  arrange(Relationship, Estuary)

# Mostrar no console
print(tabela_regressoes)

# Salvar Excel
write_xlsx(
  tabela_regressoes,
  "Tabelas/Regressions_by_Estuary.xlsx"
)

###### MAP #####

# ============================================================
# General study-area map with bathymetry and rivers
# Juvenile European seabass sampling:
# Gironde, Loire and Seine estuaries
# ============================================================

# Packages ------------------------------------------------------
library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(ggspatial)
library(marmap)
library(viridis)

# 1. Read sampling coordinates ---------------------------------
file <- "Copie de Fish_sampling_station_info_TO COMPLETE md_Phill.xlsx"

stations <- read_excel(
  file,
  sheet = "Planilha1"
)

# The records originally entered as 2018 were confirmed to be
# typographical errors and should be treated as 2019.
stations <- stations %>%
  mutate(
    Sampling_year = if_else(
      Sampling_year == 2018,
      2019L,
      as.integer(Sampling_year)
    )
  )

# 2. Convert coordinates to sf ---------------------------------
stations_sf <- st_as_sf(
  stations,
  coords = c("X_Longitude_dd", "Y_Latitude_dd"),
  crs = 4326,
  remove = FALSE
)

# 3. Colours used throughout the manuscript --------------------
estuary_colors <- c(
  "Gironde" = "#F8766D",
  "Loire"   = "#00BA38",
  "Seine"   = "#619CFF"
)

# 4. Regional geographic layers --------------------------------
countries <- ne_countries(
  scale = 50,
  returnclass = "sf"
)

coastline <- ne_coastline(
  scale = 50,
  returnclass = "sf"
)

# Natural Earth river/lake centerlines.
# If your rnaturalearth version does not provide this object
# directly, the code below downloads it at 50 m scale.
rivers <- ne_download(
  scale = 50,
  type = "rivers_lake_centerlines",
  category = "physical",
  returnclass = "sf"
)

# Keep only rivers/streams relevant to the study-area map.
# The Natural Earth layer is already spatially appropriate for
# a regional overview.
rivers_region <- st_crop(
  rivers,
  xmin = -6.0,
  xmax = 3.0,
  ymin = 43.0,
  ymax = 51.2
)

# 5. Bathymetry from NOAA --------------------------------------
# Download GEBCO/NOAA bathymetry for the regional extent.
# Resolution = 2 minutes is appropriate for the overview map
# while keeping the object manageable.
#
# This step requires an internet connection when the script is
# run for the first time.

bathymetry <- getNOAA.bathy(
  lon1 = -6.0,
  lon2 = 3.0,
  lat1 = 43.0,
  lat2 = 51.2,
  resolution = 2,
  keep = TRUE
)

# Convert bathymetry object to a data frame for ggplot.
bath_df <- fortify.bathy(bathymetry)

# Keep only ocean/deeper-water values.
# Land values are handled by the country layer.
bath_df <- bath_df %>%
  filter(!is.na(z), z <= 0)

# 6. Label positions --------------------------------------------
# Positions are approximate cartographic label positions chosen
# to avoid overlapping the sampling stations.
river_labels <- data.frame(
  river = c("Loire", "Seine", "Garonne", "Dordogne"),
  longitude = c(-1.8, 0.0, -0.55, -0.10),
  latitude  = c(47.25, 49.35, 44.85, 45.05)
)

estuary_labels <- stations %>%
  group_by(Estuary_or_Zone) %>%
  summarise(
    longitude = mean(X_Longitude_dd, na.rm = TRUE),
    latitude  = mean(Y_Latitude_dd, na.rm = TRUE),
    .groups = "drop"
  )

# 7. General regional map ---------------------------------------
map_general <- ggplot() +
  
  # Bathymetry
  geom_raster(
    data = bath_df,
    aes(
      x = x,
      y = y,
      fill = z
    ),
    interpolate = TRUE
  ) +
  
  # Land
  geom_sf(
    data = countries,
    fill = "grey92",
    colour = "grey45",
    linewidth = 0.25
  ) +
  
  # Rivers
  geom_sf(
    data = rivers_region,
    colour = "grey45",
    linewidth = 0.30,
    alpha = 0.80
  ) +
  
  # Coastline
  geom_sf(
    data = coastline,
    colour = "grey25",
    linewidth = 0.35
  ) +
  
  # Sampling stations
  geom_sf(
    data = stations_sf,
    aes(
      colour = Estuary_or_Zone
    ),
    size = 2.6,
    alpha = 0.95
  ) +
  
  # Estuary labels
  geom_label(
    data = estuary_labels,
    aes(
      x = longitude,
      y = latitude,
      label = Estuary_or_Zone,
      colour = Estuary_or_Zone
    ),
    fill = "white",
    alpha = 0.92,
    label.size = 0.25,
    size = 4.1,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  # River labels
  geom_text(
    data = river_labels,
    aes(
      x = longitude,
      y = latitude,
      label = river
    ),
    colour = "grey25",
    size = 3.6,
    fontface = "italic"
  ) +
  
  # Regional labels
  annotate(
    "text",
    x = -3.0,
    y = 46.0,
    label = "Bay of Biscay",
    size = 5.0,
    fontface = "italic",
    colour = "grey35"
  ) +
  
  annotate(
    "text",
    x = 0.0,
    y = 50.2,
    label = "English Channel",
    size = 4.0,
    fontface = "italic",
    colour = "grey35"
  ) +
  
  scale_fill_viridis_c(
    option = "mako",
    direction = 1,
    name = "Depth (m)",
    limits = c(-5000, 0),
    breaks = c(-4000, -2000, -1000, -500, 0),
    oob = scales::squish
  ) +
  
  scale_colour_manual(
    values = estuary_colors,
    breaks = names(estuary_colors),
    name = "Estuary"
  ) +
  
  coord_sf(
    xlim = c(-6.0, 3.0),
    ylim = c(43.0, 51.2),
    expand = FALSE
  ) +
  
  annotation_scale(
    location = "bl",
    width_hint = 0.20,
    text_cex = 0.8
  ) +
  
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = unit(1.0, "cm"),
    width = unit(1.0, "cm")
  ) +
  
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  
  theme_classic(base_size = 13) +
  
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black"),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.5
    )
  )

# 8. Display map ------------------------------------------------
map_general

# 9. Export -----------------------------------------------------
ggsave(
  filename = "Figure_Study_Area_General_Bathymetry.png",
  plot = map_general,
  width = 8.5,
  height = 7.0,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "Figure_Study_Area_General_Bathymetry.tiff",
  plot = map_general,
  width = 8.5,
  height = 7.0,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)


##### CLEAN VERSION #####

# 7. General regional map ---------------------------------------
map_general <- ggplot() +
  
  # Bathymetry
  geom_raster(
    data = bath_df,
    aes(
      x = x,
      y = y,
      fill = z
    ),
    interpolate = TRUE
  ) +
  
  # Land
  geom_sf(
    data = countries,
    fill = "grey92",
    colour = "grey45",
    linewidth = 0.25
  ) +
  
  # Rivers
  geom_sf(
    data = rivers_region,
    colour = "grey45",
    linewidth = 0.30,
    alpha = 0.80
  ) +
  
  # Coastline
  geom_sf(
    data = coastline,
    colour = "grey25",
    linewidth = 0.35
  ) +
  
  scale_fill_viridis_c(
    option = "mako",
    direction = 1,
    name = "Depth (m)",
    limits = c(-5000, 0),
    breaks = c(-4000, -2000, -1000, -500, 0),
    oob = scales::squish
  ) +
  
  scale_colour_manual(
    values = estuary_colors,
    breaks = names(estuary_colors),
    name = "Estuary"
  ) +
  
  coord_sf(
    xlim = c(-6.0, 3.0),
    ylim = c(43.0, 51.2),
    expand = FALSE
  ) +
  
  annotation_scale(
    location = "bl",
    width_hint = 0.20,
    text_cex = 0.8
  ) +
  
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = unit(1.0, "cm"),
    width = unit(1.0, "cm")
  ) +
  
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  
  theme_classic(base_size = 13) +
  
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black"),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.5
    )
  )

# 8. Display map ------------------------------------------------
map_general

# 9. Export -----------------------------------------------------
ggsave(
  filename = "Figure_Study_Area_General_Bathymetry2.png",
  plot = map_general,
  width = 8.5,
  height = 7.0,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "Figure_Study_Area_General_Bathymetry2.tiff",
  plot = map_general,
  width = 8.5,
  height = 7.0,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)


# ============================================================
# Gironde estuary: detailed local map
# High-resolution coastline and rivers
# Juvenile European seabass sampling
# ============================================================

# Packages ------------------------------------------------------
library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(marmap)
library(viridis)

# 1. Read sampling coordinates ---------------------------------
file <- "Copie de Fish_sampling_station_info_TO COMPLETE md_Phill.xlsx"

stations <- read_excel(
  file,
  sheet = "Planilha1"
)

# Records entered as 2018 were confirmed to be typographical
# errors and should be treated as 2019.
stations <- stations %>%
  mutate(
    Sampling_year = if_else(
      Sampling_year == 2018,
      2019L,
      as.integer(Sampling_year)
    )
  )

gironde <- stations %>%
  filter(Estuary_or_Zone == "Gironde")

gironde_sf <- st_as_sf(
  gironde,
  coords = c("X_Longitude_dd", "Y_Latitude_dd"),
  crs = 4326,
  remove = FALSE
)

# 2. Colour -----------------------------------------------------
gironde_color <- "#F8766D"

# 3. Map extent -------------------------------------------------
lon_min <- -1.55
lon_max <- -0.55
lat_min <- 45.20
lat_max <- 45.85

# 4. Download/read Natural Earth 10m local layers --------------
# This avoids rnaturalearthhires, which is not available for
# the user's current R version.

ne_dir <- file.path(getwd(), "naturalearth_10m")

if (!dir.exists(ne_dir)) {
  dir.create(ne_dir, recursive = TRUE)
}

download_ne10 <- function(file_name, subdir = "physical") {
  
  zip_url <- paste0(
    "https://naciscdn.org/naturalearth/10m/",
    subdir,
    "/",
    file_name,
    ".zip"
  )
  
  zip_file <- file.path(ne_dir, paste0(file_name, ".zip"))
  
  if (!file.exists(zip_file)) {
    download.file(
      zip_url,
      zip_file,
      mode = "wb"
    )
  }
  
  unzip(
    zip_file,
    exdir = ne_dir
  )
}

# High-resolution land polygon
download_ne10("ne_10m_land")

# High-resolution coastline
download_ne10("ne_10m_coastline")

# High-resolution river/lake centerlines
download_ne10("ne_10m_rivers_lake_centerlines")

land_10 <- st_read(
  file.path(ne_dir, "ne_10m_land.shp"),
  quiet = TRUE
)

coastline_10 <- st_read(
  file.path(ne_dir, "ne_10m_coastline.shp"),
  quiet = TRUE
)

rivers_10 <- st_read(
  file.path(ne_dir, "ne_10m_rivers_lake_centerlines.shp"),
  quiet = TRUE
)

# Crop the geographic layers to the study area
land_gironde <- st_crop(
  land_10,
  xmin = lon_min,
  xmax = lon_max,
  ymin = lat_min,
  ymax = lat_max
)

coastline_gironde <- st_crop(
  coastline_10,
  xmin = lon_min,
  xmax = lon_max,
  ymin = lat_min,
  ymax = lat_max
)

rivers_gironde <- st_crop(
  rivers_10,
  xmin = lon_min,
  xmax = lon_max,
  ymin = lat_min,
  ymax = lat_max
)

# 5. Bathymetry -------------------------------------------------
bathymetry <- getNOAA.bathy(
  lon1 = lon_min,
  lon2 = lon_max,
  lat1 = lat_min,
  lat2 = lat_max,
  resolution = 1,
  keep = TRUE
)

bath_df <- fortify.bathy(bathymetry) %>%
  filter(!is.na(z), z <= 0)

# 6. Identify major rivers --------------------------------------
# Natural Earth uses the "name" field for river names.
# We use it only for the major rivers in the Gironde system.

major_rivers <- rivers_gironde %>%
  filter(
    grepl(
      "Garonne|Dordogne|Gironde",
      name,
      ignore.case = TRUE
    )
  )

# 7. Labels -----------------------------------------------------
river_labels <- data.frame(
  river = c("Garonne", "Dordogne"),
  longitude = c(-0.56, -0.76),
  latitude  = c(45.35, 45.09)
)

# 8. Map --------------------------------------------------------
map_gironde <- ggplot() +
  
  # Bathymetry
  geom_raster(
    data = bath_df,
    aes(
      x = x,
      y = y,
      fill = z
    ),
    interpolate = TRUE
  ) +
  
  # High-resolution land
  geom_sf(
    data = land_gironde,
    fill = "grey92",
    colour = NA
  ) +
  
  # River network
  geom_sf(
    data = rivers_gironde,
    colour = "#7F9EAA",
    linewidth = 0.28,
    alpha = 0.85
  ) +
  
  # Major rivers emphasized
  geom_sf(
    data = major_rivers,
    colour = "#5E7F8A",
    linewidth = 0.65,
    alpha = 0.95
  ) +
  
  # High-resolution coastline
  geom_sf(
    data = coastline_gironde,
    colour = "grey25",
    linewidth = 0.45
  ) +
  
  # Sampling stations
  geom_sf(
    data = gironde_sf,
    colour = gironde_color,
    fill = "white",
    shape = 21,
    size = 3.3,
    stroke = 1.0
  ) +
  
  # Station identifiers
  geom_text(
    data = gironde,
    aes(
      x = X_Longitude_dd,
      y = Y_Latitude_dd,
      label = Trawl_number_or_station
    ),
    nudge_y = 0.012,
    size = 3.0,
    colour = "grey20"
  ) +
  
  # River labels
  geom_text(
    data = river_labels,
    aes(
      x = longitude,
      y = latitude,
      label = river
    ),
    colour = "#4F6870",
    size = 4.0,
    fontface = "italic"
  ) +
  
  # Estuary label
  annotate(
    "text",
    x = -1.20,
    y = 45.72,
    label = "Gironde Estuary",
    colour = gironde_color,
    size = 5.0,
    fontface = "bold"
  ) +
  
  # Ocean label
  annotate(
    "text",
    x = -1.22,
    y = 45.32,
    label = "Atlantic Ocean",
    colour = "grey35",
    size = 4.0,
    fontface = "italic"
  ) +
  
  scale_fill_viridis_c(
    option = "mako",
    direction = 1,
    name = "Depth (m)",
    limits = c(-500, 0),
    breaks = c(-500, -200, -100, -50, 0),
    oob = scales::squish
  ) +
  
  coord_sf(
    xlim = c(lon_min, lon_max),
    ylim = c(lat_min, lat_max),
    expand = FALSE
  ) +
  
  annotation_scale(
    location = "bl",
    width_hint = 0.22,
    text_cex = 0.8
  ) +
  
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = unit(1.0, "cm"),
    width = unit(1.0, "cm")
  ) +
  
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  
  theme_classic(base_size = 13) +
  
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black"),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.5
    )
  )

# 9. Display ----------------------------------------------------
map_gironde

# 10. Export ----------------------------------------------------
ggsave(
  filename = "Map_Gironde_Estuary_10m.png",
  plot = map_gironde,
  width = 8.0,
  height = 6.5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "Map_Gironde_Estuary_10m.tiff",
  plot = map_gironde,
  width = 8.0,
  height = 6.5,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)



# ============================================================
# Gironde estuary: clean detailed local map
# High-resolution coastline and rivers
# Juvenile European seabass sampling
#
# Clean version:
# - no north arrow
# - no station numbers
# - no estuary name
# - no Atlantic Ocean label
# - no river labels
# - sampling stations fully filled
# - standard latitude axis on the LEFT
# - longitude axis on the BOTTOM
# - scale bar retained
# - bathymetry legend retained
# ============================================================

# Packages ------------------------------------------------------
library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(marmap)
library(viridis)

# 1. Read sampling coordinates ---------------------------------
file <- "Copie de Fish_sampling_station_info_TO COMPLETE md_Phill.xlsx"

stations <- read_excel(
  file,
  sheet = "Planilha1"
)

# Records entered as 2018 were confirmed to be typographical
# errors and should be treated as 2019.
stations <- stations %>%
  mutate(
    Sampling_year = if_else(
      Sampling_year == 2018,
      2019L,
      as.integer(Sampling_year)
    )
  )

gironde <- stations %>%
  filter(
    Estuary_or_Zone == "Gironde"
  )

gironde_sf <- st_as_sf(
  gironde,
  coords = c(
    "X_Longitude_dd",
    "Y_Latitude_dd"
  ),
  crs = 4326,
  remove = FALSE
)

# 2. Colour -----------------------------------------------------
gironde_color <- "#F8766D"

# 3. Map extent -------------------------------------------------
lon_min <- -1.55
lon_max <- -0.55
lat_min <- 45.20
lat_max <- 45.85

# 4. Download/read Natural Earth 10m local layers --------------

ne_dir <- file.path(
  getwd(),
  "naturalearth_10m"
)

if (!dir.exists(ne_dir)) {
  dir.create(
    ne_dir,
    recursive = TRUE
  )
}

download_ne10 <- function(
    file_name,
    subdir = "physical"
) {
  
  zip_url <- paste0(
    "https://naciscdn.org/naturalearth/10m/",
    subdir,
    "/",
    file_name,
    ".zip"
  )
  
  zip_file <- file.path(
    ne_dir,
    paste0(
      file_name,
      ".zip"
    )
  )
  
  if (!file.exists(zip_file)) {
    download.file(
      zip_url,
      zip_file,
      mode = "wb"
    )
  }
  
  unzip(
    zip_file,
    exdir = ne_dir
  )
}

# High-resolution land polygon
download_ne10(
  "ne_10m_land"
)

# High-resolution coastline
download_ne10(
  "ne_10m_coastline"
)

# High-resolution river/lake centerlines
download_ne10(
  "ne_10m_rivers_lake_centerlines"
)

land_10 <- st_read(
  file.path(
    ne_dir,
    "ne_10m_land.shp"
  ),
  quiet = TRUE
)

coastline_10 <- st_read(
  file.path(
    ne_dir,
    "ne_10m_coastline.shp"
  ),
  quiet = TRUE
)

rivers_10 <- st_read(
  file.path(
    ne_dir,
    "ne_10m_rivers_lake_centerlines.shp"
  ),
  quiet = TRUE
)

# Crop the geographic layers to the study area
land_gironde <- st_crop(
  land_10,
  xmin = lon_min,
  xmax = lon_max,
  ymin = lat_min,
  ymax = lat_max
)

coastline_gironde <- st_crop(
  coastline_10,
  xmin = lon_min,
  xmax = lon_max,
  ymin = lat_min,
  ymax = lat_max
)

rivers_gironde <- st_crop(
  rivers_10,
  xmin = lon_min,
  xmax = lon_max,
  ymin = lat_min,
  ymax = lat_max
)

# 5. Bathymetry -------------------------------------------------
bathymetry <- getNOAA.bathy(
  lon1 = lon_min,
  lon2 = lon_max,
  lat1 = lat_min,
  lat2 = lat_max,
  resolution = 1,
  keep = TRUE
)

bath_df <- fortify.bathy(
  bathymetry
) %>%
  filter(
    !is.na(z),
    z <= 0
  )

# 6. Identify major rivers --------------------------------------
major_rivers <- rivers_gironde %>%
  filter(
    grepl(
      "Garonne|Dordogne|Gironde",
      name,
      ignore.case = TRUE
    )
  )

# 7. Clean map --------------------------------------------------

map_gironde_clean <- ggplot() +
  
  # Bathymetry
  geom_raster(
    data = bath_df,
    aes(
      x = x,
      y = y,
      fill = z
    ),
    interpolate = TRUE
  ) +
  
  # High-resolution land
  geom_sf(
    data = land_gironde,
    fill = "grey92",
    colour = NA
  ) +
  
  # River network
  geom_sf(
    data = rivers_gironde,
    colour = "#7F9EAA",
    linewidth = 0.28,
    alpha = 0.85
  ) +
  
  # Major rivers emphasized
  geom_sf(
    data = major_rivers,
    colour = "#5E7F8A",
    linewidth = 0.65,
    alpha = 0.95
  ) +
  
  # High-resolution coastline
  geom_sf(
    data = coastline_gironde,
    colour = "grey25",
    linewidth = 0.45
  ) +
  
  # Sampling stations — fully filled
  geom_sf(
    data = gironde_sf,
    shape = 21,
    size = 3.3,
    stroke = 1.0,
    fill = gironde_color,
    colour = gironde_color
  ) +
  
  # Bathymetry legend
  scale_fill_viridis_c(
    option = "mako",
    direction = 1,
    name = "Depth (m)",
    limits = c(
      -500,
      0
    ),
    breaks = c(
      -500,
      -200,
      -100,
      -50,
      0
    ),
    oob = scales::squish
  ) +
  
  # Map extent
  coord_sf(
    xlim = c(
      lon_min,
      lon_max
    ),
    ylim = c(
      lat_min,
      lat_max
    ),
    expand = FALSE
  ) +
  
  # Scale bar only
  annotation_scale(
    location = "bl",
    width_hint = 0.22,
    text_cex = 0.8
  ) +
  
  # Axis labels
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  
  # Standard axes
  theme_classic(
    base_size = 13
  ) +
  
  theme(
    
    # Bathymetry legend
    legend.position = "bottom",
    
    legend.title = element_text(
      face = "bold"
    ),
    
    legend.text = element_text(
      size = 10
    ),
    
    # Axis titles
    axis.title = element_text(
      face = "bold"
    ),
    
    # Axis text
    axis.text = element_text(
      colour = "black"
    ),
    
    # Map border
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.5
    )
  )

# 8. Display ----------------------------------------------------

map_gironde_clean

# 9. Export -----------------------------------------------------

ggsave(
  filename = "Map_Gironde_Estuary_10m_clean.png",
  plot = map_gironde_clean,
  width = 6.0,
  height = 4.5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "Map_Gironde_Estuary_10m_clean.tiff",
  plot = map_gironde_clean,
  width = 6.0,
  height = 4.5,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)


# ============================================================
# Loire estuary: clean detailed local map
# High-resolution coastline and rivers
# Juvenile European seabass sampling
#
# Same visual standard as the final Gironde map:
# - no north arrow
# - no station numbers
# - no estuary name
# - no Atlantic Ocean label
# - no river labels
# - sampling stations fully filled
# - standard latitude axis on the LEFT
# - longitude axis on the BOTTOM
# - scale bar retained
# - bathymetry legend retained
# ============================================================

# Packages ------------------------------------------------------
library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(marmap)
library(viridis)

# 1. Read sampling coordinates ---------------------------------
file <- "Copie de Fish_sampling_station_info_TO COMPLETE md_Phill.xlsx"

stations <- read_excel(
  file,
  sheet = "Planilha1"
)

# Records entered as 2018 were confirmed to be typographical
# errors and should be treated as 2019.
stations <- stations %>%
  mutate(
    Sampling_year = if_else(
      Sampling_year == 2018,
      2019L,
      as.integer(Sampling_year)
    )
  )

loire <- stations %>%
  filter(
    Estuary_or_Zone == "Loire"
  )

loire_sf <- st_as_sf(
  loire,
  coords = c(
    "X_Longitude_dd",
    "Y_Latitude_dd"
  ),
  crs = 4326,
  remove = FALSE
)

# 2. Colour -----------------------------------------------------
loire_color <- "#00BA38"

# 3. Map extent -------------------------------------------------
# Based on the Loire sampling-station distribution.
lon_min <- -2.40
lon_max <- -1.90
lat_min <- 47.05
lat_max <- 47.40

# 4. Download/read Natural Earth 10m local layers --------------

ne_dir <- file.path(
  getwd(),
  "naturalearth_10m"
)

if (!dir.exists(ne_dir)) {
  dir.create(
    ne_dir,
    recursive = TRUE
  )
}

download_ne10 <- function(
    file_name,
    subdir = "physical"
) {
  
  zip_url <- paste0(
    "https://naciscdn.org/naturalearth/10m/",
    subdir,
    "/",
    file_name,
    ".zip"
  )
  
  zip_file <- file.path(
    ne_dir,
    paste0(
      file_name,
      ".zip"
    )
  )
  
  if (!file.exists(zip_file)) {
    download.file(
      zip_url,
      zip_file,
      mode = "wb"
    )
  }
  
  unzip(
    zip_file,
    exdir = ne_dir
  )
}

# High-resolution land polygon
download_ne10(
  "ne_10m_land"
)

# High-resolution coastline
download_ne10(
  "ne_10m_coastline"
)

# High-resolution river/lake centerlines
download_ne10(
  "ne_10m_rivers_lake_centerlines"
)

land_10 <- st_read(
  file.path(
    ne_dir,
    "ne_10m_land.shp"
  ),
  quiet = TRUE
)

coastline_10 <- st_read(
  file.path(
    ne_dir,
    "ne_10m_coastline.shp"
  ),
  quiet = TRUE
)

rivers_10 <- st_read(
  file.path(
    ne_dir,
    "ne_10m_rivers_lake_centerlines.shp"
  ),
  quiet = TRUE
)

# Crop the geographic layers to the study area
land_loire <- st_crop(
  land_10,
  xmin = lon_min,
  xmax = lon_max,
  ymin = lat_min,
  ymax = lat_max
)

coastline_loire <- st_crop(
  coastline_10,
  xmin = lon_min,
  xmax = lon_max,
  ymin = lat_min,
  ymax = lat_max
)

rivers_loire <- st_crop(
  rivers_10,
  xmin = lon_min,
  xmax = lon_max,
  ymin = lat_min,
  ymax = lat_max
)

# 5. Bathymetry -------------------------------------------------
bathymetry <- getNOAA.bathy(
  lon1 = lon_min,
  lon2 = lon_max,
  lat1 = lat_min,
  lat2 = lat_max,
  resolution = 1,
  keep = TRUE
)

bath_df <- fortify.bathy(
  bathymetry
) %>%
  filter(
    !is.na(z),
    z <= 0
  )

# 6. Identify major rivers --------------------------------------
major_rivers <- rivers_loire %>%
  filter(
    grepl(
      "Loire|Erdre",
      name,
      ignore.case = TRUE
    )
  )

# 7. Clean map --------------------------------------------------

map_loire_clean <- ggplot() +
  
  # Bathymetry
  geom_raster(
    data = bath_df,
    aes(
      x = x,
      y = y,
      fill = z
    ),
    interpolate = TRUE
  ) +
  
  # High-resolution land
  geom_sf(
    data = land_loire,
    fill = "grey92",
    colour = NA
  ) +
  
  # River network
  geom_sf(
    data = rivers_loire,
    colour = "#7F9EAA",
    linewidth = 0.28,
    alpha = 0.85
  ) +
  
  # Major rivers emphasized
  geom_sf(
    data = major_rivers,
    colour = "#5E7F8A",
    linewidth = 0.65,
    alpha = 0.95
  ) +
  
  # High-resolution coastline
  geom_sf(
    data = coastline_loire,
    colour = "grey25",
    linewidth = 0.45
  ) +
  
  # Sampling stations — fully filled
  geom_sf(
    data = loire_sf,
    shape = 21,
    size = 3.3,
    stroke = 1.0,
    fill = loire_color,
    colour = loire_color
  ) +
  
  # Bathymetry legend
  scale_fill_viridis_c(
    option = "mako",
    direction = 1,
    name = "Depth (m)",
    limits = c(
      -500,
      0
    ),
    breaks = c(
      -500,
      -200,
      -100,
      -50,
      0
    ),
    oob = scales::squish
  ) +
  
  # Map extent
  coord_sf(
    xlim = c(
      lon_min,
      lon_max
    ),
    ylim = c(
      lat_min,
      lat_max
    ),
    expand = FALSE
  ) +
  
  # Scale bar only
  annotation_scale(
    location = "bl",
    width_hint = 0.22,
    text_cex = 0.8
  ) +
  
  # Axis labels
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  
  # Standard axes
  theme_classic(
    base_size = 13
  ) +
  
  theme(
    
    # Bathymetry legend
    legend.position = "bottom",
    
    legend.title = element_text(
      face = "bold"
    ),
    
    legend.text = element_text(
      size = 10
    ),
    
    # Axis titles
    axis.title = element_text(
      face = "bold"
    ),
    
    # Axis text
    axis.text = element_text(
      colour = "black"
    ),
    
    # Map border
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.5
    )
  )

# 8. Display ----------------------------------------------------

map_loire_clean

# 9. Export -----------------------------------------------------

ggsave(
  filename = "Map_Loire_Estuary_10m_clean.png",
  plot = map_loire_clean,
  width = 6.0,
  height = 4.5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "Map_Loire_Estuary_10m_clean.tiff",
  plot = map_loire_clean,
  width = 6.0,
  height = 4.5,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)


# ============================================================
# Seine estuary: clean detailed local map
# High-resolution coastline and rivers
# Juvenile European seabass sampling
# ============================================================

# Packages ------------------------------------------------------
library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(marmap)
library(viridis)

# 1. Read sampling coordinates ---------------------------------
file <- "Copie de Fish_sampling_station_info_TO COMPLETE md_Phill.xlsx"

stations <- read_excel(
  file,
  sheet = "Planilha1"
)

# Records entered as 2018 were confirmed to be typographical
# errors and should be treated as 2019.
stations <- stations %>%
  mutate(
    Sampling_year = if_else(
      Sampling_year == 2018,
      2019L,
      as.integer(Sampling_year)
    )
  )

seine <- stations %>%
  filter(Estuary_or_Zone == "Seine")

seine_sf <- st_as_sf(
  seine,
  coords = c("X_Longitude_dd", "Y_Latitude_dd"),
  crs = 4326,
  remove = FALSE
)

# 2. Colour -----------------------------------------------------
seine_color <- "#619CFF"

# 3. Map extent -------------------------------------------------
lon_min <- -0.10
lon_max <- 0.35
lat_min <- 49.30
lat_max <- 49.55

# 4. Download/read Natural Earth 10m local layers --------------

ne_dir <- file.path(getwd(), "naturalearth_10m")

if (!dir.exists(ne_dir)) {
  dir.create(ne_dir, recursive = TRUE)
}

download_ne10 <- function(file_name, subdir = "physical") {
  
  zip_url <- paste0(
    "https://naciscdn.org/naturalearth/10m/",
    subdir, "/", file_name, ".zip"
  )
  
  zip_file <- file.path(
    ne_dir,
    paste0(file_name, ".zip")
  )
  
  if (!file.exists(zip_file)) {
    download.file(
      zip_url,
      zip_file,
      mode = "wb"
    )
  }
  
  unzip(
    zip_file,
    exdir = ne_dir
  )
}

download_ne10("ne_10m_land")
download_ne10("ne_10m_coastline")
download_ne10("ne_10m_rivers_lake_centerlines")

land_10 <- st_read(
  file.path(ne_dir, "ne_10m_land.shp"),
  quiet = TRUE
)

coastline_10 <- st_read(
  file.path(ne_dir, "ne_10m_coastline.shp"),
  quiet = TRUE
)

rivers_10 <- st_read(
  file.path(ne_dir, "ne_10m_rivers_lake_centerlines.shp"),
  quiet = TRUE
)

land_seine <- st_crop(
  land_10,
  xmin = lon_min,
  xmax = lon_max,
  ymin = lat_min,
  ymax = lat_max
)

coastline_seine <- st_crop(
  coastline_10,
  xmin = lon_min,
  xmax = lon_max,
  ymin = lat_min,
  ymax = lat_max
)

rivers_seine <- st_crop(
  rivers_10,
  xmin = lon_min,
  xmax = lon_max,
  ymin = lat_min,
  ymax = lat_max
)

# 5. Bathymetry -------------------------------------------------
bathymetry <- getNOAA.bathy(
  lon1 = lon_min,
  lon2 = lon_max,
  lat1 = lat_min,
  lat2 = lat_max,
  resolution = 1,
  keep = TRUE
)

bath_df <- fortify.bathy(bathymetry) %>%
  filter(!is.na(z), z <= 0)

# 6. Identify major rivers --------------------------------------
major_rivers <- rivers_seine %>%
  filter(
    grepl(
      "Seine|Risle",
      name,
      ignore.case = TRUE
    )
  )

# 7. Clean map --------------------------------------------------
map_seine_clean <- ggplot() +
  
  geom_raster(
    data = bath_df,
    aes(x = x, y = y, fill = z),
    interpolate = TRUE
  ) +
  
  geom_sf(
    data = land_seine,
    fill = "grey92",
    colour = NA
  ) +
  
  geom_sf(
    data = rivers_seine,
    colour = "#7F9EAA",
    linewidth = 0.28,
    alpha = 0.85
  ) +
  
  geom_sf(
    data = major_rivers,
    colour = "#5E7F8A",
    linewidth = 0.65,
    alpha = 0.95
  ) +
  
  geom_sf(
    data = coastline_seine,
    colour = "grey25",
    linewidth = 0.45
  ) +
  
  geom_sf(
    data = seine_sf,
    shape = 21,
    size = 3.3,
    stroke = 1.0,
    fill = seine_color,
    colour = seine_color
  ) +
  
  scale_fill_viridis_c(
    option = "mako",
    direction = 1,
    name = "Depth (m)",
    limits = c(-500, 0),
    breaks = c(-500, -200, -100, -50, 0),
    oob = scales::squish
  ) +
  
  coord_sf(
    xlim = c(lon_min, lon_max),
    ylim = c(lat_min, lat_max),
    expand = FALSE
  ) +
  
  annotation_scale(
    location = "bl",
    width_hint = 0.22,
    text_cex = 0.8
  ) +
  
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  
  theme_classic(base_size = 13) +
  
  theme(
    legend.position = "bottom",
    
    legend.title = element_text(
      face = "bold"
    ),
    
    legend.text = element_text(
      size = 10
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    axis.text = element_text(
      colour = "black"
    ),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.5
    )
  )

# 8. Display ----------------------------------------------------
map_seine_clean

# 9. Export -----------------------------------------------------
ggsave(
  filename = "Map_Seine_Estuary_10m_clean.png",
  plot = map_seine_clean,
  width = 6.0,
  height = 4.5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "Map_Seine_Estuary_10m_clean.tiff",
  plot = map_seine_clean,
  width = 6.0,
  height = 4.5,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)
