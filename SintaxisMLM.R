library(haven)

options(scipen = 999) # Elimina notación científica
datosMLM <- read_sav("datosMLM.sav")

library(lme4)
library(lmerTest)

# Modelos de regresión multinivel lineales

# Modelo nulo (solo intercepto, o no condicional)

M.nulo <- lmer(Actividad_Fisica ~ 1 + (1 | IDvecindario), 
               data = datosMLM, 
               REML = TRUE)

summary(M.nulo)
performance:: icc(M.nulo)

# Centrar intención conductual en el promedio grupal

library(misty)

datosMLM$Intencion_ConductualCPG <- center(datosMLM$Intencion_Conductual, 
                                           type = "CWC", 
                                           cluster = datosMLM$IDvecindario)

# Modelo de intercepto aleatorio

M.interaleatorio <- lmer(Actividad_Fisica ~ Intencion_ConductualCPG + Disp_Area +
                           (1 | IDvecindario), 
                         data = datosMLM, 
                         REML = TRUE)

summary(M.interaleatorio, ddf = "Kenward-Roger") # valores p método de Kenward-Roger

# Modelo de coeficientes aleatorios

M.coefaleatorios <- lmer(Actividad_Fisica ~ Intencion_ConductualCPG + Disp_Area + 
                           (1 + Intencion_ConductualCPG | IDvecindario), 
                         data = datosMLM, 
                         REML = TRUE)

ranova(M.coefaleatorios)

summary(M.coefaleatorios, ddf = "Kenward-Roger") # valores p método de Kenward-Roger

# Modelo de interceptos y pendientes como desenlace

M.internivelcruzado <- lmer(Actividad_Fisica ~ Intencion_ConductualCPG + Disp_Area + 
                              Intencion_ConductualCPG:Disp_Area + 
                              (1 + Intencion_ConductualCPG | IDvecindario), 
                            data = datosMLM, 
                            REML = TRUE)

summary(M.internivelcruzado, ddf = "Kenward-Roger") # valores p método de Kenward-Roger


# Cálculo de varianza explicada (R2)

library(r2mlm)

datosMLM$Actividad_Fisica <- as.numeric(datosMLM$Actividad_Fisica)
datosMLM$Intencion_ConductualCPG <- as.numeric(datosMLM$Intencion_ConductualCPG)

r2 <- r2mlm(M.internivelcruzado)
r2$Decompositions


summary(M.internivelcruzado, ddf = "Kenward-Roger")


### Varianza total explicada por el efecto fijo y aleatorio de intención
0.2038452 + 0.0505839

### Varianza intra-grupos explicada por el efecto fijo y aleatorio de intención

0.26784401 + 0.06646512

### Varianza total explicada por el efecto fijo de areas

0.1220435

### Varianza entre-grupos explicada por el efecto fijo de areas

0.5107688


# Gráficos

library(ggplot2)

ggplot(datosMLM, aes(Intencion_Conductual, Actividad_Fisica)) + 
  geom_point(size = 1, alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~IDvecindario) +
  xlab("Intención conductual") + 
  ylab("Actividad física semanal") +
  theme_bw()
  

