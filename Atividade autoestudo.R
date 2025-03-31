#	WDI - WORLD DEVELOPMENT INDICATORS
# Exports of goods and services (BoP, current US$)(BX.GSR.GNFS.CD)
library(WDI)
# cÓDIGO (BX.GSR.GNFS.CD)
# EXPORTAÇÕES DE BENS E SERVIÇOS (EM DOLAR)
options (scipen = 999)
dadosexport <- WDI(country = 'all',
                   indicator = 'BX.GSR.GNFS.CD')
dadosexport2023 <- WDI(country = 'all',
                       indicator ='BX.GSR.GNFS.CD',
                       start = 2023, end = 2023)
paises <- c('BR', 'US')

dadosexportbrus <- WDI(country = paises,
                       indicator ='BX.GSR.GNFS.CD')
dadosexportbr <- WDI(country = 'BR',
                     indicator = 'BX.GSR.GNFS.CD')
