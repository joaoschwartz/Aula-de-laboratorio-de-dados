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

# DADOS EM PAINEL

grafpainel <- ggplot(dadosexport,
                     mapping = aes(y = BX.GSR.GNFS.CD,
                                   x = year)) +
  geom_point()

print(grafpainel)

library(WDI)
library(ggplot2)
library(dplyr)

# Coleta dos dados
dadosexport <- WDI(country = 'all',
                   indicator = 'BX.GSR.GNFS.CD')

# Gráfico com as modificações
grafpainel <- ggplot(dadosexport, aes(x = year, y = BX.GSR.GNFS.CD, group = country)) +
  geom_line(color = "grey70", alpha = 0.4) +  # Linhas mais suaves para todos os países
  geom_line(data = filter(dadosexport, country == "Brazil"), 
            aes(x = year, y = BX.GSR.GNFS.CD), 
            color = "forestgreen", size = 1.2) +  # Destaque para o Brasil
  labs(
    title = "Exportações de Bens e Serviços (1960 - atual)",
    x = "Ano",
    y = "Exportações (Em Dólar)"
  ) +
  theme_minimal(base_family = "Helvetica") +  # Fonte moderna
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    legend.position = "none"  # Remove legenda
  )

# Exibir o gráfico
print(grafpainel)


#CORTE TRANSVERSAL

grafcorte <- ggplot(dadosexport2023,
                    mapping = aes(y = BX.GSR.GNFS.CD,
                                  x = year)) +
  geom_point()

print(grafcorte)

library(WDI)
library(ggplot2)
library(dplyr)

# Baixando os dados
dadosexport2023 <- WDI(country = 'all',
                       indicator ='BX.GSR.GNFS.CD',
                       start = 2023, end = 2023)

# Tratando os dados para remover NAs e selecionar o Brasil
dados_filtrados <- dadosexport2023 %>%
  filter(!is.na(BX.GSR.GNFS.CD)) %>%
  mutate(is_brazil = ifelse(iso2c == "BR", "Brasil", "Outros"))

# Gráfico reformulado
grafico_exportacoes <- ggplot(dados_filtrados,
                              aes(x = reorder(country, -BX.GSR.GNFS.CD),
                                  y = BX.GSR.GNFS.CD,
                                  fill = is_brazil)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("Brasil" = "blue", "Outros" = "gray70")) +
  labs(title = "Exportações de Bens e Serviços em 2023",
       x = "País",
       y = "Exportações (Em Dólar)") +
  theme_minimal(base_family = "Helvetica") +
  theme(axis.text.x = element_blank(),  
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(size = 12))

# Exibir o gráfico
print(grafico_exportacoes)

# SÉRIE TEMPORAL

grafserie <- ggplot(dadosexportbr,
                    mapping = aes(y = BX.GSR.GNFS.CD,
                                  x = year)) +
  geom_line()
print(grafserie)

library(WDI)
library(ggplot2)
library(dplyr)

# Coleta dos dados
dadosexportbr <- WDI(country = 'BR',
                     indicator = 'BX.GSR.GNFS.CD')

# Criação do gráfico
grafserie <- ggplot(dadosexportbr, 
                    aes(x = year, y = BX.GSR.GNFS.CD)) +
  geom_col(fill = "#0073C2FF") +  # barras em azul moderno
  geom_col(data = filter(dadosexportbr, year == 2000), 
           aes(x = year, y = BX.GSR.GNFS.CD), 
           fill = "red") +  # destaque em vermelho para o ano 2000
  labs(title = "Exportações do Brasil ao Longo dos Anos",
       x = "Ano",
       y = "Exportações (Em Dólar)") +
  theme_minimal(base_family = "Helvetica") +  
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(color = "#2c3e50"),
    legend.position = "none"  
  )

# Exibe o gráfico
print(grafserie)
