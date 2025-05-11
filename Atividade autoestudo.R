#	WDI - WORLD DEVELOPMENT INDICATORS
# Exports of goods and services (BoP, current US$)(BX.GSR.GNFS.CD)
library(WDI)
library(tidyverse)
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
library(scales)
library(countrycode)

# Baixar e preparar os dados
dadosexport <- WDI(country = 'all', indicator = 'BX.GSR.GNFS.CD') %>%
  filter(!is.na(BX.GSR.GNFS.CD), year >= 1960) %>%
  filter(!is.na(countrycode(country, "country.name", "iso2c")))

# Calcular exportações totais por país e selecionar os 15 maiores
top15_paises <- dadosexport %>%
  group_by(country) %>%
  summarise(total_export = sum(BX.GSR.GNFS.CD, na.rm = TRUE)) %>%
  arrange(desc(total_export)) %>%
  slice_head(n = 15) %>%
  pull(country)

# Filtrar dados apenas para os top 15 e calcular valores em bilhões
dados_top15 <- dadosexport %>%
  filter(country %in% top15_paises) %>%
  mutate(export_bilhoes = BX.GSR.GNFS.CD / 1e9)

# Obter os 3 maiores picos (valor mais alto por país, depois top 3 no geral)
picos_gerais <- dados_top15 %>%
  group_by(country) %>%
  filter(export_bilhoes == max(export_bilhoes, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(export_bilhoes)) %>%
  slice_head(n = 3)

# Gráfico com destaque dos 3 picos
# Gráfico com legenda ajustada
grafico <- ggplot() +
  geom_line(data = dados_top15,
            aes(x = year, y = export_bilhoes, color = country),
            size = 1) +
  
  geom_point(data = picos_gerais,
             aes(x = year, y = export_bilhoes, color = country),
             size = 3) +
  
  geom_text(data = picos_gerais,
            aes(x = year, y = export_bilhoes, 
                label = paste0("US$ ", round(export_bilhoes, 1), " bi")),
            vjust = 1.5, hjust = 1.2, size = 3.2, color = "black") +
  
  scale_y_continuous(
    labels = label_number(prefix = "US$ ", suffix = " bi", big.mark = ".", decimal.mark = ","),
    breaks = pretty_breaks(n = 10)
  ) +
  
  labs(title = "Top 15 Países em Exportações de Bens e Serviços (1960 - Atual)",
       x = "Ano",
       y = "Exportações (em bilhões de US$)",
       color = "País") +
  
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE))  # <-- aqui está o ajuste




# Exibir gráfico
print(grafico)


#CORTE TRANSVERSAL

grafcorte <- ggplot(dadosexport2023,
                    mapping = aes(y = BX.GSR.GNFS.CD,
                                  x = year)) +
  geom_point()

print(grafcorte)

library(WDI)
library(dplyr)
library(ggplot2)
library(scales)
library(countrycode)

# Buscar os dados de exportação de 2023
dadosexport2023 <- WDI(country = 'all',
                       indicator = 'BX.GSR.GNFS.CD',
                       start = 2023, end = 2023)

# Filtrar apenas países reais com base no código ISO2
dados_filtrados <- dadosexport2023 %>%
  filter(!is.na(BX.GSR.GNFS.CD)) %>%
  filter(iso2c %in% countrycode::codelist$iso2c) %>%
  arrange(desc(BX.GSR.GNFS.CD))

# Selecionar os 15 países com maior exportação
top15 <- dados_filtrados %>%
  slice_max(BX.GSR.GNFS.CD, n = 15) %>%
  mutate(destacar = case_when(
    country == "Brazil" ~ "Brasil",
    country == country[which.max(BX.GSR.GNFS.CD)] ~ "Maior Exportador",
    country == country[which.min(BX.GSR.GNFS.CD)] ~ "Menor Exportador",
    TRUE ~ "Outros"
  ))

# Gráfico com os 15 maiores exportadores
ggplot(top15, aes(x = reorder(country, BX.GSR.GNFS.CD),
                  y = BX.GSR.GNFS.CD,
                  fill = destacar)) +
  geom_col(width = 0.7) +
  scale_fill_manual(
    values = c(
      "Brasil" = "#A2C4EC",              # Azul pastel
      "Maior Exportador" = "#228B22",    # Verde escuro (mantido)
      "Menor Exportador" = "firebrick",    # Vermelho suave
      "Outros" = "#A2C4EC"               # Cinza claro pastel
    ),
    name = "Categoria"
  ) +
  labs(title = "Top 15 Exportadores Mundiais em 2023",
       x = NULL,
       y = "Exportações (Em Dólar)") +
  coord_flip(clip = "off") +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 8),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.position = "bottom",
    plot.margin = margin(10, 60, 10, 10)
  ) +
  scale_y_continuous(labels = dollar_format(scale = 1e-9, suffix = " Bi", prefix = "US$ "),
                     expand = expansion(mult = c(0, 0.1))) +
  geom_text(aes(label = dollar_format(scale = 1e-9, suffix = " Bi", prefix = "US$ ")(BX.GSR.GNFS.CD),
                y = BX.GSR.GNFS.CD * 1.01),
            size = 2.8, color = "black", hjust = 0)

# SÉRIE TEMPORAL

grafserie <- ggplot(dadosexportbr,
                    mapping = aes(y = BX.GSR.GNFS.CD,
                                  x = year)) +
  geom_line()
print(grafserie)

library(WDI)
library(ggplot2)
library(dplyr)
library(scales)

# Dados do Brasil
dadosexportbr <- WDI(country = 'BR', indicator = 'BX.GSR.GNFS.CD') %>%
  filter(year >= 1975) %>%
  mutate(destacar = case_when(
    BX.GSR.GNFS.CD == max(BX.GSR.GNFS.CD) ~ "Máximo",
    BX.GSR.GNFS.CD == min(BX.GSR.GNFS.CD) ~ "Mínimo",
    TRUE ~ "Outros"
  ))

# Eventos históricos
anos_eventos <- c(1994, 2008, 2015, 2020)
nomes_eventos <- c("Plano Real", "Crise Global", "Recessão do PIB", "Pandemia")
rotulos_eventos <- paste0(anos_eventos, " - ", nomes_eventos)

# Pontos máximo e mínimo
ponto_max <- dadosexportbr %>% filter(destacar == "Máximo")
ponto_min <- dadosexportbr %>% filter(destacar == "Mínimo")

# Gráfico
ggplot(dadosexportbr, aes(x = year, y = BX.GSR.GNFS.CD)) +
  geom_area(fill = "#cce5ff", alpha = 0.6) +
  geom_line(color = "#099066", size = 1.1) +
  
  # Pontos máximo e mínimo
  geom_point(data = ponto_max, aes(color = "Máximo"), size = 3.5) +
  geom_point(data = ponto_min, aes(color = "Mínimo"), size = 3.5) +
  scale_color_manual(values = c("Máximo" = "green", "Mínimo" = "red")) +
  
  # Rótulo do ponto mínimo
  geom_label(data = ponto_min,
             aes(label = paste0(year, "\n", dollar(BX.GSR.GNFS.CD, scale = 1e-9, suffix = " Bi"))),
             fill = "white", size = 3, fontface = "plain", color = "black",
             vjust = -0.6, label.size = 0.3) +
  
  # Rótulo do ponto máximo (caixa abaixo)
  geom_label(data = ponto_max,
             aes(label = paste0(year, "\n", dollar(BX.GSR.GNFS.CD, scale = 1e-9, suffix = " Bi"))),
             fill = "white", size = 3, fontface = "plain", color = "black",
             vjust = 1.8, label.size = 0.3) +
  
  # Linhas verticais dos eventos
  geom_vline(xintercept = anos_eventos, linetype = "dotted", color = "gray50") +
  
  # Rótulos dos eventos com tamanho maior e negrito
  annotate("text", x = anos_eventos,
           y = max(dadosexportbr$BX.GSR.GNFS.CD) * 0.50,
           label = rotulos_eventos, angle = 90, vjust = -0.3,
           size = 4.8, color = "gray10", fontface = "bold") +
  
  # Eixo Y formatado
  scale_y_continuous(labels = dollar_format(scale = 1e-9, suffix = " Bi")) +
  
  # Título e tema
  labs(title = "Exportações Brasileiras (1975 - Atual)",
       x = "Ano",
       y = "Exportações (em bilhões de US$)",
       color = NULL) +
  
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9),
    legend.position = "none"
  )