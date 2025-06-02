library(ggplot2)
library(dplyr)
library(lubridate)
library(gganimate)
library(scales)

# Dados anuais normalizados
my.id <- c(SUP = 1570)

df.bcb <- gbcbd_get_series(id = my.id ,
                           first.date = '1986-03-06',
                           last.date = Sys.Date(),
                           format.data = 'long',
                           use.memoise = TRUE, 
                           cache.path = tempdir(), # use tempdir for cache folder
                           do.parallel = FALSE)
df.anual <- df.bcb %>%
  filter(year(ref.date) >= 2003) %>%
  mutate(ano = year(ref.date)) %>%
  group_by(ano) %>%
  summarise(media_anual = mean(value, na.rm = TRUE)) %>%
  arrange(ano) %>%
  mutate(
    media_anual_norm = media_anual / media_anual[ano == 2003] * 100,
    variacao_pct = (media_anual_norm / lag(media_anual_norm) - 1) * 100,
    cor = case_when(
      is.na(variacao_pct) ~ "sem_var",
      variacao_pct < 0 ~ "queda",
      TRUE ~ "alta"
    )
  )

# Adiciona coluna de frame para revelar pontos cumulativamente
df.anim <- df.anual %>%
  mutate(frame = ano) %>%
  group_by(frame) %>%
  mutate(temp_id = row_number()) %>%
  ungroup() %>%
  arrange(frame) %>%
  group_by(frame) %>%
  mutate(show_ate_ano = frame) %>%
  ungroup()

# Dados cumulativos por ano para manter pontos fixos
df.cumulativo <- purrr::map_dfr(unique(df.anim$frame), function(ano_atual) {
  df.anim %>% filter(ano <= ano_atual) %>% mutate(frame = ano_atual)
})

# Cores
cores <- c("alta" = "#1f77b4", "queda" = "#d62728", "sem_var" = "#1f77b4")

# Gráfico
p <- ggplot(df.cumulativo, aes(x = ano, y = media_anual_norm)) +
  geom_line(color = "gray40", size = 1) +
  geom_point(aes(color = cor), size = 5) +
  geom_text(
    aes(
      label = ifelse(!is.na(variacao_pct), paste0(sprintf("%.1f", variacao_pct), "%"), ""),
      color = cor
    ),
    vjust = -1.5,
    size = 4,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = cores,
    breaks = c("alta", "queda"),
    labels = c("Crescimento", "Queda"),
    name = "Variação"
  ) +
  labs(
    title = "Índice de Vendas no Varejo - Média Anual (base 2003 = 100)",
    subtitle = "Ano: {closest_state}",
    x = "Ano",
    y = "Índice Médio Anual"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 14),
    legend.position = "bottom"
  ) +
  coord_cartesian(ylim = c(min(df.cumulativo$media_anual_norm)*0.95,
                           max(df.cumulativo$media_anual_norm)*1.05)) +
  transition_states(frame, transition_length = 2, state_length = 1, wrap = FALSE) +
  ease_aes('linear')

# Animação em GIF
animate(p, nframes = 100, fps = 10, width = 900, height = 500, renderer = gifski_renderer())

# Salvar como GIF
anim_save("grafico_varejo_pontos_fixos.gif")
