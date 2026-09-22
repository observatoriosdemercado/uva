#' Helpers analiticos para o Dashboard Mercado de Uva (Quarto).

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(tsutils)
  library(lubridate)
})

DADOS_DIR <- "C:/Users/Lenovo/Dropbox/tempecon/dados_uva"

#' Le uma serie semanal CEPEA + IGP-DI e retorna data.frame com preco
#' deflacionado e coluna date. Usa colunas por nome (robusto a estruturas
#' com ou sem coluna `regiao`).
ler_serie_preco <- function(arquivo_csv, dir = DADOS_DIR,
                            igpdi_csv = "igpdi_uva.csv",
                            data_inicio = as.Date("2017-01-01")) {
  dados <- read.csv2(file.path(dir, arquivo_csv),
                     header = TRUE, sep = ";", dec = ".") |>
    rename_with(tolower)

  igpdi <- read.csv2(file.path(dir, igpdi_csv),
                     header = TRUE, sep = ";", dec = ".") |>
    rename_with(tolower)

  # Garantir que preco seja numerico (alguns CSVs vem com NAs em branco)
  dados$preco <- as.numeric(as.character(dados$preco))

  dadosp <- dados |>
    select(ano, semana, preco) |>
    inner_join(igpdi |> select(ano, semana, igpdi),
               by = c("ano", "semana"))

  ult_igpdi <- tail(dadosp$igpdi, 1)
  dadosp$preco_def <- dadosp$preco * (ult_igpdi / dadosp$igpdi)

  # Datas semanais consecutivas a partir de data_inicio
  date <- seq(data_inicio, by = "1 week", length.out = nrow(dadosp))
  dadosp$date <- date

  dadosp
}

#' Analise semanal: serie, tendencia, sazonalidade, envelope min/med/max,
#' tabelas de anos recentes e variacao semanal %.
analise_preco <- function(dadosp, ano_atual, semana_atual) {
  ano_inicio <- min(dadosp$ano, na.rm = TRUE)

  # Imputar NAs com interpolacao linear para evitar quebras em ts()/decompose()
  preco_imp <- dadosp$preco_def
  if (any(is.na(preco_imp))) {
    preco_imp <- approx(seq_along(preco_imp), preco_imp,
                        xout = seq_along(preco_imp), rule = 2)$y
  }

  preco_ts <- ts(preco_imp, start = c(ano_inicio, 1), frequency = 52)
  trend <- as.numeric(cmav(preco_ts, outplot = FALSE))

  # Versao interpolada para plotagem continua da linha de preco
  dadosp$preco_def_filled <- preco_imp
  decompa <- decompose(preco_ts, type = "multiplicative")
  saz <- tibble(semana = 1:52, fator = as.numeric(decompa$figure))

  preco_completo <- window(preco_ts, end = c(ano_atual - 1, 52))
  seas_ref <- seasplot(preco_completo, trend = FALSE, outplot = FALSE)
  medias <- colMeans(seas_ref$season)[1:52]

  envelope <- tibble(
    semana = 1:52,
    minimo = apply(seas_ref$season, 2, min)[1:52],
    media  = round(medias, 2),
    maximo = apply(seas_ref$season, 2, max)[1:52]
  )

  preco_por_ano <- function(yr) {
    v <- dadosp %>% filter(ano == yr) %>% pull(preco_def)
    out <- rep(NA_real_, 52)
    out[seq_along(v)] <- v
    out
  }
  anos_recentes <- tibble(
    semana = 1:52,
    !!as.character(ano_atual - 2) := round(preco_por_ano(ano_atual - 2), 2),
    !!as.character(ano_atual - 1) := round(preco_por_ano(ano_atual - 1), 2),
    !!as.character(ano_atual)     := round(preco_por_ano(ano_atual),     2)
  )

  var_pct <- function(v) (v / dplyr::lag(v) - 1) * 100
  variacao <- tibble(
    semana = 1:52,
    !!as.character(ano_atual - 1) := round(var_pct(preco_por_ano(ano_atual - 1)), 2),
    !!as.character(ano_atual)     := round(var_pct(preco_por_ano(ano_atual)),     2)
  )

  list(
    serie    = dadosp,
    ts       = preco_ts,
    trend    = trend,
    sazonal  = saz,
    envelope = envelope,
    anos     = anos_recentes,
    variacao = variacao,
    preco_atual = tail(na.omit(dadosp$preco_def), 1),
    preco_mesma_sem_ano_ant1 = preco_por_ano(ano_atual - 1)[semana_atual],
    preco_mesma_sem_ano_ant2 = preco_por_ano(ano_atual - 2)[semana_atual]
  )
}

#' Analise mensal de exportacoes (12 meses por ano, ts freq=12).
#' Espera dataframe com colunas: ano (numeric), mes (numeric 1-12), valor, volume.
#' O mes de referencia eh autodetectado como o ultimo mes do ano_atual com dado
#' (ignorando NAs), nao Sys.Date().
analise_exportacao <- function(df, ano_atual,
                                col_volume = "volume", divisor_volume = 1000) {
  df <- df %>%
    arrange(ano, mes) %>%
    mutate(
      volume_ton = .data[[col_volume]] / divisor_volume,
      date = as.Date(sprintf("%04d-%02d-01", ano, mes))
    )

  vol_ts <- ts(df$volume_ton, start = c(min(df$ano), 1), frequency = 12)
  trend  <- as.numeric(cmav(vol_ts, outplot = FALSE))
  decompa <- decompose(vol_ts, type = "multiplicative")

  meses_pt <- c("janeiro","fevereiro","março","abril","maio","junho",
                "julho","agosto","setembro","outubro","novembro","dezembro")

  saz <- tibble(
    mes = factor(meses_pt, levels = meses_pt, ordered = TRUE),
    fator = as.numeric(decompa$figure)
  )

  vol_completo <- window(vol_ts, end = c(ano_atual - 1, 12))
  seas_ref <- seasplot(vol_completo, trend = FALSE, outplot = FALSE)
  medias <- colMeans(seas_ref$season)[1:12]

  envelope <- tibble(
    mes_num = 1:12,
    mes = factor(meses_pt, levels = meses_pt, ordered = TRUE),
    minimo = apply(seas_ref$season, 2, min)[1:12],
    media  = round(medias, 0),
    maximo = apply(seas_ref$season, 2, max)[1:12]
  )

  volume_por_ano <- function(yr) {
    v <- df %>% filter(ano == yr) %>% pull(volume_ton)
    out <- rep(NA_real_, 12)
    out[seq_along(v)] <- v
    out
  }
  anos_recentes <- tibble(
    mes_num = 1:12,
    mes = factor(meses_pt, levels = meses_pt, ordered = TRUE),
    !!as.character(ano_atual - 2) := round(volume_por_ano(ano_atual - 2), 0),
    !!as.character(ano_atual - 1) := round(volume_por_ano(ano_atual - 1), 0),
    !!as.character(ano_atual)     := round(volume_por_ano(ano_atual),     0)
  )

  # Detectar ultimo mes com dado no ano atual
  vol_ano_atual <- volume_por_ano(ano_atual)
  mes_ref <- if (all(is.na(vol_ano_atual))) {
    12
  } else {
    max(which(!is.na(vol_ano_atual)))
  }

  list(
    serie = df,
    ts = vol_ts,
    trend = trend,
    sazonal = saz,
    envelope = envelope,
    anos = anos_recentes,
    mes_referencia = mes_ref,
    vol_atual = vol_ano_atual[mes_ref],
    vol_mesmo_mes_ano_ant1 = volume_por_ano(ano_atual - 1)[mes_ref],
    vol_mesmo_mes_ano_ant2 = volume_por_ano(ano_atual - 2)[mes_ref]
  )
}

#' Formata valor em R$ com virgula decimal.
fmt_brl <- function(x, casas = 2) {
  if (is.na(x)) return("—")
  paste0("R$ ", format(round(x, casas),
                       decimal.mark = ",",
                       big.mark = ".",
                       nsmall = casas))
}

fmt_num <- function(x, casas = 0) {
  if (is.na(x)) return("—")
  formatC(round(x, casas), format = "d", big.mark = ".", decimal.mark = ",")
}

#' Formata área em hectares (valor bruto) como "XX,X Mil ha" — usado nos
#' valueboxes de Área Plantada (Brasil / Vale do São Francisco).
fmt_area_mil_ha <- function(x) {
  paste0(format(round(x / 1000, 1), decimal.mark = ",", nsmall = 1), " Mil ha")
}

#' Formata produtividade (t/ha) como "XX t/ha".
fmt_produtividade <- function(x) {
  paste0(formatC(round(x), format = "d"), " t/ha")
}

#' Formata volume produzido em toneladas (valor bruto) como "X,XX Mi t".
fmt_volume_mi_t <- function(x) {
  paste0(format(round(x / 1e6, 2), decimal.mark = ",", nsmall = 2), " Mi t")
}

#' Formata VBP em mil R$ (unidade do IBGE/PAM) como "R$ X,X Bi".
fmt_vbp_bi <- function(x) {
  paste0("R$ ", format(round(x / 1e6, 1), decimal.mark = ",", nsmall = 1), " Bi")
}

#' Remove sufixos ",1" / ",NA" e parenteses que o ggplotly insere quando
#' uma variavel esta mapeada em multiplas esteticas.
limpar_legenda_plotly <- function(p) {
  limpar_nome <- function(x) {
    if (is.null(x)) return(x)
    x <- sub("^\\(([^,]+).*\\)$", "\\1", x)
    repeat {
      novo <- sub(",(\\d+|NA)$", "", x)
      if (identical(novo, x)) break
      x <- novo
    }
    x
  }
  p$x$data <- lapply(p$x$data, function(d) {
    if (!is.null(d$name))        d$name        <- limpar_nome(d$name)
    if (!is.null(d$legendgroup)) d$legendgroup <- limpar_nome(d$legendgroup)
    d
  })
  vistos <- character(0)
  p$x$data <- lapply(p$x$data, function(d) {
    if (!is.null(d$name) && d$name %in% vistos) {
      d$showlegend <- FALSE
    } else if (!is.null(d$name)) {
      vistos <<- c(vistos, d$name)
    }
    d
  })
  p
}
