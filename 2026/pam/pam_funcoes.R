# Funções compartilhadas - PAM/IBGE (Observatórios de Mercado de Uva e de Manga da Embrapa)
# IDÊNTICO nas pastas da uva e da manga (pam/ e boletim pam/). Não tem nada específico de
# cultura: configuração e texto da introdução ficam em pam_cultura.R (que precisa ser
# carregado junto:  source("pam_funcoes.R"); source("pam_cultura.R")).

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

# ---- Localização dos dados --------------------------------------------------
# Procura tempecon/<pasta_dados>/<pasta_ano> no Dropbox (Windows ou Mac).
# Para usar outro local: Sys.setenv(PAM_DADOS_BASE = "caminho/para/pasta_dos_dados")
pam_dir_dados <- function(pasta_ano, criar = FALSE) {
  bases <- c(
    Sys.getenv("PAM_DADOS_BASE"),
    file.path(Sys.getenv("USERPROFILE"), "Dropbox", "tempecon", pam_cfg$pasta_dados),
    file.path(Sys.getenv("HOME"), "Dropbox", "tempecon", pam_cfg$pasta_dados)
  )
  bases <- bases[nzchar(bases)]
  dirs <- file.path(bases, pasta_ano)
  achou <- dirs[dir.exists(dirs)]
  if (length(achou)) return(achou[1])
  base <- bases[dir.exists(bases)][1]
  if (criar && !is.na(base)) {
    dir.create(file.path(base, pasta_ano))
    return(file.path(base, pasta_ano))
  }
  stop("Pasta de dados não encontrada. Procurei em:\n", paste(dirs, collapse = "\n"))
}

# ---- Leitura das tabelas do SIDRA/IBGE ---------------------------------------
# Lê uma planilha exportada do SIDRA (tabela 5457) e devolve formato longo:
# local | ano | valor. Detecta sozinha a linha de anos, a coluna do nome e o
# rodapé, então não depende de skip nem do número de anos/linhas.
# `arquivo` pode ser o nome com ou sem extensão: se existir um .csv (gerado por
# pam_baixar.R, já no formato local/ano/valor) ele é usado; senão, o .xlsx do SIDRA.
pam_ler_bruto <- function(arquivo, dir = pam_dir) {
  csv <- file.path(dir, paste0(tools::file_path_sans_ext(arquivo), ".csv"))
  if (file.exists(csv)) {
    return(tibble::as_tibble(utils::read.csv(csv, encoding = "UTF-8", colClasses = c("character", "integer", "numeric"))))
  }
  arquivo <- paste0(tools::file_path_sans_ext(arquivo), ".xlsx")
  bruto <- readxl::read_excel(file.path(dir, arquivo), col_names = FALSE,
                              col_types = "text", .name_repair = "minimal")
  bruto <- as.matrix(bruto)
  eh_ano <- function(x) grepl("^(19|20)[0-9]{2}$", x)

  topo <- head(bruto, 10)
  linha_anos <- which(rowSums(matrix(eh_ano(topo), nrow = nrow(topo)), na.rm = TRUE) >= 3)[1]
  cols_anos  <- which(eh_ano(bruto[linha_anos, ]))
  col_nome   <- min(cols_anos) - 1L            # município é a última coluna antes dos anos

  dados <- bruto[-seq_len(linha_anos), , drop = FALSE]
  dados <- dados[!is.na(dados[, col_nome]) & !grepl("^Fonte", dados[, col_nome]), , drop = FALSE]

  tibble::tibble(
    local = rep(dados[, col_nome], times = length(cols_anos)),
    ano   = rep(as.integer(bruto[linha_anos, cols_anos]), each = nrow(dados)),
    valor = suppressWarnings(as.numeric(dados[, cols_anos]))   # "-" e ".." viram NA
  )
}

# Lê e aplica os rótulos abreviados de pam_cfg$rotulos, se houver (ex.: "Rio G. do Norte").
pam_ler <- function(arquivo, dir = pam_dir) {
  d <- pam_ler_bruto(arquivo, dir)
  if (length(pam_cfg$rotulos)) d <- mutate(d, local = dplyr::recode(local, !!!pam_cfg$rotulos))
  d
}

# ---- Transformações --------------------------------------------------------
# Com pam_cfg$excluir_rs (uva): Brasil e Sul sem o Rio Grande do Sul. Sem (manga): só ordena as 6 regiões.
pam_regioes <- function(d) {
  if (!pam_cfg$excluir_rs) {
    ordem <- c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
    return(d |> filter(local %in% ordem) |> mutate(local = factor(local, ordem)) |> arrange(local, ano))
  }
  d |>
    pivot_wider(names_from = local, values_from = valor) |>
    mutate(Brasil = Brasil - `Rio Grande do Sul`, Sul = Sul - `Rio Grande do Sul`) |>
    select(ano, Brasil, Norte, Nordeste, Sudeste, Sul, `Centro-Oeste`) |>
    pivot_longer(-ano, names_to = "local", values_to = "valor") |>
    mutate(local = factor(local, unique(local))) |>
    arrange(local, ano)
}

# Soma das duas mesorregiões do Vale do São Francisco.
pam_vale <- function(d) {
  d |>
    summarise(valor = sum(valor, na.rm = TRUE), .by = ano) |>
    mutate(local = "Vale do São Francisco", .before = 1)
}

# Os n maiores locais no ano de referência (com pam_cfg$excluir_rs, sem o Rio Grande
# do Sul: estado ou municípios "(RS)"). Devolve os nomes na ordem do ranking.
pam_top <- function(d, n, ano_ref = max(d$ano)) {
  d |>
    filter(ano == ano_ref, !pam_cfg$excluir_rs | !grepl("Rio Grande do Sul|\\(RS\\)", local), !is.na(valor)) |>
    slice_max(valor, n = n, with_ties = FALSE) |>
    pull(local)
}

# Ordena os locais pelo valor (decrescente) no ano de referência; usado nas tabelas.
pam_ordenar <- function(d, ano_ref = max(d$ano)) {
  ordem <- d |> filter(ano == ano_ref) |> arrange(desc(valor)) |> pull(local)
  d |> arrange(match(local, ordem), ano)
}

# Razão num/den por local e ano (ex.: produção / área = produtividade em t/ha).
pam_razao <- function(num, den, digitos = 1) {
  inner_join(num, den, by = c("local", "ano"), suffix = c("_n", "_d")) |>
    transmute(local, ano, valor = round(valor_n / valor_d, digitos))
}

# ---- Carga completa ---------------------------------------------------------
# Lê as 4 planilhas de uma variável (v = "area", "quanti" ou "valor"):
# regiões, estados (n_estados maiores no ano final), cidades (n_cidades maiores) e Vale.
pam_carregar <- function(v) {
  estados <- pam_ler(paste0(v, "_estados.xlsx"))
  cidades <- pam_ler(paste0(v, "_cidades.xlsx"))
  list(
    regioes = pam_ler(paste0(v, "_regioes.xlsx")) |> pam_regioes(),
    estados = estados |> filter(local %in% pam_top(estados, pam_cfg$n_estados)) |> pam_ordenar(),
    cidades = cidades |> filter(ano == max(ano), local %in% pam_top(cidades, pam_cfg$n_cidades)) |> pam_ordenar(),
    vale    = pam_ler(paste0(v, "_vale.xlsx")) |> pam_vale()
  )
}

# Produtividade (t/ha) = quantidade / área; para estados usa o rendimento médio
# do IBGE (kg/ha), nos mesmos estados do ranking de quantidade.
pam_produtividade <- function(area, quanti) {
  list(
    regioes = pam_razao(quanti$regioes, area$regioes),
    estados = pam_ler("produti_estados.xlsx") |>
      filter(local %in% quanti$estados$local) |> pam_ordenar(),
    cidades = pam_razao(quanti$cidades, area$cidades) |> pam_ordenar(),
    vale    = pam_razao(quanti$vale, area$vale)
  )
}

# ---- Gráficos ---------------------------------------------------------------
pam_cores <- c("darkgray", "lightblue3", "orange", "darkblue", "red", "darkgreen",
               "gold", "#0A6269", "#690F0A", "#6675E6", "purple3", "deepskyblue4",
               "tomato3", "forestgreen")

# tipo = "regiao": barras agrupadas por ano, legenda à direita (Brasil/regiões/estados)
#        "cidade": um único ano, rótulos inclinados, legenda embaixo
#        "vale"  : série única no tempo (x = ano)
pam_grafico <- function(d, ylab, xlab, escala = 1, tipo = c("regiao", "cidade", "vale"),
                        serie = "") {
  tipo <- match.arg(tipo)
  d <- mutate(d, valor = round(valor / escala, 2), ano = factor(ano))
  fonte <- paste0("Fonte: PAM/IBGE reprocessado pelo Observatório de Mercado de ", tools::toTitleCase(pam_cfg$cultura), " da Embrapa")

  g <- if (tipo == "vale") {
    ggplot(d, aes(ano, valor, fill = serie)) +
      geom_col() +
      scale_fill_manual(values = "blue")
  } else {
    cores <- if (tipo == "cidade") pam_cfg$cor_cidade else   # cidades: um único ano
      c(pam_cores, scales::hue_pal()(30))[seq_len(nlevels(d$ano))]   # mais de 14 anos: completa a paleta
    ggplot(d, aes(forcats::fct_reorder(local, valor, \(v) mean(v, na.rm = TRUE), .desc = TRUE),
                  valor, fill = ano)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = cores)
  }

  cidade <- tipo == "cidade"
  g + labs(y = ylab, x = xlab, caption = fonte) +
    theme_minimal() +
    theme(
      axis.text.x  = element_text(angle = if (cidade) 20 else 0, hjust = 0.5,
                                  size = if (cidade) 8 else 11, margin = margin(b = 20)),
      axis.text.y  = element_text(hjust = 0.5, size = if (cidade) 8 else 12, margin = margin(l = 20)),
      axis.title   = element_text(size = if (cidade) 10 else 12, face = "bold"),
      panel.grid   = element_blank(),
      plot.caption = element_text(hjust = 0, size = 12),
      legend.position = if (tipo == "regiao") "right" else "bottom",
      legend.title = element_blank(),
      legend.text  = element_text(size = if (tipo == "regiao") 10 else 12)
    )
}

# Versão interativa (HTML): converte o ggplot e posiciona a legenda.
pam_plotly <- function(g, tipo = c("regiao", "cidade", "vale")) {
  tipo <- match.arg(tipo)
  leg <- switch(tipo,
    regiao = list(orientation = "v", x = 1.0,  y = 0.1),
    cidade = list(orientation = "h", x = 0.35, y = -0.35),
    vale   = list(orientation = "h", x = 0.35, y = -0.2))
  plotly::ggplotly(g) |> plotly::layout(legend = c(leg, list(title = "")))
}

# ---- Tabelas ---------------------------------------------------------------
# Formato largo (uma coluna por ano), valores divididos por `escala`.
pam_tabela <- function(d, rotulo, escala = 1, digitos = 1) {
  d |>
    mutate(valor = round(valor / escala, digitos)) |>
    pivot_wider(names_from = ano, values_from = valor) |>
    rename(!!rotulo := local) |>
    DT::datatable(options = list(autoWidth = TRUE,
                                 columnDefs = list(list(className = "dt-center", targets = "_all"))))
}


# ---- Formatação de números para o texto automático ---------------------------
# Números em português (vírgula decimal, ponto de milhar), sem zeros à direita.
pam_fmt <- function(x, digitos = 1) {
  s <- formatC(x, format = "f", digits = digitos, big.mark = ".", decimal.mark = ",")
  if (digitos > 0) s <- sub(",?0+$", "", s)
  s
}
pam_lista <- function(x) if (length(x) < 2) x else paste(paste(head(x, -1), collapse = ", "), "e", tail(x, 1))
pam_var <- function(novo, velho, subiu = "crescimento", caiu = "redução", digitos = 1) {   # "crescimento de 3,1%"
  p <- (novo / velho - 1) * 100
  paste0(if (p >= 0) subiu else caiu, " de ", pam_fmt(abs(p), digitos), "%")
}
