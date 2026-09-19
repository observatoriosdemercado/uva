# Funções compartilhadas - PAM/IBGE (Observatório de Mercado de Uva da Embrapa)
# Usado por pam/index.qmd (HTML) e boletim pam/boletimPAM_uva.qmd (PDF).
# Mantenha as duas cópias idênticas.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

# ---- Localização dos dados --------------------------------------------------
# Procura tempecon/dados_uva/<pasta_ano> no Dropbox (Windows ou Mac).
# Para usar outro local: Sys.setenv(PAM_DADOS_BASE = "caminho/para/dados_uva")
pam_dir_dados <- function(pasta_ano, criar = FALSE) {
  bases <- c(
    Sys.getenv("PAM_DADOS_BASE"),
    file.path(Sys.getenv("USERPROFILE"), "Dropbox", "tempecon", "dados_uva"),
    file.path(Sys.getenv("HOME"), "Dropbox", "tempecon", "dados_uva")
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
pam_ler <- function(arquivo, dir = pam_dir) {
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

# ---- Transformações --------------------------------------------------------
# Brasil e Sul sem o Rio Grande do Sul (foco em uva de mesa).
pam_regioes <- function(d) {
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

# Os n maiores locais no ano de referência, excluindo o Rio Grande do Sul
# (estado ou municípios "(RS)"). Devolve os nomes na ordem do ranking.
pam_top <- function(d, n, ano_ref = max(d$ano)) {
  d |>
    filter(ano == ano_ref, !grepl("Rio Grande do Sul|\\(RS\\)", local), !is.na(valor)) |>
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
# regiões, estados (6 maiores no ano final), cidades (16 maiores no ano final) e Vale.
pam_carregar <- function(v) {
  estados <- pam_ler(paste0(v, "_estados.xlsx"))
  cidades <- pam_ler(paste0(v, "_cidades.xlsx"))
  list(
    regioes = pam_ler(paste0(v, "_regioes.xlsx")) |> pam_regioes(),
    estados = estados |> filter(local %in% pam_top(estados, 6)) |> pam_ordenar(),
    cidades = cidades |> filter(ano == max(ano), local %in% pam_top(cidades, 16)) |> pam_ordenar(),
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
  fonte <- "Fonte: PAM/IBGE reprocessado pelo Observatório de Mercado de Uva da Embrapa"

  g <- if (tipo == "vale") {
    ggplot(d, aes(ano, valor, fill = serie)) +
      geom_col() +
      scale_fill_manual(values = "blue")
  } else {
    cores <- if (tipo == "cidade") "darkblue" else   # cidades: um único ano
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

# ---- Texto automático da introdução ------------------------------------------
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

# Devolve os trechos (strings) usados na introdução de index.qmd e do boletim.
# Pontos de partida: objetos area, quanti, valor e prod já carregados.
pam_texto <- function(area, quanti, valor, prod) {
  fim <- max(area$regioes$ano); ant <- fim - 1
  ini <- min(area$regioes$ano); ini10 <- max(ini, fim - 10)
  no  <- \(d, l, a = fim) d$valor[d$local == l & d$ano == a]                # valor de um local/ano
  top <- \(d, n = Inf) d |> filter(ano == fim) |> arrange(desc(valor)) |> head(n)
  mil <- \(x, dg = 1) pam_fmt(x / 1000, dg)
  ibge <- fim + 1                                                            # ano de divulgação
  t <- list(ini = ini, fim = fim)

  # ---- área
  t$area_br <- mil(no(area$regioes, "Brasil"))
  reg <- top(filter(area$regioes, local != "Brasil"), 3)
  t$area_regioes <- pam_lista(sprintf("a %s com %s mil ha",
    ifelse(reg$local == "Sul", "Sul (Paraná e Santa Catarina)", as.character(reg$local)), mil(reg$valor)))
  est <- top(area$estados)
  t$area_estados <- pam_lista(sprintf("%s (%s mil ha)", est$local, mil(est$valor)))
  a0 <- no(area$vale, "Vale do São Francisco", ini10); a1 <- no(area$vale, "Vale do São Francisco")
  cresc <- pam_var(a1, a0, "aumento", "redução", digitos = 0)
  t$area_vale <- sprintf("%s, saindo de %s mil ha em %d para %s mil ha em %d, segundo o IBGE (%d), %s.",
    if (a1 > a0 && a1 >= no(area$vale, "Vale do São Francisco", ant))
      "O Vale do São Francisco mantém sua trajetória de crescimento de área"
    else "A área do Vale do São Francisco passou por variação no período",
    pam_fmt(a0 / 1000, 0), ini10, pam_fmt(a1 / 1000, 0), fim, ibge, paste("um", cresc))
  t$area_vale <- sub("um redução", "uma redução", t$area_vale)

  vale_mun <- if (file.exists(f <- file.path(pam_dir, "vale_municipios.csv")))
    read.csv(f, encoding = "UTF-8")$local else NULL
  c5 <- top(area$cidades, 5)
  no_vale <- if (is.null(vale_mun)) grepl("[(](PE|BA)[)]", c5$local) else c5$local %in% vale_mun
  n <- sum(no_vale)
  lista_c <- pam_lista(sprintf("%s (%s mil ha)", sub(" [(](..)[)]$", "/\\1", c5$local[no_vale]), pam_fmt(c5$valor[no_vale] / 1000, 2)))
  t$area_cidades <- if (n == 0) "nenhuma das cinco maiores produtoras está no Vale do São Francisco" else
    sprintf("%s do Vale do São Francisco %s entre as cinco maiores produtoras: %s",
            c("uma", "duas", "três", "quatro", "cinco")[n], if (n == 1) "está" else "estão", lista_c)

  # ---- quantidade
  brq <- no(quanti$regioes, "Brasil")
  t$quanti_br <- if (brq >= 1e6) paste(pam_fmt(brq / 1e6, 2), "milhões de") else paste(mil(brq), "mil")
  t$quanti_ne <- pam_fmt(no(quanti$regioes, "Nordeste") / brq * 100, 0)
  est <- top(quanti$estados)
  t$quanti_estados <- pam_lista(sprintf("%s (%s%%)", est$local, pam_fmt(est$valor / brq * 100, 1)))
  qv <- no(quanti$vale, "Vale do São Francisco")
  t$quanti_vale <- sprintf("Em %d, o Vale do São Francisco produziu cerca de %s mil toneladas, um %s em relação ao volume de %d.",
    fim, mil(qv), sub("^crescimento de", "crescimento de", pam_var(qv, no(quanti$vale, "Vale do São Francisco", ant))), ant)
  t$quanti_vale <- sub("um redução", "uma redução", t$quanti_vale)

  # ---- produtividade (t/ha)
  pne <- no(prod$regioes, "Nordeste"); pbr <- no(prod$regioes, "Brasil")
  t$prod_ne <- pam_fmt(pne, 1); t$prod_br <- pam_fmt(pbr, 1)
  t$prod_comp <- if (pne >= pbr) "maior" else "menor"
  pe <- top(prod$estados, 3)
  t$prod_estado1 <- as.character(pe$local[1])
  t$prod_estados <- sprintf("%s t/ha", pam_fmt(pe$valor / 1000, 0))
  t$prod_seguido <- pam_lista(sprintf("%s (%s t/ha)", pe$local[-1], pam_fmt(pe$valor[-1] / 1000, 0)))
  pv <- no(prod$vale, "Vale do São Francisco")
  t$prod_vale <- sprintf("Em %d a produtividade estimada para o Vale do São Francisco foi de %s t/ha, um %s em relação ao ano anterior.",
    fim, pam_fmt(pv, 1), pam_var(pv, no(prod$vale, "Vale do São Francisco", ant)))
  t$prod_vale <- sub("um redução", "uma redução", t$prod_vale)

  # ---- valor da produção (mil R$ -> bilhões)
  vbr <- no(valor$regioes, "Brasil"); vv <- no(valor$vale, "Vale do São Francisco")
  t$valor_br <- pam_fmt(floor(vbr / 1e5) / 10, 1)        # "mais de X"
  t$valor_vale_pct <- pam_fmt(vv / vbr * 100, 0)
  t$valor_vale <- pam_fmt(vv / 1e6, 1)
  t
}
