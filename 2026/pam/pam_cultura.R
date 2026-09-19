# Específico da UVA: configuração e texto da introdução (_intro.qmd).
# Usado junto com pam_funcoes.R (genérico, idêntico ao da manga).

# ---- Configuração da cultura --------------------------------------------------
pam_cfg <- list(
  cultura     = "uva",
  produto     = "c782/40274",           # SIDRA 5457: classificação 782, categoria 40274 = Uva
  pasta_dados = "dados_uva",            # tempecon/<pasta_dados>/<ano>/
  excluir_rs  = TRUE,                   # uva de mesa: subtrai o Rio Grande do Sul dos totais
  n_estados   = 6,                      # estados nos gráficos e tabelas
  n_cidades   = 16,                     # cidades nos gráficos e tabelas
  cor_cidade  = "darkblue",             # cor das barras de cidades
  rotulos     = NULL                    # sem nomes abreviados
)

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
