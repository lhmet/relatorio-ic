
pctg <- function(number, total, houses = 1) {
  p = round(((number/total) * 100),houses)
  return(p)}

################################################################################

grama.phps.create <- function(HELP = F, TYPE = "default", mdet_df = "default", STATES = "default", SUMMARY = "default", REFER_ANO = 2008, IS_QC = "default", PRODUCT3 = "default", VARIABLE = "default", MAKE_RECORDS = F, DATA = "default", DATA_RECORDS = "default", LIMIAR = 1, VAR_LEGEND = "default", WITH_TITLE = F, TITLE = "default", SAVE = F, NAME_SAVE = "default", DIRECTORY_SAVE = "default") {
  if (HELP == T) { # sumário para ajuda
    gg <- c(
      "1. Se TYPE == 'relevo', serão necessárias as variáveis mdet_df', 'SUMMARY';", 
      "2. Se TYPE == 'prepos', serão necessárias as variáveis 'STATES', 'SUMMARY', 'REFER_ANO';",
      "3.1 Se TYPE == 'bubble' e IS_QC == T, serão necessárias as variáveis 'PRODUCT3', 'SUMMARY', 'VARIABLE', 'STATES', 'VAR_LEGEND';",
      "3.2 Se TYPE == 'bubble' e IS_QC == F, serão necessárias as variáveis 'SUMMARY, 'VARIABLE', 'STATES', 'VAR_LEGEND';",
      "4.1 Se TYPE == 'yellow' e MAKE_RECORDS == T, serão necessárias as variáveis 'DATA', 'VARIABLE';",
      "4.2 Se TYPE == 'yellow' e MAKE_RECORDS == F, serão necessárias as variáveis 'DATA_RECORDS', 'VARIABLE';",
      "5.1 Se TYPE == 'K' e MAKE_RECORDS == T, serão necessárias as variáveis 'DATA', 'VARIABLE', 'LIMIAR';",
      "5.2 Se TYPE == 'K' e MAKE_RECORDS == F, serão necessárias as variáveis 'DATA_RECORDS', 'VARIABLE', 'LIMIAR';",
      "* Se deseja que o mapa tenha título (WITH_TITLE == T), serão necessárias as variáveis 'WITH_TITLE', 'TITLE';",
      "** Se deseja salvar qualquer situação acima (SAVE == T), serão necessárias as variáveis 'SAVE', 'NAME_SAVE', 'DIRECTORY_SAVE'.")
    }
  if (TYPE == "relevo") { # mapa do relevo de altitude das EMAs
    g <- 
      # gera superficie branca
      ggplot2::ggplot(mdet_df) +
      # ajusta o espaço
      ggplot2::coord_equal() +
      # plota relevo baseado em uma variavel
      ggplot2::geom_raster(aes(lon, lat, fill = alt)) +
      # torna o fundo branco
      ggplot2::theme_bw() +
      # muda a paleta de cores e adiciona legenda da variavel
      ggplot2::scale_fill_gradientn(
        colors = topo.colors(n = 6, alpha = 0), 
        name = "Altitude (em metros)",
        guide = "colourbar", 
        space = "Lab") +
      # adiciona contorno ao mapa
      ## ggplot2::geom_path(
        ## data = STATES,
        ## aes(x = lon, y = lat, group = group)) +
      # limpa o fundo, deixando coloramente homogeneo
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "transparent"),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_text(size = 15),
        axis.title.y = ggplot2::element_text(size = 15, angle = 90),
        axis.text.x = ggplot2::element_text(size = 13),
        axis.text.y = ggplot2::element_text(size = 13),
        legend.title = ggplot2::element_text(size = 11),
        legend.background = element_rect(fill = "transparent")) +
      # adiciona legendas x e y
      ggplot2::scale_y_continuous(name = "Latitude (°)") +
      ggplot2::scale_x_continuous(name = "Longitude (°)") +
      # adiciona pontos ao mapa correspondentes a localizaçao das EMAs
      ggplot2::geom_point(
        data = SUMMARY, 
        aes(x = lon, y = lat), 
        colour = "red",
        size = 5,
        shape = 20, # aparência do "ponto", 120 para 'x'
        show.legend = TRUE) +
      # adiciona nomes ao mapa correspondentes as sites das EMAs 
      ggrepel::geom_text_repel(
        data = SUMMARY,
        aes(x = lon, y = lat, label = site),
        fontface = "bold",
        vjust = -1.4,
#        repel_min_seg_len = 2.5,
        col = "black",
        size = 2.5) +
      # arruma a posiçao da legenda
      ggplot2::theme(legend.justification = c(0,1) ,legend.position = c(0.01, 0.99))
    if (WITH_TITLE == T) {
      gg <- g + ggplot2::ggtitle(TITLE) + ggplot2::theme(plot.title = element_text(hjust = 0.5))
      }
    if (WITH_TITLE == F) {
      gg <- g
      }
    }
  if (TYPE == "prepos") { # mapa para verificar as EMAs pré-pós 2008
    classe <- as.integer(year(SUMMARY$start) < REFER_ANO)
    SUMMARY$classe <- factor(classe)
    g <- 
      # add o espaço
      ggplot2::ggplot(data = STATES, aes(x = lon, y = lat)) +
      # ajusta o espaço
      ggplot2::coord_equal() +
      # add a figura correspondente ao sul
      ggplot2::geom_polygon(aes(group = group), color = "gray57", fill = "burlywood3") +
      # torna o fundo branco
      ggplot2::theme_bw() +
      # limpa o fundo
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "transparent"),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_text(size = 15),
        axis.title.y = ggplot2::element_text(size = 15, angle = 90),
        axis.text.x = ggplot2::element_text(size = 13),
        axis.text.y = ggplot2::element_text(size = 13),
        legend.title = ggplot2::element_text(size = 11),
        legend.background = element_rect(fill = "transparent")) +
      # latitude
      ggplot2::scale_y_continuous(
        breaks = pretty_breaks(5),
        minor_breaks = pretty_breaks(20),
        name = expression(Latitude~~(degree)),
        limits = c(-34.5, -22)) +
      # longitude
      ggplot2::scale_x_continuous(
        breaks = pretty_breaks(5),
        minor_breaks = pretty_breaks(20),
        name = expression(Longitude~~(degree)) ) +
      # add pontos
      ggplot2::geom_point(
        data = SUMMARY,
        aes(x = lon, y = lat, colour = classe),
        size = 4.5, # size: altera o tamanho dos pontos das EMAs no mapa
        shape = 20, # shape: formato dos pontos das EMAs no mapa, shape = 20 = "ponto"
        show.legend = TRUE) +
      ggplot2::geom_text(
        data = SUMMARY,
        aes(x = lon, y = lat, label = site),
        fontface = "bold",
        vjust = -1.4,
        col = "black",
        size = 2.8) +
      ggplot2::scale_colour_manual(
        name = "Data inicial",
        labels = c(paste0("Depois de ", REFER_ANO), paste0("Antes de ", REFER_ANO)),
        values = c("red", "blue")) +
      # posição da legenda
      ggplot2::theme(
        legend.justification = c(0,1) ,
        legend.position = c(0.01, 0.99))
    if (WITH_TITLE == T) {
      gg <- g + ggplot2::ggtitle(TITLE) + ggplot2::theme(plot.title = element_text(hjust = 0.5))
      }
    if (WITH_TITLE == F) {
      gg <- g
      }
    }
  if (TYPE == "bubble") { # Mapa de pontos com paleta de cores variante
    if (IS_QC == T) {
      data1 <-
        PRODUCT3 %>%
        dplyr::left_join(dplyr::select(SUMMARY, site, name, lat, lon), by = "site") %>%
        dplyr::select(site, name, everything()) %>%
        dplyr::filter(!is.na(tot)) %>%
        dplyr::arrange(desc(tot), site)
      data2 <- 
        PRODUCT3 %>%
        dplyr::left_join(dplyr::select(SUMMARY, site, name, lat, lon), by = "site") %>%
        dplyr::select(site, name, everything()) %>%
        dplyr::filter(is.na(tot)) %>%
        dplyr::arrange(desc(tot), site)
      if (NROW(data1) == 0) {
        g <- 
          ggplot2::ggplot(data = STATES, aes(x = lon, y = lat)) +
          ggplot2::coord_equal() +
          ggplot2::geom_polygon(aes(group = group), color = "gray57", fill = "burlywood3") +
          ggplot2::theme_bw() +
          ggplot2::geom_point(
            data = data2, 
            aes(x = lon, y = lat), 
            colour = "red",
            size = 5,
            shape = 120, # aparência do "ponto", 120 para 'x'
            show.legend = TRUE) +
          ggrepel::geom_text_repel(
            data = data2,
            aes(x = lon, y = lat, label = site),
            fontface = "bold",
            repel_min_seg_len = 2,
            vjust = -1.4,
            col = "gray30",
            size = 2.6) +
          ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = "transparent"),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_text(size = 15),
            axis.title.y = ggplot2::element_text(size = 15, angle = 90),
            axis.text.x = ggplot2::element_text(size = 13),
            axis.text.y = ggplot2::element_text(size = 13),
            legend.title = ggplot2::element_text(size = 11),
            legend.background = element_rect(fill = "transparent")) +
          ggplot2::scale_y_continuous(
            breaks = pretty_breaks(5),
            minor_breaks = pretty_breaks(20),
            name = expression(Latitude~~(degree)),
            limits = c(-34.5, -22)) +
          ggplot2::scale_x_continuous(
            breaks = pretty_breaks(5),
            minor_breaks = pretty_breaks(20),
            name = expression(Longitude~~(degree)))
        }
      if (NROW(data1) > 0) {
        g <-
          gg_bubble(
            data = data1,
            z = VARIABLE,
            breaks = c(pretty(data1[[VARIABLE]], n = 6)),
            limites = STATES,
            colors_z = viridis::viridis,
            color_fill = "burlywood3",
            z_legend = VAR_LEGEND,
            text_color = "black",
            point_color = "transparent",
            text_size = 2.6,
            point_size = 5,
            repel_min_seg_len = 2,
            legend.justification = c(0,1),
            legend.position = c(0.01, 0.99),
            guide_type = "colourbar") +
            ggplot2::geom_point(
              data = data2, 
              aes(x = lon, y = lat), 
              colour = "red",
              size = 5,
              shape = 120, # aparência do "ponto", 120 para 'x'
              show.legend = TRUE) +
            ggrepel::geom_text_repel(
              data = data2,
              aes(x = lon, y = lat, label = site),
              fontface = "bold",
              repel_min_seg_len = 2,
              vjust = -1.4,
              col = "gray30",
              size = 2.6)
        }
      if (WITH_TITLE == T) {
        gg <- g + ggplot2::ggtitle(TITLE) + ggplot2::theme(plot.title = element_text(hjust = 0.5))
        }
      if (WITH_TITLE == F) {
        gg <- g
        }
      }
    if (IS_QC == F) {
      g <- gg_bubble(
        data = SUMMARY,
        z = VARIABLE,
        breaks = c(pretty(SUMMARY[[VARIABLE]], n = 6)),
        limites = STATES,
        colors_z = viridis::viridis,
        color_fill = "burlywood3",
        z_legend = VAR_LEGEND,
        text_color = "black",
        point_color = "transparent",
        text_size = 2.6,
        point_size = 5,
        repel_min_seg_len = 2,
        legend.justification = c(0,1),
        legend.position = c(0.01, 0.99),
        guide_type = "colourbar")
      if (WITH_TITLE == T) {
        gg <- g + ggplot2::ggtitle(TITLE) + ggplot2::theme(plot.title = element_text(hjust = 0.5))
        }
      if (WITH_TITLE == F) {
        gg <- g
        }
    }
  }
  if (TYPE == "yellow") { # Gráfico amarelo
    if (MAKE_RECORDS == T) {
      DATA_RECORDS <-
        DATA %>%
        select(date, site, c(VARIABLE)) %>%
        group_by(site, format(date, "%m")) %>%
        openair::timeAverage(
          avg.time = "month", statistic = "data.cap", type = "site") %>%
        ungroup()
      n <- which(names(DATA_RECORDS) == VARIABLE)
      names(DATA_RECORDS)[n] <- "VaR"
      sites_ord <-
        DATA_RECORDS %>%
        group_by(site) %>%
        summarise(disp = mean(VaR, na.rm = TRUE)) %>%
        arrange(desc(site)) %>%
        pull(site) %>%
        as.character()
      records_month_plot <-
        DATA_RECORDS %>%
        mutate(
          tavg = ifelse(VaR > 0, VaR, NA),
          site = ordered(site, levels = sites_ord) )
      g <-
        ggplot(aes(x = date, y = site), data = records_month_plot) +
        geom_point(aes(colour = tavg), shape = 15, size = 2.5) + 
        labs(x = "Tempo (em anos)", y = "Estações Meteorológicas Automáticas - INMET") +
        scale_colour_gradientn(
          "obs/month\n(%)", colours = viridis::viridis(n = 256), na.value = NA) +
        scale_x_datetime(
          date_breaks = "12 months", 
          date_labels = "%Y") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme(text = element_text(size = 7))
      if (WITH_TITLE == T) {
        gg <- g + ggplot2::ggtitle(TITLE) + ggplot2::theme(plot.title = element_text(hjust = 0.5))
        }
      if (WITH_TITLE == F) {
        gg <- g
        }
    }
    if (MAKE_RECORDS == F) {
      n <- which(names(DATA_RECORDS) == VARIABLE)
      names(DATA_RECORDS)[n] <- "VaR"
      sites_ord <-
        DATA_RECORDS %>%
        group_by(site) %>%
        summarise(disp = mean(VaR, na.rm = TRUE)) %>%
        arrange(desc(site)) %>%
        pull(site) %>%
        as.character()
      records_month_plot <-
        DATA_RECORDS %>%
        mutate(
          tavg = ifelse(VaR > 0, VaR, NA),
          site = ordered(site, levels = sites_ord) )
      g <-
        ggplot(aes(x = date, y = site), data = records_month_plot) +
        geom_point(aes(colour = tavg), shape = 15, size = 2.5) + 
        labs(x = "Tempo (em anos)", y = "Estações Meteorológicas Automáticas - INMET") +
        scale_colour_gradientn(
          "obs/month\n(%)", colours = viridis::viridis(n = 256), na.value = NA) +
        scale_x_datetime(
          date_breaks = "12 months", 
          date_labels = "%Y") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme(text = element_text(size = 7))
      if (WITH_TITLE == T) {
        gg <- g + ggplot2::ggtitle(TITLE) + ggplot2::theme(plot.title = element_text(hjust = 0.5))
        }
      if (WITH_TITLE == F) {
        gg <- g
        }
      }
  }
  if (TYPE == "K") {
    if (MAKE_RECORDS == T) {
      DATA_RECORDS <-
        DATA %>%
        select(date, site, c(VARIABLE)) %>%
        group_by(site, format(date, "%m")) %>%
        openair::timeAverage(
          avg.time = "month", statistic = "data.cap", type = "site") %>%
        ungroup()
      n <- which(names(DATA_RECORDS) == VARIABLE)
      names(DATA_RECORDS)[n] <- "VaR"
      n_emas_by_month <-
        DATA_RECORDS %>% 
        mutate(is_avail = ifelse(VaR >= LIMIAR, 1, 0)) %>%
        group_by(date) %>%
        summarise(N = sum(is_avail))
      g <-
        n_emas_by_month %>%
        ggplot(., aes(x = date, y = N)) +
        geom_line() +
        labs(x = "Tempo (em anos)", y = "Estações Meteorológicas Automáticas - INMET") +
        scale_x_datetime(
          date_breaks = "12 months", 
          date_labels = "%Y") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme(text = element_text(size = 7))
      if (WITH_TITLE == T) {
        gg <- g + ggplot2::ggtitle(TITLE) + ggplot2::theme(plot.title = element_text(hjust = 0.5))
        }
      if (WITH_TITLE == F) {
        gg <- g
        }
    }
    if (MAKE_RECORDS == F) {
      n <- which(names(DATA_RECORDS) == VARIABLE)
      names(DATA_RECORDS)[n] <- "VaR"
      n_emas_by_month <-
        DATA_RECORDS %>% 
        mutate(is_avail = ifelse(VaR >= LIMIAR, 1, 0)) %>%
        group_by(date) %>%
        summarise(N = sum(is_avail))
      g <-
        n_emas_by_month %>%
        ggplot(., aes(x = date, y = N)) +
        geom_line() +
        labs(x = "Tempo (em anos)", y = "Estações Meteorológicas Automáticas - INMET") +
        scale_x_datetime(
          date_breaks = "12 months", 
          date_labels = "%Y") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme(text = element_text(size = 7))
      if (WITH_TITLE == T) {
        gg <- g + ggplot2::ggtitle(TITLE) + ggplot2::theme(plot.title = element_text(hjust = 0.5))
        }
      if (WITH_TITLE == F) {
        gg <- g
        }
    }
  }
  if (SAVE == T) {
    ggplot2::ggsave(
      filename = NAME_SAVE,
      plot = gg,
      device = "png",
      path = DIRECTORY_SAVE,
      width = 8.3,
      height = 6.25)
    }
  return(gg)
  }

################################################################################

qc.maps <- function(DIRECTORY, SUMMARY, STATES, VAR, LEGEND, TITLE) {
  qc <-
    readRDS(file = DIRECTORY) %>%
    left_join(select(SUMMARY, site, name, lat, lon), by = "site") %>%
    select(site, name, everything()) %>%
    filter(tot >= 0) %>%
    arrange(desc(tot), site)
  map <-
    simple.bubble(
      data = qc,
      variable = VAR,
      limits = STATES,
      legend = LEGEND,
      title = TITLE)
  return(map) }

################################################################################

# Funções Criadas para Ajuda
table.qc <- function(DIRECTORY, EMAS, TIME) {
    hm <-
      readRDS(DIRECTORY) %>% # Local onde encontra-se o produto 3 dos QCs
      dplyr::select(site, tot, perc) %>%
      dplyr::left_join(EMAS, by = "site") %>% 
      dplyr::select(site, state, name, tot, perc) %>%
      dplyr::filter(!is.na(tot)) %>%
      dplyr::arrange(desc(tot), site)
    names(hm)[1] <- "Código"
    names(hm)[2] <- "Estado"
    names(hm)[3] <- "Nome"
    if (TIME == "hours") {names(hm)[4] <- "Dados Suspeitos (em horas)"}
    if (TIME == "days") {names(hm)[4] <- "Dados Suspeitos (em dias)"}
    names(hm)[5] <- "Dados Suspeitos (em %)"
#    hmi <- knitr::kable(hm)
    hmi <- DT::datatable(hm)
    return(hmi) }




