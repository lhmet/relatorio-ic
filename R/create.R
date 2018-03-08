# DIRECTORY = "/home/jonas/Documents/GITHUB/jbc-ic/output/"
# DIR_SUMMARY = "/home/jonas/Documents/GITHUB/jbc-ic/data_saves/summary_80.rds"
# SUMMARY <- readRDS(DIR_SUMMARY) %>% dplyr::select(site, state, name, lat, lon, alt)

create.list.p3 <- function(DIRECTORY) {
  files_dir <- list.files(path = DIRECTORY)
  n_p3 <- which(substr(x = files_dir, start = 6, stop = 12) == "summary")
  files_p3 <- files_dir[n_p3]
  CEL <- list()
  for (i in 1:length(files_p3)) {
    CEL[[i]] <- readRDS(paste0(DIRECTORY, files_p3[i])) }
  return(CEL) }

suspect.by.qc <- function(DIRECTORY, TABLE = FALSE) {
  PRODUCT3 <- create.list.p3(DIRECTORY = DIRECTORY)
  p0 <- 
    dplyr::bind_rows(lapply(
      X = PRODUCT3,
      FUN = function(PRODUCT3) {
        t <- PRODUCT3 %>%
          dplyr::summarise(
            qc = unique(qc),
            tot_obs = sum(n_obs),
            suspect = sum(tot, na.rm = TRUE),
            perc = pctg(suspect, tot_obs, houses = 7))
        return(t)}))
  obs_hour <- max(p0$tot_obs)
  obs_day <- min(p0$tot_obs)
  thour <-
    p0 %>%
    dplyr::filter(tot_obs == obs_hour) %>%
    dplyr::mutate(
      tot_obs = paste0(tot_obs, " h"),
      suspect = paste0(suspect, " h"))
  tday <-
    p0 %>%
    dplyr::filter(tot_obs == obs_day) %>%
    dplyr::mutate(
      tot_obs = paste0(tot_obs, " d"),
      suspect = paste0(suspect, " d"))
  p1 <-
    dplyr::bind_rows(thour, tday) %>%
    dplyr::arrange(qc)
  p2 <-
    p1 %>%
    dplyr::summarise(
      qc = "Total",
      tot_obs = paste0(" "),
      suspect = paste0(" "),
      perc = sum(perc))
  p3 <-
    dplyr::bind_rows(p1, p2)
  names(p3) [1] <- "QC"
  names(p3) [2] <- "Total de dados"
  names(p3) [3] <- "Suspeitos"
  names(p3) [4] <- "Suspeitos (em %)"
#  sbq <- knitr::kable(p3)
  sbq <- DT::datatable(p3)
  ifelse(TABLE == TRUE, return(sbq), return(p0))
  }

suspect.by.site <- function(DIRECTORY, SUMMARY, TABLE = FALSE) {
  PRODUCT3 <- create.list.p3(DIRECTORY = DIRECTORY)
  summ_sel1 <- SUMMARY %>% dplyr::select(site, state, name, lat, lon, alt)
  p0 <- 
    dplyr::bind_rows(lapply(
      X = PRODUCT3,
      FUN = function(PRODUCT3) {
        t <- PRODUCT3 %>%
          dplyr::summarise(
            qc = unique(qc),
            tot_obs = sum(n_obs))
        return(t)}))
  obs_hour <- max(p0$tot_obs)
  n_hour <- which(p0$tot_obs == obs_hour)
  obs_day <- min(p0$tot_obs)
  n_day <- which(p0$tot_obs == obs_day)
  PRODUCT3_hour <- dplyr::bind_rows(PRODUCT3[n_hour])
  PRODUCT3_day <- dplyr::bind_rows(PRODUCT3[n_day])
  p1_hour <- # para 11 qcs horários aplicados, tendo cada qc 78912 h (≅ 9 anos) de dados
    PRODUCT3_hour %>%
    dplyr::group_by(site) %>%
    dplyr::summarise(
      tot_obs_hour = sum(n_obs, na.rm = TRUE),
      suspect_hour = sum(tot, na.rm = TRUE),
      perc_hour = pctg(suspect_hour, tot_obs_hour, 7)
    ) %>% 
    dplyr::ungroup()
  p1_day <- # para 3 qcs diários aplicados, tendo cada qc 3288 d (≅ 9 anos) de dados
    PRODUCT3_day %>%
    dplyr::group_by(site) %>%
    dplyr::summarise(
      tot_obs_day = sum(n_obs, na.rm = TRUE),
      suspect_day = sum(tot, na.rm = TRUE),
      perc_day = pctg(suspect_day, tot_obs_day, 7)
    ) %>% 
    dplyr::ungroup()
  p1 <-
    dplyr::full_join(p1_hour, p1_day, by = "site") %>%
    dplyr::select(-(tot_obs_hour), -(tot_obs_day)) %>%
    dplyr::arrange(site)
  p2 <-
    p1 %>% 
    dplyr::full_join(summ_sel1, by = "site") %>% 
    dplyr::select(site, state, name, everything())
  p3 <-
    p2 %>%
    dplyr::summarise(
      site = "Total",
      state = paste0(" "),
      name = paste0(" "),
      suspect_hour = sum(suspect_hour, na.rm = TRUE),
#      perc_hour = sum(perc_hour, na.rm = TRUE),
      suspect_day = sum(suspect_day, na.rm = TRUE),
#      perc_day = sum(perc_day, na.rm = TRUE)
      )
  p4 <-
    p2 %>%
    dplyr::select(-perc_hour, -perc_day, -lat, -lon, -alt) %>% 
    dplyr::mutate( 
      state = as.character(state))
  p5 <- dplyr::bind_rows(p3, p4) %>% dplyr::arrange(site)
  names(p5) [1] <- "Site"
  names(p5) [2] <- "Estado"
  names(p5) [3] <- "Nome"
  names(p5) [4] <- "Suspeitos* (em h)"
  names(p5) [5] <- "Suspeitos** (em d)"
#  sbs <- knitr::kable(p5)
  sbs <- DT::datatable(p5)
  ifelse(TABLE == TRUE, return(sbs), return(p2))
  }




# dplyr::summarise(suspect = paste(rep("-", max(stringr::str_length(suspect))), collapse = '')
    
    
    
    