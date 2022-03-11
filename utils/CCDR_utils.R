#' @export
wbg_name <- function(indicatorID, indicator, by, year, mrv, denom) {
  if (!missing(indicatorID) && !missing(indicator)) {
    stop("Provide either indicatorID to lookup name, or indicator to custom build - not both.")
  }
  
  if (!missing(mrv)) {
    year = wbg_name_mrv(mrv)
  }
  
  if (!missing(indicatorID)) {
    wdi_ind <- get_wbcache()$indicators
    
    if (!(indicatorID %in% wdi_ind$indicatorID)) {
      stop("IndicatorID not found. Did you mean to name the first argument using indicator = ?")
    }
    
    ind_name <- wdi_ind$indicator[wdi_ind$indicatorID == indicatorID]
    
    m <- regexpr("(?<stem>[^\\(]+)\\((?<paren>[^\\)]+)\\)",ind_name, perl=TRUE)
    if (m == -1) {
      stem <- ind_name
      paren <- NULL
    } else {
      stem <- substr(ind_name, attr(m, "capture.start")[1], attr(m, "capture.start")[1]+attr(m, "capture.length")[1]-1)
      stem <- trimws(stem)
      paren <- substr(ind_name, attr(m, "capture.start")[2], attr(m, "capture.start")[2]+attr(m, "capture.length")[2]-1)
      
      if (attr(m, "match.length") < nchar(ind_name)) {
        warning(paste0("wbg_name() may be losing indicator name information for ",indicatorID,". Check this."))
      }
    }
  } else {
    stem <- indicator
    paren <- NULL
  }
  
  if (!missing(denom)) {
    paren <- denom
  }
  
  return(paste0(
    stem,
    if (!missing(by)) paste0(", ", by),
    if (!missing(year)) paste0(", ", endashify(year)),
    if (!is.null(paren)) paste0(" (", paren, ")")
  ))
}



#' @export
endashify <- function(s) {
  gsub("-", "–", s)
}



#' @export
create_wbgref <- function() {
  
  wbgref <- list()
  
  countries_df <- wbstats::wb_countries()
  
  wbgref$countries <- list(
    iso2c = countries_df$iso2c,
    iso3c = countries_df$iso3c,
    labels = unlist(modifyList(
      as.list(setNames(trimws(countries_df$country), countries_df$iso3c)),
      list(
        CIV = "Côte d\u2019Ivoire",
        FRO = "Faeroe Islands",
        STP = "São Tomé and Príncipe"
      ))
    ),
    iso2to3 = countries_df %>% select(iso2c, iso3c),
    regions = countries_df %>% select(iso3c, region_iso3c),
    incomegroups = countries_df %>% select(iso3c, income_level_iso3c)
  )
  
  regions_df <- countries_df %>%
    filter(iso3c %in% c("EAS","ECS","LCN","MEA","NAC","SAS","SSF"))
  
  wbgref$regions <- list(
    iso2c = regions_df$iso2c,
    iso3c = regions_df$iso3c,
    labels = setNames(trimws(regions_df$country), regions_df$iso3c),
    iso2to3 = regions_df[,c("iso2c", "iso3c")]
  )
  
  regions_excl_high_income_iso3c <- c(
    "EAS" = "EAP",
    "ECS" = "ECA",
    "LCN" = "LAC",
    "MEA" = "MNA",
    "SAS" = "SAS",
    "SSF" = "SSA"
  )
  
  regions_excl_high_income_df <- countries_df %>%
    filter(iso3c %in% regions_excl_high_income_iso3c)
  
  wbgref$regions_excl_high_income <- list(
    iso2c = regions_excl_high_income_df$iso2c,
    iso3c = regions_excl_high_income_df$iso3c,
    labels = setNames(trimws(regions_excl_high_income_df$country), regions_excl_high_income_df$iso3c),
    labels_2line = setNames(trimws(sub(" (", "\n(", regions_excl_high_income_df$country, fixed = TRUE)), regions_excl_high_income_df$iso3c),
    regions = data.frame(iso3c = regions_excl_high_income_iso3c, region_iso3c = names(regions_excl_high_income_iso3c), stringsAsFactors = FALSE),
    iso2to3 = regions_excl_high_income_df[,c("iso2c", "iso3c")]
  )
  
  incomes_df <- countries_df %>%
    filter(iso3c %in% c("HIC", "UMC", "LMC", "LIC")) %>%
    arrange(match(iso3c, c("HIC", "UMC", "LMC", "LIC")))
  
  wbgref$incomes <- list(
    iso2c = incomes_df$iso2c,
    iso3c = incomes_df$iso3c,
    labels = setNames(trimws(incomes_df$country), incomes_df$iso3c),
    iso2to3 = incomes_df[,c("iso2c", "iso3c")]
  )
  
  incomes3_df <- countries_df %>%
    filter(iso3c %in% c("HIC", "MIC", "LIC")) %>%
    arrange(match(iso3c, c("HIC", "MIC", "LIC")))
  
  wbgref$incomes3 <- list(
    iso2c = incomes3_df$iso2c,
    iso3c = incomes3_df$iso3c,
    labels = setNames(trimws(incomes3_df$country), incomes3_df$iso3c),
    iso2to3 = incomes3_df[,c("iso2c", "iso3c")]
  )
  
  wbgref$all_geo <- list(
    iso2c = countries_df$iso2c,
    iso3c = countries_df$iso3c,
    labels = setNames(trimws(countries_df$country), countries_df$iso3c),
    iso2to3 = countries_df[,c("iso2c", "iso3c")]
  )
  
  wbgref <<- wbgref
}