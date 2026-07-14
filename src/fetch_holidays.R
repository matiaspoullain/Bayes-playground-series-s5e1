library(httr)
library(jsonlite)
library(data.table)

COUNTRY_CODE_MAP <- c(
  Canada = "CA",
  Finland = "FI",
  Italy = "IT",
  Kenya = "KE",
  Norway = "NO",
  Singapore = "SG"
)

fetch_country_holidays <- function(country_name, country_code, year) {
  url <- sprintf("https://date.nager.at/api/v3/PublicHolidays/%d/%s", year, country_code)
  resp <- httr::GET(url)

  if (httr::status_code(resp) != 200) {
    warning(sprintf(
      "No holiday data for %s (%s) in %d (HTTP %d) - continuing with zero holidays for this country/year",
      country_name, country_code, year, httr::status_code(resp)
    ))
    return(data.table(date = character(0), country = character(0)))
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
  if (length(parsed) == 0 || is.null(parsed$date)) {
    return(data.table(date = character(0), country = character(0)))
  }

  data.table(date = parsed$date, country = country_name)
}

fetch_all_holidays <- function(country_code_map, years) {
  combos <- CJ(country_name = names(country_code_map), year = years, sorted = FALSE)
  combos[, country_code := country_code_map[country_name]]

  holiday_list <- Map(
    fetch_country_holidays,
    combos$country_name, combos$country_code, combos$year
  )
  rbindlist(holiday_list)
}
