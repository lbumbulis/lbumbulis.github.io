library(stringr)
na_blank <- function(x) {
  x[is.na(x)] <- ""
  x
}

add_dot <- function(x) {
  ifelse(x=="", x, paste0(x, "."))
}

fmt_names <- function(x, max_names = 4) {
  x <- x |>
    mutate(fn = add_dot(substr(first_name, 1, 1)),
           mn = add_dot(substr(na_blank(middle_name), 1, 1)),
           nn = str_glue("{last_name}, {fn}{mn}")) |>
    pull(nn) |>
    as.character()
  if (length(x) > max_names) x <- c(x[1:max_names], "et al.")
  paste(x, collapse = ", ")
}

clean_str <- function(str) {
  str <- str_remove_all(str, "\\{")
  str <- str_remove_all(str, "\\}")
  str <- str_replace(str, '\\\\+\"', "")
  str <- str_replace(str, "\\+", "")
  tools::toTitleCase(str)
}

bracket_wrap <- function(str) {
  paste0("[", str, "]")
}

paren_wrap <- function(str) {
  paste0("(", str, ")")
}

fmt_title <- function(title, url) {
  unname(str_glue("<p style='display:inline'>{bracket_wrap(clean_str(title))}{paren_wrap(url)}</p>"))
}

fmt_jvpy <- function(journal, volume, pgs, year) {
  jc <- case_when(
    !is.na(journal) ~ journal,
    TRUE ~ ""
  )
  jc <- na_blank(clean_str(jc))
  vol <- case_when(
    !is.na(volume) & !is.na(pgs) ~ str_glue("{volume}, {pgs}, {year}"),
    !is.na(volume) ~ str_glue("{volume}, {year}"),
    is.na(volume) ~ str_glue("{clean_str(year)}"),
  )
  unname(str_glue("<p>{jc}, {vol}</p>"))
}

clean_urls <- function(url) {
  str <- str_remove_all(url, "\\{")
  str <- str_remove_all(url, "\\}")
  str
}
