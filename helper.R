#
# Loads a table corresponding to a sepcified config entry
#
load_table <- function(data_config, score_field) {
  # load data from csv/tsv
  if (endsWith(data_config$path, 'csv') || endsWith(data_config$path, 'csv.gz')) {
    dat <- read_csv(data_config$path, col_types = cols())
  } else if (endsWith(data_config$path, 'tsv') || endsWith(data_config$path, 'tsv.gz')) {
    dat <- read_tsv(data_config$path, col_types = cols())
  }

  cat(file=stderr(), "Score field: ", score_field, "\n")

  # drop excluded fields
  if (length(data_config$exclude) > 0) {
    dat <- dat %>%
      select(!!-data_config$exclude)
  }

  # drop non-selected weights/scores
  other_scores <- data_config$scores[!data_config$scores == score_field]

  if (length(other_scores) > 0) {
    dat <- dat %>%
      select(!!-other_scores)
  }

  # sort by score
  dat <- dat %>%
    arrange(desc(!!sym(score_field)))

  # round score and return result
  dat[, score_field] <- round(dat[, score_field], 3)

  dat
}
