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

get_gene_external_links <- function(genes) {
  res <- tibble(
    'symbol'   = genes,
    'cBioPortal'  = sprintf("<a target='blank' href='https://www.cbioportal.org/results/cancerTypesSummary?case_set_id=all&gene_list=%s&cancer_study_list=5c8a7d55e4b046111fee2296'>link</a>", genes),
    'CARE'        = sprintf("<a target='blank' href='http://care.dfci.harvard.edu/?query_input=%s'>link</a>", genes),
    'COSMIC'      = sprintf("<a target='blank' href='https://cancer.sanger.ac.uk/cosmic/search?q=%s'>link</a>", genes),
    'GeneCards'   = sprintf("<a target='blank' href='https://www.genecards.org/cgi-bin/carddisp.pl?gene=%s'>link</a>", genes),
    'GDSC'        = sprintf("<a target='blank' href='https://www.cancerrxgene.org/search?query=%s'>link</a>", genes),
    'MeTeOR'      = sprintf("<a target='blank' href='http://meteor.lichtargelab.org/entity?entityInput=%s'>link</a>", genes),
    'PHARMGKB'    = sprintf("<a target='blank' href='https://www.pharmgkb.org/search?gaSearch=%s&query=%s'>link</a>", genes, genes),
    'Prot Atlas'  = sprintf("<a target='blank' href='https://www.proteinatlas.org/search/%s'>link</a>", genes),
    'Scholar'     = sprintf("<a target='blank' href='https://scholar.google.com/scholar?q=%s+cancer+sensitivity'>link</a>", genes)
  )
}
