# Tai Sakuma <sakuma@cern.ch>

library(readr)
library(stringr)

library(gtools) # for mixedsort()

##__________________________________________________________________||
custom_read_table <- function(file)
{
  ## read_table() in readr 3.3.0 doesn't correctly determine the
  ## widths of columns. In particular, this is because
  ## whitespaceColumns(), used in fwf_empty(), doesn't correctly
  ## determine the position of the beginnings of columns.

  ## This function determines the widths of the columns from the first
  ## line (header) of a table file and use read_fwf().
  l <- read_lines(file, n_max = 1)

  whitespace_pos <- gregexpr(' ', l)[[1]]
  whitespace_pos <- c(0, whitespace_pos)
  diff_pos <- c(0, diff(whitespace_pos))

  begin <- c(0, whitespace_pos[diff_pos > 1])
  end <- c(begin[2:length(begin)] - 1, str_length(l))

  col_names <- str_split(l, ' ')[[1]]
  col_names <- col_names[col_names != '']

  col_positions <- list(
    begin = begin,
    end = end,
    col_names = col_names
  )

  tbl <- read_fwf(file, col_positions = col_positions, col_types = cols(), skip = 1, guess_max = Inf)
  ## - col_types = cols() suppresses the message "Parsed with column
  ##   ...""
  ## - skip = 1 skips the header, the first line
  ## - guess_max = Inf prevents float from incorrectly being
  ##   determined as int, which happens when a float column starts
  ##   with whole numbers without ".0", e.g., 1, 1, 3, 3.2, 5.6.
  ##   ** this option is slow

  ## convert 'character' to 'factor'
  col_types <- sapply(tbl, typeof)
  col_character <- names(col_types)[col_types == 'character']
  for(col in col_character) {
    tbl[[col]] <- factor(tbl[[col]])
    tbl[[col]] <- factor(tbl[[col]], levels = mixedsort(levels(tbl[[col]])))
  }

  tbl
}

##__________________________________________________________________||
