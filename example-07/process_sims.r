library(tidyverse)


sims <- list.files('.', pattern = 'sim.*\\.tsv')

parse_name <- function(x) {
  res <- gsub('sim_([^N]+)N_([^s]+)s_([^r]+)rbp_(\\d+)rep.tsv', 
              '\\1;;\\2;;\\3;;\\4', x)
  mat <- do.call(rbind, strsplit(res, ';;'))
  tibble(N = as.integer(mat[, 1]), 
         s = as.numeric(mat[, 2]), 
         rbp = as.numeric(mat[, 3]),
         rep = as.integer(mat[, 4]))
}


midpoint <- function(x) {
  out <- lapply(strsplit(as.character(x), ','), function(y) {
           as.numeric(gsub('(\\)|\\]|\\[|\\()', '', y))
       })
  bins <- do.call(rbind, out)
  rowMeans(bins)
}

bin_region <- function(x, nbins, total_length) {
  cut(x, seq(0, total_length, length.out=nbins))
}

d <- tibble(sims = sims) %>%
      mutate(params = map(sims, parse_name)) %>%
      mutate(results = map(sims, read_tsv, col_types = "id")) %>%
      unnest(c(params, results))


ds <- d %>% 
        mutate(bins = bin_region(pos, 100, 50e6),
               midpoint = midpoint(bins)) %>%
        group_by(N, s, rbp, midpoint) %>%
        summarize(het = mean(het))

ggplot(ds, aes(midpoint, het, color=as.factor(s)))  + geom_point() + 
  facet_wrap(~ rbp)  + geom_smooth()
