#------------------------------------------------------------------------------#
#' Collect journal impact factors in the Science Citation Index
#'
#' Gather impact factors of academic journals requested by the user and
#' published on the SCI website (www.scijournal.org).
#'
#' "The impact factor (IF) or journal impact factor (JIF) of an academic journal
#' is a measure reflecting the yearly average number of citations to recent
#' articles published in that journal. It is frequently used as a proxy for the
#' relative importance of a journal within its field; journals with higher
#' impact factors are often deemed to be more important than those with lower
#' ones. The impact factor was devised by Eugene Garfield, the founder of the
#' Institute for Scientific Information. Impact factors are calculated yearly
#' starting from 1975 for those journals that are listed in the Journal Citation
#' Reports." (source: \url{https://en.wikipedia.org/wiki/Impact_factor})
#'
#' @param file Name of the file with academic journal names and relative URLs
#'     related to those journals on the SCI website. Each line of this file is
#'     about one journal and must have the following structure:
#'     `"journal name","relative URL"`. For a file example, take a look at:
#'     `system.file("extdata", "some_journals.txt", package = "cgmisc")`.
#' @param ... Not yet implemented.
#'
#' @return A data frame with three columns: "journal", "year", "impact_factor".
#'
#' @examples
#' my_file <- system.file("extdata", "some_journals.txt", package = "cgmisc")
#' my_data <- collect_impact_factor(my_file)
#' plot(my_data)
#'
#' @export
#------------------------------------------------------------------------------#
collect_impact_factor <- function(file, ...) {

    extract_data <- function(name = NULL, url) {
        html_page <- as.character(xml2::read_html(url))
        m   <- gregexpr("([0-9]{4}/)?[0-9]{4} Impact Factor : [0-9]*\\.[0-9]*",
                        html_page, perl = TRUE)
        res <- regmatches(html_page, m)
        res <- strsplit(res[[1]], " Impact Factor : ")
        res <- as.data.frame(matrix(unlist(res), nrow = length(res), byrow = TRUE),
                             stringsAsFactors = FALSE) %>%
            setNames(c("year", "impact_factor")) %>%
            dplyr::mutate(impact_factor = as.numeric(impact_factor))
        res$journal <- name
        res %>% dplyr::select(journal, year, impact_factor)
    }

    journals <- setNames(read.csv(file, header = FALSE, stringsAsFactors = FALSE),
                         c("name", "url"))
    journals$url <- paste0("http://www.scijournal.org/", journals$url)
    pbar   <- txtProgressBar(0, nrow(journals), style = 3)
    setTxtProgressBar(pbar, 0)
    out <- do.call(dplyr::bind_rows, lapply(1:nrow(journals), function(i) {
        res <- extract_data(journals[[i, 1]], journals[[i, 2]])
        setTxtProgressBar(pbar, i)
        res
    }))
    structure(out, class = c("impact_factor", "data.frame"))
}

#------------------------------------------------------------------------------#
#' @export
#------------------------------------------------------------------------------#
plot.impact_factor <- function(x, ...) {
    ggplot(x, aes(x = year, y = impact_factor, color = journal)) +
        geom_line(aes(group = journal)) +
        geom_point() +
        scale_color_discrete(name = "Journal") +
        annotate(geom = "text",
                 # "Character versions [of e.g. max] are sorted lexicographically"
                 x = max(x$year, na.rm = TRUE),
                 y = 0,
                 hjust = 1,
                 label = "Source: www.scijournal.org",
                 color = "grey") +
        expand_limits(y = 0) +
        labs(x = "Year", y = "Impact Factor") +
        theme_bw()
}

