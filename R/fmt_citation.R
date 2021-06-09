#' Accepts a bibtex file, returns a formatted citation/bibtex key join table, meant to supplement the gt format functions
#'
#' @param bibtex_in bibtex file to read in
#' @return join table between BIBTEXKEY and citation
#' @examples
#' software.yaml <- yaml::read_yaml("data/softwareUsed.yaml")
#' software.df <- plyr::ldply(software.yaml$software, data.frame)
#' join.tbl <- fmt_citation("data/software.bib")
#' inner_join(software.df %>% separate_rows(bibtex) , join.tbl, by=c("bibtex"="BIBTEXKEY")) %>% group_by(name) %>%  mutate(allCit = paste0(cite, collapse = "; ")) %>% select(-c(bibtex,cite)) %>% unique() %>% ungroup() %>%  gt::gt() %>% tab_header(title="Bioinformatics Software Used", subtitle= md("&nbsp;") )
fmt_citation <- function(bibtex_in) {
	bibdf <- bib2df::bib2df(bibtex_in,separate_names = TRUE)

	bibdf.unnest <- bibdf  %>% unnest(cols=c("AUTHOR"))
	bibdf.counts <- bibdf.unnest %>% group_by(BIBTEXKEY) %>% summarise(num_auth = n())

	bibdf.rejoin <- inner_join(bibdf.unnest, bibdf.counts, by=c("BIBTEXKEY"="BIBTEXKEY"))

	bibdf.join <- rbind( bibdf.rejoin %>% filter(num_auth == 1 )%>% ungroup() %>% select(c("BIBTEXKEY", "full_name", "YEAR")),
						 bibdf.rejoin %>% filter(num_auth == 2 ) %>% group_by(BIBTEXKEY) %>% mutate(full_name =paste0(full_name, collapse = " & ")) %>% ungroup() %>% select(c("BIBTEXKEY","full_name", "YEAR")),
						 bibdf.rejoin %>% filter(num_auth > 2 )%>% group_by(BIBTEXKEY) %>% summarise(first_auth = head(full_name, 1), full_name=paste(first_auth, "et al. "), YEAR=unique(YEAR)) %>% ungroup() %>% select(c("BIBTEXKEY","full_name", "YEAR"))
						 ) %>% ungroup() %>% mutate(cite = paste(full_name, YEAR, sep = " "))  %>% select(c(BIBTEXKEY, cite)) %>%  unique()# %>% group_by(BIBTEXKEY) %>% summarise(citation = paste0(cite, collapse = "; ")) %>% ungroup()

	return(bibdf.join)
}
