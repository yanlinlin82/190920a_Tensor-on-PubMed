library(tidyverse)
library(rentrez)
library(xml2)

query_pubmed <- function(keyword, n = 100) {
  q <- entrez_search(db = "pubmed", term = keyword, use_history = TRUE)
  r <- entrez_fetch(db = "pubmed", web_history = q$web_history, rettype = "xml", retmax = n, retstart = 1)
  x <- read_xml(r)
  x
}

xml_to_tibble <- function(x, keyword) {
  tibble(pmid = xml_text(xml_find_all(x, "//MedlineCitation/PMID")),
         group = keyword,
         pub_date = sprintf("%04d", (x %>% xml_find_all("//PubDate/Year") %>% xml_text %>% as.integer)))
}

x1 <- query_pubmed("TensorFlow")
x2 <- query_pubmed("PyTorch")

a <- rbind(xml_to_tibble(x1, "TensorFlow"),
           xml_to_tibble(x2, "PyTorch"))

g <- a %>%
  count(group, pub_date) %>%
  ggplot(aes(x = pub_date, y = n)) +
  geom_path(aes(group = group)) +
  geom_point(aes(color = group), size = 15) +
  geom_text(aes(label = n)) +
  scale_y_continuous(breaks = seq(0,40,by=5)) +
  labs(x = "Publication Date", y = "# Papers", color = "Package")
g %>% ggsave(filename = "plot.png", width = 8, height = 6) 
