library(rvest)
library(dplyr)
library(tidyr)
library(stringr)


##############################################
# 1. get all links to rated newspapers (319):#
##############################################
link = "https://adfontesmedia.com/rankings-by-individual-news-source/"
page_headlines = read_html(link) %>% html_nodes(".fusion-post-content") %>% html_text() # get headlines of all linked pages 
urls = sapply(read_html(link) %>% html_nodes(".fusion-post-content"), function(x) {x %>% html_nodes("a") %>% html_attr("href")}) # get urls to all linked pagse


# string manipulation: strip " Bias and Reliability" from each headline to get putlerts name
for (i in 1:length(page_headlines)) {
  page_headlines[i] <- gsub(" Bias and Reliability", "", page_headlines[i])
}


# merge outlets with corresponding adfontes link
data = data.frame(page_headlines, urls)
colnames(data) <- c("outlet","adfontes_url")


##########################################################################
# 2. get overall bias and reliability score & bias and reliability class:#
##########################################################################

# define the two sets of urls: urls_ra <- all outlets for which articles have been rated; urls_nra <- all outlets for which no articles have been rated
urls_nra <- urls[c(7, 13, 61, 66, 85, 92, 104, 112, 132, 141, 145, 152, 181, 192, 210, 242, 246, 249, 252, 253, 262, 264, 266, 268, 269, 270, 277, 288, 293, 296, 315)]
urls_ra <- setdiff(urls, urls_nra)

# 2.1: bias & reliability scores + bias & reliability class for urls_ra >> "p strong"
df_1 <- data.frame(matrix(ncol = 2, nrow = 0), stringsAsFactors = FALSE) # define empty dataframe
colnames(df_1) <- c("html_text", "url") # rename columns

for (i in 1:length(urls_ra)) {
  overall_1 <- data.frame(read_html(urls_ra[i]) %>% html_nodes("p strong") %>% html_text(), stringsAsFactors = FALSE)
  overall_1$newcol <- urls_ra[i] 
  colnames(overall_1) <- c("text", "url")
  
  df_1 <- rbind(df_1, overall_1)
}

values_1 <- df_1$text


# 2.2: bias & reliability scores + bias & reliability class for urls_nra >> "b , p:nth-child(6)"
df_2 <- data.frame(matrix(ncol = 2, nrow = 0), stringsAsFactors = FALSE) # define empty dataframe
colnames(df_2) <- c("html_text", "url") # rename columns

for (i in 1:length(urls_nra)) {
  overall_2 <- data.frame(read_html(urls_nra[i]) %>% html_nodes("b , p:nth-child(6)") %>% html_text(), stringsAsFactors = FALSE)
  overall_2$newcol <- urls_nra[i] 
  colnames(overall_2) <- c("text", "url")
  
  df_2 <- rbind(df_2, overall_2)
}

values_2 <- df_2$text


# String Manipulation for df_1: add "Bias Class" and "Reliability Class" to each cell where needed: 
for (i in 1:length(values_1)){
  values_1[i] <- gsub("Skews Left", "Bias Class: skews left", values_1[i])
  values_1[i] <- gsub("Middle", "Bias Class: middle", values_1[i])
  values_1[i] <- gsub("Hyper-partisan Left", "Bias Class: hyper-partisan left", values_1[i])
  values_1[i] <- gsub("Hyper-partisan Right", "Bias Class: hyper-partisan right", values_1[i])
  values_1[i] <- gsub("Most extreme Right", "Bias Class: most extreme right", values_1[i])
  values_1[i] <- gsub("Skews Right", "Bias Class: skews right", values_1[i])
  values_1[i] <- gsub("Generally Reliable, Analysis OR Other Issues", "Reliability Class: generally reliable, analysis or other issues", values_1[i])
  values_1[i] <- gsub("Mixed Reliability, Opinion OR Other Issues", "Reliability Class: mixed reliability, opinion or other issues", values_1[i])
  values_1[i] <- gsub("Reliable, Analysis/Fact Reporting", "Reliability Class: reliable, analysis/fact reporting", values_1[i])
  values_1[i] <- gsub("Reliable, Fact Reporting", "Reliability Class: reliable, fact reporting", values_1[i])
  values_1[i] <- gsub("Unreliable, Inaccurate", "Reliability Class: unreliable, inaccurate", values_1[i])
  values_1[i] <- gsub("Unreliable, Misleading", "Reliability Class: unreliable, misleading", values_1[i])
  values_1[i] <- gsub("Unreliable, Problematic", "Reliability Class: unreliable, problematic", values_1[i])
}

# overwrite text column with adjusted values:
df_1$text <- values_1


# String Manipulation for df_2: add "Bias Class" and "Reliability Class" to each cell where needed:
for (i in 1:length(values_2)){
  values_2[i] <- gsub("skews left", "Bias Class: skews left", values_2[i])
  values_2[i] <- gsub("middle", "Bias Class: middle", values_2[i])
  values_2[i] <- gsub("hyper-partisan left", "Bias Class: hyper-partisan left", values_2[i])
  values_2[i] <- gsub("hyper-partisan right", "Bias Class: hyper-partisan right", values_2[i])
  values_2[i] <- gsub("most extreme right", "Bias Class: most extreme right", values_2[i])
  values_2[i] <- gsub("skews right", "Bias Class: skews right", values_2[i])
  values_2[i] <- gsub("generally reliable, analysis or other issues", "Reliability Class: generally reliable, analysis or other issues", values_2[i])
  values_2[i] <- gsub("generally reliable, analysis and other issues", "Reliability Class: generally reliable, analysis and other issues", values_2[i])
  values_2[i] <- gsub("reliable, fact reporting", "Reliability Class: reliable, fact reporting", values_2[i])
  values_2[i] <- gsub("unreliable, problematic", "Reliability Class: unreliable, problematic", values_2[i])
  values_2[i] <- gsub("unreliable, misleading", "Reliability Class: unreliable, misleading", values_2[i])
  values_2[i] <- gsub("reliable, analysis/fact reporting", "Reliability Class: reliable, analysis/fact reporting", values_2[i])
  values_2[i] <- gsub("mixed reliability, opinion or other issues", "Reliability Class: mixed reliability, opinion or other issues", values_2[i])
  values_2[i] <- gsub("mixed reliability, opinion and other issues", "Reliability Class: mixed reliability, opinion and other issues", values_2[i])
  values_2[i] <- gsub("unreliable, inaccurate", "Reliability Class: unreliable, inaccurate", values_2[i])
}

# overwrite text column with adjusted values:
df_2$text <- values_2


# concatenate df_1 and df_2:
overall_ratings = rbind(df_1, df_2)


# split text column at ": "
overall_ratings <- overall_ratings %>% separate(text, c("type", "value"), ": ") # split overall_ratings cell in 2 cells: type and value
overall_ratings <- reshape(overall_ratings, direction = "wide", idvar = "url", timevar = "type") # pivot
colnames(overall_ratings) <- c("url", "bias_class", "reliability_class", "reliability_score", "bias_score") # rename columns

overall_ratings <- merge(overall_ratings, data, by.x = "url", by.y = "adfontes_url") # add outlet name

# save as csv:
#write.csv(overall_ratings, "../data/ad_fontes/outlets_classes_scores.csv")


###############################################################
# 3. get ratings & article links for all newspapers >> urls_ra#
###############################################################
# outlets where table is empty and outlets which are not on Twitter (banned): 
urls_exclude <- union(urls[c(143, 156, 124, 159, 258, 265, 320)], urls_nra)

# exclude those outlets from urls_ra:
urls_ra <- setdiff(urls, urls_exclude)


# define empty data frame
df <- data.frame(matrix(ncol = 5, nrow = 0), stringsAsFactors = FALSE)
colnames(df) <- c('title', 'bias_score', 'reliability_score', 'article_url', 'adfontes_url')

for(i in 1:length(urls_ra)) {
  collect <- data.frame(read_html(urls_ra[i]) %>% html_nodes(".has-text-align-left a") %>% html_text(), 
                        read_html(urls_ra[i]) %>% html_nodes(".has-text-align-left+ td") %>% html_text(), 
                        read_html(urls_ra[i]) %>% html_nodes(".has-text-align-center+ td") %>% html_text(),
                        sapply(read_html(urls_ra[i]) %>% html_nodes("td.has-text-align-left"), function(x) {x %>% html_nodes("a") %>% html_attr("onclick")}),
                        stringsAsFactors = FALSE)
  collect$newcol <- urls_ra[i]
  colnames(collect) <- c('title', 'bias_score', 'reliability_score', 'article_url', 'adfontes_url')
  
  df <- rbind(df, collect)
}


# String manipulation of artice urls (remove unnecessary infos):
article_url <- df$article_url

for (i in 1:length(article_url)){
  article_url[i] <- gsub("window.open\\(\\'|\\'", "", article_url[i])
  article_url[i] <- noquote(str_split(gsub("window.open\\(\\'|\\'", "", article_url[i]), ",")[[1]][1])
}


# overwrite text column with adjusted values:
df$article_url <- article_url

# save as csv:
#write.csv(df, "../data/ad_fontes/articles_scores.csv")
