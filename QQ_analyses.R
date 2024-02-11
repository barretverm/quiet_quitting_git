library(stringr)
library(tidyverse)
library(tidytext)
library(magrittr)
library(tidyr)

setwd('~/')
df <- read.csv('.csv', stringsAsFactors = F)

# PREP DATA ----

## unnest_tokens function ----

## to turn into tidy dataset, first put it into a dataframe
## tibble is good because it doesn't convert strings to factors
## and doesn't use row names
text_df <- tibble(text=text)
text_df

## break text into individual tokens and transform it 
## to a tidy data structure
text_df %>% 
  unnest_tokens(word, text)

## convert to tidy dataset
qq.df <- df %>% 
  group_by(month) %>% 
  mutate(
    linenumber=row_number(),
    tweet=cumsum(
      str_detect(
        text, month))) %>% 
  ungroup()

## tokenize ----
tidy_qq.df <- qq.df %>% 
  unnest_tokens(word, text)
tidy_qq.df


# EXPLORATORY ANALYSIS ----

## calculate most common words ----
tidy_qq.df %>% 
  count(word, sort=T) %>% 
  print(n=100)

## pipe words into a ggplot, terms > 2500 words
tidy_qq.df %>%
  count(word, sort = TRUE) %>%
  filter(n > 2500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

## break frequency down by months ----
jul <- tidy_qq.df %>% 
  filter(month=='jul')
aug <- tidy_qq.df %>% 
  filter(month=='aug')
sep <- tidy_qq.df %>% 
  filter(month=='sep')

jul %>%
  count(word, sort = TRUE)
aug %>%
  count(word, sort = TRUE)
sep %>%
  count(word, sort = TRUE)

## calculate the frequency of each word by month ----
library(tidyr)

frequency <- tidy_qq.df %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(month, word) %>%
  group_by(month) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = month, values_from = proportion) %>%
  pivot_longer(`jul`:`sep`,
               names_to = 'month', values_to = 'proportion')
frequency


# N-GRAMS ----
# you get n-grams by adding token = "ngrams" option to unnest_tokens()
qq.bi <- df %>%
  unnest_tokens(bigram, text, token = 'ngrams', n = 2) %>%
  filter(!is.na(bigram))
qq.bi

## counting and filtering bigrams ----
qq.bi %>% 
  count(bigram, sort=T)

## filter out uninteresting words ----

filtered <- separated %>%
  filter(!word1 %in% custom_stopwords) %>%
  filter(!word2 %in% custom_stopwords)

## create custom stop words list ----
custom_stopwords <- c(
  'quiet_quit','like', 'ie','im',
  'ive', 'just','basically', 'good')

## separate the bigrams
separated <- qq.bi %>%
  separate(bigram, c('word1', 'word2'), sep = ' ')

## filter out bigrams containing custom stop words ----
filtered <- separated %>%
  filter(!word1 %in% custom_stopwords) %>%
  filter(!word2 %in% custom_stopwords)

## count the new bigrams
counts <- filtered %>% 
  count(word1, word2, sort = TRUE)

## recombine the columns into one
bigrams_united <- filtered %>%
  unite(bigram, word1, word2, sep = ' ')

bigrams_united


## visualize n-grams ----

## pipe words into a ggplot
bigrams_united %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 250) %>%
  mutate(birgram = reorder(bigram, n)) %>%
  ggplot(aes(n, bigram)) +
  geom_col() +
  labs(y = NULL)

## tf-idf for bigrams ----
bigram_tf_idf <- bigrams_united %>%
  count(month, bigram) %>%
  bind_tf_idf(bigram, month, n) %>%
  arrange(desc(tf_idf))

## check
bigram_tf_idf

bigram_tf_idf %>%
  #group_by(month) %>%
  slice_max(tf_idf, n = 25) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf))) +
  geom_col(show.legend = FALSE) +
  #facet_wrap(~month, ncol = 2, scales = 'free') +
  labs(x = 'tf-idf', y = NULL)

## using bigrams to provide context in sentiment analysis ----

## visualizing a network of bigrams with a ggraph ----
library(igraph)

## bigram counts
counts

## filter for only relatively common combinations
bigram_graph <- counts %>%
  filter(n > 105) %>%
  graph_from_data_frame()

bigram_graph

## you can convert the igraph into a visualization with ggraph
library(ggraph)

ggraph(bigram_graph, layout = 'fr') +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


## make it prettier - the edge_alpha aesthetic makes links transparent based
## on how common or how rare. grid::arrow() adds directionality, including an 
## end_cap

set.seed(2020)

a <- grid::arrow(type = 'closed', length = unit(.15, 'inches'))

ggraph(bigram_graph, layout = 'fr') +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = 'lightblue', size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


# TOPIC MODELING ----
library(topicmodels)

tidy_qq.df

## run topic model ----
qq_dtm <- tidy_qq.df %>%
  unnest_tokens(word, text) %>%
  count(X, word) %>%
  cast_dtm(X, word, n)
qq_dtm



