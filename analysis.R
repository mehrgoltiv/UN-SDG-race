#Scanning SDG Doc for word frequencies
#July 24, 2020
#Mehrgol Tiv


# TOP ---------------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(corpus)
library(tidytext)
library(udpipe)

df = read.csv("sdg_doc.txt", encoding = "UTF-8")


# (1) Preparation ---------------------------------------------------------

udmodel = udpipe_download_model(language = "english") #downloads the model

udmodel = udpipe_load_model(udmodel$file_model) #loads the model so it can be used

x = udpipe_annotate(udmodel, x = df$VSN) #tokenizes by word - POS tagging, lemmatization
x = as.data.frame(x) #viewable

#Remove stop words
x.clean = x %>% anti_join(get_stopwords(), by = c(token = "word")) 


# (2) Text Frequency ------------------------------------------------------

nouns = x.clean %>% filter(upos == "NOUN") 

freq = txt_freq(nouns$lemma)

freq %>% top_n(15, abs(freq)) %>%
  mutate(key = reorder(key, freq)) %>%
  ggplot(aes(key, freq)) +
  geom_segment(aes(x = key, xend = key,
                   y = 0, yend = freq), 
               size = 1.1, alpha = 0.6, color = "pink") +
  geom_point(size = 3.5, color = "gray") +
  coord_flip() +
  theme_classic(base_size = 16, base_family = "Arial") +
  ylab("Frequency") + xlab("Word") + ggtitle("15 Most Frequent Nouns in Report")


# (3) Keywords ------------------------------------------------------------

#Taken from UN doc itself 

View(x.clean %>% filter(str_detect(lemma, "countr")))

terms = c("race", "colour", "sex", "language", "religion", 
          "birth", "disability", "ethnicity", "gender", "youth",
          "age", "migrant", "indigenous", "woman", "child", "girl")

#Generally, lemmatization worked. But it missed a couple instances of "women"/"woman" and "disability"/"disabilitie"
x.clean = x.clean %>% mutate(lemma = str_replace_all(lemma, "women", "woman"), 
                             lemma = str_replace_all(lemma, "disabilitie", "disability"),
                             lemma = str_replace_all(lemma, "migration", "migrant"),
                             lemma = str_replace_all(lemma, "migratory", "migrant"))

x.clean.terms = x.clean %>% filter(lemma %in% terms)

freq = txt_freq(x.clean.terms$lemma)


# (4) Co-occurrences ------------------------------------------------------

#Economic status, national origin, social origin, country origin, economic life, political life, political opinion

cooc = cooccurrence(x.clean, term = "lemma", group = "doc_id", skipgram = 5, order = F)

#Economic...
econ = cooc %>% filter(term1 == "economic" | term2 == "economic")

status = econ %>% filter(term1 == "status" | term2 == "status")
life = econ %>% filter(term1 == "life" | term2 == "life")

#...Origin
origin = cooc %>% filter(term1 == "origin" | term2 == "origin")

nat = origin %>% filter(term1 == "national" | term2 == "national")
count = origin %>% filter(term1 == "country" | term2 == "country")

#Political...
politic = cooc %>% filter(term1 == "political" | term2 == "political")

p.life = politic %>% filter(term1 == "life" | term2 == "life")
op = politic %>% filter(term1 == "opinion" | term2 == "opinion")

#MERGE all co-occurrences
cooc.merge = rbind(status, life, nat, count, p.life, op)

#Summarise
cooc.merge = cooc.merge %>% 
  mutate(key = ifelse(str_detect(cooc.merge$term1, "economic") | str_detect(cooc.merge$term2, "economic"), "economic", 
                      ifelse(str_detect(cooc.merge$term1, "origin") | str_detect(cooc.merge$term2, "origin"), "nationality", 
                             ifelse(str_detect(cooc.merge$term1, "political") | str_detect(cooc.merge$term2, "political"), "political", "")))) %>% 
  group_by(key) %>% 
  summarise(freq = sum(cooc)) 


# (5) Combine & Plot ------------------------------------------------------

all.freq = freq %>% full_join(cooc.merge)

freq.plot = all.freq %>% 
  mutate(key = reorder(key, freq)) %>%
  ggplot(aes(key, freq)) +
  geom_segment(aes(x = key, xend = key,
                   y = 0, yend = freq), 
               size = 1.1, alpha = 0.6, color = "gray") +
  geom_point(size = 3.5, color = "hotpink4") +
  coord_flip() +
  theme_classic(base_size = 16, base_family = "Arial") +
  ylab("Frequency") + xlab("") + ggtitle("Frequency of identity terms")


# (6) Export --------------------------------------------------------------

ggsave("identity.freq.png", freq.plot)
write.csv(all.freq, "freq.counts.csv", row.names = F)






