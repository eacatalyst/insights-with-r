# 09/19/2020

# 11- Strings - stringr

library(tidyverse)
library(stringr)

string1 <- "This is a string"
string2 <- 'To put a "quote" inside a string, use single quotes'

x <- c("\"", "\\")
x

writeLines(x)

x <- "\uOOb5"

x <- "\u00b5"
x

c("one", "two", "three")

str_length(c("a", "R for data science", NA))

str_c("x", "y")

str_c("x", "y", "z")

str_c("x", "y", sep = " , ")

x <- c("abc", NA)
str_c("|-", x, "-|")

str_c("1-", str_replace_na(x), "-|")

str_c("prefix-", c("a", "b", "c"), "-suffix")

name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE

str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
)

str_c(c("x", "y", "z"), collapse = ", ")

x <- c("Apple", "Banana", "Pear")
str_sub(x, 1,3)

str_sub(x, -3, -1)

x <- c("apple", "eggplant", "banana")
str_sort(x, locale = "en")

str_sort(x, local = "haw")

str_view()

x <- c("apple", "banana", "pear")
str_view(x, "an")

str_view(x, ".a.")

dot <- "\\."
writeLines(dot)

str_view(c("abc","a.c", "bef"), "a\\.c")

x <- "a\\b"
writeLines(x)

str_view(x, "\\\\")

x <- c("apple", "banana", "pear")
str_view(x, "^a")

str_view(x, "a$")

x <- c("applie pie", "apple", "apple cake")
str_view(x, "apple")

str_view(x, "^apple$")

str_view(c("grey", "gray"), "gr(e|a)y")

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")

str_view(x,"CC+")

str_view(x, 'C[LX]+')

str_view(x, "C{2}")

str_view(x, "C{2,}")

str_view(x, "C{2,3}")

str_view(x, 'C{2,3}?')

fruit

str_view(fruit, "(..)\\1", match = TRUE)

x <- c("apple", "banana", "pear")
str_detect(x,"e")

sum(str_detect(words, "^t"))

mean(str_detect(words, "[aeiou]$"))

# Find all words containing at least one vowel, and negate
no_vowels_1 <- !str_detect(words, "[aeiou]")
no_vowels_1
no_vowels_2 <- str_detect(words, "^[^aeiou]+$")
identical(no_vowels_1,no_vowels_2)

words[str_detect(words, "x$")]
str_subset(words, "x$")


df <- tibble(
  word = words,
  i = seq_along(word)
)

df %>%
  filter(str_detect(words, "x$"))

x <- c("apple", "banana", "pear")
str_count(x, "a")

# on average how many vowels per word?
mean(str_count(words, "[aeiou]"))

df %>%
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  )

str_count("abababab", "aba")

str_view_all("abababab", "aba")

length(sentences)

head(sentences)

colors <- c(
  "red", "orange", "yellow", "green", "blue", "purple"
)

color_match <- str_c(colors, collapse = "|")
color_match

has_color <- str_subset(sentences, color_match)
has_color
mataches <- str_extract(has_color, color_match)
head(matches)

matches

more <- sentences[str_count(sentences,color_match) >1]
str_view_all(more, color_match)

str_extract(more, color_match)

str_extract_all(more, color_match)

str_extract_all(more, color_match, simplify = TRUE)

noun <- "(a|the) ([^ ]+)"

has_noun <- sentences %>%
  str_subset(noun) %>%
  head(10)
has_noun %>%
  str_extract(noun)

has_noun %>%
  str_match(noun)

tibble(sentence = sentences) %>%
  tidyr::extract(
    sentence, c("article", "noun"), "(a|then) ([^ ]+)",
    remove = FALSE
  )

x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-")

str_replace_all(x, "[aeiou]", "-")

x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three"))

sentences %>%
  head(5) %>%
  str_split(" ")

"a|b|c|d" %>%
  str_split("\\|") %>%
  .[[1]]

sentences %>%
  head(5) %>%
  str_split(" ", simplify = TRUE)

fields <- c("Name: Hadley", "Country: NZ", "Age: 35")
fields %>% str_split(": ", n = 2, simplify = TRUE)

x <- "This is a sentence. This is another sentence."
str_view_all(x, boundary("word"))

str_split(x, " ")[1]
str_split(x, boundary("word"))[[1]]

str_view(fruit, "nana")
str_view(fruit, regex("nana"))

bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")

str_view(bananas, regex("banana", ignore_case = TRUE))

x <- "Line 1\nLine 2\nLine 3"
str_extract_all(x, "^Line")[[1]]
str_extract_all(x, regex("^Line", multiline = TRUE))[[1

library(microbenchmark)
microbenchmark::microbenchmark(  
  fixed = str_detect(sentences, fixed("the")),
  regex = str_detect(sentences, "the"),
  times = 20
  )

a1 <- "\u00e1"
a2 <- "a\u0301"

a1 <- "\u00e1"a2 
<- "a\u0301"


c(a1,a2)

str_detect(a1, fixed(a2))
str_detect(a1, coll(a2))

i <- c("I", "İ", "i", "ı")

str_subset(i, coll("i", ignore_case = TRUE))
str_subset(
  i,
  coll("i", ignore_case = TRUE, locale = "tr")
)

stringi::stri_locale_info()


x <- "This is a sentence"

str_view_all(x, boundary("word"))
str_extract_all(x, boundary("word"))

apropos("replace")

head(dir(pattern="\\.Rmd$"))


























