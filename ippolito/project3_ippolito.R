library(tidyverse)
library(RJSONIO)
library(tokenizers)
library(stopwords)
library(RMySQL)
library(RCurl)
library(stringr)

# Read CUNY master's in data science course catalog
cunyUrl <- "http://catalog.sps.cuny.edu/preview_program.php?catoid=2&poid=607"
catalog <- getURL(cunyUrl)

# Each course will show up like this in the html source:
# <a href="#" aria-expanded="false" onClick="showCourse('2', '979',this, 'a:2:{s:8:~location~;s:7:~program~;s:4:~core~;s:4:~1598~;}'); return false;">DATA 602 - Advanced Programming Techniques (3 Credits) </a>
# Use regex to parse
matches <- str_match_all(catalog, "showCourse\\('(\\d+)', *'(\\d+)',this, '(.+?)'.+?>(DATA \\d{3}) - (.+?) \\(?(\\d+.*?) Credits\\)?.+?<\\/a>")

# Build URL to fetch each course
# We need to end up with this format:
# http://catalog.sps.cuny.edu/ajax/preview_course.php?catoid=2&coid=979&link_text=&display_options=a:2:{s:8:~location~;s:7:~program~;s:4:~core~;s:4:~1598~;}&show
courseUrls <- str_c("http://catalog.sps.cuny.edu/ajax/preview_course.php?catoid=",
      matches[[1]][,2], "&coid=", matches[[1]][,3], "&link_text=&display_options=", matches[[1]][,4], "&show")

# Build dataframe
dfcuny <- data.frame(courseno = matches[[1]][,5], descr = matches[[1]][,6], credits = matches[[1]][,7], url = courseUrls)

# Fetch course descriptions using courseUrls
results <- sapply(dfcuny$url, function(x) try(getURL(x)))

# Extract first element from results
tmpa <- sapply(results, "[", 1)

# Parse results; this is the part we're interested in:
# DATA 607</a><span style="display: none !important">&#160;</span></em><br>In this course students will learn...<br><br>
coursematches <- str_match_all(tmpa, regex('<\\/em>.*<br>(.+?)<br><br>', dotall = TRUE))

# Extract second element from results
tmpb <- sapply(coursematches, "[", 2)

# Add the course text to the data frame
dfcuny <- mutate(dfcuny, course_text = tmpb)

# Tokenize
tokenize_ngrams(dfcuny$course_text, n = 5, n_min = 1, stopwords = stopwordlist)
