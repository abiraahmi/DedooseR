# Generate a word cloud for excerpts by code

Creates a word cloud of words from all excerpts where a given code is
applied. Common English stop words, user-supplied stop words, and
punctuation are removed.

## Usage

``` r
wordcloud(data, code, max_words = 100, custom_stopwords = NULL)
```

## Arguments

- data:

  A data.frame or tibble containing at least one `excerpt` column and
  one or more code columns starting with `"c_"`.

- code:

  A string giving the name of the code column to filter on (e.g.
  "c_belonging").

- max_words:

  Maximum number of words to display in the word cloud (default = 100).

- custom_stopwords:

  A character vector of additional stop words to remove (default =
  `NULL`).

## Value

An interactive word cloud (from wordcloud2).

## Examples

``` r
library(dplyr)
df <- tibble::tibble(
  excerpt = c(
    "I felt connected to peers and friends.",
    "We should normalize conversations about mental health.",
    "My teachers helped me belong at school.",
    "I am comfortable talking about suicide prevention."
  ),
  c_belonging = c(TRUE, FALSE, TRUE, FALSE),
  c_destigmatization = c(FALSE, TRUE, FALSE, FALSE)
)

# Word cloud for belonging excerpts
wordcloud(df, "c_belonging")

{"x":{"word":["belong","connected","friends","helped","peers","school","teachers"],"freq":[1,1,1,1,1,1,1],"fontFamily":"Segoe UI","fontWeight":"bold","color":"random-dark","minSize":0,"weightFactor":180,"backgroundColor":"white","gridSize":0,"minRotation":-0.7853981633974483,"maxRotation":0.7853981633974483,"shuffle":true,"rotateRatio":0.4,"shape":"circle","ellipticity":0.65,"figBase64":null,"hover":null},"evals":[],"jsHooks":{"render":[{"code":"function(el,x){\n                        console.log(123);\n                        if(!iii){\n                          window.location.reload();\n                          iii = False;\n\n                        }\n  }","data":null}]}}
# With custom stop words
wordcloud(df, "c_belonging", custom_stopwords = c("connected", "school"))

{"x":{"word":["belong","friends","helped","peers","teachers"],"freq":[1,1,1,1,1],"fontFamily":"Segoe UI","fontWeight":"bold","color":"random-dark","minSize":0,"weightFactor":180,"backgroundColor":"white","gridSize":0,"minRotation":-0.7853981633974483,"maxRotation":0.7853981633974483,"shuffle":true,"rotateRatio":0.4,"shape":"circle","ellipticity":0.65,"figBase64":null,"hover":null},"evals":[],"jsHooks":{"render":[{"code":"function(el,x){\n                        console.log(123);\n                        if(!iii){\n                          window.location.reload();\n                          iii = False;\n\n                        }\n  }","data":null}]}}
```
