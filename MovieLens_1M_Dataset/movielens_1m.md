MovieLens 1M Dataset
================
Roberto Preste
2018-09-06

From the book (chapter 14.2):

> [GroupLens Research](http://www.grouplens.org/node/73) provides a number of collections of movie ratings data collected from users of MovieLens in the late 1990s and early 2000s. The data provide movie ratings, movie metadata (genres and year), and demographic data about the users (age, zip code, gender identification, and occupation). \[...\] The MovieLens 1M dataset contains 1 million ratings collected from 6,000 users on 4,000 movies.

------------------------------------------------------------------------

``` r
library(tidyverse)
library(magrittr)
```

------------------------------------------------------------------------

Loading the data
----------------

This dataset is split over 3 files, containing different information. The `movies` dataset contains movies information, the `users` dataset contains voting users information and the `ratings` dataset contains movie ratings.

To my knowledge, no `R` package is capable of reading a file with double characters as column delimiter, as is in this case, where each column in the files is delimited using `::`. For this reason, we are first going to read in the entire raw files, replace occurrences of `::` with `_`, and then parse the datasets using a classic `read_delim`. Any other delimiter can be used instead of `_`, as long as it is not already present in the file (otherwise you will have issues when trying to load the data).

``` r
users <- read_file("../datasets/movielens/users.dat") %>% 
    str_replace_all("::", "_") %>% 
    read_delim(delim = "_", col_names = c("user_id", "gender", "age", "occupation", "zip"))

ratings <- read_file("../datasets/movielens/ratings.dat") %>% 
    str_replace_all("::", "_") %>% 
    read_delim(delim = "_", col_names = c("user_id", "movie_id", "rating", "timestamp"))

movies <- read_file("../datasets/movielens/movies.dat") %>% 
    str_replace_all("::", "_") %>% 
    read_delim(delim = "_", col_names = c("movie_id", "title", "genres"))
```

In order to access these data easily, we will merge them into a single dataframe instead of having data spread across 3 different tables.

``` r
mov_data <- ratings %>% 
    full_join(users, by = "user_id") %>% 
    full_join(movies, by = "movie_id")
```

``` r
head(mov_data)
```

    ## # A tibble: 6 x 10
    ##   user_id movie_id rating timestamp gender   age occupation zip   title
    ##     <int>    <int>  <int>     <int> <chr>  <int>      <int> <chr> <chr>
    ## 1       1     1193      5 978300760 F          1         10 48067 One …
    ## 2       1      661      3 978302109 F          1         10 48067 Jame…
    ## 3       1      914      3 978301968 F          1         10 48067 My F…
    ## 4       1     3408      4 978300275 F          1         10 48067 Erin…
    ## 5       1     2355      5 978824291 F          1         10 48067 Bug'…
    ## 6       1     1197      3 978302268 F          1         10 48067 Prin…
    ## # ... with 1 more variable: genres <chr>

Let's check the average rating for each movie grouped by gender of voting users.

``` r
mean_ratings <- mov_data %>% 
    group_by(title, gender) %>% 
    summarise(mean_rating = mean(rating)) %>% 
    spread(gender, mean_rating, fill = 0) %>% 
    select(-`<NA>`)
```

``` r
head(mean_ratings)
```

    ## # A tibble: 6 x 3
    ## # Groups:   title [6]
    ##   title                             F     M
    ##   <chr>                         <dbl> <dbl>
    ## 1 ...And Justice for All (1979)  3.83  3.69
    ## 2 'burbs, The (1989)             2.79  2.96
    ## 3 'Night Mother (1986)           3.39  3.35
    ## 4 'Til There Was You (1997)      2.68  2.73
    ## 5 $1,000,000 Duck (1971)         3.38  2.76
    ## 6 1-900 (1994)                   2     3

We may want to filter this dataframe to keep only movies that were rated by at least 250 users. So let's get the total number of users that rated each movie.

``` r
ratings_by_title <- mov_data %>% 
    group_by(title) %>% 
    summarise(voting_users = n())
```

``` r
head(ratings_by_title)
```

    ## # A tibble: 6 x 2
    ##   title                         voting_users
    ##   <chr>                                <int>
    ## 1 ...And Justice for All (1979)          199
    ## 2 'burbs, The (1989)                     303
    ## 3 'Night Mother (1986)                    70
    ## 4 'Til There Was You (1997)               52
    ## 5 $1,000,000 Duck (1971)                  37
    ## 6 1-900 (1994)                             2

``` r
active_titles <- ratings_by_title %>% 
    filter(voting_users >= 250)
```

``` r
head(active_titles)
```

    ## # A tibble: 6 x 2
    ##   title                             voting_users
    ##   <chr>                                    <int>
    ## 1 'burbs, The (1989)                         303
    ## 2 10 Things I Hate About You (1999)          700
    ## 3 101 Dalmatians (1961)                      565
    ## 4 101 Dalmatians (1996)                      364
    ## 5 12 Angry Men (1957)                        616
    ## 6 13th Warrior, The (1999)                   750

Now we can get the mean rating for this new subset of movies.

``` r
active_mean_ratings <- mean_ratings %>% 
    filter(title %in% active_titles$title)
```

Let's view the top rated movies across men and women voters.

``` r
top_male_ratings <- active_mean_ratings %>% 
    arrange(desc(M)) 
```

``` r
top_male_ratings[1:10, ]
```

    ## # A tibble: 10 x 3
    ## # Groups:   title [10]
    ##    title                                                           F     M
    ##    <chr>                                                       <dbl> <dbl>
    ##  1 Godfather, The (1972)                                        4.31  4.58
    ##  2 Seven Samurai (The Magnificent Seven) (Shichinin no samura…  4.48  4.58
    ##  3 Shawshank Redemption, The (1994)                             4.54  4.56
    ##  4 Raiders of the Lost Ark (1981)                               4.33  4.52
    ##  5 Usual Suspects, The (1995)                                   4.51  4.52
    ##  6 Star Wars: Episode IV - A New Hope (1977)                    4.30  4.50
    ##  7 Schindler's List (1993)                                      4.56  4.49
    ##  8 Wrong Trousers, The (1993)                                   4.59  4.48
    ##  9 Close Shave, A (1995)                                        4.64  4.47
    ## 10 Rear Window (1954)                                           4.48  4.47

``` r
top_female_ratings <- active_mean_ratings %>% 
    arrange(desc(F)) 
```

``` r
top_female_ratings[1:10, ]
```

    ## # A tibble: 10 x 3
    ## # Groups:   title [10]
    ##    title                                                      F     M
    ##    <chr>                                                  <dbl> <dbl>
    ##  1 Close Shave, A (1995)                                   4.64  4.47
    ##  2 Wrong Trousers, The (1993)                              4.59  4.48
    ##  3 Sunset Blvd. (a.k.a. Sunset Boulevard) (1950)           4.57  4.46
    ##  4 Wallace & Gromit: The Best of Aardman Animation (1996)  4.56  4.39
    ##  5 Schindler's List (1993)                                 4.56  4.49
    ##  6 Shawshank Redemption, The (1994)                        4.54  4.56
    ##  7 Grand Day Out, A (1992)                                 4.54  4.29
    ##  8 To Kill a Mockingbird (1962)                            4.54  4.37
    ##  9 Creature Comforts (1990)                                4.51  4.27
    ## 10 Usual Suspects, The (1995)                              4.51  4.52

Measuring rating disagreement
-----------------------------

If we want to find movies that are most divisive between male and female viewers, we can add a new `mean_diff` column to the `mean_ratings` dataset, showing the difference in mean rating across both genders.

``` r
active_mean_ratings %<>%  
    mutate(mean_diff = M - F)
```

Sorting the dataset by this new column in ascending order will return movies that were rated higher by women, while sorting in descending order will return movies rated higher by men.

``` r
high_ratings_male <- active_mean_ratings %>% 
    arrange(desc(mean_diff))
```

``` r
head(high_ratings_male)
```

    ## # A tibble: 6 x 4
    ## # Groups:   title [6]
    ##   title                                      F     M mean_diff
    ##   <chr>                                  <dbl> <dbl>     <dbl>
    ## 1 Good, The Bad and The Ugly, The (1966)  3.49  4.22     0.726
    ## 2 Kentucky Fried Movie, The (1977)        2.88  3.56     0.676
    ## 3 Dumb & Dumber (1994)                    2.70  3.34     0.639
    ## 4 Longest Day, The (1962)                 3.41  4.03     0.620
    ## 5 Cable Guy, The (1996)                   2.25  2.86     0.614
    ## 6 Evil Dead II (Dead By Dawn) (1987)      3.30  3.91     0.612

``` r
high_ratings_female <- active_mean_ratings %>% 
    arrange(mean_diff)
```

``` r
head(high_ratings_female)
```

    ## # A tibble: 6 x 4
    ## # Groups:   title [6]
    ##   title                         F     M mean_diff
    ##   <chr>                     <dbl> <dbl>     <dbl>
    ## 1 Dirty Dancing (1987)       3.79  2.96    -0.831
    ## 2 Jumpin' Jack Flash (1986)  3.25  2.58    -0.676
    ## 3 Grease (1978)              3.98  3.37    -0.608
    ## 4 Little Women (1994)        3.87  3.32    -0.549
    ## 5 Steel Magnolias (1989)     3.90  3.37    -0.536
    ## 6 Anastasia (1997)           3.8   3.28    -0.518

Movies that elicited the most disagreement among all viewers (both male and female) can be found by exploiting the standard deviation of ratings.

``` r
rating_std_by_title <- mov_data %>% 
    group_by(title) %>% 
    summarise(std = sd(rating))
```

We will only keep movies that were rated at least 250 times, as before, and sort the data to find the most controverse titles.

``` r
rating_std_by_title %<>% 
    filter(title %in% active_mean_ratings$title) %>% 
    arrange(desc(std))
```

``` r
rating_std_by_title[1:10, ]
```

    ## # A tibble: 10 x 2
    ##    title                                   std
    ##    <chr>                                 <dbl>
    ##  1 Dumb & Dumber (1994)                   1.32
    ##  2 Blair Witch Project, The (1999)        1.32
    ##  3 Natural Born Killers (1994)            1.31
    ##  4 Tank Girl (1995)                       1.28
    ##  5 Rocky Horror Picture Show, The (1975)  1.26
    ##  6 Eyes Wide Shut (1999)                  1.26
    ##  7 Evita (1996)                           1.25
    ##  8 Billy Madison (1995)                   1.25
    ##  9 Fear and Loathing in Las Vegas (1998)  1.25
    ## 10 Bicentennial Man (1999)                1.25

------------------------------------------------------------------------

``` r
sessionInfo()
```

    ## R version 3.5.1 (2018-07-02)
    ## Platform: x86_64-apple-darwin15.6.0 (64-bit)
    ## Running under: macOS High Sierra 10.13.6
    ## 
    ## Matrix products: default
    ## BLAS: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] bindrcpp_0.2.2  magrittr_1.5    forcats_0.3.0   stringr_1.3.1  
    ##  [5] dplyr_0.7.6     purrr_0.2.5     readr_1.1.1     tidyr_0.8.1    
    ##  [9] tibble_1.4.2    ggplot2_3.0.0   tidyverse_1.2.1
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_0.12.18     cellranger_1.1.0 pillar_1.3.0     compiler_3.5.1  
    ##  [5] plyr_1.8.4       bindr_0.1.1      tools_3.5.1      digest_0.6.15   
    ##  [9] lubridate_1.7.4  jsonlite_1.5     evaluate_0.11    nlme_3.1-137    
    ## [13] gtable_0.2.0     lattice_0.20-35  pkgconfig_2.0.1  rlang_0.2.1     
    ## [17] cli_1.0.0        rstudioapi_0.7   yaml_2.2.0       haven_1.1.2     
    ## [21] withr_2.1.2      xml2_1.2.0       httr_1.3.1       knitr_1.20      
    ## [25] hms_0.4.2        rprojroot_1.3-2  grid_3.5.1       tidyselect_0.2.4
    ## [29] glue_1.3.0       R6_2.2.2         fansi_0.3.0      readxl_1.1.0    
    ## [33] rmarkdown_1.10   modelr_0.1.2     backports_1.1.2  scales_1.0.0    
    ## [37] htmltools_0.3.6  rvest_0.3.2      assertthat_0.2.0 colorspace_1.3-2
    ## [41] utf8_1.1.4       stringi_1.2.4    lazyeval_0.2.1   munsell_0.5.0   
    ## [45] broom_0.5.0      crayon_1.3.4
