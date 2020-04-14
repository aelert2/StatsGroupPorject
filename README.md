Loading the libraries needed
============================

    rm(list = ls())

    library(data.table)
    library(tidyverse)

    ## ── Attaching packages ───────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.2.1     ✓ purrr   0.3.3
    ## ✓ tibble  2.1.3     ✓ dplyr   0.8.3
    ## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.4.0

    ## ── Conflicts ──────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::between()   masks data.table::between()
    ## x dplyr::filter()    masks stats::filter()
    ## x dplyr::first()     masks data.table::first()
    ## x dplyr::lag()       masks stats::lag()
    ## x dplyr::last()      masks data.table::last()
    ## x purrr::transpose() masks data.table::transpose()

    library(ggrepel)

Background about the data
=========================

Our project covers [NFL play-by-play data from 2009 to 2018 from
Kaggle](https://www.kaggle.com/maxhorowitz/nflplaybyplay2009to2016), and
we are using betting data from Kaggle (link to data here) to
contextualize the play-by-play data. <br> <br> The [extra point yard
line was
moved](http://www.nfl.com/news/story/0ap3000000493347/article/nfl-moves-extra-point-to-15yard-line-for-2015-season)
from the 2-yardline to the 15-yardline in 2015 because the NFL wanted to
increase the number of 2-point conversion attempts to boast ratings. We
want to analyze how attempting more 2-point conversions, especially in
the 4th quarter, can benefit teams.

Data Cleaning
=============

[Follow this link for using an open source Git extension to deal with
versioning large files.](https://git-lfs.github.com/) This is how we
were able to load the large play-by-play dataset.

    nfl_data <-  read_csv("NFLPlaybyPlay 2009-2018.csv")

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   home_team = col_character(),
    ##   away_team = col_character(),
    ##   posteam = col_character(),
    ##   posteam_type = col_character(),
    ##   defteam = col_character(),
    ##   side_of_field = col_character(),
    ##   game_date = col_date(format = ""),
    ##   game_half = col_character(),
    ##   time = col_time(format = ""),
    ##   yrdln = col_character(),
    ##   desc = col_character(),
    ##   play_type = col_character(),
    ##   pass_length = col_character(),
    ##   pass_location = col_character(),
    ##   run_location = col_character(),
    ##   run_gap = col_character(),
    ##   field_goal_result = col_character(),
    ##   extra_point_result = col_character(),
    ##   two_point_conv_result = col_character(),
    ##   timeout_team = col_character()
    ##   # ... with 88 more columns
    ## )

    ## See spec(...) for full column specifications.

    ## Warning: 3240 parsing failures.
    ##  row                                col           expected      actual                          file
    ## 1081 pass_defense_2_player_id           1/0/T/F/TRUE/FALSE 00-0025828  'NFLPlaybyPlay 2009-2018.csv'
    ## 1081 pass_defense_2_player_name         1/0/T/F/TRUE/FALSE W.Woodyard  'NFLPlaybyPlay 2009-2018.csv'
    ## 1619 forced_fumble_player_2_team        1/0/T/F/TRUE/FALSE MIA         'NFLPlaybyPlay 2009-2018.csv'
    ## 1619 forced_fumble_player_2_player_id   1/0/T/F/TRUE/FALSE 00-0023408  'NFLPlaybyPlay 2009-2018.csv'
    ## 1619 forced_fumble_player_2_player_name 1/0/T/F/TRUE/FALSE G.Camarillo 'NFLPlaybyPlay 2009-2018.csv'
    ## .... .................................. .................. ........... .............................
    ## See problems(...) for more details.

    spread_data = read.csv("spreadspoke_scores.csv")
    spread_data$newdate <- strptime((spread_data$schedule_date), "%m/%d/%Y")
    spread_data$newdate = format(spread_data$newdate, "%Y-%m-%d")
    spread_data$newdate = as.character(spread_data$newdate)
    post_td_plays <- nfl_data %>% 
      select(play_id,
             game_id,
             home_team,
             away_team,
             posteam,
             posteam_type,
             defteam,
             posteam_score,
             defteam_score,
             score_differential,
             posteam_score_post,
             defteam_score_post,
             score_differential_post,
             desc,
             game_date,
             qtr,
             game_seconds_remaining,
             play_type,
             yards_gained,
             contains("two_point"),
             kicker_player_name,
             kicker_player_id,
             blocked_player_id,
             blocked_player_name,
             contains("extra_point"),
             wp,
             def_wp,
             home_wp,
             away_wp,
             wpa,
             home_wp_post,
             away_wp_post,
             ydsnet, 
             ep, 
             epa) %>% 
      mutate(year = substr(game_id, 1, 4)) %>%
      filter(extra_point_attempt == 1 | two_point_attempt == 1 | defensive_extra_point_attempt == 1 | defensive_two_point_attempt == 1) %>% filter(home_team != "JAC", away_team != "JAC")

    post_td_plays$newdate <- as.character(strptime(as.Date(post_td_plays$game_date), "%Y-%m-%d"))        
    post_td_plays <- post_td_plays %>% left_join(spread_data, by = "newdate" )
    post_td_plays$home_team <- as.character(post_td_plays$home_team)
    post_td_plays$away_team <- as.character(post_td_plays$away_team)
    # post_td_plays$team_favorite_id <- as.character(post_td_plays$team_favorite_id)

    post_td_plays <- post_td_plays %>% filter(home_team == team_favorite_id | away_team == team_favorite_id)

    #Create one column that separates the types of extra point(s) try
    post_td_plays <- post_td_plays %>% 
      mutate(extra_point_type = ifelse(extra_point_attempt == 1,
                                   "Kick", "Two-PointConversion"))


    # Ratio of XPT:2PT attempts
    post_td_plays %>% 
      group_by(year, extra_point_attempt, two_point_attempt) %>% 
      summarise(n = n())

    ## # A tibble: 20 x 4
    ## # Groups:   year, extra_point_attempt [20]
    ##    year  extra_point_attempt two_point_attempt     n
    ##    <chr>               <dbl>             <dbl> <int>
    ##  1 2009                    0                 1    45
    ##  2 2009                    1                 0   918
    ##  3 2010                    0                 1    40
    ##  4 2010                    1                 0   968
    ##  5 2011                    0                 1    40
    ##  6 2011                    1                 0   986
    ##  7 2012                    0                 1    43
    ##  8 2012                    1                 0  1088
    ##  9 2013                    0                 1    63
    ## 10 2013                    1                 0  1042
    ## 11 2014                    0                 1    51
    ## 12 2014                    1                 0  1035
    ## 13 2015                    0                 1    68
    ## 14 2015                    1                 0   920
    ## 15 2016                    0                 1    91
    ## 16 2016                    1                 0  1052
    ## 17 2017                    0                 1    77
    ## 18 2017                    1                 0  1062
    ## 19 2018                    0                 1   111
    ## 20 2018                    1                 0  1009

    #Mean WPA change based on play type for post-touchdown 
    post_td_plays %>% 
      group_by(play_type) %>% 
      summarise(mean(wpa, na.rm = T))

    ## # A tibble: 4 x 2
    ##   play_type   `mean(wpa, na.rm = T)`
    ##   <chr>                        <dbl>
    ## 1 extra_point              -0.000687
    ## 2 no_play                   0.00763 
    ## 3 pass                     -0.0114  
    ## 4 run                      -0.00217

Data Visualization
==================

    # 2-pt Conversion Distribution
    post_td_plays %>% 
      filter(two_point_attempt == 1) %>% 
      group_by(year, two_point_conv_result) %>% 
      summarise(n = n()) %>% 
      ggplot(aes(x = year, y = n, group = two_point_conv_result)) +
      geom_line(aes(color = two_point_conv_result)) +
      geom_point(aes(color = two_point_conv_result)) +
      annotate(geom="text", x='2016', y=49, label="2015: XPT moved from 2YL to 15YL", color="darkgray") +
      labs(title="Plot of 2-pt Conversions in the NFL from 2009 to 2018",x="Year", y = "Count")

![](README_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    # XPT Conversion Distribution
    post_td_plays %>% 
      filter(extra_point_attempt == 1) %>% 
      group_by(year, extra_point_result) %>% 
      summarise(n = n()) %>% 
      ggplot(aes(x = year, y = n, group = extra_point_result)) +
      geom_line(aes(color = extra_point_result)) +
      geom_point(aes(color = extra_point_result)) +
      annotate(geom="text", x='2016', y=1100, label="2015: XPT moved from 2YL to 15YL", color="darkgray") +
      labs(title="Plot of Extra Points in the NFL from 2009 to 2018", x="Year", y = "Count")

![](README_files/figure-markdown_strict/unnamed-chunk-3-2.png)

    #Graph 4th quarter win probability changes based on type of extra point try
    post_td_plays %>% 
      filter(qtr == 4) %>% 
      ggplot(., aes(x=game_seconds_remaining, y=wpa, 
                                     color=extra_point_type)) + geom_point() + scale_x_reverse() +
      ggtitle("Win Probability Shifts in the 4th Quarter")

    ## Warning: Removed 146 rows containing missing values (geom_point).

![](README_files/figure-markdown_strict/unnamed-chunk-3-3.png)

    #Graph 4th quarter, 7 minutes left win probability changes based on type of extra point try
    post_td_plays %>% 
      filter(qtr == 4, game_seconds_remaining <= 420) %>% 
      ggplot(., aes(x=game_seconds_remaining, y=wpa, 
                    color=extra_point_type)) + geom_point() + scale_x_reverse()  +
      ggtitle("Win Probability Shifts in the last 7 min")

    ## Warning: Removed 96 rows containing missing values (geom_point).

![](README_files/figure-markdown_strict/unnamed-chunk-3-4.png)

Data Modeling
=============

    #str(post_td_plays)
    post_td_plays$posteam <- as.factor(post_td_plays$posteam)
    post_td_plays$defteam <- as.factor(post_td_plays$defteam)
    post_td_plays$play_type <- as.factor(post_td_plays$play_type)
    post_td_plays$two_point_attempt <- as.factor(post_td_plays$two_point_attempt)
    post_td_plays$two_point_conv_result <- as.factor(post_td_plays$two_point_conv_result)
    post_td_plays$kicker_player_id <- as.factor(post_td_plays$kicker_player_id)
    post_td_plays$extra_point_attempt <- as.factor(post_td_plays$extra_point_attempt)
    post_td_plays$extra_point_result <- as.factor(post_td_plays$extra_point_result)
    post_td_plays$year <- as.factor(post_td_plays$year)

    #str(post_td_plays)
    #summary(post_td_plays)

    # mod1 <- lm(wpa ~ posteam + defteam + score_differential + play_type + two_point_attempt + two_point_conv_result + kicker_player_id + extra_point_attempt + extra_point_result + year, data = post_td_plays)

    #Regression to see influential factors for two-point conversions specifcially
    #Just a test, planning to further develop
    lm_twopoint <- post_td_plays %>% 
      filter(extra_point_type == "Two-PointConversion") %>% 
      lm(wpa ~ game_seconds_remaining + ydsnet + posteam_score + defteam_score +
           score_differential + extra_point_prob + two_point_conversion_prob + ep + epa, data = .)

    summary(lm_twopoint)

    ## 
    ## Call:
    ## lm(formula = wpa ~ game_seconds_remaining + ydsnet + posteam_score + 
    ##     defteam_score + score_differential + extra_point_prob + two_point_conversion_prob + 
    ##     ep + epa, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.55329 -0.01590  0.01095  0.02582  0.38174 
    ## 
    ## Coefficients: (3 not defined because of singularities)
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                4.663e-03  6.900e-02   0.068    0.946    
    ## game_seconds_remaining     1.920e-05  4.176e-06   4.598 5.19e-06 ***
    ## ydsnet                    -1.338e-04  1.126e-04  -1.189    0.235    
    ## posteam_score             -4.080e-04  3.191e-04  -1.279    0.201    
    ## defteam_score              6.313e-04  2.661e-04   2.372    0.018 *  
    ## score_differential                NA         NA      NA       NA    
    ## extra_point_prob                  NA         NA      NA       NA    
    ## two_point_conversion_prob         NA         NA      NA       NA    
    ## ep                        -2.526e-02  7.206e-02  -0.351    0.726    
    ## epa                        3.494e-02  2.220e-03  15.738  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.05458 on 609 degrees of freedom
    ##   (13 observations deleted due to missingness)
    ## Multiple R-squared:  0.3127, Adjusted R-squared:  0.306 
    ## F-statistic: 46.19 on 6 and 609 DF,  p-value: < 2.2e-16
