library(rvest)
library(tidyverse)
library(data.table)
library(lubridate)

gather_data <- function(x){
  url <- paste("https://www.j-archive.com/showseason.php?season=",as.character(x), sep="")
  h <- read_html(url)
  tab <- html_nodes(h, "table")
  tab <- html_table(tab)
  tab <- tab[[1]]
  tab <- tab %>% setNames(c("Date", "Contestants", "Notes"))
  # Grouped regex to divide first column by episode number and date:
  regex <- "#(\\d*), aired (\\d{4}-\\d{2}-\\d{2})"
  
  # Decoding Date column from utf-8:
  tab$Date <- iconv(tab$Date, to='ASCII//TRANSLIT')
  
  # Splitting date column into "Episode #" and "Date":
  tab <- tab %>% extract(Date, c("Episode #", "Date"), regex = regex)
  
  # Convert date column to class "Date":
  tab$Date <- as.Date(tab$Date)
  
  # Change episode #'s from characters to integers:
  tab$`Episode #` <- as.integer(tab$`Episode #`)
  
  # Separate contestants into individual columns:
  tab_cont <- tab %>% separate(Contestants,c("Contestant 1","Contestant 2","Contestant 3"), sep = " vs. ")
  tab_cont$Season <- x
  tab_cont <- tab_cont[,c(7,1,2,3,4,5,6)]
  tab_cont
}

seasons <- seq(1,37)
# all_seasons_df <- sapply(seasons,gather_data)

every_season <- list()
for(x in seasons) {
  every_season[[(length(every_season) + 1)]] = gather_data(x)
}

#_______________________________________________________________________________
# Assessing how complete our data is:
find_gaps <- function(x){
  tester <- seq(min(x),max(x))
  setdiff(tester,x)
}

total_episodes <- function(x){
  as.integer(max(every_season[[x]]$`Episode #`))-as.integer(min(every_season[[x]]$`Episode #`))
}
total_missing <- function(x){
  length(find_gaps(every_season[[x]]$`Episode #`))
}
assess_data <- data.frame(season = seasons, episode_count = sapply(seasons,total_episodes), missing = sapply(seasons,total_missing))
assess_data <- assess_data %>% mutate(percent = (episode_count - missing) / episode_count)

# Viewing completeness of data:
ggplot(assess_data,aes(season,percent)) +
  geom_bar(stat = "identity")
#_______________________________________________________________________________
# Creating a single data frame with all of the data:
every_season_df <- rbindlist(every_season)
every_season_df <- every_season_df %>%
  arrange(`Episode #`)

#_______________________________________________________________________________
# Detecting episodes in our data for which contestant 1 did not appear the previous week:
problem_finder5 <- function(df){
  # Apply the nested function to the entire range of the data frame to produce 
  # a list of indexes that satisfy the nested function criteria
  len <- seq(2, nrow(df))
  return_ind <- function(index){
    a <- df$`Contestant 1`[index]
    b <- df$`Contestant 1`[index-1]
    c <- df$`Contestant 2`[index-1]
    d <- df$`Contestant 3`[index-1]
    e <- df$`Episode #`[index]
    f <- df$`Episode #`[index-1]
    g <- df$Notes[index-1]
    # If contestant 1 does not appear in the previous episode, return index for this episode.
    if ((a != b) & (a != c) & (a != d) & e == f + 1 & (str_detect(g," game 5.") == FALSE)){
      index
    }
  }
  unlist(sapply(len, return_ind))
}
problem_inds <- problem_finder5(every_season_df)

# Tibble showing "Notes" entry for each problem found
tibble(Index = problem_inds, Notes = every_season_df$Notes[problem_inds])

# We see that most of the problems we detected were due to the presence of 
# "tournament play". Let's remove tournaments to run this analysis on the 
# regular season only:

# Removing tournament play:
tournaments <- c("Back to School Week", "College Championship",
                 "Teen Tournament", "Kids Week", "Power Players", 
                 "Tournament of Champions", "Celebrity Jeopardy!",
                 "2009-2010 Million Dollar Celebrity Invitational",
                 "Teachers Tournament", "All-Star Games", 
                 "Battle of the Decades", 
                 "The IBM Challenge game 1, Jeopardy! Round only.",
                 "The IBM Challenge, continuation of game 1.",
                 "The IBM Challenge game 2.", "Returning Champions",
                 "Senior Tournament", "Seniors Tournament",
                 "Anniversary Tournament", "Olympic Games Tournament",
                 "International Tournament", "Celebrity Invitational",
                 "Teen Reunion Tournament", "Armed Forces Week", 
                 "International Championship", "Million Dollar Masters",
                 "Returning champions")

# Returns a logical vector with 'TRUE' for rows in our data frame that 
# include phrases from our tournament list in the notes column
str_detect(every_season_df$Notes, paste(tournaments, collapse = "|"))

# Removing rows from the data frame that include tournament names in notes:
removed_tournaments <- every_season_df %>% filter(!str_detect(Notes, paste(tournaments, collapse = "|")))

# Display tibble of remaining problem indexes and accompanying notes. We see
# three games in which there was no winner the previous week and three games
# in which the reigning champion didn't return. The ones in which there was no
# winner won't be a problem but the ones where the champ didn't return will give 
# misleading results (They will count as losses).
tibble(Index = problem_finder5(removed_tournaments), Notes = removed_tournaments$Notes[problem_finder5(removed_tournaments)])

# Removing the three weeks where reigning champion didn't return:
removed_tournaments <- removed_tournaments[-c(3203,4485,5370),]

#_______________________________________________________________________________
# Let's wrangle all of the scores and individual episode data:
scores_regex <- paste(c("-{0,1}\\$\\d+,\\d+", "-{0,1}\\$\\d+"), collapse="|")

get_scores <- function(x){
  url <- paste("https://www.j-archive.com/showgame.php?game_id=",as.character(x), sep="")
  h <- read_html(url)
  scores <- h %>% html_nodes("#final_jeopardy_round > table:nth-child(4)") %>% html_text()
  dj_scores <- h %>% html_nodes("#double_jeopardy_round > table:nth-child(4)") %>% html_text()
  jr_scores <- h %>% html_nodes("#jeopardy_round > table:nth-child(6)") %>% html_text()
  cb_scores <- h %>% html_nodes("#jeopardy_round > table:nth-child(4)") %>% html_text()
  coryat_scores <- h %>% html_nodes("#final_jeopardy_round > table:nth-child(8)") %>% html_text()
  if(is_empty(coryat_scores) == TRUE){
    coryat_scores <- h %>% html_nodes("#final_jeopardy_round > table:nth-child(10)") %>% html_text()
    cumulative_scores <- h %>% html_nodes("#final_jeopardy_round > table:nth-child(6)") %>% html_text()
  }
  regex <- paste(c("^Show #\\d+"," show #\\d+"), collapse="|")
  Date <- h %>% 
    html_node("#game_title > h1") %>%
    html_text() %>%
    str_extract("[a-zA-Z]+, [a-zA-Z]+ \\d{1,2}, \\d+")
  scores <- unlist(str_extract_all(scores, scores_regex))[1:3]
  dj_scores <- unlist(str_extract_all(dj_scores, scores_regex))[1:3]
  jr_scores <- unlist(str_extract_all(jr_scores, scores_regex))[1:3]
  cb_scores <- unlist(str_extract_all(cb_scores, scores_regex))[1:3]
  coryat_scores <- unlist(str_extract_all(coryat_scores, scores_regex))[1:3]
  # scores <- scores[1:3]
  c(Date, cb_scores, jr_scores, dj_scores, scores, coryat_scores)
  if(exists("cumulative_scores") == TRUE){
    cumulative_scores <- unlist(str_extract_all(cumulative_scores, scores_regex))[1:3]
    return(c(Date, cb_scores, jr_scores, dj_scores, scores, coryat_scores, cumulative_scores))
  } else {
    return(c(Date, cb_scores, jr_scores, dj_scores, scores, coryat_scores))
  }
  # score_df <- data.frame(cont1_score = scores[1], cont2_score = scores[2], cont3_score = scores[3], stringsAsFactors = FALSE)
  # score_df
}

url_nums <- seq(7024)
all_scores <- vector("list", length(url_nums))

for (i in seq_along(url_nums)) {
  if (!(i %in% names(all_scores))) {
    cat(paste("Doing", i, "..."))
    ok <- FALSE
    counter <- 0
    while (ok == FALSE & counter <= 5) {
      counter <- counter + 1
      out <- tryCatch({                  
        get_scores(i)
      },
      error = function(e) {
        Sys.sleep(2)
        e
      }
      )
      if ("error" %in% class(out)) {
        cat(".")
      } else {
        ok <- TRUE
        cat(" Done.")
      }
    }
    cat("\n")
    all_scores[[i]] <- out
    names(all_scores)[i] <- i
  }
} 

compile_scores <- function(){
  vars <- c("Date", "cb_1", "cb_2", "cb_3", "jr_1", "jr_2", "jr_3", 
            "dj_1", "dj_2", "dj_3", "fin_1", "fin_2", "fin_3", "coryat_1", 
            "coryat_2", "coryat_3")
  for (var in vars){
    assign(var, vector("list", length(all_scores)))
  }
  #cb_1 <- vector("list", length(all_scores))
  #cb_2 <- vector("list", length(all_scores))
  #cb_3 <- vector("list", length(all_scores))
  #jr_1 <- vector("list", length(all_scores))
  #jr_2 <- vector("list", length(all_scores))
  #jr_3 <- vector("list", length(all_scores))
  #dj_1 <- vector("list", length(all_scores))
  #dj_2 <- vector("list", length(all_scores))
  #dj_3 <- vector("list", length(all_scores))
  #fin_1 <- vector("list", length(all_scores))
  #fin_2 <- vector("list", length(all_scores))
  #fin_3 <- vector("list", length(all_scores))
  #coryat_1 <- vector("list", length(all_scores))
  #coryat_2 <- vector("list", length(all_scores))
  #coryat_3 <- vector("list", length(all_scores))
  #cum_1 <- vector("list", length(all_scores))
  #cum_2 <- vector("list", length(all_scores))
  #cum_3 <- vector("list", length(all_scores))
  for (i in seq(length(url_nums))) {
    Date[[i]] <- unlist(all_scores[i])[1]
    cb_1[[i]] <- unlist(all_scores[i])[2]
    cb_2[[i]] <- unlist(all_scores[i])[3]
    cb_3[[i]] <- unlist(all_scores[i])[4]
    jr_1[[i]] <- unlist(all_scores[i])[5]
    jr_2[[i]] <- unlist(all_scores[i])[6]
    jr_3[[i]] <- unlist(all_scores[i])[7]
    dj_1[[i]] <- unlist(all_scores[i])[8]
    dj_2[[i]] <- unlist(all_scores[i])[9]
    dj_3[[i]] <- unlist(all_scores[i])[10]
    fin_1[[i]] <- unlist(all_scores[i])[11]
    fin_2[[i]] <- unlist(all_scores[i])[12]
    fin_3[[i]] <- unlist(all_scores[i])[13]
    coryat_1[[i]] <- unlist(all_scores[i])[14]
    coryat_2[[i]] <- unlist(all_scores[i])[15]
    coryat_3[[i]] <- unlist(all_scores[i])[16]
    # cum_1[[i]] <- unlist(all_scores[i])[17]
    # cum_2[[i]] <- unlist(all_scores[i])[18]
    # cum_3[[i]] <- unlist(all_scores[i])[19]
  }
  data.frame(unlist(Date), unlist(cb_1), unlist(cb_2), unlist(cb_3), 
             unlist(jr_1), unlist(jr_2), unlist(jr_3), unlist(dj_1), 
             unlist(dj_2), unlist(dj_3), unlist(fin_1), unlist(fin_2), 
             unlist(fin_3), unlist(coryat_1), unlist(coryat_2), unlist(coryat_3))
  #unlist(episode_num)
}


compile_scores2 <- function(){
  vars <- c("episode_num", "cb_1", "cb_2", "cb_3", "jr_1", "jr_2", "jr_3", 
            "dj_1", "dj_2", "dj_3", "fin_1", "fin_2", "fin_3", "coryat_1", 
            "coryat_2", "coryat_3")
  for (var in vars){
    assign(var, vector("list", length(all_scores)))
  }
  vars2 <- c(episode_num, cb_1, cb_2, cb_3, jr_1, jr_2, jr_3, 
             dj_1, dj_2, dj_3, fin_1, fin_2, fin_3, coryat_1, 
             coryat_2, coryat_3)
  for (i in seq(length(all_scores))) {
    for (var in vars2){
      cat(i)
      x <- 1
      var[[i]] <- unlist(all_scores[i])[x]
      x <- x + 1
    }
    data.frame(unlist(episode_num), unlist(cb_1), unlist(cb_2), unlist(cb_3), 
               unlist(jr_1), unlist(jr_2), unlist(jr_3), unlist(dj_1), 
               unlist(dj_2), unlist(dj_3), unlist(fin_1), unlist(fin_2), 
               unlist(fin_3), unlist(coryat_1), unlist(coryat_2), unlist(coryat_3))
    #unlist(episode_num)
  }
}



scores_df <- compile_scores()

colnames(scores_df) <- c("Date", "cb_1", "cb_2", "cb_3", "jr_1", "jr_2", 
                         "jr_3", "dj_1", "dj_2", "dj_3", "fin_1", "fin_2", 
                         "fin_3", "coryat_1", "coryat_2", "coryat_3")

# Convert from factors to characters
scores_df[] <- data.frame(lapply(scores_df, as.character), stringsAsFactors=FALSE)


clean_scores <- function(x){
  as.numeric(gsub("[\\$,]", "", x))
}

scores_df[2:16] <- lapply(scores_df[2:16], clean_scores)
# colnames(scores_df)[1] <- "Episode #"

scores_df$Date <- mdy(scores_df$Date)
full_df <- left_join(every_season_df,scores_df)
fullscores_no_tourn <- left_join(removed_tournaments,scores_df)


# episode_num <- unlist(all_scores[x])[1]

#  Create data frame of rows with NAs to inspect further:
problems_NAs <- fullscores_no_tourn[rowSums(is.na(fullscores_no_tourn)) > 0,]
# Most NA's were due to incomplete game data on J! Archive. Let's remove these rows:
fullscores_no_tourn <- fullscores_no_tourn[complete.cases(fullscores_no_tourn), ]

# Define function to return which contestant won:
winner <- function(x){
  fin_scores <- c(fullscores_no_tourn$fin_1[x],
                  fullscores_no_tourn$fin_2[x],
                  fullscores_no_tourn$fin_3[x])
  win <- ""
  if(which.max(fin_scores)==1 & fullscores_no_tourn$fin_1[x] != 0){
    win <- "Contestant 1"
  }
  if(which.max(fin_scores)==2){
    win <- "Contestant 2"
  }
  if(which.max(fin_scores)==3){
    win <- "Contestant 3"
  }
  if(fullscores_no_tourn$fin_1[x] == fullscores_no_tourn$fin_2[x] & 
     fullscores_no_tourn$fin_1[x] == fullscores_no_tourn$fin_3[x]){
    win <- "Tie"
  }
  if(fullscores_no_tourn$fin_1[x] < 1 &
     fullscores_no_tourn$fin_2[x] < 1 &
     fullscores_no_tourn$fin_3[x] < 1){
    win <- "Tie"
  }
  win
}

# Apply function to entire data frame to create vector of winners:    
sapply(seq(nrow(fullscores_no_tourn)), winner)
fullscores_no_tourn <- fullscores_no_tourn %>% mutate(Winner = sapply(seq(nrow(fullscores_no_tourn)), winner))

fullscores_no_tourn %>% filter(!Winner == "Tie") %>% ggplot() + geom_bar(aes(Winner))
which(sapply(seq(nrow(fullscores_no_tourn)), winner)=='')

game2 <- function(df){
  len <- seq(nrow(df))
  return_ind <- function(index){
    a <- df$`Contestant 1`[index]
    b <- df$`Contestant 2`[index-1]
    c <- df$`Contestant 3`[index-1]
    d <- df$`Episode #`[index]
    e <- df$`Episode #`[index - 1]
    f <- df$Notes[index]
    g <- df$`Contestant 2`[index-2]
    h <- df$`Contestant 3`[index-2]
    ifelse(index == 1, FALSE, ((d == e + 1) & (a == b | a == c) & (a != g) & (a != h)) | str_detect(f, " game 2.$"))
  }
  unlist(sapply(len, return_ind))
}

fullscores_no_tourn <- fullscores_no_tourn %>% mutate(game2 = game2(fullscores_no_tourn))
fullscores_no_tourn$game2[2] <- TRUE

# Contestant returned later in week due to mistake
fullscores_no_tourn$game2[4466] <- FALSE

game2df <- fullscores_no_tourn[fullscores_no_tourn$game2 == TRUE,]

sum(game2df$Winner == "Contestant 1")
nrow(game2df)

game2_prop_test <- prop.test(x = sum(game2df$Winner == "Contestant 1"), n = nrow(game2df), p = (1/3))
game2_prop_test
# CI is 42.1%-45.9%, p = 44%, p-value < 2.2e-16


game3 <- function(df){
  len <- seq(nrow(df))
  return_ind <- function(index){
    a <- df$`Contestant 1`[index]
    b <- df$`Contestant 2`[index-2]
    c <- df$`Contestant 3`[index-2]
    d <- df$`Episode #`[index]
    e <- df$`Episode #`[index - 1]
    f <- df$Notes[index]
    g <- df$`Episode #`[index - 2]
    h <- df$`Contestant 2`[index-3]
    i <- df$`Contestant 3`[index-3]
    # if episodes in order, cont 1 was 2 or 3 two episodes ago
    ifelse((index == 1 | index == 2 | index == 3), FALSE, 
           ((d == e + 1) & (d == g + 2) & (a == b | a == c) & (a != h) & (a != i)) 
           | str_detect(f, " game 3.$"))
  }
  unlist(sapply(len, return_ind))
}

fullscores_no_tourn <- fullscores_no_tourn %>% 
  mutate(game3 = game3(fullscores_no_tourn))

# Contestant returned later in week due to mistake
fullscores_no_tourn$game3[4466] <- TRUE
fullscores_no_tourn$game3[4467] <- FALSE

game3df <- fullscores_no_tourn[fullscores_no_tourn$game3 == TRUE,]

sum(game3df$Winner == "Contestant 1")
nrow(game3df)

game3_prop_test <- prop.test(x = sum(game3df$Winner == "Contestant 1"), n = nrow(game3df), p = (1/3))
game3_prop_test
# CI is 47.7%-53.7%, p = 50.7%, p-value < 2.2e-16


game4 <- function(df){
  len <- seq(nrow(df))
  return_ind <- function(index){
    a <- df$`Contestant 1`[index]
    b <- df$`Contestant 2`[index-3]
    c <- df$`Contestant 3`[index-3]
    d <- df$`Episode #`[index]
    e <- df$`Episode #`[index - 1]
    f <- df$Notes[index]
    g <- df$`Episode #`[index - 3]
    # if episodes in order, cont 1 was 2 or 3 two episodes ago
    ifelse((index == 1 | index == 2 | index == 3), FALSE, 
           ((d == g + 3) & (a == b | a == c)) 
           | str_detect(f, " game 4.$"))
  }
  unlist(sapply(len, return_ind))
}

fullscores_no_tourn <- fullscores_no_tourn %>% 
  mutate(game4 = game4(fullscores_no_tourn))

# Contestant returned later in week due to mistake
fullscores_no_tourn$game4[4466] <- FALSE
fullscores_no_tourn$game4[4467] <- TRUE

game4df <- fullscores_no_tourn[fullscores_no_tourn$game4 == TRUE,]

sum(game4df$Winner == "Contestant 1")
nrow(game4df)

game4_prop_test <- prop.test(x = sum(game4df$Winner == "Contestant 1"), n = nrow(game4df), p = (1/3))
game4_prop_test
# CI is 53.4%-61.8%, p = 57.7%, p-value < 2.2e-16



game5 <- function(df){
  len <- seq(nrow(df))
  return_ind <- function(index){
    a <- df$`Contestant 1`[index]
    b <- df$`Contestant 2`[index-4]
    c <- df$`Contestant 3`[index-4]
    d <- df$`Episode #`[index]
    e <- df$`Episode #`[index - 1]
    f <- df$Notes[index]
    g <- df$`Episode #`[index - 4]
    # if episodes in order, cont 1 was 2 or 3 two episodes ago
    ifelse((index == 1 | index == 2 | index == 3 | index == 4), FALSE, 
           ((d == g + 4) & (a == b | a == c)) 
           | str_detect(f, " game 5.$"))
  }
  unlist(sapply(len, return_ind))
}

fullscores_no_tourn <- fullscores_no_tourn %>% 
  mutate(game5 = game5(fullscores_no_tourn))

# Contestant returned later in week due to mistake
fullscores_no_tourn$game5[4467] <- FALSE

game5df <- fullscores_no_tourn[fullscores_no_tourn$game5 == TRUE,]

sum(game5df$Winner == "Contestant 1")
nrow(game5df)

game5_prop_test <- prop.test(x = sum(game5df$Winner == "Contestant 1"), n = nrow(game5df), p = (1/3))
game5_prop_test
# CI is 56.9%-67.8%, p = 62.5%, p-value < 2.2e-16

