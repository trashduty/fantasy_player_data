library(dplyr)
library(lubridate)
library(nflreadr)
library(janitor)
library(readr)
library(purrr)

nfl_data <- load_pbp()

player_data <- load_players()

nfl <- load_pbp()

players <- load_players()


# Quarterbacks (QB)
# 
# Basic Passing Metrics
# 
# player_id: Unique identifier for the player.
# player_name: Name of the player.
# team: The team the player belongs to.
# attempts: Number of passing attempts.
# completions: Number of completed passes.
# passing_yards: Total passing yards.
# passing_tds: Total passing touchdowns.
# interceptions: Number of interceptions thrown.
# sacks: Number of times sacked.
# sack_yards: Total yards lost due to sacks.
# completion_percentage: Percentage of completed passes (completions/attempts).
# yards_per_attempt: Average yards gained per passing attempt.
# yards_per_completion: Average yards gained per completed pass.
# passer_rating: Traditional passer rating.
# air_yards: Total yards the ball traveled in the air for completed passes.
# adot: Average depth of target (yards downfield the pass was intended).
# pass_attempt_distance: Distance of pass attempts.
# air_yards_per_attempt: Average air yards per passing attempt.
# Advanced Passing Metrics
# 
# passing_epa: Expected Points Added on passing plays.
# passing_wpa: Win Probability Added on passing plays.
# cpoe: Completion Percentage Over Expected, which adjusts for the difficulty of passes.
# passer_rating_qb_3d: Passer rating in third down situations.
# success_rate: Percentage of successful passing plays (plays that gain positive EPA).
# expected_completion_percentage: Expected completion percentage based on pass difficulty.
# pass_attempts_inside_ten: Number of pass attempts inside the opponent's 10-yard line.
# passing_first_downs: Number of first downs achieved via passing.
#   

# Something is slightly off with the penalty plays
  
  passer_rating_calc <- function(attempts,completions,yards,tds,ints) {
    
    a <- (completions / attempts - 0.3) * 5
    b <- (yards / attempts - 3) * 0.25
    c <- tds / attempts * 20
    d <- 2.35 - (ints / attempts * 25)
    
    return(
      ((a+b+c+d)/6)*100
    )
  }
  
  qbs <- players |> 
    filter(position == "QB")
  
  qb_data <- nfl %>%
    filter(passer_id %in% na.omit(qbs$gsis_id),
           season_type == "REG") %>%
    group_by(passer_id,passer,posteam) %>%
    summarise(attempts = pick(pass_attempt,sack) %>% filter(sack != 1) %>% 
                pull(pass_attempt) %>%sum(.,na.rm = T),
              completions = sum(complete_pass,na.rm = T),
              yards = pick(yards_gained,complete_pass) %>% filter(complete_pass == 1) %>%
                pull(yards_gained) %>% sum(.,na.rm = T),
              tds = sum(pass_touchdown,na.rm =T),
              ints = sum(interception,na.rm = T),
              sacks = sum(sack,na.rm = T),
              sack_yards = pick(yards_gained,sack) %>% filter(sack == 1) %>%
                pull(yards_gained) %>% sum(.,na.rm = T),
              completion_percentage = completions / attempts,
              yards_per_attempt = yards / attempts,
              yards_per_completion = yards / completions,
              passer_rating = passer_rating_calc(attempts,completions,yards,tds,ints),
              air_yards = sum(air_yards, na.rm = T),
              adot = air_yards / attempts,
              epa = sum(epa, na.rm = T),
              wpa = sum(wpa, na.rm = T),
              cpoe = mean(cpoe,na.rm = T),
              passer_rating_3d = pick(down,sack,pass_attempt,complete_pass,yards_gained,pass_touchdown,interception) %>%
                filter(down == 3, sack != 1, pass_attempt == 1) %>% 
                summarise(across(c(pass_attempt:interception),sum),
                          pr = passer_rating_calc(pass_attempt,complete_pass,yards_gained,pass_touchdown,interception)) %>%
                pull(pr),
              success_rate = mean(success,na.rm = T),
              expected_completion_percentage = mean(cp,na.rm = T),
              pass_attempts_inside_10 = pick(yardline_100,pass_attempt) %>%
                filter(yardline_100 <= 10) %>% pull(pass_attempt) %>% sum(.,na.rm = T),
              passing_first_downs = sum(first_down_pass,na.rm = T)
    ) |> 
    ungroup() |> 
    rename(player_id = passer_id, player = passer, team = posteam) |> 
    mutate(name_team = paste0(player, " (", team, ")"))
  
  # Wide Receivers (WR)
  # 
  # Basic Receiving Metrics:
  #   
  #   player_id: Unique identifier for the player.
  # player_name: Name of the player.
  # team: The team the player belongs to.
  # targets: Number of times the player was targeted.
  # receptions: Number of catches made by the player.
  # receiving_yards: Total receiving yards.
  # receiving_tds: Total receiving touchdowns.
  # yards_per_reception: Average yards gained per reception.
  # yards_after_catch: Yards gained after the catch.
  # air_yards: Total yards the ball traveled in the air towards the receiver.
  # adot: Average depth of target.
  # catch_rate: Receptions per target (receptions/targets).
  # Advanced Receiving Metrics:
  #   
  #   yac_per_reception: Yards after catch per reception.
  # receiving_epa: Expected Points Added on receptions.
  # receiving_wpa: Win Probability Added on receptions.
  # receiving_success_rate: Percentage of successful receptions.
  # receiving_epa_per_target: Expected Points Added per target.
  # wopr: Weighted Opportunity Rating, combining target share and air yard share.
  
  wrs <- players |> 
    filter(position %in% c("WR", "TE"))
    
    wr_data <- nfl %>%
      filter(season_type == "REG") %>%
      group_by(posteam) %>%
      mutate(team_targets = pick(pass_attempt,sack) %>% filter(sack != 1) %>% 
               pull(pass_attempt) %>%sum(.,na.rm = T),
             team_air_yards = sum(air_yards,na.rm = T)) %>%
      ungroup() %>%
      filter(receiver_id %in% na.omit(wrs$gsis_id)) %>%
      group_by(receiver_id,receiver,posteam) %>%
      summarise(games = length(unique(game_id)),
                targets = n(),
                receptions = complete_pass[complete_pass == 1] %>% length(),
                yards = sum(yards_gained,na.rm = T),
                tds = sum(touchdown, na.rm = T),
                yards_per_reception = yards / receptions,
                yac = sum(yards_after_catch, na.rm = T),
                air_yards = sum(air_yards,na.rm = T),
                adot = air_yards / targets,
                catch_rate = receptions / targets,
                yac_per_reception = yac / receptions,
                epa = sum(epa,na.rm = T),
                wpa = sum(wpa, na.rm = T),
                success_rate = mean(success,na.rm = T),
                epa_per_target = epa / targets,
                target_share = targets / mean(team_targets),
                air_yards_share = air_yards / mean(team_air_yards),
                wopr = (target_share*1.5 + air_yards_share*0.7)
      ) %>%
      ungroup() %>%
      left_join(wrs %>% select(gsis_id,position_group), by = c("receiver_id" = "gsis_id")) %>%
      relocate(position_group, .after = receiver) |> 
      rename(player_id = receiver_id, player = receiver, team = posteam) |> 
      mutate(name_team = paste0(player, " (", team, ")"))

    
    
    # Running Backs (RB)
    # 
    # Basic Rushing Metrics:
    #   
    #   player_id: Unique identifier for the player.
    # player_name: Name of the player.
    # team: The team the player belongs to.
    # carries: Number of rushing attempts.
    # rushing_yards: Total rushing yards.
    # rushing_tds: Total rushing touchdowns.
    # yards_per_carry: Average yards gained per carry.
    # Advanced Rushing Metrics:
    #   
    #   rushing_epa: Expected Points Added on rushing plays.
    # rushing_wpa: Win Probability Added on rushing plays.
    # rushing_success_rate: Percentage of successful rushes.
    # rushing_first_downs: Number of first downs achieved by rushing.
    # Receiving Metrics for RBs:
    #   
    #   targets: Number of times the player was targeted.
    # receptions: Number of catches made by the player.
    # receiving_yards: Total receiving yards.
    # receiving_tds: Total receiving touchdowns.
    # yards_per_reception: Average yards gained per reception.
    # yards_after_catch: Yards gained after the catch.
    # catch_rate: Receptions per target (receptions/targets).
    # General Metrics
    # 
    # games_played: Number of games played by the player.
    # snap_count: Number of snaps played.
    # snap_share: Percentage of team snaps played.
    # routes_run: Number of routes run by the player.
    
    rbs <- players |> 
      filter(position == "RB")
      
      rb_run <- nfl %>%
        filter(rusher_id %in% na.omit(rbs$gsis_id),
               season_type == "REG") %>%
        group_by(rusher_id,rusher,posteam) %>%
        summarise(games = length(unique(game_id)),
                  carries = n(),
                  rush_yards = sum(yards_gained,na.rm = T),
                  rush_tds = sum(touchdown,na.rm = T),
                  yards_per_carry = rush_yards / carries,
                  epa = sum(epa,na.rm = T),
                  wpa = sum(wpa, na.rm = T),
                  success_rate = mean(success,na.rm = T),
                  first_downs = sum(first_down_rush,na.rm = T)
        ) %>%
        ungroup()
      
      rb_rec <- nfl %>%
        filter(receiver_id %in% na.omit(rbs$gsis_id),
               season_type == "REG") %>%
        group_by(receiver_id,receiver,posteam) %>%
        summarise(targets = n(),
                  receptions = complete_pass[complete_pass == 1] %>% length(),
                  rec_yards = sum(yards_gained,na.rm = T),
                  rec_tds = sum(touchdown, na.rm = T),
                  yards_per_reception = rec_yards / receptions,
                  yac = sum(yards_after_catch, na.rm = T),
                  catch_rate = receptions / targets
        ) %>%
        ungroup()
      
      rb_data <- rb_run %>%
        left_join(rb_rec, by = c("rusher_id" = "receiver_id",
                                 "rusher" = "receiver",
                                 "posteam" = "posteam")) %>%
        rename(player_id = rusher_id, player = rusher, team = posteam) |> 
        mutate(name_team = paste0(player, " (", team, ")"))

      
    
    write_csv(qb_data, "Data/qb_data.csv")
    write_csv(wr_data, "Data/wr_data.csv")
    write_csv(rb_data, "Data/rb_data.csv")
    
    
  
    
