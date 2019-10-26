library(dplyr)
library(plotly)
library(kableExtra)
#This script will try to predict qualification match rankings. 

setwd("C:/Users/Patrick/Documents/R-Scouting/")
df <- read.csv("DD 2019 Event Scouting.txt", header = FALSE)

df[df == "null"] = NA

colnames(df) <- c("Name", "Team", "Position", "Match", "Is_Start_Level_2",
                  "S_Hatch_CS", "S_Hatch_R1", "S_Hatch_R2", "S_Hatch_R3",
                  "S_Cargo_CS", "S_Cargo_R1", "S_Cargo_R2", "S_Cargo_R3",
                  "T_Hatch_CS", "T_Hatch_R1", "T_Hatch_R2", "T_Hatch_R3",
                  "Hatch_GP", "Hatch_SP",
                  "T_Cargo_CS", "T_Cargo_R1", "T_Cargo_R2", "T_Cargo_R3",
                  "Is_Defensive", "Habitat_Level", "Time_For_Climb", "Notes")

#Get unique teams
uniqueTeams <- unique(df$Team)

c1 <- c()
c2 <- c()
c3 <- c()
c4 <- c()
c5 <- c()
c6 <- c()
c7 <- c()
c8 <- c()
c9 <- c()
c10 <- c()
c11 <- c()
c12 <- c()
c13 <- c()
c14 <- c()
c15 <- c()
c16 <- c()
c17 <- c()
c18 <- c()
c19 <- c()
c20 <- c()
c21 <- c()
c22 <- c()
c23 <- c()
c24 <- c()
c25 <- c()
c26 <- c()
c27 <- c()
c28 <- c()
c29 <- c()
c30 <- c()
c31 <- c()
c32 <- c()
c33 <- c()
c34 <- c()
c35 <- c()
c36 <- c()
c37 <- c()
c38 <- c()
c39 <- c()
c40 <- c()
c41 <- c()
c42 <- c()
c43 <- c()
c44 <- c()
c45 <- c()
c46 <- c()
c47 <- c()
c48 <- c()

pointfunction <- function (v) {
  
  #Points Scale - Hab Level:
  # HL 0 - 0 points
  # HL 1 - 3 points
  # HL 2 - 6 points
  # HL 3 - 12 points
  
  c <- ifelse(v < 3, 3 * v, 12)
}
for (team in uniqueTeams) {
  a <- which(df$Team == team)
  
  #Sandstorm Vars
  
  #Start level is not really going to matter much for data collection only because
  #sandstorm points are only granted when a robot crosses the line.
  
  #b <- df$Is_Start_Level_2
  c <- df$S_Hatch_CS
  d <- df$S_Hatch_R1
  e <- df$S_Hatch_R2
  f <- df$S_Hatch_R3
  
  g <- df$S_Cargo_CS
  h <- df$S_Cargo_R1
  i <- df$S_Hatch_R2
  j <- df$S_Hatch_R3
  
  #Teleop Vars
  k <- df$T_Hatch_CS
  l <- df$T_Hatch_R1
  m <- df$T_Hatch_R2
  n <- df$T_Hatch_R3
  
  o <- df$T_Cargo_CS
  p <- df$T_Cargo_R1
  q <- df$T_Cargo_R2
  r <- df$T_Cargo_R3
  
  s <- df$Habitat_Level
  t <- df$Hatch_GP
  u <- df$Hatch_SP
  
  v <- df$Is_Defensive
  
  #Generally says seconds but can say something else. So that
  #should also be out of the question.
  
  #w <- df$Time_For_Climb
  
  #Means
  c1 <- c(c1, mean(c[a], na.rm = TRUE)) #S Hatch CS
  c2 <- c(c2, mean(d[a], na.rm = TRUE)) #S Hatch R1
  c3 <- c(c3, mean(e[a], na.rm = TRUE)) #S Hatch R2
  c4 <- c(c4, mean(f[a], na.rm = TRUE)) #S Hatch R3
  c5 <- c(c5, mean(g[a], na.rm = TRUE)) #S Cargo CS
  c6 <- c(c6, mean(h[a], na.rm = TRUE)) #S Cargo R1
  c7 <- c(c7, mean(i[a], na.rm = TRUE)) #S Cargo R2
  c8 <- c(c8, mean(j[a], na.rm = TRUE)) #S Cargo R3
  c9 <- c(c9, mean(k[a], na.rm = TRUE)) #T Hatch CS
  c10 <- c(c10, mean(l[a], na.rm = TRUE)) #T Hatch R1
  c11 <- c(c11, mean(m[a], na.rm = TRUE)) #T Hatch R2
  c12 <- c(c12, mean(n[a], na.rm = TRUE)) #T Hatch R3
  c13 <- c(c13, mean(o[a], na.rm = TRUE)) #T Cargo CS
  c14 <- c(c14, mean(p[a], na.rm = TRUE)) #T Cargo R1
  c15 <- c(c15, mean(q[a], na.rm = TRUE)) #T Cargo R2
  c16 <- c(c16, mean(r[a], na.rm = TRUE)) #T Cargo R3
  c17 <- c(c17, mean(s[a], na.rm = TRUE)) #Hab Level
  c18 <- c(c18, mean(pointfunction(s[a]), na.rm = TRUE)) #Mean Hab Level Points
  c19 <- c(c19, mean(c[a] + d[a] + e[a] + f[a] + k[a] + l[a] + m[a] + n[a], na.rm = TRUE)) #Total Hatches
  c20 <- c(c20, mean(g[a] + h[a] + i[a] + j[a] + o[a] + p[a] + q[a] + r[a], na.rm = TRUE)) #Total Cargo
  c21 <- c(c21, mean(t[a], na.rm = TRUE)) #Hatch GP
  c22 <- c(c22, mean(u[a], na.rm = TRUE)) #Hatch SP
  
  #Standard Deviations
  c23 <- c(c23, sd(c[a], na.rm = TRUE))
  c24 <- c(c24, sd(d[a], na.rm = TRUE))
  c25 <- c(c25, sd(e[a], na.rm = TRUE))
  c26 <- c(c26, sd(f[a], na.rm = TRUE))
  c27 <- c(c27, sd(g[a], na.rm = TRUE))
  c28 <- c(c28, sd(h[a], na.rm = TRUE))
  c29 <- c(c29, sd(i[a], na.rm = TRUE))
  c30 <- c(c30, sd(j[a], na.rm = TRUE))
  c31 <- c(c31, sd(k[a], na.rm = TRUE))
  c32 <- c(c32, sd(l[a], na.rm = TRUE))
  c33 <- c(c33, sd(m[a], na.rm = TRUE))
  c34 <- c(c34, sd(n[a], na.rm = TRUE))
  c35 <- c(c35, sd(o[a], na.rm = TRUE))
  c36 <- c(c36, sd(p[a], na.rm = TRUE))
  c37 <- c(c37, sd(q[a], na.rm = TRUE))
  c38 <- c(c38, sd(r[a], na.rm = TRUE))
  c39 <- c(c39, sd(s[a], na.rm = TRUE))
  c40 <- c(c40, sd(pointfunction(s[a]), na.rm = TRUE))
  c41 <- c(c41, sd(c[a] + d[a] + e[a] + f[a] + k[a] + l[a] + m[a] + n[a], na.rm = TRUE))
  c42 <- c(c42, sd(g[a] + h[a] + i[a] + j[a] + o[a] + p[a] + q[a] + r[a], na.rm = TRUE))
  c43 <- c(c43, sd(t[a], na.rm = TRUE))
  c44 <- c(c44, sd(u[a], na.rm = TRUE))
  
  #Maxima
  c45 <- c(c45, max(c[a] + d[a] + e[a] + f[a] + k[a] + l[a] + m[a] + n[a], na.rm = T))
  c46 <- c(c46, max(g[a] + h[a] + i[a] + j[a] + o[a] + p[a] + q[a] + r[a], na.rm = T))
  c47 <- c(c47, max(pointfunction(s[a])))
  #Booleans
  c48 <- c(c48, mean(v[a], na.rm = TRUE)) #Is Defensive (Aka "Defensive Rating")
}

df2 <- data.frame(Teams = uniqueTeams, #Identifier
                  
                  #Means
                  S_Hatch_CS_Mean = c1, S_Hatch_R1_Mean = c2, S_Hatch_R2_Mean = c3, S_Hatch_R3_Mean = c4,
                  S_Cargo_CS_Mean = c5, S_Cargo_R1_Mean = c6, S_Cargo_R2_Mean = c7, S_Cargo_R3_Mean = c8,
                  T_Hatch_CS_Mean = c9, T_Hatch_R1_Mean = c10, T_Hatch_R2_Mean = c11, T_Hatch_R3_Mean = c12,
                  T_Cargo_CS_Mean = c13, T_Cargo_R1_Mean = c14, T_Cargo_R2_Mean = c15, T_Cargo_R3_Mean = c16,
                  Habitat_Level_Mean = c17, Habitat_Points_Mean = c18, Hatch_Total_Mean = c19,
                  Cargo_Total_Mean = c20, Hatch_GP = c21, Hatch_SP = c22,
                  
                  #Standard Deviations
                  S_Hatch_CS_SD = c23, S_Hatch_R1_SD = c24, S_Hatch_R2_SD = c25, S_Hatch_R3_SD = c26,
                  S_Cargo_CS_SD = c27, S_Cargo_R1_SD = c28, S_Cargo_R2_SD = c29, S_Cargo_R3_SD = c30,
                  T_Hatch_CS_SD = c31, T_Hatch_R1_SD = c32, T_Hatch_R2_SD = c33, T_Hatch_R3_SD = c34,
                  T_Cargo_CS_SD = c35, T_Cargo_R1_SD = c36, T_Cargo_R2_SD = c37, T_Cargo_R3_SD = c38,
                  Habitat_Level_SD = c39, Habitat_Points_SD = c40, Hatch_Total_SD = c41, Cargo_Total_SD = c42,
                  Hatch_GP_SD = c43, Hatch_SP_SD = c44,
                  
                  #Maxima
                  Hatch_Total_Max = c45, Cargo_Total_Max = c46, Habitat_Points_Max = c47,
                  #Boolean Means
                  Is_Defensive = c48)

df2 <- df2[with(df2, order(Teams)), ]

setwd("html_files/quals")

#Qualification Matches - Rank

df_quals <- read.csv("quals.txt", header = F)
colnames(df_quals) <- c('R1','R2','R3','B1','B2','B3')


defensiveRating <- function(alliance) {
  max(df2$Is_Defensive[df2$Teams %in% alliance])
}

mvpIndex <- function(alliance) {
  max(df2$Cargo_Total_Mean[df2$Teams %in% alliance] * 3 +
        df2$Hatch_Total_Mean[df2$Teams %in% alliance] * 2 +
        df2$Habitat_Points_Mean[df2$Teams %in% alliance])
}

probability_of_winning <- function(allianceA, allianceB) {
  
  df_ratings <- data.frame(Teams = df2$Teams,
                     Offensive_Rating = df2$Cargo_Total_Mean * 3 +
                       df2$Hatch_Total_Mean * 2 + df2$Habitat_Points_Mean,
                     Offensive_Rating_SD =
                       sqrt(9 * (df2$Cargo_Total_SD ** 2) +
                              4 * (df2$Hatch_Total_SD ** 2) +
                              (df2$Habitat_Points_SD ** 2)))
  
  #In other words, this algorithm will try to find the most offensive robot's point total
  allianceAMaxOffensiveRating <- mvpIndex(allianceA)
  allianceBMaxOffensiveRating <- mvpIndex(allianceB)
  
  # There can only be one defensive bot; otherwise, the alliance breaks the rules.
  # Each team does a HAB Climb 10 seconds before the end of the game. Since there are
  # 135 seconds in teleop, an ideal defensive bot should spend around 105-120 seconds
  # defending the other team (15-30 seconds, averaged to 22.5 seconds, for a round trip
  # of the field at around 5 ft/s), which is equal to 5/6 of the total.
  
  allianceADefensiveRating <- max(defensiveRating(allianceA) - defensiveRating(allianceB), 0) * 5/6
  
  allianceBDefensiveRating <- max(defensiveRating(allianceB) - defensiveRating(allianceA), 0) * 5/6
  
  #Assuming the population variances are not equal:
  
  allianceAPointLoss <- allianceAMaxOffensiveRating * allianceBDefensiveRating
  allianceBPointLoss <- allianceBMaxOffensiveRating * allianceADefensiveRating
  
  predictedAllianceANetGain <- sum(df_ratings$Offensive_Rating[df_ratings$Teams %in% allianceA]) - allianceAPointLoss
  predictedAllianceBNetGain <- sum(df_ratings$Offensive_Rating[df_ratings$Teams %in% allianceB]) - allianceBPointLoss
  
  predictedAllianceAStdErr <- sqrt(sum(df_ratings$Offensive_Rating_SD[df_ratings$Teams %in% allianceA] ** 2))
  predictedAllianceBStdErr <- sqrt(sum(df_ratings$Offensive_Rating_SD[df_ratings$Teams %in% allianceB] ** 2))
  
  #The core of all the probabilities - Meet the Ultimate Trial and Error (Standard Deviation +/- 0.05%)
  normDistA <- rnorm(1000000, mean = predictedAllianceANetGain, sd = predictedAllianceAStdErr)
  normDistB <- rnorm(1000000, mean = predictedAllianceBNetGain, sd = predictedAllianceBStdErr)
  probability <- sum(normDistA - normDistB > 0.5) / 1000000
  
  probability
}



probability_of_climb_point <- function(alliance) {
  
  df_climb_ratings <- data.frame(Teams = df2$Teams, Mean = df2$Habitat_Points_Mean,
                                 SD = df2$Habitat_Points_SD)
  
  predictedAllianceMean <- sum(df_climb_ratings$Mean[df_climb_ratings$Teams %in% alliance])
  predictedAllianceStdErr <- sqrt(sum(df_climb_ratings$SD[df_climb_ratings$Teams %in% alliance] ** 2))
  probability <- pnorm(predictedAllianceMean, mean = 15, sd = predictedAllianceStdErr)
  
  probability
}

probability_of_rocket_point <- function(alliance) {
  #This is most likely an overestimate.
  
  #Probability of all hatches placed * Probability of all cargos placed = Probability of rocket.
  
  df_rocket_ratings <- data.frame(Teams = df2$Teams,
                                  Mean_Hatches = df2$Cargo_Total_Mean,
                                  Mean_Cargo = df2$Cargo_Total_Mean,
                                  SD_Hatches = df2$Hatch_Total_SD,
                                  SD_Cargo = df2$Cargo_Total_SD)
  
  
  robot_rocket_probability <- 1
  
  for (index in 2:3) {
    robot_index <- which(df_rocket_ratings$Teams %in% alliance)
    normDist <- rnorm(1000000, mean = sum(df_rocket_ratings[robot_index, index]),
                      sd = sum(df_rocket_ratings[robot_index, index + 2] ** 2))
    robot_rocket_probability <- robot_rocket_probability * length(normDist[normDist > 6]) / 1000000
  }
  
  robot_rocket_probability
}


#Common Fractions Less than 1
dec_to_frac <- function (dec) {
  if (dec < 0.001) {
    return("< 1 in 1000")
  } else if (dec > 0.999) {
    return("> 999 in 1000")
  } else if (dec < 0.1) {
    return(paste("1 in", round(1/dec)))
  } else if (dec > 0.9) {
    r <- round(1/(1 - dec))
    return(paste(r - 1, "in", r))
  } else {
    a <- c()
    for(i in 1:20) {
      
      #Calculates the errors in the fractions with denominators from 1 to 20.
      a <- c(a, abs(i * dec - round(i * dec)))
    }
    
    #Determine which is the minimum (works because R is 1-based.)
    b <- which.min(a)
    
    #Then, determine which fraction to use
    c <- round(dec * b)
    d <- b
    return(paste(c, "in", d))
  }
}

dec_to_frac_of_vector <- function (v1) {
  v2 = c()
  for (dec in v1) {
    v2 = c(v2, dec_to_frac(dec))
  }
  v2
}
num_quals <- nrow(df_quals)

df_quals_probability <- data.frame(Red_Rate = numeric(num_quals),
                                       Blue_Rate = numeric(num_quals),
                                       Tie_Rate = numeric(num_quals),
                                       Red_Climb_Rate = numeric(num_quals),
                                       Blue_Climb_Rate = numeric(num_quals),
                                       Red_Rocket_Rate = numeric(num_quals),
                                       Blue_Rocket_Rate = numeric(num_quals))

df_current_ranks <- read.csv("current_ranks.txt")

df_ratings <- data.frame(Teams = df2$Teams, Ranking_Points_Avg = df_current_ranks[,2], Ranking_Points_SD = 0)

for (index in 1:num_quals) {
  red <- c(df_quals[index, 1], df_quals[index, 2], df_quals[index, 3])
  blue <- c(df_quals[index, 4], df_quals[index, 5], df_quals[index, 6])
  red_rate <- probability_of_winning(red, blue)
  blue_rate <- probability_of_winning(blue, red)
  red_rocket_rate <- probability_of_rocket_point(red)
  blue_rocket_rate <- probability_of_rocket_point(blue)
  tie_rate <- 1 - (red_rate + blue_rate)
  red_climb_rate <- probability_of_climb_point(red)
  blue_climb_rate <- probability_of_climb_point(blue)
  df_quals_probability$Red_Rate[index] <- red_rate
  df_quals_probability$Blue_Rate[index] <- blue_rate
  df_quals_probability$Tie_Rate[index] <- tie_rate
  df_quals_probability$Red_Climb_Rate[index] <- red_climb_rate
  df_quals_probability$Blue_Climb_Rate[index] <- blue_climb_rate
  df_quals_probability$Red_Rocket_Rate[index] <- red_rocket_rate
  df_quals_probability$Blue_Rocket_Rate[index] <- blue_rocket_rate
  
  df_ratings[which(df2$Teams == red[1]), 2] <- df_ratings[which(df2$Teams == red[1]), 2] + red_rate * 2 + tie_rate + red_climb_rate + red_rocket_rate
  df_ratings[which(df2$Teams == red[2]), 2] <- df_ratings[which(df2$Teams == red[2]), 2] + red_rate * 2 + tie_rate + red_climb_rate + red_rocket_rate
  df_ratings[which(df2$Teams == red[3]), 2] <- df_ratings[which(df2$Teams == red[3]), 2] + red_rate * 2 + tie_rate + red_climb_rate + red_rocket_rate
  
  df_ratings[which(df2$Teams == blue[1]), 2] <- df_ratings[which(df2$Teams == blue[1]), 2] + blue_rate * 2 + tie_rate + blue_climb_rate + blue_rocket_rate
  df_ratings[which(df2$Teams == blue[2]), 2] <- df_ratings[which(df2$Teams == blue[2]), 2] + blue_rate * 2 + tie_rate + blue_climb_rate + blue_rocket_rate
  df_ratings[which(df2$Teams == blue[3]), 2] <- df_ratings[which(df2$Teams == blue[3]), 2] + blue_rate * 2 + tie_rate + blue_climb_rate + blue_rocket_rate
  
  df_ratings[which(df2$Teams == red[1]), 3] <- df_ratings[which(df2$Teams == red[1]), 3] + red_rate * 4 + tie_rate + red_climb_rate + red_rocket_rate - (red_rate * 2 + tie_rate) ** 2 - red_climb_rate ** 2 - red_rocket_rate ** 2
  df_ratings[which(df2$Teams == red[2]), 3] <- df_ratings[which(df2$Teams == red[2]), 3] + red_rate * 4 + tie_rate + red_climb_rate + red_rocket_rate - (red_rate * 2 + tie_rate) ** 2 - red_climb_rate ** 2 - red_rocket_rate ** 2
  df_ratings[which(df2$Teams == red[3]), 3] <- df_ratings[which(df2$Teams == red[3]), 3] + red_rate * 4 + tie_rate + red_climb_rate + red_rocket_rate - (red_rate * 2 + tie_rate) ** 2 - red_climb_rate ** 2 - red_rocket_rate ** 2
  
  df_ratings[which(df2$Teams == blue[1]), 3] <- df_ratings[which(df2$Teams == blue[1]), 3] + blue_rate * 4 + tie_rate + blue_climb_rate + blue_rocket_rate - (blue_rate * 2 + tie_rate) ** 2 - blue_climb_rate ** 2 - blue_rocket_rate ** 2
  df_ratings[which(df2$Teams == blue[2]), 3] <- df_ratings[which(df2$Teams == blue[2]), 3] + blue_rate * 4 + tie_rate + blue_climb_rate + blue_rocket_rate - (blue_rate * 2 + tie_rate) ** 2 - blue_climb_rate ** 2 - blue_rocket_rate ** 2
  df_ratings[which(df2$Teams == blue[3]), 3] <- df_ratings[which(df2$Teams == blue[3]), 3] + blue_rate * 4 + tie_rate + blue_climb_rate + blue_rocket_rate - (blue_rate * 2 + tie_rate) ** 2 - blue_climb_rate ** 2 - blue_rocket_rate ** 2
  
}
df_ratings$Ranking_Points_SD <- sqrt(df_ratings$Ranking_Points_SD)
#Calculate Average Ranking Points

newData <- df_ratings[with(df_ratings, order(-Ranking_Points_Avg)),]

#Ranks are based on percentile.
mean <- mean(df_ratings$Ranking_Points_Avg, na.rm = T)

sd <- sd(df_ratings$Ranking_Points_Avg, na.rm = T)
newData$Z_Score <- (newData$Ranking_Points_Avg - mean) / sd

#Conversion of Rank to z-score - Min z-score is equal to the number of teams in ranking, Max z-score is equal to 0

newData$Average_Percentile <- 100 * pnorm(newData$Z_Score)
newData$Average_Rank <- 1 + (100 - newData$Average_Percentile) * (length(df2$Teams) - 1) / 100

position <- function(i) {
  if (i %% 10 == 1 && i %% 100 != 11) {
    return(paste(i, "st", sep = ""))
  } else if (i %% 10 == 2 && i %% 100 != 12) {
    return(paste(i, "nd", sep = ""))
  } else if (i %% 10 == 3 && i %% 100 != 13) {
    return(paste(i, "rd", sep = ""))
  }
  paste(i, "th", sep = "")
}

for (i in (length(df2$Teams)-1):1) {
  newData[position(i)] <- 0
}

#Rank Distributions - 250000 (that's one quarter of a million, folks!) simulations per Team

for (i in 1:250000) {
  list <- rnorm(newData$Ranking_Points_Avg, mean = newData$Ranking_Points_Avg, sd = newData$Ranking_Points_SD)

  #Less than zero is not possible, so we rounded up those negative numbers to zero.
  list[list < 0] <- 0
  for (i in 7:(5+length(df2$Teams))) {
    toplist <- list
    toplist = (rank(-list) <= length(df2$Teams) + 6 - i)
    newData[[i]] <- newData[[i]] + toplist / 250000
  }
}
#Add ranking-point averages.

medians <- c()
#Histograms - Probability Distributions
for (i in 1:(length(df2$Teams))) {
  x <- 1:(length(df2$Teams) - 1)
  y <- as.numeric(newData[i, (5+length(df2$Teams)):7])
  z <- c(0, y)
  dist <- c()
  for (j in 1:(length(z) - 1)) {
    dist <- c(dist, z[j + 1] - z[j])
  }
  
  plot <- plot_ly(x = x, y = dist, type = 'bar', hoverinfo = 'text', name = 'Probability',
                  text = paste("Rank ", x, ": ", dec_to_frac_of_vector(dist), " (", floor(10000 * dist) / 100, "%)", sep = '')) %>%
    add_trace(x = x, y = y, type = 'scatter', mode = 'lines', hoverinfo = 'text', name = 'Cumulative Probability',
              text = paste("Rank ", x, ifelse(x > 1," or Higher: ", ": "), dec_to_frac_of_vector(y), " (", floor(10000 * y) / 100, "%)", sep = '')) %>%
    layout(title = paste("Rank Probabilities of", newData$Teams[i]), xaxis = list(title = "Rank"),
           yaxis = list(title = "Probability", tickformat = ".2%"))
  htmlwidgets::saveWidget(plot, paste("probability_", newData$Teams[i], ".html", sep = ''))
  
  #Median rank
  medians <- c(medians, length(df2$Teams) - length(y[y > 0.5]))
}
newData$Median_Rank <- medians

scoutingData <- data.frame("Teams" = newData$Teams, "Mean_RP" = newData$Ranking_Points_Avg, "Standard_Deviation_of_RP" = newData$Ranking_Points_SD, "Mean_Rank" = newData$Average_Rank, "Median_Rank" = newData$Median_Rank)
kable(scoutingData, col.names = c("Team", "Average Ranking Points", "Standard Deviation", "Average Rank", "Median Rank")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  save_kable("Ranking Data.html", self_contained = T)

