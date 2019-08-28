library(dplyr)
library(plotly)

# Build an R script that can do the following:
# 
# 1) Read values from a .txt file every 30 seconds (i.e. Conversion from .txt to Data Frame).
# 
# 2) Separate into Teams for Mean and Standard Deviation.
# 
# 3) Interpret these values as Graphs for each team.
# 
# 4) If possible, integrate the script with Tableau (Not possible due to the illegality
# of connections in the FRC events).
# 
# 
# Note: R1, R2, R3 mean the respective levels of the rocket ship, and CS means cargo ship.
# S means Sandstorm and T means Teleop.
# 
# SP means Station Pickup and GP means Ground Pickup

#The path is necessary for the R script to read the file. To access the file, use that command:
# setwd("Your file here")

df <- read.csv("Bensalem Event 2019 Scouting.txt", header = FALSE)

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
  
  #Generally says seconds but can say something else in extreme cases. So that
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
  #Booleans
  c45 <- c(c45, mean(v[a], na.rm = TRUE)) #Is Defensive (Aka "Defensive Rating")
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
                  
                  #Boolean Means
                  Is_Defensive = c45)

df2 <- df2[with(df2, order(Teams)), ]

# All of the sorting operations commented below are descending.
# To order by Cargo ascending, for instance, use the following code:
# df2 <- df2[with(df2, order(Cargo)), ]

# To order by Defensiveness, use the following code:
# df2 <- df2[with(df2, order(-Is_Defensive)), ]

# To order by Hatch Panels, use the following code:
# df2 <- df2[with(df2, order(-Hatch_Total_Mean)), ]

# To order by Cargo, descending, use the following code:
# df2 <- df2[with(df2, order(-Cargo_Total_Mean)), ]

# To order by Hab Level, descending, use the following code:
# df2 <- df2[with(df2, order(-Habitat_Level_Mean)), ]

# To order by Hab Points, descending, use the following code:
# df2 <- df2[with(df2, order(-Habitat_Points_Mean)), ]

df3 <- data.frame(Cargos = df2$Cargo_Total_Mean, Hatches = df2$Hatch_Total_Mean)

png("frc_teams.png", width = 1500, height = 900)
plot(df3, main="FRC Teams", col = df2$Teams %>% length() %>% rainbow() %>% palette())
text(df3$Cargos, df3$Hatches + 0.1, labels = df2$Teams, col = df2$Teams %>% length() %>% rainbow() %>% palette())
dev.off()

df4 <- data.frame(Offensive_Rating = (2 * df2$Hatch_Total_Mean + 3 * df2$Cargo_Total_Mean + df2$Habitat_Points_Mean),
                  Defensive_Rating = df2$Is_Defensive)

colors <- df2$Teams %>% length() %>% rainbow() %>% palette()
total_rating <- df4$Defensive_Rating + df4$Offensive_Rating * max(df4$Defensive_Rating) / max(df4$Offensive_Rating)

plot1 <- plot_ly(data = df4, x = ~Offensive_Rating, y = ~Defensive_Rating, 
             colors = ~colors, text = ~paste("Team: ", df2$Teams),
             marker = list(size = 15 + 15 * df2$Teams / max(df2$Teams), line = list(color = 'black', width = 2), color =
                             hsv(v = 1, s = 1, h = (rank(total_rating, ties.method = "random") - 1) / 30))) %>% layout(title = "Robot Offensive-Defensive Scale",
                                                                             xaxis = list(title = "Offensive Rating (Total Points without Autonomous)"),
                                                                             yaxis = list(title = "Defensiveness", tickformat = "%")) %>% hide_colorbar()
# If you see any warnings, ignore them--the program should display a plot by default.
# Do not save this plot as an image because that will make it not interactive.

# For some reason, this script below when executed via Java does not do anything,
# so it has to be saved via htmlwidgets (which does not require internet to access).

# print(plot1)

# You might need the latest version of pandoc for this command to work.
# Here is the link to the website to download pandoc: https://pandoc.org/installing.html
htmlwidgets::saveWidget(as_widget(plot1), "frc_ratings.html")

# # Errorbars without Plotly
# df5 <- data.frame(Cargos = df2$Cargo_Total_Mean, Hatches = df2$Hatch_Total_Mean,
#                   Cargos.StdErr = df2$Cargo_Total_SD, Hatches.StdErr = df2$Hatch_Total_SD)
#
# #However, you can save this plot as an image.
# plot(df5$Hatches, df5$Cargos, xlab = "Mean Hatches +/- 0.1 SD", ylab = "Mean Cargos +/- 0.1 SD")
# text(df5$Hatches, df5$Cargos - 0.05, labels = df2$Teams, col = df2$Teams %>% length() %>% rainbow() %>% palette())
# arrows(df5$Hatches, df5$Cargos + df5$Cargos.StdErr * .1, df5$Hatches, df5$Cargos - df5$Cargos.StdErr * .1, length = 0.05, angle = 90, code = 3, col = "red")
# arrows(df5$Hatches + df5$Hatches.StdErr * .1, df5$Cargos, df5$Hatches - df5$Hatches.StdErr * .1, df5$Cargos, length = 0.05, angle = 90, code = 3, col = "blue")

# Checks # of distribution files.
if (!(sum(file.exists(paste("distribution", df2$Teams,".html", sep = ""))) == length(df2$Teams))) {
  for (index in 1:nrow(df2)) {
    # Pie charts for distribution of cargo/hatch panels are available as .html files and they do not need access
    # to the internet to be displayed.
    df6 <- data.frame(Teams = df2$Teams, Offensive_Rating = 2 * df2$Hatch_Total_Mean +
                        3 * df2$Cargo_Total_Mean + df2$Habitat_Points_Mean,
                      S_Cargo_CS_Mean = c5, S_Cargo_R1_Mean = c6, S_Cargo_R2_Mean = c7, S_Cargo_R3_Mean = c8,
                      T_Cargo_CS_Mean = c13, T_Cargo_R1_Mean = c14, T_Cargo_R2_Mean = c15, T_Cargo_R3_Mean = c16)
    df6 <- df6[index, ]
    df6 <- as.data.frame(t(df6))
    colnames(df6) <- c("Values")
  
    df6$Categories <- c("Team", "Offensive Rating", "Sandstorm Hatches in Cargo Ship",
                        "Sandstorm Hatches in Rocket L1", "Sandstorm Hatches in Rocket L2",
                        "Sandstorm Hatches in Rocket L3", "Teleop Hatches in Cargo Ship",
                        "Teleop Hatches in Rocket L1", "Teleop Hatches in Rocket L2",
                        "Teleop Hatches in Rocket L3")
  
    team <- df6[1, 1]
    df6 <- df6[-c(1, 2), ]
  
    #Hatches
    df7 <- data.frame(Teams = df2$Teams, Offensive_Rating = 2 * df2$Hatch_Total_Mean +
                        3 * df2$Cargo_Total_Mean + df2$Habitat_Points_Mean,
                      S_Hatch_CS_Mean = c1, S_Hatch_R1_Mean = c2, S_Hatch_R2_Mean = c3, S_Hatch_R3_Mean = c4,
                      T_Hatch_CS_Mean = c9, T_Hatch_R1_Mean = c10, T_Hatch_R2_Mean = c11, T_Hatch_R3_Mean = c12)
    df7 <- as.data.frame(t(df7[index, ]))
    colnames(df7) <- c("Values")
    df7$Categories <- c("Team", "Offensive Rating", "Sandstorm Cargo in Cargo Ship",
                        "Sandstorm Cargo in Rocket L1", "Sandstorm Cargo in Rocket L2",
                        "Sandstorm Cargo in Rocket L3", "Teleop Cargo in Cargo Ship",
                        "Teleop Cargo in Rocket L1", "Teleop Cargo in Rocket L2",
                        "Teleop Cargo in Rocket L3")
    df7 <- df7[-c(1, 2), ]
  
    plot2 <- plot_ly() %>%
      add_pie(data = df6, labels = ~Categories, values = ~Values,
              name = "", domain = list(x = c(0, 0.5), y = c(0, 0.5))) %>%
      add_pie(data = df7, labels = ~Categories, values = ~Values,
              name = "", domain = list(x = c(0.5, 1), y = c(0, 0.5))) %>%
      layout(title = paste("Distribution of Hatches and Cargoes of", team), showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    htmlwidgets::saveWidget(as_widget(plot2), paste("distribution", team, ".html", sep = ""))
  }
}

# Next: Error Bars in Plotly Bar Charts
df8 <- data.frame(Teams = df2$Teams, Cargos = df2$Cargo_Total_Mean,
                    Hatches = df2$Hatch_Total_Mean, Cargos.SD = df2$Cargo_Total_SD,
                    Hatches.SD = df2$Hatch_Total_SD, Habitat_Points = df2$Habitat_Points_Mean,
                    Habitat_Points.SD = df2$Habitat_Points_SD)

plot3 <- plot_ly(df8, x = ~paste(Teams), y = ~Hatches, type = 'bar', name = "Hatches",
                 marker = list(color = 'red'),
                 error_y = ~list(array = Hatches.SD, color = 'black')) %>%
  add_trace(y = ~Cargos, name = 'Cargos', marker = list(color = 'blue'),
            error_y = ~list(array = Cargos.SD, color = 'black')) %>%
  add_trace(y = ~Habitat_Points, name = 'Habitat Points', marker = list(color = 'lime'),
            error_y = ~list(array = Habitat_Points.SD, color = 'black')) %>%
  layout(title = "Offensive Stats of Teams",
         xaxis = list(title = "Team"), yaxis = list(title = "Number +/- SD"),
         barmode = 'group', bargap = 0.15)

htmlwidgets::saveWidget(as_widget(plot3), "frc_offensive_ratings.html")


df9 <- data.frame(Teams = df2$Teams, Offensive_Rating = df2$Cargo_Total_Mean * 3 +
                    df2$Hatch_Total_Mean * 2 + df2$Habitat_Points_Mean,
                    Offensive_Rating_SD =
                      sqrt(9 * (df2$Cargo_Total_SD ** 2) +
                            4 * (df2$Hatch_Total_SD ** 2) +
                           (df2$Habitat_Points_SD ** 2)))

plot4 <- plot_ly(data = df9, x = ~paste(Teams), y = ~Offensive_Rating, type = 'bar', name = 'Total Offensive Points',
                 marker = list(color = 'blue'),
                 error_y = ~list(array = Offensive_Rating_SD, color = 'black')) %>%
  layout(title = "Offensive Ratings of Teams",
         xaxis = list(title = "Team"), yaxis = list(title = "Points +/- SD"))

htmlwidgets::saveWidget(as_widget(plot4), "frc_total_offensive_points.html")

# Alliance Selection
#
# 1st - 2607, 1640, 219 - 1 Def. Bot [219]
# 2nd - 2590, 1807, 7599, 555 - 1 Def. Bot (later 0) [7599]
# 3rd - 1391, 5401, 4573 - 0 Def. Bots
# 4th - 303, 341, 6808 - 1 Def. Bot [6808]
# 5th - 1218, 5404, 2554 - 0 Def. Bots
# 6th - 224, 316, 204 - 0 Def. Bots
# 7th - 2016, 1089, 4361 - 1 Def. Bot [4361]
# 8th - 708, 272, 321 - 1 Def. Bot [272]

# Playoffs Probabilities
#
# This model assumes that the best offensive robot in each alliance gets defended
# by the most defensive robot and that the number of points lost follows the
# mathematical equation below:
#
# Points Lost = (Offensive Rating of the Best Offensive Bot) * (Defensiveness of Enemy Bot)
#
# Has to be manually set to the alliance captain, first pick, and second pick
# (or backup robot if necessary).
#
# If there is no alliance selection yet, just comment out the rest of the code
# (using Ctrl-Shift-C in RStudio, which is available for download in
# https://www.rstudio.com/products/rstudio/download/), but remember to
# uncomment using the same shortcut once there is alliance selection.

first <- c(2607, 1640, 219)
second <- c(2590, 1807, 555)
third <- c(1391, 5401, 4573)
fourth <- c(303, 341, 6808)
fifth <- c(1218, 5404, 2554)
sixth <- c(224, 316, 204)
seventh <- c(2016, 1089, 4361)
eighth <- c(708, 272, 321)

#Then, here comes the ratings:

defensiveRating <- function(alliance) {
  max(df2$Is_Defensive[df2$Teams %in% alliance])
}

mvpIndex <- function(alliance) {
  max(df2$Cargo_Total_Mean[df2$Teams %in% alliance] * 3 +
            df2$Hatch_Total_Mean[df2$Teams %in% alliance] * 2 +
            df2$Habitat_Points_Mean[df2$Teams %in% alliance])
}

probability_of_winning <- function(allianceA, allianceB) {

  df10 <- data.frame(Teams = df2$Teams,
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

  allianceADefensiveRating <- defensiveRating(allianceA) * 5/6

  allianceBDefensiveRating <- defensiveRating(allianceB) * 5/6

  #Assuming the population variances are not equal:

  allianceAPointLoss <- allianceAMaxOffensiveRating * allianceBDefensiveRating
  allianceBPointLoss <- allianceBMaxOffensiveRating * allianceADefensiveRating

  predictedAllianceANetGain <- sum(df10$Offensive_Rating[df10$Teams %in% allianceA]) - allianceAPointLoss
  predictedAllianceBNetGain <- sum(df10$Offensive_Rating[df10$Teams %in% allianceB]) - allianceBPointLoss

  predictedAllianceAStdErr <- sqrt(sum(df10$Offensive_Rating_SD[df10$Teams %in% allianceA] ** 2))
  predictedAllianceBStdErr <- sqrt(sum(df10$Offensive_Rating_SD[df10$Teams %in% allianceB] ** 2))

  #The core of all the probabilities - Meet the Ultimate Trial and Error (Standard Deviation +/- 0.05%)
  normDistA <- rnorm(1000000, mean = predictedAllianceANetGain, sd = predictedAllianceAStdErr)
  normDistB <- rnorm(1000000, mean = predictedAllianceBNetGain, sd = predictedAllianceBStdErr)
  probability <- sum(normDistA - normDistB > 0) / 1000000

  #P(2 or 3 out of 3) = 3 * (x ** 2) * (1 - x) + x ** 3
  three_match_prob <- (probability ** 2) * (3 - 2 * probability)
  three_match_prob
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

#All add up to 400%. Nice.
first_semis <- probability_of_winning(first, eighth)
second_semis <- probability_of_winning(second, seventh)
third_semis <- probability_of_winning(third, sixth)
fourth_semis <- probability_of_winning(fourth, fifth)

first_finals_given_semis <- fourth_semis * probability_of_winning(first, fourth) + (1 - fourth_semis) * probability_of_winning(first, fifth)

second_finals_given_semis <- third_semis * probability_of_winning(second, third) + (1 - third_semis) * probability_of_winning(second, sixth)

third_finals_given_semis <- second_semis * probability_of_winning(third, second) + (1 - second_semis) * probability_of_winning(third, seventh)

fourth_finals_given_semis <- first_semis * probability_of_winning(fourth, first) + (1 - first_semis) * probability_of_winning(fourth, eighth)

fifth_finals_given_semis <- first_semis * probability_of_winning(fifth, first) + (1 - first_semis) * probability_of_winning(fifth, eighth)

sixth_finals_given_semis <- second_semis * probability_of_winning(sixth, second) + (1 - second_semis) * probability_of_winning(sixth, seventh)

seventh_finals_given_semis <- third_semis * probability_of_winning(seventh, third) + (1 - third_semis) * probability_of_winning(seventh, sixth)

eighth_finals_given_semis <- fourth_semis * probability_of_winning(eighth, fourth) + (1 - fourth_semis) * probability_of_winning(eighth, fifth)

#Now, for the finals variables
first_finals <- first_semis * first_finals_given_semis
second_finals <- second_semis * second_finals_given_semis
third_finals <- third_semis * third_finals_given_semis
fourth_finals <- fourth_semis * fourth_finals_given_semis
fifth_finals <- (1 - fourth_semis) * fifth_finals_given_semis
sixth_finals <- (1 - third_semis) * sixth_finals_given_semis
seventh_finals <- (1 - second_semis) * seventh_finals_given_semis
eighth_finals <- (1 - first_semis) * eighth_finals_given_semis

#Again, they do not necessarily add up to 200%, so this should adjust for that.
total_finals <- first_finals + second_finals + third_finals + fourth_finals + fifth_finals + sixth_finals + seventh_finals + eighth_finals

first_finals <- first_finals * 2 / total_finals
second_finals <- second_finals * 2 / total_finals
third_finals <- third_finals * 2 / total_finals
fourth_finals <- fourth_finals * 2 / total_finals
fifth_finals <- fifth_finals * 2 / total_finals
sixth_finals <- sixth_finals * 2 / total_finals
seventh_finals <- seventh_finals * 2 / total_finals
eighth_finals <- eighth_finals * 2 / total_finals

#Now, for the finals variables
first_finals <- first_semis * first_finals_given_semis
second_finals <- second_semis * second_finals_given_semis
third_finals <- third_semis * third_finals_given_semis
fourth_finals <- fourth_semis * fourth_finals_given_semis
fifth_finals <- (1 - fourth_semis) * fifth_finals_given_semis
sixth_finals <- (1 - third_semis) * sixth_finals_given_semis
seventh_finals <- (1 - second_semis) * seventh_finals_given_semis
eighth_finals <- (1 - first_semis) * eighth_finals_given_semis

#Again, they do not necessarily add up to 200%, so this should adjust for that.
total_finals <- first_finals + second_finals + third_finals + fourth_finals + fifth_finals + sixth_finals + seventh_finals + eighth_finals

first_finals <- first_finals * 2 / total_finals
second_finals <- second_finals * 2 / total_finals
third_finals <- third_finals * 2 / total_finals
fourth_finals <- fourth_finals * 2 / total_finals
fifth_finals <- fifth_finals * 2 / total_finals
sixth_finals <- sixth_finals * 2 / total_finals
seventh_finals <- seventh_finals * 2 / total_finals
eighth_finals <- eighth_finals * 2 / total_finals

#First - 2nd, 3rd, 6th, and 7th
first_champions_given_finals <-
  second_semis * second_finals_given_semis * probability_of_winning(first, second) +
  third_semis * third_finals_given_semis * probability_of_winning(first, third) +
  (1 - third_semis) * sixth_finals_given_semis * probability_of_winning(first, sixth) +
  (1 - second_semis) * seventh_finals_given_semis * probability_of_winning(first, seventh)

#Second - 1st, 4th, 5th, and 8th
second_champions_given_finals <-
  first_semis * first_finals_given_semis * probability_of_winning(second, first) +
  fourth_semis * fourth_finals_given_semis * probability_of_winning(second, fourth) +
  (1 - fourth_semis) * fifth_finals_given_semis * probability_of_winning(second, fifth) +
  (1 - first_semis) * eighth_finals_given_semis * probability_of_winning(second, eighth)

#Third - 1st, 4th, 5th, and 8th
third_champions_given_finals <-
  first_semis * first_finals_given_semis * probability_of_winning(third, first) +
  fourth_semis * fourth_finals_given_semis * probability_of_winning(third, fourth) +
  (1 - fourth_semis) * fifth_finals_given_semis * probability_of_winning(third, fifth) +
  (1 - first_semis) * eighth_finals_given_semis * probability_of_winning(third, eighth)

#Fourth - 2nd, 3rd, 6th, and 7th
fourth_champions_given_finals <-
  second_semis * second_finals_given_semis * probability_of_winning(fourth, second) +
  third_semis * third_finals_given_semis * probability_of_winning(fourth, third) +
  (1 - third_semis) * sixth_finals_given_semis * probability_of_winning(fourth, sixth) +
  (1 - second_semis) * seventh_finals_given_semis * probability_of_winning(fourth, seventh)

#Fifth - 2nd, 3rd, 6th, and 7th
fifth_champions_given_finals <-
  second_semis * second_finals_given_semis * probability_of_winning(fifth, second) +
  third_semis * third_finals_given_semis * probability_of_winning(fifth, third) +
  (1 - third_semis) * sixth_finals_given_semis * probability_of_winning(fifth, sixth) +
  (1 - second_semis) * seventh_finals_given_semis * probability_of_winning(fifth, seventh)

#Sixth - 1st, 4th, 5th, and 8th
sixth_champions_given_finals <-
  first_semis * first_finals_given_semis * probability_of_winning(sixth, first) +
  fourth_semis * fourth_finals_given_semis * probability_of_winning(sixth, fourth) +
  (1 - fourth_semis) * fifth_finals_given_semis * probability_of_winning(sixth, fifth) +
  (1 - first_semis) * eighth_finals_given_semis * probability_of_winning(sixth, eighth)

#Seventh - 1st, 4th, 5th, and 8th
seventh_champions_given_finals <-
  first_semis * first_finals_given_semis * probability_of_winning(seventh, first) +
  fourth_semis * fourth_finals_given_semis * probability_of_winning(seventh, fourth) +
  (1 - fourth_semis) * fifth_finals_given_semis * probability_of_winning(seventh, fifth) +
  (1 - first_semis) * eighth_finals_given_semis * probability_of_winning(seventh, eighth)

#Eighth - 2nd, 3rd, 6th, and 7th
eighth_champions_given_finals <-
  second_semis * second_finals_given_semis * probability_of_winning(eighth, second) +
  third_semis * third_finals_given_semis * probability_of_winning(eighth, third) +
  (1 - third_semis) * sixth_finals_given_semis * probability_of_winning(eighth, sixth) +
  (1 - second_semis) * seventh_finals_given_semis * probability_of_winning(eighth, seventh)

#All conditional probabilities are formatted and rounded to the nearest 0.1%

pw14 <- probability_of_winning(first, fourth)
pw15 <- probability_of_winning(first, fifth)
pw84 <- probability_of_winning(eighth, fourth)
pw85 <- probability_of_winning(eighth, fifth)
pw23 <- probability_of_winning(second, third)
pw26 <- probability_of_winning(second, sixth)
pw73 <- probability_of_winning(seventh, third)
pw76 <- probability_of_winning(seventh, sixth)

df11 <- data.frame(Alliance_Numbers = 1:8,
                   Semifinals_Probabilities = c(first_semis, second_semis, third_semis,
                                                fourth_semis, 1 - fourth_semis,
                                                1 - third_semis, 1 - second_semis,
                                                1 - first_semis),
                   Finals_Probabilities = c(first_finals, second_finals, third_finals,
                                            fourth_finals, fifth_finals, sixth_finals,
                                            seventh_finals, eighth_finals),
                   Champion_Probabilities = c(first_champions, second_champions, third_champions,
                                            fourth_champions, fifth_champions, sixth_champions,
                                            seventh_champions, eighth_champions))

plot5 <- plot_ly(data = df11, x = ~Alliance_Numbers, y = ~Semifinals_Probabilities, type = 'bar',
                 hoverinfo = 'text', text = ~paste("Alliance ", Alliance_Numbers, ": ",
                                                   dec_to_frac_of_vector(Semifinals_Probabilities),
                                                   " (", floor(10000 * Semifinals_Probabilities) / 100, "%)",
                                                   sep = "")) %>%
  layout(title = "Quarterfinal Probabilities", xaxis = list(title = "Alliance #"),
         yaxis = list(title = "Probability", tickformat = ".2%"))
htmlwidgets::saveWidget(as_widget(plot5), "Quarterfinals.html")

plot6 <- plot_ly(data = df11, x = ~Alliance_Numbers, y = ~Finals_Probabilities, type = 'bar',
                 hoverinfo = 'text', text = ~paste("Alliance ", Alliance_Numbers, ": ",
                                                   dec_to_frac_of_vector(Finals_Probabilities),
                                                   " (", floor(10000 * Finals_Probabilities) / 100, "%)",
                                                   sep = "")) %>%
  layout(title = "Probability of Alliance going to Finals", xaxis = list(title = "Alliance #"),
         yaxis = list(title = "Probability", tickformat = ".2%"))
htmlwidgets::saveWidget(as_widget(plot6), "AllianceProbabilityFinals.html")

plot7 <- plot_ly(data = df11, x = ~Alliance_Numbers, y = ~Champion_Probabilities, type = 'bar',
                 hoverinfo = 'text', text = ~paste("Alliance ", Alliance_Numbers, ": ",
                                                   dec_to_frac_of_vector(Champion_Probabilities),
                                                   " (", floor(10000 * Champion_Probabilities) / 100, "%)",
                                                   sep = "")) %>%
  layout(title = "Championship Probabilities", xaxis = list(title = "Alliance #"),
         yaxis = list(title = "Probability", tickformat = ".2%"))
htmlwidgets::saveWidget(as_widget(plot7), "AllianceProbabilityChamps.html")

df12 <- data.frame(Red = c(1, 8),
                   Blue = c(4, 5),
                   Primary_Matches = c(pw14, pw84),
                   Secondary_Matches = c(pw15, pw85))
plot8 <- plot_ly(data = df12, x = ~paste("Alliance", Red), y = ~Primary_Matches, type = 'bar',
                 hoverinfo = 'text', name = "vs. Alliance 4", text = ~paste("Alliance ", Red, ": ",
                                                   dec_to_frac_of_vector(Primary_Matches),
                                                   " (", floor(10000 * Primary_Matches) / 100,
                                                   "%) vs. Alliance 4", sep = "")) %>%
  add_trace(y = ~Secondary_Matches, name = "vs. Alliance 5", hoverinfo = 'text', text = ~paste("Alliance ", Red, ": ",
                                                     dec_to_frac_of_vector(Secondary_Matches),
                                                     " (", floor(10000 * Secondary_Matches) / 100,
                                                     "%) vs. Alliance 5", sep = "")) %>%
  layout(title = "Semifinals Probabilities", xaxis = list(title = "Alliance #"),
         yaxis = list(title = "Probability", tickformat = ".2%"))
print(plot8)
htmlwidgets::saveWidget(as_widget(plot8), "Semis1.html")

df13 <- data.frame(Red = c(2, 7),
                   Primary_Matches = c(pw23, pw73),
                   Secondary_Matches = c(pw26, pw76))

plot9 <- plot_ly(data = df13, x = ~paste("Alliance", Red), y = ~Primary_Matches, type = 'bar',
                 hoverinfo = 'text', name = "vs. Alliance 3", text = ~paste("Alliance ", Red, ": ",
                                                                            dec_to_frac_of_vector(Primary_Matches),
                                                                            " (", floor(10000 * Primary_Matches) / 100,
                                                                            "%) vs. Alliance 3", sep = "")) %>%
  add_trace(y = ~Secondary_Matches, name = "vs. Alliance 6", hoverinfo = 'text', text = ~paste("Alliance ", Red, ": ",
                                                                                               dec_to_frac_of_vector(Secondary_Matches),
                                                                                               " (", floor(10000 * Secondary_Matches) / 100,
                                                                                               "%) vs. Alliance 6", sep = "")) %>%
  layout(title = "Semifinals Probabilities", xaxis = list(title = "Alliance #"),
         yaxis = list(title = "Probability", tickformat = ".2%"))
htmlwidgets::saveWidget(as_widget(plot9), "Semis2.html")

df14 <- data.frame(Red = c(1, 4, 5, 8),
                   Matches_Second = c(pw12, pw42, pw52, pw82),
                   Matches_Third = c(pw13, pw43, pw53, pw83),
                   Matches_Sixth = c(pw16, pw46, pw56, pw86),
                   Matches_Seventh = c(pw17, pw47, pw57, pw87))

plot10 <- plot_ly(data = df14, x = ~paste("Alliance", Red), y = ~Matches_Second, type = 'bar',
                 hoverinfo = 'text', name = "vs. Alliance 2", text = ~paste("Alliance ", Red, ": ",
                                                                            dec_to_frac_of_vector(Matches_Second),
                                                                            " (", floor(10000 * Matches_Second) / 100,
                                                                            "%) vs. Alliance 2", sep = "")) %>%
  add_trace(y = ~Matches_Third, name = "vs. Alliance 3", hoverinfo = 'text', text = ~paste("Alliance ", Red, ": ",
                                                                                               dec_to_frac_of_vector(Matches_Third),
                                                                                               " (", floor(10000 * Matches_Third) / 100,
                                                                                               "%) vs. Alliance 3", sep = "")) %>%
  add_trace(y = ~Matches_Sixth, name = "vs. Alliance 6", hoverinfo = 'text', text = ~paste("Alliance ", Red, ": ",
                                                                                               dec_to_frac_of_vector(Matches_Sixth),
                                                                                               " (", floor(10000 * Matches_Sixth) / 100,
                                                                                               "%) vs. Alliance 6", sep = "")) %>%
  add_trace(y = ~Matches_Seventh, name = "vs. Alliance 7", hoverinfo = 'text', text = ~paste("Alliance ", Red, ": ",
                                                                                               dec_to_frac_of_vector(Matches_Seventh),
                                                                                               " (", floor(10000 * Matches_Seventh) / 100,
                                                                                               "%) vs. Alliance 7", sep = "")) %>%
  layout(title = "Semifinals Probabilities", xaxis = list(title = "Alliance #"),
         yaxis = list(title = "Probability", tickformat = ".2%"))
htmlwidgets::saveWidget(as_widget(plot10), "Finals.html")

pw12 <- probability_of_winning(first, second)
pw13 <- probability_of_winning(first, third)
pw16 <- probability_of_winning(first, sixth)
pw17 <- probability_of_winning(first, seventh)
pw42 <- probability_of_winning(fourth, second)
pw43 <- probability_of_winning(fourth, third)
pw46 <- probability_of_winning(fourth, sixth)
pw47 <- probability_of_winning(fourth, seventh)
pw52 <- probability_of_winning(fifth, second)
pw53 <- probability_of_winning(fifth, third)
pw56 <- probability_of_winning(fifth, sixth)
pw57 <- probability_of_winning(fifth, seventh)
pw82 <- probability_of_winning(eighth, second)
pw83 <- probability_of_winning(eighth, third)
pw86 <- probability_of_winning(eighth, sixth)
pw87 <- probability_of_winning(eighth, seventh)

#Probability that each alliance will make it to become the champions:
first_champions <- first_semis * first_finals_given_semis * first_champions_given_finals
second_champions <- second_semis * second_finals_given_semis * second_champions_given_finals
third_champions <- third_semis * third_finals_given_semis * third_champions_given_finals
fourth_champions <- fourth_semis * fourth_finals_given_semis * fourth_champions_given_finals
fifth_champions <- (1 - fourth_semis) * fifth_finals_given_semis * fifth_champions_given_finals
sixth_champions <- (1 - third_semis) * sixth_finals_given_semis * sixth_champions_given_finals
seventh_champions <- (1 - second_semis) * seventh_finals_given_semis * seventh_champions_given_finals
eighth_champions <- (1 - first_semis) * eighth_finals_given_semis * eighth_champions_given_finals

total <- first_champions + second_champions + third_champions + fourth_champions + fifth_champions + sixth_champions + seventh_champions + eighth_champions
#The percentages calculated in the 8 variables above
#may not add up to 100% due to the random distribution, so they will be adjusted.
first_champions <- first_champions / total
second_champions <- second_champions / total
third_champions <- third_champions / total
fourth_champions <- fourth_champions / total
fifth_champions <- fifth_champions / total
sixth_champions <- sixth_champions / total
seventh_champions <- seventh_champions / total
eighth_champions <- eighth_champions / total

#Total - Mean Points
first_points <- 10 * (first_semis + first_finals + first_champions)
second_points <- 10 * (second_semis + second_finals + second_champions)
third_points <- 10 * (third_semis + third_finals + third_champions)
fourth_points <- 10 * (fourth_semis + fourth_finals + fourth_champions)
fifth_points <- 10 * ((1 - fourth_semis) + fifth_finals + fifth_champions)
sixth_points <- 10 * ((1 - third_semis) + sixth_finals + sixth_champions)
seventh_points <- 10 * ((1 - second_semis) + seventh_finals + seventh_champions)
eighth_points <- 10 * ((1 - first_semis) + eighth_finals + eighth_champions)

#Total - Standard Deviations
first_std_dev <- sqrt(sum(c(1 - first_semis, first_semis - first_finals,
                            first_finals - first_champions, first_champions) *
                            (c(0, 10, 20, 30) - first_points) ** 2))

second_std_dev <- sqrt(sum(c(1 - second_semis, second_semis - second_finals,
                             second_finals - second_champions, second_champions) *
                            (c(0, 10, 20, 30) - second_points) ** 2))

third_std_dev <- sqrt(sum(c(1 - third_semis, third_semis - third_finals,
                            third_finals - third_champions, third_champions) *
                            (c(0, 10, 20, 30) - third_points) ** 2))

fourth_std_dev <- sqrt(sum(c(1 - fourth_semis, fourth_semis - fourth_finals,
                             fourth_finals - fourth_champions, fourth_champions) *
                            (c(0, 10, 20, 30) - fourth_points) ** 2))

fifth_std_dev <- sqrt(sum(c(fourth_semis, 1 - fourth_semis - fifth_finals,
                            fifth_finals - fifth_champions, fifth_champions) *
                            (c(0, 10, 20, 30) - fifth_points) ** 2))

sixth_std_dev <- sqrt(sum(c(third_semis, 1 - third_semis - sixth_finals,
                            sixth_finals - sixth_champions, sixth_champions) *
                            (c(0, 10, 20, 30) - sixth_points) ** 2))

seventh_std_dev <- sqrt(sum(c(second_semis, 1 - second_semis - seventh_finals,
                              seventh_finals - seventh_champions, seventh_champions) *
                            (c(0, 10, 20, 30) - seventh_points) ** 2))

eighth_std_dev <- sqrt(sum(c(first_semis, 1 - first_semis - eighth_finals,
                             eighth_finals - eighth_champions, eighth_champions) *
                            (c(0, 10, 20, 30) - eighth_points) ** 2))

#Probability, rounded to the nearest 0.01% (varies because of rnorm()'s RNG):

total_points <- first_points + second_points + third_points + fourth_points +
  fifth_points + sixth_points + seventh_points + eighth_points

first_points <- first_points * 70 / total_points
second_points <- second_points * 70 / total_points
third_points <- third_points * 70 / total_points
fourth_points <- fourth_points * 70 / total_points
fifth_points <- fifth_points * 70 / total_points
sixth_points <- sixth_points * 70 / total_points
seventh_points <- seventh_points * 70 / total_points
eighth_points <- eighth_points * 70 / total_points

df15 <- data.frame(Alliances = 1:8, Points = c(first_points, second_points, third_points, fourth_points,
                                               fifth_points, sixth_points, seventh_points, eighth_points),
                   Points.SD = c(first_std_dev, second_std_dev, third_std_dev, fourth_std_dev,
                                 fifth_std_dev, sixth_std_dev, seventh_std_dev, eighth_std_dev))

plot11 <- plot_ly(data = df15, x = ~Alliances, y = ~Points, type = 'bar', name = 'Total Expected Championship Points',
                 marker = list(color = 'orange'),
                 error_y = ~list(array = Points.SD, color = 'black')) %>%
  layout(title = "Expected Alliance Points",
         xaxis = list(title = "Alliance"), yaxis = list(title = "Points +/- SD"))
htmlwidgets::saveWidget(as_widget(plot11), "ExpectedAlliancePoints.html")