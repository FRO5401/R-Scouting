require(ggplot2)
"
Build an R script that can do the following:

1) Read values from an Excel file every 30 seconds (i.e. Conversion from .txt to Data Frame).

2) Separate into Teams for Mean and Standard Deviation.

3) Interpret these values as Graphs for each team.

4) If possible, integrate the script with Tableau (Not possible due to the illegality
of connections in the FRC events).

Note: R1, R2, R3 mean the respective levels of the rocket ship, and CS means cargo ship.
S means Sandstorm and T means Teleop.

SP means Station Pickup and GP means Ground Pickup
"


df <- read.csv("Bensalem Event 2019 Scouting.txt", header = FALSE) #The path is necessary for the R script to read the file.

df[df == "null"] = NA


#df <- data.frame(names = c("A", "A", "A", "A", "A", "A", "A", "B"),
#collection = c(28, 23, 49, 29, 38, 23, 29, 21))

colnames(df) <- c("Name", "Team", "Position", "Match", "Is_Start_Level_2",
                  "S_Hatch_CS", "S_Hatch_R1", "S_Hatch_R2", "S_Hatch_R3",
                  "S_Cargo_CS", "S_Cargo_R1", "S_Cargo_R2", "S_Cargo_R3",
                  "T_Hatch_CS", "T_Hatch_R1", "T_Hatch_R2", "T_Hatch_R3",
                  "Hatch_GP", "Hatch_SP",
                  "T_Cargo_CS", "T_Cargo_R1", "T_Cargo_R2", "T_Cargo_R3",
                  "Is_Defensive", "Habitat_Level", "Time_For_Climb", "Notes")

print(typeof(df))
#Get unique team
uniqueTeams <- unique(df$Team)
print(uniqueTeams)

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
  c <- ifelse(v < 3, 3 * v, 12)
}
for (team in uniqueTeams) {
  a <- which(df$Team == team)
  
  #Sandstorm Vars
  b <- df$Is_Start_Level_2
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
  #u <- df$Time_For_Climb
  #String... generally says seconds but can say minutes in extreme cases. So no stats.
  
  #Means
  c1 <- c(c1, mean(c[a], na.rm = TRUE))
  c2 <- c(c2, mean(d[a], na.rm = TRUE))
  c3 <- c(c3, mean(e[a], na.rm = TRUE))
  c4 <- c(c4, mean(f[a], na.rm = TRUE))
  c5 <- c(c5, mean(g[a], na.rm = TRUE))
  c6 <- c(c6, mean(h[a], na.rm = TRUE))
  c7 <- c(c7, mean(i[a], na.rm = TRUE))
  c8 <- c(c8, mean(j[a], na.rm = TRUE))
  c9 <- c(c9, mean(k[a], na.rm = TRUE))
  c10 <- c(c10, mean(l[a], na.rm = TRUE))
  c11 <- c(c11, mean(m[a], na.rm = TRUE))
  c12 <- c(c12, mean(n[a], na.rm = TRUE))
  c13 <- c(c13, mean(o[a], na.rm = TRUE))
  c14 <- c(c14, mean(p[a], na.rm = TRUE))
  c15 <- c(c15, mean(q[a], na.rm = TRUE))
  c16 <- c(c16, mean(r[a], na.rm = TRUE))
  c17 <- c(c17, mean(s[a], na.rm = TRUE))
  c18 <- c(c18, mean(pointfunction(s[a]), na.rm = TRUE))
  c19 <- c(c19, mean(c[a] + d[a] + e[a] + f[a] + k[a] + l[a] + m[a] + n[a], na.rm = TRUE))
  c20 <- c(c20, mean(g[a] + h[a] + i[a] + j[a] + o[a] + p[a] + q[a] + r[a], na.rm = TRUE))
  c21 <- c(c21, mean(t[a], na.rm = TRUE))
  c22 <- c(c22, mean(u[a], na.rm = TRUE))
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
  c45 <- c(c45, mean(v[a], na.rm = TRUE))
}

#Start level is not really going to matter much in this DF.
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

#Order by Defensiveness, descending.
df2 <- df2[with(df2, order(-Is_Defensive)), ]
print(df2)

#Order by Hatch Panels, descending.
df2 <- df2[with(df2, order(-Hatch_Total_Mean)), ]
print(df2)

#Order by Cargo, descending.
df2 <- df2[with(df2, order(-Cargo_Total_Mean)), ]
print(df2)

#Order by Hab Level, descending
df2 <- df2[with(df2, order(-Habitat_Level_Mean)), ]
print(df2)

#Order by Hab Points, descending
df2 <- df2[with(df2, order(-Habitat_Points_Mean)), ]
print(df2)

df3 <- data.frame(Cargos = df2$Cargo_Total_Mean, Hatches = df2$Hatch_Total_Mean)

#Yes, the plot can get pretty cramped especially at the bottom,
#but this is probably the only plot I can do at present.

plot(df3, main="FRC Teams")
text(df3$Cargos, df3$Hatches + 0.25, labels = df2$Teams)