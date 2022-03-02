library(dplyr)
library(readxl)
FANGRAPHS_2022 <- read_excel("~/FANGRAPHS-2022.xlsx")


pitchers <-FANGRAPHS_2022 %>%
  filter(Pos == "SP" | Pos == "MIRP" | Pos == "SIRP")

hitters <- FANGRAPHS_2022 %>%
  filter(Pos != "SP" ) %>%
  filter(Pos != "MIRP") %>%
  filter(Pos != "SIRP")


# separate kelly formulas in case I want to change payoffs for pitchers and hitters

hitter_kelly <- function(bust, f40, f50, f60, f70){
  x = seq(0,1,.001)
  y = bust*log(1-x)+f40*log(1+54*x)+f50*log(1+135*x)+f60*log(1+270*x)+f70*log(1+378*x)
  z = data.frame(x,y)
  
  max_x = z[z$y == max(z$y),][1]
  
  return(as.numeric(max_x))
}

pitcher_kelly <- function(bust, f40, f50, f60, f70){
  x = seq(0,1,.001)
  y = bust*log(1-x)+f40*log(1+54*x)+f50*log(1+135*x)+f60*log(1+270*x)+f70*log(1+378*x)
  z = data.frame(x,y)
  
  max_x = z[z$y == max(z$y),][1]
  
  return(as.numeric(max_x))
}



kelly_size <- c()
for (i in 1:82){
  
  y = hitter_kelly(hitters$Bust[i], hitters$`40/45`[i], hitters$`50/55`[i], hitters$`60/65`[i],hitters$`70+`[i])
  
  kelly_size = append(kelly_size, y)
}

hitters$kelly = kelly_size


kelly_size <- c()
for (i in 1:32){
  
  y = hitter_kelly(pitchers$Bust[i], pitchers$`40/45`[i], pitchers$`50/55`[i], pitchers$`60/65`[i], pitchers$`70+`[i])
  
  kelly_size = append(kelly_size, y)
}

pitchers$kelly = kelly_size


master <- rbind(hitters, pitchers)
final <- master %>%
  select(Name, Pos, Team, kelly, `FG Rank`)
  
masterk <- master %>%
  mutate(levelAA_above = if_else(`Highest Level` == "MLB", 1, 
                        if_else(`Highest Level` == "AAA", 1,
                                if_else(`Highest Level` == "AA", 1, 0)))) %>%
  group_by(levelAA_above) %>%
  summarise(avg_kelly = mean(kelly)) 
        
masterA <- master %>%
  mutate(levelAA_above = if_else(`Highest Level` == "MLB", 1, 
                                 if_else(`Highest Level` == "AAA", 1,
                                         if_else(`Highest Level` == "AA", 1, 0))),
         adjkelly = if_else(levelAA_above == 1, kelly - .72, kelly - .62))


finalA <- masterA %>%
  select(Name, Pos, Team, `Highest Level`, kelly, adjkelly, `FG Rank`)

write.csv(final, "fangraphs-kelly.csv")
write.csv(finalA, "fangraphs-adj-kelly.csv")

