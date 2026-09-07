---
title: 'Formula 1 70th Anniversary'
author: 
  name: "Ekrem Bayar"
date: '`r Sys.Date()`'
output:
  html_document:
    toc: yes
    number_sections: yes
    code_folding: hide
    theme: spacelab
    highlight: tango
---

```{css, echo=FALSE}
@font-face {
  font-family: F1;
  src: url(https://www.formula1.com/etc/designs/fom-website/fonts/F1Regular/Formula1-Regular.ttf);
}

span{
  font-family: F1;
}

a{
  font-family: F1;
}

.nav-pills>li.active>a:focus {
    color: #ffffff;
    background-color: lightgray;
}

.container-fluid, .container-fluid h1 {
    font-family: F1;
    line-height: 1.7;
}

.container-fluid p {
    font-family: F1;
    color:black;
}

h1,h2,h3,h4,h5,h6,p, table {
  font-family: F1;
  color: black
}
```


<br>


<img src = "https://img.fanatik.com.tr/img/75/0x0/5ebbe038ae298bce9148f814.jpg"></img>




<br>

<center><p style="font-size:14pt; font-family:F1; font-style:italic">
“Really, you should always discuss the defeats because you can learn much more from failure than from success.”     
</p></center>
<div style="text-align: right; color: red; font-family:F1"> **Niki Lauda** </div>

<br>


# F1 2020 Champion
<hr>

<div style="width: 100%;">

  <div style="background-color: #00D2BE;width:9px;height:351px;float: left;"></div>
  
  
  <div style="float:left; margin-left:25px; margin-top:15px; font-size:20px; color:black;">
   Lewis

  **Hamilton**

  British

  1985-01-07

  **44**

  **HAM**

  Mercedes
  
  <img src = "https://www.formula1.com/content/fom-website/en/drivers/lewis-hamilton/_jcr_content/countryFlag.img.jpg/1536080051510.jpg"></img>
  </div>
  
  
  
  <div style="width:281px;height:351px;float: left; margin-left:95px;">
  <img src="https://pbs.twimg.com/media/Em3Wj8LW8AUKPCf.jpg"></img>
  </div>
  
  
  <div style="float:left; margin-left:95px; margin-top:50px; color:black;"> 
  
|   **Stats**    |       |   
|:--------------|:-------|
| **Podiums**       | 165|     
| **Points**        |3778|
| **Grands Prix entered**  |266| 
| **World Championships**  |7|        
| **Highest race finish**  |1 (x95) | 
| **Highest grid position**|1|
</div>

<div style="clear:both;">


<div style="width: 100%;">
  <div style="margin-top:20px;float:left; width:35%"><img src="https://static.aiqu.design/images/f1/2020-08/g4v32b4bn0n.png"></img></div>
  <div style="float:right; width:35%"><img src="https://apicms.thestar.com.my/uploads/images/2020/03/30/625635.jpg"></img></div>
</div>
<div style="clear:both;">






# Packages
<hr>
```{r message=FALSE, warning=FALSE}
library(magrittr)
library(tidyverse)
library(plotly)
library(DT)
library(extrafont)
ttf_import(paths = "../input/f1ekrem")
```



# Data {.tabset .tabset-fade .tabset-pills}
<hr>
## Data Manipulation
```{r message=FALSE, warning=FALSE}
# 1. Grand Prix: 2020
races <- read.csv("../input/formula-1-world-championship-1950-2020/races.csv") %>% 
  filter(year == 2020) %>% 
  rename(circuit = name)

# 2. Results 2020
status = read.csv("../input/formula-1-world-championship-1950-2020/status.csv")
results <- read.csv("../input/formula-1-world-championship-1950-2020/results.csv") %>% 
  filter(raceId %in% races$raceId) %>% 
  left_join(status, by = "statusId") %>% 
  select(-statusId, -resultId)
remove(status)

# 3. Driver Standings
driver_standings <- read.csv("../input/formula-1-world-championship-1950-2020/driver_standings.csv") %>% 
  filter(raceId %in% races$raceId,
         # Remove Hülkenberg, Pittipaldi, Aitken
         !driverId %in% c(807, 850, 851)) %>% 
  select(-driverStandingsId)


# 5. Constructor Standings
constructor_standings <- read.csv("../input/formula-1-world-championship-1950-2020/constructor_standings.csv") %>% 
  filter(raceId %in% races$raceId)%>% 
  select(-constructorStandingsId)

# 6. Lap, Pit, Qua
lap_times <- read.csv("../input/formula-1-world-championship-1950-2020/lap_times.csv") %>% 
  filter(raceId %in% races$raceId,
         !driverId %in% c(807, 850, 851))
pit_stops <- read.csv("../input/formula-1-world-championship-1950-2020/pit_stops.csv") %>% 
  filter(raceId %in% races$raceId,
         !driverId %in% c(807, 850, 851))
qualifying <- read.csv("../input/formula-1-world-championship-1950-2020/qualifying.csv") %>% 
  filter(raceId %in% races$raceId,
         !driverId %in% c(807, 850, 851))

 


# 4. Drivers & Constructors -----------------------------------------------
drivers <- read.csv("../input/formula-1-world-championship-1950-2020/drivers.csv", encoding = "UTF-8") %>% 
  filter(driverId %in% driver_standings$driverId) %>% 
  unite(driver.name, c("forename", "surname"), sep = " ") %>% 
  select(driverId, code, number, driver.name, dob, nationality, url) %>% 
  rename(driver.number = number)

constructors <- read.csv("../input/formula-1-world-championship-1950-2020/constructors.csv") %>% 
  filter(constructorId %in% constructor_standings$constructorId) %>% 
  select(-constructorRef) %>% rename(cons.name = name)





# 5. Name Matching --------------------------------------------------------
gp <- races %>% select(raceId, round, circuit)
dr <- drivers %>% select(driverId, driver.name, code, driver.number)
cons <- constructors %>% select(constructorId, cons.name) 

results %<>%  left_join(gp, by = "raceId") %>%
  left_join(dr, by = "driverId") %>% 
  left_join(cons, by = "constructorId")%>% 
  select(-raceId, -driverId, -constructorId)

constructor_standings %<>%  left_join(gp, by = "raceId") %>%
  left_join(cons, by = "constructorId")%>% 
  select(-raceId, -constructorId)

driver_standings %<>%  left_join(gp, by = "raceId") %>%
  left_join(dr, by = "driverId") %>% 
  select(-raceId, -driverId)

lap_times %<>%  left_join(gp, by = "raceId") %>%
  left_join(dr, by = "driverId")%>% 
  select(-raceId, -driverId)

pit_stops %<>%  left_join(gp, by = "raceId") %>%
  left_join(dr, by = "driverId") %>% 
  select(-raceId, -driverId)

qualifying %<>%  left_join(gp, by = "raceId") %>%
  left_join(dr, by = "driverId") %>% 
  left_join(cons, by = "constructorId") %>% 
  select(-raceId, -driverId, -constructorId, -qualifyId)

rm(gp, dr, cons)


# 6. Add Colors -----------------------------------------------------------

color <- read.csv("../input/f1ekrem/color.csv", encoding = "UTF-8", sep = ";", stringsAsFactors = FALSE) %>% 
  mutate(new = paste0(driver.name, "-", cons.name)) %>% 
  filter(new != "George Russell-Mercedes") %>% select(-new) 

qualifying %<>%  left_join(color, by = "driver.name") %>% 
  group_by(round) %>%
  mutate_at(vars(q1:q3), funs(as.character)) %>% 
  arrange(q1) %>% 
  mutate(pos1 = row_number(),
         q2 = if_else(q2 %in% "\\N", "10:00.000", q2)) %>% 
  arrange(q2) %>% 
  mutate(pos2 = row_number(),
         pos2 = if_else(pos2 == "10:00.000", position, pos2),
         q3 = if_else(q3 %in% "\\N", "10:00.000", q3)) %>% 
  select(circuit ,position, q1, q2, q3, driver.name, pos1, pos2, colors) %>% 
  mutate(rmean = (position + pos1 + pos2) / 3) %>% 
  ungroup() %>% 
  mutate(q2 = if_else(q2 %in% "10:00.000", "\\N", q2),
         q3 = if_else(q3 %in% "10:00.000", "\\N", q3))
pit_stops %<>%  left_join(color) 
lap_times %<>%  left_join(color) 
constructor_standings %<>%  left_join(color %>% select(-driver.name) %>% distinct()) 
driver_standings %<>%  left_join(color) 

results %<>%  left_join(color) 

qualifying %<>% left_join(color %>% select(driver.name, cons.name), by = "driver.name")





# 9. Comparision ----------------------------------------------------------

## 9.1. Drivers

# Races
driver_comp <- results %>%
  group_by(driver.name) %>% 
  count(name = "Races") %>% 
# Points
left_join(
  driver_standings %>% 
    group_by(driver.name) %>% 
    summarise(Points = max(points)) %>% 
    ungroup() 
) %>% 
# Qualifying
left_join(
  qualifying %>% 
    mutate(pass = if_else(position < 11, 1, 0)) %>% 
    group_by(driver.name) %>% 
    summarise(Qualifying = sum(pass)) 
) %>% 
# Podiums
left_join(
  results %>%
    filter(positionOrder < 4) %>% 
    group_by(driver.name) %>% 
    count(name = "Podiums") 
) %>% 
# Wins
left_join(
  driver_standings %>% 
    group_by(driver.name) %>% 
    summarise(Wins = max(wins))
) %>% 
# Best Race Finish
left_join(
  results %>% 
    group_by(driver.name) %>% 
    summarise(BRF = min(positionOrder)) %>% 
    ungroup()# %>% 
    # mutate(BRFTEXT = as.character(case_when(
    #   BRF == 1 ~ paste0(BRF, "ST"),
    #   BRF == 2 ~ paste0(BRF, "ND"),
    #   BRF == 3 ~ paste0(BRF, "RD"),
    #   TRUE ~ paste0(BRF, "TH")
    # )),
    # BRF = 10
    #)
)  %>% 
# Best Qualifying Finish
left_join(
  qualifying %>% 
    group_by(driver.name) %>% 
    summarise(BQF = min(position)) %>% 
    ungroup() #%>% 
    # mutate(BQFTEXT = as.character(case_when(
    #   BQF == 1 ~ paste0(BQF, "ST"),
    #   BQF == 2 ~ paste0(BQF, "ND"),
    #   BQF == 3 ~ paste0(BQF, "RD"),
    #   TRUE ~ paste0(BQF, "TH")
    # )),
    # BQF = 10)
) %>% 
# DNF
left_join(
  results %>%
    filter(positionText == "R") %>% 
    group_by(driver.name) %>% 
    count(name = "DNF")
) %>% 
# Poles
left_join(
  qualifying %>% 
    filter(position == 1) %>% 
    group_by(driver.name) %>% 
    count(name = "Poles")
) %>% 
  mutate(
    Podiums = ifelse(is.na(Podiums), 0, Podiums),
    DNF = ifelse(is.na(DNF), 0, DNF),
    Poles = ifelse(is.na(Poles), 0, Poles)
    ) %>% 
  filter(!is.na(driver.name)) %>% 
  left_join(color) %>% 
  gather(var, key, -driver.name, -colors, -cons.name) %>%
  mutate(label =case_when(
    (var == "BQF" | var == "BRF" & key == 1) ~ "1ST",
    (var == "BQF" | var == "BRF" & key == 2) ~ "2ND",
    (var == "BQF" | var == "BRF" & key == 3) ~ "3RD",
    (var == "BQF" | var == "BRF" & key > 3) ~ paste0(key, "TH"),
    TRUE ~ as.character(key)),
    key = case_when(
      (var == "BQF" | var == "BRF" | var == "Points") ~ 10,
      TRUE  ~ key
    )
  )


## 9.2. Teams

cons_comp <- constructor_standings %>% 
# Podiums
left_join(
  results %>%
    filter(positionOrder < 4) %>% 
    group_by(cons.name) %>% 
    count(name = "Podiums") 
) %>% 
# Poles
left_join(
  qualifying %>% 
    filter(position == 1) %>% 
    group_by(cons.name) %>% 
    count(name = "Poles")
) %>%  
# DNF
left_join(
  results %>%
    filter(positionText == "R") %>% 
    group_by(cons.name) %>% 
    count(name = "DNF")
) %>% 
# # Qualifying
left_join(
  qualifying %>%
    mutate(pass = if_else(position < 11, 1, 0)) %>%
    group_by(cons.name) %>%
    summarise(Qualifying = sum(pass))
) %>% 
# Best Race Finish
left_join(
  results %>% 
    group_by(cons.name) %>% 
    summarise(BRF = min(positionOrder)) %>% 
    ungroup() %>% 
    mutate(BRFTEXT = as.character(case_when(
      BRF == 1 ~ paste0(BRF, "ST"),
      BRF == 2 ~ paste0(BRF, "ND"),
      BRF == 3 ~ paste0(BRF, "RD"),
      TRUE ~ paste0(BRF, "TH")
    )))
)  %>% 
# Best Qualifying Finish
left_join(
  qualifying %>% 
    group_by(cons.name) %>% 
    summarise(BQF = min(position)) %>% 
    ungroup() %>% 
    mutate(BQFTEXT = as.character(case_when(
      BQF == 1 ~ paste0(BQF, "ST"),
      BQF == 2 ~ paste0(BQF, "ND"),
      BQF == 3 ~ paste0(BQF, "RD"),
      TRUE ~ paste0(BQF, "TH")
    )))
) %>% 
  mutate(
    Podiums = ifelse(is.na(Podiums), 0, Podiums),
    Poles = ifelse(is.na(Poles), 0, Poles)
  )
```


## Drivers
```{r}
datatable(drivers)
```

## Constructors
```{r}
datatable(constructors)
```

## Driver Standings
```{r}
datatable(driver_standings)
```

## Constructors Standings
```{r}
datatable(constructor_standings)
```

## Races
```{r}
datatable(races)
```


## Results
```{r}
datatable(results)
```


## Qualifying
```{r}
datatable(qualifying)
```

## Lap Times
```{r}
datatable(lap_times)
```

## Pit Stops
```{r}
datatable(pit_stops)
```







# Driver Standings
<hr>
```{r}
clr <- driver_standings %>% filter(driver.name == "Lewis Hamilton") %>% pull(colors) %>% unique() %>% as.character()

my_vals = unique(driver_standings$driver.name)
my_colors = ifelse(my_vals=="Lewis Hamilton", clr, "")
my_colors2 = ifelse(my_vals=="Lewis Hamilton",'white', "black")

datatable(
  driver_standings %>% 
    filter(round == 17) %>% 
    arrange(-points) %>%
    select(position, driver.number, code, driver.name, wins,points) %>% 
    rename(number = driver.number, driver = driver.name) %>% distinct(),
  options = list(
    dom = 't',
    pageLength = 20,
    scrollX = TRUE
  ),rownames = FALSE
) %>% 
  formatStyle('driver', target = 'row', 
              backgroundColor = styleEqual(my_vals,my_colors),
              color = styleEqual(my_vals,my_colors2)) 
```

```{r fig.width=13, fig.height=8}
driver_standings %>% 
    mutate(colors = if_else(driver.name == "Lewis Hamilton", colors, "gray"),
           circuit = str_replace_all(circuit, " Grand Prix", " GP")) %>% 
    ggplot(aes(reorder(circuit, round), points, color = colors, group = driver.name, alpha = colors, size = colors))+
    geom_line(show.legend = FALSE)+
    scale_color_identity()+
    scale_alpha_manual(values = c(1, .6))+
    scale_size_manual(values = c(1, .3))+
    # Theme
    theme(
      text = element_text(color = "white", family = "Formula1 Display-Regular"),
      axis.text = element_text(color = "white", size = 12, family = "Formula1 Display-Regular"),
      axis.title = element_text(color = "white", size = 15, family = "Formula1 Display-Regular"), 
      title = element_text(size = 15, family = "Formula1 Display-Regular"),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_rect(fill = "black"),
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 70,hjust = 1)
    )+
    # Labs
    labs(x = "Circuit", y = "Points", title = "Driver Standings")
     
```

```{r eval=FALSE, include=TRUE}
driver_standings %>% 
  inner_join(drivers %>% select(driver.name, cons.name))%>% 
  filter(round == input$r) %>% 
  mutate(cars = paste0("www/cars/",cons.name, ".png")) %>% 
  ggplot()+
  
  geom_hline(aes(yintercept = 0),color = "gray", alpha = 0.2)+
  geom_hline(aes(yintercept = 150),color = "gray", alpha = 0.2)+
  geom_hline(aes(yintercept = 300),color = "gray", alpha = 0.2)+
  
  geom_image(aes(x = driver.name, y = points, image = cars),size = 0.2)+
  geom_text(aes(x = driver.name, y = -50, label = driver.name), 
            hjust = 1.15 ,color = "white", family = "Formula1 Display-Regular")+
  
  coord_flip()+
  theme(
    axis.ticks = element_blank(),
    axis.text.x = element_text(color = "white", family = "Formula1 Display-Regular"),
    axis.text.y = element_blank(),
    plot.background = element_rect(fill = "#535152", color = "#535152"),
    panel.background = element_rect(fill = "#535152", color = "#535152"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid = element_line(color = "gray"),
    panel.grid = element_blank(),
    plot.title = element_text(color = "white", family = "Formula1 Display-Regular", hjust = 0.5, size = 20)
  )+
  labs(x = NULL, y = NULL, title = "F1 2020 - Driver Standings")+
  facet_wrap(~round)+
  facet_null() + 
  #scale_y_continuous(position = "bottom",label = c("", "0", "100", "200", "300", "400"))+
  ylim(-300,450)+
  geom_text(x = 1 , y = 320,
            family = "Formula1 Display-Regular",
            aes(label = str_replace_all(as.character(circuit), "Grand Prix", "GP")),
            size = 5, col = "white")+
transition_time(round)

```


<center><img src="https://github.com/EkremBayar/Formula-1-App/raw/main/driver_standings.gif" width="600"/></center>


# Constructor Standings
<hr>

```{r}
clr <- constructor_standings %>% filter(cons.name == "Mercedes") %>% pull(colors) %>% unique() %>% as.character()

my_vals = unique(constructor_standings$cons.name)
my_colors = ifelse(my_vals=="Mercedes", clr, "")
my_colors2 = ifelse(my_vals=="Mercedes",'white', "black")

datatable(
  constructor_standings %>% 
    filter(round == 17) %>% 
    arrange(-points) %>%
    select(position, cons.name, wins,points),
  options = list(
    dom = 't',
    pageLength = 10,
    scrollX = TRUE
  ),rownames = FALSE
) %>% 
  formatStyle('cons.name', target = 'row', 
              backgroundColor = styleEqual(my_vals,my_colors),
              color = styleEqual(my_vals,my_colors2)) 
```

```{r fig.width=13, fig.height=8}
constructor_standings %>% 
    mutate(
      colors = case_when(
        cons.name %in% c("Mercedes", "Red Bull", "McLaren") ~ colors,
        TRUE ~ "gray"
      ) ,
      circuit = str_replace_all(circuit, " Grand Prix", " GP")) %>% 
    ggplot(aes(reorder(circuit, round), points, color = colors, group = cons.name, size = colors, alpha = colors))+
    geom_line(show.legend = FALSE)+
    scale_color_identity()+
    scale_size_manual(values = c(1.5,1.5,1.5,1))+
    scale_alpha_manual(values = c(1,1,1,0.7))+
    # Theme
    theme(
      text = element_text(color = "white", family = "Formula1 Display-Regular"),
      axis.text = element_text(color = "white", size = 12, family = "Formula1 Display-Regular"),
      axis.title = element_text(color = "white", size = 15, family = "Formula1 Display-Regular"), 
      title = element_text(size = 15, family = "Formula1 Display-Regular"),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_rect(fill = "black"),
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      axis.text.x = element_text(angle = 70,hjust = 1)
    )+
    # Labs
    labs(x = "Circuit", y = "Points", title = "Constructor Standings", subtitle = "1.Mercedes \t 2.Red Bull \t 3.McLaren")
```

<center><img src="https://github.com/EkremBayar/Formula-1-App/raw/main/constructor_standings.gif" width="600"/></center>

# Results
<hr>

```{r}
datatable(
    results %>% 
      filter(driver.name == "Lewis Hamilton") %>% 
      select(circuit, grid, positionText, laps, time, milliseconds, fastestLap, 
             rank, fastestLapTime, status, points),
    options = list(
      pageLength = 5,
      scrollX = TRUE,
      dom = 'rtip'
    ),rownames = FALSE
  )
```

```{r fig.width=13, fig.height=8}
lap_times %>%
  filter(driver.name == "Lewis Hamilton",
         !milliseconds %in% boxplot.stats(milliseconds)$out,) %>%
  mutate(circuit = str_replace_all(circuit, " Grand Prix", " GP")) %>% 
  ggplot(aes(reorder(circuit,round), milliseconds, fill = colors, group = round))+
  geom_boxplot(outlier.color = "white", color = "white")+
  scale_fill_identity()+
  theme(
    text = element_text(color = "white", family = "Formula1 Display-Regular"),
    axis.text = element_text(color = "white", size = 12, family = "Formula1 Display-Regular"),
    axis.title = element_text(color = "white", size = 13, family = "Formula1 Display-Regular"), 
    title = element_text(size = 15, family = "Formula1 Display-Regular"),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 70,hjust = 1),
    plot.subtitle = element_text(hjust=0.5)
  )+
  labs(x = "Circuit", y = "Milliseconds", title = "Lap Times")
```


# Qualifying

<hr>

```{r fig.height=8, fig.width=13}
qualifying %>%
    filter(driver.name == "Lewis Hamilton") %>% 
    arrange(round) %>% 
    ggplot()+
    # Q-Line
    geom_hline(aes(yintercept = 3), color ="seagreen", size= 1, linetype =2)+
    geom_hline(aes(yintercept = 10), color = "orange", size= 1, linetype =2)+
    geom_hline(aes(yintercept = 15), color = "red", size= 1, linetype =2)+
    geom_hline(aes(yintercept = 1), color = "white", size= 1, alpha = 0.5)+
    geom_hline(aes(yintercept = 20), color = "white", size= 1, alpha = 0.5)+
    #
    geom_line(aes(round, rmean), alpha = 0.7, color = "white")+
    geom_point(aes(round, pos1), size = 2.5, color = "red")+
    geom_point(aes(round, pos2), color = "orange", size = 2.5)+
    geom_point(aes(round, position, color = colors), size = 4.3)+
  
      # Q-Text
    geom_text(aes(x=1, y = 14), label = "Q1", alpha = 0.07,family = "Formula1 Display-Regular", color = "white")+
    geom_text(aes(x=1, y = 9), label = "Q2", alpha = 0.07,family = "Formula1 Display-Regular", color = "white")+
    geom_text(aes(x=1, y = 2), label = "Q3", alpha = 0.07,family = "Formula1 Display-Regular", color = "white")+
    scale_color_identity()+
    theme(
      panel.grid = element_blank(),
      text = element_text(family = "Formula1 Display-Regular", color = "white", size = 25),
      axis.text = element_text(color = "white", size = 12),
      plot.background = element_rect(fill = "black"),
      panel.background = element_rect(fill = "black"),
      legend.background = element_rect(fill = "black"),
      legend.key = element_rect(fill = "black"),
      legend.text = element_text(color = "white"),
      legend.title = element_text(color = "white"),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    )+
    # Labs
    labs(color = NULL, x = "Round", y = "Position", title = "Qualifying")+
    scale_y_reverse(breaks=1:20, labels = paste0("P", 1:20))+
#    scale_y_continuous()+
    scale_x_continuous(breaks=1:17)
```


```{r}
datatable(
    qualifying %>%
        filter(driver.name == "Lewis Hamilton") %>% 
        arrange(round) %>% 
      select(round:driver.name,cons.name),
    options = list(
      pageLength = 5,
      scrollX = TRUE,
      dom = 'rtip'
    ),rownames = FALSE
  )
```

# Turkish Grand Prix
<hr>

<center><img
src="https://imgresizer.eurosport.com/unsafe/2560x0/filters:format(jpeg)/origin-imgresizer.eurosport.com/2020/11/15/2937217-60300308-2560-1440.jpg">
</center>

<br>

## Lap Times
<hr>

```{r fig.height=8, fig.width=13, message=FALSE, warning=FALSE}
dp <- lap_times %>% 
  filter(circuit == "Turkish Grand Prix") %>% 
  group_by(driver.name) %>% 
  filter(!milliseconds %in% boxplot.stats(milliseconds)$out) %>% 
  ungroup() %>% 
  mutate(colors = if_else(driver.name == "Lewis Hamilton", colors, "gray"))

ggplot()+
    geom_line(filter(dp, driver.name != "Lewis Hamilton"), mapping = aes(lap, milliseconds, label = time, color = colors, group = driver.name, size = colors, alpha = colors), show.legend = FALSE)+
    geom_line(filter(dp, driver.name == "Lewis Hamilton"), mapping = aes(lap, milliseconds, label = time, color = colors, group = driver.name, size = colors, alpha = colors), show.legend = FALSE)+
    scale_y_reverse()+
    scale_color_identity()+
    scale_alpha_manual(values = c(1, .7))+
    scale_size_manual(values = c(1, .3))+
    # Theme
    theme(
      text = element_text(color = "white", family = "Formula1 Display-Regular"),
      axis.text = element_text(color = "white", size = 12, family = "Formula1 Display-Regular"),
      axis.title = element_text(color = "white", size = 15, family = "Formula1 Display-Regular"), 
      title = element_text(size = 15, family = "Formula1 Display-Regular"),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_rect(fill = "black"),
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )+
    # Labs
    labs(x = "Lap", y = "Milliseconds", subtitle = "Outlier Lap Times Removed! \n Lewis Hamilton vs Other Drivers", title = "Turkish Grand Prix Lap Times")
```

```{r fig.height=8, fig.width=13}
lap_times %>% 
    filter(circuit == "Turkish Grand Prix") %>% 
    mutate(
      colors = if_else(driver.name == "Lewis Hamilton", as.character(colors), "gray")
    ) %>% 
    ggplot(aes(lap, position, group = driver.name, color = colors, alpha = colors, size = colors))+
    geom_line(size = 2, show.legend = FALSE)+
    scale_color_identity()+
    scale_alpha_manual(values = c(1, .4))+
    scale_size_manual(values = c(1, .3))+
    scale_y_reverse(breaks = 20:1)+
    theme(
      text = element_text(color = "white",family = "Formula1 Display-Regular"),
      axis.text = element_text(color = "white", size = 12,family = "Formula1 Display-Regular"),
      axis.title = element_text(color = "white", size = 15,family = "Formula1 Display-Regular"), 
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_rect(fill = "black"),
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(hjust = 0.5, size = 20),
      plot.subtitle = element_text(hjust = 0.5, size = 15)
    )+
    labs(x = "Lap", y = "Position", title = "Turkish Grand Prix - Lap Times", subtitle = "Lewis Hamilton vs Other Drivers")
```

 
## Position Change Animation
<hr>

```{r fig.width=9, fig.height=6}
p <- data.frame(y = rep(c(0, 2), 10), x = seq(0,99, 5), position = 20:1)
a <- lap_times %>% filter(driver.name == "Lewis Hamilton", 
                         circuit == "Turkish Grand Prix") %>% 
  select(driver.name,lap, position, colors)
a <- left_join(a, p, by = "position")
aa <- ggplot()+
  geom_point(p, mapping = aes(x,y), shape = 22, size = 20, fill = "gray")+
  geom_text(p, mapping = aes(x,y, label = position))+
  geom_point(a, mapping = aes(x,y, fill = colors, frame = lap), shape = 22, size = 20)+
  geom_text(a, mapping = aes(0,3, label = paste0(" Lap:", lap), frame = lap), size = 8)+
  geom_text(a, mapping = aes(x,y, label = position))+
  scale_fill_identity()+
  ylim(-1,3)+
  xlim(-5, 99)+
  theme(
    text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 20, hjust = 0.5, color = "black"),
    panel.background = element_rect(fill = "lightgray"),
    plot.background = element_rect(fill = "lightgray"),
    panel.grid = element_blank()
    
  )+
  labs(title = "Position Change Animation")
ggplotly(aa, 
         tooltip = c("driver.name","lap")) %>%
  animation_slider(hide=T) %>% 
  hide_legend() %>% 
  config(displayModeBar = F) %>% 
  layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))

```
## Pit Stops
<hr>

```{r fig.height=8, fig.width=13}
ggplot()+
  
  geom_point(
    pit_stops %>% 
      filter(circuit == "Turkish Grand Prix", driver.name != "Lewis Hamilton"), 
    mapping = aes(stop,lap), color = "gray", size = 6, alpha = 0.7)+ 
  geom_line(
    pit_stops %>% 
      filter(circuit == "Turkish Grand Prix", driver.name != "Lewis Hamilton"), 
    mapping = aes(stop,lap, group = driver.name), color = "gray", size = 2, alpha = 0.5)+ 
  
  geom_point(
    pit_stops %>% 
    filter(circuit == "Turkish Grand Prix", driver.name == "Lewis Hamilton"), 
    mapping = aes(stop, lap, color = colors), size = 8)+
  geom_line(
    pit_stops %>% 
    filter(circuit == "Turkish Grand Prix", driver.name == "Lewis Hamilton"), 
    mapping = aes(stop, lap, color = colors), size = 6)+
  geom_text(
    pit_stops %>% 
    filter(circuit == "Turkish Grand Prix", driver.name == "Lewis Hamilton"), 
    mapping = aes(stop, lap, color = colors, label = paste0("Lap: ",lap)), size = 7,
    hjust = -0.2, vjust = 1,,family = "Formula1 Display-Regular"
  )+
  
  scale_color_identity()+
  scale_x_discrete()+
  theme(
    text = element_text(color = "white",family = "Formula1 Display-Regular"),
    axis.text = element_text(color = "white", size = 12,family = "Formula1 Display-Regular"),
    axis.title = element_text(color = "white", size = 15,family = "Formula1 Display-Regular"), 
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 15)
  )+
  labs(x = NULL, y = "LAP", title = "Turkish Grand Prix Pit Stops", subtitle = "Lewis Hamilton vs Other Drivers")+
  coord_flip()
```

# Lewis Hamilton vs Max Verstappen
<hr>


<div style="width: 100%;">
  <div style="float:left; width:35%"><img src="https://merahputih.com/media/9a/cc/78/9acc78fbe23e1ffec2567f5b364dafde.jpg"></img></div>
  <div style="float:right; width:35%"><img src="https://i.imgur.com/ehqnTCh.jpg"></img></div>
</div>
<div style="clear:both;">


```{r pressure, warning = FALSE, echo=FALSE, dev=c('svg')}
driver_comp <- read.csv("../input/f1ekrem/tidy_driver_comparison.csv")

cp <- driver_comp  %>% 
    filter(driver.name %in% c("Lewis Hamilton", "Max Verstappen")) %>% 
    mutate(key = if_else(driver.name == "Lewis Hamilton", -key-.1, key+.1)) 
  
  cp %>% 
    ggplot(aes(var, key, fill = colors, group = driver.name))+
    geom_col(show.legend = FALSE, width = 0.5, color = "#535152", size = 1)+
    geom_text(cp %>% filter(driver.name == "Lewis Hamilton"), mapping = aes(label = label), size = 9, color = "white",
              family = "Formula1 Display-Regular",  position = position_fill(vjust = -20))+
    geom_text(na.rm = F, cp %>% filter(driver.name ==  "Max Verstappen"), mapping = aes(label =label), size = 9, color = "white",
              family = "Formula1 Display-Regular",  position = position_fill(vjust = 20))+
    scale_fill_identity()+
    coord_flip()+
    theme(
      panel.background = element_rect(fill = "#535152", color = "#535152"),
      plot.background = element_rect("#535152",  color = "#535152"),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank()
    )+
    annotate("text", x = 9.5, y = 0, label = "WINS", family = "Formula1 Display-Regular", color = "white", size = 6)+
    annotate("text", x = 8.5, y = 0, label = "RACES", family = "Formula1 Display-Regular", color = "white", size = 6)+
    annotate("text", x = 7.5, y = 0, label = "QUALIFYING", family = "Formula1 Display-Regular", color = "white", size = 6)+
    annotate("text", x = 6.5, y = 0, label = "POLES", family = "Formula1 Display-Regular", color = "white", size = 6)+
    annotate("text", x = 5.5, y = 0, label = "POINTS", family = "Formula1 Display-Regular", color = "white", size = 6)+
    annotate("text", x = 4.5, y = 0, label = "PODIUMS", family = "Formula1 Display-Regular", color = "white", size = 6)+
    annotate("text", x = 3.5, y = 0, label = "DNF", family = "Formula1 Display-Regular", color = "white", size = 6)+
    annotate("text", x = 2.5, y = 0, label = "BEST RACE FINISH", family = "Formula1 Display-Regular", color = "white", size = 6)+
    annotate("text", x = 1.5, y = 0, label = "BEST QUALIFYING FINISH", family = "Formula1 Display-Regular", color = "white", size = 6)
```


