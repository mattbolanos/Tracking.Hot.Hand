#Load packages
library(tidyverse)
library(extrafont)
library(shinyWidgets)
library(shiny)
library(htmltools)
library(hexbin)
library(scales)
library(prismatic)
library(cowplot)
library(png)
library(grid)
library(readr)
library(nbastatR)
library(data.table)
library(future)
library(furrr)
library(gt)
library(scales)
library(paletteer)
library(RPostgres)
library(DBI)
library(shinycssloaders)
library(DT)
library(reshape2)
library(janitor)

#Connect to db
db <- 'postgres' 

host_db <- [redacted]

db_port <- [redacted]    

db_user <- [redacted]  

db_password <- [redacted]  

con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)  

input_columns <- dbGetQuery(con, "SELECT shooter,slug_season FROM streak_names_and_years")

plan('multicore') 

#User interface
ui <- navbarPage(
  "NBA Hot Hand Analysis",
  tabPanel("Welcome",
           strong("NBA Hot Hand Analysis",
                  style = "font-size:35px; font-family:Georgia; color:black"),
           br(),
           p("By Matt Bolaños | matthew.a.bolanos@gmail.com |", a("Portfolio", href="https://www.matthewabolanos.com/", target="_blank"),
                  style = "font-size:16px;font-family:Georgia; color:black"),
           strong("Introduction", style = "font-size:24px;font-family:Georgia; color:black; font-style: italic"),
           br(),
           p("This application summarizes and visualizes some of my research 
             surrounding the infamous “Hot Hand Fallacy” in basketball. 
             The fallacy stems from the idea that a player has an increased 
             probability of making their next shot if they are “on fire”, or in other 
             words have made their previous couple of shots.",
             style= "font-size:16px; font-family:Arial;color:black"),
           p("There have been numerous studies dedicated to investigating the Hot Hand, 
             from the pioneers of Gilovich, Vallone & Tversky (1985) to more 
             contemporary analyses by Miller & Sanjuro (2018). The former’s 
             results were consistent with the long-standing notion that the percentage 
             of made shots after a string of made shots is not significantly higher, 
             with the latter taking the other side and providing evidence that 
             the hot hand is statistically consistent with expectations. 
             However, Miller & Sanjuro’s Bayesian application is better 
             utilized in scenarios that are random chance (e.g. flipping a coin) 
             rather than skill performance (e.g. muscle memory).",
             style= "font-size:16px; font-family:Arial;color:black"),
           p("In the past, I have analyzed NBA shot data from the ESPN play-by-play 
             archives to investigate player field goal percentages after strings 
             of misses and makes. My work did not yield evidence that shooters 
             are statistically more likely to make their next shot after a 
             string of makes or misses. However, there was minimal evidence of 
             a “rubber band” effect where some of the best shooters in the league 
             had a significantly greater chance of making their next shot after 
             missing 3 or more shots in a row. Overall, my findings were 
             consistent with that of Gilovich, Vallone & Tversky. Eighteen months 
             later, after coming across some SportVU Game Logs from the 2015-16 
             NBA Season, I started re-working my Hot Hand analysis.",
             style= "font-size:16px; font-family:Arial;color:black"),
        
           strong("Tab 1: 2015-21 Shot Charts",style = "font-size:24px;font-family:Georgia; color:black; font-style: italic"),
           br(),
           p("The first tab of this application is a visualization tool that 
             displays how players shoot relative to league average from every 
             spot on the court, after a particular streak. To keep bins from 
             getting too small (players don’t often make 7 shots in a row), 
             I categorized shots as being one of 1, 2, or 3+ Makes or Misses. 
             In an attempt to compare apples to apples, players are compared to 
             league average after the same streak, rather than the base league 
             average (e.g. Player X after 3+ Makes is compared to how the league 
             shot after 3+ Makes).",
             style= "font-size:16px; font-family:Arial;color:black"),
           p("This tool by itself falls prey to the same limitations that all 
           shot charts do, in the sense that the dots on the court contain no 
           information about how that shot transpired. So while it may be useful 
           for opponents to know that Klay Thompson had a +15 FG% relative to 
             league average on all above the break 3s after 3+ Makes in 2016-17, 
             there is still much more information to be gathered about the sets 
             and actions used by the Warriors to create the attempts for Klay, 
             or the defensive scheme ran against each shot. Joining these shots 
             locations with the corresponding film clips would be a logical 
             extension of this analysis.",
             style= "font-size:16px; font-family:Arial;color:black"),
           
           strong("Tab 2: 2015-16 SportVU Tracking Hot Hand",style = "font-size:24px;font-family:Georgia; color:black; font-style: italic"),
           br(),
           p("The second tab of this application summarizes my results of joining 
             the 2015-16 SportVU Game Logs with shot location data. What made 
             the player tracking data so interesting to apply to the Hot Hand is 
             that I could add the nuance of closest defender distance into my 
             calculations. This boils the fallacy down into its basic parts, 
             helping inch towards the question: “Does Player X have a higher chance of 
             making more difficult shots after a string of makes?”", 
             style= "font-size:16px; font-family:Arial;color:black"),
           p("As I began my research, I quickly noticed the high frequency of 
             discrepancies between the SportVU logs and shot location data. 
             This, on top of the data lacking information about when exactly 
             each shot occurred and random instances of the game clock column 
             being off by as much as a minute, presented interesting challenges. 
             Needing the correct shot time was crucial for my investigation, as 
             it is the only way to calculate closest defender distance from the 
             raw tracking data. The times in the shot locations dataset unfortunately 
             originate from play-by-play data, which typically list an event’s 
             time approximately 2 or 3 seconds after a shot occurs. In order to 
             find shot times, I created a proxy. My proxy was built from the 
             distance between the ball and the shooter, along with the play-by-play 
             event time. For each shot, I took the latest time (minimum on game clock) 
             where the Euclidean distance between ball and shooter was less than 1.5 
             while the game clock was still before the play-by-play event time. 
             This method yielded pretty accurate results upon cross-validating my 
             generated shot times with Synergy game film. However, some of the 
             SportVU data was particularly messy, with game clock columns 
             borderline unsalvageable. A characteristic of these messy logs 
             were a large difference between the actual amount of shots taken 
             in the game (amount of shots reflected in the shot location data) 
             and the amount of shots generated by my proxy. To account for this, 
             I created a “quality” threshold where my program only accepted shots 
             from games where this difference was below 30. One of my principles 
             for this project was quality over quantity, as I would rather 
             have 10,000 shots with accurate closest defender distances than 
             100,000 shots with inaccurate defender distances.",
             style= "font-size:16px; font-family:Arial;color:black"),
           p("Of course, there has been much discourse over the accuracy of 
             defender distances in general, even with the private Second Spectrum 
             employed by the league today. Defender wingspan and late contests 
             can make one “Open” look a lot more contested than a different “Open” 
             one. Additionally, different arenas have been known to have more or 
             less variation in their tracking cameras, further muddying up the 
             information that can be taken away. My proxy is far from perfect, 
             much like how the current way of measuring openness of shots 
             withholds flaws. My program ended up recording 46,405 shots from 
             the 636 available game logs. Even if my program perfectly recorded 
             every single shot from those 636 games, the sample sizes are quite 
             small here. The game logs range from the first game of the 15-16 
             season in October up to January 23rd, so at best this is about half 
             a season of data. In an attempt to make the bins at least a little 
             bigger, I condensed the commonplace coverage bins of Wide Open, Open, 
             Tight, and Very Tight to Open and Tight. One silver lining is many 
             players have enough total FGAs in this dataset to reach the “stabilization” 
             point of FG%, which is approximately ~102 attempts based on Kostya Medvedovsky’s work. 
             Another caveat I will mention here is not all “Tight” attempts are 
             created equal. A Tight attempt from 24 feet is a much tougher shot 
             than a Tight attempt from 4 feet away. However the samples would 
             become even smaller if I filtered out attempts from a certain 
             threshold of shot distance, so I decided against this.",
             style= "font-size:16px; font-family:Arial;color:black"),
           strong("Acknowledgements",style = "font-size:20px;font-family:Georgia; color:black; font-style: italic"),
           br(),
           p("The SportVU game logs I used are courtesy of", a("Neil Seward", href="https://github.com/sealneaward", target="_blank"), "at his 
             GitHub. Neil also provided some great frameworks for cleaning and 
             extracting the game logs that I used in my process. Shot location data 
             came way of", a("Alex Bresler", href="https://github.com/abresler", target="_blank"), "and nbastatR.",
             style= "font-size:16px; font-family:Arial;color:black"),
           p("I drew a lot of inspiration from various titans in the analytics space. I’ve already mentioned", 
           a("Kostya", href="https://kmedved.com/2020/08/06/nba-stabilization-rates-and-the-padding-approach/", target="_blank"), "and his work about padding and 
             stabilization. The format of his and Andrew Patton’s current", 
             a("DARKO",href = "https://apanalytics.shinyapps.io/DARKO/", target="_blank"),
             "player projections inspired how I wanted to build my table 
             in the second tab.",a("Owen Phillips", href = "https://thef5.substack.com/people/479475-owen-phillips", target="_blank"), "over at his Substack has a great 
             tutorial on working with hexbin data with R and using it for shot 
             charts, and great R tutorials in general. Finally, thank you to the
             groundbreakers of Gilovich and co who took the first stab at 
             investigating the Hot Hand phenomenon.",
             style= "font-size:16px; font-family:Arial;color:black")),
  
  tabPanel("Streak Shooting by Zone: 2015-2021",
           strong("Version: 2021-06-29",style = "font-size:16px;font-family:Georgia; color:red"),
           strong("Created by Matt Bolaños | matthew.a.bolanos@gmail.com",style = "font-size:14px;font-family:Georgia; color:black"),
           br(),
           p("**CHART AND TABLE WILL TAKE A FEW MOMENTS TO LOAD**",
             style = "font-size:26px;font-family:Georgia; color:black"),
           sidebarLayout(sidebarPanel(pickerInput('player', 'Player', 
                                                  choices = c(unique(input_columns$shooter)),
                                                  selected = "Klay Thompson", 
                                                  options = pickerOptions(liveSearch=T,
                                                                          liveSearchStyle = 'contains'), 
                                                  width = '100%'),
                                      pickerInput('year', 'Season(s)', 
                                                  choices = c("2015-2021", unique(input_columns$slug_season)),
                                                  selected = '2016-17', width = '100%', multiple = FALSE),
                                      pickerInput('streak', 'Consecutive Makes/Misses',
                                                  choices = c("1 Make", "2 Makes", "3+ Makes", 
                                                              "1 Miss", "2 Misses", "3+ Misses"),
                                                  selected = "3+ Makes", width = '100%'),
                                      
                                      withSpinner(gt_output("table1")),
                                      width = 4),
                         mainPanel(withSpinner(plotOutput("plot1")), 
                                   width = 6))),
  tabPanel("Streak Shooting and Defender Distance: 2015-16",
           strong("Version: 2021-06-29",style = "font-size:16px;font-family:Georgia; color:red"),
           strong("Created by Matt Bolaños | matthew.a.bolanos@gmail.com",style = "font-size:14px;font-family:Georgia; color:black"),
           br(),
           br(),
           strong("**Select Consecutive Makes/Misses**",style = "font-size:26px;font-family:Georgia; color:black"),
           mainPanel(pickerInput('streak2',
                                  choices = c("1 Make", "2 Makes", "3+ Makes", 
                                              "1 Miss", "2 Misses", "3+ Misses"),
                                  selected = "3+ Makes", width = '50%'),
                     br(),
                     withSpinner(DT::dataTableOutput("table2")),
                     width = 15)))
           #tags$head(tags$style(HTML( '.has-feedback .form-control { padding-right: 0px;}'
                      
  
  
  
  
#Server
server <- function(input, output) {
  
  plan('multicore') 
  
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })
  
  
  
  players <- nba_players() %>% 
    filter(yearSeasonLast > 2014) %>% 
    select(urlPlayerHeadshot,namePlayer)
  
  #Draw Court h/t https://thef5.substack.com/people/479475-owen-phillips
  circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
    angles = seq(0, 2 * pi, length.out = npoints)
    return(data_frame(x = center[1] + radius * cos(angles),
                      y = center[2] + radius * sin(angles)))
  }
  
  width = 50
  height = 94 / 2
  key_height = 19
  inner_key_width = 12
  outer_key_width = 16
  backboard_width = 6
  backboard_offset = 4
  neck_length = 0.5
  hoop_radius = 0.75
  hoop_center_y = backboard_offset + neck_length + hoop_radius
  three_point_radius = 23.75
  three_point_side_radius = 22
  three_point_side_height = 14
  
  court_themes = list(
    light = list(
      court = 'antiquewhite',
      lines = '#999999',
      text = '#222222',
      made = '#00bfc4',
      missed = '#f8766d',
      hex_border_size = 1,
      hex_border_color = "#000000"
    ),
    dark = list(
      court = '#000004',
      lines = '#999999',
      text = '#f0f0f0',
      made = '#00bfc4',
      missed = '#f8766d',
      hex_border_size = 0,
      hex_border_color = "#000000"
    )
  )
  
  
  plot_court = function(court_theme = court_themes$light, use_short_three = FALSE) {
    if (use_short_three) {
      three_point_radius = 22
      three_point_side_height = 0
    }
    
    court_points = data_frame(
      x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
      y = c(height, 0, 0, height, height),
      desc = "perimeter"
    )
    
    court_points = bind_rows(court_points , data_frame(
      x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
      y = c(0, key_height, key_height, 0),
      desc = "outer_key"
    ))
    
    court_points = bind_rows(court_points , data_frame(
      x = c(-backboard_width / 2, backboard_width / 2),
      y = c(backboard_offset, backboard_offset),
      desc = "backboard"
    ))
    
    court_points = bind_rows(court_points , data_frame(
      x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
    ))
    
    foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
    
    foul_circle_top = filter(foul_circle, y > key_height) %>%
      mutate(desc = "foul_circle_top")
    
    foul_circle_bottom = filter(foul_circle, y < key_height) %>%
      mutate(
        angle = atan((y - key_height) / x) * 180 / pi,
        angle_group = floor((angle - 5.625) / 11.25),
        desc = paste0("foul_circle_bottom_", angle_group)
      ) %>%
      filter(angle_group %% 2 == 0) %>%
      select(x, y, desc)
    
    hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
      mutate(desc = "hoop")
    
    restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
      filter(y >= hoop_center_y) %>%
      mutate(desc = "restricted")
    
    three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
      filter(y >= three_point_side_height, y >= hoop_center_y)
    
    three_point_line = data_frame(
      x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
      y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
      desc = "three_point_line"
    )
    
    court_points = bind_rows(
      court_points,
      foul_circle_top,
      foul_circle_bottom,
      hoop,
      restricted,
      three_point_line
    )
    
    
    court_points <- court_points
    
    ggplot() +
      geom_path(
        data = court_points,
        aes(x = x, y = y, group = desc),
        color = court_theme$lines
      ) +
      coord_fixed(ylim = c(0, 45), xlim = c(-25, 25)) +
      theme_minimal(base_size = 22) +
      theme(
        text = element_text(color = court_theme$text),
        plot.background = element_rect(fill = 'antiquewhite', color = 'antiquewhite'),
        panel.background = element_rect(fill = court_theme$court, color = court_theme$court),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = court_theme$court, color = court_theme$court),
        legend.margin = margin(-1, 0, 0, 0, unit = "lines"),
        legend.position = "bottom",
        legend.key = element_blank(),
        legend.text = element_text(size = rel(1.0))
      )
  }
  
  
  #First tab plot
  output$plot1 <- renderPlot({
    
    
    
    
    vals = reactiveValues(
      player = "",
      year = "",
      streak = ""
    )
    
    
    vals$player <- as.vector(input$player)
    vals$year <- if ((input$year)== "2015-2021") unique(as.vector(input_columns$slug_season)) else as.vector(input$year)
    vals$streak <- as.vector(input$streak)
    
    player_sel <-  gsub("'","''",vals$player)
    year_sel <- paste("(",toString(paste("'",vals$year,"'", sep='')),")", sep='')
    streak_sel <- vals$streak
   
    input_query <- paste0("SELECT location_x, location_y, distance_shot, 
                is_shot_made, is_shot_attempted, type_shot, zone_range, name_zone 
                FROM streak_shots_nba WHERE shooter='", player_sel, "'", "
                AND slug_season IN", year_sel, "", "AND streak_col = '", streak_sel, "'")
    
    shots <- dbGetQuery(con, input_query)
    
    
    
    player_shots <- shots %>% 
      mutate(loc_x = location_x / 10,
             loc_y = location_y / 10 + hoop_center_y,
             shot_distance = distance_shot,
             shot_made_numeric = as.numeric(is_shot_made),
             shot_made_flag = factor(is_shot_made, levels = c("TRUE", "FALSE"), labels = c("made", "missed")),
             shot_attempted_flag = as.numeric(is_shot_attempted),
             shot_value = ifelse(tolower(type_shot) == "3pt field goal", 3, 2)) %>% 
      subset(select = -c(location_x, location_y, is_shot_made,is_shot_attempted)) %>% 
      as.data.frame()
    

    headshot <- players %>%
      filter(namePlayer == vals$player) %>%
      select(urlPlayerHeadshot) %>%
      as.vector()
    
    myurl <- headshot$urlPlayerHeadshot
    z <- tempfile()
    download.file(myurl,z,mode="wb")
    pic <- readPNG(z)
    file.remove(z)
    
    if (nrow(player_shots) != 0){
      
      league_query <- paste0("SELECT is_shot_attempted, is_shot_made, zone_basic, 
                             zone_range, name_zone FROM streak_shots_nba WHERE slug_season IN", year_sel, "", 
                             "AND streak_col = '", streak_sel, "'")
      
      league_shots <- dbGetQuery(con, league_query)
      
      league_averages <- league_shots %>% 
        mutate(fga = as.numeric(is_shot_attempted),
               fgm = as.numeric(is_shot_made),
               shot_value = ifelse(zone_basic %in% c("Above the Break 3", "Backcourt", "Left Corner 3", "Right Corner 3"), 3, 2))
      
      #hexbin calcs h/t https://thef5.substack.com/people/479475-owen-phillips
      hex_bounds <- function(x, binwidth) {
        c(
          plyr::round_any(min(x), binwidth, floor) - 1e-6,
          plyr::round_any(max(x), binwidth, ceiling) + 1e-6
        )
      }
      
      xbnds = hex_bounds(player_shots$loc_x, 1.5)
      xbins = diff(xbnds) / 1.5
      ybnds = hex_bounds(player_shots$loc_y, 1.5)
      ybins = diff(ybnds) / 1.5
      
      hb = hexbin(
        x = player_shots$loc_x,
        y = player_shots$loc_y,
        xbins = xbins,
        xbnds = xbnds,
        ybnds = ybnds,
        shape = ybins / xbins,
        IDs = TRUE
      )
      
      player_shots <- player_shots %>% 
        mutate(hexbin_id = hb@cID)
      
      hexbin_stats = player_shots %>%
        group_by(hexbin_id) %>%
        summarize(
          hex_attempts = n(),
          hex_pct = mean(shot_made_numeric),
          hex_points_scored = sum(shot_made_numeric * shot_value),
          hex_points_per_shot = mean(shot_made_numeric * shot_value)
        ) 
      
      hexbin_ids_to_zones = player_shots %>%
        group_by(hexbin_id, zone_range, name_zone) %>%
        summarize(attempts = n()) %>%
        ungroup() %>%
        arrange(hexbin_id, desc(attempts)) %>%
        group_by(hexbin_id) %>%
        filter(row_number() == 1) %>%
        select(hexbin_id, zone_range, name_zone)
      
      hexbin_stats = inner_join(hexbin_stats, hexbin_ids_to_zones, by = "hexbin_id")
      
      
      sx = hb@xbins / diff(hb@xbnds)
      sy = (hb@xbins * hb@shape) / diff(hb@ybnds)
      dx = 1 / (2 * sx)
      dy = 1 / (2 * sqrt(3) * sy)
      origin_coords = hexcoords(dx, dy)
      
      hex_centers = hcell2xy(hb)
      
      hexbin_coords = bind_rows(lapply(1:hb@ncells, function(i) {
        data.frame(
          x = origin_coords$x + hex_centers$x[i],
          y = origin_coords$y + hex_centers$y[i],
          center_x = hex_centers$x[i],
          center_y = hex_centers$y[i],
          hexbin_id = hb@cell[i]
        )
      }))
      
      hexbin_coords <- inner_join(hexbin_coords, hexbin_stats, by = "hexbin_id")
      
      grouped_shots <- player_shots %>% 
        group_by(zone_range, name_zone)
      
      zone_stats <- grouped_shots %>%
        summarize(
          zone_attempts = n(),
          zone_pct = mean(shot_made_numeric),
          zone_points_scored = sum(shot_made_numeric * shot_value),
          zone_points_per_shot = mean(shot_made_numeric * shot_value)
        )
      
      zone_sum <- zone_stats %>% 
        ungroup() %>% 
        select(zone_range,name_zone, zone_pct, zone_points_per_shot) %>% 
        mutate(across(zone_pct, ~ round(., 2)),
               across(zone_points_per_shot, ~ round(., 2))) %>% 
        as.data.frame()
      
      league_zone_stats <- league_averages %>%
        group_by(zone_range, name_zone) %>%
        summarize(league_pct = sum(fgm) / sum(fga))
      
      hex_data <- hexbin_coords
      
      join_keys = c("zone_range", "name_zone")
      
      hex_data = hex_data %>%
        inner_join(zone_stats, by = join_keys) %>%
        inner_join(league_zone_stats, by = join_keys)
      
      max_hex_attempts = max(hex_data$hex_attempts)
      
      hex_data = mutate(hex_data,
                        radius_factor = .25 + (1 - .25) * log(hex_attempts + 1) / log(max_hex_attempts + 1),
                        adj_x = center_x + radius_factor * (x - center_x),
                        adj_y = center_y + radius_factor * (y - center_y),
                        bounded_fg_diff = pmin(pmax(zone_pct - league_pct, -.15), .15),
                        bounded_fg_pct = pmin(pmax(zone_pct, .2), .7),
                        bounded_points_per_shot = pmin(pmax(zone_points_per_shot, .5), 1.5),
                        adj_x = adj_x*-1)
      
      
     
        
        chart <- plot_court(court_themes$light) +
        geom_polygon(
          data = hex_data,
          aes(
            x = adj_x,
            y = adj_y,
            group = hexbin_id, 
            fill = bounded_fg_diff, 
            color = after_scale(clr_darken(fill, .333))),
          size = .3) + 
        scale_x_continuous(limits = c(-27.5, 27.5)) + 
        scale_y_continuous(limits = c(0, 62)) +
        scale_fill_distiller(direction = -1, 
                             palette = "RdBu", 
                             limits = c(-.15, .15), 
                             breaks = seq(-.15, .15, .03),
                             labels = c("-15%", "-12%", "-9%", "-6%", "-3%", "0%", "+3%", "+6%", "+9%", "+12%", "+15%"),
                             paste0("FG Percentage Points vs. League Average After ", input$streak)) +
        guides(fill=guide_legend(
          label.position = 'bottom', 
          title.position = 'top', 
          keywidth=.45,
          keyheight=.15, 
          default.unit="inch", 
          title.hjust = .5,
          title.vjust = -1,
          label.vjust = 3,
          nrow = 1))  +
        theme(text=element_text(size=16,  family= "Consolas"), 
              legend.spacing.x = unit(0, 'cm'), 
              legend.title=element_text(size=14), 
              legend.text = element_text(size = rel(.75)), 
              legend.margin=margin(-10,0,-1,0),
              legend.position = 'bottom',
              legend.box.margin=margin(-30,0,15,0), 
              plot.title = element_text(hjust = 0.5, vjust = -1, face = "bold", size=20),
              plot.subtitle = element_text(hjust = 0.5, size = 15, vjust = -.5), 
              plot.caption = element_text(face = "italic", size = 8), 
              plot.margin = margin(0, -5, 0, -5, "cm"))+
        labs(title = paste0(input$player, " After ", input$streak),
             subtitle = paste0(input$year, " Regular Season"))
      
      ggdraw(chart) + 
        theme(plot.background = element_rect(fill="antiquewhite", color = NA))+
        annotation_raster(pic, ymin = .85,ymax= 1,xmin = 0.01,xmax = .15)
      
    }else {
      empty_data <- plot_court(court_themes$light) +
        geom_text(size=12,aes(0, 40,
                             label = "No Shots Found"), family='Consolas')
      
      ggdraw(empty_data) +
        theme(plot.background = element_rect(fill="antiquewhite", color = NA)) + 
        annotation_raster(pic, ymin = .85,ymax= 1,xmin = 0.01,xmax = .15)
    }
      
    
    
    
    
    
  }, height = 550,width = 800)
  
  

  
  
  
  #First tab gt table
  output$table1 <- render_gt({
      
      
      vals = reactiveValues(
        player = "",
        year = "",
        streak = ""
      )
      
      
      vals$player <- as.vector(input$player)
      vals$year <- if ((input$year)== "2015-2021") unique(as.vector(input_columns$slug_season)) else as.vector(input$year)
      vals$streak <- as.vector(input$streak)
      
      
      player_sel <-  gsub("'","''",vals$player)
      year_sel <- paste("(",toString(paste("'",vals$year,"'", sep='')),")", sep='')
      streak_sel <- vals$streak
      
      query <- paste0("SELECT location_x, location_y, distance_shot, 
                is_shot_made, is_shot_attempted, type_shot, zone_range, name_zone
                FROM streak_shots_nba WHERE shooter='", player_sel, "'", "
                AND slug_season IN", year_sel, "", "AND streak_col = '", streak_sel, "'")
      
      shots <- dbGetQuery(con, query)
      
      
      league_query <- paste0("SELECT is_shot_attempted, is_shot_made, zone_basic, 
                             zone_range, name_zone 
                             FROM streak_shots_nba WHERE slug_season IN", year_sel, "", 
                             "AND streak_col = '", streak_sel, "'")
      
      league_shots <- dbGetQuery(con, league_query)
      
      player_shots <- shots %>% 
        mutate(loc_x = location_x / 10,
               loc_y = location_y / 10 + 5.25,
               shot_distance = distance_shot,
               shot_made_numeric = as.numeric(is_shot_made),
               shot_made_flag = factor(is_shot_made, levels = c("TRUE", "FALSE"), labels = c("made", "missed")),
               shot_attempted_flag = as.numeric(is_shot_attempted),
               shot_value = ifelse(tolower(type_shot) == "3pt field goal", 3, 2)) %>% 
        subset(select = -c(location_x, location_y, is_shot_made,is_shot_attempted)) %>% 
        as.data.frame()
      
      grouped_shots <- player_shots %>% 
        group_by(zone_range, name_zone)
      
      zone_stats <- grouped_shots %>%
        summarize(
          zone_attempts = n(),
          zone_pct = mean(shot_made_numeric),
          zone_points_scored = sum(shot_made_numeric * shot_value),
          zone_points_per_shot = mean(shot_made_numeric * shot_value)
        )
      
      league_averages <- league_shots %>% 
        mutate(fga = as.numeric(is_shot_attempted),
               fgm = as.numeric(is_shot_made),
               shot_value = ifelse(zone_basic %in% c("Above the Break 3", "Backcourt", "Left Corner 3", "Right Corner 3"), 3, 2))
      
      league_zone_stats <- league_averages %>%
        group_by(zone_range, name_zone) %>%
        summarize(league_pct = sum(fgm) / sum(fga),
                  zone_points_per_shot = mean(fgm * shot_value))
      
      league_zone_sum <- league_zone_stats %>% 
        ungroup() %>% 
        select(zone_range,name_zone, zone_points_per_shot) %>% 
        mutate(league_pps = zone_points_per_shot,
               across(league_pps, ~ round(., 2))) %>% 
        subset(select = -c(zone_points_per_shot)) %>% 
        as.data.frame() 
      
      zone_sum <- zone_stats %>% 
        ungroup() %>% 
        select(zone_range,name_zone, zone_pct, zone_points_per_shot) %>% 
        mutate(across(zone_pct, ~ round(., 2)),
               across(zone_points_per_shot, ~ round(., 2))) %>% 
        as.data.frame() %>% 
        left_join(league_zone_sum, by = c("zone_range", "name_zone")) %>% 
        mutate(dist = case_when(zone_range %like% "16-24" ~ 18,
                                zone_range %like% "24+" ~ 24,
                                zone_range %like% "8-16" ~ 12,
                                zone_range %like% "Back Court" ~ 40,
                                zone_range %like% "Less Than" ~ 4)) %>% 
        arrange(dist) %>% 
        subset(select=-c(dist)) %>% 
        as.data.frame() 
      
      zone_sum %>% 
        gt()  %>% 
        cols_label(zone_range = "Distance",
                   name_zone = "Zone",
                   zone_pct = "FG%",
                   zone_points_per_shot = "PPS",
                   league_pps = "LA PPS") %>% 
        data_color(
          columns = vars(zone_points_per_shot),
          colors = col_numeric(
            palette =paletteer_d(
              palette = "RColorBrewer::YlOrRd",
              direction  = 1
            ) %>% as.character(),
            domain = c(0, 3), 
            na.color = "#ff0000"
          )
        ) %>%
        data_color(
          columns = vars(league_pps),
          colors = col_numeric(
            palette =paletteer_d(
              palette = "RColorBrewer::YlOrRd",
              direction  = 1
            ) %>% as.character(),
            domain = c(0, 3), 
            na.color = "#ff0000"
          )
        ) %>%
        data_color(
          columns = vars(zone_pct),
          colors = col_numeric(
            palette =paletteer_d(
              palette = "RColorBrewer::YlOrRd",
              direction  = 1
            ) %>% as.character(),
            domain = c(0, 1), 
            na.color = "#ff0000"
          )
        ) %>%
        tab_options(
          table.background.color = "floralwhite",
          column_labels.font.size = 12,
          table.font.size = 11,
          heading.title.font.size  = 15,
          heading.title.font.weight = "bold",
          heading.subtitle.font.size = 11,
          heading.subtitle.font.weight = "italic",
          table.font.names = "Consolas", 
          table.font.color = "black",
          table.border.top.color = "transparent",
          data_row.padding = px(2)
        ) %>% cols_width(vars(zone_pct, 
                              zone_points_per_shot,league_pps) ~ px(60),
                         vars(zone_range) ~ px(100),
                         vars(name_zone) ~ px(75)) %>%
        opt_align_table_header(align = "left")  %>% 
        tab_header(
          title = paste0("  FG% and Points Per Shot by Zone After ", vals$streak),
          subtitle = paste0("  LA = League Average"))
      
      
      
      
    })

  
  
  #Second tab table
  output$table2 <- DT::renderDataTable({
    
    
    vals = reactiveValues(
      streak = ""
    )
    
    vals$streak <- as.vector(input$streak2)
    
    streaky_sel <- vals$streak
    
    tab_query <- paste0("SELECT shooter,shooter_team_name, streak_col, type_shot,zone_basic,name_zone,
         zone_range, distance_shot, is_shot_attempted, is_shot_made,
         closest_defender_distance_bin FROM hhand_shots_tracking WHERE streak_col = '", streaky_sel, "'")
    
    all_shots <- dbGetQuery(con, tab_query)
    
    total_shots <- dbGetQuery(con, "SELECT shooter, closest_defender_distance_bin,
                              is_shot_attempted FROM hhand_shots_tracking")
    
    total_fgas <- total_shots %>% 
      mutate(defender_distance= case_when(closest_defender_distance_bin == "Wide Open" ~ "Open",
                                          closest_defender_distance_bin == "Very Tight" ~ "Tight",
                                          TRUE ~ closest_defender_distance_bin)) %>% 
      group_by(shooter) %>% 
      summarise(total_fga = sum(is_shot_attempted),
                total_tight_attempts = sum(defender_distance =="Tight"),
                percent_fga_tight = total_tight_attempts/total_fga) 
    
    
    
    teams <- all_shots %>% 
      select(shooter_team_name, shooter) %>% 
      group_by(shooter) %>% 
      slice(n()) %>% 
      ungroup()
    
    
    averages <- all_shots %>% 
      group_by(shooter,streak_col) %>% 
      summarise(fgm = sum(is_shot_made),
                fga = sum(is_shot_attempted),
                threes = sum(type_shot %like% "3PT"),
                base_streak_efg = (fgm+(.5*threes))/fga) %>% 
      select(shooter, base_streak_efg)
    
    final_data <- all_shots %>% 
      mutate(defender_distance= case_when(closest_defender_distance_bin == "Wide Open" ~ "Open",
                                          closest_defender_distance_bin == "Very Tight" ~ "Tight",
                                          TRUE ~ closest_defender_distance_bin)) %>% 
      group_by(shooter, streak_col, defender_distance) %>%
      
      summarise(fgm = sum(is_shot_made),
                fga = sum(is_shot_attempted),
                fg_percent = fgm/fga,
                threes = sum(type_shot %like% "3PT"),
                efg_percent = (fgm+(.5*threes))/fga) %>% 
      ungroup() %>% 
      select(shooter,streak_col, defender_distance, efg_percent,fga) %>%
      pivot_wider(names_from = c("defender_distance","streak_col"),
                  values_from = c("efg_percent", "fga"),values_fill = 0, names_sep = "_") %>%
      left_join(averages, by = c("shooter")) %>%
      clean_names() %>%
      left_join(total_fgas, by = c("shooter")) %>%
      mutate(streak_fga=Reduce("+",.[4:5]),
             coverage_diff = Reduce("-",.[3:2])
      ) %>%
      mutate_if(is.numeric, round,digits=4) %>% 
      left_join(teams, by = c("shooter")) %>% 
      select(12,1,7,10,9,6,2,3,11) %>% 
      arrange(desc(streak_fga))
    
    #Configure color gradient
    color_col <-final_data[6:6]
    
    brks <- quantile(color_col, probs = seq(0, 1, .01), na.rm = TRUE))
    max_val <-max(color_col,na.rm=TRUE)
    
    clrs_rmp <- colorRamp(c("lightskyblue","red"))(c(0,brks/max_val))
    
    clrs_df <- clrs_rmp %>% 
        as_tibble(.name_repair ="minimal") %>% 
        setNames(nm=c("r","g","b")) %>% 
        mutate_all(~as.character(round(.,digits=0)))  %>% mutate(mycolor=paste0("rgb(",
                                                                                paste(r,g,b,sep = ","),
                                                                                ")"))
    clrs <- pull(clrs_df,mycolor)
    
    
    
    DT::datatable(final_data,filter = "top",rownames = FALSE,options = list(pagelength = 15, scrollX=TRUE,
                                                  columnDefs = list(list(className = 'dt-center', targets = 0:8))),
                  colnames= c("Team", "Player", "Total FGA", 'Total Streak FGA',
                              "%FGA Tight", "Streak eFG%", "Streak Open eFG%", 
                              "Streak Tight eFG%", "Coverage Diff")) %>%
      formatStyle(c(6,8), backgroundColor = styleInterval(brks, clrs))
    
    
    }
    )
    
  


  
}

shinyApp(ui = ui, server = server)  


