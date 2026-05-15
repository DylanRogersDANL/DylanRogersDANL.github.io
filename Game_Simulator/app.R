library(shiny)
library(httr)
library(jsonlite)

getAPI <- function(url)
{
  response <- GET(url)
  raw_json <- content(response, "text", encoding = "UTF-8")
  fromJSON(raw_json)
}

normal <- function(abv, minDSI, maxDSI)
{
  teamABV <- abv
  team <- getAPI(paste0("https://api-web.nhle.com/v1/club-stats/", teamABV, "/20252026/2"))
  
  gp <- 0
  ga <- 0
  sa <- 0
  for(y in 1:nrow(team$goalies))
  {
    gp <- gp + team$goalies$gamesPlayed[y]
    ga <- ga + team$goalies$goalsAgainst[y]
    sa <- sa + team$goalies$shotsAgainst[y]
  }
  gaa <- ga/gp
  sapg <- sa/gp
  dsi <- (gaa + (sapg/100))/2
  
  normalDSI_original <- (dsi - minDSI)/(maxDSI - minDSI)
  normalDSI_original <- 0.01 + (normalDSI_original * 0.98)
  
  if(normalDSI_original > 0.85)
  {
    normalDSI <- min(normalDSI_original, 0.80)
  }
  else if(normalDSI_original < 0.15)
  {
    normalDSI <- max(normalDSI_original, 0.20)
  }
  else
  {
    normalDSI <- normalDSI_original
  }
  return(normalDSI)
}

normalWithGoalie <- function(abv, minDSI, maxDSI, goalie) 
{
  teamABV <- abv
  team <- getAPI(paste0("https://api-web.nhle.com/v1/club-stats/", teamABV, "/20252026/2"))
  
  if(length(goalie) == 0 || goalie == 0)
  {
    return(normal(abv, minDSI, maxDSI))
  }
  
  gp <- team$goalies$gamesPlayed[goalie]
  ga <- team$goalies$goalsAgainst[goalie]
  sa <- team$goalies$shotsAgainst[goalie]
  
  if(is.na(gp) || gp == 0)
  {
    return(normal(abv, minDSI, maxDSI))
  }
  
  gaa <- ga/gp
  sapg <- sa/gp
  dsi <- (gaa + (sapg/100))/2
  
  normalDSI_original <- (dsi - minDSI)/(maxDSI - minDSI)
  normalDSI_original <- 0.01 + (normalDSI_original * 0.98)
  
  if(normalDSI_original > 0.85)
  {
    normalDSI <- min(normalDSI_original, 0.80)
  }
  else if(normalDSI_original < 0.15)
  { 
    normalDSI <- max(normalDSI_original, 0.20)
  }
  else
  {
    normalDSI <- normalDSI_original
  }
  return(normalDSI)
}

setUp <- function(home_team, away_team, home_goalie_name, home_goalie_index, away_goalie_name, away_goalie_index)
{
  thisYearWeight <- 0.7
  lastYearWeight <- 1 - thisYearWeight
  playersExpectedGoalsMin <- 0.35 # Show players that have expected goals of this or more
  goalValue <- 0.6 # The "estimated" value of a goal
  
  teams <- getAPI("https://api.nhle.com/stats/rest/en/team")$data
  lastYear <- getAPI("https://api.nhle.com/stats/rest/en/skater/summary?limit=-1&start=17&sort=points&cayenneExp=seasonId=20242025")
  inj <- getAPI("https://site.api.espn.com/apis/site/v2/sports/hockey/nhl/injuries")
  
  maxDSI <- 0
  minDSI <- 100
  
  for(x in 1:nrow(teams))
  {
    if(((teams$id[x] < 31) && (teams$id[x] != 11) && (teams$id[x] != 27)) || 
       (teams$id[x] == 52) || 
       (teams$id[x] == 54) || 
       (teams$id[x] == 55) || 
       (teams$id[x] == 59))
    {
      teamAPIStart <- "https://api-web.nhle.com/v1/club-stats/"
      season <- "20252026"
      teamAPIEnd <- paste0("/", season, "/2")
      
      teamABV <- teams$triCode[x]
      team <- getAPI(paste0(teamAPIStart, teamABV, teamAPIEnd))
      
      gp <- 0
      ga <- 0
      sa <- 0
      
      goalie <- 0
      
      if(teamABV == home_team && home_goalie_name != "None")
      {
        goalie <- home_goalie_index
      }
      else if(teamABV == away_team && away_goalie_name != "None")
      {
        goalie <- away_goalie_index
      }
      
      if(length(goalie) > 0 && goalie != 0)
      {
        gp <- team$goalies$gamesPlayed[goalie]
        ga <- team$goalies$goalsAgainst[goalie]
        sa <- team$goalies$shotsAgainst[goalie]
      }
      else
      {
        for(y in 1:nrow(team$goalies))
        {
          gp <- gp + team$goalies$gamesPlayed[y]
          ga <- ga + team$goalies$goalsAgainst[y]
          sa <- sa + team$goalies$shotsAgainst[y]
        }
      }
      
      if(gp > 0)
      {
        gaa <- ga/gp
        sapg <- sa/gp
        dsi <- (gaa + (sapg/100))/2
        
        if(dsi > maxDSI)
        {
          maxDSI <- dsi
        }
        if(dsi < minDSI)
        {
          minDSI <- dsi
        }
      }
    }
  }
  
  return(list(
    thisYearWeight = thisYearWeight,
    lastYearWeight = lastYearWeight,
    playersExpectedGoalsMin = playersExpectedGoalsMin,
    goalValue = goalValue,
    teams = teams,
    lastYear = lastYear,
    inj = inj,
    minDSI = minDSI,
    maxDSI = maxDSI
  ))
}

game <- function(home_team, away_team, home_goalie_name, home_goalie_index, away_goalie_name, away_goalie_index, setup)
{
  thisYearWeight <- setup$thisYearWeight
  lastYearWeight <- setup$lastYearWeight
  playersExpectedGoalsMin <- setup$playersExpectedGoalsMin
  goalValue <- setup$goalValue
  lastYear <- setup$lastYear
  inj <- setup$inj
  minDSI <- setup$minDSI
  maxDSI <- setup$maxDSI
  
  printPlayers <- TRUE
  showExpectedGoals <- FALSE
  showInjuried <- FALSE
  
  ml <- data.frame()
  allHomePoints <- list()
  allAwayPoints <- list()
  homeExpectedScorers <- c()
  awayExpectedScorers <- c()
  
  if(away_goalie_name != "None" && length(away_goalie_index) > 0 && away_goalie_index != 0)
  {
    normalDSI <- normalWithGoalie(away_team, minDSI, maxDSI, away_goalie_index)
  }
  else
  {
    normalDSI <- normal(away_team, minDSI, maxDSI)
  }
  
  teamAPIStart <- "https://api-web.nhle.com/v1/club-stats/"
  teamAPIEnd <- "/20252026/2"
  teamABV <- home_team
  team <- getAPI(paste0(teamAPIStart, teamABV, teamAPIEnd))
  skaters <- team$skaters
  
  xG <- 0
  pxG <- 0
  
  for(p in 1:nrow(skaters))
  {
    injured <- FALSE
    
    for(t in 1:nrow(inj$injuries))
    {
      for(i in 1:nrow(inj$injuries$injuries[[t]]))
      {
        if(length(inj$injuries$injuries[[t]]$athlete$lastName[i]) > 0 &&
           length(skaters$lastName$default[p]) > 0 &&
           inj$injuries$injuries[[t]]$athlete$lastName[i] == skaters$lastName$default[p])
        {
          injured <- TRUE
        }
      }
    }
    
    if(injured == FALSE)
    {
      first <- skaters$firstName$default[p]
      last <- skaters$lastName$default[p]
      
      pxG <- 0
      
      for(z in 1:nrow(lastYear$data))
      {
        if(skaters$playerId[p] == lastYear$data$playerId[z])
        {
          toi  <- (((skaters$avgTimeOnIcePerGame[p] * thisYearWeight) + (lastYear$data$timeOnIcePerGame[z] * lastYearWeight)) / ((60 * thisYearWeight) + (60 * lastYearWeight)))
          
          pxG <- round(
            (
              (
                ((skaters$goals[p] * thisYearWeight) + (lastYear$data$goals[z] * lastYearWeight)) / 
                  ((skaters$gamesPlayed[p] * thisYearWeight) + (lastYear$data$gamesPlayed[z] * lastYearWeight))
              ) *
                (
                  ((skaters$shots[p] * thisYearWeight) + (lastYear$data$shots[z] * lastYearWeight)) / 
                    ((skaters$gamesPlayed[p] * thisYearWeight) + (lastYear$data$gamesPlayed[z] * lastYearWeight))
                ) *
                (1-normalDSI) * 
                (toi / 60)
            ), 
            5
          )
        }
      }
      
      if(pxG == 0 && skaters$gamesPlayed[p] > 0)
      {
        toi  <- (skaters$avgTimeOnIcePerGame[p] / 60)
        pxG <- round(((skaters$goals[p] / skaters$gamesPlayed[p]) * (skaters$shots[p] / skaters$gamesPlayed[p]) * (1-normalDSI) * (toi / 60)), 5)
      }
      
      playerID <- skaters$playerId[p]
      teamABV <- teamABV
      date <- format(Sys.Date(), "%Y-%m-%d")
      goals <- skaters$goals[p]
      gp <- skaters$gamesPlayed[p]
      shots <- skaters$shots[p]
      opponentDSI <- normalDSI
      expectedPoints <- pxG
      atLeastOnePoint <- 0
      toi <- toi
      
      if(pxG >= playersExpectedGoalsMin)
      {
        atLeastOnePoint <- 1
      }
      
      row <- data.frame(
        PlayerID = playerID, 
        TeamABV = teamABV, 
        Date = date, 
        Goals = goals, 
        GamesPlayed = gp, 
        Shots = shots, 
        OpponentDSI = opponentDSI, 
        TOI = toi, 
        ExpectedPoints = expectedPoints, 
        AtLeastOnePoint = atLeastOnePoint
      )
      
      ml <- rbind(ml, row)
      
      if(printPlayers == TRUE)
      {
        if(pxG >= playersExpectedGoalsMin)
        {
          print(paste0(skaters$firstName$default[p], " " , skaters$lastName$default[p], ", ", teamABV, ": ", pxG, "      (", (pxG/goalValue), ")"))
        }
      }
      
      if((showExpectedGoals == TRUE) && (pxG >= playersExpectedGoalsMin))
      {
        print(pxG)
      }
      
      playerName <- paste0(skaters$firstName$default[p], " " , skaters$lastName$default[p])
      playerPoints <- pxG / playersExpectedGoalsMin
      playerProbability <- round((1 - exp(-pxG)) * 100, 2)
      allHomePoints[[playerName]] <- c(round(playerPoints, 4), paste0(playerProbability, "%"))
      
      xG <- xG + pxG
    }
  }
  
  homeXG <- xG
  
  if(home_goalie_name != "None" && length(home_goalie_index) > 0 && home_goalie_index != 0)
  {
    normalDSI <- normalWithGoalie(home_team, minDSI, maxDSI, home_goalie_index)
  }
  else
  {
    normalDSI <- normal(home_team, minDSI, maxDSI)
  }
  
  teamAPIStart <- "https://api-web.nhle.com/v1/club-stats/"
  teamAPIEnd <- "/20252026/2"
  teamABV <- away_team
  team <- getAPI(paste0(teamAPIStart, teamABV, teamAPIEnd))
  skaters <- team$skaters
  
  xG <- 0
  pxG <- 0
  
  for(p in 1:nrow(skaters))
  {
    injured <- FALSE
    
    for(t in 1:nrow(inj$injuries))
    {
      for(i in 1:nrow(inj$injuries$injuries[[t]]))
      {
        if(length(inj$injuries$injuries[[t]]$athlete$lastName[i]) > 0 &&
           length(skaters$lastName$default[p]) > 0 &&
           inj$injuries$injuries[[t]]$athlete$lastName[i] == skaters$lastName$default[p])
        {
          if(showInjuried == TRUE)
          {
            print(paste0(teamABV, ": ", skaters$firstName$default[p], " ", skaters$lastName$default[p], " is injuried"))
          }
          injured <- TRUE
        }
      }
    }
    
    if(injured == FALSE)
    {
      first <- skaters$firstName$default[p]
      last <- skaters$lastName$default[p]
      
      pxG <- 0
      
      for(z in 1:nrow(lastYear$data))
      {
        if(skaters$playerId[p] == lastYear$data$playerId[z])
        {
          toi  <- (((skaters$avgTimeOnIcePerGame[p] * thisYearWeight) + (lastYear$data$timeOnIcePerGame[z] * lastYearWeight)) / ((60 * thisYearWeight) + (60 * lastYearWeight)))
          
          pxG <- round(
            (
              (
                ((skaters$goals[p] * thisYearWeight) + (lastYear$data$goals[z] * lastYearWeight)) / 
                  ((skaters$gamesPlayed[p] * thisYearWeight) + (lastYear$data$gamesPlayed[z] * lastYearWeight))
              ) *
                (
                  ((skaters$shots[p] * thisYearWeight) + (lastYear$data$shots[z] * lastYearWeight)) / 
                    ((skaters$gamesPlayed[p] * thisYearWeight) + (lastYear$data$gamesPlayed[z] * lastYearWeight))
                ) *
                (1-normalDSI) * 
                (toi / 60)
            ), 
            5
          )
        }
      }
      
      if(pxG == 0 && skaters$gamesPlayed[p] > 0)
      {
        toi  <- (skaters$avgTimeOnIcePerGame[p] / 60)
        pxG <- round(((skaters$goals[p] / skaters$gamesPlayed[p]) * (skaters$shots[p] / skaters$gamesPlayed[p]) * (1-normalDSI) * (toi / 60)), 5)
      }
      
      playerID <- skaters$playerId[p]
      teamABV <- teamABV
      date <- format(Sys.Date(), "%Y-%m-%d")
      goals <- skaters$goals[p]
      gp <- skaters$gamesPlayed[p]
      shots <- skaters$shots[p]
      opponentDSI <- normalDSI
      expectedPoints <- pxG
      atLeastOnePoint <- 0
      toi <- toi
      
      if(pxG >= playersExpectedGoalsMin)
      {
        atLeastOnePoint <- 1
      }
      
      row <- data.frame(
        PlayerID = playerID, 
        TeamABV = teamABV, 
        Date = date, 
        Goals = goals, 
        GamesPlayed = gp, 
        Shots = shots, 
        OpponentDSI = opponentDSI, 
        TOI = toi, 
        ExpectedPoints = expectedPoints, 
        AtLeastOnePoint = atLeastOnePoint
      )
      
      ml <- rbind(ml, row)
      
      if(printPlayers == TRUE)
      {
        if(pxG >= playersExpectedGoalsMin)
        {
          print(paste0(skaters$firstName$default[p], " " , skaters$lastName$default[p], ", ", teamABV, ": ", pxG, "      (", (pxG/goalValue), ")"))
        }
      }
      
      if((showExpectedGoals == TRUE) && (pxG >= playersExpectedGoalsMin))
      {
        print(pxG)
      }
      
      playerName <- paste0(skaters$firstName$default[p], " " , skaters$lastName$default[p])
      playerPoints <- pxG / playersExpectedGoalsMin
      playerProbability <- round((1 - exp(-pxG)) * 100, 2)
      allAwayPoints[[playerName]] <- c(round(playerPoints, 4), paste0(playerProbability, "%"))
      
      xG <- xG + pxG
    }
  }
  
  awayXG <- xG
  
  homeWinProb <- round((1 / (1 + exp(-(homeXG - awayXG)))) * 100, 2)
  awayWinProb <- round(100 - homeWinProb, 2)
  
  if(homeXG > awayXG)
  {
    predictedWinner <- home_team
  }
  else
  {
    predictedWinner <- away_team
  }
  
  homeExpectedPoints <- allHomePoints[
  sapply(allHomePoints, function(x) as.numeric(x[1]) >= 1)
]

awayExpectedPoints <- allAwayPoints[
  sapply(allAwayPoints, function(x) as.numeric(x[1]) >= 1)
]

return(list(
  predictedWinner = predictedWinner,
  homeTeam = home_team,
  awayTeam = away_team,
  homeXG = homeXG,
  awayXG = awayXG,
  homeWinProb = homeWinProb,
  awayWinProb = awayWinProb,
  allHomePoints = allHomePoints,
  allAwayPoints = allAwayPoints,
  homeExpectedPoints = homeExpectedPoints,
  awayExpectedPoints = awayExpectedPoints
))
  
  return(result)
}

playGame <- function(home_team, away_team, home_goalie, away_goalie) {
  
  homeTeam <- getAPI(paste0("https://api-web.nhle.com/v1/club-stats/", home_team, "/20252026/2"))$goalies
  homeGoalies <- paste(homeTeam$firstName$default, homeTeam$lastName$default)
  
  home_goalie_name <- home_goalie
  
  if(home_goalie == "None")
  {
    home_goalie_index <- 0
  }
  else
  {
    home_goalie_index <- which(homeGoalies == home_goalie)
    
    if(length(home_goalie_index) == 0)
    {
      home_goalie_index <- 0
    }
  }
  
  awayTeam <- getAPI(paste0("https://api-web.nhle.com/v1/club-stats/", away_team, "/20252026/2"))$goalies
  awayGoalies <- paste(awayTeam$firstName$default, awayTeam$lastName$default)
  
  away_goalie_name <- away_goalie
  
  if(away_goalie == "None")
  {
    away_goalie_index <- 0
  }
  else
  {
    away_goalie_index <- which(awayGoalies == away_goalie)
    
    if(length(away_goalie_index) == 0)
    {
      away_goalie_index <- 0
    }
  }
  
  setup <- setUp(
    home_team,
    away_team,
    home_goalie_name,
    home_goalie_index,
    away_goalie_name,
    away_goalie_index
  )
  
  result <- game(
    home_team,
    away_team,
    home_goalie_name,
    home_goalie_index,
    away_goalie_name,
    away_goalie_index,
    setup
  )
  
  return(result)
}

teams <- list(
  "ANA", "BOS", "BUF", "CGY", "CAR", "CHI", "COL", "CBJ",
  "DAL", "DET", "EDM", "FLA", "LAK", "MIN", "MTL", "NSH",
  "NJD", "NYI", "NYR", "OTT", "PHI", "PIT", "SJS", "SEA",
  "STL", "TBL", "TOR", "UTA", "VAN", "VGK", "WSH", "WPG"
)

ui <- fluidPage(
  titlePanel("NHL Game Simulator"),
  sidebarLayout(
    sidebarPanel(
      selectInput("home_team", "Choose Home Team:", choices = teams, selected = "FLA"),
      selectInput("away_team", "Choose Away Team:", choices = teams, selected = "EDM"),
      
      selectInput("home_goalie", "Optional Home Goalie:", choices = "None"),
      selectInput("away_goalie", "Optional Away Goalie:", choices = "None"),
      
      actionButton("run", "Run Simulation")
    ),
    mainPanel(
      mainPanel(
        verbatimTextOutput("selected"),
        
        h3("Game Prediction"),
        verbatimTextOutput("game_prediction"),
        
        h3("Expected Point Scorers"),
        verbatimTextOutput("expected_scorers"),
        
        h3("All Home Player Point Values"),
        tableOutput("home_points"),
        
        h3("All Away Player Point Values"),
        tableOutput("away_points")
      )
    )
  )
)

server <- function(input, output, session) {
  observe({
    if(!is.null(input$home_team))
    {
      homeTeam <- getAPI(paste0("https://api-web.nhle.com/v1/club-stats/", input$home_team, "/20252026/2"))$goalies
      homeGoalies <- paste(homeTeam$firstName$default, homeTeam$lastName$default)
      updateSelectInput(session, "home_goalie", choices = c("None", homeGoalies), selected = "None")
    }
  })
  
  observe({
    if(!is.null(input$away_team))
    {
      awayTeam <- getAPI(paste0("https://api-web.nhle.com/v1/club-stats/", input$away_team, "/20252026/2"))$goalies
      awayGoalies <- paste(awayTeam$firstName$default, awayTeam$lastName$default)
      updateSelectInput(session, "away_goalie", choices = c("None", awayGoalies), selected = "None")
    }
  })
  
  output$selected <- renderPrint({
    list(
      HomeTeam = input$home_team,
      AwayTeam = input$away_team,
      HomeGoalie = input$home_goalie,
      AwayGoalie = input$away_goalie
    )
  })
  
  observeEvent(input$run, {
    home_team <- input$home_team
    away_team <- input$away_team
    home_goalie <- input$home_goalie
    away_goalie <- input$away_goalie
    
    result <- playGame(home_team, away_team, home_goalie, away_goalie)
    
    output$game_prediction <- renderPrint({
      cat(
        "Predicted Winner:", result$predictedWinner,
        "\n\n",
        result$homeTeam, "Expected Goals:", round(result$homeXG, 4),
        "\n",
        result$awayTeam, "Expected Goals:", round(result$awayXG, 4),
        "\n\n",
        result$homeTeam, "Win Probability:", result$homeWinProb, "%",
        "\n",
        result$awayTeam, "Win Probability:", result$awayWinProb, "%"
      )
    })
    
    output$expected_scorers <- renderPrint({
      
      if(length(result$homeExpectedPoints) == 0) {
        cat(result$homeTeam, "does not have any players, that are expected to have at least one point\n")
      } else {
        cat(result$homeTeam, "Expected Players with at least one point:\n", sep = " ")
        
        for(player in names(result$homeExpectedPoints)) {
          cat(
            player, ": ",
            result$homeExpectedPoints[[player]][1],
            " Expected points (",
            result$homeExpectedPoints[[player]][2],
            ")\n",
            sep = ""
          )
        }
      }
      
      cat("\n")
      
      if(length(result$awayExpectedPoints) == 0) {
        cat(result$awayTeam, "does not have any players, that are expected to have at least one point\n")
      } else {
        cat(result$awayTeam, "Expected Players with at least one point:\n", sep = " ")
        
        for(player in names(result$awayExpectedPoints)) {
          cat(
            player, ": ",
            result$awayExpectedPoints[[player]][1],
            " Expected points (",
            result$awayExpectedPoints[[player]][2],
            ")\n",
            sep = ""
          )
        }
      }
    })
    
    output$home_points <- renderTable({
      if(length(result$allHomePoints) == 0) {
        return(data.frame(Player = character(), Score = numeric(), Probability = character()))
      }
      
      data.frame(
        Player = names(result$allHomePoints),
        Score = sapply(result$allHomePoints, function(x) x[1]),
        Probability = sapply(result$allHomePoints, function(x) x[2]),
        row.names = NULL
      )
    })
    
    output$away_points <- renderTable({
      if(length(result$allAwayPoints) == 0) {
        return(data.frame(Player = character(), Score = numeric(), Probability = character()))
      }
      
      data.frame(
        Player = names(result$allAwayPoints),
        Score = sapply(result$allAwayPoints, function(x) x[1]),
        Probability = sapply(result$allAwayPoints, function(x) x[2]),
        row.names = NULL
      )
    })
  })
}

shinyApp(ui, server)