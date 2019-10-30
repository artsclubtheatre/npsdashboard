library(shiny)
library(tidyverse)
library(flexdashboard)
library(plotly)
library(reshape2)
library(ggwordcloud)

load("npsData.RData")

shinyServer(function(input, output, session) {
   
  output$companyScoreGauge <- renderGauge({
    
    score <- round(companyScore$npsScore)
    
    gauge(value = score, min = -100, max = 100, symbol = '', gaugeSectors(
      success = c(50, 100),
      warning = c(0, 50),
      danger = c(-100, 0)
    ))
    
  })
  
  output$companyScoreOverTime <- renderPlotly({
    plot <- ggplot(
      nps_company, 
      aes(
        x=create_dt, 
        y=cumulativeScore, 
        group=created_by, 
        text=paste("Score: ", round(cumulativeScore, 1)
                   )
        )
      )+
      geom_line(size=1.5, col="steelblue")+
      ylab("Net Promoter Score")+
      xlab("Date")+
      theme_minimal()+
      theme(axis.text = element_text(size=14))
    
    ggplotly(plot, tooltip=c("text"))
  })
  
  output$companyRatingsOverTime <- renderPlot({
    smallCompany <- nps_company %>%
      select(create_dt, totalPromoters, totalPassives, totalDetractors) %>%
      group_by(create_dt) %>%
      mutate(total = sum(totalPromoters, totalPassives, totalDetractors),
             promoterPercent = totalPromoters / total,
             passivePercent = totalPassives / total,
             detractorPercent = totalDetractors / total) %>%
      select(create_dt, promoterPercent, passivePercent, detractorPercent)
    meltedCompany <- melt(list(smallCompany),
                          id.vars = c("create_dt"))
    
    ggplot(meltedCompany, aes(create_dt, value, group=variable, col=variable))+
      geom_line(size=1.5)+
      ylab("Total Patrons")+
      xlab("Date")+
      scale_y_continuous(labels = scales::percent)+
      theme_minimal()+
      theme(legend.position = "bottom")+
      scale_color_manual(name="Patrons",
                         labels=c("Promoters",
                                  "Passives",
                                  "Detractors"),
                         values = c("promoterPercent" = "seagreen4",
                                    "passivePercent" = "steelblue",
                                    "detractorPercent" = "firebrick3")
                         )+
      theme(axis.text = element_text(size=14))
  })
  
  
  output$companyWordCloud <- renderPlot({
    text <- companyText %>% top_n(75)
    ggplot(text, aes(label=word, size=n, col=word))+
      geom_text_wordcloud()+
      scale_size_area(max_size = 25)+
      theme_minimal()
  })
  
  output$companyScoreBySegment <- renderPlot({
    ggplot(companyScoreBySegment, aes(segment, score, fill=segment))+
      geom_bar(stat='identity')+
      geom_text(aes(label=score), position = position_nudge(y=-5), size=10, color="white")+
      ylab("Score")+
      xlab("Patron Segment")+
      theme_minimal()
  })
  
  output$companyProductionGreater <- renderText({
    avgCompany <- mean(allScores$nps_company_score)
    avgProd <- mean(allScores$nps_prod_score)
    
    if(avgCompany > avgProd){
      return(paste(
        "On average, patrons rate the company (", 
        round(avgCompany, 1), 
        " avg ) higher than the production (",
        round(avgProd, 1),
        " avg )"
        ))
    } else if (avgCompany < avgProd){
      return(paste(
        "On average, patrons rate the company (", 
        round(avgCompany, 1), 
        " avg ) lower than the production (",
        round(avgProd, 1),
        " avg )"
      ))
    } else {
      return(paste(
        "On average, patrons rate the company (", 
        round(avgCompany, 1), 
        " avg ) and the production (",
        round(avgProd, 1),
        " avg ) about the same"
      ))
    }
  })

  output$companyProductionCorrelation <- renderPlot({
    ggplot(allScores, aes(nps_company_score, nps_prod_score))+
      geom_count(col="steelblue")+
      geom_smooth(method="lm")+
      geom_abline(intercept = 0, linetype="dashed")+
      scale_size_area(max_size = 20)+
      scale_x_continuous(breaks = c(0:10))+
      scale_y_continuous(breaks = c(0:10))+
      ylab("Production Score")+
      xlab("Company Score")+
      theme_minimal()+
      theme(axis.text = element_text(size=14))
  })
  
  output$prodPlots <- renderUI({
    plotOuputList <- lapply(productionScores$prodSeason, function(prod){
      plotname <- prod
      cloudname <- paste0(prod, "cloud")
      title <- productionScores$title[productionScores$prodSeason == plotname]
      list(
        div(class="col-xs-12 col-md-3 panel panel-default",
            h3(title),
            p(
              strong(productionScores$totalPromoters[productionScores$prodSeason == plotname]), " Promoters, ",
              strong(productionScores$totalPassives[productionScores$prodSeason == plotname]), " Passives, and ",
              strong(productionScores$totalDetractors[productionScores$prodSeason == plotname]), " Detractors "
            ),
            gaugeOutput(plotname),
            p(strong(paste("What are the most common words used in", title,"ratings?"))),
            plotOutput(cloudname, height="200")
          )
      )
    })
    
    do.call(tagList, plotOuputList)
  })
  
  for(prod in factor(productionScores$prodSeason)) {
    local({
      plotname <- prod
      cloudname <- paste0(prod, "cloud")
      
      output[[plotname]] <- renderGauge({
        score <- productionScores %>% 
          filter(prodSeason == plotname) %>% 
          mutate(npsScore = round(npsScore))
        
        gauge(value = score$npsScore, min = -100, max = 100, symbol = '', gaugeSectors(
          success = c(50, 100),
          warning = c(0, 50),
          danger = c(-100, 0)
        ))
      })
      
      output[[cloudname]] <- renderPlot({
        prodText <- productionText %>%
          filter(prodSeasonNo == plotname) %>%
          top_n(20)
        
        ggplot(prodText, aes(label=word, size=n, col=word))+
          geom_text_wordcloud()+
          scale_size_area(max_size = 15)+
          theme_minimal()
      })
    })
  }
  
})
