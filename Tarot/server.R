library(RColorBrewer)
library(data.table)
library(scales)
library(lattice)
library(dplyr)

scores=fread("D:/local_project/tarot/tarot/scores.csv")
squads=fread("D:/local_project/tarot/tarot/squads.csv")

config= squads[,.(players=paste(squads$Player,collapse = ", ")),by=.(Squad)]

Contrat_table=data.table(contrat=c("Petite (bite)","Garde","Garde Sans","Garde Contre"),mult=c(1,2,4,6))


function(input, output, session) {
    

    ## Data Explorer ###########################################
    
    
    observe({
        # browser()
        players <- if (is.null(input$Squads)) character(0) else {
            squads[Squad==config$Squad,Player]
        }
        # stillSelected <- isolate(input$cities[input$cities %in% cities])
        updateSelectizeInput(session, "Preneur", choices = players,server = TRUE)
    })
    
    
    updated_scores=eventReactive(input$RUN_CALC,{
     
     
        # browser()
        scores=fread("D:/local_project/tarot/tarot/scores.csv")
        temp=copy(scores)
        temp[,last_donne:=max(Donne)]
        temp=temp[Donne==last_donne]
        
        next_donne=temp[1,Donne]+1
        
        #INIT table to be rbinded 
        temp[,Donne:=next_donne]
        temp[,Preneur:=input$Preneur]
        temp[,contrat:=input$contrat_type]
        temp[,succeed:=input$itpasses]
        temp[,ecart :=input$ecart]
        temp[,Petit_end  :=input$petit]
        temp[,Poignee:=NA]
        temp[,score_mene:=NA]
        temp[,Previous_score:=score_global]
        temp[,score_global:=NA]
        
        contrat_multiplier=Contrat_table[contrat==input$contrat_type,mult]
        player_mutplier=nrow(temp)-1
        
        score_preneur=ifelse(input$itpasses,1,-1)*(25+input$ecart+ifelse(input$petit,10,0))*contrat_multiplier*player_mutplier
        score_defense=ifelse(input$itpasses,-1,1)*(25+input$ecart+ifelse(input$petit,10,0))*contrat_multiplier
        
        temp[Player==Preneur,score_mene:=score_preneur]
        temp[Player!=Preneur,score_mene:=score_defense]
        
        temp[,score_global:=Previous_score+score_mene]
        # temp[,Previous_score:=score_global]
        temp[,last_donne:=NULL]
        
        
        scores=rbind(scores,temp)

        fwrite(scores,file="D:/local_project/tarot/tarot/scores.csv")
        
        return(scores)
        
    })
    # observe({
    #     zipcodes <- if (is.null(input$states)) character(0) else {
    #         cleantable %>%
    #             filter(State %in% input$states,
    #                    is.null(input$cities) | City %in% input$cities) %>%
    #             `$`('Zipcode') %>%
    #             unique() %>%
    #             sort()
    #     }
    #     stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    #     updateSelectizeInput(session, "zipcodes", choices = zipcodes,
    #                          selected = stillSelected, server = TRUE)
    # })
    
    # observe({
    #     if (is.null(input$goto))
    #         return()
    #     isolate({
    #         map <- leafletProxy("map")
    #         map %>% clearPopups()
    #         dist <- 0.5
    #         zip <- input$goto$zip
    #         lat <- input$goto$lat
    #         lng <- input$goto$lng
    #         showZipcodePopup(zip, lat, lng)
    #         map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    #     })
    # })
    
    

    output$scores <- DT::renderDataTable({
       
        # browser()
        dt=updated_scores()
        dt=dcast(dt,Donne~Player,value.var="score_global")[order(-Donne)]
        return(dt)
    })
}