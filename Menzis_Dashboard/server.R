#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(ggplot2)
library(readxl)
library(sf)
library(gganimate)
library(plotly)
library(leaflet)
library(corrr)
library(interplot)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    ## DATA FILE VOOR MIJ
    data_geo_gm <- readRDS("leaflet_data.rds")
    data_gm_gezmon <-readRDS("cbs_gemeente_gzmon.rds")
    PP3 <- readRDS("pp3_gemeente.rds")
    data_pp3v2 <- readRDS("pp3_gemeentev2.rds")
    bodemgebruik <- readRDS("bodemgebruik.rds")
    CBS_data_pp4_met_gemeenten <- readRDS("CBS_selectie.rds")
    
    # GEMEENTEN KAART PLOT
    output$geo_gemeenten <-
        renderLeaflet({
            data <-
                data_geo_gm %>%
                filter(
                    GESLACHT %in% paste(input$geo_GESLACHT) &
                        JAAR == input$geo_JAAR &
                        KOSTENTYPE %in% paste(input$geo_kostentype)
                ) %>%
                dplyr::select(statcode,JAAR,GESLACHT,KOSTENTYPE,`RELATIEVE KOSTEN`) %>%
                group_by(statcode) %>%
                summarize(`RELATIEVE KOSTEN` = sum(`RELATIEVE KOSTEN`))

            pal <-
                colorNumeric(palette = "viridis",
                             domain = data$`RELATIEVE KOSTEN`)
            
            
            
            leaflet() %>%
                #addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
                addPolygons(
                    data = data,
                    color = ~ pal(`RELATIEVE KOSTEN`),
                    stroke = FALSE,
                    smoothFactor = 0.5,
                    fillOpacity = 0.9,
                    label = ~ `RELATIEVE KOSTEN`
                ) %>%
                leaflet::addLegend(
                    data = data,
                    pal = pal,
                    values = data$`RELATIEVE KOSTEN`
                ) %>%
                addProviderTiles(providers$OpenStreetMap)
        })
    ## OUTPUT GEO SOMMEN
    output$geo_plotly <-
        renderPlotly({
          p <-
            data_geo_gm %>%
            filter(
              GESLACHT %in% paste(input$geo_GESLACHT) &
                JAAR == input$geo_JAAR
            ) %>% 
            ggplot(aes(
              x = reorder(KOSTENTYPE, `RELATIEVE KOSTEN`),
              y = `RELATIEVE KOSTEN` ,
              color = KOSTENTYPE,
              fill = KOSTENTYPE
            )) +
            geom_bar(stat = "identity") +
            xlab(" ") + ylab("Relatieve Kosten") + 
            ggtitle("Relatieve kosten per categorie")
            ggplotly(p)
        })
    ## CORRELATIE  GZ MONITOR
    output$gzmon_plotly_corr <-
      
      renderPlotly({
        x <- correlate(data_gm_gezmon[c(-(1),-(12))])
        cor_specialistisch <- focus(x, RELATIEVE_SPECIALISTISCHE_GGZ) 
        
        cor_specialistisch %>%
          mutate(pathway = forcats::fct_reorder(rowname, RELATIEVE_SPECIALISTISCHE_GGZ)) %>% 
          ggplot(aes(x = reorder(rowname, RELATIEVE_SPECIALISTISCHE_GGZ), y = RELATIEVE_SPECIALISTISCHE_GGZ)) +
          geom_bar(stat = "identity",  position = position_dodge(), fill = 'dodgerblue2')+  
          coord_flip() +
          ylab("Correlatie") +  xlab("") +  
          ggtitle("Verband gezondheid en specialistische GGZ kosten")
      })
    ## MSE PLOT GZ MONITOR
    output$gzmon_mse_plot <- 
      renderPlotly({
        
        model <- lm(formula = RELATIEVE_SPECIALISTISCHE_GGZ ~ ., data = data_gm_gezmon[,c(-1)])
        summary(model)
        data_gm_gezmon$PRED_RELATIEVE_SPECIALISTISCHE_GGZ <-predict(model, data_gm_gezmon[,c(-1)])
        
        #Reken de mse uit
        Evaluation <- data_gm_gezmon[,c("RELATIEVE_SPECIALISTISCHE_GGZ","PRED_RELATIEVE_SPECIALISTISCHE_GGZ")]
        Evaluation$MSE <- ((Evaluation$RELATIEVE_SPECIALISTISCHE_GGZ - Evaluation$PRED_RELATIEVE_SPECIALISTISCHE_GGZ)^2)
        Evaluation$Gemeente <- data_gm_gezmon$GEMEENTENAAM
        
        Evaluation %>%
          ggplot(aes(y = MSE, x = RELATIEVE_SPECIALISTISCHE_GGZ)) +
          geom_point() +
          geom_smooth() +
          ylab("Afwijking")  + xlab("Relatieve kosten specialistische GGZ") +
          ggtitle("Verschil tussen voorspelde en daadwerkelijke kosten")
      })
    ##  CORR PLOT PP3
    output$pp3_corr_plot <- 
        renderPlotly({
          PP3 <- PP3
          PP3$REL_KOSTEN_SPECIALISTISCHE_GGZ = PP3$KOSTEN_SPECIALISTISCHE_GGZ/PP3$AANTAL_VERZEKERDEJAREN
          
          PP3_REL = PP3
          
          PP3_REL[,c(9:16)] = PP3_REL[,c(9:16)]/PP3_REL$INWONER
          PP3_REL[,c(22:25)] = PP3_REL[,c(22:25)]/PP3_REL$INWONER
          PP3_REL[,c((17:20),(28:29))] = PP3_REL[,c((17:20),(28:29))]/PP3_REL$AANTAL_HH
          PP3_REL[,c(21,(26:27))] = PP3_REL[,c(21,(26:27))]/PP3_REL$WONING
          
          x <- correlate(PP3_REL[c(-(1:8),-(30:31))])
          cor_specialistisch <- focus(x, REL_KOSTEN_SPECIALISTISCHE_GGZ)  
          
          #Plot van alle correlaties in GGZ data 
          cor_specialistisch %>%
            mutate(pathway = forcats::fct_reorder(rowname, REL_KOSTEN_SPECIALISTISCHE_GGZ)) %>% 
            ggplot(aes(x = reorder(rowname, REL_KOSTEN_SPECIALISTISCHE_GGZ), y = REL_KOSTEN_SPECIALISTISCHE_GGZ)) +
            geom_bar(stat = "identity",  position = position_dodge(), fill = 'dodgerblue2')+  
            coord_flip() +
            ylab("Correlatie") +  xlab("") +  
            ggtitle("Verband CBS data en specialistische GGZ kosten")
          
          #
          #tabel$variabelen <- c('Intercept', 'Percentage eenpersoons huishoudens', 'Percentage huurwoningen', 'Percentage uitkeringen zonder AOW', 'Percentage lage inkomen huishoudens'
          #                      , 'Percentage huishoudens met twee ouders', 'Percentage koopwoningen', 'Percentage vrouwen', 'Percentage inwoners in de leeftijdscategorie 15-24'
          #                      , 'Percentage inwoners in de leeftijdscategorie 25-44', 'Percentage inwoners in de leeftijdscategorie 45-65', 'Percentage inwoners in de leeftijdscategorie 65 plus'
          #                      , 'Geboorte percentage', 'Percentage huishoudens met een ouder')
          #  
        })
    ## PLOT COEFFICIENTEN TABEL
    output$pp3_coef_table <-  
      DT::renderDataTable({
            
            data_pp3 <- data_pp3v2
            
            model <- lm(formula = RELATIEF_KOSTEN_SPECIALISTISCHE_GGZ ~ P_TOTHH_EENP + P_HUURWON + P_UITKMINAOW + P_LINK_HH + P_HH_TWEEOUD + P_KOOPWON, data = data_pp3)
            
            data_pp3$PRED_RELATIEVE_SPECIALISTISCHE_GGZ <-predict(model, data_pp3)
            
            # OLS op alle relatieve variabelen tov specialistische GGZ
            
            model_selectie_data <- data_pp3 %>%
                dplyr::select(starts_with('p_'), RELATIEF_KOSTEN_SPECIALISTISCHE_GGZ)
            
            model <- lm(formula = RELATIEF_KOSTEN_SPECIALISTISCHE_GGZ ~ ., data = model_selectie_data)

            coeff <- as.data.frame(model$coefficients)
            p_values <- as.data.frame(summary(model)$coefficients[,4])
            
            tabel <- bind_cols(coeff, round(p_values,6))
            tabel$variabelen <- c('Intercept', 'Percentage eenpersoons huishoudens', 'Percentage huurwoningen', 'Percentage uitkeringen zonder AOW', 'Percentage lage inkomen huishoudens'
                                 , 'Percentage huishoudens met twee ouders', 'Percentage koopwoningen', 'Percentage vrouwen', 'Percentage inwoners in de leeftijdscategorie 15-24'
                                 , 'Percentage inwoners in de leeftijdscategorie 25-44', 'Percentage inwoners in de leeftijdscategorie 45-65', 'Percentage inwoners in de leeftijdscategorie 65 plus'
                                 , 'Geboorte percentage', 'Percentage huishoudens met een ouder')
            tabel$`Coefficienten model` <- paste('€',as.character(round(tabel$`model$coefficients`, 2)), sep = ' ')
            tabel <- tabel[,c(3,4,2)]
            
            colnames(tabel) =c('_',' Schatting per verzekerdejaar','P-waardes')
            
            tabel <-  tabel %>% mutate(`P-waardes` = if_else(`P-waardes` <= 0.05, '< 0.001', as.character(`P-waardes`)))
            
            print(tabel)
            DT::datatable(tabel, options = list(pageLength = 25), rownames = FALSE)  %>% formatStyle('P-waardes', backgroundColor = styleEqual(levels = '< 0.001', values = 'green'))
            
        })
    
    
    ## EFFECTPLOT 1
    output$pp3_effect_plot <- 
        renderPlotly({ 
            
          data_pp3 <- data_pp3v2 
          model_selectie_data <- data_pp3 %>%
            dplyr::select(starts_with('p_'), RELATIEF_KOSTEN_SPECIALISTISCHE_GGZ)
          model <- lm(formula = RELATIEF_KOSTEN_SPECIALISTISCHE_GGZ ~ P_VROUW * P_TOTHH_EENP + P_UITKMINAOW + P_LINK_HH , data = model_selectie_data)
          
          interplot::interplot(m = model, var1 = 'P_TOTHH_EENP', var2= 'P_VROUW')+
            theme_bw() +
            labs(x = '% vrouw',
                 y = 'Effect eenpersoons huishoudens') 
            
        })
    ## EFFECTPLOT 2
    output$pp3_effect_plot2 <- 
        renderPlotly({ 
            
          data_pp3 <- data_pp3v2 
          model_selectie_data <- data_pp3 %>%
            dplyr::select(starts_with('p_'), RELATIEF_KOSTEN_SPECIALISTISCHE_GGZ)
          model <- lm(formula = RELATIEF_KOSTEN_SPECIALISTISCHE_GGZ ~  P_UITKMINAOW + I(P_UITKMINAOW^2) , data = model_selectie_data)
          #summary(model)
          prd <- data.frame(P_UITKMINAOW = seq(from = range(model_selectie_data$P_UITKMINAOW)[1], to = range(model_selectie_data$P_UITKMINAOW)[2], length.out = 100))
          err <- predict(model, newdata = prd, se.fit = TRUE)
          
          prd$lci <- err$fit - 1.96 * err$se.fit
          prd$fit <- err$fit
          prd$uci <- err$fit + 1.96 * err$se.fit
          
          interplot(m = model, var1 = 'P_UITKMINAOW', var2 = 'P_UITKMINAOW') +
            theme_bw() +
            labs(x = '% uitkering',
                 y = 'Effect op Relatieve Specialistische GGZ Kosten') +
            geom_hline(yintercept = 0,
                       linetype = "dashed")
            
        })
    ## Leaflet met de verschillen tussen predicted en true?
    
    ### Tabel met verschillen tussen predicted en true
    output$pp3_diff_pred_real_table <- 
      DT::renderDataTable({
            
            model_selectie_data <- data_pp3v2 %>%
               dplyr::select(starts_with('p_'), RELATIEF_KOSTEN_SPECIALISTISCHE_GGZ)
            
            model <- lm(formula = RELATIEF_KOSTEN_SPECIALISTISCHE_GGZ ~ ., data = model_selectie_data)


            data_pp3v2$PRED_RELATIEVE_SPECIALISTISCHE_GGZ <- predict(model, data_pp3v2)

            data_gemeente <- data_pp3v2 %>%
                group_by(GWBlabel) %>%
                mutate(ECHTE_KOSTEN = RELATIEF_KOSTEN_SPECIALISTISCHE_GGZ * AANTAL_VERZEKERDEJAREN) %>%
                mutate(GESCHATTE_KOSTEN = PRED_RELATIEVE_SPECIALISTISCHE_GGZ * AANTAL_VERZEKERDEJAREN) %>%
                dplyr::select(ECHTE_KOSTEN, GESCHATTE_KOSTEN, AANTAL_VERZEKERDEJAREN, GWBlabel) %>%
                summarise_all(sum) %>%
                mutate(Verschil_relatief_geschat_abs = abs(ECHTE_KOSTEN- GESCHATTE_KOSTEN)/AANTAL_VERZEKERDEJAREN) %>%
                mutate(Verschil_relatief_geschat = round((ECHTE_KOSTEN - GESCHATTE_KOSTEN)/AANTAL_VERZEKERDEJAREN,2)) %>%
                dplyr::select(Gemeente = GWBlabel, `Verschil tussen de geschatte waarde en de daadwerkelijke waarde` = Verschil_relatief_geschat) %>%
                arrange(desc(`Verschil tussen de geschatte waarde en de daadwerkelijke waarde`))
                

        })
    
    ##Berekenen van predicted values aan de hand van input
    output$pp3_reg_text_formule <- renderUI({
      withMathJax("$Y = \\beta_0  + \\beta_1 * $(P_VROUW) $ + \\beta_2 * $ (P_TOTHH_EENP) $ + \\beta_3 * $ (P_UITKMINAOW) $ \\beta4 * $(P_LINK_HH)$ + \\epsilon$")
    })
    output$pp3_reg_text_getallen <- renderUI({
      model_selectie_data <- data_pp3v2 %>%
        dplyr::select(P_VROUW, P_TOTHH_EENP, P_UITKMINAOW, P_LINK_HH, RELATIEF_KOSTEN_SPECIALISTISCHE_GGZ)
      model <- lm(formula = RELATIEF_KOSTEN_SPECIALISTISCHE_GGZ ~ P_VROUW + P_TOTHH_EENP + P_UITKMINAOW + P_LINK_HH , data = model_selectie_data)
      
      coef_names <- names(model$coefficients)
      values <- model$coefficients
      
      pred_data  <- data.frame(P_VROUW = input$P_Vrouw, P_TOTHH_EENP = input$P_Tothh_eenp, P_UITKMINAOW = input$P_Uitkminaow, P_LINK_HH = input$P_Link_hh)
      y = round(predict(model,pred_data),4)
      #print(y)
     withMathJax(sprintf("$%s = %s  + %s * (%s)  + %s * (%s)  + %s *  (%s)  + %s *  (%s) + \\epsilon$",
                         y,
                         round(values[1],2), # Intercerpt
                         round(values[2],2),
                         pred_data$P_VROUW, 
                         round(values[3],2),
                         pred_data$P_TOTHH_EENP,
                         round(values[4],2),
                         pred_data$P_UITKMINAOW,
                         round(values[5],2),
                         pred_data$P_LINK_HH
                 ))
     #withMathJax("$X = \\beta_0  + \\beta_1 * $(P_VROUW) $ + \\beta_2 * $ (P_TOTHH_EENP) $ + \\beta_3 * $ (P_UITKMINAOW) $ + \\epsilon$")
    # withMathJax(paste0(y, "")
    })
    #output$pp3_pred_value_table <- renderTable( {print(input$P_Vrouw)} )
    #output$pp3_pred_value_table <- DT::renderDataTable({`voorspelde waarde` <- predict(model, data.frame(P_VROUW = input$P_Vrouw, P_TOTHH_EENP = input$P_Tothh_eenp, P_UITKMINAOW = input$P_Uitkminaow, P_LINK_HH = input$P_Link_hh))})
    
    ## Exploratie van verschillende dimensies
    output$exp_sportter <-
        renderPlotly({
          
          bodemgebruik_sub <- bodemgebruik %>% 
            dplyr::select(Gemeentenaam:Noordzee) %>%
            filter(SoortRegio  == "Gemeente")
          
          #dataset met alleen de uitschieter gemeenten
          bodemgebruik_sub_uit <- bodemgebruik_sub %>% 
            dplyr::select(Gemeentenaam: Noordzee) %>%
            filter(Gemeentenaam %in% c( "Alphen aan den Rijn", "Albrandswaard", "Goes", "Renkum", "Apeldoorn", "Tynaarlo","Renkum"))
          
          
          #barplot sportterein gemeente tov gemid
          ggplot(bodemgebruik_sub_uit, aes(x = Gemeentenaam, y = Sportterrein)) +
            geom_bar(stat = "identity") +
            geom_hline(yintercept = mean(bodemgebruik_sub$Sportterrein)) +
            xlab(" ") +
            ggtitle("Oppervlakte sportterein tov gemiddelde in Nederland")
          
          
        })
    
    output$exp_park_plantsoen <-
        renderPlotly({
            
          bodemgebruik_sub <- bodemgebruik %>% 
            dplyr::select(Gemeentenaam:Noordzee) %>%
            filter(SoortRegio  == "Gemeente")
          
          #dataset met alleen de uitschieter gemeenten
          bodemgebruik_sub_uit <- bodemgebruik_sub %>% 
            dplyr::select(Gemeentenaam: Noordzee) %>%
            filter(Gemeentenaam %in% c( "Alphen aan den Rijn", "Albrandswaard", "Goes", "Renkum", "Apeldoorn", "Tynaarlo","Renkum"))
          
          ggplot(bodemgebruik_sub_uit, aes(x = Gemeentenaam, y =  ParkEnPlantsoen)) +
            geom_bar(stat = "identity") +
            geom_hline(yintercept = mean(bodemgebruik_sub$ParkEnPlantsoen)) +
            xlab(" ") + ylab("Park en plantsoen") +
            ggtitle("Oppervlakte park en plantsoen tov gemiddelde in Nederland")
        })
    
    output$exp_10_kwal_analyse_plot <- 
        renderPlotly({
            
            CBS <- CBS_data_pp4_met_gemeenten %>% dplyr::select(-starts_with('P'))
            
            #Splits de variabelen op om apart te kunnen rekenen
            CBS_afs <- CBS %>% dplyr::select(starts_with('AFS'),GWBlabel,Jaar)
            CBS <- CBS %>% dplyr::select(-starts_with('AFS'))
            
            #Neem bij afstanden het minimum en aggregeer naar gemeente
            CBS_afs_min = aggregate(CBS_afs[, -c(31)], list(CBS_afs$GWBlabel,CBS_afs$Jaar), min)
            
            #Neem bij de rest de som en aggregeer naar gemeente
            CBS_sum = aggregate(CBS[, c(-(20:21))], list(CBS$GWBlabel,CBS_afs$Jaar), sum)
            
            #Reken alle variabelen uit per inwoner, huishoudens of woning
            CBS_sum[,c(3:10,16)] = CBS_sum[,c(3:10,16)]/CBS_sum$INWONER
            CBS_sum[,c(11:14)] = CBS_sum[,c(11:14)]/CBS_sum$AANTAL_HH
            CBS_sum[,c(15)] = CBS_sum[,c(15)]/CBS_sum$WONING
            
            #Voeg weer samen
            CBS2 <- cbind(CBS_sum, CBS_afs_min)
            
            #Selecteer de juiste gemeenten
            CBS_selectie = CBS2[CBS2$Group.1 %in% c('Alphen aan den Rijn','Goes','Renkum','Apeldoorn','Albrandswaard','Tynaarlo'),-c(21)]
            
            
            #Reken alle gemiddeldes uit
            colmeans = colMeans(CBS2[,-c((1:2),(22:23),53)])
            
            #Plots
            ggplot(data=CBS_selectie, aes(x=Group.1, y=OAD)) +
                geom_col(position = 'dodge')+
                geom_hline(yintercept = colmeans['OAD'], color="black")+
                xlab("Gemeente") +
                ylab("Omgevingsadressen dichtheid") +
                theme(axis.text.x = element_text())
        })
})
