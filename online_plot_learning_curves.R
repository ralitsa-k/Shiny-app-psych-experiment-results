#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggpubr)
library(wesanderson)


sort_social = read_csv('sorted_social.csv')

my_theme =   theme(panel.spacing = unit(2, "lines"),
                   text = element_text(size=27),
                   axis.title.x = element_text(size = 21, hjust=0.5, vjust=-1),
                   axis.title.y = element_text(size = 21, hjust=0.5, vjust =1.5),
                   axis.text.x = element_text(size = 19),
                   axis.text.y = element_text(size = 19))
scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = wes_palette('GrandBudapest1', 3, type = c("discrete"))
  )
} 
  
anx_risk <- read_csv('anxiety_risk.csv') %>%
  filter(anx_high == 1) %>%
  select(ID)

  # basic example
ui = fluidPage(

  
  titlePanel("Risk-taking learning curves per block"),
  
    
    mainPanel(
      selectInput("variable", "Variable:",
                          c("Baseline" = "baseline","In-group" = "in-group","Out-group" = "out-group",
                            "All" = 'all')),
      tabsetPanel(
        tabPanel("Individual", plotOutput("fig1")), 
        tabPanel("Summary", plotOutput("fig2")),
        tabPanel("Reaction times Low-High EV", h4("Reaction times from choosing stimulus.
                                      Slower when both means are low compared to different means or high average EV."),
                                      h4("No difference between social conditions"),
                 plotOutput("fig3")),
        tabPanel("RT Risky-Safe Social-wise", h4("Reaction times of choosing risky or safe stimulus."),
                 h4("No difference between social conditions or risky choices"),
                 plotOutput("fig4")),
        tabPanel("RT Risky-Safe EV-wise", h4("Reaction times of choosing risky or safe stimulus within EV pairs."),
                 h4("Again, slower when EVs are both-low"),
                 plotOutput("fig5")),
        tabPanel("Anxiety and learning curves", h4("High or low (trait) anxiety on learning curves. (Takes TIME to load)"),
                 plotOutput("fig6"), plotOutput("fig7"))
        
      )
    )
  )



server <- function(input, output) {
    
    tt <- reactive({
      paste('Risk-taking across task ', input$variable)
    })
    p <- reactive({ # Everything that should update is reactive
    
        if (input$variable == 'all') {
          ch1 <- sort_social %>%
            mutate(bins = rep(1:5,each = 3*36, times = 28)) %>%
            filter(typeOfChoice %in% c(1,6)) %>%
            group_by(pair_type,bins,ID) %>%
            mutate(c_bins = n())
          ch1_sum = ch1 %>%
            ungroup() %>%
            group_by(pair_type, ID, bins) %>%
            mutate(perc_risky = mean(risky, na.rm = TRUE)) %>%
            mutate(se = sd(risky, na.rm = TRUE)/sqrt(c_bins)) %>%
            mutate(semin = perc_risky - se, semax = perc_risky + se) 
          
          ch1_sum %>%
            ggplot(aes(x = factor(bins), y = perc_risky, color = pair_type)) +
            geom_point(size = 3) +
            geom_errorbar(aes(ymin=semin, ymax=semax), 
                          width=0.2, lty=1, size=0.5) +
            geom_abline(intercept = 0.5, slope = 0, size = 1) +
            facet_wrap(~ID)+
            scale_colour_manual(values = c('cadetblue2', 'darkolivegreen3')) +
            geom_line(aes(group = pair_type), size = 2) +
            labs(x = 'trial bins', y = 'P(risky)', color = 'Pair type')
          
          
        } else if (input$variable %in% c('in-group', 'out-group', 'baseline')) {
          ch1 <- sort_social %>%
            filter(block_soc == input$variable) %>%
            mutate(bins = rep(1:5,each =  36, times = 28)) %>%
            filter(typeOfChoice %in% c(1,6)) %>%
            group_by(pair_type, bins,ID) %>%
            mutate(c_bins = n())
          ch1_sum = ch1 %>%
            ungroup() %>%
            group_by(pair_type, ID, bins,block_soc) %>%
            mutate(perc_risky = mean(risky, na.rm = TRUE)) %>%
            mutate(se = sd(risky, na.rm = TRUE)/sqrt(c_bins)) %>%
            mutate(semin = perc_risky - se, semax = perc_risky + se) 
          
          ch1_sum %>%
            ggplot(aes(x = factor(bins), y = perc_risky, color = pair_type)) +
            geom_point(size = 3) +
            geom_errorbar(aes(ymin=semin, ymax=semax), 
                          width=0.2, lty=1, size=0.5) +
            geom_abline(intercept = 0.5, slope = 0, size = 1) +
            facet_wrap(~ID)+
            scale_colour_manual(values = c('cadetblue2', 'darkolivegreen3')) +
            geom_line(aes(group = pair_type), size = 2) +
            labs(x = 'trial bins', y = 'P(risky)', color = 'Pair type')
        }
    })
    
    m <- reactive({
      if (input$variable != 'all'){
      ch1 <- sort_social %>%
        filter(block_soc == input$variable) %>%
        mutate(bins = rep(1:5,each =  36, times = 28)) %>%
        filter(typeOfChoice %in% c(1,6)) %>%
        group_by(bins) %>%
        mutate(c_bins = n()) %>%
        group_by(bins,pair_type) %>%
        mutate(perc_risky = mean(risky, na.rm = TRUE)) %>%
        mutate(se = sd(risky, na.rm = TRUE)/sqrt(c_bins)) %>%
        mutate(semin = perc_risky - se, semax = perc_risky + se) 
      
      ch1 %>%
        ggplot(aes(x = factor(bins), y = perc_risky,color = pair_type)) +
        geom_point(size = 5) +
        geom_errorbar(aes(ymin=semin, ymax=semax), 
                      width=0.2, lty=1, size=0.5) +
        geom_abline(intercept = 0.5, slope = 0, size = 1) +
        scale_colour_manual(values = c('cadetblue2', 'darkolivegreen3')) +
        geom_line(aes(group = pair_type), size = 2) +
        labs(x = 'trial bins', y = 'P(risky)', color = 'Pair type')
      
      } else if (input$variable == 'all') {
      
      ch1 <- sort_social %>%
        mutate(bins = rep(1:5,each = 36, times = 3*28)) %>%
        filter(typeOfChoice %in% c(1,6)) %>%
        group_by(bins) %>%
        mutate(c_bins = n()) %>%
        group_by(bins,pair_type) %>%
        mutate(perc_risky = mean(risky, na.rm = TRUE)) %>%
        mutate(se = sd(risky, na.rm = TRUE)/sqrt(c_bins)) %>%
        mutate(semin = perc_risky - se, semax = perc_risky + se) 
      
      ch1 %>%
        ggplot(aes(x = factor(bins), y = perc_risky,color = pair_type)) +
        geom_point(size = 5) +
        geom_errorbar(aes(ymin=semin, ymax=semax), 
                      width=0.2, lty=1, size=0.5) +
        geom_abline(intercept = 0.5, slope = 0, size = 1) +
        scale_colour_manual(values = c('cadetblue2', 'darkolivegreen3')) +
        geom_line(aes(group = pair_type), size = 2)+
        labs(x = 'trial bins', y = 'P(risky)', color = 'Pair type')
      }
    })  
    
  
    # Output here
    output$text <- renderText({
      tt()
    })
    output$fig1 <- renderPlot({
      p()
      
    },height = 700, width = 900)
    
    output$fig2 <- renderPlot({
      m()
    },height = 500, width = 500)
    
    output$fig3 <- renderPlot({
      
      
      rts <- sort_social %>%
        select(ID, trial_id, block_soc, pair_type, typeOfChoice,  rt) %>%
        group_by(ID, pair_type, block_soc) %>%
        summarise(mean_rt = mean(rt, na.rm = TRUE))
      
      ggplot(rts, aes(x = pair_type, y = mean_rt, fill = block_soc)) +
        geom_boxplot() +
        stat_compare_means(method = "anova", label.y = 1.8) +
        stat_compare_means(comparisons = list(c("both_high","both_low")), method = "t.test", label.y = 1.6) +
        stat_compare_means(comparisons = list(c("both_low","different")), method = "t.test", label.y = 1.6) +
        annotate(size = 4, geom="text", x=2, y=1.7, label=paste('t-tests, p ='), color="black") +
        labs(x = 'Pair type', y = 'Reaction times from choosing stimulus', fill = 'Social block') +
        my_theme
      
    },height = 600, width = 900)
    
    
    output$fig4 <- renderPlot({
      
      sort_social %>%
        select(ID, risky, trial_id, block_soc, pair_type, typeOfChoice,  rt) %>%
        filter(!is.na(risky)) %>%
        mutate(risky =recode(risky, '1' = 'risky_chosen', '0' = 'safe_chosen')) %>%
        group_by(ID, risky, block_soc) %>%
        summarise(mean_rt = mean(rt, na.rm = TRUE)) %>%
      ggplot(aes(x = risky, y = mean_rt, fill = block_soc)) +
        geom_boxplot() +
        stat_compare_means(method = "anova", label.y = 1.8) +
        stat_compare_means(comparisons = list(c("risky_chosen","safe_chosen")), method = "t.test", label.y = 1.6) +
        annotate(size = 4, geom="text", x=1.26, y=1.7, label=paste('t-tests, p ='), color="black") +
        labs(x = 'Pair type', y = 'Reaction times from choosing stimulus', fill = 'Social block') +
        my_theme
      
    },height = 600, width = 900)
    
    output$fig5 <- renderPlot({
    sort_social %>%
      select(ID, risky, trial_id, block_soc, pair_type, typeOfChoice,  rt) %>%
      filter(!is.na(risky)) %>%
      mutate(risky =recode(risky, '1' = 'risky_chosen', '0' = 'safe_chosen')) %>%
      group_by(ID, risky, pair_type) %>%
      summarise(mean_rt = mean(rt, na.rm = TRUE)) %>%
      ggplot(aes(x = risky, y = mean_rt, fill = pair_type)) +
      geom_boxplot() +
      stat_compare_means(method = "anova", label.y = 1.8) +
      stat_compare_means(comparisons = list(c("risky_chosen","safe_chosen")), method = "t.test", label.y = 1.6) +
      annotate(size = 4, geom="text", x=1.26, y=1.7, label=paste('t-tests, p ='), color="black") +
      labs(x = 'Pair type', y = 'Reaction times from choosing stimulus', fill = 'Social block') +
        my_theme
    },height = 600, width = 900)
    
    output$fig6 <- renderPlot({

      # High anxiety participants 
      
      sort_social %>%
        inner_join(anx_risk, by = "ID") %>%
        mutate(bins = rep(1:5,each = 36, times = 3*15)) %>%
        filter(typeOfChoice %in% c(1,6)) %>%
        group_by(block_soc, bins) %>%
        mutate(c_bins = n()) %>%
        group_by(block_soc, bins,pair_type) %>%
        mutate(perc_risky = mean(risky, na.rm = TRUE)) %>%
        mutate(se = sd(risky, na.rm = TRUE)/sqrt(c_bins)) %>%
        mutate(semin = perc_risky - se, semax = perc_risky + se) %>%
        ggplot(aes(x = factor(bins), y = perc_risky,color = pair_type)) +
        geom_point(size = 5) +
        geom_errorbar(aes(ymin=semin, ymax=semax), 
                      width=0.2, lty=1, size=0.5) +
        geom_abline(intercept = 0.5, slope = 0, size = 1) +
        scale_colour_manual(values = c('cadetblue2', 'darkolivegreen3')) +
        geom_line(aes(group = pair_type), size = 2) +
        labs(x = 'trial bins', y = 'P(risky)', color = 'Pair type') +
        facet_wrap(~block_soc) +
        ggtitle('High anxiety participants')
        
      
    }, height = 400, width = 700)
    
    output$fig7 <- renderPlot({
      sort_social %>%
        anti_join(anx_risk, by = "ID") %>%
        #filter(ID != c(25, 13, 28)) %>%
        mutate(bins = rep(1:5,each = 36, times = 3*13)) %>%
        filter(typeOfChoice %in% c(1,6)) %>%
        group_by(block_soc, bins) %>%
        mutate(c_bins = n()) %>%
        group_by(block_soc, bins,pair_type) %>%
        mutate(perc_risky = mean(risky, na.rm = TRUE)) %>%
        mutate(se = sd(risky, na.rm = TRUE)/sqrt(c_bins)) %>%
        mutate(semin = perc_risky - se, semax = perc_risky + se) %>%
        ggplot(aes(x = factor(bins), y = perc_risky,color = pair_type)) +
        geom_point(size = 5) +
        geom_errorbar(aes(ymin=semin, ymax=semax), 
                      width=0.2, lty=1, size=0.5) +
        geom_abline(intercept = 0.5, slope = 0, size = 1) +
        scale_colour_manual(values = c('cadetblue2', 'darkolivegreen3')) +
        geom_line(aes(group = pair_type), size = 2) +
        labs(x = 'trial bins', y = 'P(risky)', color = 'Pair type') +
        facet_wrap(~block_soc) +
        ggtitle('Low anxiety participants')
    }, height = 400, width = 700)
}
    
shinyApp(ui = ui, server = server)
