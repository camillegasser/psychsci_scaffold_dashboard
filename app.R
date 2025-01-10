# shiny web app

library(shiny)
library(bslib)
library(tidyverse)
library(DT)

sem <- function(x) sd(x)/sqrt(length(x))

get_sig <- function(p) {
  if (p < .001) { p_str <- '***' } 
  else if (p < .01) { p_str <- '**' }
  else if (p < .05) { p_str <- '*' }
  else { p_str <- '' }
  return(p_str)
}

# load data
memory_data <- read.csv('data/all_memory_data.csv', stringsAsFactors = F)

plot_theme <- theme_light() +
  theme(panel.grid = element_blank(),
        legend.position = 'none',
        text = element_text(size = 15, family = 'Helvetica'),
        axis.text = element_text(size = 14, family = 'Helvetica'),
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(margin = margin(t = 5)),
        axis.text.y = element_text(margin = margin(r = 5)))

## start building app

cards <- list(
  card(
    full_screen = TRUE,
    card_header('raw data'),
    dataTableOutput('raw_data')),
  card(
    full_screen = TRUE,
    card_header('overall memory performance'),
    plotOutput('memory_hist'),
  ),
  card(
    full_screen = TRUE,
    card_header('memory performance by condition'),
    plotOutput('memory_by_condition')
  ),
  card(
    full_screen = TRUE,
    card_header('memory performance by condition & sequence position'),
    plotOutput('memory_by_position')
  )
)

img_df <- data.frame(memory_test = c('order memory', 'spatial memory', 'item memory'), 
                     img_loc = c('img/order_memory.png', 'img/spatial_memory.png', 'img/item_memory.png'),
                     stringsAsFactors = FALSE)

exp_select <- selectInput(inputId = 'experiment', label = 'experiment', choices = c(1, 2, 3))
test_select <- selectInput(inputId = 'memory_test', label = 'memory test', choices = c('order memory', 'spatial memory', 'item memory'))

ui <- fluidPage(
  page_sidebar(
    title = 'Effects of predictable behavior on episodic memory',
    sidebar = tagList(div('Interactive data dashboard for',
                          a(href = 'https://journals.sagepub.com/doi/10.1177/09567976231158292', target = '_blank', 'Gasser & Davachi (2023)')),
                      div('Select the experiment & memory test you want to explore.',
                          hr(),
                          exp_select,
                          test_select),
                      imageOutput('test_image', height = 'auto'),
                      div(HTML('<br>
                                <font color="#fa7189"><b>predictable</b></font>: participants execute a <i>familiar & well-learned</i> action sequence during object encoding<br><br>
                                <font color="#2cada4"><b>variable</b></font>: participants execute a <i>random</i> action sequence during encoding'))),
    fluidRow(
      column(8, do.call(tagList, cards[1])),
      column(4, do.call(tagList, cards[2]))
    ),
    fluidRow(
      column(5, do.call(tagList, cards[3])),
      column(7, do.call(tagList, cards[4]))
    ),
    tags$head(includeCSS(path = 'www/style.css'))
  )
)

server <- function(input, output) {
  
  # image
  output$test_image <- renderImage({
    imgtxt <- img_df[img_df$memory_test == input$memory_test, 2]
    list(src = imgtxt,
         contentType = 'image/png',
         width = '100%'
    )
  }, deleteFile = FALSE)
  
  # raw memory data
  output$raw_data <- renderDataTable({
    filtered_data <- memory_data %>%
      filter(experiment == input$experiment, test == input$memory_test) %>%
      select(-c(experiment, test, response, chance))
    datatable(filtered_data, options = list(pageLength = 7))
  })
  
  # accuracy histogram
  output$memory_hist <- renderPlot({
    cc <- ifelse(input$memory_test == 'order memory', '#7dcfb6',
                 ifelse(input$memory_test == 'spatial memory', '#00b2ca', '#1d4e89'))
    
    filtered_data <- memory_data %>%
      group_by(subject, chance) %>%
      filter(experiment == input$experiment, test == input$memory_test) %>%
      summarise(mean_acc = mean(accuracy, na.rm = T),
                chance_level = mean(chance))
    
    ggplot(filtered_data, aes(x = mean_acc)) + 
      geom_histogram(fill = cc, color = cc, bins = 15, alpha = 0.5) +
      geom_vline(xintercept = mean(filtered_data$chance_level), linetype = 'dashed', color = 'grey20') +
      geom_vline(xintercept = mean(filtered_data$mean_acc), linewidth = 1.5, color = 'grey20') +
      labs(x = 'proportion correct\n(per subject)',
           subtitle = 'dotted line: chance\nsolid line: avg') +
      expand_limits(x = 1) +
      plot_theme
  })
  
  # accuracy by condition
  output$memory_by_condition <- renderPlot({
    
    filtered_data <- memory_data %>%
      filter(experiment == input$experiment, test == input$memory_test) %>%
      group_by(subject, condition) %>%
      summarise(mean_acc = mean(accuracy, na.rm = T),
                chance_level = mean(chance))
    
    group_data <- filtered_data %>%
      group_by(condition) %>%
      summarise(group_mean_acc = mean(mean_acc), group_sem_acc = sem(mean_acc)) %>%
      ungroup()
    
    if (nrow(filtered_data) > 0) {
      t <- t.test(mean_acc ~ condition, data = filtered_data, paired = T)
      p_str <- ifelse(t$p.value < .001, '< 0.001', sprintf('= %.3f', t$p.value))
      stat_str <- sprintf('paired t-test:\nt(%d) = %.2f, p %s%s',
                          t$parameter, t$statistic, p_str, get_sig(t$p.value))
    } else {
      stat_str <- ''
    }
    
    ggplot(filtered_data, aes(x = condition, y = mean_acc, fill = condition)) +
      geom_violin(color = 'white', alpha = 0.3) +
      geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1.2, color = 'white', stroke = 1.5) +
      geom_point(data = group_data, aes(x = condition, y = group_mean_acc), size = 3) +
      geom_errorbar(data = group_data, width = 0.35, size = 1.5,
                    aes(x = condition, y = group_mean_acc, ymin = group_mean_acc, ymax = group_mean_acc)) +
      geom_errorbar(data = group_data, width = 0.08, size = 1,
                    aes(x = condition, y = group_mean_acc,
                        ymin = group_mean_acc - group_sem_acc, ymax = group_mean_acc + group_sem_acc)) +
      scale_fill_manual(values = c('#fa7189','#2cada4')) +
      scale_color_manual(values = c('#fa7189','#2cada4')) +
      labs(x = 'condition', y = 'accuracy',
           subtitle = stat_str) +
      expand_limits(y = 0) +
      plot_theme +
      theme(plot.subtitle = element_text(face = 'bold'))
    
  })
  
  # accuracy by condition & sequence position
  output$memory_by_position <- renderPlot({
    
    filtered_data <- memory_data %>%
      filter(experiment == input$experiment, test == input$memory_test) %>%
      group_by(subject, condition, position) %>%
      summarise(mean_acc = mean(accuracy, na.rm = T),
                chance_level = mean(chance))
    
    group_data <- filtered_data %>%
      group_by(condition, position) %>%
      summarise(group_mean_acc = mean(mean_acc), group_sem_acc = sem(mean_acc)) %>%
      ungroup()
    
    ggplot(group_data, aes(x = position, y = group_mean_acc, color = condition)) +
      geom_jitter(data = filtered_data, aes(x = position, y = mean_acc, fill = condition),
                  width = 0.1, height = 0.02, color = 'white', alpha = 0.3, shape = 21, size = 4) +
      geom_line(size = 2) +
      geom_errorbar(aes(x = position, ymin = group_mean_acc - group_sem_acc, ymax = group_mean_acc + group_sem_acc),
                    width = 0, size = 1) +
      scale_x_continuous(breaks = seq(1, 6)) +
      scale_color_manual(values = c('#fa7189','#2cada4')) +
      scale_fill_manual(values = c('#fa7189','#2cada4')) +
      labs(x = 'sequence position', y = 'accuracy') +
      plot_theme
    
  })
  
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
