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
        plot.subtitle = element_text(margin = margin(b = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(margin = margin(t = 5)),
        axis.text.y = element_text(margin = margin(r = 5)))


library(shiny)

img_df <- data.frame(memory_test = c('order memory', 'spatial memory', 'item memory'), 
                     img_loc = c('img/order_memory.png', 'img/spatial_memory.png', 'img/item_memory.png'),
                     stringsAsFactors = FALSE)

ui <- navbarPage(
  title = 'Effects of predictable behavior on episodic memory',
  theme = shinythemes::shinytheme('simplex'),
  
  # include external CSS and custom inline styles
  # tags$head(includeCSS(path = 'www/style.css')),
  tags$style(HTML('* { 
                      margin-top: 2px;
                      margin-bottom: 2px;
                   }')),
  
  # header
  header = tags$div(
    style = 'display: flex; flex-direction: column; margin-right: 15px; margin-top: 0px; margin-left: 15px',
    
    # Row for selectors
    tags$div(
      style = 'display: flex; align-items: center; gap: 10px;',
      selectInput(
        inputId = 'experiment',
        label = 'experiment',
        choices = c(1, 2, 3),
        width = '50%'
      ),
      selectInput(
        inputId = 'memory_test',
        label = 'memory test',
        choices = c('order memory', 'spatial memory', 'item memory'),
        width = '50%'
      )
    ),
    
    # Row for additional text
    tags$div(
      style = 'margin-top: 5px;',
      p('Note that spatial memory was only tested in Experiments 1 & 2, and item memory was only tested in Experiment 3.')
    )
  ),
  
  # Define tabs
  tabsetPanel(
    # selected = 'memory results',
    
    ## finicky if you want this to look good at multiple sizes... just leaving out for now
    # tabPanel(
    #   'data overview',
    #   fluidRow(
    #     column(width = 12,
    #            br(),
    #            p('The goal of this experiment was to test whether engaging in familiar actions enhances
    #               memory for simultaneous experience.',
    #              style = 'font-size:14px; margin-left:5px'),
    #            p('During the main task, subjects "ran errands" in two stores. Each errand involved visiting a sequence
    #               of aisles (by making simple motor responses), and "collecting" an item from each aisle.
    #               After the errands were over, memory was tested for the collected items.',
    #              style = 'font-size:14px; margin-left:5px'),
    #            HTML('<p style="font-size:14px; margin-left:5px">Participants visited two different stores during the experiment.
    #               In one of these stores (“<font color="#fa7189"><b>predictable</b></font>”), the sequence of aisles was always the exact same — 
    #               and participants memorized this sequence before the errands began. In the other (“<font color="#2cada4"><b>variable</b></font>”),
    #               the aisle sequence changed on each errand.'),
    #            HTML('<p style="font-size:14px; margin-left:5px">For more details,
    #                  see <a href="https://journals.sagepub.com/doi/10.1177/09567976231158292">this paper</a>.</p>'),
    #     ),
    #     # column(width = 7,
    #     #        imageOutput('task_img', width = '95%')
    #     # )
    #   )
    # ),
    
    # Second panel: Data plots
    tabPanel(
      'memory results',
      fluidRow(
        column(width = 6,
               h4('Distribution of overall memory accuracy'),
               plotOutput(outputId = 'memory_histogram')),
        column(width = 6,
               h4('Memory accuracy by condition'),
               plotOutput(outputId = 'memory_by_condition'))
      ),
      fluidRow(
        column(width = 6,
               h4('Memory accuracy by sequence position'),
               p('Each errand involved visiting 6 aisles and collecting 6 items.'),
               plotOutput(outputId = 'memory_by_position')),
        column(width = 6,
               h4('Memory accuracy by task block'),
               p('The experiment involved 8 blocks in total (each with 4 errands).'),
               plotOutput(outputId = 'memory_by_block'))
      )
    ),
    
    # third panel: raw data
    tabPanel('raw data',
             dataTableOutput('raw_data')
    )
  )
)

# define the server
server <- function(input, output) {
  
  # imgae of task
  output$task_img <- renderImage({
    list(src = 'img/task_graphic.jpg',
         contentType = 'image/png',
         width = '50%',
         height = 'auto'
    )
  }, deleteFile = FALSE)
  
  # # image of memory test
  # output$memory_test_img <- renderImage({
  #   imgtxt <- img_df[img_df$memory_test == input$memory_test, 2]
  #   list(src = imgtxt,
  #        contentType = 'image/png',
  #        width = '90%'
  #   )
  # }, deleteFile = FALSE)
  
  output$memory_histogram <- renderPlot({
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
      labs(x = 'memory accuracy\n(per subject)',
           subtitle = 'solid line: mean across subjects') +
      expand_limits(x = 1) +
      plot_theme
  })
  
  # raw memory data table
  output$raw_data <- renderDataTable({
    filtered_data <- memory_data %>%
      filter(experiment == input$experiment, test == input$memory_test) %>%
      select(-c(experiment, test, response, chance))
    datatable(filtered_data, options = list(pageLength = 10))
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
      labs(x = 'condition', y = 'memory accuracy',
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
      plot_theme +
      theme(legend.position = 'bottom')
    
  })
  
  output$memory_by_block <- renderPlot({
    
    filtered_data <- memory_data %>%
      filter(experiment == input$experiment, test == input$memory_test) %>%
      group_by(subject, condition, block) %>%
      summarise(mean_acc = mean(accuracy, na.rm = T))
    
    group_data <- filtered_data %>%
      group_by(condition, block) %>%
      summarise(group_mean_acc = mean(mean_acc), group_sem_acc = sem(mean_acc)) %>%
      ungroup()
    
    # plot
    ggplot(group_data, aes(x = block, y = group_mean_acc, fill = condition, color = condition)) +
      geom_jitter(data = filtered_data, aes(x = block, y = mean_acc, fill = condition),
                  color = 'white', alpha = 0.4, shape = 21, size = 2.3,
                  position = position_jitterdodge(jitter.width = 0.15, jitter.height = 0.02, dodge.width = 0.9)) +
      # geom_line(size = 1.5) +
      geom_bar(alpha = 0.5, stat = 'summary', position = position_dodge(0.9)) +
      geom_errorbar(aes(x = block, ymin = group_mean_acc - group_sem_acc, ymax = group_mean_acc + group_sem_acc),
                    width = 0, size = 1, position = position_dodge(0.9)) +
      scale_fill_manual(values = c('#fa7189','#2cada4')) +
      scale_color_manual(values = c('#fa7189','#2cada4')) +
      scale_x_continuous(breaks = 1:8) +
      labs(x = 'block number', y = 'memory accuracy') +
      plot_theme +
      theme(legend.position = 'bottom')
    
  })
  
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
