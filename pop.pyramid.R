

# Function to Create a Population Pyramid by age groups and gender -------------------------------------------

pop.pyramid <- function (data, pyear){
  pyear <- as.character(pyear)
  
  gg.female <- 
    ggplot(data = data, aes(x = ageg, y = female, fill = ageg)) +
    geom_bar(stat = "identity") + 
    coord_flip() +
    ylab("Female") +
    #ggtitle("Female") +
    scale_y_continuous(trans = 'reverse') +
    theme(legend.position = 'none' ,
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          #axis.text.x = element_blank(),
          axis.ticks = element_blank()
    )
  
  ## Male 4 pyramid
  gg.male <- 
    ggplot(data = data, aes(x=ageg, y = male, fill = ageg)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    ylab("Male") +
    # ggtitle("Male") +
    theme(legend.position = 'none' ,
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          # axis.text.x = element_blank(),
          axis.ticks = element_blank()
    )
  
  
  # Age group column 4 pyramid
  gg.ages <- 
    ggplot(data = data, aes(x = ageg,  y = 0, fill = ageg)) + 
    geom_bar(stat = "identity") +
    geom_text( aes( y = 0,  label = as.character(ageg)), size = 3) +
    coord_flip() +
    theme(plot.title = element_text('Ages'),
          legend.position = 'none' ,
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          # axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
    )  +
    ylab("- ")
  ggtitle(paste("ASES Population Pyramid,", pyear, sep = " ")) +
    theme(plot.title = element_text(size = 20, hjust = 0.5, vjust = 4))
  # The bellow chunk has the grpah title but activate the previous to line to include the title with the graph
  
  library(grid)
  grid.newpage()
pushViewport(
    viewport(x = unit(0.5, "npc"), y = unit(0.5, "npc"),
             width = unit(1, "npc"), 
             layout = grid.layout(nrow = 1,
                                  ncol = 3, 
                                  widths = c(0.5, .2, 0.5)))
  )
  
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  print(gg.female, vp = vplayout(1,1))
  print(gg.ages,   vp = vplayout(1,2))
  print(gg.male,   vp = vplayout(1,3))

}

