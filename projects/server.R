shinyServer(function(input, output, session) {

  output$map <- renderLeaflet({
    leaflet(
      data = prj_sites, width = "100%") |>
      # add base: blue bathymetry and light brown/green topography
      addProviderTiles(
        "Esri.OceanBasemap",
        options = providerTileOptions(
          variant = "Ocean/World_Ocean_Base")) |>
      # add reference: placename labels and borders
      addProviderTiles(
        "Esri.OceanBasemap",
        options = providerTileOptions(
          variant = "Ocean/World_Ocean_Reference")) |>
      addMarkers(
        label        = ~label_html, 
        popup        = ~popup_html) })
  
  output$p <- renderPlotly({
    
    #pick the color and shape scale
    scale <- RColorBrewer::brewer.pal(n=10, name = 'PiYG')
    scale <- scale[c(1:5, 5, 7, 7:10, 10)]
    shp   <- c(rep(24, 3), 25, 24, 25, 24, 25, rep(24, 3), 25) 
    
    #the input to ggplot is what determines the tooltip label
    g <- ggplot(
      d_permits, 
      aes(text = paste('License Date: ', license_date, '\nProject Name: ', project_name, '\nPermit Type: ', permit_type))) +
      #the segment is a gray bar that covers the time period of the permits
      geom_segment(
        data = d_times, 
        aes(x = date_beg, y = project_name, xend = date_end, yend = project_name, color = project_status), size = 4) +
      #the points have colors and shapes indicating different permit types
      geom_point(data = d_permits, 
                 aes(x = license_date, y = project_name, fill = permit_type, shape = permit_type), size = 4, stroke = 0) +
      scale_color_manual(name = "", values = c("#30A4E1", "#999999"), breaks = c("Active Project", "Inactive Project"))+
      scale_fill_manual(name = "", values = scale) + 
      scale_shape_manual(name = "", values = shp) +
      #label the plot
      labs(title = "MHK Project Timeline", x = "Year of Project", y = "") +
      facet_grid(rows = vars(technology_type), scales='free_y', space = 'free') +
      #choose a theme
      theme(panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            #legend.margin=margin(100,100,100,100),
            #legend.box.margin=margin(20,20,20,20),
            #legend.position = c(0.9, 0.84),
            #legend.background = element_rect(fill = "transparent", colour = NA),
            #axis.text.y = axis.groups(unique(d_times$technology_type)),
            axis.text.x = element_text(color="black", size=12, angle=45, vjust=1, hjust = 1),
            axis.text.y = element_text(color="black", size=12, vjust = -1),
            axis.title.y=element_text(face="bold", size=13),
            axis.title.x=element_text(face="bold", size=13),
            #plot.margin = margin(.15, .2, 0, 0, "cm"),
            plot.background = element_rect(fill = "transparent", colour = NA))
    
    # interactive plot with tooltip
    #specify the tooltip in the ggplotly function to get custom text
    ggplotly(g, tooltip = 'text', height = 700, width = 1000)
  })

})
