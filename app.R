library(shiny)

# WICHTIG: Upload-Limit auf 30 MB erhöhen
options(shiny.maxRequestSize = 30 * 1024^2)

# --- UI ---
ui <- fluidPage(
  titlePanel("ER Model Viewer (Browser Edition)"),
  
  sidebarLayout(
    sidebarPanel(
      # Upload
      fileInput("file1", "Upload CSV Data Dictionary", accept = c(".csv", "text/csv")),
      
      # Trennzeichen: Jetzt mit Pipe (|) als Standard
      radioButtons("sep", "CSV Trennzeichen:",
                   choices = c("Pipe (|)" = "|", 
                               "Semikolon (;)" = ";", 
                               "Komma (,)" = ","),
                   selected = "|", inline = TRUE),
      
      hr(),
      
      h4("Suche & Filter"),
      selectizeInput("table_search", "Tabelle suchen:", choices = NULL, multiple = TRUE, 
                     options = list(placeholder = 'Tippen zum Suchen...')),
      
      helpText("ODER Slider nutzen:"),
      sliderInput("table_limit", "Zeige Top N Tabellen:", min = 1, max = 50, value = 5),
      
      hr(),
      checkboxInput("auto_connect", "Beziehungen anzeigen?", value = TRUE),
      checkboxInput("show_ghosts", "Externe Referenzen (Ghosts)?", value = TRUE),
      
      hr(),
      selectInput("rank_dir", "Ausrichtung:", choices = c("Links-nach-Rechts" = "LR", "Oben-nach-Unten" = "TB")),
      selectInput("line_style", "Linienstil:", choices = c("Orthogonal" = "ortho", "Kurvig" = "spline", "Polyline" = "polyline"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("ER Diagramm", 
                 DiagrammeR::grVizOutput("er_plot", height = "800px")
        ),
        tabPanel("Daten Prüfung", 
                 h4("Geparste CSV Daten"),
                 tableOutput("debug_table")
        )
      )
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  # 1. READ & INDEX
  full_data <- reactive({
    req(input$file1)
    
    # readr::read_delim mit dynamischem Trenner
    raw_df <- readr::read_delim(
      input$file1$datapath, 
      delim = input$sep, 
      col_types = readr::cols(.default = "c"), 
      show_col_types = FALSE
    )
    
    # Pre-process
    df_indexed <- raw_df |>
      dplyr::mutate(global_row_id = dplyr::row_number()) |>
      janitor::clean_names() |>
      dplyr::rename(
        table_name = ida_view,
        col_name   = ida_attribut,
        pk_flag    = primarschlussel,
        data_type  = datentyp
      ) |>
      dplyr::filter(!is.na(table_name)) |>
      dplyr::mutate(table_name = trimws(table_name))
    
    # Sortier-Logik
    table_order <- df_indexed |>
      dplyr::group_by(table_name) |>
      dplyr::summarise(first_appearance = min(global_row_id)) |>
      dplyr::arrange(first_appearance)
    
    # Join & Clean
    df_clean <- df_indexed |>
      dplyr::left_join(table_order, by = "table_name") |>
      dplyr::group_by(table_name) |>
      dplyr::mutate(col_order = dplyr::row_number()) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        table_id = paste0("t_", gsub("[^a-zA-Z0-9]", "_", table_name)),
        table_label_safe = htmltools::htmlEscape(table_name),
        col_label_safe   = htmltools::htmlEscape(col_name),
        type_label_safe  = htmltools::htmlEscape(data_type),
        is_pk = grepl("(?i)(yes|x|true|1|j|y|ja)", as.character(pk_flag))
      )
    
    # FK Detection
    all_pks <- df_clean |> 
      dplyr::filter(is_pk) |> 
      dplyr::select(pk_col = col_name, target_table = table_id) |> 
      dplyr::distinct() |>
      dplyr::filter(!tolower(pk_col) %in% c("id", "status", "source", "mandant"))
    
    df_enriched <- df_clean |>
      dplyr::left_join(all_pks, by = c("col_name" = "pk_col"), relationship = "many-to-many") |>
      dplyr::mutate(is_fk = !is.na(target_table) & (table_id != target_table)) |>
      dplyr::group_by(table_id, col_name) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::arrange(first_appearance, col_order)
    
    df_enriched
  })
  
  # 2. UPDATE SEARCH BOX
  observe({
    req(full_data())
    tables <- unique(full_data()$table_name)
    updateSelectizeInput(session, "table_search", choices = tables, server = TRUE)
  })
  
  # 3. FILTER LOGIC
  filtered_data <- reactive({
    req(full_data())
    df <- full_data()
    ordered_ids <- unique(df$table_id)
    
    target_ids <- if (!is.null(input$table_search) && length(input$table_search) > 0) {
      df |> 
        dplyr::filter(table_name %in% input$table_search) |> 
        dplyr::pull(table_id) |> 
        unique()
    } else {
      head(ordered_ids, input$table_limit)
    }
    
    df |> 
      dplyr::filter(table_id %in% target_ids) |>
      dplyr::arrange(first_appearance, col_order)
  })
  
  # 4. GRAPH CODE
  graph_code <- reactive({
    req(filtered_data())
    df_render <- filtered_data()
    
    # Nodes
    df_nodes <- df_render |>
      dplyr::mutate(base_port_id = gsub("[^a-zA-Z0-9]", "_", col_name)) |>
      dplyr::arrange(dplyr::desc(is_pk), col_order) |>
      dplyr::mutate(
        flag_text = dplyr::case_when(is_pk & is_fk ~ "PK,FK", is_pk ~ "PK", is_fk ~ "FK", TRUE ~ ""),
        flag_html = ifelse(flag_text != "", paste0("<b>", flag_text, "</b>"), ""),
        label_html = glue::glue(
          "<tr><td align='right' width='30' port='{base_port_id}_L'>{flag_html}</td><td align='left'> {col_label_safe} </td><td align='right' port='{base_port_id}_R'><i>{type_label_safe}</i></td></tr>"
        )
      ) |>
      dplyr::group_by(table_id, table_label_safe) |>
      dplyr::summarise(cols_string = paste(label_html, collapse = ""), .groups = "drop") |>
      dplyr::mutate(node_def = glue::glue('{table_id} [label=<<table border="0" cellborder="1" cellspacing="0"><tr><td colspan="3" bgcolor="#E0E0E0"><b>{table_label_safe}</b></td></tr>{cols_string}</table>> shape=plain]'))
    
    main_nodes_def <- paste(df_nodes$node_def, collapse = "\n")
    
    # Edges & Ghosts
    ghost_nodes_def <- ""
    all_edges <- ""
    
    if (input$auto_connect) {
      relationships <- df_render |>
        dplyr::filter(is_fk) |>
        dplyr::mutate(col_port_id = gsub("[^a-zA-Z0-9]", "_", col_name)) |>
        dplyr::select(source_table = table_id, target_table, col_port_id) |>
        dplyr::distinct()
      
      visible_ids <- unique(df_render$table_id)
      ghost_ids   <- setdiff(relationships$target_table, visible_ids)
      
      if (input$show_ghosts && length(ghost_ids) > 0) {
        ghost_defs <- data.frame(id = ghost_ids) |>
          dplyr::mutate(label = sub("^t_", "", id), def = glue::glue('{id} [label="{label}", shape=box, style=dashed, fontcolor="#777777", color="#777777", fontsize=10]'))
        ghost_nodes_def <- paste(ghost_defs$def, collapse = "\n")
      }
      
      if (nrow(relationships) > 0) {
        relationships <- relationships |>
          dplyr::mutate(
            is_ghost = target_table %in% ghost_ids,
            should_draw = !is_ghost | (is_ghost & input$show_ghosts),
            edge_def = dplyr::if_else(is_ghost,
                                      glue::glue("{source_table}:{col_port_id}_R:e -> {target_table} [arrowtail=crow, arrowhead=none, dir=both, style=dashed, color=\"#999999\"]"),
                                      glue::glue("{source_table}:{col_port_id}_R:e -> {target_table}:{col_port_id}_L:w [arrowtail=crow, arrowhead=tee, dir=both, color=\"#555555\"]")
            )
          ) |>
          dplyr::filter(should_draw)
        all_edges <- paste(relationships$edge_def, collapse = "\n")
      }
    }
    
    glue::glue("digraph ER {{ graph [rankdir={input$rank_dir}, splines={input$line_style}, nodesep=0.8, ranksep=1.0] node [fontname = \"Helvetica\", fontsize=10] edge [arrowhead=none, arrowtail=crow] \n {main_nodes_def} \n {ghost_nodes_def} \n {all_edges} }}")
  })
  
  # 5. OUTPUTS
  output$er_plot <- DiagrammeR::renderGrViz({ DiagrammeR::grViz(graph_code()) })
  
  output$debug_table <- renderTable({
    filtered_data() |> 
      dplyr::select(Table = table_name, Column = col_name, Type = data_type, PK = is_pk, FK = is_fk, Target = target_table) |>
      head(50)
  })
}

shinyApp(ui, server)