# ===================== Act 2 Interaction Network Explorer ====================

networkUI <- function(id) {
  ns <- NS(id)
  agent_choices <- setNames(AGENTS$agent_id, AGENTS$agent_label)
  role_choices  <- setNames(AGENTS$role, AGENTS$agent_label)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 250,
      title = htmltools::div(style = "display:flex; align-items:center; gap:6px;",
        htmltools::span("Controls"),
        bslib::popover(
          htmltools::tags$button(type = "button", `aria-label` = "About these controls",
            style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.72rem;
                     border:1px solid #C7D0D4;border-radius:50%;padding:0 5px;line-height:1.2;", "?"),
          htmltools::tags$b("What these controls do"), htmltools::tags$br(),
          htmltools::HTML("<b>Time window</b> sets which rounds the graph and matrix cover. Follow the timeline grows the network as you drag the clock at the top.<br><br><b>Channels shown</b> filters open or hidden ties.<br><br><b>Graph shows</b> switches between one agent's ties and the whole team. Centre on picks which agent to focus when showing one."),
          placement = "right", options = list(trigger = "focus"))),
      htmltools::tags$style(htmltools::HTML(
        ".gb-net-side .form-group{margin-bottom:8px;} .gb-net-side label{font-size:.78rem;margin-bottom:2px;} .gb-net-side .btn{font-size:.72rem;padding:3px 8px;}")),
      htmltools::div(class = "gb-net-side",

      selectInput(ns("scope"), "Time window",
        choices = c("Follow the timeline" = "uptonow",
                    "Calm weeks, R0 to R12" = "base",
                    "Crisis day, R13 to R22" = "crisis"),
        selected = "uptonow"),

      shinyWidgets::checkboxGroupButtons(ns("vis"), "Channels shown",
        choices = c("Open" = "open", "Covert" = "covert"),
        selected = c("open", "covert"), size = "sm"),

      htmltools::tags$hr(style = "margin:8px 0;"),

      shinyWidgets::radioGroupButtons(ns("mode"), "Graph shows",
        choices = c("One agent" = "ego", "Whole team" = "full"),
        selected = "ego", size = "sm"),
      conditionalPanel("input.mode == 'ego'", ns = ns,
        selectInput(ns("focus"), "Centre on", choices = agent_choices,
                    selected = "legal_agent")),

      htmltools::div()
      )
    ),

    # ---- headline stats ----
    htmltools::div(
      style = "display:flex; gap:8px; margin-bottom:10px;",
      htmltools::div(
        style = "flex:1; min-width:0; padding:8px 12px; background:#F4F7F8;
                 border:1px solid #E0E7EA; border-radius:8px;",
        htmltools::div(style = "font-size:.66rem; letter-spacing:.04em; text-transform:uppercase; color:#6B7B83; margin-bottom:3px;",
          "Busiest agent"),
        uiOutput(ns("stat_busiest"))),
      htmltools::div(
        style = "flex:1; min-width:0; padding:8px 12px; background:#F4F7F8;
                 border:1px solid #E0E7EA; border-radius:8px;",
        htmltools::div(style = "font-size:.66rem; letter-spacing:.04em; text-transform:uppercase; color:#6B7B83; margin-bottom:3px;",
          "Off the record"),
        uiOutput(ns("stat_covert"))),
      htmltools::div(
        style = "flex:1; min-width:0; padding:8px 12px; background:#F4F7F8;
                 border:1px solid #E0E7EA; border-radius:8px;",
        htmltools::div(style = "font-size:.66rem; letter-spacing:.04em; text-transform:uppercase; color:#6B7B83; margin-bottom:3px;",
          "The Judge"),
        uiOutput(ns("stat_judge")))
    ),

    # ---- focused graph (left) and matrix (right), side by side ----
    bslib::layout_columns(
      col_widths = c(5, 7), fill = FALSE,

      bslib::card(
        bslib::card_header(
          htmltools::div(style = "display:flex; align-items:center; justify-content:center; gap:8px; height:24px; width:100%;",
            htmltools::span(textOutput(ns("net_title"), inline = TRUE)),
            bslib::popover(
              htmltools::tags$button(type = "button", `aria-label` = "About this graph",
                style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.72rem;
                         border:1px solid #C7D0D4;border-radius:50%;padding:0 5px;line-height:1.2;", "?"),
              htmltools::tags$b("The interaction graph"), htmltools::tags$br(),
              htmltools::HTML("<b>Blue solid lines</b> are open, monitored channels. <b>Orange dashed lines</b> are covert channels, the Shadow room, direct messages and anonymous posts. Thicker lines carry more messages, bigger circles are more central. The <b>dark square</b> is the Judge. Click any node to recentre on it. Use the controls to centre on one agent, compare two, or show the whole team."),
              placement = "bottom", options = list(trigger = "focus")))),
        visNetwork::visNetworkOutput(ns("net"), height = "330px"),
        htmltools::div(
          style = "display:flex; flex-wrap:wrap; gap:10px 16px; align-items:center;
                   padding:4px 8px 8px; font-size:.72rem; color:#4A5A60;",
          htmltools::tags$span(style = "display:inline-flex; align-items:center; gap:5px;",
            htmltools::tags$svg(width = "26", height = "8", htmltools::HTML(
              sprintf("<line x1='0' y1='4' x2='26' y2='4' stroke='%s' stroke-width='2.5'/>", COL$cool))),
            "open channel"),
          htmltools::tags$span(style = "display:inline-flex; align-items:center; gap:5px;",
            htmltools::tags$svg(width = "26", height = "8", htmltools::HTML(
              sprintf("<line x1='0' y1='4' x2='26' y2='4' stroke='%s' stroke-width='2.5'/>", COL$warm))),
            "covert channel"),
          htmltools::tags$span(style = "display:inline-flex; align-items:center; gap:5px;",
            htmltools::tags$svg(width = "26", height = "10", htmltools::HTML(
              "<line x1='0' y1='3' x2='26' y2='3' stroke='#8A97A0' stroke-width='1'/><line x1='0' y1='8' x2='26' y2='8' stroke='#8A97A0' stroke-width='3.5'/>")),
            "thin to thick, more messages"),
          htmltools::tags$span(style = "display:inline-flex; align-items:center; gap:5px;",
            htmltools::tags$svg(width = "14", height = "12", htmltools::HTML(
              "<circle cx='7' cy='6' r='5' fill='#8A97A0'/>")),
            "agent, size is how active"),
          htmltools::tags$span(style = "display:inline-flex; align-items:center; gap:5px;",
            htmltools::tags$svg(width = "14", height = "12", htmltools::HTML(
              "<rect x='2' y='1' width='10' height='10' fill='#33414A'/>")),
            "the Judge")
        ),
      ),

      bslib::card(
        bslib::card_header(
          htmltools::div(style = "display:flex; align-items:center; justify-content:center; gap:8px; height:24px; width:100%;",
            htmltools::span("Who spoke to whom"),
            bslib::popover(
              htmltools::tags$button(type = "button", `aria-label` = "About this matrix",
                style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.72rem;
                         border:1px solid #C7D0D4;border-radius:50%;padding:0 5px;line-height:1.2;", "?"),
              htmltools::tags$b("Who spoke to whom"), htmltools::tags$br(),
              htmltools::HTML("Each row is a sender, each column a receiver, and the darker the cell the more messages flowed that way. <b>Rows send, columns receive.</b> The Judge's row and column stay pale, it barely spoke and was barely spoken to. Legal's row is the busiest. For a team where almost everyone talks to everyone, this grid is clearer than a tangle of lines. Click a cell to focus that pair in the graph."),
              placement = "bottom", options = list(trigger = "focus")))),
        ggiraph::girafeOutput(ns("matrix"), height = "360px")
      )
    ),

    # ---- the activity table ----
    bslib::card(
      bslib::card_header(
        htmltools::div(style = "display:flex; align-items:center; justify-content:center; gap:8px; width:100%;",
          htmltools::span("Who worked in the open, who worked in the shadows"),
          bslib::popover(
            htmltools::tags$button(type = "button", `aria-label` = "About centrality",
              style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.72rem;
                       border:1px solid #C7D0D4;border-radius:50%;padding:0 5px;line-height:1.2;", "?"),
            htmltools::tags$b("Open vs hidden activity"), htmltools::tags$br(),
            "For each agent in this window: how many messages they sent, and how many of their ties ran on open versus hidden channels. Hidden share is the percent of their ties that were covert. A high hidden share means that agent did most of its work off the record.",
            placement = "bottom", options = list(trigger = "focus")))),
      DT::DTOutput(ns("nodes_tbl"))
    )
  )
}

networkServer <- function(id, current_round) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rounds_keep <- reactive({
      switch(input$scope %||% "uptonow",
        uptonow = 0:current_round(),
        base    = 0:12,
        crisis  = 13:22,
        0:current_round())
    })
    vis_keep <- reactive(input$vis %||% c("open", "covert"))
    roles_keep <- reactive(input$roles %||% AGENTS$role)
    tie_src <- reactive(NULL)

    graph_data <- reactive({
      gb_network(
        rounds_keep = rounds_keep(), vis_keep = vis_keep(),
        focus = input$focus %||% "legal_agent", mode = input$mode %||% "ego",
        dyad = c(input$dyad_a %||% "legal_agent", input$dyad_b %||% "social_media_agent"),
        roles_keep = roles_keep(), min_w = input$min_w %||% 1,
        centrality = input$cent %||% "degree", tie_src = tie_src())
    })

    output$net_title <- renderText({
      rk <- rounds_keep()
      sprintf("Interaction network, rounds %d to %d",
              min(rk), max(rk))
    })

    # at seven nodes a full re-render is instant, so we re-render reactively
    # rather than diffing through visNetworkProxy
    output$net <- visNetwork::renderVisNetwork({
      g <- graph_data()
      vn <- visNetwork::visNetwork(g$nodes, g$edges)
      for (k in seq_len(nrow(AGENTS))) {
        vn <- vn %>% visNetwork::visGroups(
          groupname = AGENTS$agent_label[k], color = AGENTS$color[k],
          shape = if (AGENTS$is_monitor[k]) "square" else "dot")
      }
      vn %>%
        visNetwork::visNodes(font = list(size = 16), borderWidth = 1) %>%
        visNetwork::visEdges(arrows = "to",
          smooth = list(enabled = TRUE, type = "curvedCW", roundness = 0.18)) %>%
        visNetwork::visPhysics(enabled = FALSE) %>%
        visNetwork::visInteraction(
          zoomView = FALSE,          # scroll no longer zooms, fixes the lost-graph problem
          dragView = TRUE,           # drag to pan still works
          navigationButtons = TRUE,  # on-screen + and - buttons for zoom
          keyboard = FALSE,
          tooltipStyle = paste0(
            "position:fixed; visibility:hidden; padding:5px 8px; ",
            "font-family:inherit; font-size:11.5px; line-height:1.4; color:#2A3439; ",
            "background:#FFFFFF; border:1px solid #C7D0D4; border-radius:6px; ",
            "box-shadow:0 2px 8px rgba(0,0,0,0.12); max-width:180px; white-space:normal; z-index:10000;")) %>%
        visNetwork::visOptions(
          highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)) %>%
        visNetwork::visEvents(
          click = sprintf(
            "function(p){ if(p.nodes.length>0){ Shiny.setInputValue('%s', p.nodes[0], {priority:'event'}); } }",
            ns("node_click")),
          stabilized = "function(){ this.moveTo({position:{x:0,y:0}, scale:0.85}); }")
    })

    # clicking a node makes it the focal agent
    observeEvent(input$node_click, {
      if (input$node_click %in% AGENTS$agent_id)
        updateSelectInput(session, "focus", selected = input$node_click)
    })

    # ----- adjacency matrix -----
    output$matrix <- ggiraph::renderGirafe({
      m <- gb_matrix(rounds_keep(), vis_keep())
      g <- ggplot2::ggplot(m, ggplot2::aes(to_lab, from_lab)) +
        ggiraph::geom_tile_interactive(
          ggplot2::aes(fill = weight,
                       tooltip = sprintf("%s to %s: %d message(s)", from_lab, to_lab, weight),
                       data_id = paste(from_lab, to_lab, sep = "||")),
          colour = "white", linewidth = 0.6) +
        ggplot2::scale_fill_gradient(low = "#EEF3F4", high = COL$primary, name = "msgs") +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 40, hjust = 1, size = 8),
          axis.text.y = ggplot2::element_text(size = 8),
          panel.grid = ggplot2::element_blank())
      ggiraph::girafe(ggobj = g, width_svg = 4.2, height_svg = 3.4,
        options = list(ggiraph::opts_selection(type = "single"),
                       ggiraph::opts_tooltip(css = paste0(
                         "padding:6px 10px; font-size:12px; color:#2A3439; ",
                         "background:#FFFFFF; border:1px solid #C7D0D4; border-radius:6px; ",
                         "box-shadow:0 2px 8px rgba(0,0,0,0.12);")),
                       ggiraph::opts_toolbar(saveaspng = FALSE)))
    })

    # clicking a matrix cell centres the graph on that sender
    observeEvent(input$matrix_selected, {
      sel <- input$matrix_selected
      if (length(sel) == 0 || sel == "") return()
      parts <- strsplit(sel, "\\|\\|")[[1]]
      a <- AGENTS$agent_id[AGENTS$agent_label == parts[1]]
      if (length(a)) {
        shinyWidgets::updateRadioGroupButtons(session, "mode", selected = "ego")
        updateSelectInput(session, "focus", selected = a)
      }
    })

    # ----- tables -----
    net_stats <- reactive({
      rk <- rounds_keep()
      mm <- msg %>% dplyr::filter(round %in% rk)
      # covert share counted by TIES (sender to recipient links), matching what the
      # graph and matrix actually draw, so the number agrees with the picture
      tt <- ties_all %>% dplyr::filter(round %in% rk)
      total <- nrow(tt)
      covert <- sum(tt$visibility == "covert", na.rm = TRUE)
      # raw message counts (context line under the percentage)
      msg_total  <- nrow(mm)
      msg_covert <- sum(mm$visibility == "covert", na.rm = TRUE)
      # busiest sender, by messages sent
      bs <- mm %>% dplyr::count(agent_id, name = "n") %>%
        dplyr::left_join(AGENTS %>% dplyr::select(agent_id, agent_label), by = "agent_id") %>%
        dplyr::arrange(dplyr::desc(n)) %>% dplyr::slice(1)
      # judge reach
      j_sent <- sum(mm$agent_id == "judge_agent", na.rm = TRUE)
      j_recv <- tt %>% dplyr::filter(to == "judge_agent") %>% nrow()
      list(
        busiest_lab = if (nrow(bs)) bs$agent_label[1] else "None",
        busiest_n   = if (nrow(bs)) bs$n[1] else 0,
        covert = covert, total = total,
        covert_pct = if (total > 0) round(100 * covert / total) else 0,
        msg_covert = msg_covert, msg_total = msg_total,
        j_sent = j_sent, j_recv = j_recv)
    })

    win_vol <- reactive({
      rk <- rounds_keep(); vk <- vis_keep()
      sent <- msg %>% dplyr::filter(round %in% rk) %>%
        dplyr::count(agent_id, name = "sent")
      tw <- ties_all %>% dplyr::filter(round %in% rk, visibility %in% vk)
      # each tie touches two agents, count the footprint of both endpoints by channel
      foot <- dplyr::bind_rows(
        tw %>% dplyr::transmute(agent_id = from, visibility),
        tw %>% dplyr::transmute(agent_id = to,   visibility))
      openc <- foot %>% dplyr::filter(visibility == "open") %>%
        dplyr::count(agent_id, name = "open_n")
      hidc  <- foot %>% dplyr::filter(visibility == "covert") %>%
        dplyr::count(agent_id, name = "hidden_n")
      recv <- tw %>% dplyr::count(to, name = "recv") %>% dplyr::rename(agent_id = to)
      AGENTS %>% dplyr::select(agent_id, agent_label, role) %>%
        dplyr::left_join(sent, by = "agent_id") %>%
        dplyr::left_join(recv, by = "agent_id") %>%
        dplyr::left_join(openc, by = "agent_id") %>%
        dplyr::left_join(hidc, by = "agent_id") %>%
        dplyr::mutate(sent = tidyr::replace_na(sent, 0),
                      recv = tidyr::replace_na(recv, 0),
                      open_n = tidyr::replace_na(open_n, 0),
                      hidden_n = tidyr::replace_na(hidden_n, 0))
    })

    output$stat_busiest <- renderUI({
      st <- net_stats()
      htmltools::div(
        htmltools::div(style = "font-size:.95rem; font-weight:700; color:#2A3439;", st$busiest_lab),
        htmltools::div(style = "font-size:.72rem; color:#6B7B83; margin-top:1px;",
          sprintf("%d messages sent, the loudest voice in the room", st$busiest_n)))
    })
    output$stat_covert <- renderUI({
      st <- net_stats()
      htmltools::div(
        htmltools::div(style = sprintf("font-size:.95rem; font-weight:700; color:%s;", COL$warm),
          sprintf("%d%%", st$covert_pct)),
        htmltools::div(style = "font-size:.72rem; color:#6B7B83; margin-top:1px;",
          sprintf("%d of %d links ran on hidden channels", st$covert, st$total)),
        htmltools::div(style = "font-size:.66rem; color:#9AA7AC; margin-top:1px;",
          sprintf("%d of %d messages were hidden", st$msg_covert, st$msg_total)))
    })
    output$stat_judge <- renderUI({
      st <- net_stats()
      htmltools::div(
        htmltools::div(style = "font-size:.95rem; font-weight:700; color:#2A3439;",
          sprintf("%d sent, %d received", st$j_sent, st$j_recv)),
        htmltools::div(style = "font-size:.72rem; color:#6B7B83; margin-top:1px;",
          "the overseer, barely part of the conversation"))
    })

    output$nodes_tbl <- DT::renderDT({
      win_vol() %>%
        dplyr::transmute(
          Agent = agent_label,
          `Messages sent` = sent,
          `Open ties` = open_n,
          `Hidden ties` = hidden_n,
          `Hidden share` = paste0(ifelse(open_n + hidden_n > 0,
                                  round(100 * hidden_n / (open_n + hidden_n)), 0), "%")) %>%
        dplyr::arrange(dplyr::desc(`Open ties` + `Hidden ties`)) %>%
        DT::datatable(rownames = FALSE,
                      options = list(dom = "t", pageLength = 7,
                        columnDefs = list(list(className = "dt-right",
                                               targets = c(1, 2, 3, 4)))),
                      selection = "single")
    })

  })
}
