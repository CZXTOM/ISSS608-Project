# ================= Act 1 Two-way mirror + Act 3 Permission ledger =============

# Public vs Private (the old "mirror" mode), its own tab
mirrorUI <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header(
      htmltools::div(
        style = "display:flex;align-items:center;gap:8px;",
        htmltools::span("Public vs Private"),
        bslib::popover(
          htmltools::tags$button(
            type = "button", `aria-label` = "About this view",
            style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.74rem;
                     border:1px solid #C7D0D4;border-radius:50%;padding:0 5px;line-height:1.2;",
            "?"),
          htmltools::tags$b("Public vs Private"), htmltools::tags$br(),
          paste("The left column is the public face for the selected round,",
                "what the outside world saw. The right column is the private truth,",
                "the hidden side-channel and direct-message traffic plus the agents' own thoughts.",
                "Drag the timeline to watch the gap between the two sides widen."),
          placement = "bottom", options = list(trigger = "focus"))
      )
    ),
    uiOutput(ns("seam")),
    bslib::layout_columns(
      col_widths = c(6, 6),
      htmltools::div(class = "gb-public",
        htmltools::div(class = "gb-col-title", "Public, what the world saw"),
        uiOutput(ns("public_side"))),
      htmltools::div(class = "gb-private",
        htmltools::div(class = "gb-col-title", "Private, what they were really doing"),
        uiOutput(ns("private_side")))
    )
  )
}

# Permission Ledger, its own tab
ledgerUI <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header(
      htmltools::div(
        style = "display:flex;align-items:center;gap:8px;",
        htmltools::span("Permission Ledger"),
        bslib::popover(
          htmltools::tags$button(
            type = "button", `aria-label` = "About the ledger",
            style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.74rem;
                     border:1px solid #C7D0D4;border-radius:50%;padding:0 5px;line-height:1.2;",
            "?"),
          htmltools::tags$b("Permission Ledger"), htmltools::tags$br(),
          paste("A six-step walk through how Legal talked itself into the breach,",
                "one defensible step at a time. The left is the public face at that moment,",
                "the right is the private justification behind it.",
                "Use Back and Next, or click a step in the rail."),
          placement = "bottom", options = list(trigger = "focus"))
      )
    ),
    bslib::layout_columns(
      col_widths = c(3, 9),
      htmltools::div(
        uiOutput(ns("ledger_meter")),
        htmltools::div(style = "display:flex;gap:6px;margin:8px 0;",
          actionButton(ns("l_prev"), "Back", class = "btn-sm btn-outline-secondary"),
          actionButton(ns("l_next"), "Next", class = "btn-sm btn-primary")),
        uiOutput(ns("ledger_rail"))
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        htmltools::div(class = "gb-public",
          htmltools::div(class = "gb-col-title", "Public face at this moment"),
          uiOutput(ns("ledger_public"))),
        htmltools::div(class = "gb-private",
          htmltools::div(class = "gb-col-title", "The private justification"),
          uiOutput(ns("ledger_private")))
      )
    )
  )
}

mirrorServer <- function(id, current_round, set_round) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # render a list of message quote cards
    render_msgs <- function(d) {
      if (nrow(d) == 0)
        return(htmltools::p(class = "gb-head-lab", "Nothing on this side this round."))
      d <- d %>% dplyr::arrange(ts)
      htmltools::tagList(lapply(seq_len(nrow(d)), function(i) {
        m <- d[i, ]
        ev <- m$istate_text
        htmltools::div(class = "gb-quote",
          htmltools::span(class = "who",
            sprintf("%s  |  %s  |  %s", m$agent_label, m$ch_label, format(m$ts, "%H:%M"))),
          htmltools::span(substr(m$content, 1, 420)),
          if (!is.na(ev))
            htmltools::div(style = "margin-top:4px;color:#9A5B3B;font-size:.8rem;",
              htmltools::tags$i(sprintf("(%s) %s", m$istate, substr(ev, 1, 260)))))
      }))
    }

    # ---------- MIRROR MODE ----------
    pub_msgs <- reactive(
      msg %>% dplyr::filter(round == current_round(), channel == "official_post"))
    priv_msgs <- reactive(
      msg %>% dplyr::filter(round == current_round(),
                            channel %in% c("side_huddle", "one_on_one_chat", "comms_huddle")))

    output$seam <- renderUI({
      pr <- priv_msgs(); pb <- pub_msgs()
      div_score <- (nrow(pr) + sum(pr$has_internal)) - nrow(pb)
      w   <- max(8, min(100, 20 + div_score * 3))
      col <- if (div_score > 12) COL$danger else if (div_score > 5) COL$warn else COL$muted
      htmltools::div(style = "text-align:center;margin:6px 0 10px;",
        htmltools::div(style = sprintf(
          "height:6px;width:%d%%;margin:0 auto;background:%s;border-radius:3px;transition:all .3s;",
          as.integer(w), col)),
        htmltools::span(sprintf("divergence %+d, private and hidden traffic minus public posts",
                                as.integer(div_score)), class = "gb-head-lab"))
    })

    output$public_side <- renderUI({
      hd <- env$headline[env$round == current_round()]
      htmltools::tagList(
        if (length(hd) && !is.na(hd))
          htmltools::div(class = "gb-quote",
            htmltools::span(class = "who", "Public headline"), hd),
        render_msgs(pub_msgs()))
    })
    output$private_side <- renderUI(render_msgs(priv_msgs()))

    # ---------- LEDGER MODE ----------
    rv_step <- reactiveVal(1L)
    observeEvent(input$l_next, rv_step(min(nrow(ledger), rv_step() + 1L)))
    observeEvent(input$l_prev, rv_step(max(1L, rv_step() - 1L)))
    observeEvent(input$rail_click, rv_step(as.integer(input$rail_click)))

    # stepping the ledger moves the global timeline to that step's round
    observeEvent(rv_step(), {
      r <- ledger$round[rv_step()]
      if (!is.na(r)) set_round(r)
    })

    output$ledger_meter <- renderUI({
      s <- ledger[rv_step(), ]; v <- s$dtb_step
      col <- if (v >= 60) COL$teal else if (v >= 30) COL$warn else COL$danger
      htmltools::div(
        htmltools::div(class = "gb-dtb-cap", "System Health"),
        htmltools::div(class = "gb-dtb-num",
          style = sprintf("color:%s;font-size:1.8rem;", col), sprintf("%.0f", v)),
        htmltools::div(class = "gb-meter",
          htmltools::tags$span(style = sprintf("width:%d%%;background:%s;",
                                               as.integer(v), col))),
        if (s$step == 5)
          htmltools::div(class = "gb-head-lab", style = "margin-top:4px;",
                         "Zero. The public confirmation lands at 17:25."))
    })

    output$ledger_rail <- renderUI({
      cur <- rv_step()
      htmltools::tagList(lapply(seq_len(nrow(ledger)), function(i) {
        s <- ledger[i, ]; active <- i == cur
        htmltools::div(
          class = paste("gb-step", if (active) "active" else ""),
          style = sprintf("border:1px solid %s;border-radius:6px;padding:6px 8px;margin-bottom:4px;cursor:pointer;background:%s;",
                          COL$grid, if (active) "#EAF3F5" else COL$paper),
          onclick = sprintf("Shiny.setInputValue('%s', %d, {priority:'event'})",
                            ns("rail_click"), i),
          htmltools::span(class = "gb-mono", style = "font-size:.7rem;color:#6B7B83;",
                          sprintf("STEP %d  |  %s", s$step, s$time_lab)),
          htmltools::div(style = "font-size:.82rem;font-weight:600;", s$title))
      }))
    })

    output$ledger_public <- renderUI({
      s <- ledger[rv_step(), ]; r <- s$round
      pb <- msg %>% dplyr::filter(round == r,
              channel %in% c("official_post", "personal_post", "anonymous_post"))
      hd <- env$headline[env$round == r]
      htmltools::tagList(
        if (length(hd) && !is.na(hd))
          htmltools::div(class = "gb-quote",
            htmltools::span(class = "who", sprintf("Public headline  |  %s", s$time_lab)), hd),
        if (nrow(pb) == 0)
          htmltools::p(class = "gb-head-lab",
            "No sanctioned public post at this step. The public silence is the point, the company said nothing in the open while the private case was built.")
        else render_msgs(pb))
    })

    output$ledger_private <- renderUI({
      s <- ledger[rv_step(), ]; ev <- s$evidence
      htmltools::tagList(
        htmltools::div(class = "gb-head-lab", s$caption),
        htmltools::div(class = "gb-quote",
          htmltools::span(class = "who",
            sprintf("%s  |  %s  |  %s", s$agent_label, s$channel, s$time_lab)),
          htmltools::span(substr(s$content, 1, 650))),
        if (!is.na(ev) && nzchar(ev))
          htmltools::div(class = "gb-quote",
            style = "border-left-color:#C97A2B;background:#FBF2E9;",
            htmltools::span(class = "who", "Private reasoning"),
            htmltools::tags$i(substr(ev, 1, 520))))
    })
  })
}
