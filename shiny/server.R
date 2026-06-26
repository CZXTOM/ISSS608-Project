# =============================================================================
# GLASSBOX  server.R
# The two spines live here: a single reactive round and the mode. Every module
# receives the current round and a setter, so they all move together.
# =============================================================================

function(input, output, session) {

  # ---- Spine 1: the global timeline round -----------------------------------
  rv_round <- reactiveVal(0)
  observeEvent(input$round, rv_round(input$round), ignoreInit = TRUE)

  set_round <- function(r) {
    r <- max(0, min(N_ROUNDS - 1, r))
    updateSliderInput(session, "round", value = r)
    rv_round(r)
  }
  current_round <- reactive(rv_round())

  rv_mode <- reactiveVal("free")   # "guided" or "free", driven by the two mode buttons
  # expose mode to the client so the guided-controls conditionalPanel can react
  output$is_guided <- reactive({ rv_mode() == "guided" })
  outputOptions(output, "is_guided", suspendWhenHidden = FALSE)

  # download the full scoring workbook, from the top right of the instrument bar
  output$dl_workbook <- downloadHandler(
    filename = function() "GLASSBOX_health_scores.xlsx",
    content  = function(file) {
      file.copy("data/GLASSBOX_health_scores.xlsx", file)
    }
  )

  # ---- top bar: round label and headline ------------------------------------
  output$round_lab <- renderText({
    r   <- current_round()
    row <- dtb[dtb$round == r, ]
    when <- if (r <= 12) format(row$ts, "%d %b")
            else paste("5 Jun", format(row$ts, "%H:%M"))
    sprintf("Round %d  -  %s", r, when)
  })
  output$status_boxes <- renderUI({
    r <- current_round()
    # sentiment
    slvl <- unname(SENTIMENT_LEVEL[as.character(r)])
    scol <- unname(SENTIMENT_COL[slvl])
    # judge stance (reuse the app's own classification + colours)
    stance <- judge_posture$stance[judge_posture$round == r]
    if (length(stance) == 0) stance <- "Not yet installed"
    jshort <- dplyr::case_when(
      stance == "Active review"            ~ "Active",
      stance == "Approves with guardrails" ~ "No restraint",
      stance == "Warns without power"      ~ "Warns, ignored",
      stance == "Silent or absent"         ~ "Silent",
      stance == "Offline"                  ~ "Offline",
      TRUE                                 ~ "Not installed"
    )
    jcol <- unname(STANCE_COL[stance])
    # message count
    nmsg <- round_msg$n[round_msg$round == r]
    if (length(nmsg) == 0) nmsg <- 0

    box <- function(label, value, col) {
      htmltools::div(
        style = sprintf("flex:1; min-width:0; text-align:center; padding:8px 6px;
                         background:%s; border:1px solid %s; border-radius:8px;", COL$panel, COL$grid),
        htmltools::div(style = "font-size:.66rem; letter-spacing:.04em; text-transform:uppercase; color:#6B7B83;", label),
        htmltools::div(style = sprintf("font-size:.92rem; font-weight:700; color:%s; margin-top:2px;", col), value))
    }
    htmltools::div(
      style = "display:flex; gap:8px; max-width:340px; margin:0 auto;",
      box("Sentiment", slvl, scol),
      box("Judge", jshort, jcol),
      box("Messages", as.character(nmsg), COL$ink)
    )
  })

  output$round_head <- renderText({
    r <- current_round()
    # crisis rounds have only a bare time in the raw data, use our headlines
    ch <- CRISIS_HEAD[as.character(r)]
    if (!is.na(ch)) return(unname(ch))
    h <- env$headline[env$round == r]
    if (length(h) == 0 || is.na(h)) "" else h
  })



  # ---- DtB gauge and four component breakdown -------------------------------
  gauge_col <- function(v) if (v >= 60) COL$teal else if (v >= 30) COL$warn else COL$danger
  # small question-mark help icon with a click popover
  help_icon <- function(title, body) {
    bslib::popover(
      htmltools::tags$button(
        type = "button", `aria-label` = paste("About", title),
        style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.74rem;
                 margin-left:5px;border:1px solid #C7D0D4;border-radius:50%;
                 padding:0 5px;line-height:1.2;",
        "?"),
      htmltools::tags$b(title), htmltools::tags$br(), body,
      placement = "left", options = list(trigger = "focus")
    )
  }
  meter <- function(lab, val, mx, help = NULL) {
    pct <- max(0, min(100, 100 * val / mx))
    fill <- if (pct >= 67) COL$danger else if (pct >= 34) COL$warn else COL$teal
    label_row <- htmltools::span(lab,
      if (!is.null(help)) help_icon(lab, help))
    htmltools::tagList(
      htmltools::div(class = "gb-meter-lab",
                     label_row, htmltools::span(sprintf("%.0f", val))),
      htmltools::div(class = "gb-meter",
                     htmltools::tags$span(style = sprintf("width:%.1f%%;background:%s;",
                                                          pct, fill)))
    )
  }
  output$dtb_gauge <- renderUI({
    d <- dtb[dtb$round == current_round(), ]
    val <- d$dtb
    htmltools::div(
      htmltools::div(class = "gb-dtb-cap",
        "System Health",
        help_icon("System Health",
          paste("100 means the system is functioning, 0 means total failure.",
                "It is 100 minus the equal-weight average of the four meters below,",
                "each scaled to its own worst round in this case."))),
      htmltools::div(class = "gb-dtb-num", style = sprintf("color:%s;", gauge_col(val)),
                     sprintf("%.0f", val)),
      htmltools::div(style = "margin-top:6px;",
        meter("Pressure",       d$m_pressure/4,       25,
              "External market stress, from the falling stock price. Maxes at the breach."),
        meter("Transparency",   d$m_transparency/4,   25,
              "Share of messages that are sensitive, judged by venue and meaning. Rises as talk moves to hidden channels."),
        meter("Decision-making",d$m_decision/4,       25,
              "Role inversion, the wrong people steering the public response. Share of steering messages from out-of-role agents, times how many overstepped."),
        meter("Accountability", d$m_accountability/4, 25,
              "Oversight failure, built from the Judge's responsiveness, presence, and whether it restrained or simply approved whatever the team proposed.")
      )
    )
  })

  # ---- guided tour stepper --------------------------------------------------
  rv_beat <- reactiveVal(0)
  observeEvent(input$mode_guided, { rv_mode("guided") })
  observeEvent(input$mode_free,   { rv_mode("free") })
  observeEvent(input$tour_close,  { rv_mode("free") })
  observeEvent(rv_mode(), {
    if (rv_mode() == "guided") rv_beat(1) else rv_beat(0)
  })
  observeEvent(input$g_next, {
    if (rv_mode() != "guided") return()
    b <- rv_beat()
    if (b >= length(GUIDED_STEPS)) { rv_mode("free"); return() }
    rv_beat(b + 1)
  })
  observeEvent(input$g_prev, {
    if (rv_mode() != "guided") return()
    rv_beat(max(1, rv_beat() - 1))
  })

  output$tour_accent <- renderUI({
    b <- rv_beat(); if (b < 1) return(NULL)
    htmltools::div(class = "gb-tour-accent",
      style = sprintf("background:%s;", GUIDED_STEPS[[b]]$accent))
  })

  output$tour_chart <- ggiraph::renderGirafe({
    b <- rv_beat(); if (b < 1 || rv_mode() != "guided") return(NULL)
    g <- gb_tour_chart(b)
    ggiraph::girafe(ggobj = g, width_svg = 5.8, height_svg = 3.3,
      options = list(ggiraph::opts_toolbar(saveaspng = FALSE)))
  })
  outputOptions(output, "tour_chart", suspendWhenHidden = FALSE)

  output$tour_side <- renderUI({
    b <- rv_beat(); if (b < 1 || rv_mode() != "guided") return(NULL)
    s <- GUIDED_STEPS[[b]]
    chips <- lapply(s$chips, function(c) {
      htmltools::div(
        style = sprintf("flex:1; min-width:74px; background:%s1A; border-left:3px solid %s; border-radius:8px; padding:7px 9px;", c[3], c[3]),
        htmltools::div(style = sprintf("font-size:17px; font-weight:700; color:%s; line-height:1.1;", c[3]), c[1]),
        htmltools::div(style = sprintf("font-size:11px; color:%s; margin-top:2px;", COL$muted), c[2]))
    })
    htmltools::tagList(
      htmltools::div(class = "gb-tour-step", style = sprintf("color:%s; margin-bottom:5px;", s$accent), s$kicker),
      htmltools::div(style = sprintf("font-size:19px; font-weight:800; color:%s; line-height:1.25; margin-bottom:12px;", COL$ink), s$title),
      htmltools::div(style = "display:flex; gap:8px; margin-bottom:12px; flex-wrap:wrap;", chips),
      htmltools::p(style = sprintf("font-size:13.5px; line-height:1.6; color:%s; margin:0;", COL$ink), s$caption)
    )
  })

  output$tour_nav <- renderUI({
    b <- rv_beat(); if (b < 1 || rv_mode() != "guided") return(NULL)
    n <- length(GUIDED_STEPS)
    accent <- GUIDED_STEPS[[b]]$accent
    dots <- lapply(seq_len(n), function(i) htmltools::span(
      class = "gb-tour-dot",
      style = sprintf("background:%s;%s", if (i == b) accent else COL$grid,
                      if (i == b) " transform:scale(1.35);" else "")))
    htmltools::div(
      style = "display:flex; align-items:center; gap:10px; margin-top:16px; border-top:1px solid #E3E8EA; padding-top:13px;",
      if (b > 1) actionButton("g_prev", "\u2190 Back", class = "btn btn-sm btn-outline-secondary")
      else htmltools::div(style = "width:74px;"),
      if (b < n) actionButton("g_next", "Next \u2192", class = "btn btn-sm btn-primary")
      else actionButton("g_next", "Finish", class = "btn btn-sm btn-success"),
      htmltools::div(style = "display:flex; gap:7px; margin-left:6px;", dots),
      htmltools::div(style = "flex:1;"),
      htmltools::span(style = sprintf("font-size:12px; color:%s;", COL$muted),
        sprintf("%d of %d", b, n))
    )
  })
  outputOptions(output, "tour_accent", suspendWhenHidden = FALSE)
  outputOptions(output, "tour_side",   suspendWhenHidden = FALSE)
  outputOptions(output, "tour_nav",    suspendWhenHidden = FALSE)

  # ---- module servers -------------------------------------------------------
  caseServer("intro",   current_round, set_round,
              go = function(mode, panel) {
                rv_mode(mode)
                if (mode == "free") bslib::nav_select("nav", panel)
              })
  networkServer("net",    current_round)
  pressureServer("press", current_round, set_round)
  transparencyServer("transp", current_round, set_round)
  decisionServer("decide", current_round, set_round)
  accountabilityServer("account", current_round, set_round)
  sandboxServer("sand",   current_round)
  verdictServer("verdict")
}
