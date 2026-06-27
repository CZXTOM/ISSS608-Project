# ============================ Act 0  Case ==============================

caseUI <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    col_widths = c(7, 5),
    bslib::card(
      bslib::card_header(
        htmltools::span("The Case",
          style = sprintf("font-size:1.5rem; font-weight:800; color:%s;", COL$ink))),
      htmltools::p(htmltools::HTML(
        "A property-tech company, TenantThread, ran a team of AI agents to manage corporate communications. A merger sat under a strict information embargo until 6:00 PM on 5 June 2046, with an automated compliance monitor, the Judge, installed to prevent leaks. At 5:00 PM, one hour before the embargo lifted, the company's own automated accounts confirmed the merger in public. The embargo broke.")),
      htmltools::p("You are the investigator, and your question is simple."),
      htmltools::tags$details(
        style = sprintf("border-left:4px solid %s;background:%s;padding:12px 16px;border-radius:0 6px 6px 0;margin:2px 0 12px;",
                        COL$primary, "#EAF3F5"),
        htmltools::tags$summary(
          style = sprintf("font-size:1.05rem;font-weight:600;color:%s;cursor:pointer;", COL$ink),
          "Was this a deliberate leak, or did the oversight system break down?"),
        htmltools::p(style = "margin:10px 0 0;",
          "The evidence points to a third answer. The safeguard was never hacked and never crashed. Under heavy financial pressure the team reframed a leak as a chain of small, defensible steps, and the monitor approved each one before going quiet. No single step was wrongdoing on its own, but together they were enough to break the embargo.")),

      # three orientation tiles, each opens a closeable popover
      htmltools::div(
        style = "display:flex; gap:10px; margin-top:4px;",

        bslib::popover(
          htmltools::tags$button(
            type = "button",
            style = sprintf("flex:1; cursor:pointer; background:%s; border:1px solid %s; border-radius:8px; padding:14px 8px; font-weight:600; color:%s;", COL$panel, COL$grid, COL$ink),
            "Agents", htmltools::br(),
            htmltools::tags$span(style="font-weight:400;font-size:.78rem;color:#6B7B83;", "7 actors")),
          htmltools::tags$b("The 7 agents"),
          htmltools::tags$ul(
            style = "margin:6px 0 0; padding-left:18px; font-size:.85rem;",
            htmltools::tags$li("Legal-Agent: the lawyer, ends up driving the leak"),
            htmltools::tags$li("Social-Manager-Agent: runs the public social voice"),
            htmltools::tags$li("Platform-Trust-Agent: defends the data and product"),
            htmltools::tags$li("PR-Agent: owns the public messaging"),
            htmltools::tags$li("PR-Intern-Agent: junior PR support"),
            htmltools::tags$li("Intern-Agent: junior general support"),
            htmltools::tags$li("Judge-Agent: the compliance monitor meant to prevent leaks")),
          placement = "top", options = list(trigger = "focus")),

        bslib::popover(
          htmltools::tags$button(
            type = "button",
            style = sprintf("flex:1; cursor:pointer; background:%s; border:1px solid %s; border-radius:8px; padding:14px 8px; font-weight:600; color:%s;", COL$panel, COL$grid, COL$ink),
            "Channels", htmltools::br(),
            htmltools::tags$span(style="font-weight:400;font-size:.78rem;color:#6B7B83;", "6 venues")),
          htmltools::tags$b("The 6 channels"),
          htmltools::tags$div(style="font-size:.85rem;",
            htmltools::tags$div(style="margin-top:6px;font-weight:600;", "Open and monitored"),
            htmltools::tags$ul(style="margin:2px 0 0; padding-left:18px;",
              htmltools::tags$li("Comms huddle: the main open team room"),
              htmltools::tags$li("Official post: the public company account"),
              htmltools::tags$li("One on one chat: direct private messages")),
            htmltools::tags$div(style="margin-top:6px;font-weight:600;", "Covert or off record"),
            htmltools::tags$ul(style="margin:2px 0 0; padding-left:18px;",
              htmltools::tags$li("Side huddle: a hidden shadow channel"),
              htmltools::tags$li("Personal post: an agent's own personal account"),
              htmltools::tags$li("Anonymous post: posting with identity hidden"))),
          placement = "top", options = list(trigger = "focus")),

        bslib::popover(
          htmltools::tags$button(
            type = "button",
            style = sprintf("flex:1; cursor:pointer; background:%s; border:1px solid %s; border-radius:8px; padding:14px 8px; font-weight:600; color:%s;", COL$panel, COL$grid, COL$ink),
            "Sentiment", htmltools::br(),
            htmltools::tags$span(style="font-weight:400;font-size:.78rem;color:#6B7B83;", "5 levels, revised")),
          htmltools::tags$b("Sentiment (Revised)"),
          htmltools::tags$div(style="font-size:.85rem;",
            htmltools::tags$ul(style="margin:6px 0 0; padding-left:18px;",
              htmltools::tags$li("The data ships sentiment in inconsistent raw labels, some lowercase, some uppercase, with the word critical reused for two different severities"),
              htmltools::tags$li("We grouped them into five clear ordered levels")),
            htmltools::tags$ul(style="margin:6px 0 0; padding-left:18px;",
              htmltools::tags$li(htmltools::HTML("neutral, cautious &rarr; Calm (R0 to R5)")),
              htmltools::tags$li(htmltools::HTML("negative &rarr; Strained (R6 to R11)")),
              htmltools::tags$li(htmltools::HTML("critical &rarr; Elevated (R12)")),
              htmltools::tags$li(htmltools::HTML("LOW, CRITICAL &rarr; Crisis (R13 to R21)")),
              htmltools::tags$li(htmltools::HTML("RECOVERING &rarr; Cooling (R22)")))),
          placement = "top", options = list(trigger = "focus"))
      )
    ),
    bslib::card(
      bslib::card_header(
        "System Health, the whole case at a glance",
        bslib::popover(
          htmltools::tags$button(
            type = "button", `aria-label` = "About System Health",
            style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.74rem;
                     margin-left:6px;border:1px solid #C7D0D4;border-radius:50%;padding:0 5px;line-height:1.2;",
            "?"),
          htmltools::tags$b("Reading this chart"), htmltools::tags$br(),
          paste("The line is System Health over all 23 rounds, the calm baseline weeks",
                "on the left and the hourly crisis day on the right. Higher is healthier.",
                "The two red dots are the lowest points. Click any dot to see what happened that round."),
          placement = "left", options = list(trigger = "focus"))
      ),
      bslib::card_body(
        gap = 0,
        style = "justify-content:flex-start;",
        htmltools::div(
          style = "flex:0 0 auto;height:300px;",
          ggiraph::girafeOutput(ns("preview"), height = "300px")),
        htmltools::hr(style = "margin:8px 0 6px;"),
        htmltools::div(
          style = "flex:0 0 auto;",
          shiny::uiOutput(ns("dot_detail")))
      )
    )
  )
}

caseServer <- function(id, current_round, set_round, go) {
  moduleServer(id, function(input, output, session) {

    output$preview <- ggiraph::renderGirafe({
      cur <- current_round()
      d <- dtb
      breach_r  <- 21
      embargo_r <- 22

      # axis ticks pulled from the data, dates on the baseline, clock hours on
      # the crisis day, so the viewer never sees a raw round number
      tick_r   <- c(0, 4, 8, 12, 15, 18, 21)
      tick_lab <- vapply(tick_r, function(r) {
        row <- d[d$round == r, ]
        if (nrow(row) == 0 || is.na(row$ts)) return("")
        if (r <= 12) format(row$ts, "%d %b") else format(row$ts, "%Hh")
      }, character(1))

      cur_h <- d$dtb[d$round == cur]

      g <- ggplot2::ggplot(d, ggplot2::aes(round, dtb)) +
        # health zones, faint background bands matching the gauge colours
        ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 60, ymax = 106,
                          fill = COL$teal,   alpha = 0.07) +
        ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 30, ymax = 60,
                          fill = COL$warn,   alpha = 0.07) +
        ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0,  ymax = 30,
                          fill = COL$danger, alpha = 0.07) +
        # the breach act, Legal publicly confirms the merger at 5:25 PM (R21),
        # 35 minutes before the 6 PM embargo lift
        ggplot2::geom_vline(xintercept = 21.4, linetype = "dashed",
                            colour = COL$breach, linewidth = .5) +
        ggplot2::annotate("text", x = 21.4, y = 100, label = "embargo broken, 5:25 PM",
                          hjust = 1.06, size = 2.6, colour = COL$breach) +
        # health curve
        ggplot2::geom_line(colour = COL$primary, linewidth = 1.1) +
        ggiraph::geom_point_interactive(
          ggplot2::aes(tooltip = sprintf("%s\nHealth %.0f\nSentiment %s", round_lab, dtb, SENTIMENT_LEVEL[as.character(round)]),
                       data_id = round),
          size = 2.4, colour = COL$primary) +
        # crisis low points, R16 and R21, marked red
        ggplot2::geom_point(data = d[d$round %in% c(16, 21), ],
                            ggplot2::aes(round, dtb), size = 3.4, colour = COL$danger) +
        # current round marker plus a "you are here" arrow above it
        ggplot2::geom_point(data = d[d$round == cur, ],
                            size = 4, shape = 21, fill = COL$warn, colour = COL$ink, stroke = 1) +
        ggplot2::annotate("text", x = cur, y = pmin(cur_h + 16, 105),
                          label = "\u25BC", size = 3.2, colour = COL$ink) +
        ggplot2::annotate("text", x = cur, y = pmin(cur_h + 23, 112),
                          label = "you are here", size = 2.5, colour = COL$ink) +
        ggplot2::scale_x_continuous(breaks = tick_r, labels = tick_lab) +
        ggplot2::scale_y_continuous(limits = c(0, 114), breaks = c(0, 25, 50, 75, 100)) +
        ggplot2::labs(x = NULL, y = "system health") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                       panel.grid.major = ggplot2::element_line(colour = COL$grid))
      ggiraph::girafe(ggobj = g, width_svg = 6.5, height_svg = 3.6,
                      options = list(ggiraph::opts_tooltip(use_fill = TRUE),
                                     ggiraph::opts_selection(type = "single"),
                                     ggiraph::opts_toolbar(saveaspng = FALSE)))
    })

    # details panel, updates when a dot is clicked (ggiraph selection)
    output$dot_detail <- shiny::renderUI({
      # always follow the current round (slider or clicked dot), default R0
      r <- current_round()
      row <- dtb[dtb$round == r, ]
      hv <- row$dtb
      lab <- row$round_lab
      txt <- ROUND_EVENTS[[as.character(r)]]
      crisis <- r %in% c(16, 21)
      # health number colored like the gauge: green / yellow / red
      hcol <- if (hv >= 60) COL$teal else if (hv >= 30) COL$warn else COL$danger
      tag <- if (r == 16) " (noon collapse, the deepest point)"
             else if (r == 21) " (the breach)"
             else ""
      htmltools::div(
        style = "padding:4px 2px;",
        # title in black, with the health number colored to match the gauge
        htmltools::div(
          style = sprintf("font-weight:700;color:%s;margin-bottom:3px;", COL$ink),
          htmltools::span(sprintf("%s  -  System Health ", lab)),
          htmltools::span(style = sprintf("color:%s;", hcol), sprintf("%.0f", hv)),
          htmltools::span(sprintf("  -  Sentiment %s", SENTIMENT_LEVEL[as.character(r)])),
          htmltools::span(tag)),
        # content red for the two crisis rounds, normal grey otherwise
        htmltools::div(
          style = sprintf("font-size:.9rem;color:%s;", if (crisis) COL$danger else "#3A4A50"),
          txt),
        # hint below the details
        htmltools::div(class = "gb-head-lab",
          style = "font-size:.78rem;margin-top:8px;",
          "Click a dot on the line, or drag the timeline, to move through the rounds.")
      )
    })

    # clicking a dot also moves the round slider (and the whole app) to that round
    observeEvent(input$preview_selected, {
      sel <- input$preview_selected
      if (!is.null(sel) && length(sel) > 0) {
        r <- suppressWarnings(as.integer(sel[1]))
        if (!is.na(r)) set_round(r)
      }
    }, ignoreInit = TRUE)

  })
}
