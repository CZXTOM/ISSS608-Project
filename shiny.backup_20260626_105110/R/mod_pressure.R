# ================= Vital Signs : Pressure =====================================
# Status boxes (price, sentiment, pressure) with honest per-round descriptions
# and a red data-quality note, then two clean charts in the Case-page style
# (plain line, yellow "you are here" dot, click to jump). Method in the fold.
# Source of truth: GLASSBOX_MC1_stock.xlsx, Pressure tab.

# which rounds carry bad raw feed values that we corrected
PRESS_BAD <- c("15" = 180, "18" = 18, "19" = 18)

pressureUI <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header(
      htmltools::div(
        style = "display:flex;align-items:center;gap:8px;",
        htmltools::span("Pressure, built from the stock and the sentiment"),
        bslib::popover(
          htmltools::tags$button(
            type = "button", `aria-label` = "About the pressure meter",
            style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.74rem;
                     border:1px solid #C7D0D4;border-radius:50%;padding:0 5px;line-height:1.2;",
            "?"),
          htmltools::tags$b("Pressure"), htmltools::tags$br(),
          "Market stress each round, based on how far the share price has fallen. Higher means more strain on the company.",
          placement = "bottom", options = list(trigger = "focus"))
      )
    ),
    bslib::card_body(
      gap = 0,
      uiOutput(ns("boxes")),
      htmltools::tags$div(style = "height:8px;"),
      # always-visible data-quality notes for the stock feed
      htmltools::div(
        style = "border:1px solid #F0D5D9; background:#FCF3F4; border-radius:8px; padding:8px 12px; margin-bottom:10px;",
        htmltools::div(style = "font-size:.7rem; letter-spacing:.04em; text-transform:uppercase; color:#9AA7AC; margin-bottom:3px;",
          "Notes on the stock data"),
        htmltools::tags$ul(
          style = "margin:0; padding-left:18px; font-size:.78rem; line-height:1.5;",
          htmltools::tags$li(htmltools::span(style = sprintf("color:%s; font-weight:600;", COL$danger),
            "Round 15, the data cell showed 180. That was the company valuation, not the share price, and its percent did not match the message. We used the real price of 27.80 stated in the round 15 message instead.")),
          htmltools::tags$li(htmltools::span(style = sprintf("color:%s; font-weight:600;", COL$danger),
            "Rounds 18 and 19, the data cell showed 18. That was annual recurring revenue, not the share price. We used the real prices of 26.40 and 25.80 stated in those messages.")),
          htmltools::tags$li(htmltools::span(style = "color:#6B7B83;",
            "Baseline prices are given in the case. Crisis prices are read from the message text where stated. Round 14 is interpolated between its real neighbours. Rounds 20 and 21 are estimated, since the price did not recover until after the 6 PM close. The estimate is shaped by a set of real cause matched crash and rescue days pulled from Yahoo Finance, the breach and privacy cases CPNG, FFIV and AT&T, the trust case CRWD, the antitrust case LYV, the operational case UNH, and the rescue and merger case CWAN."))
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        htmltools::div(
          htmltools::div(style = "display:flex; align-items:center; justify-content:center; gap:6px;",
            htmltools::tags$div(class = "gb-col-title", "Stock price"),
            bslib::popover(
              htmltools::tags$button(
                type = "button", `aria-label` = "About stock chart",
                style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.68rem;
                         border:1px solid #C7D0D4;border-radius:50%;padding:0 5px;line-height:1.2;",
                "?"),
              htmltools::tags$b("Stock price"), htmltools::tags$br(),
              "Share price each round. Real values from the case data, estimated on crisis rounds by matching real crash-day patterns from similar company scandals on Yahoo Finance. Click a dot to jump there.",
              placement = "bottom", options = list(trigger = "focus"))),
          ggiraph::girafeOutput(ns("stock"), height = "240px")),
        htmltools::div(
          htmltools::div(style = "display:flex; align-items:center; justify-content:center; gap:6px;",
            htmltools::tags$div(class = "gb-col-title", "Pressure score"),
            bslib::popover(
              htmltools::tags$button(
                type = "button", `aria-label` = "About pressure score chart",
                style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.68rem;
                         border:1px solid #C7D0D4;border-radius:50%;padding:0 5px;line-height:1.2;",
                "?"),
              htmltools::tags$b("Pressure score"), htmltools::tags$br(),
              "The pressure score each round, combining how far the price fell with how stressed the market mood was. Higher means more strain. Click a dot to jump there.",
              placement = "bottom", options = list(trigger = "focus"))),
          ggiraph::girafeOutput(ns("pressure"), height = "240px"))
      ),
      htmltools::tags$details(
        style = "margin-top:10px;",
        htmltools::tags$summary(
          style = "cursor:pointer;font-weight:600;color:#3A4A50;",
          "How we measured pressure"),
        htmltools::div(
          class = "gb-head-lab",
          style = "margin-top:8px;line-height:1.55;",

          htmltools::tags$div(style = "font-weight:600;color:#3A4A50;margin-bottom:3px;", "The formula"),
          htmltools::p(
            "For every round we compute a price level, then floor it by sentiment, then normalise."),
          htmltools::tags$ol(
            style = "margin:0 0 8px 18px;",
            htmltools::tags$li(htmltools::HTML(
              "Price level = (38.70 minus price) divided by 13.10. The 38.70 is the round 0 starting price and 13.10 is the span down to the crisis trough, so the level runs from 0 at the start to about 1 at the bottom.")),
            htmltools::tags$li(htmltools::HTML(
              "Pressure = the higher of the price level and the sentiment floor for that round. The floors are Calm 0, Strained 0.25, Elevated 0.45, Crisis 0.80, Cooling 0.30. This is why pressure jumps to 80 at round 13, the price alone gives about 0.76 but the crisis floor lifts it to 0.80.")),
            htmltools::tags$li(
              "Normalise to 0 to 100 for display, and to 0 to 25 for the System Health meter.")),

          htmltools::tags$div(style = "font-weight:600;color:#3A4A50;margin:8px 0 3px;", "Where the prices come from"),
          htmltools::tags$ul(
            style = "margin:0 0 8px 18px;",
            htmltools::tags$li("Baseline rounds 0 to 12, real prices given in the case data."),
            htmltools::tags$li("Crisis rounds, read from the round message text where a price is stated, rounds 13, 15, 16, 17, 18, 19 and the 22 after-hours mid."),
            htmltools::tags$li("Round 14, interpolated between its two real neighbours, 28.70 and 27.80, giving 28.25."),
            htmltools::tags$li("Rounds 20 and 21, estimated, because the regular session never recovered, the first upward move was at 6:10 PM after the 5 PM breach.")),

          htmltools::tags$div(style = "font-weight:600;color:#3A4A50;margin:8px 0 3px;", "How the estimates were shaped"),
          htmltools::p(htmltools::HTML(
            "To make the estimated crisis prices behave like a real crash rather than a guess, we pulled real market data from Yahoo Finance for seven companies whose event days match this scenario by cause. We retrieved hourly bars, 60 minute open high low close, using a Python script, for the breach and privacy cases CPNG, FFIV and AT&T, the trust and regulator case CRWD, and the rescue and merger case CWAN, plus daily bars for the antitrust case LYV and the operational case UNH. From each real day we measured the intraday shape, where the trough fell during the session and how far the close recovered from the low, then used that shape to size the estimated MC1 crisis hours.")),

          htmltools::tags$div(style = sprintf("font-weight:600;color:%s;margin:8px 0 3px;", COL$danger), "Corrupted cells we discarded"),
          htmltools::tags$ul(
            style = "margin:0 0 8px 18px;",
            htmltools::tags$li(htmltools::span(style = sprintf("color:%s;", COL$danger),
              "Round 15, the cell read 180. That is the company valuation, not the share price, and its percent did not match the message, so we used the stated price of 27.80.")),
            htmltools::tags$li(htmltools::span(style = sprintf("color:%s;", COL$danger),
              "Rounds 18 and 19, the cell read 18. That is annual recurring revenue, not the share price, so we used the stated prices of 26.40 and 25.80."))),

          htmltools::p(style = "margin-top:6px;color:#9AA7AC;",
            "Sources, the Pressure and Raw_Data sheets of the project workbook.")
        )
      )
    )
  )
}

pressureServer <- function(id, current_round, set_round) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- status boxes with honest descriptions and per-title help ----
    help_btn <- function(title, body) {
      bslib::popover(
        htmltools::tags$button(
          type = "button", `aria-label` = paste("About", title),
          style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.68rem;
                   border:1px solid #C7D0D4;border-radius:50%;padding:0 5px;line-height:1.2;",
          "?"),
        htmltools::tags$b(title), htmltools::tags$br(), body,
        placement = "bottom", options = list(trigger = "focus"))
    }

    output$boxes <- renderUI({
      r <- current_round()
      row <- PRESSURE_BUILD[PRESSURE_BUILD$round == r, ]
      price <- row$price; src <- row$src
      pv <- dtb$m_pressure[dtb$round == r]              # 0-100 normalized
      is_peak_p <- (r == 21)  # R21 verified peak, stock at trough, breach hour
      pcol <- if (is_peak_p) COL$breach else if (pv >= 67) COL$danger else if (pv >= 34) COL$warn else COL$teal

      price_note <- switch(src,
        "real"          = "real value from the case data",
        "message"       = "read from the round message text",
        "interpolated"  = "interpolated between real neighbours",
        "estimated"     = "estimated, shaped by real crash days",
        "real (cell)"   = "real value from the case data",
        src)
      driver <- if (r >= 13 && r <= 21) "held up by the crisis sentiment, not the price"
                else if (r == 22) "easing as the sentiment recovers"
                else "driven by the falling price"
      bad <- as.character(r) %in% names(PRESS_BAD)

      box <- function(label, help, value_ui, desc, extra = NULL) {
        htmltools::div(
          style = sprintf("flex:1; min-width:0; padding:8px 10px; background:%s;
                           border:1px solid %s; border-radius:8px;", COL$panel, COL$grid),
          htmltools::div(style = "display:flex; align-items:center; gap:6px;",
            htmltools::div(style = "font-size:.66rem; letter-spacing:.04em; text-transform:uppercase; color:#6B7B83;", label),
            help),
          value_ui,
          if (!is.null(desc)) htmltools::div(style = "font-size:.7rem; color:#6B7B83; margin-top:3px; line-height:1.35;", desc),
          if (!is.null(extra)) extra)
      }

      htmltools::tagList(
        htmltools::div(
          style = "display:flex; gap:8px;",
          box("Stock price",
            help_btn("Stock price",
              paste("The share price each round. Baseline prices are given in the case.",
                    "Crisis day prices are read from the round message text where stated.",
                    "Where a price is missing it is interpolated between real neighbours,",
                    "or estimated by matching the price pattern of real companies that faced similar crises,",
                    "crash and rescue days pulled from Yahoo Finance, CPNG, FFIV and AT&T for",
                    "privacy and breach, CRWD for trust, LYV for antitrust, UNH for operational,",
                    "and CWAN for the rescue and merger recovery.",
                    "Two corrupted data cells, a 180 valuation and an 18 revenue figure,",
                    "were discarded in favour of the real prices stated in the messages.")),
            htmltools::div(style = sprintf("font-size:1.05rem; font-weight:700; color:%s; margin-top:1px;", COL$ink),
              sprintf("$%.2f", price)),
            htmltools::tagList(
              price_note,
              if (bad) htmltools::div(style = sprintf("color:%s; font-weight:600; margin-top:2px;", COL$danger),
                "this round had a corrupted data cell, corrected"))),

          box("Outside the room",
            help_btn("Outside the room",
              paste("The public and press context this round, drawn from the case social state and news.",
                    "It is shown as plain context, not a score.",
                    "The outside pressure already feeds the stock price, which is what the pressure meter measures,",
                    "so this box explains the why behind the price rather than adding a separate number.")),
            htmltools::div(style = sprintf("font-size:.82rem; color:%s; margin-top:2px; line-height:1.3;", COL$ink),
              EXT_CONTEXT[as.character(r)]),
            NULL),

          box("Pressure",
            help_btn("Pressure",
              paste("How we turn the two inputs into one score.",
                    "First, level = (38.70 minus price) divided by 13.10,",
                    "so a full fall from the 38.70 start reads as maximum.",
                    "Second, take the higher of that level and the sentiment floor.",
                    "Third, normalise to a 0 to 100 percentage.",
                    "The big number here is on the 0 to 25 meter scale, the same number the System Health gauge shows,",
                    "and the small number is that value as a 0 to 100 percentage.")),
            htmltools::div(style = "display:flex; align-items:baseline; gap:6px; margin-top:1px;",
              htmltools::span(style = sprintf("font-size:1.05rem; font-weight:700; color:%s;", pcol),
                sprintf("%.1f", pv/100*25)),
              htmltools::span(style = "font-size:.72rem; color:#9AA7AC;",
                sprintf("of 25  (%.0f%%)", pv))),
            if (is_peak_p) "worst round, stock at trough" else driver,
            extra = if (is_peak_p) htmltools::div(
              style = sprintf("font-size:.7rem; color:%s; margin-top:3px; font-weight:600;", COL$breach),
              "Worst round. The 5 PM breach hour. Stock estimated at $25.60, its lowest point, as the merger goes public before the 6 PM embargo lifts.") else NULL)
        )
      )
    })

    # ---- stock price chart, Case-page style ----
    output$stock <- ggiraph::renderGirafe({
      cur <- current_round()
      d <- PRESSURE_BUILD
      cur_p <- d$price[d$round == cur]
      lo_r <- d$round[which.min(d$price)]   # lowest price round
      g <- ggplot2::ggplot(d, ggplot2::aes(round, price)) +
        # the breach, 5:25 PM, just past R21
        ggplot2::geom_vline(xintercept = 21.4, linetype = "dashed",
                            colour = COL$breach, linewidth = 0.5) +
        ggplot2::annotate("text", x = 21.4, y = 41, label = "embargo broken, 5:25 PM",
                          hjust = 1.05, size = 2.4, colour = COL$breach) +
        ggplot2::geom_line(colour = COL$primary, linewidth = 1.0) +
        ggiraph::geom_point_interactive(
          ggplot2::aes(data_id = round,
                       tooltip = sprintf("Round %d\n$%.2f\n%s", round, price, src)),
          size = 2.2, colour = COL$primary) +
        # red dot at the lowest price, the trough
        ggplot2::geom_point(data = d[d$round == lo_r, ],
                            size = 3.4, colour = COL$danger) +
        ggplot2::geom_point(data = d[d$round == cur, ],
                            size = 4, shape = 21, fill = COL$warn, colour = COL$ink, stroke = 1) +
        ggplot2::annotate("text", x = cur, y = cur_p + 2.4, label = "\u25BC", size = 3, colour = COL$ink) +
        ggplot2::annotate("text", x = cur, y = cur_p + 4.0, label = "you are here", size = 2.4, colour = COL$ink) +
        ggplot2::scale_x_continuous(breaks = seq(0, 22, 2)) +
        ggplot2::scale_y_continuous(limits = c(22, 42)) +
        ggplot2::labs(x = NULL, y = "price, USD") +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                       panel.grid.major = ggplot2::element_line(colour = COL$grid),
                       axis.title.y = ggplot2::element_text(colour = "#6B7B83", size = 9))
      ggiraph::girafe(ggobj = g, width_svg = 4.6, height_svg = 2.8,
        options = list(ggiraph::opts_selection(type = "single"),
                       ggiraph::opts_hover(css = "stroke:#1C2B33;stroke-width:1.5px;"),
                       ggiraph::opts_tooltip(css = paste0(
                         "padding:6px 10px; font-size:12px; color:#2A3439; ",
                         "background:#FFFFFF; border:1px solid #C7D0D4; border-radius:6px; ",
                         "box-shadow:0 2px 8px rgba(0,0,0,0.12);")),
                       ggiraph::opts_toolbar(saveaspng = FALSE)))
    })
    observeEvent(input$stock_selected, {
      sel <- suppressWarnings(as.integer(input$stock_selected))
      if (!is.na(sel) && length(sel)) set_round(sel)
    })

    # ---- pressure chart, Case-page style ----
    output$pressure <- ggiraph::renderGirafe({
      cur <- current_round()
      d <- dtb
      cur_p <- d$m_pressure[d$round == cur]
      g <- ggplot2::ggplot(d, ggplot2::aes(round, m_pressure)) +
        # danger zones, faint bands (low pressure good = teal, mid = amber, high = red)
        ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0,  ymax = 33,
                          fill = COL$teal,   alpha = 0.07) +
        ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 33, ymax = 67,
                          fill = COL$warn,   alpha = 0.07) +
        ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 67, ymax = 106,
                          fill = COL$danger, alpha = 0.07) +
        # the breach, 5:25 PM, just past R21 so it does not sit on the point
        ggplot2::geom_vline(xintercept = 21.4, linetype = "dashed",
                            colour = COL$breach, linewidth = 0.5) +
        ggplot2::annotate("text", x = 21.4, y = 108, label = "embargo broken, 5:25 PM",
                          hjust = 1.05, size = 2.4, colour = COL$breach) +
        ggplot2::geom_line(colour = COL$primary, linewidth = 1.0) +
        ggiraph::geom_point_interactive(
          ggplot2::aes(data_id = round,
                       tooltip = sprintf("%s\nPressure %.0f\nSentiment %s",
                                         round_lab, m_pressure, SENTIMENT_LEVEL[as.character(round)])),
          size = 2.2, colour = COL$primary) +
        ggplot2::geom_point(data = d[d$round == 21, ],
                            size = 3.4, colour = COL$danger) +
        ggplot2::geom_point(data = d[d$round == cur, ],
                            size = 4, shape = 21, fill = COL$warn, colour = COL$ink, stroke = 1) +
        ggplot2::annotate("text", x = cur, y = pmin(cur_p + 7, 104), label = "\u25BC", size = 3, colour = COL$ink) +
        ggplot2::annotate("text", x = cur, y = pmin(cur_p + 12, 110), label = "you are here", size = 2.4, colour = COL$ink) +
        ggplot2::scale_x_continuous(breaks = seq(0, 22, 2)) +
        ggplot2::scale_y_continuous(limits = c(0, 114), breaks = c(0, 25, 50, 75, 100)) +
        ggplot2::labs(x = NULL, y = "pressure") +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                       panel.grid.major = ggplot2::element_line(colour = COL$grid),
                       axis.title.y = ggplot2::element_text(colour = "#6B7B83", size = 9))
      ggiraph::girafe(ggobj = g, width_svg = 4.6, height_svg = 2.8,
        options = list(ggiraph::opts_selection(type = "single"),
                       ggiraph::opts_hover(css = "stroke:#1C2B33;stroke-width:1.5px;"),
                       ggiraph::opts_tooltip(css = paste0(
                         "padding:6px 10px; font-size:12px; color:#2A3439; ",
                         "background:#FFFFFF; border:1px solid #C7D0D4; border-radius:6px; ",
                         "box-shadow:0 2px 8px rgba(0,0,0,0.12);")),
                       ggiraph::opts_toolbar(saveaspng = FALSE)))
    })
    observeEvent(input$pressure_selected, {
      sel <- suppressWarnings(as.integer(input$pressure_selected))
      if (!is.na(sel) && length(sel)) set_round(sel)
    })
  })
}
