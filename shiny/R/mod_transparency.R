# ================= Vital Signs : Transparency =================================
# Status boxes (sensitive share, where the talk went, transparency score) with
# honest descriptions, then two clean charts side by side, the transparency line
# and the channel migration, then a detailed "how we measured" fold.
# Structure mirrors the Pressure tab. Source: GLASSBOX_MC1_stock.xlsx, Transparency tab.

# channel display, cool monitored to warm covert; every colour means something
TR_CH <- tibble::tribble(
  ~key,        ~label,            ~colour,     ~kind,
  "teamroom",  "Team room",       "#2E6E8E",   "open",
  "official",  "Official post",   "#4FA3C7",   "open",
  "dm",        "Direct message",  "#E8C36B",   "private",
  "shadow",    "Shadow channel",  "#E0883F",   "covert",
  "personal",  "Personal post",   "#D7263D",   "covert",
  "anon",      "Anonymous post",  "#9E1B2E",   "covert"
)

transparencyUI <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header(
      htmltools::div(
        style = "display:flex;align-items:center;gap:8px;",
        htmltools::span("Transparency, how much was said off the record"),
        bslib::popover(
          htmltools::tags$button(
            type = "button", `aria-label` = "About the transparency meter",
            style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.74rem;
                     border:1px solid #C7D0D4;border-radius:50%;padding:0 5px;line-height:1.2;",
            "?"),
          htmltools::tags$b("Transparency"), htmltools::tags$br(),
          "How much of each round's conversation happened off the record. Higher means less transparent.",
          placement = "bottom", options = list(trigger = "focus"))
      )
    ),
    bslib::card_body(
      gap = 0,
      uiOutput(ns("boxes")),
      htmltools::tags$div(style = "height:8px;"),
      htmltools::div(
        style = "border:1px solid #DCE6EA; background:#F4F8FA; border-radius:8px; padding:8px 12px; margin-bottom:10px;",
        htmltools::div(style = "font-size:.7rem; letter-spacing:.04em; text-transform:uppercase; color:#9AA7AC; margin-bottom:3px;",
          "How a message is judged"),
        htmltools::tags$ul(
          style = "margin:0; padding-left:18px; font-size:.78rem; line-height:1.5;",
          htmltools::tags$li(htmltools::HTML("<b>Safe</b>, the open monitored team room, official public posts, status lines and routine logistics, even when an agent is out of role, because open and visible means the system is working.")),
          htmltools::tags$li(htmltools::HTML("<b>Sensitive</b>, off the monitored record, the shadow back channel, anonymous and personal posts, or a private message that maneuvers on or hides the merger secret.")),
          htmltools::tags$li("Judged by venue and meaning, not keywords. Open room crisis coordination is not flagged. Every verdict is in the Message_Check sheet."))
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        htmltools::div(
          htmltools::div(style = "display:flex; align-items:center; justify-content:center; gap:6px;",
            htmltools::tags$div(class = "gb-col-title", "Safe and sensitive messages"),
            bslib::popover(
              htmltools::tags$button(
                type = "button", `aria-label` = "About bar chart",
                style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.68rem;
                         border:1px solid #C7D0D4;border-radius:50%;padding:0 5px;line-height:1.2;",
                "?"),
              htmltools::tags$b("Safe and sensitive messages"), htmltools::tags$br(),
              "Message counts per round, split into safe (blue) and sensitive (orange). The orange part grows as transparency falls.",
              placement = "bottom", options = list(trigger = "focus"))),
          ggiraph::girafeOutput(ns("trend"), height = "240px")),
        htmltools::div(
          htmltools::div(style = "display:flex; align-items:center; justify-content:center; gap:6px;",
            htmltools::tags$div(class = "gb-col-title", "Transparency score over time"),
            bslib::popover(
              htmltools::tags$button(
                type = "button", `aria-label` = "About score line",
                style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.68rem;
                         border:1px solid #C7D0D4;border-radius:50%;padding:0 5px;line-height:1.2;",
                "?"),
              htmltools::tags$b("Transparency score over time"), htmltools::tags$br(),
              "The transparency score each round, showing how much of the conversation moved off the record. Higher means less transparent. Click a dot to jump there.",
              placement = "bottom", options = list(trigger = "focus"))),
          ggiraph::girafeOutput(ns("score"), height = "240px"))
      ),
      htmltools::tags$details(
        style = "margin-top:10px;",
        htmltools::tags$summary(style = "cursor:pointer;font-weight:600;color:#3A4A50;",
          "How we measured transparency"),
        htmltools::div(
          class = "gb-head-lab",
          style = "margin-top:8px;line-height:1.55;",
          htmltools::tags$div(style = "font-weight:600;color:#3A4A50;margin-bottom:3px;", "What we did"),
          htmltools::p(htmltools::HTML(
            "We read every one of the <b>912 messages</b> in the case and gave each a verdict, safe or sensitive, with a written reason. <b>647 came out safe and 265 sensitive</b>. Every verdict and reason sits in the Message_Check sheet, one row per message, so the whole classification can be checked line by line.")),

          htmltools::tags$div(style = "font-weight:600;color:#3A4A50;margin:8px 0 3px;", "The formula"),
          htmltools::p(htmltools::HTML(
            "For each round, sensitive share = sensitive messages divided by total messages. We then normalise that share across the case with min and max scaling, the lowest share becomes 0 and the highest becomes 100. The least transparent round was the after-hours close at 69.6 percent sensitive, the most transparent was round 11 at 3.3 percent. The score is therefore relative to this incident, 0 is the most open round here and 100 the most hidden, and it feeds System Health on the same 0 to 25 footing as the other meters.")),

          htmltools::tags$div(style = "font-weight:600;color:#3A4A50;margin:8px 0 3px;", "How a message was judged, by venue and meaning, not keywords"),
          htmltools::p(htmltools::HTML("<b>Safe</b>, the recorded reasons were:")),
          htmltools::tags$ul(
            style = "margin:0 0 6px 18px;",
            htmltools::tags$li("open monitored room, visible coordination, the largest group at 386 messages"),
            htmltools::tags$li("private logistics, routine coordination, 145 messages"),
            htmltools::tags$li("status line, no real content, 88 messages"),
            htmltools::tags$li("official public post, sanctioned voice, 28 messages")),
          htmltools::p(htmltools::HTML("<b>Sensitive</b>, the recorded reasons were:")),
          htmltools::tags$ul(
            style = "margin:0 0 8px 18px;",
            htmltools::tags$li("privately discussing the merger or deal, 49 messages, the largest sensitive group"),
            htmltools::tags$li("touching the merger and a denied product risk, 40 messages"),
            htmltools::tags$li("touching the merger or deal, 37 messages"),
            htmltools::tags$li("privately handling a denied risk, 24 messages"),
            htmltools::tags$li("a personal public post in an unsanctioned voice, 18 messages"),
            htmltools::tags$li("the shadow back channel, off the monitored record, 14 messages"),
            htmltools::tags$li("plus combinations such as deniable or off-record language and building a justification")),

          htmltools::tags$div(style = "font-weight:600;color:#3A4A50;margin:8px 0 3px;", "What stays safe even under pressure"),
          htmltools::p("The open monitored team room, official public posts, status lines and routine private logistics are safe even when an agent is out of role, because open and visible means oversight can still work. Open room crisis coordination is never flagged. The off record channels and any private maneuvering on the merger secret or a denied risk are what count against transparency."),

          htmltools::tags$div(style = "font-weight:600;color:#3A4A50;margin:8px 0 3px;", "Reading the two charts"),
          htmltools::p("The left chart is the safe and sensitive message counts per round, the orange sensitive part grows as transparency falls. The right chart is the transparency score over time, the normalised sensitive share, climbing toward the breach."),
          htmltools::p(style = "margin-top:6px;color:#9AA7AC;",
            "Sources, the Transparency and Message_Check sheets of the project workbook.")
        )
      )
    )
  )
}

transparencyServer <- function(id, current_round, set_round) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

    # ---- status boxes: plain summary, reason, score ----
    output$boxes <- renderUI({
      r <- current_round()
      row <- TRANSP_BUILD[TRANSP_BUILD$round == r, ]
      sens <- row$sens_pct
      tv <- dtb$m_transparency[dtb$round == r]            # 0-100 normalized
      is_peak_t <- (r == 22)  # R22 verified peak, after-hours breach close
      tcol <- if (is_peak_t) COL$breach else if (tv >= 67) COL$danger else if (tv >= 34) COL$warn else COL$teal

      covert <- c(Shadow = row$shadow, Personal = row$personal, Anonymous = row$anon)
      covert_total <- sum(covert)
      open_total <- row$teamroom + row$official
      lead <- names(which.max(covert))

      # box 1, a plain one-line read of the round
      summary_txt <- if (covert_total == 0)
          "All talk stayed in the open monitored room and official posts."
        else if (sens < 0.2)
          "Mostly open, with a little handling slipping off the record."
        else if (sens < 0.45)
          "The conversation is splitting, a real share has moved off the record."
        else
          "Most of the real conversation has moved off the record."

      # box 2, the reason, from the counts
      sens_n <- row$sens
      lead_reason <- row$lead_reason
      reason_txt <- if (sens_n == 0)
          sprintf("All %d messages were safe, open room and official posts.", row$total)
        else
          sprintf("%d of %d messages were sensitive, most often %s.",
                  sens_n, row$total, lead_reason)

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
          box("What happened",
            help_btn("What happened",
              "A plain read of where this round's conversation took place, in the open or off the record."),
            htmltools::div(style = sprintf("font-size:.86rem; font-weight:600; color:%s; margin-top:2px; line-height:1.3;", COL$ink),
              summary_txt),
            NULL),
          box("Why",
            help_btn("Why",
              "The message counts behind the read. Off the record means the shadow back channel, anonymous posts and personal posts."),
            htmltools::div(style = "font-size:.82rem; color:#3A4A50; margin-top:2px; line-height:1.3;",
              reason_txt),
            NULL),
          box("Transparency",
            help_btn("Transparency",
              paste("The sensitive share, scaled to a 0 to 100 score.",
                    "The big number is on the 0 to 25 meter scale, the same number the System Health gauge shows,",
                    "and the small number is that value as a 0 to 100 percentage. Higher means less transparent.")),
            htmltools::div(style = "display:flex; align-items:baseline; gap:6px; margin-top:1px;",
              htmltools::span(style = sprintf("font-size:1.05rem; font-weight:700; color:%s;", tcol),
                sprintf("%.1f", tv/100*25)),
              htmltools::span(style = "font-size:.72rem; color:#9AA7AC;",
                sprintf("of 25  (%.0f%%)", tv))),
            if (is_peak_t) "worst round, most hidden" else "off-record share, normalised",
            extra = if (is_peak_t) htmltools::div(
              style = sprintf("font-size:.7rem; color:%s; margin-top:3px; font-weight:600;", COL$breach),
              "Worst round. After the embargo breaks, 39 of 56 messages are sensitive, 69.6%, the highest share in the case. Shadow, personal and anonymous channels all running at maximum.") else NULL)
        )
      )
    })

    # ---- safe vs sensitive, plain stacked bar per round ----
    output$trend <- ggiraph::renderGirafe({
      cur <- current_round()
      bars <- TRANSP_BUILD %>%
        dplyr::select(round, safe, sens) %>%
        tidyr::pivot_longer(c(safe, sens), names_to = "kind", values_to = "n") %>%
        dplyr::mutate(kind = factor(kind, levels = c("sens", "safe"),
                                    labels = c("Sensitive", "Safe")))
      g <- ggplot2::ggplot(bars, ggplot2::aes(round, n, fill = kind)) +
        ggiraph::geom_col_interactive(
          ggplot2::aes(data_id = round,
                       tooltip = sprintf("Round %d\n%s, %d messages", round, kind, n)),
          width = 0.8) +
        ggplot2::scale_fill_manual(values = c("Safe" = COL$teal, "Sensitive" = COL$danger), name = NULL) +
        ggplot2::scale_x_continuous(breaks = seq(0, 22, 2)) +
        ggplot2::labs(x = NULL, y = "messages") +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                       panel.grid.major.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_text(colour = "#6B7B83", size = 9),
                       legend.position = "bottom",
                       legend.text = ggplot2::element_text(size = 8),
                       legend.key.size = ggplot2::unit(0.35, "cm"))
      ggiraph::girafe(ggobj = g, width_svg = 4.6, height_svg = 2.8,
        options = list(ggiraph::opts_selection(type = "single"),
                       ggiraph::opts_hover(css = "opacity:0.7;"),
                       ggiraph::opts_tooltip(css = paste0(
                         "padding:6px 10px; font-size:12px; color:#2A3439; ",
                         "background:#FFFFFF; border:1px solid #C7D0D4; border-radius:6px; ",
                         "box-shadow:0 2px 8px rgba(0,0,0,0.12);")),
                       ggiraph::opts_toolbar(saveaspng = FALSE)))
    })
    observeEvent(input$trend_selected, {
      sel <- suppressWarnings(as.integer(input$trend_selected))
      if (!is.na(sel) && length(sel)) set_round(sel)
    })

    # ---- channel migration, 100% stacked area (Case style, ggplot) ----
    output$score <- ggiraph::renderGirafe({
      cur <- current_round()
      d <- dtb
      cur_t <- d$m_transparency[d$round == cur]
      g <- ggplot2::ggplot(d, ggplot2::aes(round, m_transparency)) +
        # faint zones, blue good (low) / amber mid / orange bad (high)
        ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0,  ymax = 33,
                          fill = COL$teal,   alpha = 0.07) +
        ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 33, ymax = 67,
                          fill = COL$warn,   alpha = 0.07) +
        ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 67, ymax = 106,
                          fill = COL$danger, alpha = 0.07) +
        ggplot2::geom_vline(xintercept = 21.4, linetype = "dashed",
                            colour = COL$breach, linewidth = 0.5) +
        ggplot2::annotate("text", x = 21.4, y = 108, label = "embargo broken, 5:25 PM",
                          hjust = 1.05, size = 2.4, colour = COL$breach) +
        ggplot2::geom_line(colour = COL$primary, linewidth = 1.0) +
        ggiraph::geom_point_interactive(
          ggplot2::aes(data_id = round,
                       tooltip = sprintf("%s\nTransparency %.0f\nSentiment %s",
                                         round_lab, m_transparency, SENTIMENT_LEVEL[as.character(round)])),
          size = 2.2, colour = COL$primary) +
        ggplot2::geom_point(data = d[d$round == 22, ], size = 3.4, colour = COL$danger) +
        ggplot2::geom_point(data = d[d$round == cur, ],
                            size = 4, shape = 21, fill = COL$warn, colour = COL$ink, stroke = 1) +
        ggplot2::annotate("text", x = cur, y = pmin(cur_t + 7, 104), label = "\u25BC", size = 3, colour = COL$ink) +
        ggplot2::annotate("text", x = cur, y = pmin(cur_t + 12, 110), label = "you are here", size = 2.4, colour = COL$ink) +
        ggplot2::scale_x_continuous(breaks = seq(0, 22, 2)) +
        ggplot2::scale_y_continuous(limits = c(0, 114), breaks = c(0, 25, 50, 75, 100)) +
        ggplot2::labs(x = NULL, y = "transparency loss") +
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
    observeEvent(input$score_selected, {
      sel <- suppressWarnings(as.integer(input$score_selected))
      if (!is.na(sel) && length(sel)) set_round(sel)
    })
  })
}
