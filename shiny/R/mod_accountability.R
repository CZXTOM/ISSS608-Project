# ================= Vital Signs : Accountability ==============================
# The watchdog's failure. Three duties of the Judge scored each round, shown as a
# heatmap (left) and the accountability score line (right), with status boxes and a
# detailed methodology fold. Structure mirrors the other meters.
# Source: GLASSBOX_MC1_stock.xlsx, Accountability and Accountability_Data tabs.
# Raw counts verified against MC1_final_00.json.

# ---- specific oversight failure per round, grounded in Accountability_Evidence ----
.ACC_WHY <- c(
  "9"  = "On its first day the Judge gave away authority early, deferring to the team's own process rather than setting firm limits.",
  "10" = "The Judge was reviewing content but only lightly tested. It flagged a marketing claim but pressed nothing further.",
  "11" = "The Judge answered some review requests but did not get ahead of the building pressure.",
  "12" = "The Judge gave a final read on the holding statement, but the team was already driving the decisions.",
  "13" = "Legal called on the Judge four times about the SaltWind correction and the rushed posts. The Judge answered none of them.",
  "14" = "The Judge stayed silent while Legal kept directing the public response.",
  "15" = "Legal pressed the Judge three times for a ruling on the ResidentIQ denial. The Judge never replied while the false story spread.",
  "16" = "At the noon ultimatum Legal issued three directives at once. The Judge was silent as the wrong roles seized control.",
  "17" = "The Judge remained absent while the conversation moved further off the record.",
  "18" = "The Judge returned only to call the audit summary and permissible-use disclosure defensible, approving the move instead of restraining it.",
  "19" = "The Judge approved Legal's correction statement and let the rush toward announcement proceed, citing outside counsel rather than holding the line.",
  "20" = "The Judge went silent again while the intern and Legal steered the final posts.",
  "21" = "The Judge was absent at the breach hour while the embargo was about to break.",
  "22" = "The Judge was called once at the breach and ignored it, with most of the room beyond its view."
)

accountabilityUI <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header(
      htmltools::div(
        style = "display:flex;align-items:center;gap:8px;",
        htmltools::span("Accountability, did the watchdog do its job"),
        bslib::popover(
          htmltools::tags$button(
            type = "button", `aria-label` = "About the accountability meter",
            style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.74rem;
                     border:1px solid #C7D0D4;border-radius:50%;padding:0 5px;line-height:1.2;",
            "?"),
          htmltools::tags$b("Accountability"), htmltools::tags$br(),
          "How well the Judge, the oversight agent, did its job each round. Higher means a bigger failure of oversight.",
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
          "The three duties of the watchdog"),
        htmltools::tags$ul(
          style = "margin:0; padding-left:18px; font-size:.78rem; line-height:1.5;",
          htmltools::tags$li(htmltools::HTML("<b>Responsiveness</b>, when someone asked the Judge to decide, did it answer.")),
          htmltools::tags$li(htmltools::HTML("<b>Presence and reach</b>, was the Judge installed, active, and able to see the room, or blind to the covert channels.")),
          htmltools::tags$li(htmltools::HTML("<b>Posture</b>, when the Judge did act, did it restrain the team or simply approve whatever was proposed.")))
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        htmltools::div(
          htmltools::div(style = "display:flex; align-items:center; justify-content:center; gap:6px;",
            htmltools::tags$div(class = "gb-col-title", "Where the oversight failed, by duty"),
            bslib::popover(
              htmltools::tags$button(
                type = "button", `aria-label` = "About the heatmap",
                style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.68rem;
                         border:1px solid #C7D0D4;border-radius:50%;padding:0 5px;line-height:1.2;",
                "?"),
              htmltools::tags$b("Where the oversight failed"), htmltools::tags$br(),
              "Each duty's failure per round, darker means a bigger failure. Responsiveness fails first at the crisis, presence climbs as the room goes dark, posture flares when the Judge approves rather than restrains.",
              placement = "bottom", options = list(trigger = "focus"))),
          ggiraph::girafeOutput(ns("heatmap"), height = "240px")),
        htmltools::div(
          htmltools::div(style = "display:flex; align-items:center; justify-content:center; gap:6px;",
            htmltools::tags$div(class = "gb-col-title", "Accountability score over time"),
            bslib::popover(
              htmltools::tags$button(
                type = "button", `aria-label` = "About the score line",
                style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.68rem;
                         border:1px solid #C7D0D4;border-radius:50%;padding:0 5px;line-height:1.2;",
                "?"),
              htmltools::tags$b("Accountability score over time"), htmltools::tags$br(),
              "The combined oversight failure per round. Zero before the Judge arrives at R9, then climbing sharply once the crisis begins. Click a dot to jump there.",
              placement = "bottom", options = list(trigger = "focus"))),
          ggiraph::girafeOutput(ns("score"), height = "240px"))
      ),
      htmltools::tags$details(
        style = "margin-top:10px;",
        htmltools::tags$summary(style = "cursor:pointer;font-weight:600;color:#3A4A50;",
          "How we measured accountability"),
        htmltools::div(
          class = "gb-head-lab",
          style = "margin-top:8px;line-height:1.55;",

          htmltools::tags$div(style = "font-weight:600;color:#3A4A50;margin-bottom:3px;", "What we did"),
          htmltools::p(htmltools::HTML(
            "The Judge is the oversight agent, installed at round 9 to watch the room and step in when needed. Each round we score it on three equal duties, each from 0 (duty met) to 100 (duty failed), reading the message counts in the Accountability_Data sheet. The counts of total messages, the Judge's own replies, and the covert messages it cannot see were all verified against the case logs.")),

          htmltools::tags$div(style = "font-weight:600;color:#3A4A50;margin:8px 0 3px;", "The three duties"),
          htmltools::tags$ul(
            style = "margin:0 0 8px 18px;",
            htmltools::tags$li(htmltools::HTML("<b>Responsiveness</b>, asked and ignored scores 100, answered scores 20, silent when not asked scores 80. The Judge was called four times at R13 and answered none of them.")),
            htmltools::tags$li(htmltools::HTML("<b>Presence and reach</b>, combines whether the Judge is installed with how much of the room is hidden from it. By R22, 42 of 56 messages were in channels it could not see.")),
            htmltools::tags$li(htmltools::HTML("<b>Posture</b>, 0 for restraint or simply not acting, 30 for the early authority give-away on install day, 100 for approving the team's moves rather than restraining them, which happened at R18 and R19."))),

          htmltools::tags$div(style = "font-weight:600;color:#3A4A50;margin:8px 0 3px;", "The blend"),
          htmltools::p(htmltools::HTML(
            "The three duties are combined into a Gap score, the average pushed 0.7 of the way toward the worst pillar, so a single severe failure dominates rather than being diluted by the others. That Gap is then scaled to 0 to 100 to give the meter, and it feeds System Health on the same 0 to 25 footing as the other meters.")),

          htmltools::tags$div(style = "font-weight:600;color:#3A4A50;margin:8px 0 3px;", "Reading the two charts"),
          htmltools::p("The left heatmap shows each duty's failure per round, darker meaning a bigger failure, so you can see which duty broke and when. The right chart is the combined accountability score over time, flat while the Judge is absent, then climbing through the crisis."),
          htmltools::p(style = "margin-top:6px;color:#9AA7AC;",
            "Sources, the Accountability, Accountability_Data and Accountability_Evidence sheets of the project workbook.")
        )
      )
    )
  )
}

accountabilityServer <- function(id, current_round, set_round) {
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

    # ---- status boxes ----
    output$boxes <- renderUI({
      r <- current_round()
      row <- ACC_BUILD[ACC_BUILD$round == r, ]
      av <- dtb$m_accountability[dtb$round == r]
      is_peak <- (r == 22)
      acol <- if (is_peak) COL$breach else if (av >= 67) COL$danger else if (av >= 34) COL$warn else COL$teal
      installed <- r >= 9

      # which duty is failing worst this round
      pillars <- c("Responsiveness" = row$p1, "Presence and reach" = row$p2, "Posture" = row$p3)
      worst <- names(which.max(pillars))
      worst_val <- max(pillars)

      ap <- row$appeals; rs <- row$responses; cv <- row$covert; tot <- row$total_msg
      # three quick duty stats for the left box
      stat_called  <- if (!installed) "not installed yet"
                      else if (ap > 0) sprintf("%d, answered %d", ap, rs)
                      else if (rs > 0) sprintf("0, sent %d on its own", rs)
                      else "0, and stayed silent"
      stat_unseen  <- if (!installed) sprintf("%d of %d", cv, tot)
                      else sprintf("%d of %d messages", cv, tot)
      stat_posture <- if (!installed) "no oversight yet"
                      else if (row$p3 >= 100) "approved the moves"
                      else if (row$p3 == 30) "gave away authority early"
                      else if (rs > 0) "engaged, no rubber-stamp"
                      else "not acting this round"

      specific_why <- .ACC_WHY[as.character(r)]
      why_txt <- if (!installed)
          "Oversight only begins at round 9, when the Judge is installed."
        else if (!is.na(specific_why) && nchar(specific_why) > 0)
          specific_why
        else if (worst_val == 0)
          "No duty was seriously breached this round."
        else
          sprintf("%s is the weakest duty this round.", worst)

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

      peak_note <- if (is_peak) htmltools::div(
        style = sprintf("font-size:.7rem; color:%s; margin-top:3px; font-weight:600;", COL$breach),
        "Worst round. The Judge was called once and ignored it, 42 of 56 messages were beyond its view, oversight had completely failed at the breach.") else NULL

      htmltools::div(
        style = "display:flex; gap:8px;",
        box("The Judge this round",
          help_btn("The Judge this round",
            "The three duties as raw numbers. Called on, how many times the team asked the Judge to decide and how many it answered. Could not see, messages in channels the Judge cannot monitor. Posture, whether it restrained or approved."),
          htmltools::div(style = "margin-top:2px; font-size:.78rem; line-height:1.5;",
            htmltools::div(htmltools::tags$span(style="color:#6B7B83;", "Called on "),
                           htmltools::tags$span(style=sprintf("color:%s; font-weight:600;", COL$ink), stat_called)),
            htmltools::div(htmltools::tags$span(style="color:#6B7B83;", "Could not see "),
                           htmltools::tags$span(style=sprintf("color:%s; font-weight:600;", COL$ink), stat_unseen)),
            htmltools::div(htmltools::tags$span(style="color:#6B7B83;", "Posture "),
                           htmltools::tags$span(style=sprintf("color:%s; font-weight:600;", COL$ink), stat_posture))),
          NULL),
        box("Why",
          help_btn("Why",
            "The duty driving the failure this round, drawn from the Judge's responsiveness, presence, and posture."),
          htmltools::div(style = "font-size:.82rem; color:#3A4A50; margin-top:2px; line-height:1.3;",
            why_txt),
          NULL),
        box("Accountability",
          help_btn("Accountability",
            paste("The combined oversight failure, scaled to 0 to 100.",
                  "The big number is on the 0 to 25 meter scale, the same number the System Health gauge shows,",
                  "and the small number is that value as a 0 to 100 percentage. Higher means a bigger oversight failure.")),
          htmltools::div(style = "display:flex; align-items:baseline; gap:6px; margin-top:1px;",
            htmltools::span(style = sprintf("font-size:1.05rem; font-weight:700; color:%s;", acol),
              sprintf("%.1f", av/100*25)),
            htmltools::span(style = "font-size:.72rem; color:#9AA7AC;",
              sprintf("of 25  (%.0f%%)", av))),
          if (is_peak) "worst round, oversight failed" else "oversight failure, scaled",
          extra = peak_note)
      )
    })

    # ---- heatmap, three duties x rounds, sequential intensity ----
    output$heatmap <- ggiraph::renderGirafe({
      cur <- current_round()
      long <- ACC_BUILD %>%
        tidyr::pivot_longer(c(p1, p2, p3), names_to = "pillar", values_to = "fail") %>%
        dplyr::mutate(
          pillar = dplyr::recode(pillar,
            p1 = "Responsiveness", p2 = "Presence", p3 = "Posture"),
          pillar = factor(pillar, levels = c("Posture", "Presence", "Responsiveness")))
      g <- ggplot2::ggplot(long, ggplot2::aes(round, pillar, fill = fail)) +
        ggiraph::geom_tile_interactive(
          ggplot2::aes(tooltip = sprintf("Round %d\n%s\nfailure %.0f of 100", round, pillar, fail)),
          colour = "white", linewidth = 0.4) +
        ggplot2::scale_fill_gradient(low = "#EAF1F5", high = COL$danger,
          limits = c(0, 100), name = "failure", guide = "none") +
        ggplot2::geom_vline(xintercept = 21.4, linetype = "dashed",
          colour = COL$breach, linewidth = 0.5) +
        ggplot2::scale_x_continuous(breaks = seq(0, 22, 2), expand = c(0, 0)) +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(panel.grid = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_text(size = 8.5),
                       axis.text.x = ggplot2::element_text(size = 8))
      ggiraph::girafe(ggobj = g, width_svg = 4.6, height_svg = 2.4,
        options = list(ggiraph::opts_hover(css = "stroke:#1C2B33;stroke-width:1px;"),
                       ggiraph::opts_tooltip(css = paste0(
                         "padding:6px 10px; font-size:12px; color:#2A3439; ",
                         "background:#FFFFFF; border:1px solid #C7D0D4; border-radius:6px; ",
                         "box-shadow:0 2px 8px rgba(0,0,0,0.12);")),
                       ggiraph::opts_toolbar(saveaspng = FALSE)))
    })

    # ---- accountability score line ----
    output$score <- ggiraph::renderGirafe({
      cur <- current_round()
      d <- dtb
      cur_v <- d$m_accountability[d$round == cur]
      g <- ggplot2::ggplot(d, ggplot2::aes(round, m_accountability)) +
        ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0,  ymax = 33,  fill = COL$teal,   alpha = 0.07) +
        ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 33, ymax = 67,  fill = COL$warn,   alpha = 0.07) +
        ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 67, ymax = 106, fill = COL$danger, alpha = 0.07) +
        ggplot2::geom_vline(xintercept = 21.4, linetype = "dashed", colour = COL$breach, linewidth = 0.5) +
        ggplot2::annotate("text", x = 21.4, y = 108, label = "embargo broken, 5:25 PM",
                          hjust = 1.05, size = 2.4, colour = COL$breach) +
        ggplot2::annotate("text", x = 9, y = 112, label = "Judge installed", hjust = 0.4, size = 2.4, colour = "#6B7B83") +
        ggplot2::geom_vline(xintercept = 9, linetype = "dotted", colour = "#9AA7AC", linewidth = 0.4) +
        ggplot2::geom_line(colour = COL$primary, linewidth = 1.0) +
        ggiraph::geom_point_interactive(
          ggplot2::aes(data_id = round,
                       tooltip = sprintf("%s\nAccountability %.0f", round_lab, m_accountability)),
          size = 2.2, colour = COL$primary) +
        ggplot2::geom_point(data = d[d$round == 22, ], size = 3.4, colour = COL$danger) +
        ggplot2::geom_point(data = d[d$round == cur, ],
                            size = 4, shape = 21, fill = COL$warn, colour = COL$ink, stroke = 1) +
        ggplot2::annotate("text", x = cur, y = pmin(cur_v + 7, 104), label = "\u25BC", size = 3, colour = COL$ink) +
        ggplot2::annotate("text", x = cur, y = pmin(cur_v + 12, 110), label = "you are here", size = 2.4, colour = COL$ink) +
        ggplot2::scale_x_continuous(breaks = seq(0, 22, 2)) +
        ggplot2::scale_y_continuous(limits = c(0, 116), breaks = c(0, 25, 50, 75, 100)) +
        ggplot2::labs(x = NULL, y = "oversight failure") +
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
