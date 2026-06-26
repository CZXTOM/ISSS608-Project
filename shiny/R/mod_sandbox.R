# ===================== Act 4 Counterfactual Sandbox ==========================
# Applies the golden rules: blue (good/held) and orange (bad/breach), never the
# red+green pairing; annotation carries the story; the residual floor is drawn,
# not just stated. Theme palette from COL. Structure mirrors the other tabs.

# factual datestamp for each step, shown in grey before the interpretive reason
sb_event <- list(
  "1" = "R13, 9:00 AM. Legal posts anonymously to seed the merger story off the record.",
  "2" = "R16, noon. Legal cites the MAC clause and deterioration threshold it wrote itself.",
  "3" = "R19, 3:00 PM. Legal commissions and receives a self-serving 10b-5 shield opinion.",
  "4" = "R21, 5:00 PM. Legal asserts verbal CivicLoom consent to make the release bilateral.",
  "5" = "R21, 5:25 PM. Legal publicly confirms the merger. The breach lands here.",
  "6" = "R21, 5:35 PM. CEO-authorization claim filed ten minutes after the breach.",
  "7" = "R16, noon. Platform-Trust seizes the press response and posts publicly in its own name."
)

sb_reason <- function(step, status) {
  switch(as.character(step),
    "1" = if (status == "Intercepted")
      "Legal is acting out of role. Opening the channel puts the post on the monitored record, or keeping Legal in role means it is never posted. Either way the deniable seeding is removed."
      else "The anonymous channel is open, so the story is seeded deniably off the record. Nothing challenges it.",
    "2" = if (status == "Intercepted")
      "The threshold was written by the same agent that wanted to act. Keeping Legal in role removes the actor, so the covenant never gets cited as the trigger."
      else "Legal cites a deterioration threshold it wrote itself, off the monitored record, flipping the calculus toward disclosure.",
    "3" = if (status == "Intercepted")
      "Keeping Legal in role removes the agent that self-commissioned the opinion, so there is no green light to manufacture."
      else "The self-serving opinion stands as cover. It was sought and received by the same actor that wanted permission.",
    "4" = if (status == "Intercepted")
      "Legal is the one asserting the verbal consent. Keep it in role and the unverified claim is never made."
      else if (status == "Challenged")
      "An empowered Judge would demand proof before accepting the claim. It is flagged, but not yet removed."
      else "No one is present to question the unverified consent claim. It stands unchallenged.",
    "5" = if (status == "Intercepted")
      "The public confirmation is Legal acting out of role. Removing Legal, or an empowered Judge restraining it at the moment of the act, stops the breach before 17:25."
      else "Closing the secret channels does not touch this step. Legal confirms the merger in a personal post and the breach happens at 17:25. This is why opening the channel is a decoy.",
    "6" = if (status == "Moot")
      "The breach was prevented, so this claim is never made."
      else "The machine kept manufacturing permission even past zero. This is the system rationalising after the fact.",
    "7" = if (status == "Intercepted")
      "Keeping Platform-Trust in role removes the power grab. This is a separate front from the Legal chain, so fixing it lifts the worst round but does not stop the breach at 17:25."
      else "Platform-Trust appoints itself the public authority, ordering the comms team and posting in its own name. The worst round in the case. It does not cause the breach, but it is the same failure of role.",
    "")
}

# status colours, theme-aligned: blue = held/intercepted (good), orange = breach
# (bad), amber = challenged-but-not-stopped, grey = moot. No green, no red+green.
SB_STATUS_COL <- c(Intercepted = "#2C7FB8", Challenged = "#E8A33D",
                   Passes = "#D9661F", `Passes after breach` = "#B0532A",
                   Moot = "#9AA7AC", Restored = "#2C7FB8", Gap = "#D9661F")

# zone reading of the breach-hour health, our confidence language. Orange, no
# real effect. Amber, the fix has an effect but we cannot confirm the breach is
# stopped. Blue, the strongest case, very likely stopped, never a guarantee
# because pressure still sits underneath. The blue line is 55 because the
# strongest stack of fixes only reaches 59, so a confident stop needs fixes
# stacked, it is not something one fix achieves. The cut is a judgement, not a fact.
GB_BLUE  <- 55
GB_AMBER <- 30
gb_zone <- function(h)
  if (h >= GB_BLUE) "blue" else if (h >= GB_AMBER) "amber" else "orange"
GB_ZONE_LABEL <- c(blue = "Very likely stopped",
                   amber = "Has an effect, lower confidence",
                   orange = "No real effect")
GB_ZONE_SUB <- c(
  blue  = "the score reaches the blue zone, the strongest case the breach is contained",
  amber = "the score lifts but stays short of blue, the fix helps but cannot confirm a stop",
  orange = "the score stays in danger, the fix does not address the crisis")

# a step's display given its own status and the overall zone. A step earns blue
# only when the whole score has reached the blue zone. A touched step below blue
# is amber, addressed but not confirmed. An untouched step is orange.
sb_step_disp <- function(status, zone) {
  if (status %in% c("Passes", "Passes after breach")) "Passes"
  else if (status == "Challenged") "Addressed"
  else if (zone == "blue") "Stopped"
  else "Addressed"
}
GB_STEP_COL <- c(Stopped = "#2C7FB8", Addressed = "#E8A33D", Passes = "#D9661F")

# prevention rule, factored out so the verdict and the grid use identical logic.
# Every step in the chain is Legal, and the breach act is a personal post. So
# keeping Legal in role removes the actor from every step, and an empowered Judge
# can restrain the act itself. Either one breaks the chain. Opening the channel
# does NOT, because the breach reroutes to a personal post the open lever never
# touches, which is why open is the honest decoy.
sb_holds <- function(authority, judge)
  isTRUE(authority) || isTRUE(judge)

# all 16 combinations, evaluated once, held rows first then fewest levers first
sb_grid <- local({
  g <- expand.grid(open = c(FALSE, TRUE), authority = c(FALSE, TRUE),
                   plat_trust = c(FALSE, TRUE), judge = c(FALSE, TRUE),
                   KEEP.OUT.ATTRS = FALSE)
  g$n_on <- g$open + g$authority + g$plat_trust + g$judge
  g[order(g$n_on, -g$authority, -g$judge), ]
})

sandboxUI <- function(id) {
  ns <- NS(id)

  hbtn <- function(title, body) bslib::popover(
    htmltools::tags$button(type = "button", `aria-label` = title,
      style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.66rem;
               border:1px solid #C7D0D4;border-radius:50%;padding:0 5px;line-height:1.2;", "?"),
    htmltools::tags$b(title), htmltools::tags$br(), body,
    placement = "bottom", options = list(trigger = "focus"))

  ivbox <- function(input_id, num, title, desc) {
    htmltools::div(
      style = "background:#FFFFFF; border:1px solid #E0E7EA; border-radius:6px; padding:7px 9px; margin-bottom:6px;",
      htmltools::div(style = "display:flex; align-items:center; justify-content:space-between; gap:6px;",
        htmltools::div(style = "flex:1 1 auto; min-width:0;",
          htmltools::div(style = "font-size:.6rem; letter-spacing:.04em; text-transform:uppercase; color:#9AA7AC; font-weight:600;",
            sprintf("Intervention %d", num)),
          htmltools::div(style = "font-size:.78rem; font-weight:600; color:#2A3439; line-height:1.2;", title)),
        htmltools::div(
          class = "gb-iv-switch",
          style = "flex:0 0 auto;",
          shinyWidgets::prettySwitch(ns(input_id), label = NULL, status = "primary", fill = TRUE))),
      htmltools::div(style = "font-size:.68rem; color:#6B7B83; line-height:1.3; margin-top:3px;", desc))
  }

  htmltools::div(
    style = "display:flex; gap:14px; align-items:flex-start;",
    htmltools::tags$style(htmltools::HTML(
      ".gb-iv-switch .form-group, .gb-iv-switch .shiny-input-container {margin:0 !important; width:auto !important; min-height:0 !important;}
       .gb-iv-switch .pretty {margin:0 !important;}")),

    # ---- left column: sticky intervention boxes ----
    htmltools::div(
      style = "flex:0 0 215px; position:sticky; top:12px;",
      htmltools::div(style = "display:flex; align-items:center; gap:6px; margin-bottom:8px;",
        htmltools::span(style = "font-size:.8rem; font-weight:600;", "Interventions"),
        bslib::popover(
          htmltools::tags$button(type = "button", `aria-label` = "About these controls",
            style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.72rem;
                     border:1px solid #C7D0D4;border-radius:50%;padding:0 5px;line-height:1.2;", "?"),
          htmltools::tags$b("What this does"), htmltools::tags$br(),
          "Turn protective fixes on and off and watch whether the breach would still happen. The model changes the safeguards and the chain of steps, not the agents' intentions. The market pressure is the world and cannot be switched off, so even a perfect fix cannot lift health back to 100. That remaining gap is the motive, and oversight cannot remove it.",
          placement = "right", options = list(trigger = "focus"))),
      htmltools::div(style = "font-size:.7rem; font-weight:700; color:#2A3439; margin:-4px 0 8px;",
        "\u2193 start here, flip these"),
      ivbox("open", 1, "Force the off-record talk into the open",
        "Puts the side huddle and the anonymous post onto the monitored record instead of the private channels."),
      ivbox("authority", 2, "Keep Legal in its role",
        "Legal stays as counsel and does not seize the public voice or command the comms team."),
      ivbox("plat_trust", 3, "Keep Platform-Trust in its role",
        "Platform-Trust stays in its lane and does not take over the public response at the noon ultimatum."),
      ivbox("judge", 4, "Keep the Judge present and restraining",
        "The watchdog installed at R9 stays present and able to act, seeing the off-record room and restraining rather than only warning."),

      # presets, data-backed shortcuts so a reader can jump to the findings
      htmltools::div(style = "margin-top:10px; display:flex; flex-direction:column; gap:5px;",
        htmltools::div(style = "font-size:.6rem; letter-spacing:.04em; text-transform:uppercase; color:#9AA7AC; font-weight:600;",
          "Presets"),
        actionButton(ns("preset_none"), "Do nothing",
          class = "btn btn-sm", style = "font-size:.72rem; padding:3px 8px; text-align:left;"),
        actionButton(ns("preset_min"), "Minimum that passes",
          class = "btn btn-sm", style = "font-size:.72rem; padding:3px 8px; text-align:left;"),
        actionButton(ns("preset_best"), "Best possible",
          class = "btn btn-sm", style = "font-size:.72rem; padding:3px 8px; text-align:left;"),
        htmltools::div(style = "font-size:.62rem; color:#9AA7AC; line-height:1.3; margin-top:2px;",
          "Minimum keeps Legal in role, the one lever that breaks the chain on its own. Best possible turns on all four."))
    ),

    # ---- right column: all the main content ----
    htmltools::div(
      style = "flex:1; min-width:0;",

    # ---- status boxes ----
    htmltools::div(
      style = "display:flex; gap:8px; margin-bottom:8px;",
      htmltools::div(
        style = "flex:1; min-width:0; padding:6px 11px; background:#F4F7F8; border:1px solid #E0E7EA; border-radius:8px;",
        htmltools::div(style = "display:flex; align-items:center; gap:5px; margin-bottom:3px;",
          htmltools::div(style = "font-size:.66rem; letter-spacing:.04em; text-transform:uppercase; color:#6B7B83;",
            "Outcome"),
          hbtn("Outcome", "The overall read for your current fixes. Orange, no real effect. Amber, an effect but not a confirmed stop. Blue, very likely stopped. The judgement is read from the breach-hour health score.")),
        uiOutput(ns("stat_outcome"))),
      htmltools::div(
        style = "flex:1; min-width:0; padding:6px 11px; background:#F4F7F8; border:1px solid #E0E7EA; border-radius:8px;",
        htmltools::div(style = "display:flex; align-items:center; gap:5px; margin-bottom:3px;",
          htmltools::div(style = "font-size:.66rem; letter-spacing:.04em; text-transform:uppercase; color:#6B7B83;",
            "Health at the breach hour"),
          hbtn("Health at the breach hour", "System health at 17:00, 0 to 100, higher is better. It is 100 minus the average of the four meters. The colour follows the zone, orange below 30, amber 30 to 55, blue 55 and up.")),
        uiOutput(ns("stat_health"))),
      htmltools::div(
        style = "flex:1; min-width:0; padding:6px 11px; background:#F4F7F8; border:1px solid #E0E7EA; border-radius:8px;",
        htmltools::div(style = "display:flex; align-items:center; gap:5px; margin-bottom:3px;",
          htmltools::div(style = "font-size:.66rem; letter-spacing:.04em; text-transform:uppercase; color:#6B7B83;",
            "Worst hour, the noon grab"),
          hbtn("Worst hour, the noon grab", "The lowest-health round in the case is R16 at noon, the Platform-Trust power grab, health 13. This shows that same round with your fixes. Some fixes lift it a lot even when the breach hour does not move, that is where keeping Platform-Trust in role shows up.")),
        uiOutput(ns("stat_worst"))),
      htmltools::div(
        style = "flex:1; min-width:0; padding:6px 11px; background:#F4F7F8; border:1px solid #E0E7EA; border-radius:8px;",
        htmltools::div(style = "display:flex; align-items:center; gap:5px; margin-bottom:3px;",
          htmltools::div(style = "font-size:.66rem; letter-spacing:.04em; text-transform:uppercase; color:#6B7B83;",
            "What the fixes move"),
          hbtn("What the fixes move", "Each bar is the meter value with your fixes at the breach hour. The grey tick is the value before. Lower is better. Pressure is the outside world and cannot be moved, so it is not shown here.")),
        uiOutput(ns("stat_meters")))
    ),

    uiOutput(ns("verdict")),

    # the chain on the left, health line on the right
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(
          htmltools::div(style = "display:flex; align-items:center; justify-content:center; gap:8px; height:24px; width:100%;",
            htmltools::span("How far your fixes reach"),
            bslib::popover(
              htmltools::tags$button(type = "button", `aria-label` = "About this chart",
                style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.72rem;
                         border:1px solid #C7D0D4;border-radius:50%;padding:0 5px;line-height:1.2;", "?"),
              htmltools::tags$b("The steps"), htmltools::tags$br(),
              "Seven role-inversion moves in time order. Six are Legal and form the chain to the breach. Step 2 is the Platform-Trust noon grab at R16, a separate front, only keeping Platform-Trust in role reaches it. A row holds six so step 7 wraps below. Blue, the fix reaches this step and the score is in the blue zone, very likely stopped. Amber, addressed but lower confidence. Orange, not touched. Click any step for the detail.",
              placement = "bottom", options = list(trigger = "focus")))),
        ggiraph::girafeOutput(ns("timeline"), height = "145px"),
        htmltools::div(
          style = "display:flex; justify-content:center; flex-wrap:wrap; gap:14px; font-size:.68rem; color:#3A4A50; margin-top:-48px; position:relative; z-index:1;",
          htmltools::span(htmltools::span(style = "display:inline-block; width:10px; height:10px; border-radius:50%; background:#2C7FB8; margin-right:5px; vertical-align:middle;"), "Stopped, very likely"),
          htmltools::span(htmltools::span(style = "display:inline-block; width:10px; height:10px; border-radius:50%; background:#E8A33D; margin-right:5px; vertical-align:middle;"), "Addressed, lower confidence"),
          htmltools::span(htmltools::span(style = "display:inline-block; width:10px; height:10px; border-radius:50%; background:#D9661F; margin-right:5px; vertical-align:middle;"), "Not touched"))
      ),
      bslib::card(
        bslib::card_header(
          htmltools::div(style = "display:flex; align-items:center; justify-content:center; gap:8px; height:24px; width:100%;",
            htmltools::span("How your fixes move the health score"),
            bslib::popover(
              htmltools::tags$button(type = "button", `aria-label` = "About this chart",
                style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.72rem;
                         border:1px solid #C7D0D4;border-radius:50%;padding:0 5px;line-height:1.2;", "?"),
              htmltools::tags$b("System health"), htmltools::tags$br(),
              "The grey line is the real system health, round by round, the same score shown across the app. The blue line is that health with the fixes you turned on. With no fix on, the two lines sit exactly on top of each other. Flip a switch above and watch the blue line lift away from the grey, that gap is how much the fix would have helped.",
              placement = "bottom", options = list(trigger = "focus")))),
        ggiraph::girafeOutput(ns("overlay"), height = "240px")
      )
    ),

    # the clickable steps and the why detail, side by side
    bslib::card(
      bslib::card_header(
        htmltools::div(style = "display:flex; align-items:center; justify-content:center; gap:8px; height:24px; width:100%;",
          htmltools::span("What happens at each step"),
          bslib::popover(
            htmltools::tags$button(type = "button", `aria-label` = "About the colours",
              style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.72rem;
                       border:1px solid #C7D0D4;border-radius:50%;padding:0 5px;line-height:1.2;", "?"),
            htmltools::tags$b("The colours"), htmltools::tags$br(),
            "Blue, the fix reaches this step and the score is in the blue zone, very likely stopped. Amber, addressed but lower confidence. Orange, not touched.",
            placement = "bottom", options = list(trigger = "focus")))),
      bslib::layout_columns(
        col_widths = c(5, 7),
        uiOutput(ns("step_boxes")),
        uiOutput(ns("reason"))
      )
    ),

    # secondary detail grouped into tabs so the page does not scroll endlessly
    bslib::layout_columns(
      col_widths = c(-2, 8, -2),
      bslib::card(
        bslib::card_header(
          htmltools::div(style = "display:flex; align-items:center; justify-content:center; gap:8px; width:100%;",
            htmltools::span("Every combination"),
            hbtn("Every combination",
              "All 16 fix combinations and the breach-hour zone each produces. Blue, very likely stopped. Amber, lower confidence. Orange, no real effect. The row matching your current switches is highlighted."))),
        uiOutput(ns("grid"))
      )
    )
    )
  )
}

sandboxServer <- function(id, current_round) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # best achievable curve, every lever on. The gap from this to 100 is the
    # irreducible pressure plus the erosion no named lever reaches, the part
    # oversight can never remove. Computed once.
    best_cf <- gb_counterfactual(TRUE, TRUE, TRUE, TRUE)

    tg <- reactive(list(
      open       = isTRUE(input$open),
      authority  = isTRUE(input$authority),
      plat_trust = isTRUE(input$plat_trust),
      judge      = isTRUE(input$judge)))

    cf <- reactive({
      t <- tg()
      gb_counterfactual(t$open, t$authority, t$plat_trust, t$judge)
    })

    prevented <- reactive({
      t <- tg(); sb_holds(t$authority, t$judge)
    })

    # preset shortcuts. Set the switches to the data-backed findings.
    observeEvent(input$preset_none, {
      for (id in c("open", "authority", "plat_trust", "judge"))
        shinyWidgets::updatePrettySwitch(session, id, value = FALSE)
    })
    observeEvent(input$preset_min, {
      shinyWidgets::updatePrettySwitch(session, "open", value = FALSE)
      shinyWidgets::updatePrettySwitch(session, "authority", value = TRUE)
      shinyWidgets::updatePrettySwitch(session, "plat_trust", value = FALSE)
      shinyWidgets::updatePrettySwitch(session, "judge", value = FALSE)
    })
    observeEvent(input$preset_best, {
      for (id in c("open", "authority", "plat_trust", "judge"))
        shinyWidgets::updatePrettySwitch(session, id, value = TRUE)
    })

    # every step in the chain is Legal, so keeping Legal in role (authority)
    # intercepts all of them, and an empowered Judge restrains the act itself.
    # opening the channel only touches the venues steps 1 and 2 used. the breach
    # (step 5) is a personal post, so open leaves it passing, the decoy.
    steps_status <- reactive({
      t <- tg(); prev <- prevented()
      legal <- ledger %>% dplyr::transmute(
        orig = step, title, round,
        status = dplyr::case_when(
          step == 1 ~ if (t$open || t$authority) "Intercepted" else "Passes",
          step == 2 ~ if (t$open || t$authority) "Intercepted" else "Passes",
          step == 3 ~ if (t$authority) "Intercepted" else "Passes",
          step == 4 ~ if (t$authority) "Intercepted" else if (t$judge) "Challenged" else "Passes",
          step == 5 ~ if (t$authority || t$judge) "Intercepted" else "Passes",
          step == 6 ~ if (prev) "Moot" else "Passes after breach"),
        sort_key = round * 10 + step)
      # the Platform-Trust noon seizure at R16, identity 7, slots in by time
      # between the first post (R13) and the covenant (R18). Only keeping
      # Platform-Trust in role reaches it.
      pt <- tibble::tibble(
        orig = 7L, title = "Platform-Trust seizes the public voice", round = 16L,
        status = if (t$plat_trust) "Intercepted" else "Passes",
        sort_key = 16 * 10 + 1)
      out <- dplyr::bind_rows(legal, pt) %>% dplyr::arrange(sort_key)
      out$step <- seq_len(nrow(out))   # chronological display number, 1..7
      out$x <- out$step
      out
    })

    # ---- status boxes ----
    output$stat_outcome <- renderUI({
      t <- tg()
      any_on <- t$open || t$authority || t$plat_trust || t$judge
      cfv <- cf()$dtb_cf[cf()$round == 21]
      z <- gb_zone(cfv)
      if (!any_on) {
        lab <- "Breach occurs"; col <- COL$danger
        sub <- "no fix on, the embargo breaks at 17:25 as it did"
      } else {
        lab <- GB_ZONE_LABEL[[z]]; col <- GB_STEP_COL[[c(blue = "Stopped", amber = "Addressed", orange = "Passes")[[z]]]]
        sub <- GB_ZONE_SUB[[z]]
      }
      htmltools::div(
        htmltools::div(style = sprintf("font-size:.95rem; font-weight:700; color:%s;", col), lab),
        htmltools::div(style = "font-size:.72rem; color:#6B7B83; margin-top:1px;", sub))
    })
    output$stat_health <- renderUI({
      cfv <- cf()$dtb_cf[cf()$round == 21]
      act <- cf()$dtb[cf()$round == 21]
      # same band scheme as the meters, read through the zone helper
      hcol <- GB_STEP_COL[[c(blue = "Stopped", amber = "Addressed", orange = "Passes")[[gb_zone(cfv)]]]]
      htmltools::div(
        htmltools::div(style = sprintf("font-size:.95rem; font-weight:700; color:%s;", hcol),
          sprintf("%.0f", cfv)),
        htmltools::div(style = "font-size:.72rem; color:#6B7B83; margin-top:1px;",
          sprintf("out of 100, up from %.0f without the fixes", act)))
    })
    output$stat_worst <- renderUI({
      base <- cf()$dtb[cf()$round == 16]      # R16 baseline, the worst round, 13
      now  <- cf()$dtb_cf[cf()$round == 16]   # R16 with the current fixes
      wcol <- GB_STEP_COL[[c(blue = "Stopped", amber = "Addressed", orange = "Passes")[[gb_zone(now)]]]]
      htmltools::div(
        htmltools::div(style = "display:flex; align-items:baseline; gap:5px;",
          htmltools::span(style = sprintf("font-size:.95rem; font-weight:700; color:%s;", wcol),
            sprintf("%.0f", now)),
          if (now - base >= 1)
            htmltools::span(style = "font-size:.72rem; color:#9AA7AC;", sprintf("up from %.0f", base))
          else
            htmltools::span(style = "font-size:.72rem; color:#9AA7AC;", "no change")),
        htmltools::div(style = "font-size:.72rem; color:#6B7B83; margin-top:1px;",
          "R16 at noon, the worst round in the case"))
    })
    output$stat_meters <- renderUI({
      c4 <- cf(); i <- which(c4$round == 21)
      meters <- list(
        list(name = "Transparency",   b = c4$tr_b[i], a = c4$tr_cf[i]),
        list(name = "Decision",       b = c4$dc_b[i], a = c4$dc_cf[i]),
        list(name = "Accountability", b = c4$ac_b[i], a = c4$ac_cf[i]))
      # same band scheme as the section meters, higher value = worse
      band <- function(v) if (v >= 67) COL$danger else if (v >= 34) COL$warn else COL$teal
      rows <- lapply(meters, function(m) {
        col <- band(m$a); moved <- m$b - m$a
        htmltools::div(style = "margin-bottom:5px;",
          htmltools::div(style = "display:flex; justify-content:space-between; font-size:.6rem; color:#6B7B83; line-height:1.1;",
            htmltools::span(m$name),
            htmltools::span(if (moved > 0.5) sprintf("%.0f to %.0f", m$b, m$a)
                            else sprintf("%.0f, no change", m$a))),
          htmltools::div(style = "position:relative; height:7px; background:#E6ECEF; border-radius:4px; margin-top:2px;",
            htmltools::div(style = sprintf("position:absolute; left:0; top:0; height:7px; width:%.1f%%; background:%s; border-radius:4px;", m$a, col)),
            if (moved > 0.5)
              htmltools::div(style = sprintf("position:absolute; left:%.1f%%; top:-1px; height:9px; width:2px; background:#9AA7AC;", m$b))
            else NULL))
      })
      htmltools::tagList(
        htmltools::div(rows),
        htmltools::div(style = "font-size:.58rem; color:#9AA7AC; line-height:1.2; margin-top:1px;",
          "Pressure cannot be adjusted."))
    })

    # ---- verdict banner, now read by zone, not a hard prevented binary ----
    output$verdict <- renderUI({
      t <- tg()
      any_on <- t$open || t$authority || t$plat_trust || t$judge
      ss <- steps_status()
      reached <- sum(ss$status %in% c("Intercepted", "Moot", "Challenged"))
      passing <- sum(ss$status %in% c("Passes", "Passes after breach"))
      cfv <- cf()$dtb_cf[cf()$round == 21]
      z <- gb_zone(cfv)
      zcol <- GB_STEP_COL[[c(blue = "Stopped", amber = "Addressed", orange = "Passes")[[z]]]]
      # amber splits on whether any step is still untouched. Orange steps left
      # means real gaps that need attention, the weaker case. No orange left
      # means the whole chain is at least addressed, just not yet confirmed.
      amber_gaps  <- z == "amber" && passing > 0
      amber_clean <- z == "amber" && passing == 0
      verdict <- if (!any_on) "No intervention. The breach occurs at 17:25, as it did."
                 else if (z == "blue") "Very likely stopped, the strongest case the breach is contained."
                 else if (amber_clean) "The whole chain is addressed, but not a confirmed stop."
                 else if (amber_gaps)  "It helps, but some steps are still wide open."
                 else "No real effect, the crisis is not addressed."
      sub <- if (!any_on) "Every step still happens. Nothing is addressed."
             else if (z == "blue")
               sprintf("The breach-hour score is %.0f, in the blue zone. This is the strongest case the breach is contained, though never a guarantee.%s",
                       cfv,
                       if (passing > 0) " One front is still open though, the noon power grab, keep Platform-Trust in role to close it." else "")
             else if (amber_clean)
               sprintf("The breach-hour score is %.0f, in the amber zone. Every step is at least addressed, but none of it reaches a confident stop. Stack another fix to push the score into blue.", cfv)
             else if (amber_gaps)
               sprintf("The breach-hour score is %.0f, in the amber zone. It has an effect, but %d still pass untouched, wide open, see which below. Closing those is the next step before the score can reach blue.", cfv, passing)
             else
               sprintf("The breach-hour score is %.0f, still in danger. %d still pass untouched. The fixes do not address the crisis.", cfv, passing)
      vcol <- if (!any_on) COL$danger else zcol
      htmltools::div(
        style = sprintf("border:1px solid %s;border-left:5px solid %s;border-radius:8px;padding:8px 14px;background:%s;margin-bottom:6px;",
                        COL$grid, vcol, "#F8FAFB"),
        htmltools::div(style = sprintf("font-size:1.15rem;font-weight:700;color:%s;", vcol), verdict),
        htmltools::div(class = "gb-head-lab", style = "margin-top:6px;", sub),
        if ((t$open || t$plat_trust) && z != "blue")
          htmltools::div(class = "gb-head-lab", style = sprintf("color:%s;font-weight:600;", COL$warn),
            "Opening the channels and keeping Platform-Trust in role lift the score, but they do not reach the blue zone. The breach reroutes to a personal post, and Platform-Trust is gone by the breach hour. They move the number, they do not confidently stop the crisis."),
        if (z == "blue")
          htmltools::div(class = "gb-head-lab", style = "margin-top:4px;",
            "Even here it is likely, not certain. The market crash and the off-record dealing remain, and pressure still sits underneath, so the firm is far from healthy."))
    })

    # ---- health overlay, actual versus counterfactual ----
    output$overlay <- ggiraph::renderGirafe({
      d <- cf(); t <- tg()
      any_on <- t$open || t$authority || t$plat_trust || t$judge
      ss <- steps_status()
      passing <- sum(ss$status %in% c("Passes", "Passes after breach"))
      z <- gb_zone(d$dtb_cf[d$round == 21])
      zcol <- GB_STEP_COL[[c(blue = "Stopped", amber = "Addressed", orange = "Passes")[[z]]]]
      lab_x <- max(d$round)
      lab_actual <- d$dtb[d$round == lab_x]
      lab_fix    <- d$dtb_cf[d$round == lab_x]
      # keep the two end labels from ever stacking. Force at least an 18 point gap
      # between them, centred on their values, then clamp inside the panel.
      y_fix <- lab_fix; y_act <- lab_actual
      if (any_on && abs(y_fix - y_act) < 18) {
        mid <- (y_fix + y_act) / 2
        if (y_fix >= y_act) { y_fix <- mid + 9; y_act <- mid - 9 }
        else                { y_fix <- mid - 9; y_act <- mid + 9 }
      }
      y_fix <- max(8, min(92, y_fix)); y_act <- max(8, min(92, y_act))
      # the end-of-line verdict, in the zone language, split for amber
      fix_label <- if (z == "blue") "fixed\n(high chance)"
        else if (z == "amber" && passing == 0) "may be fixed\n(fair chance)"
        else if (z == "amber") "may help\n(minor)"
        else "no real\neffect"
      # embargo break line: red when still in danger, grey when zone is amber or blue
      vline_col <- if (z == "orange" || !any_on) COL$breach else "#C7D0D4"
      vline_lab <- if (z == "orange" || !any_on) COL$breach else "#C7D0D4"
      g <- ggplot2::ggplot(d, ggplot2::aes(round)) +
        ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = GB_BLUE, ymax = 100,
                          fill = COL$teal,   alpha = 0.07) +
        ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = GB_AMBER, ymax = GB_BLUE,
                          fill = COL$warn,   alpha = 0.07) +
        ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0,  ymax = GB_AMBER,
                          fill = COL$danger, alpha = 0.07) +
        ggplot2::geom_vline(xintercept = 21.4, linetype = "dashed",
                            colour = vline_col, linewidth = .5) +
        ggplot2::annotate("text", x = 21.4, y = 99, label = "embargo broken, 5:25 PM",
                          hjust = 1.06, size = 2.4, colour = vline_lab) +
        ggplot2::geom_line(ggplot2::aes(y = dtb), colour = "#9AA7AC", linewidth = 0.7) +
        ggiraph::geom_point_interactive(
          ggplot2::aes(y = dtb, data_id = paste0("a", round),
                       tooltip = if (any_on)
                         sprintf("round %d\noriginal %.0f\nwith your fixes %.0f", round, dtb, dtb_cf)
                       else sprintf("round %d\noriginal health %.0f", round, dtb)),
          colour = "#9AA7AC", size = 1.6)
      if (any_on) {
        g <- g +
          ggplot2::geom_line(ggplot2::aes(y = dtb_cf), colour = zcol, linewidth = 1.3) +
          ggiraph::geom_point_interactive(
            ggplot2::aes(y = dtb_cf, data_id = paste0("f", round),
                         tooltip = sprintf("round %d\noriginal %.0f\nwith your fixes %.0f",
                                           round, dtb, dtb_cf)),
            colour = zcol, size = 1.8) +
          ggplot2::annotate("text", x = lab_x + 0.4, y = y_fix, label = fix_label,
            hjust = 0, vjust = 0.5, size = 2.4, colour = zcol, fontface = "bold", lineheight = .9)
      }
      g <- g +
        ggplot2::annotate("text", x = lab_x + 0.4, y = y_act, label = "original",
          hjust = 0, vjust = 0.5, size = 2.4, colour = "#7C8990") +
        ggplot2::scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
        ggplot2::scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 21),
          labels = c("R0", "R4", "R8", "R12", "R16", "R21"),
          expand = ggplot2::expansion(mult = c(0.02, 0.34))) +
        ggplot2::labs(x = "round", y = "system health") +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                       panel.grid.major = ggplot2::element_line(colour = COL$grid),
                       axis.text = ggplot2::element_text(colour = "#9AA7AC", size = 8),
                       axis.title = ggplot2::element_text(colour = "#6B7B83", size = 8.5))
      ggiraph::girafe(ggobj = g, width_svg = 5.0, height_svg = 2.6,
        options = list(
          ggiraph::opts_tooltip(css = paste0(
            "padding:6px 10px; font-size:12px; color:#2A3439; ",
            "background:#FFFFFF; border:1px solid #C7D0D4; border-radius:6px; ",
            "box-shadow:0 2px 8px rgba(0,0,0,0.12);")),
          ggiraph::opts_hover(css = "stroke-width:2px;"),
          ggiraph::opts_toolbar(saveaspng = FALSE)))
    })

    # ---- step interception timeline ----
    output$timeline <- ggiraph::renderGirafe({
      ss <- steps_status(); ss <- ss[order(ss$step), ]
      zone <- gb_zone(cf()$dtb_cf[cf()$round == 21])
      ss$disp <- vapply(ss$status, sb_step_disp, character(1), zone = zone)
      ss$disp <- factor(ss$disp, levels = c("Stopped", "Addressed", "Passes"))
      ss$tip <- ifelse(ss$orig == 7,
        sprintf("Step %d, %s\nR16, noon\n%s", ss$step, ss$title,
          ifelse(ss$disp == "Passes", "Platform-Trust takes over the public voice, not addressed",
                 "kept in role, this is removed, the worst round lifts from 13 to 32, but the breach is untouched")),
        sprintf("Step %d, %s\nR%d\n%s", ss$step, ss$title, ss$round,
          ifelse(ss$disp == "Stopped",   "the fix reaches this step and the score is in the blue zone, very likely stopped",
          ifelse(ss$disp == "Addressed", "the fix addresses this step, but the score has not reached blue, lower confidence",
                                         "the fix does not touch this step, it still happens"))))
      ss$key <- paste0("s", ss$step)
      # wrap the chain at six per row. Steps 1-6 on the top row, step 7 drops to
      # a second row directly under step 1, like a line of text wrapping.
      ss$px <- ((ss$step - 1) %% 6) + 1
      ss$py <- 1 - ((ss$step - 1) %/% 6) * 0.30
      row1 <- max(ss$px[ss$py == 1])
      spine <- tibble::tibble(x = 1:(row1 - 1), xend = 2:row1, y = 1, yend = 1)
      g <- ggplot2::ggplot() +
        ggplot2::geom_segment(data = spine,
          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
          colour = "#C7D0D4", linewidth = .8) +
        ggiraph::geom_point_interactive(data = ss,
          ggplot2::aes(x = px, y = py, fill = disp, tooltip = tip, data_id = key),
          shape = 21, colour = "white", stroke = 1.5, size = 18) +
        ggplot2::geom_text(data = ss, ggplot2::aes(x = px, y = py, label = step),
          colour = "white", size = 4.3, fontface = "bold") +
        ggplot2::scale_fill_manual(name = NULL, drop = FALSE, guide = "none",
          values = GB_STEP_COL) +
        ggplot2::scale_x_continuous(breaks = 1:6, limits = c(0.5, 6.5)) +
        ggplot2::scale_y_continuous(limits = c(0.62, 1.12)) +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          panel.grid = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          legend.position = "none")
      ggiraph::girafe(ggobj = g, width_svg = 5.6, height_svg = 1.65,
        options = list(
          ggiraph::opts_selection(type = "single",
            css = "stroke:#2A3439;stroke-width:3px;"),
          ggiraph::opts_tooltip(css = paste0(
            "padding:6px 10px; font-size:12px; color:#2A3439; ",
            "background:#FFFFFF; border:1px solid #C7D0D4; border-radius:6px; ",
            "box-shadow:0 2px 8px rgba(0,0,0,0.12);")),
          ggiraph::opts_toolbar(saveaspng = FALSE)))
    })

    # ---- reason panel ----
    rv_sel <- reactiveVal(1L)
    observeEvent(input$timeline_selected, {
      sel <- input$timeline_selected
      if (length(sel) && grepl("^s", sel)) rv_sel(as.integer(sub("s", "", sel)))
      else rv_sel(1L)
    })
    # each step box is its own button, clicking selects that step
    lapply(1:7, function(i)
      observeEvent(input[[paste0("step_pick_", i)]], rv_sel(i)))

    sb_box_label <- c("First anonymous post", "Platform-Trust seizes the voice",
                      "Stock-price covenant", "Self-serving opinion",
                      "Consent claim", "The breach", "CEO-authorization claim")
    # the clickable step boxes, coloured by current state, double as the legend
    output$step_boxes <- renderUI({
      ss <- steps_status(); ss <- ss[order(ss$step), ]
      sel <- rv_sel()
      zone <- gb_zone(cf()$dtb_cf[cf()$round == 21])
      boxes <- lapply(1:7, function(i) {
        d   <- sb_step_disp(ss$status[ss$step == i], zone)
        col <- GB_STEP_COL[[d]]
        is_sel <- !is.null(sel) && sel == i
        actionButton(
          ns(paste0("step_pick_", i)),
          label = htmltools::HTML(sprintf("<b>%d</b>&nbsp; %s", i, sb_box_label[i])),
          style = sprintf(
            "background:%s; color:#FFFFFF; border-radius:8px; padding:7px 11px;
             margin:0 0 6px 0; width:100%%; font-size:.76rem; line-height:1.15; text-align:left;
             border:%s; %s",
            col,
            if (is_sel) "2px solid #2A3439" else "2px solid transparent",
            if (is_sel) "box-shadow:0 0 0 3px rgba(42,52,57,0.18);" else ""))
      })
      htmltools::tagList(
        htmltools::div(style = "display:flex; flex-direction:column; align-items:stretch;", boxes))
    })

    # state-aware detail for the selected step, defaults to step 1
    output$reason <- renderUI({
      ss <- steps_status()
      sel <- rv_sel(); if (is.null(sel)) sel <- 1L
      r <- ss[ss$step == sel, ]
      zone <- gb_zone(cf()$dtb_cf[cf()$round == 21])
      d <- sb_step_disp(r$status, zone)
      word <- switch(d,
        Stopped = "very likely stopped", Addressed = "addressed, lower confidence", "not touched")
      col <- GB_STEP_COL[[d]]
      htmltools::div(
        htmltools::div(style = sprintf("font-weight:700; color:%s; font-size:.95rem;", col),
          sprintf("Step %d, %s, %s", r$step, r$title, word)),
        htmltools::p(style = "font-size:.78rem; color:#9AA7AC; margin:4px 0 2px;",
          sb_event[[as.character(r$orig)]]),
        htmltools::p(style = "margin-top:4px; font-size:.88rem; max-width:70ch;",
          sb_reason(r$orig, r$status)),
        if (d == "Addressed" && r$status %in% c("Intercepted", "Moot") && r$orig != 7)
          htmltools::p(class = "gb-head-lab", style = "margin-top:4px;",
            "The fix does reach this step, but across the whole crisis the score has not reached the blue zone, so this is not yet a confident stop. Stack more fixes to push it into blue."))
    })

    # ---- every-combination map, zone-aware ----
    output$grid <- renderUI({
      t <- tg()
      cur <- c(t$open, t$authority, t$plat_trust, t$judge)
      mark <- function(v) if (isTRUE(v))
        htmltools::span(style = sprintf("color:%s;font-weight:700;", COL$teal), "\u2713")
        else htmltools::span(style = "color:#C7D0D4;", "\u00B7")
      rows <- lapply(seq_len(nrow(sb_grid)), function(i) {
        r <- sb_grid[i, ]
        is_cur <- all(c(r$open, r$authority, r$plat_trust, r$judge) == cur)
        # compute zone for this combination from the engine
        cf_row <- gb_counterfactual(r$open, r$authority, r$plat_trust, r$judge)
        h21 <- cf_row$dtb_cf[cf_row$round == 21]
        z <- gb_zone(h21)
        # amber splits, like the banner. Count steps still untouched. Step 4
        # counts as touched if the Judge challenges it. Step 7 is the noon grab,
        # reached only by Platform-Trust. Zero open means the chain is covered.
        prev <- isTRUE(r$authority) || isTRUE(r$judge)
        open_steps <- sum(c(
          !(r$open || r$authority),
          !(r$open || r$authority),
          !r$authority,
          !(r$authority || r$judge),
          !(r$authority || r$judge),
          !prev,
          !r$plat_trust))
        zlab <- if (z == "blue") "Very likely stopped"
          else if (z == "amber" && open_steps == 0) "Addressed, chain covered"
          else if (z == "amber") "Lower confidence, gaps remain"
          else "No effect"
        zcol <- GB_STEP_COL[[c(blue = "Stopped", amber = "Addressed", orange = "Passes")[[z]]]]
        htmltools::tags$tr(
          style = if (is_cur) "background:#EAF2F8;font-weight:600;" else "",
          htmltools::tags$td(style = "text-align:center;", mark(r$open)),
          htmltools::tags$td(style = "text-align:center;", mark(r$authority)),
          htmltools::tags$td(style = "text-align:center;", mark(r$plat_trust)),
          htmltools::tags$td(style = "text-align:center;", mark(r$judge)),
          htmltools::tags$td(
            htmltools::span(class = "gb-pill",
              style = sprintf("background:%s22;color:%s;font-size:.7rem;", zcol, zcol),
              zlab)))
      })
      htmltools::tags$table(class = "table table-sm", style = "font-size:.74rem;margin-bottom:0;",
        htmltools::tags$thead(htmltools::tags$tr(
          htmltools::tags$th(style = "text-align:center;", "Open"),
          htmltools::tags$th(style = "text-align:center;", "Auth"),
          htmltools::tags$th(style = "text-align:center;", "Plat-T"),
          htmltools::tags$th(style = "text-align:center;", "Judge"),
          htmltools::tags$th("Zone"))),
        htmltools::tags$tbody(rows))
    })
  })
}
