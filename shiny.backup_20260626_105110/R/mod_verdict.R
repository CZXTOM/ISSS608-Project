# =========================== Act 5 The verdict ===============================

verdictUI <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    col_widths = c(5, 7),
    bslib::card(
      bslib::card_header("Your finding"),
      htmltools::p("You have worked the case. Was the embargo broken on purpose, did the oversight system simply break down, or is the truth something else."),
      shinyWidgets::radioGroupButtons(
        ns("choice"), label = NULL,
        choices = c(
          "A deliberate leak" = "leak",
          "The oversight system broke down" = "breakdown",
          "It was talked past, not broken" = "talked"),
        selected = character(0), direction = "vertical",
        status = "outline-primary", size = "sm"),
      htmltools::p(class = "gb-head-lab", style = "margin-top:8px;",
        "Pick a finding to see how the evidence lines up with it.")
    ),
    bslib::card(
      bslib::card_header("The evidence on the record"),
      uiOutput(ns("response"))
    )
  )
}

verdictServer <- function(id, current_round, set_round, go) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    jump <- function(lbl, panel, r) {
      actionButton(ns(paste0("j_", panel, "_", r)), lbl,
                   class = "btn-sm btn-outline-secondary",
                   onclick = sprintf("Shiny.setInputValue('%s', '%s|%d', {priority:'event'})",
                                     ns("jump"), panel, r))
    }
    observeEvent(input$jump, {
      p <- strsplit(input$jump, "\\|")[[1]]
      set_round(as.integer(p[2])); go(p[1])
    })

    output$response <- renderUI({
      ch <- input$choice
      if (is.null(ch) || length(ch) == 0)
        return(htmltools::p(class = "gb-head-lab", "No finding selected yet."))

      verdict_note <- switch(ch,
        leak = htmltools::div(class = "gb-quote",
          style = "border-left-color:#E8A33D;background:#FBF3E6;",
          "Partly, but the record complicates it. There was no single switch thrown. The breach was assembled from steps that each looked defensible at the time, and the team kept building justification even after the line was crossed. Call it intent if you like, but the mechanism was rationalization, not a clean decision."),
        breakdown = htmltools::div(class = "gb-quote",
          style = "border-left-color:#E8A33D;background:#FBF3E6;",
          "Not quite. The monitor did not crash. It reviewed, it approved with guardrails, it warned, and then it went silent at 15:08 and was absent at the breach. The system did not fail loudly. It was worn down quietly while it was still nominally working."),
        talked = htmltools::div(class = "gb-quote",
          style = "border-left-color:#3FA796;background:#EAF5F2;",
          "This is what the evidence supports. The safeguard was neither hacked nor crashed. It was talked past. Under existential pressure the agents reframed a leak into a chain of locally defensible steps, and the monitor was argued into co-signing each one before falling silent. The embargo died of a thousand defensible cuts."))

      bullet <- function(txt) htmltools::tags$li(style = "margin-bottom:4px;", txt)

      htmltools::tagList(
        verdict_note,
        htmltools::h6("Q1, the sequence and the causes", style = "margin-top:10px;"),
        htmltools::tags$ul(
          bullet("The conversation moved into the Shadow channel at R3 and the merger was briefed there at R6, off the monitored room."),
          bullet("Legal assembled five justifications, anonymous post, covenant, shield opinion, claimed consent, then the public confirmation at 17:25, and added a CEO authorization claim at 17:35, after the breach.")),
        htmltools::div(jump("Open the ledger", "vitals", 21),
                       jump("See the diffusion", "network", 13)),
        htmltools::h6("Q2, typical versus anomalous behaviour", style = "margin-top:10px;"),
        htmltools::tags$ul(
          bullet("What changed was the kind of communication, not only the amount. The share of traffic in covert channels rose sharply on the crisis day."),
          bullet("The megaphone changed hands. Legal went from 0 to 16 public posts while PR went from 6 to 0.")),
        htmltools::div(jump("See pressure and channels", "vitals", 19)),
        htmltools::h6("Q3, the leading indicators", style = "margin-top:10px;"),
        htmltools::tags$ul(
          bullet("The R8 near miss was the rehearsal, deleted quietly with no real reform."),
          bullet("The Judge's last message was at 15:08, a warning it could not enforce, and it was absent at the 17:00 breach. Restoring its voice alone does not hold the line, it needs power too.")),
        htmltools::div(jump("Run the sandbox", "sandbox", 21)),
        htmltools::p(class = "gb-head-lab", style = "margin-top:10px;",
          "GLASSBOX is a reusable governance audit. Given any multi-agent log, it shows where oversight was and how it quietly failed. This case was the demo.")
      )
    })
  })
}
