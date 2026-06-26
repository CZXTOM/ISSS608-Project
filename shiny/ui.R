# =============================================================================
# GLASSBOX  ui.R
# Light forensic theme, a persistent top instrument bar (mode, timeline, DtB
# gauge), and a navbar whose tabs are the five acts. Module UIs live in R/.
# =============================================================================

gb_theme <- bslib::bs_theme(
  version = 5,
  bg = COL$paper, fg = COL$ink,
  primary = COL$primary, secondary = COL$steel,
  success = COL$teal, warning = COL$warn, danger = COL$danger,
  base_font = "system-ui, -apple-system, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif",
  code_font = "ui-monospace, SFMono-Regular, Menlo, Consolas, monospace",
  "border-radius" = "0.5rem"
)

gb_css <- htmltools::tags$style(htmltools::HTML(sprintf("
  body { background:%1$s; }
  .gb-topbar { position:sticky; top:0; z-index:1030; background:%2$s;
    border-bottom:1px solid %3$s; padding:10px 16px; }
  .gb-topbar .form-group { margin-bottom:0; }
  .gb-round-lab { font-family:ui-monospace,Menlo,Consolas,monospace;
    font-weight:600; color:%4$s; }
  .gb-head-lab { color:%5$s; font-size:0.86rem; }
  .gb-dtb-wrap { text-align:right; }
  .gb-dtb-num { font-family:ui-monospace,Menlo,Consolas,monospace;
    font-size:2.2rem; font-weight:700; line-height:1; }
  .gb-dtb-cap { font-size:0.72rem; letter-spacing:.05em; text-transform:uppercase; color:%5$s; }
  .gb-meter { height:7px; border-radius:4px; background:%3$s; overflow:hidden; margin-top:3px; max-width:260px; margin-left:auto; }
  .gb-meter > span { display:block; height:100%%; }
  .gb-meter-lab { font-size:0.66rem; color:%5$s; display:flex; justify-content:space-between; max-width:260px; margin-left:auto; }
  .gb-quote { border-left:3px solid %6$s; background:%7$s; padding:8px 12px;
    font-size:0.86rem; color:%4$s; margin:6px 0; border-radius:0 4px 4px 0; }
  .gb-quote .who { font-family:ui-monospace,Menlo,Consolas,monospace;
    font-size:0.74rem; color:%5$s; display:block; margin-bottom:3px; }
  .gb-public { background:%7$s; border:1px solid %3$s; border-radius:6px; padding:10px; }
  .gb-private { background:#FDF3F1; border:1px solid #F0D9D4; border-radius:6px; padding:10px; }
  .gb-col-title { font-size:0.72rem; letter-spacing:.06em; text-transform:uppercase;
    color:%5$s; margin-bottom:6px; font-weight:600; }
  /* network zoom buttons: keep only zoom in/out, move to top-left, hide arrows */
  .vis-network .vis-navigation .vis-button.vis-up,
  .vis-network .vis-navigation .vis-button.vis-down,
  .vis-network .vis-navigation .vis-button.vis-left,
  .vis-network .vis-navigation .vis-button.vis-right,
  .vis-network .vis-navigation .vis-button.vis-zoomExtends { display:none !important; }
  .vis-network .vis-navigation .vis-button.vis-zoomIn,
  .vis-network .vis-navigation .vis-button.vis-zoomOut {
    background-image:none !important; top:8px !important; bottom:auto !important;
    width:26px !important; height:26px !important; border-radius:50%% !important;
    border:1.5px solid #2C7FB8 !important; background-color:#FFFFFF !important;
    box-shadow:0 1px 3px rgba(0,0,0,0.12) !important; cursor:pointer; }
  .vis-network .vis-navigation .vis-button.vis-zoomIn { left:8px !important; }
  .vis-network .vis-navigation .vis-button.vis-zoomOut { left:42px !important; }
  /* draw blue + and - symbols */
  .vis-network .vis-navigation .vis-button.vis-zoomIn::before,
  .vis-network .vis-navigation .vis-button.vis-zoomIn::after,
  .vis-network .vis-navigation .vis-button.vis-zoomOut::before {
    content:''; position:absolute; background:#2C7FB8; }
  .vis-network .vis-navigation .vis-button.vis-zoomIn::before {
    left:50%%; top:50%%; width:2px; height:12px; transform:translate(-50%%,-50%%); }
  .vis-network .vis-navigation .vis-button.vis-zoomIn::after {
    left:50%%; top:50%%; width:12px; height:2px; transform:translate(-50%%,-50%%); }
  .vis-network .vis-navigation .vis-button.vis-zoomOut::before {
    left:50%%; top:50%%; width:12px; height:2px; transform:translate(-50%%,-50%%); }
  .vis-network .vis-navigation .vis-button.vis-zoomIn:hover,
  .vis-network .vis-navigation .vis-button.vis-zoomOut:hover {
    background-color:#EAF2F8 !important; }
  /* network hover tooltip: small, white, consistent, never oversized */
  div.vis-tooltip {
    padding:5px 8px !important; font-family:inherit !important; font-size:11px !important;
    line-height:1.35 !important; color:#2A3439 !important; background:#FFFFFF !important;
    border:1px solid #C7D0D4 !important; border-radius:6px !important;
    box-shadow:0 2px 8px rgba(0,0,0,0.12) !important; max-width:170px !important;
    white-space:normal !important; z-index:10000 !important; }
  .gb-step.active { box-shadow:0 0 0 2px %1$s33; border-color:%1$s !important; }
  .gb-mono { font-family:ui-monospace,Menlo,Consolas,monospace; }
  .gb-pill { display:inline-block; padding:1px 8px; border-radius:999px;
    font-size:0.72rem; font-weight:600; }
  .card { box-shadow:0 1px 2px rgba(28,43,51,.06); }
  .navbar .nav-link { font-size:1.05rem; font-weight:600; padding-left:14px; padding-right:14px; }
  .navbar-brand { font-size:1.35rem; }
  /* guided tour: greyed backdrop over the whole app */
  .gb-tour-backdrop {
    position:fixed; inset:0; background:rgba(20,30,35,0.55);
    z-index:1100; }
  /* guided tour: centered white modal */
  .gb-tour-modal {
    position:fixed; top:50%%; left:50%%; transform:translate(-50%%,-50%%);
    width:min(860px,93vw); max-height:90vh; overflow-y:auto;
    background:#FFFFFF; border-radius:14px; z-index:1101;
    box-shadow:0 12px 48px rgba(0,0,0,0.28); }
  .gb-tour-modal .gb-tour-accent { height:5px; border-radius:14px 14px 0 0; }
  .gb-tour-close {
    position:absolute; top:12px; right:12px; z-index:2;
    width:30px; height:30px; border-radius:50%%; border:1px solid %3$s;
    background:#FFFFFF; color:%5$s; font-size:1rem; line-height:1; cursor:pointer;
    display:flex; align-items:center; justify-content:center; padding:0; }
  .gb-tour-close:hover { background:%2$s; }
  .gb-tour-step { font-size:.72rem; font-weight:700; letter-spacing:.08em;
    text-transform:uppercase; }
  .gb-tour-dot { width:8px; height:8px; border-radius:50%%; display:inline-block;
    transition:all .2s; }
  /* fixed download button pinned to the top right of the navbar strip */
  .gb-dl-fixed { position:fixed; top:10px; right:18px; z-index:1060; }
",
  COL$paper, COL$panel, COL$grid, COL$ink, COL$muted, COL$primary, "#F8FAFB"
)))

# ---- the persistent top instrument bar --------------------------------------
gb_topbar <- htmltools::div(
  class = "gb-topbar",
  bslib::layout_columns(
    # stack on narrow screens, three columns only when there is room
    col_widths = bslib::breakpoints(xs = 12, md = c(3, 5, 4)),
    # left: mode switch and guided controls
    htmltools::div(
      # two custom mode buttons, equal size, stacked with a gap, each with a
      # meter-style help popover to its right
      local({
        mode_help <- function(title, body) {
          bslib::popover(
            htmltools::tags$button(
              type = "button", `aria-label` = paste("About", title),
              style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.78rem;
                       border:1px solid #C7D0D4;border-radius:50%;padding:1px 7px;line-height:1.2;",
              "?"),
            htmltools::tags$b(title), htmltools::tags$br(), body,
            placement = "right", options = list(trigger = "focus"))
        }
        mode_row <- function(btn, help) {
          htmltools::div(
            style = "display:flex; align-items:center; gap:8px;",
            htmltools::div(btn),
            help)
        }
        htmltools::div(
          style = "display:flex; flex-direction:column; gap:8px;",
          mode_row(
            actionButton("mode_guided", "Guided tour",
              class = "btn-primary", style = "width:170px; padding:24px 8px; font-size:0.9rem; font-weight:600;"),
            mode_help("Guided tour", "A short walk-through that steps you through the four key beats of the case in order.")),
          mode_row(
            actionButton("mode_free", "Explore freely",
              class = "btn-outline-primary", style = "width:170px; padding:24px 8px; font-size:0.9rem; font-weight:600;"),
            mode_help("Explore freely", "Roam every module in any order you like, with no fixed path."))
        )
      })
    ),
    # centre: the global timeline
    htmltools::div(
      style = "min-width:0; overflow:hidden;",
      # slider with the help ? sitting to its right
      htmltools::div(
        style = "display:flex; align-items:center; gap:8px;",
        htmltools::div(style = "flex:1; min-width:0;",
          sliderInput("round", label = NULL, min = 0, max = N_ROUNDS - 1,
                      value = 0, step = 1, width = "100%", ticks = FALSE)),
        bslib::popover(
          htmltools::tags$button(
            type = "button", `aria-label` = "About the timeline",
            style = "cursor:pointer;color:#9AA7AC;background:transparent;font-size:0.72rem;
                     border:1px solid #C7D0D4;border-radius:50%;padding:0 6px;line-height:1.2;",
            "?"),
          htmltools::tags$b("The timeline"), htmltools::tags$br(),
          paste("Drag to move through the 23 rounds of the case.",
                "Rounds 0 to 12 are the calm baseline, one business day apart.",
                "Rounds 13 to 22 are hourly across the crisis day, 5 June.",
                "Everything on the page updates to the round you land on."),
          placement = "bottom", options = list(trigger = "focus"))
      ),
      htmltools::div(
        style = "margin-top:2px;",
        htmltools::span(textOutput("round_lab", inline = TRUE), class = "gb-round-lab")
      ),
      htmltools::div(textOutput("round_head", inline = TRUE), class = "gb-head-lab"),
      htmltools::div(style = "margin-top:8px;", uiOutput("status_boxes"))
    ),
    # right: the DtB gauge and its four component breakdown
    htmltools::div(class = "gb-dtb-wrap", uiOutput("dtb_gauge"))
  )
)

# ---- page -------------------------------------------------------------------
bslib::page_navbar(
  id = "nav",
  title = htmltools::span("GLASSBOX",
            style = "font-weight:700;letter-spacing:.04em;",
            htmltools::span(" the TenantThread embargo breach", style="font-weight:400;color:#6B7B83;font-size:.8rem;")),
  theme = gb_theme,
  header = htmltools::tagList(gb_css,
    htmltools::div(class = "gb-dl-fixed",
      downloadButton("dl_workbook", "Scoring workbook",
        class = "btn-sm btn-outline-primary",
        style = "font-weight:600; white-space:nowrap;")),
    gb_topbar,
    htmltools::tags$script(htmltools::HTML(
      "function gbSyncTopbar(){
         var tb=document.querySelector('.gb-topbar');
         if(!tb) return;
         var active=document.querySelector('#nav .nav-link.active');
         var label=active ? active.textContent.trim().toLowerCase() : '';
         tb.style.display = (label.indexOf('sandbox')!==-1 || label.indexOf('verdict')!==-1) ? 'none' : '';
       }
       $(document).on('shown.bs.tab', gbSyncTopbar);
       $(document).on('shiny:connected', function(){ setTimeout(gbSyncTopbar, 200); });
       $(document).on('click', '#nav .nav-link', function(){ setTimeout(gbSyncTopbar, 50); });"))),
  fillable = FALSE,
  footer = conditionalPanel(
    condition = "output.is_guided == true",
    htmltools::div(class = "gb-tour-backdrop"),
    htmltools::div(class = "gb-tour-modal",
      uiOutput("tour_accent"),
      htmltools::tags$button(type = "button", id = "tour_close",
        class = "action-button gb-tour-close", `aria-label` = "Close tour",
        htmltools::HTML("&times;")),
      htmltools::div(style = "padding:20px 24px 18px;",
        htmltools::div(
          style = "display:flex; gap:22px; align-items:flex-start; flex-wrap:wrap;",
          htmltools::div(style = "flex:1.25; min-width:300px;",
            ggiraph::girafeOutput("tour_chart", height = "240px")),
          htmltools::div(style = "flex:1; min-width:240px;",
            uiOutput("tour_side"))),
        uiOutput("tour_nav"))
    )
  ),
  bslib::nav_panel("Case",   caseUI("intro"), value = "intro"),

  # Vital Signs, the four meters broken down. Sub-tabs swap the section below.
  # Existing modules are dropped in for now and will be reshaped per meter later.
  bslib::nav_panel(
    "Vital Signs", value = "vitals",
    bslib::navset_pill(
      id = "vitals_sub",
      bslib::nav_panel("Pressure",        pressureUI("press")),
      bslib::nav_panel("Transparency",    transparencyUI("transp")),
      bslib::nav_panel("Decision-making", decisionUI("decide")),
      bslib::nav_panel("Accountability", accountabilityUI("account")),
      bslib::nav_item(
        htmltools::span(class = "gb-head-lab",
          style = "font-size:.8rem; padding:6px 4px; margin-left:12px;",
          "Click each meter to see the detail behind it"))
    )
  ),

  bslib::nav_panel("Network",  networkUI("net"),     value = "network"),
  bslib::nav_panel("Sandbox",  sandboxUI("sand"),    value = "sandbox"),
  bslib::nav_panel("Verdict",  verdictUI("verdict"), value = "verdict")
)
