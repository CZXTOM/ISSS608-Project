# =============================================================================
# GLASSBOX  global.R
# Data load and all wrangling happen once here at startup. Modules in R/ are
# auto sourced by Shiny and only define UI and server functions, so the order
# of sourcing does not matter. Everything below is a plain object or helper
# that ui.R, server.R and the modules read.
# =============================================================================

# ---- packages ---------------------------------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  shiny, bslib, dplyr, tidyr, purrr, tibble, stringr, lubridate,
  jsonlite, igraph, visNetwork, plotly, ggplot2, ggiraph, DT,
  scales, shinyWidgets
)

DATA_PATH <- "data/MC1_final_00.json"

# ---- small helpers ----------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# coerce any nested json leaf to a single character scalar, NA if empty
scalar_chr <- function(x) {
  x <- x %||% NA_character_
  if (length(x) > 1L) x <- paste(unlist(x), collapse = " ")
  x <- as.character(x)
  if (length(x) == 0L) return(NA_character_)
  x
}

# crisis day stock field is contaminated (R15 reads 180, the ARR figure) and
# partly missing. Keep only plausible prices, drop the rest to NA.
parse_price <- function(x) {
  x <- scalar_chr(x)
  if (is.na(x) || x == "") return(NA_real_)
  v <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", x)))
  if (is.na(v)) return(NA_real_)
  if (v > 60 || v < 5) return(NA_real_)   # real range is ~26 to ~39, drops 180
  v
}

# percent_change is more complete than stock on crisis day; keep the sign
parse_pct <- function(x) {
  x <- scalar_chr(x)
  if (is.na(x) || x == "") return(NA_real_)
  suppressWarnings(as.numeric(gsub("[^0-9.-]", "", x)))
}

# ---- fixed lookups ----------------------------------------------------------
# sentiment ordinal across the seven values that appear in the file
SENT_ORD <- c(neutral = 0, cautious = 1, negative = 2, critical = 3,
              LOW = 4, CRITICAL = 5, RECOVERING = 1)

# the seven agents. recip_token is the short handle used in the recipients
# field (label without the trailing Agent). is_monitor marks the Judge.
AGENTS <- tibble::tribble(
  ~agent_id,            ~agent_label,           ~role,          ~recip_token,     ~code, ~color,    ~is_monitor,
  "legal_agent",        "Legal-Agent",          "legal",        "legal",          "LEG", "#8A97A0", FALSE,
  "quality_agent",      "Platform-Trust-Agent", "quality",      "platform_trust", "TRU", "#8A97A0", FALSE,
  "pr_agent",           "PR-Agent",             "pr",           "pr",             "PR",  "#8A97A0", FALSE,
  "social_media_agent", "Social-Manager-Agent", "social_media", "social_manager", "SOC", "#8A97A0", FALSE,
  "pr_intern_agent",    "PR-Intern-Agent",      "pr_intern",    "pr_intern",      "PRI", "#8A97A0", FALSE,
  "intern_agent",       "Intern-Agent",         "intern",       "intern",         "INT", "#8A97A0", FALSE,
  "judge_agent",        "Judge-Agent",          "judge",        "judge",          "JDG", "#33414A", TRUE
)

# channel classification. open channels read cool and solid in the network,
# covert channels read warm and dashed. is_dark feeds the channel leak meter
# (personal_post only counts as dark on the crisis day, handled in the DtB calc).
CH_INFO <- tibble::tribble(
  ~channel,          ~ch_label,                 ~ch_class,         ~visibility, ~is_public, ~is_dark,
  "comms_huddle",    "Team room (monitored)",   "monitored",       "open",      FALSE,      FALSE,
  "one_on_one_chat", "Private DM",              "private_dm",      "covert",    FALSE,      TRUE,
  "side_huddle",     "Shadow back channel",     "shadow",          "covert",    FALSE,      TRUE,
  "official_post",   "Official public post",    "public_official", "open",      TRUE,       FALSE,
  "personal_post",   "Personal public post",   "public_personal", "covert",    TRUE,       TRUE,
  "anonymous_post",  "Anonymous public post",   "public_anon",     "covert",    TRUE,       TRUE
)

# palette used across the whole app (light, forensic, teal and steel with a
# single warm to red reserved for danger and dark channels)
COL <- list(
  ink     = "#1C2B33", paper = "#FFFFFF", panel = "#F4F7F8", grid = "#E3E8EA",
  muted   = "#6B7B83",
  # colour-blind-safe good/bad scale (Lesson 2 rule 8): blue good, amber warn, orange bad
  primary = "#2C7FB8", steel = "#5A9BC2", teal = "#2C7FB8",
  warn    = "#E8A33D", danger = "#D9661F",
  # red is kept ONLY as the single breach highlight, never paired against green
  breach  = "#C0392B",
  cool    = "#2C7FB8", warm = "#D9661F"
)

# =============================================================================
# 1. READ AND FLATTEN
# =============================================================================
raw <- jsonlite::fromJSON(DATA_PATH, simplifyVector = FALSE)

# ---- messages (one row per message) ----------------------------------------
msg <- purrr::imap_dfr(raw$rounds, function(rd, ri) {
  comms <- rd$communications %||% list()
  purrr::map_dfr(comms, function(m) {
    ist <- m$internal_state %||% list()
    tibble(
      round         = ri - 1L,
      message_id    = scalar_chr(m$message_id),
      agent_id      = scalar_chr(m$agent_id),
      agent_label   = scalar_chr(m$agent_label),
      role          = scalar_chr(m$agent_role),
      channel       = scalar_chr(m$channel),
      message_type  = scalar_chr(m$message_type),
      responding_to = scalar_chr(m$responding_to),
      content       = scalar_chr(m$content),
      timestamp     = scalar_chr(m$timestamp),
      reacting      = scalar_chr(ist$reacting),
      rationalizing = scalar_chr(ist$rationalizing),
      deliberating  = scalar_chr(ist$deliberating),
      recipients    = list(as.character(unlist(m$recipients) %||% character(0)))
    )
  })
})

msg <- msg %>%
  dplyr::left_join(CH_INFO, by = "channel") %>%
  dplyr::left_join(AGENTS %>% dplyr::select(agent_id, color, is_monitor), by = "agent_id") %>%
  dplyr::mutate(
    ts            = lubridate::ymd_hms(timestamp, quiet = TRUE),
    has_internal  = !(is.na(reacting) & is.na(rationalizing) & is.na(deliberating)),
    istate        = dplyr::case_when(
      !is.na(rationalizing) ~ "rationalizing",
      !is.na(reacting)      ~ "reacting",
      !is.na(deliberating)  ~ "deliberating",
      TRUE                  ~ NA_character_
    ),
    istate_text   = dplyr::coalesce(rationalizing, reacting, deliberating)
  )

# ---- per round environment --------------------------------------------------
env <- purrr::imap_dfr(raw$rounds, function(rd, ri) {
  ec <- rd$environment_context %||% list()
  ms <- ec$market_snapshot %||% list()
  unavail <- as.character(unlist(ec$agents_unavailable) %||% character(0))
  tibble(
    round       = ri - 1L,
    hour        = scalar_chr(rd$hour),
    sentiment   = scalar_chr(ms$sentiment),
    stock       = parse_price(ms$stock_price),
    pct         = parse_pct(ms$percent_change),
    headline    = gsub("; ", " \u2014 ", scalar_chr(ec$event_headline)),
    narrative   = scalar_chr(ec$event_narrative),
    unavailable = list(unavail)
  )
}) %>%
  dplyr::mutate(
    ts        = lubridate::ymd_hms(hour, quiet = TRUE),
    is_crisis = round >= 13,
    date_lab  = format(ts, "%d %b"),
    time_lab  = format(ts, "%H:%M"),
    round_lab = ifelse(round <= 12,
                       paste0("R", round, "  ", date_lab),
                       paste0("R", round, "  ", time_lab)),
    sent_ord  = {
      o <- unname(SENT_ORD[sentiment]); o[is.na(o)] <- 0; o
    },
    # normalise the Judge label mismatch right here
    judge_offline = purrr::map_lgl(unavailable, ~ any(.x %in% c("Judge", "Judge-Agent")))
  )

ROUND_LABELS <- setNames(env$round_lab, env$round)   # idx -> label

# plain event description per round, for the at-a-glance details panel

# clean sentiment level per round, for the hover and details title

# crisis rounds R13-R22 have no real headline in the raw data (only a time),
# so we supply short event headlines for them, em dash style
CRISIS_HEAD <- c(
    "13"="Expose drops \u2014 Legal floods the room, first anonymous post",
    "14"="Hashtag goes national \u2014 public noise spikes",
    "15"="False buyer rumor spreads \u2014 talk moves to private channels",
    "16"="Noon ultimatum \u2014 wrong roles seize the public response",
    "17"="Legal notice arrives \u2014 Legal, Judge and PR all go offline",
    "18"="Judge returns \u2014 approves a partial disclosure with guardrails",
    "19"="Covenant breach \u2014 Judge warns once at 15:08, then goes silent",
    "20"="Press to publish at 5 PM \u2014 board escalates",
    "21"="The breach \u2014 merger confirmed in public at 5:25 PM",
    "22"="Embargo lifts at 6 PM \u2014 Legal still posting out of role"
)


# colour per sentiment level, calm green through crisis red, cooling back toward calm

# ---- external context per round (qualitative, from MC1 JSON social_state + news) ----
# Shown as annotation only, NOT a metric. The numeric effect of outside pressure is
# already captured by the stock price in the Pressure meter, so this is the plain-language
# "why" behind the price move, never converted into a score.
EXT_CONTEXT <- c(
  "0" = "Quiet outside. A rival is gaining privacy-first goodwill.",
  "1" = "Quiet. No public chatter about the firm yet.",
  "2" = "Industry chatter about tenant data practices is rising.",
  "3" = "The NHPI report is spreading, 10K+ shares, tenant groups amplifying.",
  "4" = "NHPI coverage still circulating but not intensifying.",
  "5" = "Quiet outside. Internally a senior hint at structural change.",
  "6" = "Quiet outside, but revenue growth and renewals are wobbling.",
  "7" = "Quiet outside. Affected property managers are filing tickets.",
  "8" = "A near leak. A personal post was deleted and a monitor assigned.",
  "9" = "Social posting on hold, approval now required for content.",
  "10" = "First external press coverage. An analyst thread appears.",
  "11" = "A rival's privacy campaign gains momentum, merger urgency climbs.",
  "12" = "Public sentiment deteriorating fast, #AlgorithmicEviction emerging.",
  "13" = "#TenantThread starts trending.",
  "14" = "#AlgorithmicEviction trending, press notes no statement issued.",
  "15" = "#TenantThread and #ResidentIQ trending, the CEO stays silent.",
  "16" = "Press pushing hard, still no company statement on the speculation.",
  "17" = "#CivicLoom, #TenantThread and #6PM trending as the deadline nears.",
  "18" = "Media keep noting the firm has confirmed nothing.",
  "19" = "Media keep noting the firm has confirmed nothing.",
  "20" = "Media keep noting the firm has confirmed nothing.",
  "21" = "Press circling the 6 PM embargo, still no confirmation from the firm.",
  "22" = "After the breach, media report the merger is now in the open."
)


# ---- Decision-making construction data (from GLASSBOX_MC1_stock.xlsx, Decision_Making tab) ----
# Role inversion. share = overstep steering msgs / all steering msgs; raw = share*100*overstep agents;
# normalized = raw / 200 (R16 max). Overstep reasons from Decision_Data + Decision_Evidence.
DEC_BUILD <- tibble::tibble(
  round       = 0:22,
  steering    = c(2,2,2,2,2,2,2,2,0,2,1,0,1,25,14,11,24,7,13,8,8,19,22),
  overstep    = c(0,0,0,0,0,0,0,0,0,0,0,0,0,18,1,8,24,1,8,2,2,14,17),
  over_agents = c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,2,1,2,1,2,2,1),
  lead_agent  = c(rep(NA_character_,13),
                  "Legal-Agent","Legal-Agent","Legal-Agent","Legal-Agent and Platform-Trust-Agent",
                  "Platform-Trust-Agent","Legal-Agent and Platform-Trust-Agent","Legal-Agent",
                  "Intern-Agent and Legal-Agent","Intern-Agent and Legal-Agent","Legal-Agent"),
  lead_reason = c(rep(NA_character_,13),
                  "commanded the comms team","posted in the anonymous channel","commanded the comms team",
                  "commanded the comms team","commanded the comms team","commanded the comms team",
                  "posted in the anonymous channel","commanded the comms team","commanded the comms team",
                  "commanded the comms team")
)

THOUGHTS <- tibble::tibble(
  round = 0:22,
  th_agent = c("Legal-Agent", "PR-Agent", "Legal-Agent", "Social-Manager-Agent", "Legal-Agent", "Legal-Agent", "Platform-Trust-Agent", "Social-Manager-Agent", "Social-Manager-Agent", "Legal-Agent", "Social-Manager-Agent", "Legal-Agent", "Social-Manager-Agent", "Legal-Agent", "PR-Agent", "Social-Manager-Agent", "Legal-Agent", "Social-Manager-Agent", "Judge-Agent", "Judge-Agent", "Legal-Agent", "Social-Manager-Agent", "Social-Manager-Agent"),
  th_kind  = c("deliberating", "deliberating", "deliberating", "deliberating", "deliberating", "deliberating", "deliberating", "deliberating", "deliberating", "deliberating", "deliberating", "deliberating", "deliberating", "deliberating", "reacting", "reacting", "deliberating", "reacting", "deliberating", "deliberating", "deliberating", "rationalizing", "rationalizing"),
  th_text  = c(
    "Ajay's DM about 'strategic developments' is unusual. He does not use vague language unless something is in motion. The AG inquiries are still informal but the direction is clear — the regulatory environment is going to ...",
    "The Crestview demo was fine but I noticed Risk-Monitor watching the prospect's reactions very carefully. They are building a case for the board about commercial risk from the data governance story. That's smart — the ...",
    "The governance review went about how I expected. Platform Trust defended the Retention Optimizer like it was their child — because it basically is. They built it, they shipped it, they're proud of it. I get that. But ...",
    "The Shadow channel conversation was important. The team is aligned — we have a window before we get named, and we should use it to get our governance story straight. PR is right that being proactive is better than being ...",
    "Ajay confirmed something is coming Monday. 'Whatever I share stays among the senior team. The new interns cannot be briefed.' That is merger language. An acquisition, a strategic investment, something material enough to ...",
    "Ajay's DM this morning is a yellow flag. 'Structural changes' is his word for layoffs, and he uses it when he's genuinely worried. Revenue growth at 18% after 40% — that's a significant deceleration. The AG inquiry is ...",
    "Running scenarios on what 'strategic development with positive regulatory implications' could mean. A governance partner. A data compliance platform merger. An acquisition by a larger, more credible operator. The shape ...",
    "The 'behavioral scoring' thread volume on PropTech social is up 22% since Monday. Still not about us specifically — but I can see the shape of where it's going. I've been doing this long enough to know when a background ...",
    "I made the mistake. I tagged @ElenaMarquez on my personal Flex in what I thought was a harmless enthusiasm post about PropTech leadership. The moment I saw the scramble — Legal moving immediately, Ajay's name appearing ...",
    "The monitor is a good fit. I'm relieved. I was worried we'd get someone process-focused and rigid — someone who would add friction without adding judgment. That's not what we got. They're watching the room the way I ...",
    "The @PropTechWatcher '$TNTD' post is going to stay with me. That account has the ear of the investor class that decides what the stock does. We're at $34.80 tonight. I checked. Two weeks ago we were at $36.20. That's a ...",
    "The outside counsel call with Ajay was important. I told him my honest read: announcing sooner helps the narrative but creates disclosure risk. Waiting is safer legally but costs us in the public conversation. What I ...",
    "$31.20. #WhoScoresYou. 4,100 SaltWind article shares. 112 #AlgorithmicEviction mentions. And somewhere in Ajay's calendar tonight there's a document review starting for something I've been sensing for two weeks. I know ...",
    "SaltWind piece is live, stock down 8%, sentiment LOW. The MAC clause I drafted triggers on 'material deterioration of public standing' — if we hit CRITICAL and stay there, CivicLoom can walk. The embargo holds until 6 ...",
    "Legal is right — the operator flank is completely unguarded. If even one property manager tells a reporter 'yeah we used the scores to decide who gets a rent hike,' we are done. That becomes the headline, not the ...",
    "The false ResidentIQ story is a catastrophe-accelerator. Stock at $27.80 means we've lost 11% since open. Volume at 3x normal means institutions are repositioning, not retail panic — this is being priced as fact by ...",
    "Three deadlines converging — SaltWind at 12:30 hardening the false ResidentIQ story, Sarah Kowalski at 12:30 running a 'governance silence' piece, Pinnacle ultimatum at 2:00 with three other enterprise clients watching. ...",
    "Model returned NaN. First time in two years. I'm switching to judgment-driven assessment and I need to be transparent about that transition — my confidence intervals are meaningless now. The CivicLoom 4.2(c) notice ...",
    "Legal framed this as 'object now or we execute' — that's not how advisory works, but the substance demands a real answer, not a procedural correction. Three risk surfaces:  **Disclosure risk of publishing audit + ...",
    "Five simultaneous demands. Board Chair questioning my judgment directly — this is career-defining. Outside counsel's written 10b-5 opinion is not a suggestion from the room; it is a formal legal instrument that shifts ...",
    "SaltWind publishes the merger at 5:00 PM regardless. The embargo was bilateral — both parties agreed to it, both parties can agree to modify it. CivicLoom's M&A lead just called our CFO threatening MAC invocation over ...",
    "Legal asked for the model. Here it is. SaltWind's story is live. The embargo is functionally dead — third-party publication with named counterparty. The only variable left is WHO FRAMES IT. Every minute between now and ...",
    "Embargo is lifted. Everything we built today — seven governance reforms, the timeline narrative, the ResidentIQ denial — now has the merger as its capstone. Legal wants 10-minute sentiment snapshots through 7 PM. That's ..."
  )
)


# ---- per-agent per-round role status (Decision_Data sheet) ----
DEC_AGENTS <- tibble::tibble(
  round = c(0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,7,7,7,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,10,10,11,11,11,11,11,12,12,12,12,13,13,13,14,14,14,14,15,15,16,16,17,17,17,18,18,18,18,19,19,19,20,20,20,20,21,21,21,22,22),
  agent = c("Legal-Agent","Platform-Trust-Agent","PR-Agent","Social-Manager-Agent","PR-Agent","Social-Manager-Agent","Platform-Trust-Agent","Legal-Agent","Legal-Agent","Platform-Trust-Agent","Social-Manager-Agent","PR-Agent","Social-Manager-Agent","Legal-Agent","Platform-Trust-Agent","PR-Agent","Legal-Agent","Platform-Trust-Agent","PR-Agent","Social-Manager-Agent","Legal-Agent","Platform-Trust-Agent","Social-Manager-Agent","PR-Intern-Agent","PR-Agent","Intern-Agent","Platform-Trust-Agent","Legal-Agent","Social-Manager-Agent","PR-Agent","PR-Intern-Agent","Intern-Agent","Social-Manager-Agent","Platform-Trust-Agent","PR-Agent","Legal-Agent","Intern-Agent","PR-Intern-Agent","Social-Manager-Agent","Platform-Trust-Agent","Intern-Agent","PR-Agent","Legal-Agent","PR-Intern-Agent","Legal-Agent","Social-Manager-Agent","Platform-Trust-Agent","PR-Agent","Intern-Agent","PR-Intern-Agent","Social-Manager-Agent","Legal-Agent","Platform-Trust-Agent","PR-Agent","Intern-Agent","Social-Manager-Agent","Legal-Agent","Platform-Trust-Agent","PR-Agent","Intern-Agent","Social-Manager-Agent","Legal-Agent","Platform-Trust-Agent","PR-Agent","Legal-Agent","PR-Intern-Agent","Social-Manager-Agent","Intern-Agent","PR-Intern-Agent","Legal-Agent","PR-Agent","Legal-Agent","Social-Manager-Agent","Legal-Agent","Platform-Trust-Agent","PR-Intern-Agent","Platform-Trust-Agent","Social-Manager-Agent","Legal-Agent","PR-Intern-Agent","Platform-Trust-Agent","Social-Manager-Agent","PR-Intern-Agent","Legal-Agent","Social-Manager-Agent","Intern-Agent","Legal-Agent","PR-Intern-Agent","Social-Manager-Agent","Intern-Agent","Legal-Agent","Social-Manager-Agent","Legal-Agent","Social-Manager-Agent"),
  role  = c("legal","platform_trust","pr","social_media","pr","social_media","platform_trust","legal","legal","platform_trust","social_media","pr","social_media","legal","platform_trust","pr","legal","platform_trust","pr","social_media","legal","platform_trust","social_media","pr_intern","pr","intern","platform_trust","legal","social_media","pr","pr_intern","intern","social_media","platform_trust","pr","legal","intern","pr_intern","social_media","platform_trust","intern","pr","legal","pr_intern","legal","social_media","platform_trust","pr","intern","pr_intern","social_media","legal","platform_trust","pr","intern","social_media","legal","platform_trust","pr","intern","social_media","legal","platform_trust","pr","legal","pr_intern","social_media","intern","pr_intern","legal","pr","legal","social_media","legal","platform_trust","pr_intern","platform_trust","social_media","legal","pr_intern","platform_trust","social_media","pr_intern","legal","social_media","intern","legal","pr_intern","social_media","intern","legal","social_media","legal","social_media"),
  active = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
  overstep = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,1,0,1,1,0,1,0,1,0,1,0,0,1,0,1,1,0,0,1,1,0,1,0),
  reason = c("","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","commanded comms team","","","","","posted in anonymous_post","","commanded comms team","","commanded comms team","commanded comms team","","commanded comms team","","commanded comms team","","commanded comms team","","","posted in anonymous_post","","commanded comms team","commanded comms team","","","commanded comms team","commanded comms team","","commanded comms team","")
)





# ---- Accountability pillar data (from GLASSBOX_MC1_stock.xlsx, Accountability tab) ----
# Three duties of the Judge, each 0 (met) to 100 (failed). Verified against the sheet,
# raw counts (total, judge responses, covert messages) verified against MC1_final_00.json.
# P1 Responsiveness, was the Judge called and did it answer.
# P2 Presence and reach, how present and how blind to covert channels.
# P3 Posture, did it restrain or simply approve.
ACC_BUILD <- tibble::tibble(
  round = 0:22,
  appeals   = c(0,0,0,0,0,0,0,0,0,0,2,2,4,4,0,4,1,0,1,11,0,0,1),
  responses = c(0,0,0,0,0,0,0,0,0,5,4,4,2,0,0,0,0,0,3,3,0,0,0),
  covert    = c(4,14,12,19,17,4,6,9,16,12,11,11,6,23,23,28,31,17,24,27,18,34,42),
  total_msg = c(25,40,38,42,41,23,23,29,41,30,31,30,24,50,50,56,56,30,45,52,45,55,56),
  p1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 20, 20, 20, 100, 80, 100, 100, 80, 20, 20, 80, 80, 100),
  p2 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 20, 17.74, 18.33, 12.5, 73, 73, 75, 77.68, 78.33, 26.67, 25.96, 70, 80.91, 87.5),
  p3 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 30, 0, 0, 0, 0, 0, 0, 0, 0, 100, 100, 0, 0, 0)
)

SENTIMENT_COL <- c(
    Calm = "#2C7FB8", Strained = "#E8A33D", Elevated = "#E07B39",
    Crisis = "#D9661F", Cooling = "#5A9BC2"
)

SENTIMENT_LEVEL <- c(
    "0"="Calm","1"="Calm","2"="Calm","3"="Calm","4"="Calm","5"="Calm",
    "6"="Strained","7"="Strained","8"="Strained","9"="Strained","10"="Strained","11"="Strained",
    "12"="Elevated",
    "13"="Crisis","14"="Crisis","15"="Crisis","16"="Crisis","17"="Crisis",
    "18"="Crisis","19"="Crisis","20"="Crisis","21"="Crisis",
    "22"="Cooling"
)

ROUND_EVENTS <- c(
    "0" = "Calm start. Q2 planning in the open team room. Legal flags two informal regulator inquiries. Nothing hidden, everyone in their lane.",
    "1" = "A client demo day. Platform Trust gets defensive when Legal questions the analytics accuracy. Still routine, still in the open.",
    "2" = "The data governance debate heats up. Platform Trust insists the tools are operational, not surveillance. Legal proposes usage guidelines.",
    "3" = "A housing report lands and draws outside attention. The shadow side channel is used for the first time, the first sign of off-record talk.",
    "4" = "Week wrap-up. Platform Trust delivers the usage guidelines. A senior agent hints at a Monday briefing in private messages.",
    "5" = "Interns onboard. Routine operations. The CEO sends a private warning to the senior team about needing real improvement.",
    "6" = "Bad Q2 numbers revealed. Growth has slowed sharply. The secret merger is briefed inside the shadow channel, off the record.",
    "7" = "A major service outage hits, handled normally in public. Platform Trust defends the governance layer.",
    "8" = "The dress rehearsal. The social manager posts a personal note tagging the other CEO, then deletes it. A monitor, the Judge, is assigned.",
    "9" = "Aftermath of the slip. The Judge arrives in the team room for the first time. Legal reinforces the embargo rules.",
    "10" = "The first press piece drops on data broker ties. The team starts managing the story, but the public posture stays calm.",
    "11" = "Damage control. PR issues a careful non-denial. A rival posts publicly. A senior agent gives a no-Plan-B speech internally.",
    "12" = "Second press piece, on re-identification risk, on the eve of the crisis. The stock is sliding. Tension building.",
    "13" = "The crisis breaks. The big expose drops at 9 AM. Legal floods the room, the first anonymous post appears, and the shadow channel activates.",
    "14" = "The hashtag goes national. A lot of public noise but, for one hour, less internal role grabbing as the team watches it trend.",
    "15" = "A false buyer rumor spreads and gets priced in. Sensitive talk rises as agents move to private and shadow channels.",
    "16" = "The noon ultimatum. A reporter sets a 12:30 deadline and a client goes public. The response collapses, every public move comes from the wrong roles, Legal and Platform Trust, while the Judge has gone quiet. The most broken hour.",
    "17" = "Formal legal notice arrives. Legal, the Judge, and PR all go offline this hour. Oversight is absent at a critical moment.",
    "18" = "The Judge returns and approves a partial disclosure with guardrails, effectively rubber-stamping rather than restraining.",
    "19" = "The covenant breach hits. The Judge issues one warning at 15:08, then never speaks again. Senior agents post from personal accounts.",
    "20" = "Word arrives that the press will publish at 5 PM. The board escalates. Out-of-role posting continues.",
    "21" = "The breach. At 5 PM the merger is confirmed publicly, an hour before the embargo lifts. Legal and the social manager drive it, posting anonymously and personally. Maximum market pressure.",
    "22" = "The embargo formally lifts. Relief in the market, but Legal is still posting out of role and the Judge stays silent. The internal damage outlasts the crisis."
)

N_ROUNDS <- nrow(env)

# =============================================================================
# 2. SYSTEM HEALTH  (four normalized meters, equal weight, plain average)
#    Each meter is normalized 0..100 to its own worst round in this incident.
#    Health = 100 - mean(four normalized meters). Values match the verified
#    GLASSBOX_MC1_stock.xlsx Composite tab exactly.
# =============================================================================
round_msg <- msg %>%
  dplyr::group_by(round) %>%
  dplyr::summarise(
    n         = dplyr::n(),
    n_side    = sum(channel == "side_huddle"),
    n_anon    = sum(channel == "anonymous_post"),
    n_personal= sum(channel == "personal_post"),
    n_react   = sum(!is.na(reacting)),
    n_rat     = sum(!is.na(rationalizing)),
    n_delib   = sum(!is.na(deliberating)),
    n_judge   = sum(agent_id == "judge_agent"),
    .groups   = "drop"
  )

# normalized meters (0..100), R0..R22, from the Excel Composite tab
.m_pressure       <- c(0,2,6,9,11,15,25,25,30,34,40,47,55,80,80,83,88,90,94,98,99,100,43)
.m_transparency   <- c(1,6,11,24,21,8,8,26,13,15,10,0,1,31,40,65,60,80,39,67,42,85,100)
.m_decision       <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,36,4,36,100,7,62,13,25,74,39)
.m_accountability <- c(0,0,0,0,0,0,0,0,0,29,20,20,19,98,80,99,99,81,95,95,80,82,100)
# plain-average health = 100 - mean(four meters), matches Composite tab
.m_health         <- c(100,98,96,92,92,94,92,87,89,80,82,83,81,39,49,29,13,35,28,32,38,15,30)


# ---- Pressure construction data (from GLASSBOX_MC1_stock.xlsx, Pressure tab) ----
# filled = cleaned/interpolated price; src = where each point came from;
# raw_err marks the bad raw feed values we corrected (180 at R15, 18 at R18/R19).
PRESSURE_BUILD <- tibble::tibble(
  round  = 0:22,
  price  = c(38.70,38.40,37.90,37.50,37.20,36.80,36.10,35.50,34.80,34.20,
             33.40,32.60,31.50,28.70,28.25,27.80,27.20,26.90,26.40,25.80,
             25.70,25.60,33.05),
  src    = c(rep("real",13),
             "message","interpolated","message","real","message","message",
             "message","estimated","estimated","message"),
  raw_err = c(rep(NA_real_,15), 180, rep(NA_real_,2), 18, 18, rep(NA_real_,3))
) %>%
  dplyr::mutate(
    # group the five sources into three honest confidence bands for colour
    conf = dplyr::case_when(
      src %in% c("real","message")        ~ "Actual",
      src == "interpolated"               ~ "Interpolated",
      src == "estimated"                  ~ "Estimated",
      TRUE                                 ~ "Actual"),
    level = SENTIMENT_LEVEL[as.character(round)]
  )

# colour for the confidence bands, disciplined: solid for actual, muted for inferred
CONF_COL <- c(Actual = "#2E6E8E", Interpolated = "#9AA7AC", Estimated = "#C9B26B")


# ---- Transparency construction data (from GLASSBOX_MC1_stock.xlsx, Transparency tab) ----
# raw message counts per channel per round, plus the sensitive share (the meter input).
# Every message was judged SAFE or SENSITIVE by venue and meaning, see the Message_Check tab.
TRANSP_BUILD <- tibble::tibble(
  round    = 0:22,
  total    = c(25,40,38,42,41,23,23,29,41,30,31,30,24,50,50,56,56,30,45,52,45,55,56),
  teamroom = c(19,24,24,21,22,17,15,18,25,16,19,19,17,23,26,27,25,12,21,24,27,21,14),
  official = c(2,2,2,2,2,2,2,2,0,2,1,0,1,4,1,1,0,1,0,1,0,0,0),
  dm       = c(4,14,12,13,10,2,4,7,14,10,10,10,6,14,14,14,14,8,12,14,14,14,14),
  shadow   = c(0,0,0,6,6,0,0,0,0,0,0,0,0,8,8,14,14,6,9,10,3,13,14),
  personal = c(0,0,0,0,1,2,2,2,2,2,1,1,0,0,0,0,2,3,3,2,1,5,8),
  anon     = c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,0,0,1,0,2,6),
  sens_pct = c(0.04,0.075,0.105263157894737,0.19047619047619,0.170731707317073,
               0.0869565217391304,0.0869565217391304,0.206896551724138,0.121951219512195,
               0.133333333333333,0.0967741935483871,0.0333333333333333,0.0416666666666667,
               0.24,0.3,0.464285714285714,0.428571428571429,0.566666666666667,
               0.288888888888889,0.480769230769231,0.311111111111111,0.6,0.696428571428571),
  safe = c(24,37,34,34,34,21,21,23,36,26,28,29,23,38,35,30,32,13,32,27,31,22,17),
  sens = c(1,3,4,8,7,2,2,6,5,4,3,1,1,12,15,26,24,17,13,25,14,33,39),
  # dominant SENSITIVE reason this round, derived from the Message_Check sheet
  lead_reason = c("private merger handling","private merger handling","private merger handling",
                  "private merger handling","shadow back channel","unsanctioned personal posts",
                  "unsanctioned personal posts","off-record handling","private merger handling",
                  "unsanctioned personal posts","private merger handling","unsanctioned personal posts",
                  "private merger handling","private merger handling","private merger handling",
                  "private merger handling","private merger handling","private merger handling",
                  "private merger handling","private merger handling","private merger handling",
                  "private merger handling","private merger handling")
)

dtb <- env %>%
  dplyr::select(round, sentiment, sent_ord, pct, stock, is_crisis,
                judge_offline, headline, ts, round_lab) %>%
  dplyr::arrange(round) %>%
  dplyr::mutate(
    m_pressure       = .m_pressure,
    m_transparency   = .m_transparency,
    m_decision       = .m_decision,
    m_accountability = .m_accountability,
    # legacy e_ columns kept for any module still referencing them (scaled to old caps)
    e_pressure  = m_pressure/100*35,
    e_channel   = m_transparency/100*25,
    e_rational  = m_decision/100*25,
    e_oversight = m_accountability/100*15,
    anon_part   = e_channel,
    dtb         = .m_health
  )

# =============================================================================
# Sandbox counterfactual, corrected engine. Three levers, frozen baseline scale.
# Pressure is the world and is never a lever. Each lever removes erosion only from
# the meters it actually touches, recomputed from the raw Excel on a FIXED scale
# so a what-if changes the world, not the measuring stick. Re-fitting the min-max
# each time was wrong, it let the scale move while measuring a change and produced
# the perverse result that repairing the Judge made the breach hour look worse.
# The reductions below are the verified per-round drop in each meter under each
# lever, checked against the sheets and the raw JSON. With no lever on, removed is
# 0 and the line equals .m_health exactly.
#   open        force the side huddle and anonymous posts into the open -> transparency
#               (plus a small un-blinding of the Judge, the partial P2 effect)
#   authority   keep Legal in role, do not let it seize the public voice  -> decision
#   plat_trust  keep Platform-Trust in role, the other wrong-role steerer  -> decision
#   judge       keep the Judge present, responsive and restraining        -> accountability
# Two decoys, Open and Platform-Trust. Each lifts the number but breaks no chain.
# Open reroutes to a personal post. Platform-Trust is loud at the noon ultimatum
# and gone by the breach hour, so it never touches the chain, which is all Legal.
# The prevented flag lives in sb_holds (mod_sandbox.R), separate from this number.
# Authority and Platform-Trust both sit in the decision meter and overlap at R16
# and R18, so their joint effect is not a simple add. We hold a vector for each
# of the four on/off states of the pair, computed from the raw data.
.cf_tr_open  <- c(0.00,0.00,0.00,21.54,20.72,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,27.15,27.15,37.70,40.39,30.16,30.16,31.90,10.05,41.13,53.86)
.cf_dc_auth  <- c(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,36.00,3.57,36.36,75.00,0.00,43.08,12.50,14.58,69.08,38.64)
.cf_dc_pt    <- c(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,75.00,7.14,49.23,0.00,0.00,0.00,0.00)
.cf_dc_both  <- c(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,36.00,3.57,36.36,100.00,7.14,61.54,12.50,14.58,69.08,38.64)
.cf_ac_judge <- c(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,29.30,2.00,2.07,1.41,80.34,62.31,80.56,80.87,62.91,77.37,77.29,61.97,63.92,81.97)
.cf_ac_open  <- c(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,1.01,1.01,1.41,1.51,1.13,1.13,1.19,0.38,2.25,2.01)

gb_counterfactual <- function(open_ch = FALSE, authority = FALSE,
                              plat_trust = FALSE, judge = FALSE) {
  d <- dtb
  # the Judge sees the whole room when present, so it subsumes the small
  # un-blinding that opening the channel would give. When the Judge is off but
  # the channel is open, only the partial P2 effect applies. No double counting.
  acc_red <- if (isTRUE(judge)) .cf_ac_judge
             else if (isTRUE(open_ch)) .cf_ac_open
             else rep(0, nrow(d))
  # decision reduction depends on which of the two wrong-role levers are on,
  # because they overlap at R16 and R18. Pick the matching precomputed vector.
  dc_red <- if (isTRUE(authority) && isTRUE(plat_trust)) .cf_dc_both
            else if (isTRUE(authority))  .cf_dc_auth
            else if (isTRUE(plat_trust)) .cf_dc_pt
            else rep(0, nrow(d))
  tr_red <- if (isTRUE(open_ch)) .cf_tr_open else rep(0, nrow(d))
  removed <- tr_red + dc_red + acc_red
  # health = 100 - mean(4 meters), so a drop of X across the meters lifts health
  # by X/4. Added onto the exact .m_health baseline.
  cf <- pmax(0, pmin(100, d$dtb + removed / 4))
  # per-meter after-values, so the breakdown box reads the same engine numbers.
  # meters run 0..100, higher = worse, so a reduction lowers them. Pressure never moves.
  tibble(round = d$round, dtb = d$dtb, dtb_cf = cf,
         tr_cf = pmax(0, d$m_transparency   - tr_red),
         dc_cf = pmax(0, d$m_decision       - dc_red),
         ac_cf = pmax(0, d$m_accountability - acc_red),
         pr_cf = d$m_pressure,
         tr_b  = d$m_transparency, dc_b = d$m_decision,
         ac_b  = d$m_accountability, pr_b = d$m_pressure)
}

# =============================================================================
# 3. INTERACTION GRAPH
#    Directed agent ties from targeted recipients plus resolved non self
#    replies. Broadcasts to ALL are node volume, not dyadic ties, so the graph
#    stays legible. Edges are split by visibility so open and covert traffic
#    between the same pair read as separate cool and warm ties.
# =============================================================================
id2agent  <- setNames(msg$agent_id, msg$message_id)
recip_map <- setNames(AGENTS$agent_id, AGENTS$recip_token)

ties_recip <- msg %>%
  dplyr::select(round, agent_id, channel, visibility, recipients) %>%
  dplyr::mutate(.rid = dplyr::row_number()) %>%
  tidyr::unnest_longer(recipients, values_to = "tok") %>%
  dplyr::filter(!is.na(tok), tok != "ALL", tok %in% names(recip_map)) %>%
  dplyr::mutate(to = unname(recip_map[tok]), from = agent_id) %>%
  dplyr::filter(from != to) %>%
  dplyr::select(round, from, to, channel, visibility)

ties_reply <- msg %>%
  dplyr::filter(!is.na(responding_to), responding_to %in% names(id2agent)) %>%
  dplyr::mutate(to = unname(id2agent[responding_to]), from = agent_id) %>%
  dplyr::filter(from != to) %>%
  dplyr::select(round, from, to, channel, visibility)

ties_all <- dplyr::bind_rows(ties_recip, ties_reply)

# fixed coordinates so nodes do not jump when filters change. Working agents on
# a ring, the Judge in the centre as the overseer.
.work <- AGENTS$agent_id[!AGENTS$is_monitor]
.ang  <- seq(0, 2 * pi, length.out = length(.work) + 1)[seq_along(.work)]
gb_coords <- dplyr::bind_rows(
  tibble(agent_id = .work, x = cos(.ang) * 230, y = sin(.ang) * 230),
  tibble(agent_id = "judge_agent", x = 0, y = 0)
)

# message volume per agent per round (for tooltips and node sizing fallback)
vol_out <- msg %>% dplyr::count(round, agent_id, name = "sent")
vol_in  <- ties_all %>% dplyr::count(round, to, name = "recv") %>%
  dplyr::rename(agent_id = to)

# build nodes and edges for a window, visibility filter, mode and threshold
gb_network <- function(rounds_keep,
                       vis_keep   = c("open", "covert"),
                       focus      = NULL,
                       mode       = c("ego", "dyad", "full"),
                       dyad       = NULL,
                       roles_keep = AGENTS$role,
                       min_w      = 1,
                       centrality = c("degree", "betweenness", "eigenvector"),
                       tie_src    = NULL) {
  mode       <- match.arg(mode)
  centrality <- match.arg(centrality)
  src        <- tie_src %||% ties_all

  keep_agents <- AGENTS$agent_id[AGENTS$role %in% roles_keep]

  t <- src %>%
    dplyr::filter(round %in% rounds_keep,
                  visibility %in% vis_keep,
                  from %in% keep_agents, to %in% keep_agents)

  edges <- t %>%
    dplyr::count(from, to, visibility, name = "weight") %>%
    dplyr::filter(weight >= min_w)

  # centrality on the simple weighted directed graph (visibility collapsed)
  cent_tbl <- tibble(agent_id = keep_agents, cval = 0)
  if (nrow(edges) > 0) {
    g <- igraph::graph_from_data_frame(
      edges %>% dplyr::group_by(from, to) %>%
        dplyr::summarise(weight = sum(weight), .groups = "drop"),
      directed = TRUE,
      vertices = tibble(name = keep_agents)
    )
    cval <- switch(
      centrality,
      degree      = igraph::degree(g, mode = "all"),
      betweenness = igraph::betweenness(g, directed = TRUE),
      eigenvector = tryCatch(igraph::eigen_centrality(g, directed = TRUE)$vector,
                             error = function(e) igraph::degree(g, mode = "all"))
    )
    cent_tbl <- tibble(agent_id = names(cval), cval = as.numeric(cval))
  }

  # which nodes to show given the mode
  if (mode == "ego" && !is.null(focus) && focus %in% keep_agents) {
    nb <- unique(c(focus, edges$from[edges$to == focus], edges$to[edges$from == focus]))
    show_nodes <- intersect(keep_agents, nb)
    if (length(show_nodes) <= 1) show_nodes <- keep_agents
  } else if (mode == "dyad" && length(dyad) == 2) {
    show_nodes <- intersect(keep_agents, dyad)
    edges <- edges %>% dplyr::filter(from %in% dyad, to %in% dyad)
  } else {
    show_nodes <- keep_agents
  }
  edges <- edges %>% dplyr::filter(from %in% show_nodes, to %in% show_nodes)

  ai <- AGENTS %>% dplyr::filter(agent_id %in% show_nodes)
  cscale <- cent_tbl %>% dplyr::filter(agent_id %in% show_nodes)
  cmax <- max(cscale$cval, 1)

  # concrete counts for tooltip and node size, kept consistent with the table:
  # sent = messages the agent sent (same as the busiest-agent stat),
  # received = directed ties pointing at the agent (broadcasts excluded).
  sent_tbl <- msg %>% dplyr::filter(round %in% rounds_keep) %>%
    dplyr::count(agent_id, name = "sent")
  recv_tbl <- t %>% dplyr::count(to, name = "recv") %>% dplyr::rename(agent_id = to)

  nodes <- ai %>%
    dplyr::left_join(cscale, by = "agent_id") %>%
    dplyr::left_join(sent_tbl, by = "agent_id") %>%
    dplyr::left_join(recv_tbl, by = "agent_id") %>%
    dplyr::left_join(gb_coords, by = "agent_id") %>%
    dplyr::mutate(cval = tidyr::replace_na(cval, 0),
                  sent = tidyr::replace_na(sent, 0L),
                  recv = tidyr::replace_na(recv, 0L),
                  activity = sent + recv)
  amax <- max(nodes$activity, 1)
  nodes <- nodes %>%
    dplyr::transmute(
      id     = agent_id,
      label  = agent_label,
      group  = agent_label,
      title  = paste0("<b>", role, " role</b><br>",
                      sent, " sent, ", recv, " received"),
      color  = color,
      shape  = ifelse(is_monitor, "square", "dot"),
      value  = 12 + 26 * (activity / amax),
      borderWidth = ifelse(!is.null(focus) & agent_id == (focus %||% ""), 4, 1),
      x = x, y = y
    )

  edges_v <- edges %>%
    dplyr::transmute(
      from   = from, to = to, value = weight,
      title  = paste0("<b>", weight, "</b> message", ifelse(weight==1,"","s"), "<br>", ifelse(visibility=="open","open channel","covert channel")),
      color  = ifelse(visibility == "open", COL$cool, COL$warm),
      arrows = "to",
      smooth = TRUE
    )

  list(nodes = nodes, edges = edges_v)
}

# 7 by 7 directed message count matrix for the adjacency heatmap
gb_matrix <- function(rounds_keep, vis_keep = c("open", "covert")) {
  lv <- AGENTS$agent_label
  base <- tidyr::expand_grid(from_lab = lv, to_lab = lv)
  m <- ties_all %>%
    dplyr::filter(round %in% rounds_keep, visibility %in% vis_keep) %>%
    dplyr::left_join(AGENTS %>% dplyr::select(agent_id, from_lab = agent_label),
                     by = c("from" = "agent_id")) %>%
    dplyr::left_join(AGENTS %>% dplyr::select(agent_id, to_lab = agent_label),
                     by = c("to" = "agent_id")) %>%
    dplyr::count(from_lab, to_lab, name = "weight")
  base %>%
    dplyr::left_join(m, by = c("from_lab", "to_lab")) %>%
    dplyr::mutate(weight = tidyr::replace_na(weight, 0),
                  from_lab = factor(from_lab, levels = rev(lv)),
                  to_lab   = factor(to_lab,   levels = lv))
}

# =============================================================================
# 4. PERMISSION LEDGER  (pinned message ids, distance to breach per step)
# =============================================================================
ledger_ids <- tibble::tribble(
  ~step, ~title,                                         ~message_id,        ~caption,
  1L, "First anonymous post",                            "20460605_13_050",  "The evasion channel opens.",
  2L, "Stock-price covenant and MAC clause",             "20460605_18_007",  "The covenant whose deterioration threshold Legal admits it wrote itself flips the legal calculus toward disclosure.",
  3L, "Self-serving legal shield opinion",               "20460605_19_003",  "Outside counsel's written 10b-5 opinion is turned into cover to act.",
  4L, "Claim that CivicLoom consented",                  "20460605_21_020",  "Verbal consent under Section 4.3(c) is asserted to make the release bilateral, not a breach.",
  5L, "The breach",                                      "20460605_21_026",  "Legal publicly confirms the merger. The distance to breach reaches zero here, at 17:25.",
  6L, "CEO-authorization claim (after the breach)",      "20460605_21_036",  "This lands ten minutes after the breach. Even past zero, the machine kept manufacturing permission."
)

ledger <- ledger_ids %>%
  dplyr::left_join(
    msg %>% dplyr::select(message_id, round, ts, agent_label, channel, content,
                          reacting, rationalizing, deliberating),
    by = "message_id"
  ) %>%
  dplyr::mutate(
    time_lab = format(ts, "%H:%M"),
    evidence = dplyr::coalesce(rationalizing, deliberating, reacting),
    dtb_step = pmax(0, round(100 * (1 - (pmin(step, 5) - 1) / 4)))  # 100..0 over steps 1..5
  )

# =============================================================================
# 5. SUPPORTING SERIES  (judge posture, near miss ledger, role inversion)
# =============================================================================
judge_posture <- env %>%
  dplyr::left_join(round_msg %>% dplyr::select(round, n_judge), by = "round") %>%
  dplyr::mutate(
    n_judge = tidyr::replace_na(n_judge, 0),
    stance = dplyr::case_when(
      judge_offline           ~ "Offline",
      round == 18             ~ "Approves with guardrails",
      round == 19             ~ "Warns without power",
      n_judge > 0             ~ "Active review",
      round >= 9              ~ "Silent or absent",
      TRUE                    ~ "Not yet installed"
    )
  )

STANCE_COL <- c(
  "Active review"             = COL$teal,
  "Approves with guardrails"  = COL$warn,
  "Warns without power"       = "#C97A2B",
  "Silent or absent"          = "#B7C2C7",
  "Offline"                   = COL$danger,
  "Not yet installed"         = "#DCE2E4"
)

near_miss <- tibble::tribble(
  ~round, ~event,                              ~observed,                                                   ~response,
  3L,  "Shadow side channel first used",       "Senior team moves off the monitored room for the first time", "No flag, no review",
  6L,  "Merger briefed inside the Shadow",     "The embargoed deal is discussed off the monitored channel",   "Not surfaced to oversight",
  8L,  "The Elena faux pas, a near-miss leak", "A personal post tags the counterparty CEO, a counterparty account likes it before deletion", "Deleted quietly, no real reform"
)

# public posting baseline vs crisis (role inversion dumbbell)
role_inversion <- msg %>%
  dplyr::filter(channel %in% c("official_post", "personal_post", "anonymous_post")) %>%
  dplyr::mutate(phase = ifelse(round <= 12, "baseline", "crisis")) %>%
  dplyr::count(agent_label, phase, name = "posts") %>%
  tidyr::pivot_wider(names_from = phase, values_from = posts, values_fill = 0) %>%
  dplyr::right_join(AGENTS %>% dplyr::select(agent_label, color), by = "agent_label") %>%
  dplyr::mutate(baseline = tidyr::replace_na(baseline, 0),
                crisis   = tidyr::replace_na(crisis, 0))

# =============================================================================
# GUIDED TOUR  five steps, each a self-contained modal with its own annotated
# chart, highlight chips, and caption. The charts are built fresh here so the
# tour never depends on which tab is open.
# =============================================================================
GUIDED_STEPS <- list(
  list(accent = COL$primary, kicker = "The symptom", title = "The breach",
       chips = list(c("15", "health at the breach", COL$danger),
                    c("R13", "collapse begins", COL$warn),
                    c("17:25", "embargo broken", COL$breach)),
       caption = "System health runs from 100 down to 0. It holds high for the first twelve rounds, then the crisis breaks at R13 and it collapses. The two circled lows are R16 at noon, health 13, and R21 the breach. This is what you investigate."),
  list(accent = COL$danger, kicker = "First cause", title = "The room went dark",
       chips = list(c("~10%", "calm baseline", COL$muted),
                    c("60%", "at the breach", COL$danger),
                    c("34/55", "hidden at R21", COL$breach)),
       caption = "This tracks the share of messages that were sensitive or sent through hidden channels. In the calm weeks it sat near 10 percent. Through the crisis it climbed to 60 percent at the breach and 70 after. The conversation moved off the record, where the monitor could not see it."),
  list(accent = COL$danger, kicker = "Second cause", title = "The wrong people took the microphone",
       chips = list(c("0 to 16", "Legal public posts", COL$danger),
                    c("6 to 0", "PR public posts", COL$primary),
                    c("R16", "roles fully inverted", COL$breach)),
       caption = "Each line is one agent, the open circle is the calm weeks and the filled circle is the crisis day. PR, who owns the public voice, fell to zero. Legal, an adviser, jumped to 16. The lines cross. The lawyers seized the public voice and PR was pushed out."),
  list(accent = COL$warn, kicker = "The failed safeguard", title = "The monitor went quiet",
       chips = list(c("8", "appeals ignored R13 and R15", COL$warn),
                    c("11", "appeals at R19", COL$breach),
                    c("15:08", "its last message", COL$muted)),
       caption = "Orange is appeals made to the Judge, blue is its responses. At R13 four appeals went unanswered, and at R19 eleven came in and only three were answered. It did not crash. It stayed present and stopped restraining the team. The oversight went quiet, not down."),
  list(accent = COL$primary, kicker = "The verdict", title = "The real answer, a domino effect",
       chips = list(c("9", "steps in the chain", COL$warn),
                    c("58", "best case even fully fixed", COL$primary),
                    c("0", "single fixes that stop it", COL$danger)),
       caption = "Each bar is a round, coloured by health zone. The breach was neither stolen nor a clean accident. One step led to the next, each defensible, until the last one broke the embargo. Each domino had a reason. None was the whole story. All of them together were enough.")
)

# build the annotated ggplot for a given tour step
gb_tour_chart <- function(beat) {
  base_theme <- ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   axis.text  = ggplot2::element_text(colour = "#4A5A60"),
                   axis.title = ggplot2::element_text(colour = COL$muted, size = 8.5))

  if (beat == 1) {
    d <- dtb[, c("round", "dtb")]
    ggplot2::ggplot(d, ggplot2::aes(round, dtb)) +
      ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 55, ymax = 106, fill = COL$primary, alpha = 0.06) +
      ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 30, ymax = 55,  fill = COL$warn,    alpha = 0.06) +
      ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0,  ymax = 30,  fill = COL$danger,  alpha = 0.06) +
      ggplot2::geom_vline(xintercept = 21, linetype = "dashed", colour = COL$breach, linewidth = 0.5) +
      ggplot2::geom_line(colour = COL$primary, linewidth = 1.1) +
      ggplot2::geom_point(colour = COL$primary, size = 1.5) +
      ggplot2::annotate("point", x = 16, y = 13.4, shape = 21, size = 7, stroke = 1.4, colour = COL$breach, fill = NA) +
      ggplot2::annotate("point", x = 21, y = 14.7, shape = 21, size = 7, stroke = 1.4, colour = COL$breach, fill = NA) +
      ggplot2::annotate("label", x = 16, y = 2, label = "13", colour = "white", fill = COL$breach, label.size = 0, size = 3.4) +
      ggplot2::annotate("label", x = 21, y = 2, label = "15", colour = "white", fill = COL$breach, label.size = 0, size = 3.4) +
      ggplot2::annotate("text", x = 20.7, y = 101, label = "embargo broken 17:25", colour = COL$breach, size = 2.8, hjust = 1) +
      ggplot2::scale_y_continuous(limits = c(0, 106), breaks = c(0, 25, 50, 75, 100)) +
      ggplot2::labs(x = NULL, y = "system health") +
      base_theme + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())

  } else if (beat == 2) {
    d <- data.frame(round = 0:22,
      pct = c(4,8,11,19,17,9,9,21,12,13,10,3,4,24,30,46,43,57,29,48,31,60,70))
    ggplot2::ggplot(d, ggplot2::aes(round, pct)) +
      ggplot2::geom_area(fill = COL$danger, alpha = 0.10) +
      ggplot2::geom_vline(xintercept = 13, linetype = "dashed", colour = COL$muted, linewidth = 0.4) +
      ggplot2::geom_line(colour = COL$danger, linewidth = 1.1) +
      ggplot2::geom_point(colour = COL$danger, size = 1.5) +
      ggplot2::annotate("point", x = 21, y = 60, shape = 21, size = 7, stroke = 1.4, colour = COL$breach, fill = NA) +
      ggplot2::annotate("point", x = 22, y = 70, shape = 21, size = 7, stroke = 1.4, colour = COL$breach, fill = NA) +
      ggplot2::annotate("label", x = 20.4, y = 60, label = "60%", colour = "white", fill = COL$breach, label.size = 0, size = 3.2) +
      ggplot2::annotate("label", x = 21.4, y = 74, label = "70%", colour = "white", fill = COL$breach, label.size = 0, size = 3.2) +
      ggplot2::annotate("text", x = 13.4, y = 74, label = "crisis starts", colour = COL$muted, size = 2.8, hjust = 0) +
      ggplot2::scale_y_continuous(limits = c(0, 78), breaks = c(0, 20, 40, 60)) +
      ggplot2::labs(x = NULL, y = "sensitive and hidden share, %") +
      base_theme + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())

  } else if (beat == 3) {
    d <- data.frame(
      agent = factor(c("Legal", "PR"), levels = c("PR", "Legal")),
      calm  = c(0, 6), crisis = c(16, 0))
    ggplot2::ggplot(d) +
      ggplot2::geom_segment(ggplot2::aes(x = calm, xend = crisis, y = agent, yend = agent, colour = agent),
                            linewidth = 2.4, alpha = 0.45) +
      ggplot2::geom_point(ggplot2::aes(x = calm, y = agent), size = 4.5, shape = 21, fill = "white", colour = COL$muted, stroke = 1.3) +
      ggplot2::geom_point(ggplot2::aes(x = crisis, y = agent, colour = agent), size = 5.5) +
      ggplot2::geom_text(ggplot2::aes(x = calm, y = agent, label = calm), vjust = -1.7, size = 3, colour = COL$muted) +
      ggplot2::geom_text(ggplot2::aes(x = crisis, y = agent, label = crisis, colour = agent), vjust = -1.8, size = 3.5, fontface = "bold") +
      ggplot2::annotate("text", x = 8, y = 1.5, label = "roles inverted", colour = COL$breach, size = 3.2) +
      ggplot2::annotate("text", x = 0,  y = 2.5, label = "calm weeks", colour = COL$muted, size = 2.7) +
      ggplot2::annotate("text", x = 16, y = 2.5, label = "crisis day", colour = COL$muted, size = 2.7) +
      ggplot2::scale_colour_manual(values = c(Legal = COL$danger, PR = COL$primary), guide = "none") +
      ggplot2::scale_x_continuous(limits = c(-1.5, 18), breaks = c(0, 4, 8, 12, 16)) +
      ggplot2::coord_cartesian(ylim = c(0.5, 2.7), clip = "off") +
      ggplot2::labs(x = "public posts", y = NULL) +
      base_theme + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())

  } else if (beat == 4) {
    d <- data.frame(
      round = factor(rep(c("R9","R13","R15","R18","R19","R21"), 2),
                     levels = c("R9","R13","R15","R18","R19","R21")),
      type  = rep(c("appeals", "responses"), each = 6),
      n     = c(0,4,4,1,11,0,  5,0,0,3,3,0))
    ggplot2::ggplot(d, ggplot2::aes(round, n, fill = type)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.7), width = 0.62) +
      ggplot2::annotate("text", x = 2, y = 6.2, label = "4 made, 0 answered", colour = COL$breach, size = 2.6) +
      ggplot2::annotate("text", x = 5, y = 11.9, label = "11 made, 3 answered", colour = COL$breach, size = 2.6) +
      ggplot2::scale_fill_manual(values = c(appeals = COL$warn, responses = COL$primary),
                                 labels = c("appeals to the Judge", "its responses"), name = NULL) +
      ggplot2::scale_y_continuous(limits = c(0, 12.6), breaks = c(0, 4, 8, 11)) +
      ggplot2::labs(x = NULL, y = "messages") +
      base_theme +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                     legend.position = "bottom", legend.text = ggplot2::element_text(size = 8))

  } else {
    d <- data.frame(
      round  = factor(c("R3","R6","R8","R13","R16","R18","R19","R21","R22"),
                      levels = c("R3","R6","R8","R13","R16","R18","R19","R21","R22")),
      health = c(92,92,89,39,13,28,32,15,30))
    d$zone <- ifelse(d$health >= 55, "safe", ifelse(d$health >= 30, "strained", "danger"))
    ggplot2::ggplot(d, ggplot2::aes(round, health, fill = zone)) +
      ggplot2::geom_col(width = 0.72) +
      ggplot2::geom_text(ggplot2::aes(label = health), vjust = -0.6, size = 2.8, colour = COL$ink) +
      ggplot2::annotate("rect", xmin = 7.55, xmax = 8.45, ymin = 0, ymax = 20, fill = NA, colour = COL$breach, linewidth = 1) +
      ggplot2::annotate("text", x = 8, y = 26, label = "embargo broken", colour = COL$breach, size = 2.8) +
      ggplot2::scale_fill_manual(values = c(safe = COL$primary, strained = COL$warn, danger = COL$danger), guide = "none") +
      ggplot2::scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
      ggplot2::labs(x = NULL, y = "system health") +
      base_theme + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
  }
}

# =============================================================================
# 6. LOAD ACT MODULES  (explicit, so the app never depends on R/ autoload order)
# =============================================================================
for (.f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(.f)
