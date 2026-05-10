# ============================================================================
# Plot-Skript Block 2 — Mikroökonomische Entscheidungen
#
# Verwendung in slides.qmd:
#   source(xfun::from_root("source/_assets/styles/theme-isba.R"))
#   source(xfun::from_root("source/_assets/figs/block-02-figs.R"))
#
# Dann auf Plot-Objekte zugreifen:
#   p_b2_pref_1 ... p_b2_pref_6   (Indifferenzkurven, 6 Animationsschritte)
#
# Empfohlene Chunk-Optionen in slides.qmd:
#   Modelldiagramme:  fig-width: 12, fig-height: 5.5, out-width: ~
#   out-width: ~ ist in r-stack-Blöcken Pflicht, weil das globale
#   out.width: "100%" aus _quarto.yml in der Flex/Grid-Verschachtelung
#   von .has-indicator + .r-stack die Bildhöhe kollabieren lässt.
#
# Helper-Funktion add_pt() wird mehrfach verwendet — Definition siehe
# theme-isba.R. Sie erstellt Punkt + Label aus einem tibble mit Pflicht-
# spalten 'x', 'y', 'label'.
# ============================================================================


# Einkommen (Theorie-Stil mit selektiven Labels) ----------

## 1. Daten als tibble speichern-----
tbl_hours <- tibble(
  x     = seq(0, 16, by = 2),
  y     = seq(0, 480, by = 60),
  label = c(NA, NA, "A", NA, NA, "B", NA, NA, NA)
)

## 2. Basis-Skalen und Theme definieren -------
base_scales_hours <- list(
  # X-Achse behält alle Labels (inklusive der 0)
  scale_x_continuous(limits = c(0, 17), breaks = seq(0, 16, 2), expand = c(0, 0)),

  # Y-Achse zeigt alle Labels, außer der 0 (um Überlappung zu vermeiden)
  scale_y_continuous(
    limits = c(0, 600),
    breaks = seq(0, 500, 100),
    labels = function(x) ifelse(x == 0, "", x),
    expand = c(0, 0)
  ),

  labs(x = "Arbeitszeit (Stunden)", y = "Einkommen (€)"),
  theme_isba(theory = TRUE)
)

## 3. Schrittweiser Aufbau der Abbildung (p_b2_fig3_3_income_X) ----

### Schritt 1: Die Einkommensgerade ----
p_b2_fig3_3_income_1 <- ggplot(tbl_hours, aes(x = x, y = y)) +
  base_scales_hours +
  geom_line(color = isba_blue, linewidth = 1) +
  annotate("text", x = 16.5, y = 520, label = "Einkommen\n(Income)",
           color = isba_blue, fontface = "bold", hjust = 1)

### Schritt 2: Punkt A hinzufügen ----
p_b2_fig3_3_income_2 <- p_b2_fig3_3_income_1 +
  add_pt(tbl_hours, "A", nudge_x = 0.5, nudge_y = 25)

### Schritt 3: Punkt B hinzufügen ----
p_b2_fig3_3_income_3 <- p_b2_fig3_3_income_2 +
  add_pt(tbl_hours, "B", nudge_x = 0.5, nudge_y = 25)


# ============================================================================


# Budgetrestriktion und Feasible Set ----------

## 1. Daten als tibble speichern-----
tbl_budget <- tibble(
  x     = seq(8, 24, by = 1),
  y     = seq(480, 0, length.out = 17),
  label = NA_character_
)

# Hilfs-Tibble für die Punkte C und D
pts_budget <- tibble(
  x     = c(11.5, 20),
  y     = c(430, 75),
  label = c("C", "D")
)

## 2. Basis-Skalen und Theme definieren (analog zu block-01). -------
base_scales_budget <- list(
  scale_x_continuous(limits = c(8, 25), breaks = 8:24, expand = c(0, 0)),
  scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, 100), expand = c(0, 0)),
  labs(x = "Freizeit (Stunden)", y = "Konsumausgaben (€)"),
  theme_isba(theory = TRUE)
)

## 3. Schrittweiser Aufbau der Abbildung (p_b2_fig3_6_budgetX) ----

### Schritt 1: Die Budgetgerade ----
p_b2_fig3_6_budget1 <- ggplot() +
  base_scales_budget +
  geom_line(data = tbl_budget, aes(x = x, y = y),
            color = isba_red, linewidth = 1)

### Schritt 2: Punkte C und D hinzufügen ----
p_b2_fig3_6_budget2 <- p_b2_fig3_6_budget1 +
  add_pt(pts_budget, c("C", "D"), nudge_x = 0.5, nudge_y = 20, color = "black")

### Schritt 3: Fläche (Feasible Set) und Label hinzufügen ----
p_b2_fig3_6_budget3 <- p_b2_fig3_6_budget2 +
  geom_ribbon(data = tbl_budget, aes(x = x, ymin = 0, ymax = y),
              fill = isba_red, alpha = 0.1) +
  annotate("text", x = 13, y = 150, label = "Budgetmenge\n(Feasible set)",
           fontface = "bold", color = "black", size = 4.5)




# ============================================================================

# Kombination Produktion & Konsum ----------

## 1. Daten vorbereiten -----
tbl_combined <- tibble(
  arbeitszeit = 0:24,
  freizeit    = 24 - arbeitszeit,
  konsum      = arbeitszeit * 30 # Annahme: 30€ Stundenlohn
)

## 2. Basis-Skalen -------
base_scales_combined <- list(
  scale_y_continuous(limits = c(0, 750), expand = c(0, 0)),
  theme_isba(theory = TRUE)
)

## 3. Schrittweiser Aufbau (p_b2_fig_combined_X) ----

### Schritt 1: Transformation (Zusammenhang beider Grafiken) ----
p_b2_fig_combined_1 <- ggplot(tbl_combined, aes(x = freizeit, y = konsum)) +
  base_scales_combined +
  scale_x_reverse(limits = c(24, 0), breaks = seq(0, 24, 4),
                  name = "Freizeit (Stunden) <--- vs. ---> Arbeit (Stunden)") +
  geom_line(color = isba_blue, linewidth = 1.2) +
  labs(y = "Konsumausgaben / Einkommen (€)",
       title = "Vom Einkommen zur Budgetmenge") +
  # Hilfs-Achse für Arbeit oben einblenden
  annotate("text", x = 12, y = 720, label = "Tauschverhältnis: 1h Arbeit = 30€ Konsum",
           color = isba_orange, fontface = "bold")

### Schritt 2: Die Budgetmenge markieren ----
p_b2_fig_combined_2 <- p_b2_fig_combined_1 +
  geom_ribbon(aes(ymin = 0, ymax = konsum), fill = isba_blue, alpha = 0.1) +
  annotate("text", x = 18, y = 100, label = "Mögliche Kombinationen\n(Budgetmenge / Feasible set)",
           fontface = "italic", size = 3.5)

# ============================================================================


# Didaktische Kopplung: Arbeit vs. Freizeit (Finale Version) ----------

## 1. Daten vorbereiten -----
tbl_dual <- tibble(
  arbeit   = seq(0, 24, by = 0.5),
  freizeit = 24 - arbeit,
  konsum   = arbeit * 30
)

## 2. Basis-Skalen (Fixiertes Layout & Deutsche Achsen) -------
base_scales_dual <- list(
  scale_y_continuous(limits = c(-250, 800), breaks = seq(0, 700, 100),
                     labels = function(x) ifelse(x <= 0, "", x), expand = c(0, 0)),
  scale_x_continuous(limits = c(0, 26), expand = c(0, 0)),
  labs(y = "Einkommen / Konsumausgaben (€)", x = NULL),
  theme_isba(theory = TRUE),
  theme(
    axis.line   = element_blank(),
    axis.ticks  = element_blank(),
    axis.text.x = element_blank(),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
  ),
  coord_cartesian(xlim = c(0, 25), ylim = c(-250, 750), clip = "off")
)

# Manuelle Hauptachsen (inkl. Ticks und Beschriftungen)
manual_axes <- list(
  annotate("segment", x = 0, xend = 25.5, y = 0, yend = 0,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
           color = isba_blue, linewidth = 0.8),
  annotate("segment", x = 0, xend = 0, y = 0, yend = 760,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
           color = isba_blue, linewidth = 0.8),
  annotate("text", x = seq(0, 24, 4), y = -35, label = seq(0, 24, 4),
           color = isba_gray, size = 3.5),
  annotate("text", x = 12.5, y = -75, label = "Arbeitszeit (Stunden)",
           color = isba_blue, fontface = "bold", size = 4.5)
)

## 3. Schrittweiser Aufbau (p_b2_fig_dual_X) ----

### Schritt 1: Die Einkommenslinie ----
p_b2_fig_dual_1 <- ggplot(tbl_dual, aes(x = arbeit, y = konsum)) +
  base_scales_dual +
  manual_axes +
  geom_line(color = isba_blue, linewidth = 1.2) +
  # Label direkt an das obere Ende der Linie verschoben
  annotate("text", x = 22, y = 700, label = "Einkommen",
           color = isba_blue, fontface = "bold", hjust = 1)

### Schritt 2: Vertikale Linie und „Schlafen“ ----
p_b2_fig_dual_2 <- p_b2_fig_dual_1 +
  annotate("segment", x = 16, xend = 16, y = 0, yend = 750,
           linetype = "dashed", color = isba_red, linewidth = 0.8) +
  annotate("label", x = 16, y = 350, label = "„Schlafen“",
           color = isba_red, fontface = "bold", fill = "white", label.size = 0, hjust = -0.1)

### Schritt 3: Einfärben und Label Budgetmenge (Feasible set) ----
p_b2_fig_dual_3 <- p_b2_fig_dual_2 +
  geom_ribbon(data = subset(tbl_dual, arbeit <= 16),
              aes(ymin = 0, ymax = konsum), fill = isba_red, alpha = 0.1) +
  annotate("text", x = 8, y = 120, label = "Budgetmenge\n(Feasible set)",
           fontface = "bold", color = "black", size = 4.5)

### Schritt 4: Zusätzliche Freizeitachse ----
p_b2_fig_dual_4 <- p_b2_fig_dual_3 +
  annotate("segment", x = 0, xend = 24.5, y = -140, yend = -140,
           arrow = arrow(length = unit(0.2, "cm"), ends = "first", type = "closed"),
           color = isba_blue_soft, linewidth = 0.8) +
  annotate("text", x = seq(0, 24, 4), y = -175,
           label = seq(24, 0, -4), color = isba_blue_soft, size = 3.5) +
  annotate("text", x = 12.5, y = -215, label = "Freizeit (Stunden)",
           color = isba_blue_soft, fontface = "bold", size = 4.5)



# ============================================================================

# Allgemeiner Fall - von Input-Output zu Output-Output-Diagramm ----

# Gemeinsame Konstante: Die begrenzte Ressource (z.B. 100 Arbeitsstunden)
L_max <- 100

# Hilfsfunktion: Verhindert die doppelte Null im Ursprung
hide_zero <- function(x) { ifelse(x == 0, "", x) }

# ============================================================================
# BLOCK 1: Linearer Fall
# ============================================================================

# Gut 1 ist "teuer/aufwändig" (niedrige Produktivität): Y1 = 0.5 * L
# Gut 2 ist "günstig/schnell" (hohe Produktivität): Y2 = 2.0 * L
prod_lin_1 <- function(L) 0.5 * L
prod_lin_2 <- function(L) 2.0 * L

# 1. Daten generieren (Tidy Syntax)
df_b1_lin <- tibble(L_input = seq(0, L_max, length.out = 100)) |>
  mutate(
    Y1_out = prod_lin_1(L_input),
    Y2_out = prod_lin_2(L_input),
    # Für die PPF ordnen wir die gesamte Ressource zwischen L1 und L2 zu
    L1 = L_input,
    L2 = L_max - L_input,
    PPF_Y1 = prod_lin_1(L1),
    PPF_Y2 = prod_lin_2(L2)
  )

# 2. Skalen definieren (Y-Achsen nutzen hide_zero)
scale_L_io   <- scale_x_continuous(limits = c(0, 105), expand = c(0, 0), name = "Input L (z.B. Stunden)")
scale_Y1_out <- scale_y_continuous(limits = c(0, 60),  expand = c(0, 0), name = "Output Gut 1 (Aufwändig)", labels = hide_zero)
scale_Y2_out <- scale_y_continuous(limits = c(0, 220), expand = c(0, 0), name = "Output Gut 2 (Schnell)", labels = hide_zero)
scale_PPF_X  <- scale_x_continuous(limits = c(0, 60),  expand = c(0, 0), name = "Output Gut 1")

### ---------- Schritt 1: Input-Output Gut 1 (Flache Kurve) ----------
p_b1_lin_1 <- ggplot(df_b1_lin, aes(x = L_input, y = Y1_out)) +
  geom_line(linewidth = 1.3, color = isba_orange) +
  scale_L_io + scale_Y1_out +
  theme_isba(theory = TRUE, axis_decoration = TRUE) +
  annotate("text", x = 20, y = 45, label = "Geringe Produktivität\n(Flache Steigung)",
           color = isba_orange, fontface = "bold", hjust = 0)

### ---------- Schritt 2: Input-Output Gut 2 (Steile Kurve) ----------
p_b1_lin_2 <- ggplot(df_b1_lin, aes(x = L_input, y = Y2_out)) +
  geom_line(linewidth = 1.3, color = isba_blue_soft) +
  scale_L_io + scale_Y2_out +
  theme_isba(theory = TRUE, axis_decoration = TRUE) +
  annotate("text", x = 20, y = 180, label = "Hohe Produktivität\n(Steile Steigung)",
           color = isba_blue_soft, fontface = "bold", hjust = 0)

### ---------- Schritt 3: Output-Output (Transformationskurve) ----------
p_b1_lin_3 <- ggplot(df_b1_lin, aes(x = PPF_Y1, y = PPF_Y2)) +
  geom_ribbon(aes(ymin = 0, ymax = PPF_Y2), fill = isba_blue, alpha = 0.08) +
  geom_line(linewidth = 1.3, color = isba_blue) +
  scale_PPF_X + scale_Y2_out +
  theme_isba(theory = TRUE, axis_decoration = TRUE) +
  annotate("label", x = 25, y = 100,
           label = "Hohe Opportunitätskosten für Gut 1:\nUm 1 Einheit Gut 1 zu bekommen,\nmüssen 4 Einheiten Gut 2 aufgegeben werden.",
           color = isba_blue, fill = "white", size = 3.5)


## Optimiert für Quarto Präsentation -------

library(patchwork) # Zwingend erforderlich für das Layout
# Hilfsfunktion: Verhindert die doppelte Null im Ursprung
hide_zero <- function(x) { ifelse(x == 0, "", x) }

L_max <- 100
scale_L_io <- scale_x_continuous(limits = c(0, 105), expand = c(0, 0), name = "Input L")

# ============================================================================
# BLOCK 1: Linearer Fall
# ============================================================================
# Steigungen: 1:1 (Flach) und 2:1 (Steil)
prod_lin_1 <- function(L) 1.0 * L  # Flach (Max 100)
prod_lin_2 <- function(L) 2.0 * L  # Steil (Max 200)

df_b2_lin <- tibble(L_input = seq(0, L_max, length.out = 100)) |>
  mutate(
    Y1_out = prod_lin_1(L_input),
    Y2_out = prod_lin_2(L_input),
    PPF_Y1 = prod_lin_1(L_input),
    PPF_Y2 = prod_lin_2(L_max - L_input)
  )

# Plot 1: Gut 1 (Flach) - Y-Achse bis 200 erzwungen für Vergleichbarkeit
p_base_lin_1 <- ggplot(df_b2_lin, aes(x = L_input, y = Y1_out)) +
  geom_line(linewidth = 1.3, color = isba_orange) +
  scale_L_io +
  scale_y_continuous(limits = c(0, 210), expand = c(0, 0), name = "Output Gut 1", labels = hide_zero) +
  theme_isba(theory = TRUE, axis_decoration = TRUE) +
  labs(subtitle = "Produktionsfunktion Gut 1")

# Plot 2: Gut 2 (Steil) - Y-Achse identisch bis 200
p_base_lin_2 <- ggplot(df_b2_lin, aes(x = L_input, y = Y2_out)) +
  geom_line(linewidth = 1.3, color = isba_blue_soft) +
  scale_L_io +
  scale_y_continuous(limits = c(0, 210), expand = c(0, 0), name = "Output Gut 2", labels = hide_zero) +
  theme_isba(theory = TRUE, axis_decoration = TRUE) +
  labs(subtitle = "Produktionsfunktion Gut 2")

# Plot 3: Transformationskurve (PPF)
p_base_lin_3 <- ggplot(df_b2_lin, aes(x = PPF_Y1, y = PPF_Y2)) +
  geom_ribbon(aes(ymin = 0, ymax = PPF_Y2), fill = isba_blue, alpha = 0.08) +
  geom_line(linewidth = 1.5, color = isba_blue) +
  scale_x_continuous(limits = c(0, 105), expand = c(0, 0), name = "Output Gut 1") +
  scale_y_continuous(limits = c(0, 210), expand = c(0, 0), name = "Output Gut 2", labels = hide_zero) +
  theme_isba(theory = TRUE, axis_decoration = TRUE) +
  labs(subtitle = "Transformationskurve (konstante GRT)")

# --- Layouting mit Operator-Syntax ---
p_b2_mrt_lin_1 <- (p_base_lin_1 / plot_spacer()) | plot_spacer()
p_b2_mrt_lin_2 <- (p_base_lin_1 / p_base_lin_2)  | plot_spacer()
p_b2_mrt_lin_3 <- (p_base_lin_1 / p_base_lin_2)  | p_base_lin_3


# ============================================================================
# BLOCK 2: Nicht-linearer Fall (Konkav)
# ============================================================================
# Steigungen analog: Faktor 10 (Flach, Max 100) vs Faktor 20 (Steil, Max 200)
prod_con_1 <- function(L) 10 * sqrt(L)
prod_con_2 <- function(L) 20 * sqrt(L)

df_b2_con <- tibble(L_input = seq(0, L_max, length.out = 200)) |>
  mutate(
    Y1_out = prod_con_1(L_input),
    Y2_out = prod_con_2(L_input),
    PPF_Y1 = prod_con_1(L_input),
    PPF_Y2 = prod_con_2(L_max - L_input)
  )

# Plot 1: Gut 1 - Y-Achse bis 200 erzwungen
p_base_con_1 <- ggplot(df_b2_con, aes(x = L_input, y = Y1_out)) +
  geom_line(linewidth = 1.3, color = isba_orange) +
  scale_L_io +
  scale_y_continuous(limits = c(0, 210), expand = c(0, 0), name = "Output Gut 1", labels = hide_zero) +
  theme_isba(theory = TRUE, axis_decoration = TRUE) +
  labs(subtitle = "Produktionsfunktion (nichtlinear) Gut 1")

# Plot 2: Gut 2 - Y-Achse identisch bis 200
p_base_con_2 <- ggplot(df_b2_con, aes(x = L_input, y = Y2_out)) +
  geom_line(linewidth = 1.3, color = isba_blue_soft) +
  scale_L_io +
  scale_y_continuous(limits = c(0, 210), expand = c(0, 0), name = "Output Gut 2", labels = hide_zero) +
  theme_isba(theory = TRUE, axis_decoration = TRUE) +
  labs(subtitle = "Produktionsfunktion (nichtlinear) Gut 2")

# Plot 3: Transformationskurve (PPF)
p_base_con_3 <- ggplot(df_b2_con, aes(x = PPF_Y1, y = PPF_Y2)) +
  geom_ribbon(aes(ymin = 0, ymax = PPF_Y2), fill = isba_blue, alpha = 0.08) +
  geom_line(linewidth = 1.5, color = isba_blue) +
  scale_x_continuous(limits = c(0, 105), expand = c(0, 0), name = "Output Gut 1") +
  scale_y_continuous(limits = c(0, 210), expand = c(0, 0), name = "Output Gut 2", labels = hide_zero) +
  theme_isba(theory = TRUE, axis_decoration = TRUE) +
  labs(subtitle = "Transformationskurve (variable GRT)")

# --- Layouting mit Operator-Syntax ---
p_b2_mrt_nichtlin_1 <- (p_base_con_1 / plot_spacer()) | plot_spacer()
p_b2_mrt_nichtlin_2 <- (p_base_con_1 / p_base_con_2)  | plot_spacer()
p_b2_mrt_nichtlin_3 <- (p_base_con_1 / p_base_con_2)  | p_base_con_3


# ============================================================================
# BLOCK 2: Nicht-linearer Fall (Konkav)
# ============================================================================

# Gut 1: Y1 = 12 * sqrt(L) (Maximaler Output: 120)
# Gut 2: Y2 = 30 * sqrt(L) (Maximaler Output: 300)
prod_con_1 <- function(L) 12 * sqrt(L)
prod_con_2 <- function(L) 30 * sqrt(L)

# 1. Daten generieren (Tidy Syntax)
df_b2_con <- tibble(L_input = seq(0, L_max, length.out = 200)) |>
  mutate(
    Y1_out = prod_con_1(L_input),
    Y2_out = prod_con_2(L_input),
    L1 = L_input,
    L2 = L_max - L_input,
    PPF_Y1 = prod_con_1(L1),
    PPF_Y2 = prod_con_2(L2)
  )

# 2. Spezifische Skalen für den konkaven Fall (Y-Achsen nutzen hide_zero)
scale_Y1_con <- scale_y_continuous(limits = c(0, 140), expand = c(0, 0), name = "Output Gut 1", labels = hide_zero)
scale_Y2_con <- scale_y_continuous(limits = c(0, 330), expand = c(0, 0), name = "Output Gut 2", labels = hide_zero)
scale_PPF_con_X <- scale_x_continuous(limits = c(0, 140), expand = c(0, 0), name = "Output Gut 1")

### ---------- Schritt 1: Input-Output Gut 1 ----------
p_b2_con_1 <- ggplot(df_b2_con, aes(x = L_input, y = Y1_out)) +
  geom_line(linewidth = 1.3, color = isba_orange) +
  scale_L_io + scale_Y1_con +
  theme_isba(theory = TRUE, axis_decoration = TRUE) +
  annotate("text", x = 40, y = 120, label = "Abnehmender Ertrag (Gut 1)\nMax: 120",
           color = isba_orange, fontface = "bold")

### ---------- Schritt 2: Input-Output Gut 2 ----------
p_b2_con_2 <- ggplot(df_b2_con, aes(x = L_input, y = Y2_out)) +
  geom_line(linewidth = 1.3, color = isba_blue_soft) +
  scale_L_io + scale_Y2_con +
  theme_isba(theory = TRUE, axis_decoration = TRUE) +
  annotate("text", x = 40, y = 300, label = "Abnehmender Ertrag (Gut 2)\nMax: 300",
           color = isba_blue_soft, fontface = "bold")

### ---------- Schritt 3: Output-Output (Transformationskurve) ----------
# Wir nutzen add_pt (aus dem theme-isba Helper), um die Verschiebung der MRT zu zeigen
pts_mrt_con <- tibble(
  x = c(50, 110),
  y = prod_con_2(L_max - (c(50, 110)/12)^2), # Rückrechnung von Y1 auf L1, dann L2, dann Y2
  label = c("A", "B")
)

p_b2_con_3 <- ggplot(df_b2_con, aes(x = PPF_Y1, y = PPF_Y2)) +
  geom_ribbon(aes(ymin = 0, ymax = PPF_Y2), fill = isba_blue, alpha = 0.08) +
  geom_line(linewidth = 1.3, color = isba_blue) +
  scale_PPF_con_X + scale_Y2_con +
  theme_isba(theory = TRUE, axis_decoration = TRUE) +
  # Einbinden der Hilfsfunktion für Punkte (Voraussetzung: add_pt ist in der R-Session geladen)
  add_pt(pts_mrt_con, c("A", "B"), nudge_x = 5, nudge_y = 15, size_pt = 3) +
  annotate("text", x = 80, y = 280,
           label = "Asymmetrische, konkave Frontier",
           color = isba_blue, fontface = "bold", size = 5)







# ============================================================================
# DEMO: COBB-DOUGLAS-INDIFFERENZKURVEN (Modelldiagramm, theme_isba(theory = TRUE))
#
# Modell: U(l, c) = l^alpha * c^(1 - alpha)
# Zeigt sechs Animationsschritte:
#   1. Punkte B (außerhalb IC2) und A (auf IC2) mit Referenzlinien
#   2. + Punkte C (außerhalb IC2) und D (auf IC2) mit Referenzlinien
#   3. + Punkt E (auf IC2) mit Referenzlinien
#   4. + Punkte F, G, H (auf IC2)
#   5. + IC2 (Indifferenzkurve durch A, E, F, G, H, D)
#   6. + IC1 (niedrigeres Nutzenniveau, durch C) und IC3 (höheres, durch B)
#
# Kalibrierung: alpha = 0.7280 so gewählt, dass A(15, 540) und D(20, 250)
# beide exakt auf der mittleren Indifferenzkurve IC2 liegen.
#
# Theme: theme_isba(theory = TRUE) — Pfeil-Achsen MIT Ticks/Text, weil die
# konkreten Werte (Freizeit-Stunden, Konsum-Euro) sichtbar sein sollen.
# ============================================================================
# Präferenzen ---------
## ---------- Parameter und Funktionen ----------
alpha <- 0.7280

## Nutzenniveaus (durch Ankerpunkte kalibriert)
U_mid  <- 15^alpha * 540^(1 - alpha)   # IC2: durch A, E, F, G, H, D
U_high <- 13^alpha * 540^(1 - alpha)   # IC3: durch B
U_low  <- 20^alpha *  85^(1 - alpha)   # IC1: durch C

## IC-Funktion: c = (U / l^alpha)^(1/(1-alpha))
ic_curve <- function(l, U) (U / l^alpha)^(1 / (1 - alpha))

## ---------- Datenobjekte ----------

### Indifferenzkurven (drei Niveaus)
x_seq <- seq(8.5, 24, length.out = 400)
ic_data <- bind_rows(
  tibble(x = x_seq, y = ic_curve(x_seq, U_high), curve = "IC3"),
  tibble(x = x_seq, y = ic_curve(x_seq, U_mid),  curve = "IC2"),
  tibble(x = x_seq, y = ic_curve(x_seq, U_low),  curve = "IC1")
) |> filter(y >= 0, y <= 670)

### Punkte auf IC2 (mittlere Indifferenzkurve)
pts_ic <- tibble(
  x     = c(15, 16, 17, 18, 19, 20),
  y     = ic_curve(c(15, 16, 17, 18, 19, 20), U_mid),
  label = c("A", "E", "F", "G", "H", "D")
)

### Punkte auf anderen Niveaus
pts_extra <- tibble(
  x     = c(13,  20),
  y     = c(540, 85),
  label = c("B", "C")
)

### Y-Wert von E (für gestrichelte Referenzlinie in Schritt 3)
y_E <- ic_curve(16, U_mid)

### ---------- Gestrichelte Referenzlinien ----------

# Schritt 1: B und A → horizontale Linie auf y=540, vertikale durch x=15
seg_BA <- tibble(
  x    = c(8.5, 15),
  xend = c(15,  15),
  y    = c(540, 0),
  yend = c(540, 540)
)

### Schritt 2: C und D → horizontale Linie auf y=250, vertikale durch x=20
seg_CD <- tibble(
  x    = c(8.5, 20),
  xend = c(20,  20),
  y    = c(250, 0),
  yend = c(250, 250)
)

### Schritt 3: E → horizontale Linie auf y_E, vertikale durch x=16
seg_E <- tibble(
  x    = c(8.5, 16),
  xend = c(16,  16),
  y    = c(y_E, 0),
  yend = c(y_E, y_E)
)

## ---------- Achsen-Limits und gemeinsame Layer ----------

x_lim <- c(8.5, 25)
y_lim <- c(0, 670)

# base_scales: gemeinsame Skalen + Theme für alle Plot-Schritte.
# theme_isba(theory = TRUE) liefert Pfeil-Achsen MIT axis.ticks/axis.text
# — die konkreten Werte sollen sichtbar sein.
base_scales <- list(
  scale_x_continuous(limits = x_lim, expand = c(0, 0)),
  scale_y_continuous(limits = y_lim, expand = c(0, 0)),
  labs(x = "Freizeit pro Tag",
       y = "Konsumausgaben (\u20ac)"),
  theme_isba(theory = TRUE)
)

# Helper für gestrichelte Referenzlinien (zur Vermeidung von Wiederholungen)
dashed_seg <- function(data) {
  geom_segment(data = data,
               aes(x = x, xend = xend, y = y, yend = yend),
               color = isba_gray, linewidth = 0.4, linetype = "dashed")
}


## ---------- Plot-Aufbau ----------
# Hinweis: add_pt() ist in theme-isba.R definiert.
# Pflichtspalten im Datenobjekt: 'x', 'y', 'label'.

### Schritt 1: B und A mit Referenzlinien
p_b2_pref_1 <- ggplot() +
  base_scales +
  dashed_seg(seg_BA) +
  add_pt(pts_extra, "B", nudge_x = -0.4, nudge_y = 15) +
  add_pt(pts_ic,    "A", nudge_x = -0.4, nudge_y = 15)

### Schritt 2: + C und D mit Referenzlinien
p_b2_pref_2 <- p_b2_pref_1 +
  dashed_seg(seg_CD) +
  add_pt(pts_extra, "C", nudge_x =  0.4, nudge_y = -18) +
  add_pt(pts_ic,    "D", nudge_x =  0.4, nudge_y =  15)

### Schritt 3: + E mit Referenzlinien
p_b2_pref_3 <- p_b2_pref_2 +
  dashed_seg(seg_E) +
  add_pt(pts_ic, "E", nudge_x = 0.4, nudge_y = 15)

### Schritt 4: + Punkte F, G, H (alle auf IC2)
p_b2_pref_4 <- p_b2_pref_3 +
  add_pt(pts_ic, c("F", "G", "H"), nudge_x = 0.4, nudge_y = 15)

### Schritt 5: + Indifferenzkurve IC2 (durch A, E, F, G, H, D)
p_b2_pref_5 <- p_b2_pref_4 +
  geom_line(data = filter(ic_data, curve == "IC2"),
            aes(x = x, y = y),
            color = isba_orange, linewidth = 0.9)

### Schritt 6: + IC1 (durch C) und IC3 (durch B)
p_b2_pref_6 <- p_b2_pref_5 +
  geom_line(data = filter(ic_data, curve %in% c("IC1", "IC3")),
            aes(x = x, y = y, group = curve),
            color = isba_orange, linewidth = 0.9)



# ============================================================================

# Abbildungsserie: Unterschiedliche Präferenzordnungen -----
# Basierend auf identischen Konsumplänen (A-H, B, C)

# 1. Gemeinsame Basis-Daten (aus deinem Lehrbuch-Beispiel)
alpha_cd <- 0.7280

# Hilfsfunktion und Skalen
hide_zero <- function(x) { ifelse(x == 0, "", x) }
x_lim <- c(8.5, 25); y_lim <- c(0, 670)

base_pref_scales <- list(
  scale_x_continuous(limits = x_lim, expand = c(0, 0), name = "Freizeit pro Tag"),
  scale_y_continuous(limits = y_lim, expand = c(0, 0), name = "Konsumausgaben (€)", labels = hide_zero),
  theme_isba(theory = TRUE)
)

# Konsumpläne (Punkte) generieren - Y-Werte für A, E, F, G, H, D aus CD-Funktion
U_mid_cd <- 15^alpha_cd * 540^(1 - alpha_cd)

pts_all <- bind_rows(
  tibble(x = c(15, 16, 17, 18, 19, 20), label = c("A", "E", "F", "G", "H", "D")),
  tibble(x = 13, y = 540, label = "B"),
  tibble(x = 20, y = 85,  label = "C")
) |>
  mutate(y = ifelse(is.na(y), (U_mid_cd / x^alpha_cd)^(1 / (1 - alpha_cd)), y))


## 2. Cobb-Douglas Kurvenschar (Konvexe ICs) ------
# 4 Nutzenniveaus (durch C, durch B, durch A/D, und ein höheres)
U_C_cd    <- 20^alpha_cd * 85^(1 - alpha_cd)
U_B_cd    <- 13^alpha_cd * 540^(1 - alpha_cd)
U_high_cd <- 18^alpha_cd * 600^(1 - alpha_cd)

ic_cd_data <- expand_grid(
  x = seq(8.5, 25, length.out = 300),
  U = c(U_C_cd, U_B_cd, U_mid_cd, U_high_cd)
) |>
  mutate(
    y = (U / x^alpha_cd)^(1 / (1 - alpha_cd)),
    curve = as.factor(U)
  )

p_b2_pref_type_cd <- ggplot() +
  base_pref_scales +
  geom_line(data = ic_cd_data, aes(x = x, y = y, group = curve), color = isba_orange, linewidth = 1) +
  add_pt(pts_all, labels = pts_all$label, nudge_y = 20) +
  labs(subtitle = "Cobb-Douglas-Funktion")


## 3. Lineare Kurvenschar (Perfekte Substitute) --------
# Steigung m = -58 (Gerade durch A und D).
# y = m*x + b -> b = y - m*x. Wir berechnen die Y-Achsenabschnitte für 4 Kurven:
m_lin <- -58
intercepts_lin <- c(
  85  - (m_lin * 20),  # Kurve durch C
  540 - (m_lin * 13),  # Kurve durch B
  540 - (m_lin * 15),  # Kurve durch A und D (IC_mid)
  1550                 # Höhere Außenkurve
)

p_b2_pref_type_lin <- ggplot() +
  base_pref_scales +
  geom_abline(intercept = intercepts_lin, slope = m_lin, color = isba_orange, linewidth = 1) +
  add_pt(pts_all, labels = pts_all$label, nudge_y = 20) +
  labs(subtitle = "Lineare Funktion")


## 4. Leontief Kurvenschar (Perfekte Komplemente) ------
create_leontief <- function(x_kink) {
  y_kink <- x_kink * (250 / 15)

  # Vom oberen Rand (670) zum Knick, dann zum rechten Rand (25)
  tibble(
    x = c(x_kink, x_kink, 25),
    y = c(670, y_kink, y_kink),
    curve = paste0("Kink_", x_kink)
  )
}

df_leo <- bind_rows(
  create_leontief(10),  # Untere Kurve (Knick gut sichtbar)
  create_leontief(13),  # Geht exakt durch Punkt B(13, 540) auf dem vertikalen Ast
  create_leontief(15),  # IC_mid: Durch A(15, 540) (vertikal) und D(20, 250) (horizontal)
  create_leontief(18)   # Höhere Außenkurve
)

p_b2_pref_type_leo <- ggplot() +
  base_pref_scales +
  geom_path(data = df_leo, aes(x = x, y = y, group = curve), color = isba_orange, linewidth = 1) +
  add_pt(pts_all, labels = pts_all$label, nudge_y = 20) +
  labs(subtitle = "Leontief-Funktion")


# ============================================================================

# Optimierung: Die Wahl der Arbeitszeit (Karim's Entscheidung) ----------

## 1. Daten vorbereiten -----

# 1.1 Budgetgerade: Einkommen = Lohn * (24 - Freizeit)
# Wir nehmen an: 30€/h Lohn
tbl_budget <- tibble(
  t = seq(8, 24, length.out = 100),
  c = 30 * (24 - t)
)

# 1.2 Indifferenzkurven berechnen (Cobb-Douglas Log-Nutzenfunktion)
# Funktion zur Berechnung des Konsums c in Abhängigkeit von Freizeit t und Nutzen U
calc_ic <- function(t, U) exp((U - 17 * log(t)) / 7)

# Gitter für die Freizeit (x-Achse) erstellen und Kurven berechnen
tbl_ic <- tibble(t = seq(8, 24, by = 0.1)) %>%
  mutate(
    IC1 = calc_ic(t, 81.2177), # Geht exakt durch A und B
    IC2 = calc_ic(t, 83.3236), # Geht exakt durch C und D
    IC3 = calc_ic(t, 85.5932), # Tangiert exakt in E
    IC4 = calc_ic(t, 87.5000)  # Unerreichbares höheres Nutzenniveau
  ) %>%
  pivot_longer(cols = starts_with("IC"), names_to = "curve", values_to = "c") %>%
  filter(c >= 0, c <= 650) # Schneidet Kurven optisch am Rand ab

# 1.3 Exakte Punkte für die didaktischen Schritte definieren (GEFIXTE LABEL-POSITIONEN)
tbl_pts <- tibble(
  name    = c("A", "B", "C", "D", "E"),
  t       = c(22, 9.85, 15.5, 11.88, 17),        # X-Koordinaten (Freizeit)
  c       = c(60, 423.1, 188.0, 363, 210),    # Y-Koordinaten (Konsum)
  # Neue Label-Positionen: näher am Punkt, links/unter für A-D, rechts/über für E
  label_x = c(21.7, 9.58, 15.2, 11.7, 17.3),   # X-Position des Labels
  label_y = c(50, 413.6, 178.0, 350, 220)         # Y-Position des Labels
)

## 2. Basis-Skalen definieren -------
base_scales_opt <- list(
  # X-Achse beginnt direkt bei 8 (biologisches Minimum / Schlafen)
  scale_x_continuous(limits = c(8, 25), breaks = seq(8, 24, 1), expand = c(0, 0)),
  scale_y_continuous(limits = c(0, 650), breaks = seq(0, 600, 100), expand = c(0, 0)),
  labs(x = "Freizeit (Stunden)", y = "Einkommen / Konsumausgaben (€)"),
  theme_isba(theory = TRUE),
  # clip = "off" verhindert das Abschneiden von Pfeilspitzen am Rand
  coord_cartesian(xlim = c(8, 24.5), ylim = c(0, 630), clip = "off")
)


## 3. Schrittweiser Aufbau der Abbildungen (p_b2_fig3_7a_optimierung_X) ----

### Schritt 1: Nur die Budgetgerade (Zur Erklärung der MRT) ----
p_b2_fig3_7a_optimierung_1 <- ggplot() +
  base_scales_opt +
  geom_line(data = tbl_budget, aes(x = t, y = c), color = isba_red, linewidth = 1.2) +
  annotate("text", x = 1, y = 600, label = "Budgetgerade\n(MRT)",
           color = isba_red, fontface = "bold", hjust = 0)

### Schritt 2: Indifferenzkurven hinzufügen (Präferenzen) ----
p_b2_fig3_7a_optimierung_2 <- p_b2_fig3_7a_optimierung_1 +
  geom_line(data = tbl_ic, aes(x = t, y = c, group = curve),
            color = isba_blue, linewidth = 0.8) +
  # parse = TRUE wandelt IC[1] in einen tiefgestellten Index um (IC₁)
  annotate("text", x = 8.8,  y = 620, label = "IC[1]", parse = TRUE, color = isba_blue) +
  annotate("text", x = 10.2, y = 620, label = "IC[2]", parse = TRUE, color = isba_blue) +
  annotate("text", x = 11.6, y = 620, label = "IC[3]", parse = TRUE, color = isba_blue) +
  annotate("text", x = 12.9, y = 620, label = "IC[4]", parse = TRUE, color = isba_blue)

### Schritt 3: Punkte A und B (Niedrigstes Nutzenniveau auf der Budgetgeraden) ----
p_b2_fig3_7a_optimierung_3 <- p_b2_fig3_7a_optimierung_2 +
  geom_point(data = filter(tbl_pts, name %in% c("A", "B")),
             aes(x = t, y = c), size = 3) +
  # geom_text_repel() wäre auch eine Option, aber hier haben wir spezifische label_x/label_y
  geom_text(data = filter(tbl_pts, name %in% c("A", "B")),
            aes(x = label_x, y = label_y, label = name), size = 5)

### Schritt 4: Punkte C und D (Höheres Nutzenniveau entdecken) ----
p_b2_fig3_7a_optimierung_4 <- p_b2_fig3_7a_optimierung_3 +
  geom_point(data = filter(tbl_pts, name %in% c("C", "D")),
             aes(x = t, y = c), size = 3) +
  geom_text(data = filter(tbl_pts, name %in% c("C", "D")),
            aes(x = label_x, y = label_y, label = name), size = 5)

### Schritt 5: Punkt E (Das Optimum) ----
p_b2_fig3_7a_optimierung_5 <- p_b2_fig3_7a_optimierung_4 +
  geom_point(data = filter(tbl_pts, name == "E"),
             aes(x = t, y = c), size = 3) +
  geom_text(data = filter(tbl_pts, name == "E"),
            aes(x = label_x, y = label_y, label = name), size = 5)

### Schritt 6: Didaktische Hervorhebung der Tangentialbedingung ----
p_b2_fig3_7a_optimierung_6 <- p_b2_fig3_7a_optimierung_5 +
  # Wir nutzen annotate() mit dem Typ "richtext"
  annotate("richtext",
           x = 19.5, y = 350,
           label = "**Im Optimum:**<br>Grenzrate der Transformation (MRT)<br>=<br>Grenzrate der Substitution (MRS)",
           fill  = alpha(isba_blue, 0.05),
           color = isba_blue,
           label.color = isba_blue, # Bei richtext steuert dies die Rahmenfarbe
           label.size = 0.5,
           size = 4,
           lineheight = 1.5) +
  # Das Ausrufezeichen bleibt als Standard-Text
  annotate("text", x = 19.5, y = 349, label = "!",
           color = isba_blue, fontface = "bold", size = 3.5)


# ============================================================================

# Komparative Statik: Lohnänderung und neue Entscheidung ----------

## 1. Daten vorbereiten -----

# 1.1 Zwei Budgetgeraden: Lohn 1 = 30€, Lohn 2 = 45€
tbl_budget_comp <- tibble(
  t = seq(8, 24, length.out = 100),
  c1 = 30 * (24 - t),
  c2 = 45 * (24 - t)
)

# 1.2 Indifferenzkurven berechnen (Angepasste Stone-Geary-Nutzenfunktion)
# Diese Funktion stellt sicher, dass die Kurven in E und F mathematisch echte Tangenten sind.
calc_ic_comp <- function(t, U) 58 + exp(U - 3.355 * log(t))

tbl_ic_comp <- tibble(t = seq(8, 24, by = 0.1)) %>%
  mutate(
    IC1 = calc_ic_comp(t, 14.528), # Tangiert exakt in E (Lohn 30)
    IC2 = calc_ic_comp(t, 15.059)  # Tangiert exakt in F (Lohn 45)
  ) %>%
  pivot_longer(cols = starts_with("IC"), names_to = "curve", values_to = "c") %>%
  filter(c >= 0, c <= 650) # Optischer Beschnitt am oberen Rand

# 1.3 Exakte Punkte E und F
tbl_pts_comp <- tibble(
  name    = c("E", "F"),
  t       = c(17, 17.5),
  c       = c(210, 292.5),
  label_x = c(16.5, 17.8), # F-Label leicht nach rechts/oben, E-Label nach links/unten
  label_y = c(190, 310)
)

## 2. Basis-Skalen definieren -------
base_scales_comp <- list(
  scale_x_continuous(limits = c(8, 25), breaks = seq(8, 24, 1), expand = c(0, 0)),
  scale_y_continuous(limits = c(0, 650), breaks = seq(0, 600, 100), expand = c(0, 0)),
  labs(x = "Freizeit (Stunden)", y = "Einkommen / Konsumausgaben (€)"),
  theme_isba(theory = TRUE),
  coord_cartesian(xlim = c(8, 24.5), ylim = c(0, 630), clip = "off")
)

## 3. Schrittweiser Aufbau (p_b2_fig3_9_kompstat_X) ----

### Schritt 1: Ausgangssituation (Alter Lohn, Punkt E) ----
p_b2_fig3_9_kompstat_1 <- ggplot() +
  base_scales_comp +
  # Alte Budgetgerade
  geom_line(data = tbl_budget_comp, aes(x = t, y = c1),
            color = isba_red, linewidth = 1.2) +
  # Alte Indifferenzkurve (IC1)
  geom_line(data = filter(tbl_ic_comp, curve == "IC1"), aes(x = t, y = c),
            color = isba_blue, linewidth = 0.8) +
  # Punkt E
  geom_point(data = filter(tbl_pts_comp, name == "E"), aes(x = t, y = c), size = 3) +
  geom_text(data = filter(tbl_pts_comp, name == "E"), aes(x = label_x, y = label_y, label = name), size = 5)

### Schritt 2: Lohnerhöhung (Neue Budgetgerade schwenkt nach oben) ----
p_b2_fig3_9_kompstat_2 <- p_b2_fig3_9_kompstat_1 +
  # Neue Budgetgerade hinzufügen
  geom_line(data = tbl_budget_comp, aes(x = t, y = c2),
            color = isba_red, linewidth = 1.2)

### Schritt 3: Neue Entscheidung (Höherer Nutzen, Punkt F) ----
p_b2_fig3_9_kompstat_3 <- p_b2_fig3_9_kompstat_2 +
  # Neue Indifferenzkurve (IC2)
  geom_line(data = filter(tbl_ic_comp, curve == "IC2"), aes(x = t, y = c),
            color = isba_blue, linewidth = 0.8) +
  # Punkt F
  geom_point(data = filter(tbl_pts_comp, name == "F"), aes(x = t, y = c), size = 3) +
  geom_text(data = filter(tbl_pts_comp, name == "F"), aes(x = label_x, y = label_y, label = name), size = 5)


# ============================================================================


# Jährliche Perspektive: Budgetgerade und Budgetmenge (70 Tage) ----------

## 1. Daten vorbereiten -----
# Maximal 70 freie Tage, Tageslohn = 90€
# Konsum = 90 * (70 - Freie Tage)
tbl_budget_annual <- tibble(
  T = seq(0, 70, length.out = 100),
  C = 90 * (70 - T)
)

## 2. Basis-Skalen definieren -------
base_scales_annual <- list(
  # X-Achse: 0 bis 70 freie Tage
  scale_x_continuous(limits = c(0, 75), breaks = seq(0, 70, 10), expand = c(0, 0)),

  # Y-Achse: Max. Konsum ist 6.300€ (70 * 90), wir skalieren bis 7.000€
  scale_y_continuous(limits = c(0, 7000), breaks = seq(0, 7000, 1000), expand = c(0, 0),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE)),

  # Die von dir gewünschten Achsenbeschriftungen
  labs(x = "Freie Tage pro Jahr", y = "Konsumausgaben (€)"),

  theme_isba(theory = TRUE),

  # clip = "off" verhindert das Abschneiden der Achsenpfeile
  coord_cartesian(xlim = c(0, 80), ylim = c(0, 7200), clip = "off")
)

## 3. Schrittweiser Aufbau (p_b2_fig3_10_annual_X) ----

### Schritt 1: Die Budgetgerade (MRT = 90€/Tag) ----
p_b2_fig3_10_annual_1 <- ggplot() +
  base_scales_annual +
  geom_line(data = tbl_budget_annual, aes(x = T, y = C),
            color = isba_red, linewidth = 1.2) +
  # Label direkt an die obere Kante der Gerade gesetzt
  annotate("text", x = 5, y = 6100, label = "Budgetgerade",
           color = isba_red, fontface = "bold", hjust = 0)

### Schritt 2: Die Budgetmenge (Feasible set) schraffieren ----
p_b2_fig3_10_annual_2 <- p_b2_fig3_10_annual_1 +
  geom_ribbon(data = tbl_budget_annual, aes(x = T, ymin = 0, ymax = C),
              fill = isba_blue, alpha = 0.1) +
  # Zentriertes Label im Bereich des Möglichen
  annotate("text", x = 25, y = 2000, label = "Budgetmenge\n(Feasible set)",
           fontface = "bold", color = isba_blue, size = 5)


# ============================================================================

# Reine Einkommenseffekte bei verschiedenen Präferenztypen (Konsolidiert) ----------

## 1. Gemeinsame Basis-Daten & Skalen -----

# Budgetgeraden (Reiner Einkommenseffekt = Parallelverschiebung nach oben)
# BC1: Reguläres Budget -> C = 6300 - 90*T
# BC2: Zusätzliches Einkommen von 1000€ -> C = 7300 - 90*T
tbl_budget_ee <- tibble(
  T  = seq(0, 75, length.out = 100),
  C1 = 6300 - 90 * T,
  C2 = 7300 - 90 * T
)

# Gemeinsame Achsen-Skalen für perfekte Vergleichbarkeit
base_scales_ee <- list(
  scale_x_continuous(limits = c(0, 75), breaks = seq(0, 70, 10), expand = c(0, 0)),
  scale_y_continuous(limits = c(0, 8500), breaks = seq(0, 8000, 1000), expand = c(0, 0),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE)),
  labs(x = "Freie Tage", y = "Konsumausgaben (€)"),
  theme_isba(theory = TRUE),
  coord_cartesian(xlim = c(0, 70), ylim = c(0, 8000), clip = "off")
)


## 2. Plot 1: Normales Gut (Cobb-Douglas Präferenzen) ----
# Mehr Einkommen = Mehr Freizeit und mehr Konsum

tbl_ic_p1 <- tibble(T = seq(5, 75, by = 0.5)) %>%
  mutate(
    IC1 = 110250 / T, # Tangiert exakt bei A (T = 35)
    IC2 = 148028 / T  # Tangiert exakt bei B (T = 40.56)
  ) %>%
  pivot_longer(cols = starts_with("IC"), names_to = "curve", values_to = "C") %>%
  filter(C >= 0, C <= 8500)

tbl_pts_p1 <- tibble(
  name  = c("A", "B"),
  T     = c(35, 40.56),
  C     = c(3150, 3650),
  lbl_T = c(34.2, 39.8),
  lbl_C = c(2900, 3400)
)

p_b2_fig3_11_ee <- ggplot() +
  base_scales_ee +
  geom_line(data = tbl_budget_ee, aes(x = T, y = C1), color = isba_red, linewidth = 1.2) +
  geom_line(data = tbl_budget_ee, aes(x = T, y = C2), color = isba_red, linewidth = 1.2) +
  geom_line(data = tbl_ic_p1, aes(x = T, y = C, group = curve), color = isba_blue, linewidth = 0.8) +
  geom_point(data = tbl_pts_p1, aes(x = T, y = C), size = 3) +
  geom_text(data = tbl_pts_p1, aes(x = lbl_T, y = lbl_C, label = name), size = 5)


## 3. Plot 2: Quasilineare Präferenzen (Null Einkommenseffekt) ----
# Mehr Einkommen = 100% in Konsum, Freizeit bleibt konstant bei 35

tbl_ic_p2 <- tibble(T = seq(5, 75, by = 0.5)) %>%
  mutate(
    IC1 = 3150 - 90*(T - 35) + 3*(T - 35)^2, # Tangiert bei A (35)
    IC2 = 4150 - 90*(T - 35) + 3*(T - 35)^2  # Tangiert exakt senkrecht darüber bei B (35)
  ) %>%
  pivot_longer(cols = starts_with("IC"), names_to = "curve", values_to = "C") %>%
  filter(C >= 0, C <= 8500)

tbl_pts_p2 <- tibble(
  name  = c("A", "B"),
  T     = c(35, 35),
  C     = c(3150, 4150),
  lbl_T = c(34.2, 34.2),
  lbl_C = c(2900, 3900)
)

p_b2_fig3_12_ee <- ggplot() +
  base_scales_ee +
  geom_line(data = tbl_budget_ee, aes(x = T, y = C1), color = isba_red, linewidth = 1.2) +
  geom_line(data = tbl_budget_ee, aes(x = T, y = C2), color = isba_red, linewidth = 1.2) +
  geom_line(data = tbl_ic_p2, aes(x = T, y = C, group = curve), color = isba_blue, linewidth = 0.8) +
  geom_point(data = tbl_pts_p2, aes(x = T, y = C), size = 3) +
  geom_text(data = tbl_pts_p2, aes(x = lbl_T, y = lbl_C, label = name), size = 5)


## 4. Plot 3: Inferiores Gut (Negativer Einkommenseffekt) ----
# Mehr Einkommen = Freizeit sinkt, da mehr gearbeitet wird (eher theoretischer Sonderfall)

tbl_ic_p3 <- tibble(T = seq(5, 75, by = 0.5)) %>%
  mutate(
    IC1 = 3150 - 90*(T - 35) + 3*(T - 35)^2, # Tangiert bei A (35)
    IC2 = 4600 - 90*(T - 30) + 3*(T - 30)^2  # Tangiert weiter links oben bei B (30)
  ) %>%
  pivot_longer(cols = starts_with("IC"), names_to = "curve", values_to = "C") %>%
  filter(C >= 0, C <= 8500)

tbl_pts_p3 <- tibble(
  name  = c("A", "B"),
  T     = c(35, 30),
  C     = c(3150, 4600),
  lbl_T = c(34.2, 29.2),
  lbl_C = c(2900, 4350)
)

p_b2_own_ee <- ggplot() +
  base_scales_ee +
  geom_line(data = tbl_budget_ee, aes(x = T, y = C1), color = isba_red, linewidth = 1.2) +
  geom_line(data = tbl_budget_ee, aes(x = T, y = C2), color = isba_red, linewidth = 1.2) +
  geom_line(data = tbl_ic_p3, aes(x = T, y = C, group = curve), color = isba_blue, linewidth = 0.8) +
  geom_point(data = tbl_pts_p3, aes(x = T, y = C), size = 3) +
  geom_text(data = tbl_pts_p3, aes(x = lbl_T, y = lbl_C, label = name), size = 5)

# ============================================================================


# Hicks-Zerlegung: Einkommens- und Substitutionseffekt (Didaktisch optimiert) ----------
# Äquivalente Variation (altes Preisverhältnis)

## 1. Daten und Skalen vorbereiten -----

tbl_budget_hicks <- tibble(
  T = seq(0, 75, length.out = 100),
  C1 = 6300 - 90 * T,
  C2 = 10500 - 150 * T,
  C_comp = 8340 - 90 * T
)

tbl_ic_hicks <- tibble(T = seq(10, 75, by = 0.5)) %>%
  mutate(
    IC1 = 2.5 * T^2 - 255 * T + 9022.5,
    IC2 = 2.5 * T^2 - 300 * T + 12750
  ) %>%
  pivot_longer(cols = starts_with("IC"), names_to = "curve", values_to = "C") %>%
  filter(C >= 0, C <= 8500)

tbl_pts_hicks <- tibble(
  name  = c("A", "C", "D"),
  T     = c(33, 42, 30),
  C     = c(3330, 4560, 6000),
  lbl_T = c(36,   46,   32),
  lbl_C = c(3400, 4450, 6200)
)

isba_green <- "#2ca25f"

base_scales_hicks <- list(
  scale_x_continuous(limits = c(0, 75), breaks = seq(0, 70, 10), expand = c(0, 0)),
  scale_y_continuous(limits = c(0, 8500), breaks = seq(0, 8000, 1000), expand = c(0, 0),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE)),
  labs(x = "Freie Tage", y = "Konsumausgaben (€)"),
  theme_isba(theory = TRUE),
  coord_cartesian(xlim = c(0, 78), ylim = c(0, 8500), clip = "off")
)

## 2. Modulare Bausteine für die Zwischenschritte definieren -----

# Hilfslinien (vertikal, gepunktet)
line_A <- geom_vline(xintercept = 33, linetype = "dotted", color = "darkgray", linewidth = 0.6)
line_C <- geom_vline(xintercept = 42, linetype = "dotted", color = "darkgray", linewidth = 0.6)
line_D <- geom_vline(xintercept = 30, linetype = "dotted", color = "darkgray", linewidth = 0.6)

# Pfeile und Labels für die Effekte
arrow_gesamt <- list(
  annotate("segment", x = 33, xend = 30.5, y = 2000, yend = 2000, color = "darkgray", linewidth = 0.8, arrow = arrow(length = unit(0.2, "cm"), type = "closed")),
  annotate("text", x = 31.5, y = 1700, label = "Gesamteffekt", color = "darkgray", fontface = "bold", size = 4.5)
)

arrow_inc <- list(
  annotate("segment", x = 33, xend = 41.5, y = 4000, yend = 4000, color = isba_green, linewidth = 0.8, arrow = arrow(length = unit(0.2, "cm"), type = "closed")),
  annotate("text", x = 37.5, y = 3700, label = "Einkommenseffekt", color = isba_green, fontface = "bold", size = 4.5)
)

arrow_sub <- list(
  annotate("segment", x = 42, xend = 30.5, y = 6800, yend = 6800, color = isba_green, linewidth = 0.8, arrow = arrow(length = unit(0.2, "cm"), type = "closed")),
  annotate("text", x = 38.5, y = 6500, label = "Substitutionseffekt", color = isba_green, fontface = "bold", size = 4.5) # Label nach rechts verschoben
)

# Kompensations-Elemente (Linie und Punkt C)
comp_elements <- list(
  geom_line(data = tbl_budget_hicks, aes(x = T, y = C_comp), color = isba_red, linewidth = 0.8, linetype = "dashed"),
  geom_point(data = filter(tbl_pts_hicks, name == "C"), aes(x = T, y = C), size = 3),
  geom_text(data = filter(tbl_pts_hicks, name == "C"), aes(x = lbl_T, y = lbl_C, label = name), size = 5)
)


## 3. Die 6 didaktischen Schritte rendern (p_b2_fig3_13b_hicks_X) ----

### SCHRITT 1: Budgetmenge, IC1, Punkt A, Neue Budgetgerade
p_b2_fig3_13b_hicks_1 <- ggplot() +
  base_scales_hicks +
  geom_ribbon(data = tbl_budget_hicks, aes(x = T, ymin = 0, ymax = C1), fill = isba_red, alpha = 0.05) +
  geom_line(data = tbl_budget_hicks, aes(x = T, y = C1), color = isba_red, linewidth = 1.2) +
  geom_line(data = tbl_budget_hicks, aes(x = T, y = C2), color = isba_red, linewidth = 1.2) +
  geom_line(data = filter(tbl_ic_hicks, curve == "IC1"), aes(x = T, y = C), color = isba_blue, linewidth = 0.8) +
  annotate("label", x = 16, y = 6200, label = "IC[1]", parse = TRUE, color = isba_blue, fill = "white", label.size = 0) +
  geom_point(data = filter(tbl_pts_hicks, name == "A"), aes(x = T, y = C), size = 3) +
  geom_text(data = filter(tbl_pts_hicks, name == "A"), aes(x = lbl_T, y = lbl_C, label = name), size = 5) +
  # Budgetmenge-Label auf y=2600 angehoben
  annotate("label", x = 18, y = 2600, label = "Budgetmenge", fontface = "bold", color = isba_red, fill = "white", label.size = 0, size = 4.5)

### SCHRITT 2: Zweite Indifferenzkurve und neues Optimum D
p_b2_fig3_13b_hicks_2 <- p_b2_fig3_13b_hicks_1 +
  geom_line(data = filter(tbl_ic_hicks, curve == "IC2"), aes(x = T, y = C), color = isba_blue, linewidth = 0.8) +
  annotate("label", x = 20, y = 8200, label = "IC[2]", parse = TRUE, color = isba_blue, fill = "white", label.size = 0) +
  geom_point(data = filter(tbl_pts_hicks, name == "D"), aes(x = T, y = C), size = 3) +
  geom_text(data = filter(tbl_pts_hicks, name == "D"), aes(x = lbl_T, y = lbl_C, label = name), size = 5)

### SCHRITT 3: Gesamteffekt (Vertikale Linien A & D + Pfeil)
p_b2_fig3_13b_hicks_3 <- p_b2_fig3_13b_hicks_2 +
  line_A + line_D + arrow_gesamt

### SCHRITT 4: Einkommenseffekt (Kompensierte Linie, Punkt C, Vertikale Linien A & C + Pfeil)
p_b2_fig3_13b_hicks_4 <- p_b2_fig3_13b_hicks_2 +
  comp_elements + line_A + line_C + arrow_inc

### SCHRITT 5: Substitutionseffekt (Vertikale Linien C & D + Pfeile für Inc & Sub)
p_b2_fig3_13b_hicks_5 <- p_b2_fig3_13b_hicks_2 +
  comp_elements + line_C + line_D + arrow_inc + arrow_sub

### SCHRITT 6: Finales Bild (Alle Effekte ohne Hilfslinien)
p_b2_fig3_13b_hicks_6 <- p_b2_fig3_13b_hicks_2 +
  comp_elements + arrow_inc + arrow_sub + arrow_gesamt


# Slutsky-Zerlegung: Einkommens- und Substitutionseffekt (EV-Ansatz mit Hicks-Referenz) ----------
# Äquivalente Variation (altes Preisverhältnis)

## 1. Daten und Skalen vorbereiten -----

tbl_budget_slutsky <- tibble(
  T = seq(0, 75, length.out = 100),
  C1 = 6300 - 90 * T,
  C2 = 10500 - 150 * T,
  C_comp = 8700 - 90 * T, # Slutsky-Linie (Steigung 90, durch D)
  C_hicks = 8340 - 90 * T # Hicks-Linie (Referenz)
)

# Mathematisch exakte Kalibrierung für Tangentialpunkte
tbl_ic_slutsky <- tibble(T = seq(10, 75, by = 0.5)) %>%
  mutate(
    IC1 = 2.5 * T^2 - 255 * T + 9022.5,
    IC2 = 2.5 * T^2 - 300 * T + 12750,
    IC3 = 2.5 * T^2 - 310 * T + 13540    # Tangiert Slutsky-Linie exakt in C
  ) %>%
  pivot_longer(cols = starts_with("IC"), names_to = "curve", values_to = "C") %>%
  filter(C >= 0, C <= 8500)

tbl_pts_slutsky <- tibble(
  name  = c("A", "C", "D", "C (Hicks)"),
  T     = c(33, 44, 30, 42),
  C     = c(3330, 4740, 6000, 4560),
  lbl_T = c(36,   47.5, 32,   45.5),
  lbl_C = c(3400, 4850, 6200, 4350)
)

isba_green <- "#2ca25f"
isba_gray  <- "darkgray"

base_scales_slutsky <- list(
  scale_x_continuous(limits = c(0, 75), breaks = seq(0, 70, 10), expand = c(0, 0)),
  scale_y_continuous(limits = c(0, 8500), breaks = seq(0, 8000, 1000), expand = c(0, 0),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE)),
  labs(x = "Freie Tage", y = "Konsumausgaben (€)"),
  theme_isba(theory = TRUE),
  coord_cartesian(xlim = c(0, 70), ylim = c(0, 8000), clip = "off")
)

## 2. Modulare Bausteine -----

line_A <- geom_vline(xintercept = 33, linetype = "dotted", color = isba_gray, linewidth = 0.6)
line_C <- geom_vline(xintercept = 44, linetype = "dotted", color = isba_gray, linewidth = 0.6)
line_D <- geom_vline(xintercept = 30, linetype = "dotted", color = isba_gray, linewidth = 0.6)

arrow_gesamt <- list(
  annotate("segment", x = 33, xend = 30.5, y = 1600, yend = 1600, color = isba_gray, linewidth = 0.8, arrow = arrow(length = unit(0.2, "cm"), type = "closed")),
  annotate("text", x = 31.5, y = 1300, label = "Gesamteffekt", color = isba_gray, fontface = "bold", size = 4.5)
)

arrow_inc <- list(
  annotate("segment", x = 33, xend = 43.5, y = 4000, yend = 4000, color = isba_green, linewidth = 0.8, arrow = arrow(length = unit(0.2, "cm"), type = "closed")),
  annotate("text", x = 37.5, y = 3700, label = "Einkommenseffekt", color = isba_green, fontface = "bold", size = 4.5)
)

arrow_sub <- list(
  annotate("segment", x = 44, xend = 30.5, y = 6800, yend = 6800, color = isba_green, linewidth = 0.8, arrow = arrow(length = unit(0.2, "cm"), type = "closed")),
  annotate("text", x = 38.5, y = 6500, label = "Substitutionseffekt", color = isba_green, fontface = "bold", size = 4.5)
)

## 3. Die 7 Schritte (p_b2_own_slutsky_X) ----

# SCHRITT 1: Budgetmenge, IC1, Punkt A, Neue BC
p_b2_own_slutsky_1 <- ggplot() +
  base_scales_slutsky +
  geom_ribbon(data = tbl_budget_slutsky, aes(x = T, ymin = 0, ymax = C1), fill = isba_red, alpha = 0.05) +
  geom_line(data = tbl_budget_slutsky, aes(x = T, y = C1), color = isba_red, linewidth = 1.2) +
  geom_line(data = tbl_budget_slutsky, aes(x = T, y = C2), color = isba_red, linewidth = 1.2) +
  geom_line(data = filter(tbl_ic_slutsky, curve == "IC1"), aes(x = T, y = C), color = isba_blue, linewidth = 0.8) +
  annotate("label", x = 14, y = 5800, label = "IC[1]", parse = TRUE, color = isba_blue, fill = "white", label.size = 0) +
  geom_point(data = filter(tbl_pts_slutsky, name == "A"), aes(x = T, y = C), size = 3) +
  geom_text(data = filter(tbl_pts_slutsky, name == "A"), aes(x = lbl_T, y = lbl_C, label = name), size = 5) +
  annotate("label", x = 18, y = 2600, label = "Budgetmenge", fontface = "bold", color = isba_red, fill = "white", label.size = 0, size = 4.5)

# SCHRITT 2: IC2 und Optimum D
p_b2_own_slutsky_2 <- p_b2_own_slutsky_1 +
  geom_line(data = filter(tbl_ic_slutsky, curve == "IC2"), aes(x = T, y = C), color = isba_blue, linewidth = 0.8) +
  annotate("label", x = 20, y = 8000, label = "IC[2]", parse = TRUE, color = isba_blue, fill = "white", label.size = 0) +
  geom_point(data = filter(tbl_pts_slutsky, name == "D"), aes(x = T, y = C), size = 3) +
  geom_text(data = filter(tbl_pts_slutsky, name == "D"), aes(x = lbl_T, y = lbl_C, label = name), size = 5)

# SCHRITT 3: Gesamteffekt (Dotted lines A & D)
p_b2_own_slutsky_3 <- p_b2_own_slutsky_2 + line_A + line_D + arrow_gesamt

# SCHRITT 4: Slutsky-Kompensation, IC3, Punkt C & Einkommenseffekt (Linien A & C)
p_b2_own_slutsky_4 <- p_b2_own_slutsky_2 +
  geom_line(data = tbl_budget_slutsky, aes(x = T, y = C_comp), color = isba_red, linewidth = 0.8, linetype = "dashed") +
  geom_line(data = filter(tbl_ic_slutsky, curve == "IC3"), aes(x = T, y = C), color = isba_blue, linewidth = 0.8) +
  annotate("label", x = 16, y = 7000, label = "IC[3]", parse = TRUE, color = isba_blue, fill = "white", label.size = 0) +
  geom_point(data = filter(tbl_pts_slutsky, name == "C"), aes(x = T, y = C), size = 3) +
  geom_text(data = filter(tbl_pts_slutsky, name == "C"), aes(x = lbl_T, y = lbl_C, label = name), size = 5) +
  line_A + line_C + arrow_inc

# SCHRITT 5: C (Hicks) einblenden als Referenz
p_b2_own_slutsky_5 <- p_b2_own_slutsky_4 +
  geom_line(data = tbl_budget_slutsky, aes(x = T, y = C_hicks), color = isba_gray, linewidth = 0.6, linetype = "dotted") +
  geom_point(data = filter(tbl_pts_slutsky, name == "C (Hicks)"), aes(x = T, y = C), size = 2, color = isba_gray) +
  geom_text(data = filter(tbl_pts_slutsky, name == "C (Hicks)"), aes(x = lbl_T, y = lbl_C, label = name), size = 4, color = isba_gray)

# SCHRITT 6: Substitutionseffekt (Linien C & D, ohne Hicks)
p_b2_own_slutsky_6 <- p_b2_own_slutsky_4 + line_C + line_D + arrow_sub

# SCHRITT 7: Finaler Plot (Alle Effekte, sauber)
p_b2_own_slutsky_7 <- p_b2_own_slutsky_2 +
  geom_line(data = tbl_budget_slutsky, aes(x = T, y = C_comp), color = isba_red, linewidth = 0.8, linetype = "dashed") +
  geom_line(data = filter(tbl_ic_slutsky, curve == "IC3"), aes(x = T, y = C), color = isba_blue, linewidth = 0.8) +
  geom_point(data = filter(tbl_pts_slutsky, name == "C"), aes(x = T, y = C), size = 3) +
  geom_text(data = filter(tbl_pts_slutsky, name == "C"), aes(x = lbl_T, y = lbl_C, label = name), size = 5) +
  arrow_inc + arrow_sub + arrow_gesamt



# ============================================================================

# Abbildung 3.16: Historie USA (1900-2020) - Optimierte 4-Schritt-Didaktik ----------

## 1. Daten und Kurvenschar vorbereiten -----

# Hilfsfunktion für stark gekrümmte Indifferenzkurven (stilisierter Stone-Geary)
# C = U / (T - shift) - konst
calc_c_stilisert <- function(T, U, shift = 10, konst = 20) {
  U / (T - shift) - konst
}

tbl_history_data <- tibble(
  T = seq(11, 24, length.out = 300),
  BC_1900 = 4.5 * (24 - T),
  BC_2020 = 25 * (24 - T),
  # Hicks-Linie (EV): Steigung von 1900, tangiert IC_2020 in C
  BC_hicks = 4.5 * (34.8 - T) # Manuell kalibriert für optische Klarheit
)

# Punkte für die Darstellung
tbl_pts <- tibble(
  name  = c("1900", "2020", "C"),
  T     = c(16, 19, 22),
  C     = c(36, 125, 57.6), # C liegt exakt auf BC_hicks
  lbl_T = c(15.5, 18.5, 22.8),
  lbl_C = c(22, 135, 65)
)

# Indifferenzkurven-Schar (Garantiert ohne Schnittpunkte)
tbl_ic_history <- tibble(T = seq(12.5, 23.9, by = 0.1)) %>%
  mutate(
    IC_1900 = calc_c_stilisert(T, U = 336, shift = 10, konst = 20),
    IC_2020 = calc_c_stilisert(T, U = 1305, shift = 10, konst = 20)
  ) %>%
  pivot_longer(cols = starts_with("IC"), names_to = "curve", values_to = "C") %>%
  filter(C >= 0, C <= 250)

## 2. Basis-Layout -----

base_scales <- list(
  scale_x_continuous(limits = c(12, 24.5), breaks = seq(12, 24, 2), expand = c(0, 0)),
  scale_y_continuous(limits = c(0, 250), breaks = seq(0, 200, 50), expand = c(0, 0)),
  labs(x = "Freizeit (Stunden pro Tag)", y = "Konsumgüter ($ pro Tag)"),
  theme_isba(theory = TRUE),
  coord_cartesian(xlim = c(12, 24), ylim = c(0, 220), clip = "off")
)

# Modulare Annotationen
ee_elements <- list(
  geom_vline(xintercept = c(16, 22), linetype = "dotted", color = "darkgray"),
  annotate("segment", x = 16, xend = 21.8, y = 45, yend = 45, color = "#2ca25f",
           linewidth = 0.8, arrow = arrow(length = unit(0.2, "cm"), type = "closed")),
  annotate("text", x = 19, y = 55, label = "Einkommenseffekt", color = "#2ca25f", fontface = "bold")
)

se_elements <- list(
  geom_vline(xintercept = c(22, 19), linetype = "dotted", color = "darkgray"),
  annotate("segment", x = 22, xend = 19.2, y = 160, yend = 160, color = "#2ca25f",
           linewidth = 0.8, arrow = arrow(length = unit(0.2, "cm"), type = "closed")),
  annotate("text", x = 20.6, y = 170, label = "Substitutionseffekt", color = "#2ca25f", fontface = "bold")
)

## 3. Die 4 Schritte (p_b2_fig_3_16_history_X) ----

# SCHRITT 1: Ausgangslage und 2020 (Zwei Budgets, zwei ICs)
p_b2_fig_3_16_history_1 <- ggplot() +
  base_scales +
  geom_ribbon(data = tbl_history_data, aes(x = T, ymin = 0, ymax = BC_1900), fill = isba_red, alpha = 0.05) +
  geom_line(data = tbl_history_data, aes(x = T, y = BC_1900), color = isba_red, linewidth = 1.2) +
  geom_line(data = tbl_history_data, aes(x = T, y = BC_2020), color = isba_red, linewidth = 1.2) +
  geom_line(data = tbl_ic_history, aes(x = T, y = C, group = curve), color = isba_blue, linewidth = 0.8) +
  geom_point(data = filter(tbl_pts, name != "C"), aes(x = T, y = C), size = 3) +
  geom_text(data = filter(tbl_pts, name != "C"), aes(x = lbl_T, y = lbl_C, label = name), size = 5) +
  annotate("label", x = 14, y = 25, label = "Budgetmenge (1900)", color = isba_red, fill = "white", label.size = 0)

# SCHRITT 2: Einkommenseffekt (Einführung Punkt C und gestrichelte Hicks-Linie)
p_b2_fig_3_16_history_2 <- p_b2_fig_3_16_history_1 +
  geom_line(data = tbl_history_data, aes(x = T, y = BC_hicks), color = isba_red, linetype = "dashed") +
  geom_point(data = filter(tbl_pts, name == "C"), aes(x = T, y = C), size = 3) +
  geom_text(data = filter(tbl_pts, name == "C"), aes(x = lbl_T, y = lbl_C, label = "C"), size = 5) +
  ee_elements

# SCHRITT 3: Substitutionseffekt (Fokus auf Weg von C zurück nach 2020)
p_b2_fig_3_16_history_3 <- p_b2_fig_3_16_history_1 +
  geom_line(data = tbl_history_data, aes(x = T, y = BC_hicks), color = isba_red, linetype = "dashed") +
  geom_point(data = filter(tbl_pts, name == "C"), aes(x = T, y = C), size = 3) +
  geom_text(data = filter(tbl_pts, name == "C"), aes(x = lbl_T, y = lbl_C, label = "C"), size = 5) +
  se_elements

# SCHRITT 4: Finale Darstellung (Alle Effekte kombiniert)
p_b2_fig_3_16_history_4 <- p_b2_fig_3_16_history_1 +
  geom_line(data = tbl_history_data, aes(x = T, y = BC_hicks), color = isba_red, linetype = "dashed") +
  geom_point(data = filter(tbl_pts, name == "C"), aes(x = T, y = C), size = 3) +
  geom_text(data = filter(tbl_pts, name == "C"), aes(x = lbl_T, y = lbl_C, label = "C"), size = 5) +
  ee_elements +
  se_elements


# ============================================================================

# Empirie Arbeitszeitentwicklung ---------------

## 1. Originaldaten nach Fogel (2000), Tabelle 6 ------
tbl_fogel <- tibble(
  Year = rep(c("1880", "1995", "2040"), each = 3),
  Category = rep(c("Lifetime discretionary hours",
                   "Lifetime work hours",
                   "Lifetime leisure hours"), times = 3),
  Hours = c(
    225900, 182100,  43800,  # 1880
    298500, 122400, 176100,  # 1995
    321900,  75900, 246000   # 2040
  )
) %>%
  mutate(
    # Reihenfolge der Balken fixieren
    Category = factor(Category, levels = c("Lifetime discretionary hours",
                                           "Lifetime work hours",
                                           "Lifetime leisure hours")),
    Year = factor(Year, levels = c("1880", "1995", "2040"))
  )

## 2. Ggplot im exakten ISBA-Farbschema
# 2. Ggplot im exakten ISBA-Farbschema (dynamisch via HELPER-SCALE)
p_b2_fig3_17_fogel <- ggplot(tbl_fogel, aes(x = Year, y = Hours, fill = Category)) +
  geom_col(position = "dodge", width = 0.75) +
  scale_y_continuous(
    limits = c(0, 400000),
    breaks = seq(0, 400000, by = 100000),
    labels = label_comma(big.mark = ".", decimal.mark = ",")
  ) +
  # HIER IST DIE LÖSUNG: Greift dynamisch auf deine isba_palette() zu!
  scale_fill_isba() +
  labs(
    x = "Jahr",
    y = "Lebenszeitstunden",
    fill = NULL
  ) +
  # Dein ISBA-Theme (steuert Schriften, Achsen, Legende)
  theme_isba(theory = FALSE) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.grid.major.x = element_blank()
  )




# ============================================================================


# Gender Paygap und Arbeitszeit -------
# 1. Datenbasis (Approximierte OECD Zeitnutzungs-Daten in Minuten pro Tag)
tbl_time_use <- tibble(
  Region = rep(c("China", "India", "US", "Europe"), each = 4),
  Gender = rep(c("Women", "Women", "Men", "Men"), times = 4),
  Work_Type = rep(c("Paid work", "Unpaid work", "Paid work", "Unpaid work"), times = 4),
  Minutes = c(
    250, 230, 300,  90,
    100, 350, 330,  30,
    220, 240, 280, 150,
    180, 260, 230, 140
  )
) %>%
  mutate(
    Region = factor(Region, levels = c("China", "India", "US", "Europe")),
    Gender = factor(Gender, levels = c("Women", "Men")),

    # Level 1 (Paid) bekommt Farbe 1 aus isba_palette (Blau)
    # Level 2 (Unpaid) bekommt Farbe 2 aus isba_palette (Orange)
    Work_Type = factor(Work_Type, levels = c("Paid work", "Unpaid work"))
  )

# 2. Ggplot im ISBA-Design
p_b2_fig3_20 <- ggplot(tbl_time_use, aes(x = Gender, y = Minutes, fill = Work_Type)) +

  # HIER IST DIE LÖSUNG: Wir drehen die Stapelrichtung im Balken um!
  geom_col(position = position_stack(reverse = TRUE), width = 0.6) +

  facet_wrap(~ Region, nrow = 1) +
  scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, 100), expand = c(0, 0)) +

  # Deine dynamische Funktion aufrufen
  scale_fill_isba() +

  # Wir drehen auch die Legende um, damit sie optisch zum Balken passt
  # (Unpaid steht dann oben in der Legende, Paid unten)
  guides(fill = guide_legend(reverse = TRUE)) +

  labs(
    x = NULL,
    y = "Minuten pro Tag",
    fill = NULL
  ) +
  theme_isba(theory = FALSE) +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    panel.grid.major.x = element_blank(),
    strip.text = element_text(size = rel(1.1))
  )





# ============================================================================






# ============================================================================


