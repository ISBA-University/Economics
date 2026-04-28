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

# ---------- Parameter und Funktionen ----------
alpha <- 0.7280

# Nutzenniveaus (durch Ankerpunkte kalibriert)
U_mid  <- 15^alpha * 540^(1 - alpha)   # IC2: durch A, E, F, G, H, D
U_high <- 13^alpha * 540^(1 - alpha)   # IC3: durch B
U_low  <- 20^alpha *  85^(1 - alpha)   # IC1: durch C

# IC-Funktion: c = (U / l^alpha)^(1/(1-alpha))
ic_curve <- function(l, U) (U / l^alpha)^(1 / (1 - alpha))

# ---------- Datenobjekte ----------

# Indifferenzkurven (drei Niveaus)
x_seq <- seq(8.5, 24, length.out = 400)
ic_data <- bind_rows(
  tibble(x = x_seq, y = ic_curve(x_seq, U_high), curve = "IC3"),
  tibble(x = x_seq, y = ic_curve(x_seq, U_mid),  curve = "IC2"),
  tibble(x = x_seq, y = ic_curve(x_seq, U_low),  curve = "IC1")
) |> filter(y >= 0, y <= 670)

# Punkte auf IC2 (mittlere Indifferenzkurve)
pts_ic <- tibble(
  x     = c(15, 16, 17, 18, 19, 20),
  y     = ic_curve(c(15, 16, 17, 18, 19, 20), U_mid),
  label = c("A", "E", "F", "G", "H", "D")
)

# Punkte auf anderen Niveaus
pts_extra <- tibble(
  x     = c(13,  20),
  y     = c(540, 85),
  label = c("B", "C")
)

# Y-Wert von E (für gestrichelte Referenzlinie in Schritt 3)
y_E <- ic_curve(16, U_mid)

# ---------- Gestrichelte Referenzlinien ----------

# Schritt 1: B und A → horizontale Linie auf y=540, vertikale durch x=15
seg_BA <- tibble(
  x    = c(8.5, 15),
  xend = c(15,  15),
  y    = c(540, 0),
  yend = c(540, 540)
)

# Schritt 2: C und D → horizontale Linie auf y=250, vertikale durch x=20
seg_CD <- tibble(
  x    = c(8.5, 20),
  xend = c(20,  20),
  y    = c(250, 0),
  yend = c(250, 250)
)

# Schritt 3: E → horizontale Linie auf y_E, vertikale durch x=16
seg_E <- tibble(
  x    = c(8.5, 16),
  xend = c(16,  16),
  y    = c(y_E, 0),
  yend = c(y_E, y_E)
)

# ---------- Achsen-Limits und gemeinsame Layer ----------

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


# ---------- Plot-Aufbau ----------
# Hinweis: add_pt() ist in theme-isba.R definiert.
# Pflichtspalten im Datenobjekt: 'x', 'y', 'label'.

# Schritt 1: B und A mit Referenzlinien
p_b2_pref_1 <- ggplot() +
  base_scales +
  dashed_seg(seg_BA) +
  add_pt(pts_extra, "B", nudge_x = -0.4, nudge_y = 15) +
  add_pt(pts_ic,    "A", nudge_x = -0.4, nudge_y = 15)

# Schritt 2: + C und D mit Referenzlinien
p_b2_pref_2 <- p_b2_pref_1 +
  dashed_seg(seg_CD) +
  add_pt(pts_extra, "C", nudge_x =  0.4, nudge_y = -18) +
  add_pt(pts_ic,    "D", nudge_x =  0.4, nudge_y =  15)

# Schritt 3: + E mit Referenzlinien
p_b2_pref_3 <- p_b2_pref_2 +
  dashed_seg(seg_E) +
  add_pt(pts_ic, "E", nudge_x = 0.4, nudge_y = 15)

# Schritt 4: + Punkte F, G, H (alle auf IC2)
p_b2_pref_4 <- p_b2_pref_3 +
  add_pt(pts_ic, c("F", "G", "H"), nudge_x = 0.4, nudge_y = 15)

# Schritt 5: + Indifferenzkurve IC2 (durch A, E, F, G, H, D)
p_b2_pref_5 <- p_b2_pref_4 +
  geom_line(data = filter(ic_data, curve == "IC2"),
            aes(x = x, y = y),
            color = isba_orange, linewidth = 0.9)

# Schritt 6: + IC1 (durch C) und IC3 (durch B)
p_b2_pref_6 <- p_b2_pref_5 +
  geom_line(data = filter(ic_data, curve %in% c("IC1", "IC3")),
            aes(x = x, y = y, group = curve),
            color = isba_orange, linewidth = 0.9)

