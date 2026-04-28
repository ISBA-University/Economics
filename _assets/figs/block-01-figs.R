# ============================================================================
# Plot-Skript Block 1 — Wohlstand und Ungleichheit
#
# Verwendung in slides.qmd:
#   source(xfun::from_root("source/_assets/styles/theme-isba.R"))
#   source(xfun::from_root("source/_assets/figs/block-01-figs.R"))
#
# Dann auf Plot-Objekte zugreifen:
#   p_b1_prodfn_1, p_b1_prodfn_2, p_b1_prodfn_3   (Modelldiagramm)
#   p_b1_pressure_1, p_b1_pressure_2              (Datendiagramm)
#
# Empfohlene Chunk-Optionen in slides.qmd:
#   Modelldiagramme:  fig-width: 12, fig-height: 5.5, out-width: ~
#   Datendiagramme:   fig-width: 9,  fig-height: 4.5, dev: png, out-width: ~
#
#   out-width: ~ ist in r-stack-Blöcken Pflicht, weil das globale
#   out.width: "100%" aus _quarto.yml in der Flex/Grid-Verschachtelung
#   von .has-indicator + .r-stack die Bildhöhe kollabieren lässt.
#
# Helper-Funktion add_pt() wird mehrfach verwendet — Definition siehe
# theme-isba.R. Sie erstellt Punkt + Label aus einem tibble mit Pflicht-
# spalten 'x', 'y', 'label'.
# ============================================================================


# ============================================================================
# DEMO 1: PRODUKTIONSFUNKTION (Modelldiagramm, theme_isba(theory = TRUE))
#
# Modell: Y = A * L^alpha mit alpha < 1 (abnehmender Grenzertrag)
# Drei Animationsschritte:
#   1. Funktionskurve f(L) allein
#   2. + Punkt A (geringer Arbeitseinsatz, hohe AP) mit Hilfslinie
#   3. + Punkt B (hoher Arbeitseinsatz, niedrigere AP) mit Hilfslinie
#                und Erklärungs-Box "abnehmender Grenzertrag"
#
# Theme: theme_isba(theory = TRUE, axis_decoration = FALSE) — abstrakte
# Modellskizze ohne konkrete Achsenwerte.
# ============================================================================

# ---------- Parameter und Funktionen ----------
A_param     <- 100
alpha_param <- 0.5
prod_fn     <- function(L) A_param * L^alpha_param

# ---------- Datenobjekte ----------

# Funktionskurve
L_seq   <- seq(0, 100, length.out = 200)
df_prod <- tibble(L = L_seq, Y = prod_fn(L_seq))

# Punkte A (L=20) und B (L=80) auf der Funktionskurve.
# Konvention: Spalten 'x', 'y', 'label' für add_pt()-Kompatibilität.
# Zusätzlich AP (Durchschnittsprodukt) für Annotation gespeichert.
pts_prodfn <- tibble(
  x     = c(20, 80),
  y     = prod_fn(c(20, 80)),
  label = c("A", "B"),
  AP    = y / x
)

# Hilfslinien (Strahl vom Ursprung zu den Punkten = Steigung der Durchschnittsproduktivität)
seg_A <- tibble(x = 0, xend = pts_prodfn$x[1], y = 0, yend = pts_prodfn$y[1])
seg_B <- tibble(x = 0, xend = pts_prodfn$x[2], y = 0, yend = pts_prodfn$y[2])

# ---------- Achsen-Limits und gemeinsame Layer ----------

# base_scales: gemeinsame Skalen + Theme für alle Plot-Schritte.
# theme_isba(theory = TRUE, axis_decoration = FALSE) liefert Pfeil-Achsen
# OHNE axis.ticks und axis.text — abstrakte Modellskizze.
base_scales <- list(
  scale_x_continuous(limits = c(0, 105), expand = c(0, 0)),
  scale_y_continuous(limits = c(0, 1100), expand = c(0, 0)),
  labs(x = "Arbeitseinsatz (Anzahl Arbeiter)",
       y = "Output (Getreide)"),
  theme_isba(theory = TRUE, axis_decoration = FALSE)
)

# Helper für gestrichelte Hilfslinien (Strahl vom Ursprung)
dashed_seg <- function(data) {
  geom_segment(data = data,
               aes(x = x, xend = xend, y = y, yend = yend),
               linetype = "dashed",
               color = isba_blue,
               linewidth = 0.5)
}


# ---------- Plot-Aufbau ----------
# Hinweis: add_pt() ist in theme-isba.R definiert.
# Pflichtspalten im Datenobjekt: 'x', 'y', 'label'.

# Schritt 1: Funktionskurve f(L) allein
p_b1_prodfn_1 <- ggplot() +
  base_scales +
  geom_line(data = df_prod, aes(x = L, y = Y),
            linewidth = 1.3, color = isba_orange) +
  annotate("text", x = 102, y = prod_fn(100) + 30,
           label = "f(L)",
           color = isba_orange, fontface = "bold",
           size = 5, hjust = 0)

# Schritt 2: + Punkt A mit Hilfslinie
p_b1_prodfn_2 <- p_b1_prodfn_1 +
  dashed_seg(seg_A) +
  add_pt(pts_prodfn, "A",
         nudge_x = 3, nudge_y = 40,
         size_pt = 4, size_text = 4.5) +
  annotate("text",
           x = pts_prodfn$x[1] + 3,
           y = pts_prodfn$y[1] + 80,
           label = paste0("(AP = ", round(pts_prodfn$AP[1], 1), ")"),
           color = isba_blue, fontface = "bold", size = 4)

# Schritt 3: + Punkt B mit Hilfslinie und Erklärungs-Box
p_b1_prodfn_3 <- p_b1_prodfn_2 +
  dashed_seg(seg_B) +
  add_pt(pts_prodfn, "B",
         nudge_x = 3, nudge_y = 40,
         size_pt = 4, size_text = 4.5) +
  annotate("text",
           x = pts_prodfn$x[2] + 3,
           y = pts_prodfn$y[2] + 80,
           label = paste0("(AP = ", round(pts_prodfn$AP[2], 1), ")"),
           color = isba_blue, fontface = "bold", size = 4) +
  annotate("label",
           x = 65, y = 200,
           label = "Abnehmender Durchschnittsertrag:\nJe mehr Arbeiter, umso geringer der Ertrag pro Kopf",
           fill  = alpha(isba_blue, 0.05),
           color = isba_blue,
           label.size = 0.5,
           size = 4)


# ============================================================================
# DEMO 2: AUTOMOBILDATEN-EXPLORATION (Datendiagramm, theme_isba())
#
# Zeigt mtcars-Daten: PS (hp) vs. Verbrauch (mpg) und Zylinder (cyl)
# als kategoriale Gruppe. Demonstriert:
#   - theme_isba() default mit Gridlines und Achsentexten
#   - scale_color_isba() für Gruppenfarbe
#   - Zwei Animationsschritte (Streupunkte → mit Trendlinie)
# ============================================================================

# ---------- Daten vorbereiten ----------
df_cars <- mtcars |>
  as_tibble(rownames = "model") |>
  mutate(cyl = factor(cyl, labels = c("4 Zyl.", "6 Zyl.", "8 Zyl.")))

# ---------- Basis-Plot ----------
p_b1_pressure_base <- ggplot(df_cars, aes(x = hp, y = mpg, color = cyl)) +
  scale_x_continuous(name = "Motorleistung (PS)") +
  scale_y_continuous(name = "Verbrauch (Meilen pro Gallone)") +
  scale_color_isba() +
  labs(title    = "Mehr Leistung, mehr Verbrauch",
       subtitle = "Auto-Kennzahlen aus dem mtcars-Datensatz, gruppiert nach Zylinderzahl",
       caption  = "Quelle: Henderson & Velleman (1981) via R-Paket datasets") +
  theme_isba() +
  theme(legend.position = "bottom",
        legend.title    = element_blank())

# Schritt 1: Streupunkte
p_b1_pressure_1 <- p_b1_pressure_base +
  geom_point(size = 3, alpha = 0.85)

# Schritt 2: + Trendlinie und Annotation
p_b1_pressure_2 <- p_b1_pressure_1 +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8,
              show.legend = FALSE) +
  annotate("label",
           x = 280, y = 30,
           label = "Klare negative Korrelation:\nMehr Leistung → höherer Verbrauch",
           fill  = alpha(isba_blue, 0.05),
           color = isba_blue,
           label.size = 0.5,
           size = 3.8,
           hjust = 1)
