// Reporte estacion rodaja.cl — documento unico con portada, TOC y numeracion continua
#import "_config.typ": *

#let c-brand     = rgb("#DF9531")
#let c-dark      = rgb("#394553")
#let c-water     = rgb("#378ADD")
#let c-bg        = rgb("#F4F6F8")
#let c-green     = rgb("#1B7F4B")
#let c-primary   = rgb(doc_color)
#let c-secondary = c-dark
#let c-accent    = c-water
#let c-text      = c-dark
#let c-muted     = rgb("#8A95A0")
#let c-line      = rgb("#394553").lighten(85%)
#let c-bg-note   = c-bg
#let c-bg-code   = rgb("#f5f5f5")

#let tone-color(t) = {
  if t == "green" { c-green }
  else if t == "brand" { c-brand }
  else if t == "water" { c-water }
  else { c-muted }
}

#let kpi-card(label, value, tone) = {
  let tc = tone-color(tone)
  box(
    width: 100%, inset: 10pt, radius: 4pt,
    stroke: 1pt + tc.lighten(40%),
    fill: c-bg,
  )[
    #text(8pt, fill: c-muted)[#label]
    #v(4pt)
    #text(13pt, weight: "bold", fill: tc)[#value]
  ]
}

#set text(font: ("DejaVu Sans",), size: 10pt, lang: "es", fill: c-text)
#set par(justify: true, leading: 0.72em)
#set enum(indent: 0.4cm, numbering: "1.", body-indent: 0.4cm)
#set list(indent: 0.4cm, marker: text(fill: c-brand)[•], body-indent: 0.4cm)

#show heading.where(level: 1): it => {
  v(0.8cm)
  block(width: 100%, inset: (bottom: 8pt),
        stroke: (bottom: 2.5pt + c-brand))[
    #text(16pt, weight: "bold", fill: c-brand)[#it.body]
  ]
  v(0.35cm)
}
#show heading.where(level: 2): it => {
  v(0.55cm)
  text(12pt, weight: "bold", fill: c-secondary)[#it.body]
  v(0.2cm)
}
#show heading.where(level: 3): it => {
  v(0.35cm)
  text(10.5pt, weight: "bold", fill: c-accent)[#it.body]
  v(0.12cm)
}

#show link: it => text(fill: c-water)[#underline(it)]

#show raw.where(block: false): it => box(
  inset: (x: 3pt, y: 0pt), outset: (y: 3pt),
  fill: rgb("#eeeeee"), radius: 2pt,
)[#text(9pt, font: "DejaVu Sans Mono")[#it]]

#show raw.where(block: true): it => block(
  width: 100%, inset: 10pt, fill: c-bg-code,
  stroke: 0.5pt + c-line, radius: 3pt,
)[#text(8.5pt, font: "DejaVu Sans Mono")[#it]]

#show quote.where(block: true): it => block(
  width: 100%,
  inset: (left: 14pt, top: 10pt, bottom: 10pt, right: 10pt),
  stroke: (left: 3pt + c-brand), fill: c-bg-note,
)[#text(9.5pt)[#it.body]]

#set table(
  stroke: 0.35pt + c-line,
  inset: (x: 6pt, y: 6pt),
  align: horizon + left,
  fill: (x, y) =>
    if y == 0 { c-brand.lighten(88%) }
    else if calc.odd(y) { c-bg }
    else { none },
)
#show table.cell.where(y: 0): it => strong(it)
#show table.cell: it => {
  set text(size: 8.5pt)
  set par(justify: true)
  it
}
#show figure: it => {
  set align(center)
  v(0.3cm)
  it
  v(0.25cm)
}
#show figure.caption: it => {
  set text(size: 9pt, fill: c-muted, style: "italic")
  it
}

#let horizontalrule = line(length: 100%, stroke: 0.5pt + c-line)

// --- Portada ---
#page(margin: (top: 1.8cm, bottom: 1.8cm, left: 2.2cm, right: 2.2cm), header: none, footer: none)[
  #rect(width: 100%, height: 0.9cm, fill: gradient.linear(c-brand, c-water, angle: 90deg))
  #v(0.6cm)
  #if doc_version != "" [
    #text(9.5pt, fill: c-muted)[#doc_version]
    #v(0.35cm)
  ]
  #text(26pt, weight: "bold", fill: c-brand)[#doc_title]
  #v(0.25cm)
  #text(11pt, fill: c-dark)[#doc_subtitle]
  #v(0.8cm)
  #grid(
    columns: (1fr, 1fr),
    gutter: 16pt,
    box(inset: 12pt, fill: c-bg, radius: 4pt, stroke: 0.5pt + c-line)[
      #text(10pt, weight: "bold", fill: c-dark)[Ficha técnica]
      #v(6pt)
      #text(9pt)[
        Código: #doc_station_code \
        Coordenadas: #doc_station_lat, #doc_station_lon \
        Región: #doc_station_region · #doc_station_comuna \
        Operador: #doc_station_operador \
        Calidad: #doc_station_calidad \
        Período: #doc_station_periodo
      ]
    ],
    box(inset: 8pt, radius: 4pt, stroke: 0.5pt + c-line, height: 4.5cm)[
      #if doc_has_map == "true" [
        #block(width: 100%, height: 4.5cm, clip: true)[
          #image("media/mapa_estacion.png", width: 100%, height: 100%, fit: "cover")
          #if doc_map_pin == "true" [
            #place(top + left, dx: doc_map_fx, dy: doc_map_fy)[
              #circle(radius: 4pt, fill: c-brand, stroke: 1pt + white)
            ]
          ]
        ]
      ] else [
        #align(center + horizon)[
          #text(9pt, fill: c-muted)[#doc_station_lat, #doc_station_lon]
        ]
      ]
    ],
  )
  #v(0.7cm)
  #text(10pt, weight: "bold", fill: c-dark)[Indicadores clave]
  #v(0.35cm)
  #grid(
    columns: (1fr, 1fr, 1fr, 1fr),
    gutter: 10pt,
    kpi-card(doc_kpi1_label, doc_kpi1_value, doc_kpi1_tone),
    kpi-card(doc_kpi2_label, doc_kpi2_value, doc_kpi2_tone),
    kpi-card(doc_kpi3_label, doc_kpi3_value, doc_kpi3_tone),
    kpi-card(doc_kpi4_label, doc_kpi4_value, doc_kpi4_tone),
  )
  #v(1fr)
  #align(center)[
    #text(11pt, weight: "bold", fill: c-brand)[rodaja.cl · Explorador Climático]
    #linebreak()
    #text(9pt, fill: c-muted)[#doc_date]
  ]
]

// --- TOC ---
#pagebreak()
#outline(title: "Contenido", depth: 1, indent: 1.2em)
#pagebreak()

// --- Cuerpo con header/footer continuo ---
#set page(
  paper: "us-letter",
  margin: (top: 2.6cm, bottom: 2.3cm, left: 2.4cm, right: 2.4cm),
  header: context {
    set text(8pt, fill: c-muted)
    grid(columns: (1fr, auto),
      align(left)[#text(fill: c-brand, weight: "bold")[rodaja.cl] · #doc_title],
      align(right)[#doc_date],
    )
    v(2pt)
    line(length: 100%, stroke: 0.4pt + c-brand)
  },
  footer: context {
    set text(7.5pt, fill: c-muted)
    grid(columns: (1fr, auto),
      align(left)[app.rodaja.cl · Datos DGA/DMC/INIA],
      align(right)[
        Página #counter(page).display() de #counter(page).final().first()
      ],
    )
  },
)

#include "_body.typ"
