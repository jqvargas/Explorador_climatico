# Plan de estrategia de marketing y ventas — rodaja.cl

**Versión:** 1.0  
**Fecha:** junio 2026  
**Producto:** Explorador Climático — plataforma de visualización y descarga de datos climáticos históricos de estaciones de monitoreo en Chile  
**URL:** [https://app.rodaja.cl](https://app.rodaja.cl)

---

## 1. Resumen ejecutivo

rodaja.cl agrega, unifica y pone a disposición datos observados de redes públicas chilenas (DGA, DMC, INIA) en un solo explorador con descargas masivas en CSV. Los datos en bruto son gratuitos en portales estatales, pero **fragmentados, lentos y difíciles de escalar** para proyectos reales.

**Propuesta de valor en una frase:**

> *Termina en horas un trabajo que hoy te toma semanas: series históricas unificadas de estaciones chilenas, listas para análisis.*

**Objetivo comercial a 12 meses:**

| Métrica | Meta conservadora | Meta ambiciosa |
|---------|-------------------|----------------|
| Usuarios registrados | 800 | 2.500 |
| Clientes de pago (MRR) | 40 | 120 |
| MRR | USD 2.500 | USD 8.000 |
| Contratos Enterprise anuales | 3 | 10 |
| NPS usuarios activos | ≥ 40 | ≥ 55 |

**Estrategia central:** product-led growth (PLG) con capa comercial B2B para consultoras, energía y minería. El explorador gratuito es el motor de adquisición; las descargas masivas y la licencia comercial son el monetizador.

---

## 2. Diagnóstico de mercado

### 2.1 Lo que somos (y lo que no somos)

**Principio rector de producto (sep 2026, manda sobre esta tabla en pronóstico y mapas):** [`docs/producto/PRINCIPIO_RECTOR.md`](../../docs/producto/PRINCIPIO_RECTOR.md).

| Somos | No somos |
|-------|----------|
| Infraestructura de datos climáticos históricos para Chile | App del tiempo de consumo (Windy, AccuWeather, MeteoChile app) |
| Pronóstico **de estación** comparable con esa historia (alertas, carrusel) | Explorador NWP / raster tipo Windy como producto |
| Agregador multi-fuente con UX y descarga | Fuente primaria de datos (somos capa sobre DGA/DMC/INIA) |
| Herramienta para consultores, investigadores y empresas | Repositorio académico estático |
| SaaS con cuotas de descarga | Venta de sensores o hardware |

### 2.2 Ventaja competitiva (moat)

1. **Cobertura unificada Chile:** DGA (hidrología, caudal, pluviometría) + DMC (meteorología) + INIA (agromet) en un solo catálogo y esquema.
2. **Producto usable:** mapa, gráficos, descarga por email en ~5 min, PWA de descargas.
3. **Control de calidad (QC):** flags y formato completo en tiers pagos — valor que los portales no entregan.
4. **First mover comercial:** no existe un equivalente SaaS chileno que cobre por acceso unificado a históricos de estaciones.

### 2.3 Competencia y alternativas del cliente

| Alternativa | Fortaleza | Debilidad vs. rodaja |
|-------------|-----------|----------------------|
| Portales DMC / DGA | Gratis, oficial | Fragmentado, límites de consulta, sin multi-fuente |
| Scraping / MeteoVision legacy | Control total | Mantenimiento alto, sin producto, no escalable |
| APIs globales (Visual Crossing, Weatherbit) | Pricing claro, API madura | Sin estaciones chilenas granulares ni caudales DGA |
| Consultora que arma la base a mano | Personalizado | USD 500–5.000/proyecto, lento, no reutilizable |
| Meteostat / datos abiertos | Gratis | Cobertura Chile incompleta, sin hidrología DGA |

### 2.4 Tamaño de oportunidad (Chile)

Segmentos prioritarios con presupuesto real:

- **Consultoras ambientales e ingeniería** (~200–400 firmas activas en EIA/hidrología)
- **Agtech y agronomía de precisión** (crecimiento sostenido post-sequía)
- **Energía renovable** (solar/eólica — radiación, viento, resource assessment)
- **Minería e infraestructura** (due diligence hidrometeorológica)
- **Academia** (credibilidad y pipeline de usuarios futuros; revenue bajo pero estratégico)
- **Sector público** (municipalidades, DOH, aguas — ciclos largos, tickets altos)

---

## 3. Posicionamiento y mensaje

### 3.1 Posicionamiento

**Categoría:** Plataforma de datos climáticos históricos para Chile  
**Para quién:** Profesionales que necesitan series observadas de estaciones reales  
**A diferencia de:** Portales estatales y APIs globales  
**rodaja.cl es:** El acceso más rápido y completo a históricos DGA + DMC + INIA, listos para descargar

### 3.2 Mensajes clave por audiencia

| Audiencia | Dolor | Mensaje |
|-----------|-------|---------|
| Consultor EIA | 40–120 h armando bases por proyecto | "Baja el EIA de 3 semanas a 3 días" |
| Hidrólogo | Múltiples portales, formatos distintos | "Una sola fuente para precipitación, caudal y temperatura" |
| Agtech | Series largas para modelos de riego | "40 años de datos por estación, CSV en minutos" |
| Energía solar/eólica | Resource assessment | "Radiación y viento históricos de la red oficial chilena" |
| Académico | Tiempo en limpieza de datos | "Explora gratis; cita DMC/DGA como siempre, nosotros te ahorramos el ETL" |

### 3.3 Taglines candidatos

- *Datos climáticos de Chile, unificados.*
- *De la estación al CSV en minutos.*
- *El explorador climático que Chile necesitaba.*

### 3.4 Objeciones frecuentes y respuestas

| Objeción | Respuesta |
|----------|-----------|
| "DMC es gratis" | "Sí. Nosotros cobramos por unificar DGA+DMC+INIA, descargar en bulk, QC y licencia comercial. ¿Cuántas horas te tomó la última vez?" |
| "Puedo scrapear yo" | "Puedes. ¿Quién mantiene el pipeline cuando cambia el portal? ¿Cuánto cuesta ese analista/mes?" |
| "No tengo presupuesto" | "El tier gratuito cubre exploración y descargas pequeñas. Para proyectos facturables, el ROI suele ser 10× en la primera semana." |
| "Necesito API" | "Disponible en Enterprise. Podemos hacer piloto de 30 días." |

---

## 4. Segmentación y buyer personas

### 4.1 ICP primario (Ideal Customer Profile)

**Consultora ambiental / ingeniería hidráulica**

- 5–50 empleados
- Hace 4–20 EIA/año
- Usa Excel, R o Python
- Decision maker: jefe de proyecto o gerente de estudios
- Trigger de compra: nuevo EIA con componente hidrometeorológico

### 4.2 Personas

#### Persona A — "Carolina, jefa de proyecto ambiental"

- **Edad:** 32–45
- **Objetivo:** Entregar EIA a tiempo y dentro de presupuesto
- **Frustración:** Analistas pierden semanas bajando datos estación por estación
- **Presupuesto:** USD 50–200/mes o USD 300–800 por proyecto
- **Canal:** LinkedIn, referidos de colegas, Google "datos precipitación estación Chile"
- **Plan ideal:** Pro o Pro+

#### Persona B — "Matías, data scientist agtech"

- **Edad:** 26–38
- **Objetivo:** Alimentar modelos de riego/rendimiento
- **Frustración:** Datos incompletos, sin API, limpieza manual
- **Presupuesto:** USD 30–80/mes
- **Canal:** Comunidades R/Python, Twitter/X tech, demos en meetups
- **Plan ideal:** Pro

#### Persona C — "Andrea, ingeniera de recursos renovables"

- **Edad:** 28–42
- **Objetivo:** Resource assessment para parques solar/eólico
- **Frustración:** Datos dispersos, necesita series largas de radiación/viento
- **Presupuesto:** USD 150–500/mes (empresa paga)
- **Canal:** LinkedIn, conferencias sector energía, partners
- **Plan ideal:** Pro+ o Enterprise

#### Persona D — "Felipe, académico investigador"

- **Edad:** 25–55
- **Objetivo:** Publicar con datos reproducibles
- **Frustración:** Tiempo en ETL vs. investigación
- **Presupuesto:** USD 0–15/mes (tier académico)
- **Canal:** Universidades, seminarios, citas en papers
- **Plan ideal:** Gratuito o Académico — **embajador de marca**, no cliente principal

---

## 5. Modelo de negocio y pricing

### 5.1 Principio de pricing

Cobrar por **valor entregado** (tiempo ahorrado + licencia comercial + volumen), no por "acceso a datos públicos". Anclar precio al costo de alternativa: analista junior (CLP 600k–900k/mes) × horas ahorradas.

### 5.2 Estructura de planes propuesta

| Plan | Cuota descarga | Precio mensual | Precio anual (-20%) | Target |
|------|----------------|----------------|---------------------|--------|
| **Gratuito** | 100 MB/día | USD 0 | — | Adquisición, academia, prueba |
| **Pro** | 500 MB/día | USD 29 | USD 279 | Consultor individual, agtech |
| **Pro+** | 2 GB/día | USD 79 | USD 759 | Equipos, consultoras medianas |
| **Enterprise** | 10 GB/día (negociable) | desde USD 249 | desde USD 2.390 | Minería, energía, API+SLA |

*Referencia CLP: multiplicar × 870. Tiers técnicos actuales en código: `gratuito` (100 MB), `pro` (1 GB), `enterprise` (3 GB) — alinear web y Stripe con esta propuesta comercial en fase de lanzamiento.*

### 5.3 Diferenciación por features (no solo bytes)

| Feature | Gratuito | Pro | Pro+ | Enterprise |
|---------|----------|-----|------|------------|
| Explorador y gráficos | ✅ | ✅ | ✅ | ✅ |
| Descarga CSV | ✅ limitada | ✅ | ✅ | ✅ |
| Estaciones por descarga | 200 | 200 | 500 | Ilimitado |
| Formato completo + QC | ❌ | ✅ | ✅ | ✅ |
| Cola prioritaria | ❌ | ✅ | ✅ | ✅ |
| Licencia uso comercial | ❌ | ✅ | ✅ | ✅ + redistribución interna |
| API | ❌ | ❌ | ✅ lectura | ✅ + SLA |
| Usuarios | 1 | 1 | 3 | Ilimitado |
| Soporte | — | Email 48h | Email 24h | Dedicado |
| Factura / OC Chile | ❌ | ✅ | ✅ | ✅ |

### 5.4 Add-ons (margen alto)

| Add-on | Precio |
|--------|--------|
| Pack +5 GB (30 días) | USD 49 |
| Proyecto EIA (hasta 50 estaciones, entrega garantizada) | USD 350–800 |
| Institucional académico (10 usuarios) | USD 99/mes |
| Integración white-label | USD 500+/mes |

**Roadmap de productización:** [`docs/producto/MONETIZACION_ROADMAP.md`](../../docs/producto/MONETIZACION_ROADMAP.md) + [`docs/producto/LAUNCH_BACKLOG.md`](../../docs/producto/LAUNCH_BACKLOG.md) §9b (EIA → asientos → API → add-ons → white-label). Checkboxes maestros ahí; este PLAN sigue siendo la narrativa GTM/precios aspiracionales.

### 5.5 Fases de pricing

| Fase | Período | Acción |
|------|---------|--------|
| **Lanzamiento** | Meses 1–6 | "Founding member" Pro a USD 19/mes; 3–5 Enterprise piloto a USD 150–300/mes |
| **Consolidación** | Meses 6–12 | Precios objetivo (tabla arriba); eliminar descuento founding |
| **Escala** | Año 2+ | Contratos anuales sector energía/minería USD 3k–15k/año |

---

## 6. Estrategia go-to-market (GTM)

### 6.1 Modelo: Product-Led Growth + Sales-Assisted

```
Adquisición (gratis)  →  Activación (1ª descarga)  →  Conversión (Pro+)  →  Expansión (Enterprise)
     ↑                          ↑                            ↑                        ↑
  SEO, contenido            Onboarding email            Upgrade in-app          Outbound B2B
  LinkedIn orgánico         Tutorial 5 min              Caso de uso ROI         Demos 1:1
  Referidos academia        Wizard estaciones           Trial Pro 14 días       Contratos anuales
```

### 6.2 Embudo y métricas por etapa

| Etapa | Definición | Meta mes 6 | Meta mes 12 |
|-------|------------|------------|-------------|
| Visitante | Sesión en app.rodaja.cl | 3.000/mes | 8.000/mes |
| Registrado | Cuenta Clerk creada | 400/mes acum. | 1.200 acum. |
| Activado | ≥1 descarga completada | 25% registros | 35% registros |
| Pagador | Tier pro+ | 15 clientes | 60 clientes |
| Enterprise | Contrato ≥ USD 249/mes | 2 | 8 |

### 6.3 Ciclo de venta B2B (Enterprise)

| Etapa | Duración típica | Actividad |
|-------|-----------------|-----------|
| 1. Prospección | 1–2 sem | LinkedIn, lista consultoras SEA, eventos |
| 2. Discovery | 30 min | Entender proyecto, estaciones, volumen, urgencia |
| 3. Demo | 45 min | Explorador + descarga en vivo con sus estaciones |
| 4. Propuesta | 3–5 días | Plan + precio + piloto 30 días opcional |
| 5. Cierre | 1–4 sem | OC, factura, onboarding |
| **Ciclo total** | **4–10 sem** | Más largo en minería/ público |

---

## 7. Marketing — canales y tácticas

### 7.1 Prioridad de canales (impacto × esfuerzo)

| Prioridad | Canal | Objetivo | Esfuerzo |
|-----------|-------|----------|----------|
| 🔴 P0 | SEO / contenido técnico | Tráfico cualificado long-tail | Medio |
| 🔴 P0 | Producto (onboarding, emails) | Activación y conversión | Bajo–medio |
| 🟠 P1 | LinkedIn (orgánico + founder-led) | Autoridad B2B | Medio |
| 🟠 P1 | Referidos y partnerships | Consultoras, universidades | Medio |
| 🟡 P2 | Google Ads (marca + long-tail) | Captura demanda | Medio–alto |
| 🟡 P2 | Eventos / webinars | Energía, agua, ambiente | Alto |
| 🟢 P3 | Paid social | Retargeting | Bajo presupuesto |

### 7.2 SEO — keywords objetivo

**Alta intención (prioridad):**

- datos precipitación estaciones chile csv
- series históricas dga descargar
- datos caudal estaciones chile
- base datos climáticos eia chile
- estaciones meteorológicas chile histórico

**Contenido de autoridad:**

- "Cómo obtener datos DGA y DMC para un EIA"
- "Comparativa: portales oficiales vs. plataforma unificada"
- Casos de uso por cuenca (Maipo, Biobío, etc.)

### 7.3 Content marketing — calendario inicial (90 días)

| Semana | Pieza | Canal | CTA |
|--------|-------|-------|-----|
| 1 | Landing pricing + comparativa vs. portales | Web | Registro gratuito |
| 2 | Tutorial: "Primera descarga en 5 minutos" | Blog + email | Activación |
| 3 | Caso: consultora reduce 60 h en EIA | LinkedIn + PDF | Demo Pro |
| 4 | Guía: variables disponibles DGA vs DMC | SEO | Explorador |
| 5 | Webinar con universidad (demo en vivo) | Zoom + YouTube | Registro |
| 6 | Infografía: mapa de cobertura estaciones | LinkedIn | Compartir |
| 7 | Post técnico: flags QC explicados | Blog | Upgrade Pro+ |
| 8 | Testimonial piloto Enterprise | Web + LinkedIn | Contacto ventas |
| 9–12 | Repetir ciclo por vertical (agtech, energía) | Multi | — |

### 7.4 Email lifecycle (automatización)

| Trigger | Email | Objetivo |
|---------|-------|----------|
| Registro | Bienvenida + link tutorial | Activación |
| Día 2 sin descarga | "Elige tu primera estación" | Activación |
| 1ª descarga OK | "¿Necesitas más volumen?" | Upsell |
| 80% cuota usada | Upgrade a Pro | Conversión |
| 30 días inactivo | Reactivación con caso de uso | Retención |
| Upgrade Pro | Onboarding features premium | Retención |

### 7.5 Partnerships estratégicos

| Partner tipo | Ejemplo | Modelo |
|--------------|---------|--------|
| Consultoras ambientales | 5 firmas piloto | Referido 20% primer año |
| Universidades | UC, USACH, UACh | Tier académico + co-branding |
| Cámaras / gremios | CChC, ASIVA | Webinars conjuntos |
| Software complementario | QGIS plugins, R packages | Integración técnica |

---

## 8. Ventas — playbook resumido

### 8.1 Lead scoring (0–100)

| Criterio | Puntos |
|----------|--------|
| Dominio email corporativo (no gmail) | +15 |
| ≥3 descargas en 7 días | +20 |
| Uso >50% cuota gratuita | +25 |
| Sector: consultora, energía, minería | +20 |
| Visitó página pricing / contacto | +10 |
| Respondió outbound | +10 |

**≥60 puntos:** contacto comercial en 48 h.

### 8.2 Guión discovery (30 min)

1. ¿Qué proyecto estás ejecutando? (EIA, modelo, informe)
2. ¿Cuántas estaciones y qué variables?
3. ¿Cómo lo haces hoy? ¿Cuánto tiempo toma?
4. ¿Necesitas licencia comercial para entregar al cliente?
5. ¿API o solo descargas manuales?
6. ¿Timeline de decisión?

### 8.3 Propuesta de valor en demo

1. Buscar estación del cliente en el mapa (30 s)
2. Mostrar gráfico 20–40 años (1 min)
3. Lanzar descarga CSV en vivo (2 min)
4. Mostrar cuota y plan recomendado (2 min)
5. ROI: "Esto te ahorra X horas × CLP Y/hora = Z ROI"

### 8.4 Materiales de venta a producir

- [ ] One-pager PDF (consultoras)
- [ ] Deck demo 10 slides
- [ ] Hoja de pricing con FAQ
- [ ] 2 casos de estudio con números
- [ ] Plantilla propuesta Enterprise
- [ ] Contrato / términos de uso comercial

---

## 9. Producto como palanca de marketing

Acciones in-product con mayor impacto comercial:

| Iniciativa | Impacto | Esfuerzo |
|------------|---------|----------|
| Página `/precios` clara con comparativa | Alto | Bajo |
| Banner upgrade al 80% cuota | Alto | Bajo |
| Trial Pro 14 días (tarjeta) | Alto | Medio |
| Wizard "Proyecto EIA" (estaciones sugeridas) | Alto | Medio |
| Referral: 1 mes Pro gratis por referido pagador | Medio | Medio |
| Badge "Powered by rodaja" en informes (opcional) | Medio | Bajo |
| Testimonios en explorador (social proof) | Medio | Bajo |

---

## 10. KPIs y dashboard

### 10.1 Métricas semanales

- Visitantes únicos / registrados / activados
- Descargas completadas y bytes consumidos
- Conversión registro → 1ª descarga
- MRR, churn, nuevos pagadores
- CAC por canal (cuando haya paid)

### 10.2 Métricas mensuales

- ARPU por tier
- LTV estimado (ARPU × meses retención)
- NPS (encuesta post-descarga)
- Pipeline Enterprise (leads, demos, propuestas)
- Tráfico orgánico y keywords top 10

### 10.3 Objetivos financieros año 1

| Trimestre | MRR objetivo | Clientes pago |
|-----------|--------------|---------------|
| Q1 | USD 500 | 10 |
| Q2 | USD 1.500 | 25 |
| Q3 | USD 3.500 | 50 |
| Q4 | USD 6.000 | 80 |

*Supuesto: mix 70% Pro/Pro+, 30% Enterprise en revenue.*

---

## 11. Roadmap de ejecución — 12 meses

### Fase 1: Fundamentos (meses 1–3)

**Marketing**
- [ ] Publicar landing con propuesta de valor y pricing
- [ ] Configurar analytics (Plausible/GA4) y eventos de conversión
- [ ] 4 artículos SEO fundacionales
- [ ] Perfil LinkedIn founder activo (2 posts/semana)

**Ventas**
- [ ] Lista top 50 consultoras ambientales Chile
- [ ] 10 outreach personalizados/semana
- [ ] 3 demos piloto Enterprise

**Producto**
- [ ] Página pricing + checkout (Stripe/Clerk billing)
- [ ] Emails transaccionales onboarding

### Fase 2: Tracción (meses 4–6)

**Marketing**
- [ ] Primer caso de estudio publicado
- [ ] Webinar con universidad o gremio
- [ ] Programa founding members (Pro USD 19)

**Ventas**
- [ ] Cerrar 2 contratos Enterprise
- [ ] Playbook ventas documentado
- [ ] Partner referidos con 2 consultoras

**Producto**
- [ ] Trial 14 días Pro
- [ ] Upgrade prompts in-app

### Fase 3: Escala (meses 7–9)

**Marketing**
- [ ] Google Ads long-tail (presupuesto USD 300–500/mes)
- [ ] Contenido por vertical (agtech, energía)
- [ ] Presencia en 1 conferencia sectorial

**Ventas**
- [ ] Pipeline CRM simple (Notion/HubSpot free)
- [ ] Propuestas EIA como SKU
- [ ] 5+ clientes Enterprise

### Fase 4: Consolidación (meses 10–12)

**Marketing**
- [ ] Marca reconocida en nicho (encuesta awareness)
- [ ] 3 casos de estudio con ROI cuantificado

**Ventas**
- [ ] MRR ≥ USD 6.000
- [ ] Renegociar precios founding → lista
- [ ] Evaluar primer hire comercial part-time

---

## 12. Presupuesto marketing año 1 (escenario lean)

| Partida | USD/mes | USD/año | Notas |
|---------|---------|---------|-------|
| Herramientas (email, CRM, analytics) | 50 | 600 | Brevo, Notion, Plausible |
| Google Ads | 0→500 | 3.000 | Escalar desde mes 7 |
| Diseño / one-pagers | — | 500 | One-time |
| Eventos / webinars | 100 | 1.200 | Zoom, swag mínimo |
| Contenido freelance (opcional) | 0→200 | 1.200 | Artículos SEO |
| **Total lean** | **~150→750** | **~6.500** | Sin contar tiempo founder |

**Regla:** CAC máximo = 3 meses de ARPU del tier objetivo. Para Pro (USD 29), CAC ≤ USD 87.

---

## 13. Riesgos y mitigaciones

| Riesgo | Probabilidad | Impacto | Mitigación |
|--------|--------------|---------|------------|
| "Datos gratis en DMC" | Alta | Alto | Mensaje ROI tiempo; demos; casos con horas ahorradas |
| Competidor copia agregación | Media | Medio | QC, velocidad, marca, relaciones B2B |
| Abuso tier gratuito | Media | Medio | Rate limits, detección scraping, ToS |
| Ciclo venta Enterprise largo | Alta | Medio | Pilotos 30 días; proyectos EIA one-shot |
| Churn post-proyecto | Media | Medio | Plan anual, add-ons, email retención |

---

## 14. Gobernanza del plan

- **Revisión mensual:** KPIs vs. metas; ajustar canales.
- **Revisión trimestral:** Pricing, personas, mensajes.
- **Owner:** founder/product (hasta primer hire comercial).
- **Documentos relacionados:** `api/utils/limits.R` (tiers técnicos), `docs/setup/SECURITY.md`, landing `nginx/html/index.html`.

---

## 15. Próximos pasos inmediatos (esta semana)

1. Validar internamente pricing propuesto (sección 5) y alinear tiers en código/web.
2. Redactar landing `/precios` con tabla de planes y FAQ de objeciones.
3. Armar lista de 20 consultoras + 5 contactos LinkedIn para outreach.
4. Publicar tutorial "Primera descarga en 5 minutos".
5. Configurar evento analytics: `signup`, `first_download`, `quota_80`, `upgrade_click`.

---

*Documento vivo. Actualizar tras cada revisión trimestral o cambio de pricing/producto.*
