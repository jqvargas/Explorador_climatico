package cl.rodaja.app.widget

import androidx.compose.runtime.Composable
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.glance.GlanceComposable
import androidx.glance.GlanceModifier
import androidx.glance.Image
import androidx.glance.ImageProvider
import androidx.glance.LocalSize
import androidx.glance.background
import androidx.glance.action.Action
import androidx.glance.action.clickable
import androidx.glance.appwidget.cornerRadius
import androidx.glance.layout.Alignment
import androidx.glance.layout.Box
import androidx.glance.layout.Column
import androidx.glance.layout.ColumnScope
import androidx.glance.layout.ContentScale
import androidx.glance.layout.Row
import androidx.glance.layout.RowScope
import androidx.glance.layout.Spacer
import androidx.glance.layout.fillMaxHeight
import androidx.glance.layout.fillMaxSize
import androidx.glance.layout.fillMaxWidth
import androidx.glance.layout.height
import androidx.glance.layout.padding
import androidx.glance.layout.size
import androidx.glance.layout.width
import androidx.glance.text.FontWeight
import androidx.glance.text.Text
import androidx.glance.text.TextStyle
import androidx.glance.unit.ColorProvider
import cl.rodaja.app.R

private val StaleOnDark = Color(0xFFF6B358)
private val StaleOnClear = Color(0xFFFFE08A)

private enum class Skin(val bg: Int, val stale: Color, val bar: Color) {
    RAIN(R.drawable.wbg_rain, StaleOnDark, Color(0xFF9FD8E8)),
    CLEAR(R.drawable.wbg_clear, StaleOnClear, Color(0xFFFFE08A)),
    FAIR(R.drawable.wbg_fair, StaleOnClear, Color(0xFFFFE08A)),
    CLOUD(R.drawable.wbg_cloud, StaleOnDark, Color(0xFFE9F2F8)),
    NIGHT(R.drawable.wbg_night, StaleOnDark, Color(0xFFC9D6E5)),
    SNOW(R.drawable.wbg_snow, StaleOnDark, Color.White),
    STORM(R.drawable.wbg_storm, StaleOnDark, Color(0xFFF6B358)),
    ALERT(R.drawable.wbg_alert, StaleOnDark, Color(0xFFF6B358))
}

@Composable
@GlanceComposable
internal fun PronosticoCard(
    frame: WidgetFrame,
    notice: String?,
    onOpen: Action?,
    onLocation: Action?,
    onRefresh: Action?,
    syncedAt: Long = 0L
) {
    val banner = shownBanner(frame, notice)
    val root = if (!frame.numbers && banner == WidgetFrame.BANNER_NO_LOCATION) onLocation else onOpen
    if (!frame.numbers) {
        Message(banner ?: "", root)
        return
    }
    val alert = !frame.alertVariable.isNullOrEmpty()
    val skin = if (alert) Skin.ALERT else skinOf(frame.iconStem)
    val reloj = WidgetText.reloj(frame.updatedAt, syncedAt, System.currentTimeMillis())
    val place = if (frame.name.isNotEmpty()) frame.name else frame.comuna
    val condicion = if (frame.conditionCode == null) "" else frame.iconLabel
    val title = WidgetText.proximaLluvia(frame.hourH, frame.hourMm, condicion)
    val width = LocalSize.current.width
    val height = LocalSize.current.height
    val full = width >= 180.dp && height >= 150.dp
    val footerDp = if (reloj.source) 36 else 18
    val plot = (height.value - 26 - 20 - 8 - footerDp - 42).toInt().coerceIn(56, 76)
    Box(modifier = GlanceModifier.fillMaxSize().cornerRadius(28.dp).tap(root)) {
        Image(
            provider = ImageProvider(skin.bg),
            contentDescription = null,
            modifier = GlanceModifier.fillMaxSize(),
            contentScale = ContentScale.FillBounds
        )
        Column(
            modifier = GlanceModifier
                .fillMaxSize()
                .padding(start = 16.dp, top = 14.dp, end = 16.dp, bottom = 12.dp)
        ) {
            Header(
                place,
                showLogo = !alert && width >= 180.dp && !(reloj.warn && !full),
                showAlert = alert,
                refresh = if (!full && reloj.warn) reloj.compact else null,
                refreshColor = skin.stale,
                onRefresh = onRefresh
            )
            Spacer(modifier = GlanceModifier.height(6.dp))
            // Sin serie horaria ni alerta en el JSON: no hay columna de barras ni pastilla.
            if (width < 180.dp) {
                Square(frame, title)
            } else if (!full) {
                Strip(frame, title)
            } else {
                WideBody(frame, title, skin.bar, plot)
                Footer(frame, reloj, skin, onRefresh)
            }
        }
    }
}

@Composable
@GlanceComposable
private fun Message(text: String, action: Action?) {
    Box(modifier = GlanceModifier.fillMaxSize().cornerRadius(28.dp).tap(action)) {
        Image(
            provider = ImageProvider(R.drawable.wbg_cloud),
            contentDescription = null,
            modifier = GlanceModifier.fillMaxSize(),
            contentScale = ContentScale.FillBounds
        )
        Box(
            modifier = GlanceModifier.fillMaxSize().padding(16.dp),
            contentAlignment = Alignment.Center
        ) {
            Text(text = text, style = ink(14, FontWeight.Medium), maxLines = 3)
        }
    }
}

@Composable
@GlanceComposable
private fun Header(
    place: String,
    showLogo: Boolean,
    showAlert: Boolean,
    refresh: String?,
    refreshColor: Color,
    onRefresh: Action?
) {
    Row(
        modifier = GlanceModifier.fillMaxWidth(),
        verticalAlignment = Alignment.CenterVertically
    ) {
        Image(
            provider = ImageProvider(R.drawable.ic_w_pin),
            contentDescription = null,
            modifier = GlanceModifier.size(14.dp)
        )
        Spacer(modifier = GlanceModifier.width(4.dp))
        Text(
            text = place,
            style = ink(13, FontWeight.Medium),
            maxLines = 1,
            modifier = GlanceModifier.defaultWeight()
        )
        if (!refresh.isNullOrEmpty()) {
            Text(
                text = refresh,
                style = ink(11, FontWeight.Medium, refreshColor),
                maxLines = 1,
                modifier = GlanceModifier.tap(onRefresh)
            )
        } else if (showAlert) {
            AlertPill()
        } else if (showLogo) {
            Image(
                provider = ImageProvider(R.drawable.ic_w_mark),
                contentDescription = null,
                modifier = GlanceModifier.size(10.dp)
            )
            Spacer(modifier = GlanceModifier.width(4.dp))
            Text(text = "rodaja", style = ink(11, FontWeight.Medium))
        }
    }
}

@Composable
@GlanceComposable
private fun AlertPill() {
    Row(
        modifier = GlanceModifier
            .background(Color(0xFFF6B358))
            .cornerRadius(99.dp)
            .padding(start = 6.dp, top = 3.dp, end = 9.dp, bottom = 3.dp),
        verticalAlignment = Alignment.CenterVertically
    ) {
        Image(
            provider = ImageProvider(R.drawable.ic_w_warn),
            contentDescription = null,
            modifier = GlanceModifier.size(13.dp)
        )
        Spacer(modifier = GlanceModifier.width(4.dp))
        Text(text = "Alerta", style = ink(11, FontWeight.Bold, Color(0xFF1C1303)), maxLines = 1)
    }
}

@Composable
@GlanceComposable
private fun ColumnScope.WideBody(frame: WidgetFrame, title: String, bar: Color, plot: Int) {
    Row(
        modifier = GlanceModifier.defaultWeight().fillMaxWidth(),
        verticalAlignment = Alignment.Bottom
    ) {
        Column(modifier = GlanceModifier.defaultWeight(), verticalAlignment = Alignment.Bottom) {
            Row(verticalAlignment = Alignment.CenterVertically) {
                Sky(frame, 48)
                Spacer(modifier = GlanceModifier.width(6.dp))
                Text(text = WidgetText.tempEntera(frame.currentTemp), style = tempStyle(), maxLines = 1)
            }
            if (title.isNotEmpty()) {
                Spacer(modifier = GlanceModifier.height(4.dp))
                Text(text = title, style = ink(15, FontWeight.Medium), maxLines = 2)
            }
        }
        if (frame.hourH.isNotEmpty()) {
            Spacer(modifier = GlanceModifier.width(8.dp))
            Chart(frame, bar, plot)
        }
    }
}

@Composable
@GlanceComposable
private fun RowScope.Chart(frame: WidgetFrame, bar: Color, plot: Int) {
    val n = minOf(frame.hourH.size, frame.hourMm.size)
    val ceiling = WidgetText.techo(frame.hourMm)
    val peakAt = WidgetText.picoIndex(frame.hourMm)
    Column(modifier = GlanceModifier.defaultWeight()) {
        Text(text = "Lluvia (mm/h)", style = ink(10, FontWeight.Normal), maxLines = 1)
        Spacer(modifier = GlanceModifier.height(2.dp))
        Row(modifier = GlanceModifier.fillMaxWidth().height(12.dp)) {
            for (i in 0 until n) {
                Box(
                    modifier = GlanceModifier.defaultWeight().fillMaxHeight(),
                    contentAlignment = Alignment.Center
                ) {
                    if (i == peakAt) {
                        Text(
                            text = WidgetText.pico(frame.hourMm[i]),
                            style = ink(10, FontWeight.Medium),
                            maxLines = 1
                        )
                    }
                }
            }
        }
        Row(
            modifier = GlanceModifier.fillMaxWidth().height(plot.dp),
            verticalAlignment = Alignment.Bottom
        ) {
            for (i in 0 until n) {
                val mm = frame.hourMm[i]
                val bh = (plot * mm / ceiling)
                Box(
                    modifier = GlanceModifier.defaultWeight().fillMaxHeight(),
                    contentAlignment = Alignment.BottomCenter
                ) {
                    if (bh >= 3.0) {
                        Box(
                            modifier = GlanceModifier
                                .width(14.dp)
                                .height(bh.dp)
                                .background(bar)
                                .cornerRadius(3.dp)
                        ) {}
                    } else {
                        Box(
                            modifier = GlanceModifier
                                .size(3.dp)
                                .background(Color(0x73FFFFFF))
                                .cornerRadius(2.dp)
                        ) {}
                    }
                }
            }
        }
        Spacer(
            modifier = GlanceModifier
                .fillMaxWidth()
                .height(1.dp)
                .background(Color(0x40FFFFFF))
        )
        Row(modifier = GlanceModifier.fillMaxWidth().height(13.dp)) {
            for (i in 0 until n) {
                Box(
                    modifier = GlanceModifier.defaultWeight().fillMaxHeight(),
                    contentAlignment = Alignment.Center
                ) {
                    Text(
                        text = frame.hourH[i].toString(),
                        style = ink(10, FontWeight.Normal),
                        maxLines = 1
                    )
                }
            }
        }
    }
}

@Composable
@GlanceComposable
private fun Strip(frame: WidgetFrame, title: String) {
    Row(
        modifier = GlanceModifier.fillMaxWidth(),
        verticalAlignment = Alignment.CenterVertically
    ) {
        Sky(frame, 32)
        Spacer(modifier = GlanceModifier.width(6.dp))
        Text(text = WidgetText.tempEntera(frame.currentTemp), style = tempStyle(28), maxLines = 1)
        if (title.isNotEmpty()) {
            Spacer(modifier = GlanceModifier.width(8.dp))
            Text(
                text = title,
                style = ink(14, FontWeight.Medium),
                maxLines = 2,
                modifier = GlanceModifier.defaultWeight()
            )
        }
    }
}

@Composable
@GlanceComposable
private fun Square(frame: WidgetFrame, title: String) {
    Row(verticalAlignment = Alignment.CenterVertically) {
        Sky(frame, 28)
        Spacer(modifier = GlanceModifier.width(4.dp))
        Text(text = WidgetText.tempEntera(frame.currentTemp), style = tempStyle(22), maxLines = 1)
    }
    if (title.isNotEmpty()) {
        Spacer(modifier = GlanceModifier.height(2.dp))
        Text(text = title, style = ink(13, FontWeight.Medium), maxLines = 1)
    }
}

@Composable
@GlanceComposable
private fun Footer(
    frame: WidgetFrame,
    reloj: WidgetText.Reloj,
    skin: Skin,
    onRefresh: Action?
) {
    val rain = if (frame.alertVariable.equals("pp", ignoreCase = true)) {
        WidgetText.prevista(frame.alertValor)
    } else {
        WidgetText.lluviaHoy(frame.precipMm)
    }
    val lo = WidgetText.tempEntera(frame.tmin)
    val hi = WidgetText.tempEntera(frame.tmax)
    val prob = WidgetText.prob(frame.precipProb)
    Column(modifier = GlanceModifier.fillMaxWidth()) {
        Row(
            modifier = GlanceModifier.fillMaxWidth(),
            verticalAlignment = Alignment.CenterVertically
        ) {
            if (lo.isNotEmpty()) {
                FootBit(R.drawable.ic_w_lo, lo, Color(0xFF9FD8E8))
                Spacer(modifier = GlanceModifier.width(8.dp))
            }
            if (hi.isNotEmpty()) {
                FootBit(R.drawable.ic_w_hi, hi, Color(0xFFF6B358))
                Spacer(modifier = GlanceModifier.width(8.dp))
            }
            if (prob.isNotEmpty()) {
                FootBit(R.drawable.ic_w_pop, prob, Color(0xD9FFFFFF))
                Spacer(modifier = GlanceModifier.width(8.dp))
            }
            if (rain.isNotEmpty()) {
                FootBit(R.drawable.ic_w_drop, rain, Color(0xD9FFFFFF))
            }
            Spacer(modifier = GlanceModifier.defaultWeight())
            if (!reloj.source && reloj.compact.isNotEmpty()) {
                val color = if (reloj.warn) skin.stale else Color(0xD9FFFFFF)
                val weight = if (reloj.warn) FontWeight.Medium else FontWeight.Normal
                Text(
                    text = reloj.compact,
                    style = ink(11, weight, color),
                    maxLines = 1,
                    modifier = GlanceModifier.tap(if (reloj.warn) onRefresh else null)
                )
            }
        }
        if (reloj.source && reloj.text.isNotEmpty()) {
            Spacer(modifier = GlanceModifier.height(2.dp))
            Row(modifier = GlanceModifier.fillMaxWidth(), verticalAlignment = Alignment.CenterVertically) {
                Spacer(modifier = GlanceModifier.defaultWeight())
                Text(
                    text = reloj.text,
                    style = ink(11, FontWeight.Medium, skin.stale),
                    maxLines = 1,
                    modifier = GlanceModifier.tap(onRefresh)
                )
            }
        }
    }
}

@Composable
@GlanceComposable
private fun FootBit(icon: Int, text: String, color: Color) {
    Row(verticalAlignment = Alignment.CenterVertically) {
        Image(
            provider = ImageProvider(icon),
            contentDescription = null,
            modifier = GlanceModifier.size(12.dp)
        )
        Spacer(modifier = GlanceModifier.width(3.dp))
        Text(text = text, style = ink(11, FontWeight.Normal, color), maxLines = 1)
    }
}

@Composable
private fun Sky(frame: WidgetFrame, dp: Int) {
    val id = WmoIcon.drawableRes(frame.iconStem)
    Image(
        provider = ImageProvider(if (id != 0) id else R.drawable.w_nublado),
        contentDescription = frame.iconLabel,
        modifier = GlanceModifier.size(dp.dp)
    )
}

private fun skinOf(stem: String?): Skin {
    return when (stem) {
        "lluvia", "lluvia-fuerte", "llovizna", "lluvia-y-sol", "lluvia-y-luna" -> Skin.RAIN
        "nube-truenos", "lluvia-con-granizo" -> Skin.STORM
        "soleado" -> Skin.CLEAR
        "parcial-soleado", "parcial-nublado" -> Skin.FAIR
        "luna", "luna-parcial", "luna-nubes" -> Skin.NIGHT
        "nieve", "nieve-hielo", "lluvia-y-nieve" -> Skin.SNOW
        else -> Skin.CLOUD
    }
}

private fun shownBanner(frame: WidgetFrame, notice: String?): String? {
    if (!frame.banner.isNullOrEmpty()) {
        return frame.banner
    }
    if (!notice.isNullOrEmpty()) {
        return notice
    }
    return null
}

private fun GlanceModifier.tap(action: Action?): GlanceModifier {
    if (action == null) {
        return this
    }
    return this.clickable(action)
}

private fun ink(size: Int, weight: FontWeight, color: Long = 0xEBFFFFFF): TextStyle {
    return TextStyle(
        color = ColorProvider(Color(color)),
        fontSize = size.sp,
        fontWeight = weight
    )
}

private fun ink(size: Int, weight: FontWeight, color: Color): TextStyle {
    return TextStyle(
        color = ColorProvider(color),
        fontSize = size.sp,
        fontWeight = weight
    )
}

private fun tempStyle(size: Int = 56): TextStyle {
    return TextStyle(
        color = ColorProvider(Color.White),
        fontSize = size.sp,
        fontWeight = FontWeight.Normal
    )
}
