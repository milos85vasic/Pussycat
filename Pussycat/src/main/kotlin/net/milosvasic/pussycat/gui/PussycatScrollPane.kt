package net.milosvasic.pussycat.gui


import net.milosvasic.pussycat.gui.events.SCROLLING_EVENT
import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import net.milosvasic.pussycat.listeners.Listeners
import java.awt.*
import javax.swing.JComponent
import javax.swing.JScrollPane
import javax.swing.plaf.basic.BasicScrollBarUI
import javax.swing.JButton
import java.awt.RenderingHints
import java.awt.event.AdjustmentEvent
import java.awt.event.AdjustmentListener


class PussycatScrollPane(val theme: Theme) : JScrollPane(), AdjustmentListener {

    val screenSize: Dimension = Toolkit.getDefaultToolkit().screenSize
    val scrollingEvents: Listeners<SCROLLING_EVENT> = Listeners.obtain()

    init {
        isOpaque = true
        background = theme.getColor(TYPE.BASE, INTENSITY.DARK)
        border = theme.getBorder(this)
        verticalScrollBar.ui = PussycatScrollBarUI(theme)
        horizontalScrollBar.ui = PussycatScrollBarUI(theme)
        verticalScrollBar.unitIncrement = 16
        horizontalScrollBar.unitIncrement = 16
        verticalScrollBar.addAdjustmentListener(this)
    }

    override fun paintComponent(g: Graphics?) {
        super.paintComponent(g)
        g?.color = background
        g?.fillRect(0, 0, width, height)
    }

    override fun paintBorder(g: Graphics?) {
        super.paintBorder(g)
        g?.color = background
        g?.fillRect(0, 0, width, height)
    }

    inner class PussycatScrollBarUI(val theme: Theme) : BasicScrollBarUI() {

        override fun createDecreaseButton(orientation: Int): JButton {
            return createZeroButton()
        }

        override fun createIncreaseButton(orientation: Int): JButton {
            return createZeroButton()
        }

        override fun paintTrack(g: Graphics?, c: JComponent?, trackBounds: Rectangle?) {
            super.paintTrack(g, c, trackBounds)
            if (trackBounds != null && g != null) {
                val w = trackBounds.width
                val h = trackBounds.height
                g.color = theme.getColor(TYPE.BASE, INTENSITY.DARK)
                g.fillRect(0, 0, w, h)
            }
        }

        override fun paintThumb(g: Graphics?, c: JComponent?, thumbBounds: Rectangle?) {
            if (!scrollbar.isEnabled) {
                return
            }
            if (thumbBounds != null && g != null) {
                val w = thumbBounds.width
                val h = thumbBounds.height
                var color = theme.getColor(TYPE.MAIN_COLOR_1, INTENSITY.DARK)
                if (isDragging) {
                    color = theme.getColor(TYPE.MAIN_COLOR_1, INTENSITY.MEDIUM)
                } else if (isThumbRollover) {
                    color = theme.getColor(TYPE.MAIN_COLOR_1, INTENSITY.LIGHT)
                }
                val g2 = g as Graphics2D
                val qualityHints = RenderingHints(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
                qualityHints.put(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
                g2.setRenderingHints(qualityHints)
                g2.translate(thumbBounds.x, thumbBounds.y)
                g2.color = color
                val corners = 10
                if (componentOrientation.isHorizontal) {
                    g2.fillRoundRect(0, 0, w, h, corners, corners)
                } else {
                    g2.fillRoundRect(0, 0, w, h, corners, corners)
                }
            }
        }

        override fun setThumbBounds(x: Int, y: Int, width: Int, height: Int) {
            val border = theme.getBorder(this@PussycatScrollPane)
            val borderValue = border.getBorderInsets(this@PussycatScrollPane).left * 4
            super.setThumbBounds(x + borderValue, y, width - borderValue, height)
            scrollbar.repaint()
        }

        private fun createZeroButton(): JButton {
            val jButton = JButton()
            jButton.preferredSize = Dimension(0, 0)
            jButton.minimumSize = Dimension(0, 0)
            jButton.maximumSize = Dimension(0, 0)
            return jButton
        }

    }

    override fun adjustmentValueChanged(e: AdjustmentEvent?) {
        val extent = verticalScrollBar.model.extent
        val value = verticalScrollBar.value
        val max = verticalScrollBar.maximum
        val position = value + extent
        val topDelta = (screenSize.height / 3) + extent
        val bottomDelta = max - (screenSize.height / 2)
        if (position <= topDelta) {
            scrollingEvents.notify(SCROLLING_EVENT.TOP_DELTA_REACHED)
        } else
            if (position >= bottomDelta) {
                scrollingEvents.notify(SCROLLING_EVENT.BOTTOM_DELTA_REACHED)
            }
        when (position) {
            max -> {
                scrollingEvents.notify(SCROLLING_EVENT.BOTTOM_REACHED)
            }
            extent -> {
                scrollingEvents.notify(SCROLLING_EVENT.TOP_REACHED)
            }
        }
    }

}
