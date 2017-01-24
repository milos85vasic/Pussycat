package net.milosvasic.pussycat.gui


import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import java.awt.*
import javax.swing.JComponent
import javax.swing.JScrollPane
import javax.swing.plaf.basic.BasicScrollBarUI
import javax.swing.JButton
import java.awt.RenderingHints


class PussycatScrollPane(val theme: Theme) : JScrollPane() {

    init {
        isOpaque = true
        background = theme.getColor(TYPE.BASE, INTENSITY.DARK)
        border = theme.getBorder(this)
        verticalScrollBar.ui = PussycatScrollBarUI(theme)
        horizontalScrollBar.ui = PussycatScrollBarUI(theme)
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
                g2.fillRoundRect(0, 0, w, h, w / 2, w / 2)
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

}