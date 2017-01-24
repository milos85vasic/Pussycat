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

    class PussycatScrollBarUI(val theme: Theme) : BasicScrollBarUI() {

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
                val rW = w / 4
                g2.fillRoundRect(rW, rW, w - (rW), h - (rW), w / 2, w / 2)
            }
        }

        override fun setThumbBounds(x: Int, y: Int, width: Int, height: Int) {
            super.setThumbBounds(x, y, width, height)
            scrollbar.repaint()
        }

        private fun createZeroButton(): JButton {
            val jbutton = JButton()
            jbutton.preferredSize = Dimension(0, 0)
            jbutton.minimumSize = Dimension(0, 0)
            jbutton.maximumSize = Dimension(0, 0)
            return jbutton
        }

    }

}