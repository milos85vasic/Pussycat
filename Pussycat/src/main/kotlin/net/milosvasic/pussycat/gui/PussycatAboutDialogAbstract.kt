package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.application.ApplicationInformation
import net.milosvasic.pussycat.gui.theme.Theme
import java.awt.*
import java.awt.event.WindowEvent
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import javax.swing.*
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder


abstract class PussycatAboutDialogAbstract(val information: ApplicationInformation, val theme: Theme) : JFrame() {

    protected val splashWidth = 640
    protected val splashHeight = 389
    protected var favicon: BufferedImage? = null

    fun open() {
//        if (owner.isVisible) {
//            val screenLocation = owner.locationOnScreen
//            setLocation(
//                    screenLocation.x + (owner.width / 2) - (splashWidth / 2),
//                    screenLocation.y + (owner.height / 2) - (splashHeight / 2)
//            )
//        } else {
//            val screenSize = Toolkit.getDefaultToolkit().screenSize
//            setLocation(
//                    (screenSize.width / 2) - (splashWidth / 2),
//                    (screenSize.height / 2) - (splashHeight / 2)
//            )
//        }
        val screenSize = Toolkit.getDefaultToolkit().screenSize
        setLocation(
                (screenSize.width / 2) - (splashWidth / 2),
                (screenSize.height / 2) - (splashHeight / 2)
        )
        setSize(splashWidth, splashHeight)
        val body = PussycatSplashPanel(splashWidth, splashHeight)
        val header = generateHeader()
        add(body)
        add(header, BorderLayout.NORTH)
        add(generateFooter(), BorderLayout.SOUTH)
        body.isVisible = true
        favicon = ImageIO.read(javaClass.classLoader.getResourceAsStream("icons/Favicon.png"))
        setIconImage(favicon)
        isVisible = true
    }

    fun close() {
        isVisible = false
        dispatchEvent(WindowEvent(this, WindowEvent.WINDOW_CLOSING))
        dispose()
    }

    abstract protected fun generateFooter(): JComponent?

    private fun generateHeader(): JLabel {
        val splashLabel = JLabel()
        splashLabel.isOpaque = true
        splashLabel.background = Color.BLACK
        val dimension = Dimension(splashWidth / 4, splashHeight / 4)
        splashLabel.minimumSize = dimension
        splashLabel.maximumSize = dimension
        splashLabel.preferredSize = dimension
        splashLabel.text = "<html><font color='white'>${information.name}, Version: ${information.version}<br />Author: ${information.author}<br />Website: ${information.website}</font></html>"
        splashLabel.horizontalAlignment = SwingConstants.RIGHT
        splashLabel.border = CompoundBorder(splashLabel.border, EmptyBorder(10, 10, 10, 10))
        splashLabel.isVisible = true
        return splashLabel
    }

}