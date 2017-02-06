package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import net.milosvasic.pussycat.os.OS
import java.awt.BorderLayout
import java.awt.Dimension
import java.awt.event.WindowEvent
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import javax.swing.JFrame
import java.awt.Window
import java.awt.Toolkit


abstract class PussycatWindow(val theme: Theme) : JFrame() {

    private var favicon: BufferedImage? = null

    init {
        if (OS.isMacOS()) {
            val toolkit = Toolkit.getDefaultToolkit()
            val screenSize = toolkit.screenSize
            val scnMax = toolkit.getScreenInsets(graphicsConfiguration)
            val top = scnMax.top
            val bottom = scnMax.bottom
            minimumSize = Dimension(screenSize.width, screenSize.height - bottom - top)
        }
        extendedState = JFrame.MAXIMIZED_BOTH
        favicon = ImageIO.read(javaClass.classLoader.getResourceAsStream("icons/Favicon.png"))
    }

    open fun open() {
        initialize()
        isVisible = true
    }

    open fun close() {
        isVisible = false
        dispatchEvent(WindowEvent(this, WindowEvent.WINDOW_CLOSING))
        dispose()
    }

    protected open fun initialize() {
        iconImage = favicon
        layout = BorderLayout()
        background = theme.getColor(TYPE.BASE, INTENSITY.LIGHT)
    }

    fun enableOSXFullscreen() {
        try {
            val util = Class.forName("com.apple.eawt.FullScreenUtilities")
            val params = arrayOf<Class<*>>(Window::class.java, java.lang.Boolean.TYPE)
            val method = util.getMethod("setWindowCanFullScreen", *params)
            method.invoke(util, this, true)
        } catch (e: Exception) {
            println(e)
        }
    }

    fun requestOSXFullscreen(window: Window) {
        try {
            val appClass = Class.forName("com.apple.eawt.Application")
            val params = arrayOf<Class<*>>()
            val getApplication = appClass.getMethod("getApplication", *params)
            val application = getApplication.invoke(appClass)
            val requestToggleFulLScreen = application.javaClass.getMethod("requestToggleFullScreen", Window::class.java)
            requestToggleFulLScreen.invoke(application, window)
        } catch (e: Exception) {
            println(e)
        }
    }


}
