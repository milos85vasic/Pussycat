package net.milosvasic.pussycat.android.gui

import java.awt.Color
import javax.swing.JPanel
import javax.imageio.ImageIO
import java.awt.image.BufferedImage


class PussycatSplashPanel(splashWidth: Int, splashHeight: Int) : JPanel() {

    private var image: BufferedImage? = null

    init {
        image = ImageIO.read(javaClass.classLoader.getResourceAsStream("splash/Pussycat.png"))
        background = Color.BLUE
        setSize(splashWidth, splashHeight)
    }

}