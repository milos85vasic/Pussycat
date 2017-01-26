package net.milosvasic.pussycat.gui.configuration

import net.milosvasic.pussycat.listeners.Listeners
import java.util.concurrent.atomic.AtomicBoolean


class PussycatMainWindowConfiguration {

    private val scrollbarAnchored = AtomicBoolean(true)
    val scrollbarAnchoring: Listeners<Boolean> = Listeners.obtain()

    fun isScrollbarAnchored(): Boolean {
        return scrollbarAnchored.get()
    }

    fun setScrollbarAnchored(anchored: Boolean) {
        scrollbarAnchored.set(anchored)
        scrollbarAnchoring.notify(scrollbarAnchored.get())
    }

}