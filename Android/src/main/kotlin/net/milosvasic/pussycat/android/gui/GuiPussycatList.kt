package net.milosvasic.pussycat.android.gui

import net.milosvasic.pussycat.android.data.AndroidLogCatMessage
import net.milosvasic.pussycat.gui.PussycatList
import net.milosvasic.pussycat.gui.theme.Theme
import java.util.concurrent.CopyOnWriteArrayList


class GuiPussycatList(items: CopyOnWriteArrayList<AndroidLogCatMessage>, theme: Theme): PussycatList<AndroidLogCatMessage>(items, theme)