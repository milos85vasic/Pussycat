package net.milosvasic.pussycat.android.command

import net.milosvasic.pussycat.core.COMMAND

class ANDROID_COMMAND(value: String) : COMMAND(value) {

    companion object {
        val PARENT = COMMAND
        val CHOOSE = COMMAND("CHOOSE")
        val DEVICES = COMMAND("DEVICES")
        val TAG = COMMAND("TAG")

        init {
            COMMAND.add(CHOOSE.value)
            COMMAND.add(DEVICES.value)
            COMMAND.add(TAG.value)
        }

        fun get(toFind: String): COMMAND {
            return COMMAND.get(toFind)
        }
    }

}