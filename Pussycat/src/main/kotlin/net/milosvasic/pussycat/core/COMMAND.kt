package net.milosvasic.pussycat.core


open class COMMAND(val value: String) {

    companion object {
        val list = mutableListOf<COMMAND>()

        val UNKNOWN = COMMAND("UNKNOWN")
        val LIVE = COMMAND("LIVE")
        val FILESYSTEM = COMMAND("FILESYSTEM")
        val PAUSE = COMMAND("PAUSE")
        val RESUME = COMMAND("RESUME")
        val STOP = COMMAND("STOP")
        val CLEAR = COMMAND("CLEAR")
        val RESET = COMMAND("RESET")
        val STATUS = COMMAND("STATUS")
        val EXPORT = COMMAND("EXPORT")

        init {
            list.add(UNKNOWN)
            list.add(LIVE)
            list.add(FILESYSTEM)
            list.add(PAUSE)
            list.add(RESUME)
            list.add(STOP)
            list.add(CLEAR)
            list.add(RESET)
            list.add(STATUS)
            list.add(EXPORT)
        }

        fun get(toFind: String): COMMAND {
            val cmdVal = toFind.toUpperCase()
            for (cmd in list) {
                if (cmd.value == cmdVal) {
                    return cmd
                }
            }
            return list[0]
        }

        fun add(toAdd: String) {
            val cmdVal = toAdd.toUpperCase()
            if (get(cmdVal) == UNKNOWN) {
                list.add(COMMAND(cmdVal))
            }
        }
    }

    override fun equals(other: Any?): Boolean {
        if (other is COMMAND) {
            return value == other.value
        }
        return false
    }

    override fun hashCode(): Int {
        return value.hashCode()
    }

}