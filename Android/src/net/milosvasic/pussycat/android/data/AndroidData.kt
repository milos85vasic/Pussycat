package net.milosvasic.pussycat.android.data

import net.milosvasic.pussycat.core.common.DataFilter
import net.milosvasic.pussycat.core.data.Data
import java.util.concurrent.CopyOnWriteArrayList

class AndroidData(filter: DataFilter<CopyOnWriteArrayList<String>, String>) : Data(filter) {

    override fun getTag(line: String): String? {
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.V_LIVE)) {
            return LOGCAT_TAG_TYPE.V_LIVE
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.V_FILESYSTEM)) {
            return LOGCAT_TAG_TYPE.V_FILESYSTEM
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.D_LIVE)) {
            return LOGCAT_TAG_TYPE.D_LIVE
        }
        if (line.containsIgnoreCase(net.milosvasic.pussycat.android.data.LOGCAT_TAG_TYPE.D_FILESYSTEM)) {
            return LOGCAT_TAG_TYPE.D_FILESYSTEM
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.I_LIVE)) {
            return LOGCAT_TAG_TYPE.I_LIVE
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.I_FILESYSTEM)) {
            return LOGCAT_TAG_TYPE.I_FILESYSTEM
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.W_LIVE)) {
            return LOGCAT_TAG_TYPE.W_LIVE
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.W_FILESYSTEM)) {
            return LOGCAT_TAG_TYPE.W_FILESYSTEM
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.E_LIVE)) {
            return LOGCAT_TAG_TYPE.E_LIVE
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.E_FILESYSTEM)) {
            return LOGCAT_TAG_TYPE.E_FILESYSTEM
        }
        return null
    }

}