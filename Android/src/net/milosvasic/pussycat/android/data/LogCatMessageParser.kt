package net.milosvasic.pussycat.android.data

import com.android.ddmlib.IDevice
import com.android.ddmlib.Log
import com.android.ddmlib.logcat.LogCatMessage
import com.google.common.primitives.Ints
import java.util.*
import java.util.regex.Pattern


class LogCatMessageParser {

    private var mCurLogLevel: Log.LogLevel? = Log.LogLevel.WARN
    private var mCurPid = "?"
    private var mCurTid = "?"
    private var mCurTag = "?"
    private var mCurTime = "?:??"

    /**
     * This pattern is meant to parse the first line of a log message with the option
     * 'logcat -v long'. The first line represents the date, tag, severity, etc.. while the
     * following lines are the message (can be several lines).<br></br>
     * This first line looks something like:<br></br>
     * `"[ 00-00 00:00:00.000 <pid>:0x<???> <severity>/<tag>]"`
     * <br></br>
     * Note: severity is one of V, D, I, W, E, A? or F. However, there doesn't seem to be
     * a way to actually generate an A (assert) message. Log.wtf is supposed to generate
     * a message with severity A, however it generates the undocumented F level. In
     * such a case, the parser will change the level from F to A.<br></br>
     * Note: the fraction of second value can have any number of digit.<br></br>
     * Note: the tag should be trimmed as it may have spaces at the end.
     */
    private val sLogHeaderPattern = Pattern.compile(
            "\\d+-\\d+\\s+\\d+:\\d+:\\d+.\\d+\\s+\\d+\\s+\\d+\\s+\\w\\s+(.*):\\s+(.*)"
    )

//    private val sLogHeaderPattern = Pattern.compile(
//            "^\\[\\s(\\d\\d-\\d\\d\\s\\d\\d:\\d\\d:\\d\\d\\.\\d+)" + "\\s+(\\d*):\\s*(\\S+)\\s([VDIWEAF])/(.*)\\]$"
//    )


    fun processLogLines(lines: Array<String>, device: IDevice?): List<LogCatMessage> {
        val messages = ArrayList<LogCatMessage>(lines.size)

        for (line in lines) {
            if (line.isEmpty()) {
                continue
            }

            val matcher = sLogHeaderPattern.matcher(line)
            if (matcher.matches()) {
                println("MATCHING")

//                mCurTime = matcher.group(1)
//                mCurPid = matcher.group(2)
//                mCurTid = matcher.group(3)
//                mCurLogLevel = Log.LogLevel.getByLetterString(matcher.group(4))
//                mCurTag = matcher.group(5).trim { it <= ' ' }
//
//                if (mCurLogLevel == null && matcher.group(4) == "F") {
//                    mCurLogLevel = Log.LogLevel.ASSERT
//                }
            } else {
                println("NO MATCHES")
//                var pkgName = "" //$NON-NLS-1$
//                val pid = Ints.tryParse(mCurPid)
//                if (pid != null && device != null) {
//                    pkgName = device.getClientName(pid)
//                }
//                val m = LogCatMessage(mCurLogLevel, mCurPid, mCurTid,
//                        pkgName, mCurTag, mCurTime, line)
//                messages.add(m)
            }
        }

        return messages
    }

}