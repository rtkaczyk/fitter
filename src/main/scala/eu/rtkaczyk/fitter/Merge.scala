package eu.rtkaczyk.fitter

import java.io.FileInputStream

import com.garmin.fit._

class Merge extends Mode {
  def apply(args: List[String]): Unit = {

    val inputs = args.init
    val output = args.last

    val encoder = new FileEncoder(new java.io.File(output), Fit.ProtocolVersion.V1_0)
    val listener = new Listener(encoder, inputs.size)

    for ((input, i) <- inputs.zipWithIndex) {
      val decoder = new Decode
      val in = new FileInputStream(input)

      decoder.read(in, listener, listener)

      in.close()
      listener.nextFile()
    }

    encoder.close()
  }

  class Listener(encoder: FileEncoder, noFiles: Int)
    extends MesgListener with MesgDefinitionListener {

    var lastSession: SessionMesg = null
    var lastLap: LapMesg = null

    def lastSessionTimestamp: Long =
      Option(lastSession).map(_.getTimestamp.getTimestamp.toLong).getOrElse(0L)

    def lastSessionDistance: Float =
      Option(lastSession).map(_.getTotalDistance.toFloat).getOrElse(0.0f)

    var i = 0

    def nextFile(): Unit =
      i += 1

    def first: Boolean = i == 0
    def last: Boolean = i == noFiles - 1


    def onMesg(mesg: Mesg): Unit = mesg.getNum match {
      case MesgNum.FILE_ID | MesgNum.FILE_CREATOR =>
        if (first)
          encoder.write(mesg)

      case MesgNum.SESSION =>
        mergeSession(new SessionMesg(mesg))

      case MesgNum.LAP =>
        val lap = new LapMesg(mesg)
        if (first)
          lastLap = lap
        else
          mergeLap(lap)
        if (last)
          encoder.write(lastLap)

      case MesgNum.ACTIVITY =>


      case 209 | 104 | 22 | 210 | 219 =>
        val t = mesg.getField(253).getIntegerValue + lastSessionTimestamp.toInt
        mesg.setFieldValue(253, t)
        encoder.write(mesg)

      case MesgNum.RECORD =>
        updateRecord(mesg)
        encoder.write(mesg)

      case _ =>
        updateTimestamp(mesg)
        encoder.write(mesg)
    }

    def onMesgDefinition(mesg: MesgDefinition): Unit = mesg.getNum match {
      case MesgNum.FILE_ID | MesgNum.FILE_CREATOR =>
        if (first)
          encoder.write(mesg)

      case MesgNum.ACTIVITY | MesgNum.SESSION | MesgNum.EVENT =>

      case MesgNum.LAP =>
        if (last)
          encoder.write(mesg)

      case _ =>
        encoder.write(mesg)
    }


    def mergeSession(curr: SessionMesg): Unit = {
      curr.setTotalDistance(curr.getTotalDistance + lastSessionDistance)
      updateTimestamp(curr)
      lastSession = curr
    }

    def mergeLap(curr: LapMesg): Unit = {
      lastLap.setEndPositionLat(curr.getEndPositionLat)
      lastLap.setEndPositionLong(curr.getEndPositionLong)
      lastLap.setTotalAscent(lastLap.getTotalAscent + curr.getTotalAscent)
      lastLap.setTotalDescent(lastLap.getTotalDescent + curr.getTotalDescent)
      lastLap.setTotalTimerTime(lastLap.getTotalTimerTime + curr.getTotalTimerTime)
      lastLap.setTotalElapsedTime(lastLap.getTotalElapsedTime + curr.getTotalElapsedTime)
      lastLap.setMaxAltitude(math.max(lastLap.getMaxAltitude, curr.getMaxAltitude))
      lastLap.setMaxSpeed(math.max(lastLap.getMaxSpeed, curr.getMaxSpeed))
      lastLap.setTotalDistance(lastLap.getTotalDistance + curr.getTotalDistance)
      lastLap.setEnhancedAvgSpeed(lastLap.getTotalDistance / lastLap.getTotalTimerTime)
      lastLap.setFieldValue(SessionMesg.NecLatFieldNum, curr.getFieldValue(SessionMesg.NecLatFieldNum))
      lastLap.setFieldValue(SessionMesg.NecLongFieldNum, curr.getFieldValue(SessionMesg.NecLongFieldNum))
      updateTimestamp(lastLap)
    }

    def updateRecord(mesg: Mesg): Unit = {
      val record = new RecordMesg(mesg)
      record.setDistance(record.getDistance + lastSessionDistance)
      updateTimestamp(record)
      mesg.setFields(record)
    }

    def updateTimestamp(mesg: Mesg): Unit =
      Option(mesg.getField("timestamp")).foreach { f =>
        val t = f.getLongValue + lastSessionTimestamp
        mesg.setFieldValue("timestamp", t)
      }
  }

}
