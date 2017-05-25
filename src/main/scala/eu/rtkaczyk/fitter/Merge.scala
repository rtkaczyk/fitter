package eu.rtkaczyk.fitter

import java.io.FileInputStream

import com.garmin.fit._

object Merge extends Mode {
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

    var lastActivity: ActivityMesg = null
    var lastSession: SessionMesg = null
    var ignoreEvent: Boolean = false


    var i = 0

    def nextFile(): Unit =
      i += 1

    def first: Boolean = i == 0
    def last: Boolean = i == noFiles - 1


    def onMesg(mesg: Mesg): Unit = mesg.getNum match {
      case MesgNum.FILE_ID | MesgNum.FILE_CREATOR =>
        if (first)
          encoder.write(mesg)

      case MesgNum.ACTIVITY =>
        mergeActivity(new ActivityMesg(mesg))
        if (last)
          encoder.write(lastActivity)

      case MesgNum.SESSION =>
        mergeSession(new SessionMesg(mesg))
        ignoreEvent = true
        if (last)
          encoder.write(lastSession)


      case MesgNum.LAP =>
        //skip laps

      case MesgNum.EVENT =>
        if (ignoreEvent)
          ignoreEvent = false
        else
          encoder.write(mesg)


      case MesgNum.RECORD =>
        updateRecord(new RecordMesg(mesg))
        //encoder.write(mesg)

      case _ =>
        updateTimestamp(mesg)
        encoder.write(mesg)
    }

    def onMesgDefinition(mesg: MesgDefinition): Unit = mesg.getNum match {
      case MesgNum.FILE_ID | MesgNum.FILE_CREATOR =>
        if (first)
          encoder.write(mesg)

      case MesgNum.ACTIVITY =>
        if (last)
          encoder.write(mesg)

      case MesgNum.SESSION =>
        if (last)
          encoder.write(mesg)

      case MesgNum.LAP =>
        //skip laps

      case MesgNum.EVENT =>
        if (ignoreEvent)
          ignoreEvent = false
        else
          encoder.write(mesg)

      case _ =>
        encoder.write(mesg)
    }

    def mergeActivity(curr: ActivityMesg): Unit = {
      if (lastActivity != null) {
        curr.setTotalTimerTime(lastActivity.getTotalTimerTime + curr.getTotalTimerTime)
        curr.setNumSessions(1)
        updateTimestamp(curr)
      }

      lastActivity = curr
    }


    def mergeSession(curr: SessionMesg): Unit = {
      if (lastSession != null) {
        curr.setTotalTimerTime(lastSession.getTotalTimerTime + curr.getTotalTimerTime)
        curr.setTotalElapsedTime(lastSession.getTotalElapsedTime + curr.getTotalElapsedTime)
        curr.setStartPositionLat(lastSession.getStartPositionLat)
        curr.setStartPositionLong(lastSession.getStartPositionLong)
        curr.setStartTime(lastSession.getStartTime)
        curr.setTotalDistance(curr.getTotalDistance + lastSession.getTotalDistance)
        updateTimestamp(curr)
      }

      lastSession = curr
    }

    def updateRecord(record: RecordMesg): Unit = {
      record.setDistance(record.getDistance + Option(lastSession).map(_.getTotalDistance.toFloat).getOrElse(0.0f))
      updateTimestamp(record)
      encoder.write(record)
    }

    def updateTimestamp(mesg: Mesg): Unit =
      Option(mesg.getField("timestamp")).map { f =>
        val t = f.getValue.asInstanceOf[Long]
        f.setValue(t + Option(lastActivity).map(_.getTimestamp.getTimestamp.toLong).getOrElse(0L))
        f
      }.foreach(mesg.setField)
  }

}
