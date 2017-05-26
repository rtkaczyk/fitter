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

      case MesgNum.ACTIVITY | MesgNum.LAP =>


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

      case MesgNum.ACTIVITY | MesgNum.SESSION | MesgNum.LAP | MesgNum.EVENT =>

      case _ =>
        encoder.write(mesg)
    }


    def mergeSession(curr: SessionMesg): Unit = {
      curr.setTotalDistance(curr.getTotalDistance + lastSessionDistance)
      updateTimestamp(curr)
      lastSession = curr
    }

    def updateRecord(mesg: Mesg): Unit = {
      val record = new RecordMesg(mesg)
      record.setDistance(record.getDistance + Option(lastSession).map(_.getTotalDistance.toFloat).getOrElse(0.0f))
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
