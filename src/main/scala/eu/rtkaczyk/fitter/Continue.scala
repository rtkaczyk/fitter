package eu.rtkaczyk.fitter

import java.io.{File => JFile, FileInputStream}

import com.garmin.fit._

class Continue extends Mode {

  var lastActivity: ActivityMesg = _
  var lastSession: SessionMesg = _

  def apply(args: List[String]): Unit = {

    val initial :: continued = args

    val initializer = new Initializer()
    processInput(initial, initializer)

    continued.foreach { in =>
      val encoder = new FileEncoder(new JFile(outputFilename(in)), Fit.ProtocolVersion.V1_0)
      processInput(in, new Continuator(encoder))
      encoder.close()
    }
  }

  def outputFilename(in: String): String = {
    val x = if (in.endsWith(".fit")) 4 else 0
    in.take(in.length - x) + ".continued.fit"
  }

  def processInput(in: String, listener: MesgListener with MesgDefinitionListener): Unit = {
    val decoder = new Decode
    val input = new FileInputStream(in)

    decoder.read(input, listener, listener)

    input.close()
  }

  class Initializer extends MesgListener with MesgDefinitionListener {

    def onMesg(mesg: Mesg): Unit = mesg.getNum match {
      case MesgNum.ACTIVITY =>
        lastActivity = new ActivityMesg(mesg)

      case MesgNum.SESSION =>
        lastSession = new SessionMesg(mesg)

      case _ =>
    }

    def onMesgDefinition(mesg: MesgDefinition): Unit = ()
  }

  class Continuator(encoder: FileEncoder) extends MesgListener with MesgDefinitionListener {

    def onMesg(mesg: Mesg): Unit = mesg.getNum match {
      case MesgNum.ACTIVITY =>
        mergeActivity(new ActivityMesg(mesg))
        //encoder.write(lastActivity)

      case MesgNum.SESSION =>
        mergeSession(new SessionMesg(mesg))
        //encoder.write(lastSession)

      case MesgNum.EVENT =>
        //updateTimestamp(mesg)
        //encoder.write(mesg)

      case MesgNum.LAP =>

      case MesgNum.RECORD =>
        updateRecord(mesg)
        encoder.write(mesg)

      case 209 | 104 =>
        val t = mesg.getField(253).getIntegerValue + lastSession.getTimestamp.getTimestamp.toInt
        mesg.setFieldValue(253, t)
        encoder.write(mesg)

      case 22 | 210 | 219 =>

      case _ =>
        updateTimestamp(mesg)
        encoder.write(mesg)
    }

    def onMesgDefinition(mesg: MesgDefinition): Unit = mesg.getNum match {
      case MesgNum.LAP | MesgNum.ACTIVITY | MesgNum.SESSION =>

      case _ =>
        encoder.write(mesg)
    }


    def mergeActivity(curr: ActivityMesg): Unit = {
      curr.setTotalTimerTime(lastActivity.getTotalTimerTime + curr.getTotalTimerTime)
      updateTimestamp(curr)

      lastActivity = curr
    }


    def mergeSession(curr: SessionMesg): Unit = {

      curr.setTotalTimerTime(lastSession.getTotalTimerTime + curr.getTotalTimerTime)
      curr.setTotalElapsedTime(lastSession.getTotalElapsedTime + curr.getTotalElapsedTime)
      curr.setStartTime(new DateTime(curr.getStartTime.getTimestamp + lastSession.getTimestamp.getTimestamp))
      curr.setTotalDistance(curr.getTotalDistance + lastSession.getTotalDistance)
      updateTimestamp(curr)

      lastSession = curr
    }

    def updateRecord(mesg: Mesg): Unit = {
      Option(mesg.getField("distance")).map { f =>
        val t = f.getValue.asInstanceOf[Double]
        f.setValue(t + Option(lastSession).map(_.getTotalDistance.toDouble).getOrElse(0.0))
        f
      }.foreach(mesg.setField)

      updateTimestamp(mesg)
    }

    def updateTimestamp(mesg: Mesg): Unit =
      Option(mesg.getField("timestamp")).map { f =>
        val t = f.getValue.asInstanceOf[Long]
        f.setValue(t + Option(lastActivity).map(_.getTimestamp.getTimestamp.toLong).getOrElse(0L))
        f
      }.foreach(mesg.setField)

  }
}
