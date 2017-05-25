package eu.rtkaczyk.fitter

import java.io.FileInputStream

import com.garmin.fit._

object Merge extends Mode {
  def apply(args: List[String]): Unit = {

    val inputs = args.init
    val output = args.last

    val encoder = new FileEncoder(new java.io.File(output), Fit.ProtocolVersion.V1_0)

    var lastActivity: ActivityMesg = null

    for ((input, i) <- inputs.zipWithIndex) {
      val listener = new Listener(encoder, lastActivity, i == 0, i == inputs.size - 1)
      val decoder = new Decode
      val in = new FileInputStream(input)

      decoder.read(in, listener, listener)
      in.close()

      lastActivity = listener.lastActivity
    }

    encoder.close()
  }

  class Listener(encoder: FileEncoder, prevActivity: ActivityMesg, first: Boolean, last: Boolean)
    extends MesgListener with MesgDefinitionListener {

    var lastActivity: ActivityMesg = prevActivity

    def onMesg(mesg: Mesg): Unit = mesg.getNum match {
      case MesgNum.FILE_ID | MesgNum.FILE_CREATOR =>
        if (first)
          encoder.write(mesg)

      case MesgNum.ACTIVITY =>
        val activity = new ActivityMesg(mesg)
        lastActivity = joinActivities(lastActivity, activity)
        if (last)
          encoder.write(lastActivity)

        println(s"Last activity timestamp: ${lastActivity.getTimestamp.getTimestamp} " +
          s"(${Option(lastActivity).map(_.getTimestamp.getTimestamp.toLong).getOrElse(0L)})")

      case _ =>
        Option(mesg.getField("timestamp")).map { f =>
          val t = f.getValue.asInstanceOf[Long]
          f.setValue(t + Option(lastActivity).map(_.getTimestamp.getTimestamp.toLong).getOrElse(0L))
          f
        }.foreach(mesg.setField)
        encoder.write(mesg)
    }

    def onMesgDefinition(mesg: MesgDefinition): Unit = mesg.getNum match {
      case MesgNum.FILE_ID | MesgNum.FILE_CREATOR =>
        if (first)
          encoder.write(mesg)

      case MesgNum.ACTIVITY =>
        if (last)
          encoder.write(mesg)

      case _ =>
        encoder.write(mesg)
    }

    def joinActivities(prev: ActivityMesg, curr: ActivityMesg): ActivityMesg = {
      if (prev == null)
        curr
      else {
        val joined = new ActivityMesg()
        joined.setTimestamp {
          val d = prev.getTimestamp
          d.add(curr.getTimestamp)
          d
        }
        joined.setTotalTimerTime(prev.getTotalTimerTime + curr.getTotalTimerTime)
        joined.setNumSessions(prev.getNumSessions + curr.getNumSessions)

        joined
      }
    }

  }

}
