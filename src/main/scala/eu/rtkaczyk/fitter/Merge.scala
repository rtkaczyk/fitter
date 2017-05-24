package eu.rtkaczyk.fitter

import java.io.FileInputStream

import com.garmin.fit._

object Merge extends Mode {
  def apply(args: List[String]): Unit = {

    val inputs = args.init
    val output = args.last

    val encoder = new FileEncoder(new java.io.File(output), Fit.ProtocolVersion.V1_0)

    for ((input, i) <- inputs.zipWithIndex) {
      val listener = new Listener(encoder, i == 0, i == inputs.size)
      val decoder = new Decode
      val in = new FileInputStream(input)

      decoder.read(in, listener, listener)
      in.close()
    }
  }

  class Listener(encoder: FileEncoder, first: Boolean, last: Boolean) extends MesgListener with MesgDefinitionListener {
    def onMesg(mesg: Mesg): Unit = mesg match {
      case _: FileIdMesg =>
        if (first)
          encoder.write(mesg)

      case _ =>
        encoder.write(mesg)
    }

    def onMesgDefinition(mesg: MesgDefinition): Unit = mesg.getNum match {
      case MesgNum.FILE_ID  =>
        if (first)
          encoder.write(mesg)

      case _ =>
        encoder.write(mesg)
    }
  }
}
