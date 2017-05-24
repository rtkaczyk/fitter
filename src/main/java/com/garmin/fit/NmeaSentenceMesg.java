////////////////////////////////////////////////////////////////////////////////
// The following FIT Protocol software provided may be used with FIT protocol
// devices only and remains the copyrighted property of Dynastream Innovations Inc.
// The software is being provided on an "as-is" basis and as an accommodation,
// and therefore all warranties, representations, or guarantees of any kind
// (whether express, implied or statutory) including, without limitation,
// warranties of merchantability, non-infringement, or fitness for a particular
// purpose, are specifically disclaimed.
//
// Copyright 2017 Dynastream Innovations Inc.
////////////////////////////////////////////////////////////////////////////////
// ****WARNING****  This file is auto-generated!  Do NOT edit this file.
// Profile Version = 20.33Release
// Tag = production/akw/20.33.01-0-gdd6ece0
////////////////////////////////////////////////////////////////////////////////


package com.garmin.fit;
import java.math.BigInteger;


public class NmeaSentenceMesg extends Mesg {

   
   public static final int TimestampFieldNum = 253;
   
   public static final int TimestampMsFieldNum = 0;
   
   public static final int SentenceFieldNum = 1;
   

   protected static final  Mesg nmeaSentenceMesg;
   static {
      // nmea_sentence
      nmeaSentenceMesg = new Mesg("nmea_sentence", MesgNum.NMEA_SENTENCE);
      nmeaSentenceMesg.addField(new Field("timestamp", TimestampFieldNum, 134, 1, 0, "s", false, Profile.Type.DATE_TIME));
      
      nmeaSentenceMesg.addField(new Field("timestamp_ms", TimestampMsFieldNum, 132, 1, 0, "ms", false, Profile.Type.UINT16));
      
      nmeaSentenceMesg.addField(new Field("sentence", SentenceFieldNum, 7, 1, 0, "", false, Profile.Type.STRING));
      
   }

   public NmeaSentenceMesg() {
      super(Factory.createMesg(MesgNum.NMEA_SENTENCE));
   }

   public NmeaSentenceMesg(final Mesg mesg) {
      super(mesg);
   }


   /**
    * Get timestamp field
    * Units: s
    * Comment: Timestamp message was output
    *
    * @return timestamp
    */
   public DateTime getTimestamp() {
      return timestampToDateTime(getFieldLongValue(253, 0, Fit.SUBFIELD_INDEX_MAIN_FIELD));
   }

   /**
    * Set timestamp field
    * Units: s
    * Comment: Timestamp message was output
    *
    * @param timestamp
    */
   public void setTimestamp(DateTime timestamp) {
      setFieldValue(253, 0, timestamp.getTimestamp(), Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Get timestamp_ms field
    * Units: ms
    * Comment: Fractional part of timestamp, added to timestamp
    *
    * @return timestamp_ms
    */
   public Integer getTimestampMs() {
      return getFieldIntegerValue(0, 0, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Set timestamp_ms field
    * Units: ms
    * Comment: Fractional part of timestamp, added to timestamp
    *
    * @param timestampMs
    */
   public void setTimestampMs(Integer timestampMs) {
      setFieldValue(0, 0, timestampMs, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Get sentence field
    * Comment: NMEA sentence
    *
    * @return sentence
    */
   public String getSentence() {
      return getFieldStringValue(1, 0, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Set sentence field
    * Comment: NMEA sentence
    *
    * @param sentence
    */
   public void setSentence(String sentence) {
      setFieldValue(1, 0, sentence, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

}
