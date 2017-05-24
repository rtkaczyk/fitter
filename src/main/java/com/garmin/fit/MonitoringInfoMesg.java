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


public class MonitoringInfoMesg extends Mesg {

   
   public static final int TimestampFieldNum = 253;
   
   public static final int LocalTimestampFieldNum = 0;
   
   public static final int ActivityTypeFieldNum = 1;
   
   public static final int CyclesToDistanceFieldNum = 3;
   
   public static final int CyclesToCaloriesFieldNum = 4;
   
   public static final int RestingMetabolicRateFieldNum = 5;
   

   protected static final  Mesg monitoringInfoMesg;
   static {
      // monitoring_info
      monitoringInfoMesg = new Mesg("monitoring_info", MesgNum.MONITORING_INFO);
      monitoringInfoMesg.addField(new Field("timestamp", TimestampFieldNum, 134, 1, 0, "s", false, Profile.Type.DATE_TIME));
      
      monitoringInfoMesg.addField(new Field("local_timestamp", LocalTimestampFieldNum, 134, 1, 0, "s", false, Profile.Type.LOCAL_DATE_TIME));
      
      monitoringInfoMesg.addField(new Field("activity_type", ActivityTypeFieldNum, 0, 1, 0, "", false, Profile.Type.ACTIVITY_TYPE));
      
      monitoringInfoMesg.addField(new Field("cycles_to_distance", CyclesToDistanceFieldNum, 132, 5000, 0, "m/cycle", false, Profile.Type.UINT16));
      
      monitoringInfoMesg.addField(new Field("cycles_to_calories", CyclesToCaloriesFieldNum, 132, 5000, 0, "kcal/cycle", false, Profile.Type.UINT16));
      
      monitoringInfoMesg.addField(new Field("resting_metabolic_rate", RestingMetabolicRateFieldNum, 132, 1, 0, "kcal / day", false, Profile.Type.UINT16));
      
   }

   public MonitoringInfoMesg() {
      super(Factory.createMesg(MesgNum.MONITORING_INFO));
   }

   public MonitoringInfoMesg(final Mesg mesg) {
      super(mesg);
   }


   /**
    * Get timestamp field
    * Units: s
    *
    * @return timestamp
    */
   public DateTime getTimestamp() {
      return timestampToDateTime(getFieldLongValue(253, 0, Fit.SUBFIELD_INDEX_MAIN_FIELD));
   }

   /**
    * Set timestamp field
    * Units: s
    *
    * @param timestamp
    */
   public void setTimestamp(DateTime timestamp) {
      setFieldValue(253, 0, timestamp.getTimestamp(), Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Get local_timestamp field
    * Units: s
    * Comment: Use to convert activity timestamps to local time if device does not support time zone and daylight savings time correction.
    *
    * @return local_timestamp
    */
   public Long getLocalTimestamp() {
      return getFieldLongValue(0, 0, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Set local_timestamp field
    * Units: s
    * Comment: Use to convert activity timestamps to local time if device does not support time zone and daylight savings time correction.
    *
    * @param localTimestamp
    */
   public void setLocalTimestamp(Long localTimestamp) {
      setFieldValue(0, 0, localTimestamp, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   public ActivityType[] getActivityType() {
      
      Short[] values = getFieldShortValues(1, Fit.SUBFIELD_INDEX_MAIN_FIELD);
      ActivityType[] rv = new ActivityType[values.length];
      for(int i = 0; i < values.length; i++){
         rv[i] = ActivityType.getByValue(values[i]);
      }
      return rv;
      
   }

   /**
    * @return number of activity_type
    */
   public int getNumActivityType() {
      return getNumFieldValues(1, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Get activity_type field
    *
    * @param index of activity_type
    * @return activity_type
    */
   public ActivityType getActivityType(int index) {
      Short value = getFieldShortValue(1, index, Fit.SUBFIELD_INDEX_MAIN_FIELD);
      if (value == null)
         return null;
      return ActivityType.getByValue(value);
   }

   /**
    * Set activity_type field
    *
    * @param index of activity_type
    * @param activityType
    */
   public void setActivityType(int index, ActivityType activityType) {
      setFieldValue(1, index, activityType.value, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   public Float[] getCyclesToDistance() {
      
      return getFieldFloatValues(3, Fit.SUBFIELD_INDEX_MAIN_FIELD);
      
   }

   /**
    * @return number of cycles_to_distance
    */
   public int getNumCyclesToDistance() {
      return getNumFieldValues(3, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Get cycles_to_distance field
    * Units: m/cycle
    * Comment: Indexed by activity_type
    *
    * @param index of cycles_to_distance
    * @return cycles_to_distance
    */
   public Float getCyclesToDistance(int index) {
      return getFieldFloatValue(3, index, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Set cycles_to_distance field
    * Units: m/cycle
    * Comment: Indexed by activity_type
    *
    * @param index of cycles_to_distance
    * @param cyclesToDistance
    */
   public void setCyclesToDistance(int index, Float cyclesToDistance) {
      setFieldValue(3, index, cyclesToDistance, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   public Float[] getCyclesToCalories() {
      
      return getFieldFloatValues(4, Fit.SUBFIELD_INDEX_MAIN_FIELD);
      
   }

   /**
    * @return number of cycles_to_calories
    */
   public int getNumCyclesToCalories() {
      return getNumFieldValues(4, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Get cycles_to_calories field
    * Units: kcal/cycle
    * Comment: Indexed by activity_type
    *
    * @param index of cycles_to_calories
    * @return cycles_to_calories
    */
   public Float getCyclesToCalories(int index) {
      return getFieldFloatValue(4, index, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Set cycles_to_calories field
    * Units: kcal/cycle
    * Comment: Indexed by activity_type
    *
    * @param index of cycles_to_calories
    * @param cyclesToCalories
    */
   public void setCyclesToCalories(int index, Float cyclesToCalories) {
      setFieldValue(4, index, cyclesToCalories, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Get resting_metabolic_rate field
    * Units: kcal / day
    *
    * @return resting_metabolic_rate
    */
   public Integer getRestingMetabolicRate() {
      return getFieldIntegerValue(5, 0, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Set resting_metabolic_rate field
    * Units: kcal / day
    *
    * @param restingMetabolicRate
    */
   public void setRestingMetabolicRate(Integer restingMetabolicRate) {
      setFieldValue(5, 0, restingMetabolicRate, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

}
