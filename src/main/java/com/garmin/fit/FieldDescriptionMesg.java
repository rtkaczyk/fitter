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


public class FieldDescriptionMesg extends Mesg {

   
   public static final int DeveloperDataIndexFieldNum = 0;
   
   public static final int FieldDefinitionNumberFieldNum = 1;
   
   public static final int FitBaseTypeIdFieldNum = 2;
   
   public static final int FieldNameFieldNum = 3;
   
   public static final int ArrayFieldNum = 4;
   
   public static final int ComponentsFieldNum = 5;
   
   public static final int ScaleFieldNum = 6;
   
   public static final int OffsetFieldNum = 7;
   
   public static final int UnitsFieldNum = 8;
   
   public static final int BitsFieldNum = 9;
   
   public static final int AccumulateFieldNum = 10;
   
   public static final int FitBaseUnitIdFieldNum = 13;
   
   public static final int NativeMesgNumFieldNum = 14;
   
   public static final int NativeFieldNumFieldNum = 15;
   

   protected static final  Mesg fieldDescriptionMesg;
   static {
      // field_description
      fieldDescriptionMesg = new Mesg("field_description", MesgNum.FIELD_DESCRIPTION);
      fieldDescriptionMesg.addField(new Field("developer_data_index", DeveloperDataIndexFieldNum, 2, 1, 0, "", false, Profile.Type.UINT8));
      
      fieldDescriptionMesg.addField(new Field("field_definition_number", FieldDefinitionNumberFieldNum, 2, 1, 0, "", false, Profile.Type.UINT8));
      
      fieldDescriptionMesg.addField(new Field("fit_base_type_id", FitBaseTypeIdFieldNum, 2, 1, 0, "", false, Profile.Type.FIT_BASE_TYPE));
      
      fieldDescriptionMesg.addField(new Field("field_name", FieldNameFieldNum, 7, 1, 0, "", false, Profile.Type.STRING));
      
      fieldDescriptionMesg.addField(new Field("array", ArrayFieldNum, 2, 1, 0, "", false, Profile.Type.UINT8));
      
      fieldDescriptionMesg.addField(new Field("components", ComponentsFieldNum, 7, 1, 0, "", false, Profile.Type.STRING));
      
      fieldDescriptionMesg.addField(new Field("scale", ScaleFieldNum, 2, 1, 0, "", false, Profile.Type.UINT8));
      
      fieldDescriptionMesg.addField(new Field("offset", OffsetFieldNum, 1, 1, 0, "", false, Profile.Type.SINT8));
      
      fieldDescriptionMesg.addField(new Field("units", UnitsFieldNum, 7, 1, 0, "", false, Profile.Type.STRING));
      
      fieldDescriptionMesg.addField(new Field("bits", BitsFieldNum, 7, 1, 0, "", false, Profile.Type.STRING));
      
      fieldDescriptionMesg.addField(new Field("accumulate", AccumulateFieldNum, 7, 1, 0, "", false, Profile.Type.STRING));
      
      fieldDescriptionMesg.addField(new Field("fit_base_unit_id", FitBaseUnitIdFieldNum, 132, 1, 0, "", false, Profile.Type.FIT_BASE_UNIT));
      
      fieldDescriptionMesg.addField(new Field("native_mesg_num", NativeMesgNumFieldNum, 132, 1, 0, "", false, Profile.Type.MESG_NUM));
      
      fieldDescriptionMesg.addField(new Field("native_field_num", NativeFieldNumFieldNum, 2, 1, 0, "", false, Profile.Type.UINT8));
      
   }

   public FieldDescriptionMesg() {
      super(Factory.createMesg(MesgNum.FIELD_DESCRIPTION));
   }

   public FieldDescriptionMesg(final Mesg mesg) {
      super(mesg);
   }


   /**
    * Get developer_data_index field
    *
    * @return developer_data_index
    */
   public Short getDeveloperDataIndex() {
      return getFieldShortValue(0, 0, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Set developer_data_index field
    *
    * @param developerDataIndex
    */
   public void setDeveloperDataIndex(Short developerDataIndex) {
      setFieldValue(0, 0, developerDataIndex, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Get field_definition_number field
    *
    * @return field_definition_number
    */
   public Short getFieldDefinitionNumber() {
      return getFieldShortValue(1, 0, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Set field_definition_number field
    *
    * @param fieldDefinitionNumber
    */
   public void setFieldDefinitionNumber(Short fieldDefinitionNumber) {
      setFieldValue(1, 0, fieldDefinitionNumber, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Get fit_base_type_id field
    *
    * @return fit_base_type_id
    */
   public Short getFitBaseTypeId() {
      return getFieldShortValue(2, 0, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Set fit_base_type_id field
    *
    * @param fitBaseTypeId
    */
   public void setFitBaseTypeId(Short fitBaseTypeId) {
      setFieldValue(2, 0, fitBaseTypeId, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   public String[] getFieldName() {
      
      return getFieldStringValues(3, Fit.SUBFIELD_INDEX_MAIN_FIELD);
      
   }

   /**
    * @return number of field_name
    */
   public int getNumFieldName() {
      return getNumFieldValues(3, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Get field_name field
    *
    * @param index of field_name
    * @return field_name
    */
   public String getFieldName(int index) {
      return getFieldStringValue(3, index, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Set field_name field
    *
    * @param index of field_name
    * @param fieldName
    */
   public void setFieldName(int index, String fieldName) {
      setFieldValue(3, index, fieldName, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Get array field
    *
    * @return array
    */
   public Short getArray() {
      return getFieldShortValue(4, 0, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Set array field
    *
    * @param array
    */
   public void setArray(Short array) {
      setFieldValue(4, 0, array, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Get components field
    *
    * @return components
    */
   public String getComponents() {
      return getFieldStringValue(5, 0, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Set components field
    *
    * @param components
    */
   public void setComponents(String components) {
      setFieldValue(5, 0, components, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Get scale field
    *
    * @return scale
    */
   public Short getScale() {
      return getFieldShortValue(6, 0, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Set scale field
    *
    * @param scale
    */
   public void setScale(Short scale) {
      setFieldValue(6, 0, scale, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Get offset field
    *
    * @return offset
    */
   public Byte getOffset() {
      return getFieldByteValue(7, 0, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Set offset field
    *
    * @param offset
    */
   public void setOffset(Byte offset) {
      setFieldValue(7, 0, offset, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   public String[] getUnits() {
      
      return getFieldStringValues(8, Fit.SUBFIELD_INDEX_MAIN_FIELD);
      
   }

   /**
    * @return number of units
    */
   public int getNumUnits() {
      return getNumFieldValues(8, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Get units field
    *
    * @param index of units
    * @return units
    */
   public String getUnits(int index) {
      return getFieldStringValue(8, index, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Set units field
    *
    * @param index of units
    * @param units
    */
   public void setUnits(int index, String units) {
      setFieldValue(8, index, units, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Get bits field
    *
    * @return bits
    */
   public String getBits() {
      return getFieldStringValue(9, 0, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Set bits field
    *
    * @param bits
    */
   public void setBits(String bits) {
      setFieldValue(9, 0, bits, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Get accumulate field
    *
    * @return accumulate
    */
   public String getAccumulate() {
      return getFieldStringValue(10, 0, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Set accumulate field
    *
    * @param accumulate
    */
   public void setAccumulate(String accumulate) {
      setFieldValue(10, 0, accumulate, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Get fit_base_unit_id field
    *
    * @return fit_base_unit_id
    */
   public Integer getFitBaseUnitId() {
      return getFieldIntegerValue(13, 0, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Set fit_base_unit_id field
    *
    * @param fitBaseUnitId
    */
   public void setFitBaseUnitId(Integer fitBaseUnitId) {
      setFieldValue(13, 0, fitBaseUnitId, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Get native_mesg_num field
    *
    * @return native_mesg_num
    */
   public Integer getNativeMesgNum() {
      return getFieldIntegerValue(14, 0, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Set native_mesg_num field
    *
    * @param nativeMesgNum
    */
   public void setNativeMesgNum(Integer nativeMesgNum) {
      setFieldValue(14, 0, nativeMesgNum, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Get native_field_num field
    *
    * @return native_field_num
    */
   public Short getNativeFieldNum() {
      return getFieldShortValue(15, 0, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

   /**
    * Set native_field_num field
    *
    * @param nativeFieldNum
    */
   public void setNativeFieldNum(Short nativeFieldNum) {
      setFieldValue(15, 0, nativeFieldNum, Fit.SUBFIELD_INDEX_MAIN_FIELD);
   }

}
