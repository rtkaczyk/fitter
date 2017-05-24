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


package com.garmin.fit.test;

import com.garmin.fit.*;

public class FileIdTest implements Test, MesgListener {
   private String szError = "";
   private boolean bRecvdFirstMesg = false;
   
   public void onMesg(Mesg mesg)
   {
      if (!bRecvdFirstMesg && !mesg.getName().equals("file_id"))                   
        szError = "FileID missing from start of file";      
     bRecvdFirstMesg = true;
   }
   
   public String getError()
   {
      return szError;
   }
}
