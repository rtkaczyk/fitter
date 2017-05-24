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


package com.garmin.fit.csv;

import com.garmin.fit.*;
import com.garmin.fit.csv.MesgCSVWriter;
import com.garmin.fit.csv.MesgDataCSVWriter;
import com.garmin.fit.csv.MesgFilter;
import com.garmin.fit.test.Tests;

import java.io.FileInputStream;
import java.io.InputStream;
import java.io.File;
import java.util.Arrays;
import java.util.ArrayList;

public class CSVTool {

   private final int DATA_OR_DEFINITION_SEARCH_COUNT = 2;
   private String in = "";
   private String out = "";
   private ArrayList<String> mesgDefinitionsToOutput = new ArrayList<String>();
   private ArrayList<String> dataMessagesToOutput = new ArrayList<String>();
   private MesgDataCSVWriter dataMesgWriter = null;
   private MesgCSVWriter mesgWriter;
   private MesgFilter mesgFilter;
   private boolean printBytesAsHex = false;
   private boolean fitToCsv = false;
   private boolean csvToFit = false;
   private boolean test = false;
   private boolean checkIntegrity = false;
   private boolean showInvalidValues = false;
   private boolean invalidsToEmpty = false;
   private boolean hideUnknownData = false;
   private boolean generateDataFile = false;
   private int nextArgumentDefinition = 0;
   private int nextArgumentData = 0;
   private int numUnknownFields = 0;
   private int numUnknownMesgs = 0;
   private Decode decode = new Decode();
   private Fit.ProtocolVersion protocolVersion = Fit.ProtocolVersion.V1_0;

   private CSVTool() {
   }

   private void run(String args[]) {

      int arg = 0;

      System.out.printf("FIT CSV Tool - Protocol %d.%d Profile %.2f %s\n", Fit.PROTOCOL_VERSION_MAJOR, Fit.PROTOCOL_VERSION_MINOR, Fit.PROFILE_VERSION / 100.0, Fit.PROFILE_TYPE);

      while (arg < args.length) {
         if (args[arg].equals("-b")) {
            if ((args.length - arg) < 3) {
               printUsage();
               return;
            }

            fitToCsv = true;
            in = args[arg + 1];
            out = args[arg + 2];

            arg += 2;
         } else if (args[arg].equals("-c")) {
            if ((args.length - arg) < 3) {
               printUsage();
               return;
            }

            csvToFit = true;
            in = args[arg + 1];
            out = args[arg + 2];

            arg += 2;
         } else if (args[arg].equals("-t")) {
            test = true;
         } else if (args[arg].equals("-d")) {
            Fit.debug = true;
            test = true;
         } else if (args[arg].equals("-i")) {
            checkIntegrity = true;
         } else if (args[arg].equals("--defn")) {
            nextArgumentDefinition = DATA_OR_DEFINITION_SEARCH_COUNT;
         } else if (args[arg].equals("--data")) {
            nextArgumentData = DATA_OR_DEFINITION_SEARCH_COUNT;
            generateDataFile = true;
         } else if (args[arg].charAt(0) != '-') {

            if(nextArgumentDefinition > 0) {
               mesgDefinitionsToOutput = new ArrayList<String>(Arrays.asList(args[arg].toLowerCase().split(",")));
            }
            else if(nextArgumentData > 0) {
               dataMessagesToOutput = new ArrayList<String>(Arrays.asList(args[arg].toLowerCase().split(",")));
            }
            else {
               in = args[arg];
               if (in.endsWith(".fit")) {
                   fitToCsv = true;
                   out = in.substring(0, in.length()-4) + ".csv";
               } else if (in.endsWith(".csv")) {
                   csvToFit = true;
                   out = in.substring(0, in.length()-4) + ".fit";
               }
            }
         } else if (args[arg].equals("-s")) {
            showInvalidValues = true;
         } else if (args[arg].equals("-se")) {
            showInvalidValues = true;
            invalidsToEmpty = true;
         } else if (args[arg].equals("-u")) {
            hideUnknownData = true;
         } else if (args[arg].equals("-x")) {
            printBytesAsHex = true;
         } else if(args[arg].startsWith("-p")) {
            if(args[arg].endsWith("1")) {
               protocolVersion = Fit.ProtocolVersion.V1_0;
            } else if(args[arg].endsWith("2")) {
               protocolVersion = Fit.ProtocolVersion.V2_0;
            } else {
               System.out.println("Unknown Protocol Version.");
            }
         }

         if(nextArgumentDefinition > 0) {
            nextArgumentDefinition--;
            if((nextArgumentDefinition == 0) && (mesgDefinitionsToOutput.isEmpty()))
            {
               System.out.println("No mesg definitions defined for --defn option.  Use 'none' if no definitions are desired.");
               return;
            }
         }
         if(nextArgumentData > 0) {
            nextArgumentData--;
            if((nextArgumentData == 0) && (dataMessagesToOutput.isEmpty()))
            {
               System.out.println("No data messages defined for --data option.");
               return;
            }
         }
         arg++;
      }

      if (fitToCsv) {
         if ((out.length() >= 4) && (out.substring(out.length()-4, out.length()).compareTo(".csv") == 0))
            out = out.substring(0, out.length()-4); // Remove .csv extension.

         if (checkIntegrity) {
            try {
               if (!decode.checkFileIntegrity((InputStream) new FileInputStream(in))) {
                  if (!decode.getInvalidFileDataSize())
                     throw new RuntimeException("FIT file integrity failure.");
                  else {
                     System.out.println("FIT file integrity failure. Invalid file size in header.");
                     System.out.println("Trying to continue...");
                  }
               }
            } catch (java.io.IOException e) {
               throw new RuntimeException(e);
            }
         }

         if (test) {
            Tests tests = new Tests();
            System.out.println("Running FIT verification tests...");
            if (tests.run(in))
               System.out.println("Passed FIT verification.");
            else
               System.out.println("Failed FIT verification.");
         }

         try {
            //CSV Writer writes all messages to the csv file
            setupCsvWriter();
            if( generateDataFile ) {
                //Data Writer writes the data filtered messages
                setupDataWriter();
            }
            FileInputStream fileInputStream = new FileInputStream(in);
            if (showInvalidValues == true)
               decode.showInvalidValues();

            mesgFilter = new MesgFilter();
            mesgFilter.setMesgDefinitionsToOutput(mesgDefinitionsToOutput);
            mesgFilter.setDataMessagesToOutput(dataMessagesToOutput);

            registerListenersForCsvWriter();
            registerListenersForDataWriter();

            while (decode.bytesAvailable((InputStream) fileInputStream)) { // Try to read a file while more data is available.
               try {
                  decode.read((InputStream) fileInputStream);
                  decode.nextFile(); // Initialize to read next file (if any).
               } catch (FitRuntimeException e) {
                  if (decode.getInvalidFileDataSize()) {
                     // The exception might be due to a bad file size written
                     // by a device. Retry the decoding process.
                     decode.nextFile();
                     continue;
                  }
                  else {
                      // An actual exception has occurred.
                      throw e;
                  }
               }
            }

            cleanupCsvWriter();
            cleanupDataWriter();

            if ( !mesgWriter.csvHasData() )
               System.out.println("Warning: No CSV has been written as this file does not contain FIT message data");

            numUnknownFields = mesgWriter.getNumUnknownFields();
            numUnknownMesgs = mesgWriter.getNumUnknownMesgs();

         } catch (Exception e) {
            throw new RuntimeException(e);
         }

         if ( hideUnknownData )
            System.out.printf("Hid %d unknown field(s) and %d unknown message(s).\n", numUnknownFields, numUnknownMesgs);
         System.out.printf("FIT binary file %s decoded to %s*.csv files.\n", in, out);
      } else if (csvToFit) {
         try {
            FileEncoder encoder = new FileEncoder(new File(out), protocolVersion);
            if (!CSVReader.read((InputStream) new FileInputStream(in), encoder, encoder))
               throw new RuntimeException("FIT encoding error.");
            encoder.close();

            System.out.printf("%s encoded into FIT binary file %s.\n", in, out);
         } catch (java.io.IOException e) {
            throw new RuntimeException(e);
         }
      } else {
         printUsage();
      }
   }

   private void setupCsvWriter( ) {
       mesgWriter = new MesgCSVWriter(out + ".csv");
        if (invalidsToEmpty) {
           mesgWriter.showInvalidsAsEmptyCells();
        }
        if (hideUnknownData) {
           mesgWriter.hideUnknownData();
        }

        mesgWriter.setPrintByteAsHex(printBytesAsHex);
   }

   private void registerListenersForCsvWriter() {
       mesgFilter.addListener((MesgDefinitionListener) mesgWriter);
       mesgFilter.addListener((MesgListener) mesgWriter);

       decode.addListener((MesgDefinitionListener) mesgFilter);
       decode.addListener((MesgListener) mesgFilter);
   }

   private void cleanupCsvWriter() {
       mesgWriter.close();
   }

   private void setupDataWriter( ) {
       dataMesgWriter = new MesgDataCSVWriter(out + "_data.csv");
       if (invalidsToEmpty) {
          dataMesgWriter.showInvalidsAsEmptyCells();
       }
       if (hideUnknownData) {
           dataMesgWriter.hideUnknownData();
       }
   }

  private void registerListenersForDataWriter() {
       if( dataMesgWriter != null ) {
           mesgFilter.addListener((MesgListener) dataMesgWriter);
       }
   }

   private void cleanupDataWriter() {
       if( dataMesgWriter != null ) {
           dataMesgWriter.close();
       }
   }

   public static void main(String args[]) {
       CSVTool tool = new CSVTool();
       tool.run(args);
   }

   private static void printUsage() {
      System.out.println("Usage: java -jar FitCSVTool.jar <options> <file>");
      System.out.println("      -b <FIT FILE> <CSV FILE>  FIT binary to CSV.");
      System.out.println("      -c <CSV FILE> <FIT FILE>  CSV to FIT binary.");
      System.out.println("      -t Enable file verification tests.");
      System.out.println("      -d Enable debug output.");
      System.out.println("      -i Check integrity of FIT file before decoding.");
      System.out.println("      -s Show invalid fields in the CSV file.");
      System.out.println("      -se Show invalid fields in the CSV file as empty cells.");
      System.out.println("      -u Hide unknown data and report statistics on how much is hidden.");
      System.out.println("      -x Print byte values as hexadecimal.");
      System.out.println("      -pN Encode file using Protocol Version <N>. Default: 1");
      System.out.println("      --defn <MESSAGE_STRING_0,MESSAGE_STRING_1,...> Narrows down the");
      System.out.println("          definitions output to CSV. Use 'none' for no definitions");
      System.out.println("          When this option is used only the message definitions");
      System.out.println("          in the comma separated list will be written to the CSV.");
      System.out.println("          eg. --defn file_capabilities,record,file_creator");
      System.out.println("          Note: This option is only compatible with the -b option.");
      System.out.println("      --data <MESSAGE_STRING_0,MESSAGE_STRING_1,...> Narrows down the");
      System.out.println("          data output to CSV. When this option is used only the data");
      System.out.println("          in the comma separated list will be written to the csv.");
      System.out.println("          eg. --data file_capabilities,record,file_creator");
      System.out.println("          Note: This option is only compatible with the -b option.");
      }
}
