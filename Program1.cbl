       IDENTIFICATION DIVISION. 
       PROGRAM-ID.    4240066. 
       AUTHOR.        LUCAS HAHN.
                          
       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
      *SELECT FILES TO READ FROM & AND WRITE TO
           SELECT F01-CAR-RECORDS   ASSIGN TO 'ASST2.DAT'
                                    ORGANIZATION IS LINE SEQUENTIAL.
          
           SELECT F02-PRINT-FILE    ASSIGN TO 'ASST2.OUT'
                                    ORGANIZATION IS LINE SEQUENTIAL.
 
       DATA DIVISION. 
       FILE SECTION. 
      *NAME THE VARIABLES TO HOLD DATA FROM INPUT FILE
       FD  F01-CAR-RECORDS
           RECORD CONTAINS 50 CHARACTERS.
       01  F01-CAR-SALES.
           05  F01-INVOICE-NUM  PIC 9(5).
           05  F01-YEAR         PIC 9(2).
           05  F01-MAKE         PIC X(11).
           05  F01-MODEL        PIC X(13).
           05  F01-ASKING-PRICE PIC 9(6).
           05  F01-SOLD-PRICE   PIC 9(6).
           05  F01-SALES-P      PIC X(7).
      *USED TO PRINT TO THE OUTPUT FILE 
       FD F02-PRINT-FILE 
           RECORD CONTAINS 108 CHARACTERS. 
       01  F02-PRINT-LINE               PIC X(108). 
      *CREATES THE OUTPUT FILE LAYOUT & HOLDS WORKING STORAGE DATA
       WORKING-STORAGE SECTION. 
        
       
       01  W01-HEADING-LINE.
           05                PIC X(46) VALUE SPACES.
           05                PIC X(23) VALUE 'VERY VERY NICE CARS INC'. 
           05                PIC X(39) VALUE SPACES. 
       
       01  W02-HEADING-LINE. 
           05                PIC X(48) VALUE SPACES.
           05                PIC X(17) VALUE 'COMMISSION REPORT'.
           05                PIC X(43) VALUE SPACES.
 

       01  W03-HEADING-LINE.
           05                PIC X(28) VALUE SPACES.
           05                PIC X(3)  VALUE 'CAR'.
           05                PIC X(6)  VALUE SPACES.
           05                PIC X(3)  VALUE 'CAR'.
           05                PIC X(10) VALUE SPACES.
           05                PIC X(3)  VALUE 'CAR'.
           05                PIC X(9)  VALUE SPACES.
           05                PIC X(6)  VALUE 'ASKING'.
           05                PIC X(5)  VALUE SPACES.
           05                PIC X(5)  VALUE 'PRICE'.
           05                PIC X(4)  VALUE SPACES.
           05                PIC X(4)  VALUE '% OF'.
           05                PIC X(6)  VALUE SPACES.
           05                PIC X(4)  VALUE 'COMM'.
           05                PIC X(6)  VALUE SPACES.
           05                PIC X(6)  VALUE 'NET TO'.
           
       01  W04-HEADING-LINE.
           05                PIC X(3)  VALUE SPACES.
           05                PIC X(9)  VALUE 'INVOICE #'.
           05                PIC X(2)  VALUE SPACES.
           05                PIC X(11) VALUE 'SALESPERSON'.
           05                PIC X(2)  VALUE SPACES.
           05                PIC X(4)  VALUE 'YEAR'.
           05                PIC X(5)  VALUE SPACES.
           05                PIC X(5)  VALUE 'MAKER'.
           05                PIC X(8)  VALUE SPACES.
           05                PIC X(5)  VALUE 'MODEL'.
           05                PIC X(8)  VALUE SPACES.
           05                PIC X(5)  VALUE 'PRICE'.
           05                PIC X(6)  VALUE SPACES.
           05                PIC X(4)  VALUE 'SOLD'.
           05                PIC X(4)  VALUE SPACES.
           05                PIC X(6)  VALUE 'ASKING'.
           05                PIC X(5)  VALUE SPACES.
           05                PIC X(4)  VALUE 'PAID'.
           05                PIC X(6)  VALUE SPACES.
           05                PIC X(6)  VALUE 'DEALER'.
       
       01 W05-DETAIL-LINE.
           05                 PIC X(6) VALUE SPACES.
           05 W05-INVOICE-OUT PIC ZZZZ9.
           05                 PIC X(4) VALUE SPACES.
           05 W05-SALES-OUT   PIC X(7).
           05                 PIC X(6) VALUE SPACES.
           05 W05-YEAR-OUT    PIC 99.
           05                 PIC X(4) VALUE SPACES.
           05 W05-MAKER-OUT   PIC X(11).
           05                 PIC X(2) VALUE SPACES.
           05 W05-MODEL-OUT   PIC X(13).
           05                 PIC X(1) VALUE SPACES.
           05 W05-ASKING-OUT  PIC ZZZ,ZZ9.
           05                 PIC X(3) VALUE SPACES.
           05 W05-SOLD-OUT    PIC ZZZ,ZZ9.
           05                 PIC X(4) VALUE SPACES.
           05 W05-PERC-ASK    PIC ZV99.
           05                 PIC X(5) VALUE SPACES.
           05 W05-COMM-OUT    PIC ZZ,ZZ9.
           05                 PIC X(5) VALUE SPACES.
           05 W05-NET-DEALER  PIC ZZZ,ZZ9.
      
       01  W06-DASH-LINE.
           05                 PIC X(68) VALUE SPACES.
           05                 PIC X(10) VALUE '----------'.
           05                 PIC X(10) VALUE SPACES.
           05                 PIC X(8)  VALUE '--------'.
           05                 PIC X(3)  VALUE SPACES.
           05                 PIC X(9)  VALUE '---------'.

       01 W07-TOTALS-LINE.
           05                      PIC X(68) VALUE SPACES.
           05 W07-SOLD-TOTAL-OUT   PIC $Z,ZZZ,ZZ9.
           05                      PIC X(10) VALUE SPACES.
           05 W07-COMM-TOTAL-OUT   PIC $ZZZ,ZZ9.
           05                      PIC X(2) VALUE SPACES.
           05 W07-DEALER-TOTAL-OUT PIC $Z,ZZZ,ZZ9.
       
       01 W08-MATH.
      *W08 WAS NEEDED TO CALCLUATE AMOUNTS PRIOR TO OUTPUT
           05 W08-ASKING-PERC-MATH  PIC 9V99.
           05 W08-COMM-MATH         PIC 99999.
           05 W08-NET-DEALER-MATH   PIC 999999.
           05 W08-TOTAL-SOLD-MATH   PIC 9999999.
           05 W08-COMM-TOTAL-MATH   PIC 999999.
           05 W08-DEALER-TOTAL-MATH PIC 9999999.
           
       01  W09-DATA-REMAINS-SWITCH      PIC X(2)  VALUE SPACES.
      *PERFORM ALL CALCULATIONS NEEDED - AND WRITES TO OUTPUT FILE
       PROCEDURE DIVISION. 
           PERFORM 100-OPEN-FILES
           PERFORM 200-PRINT-HEADINGS
           PERFORM 300-PROCESS-RECORDS
               UNTIL W09-DATA-REMAINS-SWITCH = 'NO'
           PERFORM 400-PRINT-TOTALS
           PERFORM 500-CLOSE-FILES
          .
       100-OPEN-FILES.  
            OPEN INPUT  F01-CAR-RECORDS
                OUTPUT F02-PRINT-FILE
           READ F01-CAR-RECORDS
               AT END MOVE 'NO' TO W09-DATA-REMAINS-SWITCH
           END-READ . 
                 
       200-PRINT-HEADINGS.
           MOVE W01-HEADING-LINE TO F02-PRINT-LINE
           WRITE F02-PRINT-LINE
           MOVE W02-HEADING-LINE TO F02-PRINT-LINE
           WRITE F02-PRINT-LINE
           WRITE F02-PRINT-LINE FROM SPACES
           MOVE W03-HEADING-LINE TO F02-PRINT-LINE
           WRITE F02-PRINT-LINE
           MOVE W04-HEADING-LINE TO F02-PRINT-LINE
           WRITE F02-PRINT-LINE
                     
            .     
      *MOVE FILES IN PREPARATION TO WRITE TO OUTPUT FILE     
       300-PROCESS-RECORDS. 
           MOVE F01-INVOICE-NUM TO W05-INVOICE-OUT
           MOVE F01-SALES-P TO W05-SALES-OUT
           MOVE F01-YEAR TO W05-YEAR-OUT
           MOVE F01-MAKE TO W05-MAKER-OUT
           MOVE F01-MODEL TO W05-MODEL-OUT
           MOVE F01-ASKING-PRICE TO W05-ASKING-OUT
           MOVE F01-SOLD-PRICE TO W05-SOLD-OUT
           PERFORM 310-DO-CALCULATIONS
           MOVE W08-COMM-MATH TO W05-COMM-OUT
           MOVE W08-NET-DEALER-MATH TO W05-NET-DEALER
           MOVE W05-DETAIL-LINE TO F02-PRINT-LINE
           
           WRITE F02-PRINT-LINE
           
           
           
           READ F01-CAR-RECORDS
              AT END MOVE 'NO' TO W09-DATA-REMAINS-SWITCH
           END-READ.
           .
      *ALL CALCULATIONS ARE COMPLETED BELOW    
        310-DO-CALCULATIONS.
           COMPUTE W05-PERC-ASK ROUNDED = F01-SOLD-PRICE     
                                        / F01-ASKING-PRICE.
           MOVE W05-PERC-ASK TO W08-ASKING-PERC-MATH.
           
           IF W08-ASKING-PERC-MATH IS >= .95
           COMPUTE W08-COMM-MATH ROUNDED = ((F01-SOLD-PRICE - 
           (F01-ASKING-PRICE * 0.95)) * 0.4) + (0.05 * F01-SOLD-PRICE)
           
           ELSE    
               IF W08-ASKING-PERC-MATH IS < .95 AND IS >= .9
           COMPUTE W08-COMM-MATH ROUNDED  = (0.05	- (0.05 * 0.10 * 
                      (0.95 - W08-ASKING-PERC-MATH)))  * F01-SOLD-PRICE   
            
            ELSE
               IF W08-ASKING-PERC-MATH IS < .90
           COMPUTE W08-COMM-MATH ROUNDED = (F01-SOLD-PRICE * 0.025) - 
                       ((0.90 - W08-ASKING-PERC-MATH) * F01-SOLD-PRICE)
               
           END-IF.
           
           COMPUTE W08-NET-DEALER-MATH ROUNDED =(F01-SOLD-PRICE -
                             W08-COMM-MATH) - (F01-ASKING-PRICE / 1.25)
           COMPUTE W08-TOTAL-SOLD-MATH ROUNDED = W08-TOTAL-SOLD-MATH +
                                                         F01-SOLD-PRICE
           COMPUTE W08-COMM-TOTAL-MATH ROUNDED = W08-COMM-TOTAL-MATH +
                                                          W08-COMM-MATH
           COMPUTE W08-DEALER-TOTAL-MATH ROUNDED =W08-DEALER-TOTAL-MATH
                                                  + W08-NET-DEALER-MATH
           
       
        
        .
      *PRINTS THE TOTALS LINE AT THE BOTTOM OF THE OUTPUT FILE 
        400-PRINT-TOTALS.
           MOVE W08-TOTAL-SOLD-MATH TO W07-SOLD-TOTAL-OUT
           MOVE W08-COMM-TOTAL-MATH TO W07-COMM-TOTAL-OUT
           MOVE W08-DEALER-TOTAL-MATH TO W07-DEALER-TOTAL-OUT
           WRITE F02-PRINT-LINE FROM SPACES
           MOVE W06-DASH-LINE TO F02-PRINT-LINE
           WRITE F02-PRINT-LINE
           WRITE F02-PRINT-LINE FROM SPACES
           MOVE W07-TOTALS-LINE TO F02-PRINT-LINE
           WRITE F02-PRINT-LINE        
        .
      *CLOSE INPUT AND OUTPUT FILES      
       500-CLOSE-FILES.  
           CLOSE F01-CAR-RECORDS
                 F02-PRINT-FILE
           STOP RUN.
          
