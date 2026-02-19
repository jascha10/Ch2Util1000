       IDENTIFICATION DIVISION.
       PROGRAM-ID. UTIL2000.
      *****************************************************************
      *  Programmer.:Jacob Schamp
      *  Date.......:02-10-2026
      *  GitHub URL.:https://github.com/jascha10/Ch2Util1000
      *  Description:The goal of this program is to calculate customer
      *  utility bills based on a three-tier rate scale. The program
      *  ensures accurate billing by applying different rates as energy
      *  consumption increases, reflecting standard utility pricing
      *  models.
      *****************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *****************************************************************
      * CONSTANTS
      *****************************************************************
       01  WS-RATE-TIER1            PIC V99      VALUE .12.
       01  WS-RATE-TIER2            PIC V99      VALUE .15.
       01  WS-RATE-TIER3            PIC V99      VALUE .18.
       01  WS-TIER1-LIMIT           PIC 9(4)     VALUE 500.
       01  WS-TIER2-LIMIT           PIC 9(4)     VALUE 500.

      *****************************************************************
      * 3 PREDEFINED CUSTOMERS (NO TABLES)
      *****************************************************************
       01  WS-CUST-1.
           05  WS-C1-NAME           PIC X(12)   VALUE 'CUST-ALPHA  '.
           05  WS-C1-KWH            PIC 9(5)    VALUE 350.
           05  WS-C1-FEE            PIC 9(3)V99 VALUE 14.95.
       01  WS-CUST-2.
           05  WS-C2-NAME           PIC X(12)   VALUE 'CUST-BRAVO  '.
           05  WS-C2-KWH            PIC 9(5)    VALUE 925.
           05  WS-C2-FEE            PIC 9(3)V99 VALUE 14.95.
       01  WS-CUST-3.
           05  WS-C3-NAME           PIC X(12)   VALUE 'CUST-CHARLIE'.
           05  WS-C3-KWH            PIC 9(5)    VALUE 1350.
           05  WS-C3-FEE            PIC 9(3)V99 VALUE 14.95.

      *****************************************************************
      * CURRENT "INPUT" FIELDS (LOADED PER CUSTOMER)
      *****************************************************************
       01  WS-CUST-NAME             PIC X(12)   VALUE SPACES.
       01  WS-KWH-USED              PIC 9(5)    VALUE 0.
       01  WS-SERVICE-FEE           PIC 9(3)V99 VALUE 0.

      *****************************************************************
      * WORK AREAS
      *****************************************************************
       01  WS-TIER1-KWH             PIC 9(5)     VALUE 0.
       01  WS-TIER2-KWH             PIC 9(5)     VALUE 0.
       01  WS-TIER3-KWH             PIC 9(5)     VALUE 0.

       01  WS-TIER1-CHARGE          PIC 9(5)V99  VALUE 0.
       01  WS-TIER2-CHARGE          PIC 9(5)V99  VALUE 0.
       01  WS-TIER3-CHARGE          PIC 9(5)V99  VALUE 0.

       01  WS-SUBTOTAL              PIC 9(6)V99  VALUE 0.
       01  WS-TOTAL-BILL            PIC 9(6)V99  VALUE 0.

      *****************************************************************
      * EDITED FIELDS FOR DISPLAY
      *****************************************************************
       01  WS-KWH-USED-ED           PIC Z,ZZZ,ZZZ,ZZ9.
       01  WS-MONEY-ED              PIC $$,$$$,$$9.99.
       01  WS-MONEY-ED2             PIC $$,$$$,$$9.99.

      *****************************************************************
      * IT'S GO TIME!
      *****************************************************************
       PROCEDURE DIVISION.

      *****************************************************************
      * MAINLINE - DISPLAY HEADING, LOAD CUSTOMER, RUN BILL, STOP
      *****************************************************************
       000-MAIN.
           DISPLAY '********************************'.
           DISPLAY '*** UTIL2000 - CUSTOMER BILL ***'.
           DISPLAY '********************************'.
           DISPLAY ' '.

           PERFORM 500-LOAD-CUST.
           PERFORM 600-RUN-BILL.

           STOP RUN.

      *****************************************************************
      * MOVE name/kwh/fee from CUST into current fields.
      *****************************************************************
       500-LOAD-CUST.
           MOVE WS-C1-NAME TO WS-CUST-NAME.
           MOVE WS-C1-KWH  TO WS-KWH-USED.
           MOVE WS-C1-FEE  TO WS-SERVICE-FEE.
           MOVE WS-C2-NAME TO WS-CUST-NAME.
           MOVE WS-C2-KWH  TO WS-KWH-USED.
           MOVE WS-C2-FEE  TO WS-SERVICE-FEE.
           MOVE WS-C3-NAME TO WS-CUST-NAME.
           MOVE WS-C3-KWH  TO WS-KWH-USED.
           MOVE WS-C3-FEE  TO WS-SERVICE-FEE.

      *****************************************************************
      * BILL ROUTINE
      *****************************************************************
       600-RUN-BILL.
           PERFORM 100-INITIALIZE.
           PERFORM 200-CALC-TIERS.
           PERFORM 300-CALC-CHARGES.
           PERFORM 400-DISPLAY-RESULTS.
           DISPLAY ' '.

      *****************************************************************
      * Zero tier kWh, charges, subtotal, total
      *****************************************************************
       100-INITIALIZE.
           MOVE 0 TO WS-TIER1-KWH
                    WS-TIER2-KWH
                    WS-TIER3-KWH
                    WS-TIER1-CHARGE
                    WS-TIER2-CHARGE
                    WS-TIER3-CHARGE
                    WS-SUBTOTAL
                    WS-TOTAL-BILL.

      *****************************************************************
      * Determine WS-TIER1-KWH, WS-TIER2-KWH, WS-TIER3-KWH
      * based on WS-KWH-USED
      *
      * These are the per-kWh rates:
      * - Tier 1: first 500 kWh at $0.12/kWh
      * - Tier 2: next 500 kWh (kWh 5011000) at $0.15/kWh
      * - Tier 3: any kWh above 1000 at $0.18/kWh
      *****************************************************************
       200-CALC-TIERS.
           *> If amount used is less than 500 kWh, all goes in tier 1
           IF WS-KWH-USED <= WS-TIER1-LIMIT
               MOVE WS-KWH-USED TO WS-TIER1-KWH
               MOVE 0 TO WS-TIER2-KWH WS-TIER3-KWH
           ELSE
               MOVE WS-TIER1-LIMIT TO WS-TIER1-KWH

               *> If amount used is between 501 and 1000 kWh,
               *> tier 1 is full, remainder goes in tier 2
               IF WS-KWH-USED <= (WS-TIER1-LIMIT + WS-TIER2-LIMIT)
                   COMPUTE WS-TIER2-KWH =
                       WS-KWH-USED - WS-TIER1-LIMIT
                   MOVE 0 TO WS-TIER3-KWH

               *> If amount used is between 1001 and above,
               *> tier 1 and tier 2 are full, remainder goes in tier 3
               ELSE
                   MOVE WS-TIER2-LIMIT TO WS-TIER2-KWH
                   COMPUTE WS-TIER3-KWH =
                       WS-KWH-USED - WS-TIER1-LIMIT - WS-TIER2-LIMIT
               END-IF
           END-IF.

      *****************************************************************
      * COMPUTE charges using ROUNDED and compute totals.
      *****************************************************************
       300-CALC-CHARGES.
           COMPUTE WS-TIER1-CHARGE ROUNDED =
               WS-TIER1-KWH * WS-RATE-TIER1.


           COMPUTE WS-TIER2-CHARGE ROUNDED =
               WS-TIER2-KWH * WS-RATE-TIER2.


           COMPUTE WS-TIER3-CHARGE ROUNDED =
               WS-TIER3-KWH * WS-RATE-TIER3.

           COMPUTE WS-SUBTOTAL =
               WS-TIER1-CHARGE + WS-TIER2-CHARGE + WS-TIER3-CHARGE.
           COMPUTE WS-TOTAL-BILL =
               WS-SUBTOTAL + WS-SERVICE-FEE.

      *****************************************************************
      * Display report including customer name.
      *****************************************************************
       400-DISPLAY-RESULTS.
           MOVE WS-KWH-USED TO WS-KWH-USED-ED.

           DISPLAY '--------------------------------'.
           DISPLAY 'CUSTOMER: ' WS-CUST-NAME.
           DISPLAY '--------------------------------'.
           DISPLAY 'KWH USED       : ' WS-KWH-USED-ED.

           MOVE WS-SERVICE-FEE TO WS-MONEY-ED.
           DISPLAY 'SERVICE FEE    : ' WS-MONEY-ED.

           MOVE WS-TIER1-CHARGE TO WS-MONEY-ED.
           DISPLAY 'TIER 1 CHARGE  : ' WS-MONEY-ED.

           MOVE WS-TIER2-CHARGE TO WS-MONEY-ED.
           DISPLAY 'TIER 2 CHARGE  : ' WS-MONEY-ED.

           MOVE WS-TIER3-CHARGE TO WS-MONEY-ED.
           DISPLAY 'TIER 3 CHARGE  : ' WS-MONEY-ED.

           MOVE WS-TOTAL-BILL TO WS-MONEY-ED2.
           DISPLAY '--------------------------------'.
           DISPLAY 'TOTAL BILL     : ' WS-MONEY-ED2.
           MOVE WS-KWH-USED TO WS-KWH-USED-ED.


           DISPLAY '--------------------------------'.
           DISPLAY 'CUSTOMER: ' WS-CUST-NAME.
           DISPLAY '--------------------------------'.
           DISPLAY 'KWH USED       : ' WS-KWH-USED-ED.

           MOVE WS-SERVICE-FEE TO WS-MONEY-ED.
           DISPLAY 'SERVICE FEE    : ' WS-MONEY-ED.

           MOVE WS-TIER1-CHARGE TO WS-MONEY-ED.
           DISPLAY 'TIER 1 CHARGE  : ' WS-MONEY-ED.

           MOVE WS-TIER2-CHARGE TO WS-MONEY-ED.
           DISPLAY 'TIER 2 CHARGE  : ' WS-MONEY-ED.

           MOVE WS-TIER3-CHARGE TO WS-MONEY-ED.
           DISPLAY 'TIER 3 CHARGE  : ' WS-MONEY-ED.

           MOVE WS-TOTAL-BILL TO WS-MONEY-ED2.
           DISPLAY '--------------------------------'.
           DISPLAY 'TOTAL BILL     : ' WS-MONEY-ED2.



           DISPLAY '--------------------------------'.
           DISPLAY 'CUSTOMER: ' WS-CUST-NAME.
           DISPLAY '--------------------------------'.
           DISPLAY 'KWH USED       : ' WS-KWH-USED-ED.

           MOVE WS-SERVICE-FEE TO WS-MONEY-ED.
           DISPLAY 'SERVICE FEE    : ' WS-MONEY-ED.

           MOVE WS-TIER1-CHARGE TO WS-MONEY-ED.
           DISPLAY 'TIER 1 CHARGE  : ' WS-MONEY-ED.

           MOVE WS-TIER2-CHARGE TO WS-MONEY-ED.
           DISPLAY 'TIER 2 CHARGE  : ' WS-MONEY-ED.

           MOVE WS-TIER3-CHARGE TO WS-MONEY-ED.
           DISPLAY 'TIER 3 CHARGE  : ' WS-MONEY-ED.

           MOVE WS-TOTAL-BILL TO WS-MONEY-ED2.
           DISPLAY '--------------------------------'.
           DISPLAY 'TOTAL BILL     : ' WS-MONEY-ED2.
           DISPLAY '--------------------------------'.
