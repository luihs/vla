       IDENTIFICATION DIVISION.                                         00010058
       PROGRAM-ID.    VL4CCACS.                                         00020062
       AUTHOR.        BBVA.                                             00030061
      ******************************************************************00040058
      * SISTEMA : VALORES                                              *00050058
      * FUNCION : GENERACION DE INFORME PARA REAJUSTE DE COMISIONES SAB*00060058
      * FECHA   : 12-10-2010                                           *00070058
      ******************************************************************00080058
      ******************************************************************00090058
      *PETIT/ SA  FECHA-MOD. PROGRAMADOR      DESCRIPCION              *00100058
      *---------- ---------- ---------------- -------------------------*00110058
      *RITM46847  12-1O-2021 LUIS RIVERA H.   CREACION DEL COMPONENTE  *00120058
      *---------- ---------- ---------------- -------------------------*00130058
      ******************************************************************00140058
       ENVIRONMENT DIVISION.                                            00150058
       CONFIGURATION SECTION.                                           00160058
       SPECIAL-NAMES.                                                   00170058
       INPUT-OUTPUT SECTION.                                            00180058
       FILE-CONTROL.                                                    00190058
                                                                        00200058
           SELECT E1VDTARC  ASSIGN       TO E1VDTARC                    00210058
                            FILE STATUS  IS FS-VLDTARC.                 00220058
                                                                        00230058
           SELECT S1RNEGBL  ASSIGN       TO S1RNEGBL                    00240058
                            FILE STATUS  IS FS-SNEGBOL.                 00250058
       DATA DIVISION.                                                   00260058
       FILE SECTION.                                                    00270058
      *                                                                *00280058
       FD  E1VDTARC                                                     00290058
           RECORDING MODE  IS  F                                        00300058
           LABEL  RECORDS  IS  STANDARD                                 00310058
           DATA RECORD     IS  REG-VLDTARC.                             00320058
         01  REG-VLDTARC           PIC X(256).                          00330058
      *                                                                *00340058
       FD  S1RNEGBL                                                     00350058
           RECORDING MODE  IS  F                                        00360058
           LABEL  RECORDS  IS  STANDARD                                 00370058
           DATA   RECORD   IS  SAL-NEG-BOLSA.                           00380058
         01 SAL-NEG-BOLSA          PIC X(159).                          00390058
      *                                                                 00400058
      ******************************************************************00410058
      **       W O R K I N G - S T O R A G E      S E C T I O N       **00420058
      ******************************************************************00430058
       WORKING-STORAGE SECTION.                                         00440058
      *************************                                         00450058
      * VARIALES GENERALES                                              00460058
       01  WS-GENERALES.                                                00470058
           05 NUMCLI-VLDTARC  PIC 9(8).                                 00480058
           05 CENTAD-VLDTARC  PIC 9(4) VALUE ZEROES.                    00490058
           05 SITUAC-VLDTARC  PIC X(1) VALUE SPACES.                    00500058
           05 CENTAD          PIC 9(4) VALUE 0069.                      00510058
           05 SITUAC          PIC X(1) VALUE 'A'.                       00520058
           05 CLI-IRELPAT     PIC X(1) VALUE SPACES.                    00530058
       01  NOMBRE-CLIE.                                                 00540058
           05 CLIE-NOMBRES    PIC X(20) VALUE SPACES.                   00550058
           05 ESPACIO         PIC X(01) VALUE SPACES.                   00560058
           05 CLIE-APE-PAT    PIC X(20) VALUE SPACES.                   00570058
           05 ESPACIO         PIC X(01) VALUE SPACES.                   00580058
           05 CLIE-APE-MAT    PIC X(20) VALUE SPACES.                   00590058
       01  NOMBRE-COMPLETO    PIC X(62) VALUE SPACES.                   00591060
       01  SALIDA-COMP.                                                 00600058
           05 SAL-CENTAD          PIC 9(4).                             00610058
           05 FILLER              PIC X(1) VALUE '|'.                   00620058
           05 SAL-SITUAC          PIC X(1).                             00630058
           05 FILLER              PIC X(1) VALUE '|'.                   00640058
           05 SAL-CTAVAL          PIC 9(8).                             00650058
           05 FILLER              PIC X(1) VALUE '|'.                   00660058
           05 SAL-MONEDA          PIC X(3).                             00670058
           05 FILLER              PIC X(1) VALUE '|'.                   00680058
           05 SAL-NUMCLI          PIC 9(8).                             00690058
           05 FILLER              PIC X(1) VALUE '|'.                   00700058
           05 SAL-INVERSOR        PIC 9(2).                             00710058
           05 FILLER              PIC X(1) VALUE '|'.                   00720058
           05 SAL-IRELPAT         PIC X(1).                             00730058
           05 FILLER              PIC X(1) VALUE '|'.                   00740058
           05 SAL-NOMBRE          PIC X(62).                            00750058
           05 FILLER              PIC X(1) VALUE '|'.                   00760058
           05 SAL-CTACARGO        PIC X(20).                            00770058
           05 FILLER              PIC X(1) VALUE '|'.                   00780058
           05 SAL-CTAABONO        PIC X(20).                            00790058
           05 FILLER              PIC X(1) VALUE '|'.                   00800058
           05 SAL-CTAVALOR        PIC X(20).                            00810058
       77  W-PROGRAMA         PIC X(08) VALUE 'VL4C7067'.               00820058
       77  PE9C5201           PIC X(08) VALUE 'PE9C5201'.               00830058
                                                                        00840058
       01 W-CONTADORES.                                                 00850058
          05 CONT-LEIDO-VLDTARC    PIC 9(10)  VALUE ZEROES.             00860058
          05 REG-IMPRESO           PIC 9(10)  VALUE ZEROES.             00870058
      * VARIABLES PARA CONTROL DE ERRORES                               00880058
       01 WS-ERROR.                                                     00890058
          05 WS-ACCION             PIC X(22).                           00900058
          05 WS-PARRAFO            PIC X(19).                           00910058
      * VARIABLES CONTROL DE ARCHIVOS - ESCRITURA                       00920058
       01 WS-CTRL-ARCHIVOS.                                             00930058
          05 FS-VLDTARC            PIC X(02) VALUE '00'.                00940058
             88 E1VDTARC-OK                  VALUE '00'.                00950058
             88 E1VDTARC-NOK                 VALUE '10'.                00960058
          05 FS-SNEGBOL            PIC X(02) VALUE '00'.                00970058
             88 S1RNEGBL-OK                  VALUE '00'.                00980058
             88 S1RNEGBL-NOK                 VALUE '10'.                00990058
          05 FIN-FILE-VLDTARC      PIC X(02).                           01000058
             88 FIN-VLDTARC                   VALUE '00'.               01010058
             88 FIN-NO-VLDTARC                VALUE '10'.               01020058
          05 FS-WRITE              PIC X(02).                           01030058
             88 WRITE-OK                      VALUE '00'.               01040058
             88 WRITE-NOK                     VALUE '10'.               01050058
      *                                                                *01060058
      *  AREA DE COMUNICACION SQLCA                                    *01070058
           EXEC SQL INCLUDE SQLCA END-EXEC.                             01080058
      *                                                                 01090058
      *  AREA DE DCLGEN'S DE TABLAS DB2                                *01100058
           EXEC SQL INCLUDE VLGTARC  END-EXEC.                          01110058
      *                                                                 01120058
      * DATOS DE CLIENTE - RUTINA PERSONAS                             *01130058
       01 PEWC5201.                                                     01140058
          COPY PEWC5201.                                                01150058
      *                                                                *01160058
       PROCEDURE DIVISION.                                              01170058
      *                                                                *01180058
           PERFORM 1000-INICIO                                          01190058
              THRU 1000-INICIO-EXIT.                                    01200058
                                                                        01210058
           PERFORM 2000-PROCESO                                         01220058
              UNTIL FIN-VLDTARC.                                        01230058
                                                                        01240058
           PERFORM 3000-FIN                                             01250058
              THRU 3000-FIN-EXIT.                                       01260058
                                                                        01270058
           STOP RUN.                                                    01280058
      *                                                                *01290058
      *-----------*                                                     01300058
       1000-INICIO.                                                     01310058
      *-----------*                                                     01320058
           INITIALIZE W-CONTADORES.                                     01330058
                                                                        01340058
           PERFORM 1100-ABRIR-FICHEROS                                  01350058
              THRU 1100-ABRIR-FICHEROS-EXIT.                            01360058
                                                                        01370058
           PERFORM 1300-LEE-VLDTARC.                                    01380058
      *                                                                *01390058
      *----------------*                                                01400058
       1000-INICIO-EXIT.                                                01410058
      *----------------*                                                01420058
           EXIT.                                                        01430058
                                                                        01440058
      *-------------------*                                             01450058
       1100-ABRIR-FICHEROS.                                             01460058
      *-------------------*                                             01470058
      *                                                                 01480058
           OPEN INPUT E1VDTARC.                                         01490058
           IF NOT E1VDTARC-OK                                           01500058
              MOVE 'OPEN FICHERO VLDTARC' TO WS-ACCION                  01510058
              MOVE '1200-ABRIR-FICHEROS'   TO WS-PARRAFO                01520058
              PERFORM 3001-ERROR                                        01530058
           END-IF                                                       01540058
      *                                                                 01550058
           OPEN OUTPUT S1RNEGBL.                                        01560058
           IF NOT S1RNEGBL-OK                                           01570058
              MOVE 'OPEN FICHERO SALIDA' TO WS-ACCION                   01580058
              MOVE '1200-ABRIR-FICHEROS'   TO WS-PARRAFO                01590058
              PERFORM 3001-ERROR                                        01600058
           END-IF.                                                      01610058
      *                                                                *01620058
      *------------------------*                                        01630058
       1100-ABRIR-FICHEROS-EXIT.                                        01640058
      *------------------------*                                        01650058
           EXIT.                                                        01660058
      *                                                                 01670058
      *----------------*                                                01680058
       1300-LEE-VLDTARC.                                                01690058
      *----------------*                                                01700058
           READ E1VDTARC                                                01710058
           AT END                                                       01720058
              SET FIN-VLDTARC TO TRUE                                   01730058
           NOT AT END                                                   01740058
              MOVE REG-VLDTARC TO DCLVLDTARC                            01750058
              ADD 1 TO CONT-LEIDO-VLDTARC                               01760058
              MOVE VARC-CENTAD TO CENTAD-VLDTARC                        01770058
              MOVE VARC-SITUAC TO SITUAC-VLDTARC                        01780058
           END-READ.                                                    01790058
      *                                                                 01800058
      *------------*                                                    01810058
       2000-PROCESO.                                                    01820058
      *------------*                                                    01830058
           IF CENTAD-VLDTARC = CENTAD AND                               01840058
              SITUAC-VLDTARC = SITUAC THEN                              01850058
              MOVE VARC-NUMCLI TO NUMCLI-VLDTARC                        01860058
              PERFORM 2100-OBTIENE-PERSONA                              01870058
              PERFORM 2200-MUEVE-DATOS                                  01880058
              PERFORM 2300-IMPRIME-REPORTE                              01890058
              PERFORM 1300-LEE-VLDTARC                                  01900058
           ELSE                                                         01910058
              PERFORM 1300-LEE-VLDTARC                                  01920058
           END-IF.                                                      01930058
      *                                                                *01940058
      *-----------------*                                               01950058
       2000-PROCESO-EXIT.                                               01960058
      *-----------------*                                               01970058
           EXIT.                                                        01980058
      *                                                                 01990058
       2200-MUEVE-DATOS.                                                02000058
      *-----------------*                                               02010058
           MOVE CENTAD-VLDTARC       TO SAL-CENTAD                      02020058
           MOVE SITUAC-VLDTARC       TO SAL-SITUAC                      02030058
           MOVE NUMCLI-VLDTARC       TO SAL-NUMCLI                      02040058
           MOVE VARC-INVERSOR        TO SAL-INVERSOR                    02050058
           MOVE CLI-IRELPAT          TO SAL-IRELPAT                     02060058
           MOVE NOMBRE-COMPLETO      TO SAL-NOMBRE                      02070060
                                                                        02080058
           MOVE VARC-CUENTA          TO SAL-CTAVAL                      02090058
           MOVE VARC-MONEDA          TO SAL-MONEDA                      02100058
           MOVE VARC-FILLER(01:20)   TO SAL-CTACARGO                    02110058
           MOVE VARC-FILLER(21:20)   TO SAL-CTAABONO                    02120058
           MOVE VARC-CTAVAL20        TO SAL-CTAVALOR                    02130058
           .                                                            02140058
      *                                                                 02150058
      *---------------------*                                           02160058
       2300-IMPRIME-REPORTE.                                            02170058
      *---------------------*                                           02180058
           MOVE SALIDA-COMP TO SAL-NEG-BOLSA                            02190058
           WRITE SAL-NEG-BOLSA                                          02200058
           ADD 1 TO REG-IMPRESO                                         02210058
           .                                                            02220058
      *                                                                *02230058
      *--------------------*                                            02240058
       2100-OBTIENE-PERSONA.                                            02250058
      *--------------------*                                            02260058
           INITIALIZE               W520-REGISTRO                       02270058
           MOVE NUMCLI-VLDTARC TO   W520-NUMCLIEN                       02280058
           CALL PE9C5201      USING W520-REGISTRO                       02290058
      *                                                                 02300058
           EVALUATE W520-PECRETOR                                       02310058
               WHEN '00'                                                02320058
                    IF  W520-IRELPAT = 'D' OR 'E' OR 'F' OR 'J'         02321058
                        MOVE W520-IRELPAT TO CLI-IRELPAT                02321158
                    ELSE                                                02321258
                        MOVE ' ' TO CLI-IRELPAT                         02321359
                    END-IF                                              02322058
                    IF W520-SUJGRUP = 'F'                               02323060
                       STRING W520-NOMBRE DELIMITED BY '  ' ' '         02324060
                              W520-PRIAPE DELIMITED BY '  ' ' '         02325060
                              W520-SEGAPE DELIMITED BY '  '             02326060
                                                   INTO NOMBRE-COMPLETO 02327060
                    ELSE                                                02328060
                       STRING W520-NOMBRE DELIMITED BY SIZE             02329060
                              W520-PRIAPE DELIMITED BY SIZE             02329160
                              W520-SEGAPE DELIMITED BY SIZE             02329260
                                                   INTO NOMBRE-COMPLETO 02329360
                    END-IF                                              02330060
      *             MOVE W520-PRIAPE    TO CLIE-APE-PAT                 02340060
      *             MOVE W520-SEGAPE    TO CLIE-APE-MAT                 02350060
      *             MOVE W520-NOMBRE    TO CLIE-NOMBRES                 02360060
               WHEN OTHER                                               02370058
                    CONTINUE                                            02380058
           END-EVALUATE.                                                02390058
      *                                                                *02400058
      *--------*                                                        02410058
       3000-FIN.                                                        02420058
      *--------*                                                        02430058
      *                                                                *02440058
           CLOSE E1VDTARC                                               02450058
                 S1RNEGBL                                               02460058
                                                                        02470058
           DISPLAY '*********** FIN DEL PROGRAMA ***********'           02480058
           DISPLAY 'LEIDOS VLDTARC:    ' CONT-LEIDO-VLDTARC             02490058
           DISPLAY 'REG.  GRABADOS:    ' REG-IMPRESO                    02500058
           .                                                            02510058
      *                                                                *02520058
      *-------------*                                                   02530058
       3000-FIN-EXIT.                                                   02540058
      *-------------*                                                   02550058
           EXIT.                                                        02560058
      *                                                                *02570058
       3001-ERROR.                                                      02580058
      *-----------*                                                     02590058
           DISPLAY '********** ERROR EN EL PROGRAMA **********'         02600058
           DISPLAY 'ACCION:          ' WS-ACCION                        02610058
           DISPLAY 'PARRAFO:         ' WS-PARRAFO                       02620058
           DISPLAY '******************************************'         02630058
           MOVE 08 TO RETURN-CODE                                       02640058
           STOP RUN.                                                    02650058
      *-----------------*                                               02660058
      * FIN DE PROGRAMA *                                               02670058
      *-----------------*                                               02680058
