      *-----------------------*                                         00010015
       IDENTIFICATION DIVISION.                                         00020015
      *-----------------------*                                         00030015
       PROGRAM-ID.   VL3CFTC0.                                          00040015
      *AUTHOR.       EULER ALVARADO.                                    00050015
      ******************************************************************00060015
       ENVIRONMENT DIVISION.                                            00070015
       CONFIGURATION SECTION.                                           00080015
       INPUT-OUTPUT SECTION.                                            00090015
      *------------*                                                    00100015
       FILE-CONTROL.                                                    00110015
      *------------*                                                    00120015
            SELECT E1DQ9FTC ASSIGN TO E1DQ9FTC                          00130015
                   FILE STATUS IS FS-E1DQ9FTC                           00140015
                   ORGANIZATION IS SEQUENTIAL.                          00150015
                                                                        00160015
            SELECT S1DQ9FTC ASSIGN TO S1DQ9FTC                          00170015
                   FILE STATUS IS FS-S1DQ9FTC                           00180015
                   ORGANIZATION IS SEQUENTIAL.                          00190015
      *-----------------------------------------------------------------00200015
      *-------------*                                                   00210015
       DATA DIVISION.                                                   00220015
      *-------------*                                                   00230015
       FILE SECTION.                                                    00240015
                                                                        00250015
       FD  E1DQ9FTC                                                     00260015
           RECORDING MODE IS F                                          00270015
           BLOCK CONTAINS 0 RECORDS                                     00280015
           DATA RECORD IS REG-E1DQ9FTC.                                 00290015
       01  REG-E1DQ9FTC   PIC X(256).                                   00300015
                                                                        00310015
       FD  S1DQ9FTC                                                     00320015
           RECORDING MODE IS F                                          00330015
           BLOCK CONTAINS 0 RECORDS                                     00340015
           DATA RECORD IS REG-S1DQ9FTC.                                 00350015
       01  REG-S1DQ9FTC.                                                00360015
           10 S01-CTAVAL20         PIC X(20).                           00370015
           10 S01-FILLER1          PIC X(01).                           00380015
           10 S01-MONEDA           PIC X(03).                           00390015
           10 S01-FILLER2          PIC X(01).                           00400015
           10 S01-NUMCLI           PIC 9(08).                           00410015
           10 S01-FILLER3          PIC X(01).                           00420015
           10 S01-CLIENTE          PIC X(60).                           00430015
           10 S01-FILLER4          PIC X(01).                           00440015
           10 S01-SITUACION        PIC X(09).                           00450015
           10 S01-FILLER5          PIC X(01).                           00460015
           10 S01-FECALTA          PIC X(10).                           00470015
           10 S01-FILLER6          PIC X(01).                           00480015
           10 S01-FECCESE          PIC X(10).                           00490015
           10 S01-FILLER7          PIC X(01).                           00500015
           10 S01-RUT              PIC 9(08).                           00510015
      *                                                                 00520015
      *-----------------------------------------------------------------00530015
       WORKING-STORAGE SECTION.                                         00540015
      *-----------------------*                                         00550015
       77  WS-NAME                 PIC X(70) VALUE                      00560015
                                   '**  INICIO WORKING VL4C9MAE **'.    00570015
      ******************************************************************00580015
      *                                                                 00590015
       01  PE9C5201                PIC X(08) VALUE 'PE9C5201'.          00600015
RTP0   01  PE9C5000                PIC X(08) VALUE 'PE9C5000'.          00601017
       01  FILE-STATUS.                                                 00610015
           10 FS-E1DQ9FTC          PIC X(02) VALUE SPACES.              00620015
           10 FS-S1DQ9FTC          PIC X(02) VALUE SPACES.              00630015
       01  WSV-CLIENTE             PIC X(60) VALUE SPACES.              00640015
       01  WSV-FECHA-10-A          PIC X(10) VALUE SPACES.              00650015
       01  WSV-FECHA-8-N           PIC 9(08) VALUE ZEROS.               00660015
       01  WSV-FECHA-8-A REDEFINES WSV-FECHA-8-N PIC X(08).             00670015
                                                                        00671016
@RTP1  01  WS-FECHA-D.                                                  00672016
@RTP1      02  WS-F-AA-D   PIC 9999.                                    00673016
@RTP1      02  WS-F-MM-D   PIC 99.                                      00674016
@RTP1      02  WS-F-DD-D   PIC 99.                                      00675016
@RTP1  01  WS-RFECHA-D  REDEFINES WS-FECHA-D PIC 9(08).                 00676016
                                                                        00677016
       01  WSV-LEIDOS              PIC 9(08) VALUE ZEROS.               00680015
       01  WSV-ESCRITOS            PIC 9(08) VALUE ZEROS.               00690015
       01  W-DCLVLDTARC.                                                00700015
           10 WARC-CUENTA          PIC S9(7)V USAGE COMP-3.             00710015
           10 WARC-CENTAD          PIC S9(4)V USAGE COMP-3.             00720015
           10 WARC-NUMCLI          PIC S9(8)V USAGE COMP-3.             00730015
           10 WARC-CLMAST          PIC X(1).                            00740015
           10 WARC-MONEDA          PIC X(3).                            00750015
           10 WARC-SUCURS          PIC S9(4)V USAGE COMP-3.             00760015
           10 WARC-CTACAR          PIC S9(12)V USAGE COMP-3.            00770015
           10 WARC-CTAABO          PIC S9(12)V USAGE COMP-3.            00780015
           10 WARC-TEXTO           PIC X(1).                            00790015
           10 WARC-PRESEN          PIC S9(5)V USAGE COMP-3.             00800015
           10 WARC-GRUPO           PIC S9(4)V USAGE COMP-3.             00810015
           10 WARC-RUT             PIC S9(8)V USAGE COMP-3.             00820015
           10 WARC-CNAE            PIC S9(4)V USAGE COMP-3.             00830015
           10 WARC-SITUAC          PIC X(1).                            00840015
           10 WARC-EXEN1           PIC S9(3)V USAGE COMP-3.             00850015
           10 WARC-EXEN2           PIC S9(3)V USAGE COMP-3.             00860015
           10 WARC-EXEN3           PIC S9(3)V USAGE COMP-3.             00870015
           10 WARC-EXEN4           PIC S9(3)V USAGE COMP-3.             00880015
           10 WARC-EXEN5           PIC S9(3)V USAGE COMP-3.             00890015
           10 WARC-EXEN6           PIC S9(3)V USAGE COMP-3.             00900015
           10 WARC-EXEN7           PIC S9(3)V USAGE COMP-3.             00910015
           10 WARC-EXEN8           PIC S9(3)V USAGE COMP-3.             00920015
           10 WARC-EXEN9           PIC S9(3)V USAGE COMP-3.             00930015
           10 WARC-EXEN10          PIC S9(3)V USAGE COMP-3.             00940015
           10 WARC-ANALIS          PIC X(1).                            00950015
           10 WARC-CLACARGO        PIC X(1).                            00960015
           10 WARC-CLABONO         PIC X(1).                            00970015
           10 WARC-NUMDOM          PIC S9(3)V USAGE COMP-3.             00980015
           10 WARC-CODSUS          PIC X(4).                            00990015
           10 WARC-FE-ULT-EXT      PIC S9(8)V USAGE COMP-3.             01000015
           10 WARC-PAIS            PIC X(4).                            01010015
           10 WARC-FE-CARTERA      PIC S9(8)V USAGE COMP-3.             01020015
           10 WARC-CLTELEX         PIC X(10).                           01030015
           10 WARC-FE-ALTA         PIC S9(8)V USAGE COMP-3.             01040015
           10 WARC-VALORACION      PIC X(1).                            01050015
           10 WARC-VALEXTRJ        PIC X(1).                            01060015
           10 WARC-INVERSOR        PIC S9(2)V USAGE COMP-3.             01070015
           10 WARC-DIRECTA         PIC X(1).                            01080015
           10 WARC-MAX-CVE-1       PIC S9(7)V USAGE COMP-3.             01090015
           10 WARC-MAX-DCU-5       PIC S9(7)V USAGE COMP-3.             01100015
           10 WARC-MAX-SUS-6       PIC S9(7)V USAGE COMP-3.             01110015
           10 WARC-MAX-DIV-7       PIC S9(7)V USAGE COMP-3.             01120015
           10 WARC-MAX-AMO-8       PIC S9(7)V USAGE COMP-3.             01130015
           10 WARC-MAX-PAJ-9       PIC S9(7)V USAGE COMP-3.             01140015
           10 WARC-FECHA-102       PIC S9(8)V USAGE COMP-3.             01150015
           10 WARC-TARIFACUS       PIC S9(1)V USAGE COMP-3.             01160015
           10 WARC-SWIFT-TELEX     PIC X(1).                            01170015
           10 WARC-TELEX-2         PIC X(2).                            01180015
           10 WARC-GRUPO-CTAS      PIC S9(3)V USAGE COMP-3.             01190015
           10 WARC-OPER-TIT        PIC X(1).                            01200015
           10 WARC-FEALTREG        PIC S9(8)V USAGE COMP-3.             01210015
           10 WARC-FEULMOD         PIC S9(8)V USAGE COMP-3.             01220015
           10 WARC-HORULMOD        PIC S9(6)V USAGE COMP-3.             01230015
           10 WARC-NUMTER          PIC X(4).                            01240015
           10 WARC-USUARIO         PIC X(7).                            01250015
           10 WARC-FILLER          PIC X(60).                           01260015
           10 WARC-CTAVAL20        PIC X(20).                           01270015
           10 WARC-NUMMAN          PIC S9(1)V USAGE COMP-3.             01280015
           10 WARC-INDIMP          PIC X(1).                            01290015
           10 WARC-INDSAB          PIC X(1).                            01300015
      *                                                                 01310015
      *    BD PERSONAS                                                  01320015
       01  W-PEWC5201.                                                  01330015
           COPY PEWC5201.                                               01340015
      *                                                                 01350015
RTP0   01 PEWC5000.                                                     01351016
RTP0       COPY PEWC5000.                                               01352016
      *                                                                 01353016
      *---------------*                                                 01360015
       LINKAGE SECTION.                                                 01370015
      *---------------*                                                 01380015
@RTP0  01  LK-PARAMETROS.                                               01381016
@RTP0      02  LK-LONGITUD     PIC S9(4)   COMP.                        01382016
@RTP0      02  LK-FECHA-D.                                              01383016
@RTP0          03  LK-F-AA-D   PIC 9999.                                01384016
@RTP0          03  LK-F-MM-D   PIC 99.                                  01385016
@RTP0          03  LK-F-DD-D   PIC 99.                                  01386016
@RTP0      02  LK-RFECHA-D  REDEFINES LK-FECHA-D PIC 9(08).             01387016
@RTP0      02  LK-FECHA-H.                                              01388016
@RTP0          03  LK-F-AA-H   PIC 9999.                                01389016
@RTP0          03  LK-F-MM-H   PIC 99.                                  01389116
@RTP0          03  LK-F-DD-H   PIC 99.                                  01389216
@RTP0      02  LK-RFECHA-H  REDEFINES LK-FECHA-H PIC 9(08).             01389316
@RTP0      02  LK-PROCESO      PIC 9(01).                               01389416
      *                                                                 01389516
      *---------------------------------------*                         01400015
@RTP0  PROCEDURE DIVISION USING LK-PARAMETROS.                          01410016
      *---------------------------------------*                         01420015
      *                                                                 01430015
           PERFORM 10000-INICIO.                                        01440015
      *                                                                 01450015
           PERFORM 20000-PROCESO UNTIL FS-E1DQ9FTC = '10'.              01460015
      *                                                                 01470015
           PERFORM 30000-FIN.                                           01480015
      *                                                                 01490015
           STOP RUN.                                                    01500015
      *                                                                 01510015
      ******************************************************************01520015
      *                       1-INICIO                                 *01530015
      *       INICIALIZA LAS WORKAS DE LAS TABLAS Y LOS CAMPOS DE      *01540015
      *       TRABAJO. LEE LA FECHA EN LA TABLA UGDTPRC, TOMANDO LA    *01550015
      *       FILA CON CODIGO DE PROCESO = 'UB00' Y COMPRUEBA QUE      *01560015
      *       EL PROCESO ESTA ACTIVO.                                  *01570015
      ******************************************************************01580015
       10000-INICIO.                                                    01590015
      *-------------*                                                   01600015
      *                                                                 01610015
           OPEN INPUT  E1DQ9FTC                                         01620015
                OUTPUT S1DQ9FTC                                         01630015
                                                                        01640015
           IF (FS-E1DQ9FTC EQUAL '00' OR '97')                          01650015
              CONTINUE                                                  01660015
           ELSE                                                         01670015
              DISPLAY '***********************************'             01680015
              DISPLAY '*  ERROR AL OPEN DE ENTRADA1      *'             01690015
              DISPLAY '*  ERROR FS-OPS ES :' FS-E1DQ9FTC                01700015
              DISPLAY '***********************************'             01710015
              MOVE '02'  TO RETURN-CODE                                 01720015
              STOP RUN                                                  01730015
           END-IF                                                       01740015
                                                                        01750015
           IF (FS-S1DQ9FTC EQUAL '00' OR '97')                          01760015
              CONTINUE                                                  01770015
           ELSE                                                         01780015
              DISPLAY '***********************************'             01790015
              DISPLAY '*  ERROR AL OPEN DE SALIDA1       *'             01800015
              DISPLAY '*  ERROR FS-OPS ES :' FS-S1DQ9FTC                01810015
              DISPLAY '***********************************'             01820015
              MOVE '02'  TO RETURN-CODE                                 01830015
              STOP RUN                                                  01840015
           END-IF                                                       01850015
                                                                        01860015
           PERFORM 10010-LEER-ENTRADA                                   01870015
           .                                                            01880015
      *                                                                 01890015
      *------------------*                                              01900015
       10010-LEER-ENTRADA.                                              01910015
      *------------------*                                              01920015
      *                                                                 01930015
           READ E1DQ9FTC                                                01940015
                                                                        01950015
           EVALUATE FS-E1DQ9FTC                                         01960015
              WHEN '00'                                                 01970015
                   ADD  1                      TO WSV-LEIDOS            01980015
                   MOVE REG-E1DQ9FTC           TO W-DCLVLDTARC          01990015
              WHEN '10'                                                 02000015
                   CONTINUE                                             02010015
              WHEN OTHER                                                02020015
                   DISPLAY '***********************************'        02030015
                   DISPLAY '*  ERROR AL LEER ENTRADA          *'        02040015
                   DISPLAY '*  ERROR FS-OPS ES :' FS-E1DQ9FTC           02050015
                   DISPLAY '***********************************'        02060015
                   MOVE '02'  TO RETURN-CODE                            02070015
                   STOP RUN                                             02080015
           END-EVALUATE                                                 02090015
           .                                                            02100015
      *                                                                 02110015
      *     *-------*                                                   02120015
       20000-PROCESO.                                                   02130015
      *     *-------*                                                   02140015
      *                                                                 02150015
           INITIALIZE REG-S1DQ9FTC                                      02160015
      *                                                                 02170015
           MOVE '|'                      TO S01-FILLER1  S01-FILLER2    02180015
                                            S01-FILLER3  S01-FILLER4    02190015
                                            S01-FILLER5  S01-FILLER6    02200015
                                            S01-FILLER7                 02210015
      *                                                                 02220015
           MOVE WARC-CTAVAL20            TO S01-CTAVAL20                02230015
           MOVE WARC-MONEDA              TO S01-MONEDA                  02240015
           MOVE WARC-NUMCLI              TO S01-NUMCLI                  02250015
           MOVE WARC-RUT                 TO S01-RUT                     02260015
                                                                        02270015
RTP0  *    PERFORM 20010-OBTIENE-CLIENTE                                02282016
RTP0       PERFORM 220-RUTINA-PERSONA                                   02283016
           MOVE WSV-CLIENTE              TO S01-CLIENTE                 02290015
                                                                        02300015
           IF WARC-SITUAC = 'A'                                         02310015
              MOVE 'ACTIVA   '           TO S01-SITUACION               02320015
           ELSE                                                         02330015
              MOVE 'CANCELADA'           TO S01-SITUACION               02340015
           END-IF                                                       02350015
                                                                        02360015
           MOVE WARC-FEALTREG            TO WSV-FECHA-8-N               02370015
           MOVE WSV-FECHA-8-A (07:02)    TO S01-FECALTA (01:02)         02380015
           MOVE '/'                      TO S01-FECALTA (03:01)         02390015
           MOVE WSV-FECHA-8-A (05:02)    TO S01-FECALTA (04:02)         02400015
           MOVE '/'                      TO S01-FECALTA (06:01)         02410015
           MOVE WSV-FECHA-8-A (01:04)    TO S01-FECALTA (07:04)         02420015
                                                                        02430015
           IF WARC-SITUAC = 'A'                                         02440015
              MOVE SPACES                TO S01-FECCESE                 02450015
           ELSE                                                         02460015
              MOVE WARC-FEULMOD             TO WSV-FECHA-8-N            02470015
              MOVE WSV-FECHA-8-A (07:02)    TO S01-FECCESE (01:02)      02480015
              MOVE '/'                      TO S01-FECCESE (03:01)      02490015
              MOVE WSV-FECHA-8-A (05:02)    TO S01-FECCESE (04:02)      02500015
              MOVE '/'                      TO S01-FECCESE (06:01)      02510015
              MOVE WSV-FECHA-8-A (01:04)    TO S01-FECCESE (07:04)      02520015
           END-IF                                                       02530015
                                                                        02540015
@RTP1      IF WARC-SITUAC = 'A' AND WSV-FECHA-8-N < LK-FECHA-D          02550018
@RTP1 *       GRABA ACTIVOS                                             02551017
@RTP1         PERFORM 20020-GRABA-SALIDA                                02552017
@RTP1      ELSE                                                         02553017
@RTP1 *       GRABA CANCELADOS                                          02554017
@RTP1 *       SI EL PROCESO ES DIARIO                                   02555017
@RTP1         IF LK-PROCESO = 1 AND LK-FECHA-D = WSV-FECHA-8-N          02556017
@RTP1            PERFORM 20020-GRABA-SALIDA                             02557017
@RTP1         END-IF                                                    02558017
@RTP1 *       SI EL PROCESO ES MENSUAL                                  02559017
@RTP1         IF LK-PROCESO = 2                                         02559117
@RTP1            MOVE WSV-FECHA-8-N TO WS-FECHA-D                       02559217
@RTP1            IF LK-F-AA-D = WS-F-AA-D AND LK-F-MM-D = WS-F-MM-D     02559317
@RTP1               PERFORM 20020-GRABA-SALIDA                          02559417
@RTP1            END-IF                                                 02559517
@RTP1         END-IF                                                    02559617
@RTP1 *       SI EL PROCESO ES TOTAL                                    02559717
@RTP1         IF LK-PROCESO = 3                                         02559817
@RTP1            PERFORM 20020-GRABA-SALIDA                             02559917
@RTP1         END-IF                                                    02560017
           END-IF                                                       02560117
                                                                        02561015
           PERFORM 10010-LEER-ENTRADA                                   02570015
           .                                                            02580015
      *                                                                 02590015
      *     *---------------*                                           02600015
       20010-OBTIENE-CLIENTE.                                           02610015
      *     *---------------*                                           02620015
      *                                                                 02630015
           INITIALIZE           W520-REGISTRO                           02640015
      *                                                                 02650015
           MOVE WARC-NUMCLI        TO W520-NUMCLIEN                     02660015
           MOVE SPACES             TO WSV-CLIENTE                       02670015
                                                                        02680015
           CALL PE9C5201 USING W-PEWC5201                               02690015
                                                                        02700015
           EVALUATE W520-PECRETOR                                       02710015
              WHEN ZEROS                                                02720015
                  IF W520-SUJGRUP = 'F'                                 02730015
                     STRING W520-PRIAPE DELIMITED BY '  ' ' '           02740015
                            W520-SEGAPE DELIMITED BY '  ' ' '           02750015
                            W520-NOMBRE DELIMITED BY '  '               02760015
                                              INTO WSV-CLIENTE          02770015
                  ELSE                                                  02780015
                     STRING W520-NOMBRE DELIMITED BY SIZE               02790015
                            W520-PRIAPE DELIMITED BY SIZE               02800015
                            W520-SEGAPE DELIMITED BY SIZE               02810015
                                              INTO WSV-CLIENTE          02820015
                  END-IF                                                02830015
              WHEN OTHER                                                02840015
                   MOVE '***NO UBICADO***'          TO  WSV-CLIENTE     02850015
           END-EVALUATE                                                 02860015
           .                                                            02870015
      *                                                                 02871017
       220-RUTINA-PERSONA.                                              02872017
      *-------------------*                                             02873017
      *                                                                 02874017
           INITIALIZE W500-REGISTRO                                     02875017
           MOVE SPACES               TO WSV-CLIENTE                     02876017
                                                                        02877017
           MOVE WARC-CTAVAL20(13:08) TO  W500-NUMECTA                   02878017
           MOVE WARC-CTAVAL20(01:04) TO  W500-PECENTID                  02879017
           MOVE WARC-CTAVAL20(05:04) TO  W500-OFIAPE                    02879117
           MOVE WARC-CTAVAL20(11:02) TO  W500-CODISER                   02879217
           MOVE 'T'                  TO  W500-CLAINTER                  02879317
           MOVE '01'                 TO  W500-SECINTER                  02879417
           MOVE 'U'                  TO  W500-PEYSELEC                  02879517
                                                                        02879617
           CALL PE9C5000 USING PEWC5000                                 02879717
                                                                        02879817
           IF W500-PECRETOR = '00'                                      02879917
              EVALUATE W500-SUJGRUP(1)                                  02880017
              WHEN 'F'                                                  02880117
                   STRING W500-PRIAPE(1) DELIMITED BY '  ' ' '          02880217
                          W500-SEGAPE(1) DELIMITED BY '  ' ' '          02880317
                          W500-NOMBRE(1) DELIMITED BY '  '              02880417
                          INTO WSV-CLIENTE                              02880517
                                                                        02880617
              WHEN 'M'                                                  02880717
                   STRING W500-NOMBRE(1) DELIMITED BY SIZE              02880817
                          W500-PRIAPE(1) DELIMITED BY SIZE              02880917
                          W500-SEGAPE(1) DELIMITED BY SIZE              02881017
                          INTO WSV-CLIENTE                              02881117
              END-EVALUATE                                              02881217
           END-IF                                                       02881317
           .                                                            02881417
                                                                        02881517
      *                                                                 02882015
      *     *------------*                                              02890015
       20020-GRABA-SALIDA.                                              02900015
      *     *------------*                                              02910015
      *                                                                 02920015
           WRITE REG-S1DQ9FTC                                           02930015
                                                                        02940015
           IF (FS-S1DQ9FTC EQUAL '00')                                  02950015
               ADD 1                   TO WSV-ESCRITOS                  02960015
           ELSE                                                         02970015
              DISPLAY '*  ERROR EN GRABAR REGISTROS FS : ' FS-S1DQ9FTC  02980015
              DISPLAY '*  REGISTRO LEIDOS              : ' WSV-ESCRITOS 02990015
              MOVE '02'  TO RETURN-CODE                                 03000015
              STOP RUN                                                  03010015
           END-IF                                                       03020015
           .                                                            03030015
      *                                                                 03040015
      ******************************************************************03050015
      *                   30000-FIN                                    *03060015
      ******************************************************************03070015
      *---------*                                                       03080015
       30000-FIN.                                                       03090015
      *---------*                                                       03100015
      *                                                                 03110015
           DISPLAY '*  REGISTROS LEIDOS     : ' WSV-LEIDOS              03120015
           DISPLAY '*  REGISTROS GRABADOS   : ' WSV-ESCRITOS            03130015
      *                                                                 03140015
           STOP RUN                                                     03150015
           .                                                            03160015
