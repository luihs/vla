      *-----------------------*                                         00010016
       IDENTIFICATION DIVISION.                                         00020016
      *-----------------------*                                         00030016
       PROGRAM-ID.   VL3CFTCZ.                                          00040016
      *AUTHOR.       EULER ALVARADO.                                    00050016
      ******************************************************************00060016
       ENVIRONMENT DIVISION.                                            00070016
       CONFIGURATION SECTION.                                           00080016
       INPUT-OUTPUT SECTION.                                            00090016
      *------------*                                                    00100016
       FILE-CONTROL.                                                    00110016
      *------------*                                                    00120016
            SELECT E1DQ9FTC ASSIGN TO E1DQ9FTC                          00130016
                   FILE STATUS IS FS-E1DQ9FTC                           00140016
                   ORGANIZATION IS SEQUENTIAL.                          00150016
                                                                        00160016
            SELECT S1DQ9FTC ASSIGN TO S1DQ9FTC                          00170016
                   FILE STATUS IS FS-S1DQ9FTC                           00180016
                   ORGANIZATION IS SEQUENTIAL.                          00190016
      *-----------------------------------------------------------------00200016
      *-------------*                                                   00210016
       DATA DIVISION.                                                   00220016
      *-------------*                                                   00230016
       FILE SECTION.                                                    00240016
                                                                        00250016
       FD  E1DQ9FTC                                                     00260016
           RECORDING MODE IS F                                          00270016
           BLOCK CONTAINS 0 RECORDS                                     00280016
           DATA RECORD IS REG-E1DQ9FTC.                                 00290016
       01  REG-E1DQ9FTC   PIC X(256).                                   00300016
                                                                        00310016
       FD  S1DQ9FTC                                                     00320016
           RECORDING MODE IS F                                          00330016
           BLOCK CONTAINS 0 RECORDS                                     00340016
           DATA RECORD IS REG-S1DQ9FTC.                                 00350016
       01  REG-S1DQ9FTC.                                                00360016
           10 S01-CTAVAL20         PIC X(20).                           00370016
           10 S01-FILLER1          PIC X(01).                           00380016
           10 S01-MONEDA           PIC X(03).                           00390016
           10 S01-FILLER2          PIC X(01).                           00400016
           10 S01-NUMCLI           PIC 9(08).                           00410016
           10 S01-FILLER3          PIC X(01).                           00420016
           10 S01-CLIENTE          PIC X(60).                           00430016
           10 S01-FILLER4          PIC X(01).                           00440016
           10 S01-SITUACION        PIC X(09).                           00450016
           10 S01-FILLER5          PIC X(01).                           00460016
           10 S01-FECALTA          PIC X(10).                           00470016
           10 S01-FILLER6          PIC X(01).                           00480016
           10 S01-FECCESE          PIC X(10).                           00490016
           10 S01-FILLER7          PIC X(01).                           00500016
           10 S01-RUT              PIC 9(08).                           00510016
      *                                                                 00520016
      *-----------------------------------------------------------------00530016
       WORKING-STORAGE SECTION.                                         00540016
      *-----------------------*                                         00550016
       77  WS-NAME                 PIC X(70) VALUE                      00560016
                                   '**  INICIO WORKING VL4C9MAE **'.    00570016
      ******************************************************************00580016
      *                                                                 00590016
       01  PE9C5201                PIC X(08) VALUE 'PE9C5201'.          00600016
RTP0   01  PE9C5000                PIC X(08) VALUE 'PE9C5000'.          00610016
       01  FILE-STATUS.                                                 00620016
           10 FS-E1DQ9FTC          PIC X(02) VALUE SPACES.              00630016
           10 FS-S1DQ9FTC          PIC X(02) VALUE SPACES.              00640016
       01  WSV-CLIENTE             PIC X(60) VALUE SPACES.              00650016
       01  WSV-FECHA-10-A          PIC X(10) VALUE SPACES.              00660016
       01  WSV-FECHA-8-N           PIC 9(08) VALUE ZEROS.               00670016
       01  WSV-FECHA-8-A REDEFINES WSV-FECHA-8-N PIC X(08).             00680016
                                                                        00690016
@RTP1  01  WS-FECHA-D.                                                  00700016
@RTP1      02  WS-F-AA-D   PIC 9999.                                    00710016
@RTP1      02  WS-F-MM-D   PIC 99.                                      00720016
@RTP1      02  WS-F-DD-D   PIC 99.                                      00730016
@RTP1  01  WS-RFECHA-D  REDEFINES WS-FECHA-D PIC 9(08).                 00740016
                                                                        00750016
       01  WSV-LEIDOS              PIC 9(08) VALUE ZEROS.               00760016
       01  WSV-ESCRITOS            PIC 9(08) VALUE ZEROS.               00770016
       01  W-DCLVLDTARC.                                                00780016
           10 WARC-CUENTA          PIC S9(7)V USAGE COMP-3.             00790016
           10 WARC-CENTAD          PIC S9(4)V USAGE COMP-3.             00800016
           10 WARC-NUMCLI          PIC S9(8)V USAGE COMP-3.             00810016
           10 WARC-CLMAST          PIC X(1).                            00820016
           10 WARC-MONEDA          PIC X(3).                            00830016
           10 WARC-SUCURS          PIC S9(4)V USAGE COMP-3.             00840016
           10 WARC-CTACAR          PIC S9(12)V USAGE COMP-3.            00850016
           10 WARC-CTAABO          PIC S9(12)V USAGE COMP-3.            00860016
           10 WARC-TEXTO           PIC X(1).                            00870016
           10 WARC-PRESEN          PIC S9(5)V USAGE COMP-3.             00880016
           10 WARC-GRUPO           PIC S9(4)V USAGE COMP-3.             00890016
           10 WARC-RUT             PIC S9(8)V USAGE COMP-3.             00900016
           10 WARC-CNAE            PIC S9(4)V USAGE COMP-3.             00910016
           10 WARC-SITUAC          PIC X(1).                            00920016
           10 WARC-EXEN1           PIC S9(3)V USAGE COMP-3.             00930016
           10 WARC-EXEN2           PIC S9(3)V USAGE COMP-3.             00940016
           10 WARC-EXEN3           PIC S9(3)V USAGE COMP-3.             00950016
           10 WARC-EXEN4           PIC S9(3)V USAGE COMP-3.             00960016
           10 WARC-EXEN5           PIC S9(3)V USAGE COMP-3.             00970016
           10 WARC-EXEN6           PIC S9(3)V USAGE COMP-3.             00980016
           10 WARC-EXEN7           PIC S9(3)V USAGE COMP-3.             00990016
           10 WARC-EXEN8           PIC S9(3)V USAGE COMP-3.             01000016
           10 WARC-EXEN9           PIC S9(3)V USAGE COMP-3.             01010016
           10 WARC-EXEN10          PIC S9(3)V USAGE COMP-3.             01020016
           10 WARC-ANALIS          PIC X(1).                            01030016
           10 WARC-CLACARGO        PIC X(1).                            01040016
           10 WARC-CLABONO         PIC X(1).                            01050016
           10 WARC-NUMDOM          PIC S9(3)V USAGE COMP-3.             01060016
           10 WARC-CODSUS          PIC X(4).                            01070016
           10 WARC-FE-ULT-EXT      PIC S9(8)V USAGE COMP-3.             01080016
           10 WARC-PAIS            PIC X(4).                            01090016
           10 WARC-FE-CARTERA      PIC S9(8)V USAGE COMP-3.             01100016
           10 WARC-CLTELEX         PIC X(10).                           01110016
           10 WARC-FE-ALTA         PIC S9(8)V USAGE COMP-3.             01120016
           10 WARC-VALORACION      PIC X(1).                            01130016
           10 WARC-VALEXTRJ        PIC X(1).                            01140016
           10 WARC-INVERSOR        PIC S9(2)V USAGE COMP-3.             01150016
           10 WARC-DIRECTA         PIC X(1).                            01160016
           10 WARC-MAX-CVE-1       PIC S9(7)V USAGE COMP-3.             01170016
           10 WARC-MAX-DCU-5       PIC S9(7)V USAGE COMP-3.             01180016
           10 WARC-MAX-SUS-6       PIC S9(7)V USAGE COMP-3.             01190016
           10 WARC-MAX-DIV-7       PIC S9(7)V USAGE COMP-3.             01200016
           10 WARC-MAX-AMO-8       PIC S9(7)V USAGE COMP-3.             01210016
           10 WARC-MAX-PAJ-9       PIC S9(7)V USAGE COMP-3.             01220016
           10 WARC-FECHA-102       PIC S9(8)V USAGE COMP-3.             01230016
           10 WARC-TARIFACUS       PIC S9(1)V USAGE COMP-3.             01240016
           10 WARC-SWIFT-TELEX     PIC X(1).                            01250016
           10 WARC-TELEX-2         PIC X(2).                            01260016
           10 WARC-GRUPO-CTAS      PIC S9(3)V USAGE COMP-3.             01270016
           10 WARC-OPER-TIT        PIC X(1).                            01280016
           10 WARC-FEALTREG        PIC S9(8)V USAGE COMP-3.             01290016
           10 WARC-FEULMOD         PIC S9(8)V USAGE COMP-3.             01300016
           10 WARC-HORULMOD        PIC S9(6)V USAGE COMP-3.             01310016
           10 WARC-NUMTER          PIC X(4).                            01320016
           10 WARC-USUARIO         PIC X(7).                            01330016
           10 WARC-FILLER          PIC X(60).                           01340016
           10 WARC-CTAVAL20        PIC X(20).                           01350016
           10 WARC-NUMMAN          PIC S9(1)V USAGE COMP-3.             01360016
           10 WARC-INDIMP          PIC X(1).                            01370016
           10 WARC-INDSAB          PIC X(1).                            01380016
      *                                                                 01390016
      *    BD PERSONAS                                                  01400016
       01  W-PEWC5201.                                                  01410016
           COPY PEWC5201.                                               01420016
      *                                                                 01430016
RTP0   01 PEWC5000.                                                     01440016
RTP0       COPY PEWC5000.                                               01450016
      *                                                                 01460016
      *---------------*                                                 01470016
       LINKAGE SECTION.                                                 01480016
      *---------------*                                                 01490016
@RTP0  01  LK-PARAMETROS.                                               01500016
@RTP0      02  LK-LONGITUD     PIC S9(4)   COMP.                        01510016
@RTP0      02  LK-FECHA-D.                                              01520016
@RTP0          03  LK-F-AA-D   PIC 9999.                                01530016
@RTP0          03  LK-F-MM-D   PIC 99.                                  01540016
@RTP0          03  LK-F-DD-D   PIC 99.                                  01550016
@RTP0      02  LK-RFECHA-D  REDEFINES LK-FECHA-D PIC 9(08).             01560016
@RTP0      02  LK-FECHA-H.                                              01570016
@RTP0          03  LK-F-AA-H   PIC 9999.                                01580016
@RTP0          03  LK-F-MM-H   PIC 99.                                  01590016
@RTP0          03  LK-F-DD-H   PIC 99.                                  01600016
@RTP0      02  LK-RFECHA-H  REDEFINES LK-FECHA-H PIC 9(08).             01610016
@RTP0      02  LK-PROCESO      PIC 9(01).                               01620016
      *                                                                 01630016
      *---------------------------------------*                         01640016
@RTP0  PROCEDURE DIVISION USING LK-PARAMETROS.                          01650016
      *---------------------------------------*                         01660016
      *                                                                 01670016
           PERFORM 10000-INICIO.                                        01680016
      *                                                                 01690016
           PERFORM 20000-PROCESO UNTIL FS-E1DQ9FTC = '10'.              01700016
      *                                                                 01710016
           PERFORM 30000-FIN.                                           01720016
      *                                                                 01730016
           STOP RUN.                                                    01740016
      *                                                                 01750016
      ******************************************************************01760016
      *                       1-INICIO                                 *01770016
      *       INICIALIZA LAS WORKAS DE LAS TABLAS Y LOS CAMPOS DE      *01780016
      *       TRABAJO. LEE LA FECHA EN LA TABLA UGDTPRC, TOMANDO LA    *01790016
      *       FILA CON CODIGO DE PROCESO = 'UB00' Y COMPRUEBA QUE      *01800016
      *       EL PROCESO ESTA ACTIVO.                                  *01810016
      ******************************************************************01820016
       10000-INICIO.                                                    01830016
      *-------------*                                                   01840016
      *                                                                 01850016
           OPEN INPUT  E1DQ9FTC                                         01860016
                OUTPUT S1DQ9FTC                                         01870016
                                                                        01880016
           IF (FS-E1DQ9FTC EQUAL '00' OR '97')                          01890016
              CONTINUE                                                  01900016
           ELSE                                                         01910016
              DISPLAY '***********************************'             01920016
              DISPLAY '*  ERROR AL OPEN DE ENTRADA1      *'             01930016
              DISPLAY '*  ERROR FS-OPS ES :' FS-E1DQ9FTC                01940016
              DISPLAY '***********************************'             01950016
              MOVE '02'  TO RETURN-CODE                                 01960016
              STOP RUN                                                  01970016
           END-IF                                                       01980016
                                                                        01990016
           IF (FS-S1DQ9FTC EQUAL '00' OR '97')                          02000016
              CONTINUE                                                  02010016
           ELSE                                                         02020016
              DISPLAY '***********************************'             02030016
              DISPLAY '*  ERROR AL OPEN DE SALIDA1       *'             02040016
              DISPLAY '*  ERROR FS-OPS ES :' FS-S1DQ9FTC                02050016
              DISPLAY '***********************************'             02060016
              MOVE '02'  TO RETURN-CODE                                 02070016
              STOP RUN                                                  02080016
           END-IF                                                       02090016
                                                                        02100016
           PERFORM 10010-LEER-ENTRADA                                   02110016
           .                                                            02120016
      *                                                                 02130016
      *------------------*                                              02140016
       10010-LEER-ENTRADA.                                              02150016
      *------------------*                                              02160016
      *                                                                 02170016
           READ E1DQ9FTC                                                02180016
                                                                        02190016
           EVALUATE FS-E1DQ9FTC                                         02200016
              WHEN '00'                                                 02210016
                   ADD  1                      TO WSV-LEIDOS            02220016
                   MOVE REG-E1DQ9FTC           TO W-DCLVLDTARC          02230016
              WHEN '10'                                                 02240016
                   CONTINUE                                             02250016
              WHEN OTHER                                                02260016
                   DISPLAY '***********************************'        02270016
                   DISPLAY '*  ERROR AL LEER ENTRADA          *'        02280016
                   DISPLAY '*  ERROR FS-OPS ES :' FS-E1DQ9FTC           02290016
                   DISPLAY '***********************************'        02300016
                   MOVE '02'  TO RETURN-CODE                            02310016
                   STOP RUN                                             02320016
           END-EVALUATE                                                 02330016
           .                                                            02340016
      *                                                                 02350016
      *     *-------*                                                   02360016
       20000-PROCESO.                                                   02370016
      *     *-------*                                                   02380016
      *                                                                 02390016
           INITIALIZE REG-S1DQ9FTC                                      02400016
      *                                                                 02410016
           MOVE '|'                      TO S01-FILLER1  S01-FILLER2    02420016
                                            S01-FILLER3  S01-FILLER4    02430016
                                            S01-FILLER5  S01-FILLER6    02440016
                                            S01-FILLER7                 02450016
      *                                                                 02460016
           MOVE WARC-CTAVAL20            TO S01-CTAVAL20                02470016
           MOVE WARC-MONEDA              TO S01-MONEDA                  02480016
           MOVE WARC-NUMCLI              TO S01-NUMCLI                  02490016
           MOVE WARC-RUT                 TO S01-RUT                     02500016
                                                                        02510016
RTP0  *    PERFORM 20010-OBTIENE-CLIENTE                                02520016
RTP0       PERFORM 220-RUTINA-PERSONA                                   02530016
           MOVE WSV-CLIENTE              TO S01-CLIENTE                 02540016
                                                                        02550016
           IF WARC-SITUAC = 'A'                                         02560016
              MOVE 'ACTIVA   '           TO S01-SITUACION               02570016
           ELSE                                                         02580016
              MOVE 'CANCELADA'           TO S01-SITUACION               02590016
           END-IF                                                       02600016
                                                                        02610016
           MOVE WARC-FEALTREG            TO WSV-FECHA-8-N               02620016
           MOVE WSV-FECHA-8-A (07:02)    TO S01-FECALTA (01:02)         02630016
           MOVE '/'                      TO S01-FECALTA (03:01)         02640016
           MOVE WSV-FECHA-8-A (05:02)    TO S01-FECALTA (04:02)         02650016
           MOVE '/'                      TO S01-FECALTA (06:01)         02660016
           MOVE WSV-FECHA-8-A (01:04)    TO S01-FECALTA (07:04)         02670016
                                                                        02680016
           IF WARC-SITUAC = 'A'                                         02690016
              MOVE SPACES                TO S01-FECCESE                 02700016
           ELSE                                                         02710016
              MOVE WARC-FEULMOD             TO WSV-FECHA-8-N            02720016
              MOVE WSV-FECHA-8-A (07:02)    TO S01-FECCESE (01:02)      02730016
              MOVE '/'                      TO S01-FECCESE (03:01)      02740016
              MOVE WSV-FECHA-8-A (05:02)    TO S01-FECCESE (04:02)      02750016
              MOVE '/'                      TO S01-FECCESE (06:01)      02760016
              MOVE WSV-FECHA-8-A (01:04)    TO S01-FECCESE (07:04)      02770016
           END-IF                                                       02780016
                                                                        02790016
@RTP1      IF WARC-SITUAC = 'A'                                         02800016
@RTP1 *       GRABA ACTIVOS                                             02810016
@RTP1         PERFORM 20020-GRABA-SALIDA                                02820016
@RTP1      ELSE                                                         02830016
@RTP1 *       GRABA CANCELADOS                                          02840016
@RTP1 *       SI EL PROCESO ES DIARIO                                   02850016
@RTP1         IF LK-PROCESO = 1 AND LK-FECHA-D = WSV-FECHA-8-N          02860016
@RTP1            PERFORM 20020-GRABA-SALIDA                             02870016
@RTP1         END-IF                                                    02880016
@RTP1 *       SI EL PROCESO ES MENSUAL                                  02890016
@RTP1         IF LK-PROCESO = 2                                         02900016
@RTP1            MOVE WSV-FECHA-8-N TO WS-FECHA-D                       02910016
@RTP1            IF LK-F-AA-D = WS-F-AA-D AND LK-F-MM-D = WS-F-MM-D     02920016
@RTP1               PERFORM 20020-GRABA-SALIDA                          02930016
@RTP1            END-IF                                                 02940016
@RTP1         END-IF                                                    02950016
@RTP1 *       SI EL PROCESO ES TOTAL                                    02960016
@RTP1         IF LK-PROCESO = 3                                         02970016
@RTP1            PERFORM 20020-GRABA-SALIDA                             02980016
@RTP1         END-IF                                                    02990016
           END-IF                                                       03000016
                                                                        03010016
           PERFORM 10010-LEER-ENTRADA                                   03020016
           .                                                            03030016
      *                                                                 03040016
      *     *---------------*                                           03050016
       20010-OBTIENE-CLIENTE.                                           03060016
      *     *---------------*                                           03070016
      *                                                                 03080016
           INITIALIZE           W520-REGISTRO                           03090016
      *                                                                 03100016
           MOVE WARC-NUMCLI        TO W520-NUMCLIEN                     03110016
           MOVE SPACES             TO WSV-CLIENTE                       03120016
                                                                        03130016
           CALL PE9C5201 USING W-PEWC5201                               03140016
                                                                        03150016
           EVALUATE W520-PECRETOR                                       03160016
              WHEN ZEROS                                                03170016
                  IF W520-SUJGRUP = 'F'                                 03180016
                     STRING W520-PRIAPE DELIMITED BY '  ' ' '           03190016
                            W520-SEGAPE DELIMITED BY '  ' ' '           03200016
                            W520-NOMBRE DELIMITED BY '  '               03210016
                                              INTO WSV-CLIENTE          03220016
                  ELSE                                                  03230016
                     STRING W520-NOMBRE DELIMITED BY SIZE               03240016
                            W520-PRIAPE DELIMITED BY SIZE               03250016
                            W520-SEGAPE DELIMITED BY SIZE               03260016
                                              INTO WSV-CLIENTE          03270016
                  END-IF                                                03280016
              WHEN OTHER                                                03290016
                   MOVE '***NO UBICADO***'          TO  WSV-CLIENTE     03300016
           END-EVALUATE                                                 03310016
           .                                                            03320016
      *                                                                 03330016
       220-RUTINA-PERSONA.                                              03340016
      *-------------------*                                             03350016
      *                                                                 03360016
           INITIALIZE W500-REGISTRO                                     03370016
           MOVE SPACES               TO WSV-CLIENTE                     03380016
                                                                        03390016
           MOVE WARC-CTAVAL20(13:08) TO  W500-NUMECTA                   03400016
           MOVE WARC-CTAVAL20(01:04) TO  W500-PECENTID                  03410016
           MOVE WARC-CTAVAL20(05:04) TO  W500-OFIAPE                    03420016
           MOVE WARC-CTAVAL20(11:02) TO  W500-CODISER                   03430016
           MOVE 'T'                  TO  W500-CLAINTER                  03440016
           MOVE '01'                 TO  W500-SECINTER                  03450016
           MOVE 'U'                  TO  W500-PEYSELEC                  03460016
                                                                        03470016
           CALL PE9C5000 USING PEWC5000                                 03480016
                                                                        03490016
           IF W500-PECRETOR = '00'                                      03500016
              EVALUATE W500-SUJGRUP(1)                                  03510016
              WHEN 'F'                                                  03520016
                   STRING W500-PRIAPE(1) DELIMITED BY '  ' ' '          03530016
                          W500-SEGAPE(1) DELIMITED BY '  ' ' '          03540016
                          W500-NOMBRE(1) DELIMITED BY '  '              03550016
                          INTO WSV-CLIENTE                              03560016
                                                                        03570016
              WHEN 'M'                                                  03580016
                   STRING W500-NOMBRE(1) DELIMITED BY SIZE              03590016
                          W500-PRIAPE(1) DELIMITED BY SIZE              03600016
                          W500-SEGAPE(1) DELIMITED BY SIZE              03610016
                          INTO WSV-CLIENTE                              03620016
              END-EVALUATE                                              03630016
           END-IF                                                       03640016
           .                                                            03650016
                                                                        03660016
      *     *------------*                                              03670016
       20020-GRABA-SALIDA.                                              03680016
      *     *------------*                                              03690016
      *                                                                 03700016
           WRITE REG-S1DQ9FTC                                           03710016
                                                                        03720016
           IF (FS-S1DQ9FTC EQUAL '00')                                  03730016
               ADD 1                   TO WSV-ESCRITOS                  03740016
           ELSE                                                         03750016
              DISPLAY '*  ERROR EN GRABAR REGISTROS FS : ' FS-S1DQ9FTC  03760016
              DISPLAY '*  REGISTRO LEIDOS              : ' WSV-ESCRITOS 03770016
              MOVE '02'  TO RETURN-CODE                                 03780016
              STOP RUN                                                  03790016
           END-IF                                                       03800016
           .                                                            03810016
      *                                                                 03820016
      ******************************************************************03830016
      *                   30000-FIN                                    *03840016
      ******************************************************************03850016
      *---------*                                                       03860016
       30000-FIN.                                                       03870016
      *---------*                                                       03880016
      *                                                                 03890016
           DISPLAY '*  REGISTROS LEIDOS     : ' WSV-LEIDOS              03900016
           DISPLAY '*  REGISTROS GRABADOS   : ' WSV-ESCRITOS            03910016
      *                                                                 03920016
           STOP RUN                                                     03930016
           .                                                            03940016
