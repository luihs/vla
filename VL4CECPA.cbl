       IDENTIFICATION DIVISION.                                         00010081
       PROGRAM-ID.    VL4CECPA.                                         00020083
       AUTHOR.        BBVA.                                             00030081
      ******************************************************************00040081
      * SISTEMA : VALORES                                              *00050081
      * FUNCION : CRUZAR ARCHIVOS DE EECC PARA CORREGIR TABLA VLDTHIS  *00060083
      * FECHA   : 23-11-2021                                           *00070083
      ******************************************************************00080081
      ******************************************************************00090081
      *PETIT/ SA  FECHA-MOD. PROGRAMADOR      DESCRIPCION              *00100081
      *---------- ---------- ---------------- -------------------------*00110081
      *RITM46847  12-1O-2021 LUIS RIVERA H.   CREACION DEL COMPONENTE  *00120081
      *---------- ---------- ---------------- -------------------------*00130081
      ******************************************************************00140081
       ENVIRONMENT DIVISION.                                            00150081
       CONFIGURATION SECTION.                                           00160081
       SPECIAL-NAMES.                                                   00170081
       INPUT-OUTPUT SECTION.                                            00180081
       FILE-CONTROL.                                                    00190081
                                                                        00200081
           SELECT E1VDTCOM  ASSIGN       TO E1VDTCOM                    00210081
                            FILE STATUS  IS FS-VLDTCOM.                 00220081
                                                                        00230081
           SELECT E2VDTARC  ASSIGN       TO E2VDTARC                    00240081
                            FILE STATUS  IS FS-VLDTARC.                 00250081
                                                                        00260081
           SELECT S1RNEGBL  ASSIGN       TO S1RNEGBL                    00270081
                            FILE STATUS  IS FS-SNEGBOL.                 00280081
       DATA DIVISION.                                                   00290081
       FILE SECTION.                                                    00300081
                                                                        00310081
       FD  E1VDTCOM                                                     00320081
           RECORDING MODE  IS  F                                        00330081
           LABEL  RECORDS  IS  STANDARD                                 00340081
           DATA RECORD     IS  REG-VLDTCOM.                             00350081
         01  REG-VLDTCOM           PIC X(124).                          00360081
      *                                                                *00370081
       FD  E2VDTARC                                                     00380081
           RECORDING MODE  IS  F                                        00390081
           LABEL  RECORDS  IS  STANDARD                                 00400081
           DATA RECORD     IS  REG-VLDTARC.                             00410081
         01  REG-VLDTARC           PIC X(256).                          00420081
      *                                                                *00430081
       FD  S1RNEGBL                                                     00440081
           RECORDING MODE  IS  F                                        00450081
           LABEL  RECORDS  IS  STANDARD                                 00460081
           DATA   RECORD   IS  SAL-NEG-BOLSA.                           00470081
         01 SAL-NEG-BOLSA          PIC X(270).                          00480081
      *                                                                 00490081
      ******************************************************************00500081
      **       W O R K I N G - S T O R A G E      S E C T I O N       **00510081
      ******************************************************************00520081
       WORKING-STORAGE SECTION.                                         00530081
      *************************                                         00540081
      * VARIALES GENERALES                                              00550081
       01  WS-GENERALES.                                                00560081
           05 NUMCLI-VLDTARC  PIC 9(8).                                 00570081
           05 CLI-IRELPAT     PIC X(1) VALUE SPACES.                    00580081
           05 CLI-SUJGRUP     PIC X(1) VALUE SPACES.                    00590081
       01  NOMBRE-CLIE.                                                 00600081
           05 CLIE-NOMBRES    PIC X(20) VALUE SPACES.                   00610081
           05 ESPACIO         PIC X(01) VALUE SPACES.                   00620081
           05 CLIE-APE-PAT    PIC X(20) VALUE SPACES.                   00630081
           05 ESPACIO         PIC X(01) VALUE SPACES.                   00640081
           05 CLIE-APE-MAT    PIC X(20) VALUE SPACES.                   00650081
       01  CORRE-FIJO.                                                  00660081
           05 CORRE-FIJO12    PIC X(12) VALUE SPACES.                   00670081
           05 FILLER          PIC X(1)  VALUE '.'.                      00680081
           05 CORRE-FIJO2     PIC X(2)  VALUE SPACES.                   00690081
       01  SALIDA-COMP.                                                 00700081
           05 SAL-SITUAC          PIC X(1).                             00710081
           05 FILLER              PIC X(1) VALUE '|'.                   00720081
           05 SAL-CUENTA          PIC 9(6).                             00730081
           05 FILLER              PIC X(1) VALUE '|'.                   00740081
           05 SAL-NUMCLI          PIC 9(6).                             00750081
           05 FILLER              PIC X(1) VALUE '|'.                   00760081
           05 SAL-CLACONT         PIC 9(2).                             00770081
           05 FILLER              PIC X(1) VALUE '|'.                   00780081
           05 SAL-TIPTAR          PIC X(1).                             00790081
           05 FILLER              PIC X(1) VALUE '|'.                   00800081
           05 SAL-DINIVAL         PIC X(10).                            00810081
           05 FILLER              PIC X(1) VALUE '|'.                   00820081
           05 SAL-DFINVAL         PIC X(10).                            00830081
           05 FILLER              PIC X(1) VALUE '|'.                   00840081
           05 SAL-CORRE-FIJO      PIC 9(12)V9(2)  VALUE ZEROES.         00850081
           05 FILLER              PIC X(1) VALUE '|'.                   00860081
           05 SAL-CORRE-PORCEN    PIC 9(03)V9(6)  VALUE ZEROES.         00870081
           05 FILLER              PIC X(1) VALUE '|'.                   00880081
@RTP  *    05 SAL-CORRE-MINIMO    PIC 9(15).                            00890081
@RTP       05 SAL-CORRE-MINIMO    PIC 9(12)V9(2)  VALUE ZEROES.         00900081
           05 FILLER              PIC X(1) VALUE '|'.                   00910081
@RTP  *    05 SAL-CORRE-MAXIMO    PIC 9(15).                            00940081
@RTP       05 SAL-CORRE-MAXIMO    PIC 9(12)V9(2)  VALUE ZEROES.         00950081
           05 FILLER              PIC X(1) VALUE '|'.                   00980081
           05 SAL-CTERMIN         PIC X(4).                             00990081
           05 FILLER              PIC X(1) VALUE '|'.                   01000081
           05 SAL-DCRIACAO        PIC X(26).                            01010081
           05 FILLER              PIC X(1) VALUE '|'.                   01020081
           05 SAL-CUSRCRI         PIC X(7).                             01030081
           05 FILLER              PIC X(1) VALUE '|'.                   01040081
           05 SAL-DMODIF          PIC X(26).                            01050081
           05 FILLER              PIC X(1) VALUE '|'.                   01060081
           05 SAL-CUSRMOD         PIC X(7).                             01070081
           05 FILLER              PIC X(1) VALUE '|'.                   01080081
           05 SAL-IRELPAT         PIC X(1).                             01090081
           05 FILLER              PIC X(1) VALUE '|'.                   01100081
           05 SAL-NOMBRE          PIC X(62).                            01110081
           05 FILLER              PIC X(1) VALUE '|'.                   01120081
           05 SAL-SUJGRUP         PIC X(1).                             01130081
       77  W-PROGRAMA         PIC X(08) VALUE 'VL4C7067'.               01140081
       77  PE9C5201           PIC X(08) VALUE 'PE9C5201'.               01150081
       77 WW-EDITADO-12-02     PIC 9(12)V9(2)  VALUE ZEROES.            01160081
                                                                        01170081
       77 WK-EDITMIN-12-02     PIC 9(12)V9(2).                          01180081
       77 WK-EDITMAX-12-02     PIC 9(12)V9(2).                          01190081
       77 WK-MINIMO-10-04      PIC 9(10)V9(4).                          01200081
       77 WK-MAXIMO-10-04      PIC 9(10)V9(4).                          01210081
       77 WK-EDITMAN-12-02     PIC 9(12)V9(2).                          01220081
       77 WW-EDITADO-02-06     PIC 9(3)V9(6)   VALUE ZEROES.            01230081
       77 WW-COMSAB-AUX        PIC 9(12)V9(2)  VALUE ZEROES.            01240081
       01 W-CONTADORES.                                                 01250081
          05 CONT-LEIDO-VLDTCOM    PIC 9(10)  VALUE ZEROES.             01260081
          05 CONT-LEIDO-VLDTARC    PIC 9(10)  VALUE ZEROES.             01270081
          05 REG-IMPRESO           PIC 9(10)  VALUE ZEROES.             01280081
      * VARIABLES PARA CONTROL DE ERRORES                               01290081
       01 WS-ERROR.                                                     01300081
          05 WS-ACCION             PIC X(22).                           01310081
          05 WS-PARRAFO            PIC X(19).                           01320081
      * VARIABLES CONTROL DE ARCHIVOS - ESCRITURA                       01330081
       01 WS-CTRL-ARCHIVOS.                                             01340081
          05 FS-VLDTCOM            PIC X(02) VALUE '00'.                01350081
             88 E1VDTCOM-OK                  VALUE '00'.                01360081
             88 E1VDTCOM-NOK                 VALUE '10'.                01370081
          05 FS-VLDTARC            PIC X(02) VALUE '00'.                01380081
             88 E2VDTARC-OK                  VALUE '00'.                01390081
             88 E2VDTARC-NOK                 VALUE '10'.                01400081
          05 FS-SNEGBOL            PIC X(02) VALUE '00'.                01410081
             88 S1RNEGBL-OK                  VALUE '00'.                01420081
             88 S1RNEGBL-NOK                 VALUE '10'.                01430081
          05 FIN-FILE-VLDTCOM      PIC X(02).                           01440081
             88 FIN-VLDTCOM                   VALUE '00'.               01450081
             88 FIN-NO-VLDTCOM                VALUE '10'.               01460081
          05 FIN-FILE-VLDTARC      PIC X(02).                           01470081
             88 FIN-VLDTARC                   VALUE '00'.               01480081
             88 FIN-NO-VLDTARC                VALUE '10'.               01490081
          05 FS-WRITE              PIC X(02).                           01500081
             88 WRITE-OK                      VALUE '00'.               01510081
             88 WRITE-NOK                     VALUE '10'.               01520081
      *                                                                *01530081
      * LLAVES                                                          01540081
       01 WS-KEYS-1.                                                    01550081
          05 KEY-COM-VLDTCOM      PIC 9(6).                             01560081
          05 KEY-COM-VLDTARC      PIC 9(6).                             01570081
      *                                                                 01580081
      *  AREA DE COMUNICACION SQLCA                                    *01590081
           EXEC SQL INCLUDE SQLCA END-EXEC.                             01600081
      *                                                                 01610081
      *  AREA DE DCLGEN'S DE TABLAS DB2                                *01620081
           EXEC SQL INCLUDE VLGTARC  END-EXEC.                          01630081
           EXEC SQL INCLUDE VLGTCOM  END-EXEC.                          01640081
      *                                                                 01650081
      * DATOS DE CLIENTE - RUTINA PERSONAS                             *01660081
       01 PEWC5201.                                                     01670081
          COPY PEWC5201.                                                01680081
      *                                                                *01690081
       PROCEDURE DIVISION.                                              01700081
      *                                                                *01710081
           PERFORM 1000-INICIO                                          01720081
              THRU 1000-INICIO-EXIT.                                    01730081
                                                                        01740081
           PERFORM 2000-PROCESO                                         01750081
              UNTIL FIN-VLDTCOM OR FIN-VLDTARC.                         01760081
                                                                        01770081
           PERFORM 3000-FIN                                             01780081
              THRU 3000-FIN-EXIT.                                       01790081
                                                                        01800081
           STOP RUN.                                                    01810081
      *                                                                *01820081
      *-----------*                                                     01830081
       1000-INICIO.                                                     01840081
      *-----------*                                                     01850081
           INITIALIZE W-CONTADORES.                                     01860081
                                                                        01870081
           PERFORM 1100-ABRIR-FICHEROS                                  01880081
              THRU 1100-ABRIR-FICHEROS-EXIT.                            01890081
                                                                        01900081
           PERFORM 1200-LEE-VLDTCOM.                                    01910081
                                                                        01920081
           PERFORM 1300-LEE-VLDTARC.                                    01930081
      *                                                                *01940081
      *----------------*                                                01950081
       1000-INICIO-EXIT.                                                01960081
      *----------------*                                                01970081
           EXIT.                                                        01980081
                                                                        01990081
      *-------------------*                                             02000081
       1100-ABRIR-FICHEROS.                                             02010081
      *-------------------*                                             02020081
           OPEN INPUT E1VDTCOM.                                         02030081
           IF NOT E1VDTCOM-OK                                           02040081
              MOVE 'OPEN FICHERO E1VDTCOM' TO WS-ACCION                 02050081
              MOVE '1200-ABRIR-FICHEROS'   TO WS-PARRAFO                02060081
              PERFORM 3001-ERROR                                        02070081
           END-IF                                                       02080081
      *                                                                 02090081
           OPEN INPUT E2VDTARC.                                         02100081
           IF NOT E2VDTARC-OK                                           02110081
              MOVE 'OPEN FICHERO E2VDTARC' TO WS-ACCION                 02120081
              MOVE '1200-ABRIR-FICHEROS'   TO WS-PARRAFO                02130081
              PERFORM 3001-ERROR                                        02140081
           END-IF                                                       02150081
      *                                                                 02160081
           OPEN OUTPUT S1RNEGBL.                                        02170081
           IF NOT S1RNEGBL-OK                                           02180081
              MOVE 'OPEN FICHERO E2VDTARC' TO WS-ACCION                 02190081
              MOVE '1200-ABRIR-FICHEROS'   TO WS-PARRAFO                02200081
              PERFORM 3001-ERROR                                        02210081
           END-IF.                                                      02220081
      *                                                                *02230081
      *------------------------*                                        02240081
       1100-ABRIR-FICHEROS-EXIT.                                        02250081
      *------------------------*                                        02260081
           EXIT.                                                        02270081
      *                                                                 02280081
      *----------------*                                                02290081
       1200-LEE-VLDTCOM.                                                02300081
      *----------------*                                                02310081
           READ E1VDTCOM                                                02320081
           AT END                                                       02330081
              SET FIN-VLDTCOM TO TRUE                                   02340081
           NOT AT END                                                   02350081
              MOVE REG-VLDTCOM TO DCLVLDTCOM                            02360081
              ADD 1 TO CONT-LEIDO-VLDTCOM                               02370081
              MOVE VCOM-CUENTA TO KEY-COM-VLDTCOM                       02380081
           END-READ.                                                    02390081
      *                                                                 02400081
      *----------------*                                                02410081
       1300-LEE-VLDTARC.                                                02420081
      *----------------*                                                02430081
           READ E2VDTARC                                                02440081
           AT END                                                       02450081
              SET FIN-VLDTARC TO TRUE                                   02460081
           NOT AT END                                                   02470081
              MOVE REG-VLDTARC TO DCLVLDTARC                            02480081
              ADD 1 TO CONT-LEIDO-VLDTARC                               02490081
              MOVE VARC-CUENTA TO KEY-COM-VLDTARC                       02500081
           END-READ.                                                    02510081
      *                                                                 02520081
      *------------*                                                    02530081
       2000-PROCESO.                                                    02540081
      *------------*                                                    02550081
           IF KEY-COM-VLDTCOM = KEY-COM-VLDTARC                         02560081
              MOVE VARC-NUMCLI TO NUMCLI-VLDTARC                        02570081
              PERFORM 2100-OBTIENE-PERSONA                              02580081
              PERFORM 2200-MUEVE-DATOS                                  02590081
              PERFORM 2300-IMPRIME-REPORTE                              02600081
              PERFORM 1300-LEE-VLDTARC                                  02610081
           ELSE                                                         02620081
              IF KEY-COM-VLDTCOM < KEY-COM-VLDTARC THEN                 02630081
                 PERFORM 1200-LEE-VLDTCOM                               02640081
              ELSE                                                      02650081
                 PERFORM 1300-LEE-VLDTARC                               02660081
              END-IF                                                    02670081
           END-IF.                                                      02680081
      *                                                                *02690081
      *-----------------*                                               02700081
       2000-PROCESO-EXIT.                                               02710081
      *-----------------*                                               02720081
           EXIT.                                                        02730081
      *                                                                 02740081
       2200-MUEVE-DATOS.                                                02750081
      *-----------------*                                               02760081
           MOVE VCOM-SITUAC          TO SAL-SITUAC                      02770081
           MOVE VCOM-CUENTA          TO SAL-CUENTA                      02780081
           MOVE NUMCLI-VLDTARC       TO SAL-NUMCLI                      02790081
           MOVE VCOM-CLACONT         TO SAL-CLACONT                     02800081
           MOVE VCOM-TIPTAR          TO SAL-TIPTAR                      02810081
           MOVE VCOM-DINIVAL         TO SAL-DINIVAL                     02820081
           MOVE VCOM-DFINVAL         TO SAL-DFINVAL                     02830081
                                                                        02840081
           INITIALIZE WW-EDITADO-12-02                                  02850081
           MOVE VCOM-CORRE-FIJO      TO WW-EDITADO-12-02                02860081
           MOVE WW-EDITADO-12-02     TO SAL-CORRE-FIJO                  02870081
                                                                        02880081
           INITIALIZE WW-EDITADO-02-06                                  02890081
           MOVE VCOM-CORRE-PORCEN    TO WW-EDITADO-02-06                02900081
           MOVE WW-EDITADO-02-06     TO SAL-CORRE-PORCEN                02910081
                                                                        02920081
           INITIALIZE WW-EDITADO-12-02                                  02930081
@RTP       MOVE VCOM-CORRE-MINIMO    TO WW-EDITADO-12-02                02940081
@RTP       MOVE WW-EDITADO-12-02     TO SAL-CORRE-MINIMO                02950081
@RTP  *    DISPLAY 'SAL-CORRE-MINIMO  '    SAL-CORRE-MINIMO             02960081
                                                                        02970081
@RTP  *    MOVE VCOM-CORRE-MINIMO    TO WK-EDITMIN-12-02                02980081
@RTP  *    MOVE WK-EDITMIN-12-02     TO SAL-CORRE-MINIMO                02990081
      *    MOVE VCOM-CORRE-MINIMO    TO WK-MINIMO-10-04                 03000081
      *    MOVE WK-MINIMO-10-04      TO SAL-CORRE-MINIMO2               03010081
      *    MOVE VCOM-CORRE-MAXIMO    TO WK-MAXIMO-10-04                 03020081
      *    MOVE WK-MAXIMO-10-04      TO SAL-CORRE-MAXIMO2               03030081
                                                                        03040081
           INITIALIZE WW-COMSAB-AUX                                     03050081
@RTP       MOVE VCOM-CORRE-MAXIMO    TO WW-COMSAB-AUX                   03060081
@RTP       MOVE WW-COMSAB-AUX        TO SAL-CORRE-MAXIMO                03070081
                                                                        03080081
@RTP  *    MOVE VCOM-CORRE-MAXIMO    TO WK-EDITMAX-12-02                03090081
@RTP  *    MOVE WK-EDITMAX-12-02     TO SAL-CORRE-MAXIMO                03100081
           MOVE VCOM-CTERMIN         TO SAL-CTERMIN                     03110081
           MOVE VCOM-DCRIACAO        TO SAL-DCRIACAO                    03120081
           MOVE VCOM-CUSRCRI         TO SAL-CUSRCRI                     03130081
           MOVE VCOM-DMODIF          TO SAL-DMODIF                      03140081
           MOVE VCOM-CUSRMOD         TO SAL-CUSRMOD                     03150081
           MOVE CLI-IRELPAT          TO SAL-IRELPAT                     03160081
           MOVE NOMBRE-CLIE          TO SAL-NOMBRE                      03170081
           MOVE CLI-SUJGRUP          TO SAL-SUJGRUP                     03180081
           .                                                            03190081
      *                                                                 03200081
      *---------------------*                                           03210081
       2300-IMPRIME-REPORTE.                                            03220081
      *---------------------*                                           03230081
           MOVE SALIDA-COMP TO SAL-NEG-BOLSA                            03240081
           WRITE SAL-NEG-BOLSA                                          03250081
           ADD 1 TO REG-IMPRESO                                         03260081
           .                                                            03270081
      *                                                                *03280081
      *--------------------*                                            03290081
       2100-OBTIENE-PERSONA.                                            03300081
      *--------------------*                                            03310081
           INITIALIZE               W520-REGISTRO                       03320081
           MOVE NUMCLI-VLDTARC TO   W520-NUMCLIEN                       03330081
           CALL PE9C5201      USING W520-REGISTRO                       03340081
      *                                                                 03350081
           EVALUATE W520-PECRETOR                                       03360081
               WHEN '00'                                                03370081
                    IF W520-IRELPAT = 'D' OR 'E' OR 'F' OR 'J'          03380081
                       MOVE W520-IRELPAT   TO CLI-IRELPAT               03390081
                    ELSE                                                03400081
@RTP                   MOVE ' '    TO CLI-IRELPAT                       03410081
                    END-IF                                              03420081
                    MOVE W520-SUJGRUP   TO CLI-SUJGRUP                  03430081
@RTP  *             MOVE W520-PRIAPE    TO CLIE-APE-PAT                 03440081
@RTP  *             MOVE W520-SEGAPE    TO CLIE-APE-MAT                 03450081
@RTP  *             MOVE W520-NOMBRE    TO CLIE-NOMBRES                 03460081
@RTP                IF W520-SUJGRUP = 'F'                               03470081
@RTP                   STRING W520-NOMBRE DELIMITED BY '  ' ' '         03480081
@RTP                          W520-PRIAPE DELIMITED BY '  ' ' '         03490081
@RTP                          W520-SEGAPE DELIMITED BY '  '             03500081
@RTP                                               INTO NOMBRE-CLIE     03510081
@RTP                ELSE                                                03520081
@RTP                   STRING W520-NOMBRE DELIMITED BY SIZE             03530081
@RTP                          W520-PRIAPE DELIMITED BY SIZE             03540081
@RTP                          W520-SEGAPE DELIMITED BY SIZE             03550081
@RTP                                               INTO NOMBRE-CLIE     03560081
@RTP                END-IF                                              03570081
                                                                        03580081
               WHEN OTHER                                               03590081
                    CONTINUE                                            03600081
           END-EVALUATE.                                                03610081
      *                                                                *03620081
      *--------*                                                        03630081
       3000-FIN.                                                        03640081
      *--------*                                                        03650081
      *                                                                *03660081
           CLOSE E1VDTCOM                                               03670081
                 E2VDTARC                                               03680081
                 S1RNEGBL                                               03690081
                                                                        03700081
           DISPLAY '*********** FIN DEL PROGRAMA ***********'           03710081
           DISPLAY 'LEIDOS VLDTCOM:    ' CONT-LEIDO-VLDTCOM             03720081
           DISPLAY 'LEIDOS VLDTARC:    ' CONT-LEIDO-VLDTARC             03730081
           DISPLAY 'REG.  GRABADOS:    ' REG-IMPRESO                    03740081
           .                                                            03750081
      *                                                                *03760081
      *-------------*                                                   03770081
       3000-FIN-EXIT.                                                   03780081
      *-------------*                                                   03790081
           EXIT.                                                        03800081
      *                                                                *03810081
       3001-ERROR.                                                      03820081
      *-----------*                                                     03830081
           DISPLAY '********** ERROR EN EL PROGRAMA **********'         03840081
           DISPLAY 'ACCION:          ' WS-ACCION                        03850081
           DISPLAY 'PARRAFO:         ' WS-PARRAFO                       03860081
           DISPLAY '******************************************'         03870081
           MOVE 08 TO RETURN-CODE                                       03880081
           STOP RUN.                                                    03890081
      *-----------------*                                               03900081
      * FIN DE PROGRAMA *                                               03910081
      *-----------------*                                               03920081
