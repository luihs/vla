       IDENTIFICATION DIVISION.                                         00010059
      *=======================*                                         00020059
       PROGRAM-ID.    VL4C3384.                                         00030059
       AUTHOR.        JHONNY I PINEDO CUEVAS.                           00040059
      ******************************************************************00050059
      *REF.PETIC FECHA-MOD. PROGRAMADOR      DESCRIPCION               *00060059
      *--------- ---------- ---------------- --------------------------*00070059
      *200711038 25-02-2008 JHONNY PINEDO C °CARGO Y ABONO POR EL TOTAL*00080059
      *                                      CONTABLE A CUENTAS DE RE- *00090059
      *                                      REGISTRO.                 *00100059
      *--------- ---------- ---------------- --------------------------*00110059
      *200806019 04-06-2008 JHONNY PINEDO C °ADICIONAR CONTABLE CONASEV*00120059
      *--------- ---------- ---------------- --------------------------*00130059
      *200806094 17-06-2008 JHONNY PINEDO C °OBTENER MONEDA PARA CONTA-*00140059
      *                                      BLE CONASEV.              *00150059
      *                                     °ANULAR CONTABLE CONASEV A *00160059
      *                                      SOLICITUD DE VICTOR BARRIOS00170059
      *                                      POR REVISION DE PRUEBAS DEL00180059
      *                                      11-06 AL 13-06-2008.      *00190059
      *--------- ---------- ---------------- --------------------------*00200059
      *200807002 14-10-2008 JHONNY PINEDO C °CORREGIR SALDO A CUENTA   *00210059
      *                                      269149 POR TENER SALDO    *00220059
      *                                      ERRADO SOLICITADO POR     *00230059
      *                                      ELIZABETH MONTOYA Y MARIA *00240059
      *                                      DE FATIMA ANDRADE.        *00250059
      *--------- ---------- ---------------- --------------------------*00260059
JPC@1 *200807002 17-10-2008 JHONNY PINEDO C °CARGO/ABONO SOLO POR SALDO*00270059
      *                                      DISPONIBLE, PARA EVITAR LA*00280059
      *                                      INCIDENCIA DEL CARGO Y PASE00290059
      *                                      EL ABONO.                 *00300059
      *--------- ---------- ---------------- --------------------------*00310059
JPC@2 *200804248 12-11-2008 JHONNY PINEDO C °POR SALDO NEGATIVO SE HACE*00320059
      *                                      ABONO A CUENTA DE REGISTRO*00330059
      *                                      Y CARGO A CTA OPERATIVA.  *00340059
      *--------- ---------- ---------------- --------------------------*00350059
JPC@3 *200808113 29-01-2009 JHONNY PINEDO C °PARA ABONO CUENTA REGISTRO*00360059
      *                                      CAMBIAR FECHA VALOR OPS A *00370059
      *                                      FECHA PROCESO MAS 1.      *00380059
      *--------- ---------- ---------------- --------------------------*00390059
      ******************************************************************00400059
       ENVIRONMENT DIVISION.                                            00410059
      *===================*                                             00420059
       CONFIGURATION SECTION.                                           00430059
       INPUT-OUTPUT SECTION.                                            00440059
       FILE-CONTROL.                                                    00450059
      *                                                                *00460059
           SELECT S1DQ3384        ASSIGN        TO UT-S-S1DQ3384        00470059
                                  ORGANIZATION  IS SEQUENTIAL           00480059
                                  ACCESS MODE   IS SEQUENTIAL           00490059
                                  FILE STATUS   IS FS-S1DQ3384.         00500059
      *                                                                *00510059
           SELECT S2DQ3384        ASSIGN        TO UT-S-S2DQ3384        00520059
                                  ORGANIZATION  IS SEQUENTIAL           00530059
                                  ACCESS MODE   IS SEQUENTIAL           00540059
                                  FILE STATUS   IS FS-S2DQ3384.         00550059
      *                                                                *00560059
           SELECT S3DQ3384        ASSIGN        TO UT-S-S3DQ3384        00570059
                                  ORGANIZATION  IS SEQUENTIAL           00580059
                                  ACCESS MODE   IS SEQUENTIAL           00590059
                                  FILE STATUS   IS FS-S3DQ3384.         00600059
      *                                                                *00610059
           SELECT S4DQ3384        ASSIGN        TO UT-S-S4DQ3384        00620059
                                  ORGANIZATION  IS SEQUENTIAL           00630059
                                  ACCESS MODE   IS SEQUENTIAL           00640059
                                  FILE STATUS   IS FS-S4DQ3384.         00650059
      *                                                                *00660059
      *200806019-INI                                                   *00670059
           SELECT S5DQ3384        ASSIGN        TO UT-S-S5DQ3384        00680059
                                  ORGANIZATION  IS SEQUENTIAL           00690059
                                  ACCESS MODE   IS SEQUENTIAL           00700059
                                  FILE STATUS   IS FS-S5DQ3384.         00710059
      *                                                                *00720059
           SELECT S6DQ3384        ASSIGN        TO UT-S-S6DQ3384        00730059
                                  ORGANIZATION  IS SEQUENTIAL           00740059
                                  ACCESS MODE   IS SEQUENTIAL           00750059
                                  FILE STATUS   IS FS-S6DQ3384.         00760059
      *200806019-FIN                                                   *00770059
      *                                                                *00780059
       DATA DIVISION.                                                   00790059
      *=============*                                                   00800059
       FILE SECTION.                                                    00810059
      *OPS                                                             *00820059
       FD  S1DQ3384                                                     00830059
           RECORDING MODE  IS F                                         00840059
           LABEL   RECORDS IS STANDARD.                                 00850059
       01  REGISTRO-CARGO  PIC X(2390).                                 00860059
      *OPS                                                             *00870059
       FD  S2DQ3384                                                     00880059
           RECORDING MODE  IS F                                         00890059
           LABEL   RECORDS IS STANDARD.                                 00900059
       01  REGISTRO-ABONO  PIC X(2390).                                 00910059
      *CTBL                                                            *00920059
       FD  S3DQ3384                                                     00930059
           RECORDING MODE  IS F                                         00940059
           LABEL   RECORDS IS STANDARD.                                 00950059
       01  CONTABLE-CARGO  PIC X(0306).                                 00960059
      *CTBL                                                            *00970059
       FD  S4DQ3384                                                     00980059
           RECORDING MODE  IS F                                         00990059
           LABEL   RECORDS IS STANDARD.                                 01000059
       01  CONTABLE-ABONO  PIC X(0306).                                 01010059
      *200806019-INI                                                   *01020059
      *CTBL-CONASEV                                                    *01030059
       FD  S5DQ3384                                                     01040059
           RECORDING MODE  IS F                                         01050059
           LABEL   RECORDS IS STANDARD.                                 01060059
       01  CONASEV-CARGO   PIC X(0128).                                 01070059
      *CTBL-CONASEV                                                    *01080059
       FD  S6DQ3384                                                     01090059
           RECORDING MODE  IS F                                         01100059
           LABEL   RECORDS IS STANDARD.                                 01110059
       01  CONASEV-ABONO   PIC X(0128).                                 01120059
      *200806019-FIN                                                   *01130059
      *                                                                *01140059
      *-----------------------*                                        *01150059
       WORKING-STORAGE SECTION.                                         01160059
      *-----------------------*                                        *01170059
       77  W-PROGRAMA             PIC  X(08)       VALUE 'VL4C3384'.    01180059
       77  BG9CMDC0               PIC  X(08)       VALUE 'BG9CMDC0'.    01190059
       77  TC8C1220               PIC  X(08)       VALUE 'TC8C1220'.    01200059
       77  TC9C1200               PIC  X(08)       VALUE 'TC9C1200'.    01210059
       77  XX-DIAS                PIC  9(02)       VALUE 1.             01220059
      *                                                                *01230059
       77  SW-FIN-VLDTARC         PIC  X(02)       VALUE 'NO'.          01240059
           88 FIN-VLDTARC                          VALUE 'SI'.          01250059
      *                                                                *01260059
      *200804248-INI                                                   *01270059
       01  W-LIMITE-AUT             PIC  X(08)   VALUE SPACES.          01280059
       01  W-LIMITE-AUT-9 REDEFINES W-LIMITE-AUT                        01290059
                                    PIC S9(13)V9(02) COMP-3.            01300059
      *200804248-FIN                                                   *01310059
      *200806019-INI                                                   *01320059
       01  REG-CONASEV.                                                 01330059
           02 CTBL-TIPOPE      PIC X(01).                               01340059
           02 CTBL-FECPRO      PIC 9(06).                               01350059
           02 CTBL-NROCOM      PIC X(04).                               01360059
           02 CTBL-MONEXT      PIC X(03).                               01370059
           02 CTBL-IMPEXT      PIC 9(11)V9(02).                         01380059
           02 CTBL-TIPCAM      PIC 9(11)V9(02).                         01390059
           02 CTBL-REFERE      PIC X(40).                               01400059
           02 CTBL-CODREL      PIC X(08).                               01410059
           02 CTBL-NUMREG      PIC X(03).                               01420059
           02 CTBL-MONEDA      PIC X(03).                               01430059
           02 CTBL-IMPORT      PIC 9(13)V9(02).                         01440059
           02 CTBL-SECORI      PIC 9(04).                               01450059
           02 CTBL-OFICIN      PIC 9(03).                               01460059
           02 CTBL-CUENTA      PIC X(12).                               01470059
      *200806019-FIN                                                   *01480059
      *                                                                *01490059
       01  WXTB-TEXCAST-003       PIC  X(25)       VALUE SPACES.        01500059
       01  WXTB-TEXCAST-023       PIC  X(25)       VALUE SPACES.        01510059
      *                                                                *01520059
       01  WMDC-OFICONT           PIC  9(04)       VALUE ZEROS.         01530059
       01  WMDC-OFICONT-PEN       PIC  9(04)       VALUE ZEROS.         01540059
       01  WMDC-OFICONT-USD       PIC  9(04)       VALUE ZEROS.         01550059
      *                                                                *01560059
       01  WA-HORA.                                                     01570059
           03  WA-HORA-H          PIC  9(02).                           01580059
           03  WA-HORA-M          PIC  9(02).                           01590059
           03  WA-HORA-S          PIC  9(02).                           01600059
       01  WR-HORA REDEFINES WA-HORA PIC 9(06).                         01610059
      *                                                                 01620059
       01  FECHA-SYSIN.                                                 01630059
           02  DD-SYS             PIC  9(02).                           01640059
           02  MM-SYS             PIC  9(02).                           01650059
           02  SS-SYS             PIC  9(02).                           01660059
           02  AA-SYS             PIC  9(02).                           01670059
      *                                                                 01680059
       01  FECHA-MAS1.                                                  01690059
           02  ANO-MAS1           PIC 9(04).                            01700059
           02  MM-MAS1            PIC 9(02).                            01710059
           02  DD-MAS1            PIC 9(02).                            01720059
      *                                                                 01730059
       01  WA-FECPRO.                                                   01740059
           02  WA-FECPRO-S        PIC  9(02).                           01750059
           02  WA-FECPRO-A        PIC  9(02).                           01760059
           02  FILLER             PIC  X(01)    VALUE '-'.              01770059
           02  WA-FECPRO-M        PIC  9(02).                           01780059
           02  FILLER             PIC  X(01)    VALUE '-'.              01790059
           02  WA-FECPRO-D        PIC  9(02).                           01800059
      *                                                                 01810059
JPC@3 *200808113-INI                                                    01820059
       01  WA-FEMAS1.                                                   01830059
           02  WA-FEMAS1-A        PIC  9(04).                           01840059
           02  FILLER             PIC  X(01)    VALUE '-'.              01850059
           02  WA-FEMAS1-M        PIC  9(02).                           01860059
           02  FILLER             PIC  X(01)    VALUE '-'.              01870059
           02  WA-FEMAS1-D        PIC  9(02).                           01880059
JPC@3 *200808113-FIN                                                    01890059
      *                                                                 01900059
       01  WA-HOYAMD              PIC  9(08).                           01910059
       01  WR-HOYAMD REDEFINES WA-HOYAMD.                               01920059
           02  WA-HOYAMD-S        PIC  9(02).                           01930059
           02  WA-HOYAMD-A        PIC  9(02).                           01940059
           02  WA-HOYAMD-M        PIC  9(02).                           01950059
           02  WA-HOYAMD-D        PIC  9(02).                           01960059
      *                                                                 01970059
       01  W-OBSERVACIONES.                                             01980059
           10  FILLER             PIC  X(03) VALUE ' - '.               01990059
           10  W-OPERAC           PIC  9(03) VALUE ZEROS.               02000059
           10  FILLER             PIC  X(03) VALUE ' - '.               02010059
           10  W-REFERENCIA       PIC  X(09) VALUE SPACES.              02020059
           10  W-REFER-N REDEFINES W-REFERENCIA PIC 9(09).              02030059
           10  FILLER             PIC  X(12) VALUE SPACES.              02040059
      *                                                                 02050059
       01  WA-MIR-OBSERVA-CAR.                                          02060059
           03  FILLER             PIC  X(40)    VALUE                   02070059
           'TRASPASO CTA-REGISTRO A OPERATIVA S.A.B.'.                  02080059
       01  WA-MIR-OBSERVA-ABO.                                          02090059
           03  FILLER             PIC  X(40)    VALUE                   02100059
           'TRASPASO CTA-OPERATIVA S.A.B. A REGISTRO'.                  02110059
      *                                                                 02120059
       01  WA-CONTADORES.                                               02130059
           02 LEI-VLDTARC         PIC  9(07)    VALUE ZEROS.            02140059
           02 PRO-VLDTARC         PIC  9(07)    VALUE ZEROS.            02150059
           02 LEI-BG9CMDC0        PIC  9(07)    VALUE ZEROS.            02160059
           02 WRI-CARGOS          PIC  9(07)    VALUE ZEROS.            02170059
           02 WRI-ABONOS          PIC  9(07)    VALUE ZEROS.            02180059
           02 WRI-CARGOS-CTBL     PIC  9(07)    VALUE ZEROS.            02190059
           02 WRI-ABONOS-CTBL     PIC  9(07)    VALUE ZEROS.            02200059
           02 WRI-CARGOS-CONA     PIC  9(07)    VALUE ZEROS.            02210059
           02 WRI-ABONOS-CONA     PIC  9(07)    VALUE ZEROS.            02220059
      *                                                                 02230059
       01  REGISTRO-BGECOPS.                                            02240059
           COPY BGECOPS.                                                02250059
       01  REGISTRO-BGECMIR.                                            02260059
           COPY BGECMIR.                                                02270059
       01  W-BGECMDC.                                                   02280059
           COPY BGECMDC.                                                02290059
      *01  HAEC040.                                                     02300059
           COPY HAEC040.                                                02310059
      *01  TCWC1750.                                                    02320059
           COPY TCWC1750.                                               02330059
      *01  TCWC1400.                                                    02340059
           COPY TCWC1400.                                               02350059
      *                                                                 02360059
       01  FS-STATUS.                                                   02370059
           03 FS-S1DQ3384         PIC  X(02)    VALUE '00'.             02380059
           03 FS-S2DQ3384         PIC  X(02)    VALUE '00'.             02390059
           03 FS-S3DQ3384         PIC  X(02)    VALUE '00'.             02400059
           03 FS-S4DQ3384         PIC  X(02)    VALUE '00'.             02410059
      *200806019-INI                                                    02420059
           03 FS-S5DQ3384         PIC  X(02)    VALUE '00'.             02430059
           03 FS-S6DQ3384         PIC  X(02)    VALUE '00'.             02440059
      *200806019-FIN                                                    02450059
      *                                                                 02460059
      ******************************************************************02470059
      ***       COPYS DE ERRORES                                     ***02480059
      ******************************************************************02490059
      *                                                                *02500059
           COPY QRECDB2.                                                02510059
           COPY VLWC8000.                                               02520059
           COPY VLWC8010.                                               02530059
           COPY VLWC8020.                                               02540059
           COPY VLWCRUTI.                                               02550059
      *                                                                *02560059
      ******************************************************************02570059
      *  DEFINICION DCLGEN DE LAS TABLAS                               *02580059
      ******************************************************************02590059
      *                                                                *02600059
           EXEC SQL INCLUDE SQLCA   END-EXEC.                           02610059
           EXEC SQL INCLUDE VLGTARC END-EXEC.                           02620059
           EXEC SQL INCLUDE VLGTXBO END-EXEC.                           02630059
           EXEC SQL INCLUDE VLGTXTB END-EXEC.                           02640059
      *                                                                *02650059
      ******************************************************************02660059
      *  DEFINICION DE CURSOR                                          *02670059
      ******************************************************************02680059
      *    CUENTAS DE REGISTRO (CONTINENTAL BOLSA)                     *02690059
           EXEC SQL                                                     02700059
                DECLARE VLDCARC CURSOR FOR                              02710059
                 SELECT VARC_FILLER                                     02720059
                      , VARC_CUENTA                                     02730059
                      , VARC_SUCURS                                     02740059
                      , VARC_CTAVAL20                                   02750059
      *200806094-INI                                                    02760059
                      , VARC_MONEDA                                     02770059
      *200806094-FIN                                                    02780059
                   FROM VLDTARC                                         02790059
      *200806019-INI                                                    02800059
      *           WHERE SUBSTR(VARC_FILLER,11,02) = '91'                02810059
                  WHERE VARC_CUENTA > :VARC-CUENTA                      02820059
      *200806019-FIN                                                    02830059
                  ORDER BY VARC_CUENTA                                  02840059
           END-EXEC.                                                    02850059
      *                                                                *02860059
      *==================*                                              02870059
       PROCEDURE DIVISION.                                              02880059
      *==================*                                              02890059
       000-RUT-PRINCIPAL.                                               02900059
      *   *-------------*                                               02910059
      *                                                                *02920059
           PERFORM 100-INICIO.                                          02930059
      *                                                                *02940059
           PERFORM 200-PROCESO                                          02950059
             UNTIL FIN-VLDTARC.                                         02960059
      *                                                                *02970059
           PERFORM 300-FIN.                                             02980059
      *                                                                *02990059
      *   *------*                                                      03000059
       100-INICIO.                                                      03010059
      *   *------*                                                      03020059
      *                                                                *03030059
           INITIALIZE DCLVLDTARC.                                       03040059
      *                                                                *03050059
           ACCEPT FECHA-SYSIN.                                          03060059
      *                                                                *03070059
           ACCEPT WA-HORA FROM  TIME.                                   03080059
      *                                                                *03090059
           MOVE SS-SYS     TO   WA-FECPRO-S, WA-HOYAMD-S                03100059
           MOVE AA-SYS     TO   WA-FECPRO-A, WA-HOYAMD-A.               03110059
           MOVE MM-SYS     TO   WA-FECPRO-M, WA-HOYAMD-M.               03120059
           MOVE DD-SYS     TO   WA-FECPRO-D, WA-HOYAMD-D.               03130059
      *                                                                *03140059
      ***                                                            ***03150059
      *                                                                *03160059
           OPEN OUTPUT S1DQ3384.                                        03170059
           IF FS-S1DQ3384 NOT = '00'                                    03180059
              MOVE  'S1DQ3384'              TO  W802-FICHERO            03190059
              MOVE  FS-S1DQ3384             TO  W802-STATUS             03200059
              MOVE  'VL4C3384'              TO  W802-PROGRAMA           03210059
              MOVE  'OPEN'                  TO  W802-ACCION             03220059
              MOVE  SPACES                  TO  W802-CLAVE              03230059
              MOVE  ZEROS                   TO  W802-ABEND              03240059
              MOVE  '100-INICIO        '    TO  W802-PARRAFO            03250059
              PERFORM VLPC8020-DISP-ABEND-FICH                          03260059
              PERFORM VLPC8020-ABEND-FICH                               03270059
           END-IF.                                                      03280059
      *                                                                *03290059
           OPEN OUTPUT S2DQ3384.                                        03300059
           IF FS-S2DQ3384 NOT = '00'                                    03310059
              MOVE  'S2DQ3384'              TO  W802-FICHERO            03320059
              MOVE  FS-S2DQ3384             TO  W802-STATUS             03330059
              MOVE  'VL4C3384'              TO  W802-PROGRAMA           03340059
              MOVE  'OPEN'                  TO  W802-ACCION             03350059
              MOVE  SPACES                  TO  W802-CLAVE              03360059
              MOVE  ZEROS                   TO  W802-ABEND              03370059
              MOVE  '100-INICIO        '    TO  W802-PARRAFO            03380059
              PERFORM VLPC8020-DISP-ABEND-FICH                          03390059
              PERFORM VLPC8020-ABEND-FICH                               03400059
           END-IF.                                                      03410059
      *                                                                *03420059
           OPEN OUTPUT S3DQ3384.                                        03430059
           IF FS-S3DQ3384 NOT = '00'                                    03440059
              MOVE  'S3DQ3384'              TO  W802-FICHERO            03450059
              MOVE  FS-S3DQ3384             TO  W802-STATUS             03460059
              MOVE  'VL4C3384'              TO  W802-PROGRAMA           03470059
              MOVE  'OPEN'                  TO  W802-ACCION             03480059
              MOVE  SPACES                  TO  W802-CLAVE              03490059
              MOVE  ZEROS                   TO  W802-ABEND              03500059
              MOVE  '100-INICIO        '    TO  W802-PARRAFO            03510059
              PERFORM VLPC8020-DISP-ABEND-FICH                          03520059
              PERFORM VLPC8020-ABEND-FICH                               03530059
           END-IF.                                                      03540059
      *                                                                *03550059
           OPEN OUTPUT S4DQ3384.                                        03560059
           IF FS-S4DQ3384 NOT = '00'                                    03570059
              MOVE  'S4DQ3384'              TO  W802-FICHERO            03580059
              MOVE  FS-S4DQ3384             TO  W802-STATUS             03590059
              MOVE  'VL4C3384'              TO  W802-PROGRAMA           03600059
              MOVE  'OPEN'                  TO  W802-ACCION             03610059
              MOVE  SPACES                  TO  W802-CLAVE              03620059
              MOVE  ZEROS                   TO  W802-ABEND              03630059
              MOVE  '100-INICIO        '    TO  W802-PARRAFO            03640059
              PERFORM VLPC8020-DISP-ABEND-FICH                          03650059
              PERFORM VLPC8020-ABEND-FICH                               03660059
           END-IF.                                                      03670059
      *                                                                *03680059
      *200806019-INI                                                   *03690059
           OPEN OUTPUT S5DQ3384.                                        03700059
           IF FS-S5DQ3384 NOT = '00'                                    03710059
              MOVE  'S5DQ3384'              TO  W802-FICHERO            03720059
              MOVE  FS-S5DQ3384             TO  W802-STATUS             03730059
              MOVE  'VL4C3384'              TO  W802-PROGRAMA           03740059
              MOVE  'OPEN'                  TO  W802-ACCION             03750059
              MOVE  SPACES                  TO  W802-CLAVE              03760059
              MOVE  ZEROS                   TO  W802-ABEND              03770059
              MOVE  '100-INICIO        '    TO  W802-PARRAFO            03780059
              PERFORM VLPC8020-DISP-ABEND-FICH                          03790059
              PERFORM VLPC8020-ABEND-FICH                               03800059
           END-IF.                                                      03810059
      *                                                                *03820059
           OPEN OUTPUT S6DQ3384.                                        03830059
           IF FS-S6DQ3384 NOT = '00'                                    03840059
              MOVE  'S6DQ3384'              TO  W802-FICHERO            03850059
              MOVE  FS-S6DQ3384             TO  W802-STATUS             03860059
              MOVE  'VL4C3384'              TO  W802-PROGRAMA           03870059
              MOVE  'OPEN'                  TO  W802-ACCION             03880059
              MOVE  SPACES                  TO  W802-CLAVE              03890059
              MOVE  ZEROS                   TO  W802-ABEND              03900059
              MOVE  '100-INICIO        '    TO  W802-PARRAFO            03910059
              PERFORM VLPC8020-DISP-ABEND-FICH                          03920059
              PERFORM VLPC8020-ABEND-FICH                               03930059
           END-IF.                                                      03940059
      *200806019-FIN                                                  * 03950059
      *                                                                *03960059
           PERFORM 400-DIA-SIGUIENTE.                                   03970059
      *                                                                *03980059
           PERFORM 240-SELECT-VLDTXBO.                                  03990059
      *                                                                *04000059
           PERFORM 245-SELECT-VLDTXTB.                                  04010059
      *                                                                *04020059
           PERFORM 210-OPEN-VLDCARC.                                    04030059
      *                                                                *04040059
           PERFORM 220-FETCH-VLDCARC.                                   04050059
      *                                                                *04060059
      *   *-------*                                                    *04070059
       200-PROCESO.                                                     04080059
      *   *-------*                                                    *04090059
      *                                                                *04100059
           INITIALIZE                   W-BGECMDC.                      04110059
           MOVE VARC-FILLER (01:04) TO  MDC-ENTIDAD.                    04120059
           MOVE VARC-FILLER (05:04) TO  MDC-CENTRO-ALTA.                04130059
           MOVE VARC-FILLER (11:10) TO  MDC-CUENTA.                     04140059
           PERFORM 250-CALL-BG9CMDC0.                                   04150059
      *                                                                *04160059
      *200807002-INI                                                    04170059
      *    IF MDC-SALDO-DISPUE > ZEROS                                  04180059
           IF MDC-SALDO-DISPON > ZEROS                                  04190059
      *200807002-FIN                                                    04200059
              PERFORM 260-REGISTRO-CARGO                                04210059
              PERFORM 270-REGISTRO-ABONO                                04220059
              PERFORM 280-CONTABLE-CARGO                                04230059
              PERFORM 290-CONTABLE-ABONO                                04240059
      *200806094-INI                                                    04250059
      *       PERFORM 295-CONASEV-CARGO                                 04260059
      *       PERFORM 297-CONASEV-ABONO                                 04270059
      *200806094-FIN                                                    04280059
      *200804248-INI                                                    04290059
           ELSE                                                         04300059
              IF MDC-SALDO-DISPON < ZEROS                               04310059
                 PERFORM 260-REGISTRO-ABONO-NEG                         04320059
                 PERFORM 270-REGISTRO-CARGO-NEG                         04330059
                 PERFORM 280-CONTABLE-ABONO-NEG                         04340059
                 PERFORM 290-CONTABLE-CARGO-NEG                         04350059
              END-IF                                                    04360059
      *200804248-FIN                                                    04370059
           END-IF.                                                      04380059
      *                                                                *04390059
           PERFORM 220-FETCH-VLDCARC.                                   04400059
      *                                                                *04410059
      *   *------------*                                               *04420059
       210-OPEN-VLDCARC.                                                04430059
      *   *------------*                                               *04440059
      *                                                                *04450059
           MOVE ZEROS TO VARC-CUENTA.                                   04460059
                                                                        04470059
           EXEC SQL                                                     04480059
               OPEN VLDCARC                                             04490059
           END-EXEC                                                     04500059
      *                                                                *04510059
           EVALUATE SQLCODE                                             04520059
               WHEN 0                                                   04530059
                    CONTINUE                                            04540059
               WHEN OTHER                                               04550059
                    MOVE 'VL4C3384'              TO  W801-PROGRAMA      04560059
                    MOVE 'VLDTARC'               TO  W801-TABLA         04570059
                    MOVE 'OPEN   '               TO  W801-ACCION        04580059
                    MOVE  SPACES                 TO  W801-CLAVE         04590059
                    MOVE  SQLCODE                TO  W801-SQLCODE       04600059
                    MOVE  SPACES                 TO  W801-SQLWARN       04610059
                    MOVE '210-OPEN-VLDCARC     ' TO  W801-PARRAFO       04620059
                    PERFORM  VLPC8010-DISP-ABEND-DB2                    04630059
                    PERFORM  VLPC8010-ABEND-DB2                         04640059
           END-EVALUATE.                                                04650059
      *                                                                *04660059
      *   *-------------*                                              *04670059
       220-FETCH-VLDCARC.                                               04680059
      *   *-------------*                                              *04690059
      *                                                                *04700059
           EXEC SQL                                                     04710059
                FETCH  VLDCARC                                          04720059
                 INTO :VARC-FILLER                                      04730059
                    , :VARC-CUENTA                                      04740059
                    , :VARC-SUCURS                                      04750059
                    , :VARC-CTAVAL20                                    04760059
      *200806094-INI                                                    04770059
                    , :VARC-MONEDA                                      04780059
      *200806094-FIN                                                    04790059
           END-EXEC                                                     04800059
      *                                                                *04810059
           EVALUATE SQLCODE                                             04820059
               WHEN 0                                                   04830059
                    ADD  1                       TO  LEI-VLDTARC        04840059
                    IF VARC-FILLER (11:02) NOT = '91'                   04850059
                       GO TO 220-FETCH-VLDCARC                          04860059
                    END-IF                                              04870059
                    ADD  1                       TO  PRO-VLDTARC        04880059
               WHEN 100                                                 04890059
                    MOVE 'SI'                    TO  SW-FIN-VLDTARC     04900059
               WHEN OTHER                                               04910059
                    MOVE 'VL4C3384'              TO  W801-PROGRAMA      04920059
                    MOVE 'VLDTARC'               TO  W801-TABLA         04930059
                    MOVE 'FETCH  '               TO  W801-ACCION        04940059
                    MOVE  VARC-CUENTA            TO  W801-CLAVE (01:07) 04950059
                    MOVE  LEI-VLDTARC            TO  W801-CLAVE (09:06) 04960059
                    MOVE  SQLCODE                TO  W801-SQLCODE       04970059
                    MOVE  SPACES                 TO  W801-SQLWARN       04980059
                    MOVE '220-FETCH-VLDCARC    ' TO  W801-PARRAFO       04990059
                    PERFORM  VLPC8010-DISP-ABEND-DB2                    05000059
                    PERFORM  VLPC8010-ABEND-DB2                         05010059
           END-EVALUATE.                                                05020059
      *                                                                *05030059
      *   *-------------*                                              *05040059
       230-CLOSE-VLDCARC.                                               05050059
      *   *-------------*                                              *05060059
      *                                                                *05070059
           EXEC SQL                                                     05080059
                CLOSE VLDCARC                                           05090059
           END-EXEC.                                                    05100059
      *                                                                *05110059
           EVALUATE SQLCODE                                             05120059
               WHEN 0                                                   05130059
                    CONTINUE                                            05140059
               WHEN OTHER                                               05150059
                    MOVE 'VL4C3384'              TO  W801-PROGRAMA      05160059
                    MOVE 'VLDTARC'               TO  W801-TABLA         05170059
                    MOVE 'CLOSE  '               TO  W801-ACCION        05180059
                    MOVE  SPACES                 TO  W801-CLAVE         05190059
                    MOVE  SQLCODE                TO  W801-SQLCODE       05200059
                    MOVE  SPACES                 TO  W801-SQLWARN       05210059
                    MOVE '230-CLOSE-VLDTARC    ' TO  W801-PARRAFO       05220059
                    PERFORM  VLPC8010-DISP-ABEND-DB2                    05230059
                    PERFORM  VLPC8010-ABEND-DB2                         05240059
           END-EVALUATE.                                                05250059
      *                                                                *05260059
      *   *--------------*                                              05270059
       240-SELECT-VLDTXBO.                                              05280059
      *   *--------------*                                              05290059
      *                                                                *05300059
           MOVE '01'      TO  VXBO-CLABOL                               05310059
      *                                                                *05320059
           EXEC SQL                                                     05330059
                SELECT  VXBO_CTAECOS                                    05340059
                     ,  VXBO_CTAECOD                                    05350059
                  INTO :VXBO-CTAECOS                                    05360059
                     , :VXBO-CTAECOD                                    05370059
                  FROM  VLDTXBO                                         05380059
                 WHERE  VXBO_CLABOL = :VXBO-CLABOL                      05390059
           END-EXEC.                                                    05400059
      *                                                                *05410059
           MOVE SQLCODE  TO  SQLCODE-AUX                                05420059
           EVALUATE TRUE                                                05430059
              WHEN DB2-OK                                               05440059
                   CONTINUE                                             05450059
              WHEN OTHER                                                05460059
                   MOVE 'VL4C3384'               TO  W801-PROGRAMA      05470059
                   MOVE 'VLDTXBO'                TO  W801-TABLA         05480059
                   MOVE 'SELECT      '           TO  W801-ACCION        05490059
                   MOVE VXBO-CLABOL              TO  W801-CLAVE         05500059
                   MOVE  SQLCODE                 TO  W801-SQLCODE       05510059
                   MOVE  SPACES                  TO  W801-SQLWARN       05520059
                   MOVE '240-SELECT-VLDTXBO    ' TO  W801-PARRAFO       05530059
                   PERFORM  VLPC8010-DISP-ABEND-DB2                     05540059
                   PERFORM  VLPC8010-ABEND-DB2                          05550059
           END-EVALUATE.                                                05560059
      ***                                                            ***05570059
      *    OBTENER EL DIGITO DE CONTROL                                *05580059
      ***                                                            ***05590059
           MOVE ZEROS                TO VARC-CUENTA.                    05600059
           INITIALIZE                   W-BGECMDC.                      05610059
           MOVE VXBO-CTAECOS (01:04) TO MDC-ENTIDAD.                    05620059
           MOVE VXBO-CTAECOS (05:04) TO MDC-CENTRO-ALTA.                05630059
           MOVE VXBO-CTAECOS (11:10) TO MDC-CUENTA.                     05640059
           MOVE 'PEN'                TO WRUT-CLAVE   (28:03).           05650059
           PERFORM 250-CALL-BG9CMDC0.                                   05660059
           MOVE MDC-DIGICCC1         TO VXBO-CTAECOS (09:01).           05670059
           MOVE MDC-DIGICCC2         TO VXBO-CTAECOS (10:01).           05680059
           MOVE MDC-CENTRO-CONTAB    TO WMDC-OFICONT-PEN.               05690059
      *                                                                *05700059
           INITIALIZE                   W-BGECMDC.                      05710059
           MOVE VXBO-CTAECOD (01:04) TO MDC-ENTIDAD.                    05720059
           MOVE VXBO-CTAECOD (05:04) TO MDC-CENTRO-ALTA.                05730059
           MOVE VXBO-CTAECOD (11:10) TO MDC-CUENTA.                     05740059
           MOVE 'USD'                TO WRUT-CLAVE   (28:03).           05750059
           PERFORM 250-CALL-BG9CMDC0.                                   05760059
           MOVE MDC-DIGICCC1         TO VXBO-CTAECOD (09:01).           05770059
           MOVE MDC-DIGICCC2         TO VXBO-CTAECOD (10:01).           05780059
           MOVE MDC-CENTRO-CONTAB    TO WMDC-OFICONT-USD.               05790059
           MOVE SPACES               TO WRUT-CLAVE.                     05800059
      *                                                                *05810059
      *   *--------------*                                              05820059
       245-SELECT-VLDTXTB.                                              05830059
      *   *--------------*                                              05840059
      *                                                                *05850059
           MOVE '003'  TO VXTB-CODIGO.                                  05860059
           MOVE '000'  TO VXTB-SUBTIPO.                                 05870059
      *                                                                *05880059
           EXEC SQL                                                     05890059
                SELECT  VXTB_TEXCAST                                    05900059
                  INTO :VXTB-TEXCAST                                    05910059
                  FROM  VLDTXTB                                         05920059
                 WHERE  VXTB_CODIGO  = :VXTB-CODIGO                     05930059
                   AND  VXTB_SUBTIPO = :VXTB-SUBTIPO                    05940059
           END-EXEC.                                                    05950059
      *                                                                *05960059
           MOVE SQLCODE  TO  SQLCODE-AUX                                05970059
           EVALUATE TRUE                                                05980059
               WHEN DB2-OK                                              05990059
                    MOVE VXTB-TEXCAST             TO  WXTB-TEXCAST-003  06000059
               WHEN OTHER                                               06010059
                    MOVE 'VL4C3384'               TO  W801-PROGRAMA     06020059
                    MOVE 'VLDTXTB'                TO  W801-TABLA        06030059
                    MOVE 'SELECT      '           TO  W801-ACCION       06040059
                    MOVE VXTB-CODIGO              TO  W801-CLAVE (01:03)06050059
                    MOVE VXTB-SUBTIPO             TO  W801-CLAVE (05:03)06060059
                    MOVE  SQLCODE                 TO  W801-SQLCODE      06070059
                    MOVE  SPACES                  TO  W801-SQLWARN      06080059
                    MOVE '245-SELECT-VLDTXTB    ' TO  W801-PARRAFO      06090059
                    PERFORM  VLPC8010-DISP-ABEND-DB2                    06100059
                    PERFORM  VLPC8010-ABEND-DB2                         06110059
           END-EVALUATE.                                                06120059
      *                                                                *06130059
           MOVE '023'  TO VXTB-CODIGO.                                  06140059
           MOVE '000'  TO VXTB-SUBTIPO.                                 06150059
      *                                                                *06160059
           EXEC SQL                                                     06170059
                SELECT  VXTB_TEXCAST                                    06180059
                  INTO :VXTB-TEXCAST                                    06190059
                  FROM  VLDTXTB                                         06200059
                 WHERE  VXTB_CODIGO  = :VXTB-CODIGO                     06210059
                   AND  VXTB_SUBTIPO = :VXTB-SUBTIPO                    06220059
           END-EXEC.                                                    06230059
      *                                                                *06240059
           MOVE SQLCODE  TO  SQLCODE-AUX                                06250059
           EVALUATE TRUE                                                06260059
               WHEN DB2-OK                                              06270059
                    MOVE VXTB-TEXCAST             TO  WXTB-TEXCAST-023  06280059
               WHEN OTHER                                               06290059
                    MOVE 'VL4C3384'               TO  W801-PROGRAMA     06300059
                    MOVE 'VLDTXTB'                TO  W801-TABLA        06310059
                    MOVE 'SELECT      '           TO  W801-ACCION       06320059
                    MOVE VXTB-CODIGO              TO  W801-CLAVE (01:03)06330059
                    MOVE VXTB-SUBTIPO             TO  W801-CLAVE (05:03)06340059
                    MOVE  SQLCODE                 TO  W801-SQLCODE      06350059
                    MOVE  SPACES                  TO  W801-SQLWARN      06360059
                    MOVE '245-SELECT-VLDTXTB    ' TO  W801-PARRAFO      06370059
                    PERFORM  VLPC8010-DISP-ABEND-DB2                    06380059
                    PERFORM  VLPC8010-ABEND-DB2                         06390059
           END-EVALUATE.                                                06400059
      *                                                                *06410059
      *   *-------------*                                               06420059
       250-CALL-BG9CMDC0.                                               06430059
      *   *-------------*                                               06440059
      *                                                                *06450059
           CALL BG9CMDC0         USING  BGECMDC.                        06460059
      *                                                                *06470059
           EVALUATE MDC-CODERR                                          06480059
               WHEN SPACES                                              06490059
                    ADD  1                    TO LEI-BG9CMDC0           06500059
               WHEN OTHER                                               06510059
                    MOVE 'BG9CMDC0'           TO WRUT-PROGRAMA          06520059
                    MOVE MDC-CODERR           TO WRUT-RETOR             06530059
                    MOVE 'BUSCA CTA-REGISTRO' TO WRUT-ACCION            06540059
                    MOVE VARC-CUENTA          TO WRUT-CLAVE (01:07)     06550059
                    MOVE MDC-DATOS-ENT        TO WRUT-CLAVE (09:18)     06560059
                    MOVE '250-CALL-BG9CMDC0'  TO WRUT-PARRAFO           06570059
                    PERFORM VLPCRUTI-DISP-ABEND-RUTI                    06580059
                    PERFORM VLPCRUTI-ABEND-RUTI                         06590059
           END-EVALUATE.                                                06600059
      *                                                                *06610059
      *   *--------------*                                             *06620059
       260-REGISTRO-CARGO.                                              06630059
      *   *--------------*                                             *06640059
      ***                                                            ***06650059
      *   SALDO CONTABLE CERO PARA CUADRE CONTABLE E INVETARIO         *06660059
      ***                                                            ***06670059
      *                                                                *06680059
           INITIALIZE                      REGISTRO-BGECOPS.            06690059
           INITIALIZE                      REGISTRO-BGECMIR.            06700059
      *                                                                 06710059
           MOVE 'VL'                    TO OPS-CODAPLI.                 06720059
           MOVE MM-SYS                  TO OPS-NUMERO-FIC (01:02).      06730059
           MOVE DD-SYS                  TO OPS-NUMERO-FIC (03:02).      06740059
           MOVE WA-HORA (01:04)         TO OPS-NUMERO-FIC (05:04).      06750059
           MOVE 'R'                     TO OPS-SIGNO-OPER.              06760059
           MOVE '0'                     TO OPS-IND-REG.                 06770059
      *                                                                 06780059
      *MUEVE-CAMPOS-BGECMIR.                                            06790059
           MOVE VARC-FILLER (01:04)     TO MIR-ENTIDAD.                 06800059
           MOVE VARC-FILLER (05:04)     TO MIR-CENTRO-ALTA.             06810059
           MOVE VARC-FILLER (11:10)     TO MIR-CUENTA.                  06820059
           MOVE MDC-DIGICCC1            TO MIR-DIGICCC1.                06830059
           MOVE MDC-DIGICCC2            TO MIR-DIGICCC2.                06840059
           MOVE VARC-FILLER (01:04)     TO MIR-ENTIDAD-ORI.             06850059
      *200806010-INI                                                    06860059
      *    MOVE '0567'                  TO MIR-CENTRO-ORI.              06870059
           MOVE VARC-SUCURS             TO MIR-CENTRO-ORI.              06880059
      *200806010-FIN                                                    06890059
           MOVE 'VL4C3384'              TO MIR-USERID-ORI.              06900059
           MOVE '065'                   TO MIR-CODIGO.                  06910059
JPC@1 *    COMPUTE MIR-IMPORTE = MDC-SALDO-DISPUE * -1.                 06920059
JPC@1      COMPUTE MIR-IMPORTE = MDC-SALDO-DISPON * -1.                 06930059
           MOVE WA-FECPRO               TO MIR-FECHA-OPER.              06940059
           MOVE WA-HORA                 TO MIR-HORA-OPER.               06950059
           MOVE WA-FECPRO               TO MIR-FECHA-CONTA.             06960059
           MOVE VARC-CUENTA             TO MIR-REF-INTERNA (01:07).     06970059
           MOVE '00000000'              TO MIR-REF-INTERNA (08:08).     06980059
           MOVE SPACES                  TO MIR-CLAVE-LIG-SIE.           06990059
           MOVE 'OPS'                   TO MIR-TIPO-CONTAB.             07000059
           MOVE 'R'                     TO MIR-IND-REALPRUE.            07010059
           MOVE 'N'                     TO MIR-IND-OPER-INTER.          07020059
           MOVE '5'                     TO MIR-IND-ORIGEN-OP.           07030059
           MOVE MDC-CDDIVIS             TO MIR-DIVISA                   07040059
           MOVE WA-FECPRO               TO MIR-FECHA-VALOR.             07050059
           MOVE WA-MIR-OBSERVA-CAR      TO MIR-OBSERVA.                 07060059
           MOVE 'VL'                    TO MIR-APLC-ORI.                07070059
      *                                                                *07080059
           MOVE REGISTRO-BGECMIR        TO OPS-BGECMIR.                 07090059
      *                                                                *07100059
           WRITE REGISTRO-CARGO FROM REGISTRO-BGECOPS.                  07110059
      *                                                                *07120059
           EVALUATE FS-S1DQ3384                                         07130059
               WHEN '00'                                                07140059
                    ADD  1                     TO  WRI-CARGOS           07150059
               WHEN OTHER                                               07160059
                    MOVE 'S1DQ3384 '           TO  W802-FICHERO         07170059
                    MOVE  FS-S1DQ3384          TO  W802-STATUS          07180059
                    MOVE 'VL4C3384'            TO  W802-PROGRAMA        07190059
                    MOVE 'WRITE'               TO  W802-ACCION          07200059
                    MOVE VARC-CUENTA           TO  W802-CLAVE (01:07)   07210059
                    MOVE MIR-CCC               TO  W802-CLAVE (09:18)   07220059
                    MOVE ZEROS                 TO  W802-ABEND           07230059
                    MOVE '260-REGISTRO-CARGO'  TO  W802-PARRAFO         07240059
                    PERFORM VLPC8020-DISP-ABEND-FICH                    07250059
                    PERFORM VLPC8020-ABEND-FICH                         07260059
           END-EVALUATE.                                                07270059
      *                                                                *07280059
      ***                                                            ***07290059
      *   ABONO A CUENTA OPERATIVA DE CONTINENTAL BOLSA                *07300059
      ***                                                            ***07310059
      *                                                                *07320059
           INITIALIZE                      REGISTRO-BGECOPS.            07330059
           INITIALIZE                      REGISTRO-BGECMIR.            07340059
      *                                                                 07350059
           MOVE 'VL'                    TO OPS-CODAPLI.                 07360059
           MOVE MM-SYS                  TO OPS-NUMERO-FIC (01:02).      07370059
           MOVE DD-SYS                  TO OPS-NUMERO-FIC (03:02).      07380059
           MOVE WA-HORA (01:04)         TO OPS-NUMERO-FIC (05:04).      07390059
           MOVE 'I'                     TO OPS-SIGNO-OPER.              07400059
           MOVE '0'                     TO OPS-IND-REG.                 07410059
      *                                                                 07420059
      *MUEVE-CAMPOS-BGECMIR.                                            07430059
           IF MDC-CDDIVIS = 'PEN'                                       07440059
              MOVE VXBO-CTAECOS (01:04) TO MIR-ENTIDAD                  07450059
              MOVE VXBO-CTAECOS (05:04) TO MIR-CENTRO-ALTA              07460059
              MOVE VXBO-CTAECOS (11:10) TO MIR-CUENTA                   07470059
              MOVE VXBO-CTAECOS (09:01) TO MIR-DIGICCC1                 07480059
              MOVE VXBO-CTAECOS (10:01) TO MIR-DIGICCC2                 07490059
              MOVE VXBO-CTAECOS (01:04) TO MIR-ENTIDAD-ORI              07500059
              MOVE WMDC-OFICONT-PEN     TO MIR-CENTRO-ORI               07510059
           ELSE                                                         07520059
              MOVE VXBO-CTAECOD (01:04) TO MIR-ENTIDAD                  07530059
              MOVE VXBO-CTAECOD (05:04) TO MIR-CENTRO-ALTA              07540059
              MOVE VXBO-CTAECOD (11:10) TO MIR-CUENTA                   07550059
              MOVE VXBO-CTAECOD (09:01) TO MIR-DIGICCC1                 07560059
              MOVE VXBO-CTAECOD (10:01) TO MIR-DIGICCC2                 07570059
              MOVE VXBO-CTAECOD (01:04) TO MIR-ENTIDAD-ORI              07580059
              MOVE WMDC-OFICONT-USD     TO MIR-CENTRO-ORI               07590059
           END-IF                                                       07600059
      *200806010-INI                                                    07610059
      *    MOVE '0567'                  TO MIR-CENTRO-ORI.              07620059
      *200806010-FIN                                                    07630059
           MOVE 'VL4C3384'              TO MIR-USERID-ORI.              07640059
           MOVE '066'                   TO MIR-CODIGO.                  07650059
JPC@1 *    MOVE MDC-SALDO-DISPUE        TO MIR-IMPORTE.                 07660059
JPC@1      MOVE MDC-SALDO-DISPON        TO MIR-IMPORTE.                 07670059
           MOVE WA-FECPRO               TO MIR-FECHA-OPER.              07680059
           MOVE WA-HORA                 TO MIR-HORA-OPER.               07690059
           MOVE WA-FECPRO               TO MIR-FECHA-CONTA.             07700059
           MOVE VARC-CUENTA             TO MIR-REF-INTERNA (01:07).     07710059
           MOVE '00000000'              TO MIR-REF-INTERNA (08:08).     07720059
           MOVE SPACES                  TO MIR-CLAVE-LIG-SIE.           07730059
           MOVE 'OPS'                   TO MIR-TIPO-CONTAB.             07740059
           MOVE 'R'                     TO MIR-IND-REALPRUE.            07750059
           MOVE 'N'                     TO MIR-IND-OPER-INTER.          07760059
           MOVE '5'                     TO MIR-IND-ORIGEN-OP.           07770059
           MOVE MDC-CDDIVIS             TO MIR-DIVISA.                  07780059
           MOVE WA-FECPRO               TO MIR-FECHA-VALOR.             07790059
           MOVE WA-MIR-OBSERVA-CAR      TO MIR-OBSERVA.                 07800059
           MOVE 'VL'                    TO MIR-APLC-ORI.                07810059
      *                                                                *07820059
           MOVE REGISTRO-BGECMIR        TO OPS-BGECMIR.                 07830059
      *                                                                *07840059
           WRITE REGISTRO-CARGO FROM REGISTRO-BGECOPS.                  07850059
      *                                                                *07860059
           EVALUATE FS-S2DQ3384                                         07870059
               WHEN '00'                                                07880059
                    ADD  1                     TO  WRI-ABONOS           07890059
               WHEN OTHER                                               07900059
                    MOVE 'S2DQ3384 '           TO  W802-FICHERO         07910059
                    MOVE  FS-S2DQ3384          TO  W802-STATUS          07920059
                    MOVE 'VL4C3384'            TO  W802-PROGRAMA        07930059
                    MOVE 'WRITE'               TO  W802-ACCION          07940059
                    MOVE VARC-CUENTA           TO  W802-CLAVE (01:07)   07950059
                    MOVE MIR-CCC               TO  W802-CLAVE (09:18)   07960059
                    MOVE ZEROS                 TO  W802-ABEND           07970059
                    MOVE '260-REGISTRO-CARGO'  TO  W802-PARRAFO         07980059
                    PERFORM VLPC8020-DISP-ABEND-FICH                    07990059
                    PERFORM VLPC8020-ABEND-FICH                         08000059
           END-EVALUATE.                                                08010059
      *                                                                *08020059
      *   *--------------*                                             *08030059
       270-REGISTRO-ABONO.                                              08040059
      *   *--------------*                                             *08050059
      ***                                                            ***08060059
      *   RECUPERA SALDO CONTABLE LUEGO DEL CONTABLE VS INVETARIO      *08070059
      ***                                                            ***08080059
      *                                                                *08090059
           INITIALIZE                      REGISTRO-BGECOPS.            08100059
           INITIALIZE                      REGISTRO-BGECMIR.            08110059
      *                                                                 08120059
           MOVE 'VL'                    TO OPS-CODAPLI.                 08130059
           MOVE FECHA-MAS1 (05:04)      TO OPS-NUMERO-FIC (01:04).      08140059
           MOVE '0001'                  TO OPS-NUMERO-FIC (05:04).      08150059
           MOVE 'I'                     TO OPS-SIGNO-OPER.              08160059
           MOVE '0'                     TO OPS-IND-REG.                 08170059
      *                                                                 08180059
      *MUEVE-CAMPOS-BGECMIR.                                            08190059
           MOVE VARC-FILLER (01:04)     TO MIR-ENTIDAD.                 08200059
           MOVE VARC-FILLER (05:04)     TO MIR-CENTRO-ALTA.             08210059
           MOVE VARC-FILLER (11:10)     TO MIR-CUENTA.                  08220059
           MOVE MDC-DIGICCC1            TO MIR-DIGICCC1.                08230059
           MOVE MDC-DIGICCC2            TO MIR-DIGICCC2.                08240059
           MOVE VARC-FILLER (01:04)     TO MIR-ENTIDAD-ORI.             08250059
      *200806010-INI                                                    08260059
      *    MOVE '0567'                  TO MIR-CENTRO-ORI.              08270059
           MOVE VARC-SUCURS             TO MIR-CENTRO-ORI.              08280059
      *200806010-FIN                                                    08290059
           MOVE 'VL4C3384'              TO MIR-USERID-ORI.              08300059
           MOVE '066'                   TO MIR-CODIGO.                  08310059
JPC@1 *    MOVE MDC-SALDO-DISPUE        TO MIR-IMPORTE.                 08320059
JPC@1      MOVE MDC-SALDO-DISPON        TO MIR-IMPORTE.                 08330059
           MOVE WA-FECPRO               TO MIR-FECHA-OPER.              08340059
           MOVE WA-HORA                 TO MIR-HORA-OPER.               08350059
           MOVE WA-FECPRO               TO MIR-FECHA-CONTA.             08360059
           MOVE VARC-CUENTA             TO MIR-REF-INTERNA (01:07).     08370059
           MOVE '00000000'              TO MIR-REF-INTERNA (08:08).     08380059
           MOVE SPACES                  TO MIR-CLAVE-LIG-SIE.           08390059
           MOVE 'OPS'                   TO MIR-TIPO-CONTAB.             08400059
           MOVE 'R'                     TO MIR-IND-REALPRUE.            08410059
           MOVE 'N'                     TO MIR-IND-OPER-INTER.          08420059
           MOVE '5'                     TO MIR-IND-ORIGEN-OP.           08430059
           MOVE MDC-CDDIVIS             TO MIR-DIVISA.                  08440059
JPC@3 *    MOVE WA-FECPRO               TO MIR-FECHA-VALOR.             08450059
JPC@3      MOVE WA-FEMAS1               TO MIR-FECHA-VALOR.             08460059
           MOVE WA-MIR-OBSERVA-ABO      TO MIR-OBSERVA.                 08470059
           MOVE 'VL'                    TO MIR-APLC-ORI.                08480059
      *                                                                *08490059
           MOVE REGISTRO-BGECMIR        TO OPS-BGECMIR.                 08500059
      *                                                                *08510059
      *200807002-INI                                                    08520059
      *    WRITE REGISTRO-ABONO FROM REGISTRO-BGECOPS.                  08530059
           IF VARC-CUENTA = 269149                                      08540059
              MOVE '00'                 TO FS-S2DQ3384                  08550059
           ELSE                                                         08560059
              WRITE REGISTRO-ABONO FROM REGISTRO-BGECOPS                08570059
           END-IF.                                                      08580059
      *200807002-FIN                                                    08590059
      *                                                                *08600059
           EVALUATE FS-S2DQ3384                                         08610059
               WHEN '00'                                                08620059
                    ADD  1                     TO  WRI-ABONOS           08630059
               WHEN OTHER                                               08640059
                    MOVE 'S2DQ3384 '           TO  W802-FICHERO         08650059
                    MOVE  FS-S2DQ3384          TO  W802-STATUS          08660059
                    MOVE 'VL4C3384'            TO  W802-PROGRAMA        08670059
                    MOVE 'WRITE'               TO  W802-ACCION          08680059
                    MOVE VARC-CUENTA           TO  W802-CLAVE (01:07)   08690059
                    MOVE MIR-CCC               TO  W802-CLAVE (09:18)   08700059
                    MOVE ZEROS                 TO  W802-ABEND           08710059
                    MOVE '270-REGISTRO-ABONO'  TO  W802-PARRAFO         08720059
                    PERFORM VLPC8020-DISP-ABEND-FICH                    08730059
                    PERFORM VLPC8020-ABEND-FICH                         08740059
           END-EVALUATE.                                                08750059
      *                                                                *08760059
      ***                                                            ***08770059
      *   CARGO A CUENTA OPERATIVA DE CONTINENTAL BOLSA                *08780059
      ***                                                            ***08790059
      *                                                                *08800059
           INITIALIZE                      REGISTRO-BGECOPS.            08810059
           INITIALIZE                      REGISTRO-BGECMIR.            08820059
      *                                                                 08830059
           MOVE 'VL'                    TO OPS-CODAPLI.                 08840059
           MOVE FECHA-MAS1 (05:04)      TO OPS-NUMERO-FIC (01:04).      08850059
           MOVE '0001'                  TO OPS-NUMERO-FIC (05:04).      08860059
           MOVE 'R'                     TO OPS-SIGNO-OPER.              08870059
           MOVE '0'                     TO OPS-IND-REG.                 08880059
      *                                                                 08890059
      *MUEVE-CAMPOS-BGECMIR.                                            08900059
           IF MDC-CDDIVIS = 'PEN'                                       08910059
              MOVE VXBO-CTAECOS (01:04) TO MIR-ENTIDAD                  08920059
              MOVE VXBO-CTAECOS (05:04) TO MIR-CENTRO-ALTA              08930059
              MOVE VXBO-CTAECOS (11:10) TO MIR-CUENTA                   08940059
              MOVE VXBO-CTAECOS (09:01) TO MIR-DIGICCC1                 08950059
              MOVE VXBO-CTAECOS (10:01) TO MIR-DIGICCC2                 08960059
              MOVE VXBO-CTAECOS (01:04) TO MIR-ENTIDAD-ORI              08970059
              MOVE WMDC-OFICONT-PEN     TO MIR-CENTRO-ORI               08980059
           ELSE                                                         08990059
              MOVE VXBO-CTAECOD (01:04) TO MIR-ENTIDAD                  09000059
              MOVE VXBO-CTAECOD (05:04) TO MIR-CENTRO-ALTA              09010059
              MOVE VXBO-CTAECOD (11:10) TO MIR-CUENTA                   09020059
              MOVE VXBO-CTAECOD (09:01) TO MIR-DIGICCC1                 09030059
              MOVE VXBO-CTAECOD (10:01) TO MIR-DIGICCC2                 09040059
              MOVE VXBO-CTAECOD (01:04) TO MIR-ENTIDAD-ORI              09050059
              MOVE WMDC-OFICONT-USD     TO MIR-CENTRO-ORI               09060059
           END-IF                                                       09070059
      *200806010-INI                                                    09080059
      *    MOVE '0567'                  TO MIR-CENTRO-ORI.              09090059
      *200806010-FIN                                                    09100059
           MOVE 'VL4C3384'              TO MIR-USERID-ORI.              09110059
           MOVE '065'                   TO MIR-CODIGO.                  09120059
JPC@1 *    COMPUTE MIR-IMPORTE  = MDC-SALDO-DISPUE * -1.                09130059
JPC@       COMPUTE MIR-IMPORTE  = MDC-SALDO-DISPON * -1.                09140059
           MOVE WA-FECPRO               TO MIR-FECHA-OPER.              09150059
           MOVE WA-HORA                 TO MIR-HORA-OPER.               09160059
           MOVE WA-FECPRO               TO MIR-FECHA-CONTA.             09170059
           MOVE VARC-CUENTA             TO MIR-REF-INTERNA (01:07).     09180059
           MOVE '00000000'              TO MIR-REF-INTERNA (08:08).     09190059
           MOVE SPACES                  TO MIR-CLAVE-LIG-SIE.           09200059
           MOVE 'OPS'                   TO MIR-TIPO-CONTAB.             09210059
           MOVE 'R'                     TO MIR-IND-REALPRUE.            09220059
           MOVE 'N'                     TO MIR-IND-OPER-INTER.          09230059
           MOVE '5'                     TO MIR-IND-ORIGEN-OP.           09240059
           MOVE MDC-CDDIVIS             TO MIR-DIVISA                   09250059
JPC@3 *    MOVE WA-FECPRO               TO MIR-FECHA-VALOR.             09260059
JPC@3      MOVE WA-FEMAS1               TO MIR-FECHA-VALOR.             09270059
           MOVE WA-MIR-OBSERVA-ABO      TO MIR-OBSERVA.                 09280059
           MOVE 'VL'                    TO MIR-APLC-ORI.                09290059
      *                                                                *09300059
           MOVE REGISTRO-BGECMIR        TO OPS-BGECMIR.                 09310059
      *                                                                *09320059
      *200807002-INI                                                    09330059
      *    WRITE REGISTRO-ABONO FROM REGISTRO-BGECOPS.                  09340059
           IF VARC-CUENTA = 269149                                      09350059
              MOVE '00'                 TO FS-S2DQ3384                  09360059
           ELSE                                                         09370059
              WRITE REGISTRO-ABONO FROM REGISTRO-BGECOPS                09380059
           END-IF.                                                      09390059
      *200807002-FIN                                                    09400059
      *                                                                *09410059
           EVALUATE FS-S1DQ3384                                         09420059
               WHEN '00'                                                09430059
                    ADD  1                     TO  WRI-CARGOS           09440059
               WHEN OTHER                                               09450059
                    MOVE 'S1DQ3384 '           TO  W802-FICHERO         09460059
                    MOVE  FS-S1DQ3384          TO  W802-STATUS          09470059
                    MOVE 'VL4C3384'            TO  W802-PROGRAMA        09480059
                    MOVE 'WRITE'               TO  W802-ACCION          09490059
                    MOVE VARC-CUENTA           TO  W802-CLAVE (01:07)   09500059
                    MOVE MIR-CCC               TO  W802-CLAVE (09:18)   09510059
                    MOVE ZEROS                 TO  W802-ABEND           09520059
                    MOVE '270-REGISTRO-ABONO'  TO  W802-PARRAFO         09530059
                    PERFORM VLPC8020-DISP-ABEND-FICH                    09540059
                    PERFORM VLPC8020-ABEND-FICH                         09550059
           END-EVALUATE.                                                09560059
      *                                                                *09570059
      *   *--------------*                                             *09580059
       280-CONTABLE-CARGO.                                              09590059
      *   *--------------*                                             *09600059
      ****                                                          ****09610059
      *    CONTABLE POR EL CARGO A CUENTA REGISTRO                     *09620059
      ****                                                          ****09630059
           INITIALIZE                    HAEC040                        09640059
           MOVE '0011'                TO E040-EMPRESA                   09650059
           MOVE 'VL1'                 TO E040-CLVINT                    09660059
           MOVE WA-HOYAMD             TO E040-FECON                     09670059
           MOVE WA-HOYAMD             TO E040-FECOP                     09680059
           MOVE ZEROES                TO E040-HACCLPRO                  09690059
           MOVE ZEROES                TO E040-HACSBPRO                  09700059
           MOVE 'T5'                  TO E040-HACCOMPT(1:2)             09710059
           MOVE SPACES                TO E040-HAYCTORD                  09720059
           MOVE '0'                   TO E040-HATINVER                  09730059
           MOVE '000'                 TO E040-HACCOMPT(3:3)             09740059
           MOVE MDC-CDDIVIS           TO E040-HACDVISA                  09750059
           MOVE '0567'                TO E040-CEOPE                     09760059
           MOVE '0567'                TO E040-CEORIGEN                  09770059
           MOVE VARC-SUCURS           TO WMDC-OFICONT                   09780059
           MOVE WMDC-OFICONT          TO E040-CEDESTIN                  09790059
           MOVE 1                     TO E040-NUMOD                     09800059
           IF MDC-CDDIVIS = 'PEN'                                       09810059
JPC@1 *       MOVE MDC-SALDO-DISPUE   TO E040-IMPD                      09820059
JPC@1         MOVE MDC-SALDO-DISPON   TO E040-IMPD                      09830059
              MOVE ZEROS              TO E040-IMPDDIV                   09840059
              MOVE '0'                TO E040-HATDVISA                  09850059
           ELSE                                                         09860059
              MOVE ZEROS              TO E040-IMPD                      09870059
JPC@1 *       MOVE MDC-SALDO-DISPUE   TO E040-IMPDDIV                   09880059
JPC@1         MOVE MDC-SALDO-DISPON   TO E040-IMPDDIV                   09890059
              MOVE '1'                TO E040-HATDVISA                  09900059
           END-IF                                                       09910059
           MOVE 'ETE'                 TO E040-CLACON                    09920059
           MOVE WXTB-TEXCAST-023      TO E040-DESCON                    09930059
           MOVE '003'                 TO W-OPERAC                       09940059
           MOVE VARC-CUENTA           TO W-REFER-N                      09950059
           MOVE W-OBSERVACIONES       TO E040-OBSERV                    09960059
           MOVE '3384'                TO E040-HACTRGEN                  09970059
           MOVE SPACES                TO E040-HACEMTUT                  09980059
           MOVE VARC-CTAVAL20 (01:08) TO E040-SANCTCCC (01:08)          09990059
           MOVE VARC-CTAVAL20 (11:10) TO E040-SANCTCCC (09:10)          10000059
      *                                                                *10010059
           WRITE CONTABLE-CARGO FROM HAEC040.                           10020059
           IF FS-S3DQ3384 NOT = '00'                                    10030059
              MOVE  'S3DQ3384'           TO  W802-FICHERO               10040059
              MOVE  FS-S3DQ3384          TO  W802-STATUS                10050059
              MOVE  'VL4C3384'           TO  W802-PROGRAMA              10060059
              MOVE  'WRITE-CARGO'        TO  W802-ACCION                10070059
              MOVE  VARC-CUENTA          TO  W802-CLAVE                 10080059
              MOVE  ZEROS                TO  W802-ABEND                 10090059
              MOVE  '280-CONTABLE-CARGO' TO  W802-PARRAFO               10100059
              PERFORM VLPC8020-DISP-ABEND-FICH                          10110059
              PERFORM VLPC8020-ABEND-FICH                               10120059
           END-IF.                                                      10130059
           ADD  1                   TO WRI-CARGOS-CTBL.                 10140059
      *                                                                *10150059
      ****                                                          ****10160059
      *    CONTABLE POR EL ABONO A CUENTA OPERATIVA DE LA SAB          *10170059
      ****                                                          ****10180059
           INITIALIZE                    HAEC040                        10190059
           MOVE '0011'                TO E040-EMPRESA                   10200059
           MOVE 'VL1'                 TO E040-CLVINT                    10210059
           MOVE WA-HOYAMD             TO E040-FECON                     10220059
           MOVE WA-HOYAMD             TO E040-FECOP                     10230059
           MOVE ZEROES                TO E040-HACCLPRO                  10240059
           MOVE ZEROES                TO E040-HACSBPRO                  10250059
           MOVE 'T5'                  TO E040-HACCOMPT(1:2)             10260059
           MOVE SPACES                TO E040-HAYCTORD                  10270059
           MOVE '0'                   TO E040-HATINVER                  10280059
           MOVE '000'                 TO E040-HACCOMPT(3:3)             10290059
           MOVE MDC-CDDIVIS           TO E040-HACDVISA                  10300059
           MOVE '0567'                TO E040-CEOPE                     10310059
           MOVE '0567'                TO E040-CEORIGEN                  10320059
           MOVE 1                     TO E040-NUMOH                     10330059
           IF MDC-CDDIVIS = 'PEN'                                       10340059
JPC@1 *       MOVE MDC-SALDO-DISPUE   TO E040-IMPH                      10350059
JPC@1         MOVE MDC-SALDO-DISPON   TO E040-IMPH                      10360059
              MOVE ZEROS              TO E040-IMPHDIV                   10370059
              MOVE '0'                TO E040-HATDVISA                  10380059
              MOVE WMDC-OFICONT-PEN   TO E040-CEDESTIN                  10390059
           ELSE                                                         10400059
              MOVE ZEROS              TO E040-IMPH                      10410059
JPC@1 *       MOVE MDC-SALDO-DISPUE   TO E040-IMPHDIV                   10420059
JPC@1         MOVE MDC-SALDO-DISPON   TO E040-IMPHDIV                   10430059
              MOVE '1'                TO E040-HATDVISA                  10440059
              MOVE WMDC-OFICONT-USD   TO E040-CEDESTIN                  10450059
           END-IF                                                       10460059
           MOVE 'ETE'                 TO E040-CLACON                    10470059
           MOVE WXTB-TEXCAST-003      TO E040-DESCON                    10480059
           MOVE '003'                 TO W-OPERAC                       10490059
           MOVE VARC-CUENTA           TO W-REFER-N                      10500059
           MOVE W-OBSERVACIONES       TO E040-OBSERV                    10510059
           MOVE '3384'                TO E040-HACTRGEN                  10520059
           MOVE SPACES                TO E040-HACEMTUT                  10530059
           MOVE VARC-CTAVAL20 (01:08) TO E040-SANCTCCC (01:08)          10540059
           MOVE VARC-CTAVAL20 (11:10) TO E040-SANCTCCC (09:10)          10550059
      *                                                                *10560059
           WRITE CONTABLE-CARGO FROM HAEC040.                           10570059
           IF FS-S3DQ3384 NOT = '00'                                    10580059
              MOVE  'S3DQ3384'           TO  W802-FICHERO               10590059
              MOVE  FS-S3DQ3384          TO  W802-STATUS                10600059
              MOVE  'VL4C3384'           TO  W802-PROGRAMA              10610059
              MOVE  'WRITE-ABONO'        TO  W802-ACCION                10620059
              MOVE  VARC-CUENTA          TO  W802-CLAVE                 10630059
              MOVE  ZEROS                TO  W802-ABEND                 10640059
              MOVE  '280-CONTABLE-CARGO' TO  W802-PARRAFO               10650059
              PERFORM VLPC8020-DISP-ABEND-FICH                          10660059
              PERFORM VLPC8020-ABEND-FICH                               10670059
           END-IF.                                                      10680059
           ADD  1                   TO WRI-CARGOS-CTBL.                 10690059
      *                                                                *10700059
      *   *--------------*                                             *10710059
       290-CONTABLE-ABONO.                                              10720059
      *   *--------------*                                             *10730059
      ****                                                          ****10740059
      *    CONTABLE POR EL ABONO A CUENTA REGISTRO                     *10750059
      ****                                                          ****10760059
           INITIALIZE                    HAEC040                        10770059
           MOVE '0011'                TO E040-EMPRESA                   10780059
           MOVE 'VL1'                 TO E040-CLVINT                    10790059
           MOVE FECHA-MAS1            TO E040-FECON                     10800059
           MOVE FECHA-MAS1            TO E040-FECOP                     10810059
           MOVE ZEROES                TO E040-HACCLPRO                  10820059
           MOVE ZEROES                TO E040-HACSBPRO                  10830059
           MOVE 'T5'                  TO E040-HACCOMPT(1:2)             10840059
           MOVE SPACES                TO E040-HAYCTORD                  10850059
           MOVE '0'                   TO E040-HATINVER                  10860059
           MOVE '000'                 TO E040-HACCOMPT(3:3)             10870059
           MOVE MDC-CDDIVIS           TO E040-HACDVISA                  10880059
           MOVE '0567'                TO E040-CEOPE                     10890059
           MOVE '0567'                TO E040-CEORIGEN                  10900059
           MOVE VARC-SUCURS           TO WMDC-OFICONT                   10910059
           MOVE WMDC-OFICONT          TO E040-CEDESTIN                  10920059
           MOVE 1                     TO E040-NUMOH                     10930059
           IF MDC-CDDIVIS = 'PEN'                                       10940059
JPC@1 *       MOVE MDC-SALDO-DISPUE   TO E040-IMPH                      10950059
JPC@1         MOVE MDC-SALDO-DISPON   TO E040-IMPH                      10960059
              MOVE ZEROS              TO E040-IMPHDIV                   10970059
              MOVE '0'                TO E040-HATDVISA                  10980059
           ELSE                                                         10990059
              MOVE ZEROS              TO E040-IMPH                      11000059
JPC@1 *       MOVE MDC-SALDO-DISPUE   TO E040-IMPHDIV                   11010059
JPC@1         MOVE MDC-SALDO-DISPON   TO E040-IMPHDIV                   11020059
              MOVE '1'                TO E040-HATDVISA                  11030059
           END-IF                                                       11040059
           MOVE 'STE'                 TO E040-CLACON                    11050059
           MOVE WXTB-TEXCAST-003      TO E040-DESCON                    11060059
           MOVE '023'                 TO W-OPERAC                       11070059
           MOVE VARC-CUENTA           TO W-REFER-N                      11080059
           MOVE W-OBSERVACIONES       TO E040-OBSERV                    11090059
           MOVE '3384'                TO E040-HACTRGEN                  11100059
           MOVE SPACES                TO E040-HACEMTUT                  11110059
           MOVE VARC-CTAVAL20 (01:08) TO E040-SANCTCCC (01:08)          11120059
           MOVE VARC-CTAVAL20 (11:10) TO E040-SANCTCCC (09:10)          11130059
      *                                                                *11140059
      *200807002-INI                                                    11150059
      *    WRITE CONTABLE-ABONO FROM HAEC040.                           11160059
           IF VARC-CUENTA = 269149                                      11170059
              MOVE '00'                 TO   FS-S4DQ3384                11180059
           ELSE                                                         11190059
              WRITE CONTABLE-ABONO FROM HAEC040                         11200059
           END-IF.                                                      11210059
      *200807002-FIN                                                    11220059
      *                                                                *11230059
           IF FS-S4DQ3384 NOT = '00'                                    11240059
              MOVE  'S3DQ3384'           TO  W802-FICHERO               11250059
              MOVE  FS-S4DQ3384          TO  W802-STATUS                11260059
              MOVE  'VL4C3384'           TO  W802-PROGRAMA              11270059
              MOVE  'WRITE-ABONO'        TO  W802-ACCION                11280059
              MOVE  VARC-CUENTA          TO  W802-CLAVE                 11290059
              MOVE  ZEROS                TO  W802-ABEND                 11300059
              MOVE  '290-CONTABLE-CARGO' TO  W802-PARRAFO               11310059
              PERFORM VLPC8020-DISP-ABEND-FICH                          11320059
              PERFORM VLPC8020-ABEND-FICH                               11330059
           END-IF.                                                      11340059
           ADD  1                   TO WRI-ABONOS-CTBL.                 11350059
      *                                                                *11360059
      ****                                                          ****11370059
      *    CONTABLE POR EL CARGO A CUENTA OPERATIVA DE LA SAB          *11380059
      ****                                                          ****11390059
           INITIALIZE                    HAEC040                        11400059
           MOVE '0011'                TO E040-EMPRESA                   11410059
           MOVE 'VL1'                 TO E040-CLVINT                    11420059
           MOVE FECHA-MAS1            TO E040-FECON                     11430059
           MOVE FECHA-MAS1            TO E040-FECOP                     11440059
           MOVE ZEROES                TO E040-HACCLPRO                  11450059
           MOVE ZEROES                TO E040-HACSBPRO                  11460059
           MOVE 'T5'                  TO E040-HACCOMPT(1:2)             11470059
           MOVE SPACES                TO E040-HAYCTORD                  11480059
           MOVE '0'                   TO E040-HATINVER                  11490059
           MOVE '000'                 TO E040-HACCOMPT(3:3)             11500059
           MOVE MDC-CDDIVIS           TO E040-HACDVISA                  11510059
           MOVE '0567'                TO E040-CEOPE                     11520059
           MOVE '0567'                TO E040-CEORIGEN                  11530059
           MOVE 1                     TO E040-NUMOD                     11540059
           IF MDC-CDDIVIS = 'PEN'                                       11550059
JPC@1 *       MOVE MDC-SALDO-DISPUE   TO E040-IMPD                      11560059
JPC@1         MOVE MDC-SALDO-DISPON   TO E040-IMPD                      11570059
              MOVE ZEROS              TO E040-IMPDDIV                   11580059
              MOVE '0'                TO E040-HATDVISA                  11590059
              MOVE WMDC-OFICONT-PEN   TO E040-CEDESTIN                  11600059
           ELSE                                                         11610059
              MOVE ZEROS              TO E040-IMPD                      11620059
JPC@1 *       MOVE MDC-SALDO-DISPUE   TO E040-IMPDDIV                   11630059
JPC@1         MOVE MDC-SALDO-DISPON   TO E040-IMPDDIV                   11640059
              MOVE '1'                TO E040-HATDVISA                  11650059
              MOVE WMDC-OFICONT-USD   TO E040-CEDESTIN                  11660059
           END-IF                                                       11670059
           MOVE 'STE'                 TO E040-CLACON                    11680059
           MOVE WXTB-TEXCAST-023      TO E040-DESCON                    11690059
           MOVE '023'                 TO W-OPERAC                       11700059
           MOVE VARC-CUENTA           TO W-REFER-N                      11710059
           MOVE W-OBSERVACIONES       TO E040-OBSERV                    11720059
           MOVE '3384'                TO E040-HACTRGEN                  11730059
           MOVE SPACES                TO E040-HACEMTUT                  11740059
           MOVE VARC-CTAVAL20 (01:08) TO E040-SANCTCCC (01:08)          11750059
           MOVE VARC-CTAVAL20 (11:10) TO E040-SANCTCCC (09:10)          11760059
      *                                                                *11770059
      *200807002-INI                                                    11780059
      *    WRITE CONTABLE-ABONO FROM HAEC040.                           11790059
           IF VARC-CUENTA = 269149                                      11800059
              MOVE '00'                  TO   FS-S4DQ3384               11810059
           ELSE                                                         11820059
              WRITE CONTABLE-ABONO FROM HAEC040                         11830059
           END-IF.                                                      11840059
      *200807002-FIN                                                    11850059
           IF FS-S4DQ3384 NOT = '00'                                    11860059
              MOVE  'S3DQ3384'           TO  W802-FICHERO               11870059
              MOVE  FS-S4DQ3384          TO  W802-STATUS                11880059
              MOVE  'VL4C3384'           TO  W802-PROGRAMA              11890059
              MOVE  'WRITE-CARGO'        TO  W802-ACCION                11900059
              MOVE  VARC-CUENTA          TO  W802-CLAVE                 11910059
              MOVE  ZEROS                TO  W802-ABEND                 11920059
              MOVE  '290-CONTABLE-CARGO' TO  W802-PARRAFO               11930059
              PERFORM VLPC8020-DISP-ABEND-FICH                          11940059
              PERFORM VLPC8020-ABEND-FICH                               11950059
           END-IF.                                                      11960059
           ADD  1                   TO WRI-ABONOS-CTBL.                 11970059
      *                                                                *11980059
      *200804248-INI                                                   *11990059
      *   *------------------*                                         *12000059
       260-REGISTRO-ABONO-NEG.                                          12010059
      *   *------------------*                                         *12020059
      ***                                                            ***12030059
      *   SALDO CONTABLE CERO PARA CUADRE CONTABLE E INVETARIO         *12040059
      ***                                                            ***12050059
      *                                                                *12060059
           INITIALIZE                      REGISTRO-BGECOPS.            12070059
           INITIALIZE                      REGISTRO-BGECMIR.            12080059
      *                                                                 12090059
           MOVE 'VL'                    TO OPS-CODAPLI.                 12100059
           MOVE MM-SYS                  TO OPS-NUMERO-FIC (01:02).      12110059
           MOVE DD-SYS                  TO OPS-NUMERO-FIC (03:02).      12120059
           MOVE WA-HORA (01:04)         TO OPS-NUMERO-FIC (05:04).      12130059
           MOVE 'I'                     TO OPS-SIGNO-OPER.              12140059
           MOVE '0'                     TO OPS-IND-REG.                 12150059
      *                                                                 12160059
      *MUEVE-CAMPOS-BGECMIR.                                            12170059
           MOVE VARC-FILLER (01:04)     TO MIR-ENTIDAD.                 12180059
           MOVE VARC-FILLER (05:04)     TO MIR-CENTRO-ALTA.             12190059
           MOVE VARC-FILLER (11:10)     TO MIR-CUENTA.                  12200059
           MOVE MDC-DIGICCC1            TO MIR-DIGICCC1.                12210059
           MOVE MDC-DIGICCC2            TO MIR-DIGICCC2.                12220059
           MOVE VARC-FILLER (01:04)     TO MIR-ENTIDAD-ORI.             12230059
           MOVE VARC-SUCURS             TO MIR-CENTRO-ORI.              12240059
           MOVE 'VL4C3384'              TO MIR-USERID-ORI.              12250059
           MOVE '066'                   TO MIR-CODIGO.                  12260059
           COMPUTE MIR-IMPORTE  = MDC-SALDO-DISPON * -1.                12270059
           MOVE WA-FECPRO               TO MIR-FECHA-OPER.              12280059
           MOVE WA-HORA                 TO MIR-HORA-OPER.               12290059
           MOVE WA-FECPRO               TO MIR-FECHA-CONTA.             12300059
           MOVE VARC-CUENTA             TO MIR-REF-INTERNA (01:07).     12310059
           MOVE '00000000'              TO MIR-REF-INTERNA (08:08).     12320059
           MOVE SPACES                  TO MIR-CLAVE-LIG-SIE.           12330059
           MOVE 'OPS'                   TO MIR-TIPO-CONTAB.             12340059
           MOVE 'R'                     TO MIR-IND-REALPRUE.            12350059
           MOVE 'N'                     TO MIR-IND-OPER-INTER.          12360059
           MOVE '5'                     TO MIR-IND-ORIGEN-OP.           12370059
           MOVE MDC-CDDIVIS             TO MIR-DIVISA.                  12380059
           MOVE WA-FECPRO               TO MIR-FECHA-VALOR.             12390059
           MOVE WA-MIR-OBSERVA-ABO      TO MIR-OBSERVA.                 12400059
           MOVE 'TRAS-NEG'              TO MIR-OBSERVA (01:08)          12410059
           MOVE 'VL'                    TO MIR-APLC-ORI.                12420059
      *                                                                *12430059
           MOVE REGISTRO-BGECMIR        TO OPS-BGECMIR.                 12440059
      *                                                                *12450059
           WRITE REGISTRO-CARGO FROM REGISTRO-BGECOPS                   12460059
      *                                                                *12470059
           EVALUATE FS-S1DQ3384                                         12480059
               WHEN '00'                                                12490059
                    ADD  1                     TO  WRI-CARGOS           12500059
               WHEN OTHER                                               12510059
                    MOVE 'S1DQ3384 '           TO  W802-FICHERO         12520059
                    MOVE  FS-S1DQ3384          TO  W802-STATUS          12530059
                    MOVE 'VL4C3384'            TO  W802-PROGRAMA        12540059
                    MOVE 'WRITE'               TO  W802-ACCION          12550059
                    MOVE VARC-CUENTA           TO  W802-CLAVE (01:07)   12560059
                    MOVE MIR-CCC               TO  W802-CLAVE (09:18)   12570059
                    MOVE ZEROS                 TO  W802-ABEND           12580059
                    MOVE '260-REGISTRO-ABONO-NEG'  TO  W802-PARRAFO     12590059
                    PERFORM VLPC8020-DISP-ABEND-FICH                    12600059
                    PERFORM VLPC8020-ABEND-FICH                         12610059
           END-EVALUATE.                                                12620059
      *                                                                *12630059
      ***                                                            ***12640059
      *   CARGO A CUENTA OPERATIVA DE CONTINENTAL BOLSA                *12650059
      ***                                                            ***12660059
      *                                                                *12670059
           INITIALIZE                      REGISTRO-BGECOPS.            12680059
           INITIALIZE                      REGISTRO-BGECMIR.            12690059
      *                                                                 12700059
           MOVE 'VL'                    TO OPS-CODAPLI.                 12710059
           MOVE MM-SYS                  TO OPS-NUMERO-FIC (01:02).      12720059
           MOVE DD-SYS                  TO OPS-NUMERO-FIC (03:02).      12730059
           MOVE WA-HORA (01:04)         TO OPS-NUMERO-FIC (05:04).      12740059
           MOVE 'R'                     TO OPS-SIGNO-OPER.              12750059
           MOVE '0'                     TO OPS-IND-REG.                 12760059
      *                                                                 12770059
      *MUEVE-CAMPOS-BGECMIR.                                            12780059
           IF MDC-CDDIVIS = 'PEN'                                       12790059
              MOVE VXBO-CTAECOS (01:04) TO MIR-ENTIDAD                  12800059
              MOVE VXBO-CTAECOS (05:04) TO MIR-CENTRO-ALTA              12810059
              MOVE VXBO-CTAECOS (11:10) TO MIR-CUENTA                   12820059
              MOVE VXBO-CTAECOS (09:01) TO MIR-DIGICCC1                 12830059
              MOVE VXBO-CTAECOS (10:01) TO MIR-DIGICCC2                 12840059
              MOVE VXBO-CTAECOS (01:04) TO MIR-ENTIDAD-ORI              12850059
              MOVE WMDC-OFICONT-PEN     TO MIR-CENTRO-ORI               12860059
           ELSE                                                         12870059
              MOVE VXBO-CTAECOD (01:04) TO MIR-ENTIDAD                  12880059
              MOVE VXBO-CTAECOD (05:04) TO MIR-CENTRO-ALTA              12890059
              MOVE VXBO-CTAECOD (11:10) TO MIR-CUENTA                   12900059
              MOVE VXBO-CTAECOD (09:01) TO MIR-DIGICCC1                 12910059
              MOVE VXBO-CTAECOD (10:01) TO MIR-DIGICCC2                 12920059
              MOVE VXBO-CTAECOD (01:04) TO MIR-ENTIDAD-ORI              12930059
              MOVE WMDC-OFICONT-USD     TO MIR-CENTRO-ORI               12940059
           END-IF                                                       12950059
           MOVE 'VL4C3384'              TO MIR-USERID-ORI.              12960059
           MOVE '065'                   TO MIR-CODIGO.                  12970059
           MOVE MDC-SALDO-DISPON        TO MIR-IMPORTE.                 12980059
           MOVE WA-FECPRO               TO MIR-FECHA-OPER.              12990059
           MOVE WA-HORA                 TO MIR-HORA-OPER.               13000059
           MOVE WA-FECPRO               TO MIR-FECHA-CONTA.             13010059
           MOVE VARC-CUENTA             TO MIR-REF-INTERNA (01:07).     13020059
           MOVE '00000000'              TO MIR-REF-INTERNA (08:08).     13030059
           MOVE SPACES                  TO MIR-CLAVE-LIG-SIE.           13040059
           MOVE 'OPS'                   TO MIR-TIPO-CONTAB.             13050059
           MOVE 'R'                     TO MIR-IND-REALPRUE.            13060059
           MOVE 'N'                     TO MIR-IND-OPER-INTER.          13070059
           MOVE '5'                     TO MIR-IND-ORIGEN-OP.           13080059
           MOVE MDC-CDDIVIS             TO MIR-DIVISA                   13090059
           MOVE WA-FECPRO               TO MIR-FECHA-VALOR.             13100059
           MOVE WA-MIR-OBSERVA-ABO      TO MIR-OBSERVA.                 13110059
           MOVE 'TRAS-NEG'              TO MIR-OBSERVA (01:08)          13120059
           MOVE 'VL'                    TO MIR-APLC-ORI.                13130059
      *                                                                *13140059
           MOVE REGISTRO-BGECMIR        TO OPS-BGECMIR.                 13150059
      *                                                                *13160059
           WRITE REGISTRO-CARGO FROM REGISTRO-BGECOPS.                  13170059
      *                                                                *13180059
           EVALUATE FS-S1DQ3384                                         13190059
               WHEN '00'                                                13200059
                    ADD  1                     TO  WRI-CARGOS           13210059
               WHEN OTHER                                               13220059
                    MOVE 'S1DQ3384 '           TO  W802-FICHERO         13230059
                    MOVE  FS-S1DQ3384          TO  W802-STATUS          13240059
                    MOVE 'VL4C3384'            TO  W802-PROGRAMA        13250059
                    MOVE 'WRITE'               TO  W802-ACCION          13260059
                    MOVE VARC-CUENTA           TO  W802-CLAVE (01:07)   13270059
                    MOVE MIR-CCC               TO  W802-CLAVE (09:18)   13280059
                    MOVE ZEROS                 TO  W802-ABEND           13290059
                    MOVE '260-REGISTRO-ABONO-NEG'  TO  W802-PARRAFO     13300059
                    PERFORM VLPC8020-DISP-ABEND-FICH                    13310059
                    PERFORM VLPC8020-ABEND-FICH                         13320059
           END-EVALUATE.                                                13330059
      *                                                                *13340059
      *200804248-INI                                                   *13350059
      *200804248-FIN                                                   *13360059
      *   *------------------*                                         *13370059
       270-REGISTRO-CARGO-NEG.                                          13380059
      *   *------------------*                                         *13390059
      ***                                                            ***13400059
      *   RECUPERA SALDO CONTABLE LUEGO DEL CONTABLE VS INVETARIO      *13410059
      ***                                                            ***13420059
      *                                                                *13430059
           INITIALIZE                      REGISTRO-BGECOPS.            13440059
           INITIALIZE                      REGISTRO-BGECMIR.            13450059
      *                                                                 13460059
           MOVE 'VL'                    TO OPS-CODAPLI.                 13470059
           MOVE FECHA-MAS1 (05:04)      TO OPS-NUMERO-FIC (01:04).      13480059
           MOVE '0001'                  TO OPS-NUMERO-FIC (05:04).      13490059
           MOVE 'R'                     TO OPS-SIGNO-OPER.              13500059
           MOVE '0'                     TO OPS-IND-REG.                 13510059
      *                                                                 13520059
      *MUEVE-CAMPOS-BGECMIR.                                            13530059
           MOVE VARC-FILLER (01:04)     TO MIR-ENTIDAD.                 13540059
           MOVE VARC-FILLER (05:04)     TO MIR-CENTRO-ALTA.             13550059
           MOVE VARC-FILLER (11:10)     TO MIR-CUENTA.                  13560059
           MOVE MDC-DIGICCC1            TO MIR-DIGICCC1.                13570059
           MOVE MDC-DIGICCC2            TO MIR-DIGICCC2.                13580059
           MOVE VARC-FILLER (01:04)     TO MIR-ENTIDAD-ORI.             13590059
           MOVE VARC-SUCURS             TO MIR-CENTRO-ORI.              13600059
           MOVE 'VL4C3384'              TO MIR-USERID-ORI.              13610059
           MOVE '065'                   TO MIR-CODIGO.                  13620059
           MOVE MDC-SALDO-DISPON        TO MIR-IMPORTE.                 13630059
           MOVE WA-FECPRO               TO MIR-FECHA-OPER.              13640059
           MOVE WA-HORA                 TO MIR-HORA-OPER.               13650059
           MOVE WA-FECPRO               TO MIR-FECHA-CONTA.             13660059
           MOVE VARC-CUENTA             TO MIR-REF-INTERNA (01:07).     13670059
           MOVE '00000000'              TO MIR-REF-INTERNA (08:08).     13680059
           MOVE SPACES                  TO MIR-CLAVE-LIG-SIE.           13690059
           MOVE 'OPS'                   TO MIR-TIPO-CONTAB.             13700059
           MOVE 'R'                     TO MIR-IND-REALPRUE.            13710059
           MOVE 'N'                     TO MIR-IND-OPER-INTER.          13720059
           MOVE '5'                     TO MIR-IND-ORIGEN-OP.           13730059
           MOVE MDC-CDDIVIS             TO MIR-DIVISA                   13740059
JPC@3 *    MOVE WA-FECPRO               TO MIR-FECHA-VALOR.             13750059
JPC@3      MOVE WA-FEMAS1               TO MIR-FECHA-VALOR.             13760059
           MOVE WA-MIR-OBSERVA-CAR      TO MIR-OBSERVA.                 13770059
           MOVE 'TRAS-NEG'              TO MIR-OBSERVA (01:08)          13780059
           MOVE 'VL'                    TO MIR-APLC-ORI.                13790059
      *PARA TENER SALDO NEGATIVO                                        13800059
           MOVE 'BGE0515'               TO MIR-CODI-ERROR (04)          13810059
           MOVE -9999999999999.99       TO W-LIMITE-AUT-9               13820059
           MOVE W-LIMITE-AUT            TO MIR-SITU-ERROR (04)          13830059
      *                                                                *13840059
           MOVE REGISTRO-BGECMIR        TO OPS-BGECMIR.                 13850059
      *                                                                *13860059
           WRITE REGISTRO-ABONO FROM REGISTRO-BGECOPS.                  13870059
      *                                                                *13880059
           EVALUATE FS-S2DQ3384                                         13890059
               WHEN '00'                                                13900059
                    ADD  1                     TO  WRI-ABONOS           13910059
               WHEN OTHER                                               13920059
                    MOVE 'S2DQ3384 '           TO  W802-FICHERO         13930059
                    MOVE  FS-S2DQ3384          TO  W802-STATUS          13940059
                    MOVE 'VL4C3384'            TO  W802-PROGRAMA        13950059
                    MOVE 'WRITE'               TO  W802-ACCION          13960059
                    MOVE VARC-CUENTA           TO  W802-CLAVE (01:07)   13970059
                    MOVE MIR-CCC               TO  W802-CLAVE (09:18)   13980059
                    MOVE ZEROS                 TO  W802-ABEND           13990059
                    MOVE '270-REGISTRO-CARGO-NEG'  TO  W802-PARRAFO     14000059
                    PERFORM VLPC8020-DISP-ABEND-FICH                    14010059
                    PERFORM VLPC8020-ABEND-FICH                         14020059
           END-EVALUATE.                                                14030059
      *                                                                *14040059
      ***                                                            ***14050059
      *   ABONO A CUENTA OPERATIVA DE CONTINENTAL BOLSA                *14060059
      ***                                                            ***14070059
      *                                                                *14080059
           INITIALIZE                      REGISTRO-BGECOPS.            14090059
           INITIALIZE                      REGISTRO-BGECMIR.            14100059
      *                                                                 14110059
           MOVE 'VL'                    TO OPS-CODAPLI.                 14120059
           MOVE FECHA-MAS1 (05:04)      TO OPS-NUMERO-FIC (01:04).      14130059
           MOVE '0001'                  TO OPS-NUMERO-FIC (05:04).      14140059
           MOVE 'I'                     TO OPS-SIGNO-OPER.              14150059
           MOVE '0'                     TO OPS-IND-REG.                 14160059
      *                                                                 14170059
      *MUEVE-CAMPOS-BGECMIR.                                            14180059
           IF MDC-CDDIVIS = 'PEN'                                       14190059
              MOVE VXBO-CTAECOS (01:04) TO MIR-ENTIDAD                  14200059
              MOVE VXBO-CTAECOS (05:04) TO MIR-CENTRO-ALTA              14210059
              MOVE VXBO-CTAECOS (11:10) TO MIR-CUENTA                   14220059
              MOVE VXBO-CTAECOS (09:01) TO MIR-DIGICCC1                 14230059
              MOVE VXBO-CTAECOS (10:01) TO MIR-DIGICCC2                 14240059
              MOVE VXBO-CTAECOS (01:04) TO MIR-ENTIDAD-ORI              14250059
              MOVE WMDC-OFICONT-PEN     TO MIR-CENTRO-ORI               14260059
           ELSE                                                         14270059
              MOVE VXBO-CTAECOD (01:04) TO MIR-ENTIDAD                  14280059
              MOVE VXBO-CTAECOD (05:04) TO MIR-CENTRO-ALTA              14290059
              MOVE VXBO-CTAECOD (11:10) TO MIR-CUENTA                   14300059
              MOVE VXBO-CTAECOD (09:01) TO MIR-DIGICCC1                 14310059
              MOVE VXBO-CTAECOD (10:01) TO MIR-DIGICCC2                 14320059
              MOVE VXBO-CTAECOD (01:04) TO MIR-ENTIDAD-ORI              14330059
              MOVE WMDC-OFICONT-USD     TO MIR-CENTRO-ORI               14340059
           END-IF                                                       14350059
           MOVE 'VL4C3384'              TO MIR-USERID-ORI.              14360059
           MOVE '066'                   TO MIR-CODIGO.                  14370059
           COMPUTE MIR-IMPORTE = MDC-SALDO-DISPON * -1.                 14380059
           MOVE WA-FECPRO               TO MIR-FECHA-OPER.              14390059
           MOVE WA-HORA                 TO MIR-HORA-OPER.               14400059
           MOVE WA-FECPRO               TO MIR-FECHA-CONTA.             14410059
           MOVE VARC-CUENTA             TO MIR-REF-INTERNA (01:07).     14420059
           MOVE '00000000'              TO MIR-REF-INTERNA (08:08).     14430059
           MOVE SPACES                  TO MIR-CLAVE-LIG-SIE.           14440059
           MOVE 'OPS'                   TO MIR-TIPO-CONTAB.             14450059
           MOVE 'R'                     TO MIR-IND-REALPRUE.            14460059
           MOVE 'N'                     TO MIR-IND-OPER-INTER.          14470059
           MOVE '5'                     TO MIR-IND-ORIGEN-OP.           14480059
           MOVE MDC-CDDIVIS             TO MIR-DIVISA.                  14490059
JPC@3 *    MOVE WA-FECPRO               TO MIR-FECHA-VALOR.             14500059
JPC@3      MOVE WA-FEMAS1               TO MIR-FECHA-VALOR.             14510059
           MOVE WA-MIR-OBSERVA-CAR      TO MIR-OBSERVA.                 14520059
           MOVE 'TRAS-NEG'              TO MIR-OBSERVA (01:08)          14530059
           MOVE 'VL'                    TO MIR-APLC-ORI.                14540059
      *                                                                *14550059
           MOVE REGISTRO-BGECMIR        TO OPS-BGECMIR.                 14560059
      *                                                                *14570059
           WRITE REGISTRO-ABONO FROM REGISTRO-BGECOPS.                  14580059
      *                                                                *14590059
           EVALUATE FS-S2DQ3384                                         14600059
               WHEN '00'                                                14610059
                    ADD  1                     TO  WRI-ABONOS           14620059
               WHEN OTHER                                               14630059
                    MOVE 'S2DQ3384 '           TO  W802-FICHERO         14640059
                    MOVE  FS-S2DQ3384          TO  W802-STATUS          14650059
                    MOVE 'VL4C3384'            TO  W802-PROGRAMA        14660059
                    MOVE 'WRITE'               TO  W802-ACCION          14670059
                    MOVE VARC-CUENTA           TO  W802-CLAVE (01:07)   14680059
                    MOVE MIR-CCC               TO  W802-CLAVE (09:18)   14690059
                    MOVE ZEROS                 TO  W802-ABEND           14700059
                    MOVE '270-REGISTRO-ABONO-NEG'  TO  W802-PARRAFO     14710059
                    PERFORM VLPC8020-DISP-ABEND-FICH                    14720059
                    PERFORM VLPC8020-ABEND-FICH                         14730059
           END-EVALUATE.                                                14740059
      *                                                                *14750059
      *200804248-INI                                                   *14760059
      *200804248-FIN                                                   *14770059
      *   *------------------*                                         *14780059
       280-CONTABLE-ABONO-NEG.                                          14790059
      *   *------------------*                                         *14800059
      ****                                                          ****14810059
      *    CONTABLE POR EL ABONO A CUENTA REGISTRO                     *14820059
      ****                                                          ****14830059
           INITIALIZE                    HAEC040                        14840059
           MOVE '0011'                TO E040-EMPRESA                   14850059
           MOVE 'VL1'                 TO E040-CLVINT                    14860059
           MOVE WA-HOYAMD             TO E040-FECON                     14870059
           MOVE WA-HOYAMD             TO E040-FECOP                     14880059
           MOVE ZEROES                TO E040-HACCLPRO                  14890059
           MOVE ZEROES                TO E040-HACSBPRO                  14900059
           MOVE 'T5'                  TO E040-HACCOMPT(1:2)             14910059
           MOVE SPACES                TO E040-HAYCTORD                  14920059
           MOVE '0'                   TO E040-HATINVER                  14930059
           MOVE '000'                 TO E040-HACCOMPT(3:3)             14940059
           MOVE MDC-CDDIVIS           TO E040-HACDVISA                  14950059
           MOVE '0567'                TO E040-CEOPE                     14960059
           MOVE '0567'                TO E040-CEORIGEN                  14970059
           MOVE VARC-SUCURS           TO WMDC-OFICONT                   14980059
           MOVE WMDC-OFICONT          TO E040-CEDESTIN                  14990059
           MOVE 1                     TO E040-NUMOH                     15000059
           IF MDC-CDDIVIS = 'PEN'                                       15010059
              MOVE MDC-SALDO-DISPON   TO E040-IMPH                      15020059
              MOVE ZEROS              TO E040-IMPHDIV                   15030059
              MOVE '0'                TO E040-HATDVISA                  15040059
           ELSE                                                         15050059
              MOVE ZEROS              TO E040-IMPH                      15060059
              MOVE MDC-SALDO-DISPON   TO E040-IMPHDIV                   15070059
              MOVE '1'                TO E040-HATDVISA                  15080059
           END-IF                                                       15090059
           MOVE 'STE'                 TO E040-CLACON                    15100059
           MOVE WXTB-TEXCAST-003      TO E040-DESCON                    15110059
           MOVE '023'                 TO W-OPERAC                       15120059
           MOVE VARC-CUENTA           TO W-REFER-N                      15130059
           MOVE W-OBSERVACIONES       TO E040-OBSERV                    15140059
           MOVE '-NEG-'               TO E040-OBSERV (26:05)            15150059
           MOVE '3384'                TO E040-HACTRGEN                  15160059
           MOVE SPACES                TO E040-HACEMTUT                  15170059
           MOVE VARC-CTAVAL20 (01:08) TO E040-SANCTCCC (01:08)          15180059
           MOVE VARC-CTAVAL20 (11:10) TO E040-SANCTCCC (09:10)          15190059
      *                                                                *15200059
           WRITE CONTABLE-CARGO FROM HAEC040.                           15210059
      *                                                                *15220059
           IF FS-S3DQ3384 NOT = '00'                                    15230059
              MOVE  'S3DQ3384'           TO  W802-FICHERO               15240059
              MOVE  FS-S3DQ3384          TO  W802-STATUS                15250059
              MOVE  'VL4C3384'           TO  W802-PROGRAMA              15260059
              MOVE  'WRITE-ABONO'        TO  W802-ACCION                15270059
              MOVE  VARC-CUENTA          TO  W802-CLAVE                 15280059
              MOVE  ZEROS                TO  W802-ABEND                 15290059
              MOVE  '280-CONTABLE-ABONO-NEG' TO  W802-PARRAFO           15300059
              PERFORM VLPC8020-DISP-ABEND-FICH                          15310059
              PERFORM VLPC8020-ABEND-FICH                               15320059
           END-IF.                                                      15330059
           ADD  1                   TO WRI-CARGOS-CTBL.                 15340059
      *                                                                *15350059
      ****                                                          ****15360059
      *    CONTABLE POR EL CARGO A CUENTA OPERATIVA DE LA SAB          *15370059
      ****                                                          ****15380059
           INITIALIZE                    HAEC040                        15390059
           MOVE '0011'                TO E040-EMPRESA                   15400059
           MOVE 'VL1'                 TO E040-CLVINT                    15410059
           MOVE WA-HOYAMD             TO E040-FECON                     15420059
           MOVE WA-HOYAMD             TO E040-FECOP                     15430059
           MOVE ZEROES                TO E040-HACCLPRO                  15440059
           MOVE ZEROES                TO E040-HACSBPRO                  15450059
           MOVE 'T5'                  TO E040-HACCOMPT(1:2)             15460059
           MOVE SPACES                TO E040-HAYCTORD                  15470059
           MOVE '0'                   TO E040-HATINVER                  15480059
           MOVE '000'                 TO E040-HACCOMPT(3:3)             15490059
           MOVE MDC-CDDIVIS           TO E040-HACDVISA                  15500059
           MOVE '0567'                TO E040-CEOPE                     15510059
           MOVE '0567'                TO E040-CEORIGEN                  15520059
           MOVE 1                     TO E040-NUMOD                     15530059
           IF MDC-CDDIVIS = 'PEN'                                       15540059
              MOVE MDC-SALDO-DISPON   TO E040-IMPD                      15550059
              MOVE ZEROS              TO E040-IMPDDIV                   15560059
              MOVE '0'                TO E040-HATDVISA                  15570059
              MOVE WMDC-OFICONT-PEN   TO E040-CEDESTIN                  15580059
           ELSE                                                         15590059
              MOVE ZEROS              TO E040-IMPD                      15600059
              MOVE MDC-SALDO-DISPON   TO E040-IMPDDIV                   15610059
              MOVE '1'                TO E040-HATDVISA                  15620059
              MOVE WMDC-OFICONT-USD   TO E040-CEDESTIN                  15630059
           END-IF                                                       15640059
           MOVE 'STE'                 TO E040-CLACON                    15650059
           MOVE WXTB-TEXCAST-023      TO E040-DESCON                    15660059
           MOVE '023'                 TO W-OPERAC                       15670059
           MOVE VARC-CUENTA           TO W-REFER-N                      15680059
           MOVE W-OBSERVACIONES       TO E040-OBSERV                    15690059
           MOVE '-NEG-'               TO E040-OBSERV (26:05)            15700059
           MOVE '3384'                TO E040-HACTRGEN                  15710059
           MOVE SPACES                TO E040-HACEMTUT                  15720059
           MOVE VARC-CTAVAL20 (01:08) TO E040-SANCTCCC (01:08)          15730059
           MOVE VARC-CTAVAL20 (11:10) TO E040-SANCTCCC (09:10)          15740059
      *                                                                *15750059
           WRITE CONTABLE-CARGO FROM HAEC040.                           15760059
      *                                                                *15770059
           IF FS-S3DQ3384 NOT = '00'                                    15780059
              MOVE  'S3DQ3384'           TO  W802-FICHERO               15790059
              MOVE  FS-S3DQ3384          TO  W802-STATUS                15800059
              MOVE  'VL4C3384'           TO  W802-PROGRAMA              15810059
              MOVE  'WRITE-CARGO'        TO  W802-ACCION                15820059
              MOVE  VARC-CUENTA          TO  W802-CLAVE                 15830059
              MOVE  ZEROS                TO  W802-ABEND                 15840059
              MOVE  '280-CONTABLE-ABONO-NEG' TO  W802-PARRAFO           15850059
              PERFORM VLPC8020-DISP-ABEND-FICH                          15860059
              PERFORM VLPC8020-ABEND-FICH                               15870059
           END-IF.                                                      15880059
           ADD  1                   TO WRI-CARGOS-CTBL.                 15890059
      *                                                                *15900059
      *200804248-INI                                                   *15910059
      *200804248-FIN                                                   *15920059
      *   *------------------*                                         *15930059
       290-CONTABLE-CARGO-NEG.                                          15940059
      *   *------------------*                                         *15950059
      ****                                                          ****15960059
      *    CONTABLE POR EL CARGO A CUENTA REGISTRO                     *15970059
      ****                                                          ****15980059
           INITIALIZE                    HAEC040                        15990059
           MOVE '0011'                TO E040-EMPRESA                   16000059
           MOVE 'VL1'                 TO E040-CLVINT                    16010059
           MOVE FECHA-MAS1            TO E040-FECON                     16020059
           MOVE FECHA-MAS1            TO E040-FECOP                     16030059
           MOVE ZEROES                TO E040-HACCLPRO                  16040059
           MOVE ZEROES                TO E040-HACSBPRO                  16050059
           MOVE 'T5'                  TO E040-HACCOMPT(1:2)             16060059
           MOVE SPACES                TO E040-HAYCTORD                  16070059
           MOVE '0'                   TO E040-HATINVER                  16080059
           MOVE '000'                 TO E040-HACCOMPT(3:3)             16090059
           MOVE MDC-CDDIVIS           TO E040-HACDVISA                  16100059
           MOVE '0567'                TO E040-CEOPE                     16110059
           MOVE '0567'                TO E040-CEORIGEN                  16120059
           MOVE VARC-SUCURS           TO WMDC-OFICONT                   16130059
           MOVE WMDC-OFICONT          TO E040-CEDESTIN                  16140059
           MOVE 1                     TO E040-NUMOD                     16150059
           IF MDC-CDDIVIS = 'PEN'                                       16160059
              MOVE MDC-SALDO-DISPON   TO E040-IMPD                      16170059
              MOVE ZEROS              TO E040-IMPDDIV                   16180059
              MOVE '0'                TO E040-HATDVISA                  16190059
           ELSE                                                         16200059
              MOVE ZEROS              TO E040-IMPD                      16210059
              MOVE MDC-SALDO-DISPON   TO E040-IMPDDIV                   16220059
              MOVE '1'                TO E040-HATDVISA                  16230059
           END-IF                                                       16240059
           MOVE 'ETE'                 TO E040-CLACON                    16250059
           MOVE WXTB-TEXCAST-023      TO E040-DESCON                    16260059
           MOVE '003'                 TO W-OPERAC                       16270059
           MOVE VARC-CUENTA           TO W-REFER-N                      16280059
           MOVE W-OBSERVACIONES       TO E040-OBSERV                    16290059
           MOVE '-NEG-'               TO E040-OBSERV (26:05)            16300059
           MOVE '3384'                TO E040-HACTRGEN                  16310059
           MOVE SPACES                TO E040-HACEMTUT                  16320059
           MOVE VARC-CTAVAL20 (01:08) TO E040-SANCTCCC (01:08)          16330059
           MOVE VARC-CTAVAL20 (11:10) TO E040-SANCTCCC (09:10)          16340059
      *                                                                *16350059
           WRITE CONTABLE-ABONO FROM HAEC040.                           16360059
           IF FS-S4DQ3384 NOT = '00'                                    16370059
              MOVE  'S4DQ3384'           TO  W802-FICHERO               16380059
              MOVE  FS-S4DQ3384          TO  W802-STATUS                16390059
              MOVE  'VL4C3384'           TO  W802-PROGRAMA              16400059
              MOVE  'WRITE-ABONO'        TO  W802-ACCION                16410059
              MOVE  VARC-CUENTA          TO  W802-CLAVE                 16420059
              MOVE  ZEROS                TO  W802-ABEND                 16430059
              MOVE  '290-CONTABLE-CARGO-NEG' TO  W802-PARRAFO           16440059
              PERFORM VLPC8020-DISP-ABEND-FICH                          16450059
              PERFORM VLPC8020-ABEND-FICH                               16460059
           END-IF.                                                      16470059
           ADD  1                   TO WRI-ABONOS-CTBL.                 16480059
      *                                                                *16490059
      ****                                                          ****16500059
      *    CONTABLE POR EL ABONO A CUENTA OPERATIVA DE LA SAB          *16510059
      ****                                                          ****16520059
           INITIALIZE                    HAEC040                        16530059
           MOVE '0011'                TO E040-EMPRESA                   16540059
           MOVE 'VL1'                 TO E040-CLVINT                    16550059
           MOVE FECHA-MAS1            TO E040-FECON                     16560059
           MOVE FECHA-MAS1            TO E040-FECOP                     16570059
           MOVE ZEROES                TO E040-HACCLPRO                  16580059
           MOVE ZEROES                TO E040-HACSBPRO                  16590059
           MOVE 'T5'                  TO E040-HACCOMPT(1:2)             16600059
           MOVE SPACES                TO E040-HAYCTORD                  16610059
           MOVE '0'                   TO E040-HATINVER                  16620059
           MOVE '000'                 TO E040-HACCOMPT(3:3)             16630059
           MOVE MDC-CDDIVIS           TO E040-HACDVISA                  16640059
           MOVE '0567'                TO E040-CEOPE                     16650059
           MOVE '0567'                TO E040-CEORIGEN                  16660059
           MOVE 1                     TO E040-NUMOH                     16670059
           IF MDC-CDDIVIS = 'PEN'                                       16680059
              MOVE MDC-SALDO-DISPON   TO E040-IMPH                      16690059
              MOVE ZEROS              TO E040-IMPHDIV                   16700059
              MOVE '0'                TO E040-HATDVISA                  16710059
              MOVE WMDC-OFICONT-PEN   TO E040-CEDESTIN                  16720059
           ELSE                                                         16730059
              MOVE ZEROS              TO E040-IMPH                      16740059
              MOVE MDC-SALDO-DISPON   TO E040-IMPHDIV                   16750059
              MOVE '1'                TO E040-HATDVISA                  16760059
              MOVE WMDC-OFICONT-USD   TO E040-CEDESTIN                  16770059
           END-IF                                                       16780059
           MOVE 'ETE'                 TO E040-CLACON                    16790059
           MOVE WXTB-TEXCAST-003      TO E040-DESCON                    16800059
           MOVE '003'                 TO W-OPERAC                       16810059
           MOVE VARC-CUENTA           TO W-REFER-N                      16820059
           MOVE W-OBSERVACIONES       TO E040-OBSERV                    16830059
           MOVE '-NEG-'               TO E040-OBSERV (26:05)            16840059
           MOVE '3384'                TO E040-HACTRGEN                  16850059
           MOVE SPACES                TO E040-HACEMTUT                  16860059
           MOVE VARC-CTAVAL20 (01:08) TO E040-SANCTCCC (01:08)          16870059
           MOVE VARC-CTAVAL20 (11:10) TO E040-SANCTCCC (09:10)          16880059
      *                                                                *16890059
           WRITE CONTABLE-ABONO FROM HAEC040.                           16900059
           IF FS-S3DQ3384 NOT = '00'                                    16910059
              MOVE  'S3DQ3384'           TO  W802-FICHERO               16920059
              MOVE  FS-S3DQ3384          TO  W802-STATUS                16930059
              MOVE  'VL4C3384'           TO  W802-PROGRAMA              16940059
              MOVE  'WRITE-CARGO'        TO  W802-ACCION                16950059
              MOVE  VARC-CUENTA          TO  W802-CLAVE                 16960059
              MOVE  ZEROS                TO  W802-ABEND                 16970059
              MOVE  '290-CONTABLE-CARGO-NEG' TO  W802-PARRAFO           16980059
              PERFORM VLPC8020-DISP-ABEND-FICH                          16990059
              PERFORM VLPC8020-ABEND-FICH                               17000059
           END-IF.                                                      17010059
           ADD  1                   TO WRI-ABONOS-CTBL.                 17020059
      *200804248-FIN                                                   *17030059
      *                                                                *17040059
      *   *-------------*                                              *17050059
       295-CONASEV-CARGO.                                               17060059
      *   *-------------*                                              *17070059
      *                                                                *17080059
      ***                                                            ***17090059
      *  SE GRABA CONTABLE DE INTERFAZ A PLAN CONTABLE CONASEV         *17100059
      ***                                                            ***17110059
      *    CARGO A LA CUENTA ECONOMICA DE REGISTRO                     *17120059
           INITIALIZE                        REG-CONASEV.               17130059
           MOVE '6'                       TO CTBL-TIPOPE                17140059
           MOVE 'CIERRE DIA CTA-REGISTRO' TO CTBL-REFERE                17150059
           MOVE VARC-CUENTA               TO CTBL-REFERE (25:07)        17160059
           MOVE WA-HOYAMD   (03:06)       TO CTBL-FECPRO                17170059
           MOVE '0001'                    TO CTBL-NROCOM                17180059
           MOVE '000'                     TO CTBL-MONEXT                17190059
           MOVE ZEROS                     TO CTBL-IMPEXT                17200059
           MOVE ZEROS                     TO CTBL-TIPCAM                17210059
           MOVE SPACES                    TO CTBL-CODREL                17220059
           MOVE '000'                     TO CTBL-NUMREG                17230059
           IF VARC-MONEDA EQUAL 'USD'                                   17240059
              MOVE 'US$'                  TO CTBL-MONEDA                17250059
              MOVE '104210  '             TO CTBL-CUENTA                17260059
           ELSE                                                         17270059
              MOVE 'S/.'                  TO CTBL-MONEDA                17280059
              MOVE '104110  '             TO CTBL-CUENTA                17290059
           END-IF.                                                      17300059
JPC@1 *    MOVE MDC-SALDO-DISPUE          TO CTBL-IMPORT                17310059
JPC@1      MOVE MDC-SALDO-DISPON          TO CTBL-IMPORT                17320059
           MOVE 0542                      TO CTBL-SECORI                17330059
           MOVE  542                      TO CTBL-OFICIN                17340059
                                                                        17350059
           WRITE CONASEV-CARGO FROM REG-CONASEV.                        17360059
                                                                        17370059
           IF FS-S5DQ3384 NOT = '00'                                    17380059
              MOVE  'S5DQ3384'            TO W802-FICHERO               17390059
              MOVE  FS-S5DQ3384           TO W802-STATUS                17400059
              MOVE  'VL4C3384'            TO W802-PROGRAMA              17410059
              MOVE  'WRITE-CARGO-CNS-1'   TO W802-ACCION                17420059
              MOVE  VARC-CUENTA           TO W802-CLAVE                 17430059
              MOVE  ZEROS                 TO W802-ABEND                 17440059
              MOVE  '295-CONASEV-CARGO '  TO W802-PARRAFO               17450059
              PERFORM VLPC8020-DISP-ABEND-FICH                          17460059
              PERFORM VLPC8020-ABEND-FICH                               17470059
           END-IF.                                                      17480059
           ADD  1                         TO WRI-CARGOS-CONA.           17490059
      *                                                                *17500059
           INITIALIZE                        REG-CONASEV.               17510059
           MOVE '5'                       TO CTBL-TIPOPE                17520059
           MOVE 'CIERRE DIA CTA-REGISTRO' TO CTBL-REFERE                17530059
           MOVE VARC-CUENTA               TO CTBL-REFERE (25:07)        17540059
           MOVE WA-HOYAMD   (03:06)       TO CTBL-FECPRO                17550059
           MOVE '0001'                    TO CTBL-NROCOM                17560059
           MOVE '000'                     TO CTBL-MONEXT                17570059
           MOVE ZEROS                     TO CTBL-IMPEXT                17580059
           MOVE ZEROS                     TO CTBL-TIPCAM                17590059
           MOVE SPACES                    TO CTBL-CODREL                17600059
           MOVE '000'                     TO CTBL-NUMREG                17610059
           IF VARC-MONEDA  EQUAL 'USD'                                  17620059
              MOVE 'US$'                  TO CTBL-MONEDA                17630059
              MOVE '46920115'             TO CTBL-CUENTA                17640059
           ELSE                                                         17650059
              MOVE 'S/.'                  TO CTBL-MONEDA                17660059
              MOVE '46910115'             TO CTBL-CUENTA                17670059
           END-IF.                                                      17680059
JPC@1 *    MOVE MDC-SALDO-DISPUE          TO CTBL-IMPORT                17690059
JPC@1      MOVE MDC-SALDO-DISPON          TO CTBL-IMPORT                17700059
           MOVE 0542                      TO CTBL-SECORI                17710059
           MOVE  542                      TO CTBL-OFICIN                17720059
                                                                        17730059
           WRITE CONASEV-CARGO FROM REG-CONASEV.                        17740059
                                                                        17750059
           IF FS-S5DQ3384 NOT = '00'                                    17760059
              MOVE  'S5DQ3384'            TO W802-FICHERO               17770059
              MOVE  FS-S5DQ3384           TO W802-STATUS                17780059
              MOVE  'VL4C3384'            TO W802-PROGRAMA              17790059
              MOVE  'WRITE-CARGO-CNS-2'   TO W802-ACCION                17800059
              MOVE  VARC-CUENTA           TO W802-CLAVE                 17810059
              MOVE  ZEROS                 TO W802-ABEND                 17820059
              MOVE  '295-CONASEV-CARGO '  TO W802-PARRAFO               17830059
              PERFORM VLPC8020-DISP-ABEND-FICH                          17840059
              PERFORM VLPC8020-ABEND-FICH                               17850059
           END-IF.                                                      17860059
           ADD  1                         TO WRI-CARGOS-CONA.           17870059
      *                                                                *17880059
      *   *-------------*                                              *17890059
       297-CONASEV-ABONO.                                               17900059
      *   *-------------*                                              *17910059
      ***                                                            ***17920059
      *  SE GRABA CONTABLE DE INTERFAZ A PLAN CONTABLE CONASEV         *17930059
      ***                                                            ***17940059
      *    ABONO A LA CUENTA ECONOMICA DE REGISTRO                     *17950059
           INITIALIZE                        REG-CONASEV.               17960059
           MOVE '5'                       TO CTBL-TIPOPE                17970059
           MOVE 'INICIO DIA CTA-REGISTRO' TO CTBL-REFERE                17980059
           MOVE VARC-CUENTA               TO CTBL-REFERE (25:07)        17990059
           MOVE FECHA-MAS1  (03:06)       TO CTBL-FECPRO                18000059
           MOVE '0001'                    TO CTBL-NROCOM                18010059
           MOVE '000'                     TO CTBL-MONEXT                18020059
           MOVE ZEROS                     TO CTBL-IMPEXT                18030059
           MOVE ZEROS                     TO CTBL-TIPCAM                18040059
           MOVE SPACES                    TO CTBL-CODREL                18050059
           MOVE '000'                     TO CTBL-NUMREG                18060059
           IF VARC-MONEDA  EQUAL 'USD'                                  18070059
              MOVE 'US$'                  TO CTBL-MONEDA                18080059
              MOVE '104210  '             TO CTBL-CUENTA                18090059
           ELSE                                                         18100059
              MOVE 'S/.'                  TO CTBL-MONEDA                18110059
              MOVE '104110  '             TO CTBL-CUENTA                18120059
           END-IF.                                                      18130059
JPC@1 *    MOVE MDC-SALDO-DISPUE          TO CTBL-IMPORT                18140059
JPC@1      MOVE MDC-SALDO-DISPON          TO CTBL-IMPORT                18150059
           MOVE 0542                      TO CTBL-SECORI                18160059
           MOVE  542                      TO CTBL-OFICIN                18170059
                                                                        18180059
           WRITE CONASEV-ABONO FROM REG-CONASEV.                        18190059
                                                                        18200059
           IF FS-S6DQ3384 NOT = '00'                                    18210059
              MOVE  'S6DQ3384'            TO W802-FICHERO               18220059
              MOVE  FS-S6DQ3384           TO W802-STATUS                18230059
              MOVE  'VL4C3384'            TO W802-PROGRAMA              18240059
              MOVE  'WRITE-ABONO-CNS-1'   TO W802-ACCION                18250059
              MOVE  VARC-CUENTA           TO W802-CLAVE                 18260059
              MOVE  ZEROS                 TO W802-ABEND                 18270059
              MOVE  '297-CONASEV-ABONO '  TO W802-PARRAFO               18280059
              PERFORM VLPC8020-DISP-ABEND-FICH                          18290059
              PERFORM VLPC8020-ABEND-FICH                               18300059
           END-IF.                                                      18310059
           ADD  1                         TO WRI-ABONOS-CONA.           18320059
      *                                                                *18330059
           INITIALIZE                        REG-CONASEV.               18340059
           MOVE '6'                       TO CTBL-TIPOPE                18350059
           MOVE 'INICIO DIA CTA-REGISTRO' TO CTBL-REFERE                18360059
           MOVE VARC-CUENTA               TO CTBL-REFERE (25:07)        18370059
           MOVE FECHA-MAS1  (03:06)       TO CTBL-FECPRO                18380059
           MOVE '0001'                    TO CTBL-NROCOM                18390059
           MOVE '000'                     TO CTBL-MONEXT                18400059
           MOVE ZEROS                     TO CTBL-IMPEXT                18410059
           MOVE ZEROS                     TO CTBL-TIPCAM                18420059
           MOVE SPACES                    TO CTBL-CODREL                18430059
           MOVE '000'                     TO CTBL-NUMREG                18440059
           IF VARC-MONEDA  EQUAL 'USD'                                  18450059
              MOVE 'US$'                  TO CTBL-MONEDA                18460059
              MOVE '46920115'             TO CTBL-CUENTA                18470059
           ELSE                                                         18480059
              MOVE 'S/.'                  TO CTBL-MONEDA                18490059
              MOVE '46910115'             TO CTBL-CUENTA                18500059
           END-IF.                                                      18510059
JPC@1 *    MOVE MDC-SALDO-DISPUE          TO CTBL-IMPORT                18520059
JPC@1      MOVE MDC-SALDO-DISPON          TO CTBL-IMPORT                18530059
           MOVE 0542                      TO CTBL-SECORI                18540059
           MOVE  542                      TO CTBL-OFICIN                18550059
                                                                        18560059
           WRITE CONASEV-ABONO FROM REG-CONASEV.                        18570059
                                                                        18580059
           IF FS-S6DQ3384 NOT = '00'                                    18590059
              MOVE  'S6DQ3384'            TO W802-FICHERO               18600059
              MOVE  FS-S6DQ3384           TO W802-STATUS                18610059
              MOVE  'VL4C3384'            TO W802-PROGRAMA              18620059
              MOVE  'WRITE-ABONO-CNS-2'   TO W802-ACCION                18630059
              MOVE  VARC-CUENTA           TO W802-CLAVE                 18640059
              MOVE  ZEROS                 TO W802-ABEND                 18650059
              MOVE  '297-CONASEV-ABONO '  TO W802-PARRAFO               18660059
              PERFORM VLPC8020-DISP-ABEND-FICH                          18670059
              PERFORM VLPC8020-ABEND-FICH                               18680059
           END-IF.                                                      18690059
           ADD  1                         TO WRI-ABONOS-CONA.           18700059
      *   *---*                                                         18710059
       300-FIN.                                                         18720059
      *   *---*                                                         18730059
      *                                                                *18740059
           PERFORM 3100-DISP-TOTALIMETROS                               18750059
              THRU 3100-DISP-TOTALIMETROS-FIN.                          18760059
      *                                                                *18770059
           CLOSE S1DQ3384.                                              18780059
           IF FS-S1DQ3384 NOT = '00'                                    18790059
              MOVE  'S1DQ3384'     TO  W802-FICHERO                     18800059
              MOVE  FS-S1DQ3384    TO  W802-STATUS                      18810059
              MOVE  'VL4C3384'     TO  W802-PROGRAMA                    18820059
              MOVE  'CLOSE'        TO  W802-ACCION                      18830059
              MOVE  SPACES         TO  W802-CLAVE                       18840059
              MOVE  ZEROS          TO  W802-ABEND                       18850059
              MOVE  '300-FIN '     TO  W802-PARRAFO                     18860059
              PERFORM VLPC8020-DISP-ABEND-FICH                          18870059
              PERFORM VLPC8020-ABEND-FICH                               18880059
           END-IF.                                                      18890059
      *                                                                *18900059
           CLOSE S2DQ3384.                                              18910059
           IF FS-S2DQ3384 NOT = '00'                                    18920059
              MOVE  'S2DQ3384'     TO  W802-FICHERO                     18930059
              MOVE  FS-S2DQ3384    TO  W802-STATUS                      18940059
              MOVE  'VL4C3384'     TO  W802-PROGRAMA                    18950059
              MOVE  'CLOSE'        TO  W802-ACCION                      18960059
              MOVE  SPACES         TO  W802-CLAVE                       18970059
              MOVE  ZEROS          TO  W802-ABEND                       18980059
              MOVE  '300-FIN '     TO  W802-PARRAFO                     18990059
              PERFORM VLPC8020-DISP-ABEND-FICH                          19000059
              PERFORM VLPC8020-ABEND-FICH                               19010059
           END-IF.                                                      19020059
      *                                                                *19030059
           CLOSE S3DQ3384.                                              19040059
           IF FS-S3DQ3384 NOT = '00'                                    19050059
              MOVE  'S3DQ3384'     TO  W802-FICHERO                     19060059
              MOVE  FS-S3DQ3384    TO  W802-STATUS                      19070059
              MOVE  'VL4C3384'     TO  W802-PROGRAMA                    19080059
              MOVE  'CLOSE'        TO  W802-ACCION                      19090059
              MOVE  SPACES         TO  W802-CLAVE                       19100059
              MOVE  ZEROS          TO  W802-ABEND                       19110059
              MOVE  '300-FIN '     TO  W802-PARRAFO                     19120059
              PERFORM VLPC8020-DISP-ABEND-FICH                          19130059
              PERFORM VLPC8020-ABEND-FICH                               19140059
           END-IF.                                                      19150059
      *                                                                *19160059
           CLOSE S4DQ3384.                                              19170059
           IF FS-S4DQ3384 NOT = '00'                                    19180059
              MOVE  'S4DQ3384'     TO  W802-FICHERO                     19190059
              MOVE  FS-S4DQ3384    TO  W802-STATUS                      19200059
              MOVE  'VL4C3384'     TO  W802-PROGRAMA                    19210059
              MOVE  'CLOSE'        TO  W802-ACCION                      19220059
              MOVE  SPACES         TO  W802-CLAVE                       19230059
              MOVE  ZEROS          TO  W802-ABEND                       19240059
              MOVE  '300-FIN '     TO  W802-PARRAFO                     19250059
              PERFORM VLPC8020-DISP-ABEND-FICH                          19260059
              PERFORM VLPC8020-ABEND-FICH                               19270059
           END-IF.                                                      19280059
      *                                                                *19290059
      *200806019-INI                                                   *19300059
           CLOSE S5DQ3384.                                              19310059
           IF FS-S5DQ3384 NOT = '00'                                    19320059
              MOVE  'S5DQ3384'     TO  W802-FICHERO                     19330059
              MOVE  FS-S5DQ3384    TO  W802-STATUS                      19340059
              MOVE  'VL4C3384'     TO  W802-PROGRAMA                    19350059
              MOVE  'CLOSE'        TO  W802-ACCION                      19360059
              MOVE  SPACES         TO  W802-CLAVE                       19370059
              MOVE  ZEROS          TO  W802-ABEND                       19380059
              MOVE  '300-FIN '     TO  W802-PARRAFO                     19390059
              PERFORM VLPC8020-DISP-ABEND-FICH                          19400059
              PERFORM VLPC8020-ABEND-FICH                               19410059
           END-IF.                                                      19420059
      *                                                                *19430059
           CLOSE S6DQ3384.                                              19440059
           IF FS-S6DQ3384 NOT = '00'                                    19450059
              MOVE  'S6DQ3384'     TO  W802-FICHERO                     19460059
              MOVE  FS-S6DQ3384    TO  W802-STATUS                      19470059
              MOVE  'VL4C3384'     TO  W802-PROGRAMA                    19480059
              MOVE  'CLOSE'        TO  W802-ACCION                      19490059
              MOVE  SPACES         TO  W802-CLAVE                       19500059
              MOVE  ZEROS          TO  W802-ABEND                       19510059
              MOVE  '300-FIN '     TO  W802-PARRAFO                     19520059
              PERFORM VLPC8020-DISP-ABEND-FICH                          19530059
              PERFORM VLPC8020-ABEND-FICH                               19540059
           END-IF.                                                      19550059
      *200806019-FIN                                                   *19560059
      *                                                                *19570059
           PERFORM 230-CLOSE-VLDCARC.                                   19580059
      *                                                                *19590059
           STOP RUN.                                                    19600059
      *                                                                *19610059
      *   *-------------*                                              *19620059
       400-DIA-SIGUIENTE.                                               19630059
      *   *-------------*                                              *19640059
      ****                                                          ****19650059
      *    OPCION 2 - SUMAMOS UN DIA HABIL AL ACTUAL                   *19660059
      ****                                                          ****19670059
           INITIALIZE                            TCWC1750               19680059
           MOVE '2'                           TO W175-CDOPCIO           19690059
           MOVE FECHA-SYSIN                   TO W175-FHGRE1            19700059
           MOVE XX-DIAS                       TO W175-FACTOR            19710059
           MOVE 'NA'                          TO W175-TPTRAT            19720059
      *                                                                *19730059
           CALL TC8C1220 USING TCWC1750.                                19740059
      *                                                                *19750059
           EVALUATE W175-CDRETORN                                       19760059
               WHEN '00'                                                19770059
                    MOVE W175-FHGREG (01:02)  TO DD-MAS1   WA-FEMAS1-D  19780059
                    MOVE W175-FHGREG (03:02)  TO MM-MAS1   WA-FEMAS1-M  19790059
                    MOVE W175-FHGREG (05:04)  TO ANO-MAS1  WA-FEMAS1-A  19800059
               WHEN OTHER                                               19810059
                    MOVE 'TC8C1220'           TO WRUT-PROGRAMA          19820059
                    MOVE W175-CDRETORN        TO WRUT-RETOR             19830059
                    MOVE 'SUMAR DIA'          TO WRUT-ACCION            19840059
                    MOVE TCWC1750             TO WRUT-CLAVE             19850059
                    MOVE '400-DIA-SIGUIENTE'  TO WRUT-PARRAFO           19860059
                    PERFORM VLPCRUTI-DISP-ABEND-RUTI                    19870059
                    PERFORM VLPCRUTI-ABEND-RUTI                         19880059
           END-EVALUATE.                                                19890059
      *                                                                *19900059
      *200806010-INI                                                  * 19910059
      *                                                                 19920059
      **** OPCION 2 - VALIDAMOS SI ES FERIADOS                          19930059
      *                                                                 19940059
           INITIALIZE                            TCWC1400.              19950059
           MOVE '2'                           TO W140-CDOPCIO.          19960059
           MOVE W175-FHGREG (01:02)           TO W140-FHTRAT1 (01:02).  19970059
           MOVE '.'                           TO W140-FHTRAT1 (03:01).  19980059
           MOVE W175-FHGREG (03:02)           TO W140-FHTRAT1 (04:02).  19990059
           MOVE '.'                           TO W140-FHTRAT1 (06:01).  20000059
           MOVE W175-FHGREG (05:04)           TO W140-FHTRAT1 (07:04).  20010059
           MOVE '1'                           TO W140-TRSABADO.         20020059
           MOVE '0'                           TO W140-TRATFEST.         20030059
           MOVE '0011'                        TO W140-CDEMPRES.         20040059
      *                                                                 20050059
           CALL TC9C1200 USING TCWC1400.                                20060059
      *                                                                 20070059
           EVALUATE W140-CDRETORN                                       20080059
               WHEN '00'                                                20090059
                    IF W140-TIPODIA = 'H'                               20100059
                       CONTINUE                                         20110059
                    ELSE                                                20120059
                       IF XX-DIAS > 7                                   20130059
                          MOVE 'TC9C1200'          TO WRUT-PROGRAMA     20140059
                          MOVE SPACES              TO WRUT-RETOR        20150059
                          MOVE 'BUSCA FERIADOS  '  TO WRUT-ACCION       20160059
                          MOVE 'MAS DE 7 DIAS '    TO WRUT-CLAVE        20170059
                          MOVE '400-DIA-SIGUIENTE' TO WRUT-PARRAFO      20180059
                          PERFORM VLPCRUTI-DISP-ABEND-RUTI              20190059
                          PERFORM VLPCRUTI-ABEND-RUTI                   20200059
                       ELSE                                             20210059
                          ADD  1                   TO XX-DIAS           20220059
      *                   DISPLAY 'FERIADO : ' XX-DIAS '-' W140-FHTRAT1 20230059
                          GO TO 400-DIA-SIGUIENTE                       20240059
                       END-IF                                           20250059
                    END-IF                                              20260059
               WHEN OTHER                                               20270059
                    MOVE 'TC9C1200'           TO WRUT-PROGRAMA          20280059
                    MOVE '* TABLA     : '     TO WRUT-MSG4  (01:14)     20290059
                    MOVE W140-TABLENAME       TO WRUT-MSG4  (15:16)     20300059
                    MOVE W140-SQLCODE         TO WRUT-MSG4  (32:09)     20310059
                    MOVE W140-CDRETORN        TO WRUT-RETOR             20320059
                    MOVE 'CALL         '      TO WRUT-ACCION            20330059
                    MOVE W140-CDOPCIO         TO WRUT-CLAVE (01:01)     20340059
                    MOVE W140-FHTRAT1         TO WRUT-CLAVE (02:10)     20350059
                    MOVE W140-TRSABADO        TO WRUT-CLAVE (12:01)     20360059
                    MOVE W140-TRATFEST        TO WRUT-CLAVE (13:01)     20370059
                    MOVE W140-CDEMPRES        TO WRUT-CLAVE (14:04)     20380059
                    MOVE '400-DIA-SIGUIENTE'  TO WRUT-PARRAFO           20390059
                    PERFORM VLPCRUTI-DISP-ABEND-RUTI                    20400059
                    PERFORM VLPCRUTI-ABEND-RUTI                         20410059
           END-EVALUATE.                                                20420059
      *200806010-FIN                                                  * 20430059
      *    *-----------------*                                          20440059
       3100-DISP-TOTALIMETROS.                                          20450059
      *    *-----------------*                                          20460059
      *                                                                *20470059
            DISPLAY '*************************************************'.20480059
            DISPLAY '********    T O T A L I M E T R O S   ***********'.20490059
            DISPLAY '********   D E L     P R O G R A M A  ***********'.20500059
            DISPLAY '*                 VL4C3384                      *'.20510059
            DISPLAY '*************************************************'.20520059
            DISPLAY 'CUENTA VALORES LEIDOS.....:' LEI-VLDTARC.          20530059
            DISPLAY 'CUENTA REGISTRO LEIDOS....:' PRO-VLDTARC.          20540059
            DISPLAY 'CALL BG9MDC0..............:' LEI-BG9CMDC0.         20550059
            DISPLAY 'CARGOS GRABADOS (OPS).....:' WRI-CARGOS.           20560059
            DISPLAY 'ABONOS GRABADOS (OPS).....:' WRI-ABONOS.           20570059
            DISPLAY 'CARGOS CONTABLE BCO. .....:' WRI-CARGOS-CTBL.      20580059
            DISPLAY 'ABONOS CONTABLE BCO. .....:' WRI-ABONOS-CTBL.      20590059
            DISPLAY 'CARGOS CONTABLE CONASEV...:' WRI-CARGOS-CONA.      20600059
            DISPLAY 'ABONOS CONTABLE CONASEV...:' WRI-ABONOS-CONA.      20610059
            DISPLAY '*************************************************'.20620059
      *                                                                *20630059
      *    *---------------------*                                      20640059
       3100-DISP-TOTALIMETROS-FIN.                                      20650059
      *    *---------------------*                                      20660059
      *                                                                *20670059
            EXIT.                                                       20680059
      *-----------------*                                               20690059
      * COPY DE ERRORES *                                               20700059
      *-----------------*                                               20710059
      *                                                                *20720059
           COPY QRWCDB20.                                               20730059
           COPY VLPC8020.                                               20740059
           COPY VLPC8010.                                               20750059
           COPY VLPCRUTI.                                               20760059
      *                                                                *20770059
      *-----------------*                                               20780059
      * FIN DE PROGRAMA *                                               20790059
      *-----------------*                                               20800059
