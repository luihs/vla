      *-----------------------*                                         00010015
       IDENTIFICATION DIVISION.                                         00020015
      *-----------------------*                                         00030015
       PROGRAM-ID.   VL4CFTC0.                                          00040015
      *AUTHOR.       BBVA PERU.                                         00050015
      ******************************************************************00060015
      *REF.PETIC FECHA-MOD. PROGRAMADOR      DESCRIPCION               *00070015
      *--------- ---------- ---------------- --------------------------*00080015
FVAXX *FVA-XX    26-08-2020 EULER ALVARADO   VALIDA SITUA. CTA Y SALDO *00090015
      *                                      CONSIDERA FECHAS DE LINKAG*00100015
      *--------- ---------- ---------------- --------------------------*00110015
      ******************************************************************00120015
       ENVIRONMENT DIVISION.                                            00130015
       CONFIGURATION SECTION.                                           00140015
       SOURCE-COMPUTER. IBM-390.                                        00150015
       OBJECT-COMPUTER. IBM-390.                                        00160015
       SPECIAL-NAMES.                                                   00170015
       INPUT-OUTPUT SECTION.                                            00180015
      *------------*                                                    00190015
       FILE-CONTROL.                                                    00200015
      *------------*                                                    00210015
            SELECT E1DQ9FTC ASSIGN TO E1DQ9FTC                          00220015
                   FILE STATUS IS FS-E1DQ9FTC                           00230015
                   ORGANIZATION IS SEQUENTIAL.                          00240015
                                                                        00250015
            SELECT E2DQ9ADS ASSIGN TO E2DQ9ADS                          00260015
                   FILE STATUS IS FS-E2DQ9ADS                           00270015
                   ORGANIZATION IS SEQUENTIAL.                          00280015
                                                                        00290015
            SELECT S1DQ9FTC ASSIGN TO S1DQ9FTC                          00300015
                   FILE STATUS IS FS-S1DQ9FTC                           00310015
                   ORGANIZATION IS SEQUENTIAL.                          00320015
      *-----------------------------------------------------------------00330015
      *-------------*                                                   00340015
       DATA DIVISION.                                                   00350015
      *-------------*                                                   00360015
       FILE SECTION.                                                    00370015
                                                                        00380015
       FD  E1DQ9FTC                                                     00390015
           RECORDING MODE IS F                                          00400015
           BLOCK CONTAINS 0 RECORDS                                     00410015
           DATA RECORD IS REG-E1DQ9FTC.                                 00420015
       01  REG-E1DQ9FTC.                                                00430015
           10 E01-CTAVAL20         PIC X(20).                           00440015
           10 E01-FILLER1          PIC X(01).                           00450015
           10 E01-MONEDA           PIC X(03).                           00460015
           10 E01-FILLER2          PIC X(01).                           00470015
           10 E01-NUMCLI           PIC 9(08).                           00480015
           10 E01-FILLER3          PIC X(01).                           00490015
           10 E01-CLIENTE          PIC X(60).                           00500015
           10 E01-FILLER4          PIC X(01).                           00510015
           10 E01-SITUACION        PIC X(09).                           00520015
           10 E01-FILLER5          PIC X(01).                           00530015
           10 E01-FECALTA          PIC X(10).                           00540015
           10 E01-FILLER6          PIC X(01).                           00550015
           10 E01-FECCESE          PIC X(10).                           00560015
           10 E01-FILLER7          PIC X(01).                           00570015
           10 E01-RUT              PIC 9(08).                           00580015
                                                                        00590015
       FD  E2DQ9ADS                                                     00600015
           RECORDING MODE IS F                                          00610015
           BLOCK CONTAINS 0 RECORDS                                     00620015
           DATA RECORD IS REG-E2DQ9ADS.                                 00630015
       01  REG-E2DQ9ADS            PIC X(354).                          00640015
                                                                        00650015
       FD  S1DQ9FTC                                                     00660015
           RECORDING MODE IS F                                          00670015
           BLOCK CONTAINS 0 RECORDS                                     00680015
           DATA RECORD IS REG-S1DQ9FTC.                                 00690015
       01  REG-S1DQ9FTC            PIC X(214).                          00700015
      *                                                                 00710015
      *-----------------------------------------------------------------00720015
       WORKING-STORAGE SECTION.                                         00730015
      *-----------------------*                                         00740015
       77  WS-NAME                 PIC X(70) VALUE                      00750015
                                   '**  INICIO WORKING VL4C9MAE **'.    00760015
      ******************************************************************00770015
      *                                                                 00780015
       77  WI                      PIC 9(02) VALUE ZEROS.               00790015
       77  WHIS-ANO                PIC S9(4)V USAGE COMP-3.             00800015
       77  WHIS-MES                PIC S9(4)V USAGE COMP-3.             00810015
       77  WPOL-FECINI             PIC S9(8)V USAGE COMP-3.             00820015
       77  WPOL-FECFIN             PIC S9(8)V USAGE COMP-3.             00830015
       77  WHAC-FECINI             PIC S9(8)V USAGE COMP-3.             00840015
       77  WHAC-FECFIN             PIC S9(8)V USAGE COMP-3.             00850015
       77  WA-APERTURA             PIC  X(40) VALUE                     00860015
                             'APERTURA DE CUENTA                      '.00870015
       77  WA-SALIDA               PIC  X(40) VALUE                     00880015
                             'TRASPASO CTA-REGISTRO A OPERATIVA S.A.B.'.00890015
       77  WA-ENTRADA              PIC  X(40) VALUE                     00900015
                             'TRASPASO CTA-OPERATIVA S.A.B. A REGISTRO'.00910015
      *                                                                 00920015
       01  WS-VARIOS.                                                   00930015
           02  WX-CUENTA-ARC7.                                          00940015
               04  WN-CUENTA-ARC7  PIC  9(7).                           00950015
           02  WA-CUENTA-ARC7      PIC S9(7)V USAGE COMP-3.             00960015
           02  WSV-FECHA-DES.                                           00970015
               04 WSV-FECHA-DES-A      PIC X(04).                       00980015
               04 WSV-FECHA-DES-M      PIC X(02).                       00990015
               04 WSV-FECHA-DES-D      PIC X(02).                       01000015
           02  WSV-FECHA-DES-N REDEFINES WSV-FECHA-DES PIC 9(08).       01010015
           02  WSV-FECHA-HAS.                                           01020015
               04 WSV-FECHA-HAS-A      PIC X(04).                       01030015
               04 WSV-FECHA-HAS-M      PIC X(02).                       01040015
               04 WSV-FECHA-HAS-D      PIC X(02).                       01050015
           02  WSV-FECHA-HAS-N REDEFINES WSV-FECHA-HAS PIC 9(08).       01060015
           02  WSMM-SALDO-AUT          PIC S9(13)V9(02) USAGE COMP-3.   01070015
           02  WHIS-FECHIS-X.                                           01080015
               04  WHIS-FECHIS-A       PIC 9(04).                       01090015
               04  WHIS-FECHIS-M       PIC 9(02).                       01100015
               04  WHIS-FECHIS-D       PIC 9(02).                       01110015
           02  WHIS-FECHIS-N REDEFINES WHIS-FECHIS-X PIC 9(08).         01120015
      *                                                                *01130015
           02  WHIS-FEC1RA-X.                                           01140015
               04  WHIS-FEC1RA-A       PIC 9(04).                       01150015
               04  WHIS-FEC1RA-M       PIC 9(02).                       01160015
               04  WHIS-FEC1RA-D       PIC 9(02).                       01170015
           02  WHIS-FEC1RA-N REDEFINES WHIS-FEC1RA-X PIC 9(08).         01180015
      *                                                                *01190015
       01  WTMP-NOM-SUS.                                                01200015
           02 WXEN-SUSPDT                 PIC S9(08)V9(10) USAGE COMP-3.01210015
           02 WXEN-NOMITEMP REDEFINES WXEN-SUSPDT                       01220015
                                          PIC S9(12)V9(06) USAGE COMP-3.01230015
      *                                                                *01240015
       01  PE9C5201                PIC X(08) VALUE 'PE9C5201'.          01250015
RTP0   01  PE9C5000                PIC X(08) VALUE 'PE9C5000'.          01251016
       01  FILE-STATUS.                                                 01260015
           10 FS-E1DQ9FTC          PIC X(02) VALUE SPACES.              01270015
           10 FS-E2DQ9ADS          PIC X(02) VALUE SPACES.              01280015
           10 FS-S1DQ9FTC          PIC X(02) VALUE SPACES.              01290015
       01  WSV-CLIENTE             PIC X(60) VALUE SPACES.              01300015
       01  WSV-FECHA-10-A          PIC X(10) VALUE SPACES.              01310015
       01  WSV-FECHA-8-N           PIC 9(08) VALUE ZEROS.               01320015
       01  WSV-FECHA-8-A REDEFINES WSV-FECHA-8-N PIC X(08).             01330015
       01  WSV-LEIDOS              PIC 9(08) VALUE ZEROS.               01340015
       01  WSV-ESCRITOS            PIC 9(08) VALUE ZEROS.               01350015
       01  WSV-FECHA-PRO.                                               01360015
           02 WSV-FECHA-PRO-A      PIC X(04).                           01370015
           02 WSV-FECHA-PRO-M      PIC X(02).                           01380015
           02 WSV-FECHA-PRO-D      PIC X(02).                           01390015
       01  WSN-FECHA-PRO-N REDEFINES WSV-FECHA-PRO PIC 9(08).           01400015
      *                                                                *01410015
       01  WR-NEGLOT.                                                   01420015
           02  WA-TIPNEG             PIC  X(01)    VALUE 'L'.           01430015
           02  WA-NEGLOT             PIC S9(07)V   USAGE COMP-3.        01440015
      *                                                                *01450015
       01  REG-S1DQ9FTC-W.                                              01460015
           10 S01-NUMCLI           PIC X(08).                           01470015
           10 S01-TIPDOC           PIC X(01).                           01480015
           10 S01-NRODOC           PIC X(11).                           01490015
           10 S01-CTAVAL20         PIC X(18).                           01500015
           10 S01-MONCONTR         PIC X(03).                           01510015
           10 S01-FECALTA          PIC X(08).                           01520015
           10 S01-FECCESE          PIC X(08).                           01530015
           10 S01-SIGNO-SDOREGI    PIC X(01).                           01540015
           10 S01-SDOREGI          PIC 9(12)V9(02).                     01550015
           10 S01-SIGNO-SDOINVE    PIC X(01).                           01560015
           10 S01-SDOINVE          PIC 9(12)V9(02).                     01570015
           10 S01-SIGNO-IMPOVTA    PIC X(01).                           01580015
           10 S01-IMPOVTA          PIC 9(12)V9(02).                     01590015
           10 S01-SIGNO-IMPINTE    PIC X(01).                           01600015
           10 S01-IMPINTE          PIC 9(12)V9(02).                     01610015
           10 S01-SIGNO-IMPDIVI    PIC X(01).                           01620015
           10 S01-IMPDIVI          PIC 9(12)V9(02).                     01630015
           10 S01-SIGNO-IMPVCTO    PIC X(01).                           01640015
           10 S01-IMPVCTO          PIC 9(12)V9(02).                     01650015
           10 S01-DIVISA           PIC X(03).                           01660015
           10 S01-RUT              PIC 9(08).                           01670015
           10 S01-IND-CTAREG       PIC X(02).                           01680015
           10 S01-SIGNO-SDOREGULTI PIC X(01).                           01690015
           10 S01-SDOREGULTI       PIC 9(12)V9(02).                     01700015
           10 S01-FCHREGULTI       PIC X(08).                           01710015
           10 S01-FEALTREG         PIC X(08).                           01720015
           10 S01-SIGNO-ULTINVE    PIC X(01).                           01730015
           10 S01-ULTINVE          PIC 9(12)V9(02).                     01740015
           10 S01-FHULINVE         PIC X(08).                           01750015
      *                                                                *01760015
       01  WA-S1DQ9FTC.                                                 01770015
           10 W01-NUMCLI           PIC X(08).                           01780015
           10 W01-TIPDOC           PIC X(01).                           01790015
           10 W01-NRODOC           PIC X(11).                           01800015
           10 W01-CTAVAL20         PIC X(18).                           01810015
           10 W01-MONCONTR         PIC X(03).                           01820015
           10 W01-FECALTA          PIC X(08).                           01830015
           10 W01-FECCESE          PIC X(08).                           01840015
           10 W01-SIGNO-SDOREGI    PIC X(01).                           01850015
           10 W01-SDOREGI          PIC 9(12)V9(02).                     01860015
           10 W01-SIGNO-SDOINVE    PIC X(01).                           01870015
           10 W01-SDOINVE          PIC 9(12)V9(02).                     01880015
           10 W01-SIGNO-IMPOVTA    PIC X(01).                           01890015
           10 W01-IMPOVTA          PIC 9(12)V9(02).                     01900015
           10 W01-SIGNO-IMPINTE    PIC X(01).                           01910015
           10 W01-IMPINTE          PIC 9(12)V9(02).                     01920015
           10 W01-SIGNO-IMPDIVI    PIC X(01).                           01930015
           10 W01-IMPDIVI          PIC 9(12)V9(02).                     01940015
           10 W01-SIGNO-IMPVCTO    PIC X(01).                           01950015
           10 W01-IMPVCTO          PIC 9(12)V9(02).                     01960015
           10 W01-DIVISA           PIC X(03).                           01970015
           10 W01-RUT              PIC 9(08).                           01980015
           10 W01-IND-CTAREG       PIC X(02).                           01990015
           10 W01-SIGNO-SDOREGULTI PIC X(01).                           02000015
           10 W01-SDOREGULTI       PIC 9(12)V9(02).                     02010015
           10 W01-FCHREGULTI       PIC X(08).                           02020015
           10 W01-FEALTREG         PIC X(08).                           02030015
           10 W01-SIGNO-ULTINVE    PIC X(01).                           02040015
           10 W01-ULTINVE          PIC 9(12)V9(02).                     02050015
           10 W01-FHULINVE         PIC X(08).                           02060015
      *                                                                *02070015
       01  WA-VAR-SALDOS.                                               02080015
           02 WA-SALDO                PIC  9(15).                       02090015
           02 WH-SALD0                PIC S9(15).                       02100015
           02 WH-NOMINEM              PIC  9(13)V9(05).                 02110015
           02 WA-SALDO-INVER          PIC  9(15)V9(02).                 02120015
           02 WA-SALDO-INVER-0        PIC  9(15)V9(02).                 02130015
           02 WA-SALDO-VENTA          PIC  9(15)V9(02).                 02140015
           02 WA-DIVPEN               PIC  9(15)V9(02).                 02150015
           02 WA-INTPEN               PIC  9(15)V9(02).                 02160015
           02 WA-AMTPEN               PIC  9(15)V9(02).                 02170015
           02 WA-DIVUSD               PIC  9(15)V9(02).                 02180015
           02 WA-INTUSD               PIC  9(15)V9(02).                 02190015
           02 WA-AMTUSD               PIC  9(15)V9(02).                 02200015
      *                                                                *02210015
      *    BD PERSONAS                                                  02220015
       01  W-PEWC5201.                                                  02230015
           COPY PEWC5201.                                               02240015
RTP0   01 PEWC5000.                                                     02241016
RTP0       COPY PEWC5000.                                               02242016
      *    TAB-VLDTHIS : COPY CON OCCURS                                02250015
           COPY VLTCHI2.                                                02260015
      *                                                                *02270015
      ******************************************************************02280015
      ********* COPYS DE ERRORES                             ***********02290015
      ******************************************************************02300015
      *                                                                *02310015
           COPY QRECDB2.                                                02320015
           COPY VLWCRUTI.                                               02330015
           COPY VLWC8000.                                               02340015
           COPY VLWC8010.                                               02350015
           COPY VLWC8020.                                               02360015
      *                                                                *02370015
      ******************************************************************02380015
      ********* INCLUDE DE TABLAS                            ***********02390015
      ******************************************************************02400015
      *                                                                *02410015
           EXEC SQL INCLUDE SQLCA   END-EXEC.                           02420015
           EXEC SQL INCLUDE VLGTARC END-EXEC.                           02430015
           EXEC SQL INCLUDE VLGTPOL END-EXEC.                           02440015
           EXEC SQL INCLUDE VLGTXEN END-EXEC.                           02450015
           EXEC SQL INCLUDE VLGTCAM END-EXEC.                           02460015
           EXEC SQL INCLUDE VLGTHAC END-EXEC.                           02470015
           EXEC SQL INCLUDE VLGTADS END-EXEC.                           02480015
N*         EXEC SQL INCLUDE VLGTSMO END-EXEC.                           02490015
N          EXEC SQL INCLUDE VLGTSMM END-EXEC.                           02500015
N          EXEC SQL INCLUDE VLGTHIS END-EXEC.                           02510015
      *                                                                *02520015
      *----------------------------------------------------------------*02530015
      *  AREA CURSORES DB2                                             *02540015
      *----------------------------------------------------------------*02550015
      *    COTIZACIONES DE INVERSIONES                                 *02560015
           EXEC SQL                                                     02570015
                DECLARE VLDCCAM  CURSOR FOR                             02580015
                 SELECT VCAM_CIERRE_D                                   02590015
                      , VCAM_FECDIA                                     02600015
                      , VCAM_FILLER                                     02610015
                   FROM VLDTCAM                                         02620015
                  WHERE VCAM_CODVALOR  = :VCAM-CODVALOR                 02630015
                    AND VCAM_FECDIA   <= :VCAM-FECDIA                   02640015
                    AND VCAM_CIERRE_D <>  0                             02650015
                  ORDER BY VCAM_FECDIA DESC                             02660015
                  OPTIMIZE FOR 1 ROW                                    02670015
           END-EXEC.                                                    02680015
      *    OPERACIONES CONTABILIZADAS                                  *02690015
           EXEC SQL                                                     02700015
                DECLARE VLDCHAC  CURSOR FOR                             02710015
                 SELECT VHAC_OPERAC                                     02720015
                      , VHAC_IMPLIQ                                     02730015
                      , VHAC_MONEDA_CTA                                 02740015
                   FROM VLDTHAC                                         02750015
                  WHERE VHAC_CUENTA = :VHAC-CUENTA                      02760015
                    AND VHAC_OPERAC IN (07, 08, 11)                     02770015
                    AND VHAC_FCONTA BETWEEN :WHAC-FECINI AND            02780015
                                            :WHAC-FECFIN                02790015
           END-EXEC.                                                    02800015
      *    POLIZAS                                                      02810015
           EXEC SQL                                                     02820015
                DECLARE VLDCPOL  CURSOR FOR                             02830015
                 SELECT VPOL_EFECTI                                     02840015
                      , VPOL_CUPCORR                                    02850015
                      , VPOL_IMPCOM1                                    02860015
                      , VPOL_IMPCOM2                                    02870015
                      , VPOL_IMPCOM3                                    02880015
                      , VPOL_IMPCOM4                                    02890015
                      , VPOL_IMPCOM5                                    02900015
                      , VPOL_IMPCOM6                                    02910015
                      , VPOL_IMPCOM7                                    02920015
                      , VPOL_IMPCOM8                                    02930015
                      , VPOL_IGV                                        02940015
                   FROM VLDTPOL                                         02950015
                  WHERE VPOL_CUENTA  = :VPOL-CUENTA                     02960015
                    AND VPOL_COMVEN  = :VPOL-COMVEN                     02970015
                    AND VPOL_SITUAC  = :VPOL-SITUAC                     02980015
                    AND VPOL_FECHEJ  BETWEEN :WPOL-FECINI AND           02990015
                                             :WPOL-FECFIN               03000015
           END-EXEC.                                                    03010015
      *    MOV-CTA-REGISTRO ULTIMO SALDO DEL AÑO                        03020015
           EXEC SQL                                                     03030015
                DECLARE VLDCSMM  CURSOR FOR                             03040015
                 SELECT VSMM_SALDO_AUT                                  03050015
                      , VSMM_NUMREF                                     03060015
                   FROM VLDTSMM                                         03070015
                  WHERE VSMM_CTAVAL    = :VSMM-CTAVAL                   03080015
                    AND VSMM_FECONTA  <= :VSMM-FECONTA                  03090015
                    AND VSMM_OBSERVA NOT IN (:WA-APERTURA, :WA-ENTRADA, 03100015
                                             :WA-SALIDA)                03110015
                  ORDER BY VSMM_NUMREF DESC                             03120015
                  OPTIMIZE FOR 1 ROW                                    03130015
           END-EXEC.                                                    03140015
      *    MOV-CTA-REGISTRO ULTIMO SALDO                                03150015
           EXEC SQL                                                     03160015
                DECLARE VLDUSMM  CURSOR FOR                             03170015
                 SELECT VSMM_SALDO_AUT                                  03180015
                      , VSMM_FECONTA                                    03190015
                   FROM VLDTSMM                                         03200015
                  WHERE VSMM_CTAVAL     = :VSMM-CTAVAL                  03210015
                    AND VSMM_FECONTA   <= :VSMM-FECONTA                 03220015
                    AND VSMM_SALDO_AUT <> 0                             03230015
                    AND VSMM_OBSERVA NOT IN (:WA-APERTURA, :WA-ENTRADA, 03240015
                                             :WA-SALIDA)                03250015
                  ORDER BY VSMM_NUMREF DESC                             03260015
                  OPTIMIZE FOR 1 ROW                                    03270015
           END-EXEC.                                                    03280015
      *    HIS-SDO-MENSUAL- ULTIMO SALDO TITULOS                        03290015
           EXEC SQL                                                     03300015
                DECLARE VLDUHIS  CURSOR FOR                             03310015
                 SELECT VHIS_CODVALOR                                   03320015
                      , VHIS_TITULOS1                                   03330015
                      , VHIS_MOVIMI1                                    03340015
                      , VHIS_CUSTODIA1                                  03350015
                      , VHIS_CAMBIO1                                    03360015
                      , VHIS_COBRADO1                                   03370015
                      , VHIS_TITULOS2                                   03380015
                      , VHIS_MOVIMI2                                    03390015
                      , VHIS_CUSTODIA2                                  03400015
                      , VHIS_CAMBIO2                                    03410015
                      , VHIS_COBRADO2                                   03420015
                      , VHIS_TITULOS3                                   03430015
                      , VHIS_MOVIMI3                                    03440015
                      , VHIS_CUSTODIA3                                  03450015
                      , VHIS_CAMBIO3                                    03460015
                      , VHIS_COBRADO3                                   03470015
                      , VHIS_TITULOS4                                   03480015
                      , VHIS_MOVIMI4                                    03490015
                      , VHIS_CUSTODIA4                                  03500015
                      , VHIS_CAMBIO4                                    03510015
                      , VHIS_COBRADO4                                   03520015
                      , VHIS_TITULOS5                                   03530015
                      , VHIS_MOVIMI5                                    03540015
                      , VHIS_CUSTODIA5                                  03550015
                      , VHIS_CAMBIO5                                    03560015
                      , VHIS_COBRADO5                                   03570015
                      , VHIS_TITULOS6                                   03580015
                      , VHIS_MOVIMI6                                    03590015
                      , VHIS_CUSTODIA6                                  03600015
                      , VHIS_CAMBIO6                                    03610015
                      , VHIS_COBRADO6                                   03620015
                      , VHIS_TITULOS7                                   03630015
                      , VHIS_MOVIMI7                                    03640015
                      , VHIS_CUSTODIA7                                  03650015
                      , VHIS_CAMBIO7                                    03660015
                      , VHIS_COBRADO7                                   03670015
                      , VHIS_TITULOS8                                   03680015
                      , VHIS_MOVIMI8                                    03690015
                      , VHIS_CUSTODIA8                                  03700015
                      , VHIS_CAMBIO8                                    03710015
                      , VHIS_COBRADO8                                   03720015
                      , VHIS_TITULOS9                                   03730015
                      , VHIS_MOVIMI9                                    03740015
                      , VHIS_CUSTODIA9                                  03750015
                      , VHIS_CAMBIO9                                    03760015
                      , VHIS_COBRADO9                                   03770015
                      , VHIS_TITULOS10                                  03780015
                      , VHIS_MOVIMI10                                   03790015
                      , VHIS_CUSTODIA10                                 03800015
                      , VHIS_CAMBIO10                                   03810015
                      , VHIS_COBRADO10                                  03820015
                      , VHIS_TITULOS11                                  03830015
                      , VHIS_MOVIMI11                                   03840015
                      , VHIS_CUSTODIA11                                 03850015
                      , VHIS_CAMBIO11                                   03860015
                      , VHIS_COBRADO11                                  03870015
                      , VHIS_TITULOS12                                  03880015
                      , VHIS_MOVIMI12                                   03890015
                      , VHIS_CUSTODIA12                                 03900015
                      , VHIS_CAMBIO12                                   03910015
                      , VHIS_COBRADO12                                  03920015
                      , VHIS_TITULOS13                                  03930015
                      , VHIS_MOVIMI13                                   03940015
                      , VHIS_CUSTODIA13                                 03950015
                      , VHIS_CAMBIO13                                   03960015
                      , VHIS_COBRADO13                                  03970015
                      , VHIS_TITULOS14                                  03980015
                      , VHIS_MOVIMI14                                   03990015
                      , VHIS_CUSTODIA14                                 04000015
                      , VHIS_CAMBIO14                                   04010015
                      , VHIS_COBRADO14                                  04020015
                      , VHIS_TITULOS15                                  04030015
                      , VHIS_MOVIMI15                                   04040015
                      , VHIS_CUSTODIA15                                 04050015
                      , VHIS_CAMBIO15                                   04060015
                      , VHIS_COBRADO15                                  04070015
                      , VHIS_TITULOS16                                  04080015
                      , VHIS_MOVIMI16                                   04090015
                      , VHIS_CUSTODIA16                                 04100015
                      , VHIS_CAMBIO16                                   04110015
                      , VHIS_COBRADO16                                  04120015
                      , VHIS_TITULOS17                                  04130015
                      , VHIS_MOVIMI17                                   04140015
                      , VHIS_CUSTODIA17                                 04150015
                      , VHIS_CAMBIO17                                   04160015
                      , VHIS_COBRADO17                                  04170015
                      , VHIS_TITULOS18                                  04180015
                      , VHIS_MOVIMI18                                   04190015
                      , VHIS_CUSTODIA18                                 04200015
                      , VHIS_CAMBIO18                                   04210015
                      , VHIS_COBRADO18                                  04220015
                      , VHIS_TITULOS19                                  04230015
                      , VHIS_MOVIMI19                                   04240015
                      , VHIS_CUSTODIA19                                 04250015
                      , VHIS_CAMBIO19                                   04260015
                      , VHIS_COBRADO19                                  04270015
                      , VHIS_TITULOS20                                  04280015
                      , VHIS_MOVIMI20                                   04290015
                      , VHIS_CUSTODIA20                                 04300015
                      , VHIS_CAMBIO20                                   04310015
                      , VHIS_COBRADO20                                  04320015
                      , VHIS_TITULOS21                                  04330015
                      , VHIS_MOVIMI21                                   04340015
                      , VHIS_CUSTODIA21                                 04350015
                      , VHIS_CAMBIO21                                   04360015
                      , VHIS_COBRADO21                                  04370015
                      , VHIS_TITULOS22                                  04380015
                      , VHIS_MOVIMI22                                   04390015
                      , VHIS_CUSTODIA22                                 04400015
                      , VHIS_CAMBIO22                                   04410015
                      , VHIS_COBRADO22                                  04420015
                      , VHIS_TITULOS23                                  04430015
                      , VHIS_MOVIMI23                                   04440015
                      , VHIS_CUSTODIA23                                 04450015
                      , VHIS_CAMBIO23                                   04460015
                      , VHIS_COBRADO23                                  04470015
                      , VHIS_TITULOS24                                  04480015
                      , VHIS_MOVIMI24                                   04490015
                      , VHIS_CUSTODIA24                                 04500015
                      , VHIS_CAMBIO24                                   04510015
                      , VHIS_COBRADO24                                  04520015
                      , VHIS_TITULOS25                                  04530015
                      , VHIS_MOVIMI25                                   04540015
                      , VHIS_CUSTODIA25                                 04550015
                      , VHIS_CAMBIO25                                   04560015
                      , VHIS_COBRADO25                                  04570015
                      , VHIS_TITULOS26                                  04580015
                      , VHIS_MOVIMI26                                   04590015
                      , VHIS_CUSTODIA26                                 04600015
                      , VHIS_CAMBIO26                                   04610015
                      , VHIS_COBRADO26                                  04620015
                      , VHIS_TITULOS27                                  04630015
                      , VHIS_MOVIMI27                                   04640015
                      , VHIS_CUSTODIA27                                 04650015
                      , VHIS_CAMBIO27                                   04660015
                      , VHIS_COBRADO27                                  04670015
                      , VHIS_TITULOS28                                  04680015
                      , VHIS_MOVIMI28                                   04690015
                      , VHIS_CUSTODIA28                                 04700015
                      , VHIS_CAMBIO28                                   04710015
                      , VHIS_COBRADO28                                  04720015
                      , VHIS_TITULOS29                                  04730015
                      , VHIS_MOVIMI29                                   04740015
                      , VHIS_CUSTODIA29                                 04750015
                      , VHIS_CAMBIO29                                   04760015
                      , VHIS_COBRADO29                                  04770015
                      , VHIS_TITULOS30                                  04780015
                      , VHIS_MOVIMI30                                   04790015
                      , VHIS_CUSTODIA30                                 04800015
                      , VHIS_CAMBIO30                                   04810015
                      , VHIS_COBRADO30                                  04820015
                      , VHIS_TITULOS31                                  04830015
                      , VHIS_MOVIMI31                                   04840015
                      , VHIS_CUSTODIA31                                 04850015
                      , VHIS_CAMBIO31                                   04860015
                      , VHIS_COBRADO31                                  04870015
                      , VHIS_FEALTREG                                   04880015
                      , VHIS_FEULMOD                                    04890015
                      , VHIS_HORULMOD                                   04900015
                      , VHIS_NUMTER                                     04910015
                      , VHIS_USUARIO                                    04920015
                   FROM VLDTHIS                                         04930015
                  WHERE VHIS_CTAVAL   = :VHIS-CTAVAL                    04940015
                    AND VHIS_TIPGAS  IN (48, 49)                        04950015
                    AND VHIS_ANO      = :VHIS-ANO                       04960015
                    AND VHIS_MES      = :VHIS-MES                       04970015
                    AND VHIS_CODVALOR > :VHIS-CODVALOR                  04980015
           END-EXEC.                                                    04990015
      *                                                                *05000015
      *---------------*                                                 05010015
       LINKAGE SECTION.                                                 05020015
      *---------------*                                                 05030015
       01  LK-PARAMETROS.                                               05040015
           02  LK-LONGITUD     PIC S9(4)   COMP.                        05050015
           02  LK-FECHA-D.                                              05060015
               03  LK-F-AA-D   PIC 9999.                                05070015
               03  LK-F-MM-D   PIC 99.                                  05080015
               03  LK-F-DD-D   PIC 99.                                  05090015
           02  LK-RFECHA-D  REDEFINES LK-FECHA-D PIC 9(08).             05100015
           02  LK-FECHA-H.                                              05110015
               03  LK-F-AA-H   PIC 9999.                                05120015
               03  LK-F-MM-H   PIC 99.                                  05130015
               03  LK-F-DD-H   PIC 99.                                  05140015
           02  LK-RFECHA-H  REDEFINES LK-FECHA-H PIC 9(08).             05150015
      *                                                                 05160015
      *---------------------------------------*                         05170015
       PROCEDURE DIVISION USING LK-PARAMETROS.                          05180015
      *---------------------------------------*                         05190015
      *                                                                 05200015
           PERFORM 10000-INICIO.                                        05210015
      *                                                                 05220015
           PERFORM 20000-PROCESO UNTIL FS-E1DQ9FTC = '10'.              05230015
      *                                                                 05240015
           PERFORM 30000-FIN.                                           05250015
      *                                                                 05260015
           STOP RUN.                                                    05270015
      *                                                                 05280015
      ******************************************************************05290015
      *                       1-INICIO                                 *05300015
      *       INICIALIZA LAS WORKAS DE LAS TABLAS Y LOS CAMPOS DE      *05310015
      *       TRABAJO. LEE LA FECHA EN LA TABLA UGDTPRC, TOMANDO LA    *05320015
      *       FILA CON CODIGO DE PROCESO = 'UB00' Y COMPRUEBA QUE      *05330015
      *       EL PROCESO ESTA ACTIVO.                                  *05340015
      ******************************************************************05350015
       10000-INICIO.                                                    05360015
      *-------------*                                                   05370015
      *                                                                 05380015
PAVXX *    MOVE    LK-RFECHA       TO WSV-FECHA-PRO.                    05390015
PAVXX *    MOVE    WSN-FECHA-PRO-N TO WSV-FECHA-DES-N, WSV-FECHA-HAS-N. 05400015
      *    MOVE    01              TO WSV-FECHA-DES-D, WSV-FECHA-DES-M. 05410015
           MOVE  LK-RFECHA-H        TO WSV-FECHA-PRO                    05420015
           MOVE  LK-RFECHA-D        TO WSV-FECHA-DES                    05430015
           MOVE  LK-RFECHA-H        TO WSV-FECHA-HAS                    05440015
           MOVE  WSV-FECHA-DES-N    TO WPOL-FECINI, WHAC-FECINI         05450015
           MOVE  WSV-FECHA-HAS-N    TO WPOL-FECFIN, WHAC-FECFIN         05460015
      *                                                                *05470015
           OPEN INPUT  E1DQ9FTC, E2DQ9ADS                               05480015
                OUTPUT S1DQ9FTC.                                        05490015
      *                                                                *05500015
           IF (FS-E1DQ9FTC EQUAL '00' OR '97')                          05510015
              CONTINUE                                                  05520015
           ELSE                                                         05530015
              DISPLAY '***********************************'             05540015
              DISPLAY '*  ERROR AL OPEN DE ENTRADA1      *'             05550015
              DISPLAY '*  ERROR FS-OPS ES :' FS-E1DQ9FTC                05560015
              DISPLAY '***********************************'             05570015
              MOVE '02'  TO RETURN-CODE                                 05580015
              STOP RUN                                                  05590015
           END-IF                                                       05600015
      *                                                                *05610015
           IF (FS-E2DQ9ADS EQUAL '00' OR '97')                          05620015
              CONTINUE                                                  05630015
           ELSE                                                         05640015
              DISPLAY '***********************************'             05650015
              DISPLAY '*  ERROR AL OPEN DE ENTRADA1      *'             05660015
              DISPLAY '*  ERROR FS-OPS ES :' FS-E2DQ9ADS                05670015
              DISPLAY '***********************************'             05680015
              MOVE '02'  TO RETURN-CODE                                 05690015
              STOP RUN                                                  05700015
           END-IF                                                       05710015
      *                                                                *05720015
           IF (FS-S1DQ9FTC EQUAL '00' OR '97')                          05730015
              CONTINUE                                                  05740015
           ELSE                                                         05750015
              DISPLAY '***********************************'             05760015
              DISPLAY '*  ERROR AL OPEN DE SALIDA1       *'             05770015
              DISPLAY '*  ERROR FS-OPS ES :' FS-S1DQ9FTC                05780015
              DISPLAY '***********************************'             05790015
              MOVE '02'  TO RETURN-CODE                                 05800015
              STOP RUN                                                  05810015
           END-IF.                                                      05820015
      *                                                                *05830015
           MOVE ZERO TO VADS-CUENTA.                                    05840015
      *                                                                *05850015
           PERFORM 10010-LEER-ENTRADA.                                  05860015
      *                                                                *05870015
           PERFORM 10020-LEER-SALDOS.                                   05880015
      *                                                                *05890015
      *     *------------*                                              05900015
       10010-LEER-ENTRADA.                                              05910015
      *     *------------*                                              05920015
      *                                                                 05930015
           READ E1DQ9FTC.                                               05940015
                                                                        05950015
           EVALUATE FS-E1DQ9FTC                                         05960015
              WHEN '00'                                                 05970015
                   ADD  1                      TO WSV-LEIDOS            05980015
                   MOVE E01-CTAVAL20 (13:07)   TO WX-CUENTA-ARC7        05990015
                   MOVE WN-CUENTA-ARC7         TO WA-CUENTA-ARC7        06000015
              WHEN '10'                                                 06010015
                   CONTINUE                                             06020015
              WHEN OTHER                                                06030015
                   DISPLAY '***********************************'        06040015
                   DISPLAY '*  ERROR AL LEER ENTRADA          *'        06050015
                   DISPLAY '*  ERROR FS-OPS ES :' FS-E1DQ9FTC           06060015
                   DISPLAY '***********************************'        06070015
                   MOVE '02'  TO RETURN-CODE                            06080015
                   STOP RUN                                             06090015
           END-EVALUATE                                                 06100015
           .                                                            06110015
      *                                                                 06120015
      *     *-----------*                                               06130015
       10020-LEER-SALDOS.                                               06140015
      *     *-----------*                                               06150015
      *                                                                 06160015
           READ E2DQ9ADS.                                               06170015
                                                                        06180015
           EVALUATE FS-E2DQ9ADS                                         06190015
              WHEN '00'                                                 06200015
                   MOVE REG-E2DQ9ADS           TO DCLVLDTADS            06210015
              WHEN '10'                                                 06220015
                   CONTINUE                                             06230015
              WHEN OTHER                                                06240015
                   DISPLAY '***********************************'        06250015
                   DISPLAY '*  ERROR AL LEER ENTRADA SALDOS   *'        06260015
                   DISPLAY '*  ERROR FS-ADS ES :' FS-E2DQ9ADS           06270015
                   DISPLAY '***********************************'        06280015
                   MOVE '02'  TO RETURN-CODE                            06290015
                   STOP RUN                                             06300015
           END-EVALUATE.                                                06310015
      *                                                                *06320015
      *     *------------*                                              06330015
       10030-PRECIO-VALOR.                                              06340015
      *     *------------*                                              06350015
      *                                                                *06360015
      *                                                                *06370015
           EXEC SQL                                                     06380015
                OPEN VLDCCAM                                            06390015
           END-EXEC                                                     06400015
      *                                                                 06410015
           MOVE SQLCODE TO SQLCODE-AUX                                  06420015
      *                                                                 06430015
           EVALUATE TRUE                                                06440015
               WHEN DB2-OK                                              06450015
                    CONTINUE                                            06460015
               WHEN OTHER                                               06470015
                    MOVE 'VL4CFTC0'              TO  W801-PROGRAMA      06480015
                    MOVE 'VLDTCAM'               TO  W801-TABLA         06490015
                    MOVE 'OPEN'                  TO  W801-ACCION        06500015
                    MOVE WX-CUENTA-ARC7          TO  W801-CLAVE (01:07) 06510015
                    MOVE VCAM-CODVALOR           TO  W801-CLAVE (09:12) 06520015
                    MOVE SQLCODE                 TO  W801-SQLCODE       06530015
                    MOVE SPACES                  TO  W801-SQLWARN       06540015
                    MOVE '10030-PRECIO-VALOR   ' TO  W801-PARRAFO       06550015
                    PERFORM VLPC8010-DISP-ABEND-DB2                     06560015
                    PERFORM VLPC8010-ABEND-DB2                          06570015
           END-EVALUATE.                                                06580015
      *                                                                 06590015
           EXEC SQL                                                     06600015
                FETCH  VLDCCAM                                          06610015
                 INTO :VCAM-CIERRE-D                                    06620015
                    , :VCAM-FECDIA                                      06630015
                    , :VCAM-FILLER                                      06640015
           END-EXEC                                                     06650015
                                                                        06660015
           MOVE  SQLCODE TO SQLCODE-AUX                                 06670015
                                                                        06680015
           EVALUATE TRUE                                                06690015
               WHEN DB2-OK                                              06700015
                    MOVE VCAM-FILLER (01:05) TO WR-NEGLOT               06710015
                    IF WA-TIPNEG = 'L' AND                              06720015
                       WA-NEGLOT >     ZEROS                            06730015
                       COMPUTE VCAM-CIERRE-D = VCAM-CIERRE-D / WA-NEGLOT06740015
                    END-IF                                              06750015
               WHEN DB2-NOTFND                                          06760015
                    IF VXEN-TIPINT = 'F'                                06770015
100%                   MOVE 100.00               TO  VCAM-CIERRE-D      06780015
                    ELSE                                                06790015
                       MOVE VXEN-NOMINEM         TO  VCAM-CIERRE-D      06800015
                    END-IF                                              06810015
               WHEN OTHER                                               06820015
                    MOVE 'VL4CFTC0'              TO  W801-PROGRAMA      06830015
                    MOVE 'VLDTCAM'               TO  W801-TABLA         06840015
                    MOVE 'SELECT-FIRST'          TO  W801-ACCION        06850015
                    MOVE WX-CUENTA-ARC7          TO  W801-CLAVE (01:07) 06860015
                    MOVE VCAM-CODVALOR           TO  W801-CLAVE (09:12) 06870015
                    MOVE SQLCODE                 TO  W801-SQLCODE       06880015
                    MOVE SPACES                  TO  W801-SQLWARN       06890015
                    MOVE '10030-PRECIO-VALOR   ' TO  W801-PARRAFO       06900015
                    PERFORM VLPC8010-DISP-ABEND-DB2                     06910015
                    PERFORM VLPC8010-ABEND-DB2                          06920015
           END-EVALUATE.                                                06930015
                                                                        06940015
           EXEC SQL                                                     06950015
                CLOSE VLDCCAM                                           06960015
           END-EXEC                                                     06970015
      *                                                                 06980015
           MOVE SQLCODE TO SQLCODE-AUX                                  06990015
      *                                                                 07000015
           EVALUATE TRUE                                                07010015
               WHEN DB2-OK                                              07020015
                    CONTINUE                                            07030015
               WHEN OTHER                                               07040015
                    MOVE 'VL4CFTC0'              TO  W801-PROGRAMA      07050015
                    MOVE 'VLDTCAM'               TO  W801-TABLA         07060015
                    MOVE 'CLOSE'                 TO  W801-ACCION        07070015
                    MOVE WX-CUENTA-ARC7          TO  W801-CLAVE (01:07) 07080015
                    MOVE VCAM-CODVALOR           TO  W801-CLAVE (09:12) 07090015
                    MOVE SQLCODE                 TO  W801-SQLCODE       07100015
                    MOVE SPACES                  TO  W801-SQLWARN       07110015
                    MOVE '10030-PRECIO-VALOR   ' TO  W801-PARRAFO       07120015
                    PERFORM VLPC8010-DISP-ABEND-DB2                     07130015
                    PERFORM VLPC8010-ABEND-DB2                          07140015
           END-EVALUATE.                                                07150015
      *                                                                *07160015
      *     *-------*                                                   07170015
       20000-PROCESO.                                                   07180015
      *     *-------*                                                   07190015
      *                                                                 07200015
           INITIALIZE REG-S1DQ9FTC-W                                    07210015
      *                                                                 07220015
           PERFORM 20030-GENERA-SALDOS                                  07230015
      *                                                                 07240015
RTP0  *    PERFORM 20010-OBTIENE-CLIENTE                                07250016
RTP0       PERFORM 220-RUTINA-PERSONA                                   07251016
      *                                                                 07260015
           MOVE E01-NUMCLI               TO S01-NUMCLI                  07270015
RTP0  *    MOVE W520-CODIDENT            TO S01-TIPDOC                  07280016
RTP0  *    MOVE W520-CLAIDENT            TO S01-NRODOC                  07290016
RTP0       MOVE W500-CODIDENT(1)         TO S01-TIPDOC                  07291016
RTP0       MOVE W500-CLAIDENT(1)         TO S01-NRODOC                  07292016
           MOVE E01-CTAVAL20 (01:08)     TO S01-CTAVAL20 (01:08)        07300015
           MOVE E01-CTAVAL20 (11:10)     TO S01-CTAVAL20 (09:10)        07310015
           MOVE E01-FECALTA (07:04)      TO S01-FECALTA (01:04)         07320015
           MOVE E01-FECALTA (04:02)      TO S01-FECALTA (05:02)         07330015
           MOVE E01-FECALTA (01:02)      TO S01-FECALTA (07:02)         07340015
           MOVE E01-FECCESE (07:04)      TO S01-FECCESE (01:04)         07350015
           MOVE E01-FECCESE (04:02)      TO S01-FECCESE (05:02)         07360015
           MOVE E01-FECCESE (01:02)      TO S01-FECCESE (07:02)         07370015
           MOVE E01-RUT                  TO S01-RUT                     07380015
                                                                        07390015
           IF WA-SALDO-INVER < ZEROS                                    07400015
              MOVE '-'                   TO S01-SIGNO-SDOINVE           07410015
           END-IF                                                       07420015
                                                                        07430015
           MOVE WA-SALDO-INVER           TO S01-SDOINVE                 07440015
                                                                        07450015
           IF WA-SALDO-VENTA < ZEROS                                    07460015
              MOVE '-'                   TO S01-SIGNO-IMPOVTA           07470015
           END-IF                                                       07480015
                                                                        07490015
           MOVE WA-SALDO-VENTA           TO S01-IMPOVTA                 07500015
           MOVE E01-MONEDA               TO S01-MONCONTR                07510015
                                                                        07520015
FVAXX *    IF VSMM-SALDO-AUT < ZEROS                                    07530015
FVAXX *       MOVE '-'                   TO S01-SIGNO-SDOREGI           07540015
FVAXX *    END-IF                                                       07550015
                                                                        07560015
FVAXX      IF E01-SITUACION = 'CANCELADA'                               07570015
FVAXX         MOVE SPACES                TO S01-SIGNO-SDOREGI           07580015
FVAXX         MOVE ZEROES                TO S01-SDOREGI                 07590015
FVAXX      ELSE                                                         07600015
FVAXX         IF VSMM-SALDO-AUT < ZEROS                                 07610015
FVAXX            MOVE SPACES             TO S01-SIGNO-SDOREGI           07620015
FVAXX            MOVE ZEROES             TO S01-SDOREGI                 07630015
FVAXX         ELSE                                                      07640015
                 MOVE VSMM-SALDO-AUT     TO S01-SDOREGI                 07650015
FVAXX         END-IF                                                    07660015
FVAXX      END-IF                                                       07670015
                                                                        07680015
FVAXX *    IF WSMM-SALDO-AUT < ZEROS                                    07690015
FVAXX *       MOVE '-'                   TO S01-SIGNO-SDOREGULTI        07700015
FVAXX *    END-IF                                                       07710015
                                                                        07720015
FVAXX      IF E01-SITUACION = 'CANCELADA'                               07730015
FVAXX         MOVE SPACES                TO S01-SIGNO-SDOREGULTI        07740015
FVAXX         MOVE ZEROES                TO S01-SDOREGULTI              07750015
FVAXX      ELSE                                                         07760015
FVAXX         IF VSMM-SALDO-AUT < ZEROS                                 07770015
FVAXX            MOVE SPACES             TO S01-SIGNO-SDOREGULTI        07780015
FVAXX            MOVE ZEROES             TO S01-SDOREGULTI              07790015
FVAXX         ELSE                                                      07800015
                 MOVE WSMM-SALDO-AUT     TO S01-SDOREGULTI              07810015
FVAXX         END-IF                                                    07820015
FVAXX      END-IF                                                       07830015
                                                                        07840015
           IF VSMM-FECONTA  = '9999-12-31'                              07850015
              MOVE SPACES                TO S01-FCHREGULTI              07860015
           ELSE                                                         07870015
              MOVE VSMM-FECONTA (01:04)  TO S01-FCHREGULTI (01:04)      07880015
              MOVE VSMM-FECONTA (06:02)  TO S01-FCHREGULTI (05:02)      07890015
              MOVE VSMM-FECONTA (09:02)  TO S01-FCHREGULTI (07:02)      07900015
           END-IF                                                       07910015
                                                                        07920015
           IF VSMO-FEALTREG = '9999-12-31'                              07930015
              MOVE 'NO'                  TO S01-IND-CTAREG              07940015
              MOVE SPACES                TO S01-FEALTREG                07950015
           ELSE                                                         07960015
              MOVE 'SI'                  TO S01-IND-CTAREG              07970015
              MOVE VSMO-FEALTREG (01:04) TO S01-FEALTREG (01:04)        07980015
              MOVE VSMO-FEALTREG (06:02) TO S01-FEALTREG (05:02)        07990015
              MOVE VSMO-FEALTREG (09:02) TO S01-FEALTREG (07:02)        08000015
           END-IF                                                       08010015
                                                                        08020015
           MOVE E01-MONEDA               TO S01-DIVISA                  08030015
                                                                        08040015
           IF WA-SALDO-INVER-0 < ZEROS                                  08050015
              MOVE '-'                   TO S01-SIGNO-ULTINVE           08060015
           END-IF                                                       08070015
                                                                        08080015
           MOVE WA-SALDO-INVER-0         TO S01-ULTINVE                 08090015
                                                                        08100015
           IF WHIS-FEC1RA-A > ZEROS                                     08110015
              MOVE WHIS-FEC1RA-A         TO S01-FHULINVE (01:04)        08120015
              MOVE WHIS-FEC1RA-M         TO S01-FHULINVE (05:02)        08130015
              MOVE WHIS-FEC1RA-D         TO S01-FHULINVE (07:02)        08140015
           ELSE                                                         08150015
              MOVE SPACES                TO S01-FHULINVE                08160015
           END-IF                                                       08170015
                                                                        08180015
           IF E01-MONEDA = 'PEN'                                        08190015
                                                                        08200015
              IF WA-DIVPEN < ZEROS                                      08210015
                 MOVE '-'                TO S01-SIGNO-IMPDIVI           08220015
              END-IF                                                    08230015
                                                                        08240015
              MOVE WA-DIVPEN             TO S01-IMPDIVI                 08250015
                                                                        08260015
              IF WA-INTPEN < ZEROS                                      08270015
                 MOVE '-'                TO S01-SIGNO-IMPINTE           08280015
              END-IF                                                    08290015
                                                                        08300015
              MOVE WA-INTPEN             TO S01-IMPINTE                 08310015
                                                                        08320015
              IF WA-AMTPEN < ZEROS                                      08330015
                 MOVE '-'                TO S01-SIGNO-IMPVCTO           08340015
              END-IF                                                    08350015
                                                                        08360015
              MOVE WA-AMTPEN             TO S01-IMPVCTO                 08370015
              MOVE REG-S1DQ9FTC          TO WA-S1DQ9FTC                 08380015
              PERFORM 20020-GRABA-SALIDA                                08390015
           ELSE                                                         08400015
                                                                        08410015
              IF WA-DIVUSD < ZEROS                                      08420015
                 MOVE '-'                TO S01-SIGNO-IMPDIVI           08430015
              END-IF                                                    08440015
                                                                        08450015
              MOVE WA-DIVUSD             TO S01-IMPDIVI                 08460015
                                                                        08470015
              IF WA-INTUSD < ZEROS                                      08480015
                 MOVE '-'                TO S01-SIGNO-IMPINTE           08490015
              END-IF                                                    08500015
                                                                        08510015
              MOVE WA-INTUSD             TO S01-IMPINTE                 08520015
                                                                        08530015
              IF WA-AMTUSD < ZEROS                                      08540015
                 MOVE '-'                TO S01-SIGNO-IMPVCTO           08550015
              END-IF                                                    08560015
                                                                        08570015
              MOVE WA-AMTUSD             TO S01-IMPVCTO                 08580015
              MOVE REG-S1DQ9FTC          TO WA-S1DQ9FTC                 08590015
              PERFORM 20020-GRABA-SALIDA                                08600015
           END-IF                                                       08610015
                                                                        08620015
      * PARA LIQUIDACIONES CON MONEDA DIFERENTE AL CONTRATO             08630015
           IF E01-MONEDA = 'PEN'                                        08640015
              IF WA-DIVUSD > ZEROS OR                                   08650015
                 WA-INTUSD > ZEROS OR                                   08660015
                 WA-AMTUSD > ZEROS                                      08670015
                 MOVE WA-S1DQ9FTC        TO REG-S1DQ9FTC                08680015
                 MOVE SPACES             TO S01-SIGNO-SDOINVE           08690015
                 MOVE ZEROS              TO S01-SDOINVE                 08700015
                 MOVE SPACES             TO S01-SIGNO-IMPOVTA           08710015
                 MOVE ZEROS              TO S01-IMPOVTA                 08720015
                                                                        08730015
                 IF WA-DIVUSD < ZEROS                                   08740015
                    MOVE '-'             TO S01-SIGNO-IMPDIVI           08750015
                 END-IF                                                 08760015
                                                                        08770015
                 MOVE WA-DIVUSD          TO S01-IMPDIVI                 08780015
                                                                        08790015
                 IF WA-INTUSD < ZEROS                                   08800015
                    MOVE '-'             TO S01-SIGNO-IMPINTE           08810015
                 END-IF                                                 08820015
                                                                        08830015
                 MOVE WA-INTUSD          TO S01-IMPINTE                 08840015
                                                                        08850015
                 IF WA-AMTUSD < ZEROS                                   08860015
                    MOVE '-'             TO S01-SIGNO-IMPVCTO           08870015
                 END-IF                                                 08880015
                                                                        08890015
                 MOVE WA-AMTUSD          TO S01-IMPVCTO                 08900015
                 MOVE 'USD'              TO S01-DIVISA                  08910015
                 PERFORM 20020-GRABA-SALIDA                             08920015
              END-IF                                                    08930015
           ELSE                                                         08940015
              IF WA-DIVPEN > ZEROS OR                                   08950015
                 WA-INTPEN > ZEROS OR                                   08960015
                 WA-AMTPEN > ZEROS                                      08970015
                 MOVE WA-S1DQ9FTC        TO REG-S1DQ9FTC                08980015
                 MOVE SPACES             TO S01-SIGNO-SDOINVE           08990015
                 MOVE ZEROS              TO S01-SDOINVE                 09000015
                 MOVE SPACES             TO S01-SIGNO-IMPOVTA           09010015
                 MOVE ZEROS              TO S01-IMPOVTA                 09020015
                                                                        09030015
                 IF WA-DIVPEN < ZEROS                                   09040015
                    MOVE '-'             TO S01-SIGNO-IMPDIVI           09050015
                 END-IF                                                 09060015
                                                                        09070015
                 MOVE WA-DIVPEN          TO S01-IMPDIVI                 09080015
                                                                        09090015
                 IF WA-INTPEN < ZEROS                                   09100015
                    MOVE '-'             TO S01-SIGNO-IMPINTE           09110015
                 END-IF                                                 09120015
                                                                        09130015
                 MOVE WA-INTPEN          TO S01-IMPINTE                 09140015
                                                                        09150015
                 IF WA-AMTPEN < ZEROS                                   09160015
                    MOVE '-'             TO S01-SIGNO-IMPVCTO           09170015
                 END-IF                                                 09180015
                                                                        09190015
                 MOVE WA-AMTPEN          TO S01-IMPVCTO                 09200015
                 MOVE 'PEN'              TO S01-DIVISA                  09210015
                PERFORM 20020-GRABA-SALIDA                              09220015
              END-IF                                                    09230015
           END-IF                                                       09240015
                                                                        09250015
           PERFORM 10010-LEER-ENTRADA                                   09260015
           .                                                            09270015
      *                                                                 09280015
      *     *---------------*                                           09290015
       20010-OBTIENE-CLIENTE.                                           09300015
      *     *---------------*                                           09310015
      *                                                                 09320015
           INITIALIZE           W520-REGISTRO                           09330015
      *                                                                 09340015
           MOVE E01-NUMCLI         TO W520-NUMCLIEN                     09350015
           MOVE SPACES             TO WSV-CLIENTE                       09360015
                                                                        09370015
           CALL PE9C5201 USING W-PEWC5201                               09380015
                                                                        09390015
           EVALUATE W520-PECRETOR                                       09400015
              WHEN ZEROS                                                09410015
                  IF W520-SUJGRUP = 'F'                                 09420015
                     STRING W520-PRIAPE DELIMITED BY '  ' ' '           09430015
                            W520-SEGAPE DELIMITED BY '  ' ' '           09440015
                            W520-NOMBRE DELIMITED BY '  '               09450015
                                              INTO WSV-CLIENTE          09460015
                  ELSE                                                  09470015
                     STRING W520-NOMBRE DELIMITED BY SIZE               09480015
                            W520-PRIAPE DELIMITED BY SIZE               09490015
                            W520-SEGAPE DELIMITED BY SIZE               09500015
                                              INTO WSV-CLIENTE          09510015
                  END-IF                                                09520015
              WHEN OTHER                                                09530015
                   MOVE '***NO UBICADO***'          TO  WSV-CLIENTE     09540015
           END-EVALUATE                                                 09550015
           .                                                            09560015
      *                                                                 09561016
       220-RUTINA-PERSONA.                                              09562016
      *-------------------*                                             09563016
      *                                                                 09564016
           INITIALIZE W500-REGISTRO                                     09565016
           MOVE SPACES               TO WSV-CLIENTE                     09566016
                                                                        09567016
           MOVE E01-CTAVAL20(13:08)  TO  W500-NUMECTA                   09568016
           MOVE E01-CTAVAL20(01:04)  TO  W500-PECENTID                  09569016
           MOVE E01-CTAVAL20(05:04)  TO  W500-OFIAPE                    09569116
           MOVE E01-CTAVAL20(11:02)  TO  W500-CODISER                   09569216
           MOVE 'T'                  TO  W500-CLAINTER                  09569316
           MOVE '01'                 TO  W500-SECINTER                  09569416
           MOVE 'U'                  TO  W500-PEYSELEC                  09569516
                                                                        09569616
           CALL PE9C5000 USING PEWC5000                                 09569716
                                                                        09569816
           IF W500-PECRETOR = '00'                                      09569916
              EVALUATE W500-SUJGRUP(1)                                  09570016
              WHEN 'F'                                                  09570116
                   STRING W500-PRIAPE(1) DELIMITED BY '  ' ' '          09570216
                          W500-SEGAPE(1) DELIMITED BY '  ' ' '          09570316
                          W500-NOMBRE(1) DELIMITED BY '  '              09570416
                          INTO WSV-CLIENTE                              09570516
                   END-STRING                                           09570616
                                                                        09570716
              WHEN 'M'                                                  09570816
                   STRING W500-NOMBRE(1) DELIMITED BY SIZE              09570916
                          W500-PRIAPE(1) DELIMITED BY SIZE              09571016
                          W500-SEGAPE(1) DELIMITED BY SIZE              09571116
                          INTO WSV-CLIENTE                              09571216
                   END-STRING                                           09571316
              END-EVALUATE                                              09571416
           END-IF                                                       09571516
           .                                                            09571616
                                                                        09571716
      *                                                                 09572015
      *     *------------*                                              09580015
       20020-GRABA-SALIDA.                                              09590015
      *     *------------*                                              09600015
      *                                                                 09610015
           WRITE REG-S1DQ9FTC FROM REG-S1DQ9FTC-W.                      09620015
                                                                        09630015
           IF (FS-S1DQ9FTC EQUAL '00')                                  09640015
               ADD 1                   TO WSV-ESCRITOS                  09650015
           ELSE                                                         09660015
              DISPLAY '*  ERROR EN GRABAR REGISTROS FS : ' FS-S1DQ9FTC  09670015
              DISPLAY '*  REGISTRO LEIDOS              : ' WSV-ESCRITOS 09680015
              MOVE '02'  TO RETURN-CODE                                 09690015
              STOP RUN                                                  09700015
           END-IF                                                       09710015
           .                                                            09720015
      *                                                                 09730015
      *     *-------------*                                             09740015
       20030-GENERA-SALDOS.                                             09750015
      *     *-------------*                                             09760015
      *                                                                *09770015
           MOVE ZEROS                 TO WA-SALDO-INVER                 09780015
           MOVE ZEROS                 TO WA-SALDO-INVER-0               09790015
           MOVE ZEROS                 TO WA-SALDO-VENTA                 09800015
           MOVE ZEROS                 TO WA-DIVPEN                      09810015
           MOVE ZEROS                 TO WA-INTPEN                      09820015
           MOVE ZEROS                 TO WA-AMTPEN                      09830015
           MOVE ZEROS                 TO WA-DIVUSD                      09840015
           MOVE ZEROS                 TO WA-INTUSD                      09850015
           MOVE ZEROS                 TO WA-AMTUSD                      09860015
      *                                                                *09870015
           PERFORM 20031-SALDO-INVERSION                                09880015
      *                                                                *09890015
           PERFORM 20032-NEGOC-VENTAS                                   09900015
      *                                                                *09910015
           PERFORM 20033-OPERAC-FINANCIERA                              09920015
      *                                                                *09930015
           PERFORM 20034-SALDO-CTAREG                                   09940015
      *                                                                *09950015
           PERFORM 20035-SALDO-CTAREG                                   09960015
      *                                                                 09970015
           PERFORM 20036-SELECT-VLDTSMO                                 09980015
           .                                                            09990015
      *                                                                 10000015
      *     *---------------*                                           10010015
       20031-SALDO-INVERSION.                                           10020015
      *     *---------------*                                           10030015
      *                                                                *10040015
           PERFORM UNTIL FS-E2DQ9ADS = '10'                             10050015
                      OR VADS-CUENTA > WA-CUENTA-ARC7                   10060015
              IF VADS-CUENTA = WA-CUENTA-ARC7                           10070015
                 COMPUTE WA-SALDO       = VADS-DEPOS  + VADS-COMPR      10080015
                                        + VADS-SUSCR  - VADS-VENTA      10090015
                                        - VADS-ORDVE  - VADS-BLOQ       10100015
                 MOVE VADS-PAVAL   TO VXEN-PAVAL                        10110015
                 MOVE VADS-VALOR   TO VXEN-VALOR                        10120015
                 MOVE VADS-ISIN    TO VXEN-ISIN                         10130015
                 PERFORM 20040-SELECT-VLDTXEN                           10140015
                 MOVE VADS-PAVAL   TO  VCAM-CODVALOR (01:03)            10150015
                 MOVE VADS-VALOR   TO  VCAM-CODVALOR (04:08)            10160015
                 MOVE VADS-ISIN    TO  VCAM-CODVALOR (12:01)            10170015
31-12            MOVE WPOL-FECFIN  TO  VCAM-FECDIA                      10180015
                 PERFORM 10030-PRECIO-VALOR                             10190015
                 IF VXEN-TIPINT = 'F'                                   10200015
                    COMPUTE WA-SALDO-INVER =  WA-SALDO-INVER            10210015
                                           + (WA-SALDO * VCAM-CIERRE-D  10220015
                                                       * VXEN-NOMINEM   10230015
                                                       / 100)           10240015
                 ELSE                                                   10250015
                    COMPUTE WA-SALDO-INVER =  WA-SALDO-INVER            10260015
                                           + (WA-SALDO * VCAM-CIERRE-D) 10270015
                 END-IF                                                 10280015
              END-IF                                                    10290015
              PERFORM 10020-LEER-SALDOS                                 10300015
           END-PERFORM.                                                 10310015
      *                                                                *10320015
           IF WA-SALDO-INVER = ZEROS                                    10330015
              PERFORM 20050-MAX-VLDTHIS                                 10340015
              PERFORM 20055-SDO-INVER-CERO                              10350015
           END-IF.                                                      10360015
      *                                                                *10370015
      *     *------------*                                              10380015
       20032-NEGOC-VENTAS.                                              10390015
      *     *------------*                                              10400015
      *                                                                *10410015
           MOVE WA-CUENTA-ARC7 TO  VPOL-CUENTA.                         10420015
           MOVE 'V'            TO  VPOL-COMVEN.                         10430015
           MOVE 'LC'           TO  VPOL-SITUAC.                         10440015
      *                                                                *10450015
           EXEC SQL                                                     10460015
                OPEN VLDCPOL                                            10470015
           END-EXEC                                                     10480015
      *                                                                 10490015
           MOVE SQLCODE TO SQLCODE-AUX                                  10500015
      *                                                                 10510015
           EVALUATE TRUE                                                10520015
               WHEN DB2-OK                                              10530015
                    CONTINUE                                            10540015
               WHEN OTHER                                               10550015
                    MOVE 'VL4CFTC0'              TO  W801-PROGRAMA      10560015
                    MOVE 'VLDTPOL'               TO  W801-TABLA         10570015
                    MOVE 'OPEN'                  TO  W801-ACCION        10580015
                    MOVE WX-CUENTA-ARC7          TO  W801-CLAVE (01:07) 10590015
                    MOVE SQLCODE                 TO  W801-SQLCODE       10600015
                    MOVE SPACES                  TO  W801-SQLWARN       10610015
                    MOVE '20032-NEGOC-VENTAS   ' TO  W801-PARRAFO       10620015
                    PERFORM VLPC8010-DISP-ABEND-DB2                     10630015
                    PERFORM VLPC8010-ABEND-DB2                          10640015
           END-EVALUATE.                                                10650015
      *                                                                 10660015
           PERFORM UNTIL DB2-NOTFND                                     10670015
      *                                                                 10680015
              EXEC SQL                                                  10690015
                   FETCH VLDCPOL                                        10700015
                    INTO :VPOL-EFECTI                                   10710015
                       , :VPOL-CUPCORR                                  10720015
                       , :VPOL-IMPCOM1                                  10730015
                       , :VPOL-IMPCOM2                                  10740015
                       , :VPOL-IMPCOM3                                  10750015
                       , :VPOL-IMPCOM4                                  10760015
                       , :VPOL-IMPCOM5                                  10770015
                       , :VPOL-IMPCOM6                                  10780015
                       , :VPOL-IMPCOM7                                  10790015
                       , :VPOL-IMPCOM8                                  10800015
                       , :VPOL-IGV                                      10810015
              END-EXEC                                                  10820015
                                                                        10830015
              MOVE SQLCODE TO SQLCODE-AUX                               10840015
                                                                        10850015
              EVALUATE TRUE                                             10860015
                  WHEN DB2-OK                                           10870015
                       COMPUTE WA-SALDO-VENTA = WA-SALDO-VENTA +        10880015
                               VPOL-EFECTI    + VPOL-CUPCORR   -        10890015
                               VPOL-IMPCOM1   - VPOL-IMPCOM2   -        10900015
                               VPOL-IMPCOM3   - VPOL-IMPCOM4   -        10910015
                               VPOL-IMPCOM5   - VPOL-IMPCOM6   -        10920015
                               VPOL-IMPCOM7   - VPOL-IMPCOM8   -        10930015
                               VPOL-IGV                                 10940015
                  WHEN DB2-NOTFND                                       10950015
                       CONTINUE                                         10960015
                  WHEN OTHER                                            10970015
                       MOVE 'VL4CFTC0'           TO  W801-PROGRAMA      10980015
                       MOVE 'VLDTPOL'            TO  W801-TABLA         10990015
                       MOVE 'FETCH'              TO  W801-ACCION        11000015
                       MOVE WX-CUENTA-ARC7       TO  W801-CLAVE (01:07) 11010015
                       MOVE SQLCODE              TO  W801-SQLCODE       11020015
                       MOVE SPACES               TO  W801-SQLWARN       11030015
                       MOVE '20032-NEGOC-VENTAS   ' TO W801-PARRAFO     11040015
                       PERFORM VLPC8010-DISP-ABEND-DB2                  11050015
                       PERFORM VLPC8010-ABEND-DB2                       11060015
              END-EVALUATE                                              11070015
           END-PERFORM.                                                 11080015
                                                                        11090015
           EXEC SQL                                                     11100015
                CLOSE VLDCPOL                                           11110015
           END-EXEC                                                     11120015
      *                                                                 11130015
           MOVE SQLCODE TO SQLCODE-AUX                                  11140015
      *                                                                 11150015
           EVALUATE TRUE                                                11160015
               WHEN DB2-OK                                              11170015
                    CONTINUE                                            11180015
               WHEN OTHER                                               11190015
                    MOVE 'VL4CFTC0'              TO  W801-PROGRAMA      11200015
                    MOVE 'VLDTPOL'               TO  W801-TABLA         11210015
                    MOVE 'CLOSE'                 TO  W801-ACCION        11220015
                    MOVE WX-CUENTA-ARC7          TO  W801-CLAVE (01:07) 11230015
                    MOVE SQLCODE                 TO  W801-SQLCODE       11240015
                    MOVE SPACES                  TO  W801-SQLWARN       11250015
                    MOVE '20032-NEGOC-VENTAS   ' TO  W801-PARRAFO       11260015
                    PERFORM VLPC8010-DISP-ABEND-DB2                     11270015
                    PERFORM VLPC8010-ABEND-DB2                          11280015
           END-EVALUATE.                                                11290015
      *                                                                *11300015
      *     *-----------------*                                         11310015
       20033-OPERAC-FINANCIERA.                                         11320015
      *     *-----------------*                                         11330015
      *                                                                *11340015
           MOVE WA-CUENTA-ARC7 TO  VHAC-CUENTA.                         11350015
           MOVE ZEROS          TO  WA-DIVPEN, WA-DIVUSD.                11360015
           MOVE ZEROS          TO  WA-INTPEN, WA-INTUSD.                11370015
           MOVE ZEROS          TO  WA-AMTPEN, WA-AMTUSD.                11380015
      *                                                                *11390015
           EXEC SQL                                                     11400015
                OPEN VLDCHAC                                            11410015
           END-EXEC                                                     11420015
      *                                                                 11430015
           MOVE SQLCODE TO SQLCODE-AUX                                  11440015
      *                                                                 11450015
           EVALUATE TRUE                                                11460015
               WHEN DB2-OK                                              11470015
                    CONTINUE                                            11480015
               WHEN OTHER                                               11490015
                    MOVE 'VL4CFTC0'                TO W801-PROGRAMA     11500015
                    MOVE 'VLDTHAC'                 TO W801-TABLA        11510015
                    MOVE 'OPEN'                    TO W801-ACCION       11520015
                    MOVE WX-CUENTA-ARC7            TO W801-CLAVE        11530015
                    MOVE SQLCODE                   TO W801-SQLCODE      11540015
                    MOVE SPACES                    TO W801-SQLWARN      11550015
                    MOVE '20033-OPERAC-FINANCIERA' TO W801-PARRAFO      11560015
                    PERFORM VLPC8010-DISP-ABEND-DB2                     11570015
                    PERFORM VLPC8010-ABEND-DB2                          11580015
           END-EVALUATE.                                                11590015
      *                                                                 11600015
           PERFORM UNTIL DB2-NOTFND                                     11610015
      *                                                                 11620015
              EXEC SQL                                                  11630015
                   FETCH VLDCHAC                                        11640015
                    INTO :VHAC-OPERAC                                   11650015
                       , :VHAC-IMPLIQ                                   11660015
                       , :VHAC-MONEDA-CTA                               11670015
              END-EXEC                                                  11680015
                                                                        11690015
              MOVE SQLCODE TO SQLCODE-AUX                               11700015
                                                                        11710015
              EVALUATE TRUE                                             11720015
                  WHEN DB2-OK                                           11730015
                       EVALUATE VHAC-MONEDA-CTA                         11740015
                           WHEN 'PEN'                                   11750015
                                EVALUATE VHAC-OPERAC                    11760015
                                    WHEN 07                             11770015
                                         ADD VHAC-IMPLIQ TO WA-DIVPEN   11780015
                                    WHEN 08                             11790015
                                         ADD VHAC-IMPLIQ TO WA-INTPEN   11800015
                                    WHEN 11                             11810015
                                         ADD VHAC-IMPLIQ TO WA-AMTPEN   11820015
                                END-EVALUATE                            11830015
                           WHEN 'USD'                                   11840015
                                EVALUATE VHAC-OPERAC                    11850015
                                    WHEN 07                             11860015
                                         ADD VHAC-IMPLIQ TO WA-DIVUSD   11870015
                                    WHEN 08                             11880015
                                         ADD VHAC-IMPLIQ TO WA-INTUSD   11890015
                                    WHEN 11                             11900015
                                         ADD VHAC-IMPLIQ TO WA-AMTUSD   11910015
                                END-EVALUATE                            11920015
                       END-EVALUATE                                     11930015
                  WHEN DB2-NOTFND                                       11940015
                       CONTINUE                                         11950015
                  WHEN OTHER                                            11960015
                       MOVE 'VL4CFTC0'                TO  W801-PROGRAMA 11970015
                       MOVE 'VLDTHAC'                 TO  W801-TABLA    11980015
                       MOVE 'FETCH'                   TO  W801-ACCION   11990015
                       MOVE WX-CUENTA-ARC7            TO  W801-CLAVE    12000015
                       MOVE SQLCODE                   TO  W801-SQLCODE  12010015
                       MOVE SPACES                    TO  W801-SQLWARN  12020015
                       MOVE '20033-OPERAC-FINANCIERA' TO W801-PARRAFO   12030015
                       PERFORM VLPC8010-DISP-ABEND-DB2                  12040015
                       PERFORM VLPC8010-ABEND-DB2                       12050015
              END-EVALUATE                                              12060015
           END-PERFORM.                                                 12070015
      *                                                                 12080015
           EXEC SQL                                                     12090015
                CLOSE VLDCHAC                                           12100015
           END-EXEC                                                     12110015
      *                                                                 12120015
           MOVE SQLCODE TO SQLCODE-AUX                                  12130015
      *                                                                 12140015
           EVALUATE TRUE                                                12150015
               WHEN DB2-OK                                              12160015
                    CONTINUE                                            12170015
               WHEN OTHER                                               12180015
                    MOVE 'VL4CFTC0'                TO  W801-PROGRAMA    12190015
                    MOVE 'VLDTHAC'                 TO  W801-TABLA       12200015
                    MOVE 'CLOSE'                   TO  W801-ACCION      12210015
                    MOVE WX-CUENTA-ARC7            TO  W801-CLAVE       12220015
                    MOVE SQLCODE                   TO  W801-SQLCODE     12230015
                    MOVE SPACES                    TO  W801-SQLWARN     12240015
                    MOVE '20033-OPERAC-FINANCIERA' TO  W801-PARRAFO     12250015
                    PERFORM VLPC8010-DISP-ABEND-DB2                     12260015
                    PERFORM VLPC8010-ABEND-DB2                          12270015
           END-EVALUATE.                                                12280015
      *                                                                *12290015
      *     *------------*                                              12300015
       20034-SALDO-CTAREG.                                              12310015
      *     *------------*                                              12320015
      *                                                                *12330015
           MOVE E01-CTAVAL20    TO  VSMM-CTAVAL                         12340015
AAAA       MOVE WSV-FECHA-HAS-A TO  VSMM-FECONTA (01:04)                12350015
           MOVE '-'             TO  VSMM-FECONTA (05:01)                12360015
12         MOVE WSV-FECHA-HAS-M TO  VSMM-FECONTA (06:02)                12370015
           MOVE '-'             TO  VSMM-FECONTA (08:01)                12380015
31         MOVE WSV-FECHA-HAS-D TO  VSMM-FECONTA (09:02)                12390015
           MOVE ZEROS           TO  VSMM-SALDO-AUT                      12400015
      *                                                                *12410015
           EXEC SQL                                                     12420015
                OPEN VLDCSMM                                            12430015
           END-EXEC                                                     12440015
      *                                                                 12450015
           MOVE SQLCODE TO SQLCODE-AUX                                  12460015
      *                                                                 12470015
           EVALUATE TRUE                                                12480015
               WHEN DB2-OK                                              12490015
                    CONTINUE                                            12500015
               WHEN OTHER                                               12510015
                    MOVE 'VL4CFTC0'              TO  W801-PROGRAMA      12520015
                    MOVE 'VLDTSMM'               TO  W801-TABLA         12530015
                    MOVE 'OPEN'                  TO  W801-ACCION        12540015
                    MOVE E01-CTAVAL20            TO  W801-CLAVE         12550015
                    MOVE SQLCODE                 TO  W801-SQLCODE       12560015
                    MOVE SPACES                  TO  W801-SQLWARN       12570015
                    MOVE '20034-SALDO-CTAREG   ' TO  W801-PARRAFO       12580015
                    PERFORM VLPC8010-DISP-ABEND-DB2                     12590015
                    PERFORM VLPC8010-ABEND-DB2                          12600015
           END-EVALUATE.                                                12610015
      *                                                                 12620015
           EXEC SQL                                                     12630015
                FETCH  VLDCSMM                                          12640015
                 INTO :VSMM-SALDO-AUT                                   12650015
                    , :VSMM-NUMREF                                      12660015
           END-EXEC                                                     12670015
                                                                        12680015
           MOVE  SQLCODE TO SQLCODE-AUX                                 12690015
                                                                        12700015
           EVALUATE TRUE                                                12710015
               WHEN DB2-OK                                              12720015
                    CONTINUE                                            12730015
               WHEN DB2-NOTFND                                          12740015
                    MOVE ZEROS                   TO  VSMM-SALDO-AUT     12750015
                    MOVE ZEROS                   TO  VSMM-NUMREF        12760015
                    MOVE '9999-12-31'            TO  VSMM-FECONTA       12770015
               WHEN OTHER                                               12780015
                    MOVE 'VL4CFTC0'              TO  W801-PROGRAMA      12790015
                    MOVE 'VLDTSMM'               TO  W801-TABLA         12800015
                    MOVE 'SELECT-SALDO'          TO  W801-ACCION        12810015
                    MOVE E01-CTAVAL20            TO  W801-CLAVE         12820015
                    MOVE SQLCODE                 TO  W801-SQLCODE       12830015
                    MOVE SPACES                  TO  W801-SQLWARN       12840015
                    MOVE '20034-SALDO-CTAREG   ' TO  W801-PARRAFO       12850015
                    PERFORM VLPC8010-DISP-ABEND-DB2                     12860015
                    PERFORM VLPC8010-ABEND-DB2                          12870015
           END-EVALUATE.                                                12880015
                                                                        12890015
           EXEC SQL                                                     12900015
                CLOSE VLDCSMM                                           12910015
           END-EXEC                                                     12920015
      *                                                                 12930015
           MOVE SQLCODE TO SQLCODE-AUX                                  12940015
      *                                                                 12950015
           EVALUATE TRUE                                                12960015
               WHEN DB2-OK                                              12970015
                    CONTINUE                                            12980015
               WHEN OTHER                                               12990015
                    MOVE 'VL4CFTC0'              TO  W801-PROGRAMA      13000015
                    MOVE 'VLDTSMM'               TO  W801-TABLA         13010015
                    MOVE 'CLOSE'                 TO  W801-ACCION        13020015
                    MOVE E01-CTAVAL20            TO  W801-CLAVE         13030015
                    MOVE SQLCODE                 TO  W801-SQLCODE       13040015
                    MOVE SPACES                  TO  W801-SQLWARN       13050015
                    MOVE '20034-SALDO-CTAREG   ' TO  W801-PARRAFO       13060015
                    PERFORM VLPC8010-DISP-ABEND-DB2                     13070015
                    PERFORM VLPC8010-ABEND-DB2                          13080015
           END-EVALUATE.                                                13090015
      *                                                                *13100015
      *     *------------*                                              13110015
       20035-SALDO-CTAREG.                                              13120015
      *     *------------*                                              13130015
      *                                                                *13140015
           MOVE E01-CTAVAL20    TO  VSMM-CTAVAL                         13150015
AAAA       MOVE WSV-FECHA-HAS-A TO  VSMM-FECONTA (01:04)                13160015
           MOVE '-'             TO  VSMM-FECONTA (05:01)                13170015
12         MOVE WSV-FECHA-HAS-M TO  VSMM-FECONTA (06:02)                13180015
           MOVE '-'             TO  VSMM-FECONTA (08:01)                13190015
31         MOVE WSV-FECHA-HAS-D TO  VSMM-FECONTA (09:02)                13200015
           MOVE ZEROS           TO  WSMM-SALDO-AUT                      13210015
      *                                                                *13220015
           EXEC SQL                                                     13230015
                OPEN VLDUSMM                                            13240015
           END-EXEC                                                     13250015
      *                                                                 13260015
           MOVE SQLCODE TO SQLCODE-AUX                                  13270015
      *                                                                 13280015
           EVALUATE TRUE                                                13290015
               WHEN DB2-OK                                              13300015
                    CONTINUE                                            13310015
               WHEN OTHER                                               13320015
                    MOVE 'VL4CFTC0'              TO  W801-PROGRAMA      13330015
                    MOVE 'VLDTSMM'               TO  W801-TABLA         13340015
                    MOVE 'OPEN'                  TO  W801-ACCION        13350015
                    MOVE E01-CTAVAL20            TO  W801-CLAVE         13360015
                    MOVE SQLCODE                 TO  W801-SQLCODE       13370015
                    MOVE SPACES                  TO  W801-SQLWARN       13380015
                    MOVE '20035-SALDO-CTAREG   ' TO  W801-PARRAFO       13390015
                    PERFORM VLPC8010-DISP-ABEND-DB2                     13400015
                    PERFORM VLPC8010-ABEND-DB2                          13410015
           END-EVALUATE.                                                13420015
      *                                                                 13430015
           EXEC SQL                                                     13440015
                FETCH  VLDUSMM                                          13450015
                 INTO :WSMM-SALDO-AUT                                   13460015
                    , :VSMM-FECONTA                                     13470015
           END-EXEC                                                     13480015
                                                                        13490015
           MOVE  SQLCODE TO SQLCODE-AUX                                 13500015
                                                                        13510015
           EVALUATE TRUE                                                13520015
               WHEN DB2-OK                                              13530015
                    CONTINUE                                            13540015
               WHEN DB2-NOTFND                                          13550015
                    MOVE ZEROS                   TO  WSMM-SALDO-AUT     13560015
                    MOVE '9999-12-31'            TO  VSMM-FECONTA       13570015
               WHEN OTHER                                               13580015
                    MOVE 'VL4CFTC0'              TO  W801-PROGRAMA      13590015
                    MOVE 'VLDTSMM'               TO  W801-TABLA         13600015
                    MOVE 'SELECT-SALDO'          TO  W801-ACCION        13610015
                    MOVE E01-CTAVAL20            TO  W801-CLAVE         13620015
                    MOVE SQLCODE                 TO  W801-SQLCODE       13630015
                    MOVE SPACES                  TO  W801-SQLWARN       13640015
                    MOVE '20035-SALDO-CTAREG   ' TO  W801-PARRAFO       13650015
                    PERFORM VLPC8010-DISP-ABEND-DB2                     13660015
                    PERFORM VLPC8010-ABEND-DB2                          13670015
           END-EVALUATE.                                                13680015
                                                                        13690015
           EXEC SQL                                                     13700015
                CLOSE VLDUSMM                                           13710015
           END-EXEC                                                     13720015
      *                                                                 13730015
           MOVE SQLCODE TO SQLCODE-AUX                                  13740015
      *                                                                 13750015
           EVALUATE TRUE                                                13760015
               WHEN DB2-OK                                              13770015
                    CONTINUE                                            13780015
               WHEN OTHER                                               13790015
                    MOVE 'VL4CFTC0'              TO  W801-PROGRAMA      13800015
                    MOVE 'VLDTSMM'               TO  W801-TABLA         13810015
                    MOVE 'CLOSE'                 TO  W801-ACCION        13820015
                    MOVE E01-CTAVAL20            TO  W801-CLAVE         13830015
                    MOVE SQLCODE                 TO  W801-SQLCODE       13840015
                    MOVE SPACES                  TO  W801-SQLWARN       13850015
                    MOVE '20035-SALDO-CTAREG   ' TO  W801-PARRAFO       13860015
                    PERFORM VLPC8010-DISP-ABEND-DB2                     13870015
                    PERFORM VLPC8010-ABEND-DB2                          13880015
           END-EVALUATE.                                                13890015
      *                                                                *13900015
      *     *--------------*                                            13910015
       20036-SELECT-VLDTSMO.                                            13920015
      *     *--------------*                                            13930015
      *                                                                *13940015
           MOVE E01-CTAVAL20    TO  VSMO-CTAVAL                         13950015
      *                                                                *13960015
           EXEC SQL                                                     13970015
                SELECT  VSMO_FEALTREG                                   13980015
                  INTO :VSMO-FEALTREG                                   13990015
                  FROM VLDTSMO                                          14000015
                  WHERE VSMO_CTAVAL    = :VSMO-CTAVAL                   14010015
           END-EXEC.                                                    14020015
      *                                                                 14030015
           EVALUATE SQLCODE                                             14040015
               WHEN ZEROS                                               14050015
                    CONTINUE                                            14060015
               WHEN 100                                                 14070015
                    MOVE '9999-12-31'           TO  VSMO-FEALTREG       14080015
               WHEN OTHER                                               14090015
                    MOVE 'VL4CFTC0'             TO  W801-PROGRAMA       14100015
                    MOVE 'VLDTSDO'              TO  W801-TABLA          14110015
                    MOVE 'SELECT'               TO  W801-ACCION         14120015
                    MOVE E01-CTAVAL20           TO  W801-CLAVE (01:07)  14130015
                    MOVE  SQLCODE               TO  W801-SQLCODE        14140015
                    MOVE  SPACES                TO  W801-SQLWARN        14150015
                    MOVE '20036-SELECT-VLDTSMO' TO  W801-PARRAFO        14160015
                    PERFORM  VLPC8010-DISP-ABEND-DB2                    14170015
                    PERFORM  VLPC8010-ABEND-DB2                         14180015
           END-EVALUATE.                                                14190015
      *                                                                *14200015
      *     *--------------*                                            14210015
       20040-SELECT-VLDTXEN.                                            14220015
      *     *--------------*                                            14230015
      *                                                                *14240015
           EXEC SQL                                                     14250015
                SELECT  VXEN_NOMINEM                                    14260015
                     ,  VXEN_SUSPDT                                     14270015
                     ,  VXEN_TIPINT                                     14280015
                     ,  VXEN_MINPOL                                     14290015
                     ,  VXEN_FPROXA                                     14300015
                     ,  VXEN_FPROXC                                     14310015
                     ,  VXEN_FILLER                                     14320015
                  INTO :VXEN-NOMINEM                                    14330015
                     , :VXEN-SUSPDT                                     14340015
                     , :VXEN-TIPINT                                     14350015
                     , :VXEN-MINPOL                                     14360015
                     , :VXEN-FPROXA                                     14370015
                     , :VXEN-FPROXC                                     14380015
                     , :VXEN-FILLER                                     14390015
                  FROM VLDTXEN                                          14400015
                  WHERE VXEN_PAVAL     = :VXEN-PAVAL                    14410015
                    AND VXEN_VALOR     = :VXEN-VALOR                    14420015
                    AND VXEN_ISIN      = :VXEN-ISIN                     14430015
           END-EXEC.                                                    14440015
      *                                                                 14450015
           EVALUATE SQLCODE                                             14460015
               WHEN ZEROS                                               14470015
                    CONTINUE                                            14480015
                    IF VXEN-SUSPDT > ZEROS                              14490016
                       MOVE VXEN-SUSPDT         TO WXEN-SUSPDT          14500016
                       MOVE WXEN-NOMITEMP       TO VXEN-NOMINEM         14510016
                    END-IF                                              14520016
               WHEN OTHER                                               14530015
                    MOVE 'VL4CFTC0'             TO  W801-PROGRAMA       14540015
                    MOVE 'VLDTXEN'              TO  W801-TABLA          14550015
                    MOVE 'SELECT'               TO  W801-ACCION         14560015
                    MOVE WX-CUENTA-ARC7         TO  W801-CLAVE (01:07)  14570015
                    MOVE VXEN-PAVAL             TO  W801-CLAVE (09:03)  14580015
                    MOVE VXEN-VALOR             TO  W801-CLAVE (12:07)  14590015
                    MOVE VXEN-ISIN              TO  W801-CLAVE (19:01)  14600015
                    MOVE  SQLCODE               TO  W801-SQLCODE        14610015
                    MOVE  SPACES                TO  W801-SQLWARN        14620015
                    MOVE '20040-SELECT-VLDTXEN' TO  W801-PARRAFO        14630015
                    PERFORM  VLPC8010-DISP-ABEND-DB2                    14640015
                    PERFORM  VLPC8010-ABEND-DB2                         14650015
           END-EVALUATE.                                                14660015
      *                                                                *14670015
      *     *-----------*                                               14680015
       20050-MAX-VLDTHIS.                                               14690015
      *     *-----------*                                               14700015
      *    BUSCA AÑO DE INFORMACION                                    *14710015
           MOVE WA-CUENTA-ARC7 TO  VHIS-CTAVAL.                         14720015
           MOVE LK-F-AA-H      TO  WHIS-ANO.                            14730015
           MOVE ZEROS          TO  VHIS-ANO.                            14740015
           MOVE ZEROS          TO  VHIS-MES.                            14750015
      *                                                                *14760015
           EXEC SQL                                                     14770015
                SELECT  MAX(VHIS_ANO)                                   14780015
                  INTO     :VHIS-ANO                                    14790015
                  FROM VLDTHIS                                          14800015
                 WHERE VHIS_CTAVAL    = :VHIS-CTAVAL                    14810015
                   AND VHIS_ANO      <= :WHIS-ANO                       14820015
                   AND VHIS_TIPGAS   IN (48, 49)                        14830015
           END-EXEC.                                                    14840015
      *                                                                 14850015
           EVALUATE SQLCODE                                             14860015
               WHEN ZEROS                                               14870015
                    CONTINUE                                            14880015
               WHEN -305                                                14890015
                    MOVE ZEROS                  TO  VHIS-ANO            14900015
                    MOVE ZEROS                  TO  VHIS-MES            14910015
               WHEN OTHER                                               14920015
                    MOVE 'VL4CFTC0'             TO  W801-PROGRAMA       14930015
                    MOVE 'VLDTHIS'              TO  W801-TABLA          14940015
                    MOVE 'MAX-AÑO'              TO  W801-ACCION         14950015
                    MOVE WX-CUENTA-ARC7         TO  W801-CLAVE (01:07)  14960015
                    MOVE  SQLCODE               TO  W801-SQLCODE        14970015
                    MOVE  SPACES                TO  W801-SQLWARN        14980015
                    MOVE '20050-MAX-VLDTHIS'    TO  W801-PARRAFO        14990015
                    PERFORM  VLPC8010-DISP-ABEND-DB2                    15000015
                    PERFORM  VLPC8010-ABEND-DB2                         15010015
           END-EVALUATE.                                                15020015
      *    BUSCA MES DE INFORMACION                                    *15030015
           MOVE WA-CUENTA-ARC7 TO  VHIS-CTAVAL.                         15040015
           MOVE ZEROS          TO  VHIS-MES.                            15050015
      *                                                                *15060015
           EXEC SQL                                                     15070015
                SELECT  MAX(VHIS_MES)                                   15080015
                  INTO     :VHIS-MES                                    15090015
                  FROM VLDTHIS                                          15100015
                 WHERE VHIS_CTAVAL = :VHIS-CTAVAL                       15110015
                   AND VHIS_ANO    = :VHIS-ANO                          15120015
                   AND VHIS_TIPGAS IN (48, 49)                          15130015
           END-EXEC.                                                    15140015
      *                                                                 15150015
           EVALUATE SQLCODE                                             15160015
               WHEN ZEROS                                               15170015
                    CONTINUE                                            15180015
               WHEN -305                                                15190015
                    MOVE ZEROS                  TO  VHIS-ANO            15200015
                    MOVE ZEROS                  TO  VHIS-MES            15210015
               WHEN OTHER                                               15220015
                    MOVE 'VL4CFTC0'             TO  W801-PROGRAMA       15230015
                    MOVE 'VLDTHIS'              TO  W801-TABLA          15240015
                    MOVE 'MAX-MES'              TO  W801-ACCION         15250015
                    MOVE WX-CUENTA-ARC7         TO  W801-CLAVE (01:07)  15260015
                    MOVE  SQLCODE               TO  W801-SQLCODE        15270015
                    MOVE  SPACES                TO  W801-SQLWARN        15280015
                    MOVE '20050-MAX-VLDTHIS'    TO  W801-PARRAFO        15290015
                    PERFORM  VLPC8010-DISP-ABEND-DB2                    15300015
                    PERFORM  VLPC8010-ABEND-DB2                         15310015
           END-EVALUATE.                                                15320015
      *                                                                *15330015
      *     *--------------*                                            15340015
       20055-SDO-INVER-CERO.                                            15350015
      *     *--------------*                                           *15360015
      *                                                                *15370015
           MOVE WA-CUENTA-ARC7 TO VHIS-CTAVAL.                          15380015
           MOVE ZEROS          TO WHIS-FEC1RA-N.                        15390015
           MOVE SPACES         TO VHIS-CODVALOR.                        15400015
      *                                                                *15410015
           EXEC SQL                                                     15420015
                OPEN VLDUHIS                                            15430015
           END-EXEC                                                     15440015
      *                                                                 15450015
           MOVE SQLCODE TO SQLCODE-AUX                                  15460015
      *                                                                 15470015
           EVALUATE TRUE                                                15480015
               WHEN DB2-OK                                              15490015
                    CONTINUE                                            15500015
               WHEN OTHER                                               15510015
                    MOVE 'VL4CFTC0'              TO  W801-PROGRAMA      15520015
                    MOVE 'VLDTHIS'               TO  W801-TABLA         15530015
                    MOVE 'OPEN'                  TO  W801-ACCION        15540015
                    MOVE E01-CTAVAL20            TO  W801-CLAVE         15550015
                    MOVE SQLCODE                 TO  W801-SQLCODE       15560015
                    MOVE SPACES                  TO  W801-SQLWARN       15570015
                    MOVE '20055-SDO-INVER-CERO ' TO  W801-PARRAFO       15580015
                    PERFORM VLPC8010-DISP-ABEND-DB2                     15590015
                    PERFORM VLPC8010-ABEND-DB2                          15600015
           END-EVALUATE.                                                15610015
      *                                                                 15620015
           PERFORM UNTIL SQLCODE = 100                                  15630015
              EXEC SQL                                                  15640015
                   FETCH  VLDUHIS                                       15650015
                    INTO :VHIS-CODVALOR                                 15660015
                       , :VHIS-TITULOS1                                 15670015
                       , :VHIS-MOVIMI1                                  15680015
                       , :VHIS-CUSTODIA1                                15690015
                       , :VHIS-CAMBIO1                                  15700015
                       , :VHIS-COBRADO1                                 15710015
                       , :VHIS-TITULOS2                                 15720015
                       , :VHIS-MOVIMI2                                  15730015
                       , :VHIS-CUSTODIA2                                15740015
                       , :VHIS-CAMBIO2                                  15750015
                       , :VHIS-COBRADO2                                 15760015
                       , :VHIS-TITULOS3                                 15770015
                       , :VHIS-MOVIMI3                                  15780015
                       , :VHIS-CUSTODIA3                                15790015
                       , :VHIS-CAMBIO3                                  15800015
                       , :VHIS-COBRADO3                                 15810015
                       , :VHIS-TITULOS4                                 15820015
                       , :VHIS-MOVIMI4                                  15830015
                       , :VHIS-CUSTODIA4                                15840015
                       , :VHIS-CAMBIO4                                  15850015
                       , :VHIS-COBRADO4                                 15860015
                       , :VHIS-TITULOS5                                 15870015
                       , :VHIS-MOVIMI5                                  15880015
                       , :VHIS-CUSTODIA5                                15890015
                       , :VHIS-CAMBIO5                                  15900015
                       , :VHIS-COBRADO5                                 15910015
                       , :VHIS-TITULOS6                                 15920015
                       , :VHIS-MOVIMI6                                  15930015
                       , :VHIS-CUSTODIA6                                15940015
                       , :VHIS-CAMBIO6                                  15950015
                       , :VHIS-COBRADO6                                 15960015
                       , :VHIS-TITULOS7                                 15970015
                       , :VHIS-MOVIMI7                                  15980015
                       , :VHIS-CUSTODIA7                                15990015
                       , :VHIS-CAMBIO7                                  16000015
                       , :VHIS-COBRADO7                                 16010015
                       , :VHIS-TITULOS8                                 16020015
                       , :VHIS-MOVIMI8                                  16030015
                       , :VHIS-CUSTODIA8                                16040015
                       , :VHIS-CAMBIO8                                  16050015
                       , :VHIS-COBRADO8                                 16060015
                       , :VHIS-TITULOS9                                 16070015
                       , :VHIS-MOVIMI9                                  16080015
                       , :VHIS-CUSTODIA9                                16090015
                       , :VHIS-CAMBIO9                                  16100015
                       , :VHIS-COBRADO9                                 16110015
                       , :VHIS-TITULOS10                                16120015
                       , :VHIS-MOVIMI10                                 16130015
                       , :VHIS-CUSTODIA10                               16140015
                       , :VHIS-CAMBIO10                                 16150015
                       , :VHIS-COBRADO10                                16160015
                       , :VHIS-TITULOS11                                16170015
                       , :VHIS-MOVIMI11                                 16180015
                       , :VHIS-CUSTODIA11                               16190015
                       , :VHIS-CAMBIO11                                 16200015
                       , :VHIS-COBRADO11                                16210015
                       , :VHIS-TITULOS12                                16220015
                       , :VHIS-MOVIMI12                                 16230015
                       , :VHIS-CUSTODIA12                               16240015
                       , :VHIS-CAMBIO12                                 16250015
                       , :VHIS-COBRADO12                                16260015
                       , :VHIS-TITULOS13                                16270015
                       , :VHIS-MOVIMI13                                 16280015
                       , :VHIS-CUSTODIA13                               16290015
                       , :VHIS-CAMBIO13                                 16300015
                       , :VHIS-COBRADO13                                16310015
                       , :VHIS-TITULOS14                                16320015
                       , :VHIS-MOVIMI14                                 16330015
                       , :VHIS-CUSTODIA14                               16340015
                       , :VHIS-CAMBIO14                                 16350015
                       , :VHIS-COBRADO14                                16360015
                       , :VHIS-TITULOS15                                16370015
                       , :VHIS-MOVIMI15                                 16380015
                       , :VHIS-CUSTODIA15                               16390015
                       , :VHIS-CAMBIO15                                 16400015
                       , :VHIS-COBRADO15                                16410015
                       , :VHIS-TITULOS16                                16420015
                       , :VHIS-MOVIMI16                                 16430015
                       , :VHIS-CUSTODIA16                               16440015
                       , :VHIS-CAMBIO16                                 16450015
                       , :VHIS-COBRADO16                                16460015
                       , :VHIS-TITULOS17                                16470015
                       , :VHIS-MOVIMI17                                 16480015
                       , :VHIS-CUSTODIA17                               16490015
                       , :VHIS-CAMBIO17                                 16500015
                       , :VHIS-COBRADO17                                16510015
                       , :VHIS-TITULOS18                                16520015
                       , :VHIS-MOVIMI18                                 16530015
                       , :VHIS-CUSTODIA18                               16540015
                       , :VHIS-CAMBIO18                                 16550015
                       , :VHIS-COBRADO18                                16560015
                       , :VHIS-TITULOS19                                16570015
                       , :VHIS-MOVIMI19                                 16580015
                       , :VHIS-CUSTODIA19                               16590015
                       , :VHIS-CAMBIO19                                 16600015
                       , :VHIS-COBRADO19                                16610015
                       , :VHIS-TITULOS20                                16620015
                       , :VHIS-MOVIMI20                                 16630015
                       , :VHIS-CUSTODIA20                               16640015
                       , :VHIS-CAMBIO20                                 16650015
                       , :VHIS-COBRADO20                                16660015
                       , :VHIS-TITULOS21                                16670015
                       , :VHIS-MOVIMI21                                 16680015
                       , :VHIS-CUSTODIA21                               16690015
                       , :VHIS-CAMBIO21                                 16700015
                       , :VHIS-COBRADO21                                16710015
                       , :VHIS-TITULOS22                                16720015
                       , :VHIS-MOVIMI22                                 16730015
                       , :VHIS-CUSTODIA22                               16740015
                       , :VHIS-CAMBIO22                                 16750015
                       , :VHIS-COBRADO22                                16760015
                       , :VHIS-TITULOS23                                16770015
                       , :VHIS-MOVIMI23                                 16780015
                       , :VHIS-CUSTODIA23                               16790015
                       , :VHIS-CAMBIO23                                 16800015
                       , :VHIS-COBRADO23                                16810015
                       , :VHIS-TITULOS24                                16820015
                       , :VHIS-MOVIMI24                                 16830015
                       , :VHIS-CUSTODIA24                               16840015
                       , :VHIS-CAMBIO24                                 16850015
                       , :VHIS-COBRADO24                                16860015
                       , :VHIS-TITULOS25                                16870015
                       , :VHIS-MOVIMI25                                 16880015
                       , :VHIS-CUSTODIA25                               16890015
                       , :VHIS-CAMBIO25                                 16900015
                       , :VHIS-COBRADO25                                16910015
                       , :VHIS-TITULOS26                                16920015
                       , :VHIS-MOVIMI26                                 16930015
                       , :VHIS-CUSTODIA26                               16940015
                       , :VHIS-CAMBIO26                                 16950015
                       , :VHIS-COBRADO26                                16960015
                       , :VHIS-TITULOS27                                16970015
                       , :VHIS-MOVIMI27                                 16980015
                       , :VHIS-CUSTODIA27                               16990015
                       , :VHIS-CAMBIO27                                 17000015
                       , :VHIS-COBRADO27                                17010015
                       , :VHIS-TITULOS28                                17020015
                       , :VHIS-MOVIMI28                                 17030015
                       , :VHIS-CUSTODIA28                               17040015
                       , :VHIS-CAMBIO28                                 17050015
                       , :VHIS-COBRADO28                                17060015
                       , :VHIS-TITULOS29                                17070015
                       , :VHIS-MOVIMI29                                 17080015
                       , :VHIS-CUSTODIA29                               17090015
                       , :VHIS-CAMBIO29                                 17100015
                       , :VHIS-COBRADO29                                17110015
                       , :VHIS-TITULOS30                                17120015
                       , :VHIS-MOVIMI30                                 17130015
                       , :VHIS-CUSTODIA30                               17140015
                       , :VHIS-CAMBIO30                                 17150015
                       , :VHIS-COBRADO30                                17160015
                       , :VHIS-TITULOS31                                17170015
                       , :VHIS-MOVIMI31                                 17180015
                       , :VHIS-CUSTODIA31                               17190015
                       , :VHIS-CAMBIO31                                 17200015
                       , :VHIS-COBRADO31                                17210015
                       , :VHIS-FEALTREG                                 17220015
                       , :VHIS-FEULMOD                                  17230015
                       , :VHIS-HORULMOD                                 17240015
                       , :VHIS-NUMTER                                   17250015
                       , :VHIS-USUARIO                                  17260015
              END-EXEC                                                  17270015
                                                                        17280015
              MOVE  SQLCODE TO SQLCODE-AUX                              17290015
                                                                        17300015
              EVALUATE TRUE                                             17310015
                  WHEN DB2-OK                                           17320015
                       MOVE DCLVLDTHIS            TO DCLVLTCHIS         17330015
                       MOVE ZEROS          TO  WH-SALD0                 17340015
                       MOVE ZEROS          TO  WH-NOMINEM               17350015
                       PERFORM VARYING WI FROM 31 BY -1                 17360015
                                 UNTIL WI    = ZEROS                    17370015
                                    OR WH-SALD0 > ZEROS                 17380015
                          IF CVHIS-TITULOS (WI) > ZEROS                 17390015
                             MOVE CVHIS-TITULOS (WI) TO WH-SALD0        17400015
                             MOVE CVHIS-CAMBIO  (WI) TO WH-NOMINEM      17410015
                             MOVE WI                 TO WHIS-FECHIS-D   17420015
                          END-IF                                        17430015
                       END-PERFORM                                      17440015
                       MOVE VHIS-CODVALOR (01:03) TO VXEN-PAVAL         17450015
                       MOVE VHIS-CODVALOR (04:08) TO VXEN-VALOR         17460015
                       MOVE VHIS-CODVALOR (12:01) TO VXEN-ISIN          17470015
                       PERFORM 20040-SELECT-VLDTXEN                     17480015
                       MOVE VHIS-CODVALOR         TO VCAM-CODVALOR      17490015
                       MOVE VHIS-ANO              TO WHIS-FECHIS-A      17500015
                       MOVE VHIS-MES              TO WHIS-FECHIS-M      17510015
                       MOVE WHIS-FECHIS-N         TO VCAM-FECDIA        17520015
                       IF WHIS-FECHIS-N > ZEROS AND                     17530015
                          WHIS-FEC1RA-N = ZEROS                         17540015
                          MOVE WHIS-FECHIS-N      TO WHIS-FEC1RA-N      17550015
                       END-IF                                           17560015
                       PERFORM 10030-PRECIO-VALOR                       17570015
                       IF VXEN-TIPINT = 'F'                             17580015
                          COMPUTE WA-SALDO-INVER-0   = WA-SALDO-INVER-0 17590015
                                         + (WH-SALD0 * VCAM-CIERRE-D    17600015
                                                     * WH-NOMINEM       17610015
                                                     / 100)             17620015
                       ELSE                                             17630015
                          COMPUTE WA-SALDO-INVER-0   = WA-SALDO-INVER-0 17640015
                                         + (WH-SALD0 * VCAM-CIERRE-D)   17650015
                       END-IF                                           17660015
                  WHEN DB2-NOTFND                                       17670015
                       CONTINUE                                         17680015
                  WHEN OTHER                                            17690015
                       MOVE 'VL4CFTC0'              TO  W801-PROGRAMA   17700015
                       MOVE 'VLDTHIS'               TO  W801-TABLA      17710015
                       MOVE 'FETCH       '          TO  W801-ACCION     17720015
                       MOVE E01-CTAVAL20            TO  W801-CLAVE      17730015
                       MOVE SQLCODE                 TO  W801-SQLCODE    17740015
                       MOVE SPACES                  TO  W801-SQLWARN    17750015
                       MOVE '20055-SDO-INVER-CERO ' TO  W801-PARRAFO    17760015
                       PERFORM VLPC8010-DISP-ABEND-DB2                  17770015
                       PERFORM VLPC8010-ABEND-DB2                       17780015
              END-EVALUATE                                              17790015
           END-PERFORM.                                                 17800015
      *                                                                 17810015
           EXEC SQL                                                     17820015
                CLOSE VLDUHIS                                           17830015
           END-EXEC                                                     17840015
      *                                                                 17850015
           MOVE SQLCODE TO SQLCODE-AUX                                  17860015
      *                                                                 17870015
           EVALUATE TRUE                                                17880015
               WHEN DB2-OK                                              17890015
                    CONTINUE                                            17900015
               WHEN OTHER                                               17910015
                    MOVE 'VL4CFTC0'              TO  W801-PROGRAMA      17920015
                    MOVE 'VLDTHIS'               TO  W801-TABLA         17930015
                    MOVE 'CLOSE'                 TO  W801-ACCION        17940015
                    MOVE E01-CTAVAL20            TO  W801-CLAVE         17950015
                    MOVE SQLCODE                 TO  W801-SQLCODE       17960015
                    MOVE SPACES                  TO  W801-SQLWARN       17970015
                    MOVE '20055-SDO-INVER-CERO ' TO  W801-PARRAFO       17980015
                    PERFORM VLPC8010-DISP-ABEND-DB2                     17990015
                    PERFORM VLPC8010-ABEND-DB2                          18000015
           END-EVALUATE.                                                18010015
      *                                                                *18020015
      *     *--------------*                                            18030015
      ******************************************************************18040015
      *                   30000-FIN                                    *18050015
      ******************************************************************18060015
      *---------*                                                       18070015
       30000-FIN.                                                       18080015
      *---------*                                                       18090015
      *                                                                 18100015
           DISPLAY 'READ  S1DQ9FTC.... : ' WSV-LEIDOS.                  18110015
           DISPLAY 'WRITE E1DQ9FTC.... : ' WSV-ESCRITOS.                18120015
           STOP RUN                                                     18130015
           .                                                            18140015
      *                                                                *18150015
      *    *-----------------*                                          18160015
       3100-DISP-TOTALIMETROS.                                          18170015
      *    *-----------------*                                          18180015
           DISPLAY '*************************************************'. 18190015
           DISPLAY '********    T O T A L I M E T R O S   ***********'. 18200015
           DISPLAY '********   D E L     P R O G R A M A  ***********'. 18210015
           DISPLAY '********           VL4CFTC0           ***********'. 18220015
           DISPLAY '*************************************************'. 18230015
           DISPLAY '*************************************************'. 18240015
      *                                                                *18250015
      *    *---------------------*                                      18260015
       3100-DISP-TOTALIMETROS-FIN.                                      18270015
      *    *---------------------*                                      18280015
           EXIT.                                                        18290015
      ******************************************************************18300015
      **                  COPYS DE ERRORES DE PROCEDURE               **18310015
      ******************************************************************18320015
      *                                                                *18330015
            COPY  QRWCDB20.                                             18340015
            COPY  VLPC8010.                                             18350015
            COPY  VLPC8020.                                             18360015
            COPY  VLPCRUTI.                                             18370015
      *                                                                *18380015
      *------------------*                                              18390015
      *99999-FIN-PROGRAMA*                                              18400015
      *------------------*                                              18410015
