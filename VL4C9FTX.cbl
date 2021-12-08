      *-----------------------*                                         00010000
       IDENTIFICATION DIVISION.                                         00020000
      *-----------------------*                                         00030000
       PROGRAM-ID.   VL4C9FTX.                                          00040002
      *AUTHOR.       BBVA PERU.                                         00050000
      ******************************************************************00060000
      *REF.PETIC FECHA-MOD. PROGRAMADOR      DESCRIPCION               *00070000
      *--------- ---------- ---------------- --------------------------*00080000
FVAXX *FVA-XX    26-08-2020 EULER ALVARADO   VALIDA SITUA. CTA Y SALDO *00090000
      *                                      CONSIDERA FECHAS DE LINKAG*00100000
      *--------- ---------- ---------------- --------------------------*00110000
      *004111    09-11-2021  LUIS RIVERA H.  SE AGREGA CONDICIONAL     *00111004
      *                                      PARA QUE SOLO SE FILTRE   *00112003
      *                                      POR FECHA DE EJECUCION    *00113003
      ******************************************************************00120000
       ENVIRONMENT DIVISION.                                            00130000
       CONFIGURATION SECTION.                                           00140000
       SOURCE-COMPUTER. IBM-390.                                        00150000
       OBJECT-COMPUTER. IBM-390.                                        00160000
       SPECIAL-NAMES.                                                   00170000
       INPUT-OUTPUT SECTION.                                            00180000
      *------------*                                                    00190000
       FILE-CONTROL.                                                    00200000
      *------------*                                                    00210000
            SELECT E1DQ9FTC ASSIGN TO E1DQ9FTC                          00220000
                   FILE STATUS IS FS-E1DQ9FTC                           00230000
                   ORGANIZATION IS SEQUENTIAL.                          00240000
                                                                        00250000
            SELECT E2DQ9ADS ASSIGN TO E2DQ9ADS                          00260000
                   FILE STATUS IS FS-E2DQ9ADS                           00270000
                   ORGANIZATION IS SEQUENTIAL.                          00280000
                                                                        00290000
            SELECT S1DQ9FTC ASSIGN TO S1DQ9FTC                          00300000
                   FILE STATUS IS FS-S1DQ9FTC                           00310000
                   ORGANIZATION IS SEQUENTIAL.                          00320000
      *-----------------------------------------------------------------00330000
      *-------------*                                                   00340000
       DATA DIVISION.                                                   00350000
      *-------------*                                                   00360000
       FILE SECTION.                                                    00370000
                                                                        00380000
       FD  E1DQ9FTC                                                     00390000
           RECORDING MODE IS F                                          00400000
           BLOCK CONTAINS 0 RECORDS                                     00410000
           DATA RECORD IS REG-E1DQ9FTC.                                 00420000
       01  REG-E1DQ9FTC.                                                00430000
           10 E01-CTAVAL20         PIC X(20).                           00440000
           10 E01-FILLER1          PIC X(01).                           00450000
           10 E01-MONEDA           PIC X(03).                           00460000
           10 E01-FILLER2          PIC X(01).                           00470000
           10 E01-NUMCLI           PIC 9(08).                           00480000
           10 E01-FILLER3          PIC X(01).                           00490000
           10 E01-CLIENTE          PIC X(60).                           00500000
           10 E01-FILLER4          PIC X(01).                           00510000
           10 E01-SITUACION        PIC X(09).                           00520000
           10 E01-FILLER5          PIC X(01).                           00530000
           10 E01-FECALTA          PIC X(10).                           00540000
           10 E01-FILLER6          PIC X(01).                           00550000
           10 E01-FECCESE          PIC X(10).                           00560000
           10 E01-FILLER7          PIC X(01).                           00570000
           10 E01-RUT              PIC 9(08).                           00580000
                                                                        00590000
       FD  E2DQ9ADS                                                     00600000
           RECORDING MODE IS F                                          00610000
           BLOCK CONTAINS 0 RECORDS                                     00620000
           DATA RECORD IS REG-E2DQ9ADS.                                 00630000
       01  REG-E2DQ9ADS            PIC X(354).                          00640000
                                                                        00650000
       FD  S1DQ9FTC                                                     00660000
           RECORDING MODE IS F                                          00670000
           BLOCK CONTAINS 0 RECORDS                                     00680000
           DATA RECORD IS REG-S1DQ9FTC.                                 00690000
       01  REG-S1DQ9FTC            PIC X(214).                          00700000
      *                                                                 00710000
      *-----------------------------------------------------------------00720000
       WORKING-STORAGE SECTION.                                         00730000
      *-----------------------*                                         00740000
       77  WS-NAME                 PIC X(70) VALUE                      00750000
                                   '**  INICIO WORKING VL4C9MAE **'.    00760000
      ******************************************************************00770000
      *                                                                 00780000
       77  WI                      PIC 9(02) VALUE ZEROS.               00790000
       77  WHIS-ANO                PIC S9(4)V USAGE COMP-3.             00800000
       77  WHIS-MES                PIC S9(4)V USAGE COMP-3.             00810000
       77  WPOL-FECINI             PIC S9(8)V USAGE COMP-3.             00820000
       77  WPOL-FECFIN             PIC S9(8)V USAGE COMP-3.             00830000
       77  WHAC-FECINI             PIC S9(8)V USAGE COMP-3.             00840000
       77  WHAC-FECFIN             PIC S9(8)V USAGE COMP-3.             00850000
       77  WA-APERTURA             PIC  X(40) VALUE                     00860000
                             'APERTURA DE CUENTA                      '.00870000
       77  WA-SALIDA               PIC  X(40) VALUE                     00880000
                             'TRASPASO CTA-REGISTRO A OPERATIVA S.A.B.'.00890000
       77  WA-ENTRADA              PIC  X(40) VALUE                     00900000
                             'TRASPASO CTA-OPERATIVA S.A.B. A REGISTRO'.00910000
      *                                                                 00920000
04111  01 WS-FECHA-ENTRADA.                                             00920104
  |       05 DIA-ENTRADA          PIC X(02).                            00920204
  |       05 MES-ENTRADA          PIC X(02).                            00920304
04111     05 ANIO-ENTRADA         PIC X(04).                            00920404
      *                                                                 00921004
       01  WS-VARIOS.                                                   00930000
           02  WX-CUENTA-ARC7.                                          00940000
               04  WN-CUENTA-ARC7  PIC  9(7).                           00950000
           02  WA-CUENTA-ARC7      PIC S9(7)V USAGE COMP-3.             00960000
           02  WSV-FECHA-DES.                                           00970000
               04 WSV-FECHA-DES-A      PIC X(04).                       00980000
               04 WSV-FECHA-DES-M      PIC X(02).                       00990000
               04 WSV-FECHA-DES-D      PIC X(02).                       01000000
           02  WSV-FECHA-DES-N REDEFINES WSV-FECHA-DES PIC 9(08).       01010000
           02  WSV-FECHA-HAS.                                           01020000
               04 WSV-FECHA-HAS-A      PIC X(04).                       01030000
               04 WSV-FECHA-HAS-M      PIC X(02).                       01040000
               04 WSV-FECHA-HAS-D      PIC X(02).                       01050000
           02  WSV-FECHA-HAS-N REDEFINES WSV-FECHA-HAS PIC 9(08).       01060000
           02  WSMM-SALDO-AUT          PIC S9(13)V9(02) USAGE COMP-3.   01070000
           02  WHIS-FECHIS-X.                                           01080000
               04  WHIS-FECHIS-A       PIC 9(04).                       01090000
               04  WHIS-FECHIS-M       PIC 9(02).                       01100000
               04  WHIS-FECHIS-D       PIC 9(02).                       01110000
           02  WHIS-FECHIS-N REDEFINES WHIS-FECHIS-X PIC 9(08).         01120000
      *                                                                *01130000
           02  WHIS-FEC1RA-X.                                           01140000
               04  WHIS-FEC1RA-A       PIC 9(04).                       01150000
               04  WHIS-FEC1RA-M       PIC 9(02).                       01160000
               04  WHIS-FEC1RA-D       PIC 9(02).                       01170000
           02  WHIS-FEC1RA-N REDEFINES WHIS-FEC1RA-X PIC 9(08).         01180000
      *                                                                *01190000
       01  WTMP-NOM-SUS.                                                01200000
           02 WXEN-SUSPDT                 PIC S9(08)V9(10) USAGE COMP-3.01210000
           02 WXEN-NOMITEMP REDEFINES WXEN-SUSPDT                       01220000
                                          PIC S9(12)V9(06) USAGE COMP-3.01230000
      *                                                                *01240000
       01  PE9C5201                PIC X(08) VALUE 'PE9C5201'.          01250000
       01  FILE-STATUS.                                                 01260000
           10 FS-E1DQ9FTC          PIC X(02) VALUE SPACES.              01270000
           10 FS-E2DQ9ADS          PIC X(02) VALUE SPACES.              01280000
           10 FS-S1DQ9FTC          PIC X(02) VALUE SPACES.              01290000
       01  WSV-CLIENTE             PIC X(60) VALUE SPACES.              01300000
       01  WSV-FECHA-10-A          PIC X(10) VALUE SPACES.              01310000
       01  WSV-FECHA-8-N           PIC 9(08) VALUE ZEROS.               01320000
       01  WSV-FECHA-8-A REDEFINES WSV-FECHA-8-N PIC X(08).             01330000
       01  WSV-LEIDOS              PIC 9(08) VALUE ZEROS.               01340000
       01  WSV-ESCRITOS            PIC 9(08) VALUE ZEROS.               01350000
       01  WSV-FECHA-PRO.                                               01360000
           02 WSV-FECHA-PRO-A      PIC X(04).                           01370000
           02 WSV-FECHA-PRO-M      PIC X(02).                           01380000
           02 WSV-FECHA-PRO-D      PIC X(02).                           01390000
       01  WSN-FECHA-PRO-N REDEFINES WSV-FECHA-PRO PIC 9(08).           01400000
      *                                                                *01410000
       01  WR-NEGLOT.                                                   01420000
           02  WA-TIPNEG             PIC  X(01)    VALUE 'L'.           01430000
           02  WA-NEGLOT             PIC S9(07)V   USAGE COMP-3.        01440000
      *                                                                *01450000
       01  REG-S1DQ9FTC-W.                                              01460000
           10 S01-NUMCLI           PIC X(08).                           01470000
           10 S01-TIPDOC           PIC X(01).                           01480000
           10 S01-NRODOC           PIC X(11).                           01490000
           10 S01-CTAVAL20         PIC X(18).                           01500000
           10 S01-MONCONTR         PIC X(03).                           01510000
           10 S01-FECALTA          PIC X(08).                           01520000
           10 S01-FECCESE          PIC X(08).                           01530000
           10 S01-SIGNO-SDOREGI    PIC X(01).                           01540000
           10 S01-SDOREGI          PIC 9(12)V9(02).                     01550000
           10 S01-SIGNO-SDOINVE    PIC X(01).                           01560000
           10 S01-SDOINVE          PIC 9(12)V9(02).                     01570000
           10 S01-SIGNO-IMPOVTA    PIC X(01).                           01580000
           10 S01-IMPOVTA          PIC 9(12)V9(02).                     01590000
           10 S01-SIGNO-IMPINTE    PIC X(01).                           01600000
           10 S01-IMPINTE          PIC 9(12)V9(02).                     01610000
           10 S01-SIGNO-IMPDIVI    PIC X(01).                           01620000
           10 S01-IMPDIVI          PIC 9(12)V9(02).                     01630000
           10 S01-SIGNO-IMPVCTO    PIC X(01).                           01640000
           10 S01-IMPVCTO          PIC 9(12)V9(02).                     01650000
           10 S01-DIVISA           PIC X(03).                           01660000
           10 S01-RUT              PIC 9(08).                           01670000
           10 S01-IND-CTAREG       PIC X(02).                           01680000
           10 S01-SIGNO-SDOREGULTI PIC X(01).                           01690000
           10 S01-SDOREGULTI       PIC 9(12)V9(02).                     01700000
           10 S01-FCHREGULTI       PIC X(08).                           01710000
           10 S01-FEALTREG         PIC X(08).                           01720000
           10 S01-SIGNO-ULTINVE    PIC X(01).                           01730000
           10 S01-ULTINVE          PIC 9(12)V9(02).                     01740000
           10 S01-FHULINVE         PIC X(08).                           01750000
      *                                                                *01760000
       01  WA-S1DQ9FTC.                                                 01770000
           10 W01-NUMCLI           PIC X(08).                           01780000
           10 W01-TIPDOC           PIC X(01).                           01790000
           10 W01-NRODOC           PIC X(11).                           01800000
           10 W01-CTAVAL20         PIC X(18).                           01810000
           10 W01-MONCONTR         PIC X(03).                           01820000
           10 W01-FECALTA          PIC X(08).                           01830000
           10 W01-FECCESE          PIC X(08).                           01840000
           10 W01-SIGNO-SDOREGI    PIC X(01).                           01850000
           10 W01-SDOREGI          PIC 9(12)V9(02).                     01860000
           10 W01-SIGNO-SDOINVE    PIC X(01).                           01870000
           10 W01-SDOINVE          PIC 9(12)V9(02).                     01880000
           10 W01-SIGNO-IMPOVTA    PIC X(01).                           01890000
           10 W01-IMPOVTA          PIC 9(12)V9(02).                     01900000
           10 W01-SIGNO-IMPINTE    PIC X(01).                           01910000
           10 W01-IMPINTE          PIC 9(12)V9(02).                     01920000
           10 W01-SIGNO-IMPDIVI    PIC X(01).                           01930000
           10 W01-IMPDIVI          PIC 9(12)V9(02).                     01940000
           10 W01-SIGNO-IMPVCTO    PIC X(01).                           01950000
           10 W01-IMPVCTO          PIC 9(12)V9(02).                     01960000
           10 W01-DIVISA           PIC X(03).                           01970000
           10 W01-RUT              PIC 9(08).                           01980000
           10 W01-IND-CTAREG       PIC X(02).                           01990000
           10 W01-SIGNO-SDOREGULTI PIC X(01).                           02000000
           10 W01-SDOREGULTI       PIC 9(12)V9(02).                     02010000
           10 W01-FCHREGULTI       PIC X(08).                           02020000
           10 W01-FEALTREG         PIC X(08).                           02030000
           10 W01-SIGNO-ULTINVE    PIC X(01).                           02040000
           10 W01-ULTINVE          PIC 9(12)V9(02).                     02050000
           10 W01-FHULINVE         PIC X(08).                           02060000
      *                                                                *02070000
       01  WA-VAR-SALDOS.                                               02080000
           02 WA-SALDO                PIC  9(15).                       02090000
           02 WH-SALD0                PIC S9(15).                       02100000
           02 WH-NOMINEM              PIC  9(13)V9(05).                 02110000
           02 WA-SALDO-INVER          PIC  9(15)V9(02).                 02120000
           02 WA-SALDO-INVER-0        PIC  9(15)V9(02).                 02130000
           02 WA-SALDO-VENTA          PIC  9(15)V9(02).                 02140000
           02 WA-DIVPEN               PIC  9(15)V9(02).                 02150000
           02 WA-INTPEN               PIC  9(15)V9(02).                 02160000
           02 WA-AMTPEN               PIC  9(15)V9(02).                 02170000
           02 WA-DIVUSD               PIC  9(15)V9(02).                 02180000
           02 WA-INTUSD               PIC  9(15)V9(02).                 02190000
           02 WA-AMTUSD               PIC  9(15)V9(02).                 02200000
      *                                                                *02210000
      *    BD PERSONAS                                                  02220000
       01  W-PEWC5201.                                                  02230000
           COPY PEWC5201.                                               02240000
      *    TAB-VLDTHIS : COPY CON OCCURS                                02250000
           COPY VLTCHI2.                                                02260000
      *                                                                *02270000
      ******************************************************************02280000
      ********* COPYS DE ERRORES                             ***********02290000
      ******************************************************************02300000
      *                                                                *02310000
           COPY QRECDB2.                                                02320000
           COPY VLWCRUTI.                                               02330000
           COPY VLWC8000.                                               02340000
           COPY VLWC8010.                                               02350000
           COPY VLWC8020.                                               02360000
      *                                                                *02370000
      ******************************************************************02380000
      ********* INCLUDE DE TABLAS                            ***********02390000
      ******************************************************************02400000
      *                                                                *02410000
           EXEC SQL INCLUDE SQLCA   END-EXEC.                           02420000
           EXEC SQL INCLUDE VLGTARC END-EXEC.                           02430000
           EXEC SQL INCLUDE VLGTPOL END-EXEC.                           02440000
           EXEC SQL INCLUDE VLGTXEN END-EXEC.                           02450000
           EXEC SQL INCLUDE VLGTCAM END-EXEC.                           02460000
           EXEC SQL INCLUDE VLGTHAC END-EXEC.                           02470000
           EXEC SQL INCLUDE VLGTADS END-EXEC.                           02480000
N*         EXEC SQL INCLUDE VLGTSMO END-EXEC.                           02490000
N          EXEC SQL INCLUDE VLGTSMM END-EXEC.                           02500000
N          EXEC SQL INCLUDE VLGTHIS END-EXEC.                           02510000
      *                                                                *02520000
      *----------------------------------------------------------------*02530000
      *  AREA CURSORES DB2                                             *02540000
      *----------------------------------------------------------------*02550000
      *    COTIZACIONES DE INVERSIONES                                 *02560000
           EXEC SQL                                                     02570000
                DECLARE VLDCCAM  CURSOR FOR                             02580000
                 SELECT VCAM_CIERRE_D                                   02590000
                      , VCAM_FECDIA                                     02600000
                      , VCAM_FILLER                                     02610000
                   FROM VLDTCAM                                         02620000
                  WHERE VCAM_CODVALOR  = :VCAM-CODVALOR                 02630000
                    AND VCAM_FECDIA   <= :VCAM-FECDIA                   02640000
                    AND VCAM_CIERRE_D <>  0                             02650000
                  ORDER BY VCAM_FECDIA DESC                             02660000
                  OPTIMIZE FOR 1 ROW                                    02670000
           END-EXEC.                                                    02680000
      *    OPERACIONES CONTABILIZADAS                                  *02690000
           EXEC SQL                                                     02700000
                DECLARE VLDCHAC  CURSOR FOR                             02710000
                 SELECT VHAC_OPERAC                                     02720000
                      , VHAC_IMPLIQ                                     02730000
                      , VHAC_MONEDA_CTA                                 02740000
                   FROM VLDTHAC                                         02750000
                  WHERE VHAC_CUENTA = :VHAC-CUENTA                      02760000
                    AND VHAC_OPERAC IN (07, 08, 11)                     02770000
                    AND VHAC_FCONTA BETWEEN :WHAC-FECINI AND            02780000
                                            :WHAC-FECFIN                02790000
           END-EXEC.                                                    02800000
      *    POLIZAS                                                      02810000
           EXEC SQL                                                     02820000
                DECLARE VLDCPOL  CURSOR FOR                             02830000
                 SELECT VPOL_EFECTI                                     02840000
                      , VPOL_CUPCORR                                    02850000
                      , VPOL_IMPCOM1                                    02860000
                      , VPOL_IMPCOM2                                    02870000
                      , VPOL_IMPCOM3                                    02880000
                      , VPOL_IMPCOM4                                    02890000
                      , VPOL_IMPCOM5                                    02900000
                      , VPOL_IMPCOM6                                    02910000
                      , VPOL_IMPCOM7                                    02920000
                      , VPOL_IMPCOM8                                    02930000
                      , VPOL_IGV                                        02940000
                   FROM VLDTPOL                                         02950000
                  WHERE VPOL_CUENTA  = :VPOL-CUENTA                     02960000
                    AND VPOL_COMVEN  = :VPOL-COMVEN                     02970000
                    AND VPOL_SITUAC  = :VPOL-SITUAC                     02980000
                    AND VPOL_FECHEJ  BETWEEN :WPOL-FECINI AND           02990000
                                             :WPOL-FECFIN               03000000
           END-EXEC.                                                    03010000
      *    MOV-CTA-REGISTRO ULTIMO SALDO DEL AÑO                        03020000
           EXEC SQL                                                     03030000
                DECLARE VLDCSMM  CURSOR FOR                             03040000
                 SELECT VSMM_SALDO_AUT                                  03050000
                      , VSMM_NUMREF                                     03060000
                   FROM VLDTSMM                                         03070000
                  WHERE VSMM_CTAVAL    = :VSMM-CTAVAL                   03080000
                    AND VSMM_FECONTA  <= :VSMM-FECONTA                  03090000
                    AND VSMM_OBSERVA NOT IN (:WA-APERTURA, :WA-ENTRADA, 03100000
                                             :WA-SALIDA)                03110000
                  ORDER BY VSMM_NUMREF DESC                             03120000
                  OPTIMIZE FOR 1 ROW                                    03130000
           END-EXEC.                                                    03140000
      *    MOV-CTA-REGISTRO ULTIMO SALDO                                03150000
           EXEC SQL                                                     03160000
                DECLARE VLDUSMM  CURSOR FOR                             03170000
                 SELECT VSMM_SALDO_AUT                                  03180000
                      , VSMM_FECONTA                                    03190000
                   FROM VLDTSMM                                         03200000
                  WHERE VSMM_CTAVAL     = :VSMM-CTAVAL                  03210000
                    AND VSMM_FECONTA   <= :VSMM-FECONTA                 03220000
                    AND VSMM_SALDO_AUT <> 0                             03230000
                    AND VSMM_OBSERVA NOT IN (:WA-APERTURA, :WA-ENTRADA, 03240000
                                             :WA-SALIDA)                03250000
                  ORDER BY VSMM_NUMREF DESC                             03260000
                  OPTIMIZE FOR 1 ROW                                    03270000
           END-EXEC.                                                    03280000
      *    HIS-SDO-MENSUAL- ULTIMO SALDO TITULOS                        03290000
           EXEC SQL                                                     03300000
                DECLARE VLDUHIS  CURSOR FOR                             03310000
                 SELECT VHIS_CODVALOR                                   03320000
                      , VHIS_TITULOS1                                   03330000
                      , VHIS_MOVIMI1                                    03340000
                      , VHIS_CUSTODIA1                                  03350000
                      , VHIS_CAMBIO1                                    03360000
                      , VHIS_COBRADO1                                   03370000
                      , VHIS_TITULOS2                                   03380000
                      , VHIS_MOVIMI2                                    03390000
                      , VHIS_CUSTODIA2                                  03400000
                      , VHIS_CAMBIO2                                    03410000
                      , VHIS_COBRADO2                                   03420000
                      , VHIS_TITULOS3                                   03430000
                      , VHIS_MOVIMI3                                    03440000
                      , VHIS_CUSTODIA3                                  03450000
                      , VHIS_CAMBIO3                                    03460000
                      , VHIS_COBRADO3                                   03470000
                      , VHIS_TITULOS4                                   03480000
                      , VHIS_MOVIMI4                                    03490000
                      , VHIS_CUSTODIA4                                  03500000
                      , VHIS_CAMBIO4                                    03510000
                      , VHIS_COBRADO4                                   03520000
                      , VHIS_TITULOS5                                   03530000
                      , VHIS_MOVIMI5                                    03540000
                      , VHIS_CUSTODIA5                                  03550000
                      , VHIS_CAMBIO5                                    03560000
                      , VHIS_COBRADO5                                   03570000
                      , VHIS_TITULOS6                                   03580000
                      , VHIS_MOVIMI6                                    03590000
                      , VHIS_CUSTODIA6                                  03600000
                      , VHIS_CAMBIO6                                    03610000
                      , VHIS_COBRADO6                                   03620000
                      , VHIS_TITULOS7                                   03630000
                      , VHIS_MOVIMI7                                    03640000
                      , VHIS_CUSTODIA7                                  03650000
                      , VHIS_CAMBIO7                                    03660000
                      , VHIS_COBRADO7                                   03670000
                      , VHIS_TITULOS8                                   03680000
                      , VHIS_MOVIMI8                                    03690000
                      , VHIS_CUSTODIA8                                  03700000
                      , VHIS_CAMBIO8                                    03710000
                      , VHIS_COBRADO8                                   03720000
                      , VHIS_TITULOS9                                   03730000
                      , VHIS_MOVIMI9                                    03740000
                      , VHIS_CUSTODIA9                                  03750000
                      , VHIS_CAMBIO9                                    03760000
                      , VHIS_COBRADO9                                   03770000
                      , VHIS_TITULOS10                                  03780000
                      , VHIS_MOVIMI10                                   03790000
                      , VHIS_CUSTODIA10                                 03800000
                      , VHIS_CAMBIO10                                   03810000
                      , VHIS_COBRADO10                                  03820000
                      , VHIS_TITULOS11                                  03830000
                      , VHIS_MOVIMI11                                   03840000
                      , VHIS_CUSTODIA11                                 03850000
                      , VHIS_CAMBIO11                                   03860000
                      , VHIS_COBRADO11                                  03870000
                      , VHIS_TITULOS12                                  03880000
                      , VHIS_MOVIMI12                                   03890000
                      , VHIS_CUSTODIA12                                 03900000
                      , VHIS_CAMBIO12                                   03910000
                      , VHIS_COBRADO12                                  03920000
                      , VHIS_TITULOS13                                  03930000
                      , VHIS_MOVIMI13                                   03940000
                      , VHIS_CUSTODIA13                                 03950000
                      , VHIS_CAMBIO13                                   03960000
                      , VHIS_COBRADO13                                  03970000
                      , VHIS_TITULOS14                                  03980000
                      , VHIS_MOVIMI14                                   03990000
                      , VHIS_CUSTODIA14                                 04000000
                      , VHIS_CAMBIO14                                   04010000
                      , VHIS_COBRADO14                                  04020000
                      , VHIS_TITULOS15                                  04030000
                      , VHIS_MOVIMI15                                   04040000
                      , VHIS_CUSTODIA15                                 04050000
                      , VHIS_CAMBIO15                                   04060000
                      , VHIS_COBRADO15                                  04070000
                      , VHIS_TITULOS16                                  04080000
                      , VHIS_MOVIMI16                                   04090000
                      , VHIS_CUSTODIA16                                 04100000
                      , VHIS_CAMBIO16                                   04110000
                      , VHIS_COBRADO16                                  04120000
                      , VHIS_TITULOS17                                  04130000
                      , VHIS_MOVIMI17                                   04140000
                      , VHIS_CUSTODIA17                                 04150000
                      , VHIS_CAMBIO17                                   04160000
                      , VHIS_COBRADO17                                  04170000
                      , VHIS_TITULOS18                                  04180000
                      , VHIS_MOVIMI18                                   04190000
                      , VHIS_CUSTODIA18                                 04200000
                      , VHIS_CAMBIO18                                   04210000
                      , VHIS_COBRADO18                                  04220000
                      , VHIS_TITULOS19                                  04230000
                      , VHIS_MOVIMI19                                   04240000
                      , VHIS_CUSTODIA19                                 04250000
                      , VHIS_CAMBIO19                                   04260000
                      , VHIS_COBRADO19                                  04270000
                      , VHIS_TITULOS20                                  04280000
                      , VHIS_MOVIMI20                                   04290000
                      , VHIS_CUSTODIA20                                 04300000
                      , VHIS_CAMBIO20                                   04310000
                      , VHIS_COBRADO20                                  04320000
                      , VHIS_TITULOS21                                  04330000
                      , VHIS_MOVIMI21                                   04340000
                      , VHIS_CUSTODIA21                                 04350000
                      , VHIS_CAMBIO21                                   04360000
                      , VHIS_COBRADO21                                  04370000
                      , VHIS_TITULOS22                                  04380000
                      , VHIS_MOVIMI22                                   04390000
                      , VHIS_CUSTODIA22                                 04400000
                      , VHIS_CAMBIO22                                   04410000
                      , VHIS_COBRADO22                                  04420000
                      , VHIS_TITULOS23                                  04430000
                      , VHIS_MOVIMI23                                   04440000
                      , VHIS_CUSTODIA23                                 04450000
                      , VHIS_CAMBIO23                                   04460000
                      , VHIS_COBRADO23                                  04470000
                      , VHIS_TITULOS24                                  04480000
                      , VHIS_MOVIMI24                                   04490000
                      , VHIS_CUSTODIA24                                 04500000
                      , VHIS_CAMBIO24                                   04510000
                      , VHIS_COBRADO24                                  04520000
                      , VHIS_TITULOS25                                  04530000
                      , VHIS_MOVIMI25                                   04540000
                      , VHIS_CUSTODIA25                                 04550000
                      , VHIS_CAMBIO25                                   04560000
                      , VHIS_COBRADO25                                  04570000
                      , VHIS_TITULOS26                                  04580000
                      , VHIS_MOVIMI26                                   04590000
                      , VHIS_CUSTODIA26                                 04600000
                      , VHIS_CAMBIO26                                   04610000
                      , VHIS_COBRADO26                                  04620000
                      , VHIS_TITULOS27                                  04630000
                      , VHIS_MOVIMI27                                   04640000
                      , VHIS_CUSTODIA27                                 04650000
                      , VHIS_CAMBIO27                                   04660000
                      , VHIS_COBRADO27                                  04670000
                      , VHIS_TITULOS28                                  04680000
                      , VHIS_MOVIMI28                                   04690000
                      , VHIS_CUSTODIA28                                 04700000
                      , VHIS_CAMBIO28                                   04710000
                      , VHIS_COBRADO28                                  04720000
                      , VHIS_TITULOS29                                  04730000
                      , VHIS_MOVIMI29                                   04740000
                      , VHIS_CUSTODIA29                                 04750000
                      , VHIS_CAMBIO29                                   04760000
                      , VHIS_COBRADO29                                  04770000
                      , VHIS_TITULOS30                                  04780000
                      , VHIS_MOVIMI30                                   04790000
                      , VHIS_CUSTODIA30                                 04800000
                      , VHIS_CAMBIO30                                   04810000
                      , VHIS_COBRADO30                                  04820000
                      , VHIS_TITULOS31                                  04830000
                      , VHIS_MOVIMI31                                   04840000
                      , VHIS_CUSTODIA31                                 04850000
                      , VHIS_CAMBIO31                                   04860000
                      , VHIS_COBRADO31                                  04870000
                      , VHIS_FEALTREG                                   04880000
                      , VHIS_FEULMOD                                    04890000
                      , VHIS_HORULMOD                                   04900000
                      , VHIS_NUMTER                                     04910000
                      , VHIS_USUARIO                                    04920000
                   FROM VLDTHIS                                         04930000
                  WHERE VHIS_CTAVAL   = :VHIS-CTAVAL                    04940000
                    AND VHIS_TIPGAS  IN (48, 49)                        04950000
                    AND VHIS_ANO      = :VHIS-ANO                       04960000
                    AND VHIS_MES      = :VHIS-MES                       04970000
                    AND VHIS_CODVALOR > :VHIS-CODVALOR                  04980000
           END-EXEC.                                                    04990000
      *                                                                *05000000
      *---------------*                                                 05010000
       LINKAGE SECTION.                                                 05020000
      *---------------*                                                 05030000
       01  LK-PARAMETROS.                                               05040000
           02  LK-LONGITUD     PIC S9(4)   COMP.                        05050000
           02  LK-FECHA-D.                                              05060000
               03  LK-F-AA-D   PIC 9999.                                05070000
               03  LK-F-MM-D   PIC 99.                                  05080000
               03  LK-F-DD-D   PIC 99.                                  05090000
           02  LK-RFECHA-D  REDEFINES LK-FECHA-D PIC 9(08).             05100000
           02  LK-FECHA-H.                                              05110000
               03  LK-F-AA-H   PIC 9999.                                05120000
               03  LK-F-MM-H   PIC 99.                                  05130000
               03  LK-F-DD-H   PIC 99.                                  05140000
           02  LK-RFECHA-H  REDEFINES LK-FECHA-H PIC 9(08).             05150000
      *                                                                 05160000
      *---------------------------------------*                         05170000
       PROCEDURE DIVISION USING LK-PARAMETROS.                          05180000
      *---------------------------------------*                         05190000
      *                                                                 05200000
           PERFORM 10000-INICIO.                                        05210000
      *                                                                 05220000
           IF WS-FECHA-ENTRADA = WS-FECHA-PROCESO                       05221004
              PERFORM 20000-PROCESO UNTIL FS-E1DQ9FTC = '10'.           05230003
           ELSE                                                         05231003
              PERFORM 10010-LEER-ENTRADA                                05232003
           END-IF                                                       05233003
      *                                                                 05240000
           PERFORM 30000-FIN.                                           05250000
      *                                                                 05260000
           STOP RUN.                                                    05270000
      *                                                                 05280000
      ******************************************************************05290000
      *                       1-INICIO                                 *05300000
      *       INICIALIZA LAS WORKAS DE LAS TABLAS Y LOS CAMPOS DE      *05310000
      *       TRABAJO. LEE LA FECHA EN LA TABLA UGDTPRC, TOMANDO LA    *05320000
      *       FILA CON CODIGO DE PROCESO = 'UB00' Y COMPRUEBA QUE      *05330000
      *       EL PROCESO ESTA ACTIVO.                                  *05340000
      ******************************************************************05350000
       10000-INICIO.                                                    05360000
      *-------------*                                                   05370000
      *                                                                 05380000
PAVXX *    MOVE    LK-RFECHA       TO WSV-FECHA-PRO.                    05390000
PAVXX *    MOVE    WSN-FECHA-PRO-N TO WSV-FECHA-DES-N, WSV-FECHA-HAS-N. 05400000
      *    MOVE    01              TO WSV-FECHA-DES-D, WSV-FECHA-DES-M. 05410000
           MOVE  LK-RFECHA-H        TO WSV-FECHA-PRO                    05420000
           MOVE  LK-RFECHA-D        TO WSV-FECHA-DES                    05430000
           MOVE  LK-RFECHA-H        TO WSV-FECHA-HAS                    05440000
           MOVE  WSV-FECHA-DES-N    TO WPOL-FECINI, WHAC-FECINI         05450000
           MOVE  WSV-FECHA-HAS-N    TO WPOL-FECFIN, WHAC-FECFIN         05460000
      *                                                                *05470000
           OPEN INPUT  E1DQ9FTC, E2DQ9ADS                               05480000
                OUTPUT S1DQ9FTC.                                        05490000
      *                                                                *05500000
           IF (FS-E1DQ9FTC EQUAL '00' OR '97')                          05510000
              CONTINUE                                                  05520000
           ELSE                                                         05530000
              DISPLAY '***********************************'             05540000
              DISPLAY '*  ERROR AL OPEN DE ENTRADA1      *'             05550000
              DISPLAY '*  ERROR FS-OPS ES :' FS-E1DQ9FTC                05560000
              DISPLAY '***********************************'             05570000
              MOVE '02'  TO RETURN-CODE                                 05580000
              STOP RUN                                                  05590000
           END-IF                                                       05600000
      *                                                                *05610000
           IF (FS-E2DQ9ADS EQUAL '00' OR '97')                          05620000
              CONTINUE                                                  05630000
           ELSE                                                         05640000
              DISPLAY '***********************************'             05650000
              DISPLAY '*  ERROR AL OPEN DE ENTRADA1      *'             05660000
              DISPLAY '*  ERROR FS-OPS ES :' FS-E2DQ9ADS                05670000
              DISPLAY '***********************************'             05680000
              MOVE '02'  TO RETURN-CODE                                 05690000
              STOP RUN                                                  05700000
           END-IF                                                       05710000
      *                                                                *05720000
           IF (FS-S1DQ9FTC EQUAL '00' OR '97')                          05730000
              CONTINUE                                                  05740000
           ELSE                                                         05750000
              DISPLAY '***********************************'             05760000
              DISPLAY '*  ERROR AL OPEN DE SALIDA1       *'             05770000
              DISPLAY '*  ERROR FS-OPS ES :' FS-S1DQ9FTC                05780000
              DISPLAY '***********************************'             05790000
              MOVE '02'  TO RETURN-CODE                                 05800000
              STOP RUN                                                  05810000
           END-IF.                                                      05820000
      *                                                                *05830000
           MOVE ZERO TO VADS-CUENTA.                                    05840000
      *                                                                *05850000
           PERFORM 10010-LEER-ENTRADA.                                  05860000
      *                                                                *05870000
           PERFORM 10020-LEER-SALDOS.                                   05880000
      *                                                                *05890000
      *     *------------*                                              05900000
       10010-LEER-ENTRADA.                                              05910000
      *     *------------*                                              05920000
      *                                                                 05930000
           READ E1DQ9FTC.                                               05940000
                                                                        05950000
           EVALUATE FS-E1DQ9FTC                                         05960000
              WHEN '00'                                                 05970000
                   ADD  1                      TO WSV-LEIDOS            05980000
                   MOVE E01-CTAVAL20 (13:07)   TO WX-CUENTA-ARC7        05990000
                   MOVE WN-CUENTA-ARC7         TO WA-CUENTA-ARC7        06000000
04111              MOVE E01-FECCESE(01:02) TO DIA-ENTRADA               06001004
  |                MOVE E01-FECCESE(04:02) TO MES-ENTRADA               06002004
04111              MOVE E01-FECCESE(07:04) TO ANIO-ENTRADA              06003004
              WHEN '10'                                                 06010000
                   CONTINUE                                             06020000
              WHEN OTHER                                                06030000
                   DISPLAY '***********************************'        06040000
                   DISPLAY '*  ERROR AL LEER ENTRADA          *'        06050000
                   DISPLAY '*  ERROR FS-OPS ES :' FS-E1DQ9FTC           06060000
                   DISPLAY '***********************************'        06070000
                   MOVE '02'  TO RETURN-CODE                            06080000
                   STOP RUN                                             06090000
           END-EVALUATE                                                 06100000
           .                                                            06110000
      *                                                                 06120000
      *     *-----------*                                               06130000
       10020-LEER-SALDOS.                                               06140000
      *     *-----------*                                               06150000
      *                                                                 06160000
           READ E2DQ9ADS.                                               06170000
                                                                        06180000
           EVALUATE FS-E2DQ9ADS                                         06190000
              WHEN '00'                                                 06200000
                   MOVE REG-E2DQ9ADS           TO DCLVLDTADS            06210000
              WHEN '10'                                                 06220000
                   CONTINUE                                             06230000
              WHEN OTHER                                                06240000
                   DISPLAY '***********************************'        06250000
                   DISPLAY '*  ERROR AL LEER ENTRADA SALDOS   *'        06260000
                   DISPLAY '*  ERROR FS-ADS ES :' FS-E2DQ9ADS           06270000
                   DISPLAY '***********************************'        06280000
                   MOVE '02'  TO RETURN-CODE                            06290000
                   STOP RUN                                             06300000
           END-EVALUATE.                                                06310000
      *                                                                *06320000
      *     *------------*                                              06330000
       10030-PRECIO-VALOR.                                              06340000
      *     *------------*                                              06350000
      *                                                                *06360000
      *                                                                *06370000
           EXEC SQL                                                     06380000
                OPEN VLDCCAM                                            06390000
           END-EXEC                                                     06400000
      *                                                                 06410000
           MOVE SQLCODE TO SQLCODE-AUX                                  06420000
      *                                                                 06430000
           EVALUATE TRUE                                                06440000
               WHEN DB2-OK                                              06450000
                    CONTINUE                                            06460000
               WHEN OTHER                                               06470000
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      06480002
                    MOVE 'VLDTCAM'               TO  W801-TABLA         06490000
                    MOVE 'OPEN'                  TO  W801-ACCION        06500000
                    MOVE WX-CUENTA-ARC7          TO  W801-CLAVE (01:07) 06510000
                    MOVE VCAM-CODVALOR           TO  W801-CLAVE (09:12) 06520000
                    MOVE SQLCODE                 TO  W801-SQLCODE       06530000
                    MOVE SPACES                  TO  W801-SQLWARN       06540000
                    MOVE '10030-PRECIO-VALOR   ' TO  W801-PARRAFO       06550000
                    PERFORM VLPC8010-DISP-ABEND-DB2                     06560000
                    PERFORM VLPC8010-ABEND-DB2                          06570000
           END-EVALUATE.                                                06580000
      *                                                                 06590000
           EXEC SQL                                                     06600000
                FETCH  VLDCCAM                                          06610000
                 INTO :VCAM-CIERRE-D                                    06620000
                    , :VCAM-FECDIA                                      06630000
                    , :VCAM-FILLER                                      06640000
           END-EXEC                                                     06650000
                                                                        06660000
           MOVE  SQLCODE TO SQLCODE-AUX                                 06670000
                                                                        06680000
           EVALUATE TRUE                                                06690000
               WHEN DB2-OK                                              06700000
                    MOVE VCAM-FILLER (01:05) TO WR-NEGLOT               06710000
                    IF WA-TIPNEG = 'L' AND                              06720000
                       WA-NEGLOT >     ZEROS                            06730000
                       COMPUTE VCAM-CIERRE-D = VCAM-CIERRE-D / WA-NEGLOT06740000
                    END-IF                                              06750000
               WHEN DB2-NOTFND                                          06760000
                    IF VXEN-TIPINT = 'F'                                06770000
100%                   MOVE 100.00               TO  VCAM-CIERRE-D      06780000
                    ELSE                                                06790000
                       MOVE VXEN-NOMINEM         TO  VCAM-CIERRE-D      06800000
                    END-IF                                              06810000
               WHEN OTHER                                               06820000
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      06830002
                    MOVE 'VLDTCAM'               TO  W801-TABLA         06840000
                    MOVE 'SELECT-FIRST'          TO  W801-ACCION        06850000
                    MOVE WX-CUENTA-ARC7          TO  W801-CLAVE (01:07) 06860000
                    MOVE VCAM-CODVALOR           TO  W801-CLAVE (09:12) 06870000
                    MOVE SQLCODE                 TO  W801-SQLCODE       06880000
                    MOVE SPACES                  TO  W801-SQLWARN       06890000
                    MOVE '10030-PRECIO-VALOR   ' TO  W801-PARRAFO       06900000
                    PERFORM VLPC8010-DISP-ABEND-DB2                     06910000
                    PERFORM VLPC8010-ABEND-DB2                          06920000
           END-EVALUATE.                                                06930000
                                                                        06940000
           EXEC SQL                                                     06950000
                CLOSE VLDCCAM                                           06960000
           END-EXEC                                                     06970000
      *                                                                 06980000
           MOVE SQLCODE TO SQLCODE-AUX                                  06990000
      *                                                                 07000000
           EVALUATE TRUE                                                07010000
               WHEN DB2-OK                                              07020000
                    CONTINUE                                            07030000
               WHEN OTHER                                               07040000
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      07050002
                    MOVE 'VLDTCAM'               TO  W801-TABLA         07060000
                    MOVE 'CLOSE'                 TO  W801-ACCION        07070000
                    MOVE WX-CUENTA-ARC7          TO  W801-CLAVE (01:07) 07080000
                    MOVE VCAM-CODVALOR           TO  W801-CLAVE (09:12) 07090000
                    MOVE SQLCODE                 TO  W801-SQLCODE       07100000
                    MOVE SPACES                  TO  W801-SQLWARN       07110000
                    MOVE '10030-PRECIO-VALOR   ' TO  W801-PARRAFO       07120000
                    PERFORM VLPC8010-DISP-ABEND-DB2                     07130000
                    PERFORM VLPC8010-ABEND-DB2                          07140000
           END-EVALUATE.                                                07150000
      *                                                                *07160000
      *     *-------*                                                   07170000
       20000-PROCESO.                                                   07180000
      *     *-------*                                                   07190000
      *                                                                 07200000
           INITIALIZE REG-S1DQ9FTC-W                                    07210000
      *                                                                 07220000
           PERFORM 20030-GENERA-SALDOS                                  07230000
      *                                                                 07240000
           PERFORM 20010-OBTIENE-CLIENTE                                07250000
      *                                                                 07260000
           MOVE E01-NUMCLI               TO S01-NUMCLI                  07270000
           MOVE W520-CODIDENT            TO S01-TIPDOC                  07280000
           MOVE W520-CLAIDENT            TO S01-NRODOC                  07290000
           MOVE E01-CTAVAL20 (01:08)     TO S01-CTAVAL20 (01:08)        07300000
           MOVE E01-CTAVAL20 (11:10)     TO S01-CTAVAL20 (09:10)        07310000
           MOVE E01-FECALTA (07:04)      TO S01-FECALTA (01:04)         07320000
           MOVE E01-FECALTA (04:02)      TO S01-FECALTA (05:02)         07330000
           MOVE E01-FECALTA (01:02)      TO S01-FECALTA (07:02)         07340000
           MOVE E01-FECCESE (07:04)      TO S01-FECCESE (01:04)         07350000
           MOVE E01-FECCESE (04:02)      TO S01-FECCESE (05:02)         07360000
           MOVE E01-FECCESE (01:02)      TO S01-FECCESE (07:02)         07370000
           MOVE E01-RUT                  TO S01-RUT                     07380000
                                                                        07390000
           IF WA-SALDO-INVER < ZEROS                                    07400000
              MOVE '-'                   TO S01-SIGNO-SDOINVE           07410000
           END-IF                                                       07420000
                                                                        07430000
           MOVE WA-SALDO-INVER           TO S01-SDOINVE                 07440000
                                                                        07450000
           IF WA-SALDO-VENTA < ZEROS                                    07460000
              MOVE '-'                   TO S01-SIGNO-IMPOVTA           07470000
           END-IF                                                       07480000
                                                                        07490000
           MOVE WA-SALDO-VENTA           TO S01-IMPOVTA                 07500000
           MOVE E01-MONEDA               TO S01-MONCONTR                07510000
                                                                        07520000
FVAXX *    IF VSMM-SALDO-AUT < ZEROS                                    07530000
FVAXX *       MOVE '-'                   TO S01-SIGNO-SDOREGI           07540000
FVAXX *    END-IF                                                       07550000
                                                                        07560000
FVAXX      IF E01-SITUACION = 'CANCELADA'                               07570000
FVAXX         MOVE SPACES                TO S01-SIGNO-SDOREGI           07580000
FVAXX         MOVE ZEROES                TO S01-SDOREGI                 07590000
FVAXX      ELSE                                                         07600000
FVAXX         IF VSMM-SALDO-AUT < ZEROS                                 07610000
FVAXX            MOVE SPACES             TO S01-SIGNO-SDOREGI           07620000
FVAXX            MOVE ZEROES             TO S01-SDOREGI                 07630000
FVAXX         ELSE                                                      07640000
                 MOVE VSMM-SALDO-AUT     TO S01-SDOREGI                 07650000
FVAXX         END-IF                                                    07660000
FVAXX      END-IF                                                       07670000
                                                                        07680000
FVAXX *    IF WSMM-SALDO-AUT < ZEROS                                    07690000
FVAXX *       MOVE '-'                   TO S01-SIGNO-SDOREGULTI        07700000
FVAXX *    END-IF                                                       07710000
                                                                        07720000
FVAXX      IF E01-SITUACION = 'CANCELADA'                               07730000
FVAXX         MOVE SPACES                TO S01-SIGNO-SDOREGULTI        07740000
FVAXX         MOVE ZEROES                TO S01-SDOREGULTI              07750000
FVAXX      ELSE                                                         07760000
FVAXX         IF VSMM-SALDO-AUT < ZEROS                                 07770000
FVAXX            MOVE SPACES             TO S01-SIGNO-SDOREGULTI        07780000
FVAXX            MOVE ZEROES             TO S01-SDOREGULTI              07790000
FVAXX         ELSE                                                      07800000
                 MOVE WSMM-SALDO-AUT     TO S01-SDOREGULTI              07810000
FVAXX         END-IF                                                    07820000
FVAXX      END-IF                                                       07830000
                                                                        07840000
           IF VSMM-FECONTA  = '9999-12-31'                              07850000
              MOVE SPACES                TO S01-FCHREGULTI              07860000
           ELSE                                                         07870000
              MOVE VSMM-FECONTA (01:04)  TO S01-FCHREGULTI (01:04)      07880000
              MOVE VSMM-FECONTA (06:02)  TO S01-FCHREGULTI (05:02)      07890000
              MOVE VSMM-FECONTA (09:02)  TO S01-FCHREGULTI (07:02)      07900000
           END-IF                                                       07910000
                                                                        07920000
           IF VSMO-FEALTREG = '9999-12-31'                              07930000
              MOVE 'NO'                  TO S01-IND-CTAREG              07940000
              MOVE SPACES                TO S01-FEALTREG                07950000
           ELSE                                                         07960000
              MOVE 'SI'                  TO S01-IND-CTAREG              07970000
              MOVE VSMO-FEALTREG (01:04) TO S01-FEALTREG (01:04)        07980000
              MOVE VSMO-FEALTREG (06:02) TO S01-FEALTREG (05:02)        07990000
              MOVE VSMO-FEALTREG (09:02) TO S01-FEALTREG (07:02)        08000000
           END-IF                                                       08010000
                                                                        08020000
           MOVE E01-MONEDA               TO S01-DIVISA                  08030000
                                                                        08040000
           IF WA-SALDO-INVER-0 < ZEROS                                  08050000
              MOVE '-'                   TO S01-SIGNO-ULTINVE           08060000
           END-IF                                                       08070000
                                                                        08080000
           MOVE WA-SALDO-INVER-0         TO S01-ULTINVE                 08090000
                                                                        08100000
           IF WHIS-FEC1RA-A > ZEROS                                     08110000
              MOVE WHIS-FEC1RA-A         TO S01-FHULINVE (01:04)        08120000
              MOVE WHIS-FEC1RA-M         TO S01-FHULINVE (05:02)        08130000
              MOVE WHIS-FEC1RA-D         TO S01-FHULINVE (07:02)        08140000
           ELSE                                                         08150000
              MOVE SPACES                TO S01-FHULINVE                08160000
           END-IF                                                       08170000
                                                                        08180000
           IF E01-MONEDA = 'PEN'                                        08190000
                                                                        08200000
              IF WA-DIVPEN < ZEROS                                      08210000
                 MOVE '-'                TO S01-SIGNO-IMPDIVI           08220000
              END-IF                                                    08230000
                                                                        08240000
              MOVE WA-DIVPEN             TO S01-IMPDIVI                 08250000
                                                                        08260000
              IF WA-INTPEN < ZEROS                                      08270000
                 MOVE '-'                TO S01-SIGNO-IMPINTE           08280000
              END-IF                                                    08290000
                                                                        08300000
              MOVE WA-INTPEN             TO S01-IMPINTE                 08310000
                                                                        08320000
              IF WA-AMTPEN < ZEROS                                      08330000
                 MOVE '-'                TO S01-SIGNO-IMPVCTO           08340000
              END-IF                                                    08350000
                                                                        08360000
              MOVE WA-AMTPEN             TO S01-IMPVCTO                 08370000
              MOVE REG-S1DQ9FTC          TO WA-S1DQ9FTC                 08380000
              PERFORM 20020-GRABA-SALIDA                                08390000
           ELSE                                                         08400000
                                                                        08410000
              IF WA-DIVUSD < ZEROS                                      08420000
                 MOVE '-'                TO S01-SIGNO-IMPDIVI           08430000
              END-IF                                                    08440000
                                                                        08450000
              MOVE WA-DIVUSD             TO S01-IMPDIVI                 08460000
                                                                        08470000
              IF WA-INTUSD < ZEROS                                      08480000
                 MOVE '-'                TO S01-SIGNO-IMPINTE           08490000
              END-IF                                                    08500000
                                                                        08510000
              MOVE WA-INTUSD             TO S01-IMPINTE                 08520000
                                                                        08530000
              IF WA-AMTUSD < ZEROS                                      08540000
                 MOVE '-'                TO S01-SIGNO-IMPVCTO           08550000
              END-IF                                                    08560000
                                                                        08570000
              MOVE WA-AMTUSD             TO S01-IMPVCTO                 08580000
              MOVE REG-S1DQ9FTC          TO WA-S1DQ9FTC                 08590000
              PERFORM 20020-GRABA-SALIDA                                08600000
           END-IF                                                       08610000
                                                                        08620000
      * PARA LIQUIDACIONES CON MONEDA DIFERENTE AL CONTRATO             08630000
           IF E01-MONEDA = 'PEN'                                        08640000
              IF WA-DIVUSD > ZEROS OR                                   08650000
                 WA-INTUSD > ZEROS OR                                   08660000
                 WA-AMTUSD > ZEROS                                      08670000
                 MOVE WA-S1DQ9FTC        TO REG-S1DQ9FTC                08680000
                 MOVE SPACES             TO S01-SIGNO-SDOINVE           08690000
                 MOVE ZEROS              TO S01-SDOINVE                 08700000
                 MOVE SPACES             TO S01-SIGNO-IMPOVTA           08710000
                 MOVE ZEROS              TO S01-IMPOVTA                 08720000
                                                                        08730000
                 IF WA-DIVUSD < ZEROS                                   08740000
                    MOVE '-'             TO S01-SIGNO-IMPDIVI           08750000
                 END-IF                                                 08760000
                                                                        08770000
                 MOVE WA-DIVUSD          TO S01-IMPDIVI                 08780000
                                                                        08790000
                 IF WA-INTUSD < ZEROS                                   08800000
                    MOVE '-'             TO S01-SIGNO-IMPINTE           08810000
                 END-IF                                                 08820000
                                                                        08830000
                 MOVE WA-INTUSD          TO S01-IMPINTE                 08840000
                                                                        08850000
                 IF WA-AMTUSD < ZEROS                                   08860000
                    MOVE '-'             TO S01-SIGNO-IMPVCTO           08870000
                 END-IF                                                 08880000
                                                                        08890000
                 MOVE WA-AMTUSD          TO S01-IMPVCTO                 08900000
                 MOVE 'USD'              TO S01-DIVISA                  08910000
                 PERFORM 20020-GRABA-SALIDA                             08920000
              END-IF                                                    08930000
           ELSE                                                         08940000
              IF WA-DIVPEN > ZEROS OR                                   08950000
                 WA-INTPEN > ZEROS OR                                   08960000
                 WA-AMTPEN > ZEROS                                      08970000
                 MOVE WA-S1DQ9FTC        TO REG-S1DQ9FTC                08980000
                 MOVE SPACES             TO S01-SIGNO-SDOINVE           08990000
                 MOVE ZEROS              TO S01-SDOINVE                 09000000
                 MOVE SPACES             TO S01-SIGNO-IMPOVTA           09010000
                 MOVE ZEROS              TO S01-IMPOVTA                 09020000
                                                                        09030000
                 IF WA-DIVPEN < ZEROS                                   09040000
                    MOVE '-'             TO S01-SIGNO-IMPDIVI           09050000
                 END-IF                                                 09060000
                                                                        09070000
                 MOVE WA-DIVPEN          TO S01-IMPDIVI                 09080000
                                                                        09090000
                 IF WA-INTPEN < ZEROS                                   09100000
                    MOVE '-'             TO S01-SIGNO-IMPINTE           09110000
                 END-IF                                                 09120000
                                                                        09130000
                 MOVE WA-INTPEN          TO S01-IMPINTE                 09140000
                                                                        09150000
                 IF WA-AMTPEN < ZEROS                                   09160000
                    MOVE '-'             TO S01-SIGNO-IMPVCTO           09170000
                 END-IF                                                 09180000
                                                                        09190000
                 MOVE WA-AMTPEN          TO S01-IMPVCTO                 09200000
                 MOVE 'PEN'              TO S01-DIVISA                  09210000
                PERFORM 20020-GRABA-SALIDA                              09220000
              END-IF                                                    09230000
           END-IF                                                       09240000
                                                                        09250000
           PERFORM 10010-LEER-ENTRADA                                   09260000
           .                                                            09270000
      *                                                                 09280000
      *     *---------------*                                           09290000
       20010-OBTIENE-CLIENTE.                                           09300000
      *     *---------------*                                           09310000
      *                                                                 09320000
           INITIALIZE           W520-REGISTRO                           09330000
      *                                                                 09340000
           MOVE E01-NUMCLI         TO W520-NUMCLIEN                     09350000
           MOVE SPACES             TO WSV-CLIENTE                       09360000
                                                                        09370000
           CALL PE9C5201 USING W-PEWC5201                               09380000
                                                                        09390000
           EVALUATE W520-PECRETOR                                       09400000
              WHEN ZEROS                                                09410000
                  IF W520-SUJGRUP = 'F'                                 09420000
                     STRING W520-PRIAPE DELIMITED BY '  ' ' '           09430000
                            W520-SEGAPE DELIMITED BY '  ' ' '           09440000
                            W520-NOMBRE DELIMITED BY '  '               09450000
                                              INTO WSV-CLIENTE          09460000
                  ELSE                                                  09470000
                     STRING W520-NOMBRE DELIMITED BY SIZE               09480000
                            W520-PRIAPE DELIMITED BY SIZE               09490000
                            W520-SEGAPE DELIMITED BY SIZE               09500000
                                              INTO WSV-CLIENTE          09510000
                  END-IF                                                09520000
              WHEN OTHER                                                09530000
                   MOVE '***NO UBICADO***'          TO  WSV-CLIENTE     09540000
           END-EVALUATE                                                 09550000
           .                                                            09560000
      *                                                                 09570000
      *     *------------*                                              09580000
       20020-GRABA-SALIDA.                                              09590000
      *     *------------*                                              09600000
      *                                                                 09610000
           WRITE REG-S1DQ9FTC FROM REG-S1DQ9FTC-W.                      09620000
                                                                        09630000
           IF (FS-S1DQ9FTC EQUAL '00')                                  09640000
               ADD 1                   TO WSV-ESCRITOS                  09650000
           ELSE                                                         09660000
              DISPLAY '*  ERROR EN GRABAR REGISTROS FS : ' FS-S1DQ9FTC  09670000
              DISPLAY '*  REGISTRO LEIDOS              : ' WSV-ESCRITOS 09680000
              MOVE '02'  TO RETURN-CODE                                 09690000
              STOP RUN                                                  09700000
           END-IF                                                       09710000
           .                                                            09720000
      *                                                                 09730000
      *     *-------------*                                             09740000
       20030-GENERA-SALDOS.                                             09750000
      *     *-------------*                                             09760000
      *                                                                *09770000
           MOVE ZEROS                 TO WA-SALDO-INVER                 09780000
           MOVE ZEROS                 TO WA-SALDO-INVER-0               09790000
           MOVE ZEROS                 TO WA-SALDO-VENTA                 09800000
           MOVE ZEROS                 TO WA-DIVPEN                      09810000
           MOVE ZEROS                 TO WA-INTPEN                      09820000
           MOVE ZEROS                 TO WA-AMTPEN                      09830000
           MOVE ZEROS                 TO WA-DIVUSD                      09840000
           MOVE ZEROS                 TO WA-INTUSD                      09850000
           MOVE ZEROS                 TO WA-AMTUSD                      09860000
      *                                                                *09870000
           PERFORM 20031-SALDO-INVERSION                                09880000
      *                                                                *09890000
           PERFORM 20032-NEGOC-VENTAS                                   09900000
      *                                                                *09910000
           PERFORM 20033-OPERAC-FINANCIERA                              09920000
      *                                                                *09930000
           PERFORM 20034-SALDO-CTAREG                                   09940000
      *                                                                *09950000
           PERFORM 20035-SALDO-CTAREG                                   09960000
      *                                                                 09970000
           PERFORM 20036-SELECT-VLDTSMO                                 09980000
           .                                                            09990000
      *                                                                 10000000
      *     *---------------*                                           10010000
       20031-SALDO-INVERSION.                                           10020000
      *     *---------------*                                           10030000
      *                                                                *10040000
           PERFORM UNTIL FS-E2DQ9ADS = '10'                             10050000
                      OR VADS-CUENTA > WA-CUENTA-ARC7                   10060000
              IF VADS-CUENTA = WA-CUENTA-ARC7                           10070000
                 COMPUTE WA-SALDO       = VADS-DEPOS  + VADS-COMPR      10080000
                                        + VADS-SUSCR  - VADS-VENTA      10090000
                                        - VADS-ORDVE  - VADS-BLOQ       10100000
                 MOVE VADS-PAVAL   TO VXEN-PAVAL                        10110000
                 MOVE VADS-VALOR   TO VXEN-VALOR                        10120000
                 MOVE VADS-ISIN    TO VXEN-ISIN                         10130000
                 PERFORM 20040-SELECT-VLDTXEN                           10140000
                 MOVE VADS-PAVAL   TO  VCAM-CODVALOR (01:03)            10150000
                 MOVE VADS-VALOR   TO  VCAM-CODVALOR (04:08)            10160000
                 MOVE VADS-ISIN    TO  VCAM-CODVALOR (12:01)            10170000
31-12            MOVE WPOL-FECFIN  TO  VCAM-FECDIA                      10180000
                 PERFORM 10030-PRECIO-VALOR                             10190000
                 IF VXEN-TIPINT = 'F'                                   10200000
                    COMPUTE WA-SALDO-INVER =  WA-SALDO-INVER            10210000
                                           + (WA-SALDO * VCAM-CIERRE-D  10220000
                                                       * VXEN-NOMINEM   10230000
                                                       / 100)           10240000
                 ELSE                                                   10250000
                    COMPUTE WA-SALDO-INVER =  WA-SALDO-INVER            10260000
                                           + (WA-SALDO * VCAM-CIERRE-D) 10270000
                 END-IF                                                 10280000
              END-IF                                                    10290000
              PERFORM 10020-LEER-SALDOS                                 10300000
           END-PERFORM.                                                 10310000
      *                                                                *10320000
           IF WA-SALDO-INVER = ZEROS                                    10330000
              PERFORM 20050-MAX-VLDTHIS                                 10340000
              PERFORM 20055-SDO-INVER-CERO                              10350000
           END-IF.                                                      10360000
      *                                                                *10370000
      *     *------------*                                              10380000
       20032-NEGOC-VENTAS.                                              10390000
      *     *------------*                                              10400000
      *                                                                *10410000
           MOVE WA-CUENTA-ARC7 TO  VPOL-CUENTA.                         10420000
           MOVE 'V'            TO  VPOL-COMVEN.                         10430000
           MOVE 'LC'           TO  VPOL-SITUAC.                         10440000
      *                                                                *10450000
           EXEC SQL                                                     10460000
                OPEN VLDCPOL                                            10470000
           END-EXEC                                                     10480000
      *                                                                 10490000
           MOVE SQLCODE TO SQLCODE-AUX                                  10500000
      *                                                                 10510000
           EVALUATE TRUE                                                10520000
               WHEN DB2-OK                                              10530000
                    CONTINUE                                            10540000
               WHEN OTHER                                               10550000
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      10560002
                    MOVE 'VLDTPOL'               TO  W801-TABLA         10570000
                    MOVE 'OPEN'                  TO  W801-ACCION        10580000
                    MOVE WX-CUENTA-ARC7          TO  W801-CLAVE (01:07) 10590000
                    MOVE SQLCODE                 TO  W801-SQLCODE       10600000
                    MOVE SPACES                  TO  W801-SQLWARN       10610000
                    MOVE '20032-NEGOC-VENTAS   ' TO  W801-PARRAFO       10620000
                    PERFORM VLPC8010-DISP-ABEND-DB2                     10630000
                    PERFORM VLPC8010-ABEND-DB2                          10640000
           END-EVALUATE.                                                10650000
      *                                                                 10660000
           PERFORM UNTIL DB2-NOTFND                                     10670000
      *                                                                 10680000
              EXEC SQL                                                  10690000
                   FETCH VLDCPOL                                        10700000
                    INTO :VPOL-EFECTI                                   10710000
                       , :VPOL-CUPCORR                                  10720000
                       , :VPOL-IMPCOM1                                  10730000
                       , :VPOL-IMPCOM2                                  10740000
                       , :VPOL-IMPCOM3                                  10750000
                       , :VPOL-IMPCOM4                                  10760000
                       , :VPOL-IMPCOM5                                  10770000
                       , :VPOL-IMPCOM6                                  10780000
                       , :VPOL-IMPCOM7                                  10790000
                       , :VPOL-IMPCOM8                                  10800000
                       , :VPOL-IGV                                      10810000
              END-EXEC                                                  10820000
                                                                        10830000
              MOVE SQLCODE TO SQLCODE-AUX                               10840000
                                                                        10850000
              EVALUATE TRUE                                             10860000
                  WHEN DB2-OK                                           10870000
                       COMPUTE WA-SALDO-VENTA = WA-SALDO-VENTA +        10880000
                               VPOL-EFECTI    + VPOL-CUPCORR   -        10890000
                               VPOL-IMPCOM1   - VPOL-IMPCOM2   -        10900000
                               VPOL-IMPCOM3   - VPOL-IMPCOM4   -        10910000
                               VPOL-IMPCOM5   - VPOL-IMPCOM6   -        10920000
                               VPOL-IMPCOM7   - VPOL-IMPCOM8   -        10930000
                               VPOL-IGV                                 10940000
                  WHEN DB2-NOTFND                                       10950000
                       CONTINUE                                         10960000
                  WHEN OTHER                                            10970000
                       MOVE 'VL4C9FTX'           TO  W801-PROGRAMA      10980002
                       MOVE 'VLDTPOL'            TO  W801-TABLA         10990000
                       MOVE 'FETCH'              TO  W801-ACCION        11000000
                       MOVE WX-CUENTA-ARC7       TO  W801-CLAVE (01:07) 11010000
                       MOVE SQLCODE              TO  W801-SQLCODE       11020000
                       MOVE SPACES               TO  W801-SQLWARN       11030000
                       MOVE '20032-NEGOC-VENTAS   ' TO W801-PARRAFO     11040000
                       PERFORM VLPC8010-DISP-ABEND-DB2                  11050000
                       PERFORM VLPC8010-ABEND-DB2                       11060000
              END-EVALUATE                                              11070000
           END-PERFORM.                                                 11080000
                                                                        11090000
           EXEC SQL                                                     11100000
                CLOSE VLDCPOL                                           11110000
           END-EXEC                                                     11120000
      *                                                                 11130000
           MOVE SQLCODE TO SQLCODE-AUX                                  11140000
      *                                                                 11150000
           EVALUATE TRUE                                                11160000
               WHEN DB2-OK                                              11170000
                    CONTINUE                                            11180000
               WHEN OTHER                                               11190000
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      11200002
                    MOVE 'VLDTPOL'               TO  W801-TABLA         11210000
                    MOVE 'CLOSE'                 TO  W801-ACCION        11220000
                    MOVE WX-CUENTA-ARC7          TO  W801-CLAVE (01:07) 11230000
                    MOVE SQLCODE                 TO  W801-SQLCODE       11240000
                    MOVE SPACES                  TO  W801-SQLWARN       11250000
                    MOVE '20032-NEGOC-VENTAS   ' TO  W801-PARRAFO       11260000
                    PERFORM VLPC8010-DISP-ABEND-DB2                     11270000
                    PERFORM VLPC8010-ABEND-DB2                          11280000
           END-EVALUATE.                                                11290000
      *                                                                *11300000
      *     *-----------------*                                         11310000
       20033-OPERAC-FINANCIERA.                                         11320000
      *     *-----------------*                                         11330000
      *                                                                *11340000
           MOVE WA-CUENTA-ARC7 TO  VHAC-CUENTA.                         11350000
           MOVE ZEROS          TO  WA-DIVPEN, WA-DIVUSD.                11360000
           MOVE ZEROS          TO  WA-INTPEN, WA-INTUSD.                11370000
           MOVE ZEROS          TO  WA-AMTPEN, WA-AMTUSD.                11380000
      *                                                                *11390000
           EXEC SQL                                                     11400000
                OPEN VLDCHAC                                            11410000
           END-EXEC                                                     11420000
      *                                                                 11430000
           MOVE SQLCODE TO SQLCODE-AUX                                  11440000
      *                                                                 11450000
           EVALUATE TRUE                                                11460000
               WHEN DB2-OK                                              11470000
                    CONTINUE                                            11480000
               WHEN OTHER                                               11490000
                    MOVE 'VL4C9FTX'                TO W801-PROGRAMA     11500002
                    MOVE 'VLDTHAC'                 TO W801-TABLA        11510000
                    MOVE 'OPEN'                    TO W801-ACCION       11520000
                    MOVE WX-CUENTA-ARC7            TO W801-CLAVE        11530000
                    MOVE SQLCODE                   TO W801-SQLCODE      11540000
                    MOVE SPACES                    TO W801-SQLWARN      11550000
                    MOVE '20033-OPERAC-FINANCIERA' TO W801-PARRAFO      11560000
                    PERFORM VLPC8010-DISP-ABEND-DB2                     11570000
                    PERFORM VLPC8010-ABEND-DB2                          11580000
           END-EVALUATE.                                                11590000
      *                                                                 11600000
           PERFORM UNTIL DB2-NOTFND                                     11610000
      *                                                                 11620000
              EXEC SQL                                                  11630000
                   FETCH VLDCHAC                                        11640000
                    INTO :VHAC-OPERAC                                   11650000
                       , :VHAC-IMPLIQ                                   11660000
                       , :VHAC-MONEDA-CTA                               11670000
              END-EXEC                                                  11680000
                                                                        11690000
              MOVE SQLCODE TO SQLCODE-AUX                               11700000
                                                                        11710000
              EVALUATE TRUE                                             11720000
                  WHEN DB2-OK                                           11730000
                       EVALUATE VHAC-MONEDA-CTA                         11740000
                           WHEN 'PEN'                                   11750000
                                EVALUATE VHAC-OPERAC                    11760000
                                    WHEN 07                             11770000
                                         ADD VHAC-IMPLIQ TO WA-DIVPEN   11780000
                                    WHEN 08                             11790000
                                         ADD VHAC-IMPLIQ TO WA-INTPEN   11800000
                                    WHEN 11                             11810000
                                         ADD VHAC-IMPLIQ TO WA-AMTPEN   11820000
                                END-EVALUATE                            11830000
                           WHEN 'USD'                                   11840000
                                EVALUATE VHAC-OPERAC                    11850000
                                    WHEN 07                             11860000
                                         ADD VHAC-IMPLIQ TO WA-DIVUSD   11870000
                                    WHEN 08                             11880000
                                         ADD VHAC-IMPLIQ TO WA-INTUSD   11890000
                                    WHEN 11                             11900000
                                         ADD VHAC-IMPLIQ TO WA-AMTUSD   11910000
                                END-EVALUATE                            11920000
                       END-EVALUATE                                     11930000
                  WHEN DB2-NOTFND                                       11940000
                       CONTINUE                                         11950000
                  WHEN OTHER                                            11960000
                       MOVE 'VL4C9FTX'                TO  W801-PROGRAMA 11970002
                       MOVE 'VLDTHAC'                 TO  W801-TABLA    11980000
                       MOVE 'FETCH'                   TO  W801-ACCION   11990000
                       MOVE WX-CUENTA-ARC7            TO  W801-CLAVE    12000000
                       MOVE SQLCODE                   TO  W801-SQLCODE  12010000
                       MOVE SPACES                    TO  W801-SQLWARN  12020000
                       MOVE '20033-OPERAC-FINANCIERA' TO W801-PARRAFO   12030000
                       PERFORM VLPC8010-DISP-ABEND-DB2                  12040000
                       PERFORM VLPC8010-ABEND-DB2                       12050000
              END-EVALUATE                                              12060000
           END-PERFORM.                                                 12070000
      *                                                                 12080000
           EXEC SQL                                                     12090000
                CLOSE VLDCHAC                                           12100000
           END-EXEC                                                     12110000
      *                                                                 12120000
           MOVE SQLCODE TO SQLCODE-AUX                                  12130000
      *                                                                 12140000
           EVALUATE TRUE                                                12150000
               WHEN DB2-OK                                              12160000
                    CONTINUE                                            12170000
               WHEN OTHER                                               12180000
                    MOVE 'VL4C9FTX'                TO  W801-PROGRAMA    12190002
                    MOVE 'VLDTHAC'                 TO  W801-TABLA       12200000
                    MOVE 'CLOSE'                   TO  W801-ACCION      12210000
                    MOVE WX-CUENTA-ARC7            TO  W801-CLAVE       12220000
                    MOVE SQLCODE                   TO  W801-SQLCODE     12230000
                    MOVE SPACES                    TO  W801-SQLWARN     12240000
                    MOVE '20033-OPERAC-FINANCIERA' TO  W801-PARRAFO     12250000
                    PERFORM VLPC8010-DISP-ABEND-DB2                     12260000
                    PERFORM VLPC8010-ABEND-DB2                          12270000
           END-EVALUATE.                                                12280000
      *                                                                *12290000
      *     *------------*                                              12300000
       20034-SALDO-CTAREG.                                              12310000
      *     *------------*                                              12320000
      *                                                                *12330000
           MOVE E01-CTAVAL20    TO  VSMM-CTAVAL                         12340000
AAAA       MOVE WSV-FECHA-HAS-A TO  VSMM-FECONTA (01:04)                12350000
           MOVE '-'             TO  VSMM-FECONTA (05:01)                12360000
12         MOVE WSV-FECHA-HAS-M TO  VSMM-FECONTA (06:02)                12370000
           MOVE '-'             TO  VSMM-FECONTA (08:01)                12380000
31         MOVE WSV-FECHA-HAS-D TO  VSMM-FECONTA (09:02)                12390000
           MOVE ZEROS           TO  VSMM-SALDO-AUT                      12400000
      *                                                                *12410000
           EXEC SQL                                                     12420000
                OPEN VLDCSMM                                            12430000
           END-EXEC                                                     12440000
      *                                                                 12450000
           MOVE SQLCODE TO SQLCODE-AUX                                  12460000
      *                                                                 12470000
           EVALUATE TRUE                                                12480000
               WHEN DB2-OK                                              12490000
                    CONTINUE                                            12500000
               WHEN OTHER                                               12510000
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      12520002
                    MOVE 'VLDTSMM'               TO  W801-TABLA         12530000
                    MOVE 'OPEN'                  TO  W801-ACCION        12540000
                    MOVE E01-CTAVAL20            TO  W801-CLAVE         12550000
                    MOVE SQLCODE                 TO  W801-SQLCODE       12560000
                    MOVE SPACES                  TO  W801-SQLWARN       12570000
                    MOVE '20034-SALDO-CTAREG   ' TO  W801-PARRAFO       12580000
                    PERFORM VLPC8010-DISP-ABEND-DB2                     12590000
                    PERFORM VLPC8010-ABEND-DB2                          12600000
           END-EVALUATE.                                                12610000
      *                                                                 12620000
           EXEC SQL                                                     12630000
                FETCH  VLDCSMM                                          12640000
                 INTO :VSMM-SALDO-AUT                                   12650000
                    , :VSMM-NUMREF                                      12660000
           END-EXEC                                                     12670000
                                                                        12680000
           MOVE  SQLCODE TO SQLCODE-AUX                                 12690000
                                                                        12700000
           EVALUATE TRUE                                                12710000
               WHEN DB2-OK                                              12720000
                    CONTINUE                                            12730000
               WHEN DB2-NOTFND                                          12740000
                    MOVE ZEROS                   TO  VSMM-SALDO-AUT     12750000
                    MOVE ZEROS                   TO  VSMM-NUMREF        12760000
                    MOVE '9999-12-31'            TO  VSMM-FECONTA       12770000
               WHEN OTHER                                               12780000
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      12790002
                    MOVE 'VLDTSMM'               TO  W801-TABLA         12800000
                    MOVE 'SELECT-SALDO'          TO  W801-ACCION        12810000
                    MOVE E01-CTAVAL20            TO  W801-CLAVE         12820000
                    MOVE SQLCODE                 TO  W801-SQLCODE       12830000
                    MOVE SPACES                  TO  W801-SQLWARN       12840000
                    MOVE '20034-SALDO-CTAREG   ' TO  W801-PARRAFO       12850000
                    PERFORM VLPC8010-DISP-ABEND-DB2                     12860000
                    PERFORM VLPC8010-ABEND-DB2                          12870000
           END-EVALUATE.                                                12880000
                                                                        12890000
           EXEC SQL                                                     12900000
                CLOSE VLDCSMM                                           12910000
           END-EXEC                                                     12920000
      *                                                                 12930000
           MOVE SQLCODE TO SQLCODE-AUX                                  12940000
      *                                                                 12950000
           EVALUATE TRUE                                                12960000
               WHEN DB2-OK                                              12970000
                    CONTINUE                                            12980000
               WHEN OTHER                                               12990000
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      13000002
                    MOVE 'VLDTSMM'               TO  W801-TABLA         13010000
                    MOVE 'CLOSE'                 TO  W801-ACCION        13020000
                    MOVE E01-CTAVAL20            TO  W801-CLAVE         13030000
                    MOVE SQLCODE                 TO  W801-SQLCODE       13040000
                    MOVE SPACES                  TO  W801-SQLWARN       13050000
                    MOVE '20034-SALDO-CTAREG   ' TO  W801-PARRAFO       13060000
                    PERFORM VLPC8010-DISP-ABEND-DB2                     13070000
                    PERFORM VLPC8010-ABEND-DB2                          13080000
           END-EVALUATE.                                                13090000
      *                                                                *13100000
      *     *------------*                                              13110000
       20035-SALDO-CTAREG.                                              13120000
      *     *------------*                                              13130000
      *                                                                *13140000
           MOVE E01-CTAVAL20    TO  VSMM-CTAVAL                         13150000
AAAA       MOVE WSV-FECHA-HAS-A TO  VSMM-FECONTA (01:04)                13160000
           MOVE '-'             TO  VSMM-FECONTA (05:01)                13170000
12         MOVE WSV-FECHA-HAS-M TO  VSMM-FECONTA (06:02)                13180000
           MOVE '-'             TO  VSMM-FECONTA (08:01)                13190000
31         MOVE WSV-FECHA-HAS-D TO  VSMM-FECONTA (09:02)                13200000
           MOVE ZEROS           TO  WSMM-SALDO-AUT                      13210000
      *                                                                *13220000
           EXEC SQL                                                     13230000
                OPEN VLDUSMM                                            13240000
           END-EXEC                                                     13250000
      *                                                                 13260000
           MOVE SQLCODE TO SQLCODE-AUX                                  13270000
      *                                                                 13280000
           EVALUATE TRUE                                                13290000
               WHEN DB2-OK                                              13300000
                    CONTINUE                                            13310000
               WHEN OTHER                                               13320000
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      13330002
                    MOVE 'VLDTSMM'               TO  W801-TABLA         13340000
                    MOVE 'OPEN'                  TO  W801-ACCION        13350000
                    MOVE E01-CTAVAL20            TO  W801-CLAVE         13360000
                    MOVE SQLCODE                 TO  W801-SQLCODE       13370000
                    MOVE SPACES                  TO  W801-SQLWARN       13380000
                    MOVE '20035-SALDO-CTAREG   ' TO  W801-PARRAFO       13390000
                    PERFORM VLPC8010-DISP-ABEND-DB2                     13400000
                    PERFORM VLPC8010-ABEND-DB2                          13410000
           END-EVALUATE.                                                13420000
      *                                                                 13430000
           EXEC SQL                                                     13440000
                FETCH  VLDUSMM                                          13450000
                 INTO :WSMM-SALDO-AUT                                   13460000
                    , :VSMM-FECONTA                                     13470000
           END-EXEC                                                     13480000
                                                                        13490000
           MOVE  SQLCODE TO SQLCODE-AUX                                 13500000
                                                                        13510000
           EVALUATE TRUE                                                13520000
               WHEN DB2-OK                                              13530000
                    CONTINUE                                            13540000
               WHEN DB2-NOTFND                                          13550000
                    MOVE ZEROS                   TO  WSMM-SALDO-AUT     13560000
                    MOVE '9999-12-31'            TO  VSMM-FECONTA       13570000
               WHEN OTHER                                               13580000
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      13590002
                    MOVE 'VLDTSMM'               TO  W801-TABLA         13600000
                    MOVE 'SELECT-SALDO'          TO  W801-ACCION        13610000
                    MOVE E01-CTAVAL20            TO  W801-CLAVE         13620000
                    MOVE SQLCODE                 TO  W801-SQLCODE       13630000
                    MOVE SPACES                  TO  W801-SQLWARN       13640000
                    MOVE '20035-SALDO-CTAREG   ' TO  W801-PARRAFO       13650000
                    PERFORM VLPC8010-DISP-ABEND-DB2                     13660000
                    PERFORM VLPC8010-ABEND-DB2                          13670000
           END-EVALUATE.                                                13680000
                                                                        13690000
           EXEC SQL                                                     13700000
                CLOSE VLDUSMM                                           13710000
           END-EXEC                                                     13720000
      *                                                                 13730000
           MOVE SQLCODE TO SQLCODE-AUX                                  13740000
      *                                                                 13750000
           EVALUATE TRUE                                                13760000
               WHEN DB2-OK                                              13770000
                    CONTINUE                                            13780000
               WHEN OTHER                                               13790000
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      13800002
                    MOVE 'VLDTSMM'               TO  W801-TABLA         13810000
                    MOVE 'CLOSE'                 TO  W801-ACCION        13820000
                    MOVE E01-CTAVAL20            TO  W801-CLAVE         13830000
                    MOVE SQLCODE                 TO  W801-SQLCODE       13840000
                    MOVE SPACES                  TO  W801-SQLWARN       13850000
                    MOVE '20035-SALDO-CTAREG   ' TO  W801-PARRAFO       13860000
                    PERFORM VLPC8010-DISP-ABEND-DB2                     13870000
                    PERFORM VLPC8010-ABEND-DB2                          13880000
           END-EVALUATE.                                                13890000
      *                                                                *13900000
      *     *--------------*                                            13910000
       20036-SELECT-VLDTSMO.                                            13920000
      *     *--------------*                                            13930000
      *                                                                *13940000
           MOVE E01-CTAVAL20    TO  VSMO-CTAVAL                         13950000
      *                                                                *13960000
           EXEC SQL                                                     13970000
                SELECT  VSMO_FEALTREG                                   13980000
                  INTO :VSMO-FEALTREG                                   13990000
                  FROM VLDTSMO                                          14000000
                  WHERE VSMO_CTAVAL    = :VSMO-CTAVAL                   14010000
           END-EXEC.                                                    14020000
      *                                                                 14030000
           EVALUATE SQLCODE                                             14040000
               WHEN ZEROS                                               14050000
                    CONTINUE                                            14060000
               WHEN 100                                                 14070000
                    MOVE '9999-12-31'           TO  VSMO-FEALTREG       14080000
               WHEN OTHER                                               14090000
                    MOVE 'VL4C9FTX'             TO  W801-PROGRAMA       14100002
                    MOVE 'VLDTSDO'              TO  W801-TABLA          14110000
                    MOVE 'SELECT'               TO  W801-ACCION         14120000
                    MOVE E01-CTAVAL20           TO  W801-CLAVE (01:07)  14130000
                    MOVE  SQLCODE               TO  W801-SQLCODE        14140000
                    MOVE  SPACES                TO  W801-SQLWARN        14150000
                    MOVE '20036-SELECT-VLDTSMO' TO  W801-PARRAFO        14160000
                    PERFORM  VLPC8010-DISP-ABEND-DB2                    14170000
                    PERFORM  VLPC8010-ABEND-DB2                         14180000
           END-EVALUATE.                                                14190000
      *                                                                *14200000
      *     *--------------*                                            14210000
       20040-SELECT-VLDTXEN.                                            14220000
      *     *--------------*                                            14230000
      *                                                                *14240000
           EXEC SQL                                                     14250000
                SELECT  VXEN_NOMINEM                                    14260000
                     ,  VXEN_SUSPDT                                     14270000
                     ,  VXEN_TIPINT                                     14280000
                     ,  VXEN_MINPOL                                     14290000
                     ,  VXEN_FPROXA                                     14300000
                     ,  VXEN_FPROXC                                     14310000
                     ,  VXEN_FILLER                                     14320000
                  INTO :VXEN-NOMINEM                                    14330000
                     , :VXEN-SUSPDT                                     14340000
                     , :VXEN-TIPINT                                     14350000
                     , :VXEN-MINPOL                                     14360000
                     , :VXEN-FPROXA                                     14370000
                     , :VXEN-FPROXC                                     14380000
                     , :VXEN-FILLER                                     14390000
                  FROM VLDTXEN                                          14400000
                  WHERE VXEN_PAVAL     = :VXEN-PAVAL                    14410000
                    AND VXEN_VALOR     = :VXEN-VALOR                    14420000
                    AND VXEN_ISIN      = :VXEN-ISIN                     14430000
           END-EXEC.                                                    14440000
      *                                                                 14450000
           EVALUATE SQLCODE                                             14460000
               WHEN ZEROS                                               14470000
                    CONTINUE                                            14480000
                    IF VXEN-SUSPDT > ZEROS                              14490001
                       MOVE VXEN-SUSPDT         TO WXEN-SUSPDT          14500001
                       MOVE WXEN-NOMITEMP       TO VXEN-NOMINEM         14510001
                    END-IF                                              14520001
               WHEN OTHER                                               14530000
                    MOVE 'VL4C9FTX'             TO  W801-PROGRAMA       14540002
                    MOVE 'VLDTXEN'              TO  W801-TABLA          14550000
                    MOVE 'SELECT'               TO  W801-ACCION         14560000
                    MOVE WX-CUENTA-ARC7         TO  W801-CLAVE (01:07)  14570000
                    MOVE VXEN-PAVAL             TO  W801-CLAVE (09:03)  14580000
                    MOVE VXEN-VALOR             TO  W801-CLAVE (12:07)  14590000
                    MOVE VXEN-ISIN              TO  W801-CLAVE (19:01)  14600000
                    MOVE  SQLCODE               TO  W801-SQLCODE        14610000
                    MOVE  SPACES                TO  W801-SQLWARN        14620000
                    MOVE '20040-SELECT-VLDTXEN' TO  W801-PARRAFO        14630000
                    PERFORM  VLPC8010-DISP-ABEND-DB2                    14640000
                    PERFORM  VLPC8010-ABEND-DB2                         14650000
           END-EVALUATE.                                                14660000
      *                                                                *14670000
      *     *-----------*                                               14680000
       20050-MAX-VLDTHIS.                                               14690000
      *     *-----------*                                               14700000
      *    BUSCA AÑO DE INFORMACION                                    *14710000
           MOVE WA-CUENTA-ARC7 TO  VHIS-CTAVAL.                         14720000
           MOVE LK-F-AA-H      TO  WHIS-ANO.                            14730000
           MOVE ZEROS          TO  VHIS-ANO.                            14740000
           MOVE ZEROS          TO  VHIS-MES.                            14750000
      *                                                                *14760000
           EXEC SQL                                                     14770000
                SELECT  MAX(VHIS_ANO)                                   14780000
                  INTO     :VHIS-ANO                                    14790000
                  FROM VLDTHIS                                          14800000
                 WHERE VHIS_CTAVAL    = :VHIS-CTAVAL                    14810000
                   AND VHIS_ANO      <= :WHIS-ANO                       14820000
                   AND VHIS_TIPGAS   IN (48, 49)                        14830000
           END-EXEC.                                                    14840000
      *                                                                 14850000
           EVALUATE SQLCODE                                             14860000
               WHEN ZEROS                                               14870000
                    CONTINUE                                            14880000
               WHEN -305                                                14890000
                    MOVE ZEROS                  TO  VHIS-ANO            14900000
                    MOVE ZEROS                  TO  VHIS-MES            14910000
               WHEN OTHER                                               14920000
                    MOVE 'VL4C9FTX'             TO  W801-PROGRAMA       14930002
                    MOVE 'VLDTHIS'              TO  W801-TABLA          14940000
                    MOVE 'MAX-AÑO'              TO  W801-ACCION         14950000
                    MOVE WX-CUENTA-ARC7         TO  W801-CLAVE (01:07)  14960000
                    MOVE  SQLCODE               TO  W801-SQLCODE        14970000
                    MOVE  SPACES                TO  W801-SQLWARN        14980000
                    MOVE '20050-MAX-VLDTHIS'    TO  W801-PARRAFO        14990000
                    PERFORM  VLPC8010-DISP-ABEND-DB2                    15000000
                    PERFORM  VLPC8010-ABEND-DB2                         15010000
           END-EVALUATE.                                                15020000
      *    BUSCA MES DE INFORMACION                                    *15030000
           MOVE WA-CUENTA-ARC7 TO  VHIS-CTAVAL.                         15040000
           MOVE ZEROS          TO  VHIS-MES.                            15050000
      *                                                                *15060000
           EXEC SQL                                                     15070000
                SELECT  MAX(VHIS_MES)                                   15080000
                  INTO     :VHIS-MES                                    15090000
                  FROM VLDTHIS                                          15100000
                 WHERE VHIS_CTAVAL = :VHIS-CTAVAL                       15110000
                   AND VHIS_ANO    = :VHIS-ANO                          15120000
                   AND VHIS_TIPGAS IN (48, 49)                          15130000
           END-EXEC.                                                    15140000
      *                                                                 15150000
           EVALUATE SQLCODE                                             15160000
               WHEN ZEROS                                               15170000
                    CONTINUE                                            15180000
               WHEN -305                                                15190000
                    MOVE ZEROS                  TO  VHIS-ANO            15200000
                    MOVE ZEROS                  TO  VHIS-MES            15210000
               WHEN OTHER                                               15220000
                    MOVE 'VL4C9FTX'             TO  W801-PROGRAMA       15230002
                    MOVE 'VLDTHIS'              TO  W801-TABLA          15240000
                    MOVE 'MAX-MES'              TO  W801-ACCION         15250000
                    MOVE WX-CUENTA-ARC7         TO  W801-CLAVE (01:07)  15260000
                    MOVE  SQLCODE               TO  W801-SQLCODE        15270000
                    MOVE  SPACES                TO  W801-SQLWARN        15280000
                    MOVE '20050-MAX-VLDTHIS'    TO  W801-PARRAFO        15290000
                    PERFORM  VLPC8010-DISP-ABEND-DB2                    15300000
                    PERFORM  VLPC8010-ABEND-DB2                         15310000
           END-EVALUATE.                                                15320000
      *                                                                *15330000
      *     *--------------*                                            15340000
       20055-SDO-INVER-CERO.                                            15350000
      *     *--------------*                                           *15360000
      *                                                                *15370000
           MOVE WA-CUENTA-ARC7 TO VHIS-CTAVAL.                          15380000
           MOVE ZEROS          TO WHIS-FEC1RA-N.                        15390000
           MOVE SPACES         TO VHIS-CODVALOR.                        15400000
      *                                                                *15410000
           EXEC SQL                                                     15420000
                OPEN VLDUHIS                                            15430000
           END-EXEC                                                     15440000
      *                                                                 15450000
           MOVE SQLCODE TO SQLCODE-AUX                                  15460000
      *                                                                 15470000
           EVALUATE TRUE                                                15480000
               WHEN DB2-OK                                              15490000
                    CONTINUE                                            15500000
               WHEN OTHER                                               15510000
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      15520002
                    MOVE 'VLDTHIS'               TO  W801-TABLA         15530000
                    MOVE 'OPEN'                  TO  W801-ACCION        15540000
                    MOVE E01-CTAVAL20            TO  W801-CLAVE         15550000
                    MOVE SQLCODE                 TO  W801-SQLCODE       15560000
                    MOVE SPACES                  TO  W801-SQLWARN       15570000
                    MOVE '20055-SDO-INVER-CERO ' TO  W801-PARRAFO       15580000
                    PERFORM VLPC8010-DISP-ABEND-DB2                     15590000
                    PERFORM VLPC8010-ABEND-DB2                          15600000
           END-EVALUATE.                                                15610000
      *                                                                 15620000
           PERFORM UNTIL SQLCODE = 100                                  15630000
              EXEC SQL                                                  15640000
                   FETCH  VLDUHIS                                       15650000
                    INTO :VHIS-CODVALOR                                 15660000
                       , :VHIS-TITULOS1                                 15670000
                       , :VHIS-MOVIMI1                                  15680000
                       , :VHIS-CUSTODIA1                                15690000
                       , :VHIS-CAMBIO1                                  15700000
                       , :VHIS-COBRADO1                                 15710000
                       , :VHIS-TITULOS2                                 15720000
                       , :VHIS-MOVIMI2                                  15730000
                       , :VHIS-CUSTODIA2                                15740000
                       , :VHIS-CAMBIO2                                  15750000
                       , :VHIS-COBRADO2                                 15760000
                       , :VHIS-TITULOS3                                 15770000
                       , :VHIS-MOVIMI3                                  15780000
                       , :VHIS-CUSTODIA3                                15790000
                       , :VHIS-CAMBIO3                                  15800000
                       , :VHIS-COBRADO3                                 15810000
                       , :VHIS-TITULOS4                                 15820000
                       , :VHIS-MOVIMI4                                  15830000
                       , :VHIS-CUSTODIA4                                15840000
                       , :VHIS-CAMBIO4                                  15850000
                       , :VHIS-COBRADO4                                 15860000
                       , :VHIS-TITULOS5                                 15870000
                       , :VHIS-MOVIMI5                                  15880000
                       , :VHIS-CUSTODIA5                                15890000
                       , :VHIS-CAMBIO5                                  15900000
                       , :VHIS-COBRADO5                                 15910000
                       , :VHIS-TITULOS6                                 15920000
                       , :VHIS-MOVIMI6                                  15930000
                       , :VHIS-CUSTODIA6                                15940000
                       , :VHIS-CAMBIO6                                  15950000
                       , :VHIS-COBRADO6                                 15960000
                       , :VHIS-TITULOS7                                 15970000
                       , :VHIS-MOVIMI7                                  15980000
                       , :VHIS-CUSTODIA7                                15990000
                       , :VHIS-CAMBIO7                                  16000000
                       , :VHIS-COBRADO7                                 16010000
                       , :VHIS-TITULOS8                                 16020000
                       , :VHIS-MOVIMI8                                  16030000
                       , :VHIS-CUSTODIA8                                16040000
                       , :VHIS-CAMBIO8                                  16050000
                       , :VHIS-COBRADO8                                 16060000
                       , :VHIS-TITULOS9                                 16070000
                       , :VHIS-MOVIMI9                                  16080000
                       , :VHIS-CUSTODIA9                                16090000
                       , :VHIS-CAMBIO9                                  16100000
                       , :VHIS-COBRADO9                                 16110000
                       , :VHIS-TITULOS10                                16120000
                       , :VHIS-MOVIMI10                                 16130000
                       , :VHIS-CUSTODIA10                               16140000
                       , :VHIS-CAMBIO10                                 16150000
                       , :VHIS-COBRADO10                                16160000
                       , :VHIS-TITULOS11                                16170000
                       , :VHIS-MOVIMI11                                 16180000
                       , :VHIS-CUSTODIA11                               16190000
                       , :VHIS-CAMBIO11                                 16200000
                       , :VHIS-COBRADO11                                16210000
                       , :VHIS-TITULOS12                                16220000
                       , :VHIS-MOVIMI12                                 16230000
                       , :VHIS-CUSTODIA12                               16240000
                       , :VHIS-CAMBIO12                                 16250000
                       , :VHIS-COBRADO12                                16260000
                       , :VHIS-TITULOS13                                16270000
                       , :VHIS-MOVIMI13                                 16280000
                       , :VHIS-CUSTODIA13                               16290000
                       , :VHIS-CAMBIO13                                 16300000
                       , :VHIS-COBRADO13                                16310000
                       , :VHIS-TITULOS14                                16320000
                       , :VHIS-MOVIMI14                                 16330000
                       , :VHIS-CUSTODIA14                               16340000
                       , :VHIS-CAMBIO14                                 16350000
                       , :VHIS-COBRADO14                                16360000
                       , :VHIS-TITULOS15                                16370000
                       , :VHIS-MOVIMI15                                 16380000
                       , :VHIS-CUSTODIA15                               16390000
                       , :VHIS-CAMBIO15                                 16400000
                       , :VHIS-COBRADO15                                16410000
                       , :VHIS-TITULOS16                                16420000
                       , :VHIS-MOVIMI16                                 16430000
                       , :VHIS-CUSTODIA16                               16440000
                       , :VHIS-CAMBIO16                                 16450000
                       , :VHIS-COBRADO16                                16460000
                       , :VHIS-TITULOS17                                16470000
                       , :VHIS-MOVIMI17                                 16480000
                       , :VHIS-CUSTODIA17                               16490000
                       , :VHIS-CAMBIO17                                 16500000
                       , :VHIS-COBRADO17                                16510000
                       , :VHIS-TITULOS18                                16520000
                       , :VHIS-MOVIMI18                                 16530000
                       , :VHIS-CUSTODIA18                               16540000
                       , :VHIS-CAMBIO18                                 16550000
                       , :VHIS-COBRADO18                                16560000
                       , :VHIS-TITULOS19                                16570000
                       , :VHIS-MOVIMI19                                 16580000
                       , :VHIS-CUSTODIA19                               16590000
                       , :VHIS-CAMBIO19                                 16600000
                       , :VHIS-COBRADO19                                16610000
                       , :VHIS-TITULOS20                                16620000
                       , :VHIS-MOVIMI20                                 16630000
                       , :VHIS-CUSTODIA20                               16640000
                       , :VHIS-CAMBIO20                                 16650000
                       , :VHIS-COBRADO20                                16660000
                       , :VHIS-TITULOS21                                16670000
                       , :VHIS-MOVIMI21                                 16680000
                       , :VHIS-CUSTODIA21                               16690000
                       , :VHIS-CAMBIO21                                 16700000
                       , :VHIS-COBRADO21                                16710000
                       , :VHIS-TITULOS22                                16720000
                       , :VHIS-MOVIMI22                                 16730000
                       , :VHIS-CUSTODIA22                               16740000
                       , :VHIS-CAMBIO22                                 16750000
                       , :VHIS-COBRADO22                                16760000
                       , :VHIS-TITULOS23                                16770000
                       , :VHIS-MOVIMI23                                 16780000
                       , :VHIS-CUSTODIA23                               16790000
                       , :VHIS-CAMBIO23                                 16800000
                       , :VHIS-COBRADO23                                16810000
                       , :VHIS-TITULOS24                                16820000
                       , :VHIS-MOVIMI24                                 16830000
                       , :VHIS-CUSTODIA24                               16840000
                       , :VHIS-CAMBIO24                                 16850000
                       , :VHIS-COBRADO24                                16860000
                       , :VHIS-TITULOS25                                16870000
                       , :VHIS-MOVIMI25                                 16880000
                       , :VHIS-CUSTODIA25                               16890000
                       , :VHIS-CAMBIO25                                 16900000
                       , :VHIS-COBRADO25                                16910000
                       , :VHIS-TITULOS26                                16920000
                       , :VHIS-MOVIMI26                                 16930000
                       , :VHIS-CUSTODIA26                               16940000
                       , :VHIS-CAMBIO26                                 16950000
                       , :VHIS-COBRADO26                                16960000
                       , :VHIS-TITULOS27                                16970000
                       , :VHIS-MOVIMI27                                 16980000
                       , :VHIS-CUSTODIA27                               16990000
                       , :VHIS-CAMBIO27                                 17000000
                       , :VHIS-COBRADO27                                17010000
                       , :VHIS-TITULOS28                                17020000
                       , :VHIS-MOVIMI28                                 17030000
                       , :VHIS-CUSTODIA28                               17040000
                       , :VHIS-CAMBIO28                                 17050000
                       , :VHIS-COBRADO28                                17060000
                       , :VHIS-TITULOS29                                17070000
                       , :VHIS-MOVIMI29                                 17080000
                       , :VHIS-CUSTODIA29                               17090000
                       , :VHIS-CAMBIO29                                 17100000
                       , :VHIS-COBRADO29                                17110000
                       , :VHIS-TITULOS30                                17120000
                       , :VHIS-MOVIMI30                                 17130000
                       , :VHIS-CUSTODIA30                               17140000
                       , :VHIS-CAMBIO30                                 17150000
                       , :VHIS-COBRADO30                                17160000
                       , :VHIS-TITULOS31                                17170000
                       , :VHIS-MOVIMI31                                 17180000
                       , :VHIS-CUSTODIA31                               17190000
                       , :VHIS-CAMBIO31                                 17200000
                       , :VHIS-COBRADO31                                17210000
                       , :VHIS-FEALTREG                                 17220000
                       , :VHIS-FEULMOD                                  17230000
                       , :VHIS-HORULMOD                                 17240000
                       , :VHIS-NUMTER                                   17250000
                       , :VHIS-USUARIO                                  17260000
              END-EXEC                                                  17270000
                                                                        17280000
              MOVE  SQLCODE TO SQLCODE-AUX                              17290000
                                                                        17300000
              EVALUATE TRUE                                             17310000
                  WHEN DB2-OK                                           17320000
                       MOVE DCLVLDTHIS            TO DCLVLTCHIS         17330000
                       MOVE ZEROS          TO  WH-SALD0                 17340000
                       MOVE ZEROS          TO  WH-NOMINEM               17350000
                       PERFORM VARYING WI FROM 31 BY -1                 17360000
                                 UNTIL WI    = ZEROS                    17370000
                                    OR WH-SALD0 > ZEROS                 17380000
                          IF CVHIS-TITULOS (WI) > ZEROS                 17390000
                             MOVE CVHIS-TITULOS (WI) TO WH-SALD0        17400000
                             MOVE CVHIS-CAMBIO  (WI) TO WH-NOMINEM      17410000
                             MOVE WI                 TO WHIS-FECHIS-D   17420000
                          END-IF                                        17430000
                       END-PERFORM                                      17440000
                       MOVE VHIS-CODVALOR (01:03) TO VXEN-PAVAL         17450000
                       MOVE VHIS-CODVALOR (04:08) TO VXEN-VALOR         17460000
                       MOVE VHIS-CODVALOR (12:01) TO VXEN-ISIN          17470000
                       PERFORM 20040-SELECT-VLDTXEN                     17480000
                       MOVE VHIS-CODVALOR         TO VCAM-CODVALOR      17490000
                       MOVE VHIS-ANO              TO WHIS-FECHIS-A      17500000
                       MOVE VHIS-MES              TO WHIS-FECHIS-M      17510000
                       MOVE WHIS-FECHIS-N         TO VCAM-FECDIA        17520000
                       IF WHIS-FECHIS-N > ZEROS AND                     17530000
                          WHIS-FEC1RA-N = ZEROS                         17540000
                          MOVE WHIS-FECHIS-N      TO WHIS-FEC1RA-N      17550000
                       END-IF                                           17560000
                       PERFORM 10030-PRECIO-VALOR                       17570000
                       IF VXEN-TIPINT = 'F'                             17580000
                          COMPUTE WA-SALDO-INVER-0   = WA-SALDO-INVER-0 17590000
                                         + (WH-SALD0 * VCAM-CIERRE-D    17600000
                                                     * WH-NOMINEM       17610000
                                                     / 100)             17620000
                       ELSE                                             17630000
                          COMPUTE WA-SALDO-INVER-0   = WA-SALDO-INVER-0 17640000
                                         + (WH-SALD0 * VCAM-CIERRE-D)   17650000
                       END-IF                                           17660000
                  WHEN DB2-NOTFND                                       17670000
                       CONTINUE                                         17680000
                  WHEN OTHER                                            17690000
                       MOVE 'VL4C9FTX'              TO  W801-PROGRAMA   17700002
                       MOVE 'VLDTHIS'               TO  W801-TABLA      17710000
                       MOVE 'FETCH       '          TO  W801-ACCION     17720000
                       MOVE E01-CTAVAL20            TO  W801-CLAVE      17730000
                       MOVE SQLCODE                 TO  W801-SQLCODE    17740000
                       MOVE SPACES                  TO  W801-SQLWARN    17750000
                       MOVE '20055-SDO-INVER-CERO ' TO  W801-PARRAFO    17760000
                       PERFORM VLPC8010-DISP-ABEND-DB2                  17770000
                       PERFORM VLPC8010-ABEND-DB2                       17780000
              END-EVALUATE                                              17790000
           END-PERFORM.                                                 17800000
      *                                                                 17810000
           EXEC SQL                                                     17820000
                CLOSE VLDUHIS                                           17830000
           END-EXEC                                                     17840000
      *                                                                 17850000
           MOVE SQLCODE TO SQLCODE-AUX                                  17860000
      *                                                                 17870000
           EVALUATE TRUE                                                17880000
               WHEN DB2-OK                                              17890000
                    CONTINUE                                            17900000
               WHEN OTHER                                               17910000
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      17920002
                    MOVE 'VLDTHIS'               TO  W801-TABLA         17930000
                    MOVE 'CLOSE'                 TO  W801-ACCION        17940000
                    MOVE E01-CTAVAL20            TO  W801-CLAVE         17950000
                    MOVE SQLCODE                 TO  W801-SQLCODE       17960000
                    MOVE SPACES                  TO  W801-SQLWARN       17970000
                    MOVE '20055-SDO-INVER-CERO ' TO  W801-PARRAFO       17980000
                    PERFORM VLPC8010-DISP-ABEND-DB2                     17990000
                    PERFORM VLPC8010-ABEND-DB2                          18000000
           END-EVALUATE.                                                18010000
      *                                                                *18020000
      *     *--------------*                                            18030000
      ******************************************************************18040000
      *                   30000-FIN                                    *18050000
      ******************************************************************18060000
      *---------*                                                       18070000
       30000-FIN.                                                       18080000
      *---------*                                                       18090000
      *                                                                 18100000
           DISPLAY 'READ  S1DQ9FTC.... : ' WSV-LEIDOS.                  18110000
           DISPLAY 'WRITE E1DQ9FTC.... : ' WSV-ESCRITOS.                18120000
           STOP RUN                                                     18130000
           .                                                            18140000
      *                                                                *18150000
      *    *-----------------*                                          18160000
       3100-DISP-TOTALIMETROS.                                          18170000
      *    *-----------------*                                          18180000
           DISPLAY '*************************************************'. 18190000
           DISPLAY '********    T O T A L I M E T R O S   ***********'. 18200000
           DISPLAY '********   D E L     P R O G R A M A  ***********'. 18210000
           DISPLAY '********           VL4C9FTX           ***********'. 18220002
           DISPLAY '*************************************************'. 18230000
           DISPLAY '*************************************************'. 18240000
      *                                                                *18250000
      *    *---------------------*                                      18260000
       3100-DISP-TOTALIMETROS-FIN.                                      18270000
      *    *---------------------*                                      18280000
           EXIT.                                                        18290000
      ******************************************************************18300000
      **                  COPYS DE ERRORES DE PROCEDURE               **18310000
      ******************************************************************18320000
      *                                                                *18330000
            COPY  QRWCDB20.                                             18340000
            COPY  VLPC8010.                                             18350000
            COPY  VLPC8020.                                             18360000
            COPY  VLPCRUTI.                                             18370000
      *                                                                *18380000
      *------------------*                                              18390000
      *99999-FIN-PROGRAMA*                                              18400000
      *------------------*                                              18410000
