      *-----------------------*                                         00010016
       IDENTIFICATION DIVISION.                                         00020016
      *-----------------------*                                         00030016
       PROGRAM-ID.   VL4C9FTZ.                                          00040016
      *AUTHOR.       BBVA PERU.                                         00050016
      ******************************************************************00060016
      *REF.PETIC FECHA-MOD. PROGRAMADOR      DESCRIPCION               *00070016
      *--------- ---------- ---------------- --------------------------*00080016
FVAXX *FVA-XX    26-08-2020 EULER ALVARADO   VALIDA SITUA. CTA Y SALDO *00090016
      *                                      CONSIDERA FECHAS DE LINKAG*00100016
      *--------- ---------- ---------------- --------------------------*00110016
      ******************************************************************00120016
       ENVIRONMENT DIVISION.                                            00130016
       CONFIGURATION SECTION.                                           00140016
       SOURCE-COMPUTER. IBM-390.                                        00150016
       OBJECT-COMPUTER. IBM-390.                                        00160016
       SPECIAL-NAMES.                                                   00170016
       INPUT-OUTPUT SECTION.                                            00180016
      *------------*                                                    00190016
       FILE-CONTROL.                                                    00200016
      *------------*                                                    00210016
            SELECT E1DQ9FTC ASSIGN TO E1DQ9FTC                          00220016
                   FILE STATUS IS FS-E1DQ9FTC                           00230016
                   ORGANIZATION IS SEQUENTIAL.                          00240016
                                                                        00250016
            SELECT E2DQ9ADS ASSIGN TO E2DQ9ADS                          00260016
                   FILE STATUS IS FS-E2DQ9ADS                           00270016
                   ORGANIZATION IS SEQUENTIAL.                          00280016
                                                                        00290016
            SELECT S1DQ9FTC ASSIGN TO S1DQ9FTC                          00300016
                   FILE STATUS IS FS-S1DQ9FTC                           00310016
                   ORGANIZATION IS SEQUENTIAL.                          00320016
      *-----------------------------------------------------------------00330016
      *-------------*                                                   00340016
       DATA DIVISION.                                                   00350016
      *-------------*                                                   00360016
       FILE SECTION.                                                    00370016
                                                                        00380016
       FD  E1DQ9FTC                                                     00390016
           RECORDING MODE IS F                                          00400016
           BLOCK CONTAINS 0 RECORDS                                     00410016
           DATA RECORD IS REG-E1DQ9FTC.                                 00420016
       01  REG-E1DQ9FTC.                                                00430016
           10 E01-CTAVAL20         PIC X(20).                           00440016
           10 E01-FILLER1          PIC X(01).                           00450016
           10 E01-MONEDA           PIC X(03).                           00460016
           10 E01-FILLER2          PIC X(01).                           00470016
           10 E01-NUMCLI           PIC 9(08).                           00480016
           10 E01-FILLER3          PIC X(01).                           00490016
           10 E01-CLIENTE          PIC X(60).                           00500016
           10 E01-FILLER4          PIC X(01).                           00510016
           10 E01-SITUACION        PIC X(09).                           00520016
           10 E01-FILLER5          PIC X(01).                           00530016
           10 E01-FECALTA          PIC X(10).                           00540016
           10 E01-FILLER6          PIC X(01).                           00550016
           10 E01-FECCESE          PIC X(10).                           00560016
           10 E01-FILLER7          PIC X(01).                           00570016
           10 E01-RUT              PIC 9(08).                           00580016
                                                                        00590016
       FD  E2DQ9ADS                                                     00600016
           RECORDING MODE IS F                                          00610016
           BLOCK CONTAINS 0 RECORDS                                     00620016
           DATA RECORD IS REG-E2DQ9ADS.                                 00630016
       01  REG-E2DQ9ADS            PIC X(354).                          00640016
                                                                        00650016
       FD  S1DQ9FTC                                                     00660016
           RECORDING MODE IS F                                          00670016
           BLOCK CONTAINS 0 RECORDS                                     00680016
           DATA RECORD IS REG-S1DQ9FTC.                                 00690016
       01  REG-S1DQ9FTC            PIC X(214).                          00700016
      *                                                                 00710016
      *-----------------------------------------------------------------00720016
       WORKING-STORAGE SECTION.                                         00730016
      *-----------------------*                                         00740016
       77  WS-NAME                 PIC X(70) VALUE                      00750016
                                   '**  INICIO WORKING VL4C9MAE **'.    00760016
      ******************************************************************00770016
      *                                                                 00780016
       77  WI                      PIC 9(02) VALUE ZEROS.               00790016
       77  WHIS-ANO                PIC S9(4)V USAGE COMP-3.             00800016
       77  WHIS-MES                PIC S9(4)V USAGE COMP-3.             00810016
       77  WPOL-FECINI             PIC S9(8)V USAGE COMP-3.             00820016
       77  WPOL-FECFIN             PIC S9(8)V USAGE COMP-3.             00830016
       77  WHAC-FECINI             PIC S9(8)V USAGE COMP-3.             00840016
       77  WHAC-FECFIN             PIC S9(8)V USAGE COMP-3.             00850016
       77  WA-APERTURA             PIC  X(40) VALUE                     00860016
                             'APERTURA DE CUENTA                      '.00870016
       77  WA-SALIDA               PIC  X(40) VALUE                     00880016
                             'TRASPASO CTA-REGISTRO A OPERATIVA S.A.B.'.00890016
       77  WA-ENTRADA              PIC  X(40) VALUE                     00900016
                             'TRASPASO CTA-OPERATIVA S.A.B. A REGISTRO'.00910016
      *                                                                 00920016
       01  WS-VARIOS.                                                   00930016
           02  WX-CUENTA-ARC7.                                          00940016
               04  WN-CUENTA-ARC7  PIC  9(7).                           00950016
           02  WA-CUENTA-ARC7      PIC S9(7)V USAGE COMP-3.             00960016
           02  WSV-FECHA-DES.                                           00970016
               04 WSV-FECHA-DES-A      PIC X(04).                       00980016
               04 WSV-FECHA-DES-M      PIC X(02).                       00990016
               04 WSV-FECHA-DES-D      PIC X(02).                       01000016
           02  WSV-FECHA-DES-N REDEFINES WSV-FECHA-DES PIC 9(08).       01010016
           02  WSV-FECHA-HAS.                                           01020016
               04 WSV-FECHA-HAS-A      PIC X(04).                       01030016
               04 WSV-FECHA-HAS-M      PIC X(02).                       01040016
               04 WSV-FECHA-HAS-D      PIC X(02).                       01050016
           02  WSV-FECHA-HAS-N REDEFINES WSV-FECHA-HAS PIC 9(08).       01060016
           02  WSMM-SALDO-AUT          PIC S9(13)V9(02) USAGE COMP-3.   01070016
           02  WHIS-FECHIS-X.                                           01080016
               04  WHIS-FECHIS-A       PIC 9(04).                       01090016
               04  WHIS-FECHIS-M       PIC 9(02).                       01100016
               04  WHIS-FECHIS-D       PIC 9(02).                       01110016
           02  WHIS-FECHIS-N REDEFINES WHIS-FECHIS-X PIC 9(08).         01120016
      *                                                                *01130016
           02  WHIS-FEC1RA-X.                                           01140016
               04  WHIS-FEC1RA-A       PIC 9(04).                       01150016
               04  WHIS-FEC1RA-M       PIC 9(02).                       01160016
               04  WHIS-FEC1RA-D       PIC 9(02).                       01170016
           02  WHIS-FEC1RA-N REDEFINES WHIS-FEC1RA-X PIC 9(08).         01180016
      *                                                                *01190016
       01  WTMP-NOM-SUS.                                                01200016
           02 WXEN-SUSPDT                 PIC S9(08)V9(10) USAGE COMP-3.01210016
           02 WXEN-NOMITEMP REDEFINES WXEN-SUSPDT                       01220016
                                          PIC S9(12)V9(06) USAGE COMP-3.01230016
      *                                                                *01240016
       01  PE9C5201                PIC X(08) VALUE 'PE9C5201'.          01250016
RTP0   01  PE9C5000                PIC X(08) VALUE 'PE9C5000'.          01260016
       01  FILE-STATUS.                                                 01270016
           10 FS-E1DQ9FTC          PIC X(02) VALUE SPACES.              01280016
           10 FS-E2DQ9ADS          PIC X(02) VALUE SPACES.              01290016
           10 FS-S1DQ9FTC          PIC X(02) VALUE SPACES.              01300016
       01  WSV-CLIENTE             PIC X(60) VALUE SPACES.              01310016
       01  WSV-FECHA-10-A          PIC X(10) VALUE SPACES.              01320016
       01  WSV-FECHA-8-N           PIC 9(08) VALUE ZEROS.               01330016
       01  WSV-FECHA-8-A REDEFINES WSV-FECHA-8-N PIC X(08).             01340016
       01  WSV-LEIDOS              PIC 9(08) VALUE ZEROS.               01350016
       01  WSV-ESCRITOS            PIC 9(08) VALUE ZEROS.               01360016
       01  WSV-FECHA-PRO.                                               01370016
           02 WSV-FECHA-PRO-A      PIC X(04).                           01380016
           02 WSV-FECHA-PRO-M      PIC X(02).                           01390016
           02 WSV-FECHA-PRO-D      PIC X(02).                           01400016
       01  WSN-FECHA-PRO-N REDEFINES WSV-FECHA-PRO PIC 9(08).           01410016
      *                                                                *01420016
       01  WR-NEGLOT.                                                   01430016
           02  WA-TIPNEG             PIC  X(01)    VALUE 'L'.           01440016
           02  WA-NEGLOT             PIC S9(07)V   USAGE COMP-3.        01450016
      *                                                                *01460016
       01  REG-S1DQ9FTC-W.                                              01470016
           10 S01-NUMCLI           PIC X(08).                           01480016
           10 S01-TIPDOC           PIC X(01).                           01490016
           10 S01-NRODOC           PIC X(11).                           01500016
           10 S01-CTAVAL20         PIC X(18).                           01510016
           10 S01-MONCONTR         PIC X(03).                           01520016
           10 S01-FECALTA          PIC X(08).                           01530016
           10 S01-FECCESE          PIC X(08).                           01540016
           10 S01-SIGNO-SDOREGI    PIC X(01).                           01550016
           10 S01-SDOREGI          PIC 9(12)V9(02).                     01560016
           10 S01-SIGNO-SDOINVE    PIC X(01).                           01570016
           10 S01-SDOINVE          PIC 9(12)V9(02).                     01580016
           10 S01-SIGNO-IMPOVTA    PIC X(01).                           01590016
           10 S01-IMPOVTA          PIC 9(12)V9(02).                     01600016
           10 S01-SIGNO-IMPINTE    PIC X(01).                           01610016
           10 S01-IMPINTE          PIC 9(12)V9(02).                     01620016
           10 S01-SIGNO-IMPDIVI    PIC X(01).                           01630016
           10 S01-IMPDIVI          PIC 9(12)V9(02).                     01640016
           10 S01-SIGNO-IMPVCTO    PIC X(01).                           01650016
           10 S01-IMPVCTO          PIC 9(12)V9(02).                     01660016
           10 S01-DIVISA           PIC X(03).                           01670016
           10 S01-RUT              PIC 9(08).                           01680016
           10 S01-IND-CTAREG       PIC X(02).                           01690016
           10 S01-SIGNO-SDOREGULTI PIC X(01).                           01700016
           10 S01-SDOREGULTI       PIC 9(12)V9(02).                     01710016
           10 S01-FCHREGULTI       PIC X(08).                           01720016
           10 S01-FEALTREG         PIC X(08).                           01730016
           10 S01-SIGNO-ULTINVE    PIC X(01).                           01740016
           10 S01-ULTINVE          PIC 9(12)V9(02).                     01750016
           10 S01-FHULINVE         PIC X(08).                           01760016
      *                                                                *01770016
       01  WA-S1DQ9FTC.                                                 01780016
           10 W01-NUMCLI           PIC X(08).                           01790016
           10 W01-TIPDOC           PIC X(01).                           01800016
           10 W01-NRODOC           PIC X(11).                           01810016
           10 W01-CTAVAL20         PIC X(18).                           01820016
           10 W01-MONCONTR         PIC X(03).                           01830016
           10 W01-FECALTA          PIC X(08).                           01840016
           10 W01-FECCESE          PIC X(08).                           01850016
           10 W01-SIGNO-SDOREGI    PIC X(01).                           01860016
           10 W01-SDOREGI          PIC 9(12)V9(02).                     01870016
           10 W01-SIGNO-SDOINVE    PIC X(01).                           01880016
           10 W01-SDOINVE          PIC 9(12)V9(02).                     01890016
           10 W01-SIGNO-IMPOVTA    PIC X(01).                           01900016
           10 W01-IMPOVTA          PIC 9(12)V9(02).                     01910016
           10 W01-SIGNO-IMPINTE    PIC X(01).                           01920016
           10 W01-IMPINTE          PIC 9(12)V9(02).                     01930016
           10 W01-SIGNO-IMPDIVI    PIC X(01).                           01940016
           10 W01-IMPDIVI          PIC 9(12)V9(02).                     01950016
           10 W01-SIGNO-IMPVCTO    PIC X(01).                           01960016
           10 W01-IMPVCTO          PIC 9(12)V9(02).                     01970016
           10 W01-DIVISA           PIC X(03).                           01980016
           10 W01-RUT              PIC 9(08).                           01990016
           10 W01-IND-CTAREG       PIC X(02).                           02000016
           10 W01-SIGNO-SDOREGULTI PIC X(01).                           02010016
           10 W01-SDOREGULTI       PIC 9(12)V9(02).                     02020016
           10 W01-FCHREGULTI       PIC X(08).                           02030016
           10 W01-FEALTREG         PIC X(08).                           02040016
           10 W01-SIGNO-ULTINVE    PIC X(01).                           02050016
           10 W01-ULTINVE          PIC 9(12)V9(02).                     02060016
           10 W01-FHULINVE         PIC X(08).                           02070016
      *                                                                *02080016
       01  WA-VAR-SALDOS.                                               02090016
           02 WA-SALDO                PIC  9(15).                       02100016
           02 WH-SALD0                PIC S9(15).                       02110016
           02 WH-NOMINEM              PIC  9(13)V9(05).                 02120016
           02 WA-SALDO-INVER          PIC  9(15)V9(02).                 02130016
           02 WA-SALDO-INVER-0        PIC  9(15)V9(02).                 02140016
           02 WA-SALDO-VENTA          PIC  9(15)V9(02).                 02150016
           02 WA-DIVPEN               PIC  9(15)V9(02).                 02160016
           02 WA-INTPEN               PIC  9(15)V9(02).                 02170016
           02 WA-AMTPEN               PIC  9(15)V9(02).                 02180016
           02 WA-DIVUSD               PIC  9(15)V9(02).                 02190016
           02 WA-INTUSD               PIC  9(15)V9(02).                 02200016
           02 WA-AMTUSD               PIC  9(15)V9(02).                 02210016
      *                                                                *02220016
      *    BD PERSONAS                                                  02230016
       01  W-PEWC5201.                                                  02240016
           COPY PEWC5201.                                               02250016
RTP0   01 PEWC5000.                                                     02260016
RTP0       COPY PEWC5000.                                               02270016
      *    TAB-VLDTHIS : COPY CON OCCURS                                02280016
           COPY VLTCHI2.                                                02290016
      *                                                                *02300016
      ******************************************************************02310016
      ********* COPYS DE ERRORES                             ***********02320016
      ******************************************************************02330016
      *                                                                *02340016
           COPY QRECDB2.                                                02350016
           COPY VLWCRUTI.                                               02360016
           COPY VLWC8000.                                               02370016
           COPY VLWC8010.                                               02380016
           COPY VLWC8020.                                               02390016
      *                                                                *02400016
      ******************************************************************02410016
      ********* INCLUDE DE TABLAS                            ***********02420016
      ******************************************************************02430016
      *                                                                *02440016
           EXEC SQL INCLUDE SQLCA   END-EXEC.                           02450016
           EXEC SQL INCLUDE VLGTARC END-EXEC.                           02460016
           EXEC SQL INCLUDE VLGTPOL END-EXEC.                           02470016
           EXEC SQL INCLUDE VLGTXEN END-EXEC.                           02480016
           EXEC SQL INCLUDE VLGTCAM END-EXEC.                           02490016
           EXEC SQL INCLUDE VLGTHAC END-EXEC.                           02500016
           EXEC SQL INCLUDE VLGTADS END-EXEC.                           02510016
N*         EXEC SQL INCLUDE VLGTSMO END-EXEC.                           02520016
N          EXEC SQL INCLUDE VLGTSMM END-EXEC.                           02530016
N          EXEC SQL INCLUDE VLGTHIS END-EXEC.                           02540016
      *                                                                *02550016
      *----------------------------------------------------------------*02560016
      *  AREA CURSORES DB2                                             *02570016
      *----------------------------------------------------------------*02580016
      *    COTIZACIONES DE INVERSIONES                                 *02590016
           EXEC SQL                                                     02600016
                DECLARE VLDCCAM  CURSOR FOR                             02610016
                 SELECT VCAM_CIERRE_D                                   02620016
                      , VCAM_FECDIA                                     02630016
                      , VCAM_FILLER                                     02640016
                   FROM VLDTCAM                                         02650016
                  WHERE VCAM_CODVALOR  = :VCAM-CODVALOR                 02660016
                    AND VCAM_FECDIA   <= :VCAM-FECDIA                   02670016
                    AND VCAM_CIERRE_D <>  0                             02680016
                  ORDER BY VCAM_FECDIA DESC                             02690016
                  OPTIMIZE FOR 1 ROW                                    02700016
           END-EXEC.                                                    02710016
      *    OPERACIONES CONTABILIZADAS                                  *02720016
           EXEC SQL                                                     02730016
                DECLARE VLDCHAC  CURSOR FOR                             02740016
                 SELECT VHAC_OPERAC                                     02750016
                      , VHAC_IMPLIQ                                     02760016
                      , VHAC_MONEDA_CTA                                 02770016
                   FROM VLDTHAC                                         02780016
                  WHERE VHAC_CUENTA = :VHAC-CUENTA                      02790016
                    AND VHAC_OPERAC IN (07, 08, 11)                     02800016
                    AND VHAC_FCONTA BETWEEN :WHAC-FECINI AND            02810016
                                            :WHAC-FECFIN                02820016
           END-EXEC.                                                    02830016
      *    POLIZAS                                                      02840016
           EXEC SQL                                                     02850016
                DECLARE VLDCPOL  CURSOR FOR                             02860016
                 SELECT VPOL_EFECTI                                     02870016
                      , VPOL_CUPCORR                                    02880016
                      , VPOL_IMPCOM1                                    02890016
                      , VPOL_IMPCOM2                                    02900016
                      , VPOL_IMPCOM3                                    02910016
                      , VPOL_IMPCOM4                                    02920016
                      , VPOL_IMPCOM5                                    02930016
                      , VPOL_IMPCOM6                                    02940016
                      , VPOL_IMPCOM7                                    02950016
                      , VPOL_IMPCOM8                                    02960016
                      , VPOL_IGV                                        02970016
                   FROM VLDTPOL                                         02980016
                  WHERE VPOL_CUENTA  = :VPOL-CUENTA                     02990016
                    AND VPOL_COMVEN  = :VPOL-COMVEN                     03000016
                    AND VPOL_SITUAC  = :VPOL-SITUAC                     03010016
                    AND VPOL_FECHEJ  BETWEEN :WPOL-FECINI AND           03020016
                                             :WPOL-FECFIN               03030016
           END-EXEC.                                                    03040016
      *    MOV-CTA-REGISTRO ULTIMO SALDO DEL AÑO                        03050016
           EXEC SQL                                                     03060016
                DECLARE VLDCSMM  CURSOR FOR                             03070016
                 SELECT VSMM_SALDO_AUT                                  03080016
                      , VSMM_NUMREF                                     03090016
                   FROM VLDTSMM                                         03100016
                  WHERE VSMM_CTAVAL    = :VSMM-CTAVAL                   03110016
                    AND VSMM_FECONTA  <= :VSMM-FECONTA                  03120016
                    AND VSMM_OBSERVA NOT IN (:WA-APERTURA, :WA-ENTRADA, 03130016
                                             :WA-SALIDA)                03140016
                  ORDER BY VSMM_NUMREF DESC                             03150016
                  OPTIMIZE FOR 1 ROW                                    03160016
           END-EXEC.                                                    03170016
      *    MOV-CTA-REGISTRO ULTIMO SALDO                                03180016
           EXEC SQL                                                     03190016
                DECLARE VLDUSMM  CURSOR FOR                             03200016
                 SELECT VSMM_SALDO_AUT                                  03210016
                      , VSMM_FECONTA                                    03220016
                   FROM VLDTSMM                                         03230016
                  WHERE VSMM_CTAVAL     = :VSMM-CTAVAL                  03240016
                    AND VSMM_FECONTA   <= :VSMM-FECONTA                 03250016
                    AND VSMM_SALDO_AUT <> 0                             03260016
                    AND VSMM_OBSERVA NOT IN (:WA-APERTURA, :WA-ENTRADA, 03270016
                                             :WA-SALIDA)                03280016
                  ORDER BY VSMM_NUMREF DESC                             03290016
                  OPTIMIZE FOR 1 ROW                                    03300016
           END-EXEC.                                                    03310016
      *    HIS-SDO-MENSUAL- ULTIMO SALDO TITULOS                        03320016
           EXEC SQL                                                     03330016
                DECLARE VLDUHIS  CURSOR FOR                             03340016
                 SELECT VHIS_CODVALOR                                   03350016
                      , VHIS_TITULOS1                                   03360016
                      , VHIS_MOVIMI1                                    03370016
                      , VHIS_CUSTODIA1                                  03380016
                      , VHIS_CAMBIO1                                    03390016
                      , VHIS_COBRADO1                                   03400016
                      , VHIS_TITULOS2                                   03410016
                      , VHIS_MOVIMI2                                    03420016
                      , VHIS_CUSTODIA2                                  03430016
                      , VHIS_CAMBIO2                                    03440016
                      , VHIS_COBRADO2                                   03450016
                      , VHIS_TITULOS3                                   03460016
                      , VHIS_MOVIMI3                                    03470016
                      , VHIS_CUSTODIA3                                  03480016
                      , VHIS_CAMBIO3                                    03490016
                      , VHIS_COBRADO3                                   03500016
                      , VHIS_TITULOS4                                   03510016
                      , VHIS_MOVIMI4                                    03520016
                      , VHIS_CUSTODIA4                                  03530016
                      , VHIS_CAMBIO4                                    03540016
                      , VHIS_COBRADO4                                   03550016
                      , VHIS_TITULOS5                                   03560016
                      , VHIS_MOVIMI5                                    03570016
                      , VHIS_CUSTODIA5                                  03580016
                      , VHIS_CAMBIO5                                    03590016
                      , VHIS_COBRADO5                                   03600016
                      , VHIS_TITULOS6                                   03610016
                      , VHIS_MOVIMI6                                    03620016
                      , VHIS_CUSTODIA6                                  03630016
                      , VHIS_CAMBIO6                                    03640016
                      , VHIS_COBRADO6                                   03650016
                      , VHIS_TITULOS7                                   03660016
                      , VHIS_MOVIMI7                                    03670016
                      , VHIS_CUSTODIA7                                  03680016
                      , VHIS_CAMBIO7                                    03690016
                      , VHIS_COBRADO7                                   03700016
                      , VHIS_TITULOS8                                   03710016
                      , VHIS_MOVIMI8                                    03720016
                      , VHIS_CUSTODIA8                                  03730016
                      , VHIS_CAMBIO8                                    03740016
                      , VHIS_COBRADO8                                   03750016
                      , VHIS_TITULOS9                                   03760016
                      , VHIS_MOVIMI9                                    03770016
                      , VHIS_CUSTODIA9                                  03780016
                      , VHIS_CAMBIO9                                    03790016
                      , VHIS_COBRADO9                                   03800016
                      , VHIS_TITULOS10                                  03810016
                      , VHIS_MOVIMI10                                   03820016
                      , VHIS_CUSTODIA10                                 03830016
                      , VHIS_CAMBIO10                                   03840016
                      , VHIS_COBRADO10                                  03850016
                      , VHIS_TITULOS11                                  03860016
                      , VHIS_MOVIMI11                                   03870016
                      , VHIS_CUSTODIA11                                 03880016
                      , VHIS_CAMBIO11                                   03890016
                      , VHIS_COBRADO11                                  03900016
                      , VHIS_TITULOS12                                  03910016
                      , VHIS_MOVIMI12                                   03920016
                      , VHIS_CUSTODIA12                                 03930016
                      , VHIS_CAMBIO12                                   03940016
                      , VHIS_COBRADO12                                  03950016
                      , VHIS_TITULOS13                                  03960016
                      , VHIS_MOVIMI13                                   03970016
                      , VHIS_CUSTODIA13                                 03980016
                      , VHIS_CAMBIO13                                   03990016
                      , VHIS_COBRADO13                                  04000016
                      , VHIS_TITULOS14                                  04010016
                      , VHIS_MOVIMI14                                   04020016
                      , VHIS_CUSTODIA14                                 04030016
                      , VHIS_CAMBIO14                                   04040016
                      , VHIS_COBRADO14                                  04050016
                      , VHIS_TITULOS15                                  04060016
                      , VHIS_MOVIMI15                                   04070016
                      , VHIS_CUSTODIA15                                 04080016
                      , VHIS_CAMBIO15                                   04090016
                      , VHIS_COBRADO15                                  04100016
                      , VHIS_TITULOS16                                  04110016
                      , VHIS_MOVIMI16                                   04120016
                      , VHIS_CUSTODIA16                                 04130016
                      , VHIS_CAMBIO16                                   04140016
                      , VHIS_COBRADO16                                  04150016
                      , VHIS_TITULOS17                                  04160016
                      , VHIS_MOVIMI17                                   04170016
                      , VHIS_CUSTODIA17                                 04180016
                      , VHIS_CAMBIO17                                   04190016
                      , VHIS_COBRADO17                                  04200016
                      , VHIS_TITULOS18                                  04210016
                      , VHIS_MOVIMI18                                   04220016
                      , VHIS_CUSTODIA18                                 04230016
                      , VHIS_CAMBIO18                                   04240016
                      , VHIS_COBRADO18                                  04250016
                      , VHIS_TITULOS19                                  04260016
                      , VHIS_MOVIMI19                                   04270016
                      , VHIS_CUSTODIA19                                 04280016
                      , VHIS_CAMBIO19                                   04290016
                      , VHIS_COBRADO19                                  04300016
                      , VHIS_TITULOS20                                  04310016
                      , VHIS_MOVIMI20                                   04320016
                      , VHIS_CUSTODIA20                                 04330016
                      , VHIS_CAMBIO20                                   04340016
                      , VHIS_COBRADO20                                  04350016
                      , VHIS_TITULOS21                                  04360016
                      , VHIS_MOVIMI21                                   04370016
                      , VHIS_CUSTODIA21                                 04380016
                      , VHIS_CAMBIO21                                   04390016
                      , VHIS_COBRADO21                                  04400016
                      , VHIS_TITULOS22                                  04410016
                      , VHIS_MOVIMI22                                   04420016
                      , VHIS_CUSTODIA22                                 04430016
                      , VHIS_CAMBIO22                                   04440016
                      , VHIS_COBRADO22                                  04450016
                      , VHIS_TITULOS23                                  04460016
                      , VHIS_MOVIMI23                                   04470016
                      , VHIS_CUSTODIA23                                 04480016
                      , VHIS_CAMBIO23                                   04490016
                      , VHIS_COBRADO23                                  04500016
                      , VHIS_TITULOS24                                  04510016
                      , VHIS_MOVIMI24                                   04520016
                      , VHIS_CUSTODIA24                                 04530016
                      , VHIS_CAMBIO24                                   04540016
                      , VHIS_COBRADO24                                  04550016
                      , VHIS_TITULOS25                                  04560016
                      , VHIS_MOVIMI25                                   04570016
                      , VHIS_CUSTODIA25                                 04580016
                      , VHIS_CAMBIO25                                   04590016
                      , VHIS_COBRADO25                                  04600016
                      , VHIS_TITULOS26                                  04610016
                      , VHIS_MOVIMI26                                   04620016
                      , VHIS_CUSTODIA26                                 04630016
                      , VHIS_CAMBIO26                                   04640016
                      , VHIS_COBRADO26                                  04650016
                      , VHIS_TITULOS27                                  04660016
                      , VHIS_MOVIMI27                                   04670016
                      , VHIS_CUSTODIA27                                 04680016
                      , VHIS_CAMBIO27                                   04690016
                      , VHIS_COBRADO27                                  04700016
                      , VHIS_TITULOS28                                  04710016
                      , VHIS_MOVIMI28                                   04720016
                      , VHIS_CUSTODIA28                                 04730016
                      , VHIS_CAMBIO28                                   04740016
                      , VHIS_COBRADO28                                  04750016
                      , VHIS_TITULOS29                                  04760016
                      , VHIS_MOVIMI29                                   04770016
                      , VHIS_CUSTODIA29                                 04780016
                      , VHIS_CAMBIO29                                   04790016
                      , VHIS_COBRADO29                                  04800016
                      , VHIS_TITULOS30                                  04810016
                      , VHIS_MOVIMI30                                   04820016
                      , VHIS_CUSTODIA30                                 04830016
                      , VHIS_CAMBIO30                                   04840016
                      , VHIS_COBRADO30                                  04850016
                      , VHIS_TITULOS31                                  04860016
                      , VHIS_MOVIMI31                                   04870016
                      , VHIS_CUSTODIA31                                 04880016
                      , VHIS_CAMBIO31                                   04890016
                      , VHIS_COBRADO31                                  04900016
                      , VHIS_FEALTREG                                   04910016
                      , VHIS_FEULMOD                                    04920016
                      , VHIS_HORULMOD                                   04930016
                      , VHIS_NUMTER                                     04940016
                      , VHIS_USUARIO                                    04950016
                   FROM VLDTHIS                                         04960016
                  WHERE VHIS_CTAVAL   = :VHIS-CTAVAL                    04970016
                    AND VHIS_TIPGAS  IN (48, 49)                        04980016
                    AND VHIS_ANO      = :VHIS-ANO                       04990016
                    AND VHIS_MES      = :VHIS-MES                       05000016
                    AND VHIS_CODVALOR > :VHIS-CODVALOR                  05010016
           END-EXEC.                                                    05020016
      *                                                                *05030016
      *---------------*                                                 05040016
       LINKAGE SECTION.                                                 05050016
      *---------------*                                                 05060016
       01  LK-PARAMETROS.                                               05070016
           02  LK-LONGITUD     PIC S9(4)   COMP.                        05080016
           02  LK-FECHA-D.                                              05090016
               03  LK-F-AA-D   PIC 9999.                                05100016
               03  LK-F-MM-D   PIC 99.                                  05110016
               03  LK-F-DD-D   PIC 99.                                  05120016
           02  LK-RFECHA-D  REDEFINES LK-FECHA-D PIC 9(08).             05130016
           02  LK-FECHA-H.                                              05140016
               03  LK-F-AA-H   PIC 9999.                                05150016
               03  LK-F-MM-H   PIC 99.                                  05160016
               03  LK-F-DD-H   PIC 99.                                  05170016
           02  LK-RFECHA-H  REDEFINES LK-FECHA-H PIC 9(08).             05180016
      *                                                                 05190016
      *---------------------------------------*                         05200016
       PROCEDURE DIVISION USING LK-PARAMETROS.                          05210016
      *---------------------------------------*                         05220016
      *                                                                 05230016
           PERFORM 10000-INICIO.                                        05240016
      *                                                                 05250016
           PERFORM 20000-PROCESO UNTIL FS-E1DQ9FTC = '10'.              05260016
      *                                                                 05270016
           PERFORM 30000-FIN.                                           05280016
      *                                                                 05290016
           STOP RUN.                                                    05300016
      *                                                                 05310016
      ******************************************************************05320016
      *                       1-INICIO                                 *05330016
      *       INICIALIZA LAS WORKAS DE LAS TABLAS Y LOS CAMPOS DE      *05340016
      *       TRABAJO. LEE LA FECHA EN LA TABLA UGDTPRC, TOMANDO LA    *05350016
      *       FILA CON CODIGO DE PROCESO = 'UB00' Y COMPRUEBA QUE      *05360016
      *       EL PROCESO ESTA ACTIVO.                                  *05370016
      ******************************************************************05380016
       10000-INICIO.                                                    05390016
      *-------------*                                                   05400016
      *                                                                 05410016
PAVXX *    MOVE    LK-RFECHA       TO WSV-FECHA-PRO.                    05420016
PAVXX *    MOVE    WSN-FECHA-PRO-N TO WSV-FECHA-DES-N, WSV-FECHA-HAS-N. 05430016
      *    MOVE    01              TO WSV-FECHA-DES-D, WSV-FECHA-DES-M. 05440016
           MOVE  LK-RFECHA-H        TO WSV-FECHA-PRO                    05450016
           MOVE  LK-RFECHA-D        TO WSV-FECHA-DES                    05460016
           MOVE  LK-RFECHA-H        TO WSV-FECHA-HAS                    05470016
           MOVE  WSV-FECHA-DES-N    TO WPOL-FECINI, WHAC-FECINI         05480016
           MOVE  WSV-FECHA-HAS-N    TO WPOL-FECFIN, WHAC-FECFIN         05490016
      *                                                                *05500016
           OPEN INPUT  E1DQ9FTC, E2DQ9ADS                               05510016
                OUTPUT S1DQ9FTC.                                        05520016
      *                                                                *05530016
           IF (FS-E1DQ9FTC EQUAL '00' OR '97')                          05540016
              CONTINUE                                                  05550016
           ELSE                                                         05560016
              DISPLAY '***********************************'             05570016
              DISPLAY '*  ERROR AL OPEN DE ENTRADA1      *'             05580016
              DISPLAY '*  ERROR FS-OPS ES :' FS-E1DQ9FTC                05590016
              DISPLAY '***********************************'             05600016
              MOVE '02'  TO RETURN-CODE                                 05610016
              STOP RUN                                                  05620016
           END-IF                                                       05630016
      *                                                                *05640016
           IF (FS-E2DQ9ADS EQUAL '00' OR '97')                          05650016
              CONTINUE                                                  05660016
           ELSE                                                         05670016
              DISPLAY '***********************************'             05680016
              DISPLAY '*  ERROR AL OPEN DE ENTRADA1      *'             05690016
              DISPLAY '*  ERROR FS-OPS ES :' FS-E2DQ9ADS                05700016
              DISPLAY '***********************************'             05710016
              MOVE '02'  TO RETURN-CODE                                 05720016
              STOP RUN                                                  05730016
           END-IF                                                       05740016
      *                                                                *05750016
           IF (FS-S1DQ9FTC EQUAL '00' OR '97')                          05760016
              CONTINUE                                                  05770016
           ELSE                                                         05780016
              DISPLAY '***********************************'             05790016
              DISPLAY '*  ERROR AL OPEN DE SALIDA1       *'             05800016
              DISPLAY '*  ERROR FS-OPS ES :' FS-S1DQ9FTC                05810016
              DISPLAY '***********************************'             05820016
              MOVE '02'  TO RETURN-CODE                                 05830016
              STOP RUN                                                  05840016
           END-IF.                                                      05850016
      *                                                                *05860016
           MOVE ZERO TO VADS-CUENTA.                                    05870016
      *                                                                *05880016
           PERFORM 10010-LEER-ENTRADA.                                  05890016
      *                                                                *05900016
           PERFORM 10020-LEER-SALDOS.                                   05910016
      *                                                                *05920016
      *     *------------*                                              05930016
       10010-LEER-ENTRADA.                                              05940016
      *     *------------*                                              05950016
      *                                                                 05960016
           READ E1DQ9FTC.                                               05970016
                                                                        05980016
           EVALUATE FS-E1DQ9FTC                                         05990016
              WHEN '00'                                                 06000016
                   ADD  1                      TO WSV-LEIDOS            06010016
                   MOVE E01-CTAVAL20 (13:07)   TO WX-CUENTA-ARC7        06020016
                   MOVE WN-CUENTA-ARC7         TO WA-CUENTA-ARC7        06030016
              WHEN '10'                                                 06040016
                   CONTINUE                                             06050016
              WHEN OTHER                                                06060016
                   DISPLAY '***********************************'        06070016
                   DISPLAY '*  ERROR AL LEER ENTRADA          *'        06080016
                   DISPLAY '*  ERROR FS-OPS ES :' FS-E1DQ9FTC           06090016
                   DISPLAY '***********************************'        06100016
                   MOVE '02'  TO RETURN-CODE                            06110016
                   STOP RUN                                             06120016
           END-EVALUATE                                                 06130016
           .                                                            06140016
      *                                                                 06150016
      *     *-----------*                                               06160016
       10020-LEER-SALDOS.                                               06170016
      *     *-----------*                                               06180016
      *                                                                 06190016
           READ E2DQ9ADS.                                               06200016
                                                                        06210016
           EVALUATE FS-E2DQ9ADS                                         06220016
              WHEN '00'                                                 06230016
                   MOVE REG-E2DQ9ADS           TO DCLVLDTADS            06240016
              WHEN '10'                                                 06250016
                   CONTINUE                                             06260016
              WHEN OTHER                                                06270016
                   DISPLAY '***********************************'        06280016
                   DISPLAY '*  ERROR AL LEER ENTRADA SALDOS   *'        06290016
                   DISPLAY '*  ERROR FS-ADS ES :' FS-E2DQ9ADS           06300016
                   DISPLAY '***********************************'        06310016
                   MOVE '02'  TO RETURN-CODE                            06320016
                   STOP RUN                                             06330016
           END-EVALUATE.                                                06340016
      *                                                                *06350016
      *     *------------*                                              06360016
       10030-PRECIO-VALOR.                                              06370016
      *     *------------*                                              06380016
      *                                                                *06390016
      *                                                                *06400016
           EXEC SQL                                                     06410016
                OPEN VLDCCAM                                            06420016
           END-EXEC                                                     06430016
      *                                                                 06440016
           MOVE SQLCODE TO SQLCODE-AUX                                  06450016
      *                                                                 06460016
           EVALUATE TRUE                                                06470016
               WHEN DB2-OK                                              06480016
                    CONTINUE                                            06490016
               WHEN OTHER                                               06500016
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      06510016
                    MOVE 'VLDTCAM'               TO  W801-TABLA         06520016
                    MOVE 'OPEN'                  TO  W801-ACCION        06530016
                    MOVE WX-CUENTA-ARC7          TO  W801-CLAVE (01:07) 06540016
                    MOVE VCAM-CODVALOR           TO  W801-CLAVE (09:12) 06550016
                    MOVE SQLCODE                 TO  W801-SQLCODE       06560016
                    MOVE SPACES                  TO  W801-SQLWARN       06570016
                    MOVE '10030-PRECIO-VALOR   ' TO  W801-PARRAFO       06580016
                    PERFORM VLPC8010-DISP-ABEND-DB2                     06590016
                    PERFORM VLPC8010-ABEND-DB2                          06600016
           END-EVALUATE.                                                06610016
      *                                                                 06620016
           EXEC SQL                                                     06630016
                FETCH  VLDCCAM                                          06640016
                 INTO :VCAM-CIERRE-D                                    06650016
                    , :VCAM-FECDIA                                      06660016
                    , :VCAM-FILLER                                      06670016
           END-EXEC                                                     06680016
                                                                        06690016
           MOVE  SQLCODE TO SQLCODE-AUX                                 06700016
                                                                        06710016
           EVALUATE TRUE                                                06720016
               WHEN DB2-OK                                              06730016
                    MOVE VCAM-FILLER (01:05) TO WR-NEGLOT               06740016
                    IF WA-TIPNEG = 'L' AND                              06750016
                       WA-NEGLOT >     ZEROS                            06760016
                       COMPUTE VCAM-CIERRE-D = VCAM-CIERRE-D / WA-NEGLOT06770016
                    END-IF                                              06780016
               WHEN DB2-NOTFND                                          06790016
                    IF VXEN-TIPINT = 'F'                                06800016
100%                   MOVE 100.00               TO  VCAM-CIERRE-D      06810016
                    ELSE                                                06820016
                       MOVE VXEN-NOMINEM         TO  VCAM-CIERRE-D      06830016
                    END-IF                                              06840016
               WHEN OTHER                                               06850016
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      06860016
                    MOVE 'VLDTCAM'               TO  W801-TABLA         06870016
                    MOVE 'SELECT-FIRST'          TO  W801-ACCION        06880016
                    MOVE WX-CUENTA-ARC7          TO  W801-CLAVE (01:07) 06890016
                    MOVE VCAM-CODVALOR           TO  W801-CLAVE (09:12) 06900016
                    MOVE SQLCODE                 TO  W801-SQLCODE       06910016
                    MOVE SPACES                  TO  W801-SQLWARN       06920016
                    MOVE '10030-PRECIO-VALOR   ' TO  W801-PARRAFO       06930016
                    PERFORM VLPC8010-DISP-ABEND-DB2                     06940016
                    PERFORM VLPC8010-ABEND-DB2                          06950016
           END-EVALUATE.                                                06960016
                                                                        06970016
           EXEC SQL                                                     06980016
                CLOSE VLDCCAM                                           06990016
           END-EXEC                                                     07000016
      *                                                                 07010016
           MOVE SQLCODE TO SQLCODE-AUX                                  07020016
      *                                                                 07030016
           EVALUATE TRUE                                                07040016
               WHEN DB2-OK                                              07050016
                    CONTINUE                                            07060016
               WHEN OTHER                                               07070016
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      07080016
                    MOVE 'VLDTCAM'               TO  W801-TABLA         07090016
                    MOVE 'CLOSE'                 TO  W801-ACCION        07100016
                    MOVE WX-CUENTA-ARC7          TO  W801-CLAVE (01:07) 07110016
                    MOVE VCAM-CODVALOR           TO  W801-CLAVE (09:12) 07120016
                    MOVE SQLCODE                 TO  W801-SQLCODE       07130016
                    MOVE SPACES                  TO  W801-SQLWARN       07140016
                    MOVE '10030-PRECIO-VALOR   ' TO  W801-PARRAFO       07150016
                    PERFORM VLPC8010-DISP-ABEND-DB2                     07160016
                    PERFORM VLPC8010-ABEND-DB2                          07170016
           END-EVALUATE.                                                07180016
      *                                                                *07190016
      *     *-------*                                                   07200016
       20000-PROCESO.                                                   07210016
      *     *-------*                                                   07220016
      *                                                                 07230016
           INITIALIZE REG-S1DQ9FTC-W                                    07240016
      *                                                                 07250016
           PERFORM 20030-GENERA-SALDOS                                  07260016
      *                                                                 07270016
RTP0  *    PERFORM 20010-OBTIENE-CLIENTE                                07280016
RTP0       PERFORM 220-RUTINA-PERSONA                                   07290016
      *                                                                 07300016
           MOVE E01-NUMCLI               TO S01-NUMCLI                  07310016
RTP0  *    MOVE W520-CODIDENT            TO S01-TIPDOC                  07320016
RTP0  *    MOVE W520-CLAIDENT            TO S01-NRODOC                  07330016
RTP0       MOVE W500-CODIDENT(1)         TO S01-TIPDOC                  07340016
RTP0       MOVE W500-CLAIDENT(1)         TO S01-NRODOC                  07350016
           MOVE E01-CTAVAL20 (01:08)     TO S01-CTAVAL20 (01:08)        07360016
           MOVE E01-CTAVAL20 (11:10)     TO S01-CTAVAL20 (09:10)        07370016
           MOVE E01-FECALTA (07:04)      TO S01-FECALTA (01:04)         07380016
           MOVE E01-FECALTA (04:02)      TO S01-FECALTA (05:02)         07390016
           MOVE E01-FECALTA (01:02)      TO S01-FECALTA (07:02)         07400016
           MOVE E01-FECCESE (07:04)      TO S01-FECCESE (01:04)         07410016
           MOVE E01-FECCESE (04:02)      TO S01-FECCESE (05:02)         07420016
           MOVE E01-FECCESE (01:02)      TO S01-FECCESE (07:02)         07430016
           MOVE E01-RUT                  TO S01-RUT                     07440016
                                                                        07450016
           IF WA-SALDO-INVER < ZEROS                                    07460016
              MOVE '-'                   TO S01-SIGNO-SDOINVE           07470016
           END-IF                                                       07480016
                                                                        07490016
           MOVE WA-SALDO-INVER           TO S01-SDOINVE                 07500016
                                                                        07510016
           IF WA-SALDO-VENTA < ZEROS                                    07520016
              MOVE '-'                   TO S01-SIGNO-IMPOVTA           07530016
           END-IF                                                       07540016
                                                                        07550016
           MOVE WA-SALDO-VENTA           TO S01-IMPOVTA                 07560016
           MOVE E01-MONEDA               TO S01-MONCONTR                07570016
                                                                        07580016
FVAXX *    IF VSMM-SALDO-AUT < ZEROS                                    07590016
FVAXX *       MOVE '-'                   TO S01-SIGNO-SDOREGI           07600016
FVAXX *    END-IF                                                       07610016
                                                                        07620016
FVAXX      IF E01-SITUACION = 'CANCELADA'                               07630016
FVAXX         MOVE SPACES                TO S01-SIGNO-SDOREGI           07640016
FVAXX         MOVE ZEROES                TO S01-SDOREGI                 07650016
FVAXX      ELSE                                                         07660016
FVAXX         IF VSMM-SALDO-AUT < ZEROS                                 07670016
FVAXX            MOVE SPACES             TO S01-SIGNO-SDOREGI           07680016
FVAXX            MOVE ZEROES             TO S01-SDOREGI                 07690016
FVAXX         ELSE                                                      07700016
                 MOVE VSMM-SALDO-AUT     TO S01-SDOREGI                 07710016
FVAXX         END-IF                                                    07720016
FVAXX      END-IF                                                       07730016
                                                                        07740016
FVAXX *    IF WSMM-SALDO-AUT < ZEROS                                    07750016
FVAXX *       MOVE '-'                   TO S01-SIGNO-SDOREGULTI        07760016
FVAXX *    END-IF                                                       07770016
                                                                        07780016
FVAXX      IF E01-SITUACION = 'CANCELADA'                               07790016
FVAXX         MOVE SPACES                TO S01-SIGNO-SDOREGULTI        07800016
FVAXX         MOVE ZEROES                TO S01-SDOREGULTI              07810016
FVAXX      ELSE                                                         07820016
FVAXX         IF VSMM-SALDO-AUT < ZEROS                                 07830016
FVAXX            MOVE SPACES             TO S01-SIGNO-SDOREGULTI        07840016
FVAXX            MOVE ZEROES             TO S01-SDOREGULTI              07850016
FVAXX         ELSE                                                      07860016
                 MOVE WSMM-SALDO-AUT     TO S01-SDOREGULTI              07870016
FVAXX         END-IF                                                    07880016
FVAXX      END-IF                                                       07890016
                                                                        07900016
           IF VSMM-FECONTA  = '9999-12-31'                              07910016
              MOVE SPACES                TO S01-FCHREGULTI              07920016
           ELSE                                                         07930016
              MOVE VSMM-FECONTA (01:04)  TO S01-FCHREGULTI (01:04)      07940016
              MOVE VSMM-FECONTA (06:02)  TO S01-FCHREGULTI (05:02)      07950016
              MOVE VSMM-FECONTA (09:02)  TO S01-FCHREGULTI (07:02)      07960016
           END-IF                                                       07970016
                                                                        07980016
           IF VSMO-FEALTREG = '9999-12-31'                              07990016
              MOVE 'NO'                  TO S01-IND-CTAREG              08000016
              MOVE SPACES                TO S01-FEALTREG                08010016
           ELSE                                                         08020016
              MOVE 'SI'                  TO S01-IND-CTAREG              08030016
              MOVE VSMO-FEALTREG (01:04) TO S01-FEALTREG (01:04)        08040016
              MOVE VSMO-FEALTREG (06:02) TO S01-FEALTREG (05:02)        08050016
              MOVE VSMO-FEALTREG (09:02) TO S01-FEALTREG (07:02)        08060016
           END-IF                                                       08070016
                                                                        08080016
           MOVE E01-MONEDA               TO S01-DIVISA                  08090016
                                                                        08100016
           IF WA-SALDO-INVER-0 < ZEROS                                  08110016
              MOVE '-'                   TO S01-SIGNO-ULTINVE           08120016
           END-IF                                                       08130016
                                                                        08140016
           MOVE WA-SALDO-INVER-0         TO S01-ULTINVE                 08150016
                                                                        08160016
           IF WHIS-FEC1RA-A > ZEROS                                     08170016
              MOVE WHIS-FEC1RA-A         TO S01-FHULINVE (01:04)        08180016
              MOVE WHIS-FEC1RA-M         TO S01-FHULINVE (05:02)        08190016
              MOVE WHIS-FEC1RA-D         TO S01-FHULINVE (07:02)        08200016
           ELSE                                                         08210016
              MOVE SPACES                TO S01-FHULINVE                08220016
           END-IF                                                       08230016
                                                                        08240016
           IF E01-MONEDA = 'PEN'                                        08250016
                                                                        08260016
              IF WA-DIVPEN < ZEROS                                      08270016
                 MOVE '-'                TO S01-SIGNO-IMPDIVI           08280016
              END-IF                                                    08290016
                                                                        08300016
              MOVE WA-DIVPEN             TO S01-IMPDIVI                 08310016
                                                                        08320016
              IF WA-INTPEN < ZEROS                                      08330016
                 MOVE '-'                TO S01-SIGNO-IMPINTE           08340016
              END-IF                                                    08350016
                                                                        08360016
              MOVE WA-INTPEN             TO S01-IMPINTE                 08370016
                                                                        08380016
              IF WA-AMTPEN < ZEROS                                      08390016
                 MOVE '-'                TO S01-SIGNO-IMPVCTO           08400016
              END-IF                                                    08410016
                                                                        08420016
              MOVE WA-AMTPEN             TO S01-IMPVCTO                 08430016
              MOVE REG-S1DQ9FTC          TO WA-S1DQ9FTC                 08440016
              PERFORM 20020-GRABA-SALIDA                                08450016
           ELSE                                                         08460016
                                                                        08470016
              IF WA-DIVUSD < ZEROS                                      08480016
                 MOVE '-'                TO S01-SIGNO-IMPDIVI           08490016
              END-IF                                                    08500016
                                                                        08510016
              MOVE WA-DIVUSD             TO S01-IMPDIVI                 08520016
                                                                        08530016
              IF WA-INTUSD < ZEROS                                      08540016
                 MOVE '-'                TO S01-SIGNO-IMPINTE           08550016
              END-IF                                                    08560016
                                                                        08570016
              MOVE WA-INTUSD             TO S01-IMPINTE                 08580016
                                                                        08590016
              IF WA-AMTUSD < ZEROS                                      08600016
                 MOVE '-'                TO S01-SIGNO-IMPVCTO           08610016
              END-IF                                                    08620016
                                                                        08630016
              MOVE WA-AMTUSD             TO S01-IMPVCTO                 08640016
              MOVE REG-S1DQ9FTC          TO WA-S1DQ9FTC                 08650016
              PERFORM 20020-GRABA-SALIDA                                08660016
           END-IF                                                       08670016
                                                                        08680016
      * PARA LIQUIDACIONES CON MONEDA DIFERENTE AL CONTRATO             08690016
           IF E01-MONEDA = 'PEN'                                        08700016
              IF WA-DIVUSD > ZEROS OR                                   08710016
                 WA-INTUSD > ZEROS OR                                   08720016
                 WA-AMTUSD > ZEROS                                      08730016
                 MOVE WA-S1DQ9FTC        TO REG-S1DQ9FTC                08740016
                 MOVE SPACES             TO S01-SIGNO-SDOINVE           08750016
                 MOVE ZEROS              TO S01-SDOINVE                 08760016
                 MOVE SPACES             TO S01-SIGNO-IMPOVTA           08770016
                 MOVE ZEROS              TO S01-IMPOVTA                 08780016
                                                                        08790016
                 IF WA-DIVUSD < ZEROS                                   08800016
                    MOVE '-'             TO S01-SIGNO-IMPDIVI           08810016
                 END-IF                                                 08820016
                                                                        08830016
                 MOVE WA-DIVUSD          TO S01-IMPDIVI                 08840016
                                                                        08850016
                 IF WA-INTUSD < ZEROS                                   08860016
                    MOVE '-'             TO S01-SIGNO-IMPINTE           08870016
                 END-IF                                                 08880016
                                                                        08890016
                 MOVE WA-INTUSD          TO S01-IMPINTE                 08900016
                                                                        08910016
                 IF WA-AMTUSD < ZEROS                                   08920016
                    MOVE '-'             TO S01-SIGNO-IMPVCTO           08930016
                 END-IF                                                 08940016
                                                                        08950016
                 MOVE WA-AMTUSD          TO S01-IMPVCTO                 08960016
                 MOVE 'USD'              TO S01-DIVISA                  08970016
                 PERFORM 20020-GRABA-SALIDA                             08980016
              END-IF                                                    08990016
           ELSE                                                         09000016
              IF WA-DIVPEN > ZEROS OR                                   09010016
                 WA-INTPEN > ZEROS OR                                   09020016
                 WA-AMTPEN > ZEROS                                      09030016
                 MOVE WA-S1DQ9FTC        TO REG-S1DQ9FTC                09040016
                 MOVE SPACES             TO S01-SIGNO-SDOINVE           09050016
                 MOVE ZEROS              TO S01-SDOINVE                 09060016
                 MOVE SPACES             TO S01-SIGNO-IMPOVTA           09070016
                 MOVE ZEROS              TO S01-IMPOVTA                 09080016
                                                                        09090016
                 IF WA-DIVPEN < ZEROS                                   09100016
                    MOVE '-'             TO S01-SIGNO-IMPDIVI           09110016
                 END-IF                                                 09120016
                                                                        09130016
                 MOVE WA-DIVPEN          TO S01-IMPDIVI                 09140016
                                                                        09150016
                 IF WA-INTPEN < ZEROS                                   09160016
                    MOVE '-'             TO S01-SIGNO-IMPINTE           09170016
                 END-IF                                                 09180016
                                                                        09190016
                 MOVE WA-INTPEN          TO S01-IMPINTE                 09200016
                                                                        09210016
                 IF WA-AMTPEN < ZEROS                                   09220016
                    MOVE '-'             TO S01-SIGNO-IMPVCTO           09230016
                 END-IF                                                 09240016
                                                                        09250016
                 MOVE WA-AMTPEN          TO S01-IMPVCTO                 09260016
                 MOVE 'PEN'              TO S01-DIVISA                  09270016
                PERFORM 20020-GRABA-SALIDA                              09280016
              END-IF                                                    09290016
           END-IF                                                       09300016
                                                                        09310016
           PERFORM 10010-LEER-ENTRADA                                   09320016
           .                                                            09330016
      *                                                                 09340016
      *     *---------------*                                           09350016
       20010-OBTIENE-CLIENTE.                                           09360016
      *     *---------------*                                           09370016
      *                                                                 09380016
           INITIALIZE           W520-REGISTRO                           09390016
      *                                                                 09400016
           MOVE E01-NUMCLI         TO W520-NUMCLIEN                     09410016
           MOVE SPACES             TO WSV-CLIENTE                       09420016
                                                                        09430016
           CALL PE9C5201 USING W-PEWC5201                               09440016
                                                                        09450016
           EVALUATE W520-PECRETOR                                       09460016
              WHEN ZEROS                                                09470016
                  IF W520-SUJGRUP = 'F'                                 09480016
                     STRING W520-PRIAPE DELIMITED BY '  ' ' '           09490016
                            W520-SEGAPE DELIMITED BY '  ' ' '           09500016
                            W520-NOMBRE DELIMITED BY '  '               09510016
                                              INTO WSV-CLIENTE          09520016
                  ELSE                                                  09530016
                     STRING W520-NOMBRE DELIMITED BY SIZE               09540016
                            W520-PRIAPE DELIMITED BY SIZE               09550016
                            W520-SEGAPE DELIMITED BY SIZE               09560016
                                              INTO WSV-CLIENTE          09570016
                  END-IF                                                09580016
              WHEN OTHER                                                09590016
                   MOVE '***NO UBICADO***'          TO  WSV-CLIENTE     09600016
           END-EVALUATE                                                 09610016
           .                                                            09620016
      *                                                                 09630016
       220-RUTINA-PERSONA.                                              09640016
      *-------------------*                                             09650016
      *                                                                 09660016
           INITIALIZE W500-REGISTRO                                     09670016
           MOVE SPACES               TO WSV-CLIENTE                     09680016
                                                                        09690016
           MOVE E01-CTAVAL20(13:08)  TO  W500-NUMECTA                   09700016
           MOVE E01-CTAVAL20(01:04)  TO  W500-PECENTID                  09710016
           MOVE E01-CTAVAL20(05:04)  TO  W500-OFIAPE                    09720016
           MOVE E01-CTAVAL20(11:02)  TO  W500-CODISER                   09730016
           MOVE 'T'                  TO  W500-CLAINTER                  09740016
           MOVE '01'                 TO  W500-SECINTER                  09750016
           MOVE 'U'                  TO  W500-PEYSELEC                  09760016
                                                                        09770016
           CALL PE9C5000 USING PEWC5000                                 09780016
                                                                        09790016
           IF W500-PECRETOR = '00'                                      09800016
              EVALUATE W500-SUJGRUP(1)                                  09810016
              WHEN 'F'                                                  09820016
                   STRING W500-PRIAPE(1) DELIMITED BY '  ' ' '          09830016
                          W500-SEGAPE(1) DELIMITED BY '  ' ' '          09840016
                          W500-NOMBRE(1) DELIMITED BY '  '              09850016
                          INTO WSV-CLIENTE                              09860016
                   END-STRING                                           09870016
                                                                        09880016
              WHEN 'M'                                                  09890016
                   STRING W500-NOMBRE(1) DELIMITED BY SIZE              09900016
                          W500-PRIAPE(1) DELIMITED BY SIZE              09910016
                          W500-SEGAPE(1) DELIMITED BY SIZE              09920016
                          INTO WSV-CLIENTE                              09930016
                   END-STRING                                           09940016
              END-EVALUATE                                              09950016
           END-IF                                                       09960016
           .                                                            09970016
                                                                        09980016
      *     *------------*                                              09990016
       20020-GRABA-SALIDA.                                              10000016
      *     *------------*                                              10010016
      *                                                                 10020016
           WRITE REG-S1DQ9FTC FROM REG-S1DQ9FTC-W.                      10030016
                                                                        10040016
           IF (FS-S1DQ9FTC EQUAL '00')                                  10050016
               ADD 1                   TO WSV-ESCRITOS                  10060016
           ELSE                                                         10070016
              DISPLAY '*  ERROR EN GRABAR REGISTROS FS : ' FS-S1DQ9FTC  10080016
              DISPLAY '*  REGISTRO LEIDOS              : ' WSV-ESCRITOS 10090016
              MOVE '02'  TO RETURN-CODE                                 10100016
              STOP RUN                                                  10110016
           END-IF                                                       10120016
           .                                                            10130016
      *                                                                 10140016
      *     *-------------*                                             10150016
       20030-GENERA-SALDOS.                                             10160016
      *     *-------------*                                             10170016
      *                                                                *10180016
           MOVE ZEROS                 TO WA-SALDO-INVER                 10190016
           MOVE ZEROS                 TO WA-SALDO-INVER-0               10200016
           MOVE ZEROS                 TO WA-SALDO-VENTA                 10210016
           MOVE ZEROS                 TO WA-DIVPEN                      10220016
           MOVE ZEROS                 TO WA-INTPEN                      10230016
           MOVE ZEROS                 TO WA-AMTPEN                      10240016
           MOVE ZEROS                 TO WA-DIVUSD                      10250016
           MOVE ZEROS                 TO WA-INTUSD                      10260016
           MOVE ZEROS                 TO WA-AMTUSD                      10270016
      *                                                                *10280016
           PERFORM 20031-SALDO-INVERSION                                10290016
      *                                                                *10300016
           PERFORM 20032-NEGOC-VENTAS                                   10310016
      *                                                                *10320016
           PERFORM 20033-OPERAC-FINANCIERA                              10330016
      *                                                                *10340016
           PERFORM 20034-SALDO-CTAREG                                   10350016
      *                                                                *10360016
           PERFORM 20035-SALDO-CTAREG                                   10370016
      *                                                                 10380016
           PERFORM 20036-SELECT-VLDTSMO                                 10390016
           .                                                            10400016
      *                                                                 10410016
      *     *---------------*                                           10420016
       20031-SALDO-INVERSION.                                           10430016
      *     *---------------*                                           10440016
      *                                                                *10450016
           PERFORM UNTIL FS-E2DQ9ADS = '10'                             10460016
                      OR VADS-CUENTA > WA-CUENTA-ARC7                   10470016
              IF VADS-CUENTA = WA-CUENTA-ARC7                           10480016
                 COMPUTE WA-SALDO       = VADS-DEPOS  + VADS-COMPR      10490016
                                        + VADS-SUSCR  - VADS-VENTA      10500016
                                        - VADS-ORDVE  - VADS-BLOQ       10510016
                 MOVE VADS-PAVAL   TO VXEN-PAVAL                        10520016
                 MOVE VADS-VALOR   TO VXEN-VALOR                        10530016
                 MOVE VADS-ISIN    TO VXEN-ISIN                         10540016
                 PERFORM 20040-SELECT-VLDTXEN                           10550016
                 MOVE VADS-PAVAL   TO  VCAM-CODVALOR (01:03)            10560016
                 MOVE VADS-VALOR   TO  VCAM-CODVALOR (04:08)            10570016
                 MOVE VADS-ISIN    TO  VCAM-CODVALOR (12:01)            10580016
31-12            MOVE WPOL-FECFIN  TO  VCAM-FECDIA                      10590016
                 PERFORM 10030-PRECIO-VALOR                             10600016
                 IF VXEN-TIPINT = 'F'                                   10610016
                    COMPUTE WA-SALDO-INVER =  WA-SALDO-INVER            10620016
                                           + (WA-SALDO * VCAM-CIERRE-D  10630016
                                                       * VXEN-NOMINEM   10640016
                                                       / 100)           10650016
                 ELSE                                                   10660016
                    COMPUTE WA-SALDO-INVER =  WA-SALDO-INVER            10670016
                                           + (WA-SALDO * VCAM-CIERRE-D) 10680016
                 END-IF                                                 10690016
              END-IF                                                    10700016
              PERFORM 10020-LEER-SALDOS                                 10710016
           END-PERFORM.                                                 10720016
      *                                                                *10730016
           IF WA-SALDO-INVER = ZEROS                                    10740016
              PERFORM 20050-MAX-VLDTHIS                                 10750016
              PERFORM 20055-SDO-INVER-CERO                              10760016
           END-IF.                                                      10770016
      *                                                                *10780016
      *     *------------*                                              10790016
       20032-NEGOC-VENTAS.                                              10800016
      *     *------------*                                              10810016
      *                                                                *10820016
           MOVE WA-CUENTA-ARC7 TO  VPOL-CUENTA.                         10830016
           MOVE 'V'            TO  VPOL-COMVEN.                         10840016
           MOVE 'LC'           TO  VPOL-SITUAC.                         10850016
      *                                                                *10860016
           EXEC SQL                                                     10870016
                OPEN VLDCPOL                                            10880016
           END-EXEC                                                     10890016
      *                                                                 10900016
           MOVE SQLCODE TO SQLCODE-AUX                                  10910016
      *                                                                 10920016
           EVALUATE TRUE                                                10930016
               WHEN DB2-OK                                              10940016
                    CONTINUE                                            10950016
               WHEN OTHER                                               10960016
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      10970016
                    MOVE 'VLDTPOL'               TO  W801-TABLA         10980016
                    MOVE 'OPEN'                  TO  W801-ACCION        10990016
                    MOVE WX-CUENTA-ARC7          TO  W801-CLAVE (01:07) 11000016
                    MOVE SQLCODE                 TO  W801-SQLCODE       11010016
                    MOVE SPACES                  TO  W801-SQLWARN       11020016
                    MOVE '20032-NEGOC-VENTAS   ' TO  W801-PARRAFO       11030016
                    PERFORM VLPC8010-DISP-ABEND-DB2                     11040016
                    PERFORM VLPC8010-ABEND-DB2                          11050016
           END-EVALUATE.                                                11060016
      *                                                                 11070016
           PERFORM UNTIL DB2-NOTFND                                     11080016
      *                                                                 11090016
              EXEC SQL                                                  11100016
                   FETCH VLDCPOL                                        11110016
                    INTO :VPOL-EFECTI                                   11120016
                       , :VPOL-CUPCORR                                  11130016
                       , :VPOL-IMPCOM1                                  11140016
                       , :VPOL-IMPCOM2                                  11150016
                       , :VPOL-IMPCOM3                                  11160016
                       , :VPOL-IMPCOM4                                  11170016
                       , :VPOL-IMPCOM5                                  11180016
                       , :VPOL-IMPCOM6                                  11190016
                       , :VPOL-IMPCOM7                                  11200016
                       , :VPOL-IMPCOM8                                  11210016
                       , :VPOL-IGV                                      11220016
              END-EXEC                                                  11230016
                                                                        11240016
              MOVE SQLCODE TO SQLCODE-AUX                               11250016
                                                                        11260016
              EVALUATE TRUE                                             11270016
                  WHEN DB2-OK                                           11280016
                       COMPUTE WA-SALDO-VENTA = WA-SALDO-VENTA +        11290016
                               VPOL-EFECTI    + VPOL-CUPCORR   -        11300016
                               VPOL-IMPCOM1   - VPOL-IMPCOM2   -        11310016
                               VPOL-IMPCOM3   - VPOL-IMPCOM4   -        11320016
                               VPOL-IMPCOM5   - VPOL-IMPCOM6   -        11330016
                               VPOL-IMPCOM7   - VPOL-IMPCOM8   -        11340016
                               VPOL-IGV                                 11350016
                  WHEN DB2-NOTFND                                       11360016
                       CONTINUE                                         11370016
                  WHEN OTHER                                            11380016
                       MOVE 'VL4C9FTX'           TO  W801-PROGRAMA      11390016
                       MOVE 'VLDTPOL'            TO  W801-TABLA         11400016
                       MOVE 'FETCH'              TO  W801-ACCION        11410016
                       MOVE WX-CUENTA-ARC7       TO  W801-CLAVE (01:07) 11420016
                       MOVE SQLCODE              TO  W801-SQLCODE       11430016
                       MOVE SPACES               TO  W801-SQLWARN       11440016
                       MOVE '20032-NEGOC-VENTAS   ' TO W801-PARRAFO     11450016
                       PERFORM VLPC8010-DISP-ABEND-DB2                  11460016
                       PERFORM VLPC8010-ABEND-DB2                       11470016
              END-EVALUATE                                              11480016
           END-PERFORM.                                                 11490016
                                                                        11500016
           EXEC SQL                                                     11510016
                CLOSE VLDCPOL                                           11520016
           END-EXEC                                                     11530016
      *                                                                 11540016
           MOVE SQLCODE TO SQLCODE-AUX                                  11550016
      *                                                                 11560016
           EVALUATE TRUE                                                11570016
               WHEN DB2-OK                                              11580016
                    CONTINUE                                            11590016
               WHEN OTHER                                               11600016
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      11610016
                    MOVE 'VLDTPOL'               TO  W801-TABLA         11620016
                    MOVE 'CLOSE'                 TO  W801-ACCION        11630016
                    MOVE WX-CUENTA-ARC7          TO  W801-CLAVE (01:07) 11640016
                    MOVE SQLCODE                 TO  W801-SQLCODE       11650016
                    MOVE SPACES                  TO  W801-SQLWARN       11660016
                    MOVE '20032-NEGOC-VENTAS   ' TO  W801-PARRAFO       11670016
                    PERFORM VLPC8010-DISP-ABEND-DB2                     11680016
                    PERFORM VLPC8010-ABEND-DB2                          11690016
           END-EVALUATE.                                                11700016
      *                                                                *11710016
      *     *-----------------*                                         11720016
       20033-OPERAC-FINANCIERA.                                         11730016
      *     *-----------------*                                         11740016
      *                                                                *11750016
           MOVE WA-CUENTA-ARC7 TO  VHAC-CUENTA.                         11760016
           MOVE ZEROS          TO  WA-DIVPEN, WA-DIVUSD.                11770016
           MOVE ZEROS          TO  WA-INTPEN, WA-INTUSD.                11780016
           MOVE ZEROS          TO  WA-AMTPEN, WA-AMTUSD.                11790016
      *                                                                *11800016
           EXEC SQL                                                     11810016
                OPEN VLDCHAC                                            11820016
           END-EXEC                                                     11830016
      *                                                                 11840016
           MOVE SQLCODE TO SQLCODE-AUX                                  11850016
      *                                                                 11860016
           EVALUATE TRUE                                                11870016
               WHEN DB2-OK                                              11880016
                    CONTINUE                                            11890016
               WHEN OTHER                                               11900016
                    MOVE 'VL4C9FTX'                TO W801-PROGRAMA     11910016
                    MOVE 'VLDTHAC'                 TO W801-TABLA        11920016
                    MOVE 'OPEN'                    TO W801-ACCION       11930016
                    MOVE WX-CUENTA-ARC7            TO W801-CLAVE        11940016
                    MOVE SQLCODE                   TO W801-SQLCODE      11950016
                    MOVE SPACES                    TO W801-SQLWARN      11960016
                    MOVE '20033-OPERAC-FINANCIERA' TO W801-PARRAFO      11970016
                    PERFORM VLPC8010-DISP-ABEND-DB2                     11980016
                    PERFORM VLPC8010-ABEND-DB2                          11990016
           END-EVALUATE.                                                12000016
      *                                                                 12010016
           PERFORM UNTIL DB2-NOTFND                                     12020016
      *                                                                 12030016
              EXEC SQL                                                  12040016
                   FETCH VLDCHAC                                        12050016
                    INTO :VHAC-OPERAC                                   12060016
                       , :VHAC-IMPLIQ                                   12070016
                       , :VHAC-MONEDA-CTA                               12080016
              END-EXEC                                                  12090016
                                                                        12100016
              MOVE SQLCODE TO SQLCODE-AUX                               12110016
                                                                        12120016
              EVALUATE TRUE                                             12130016
                  WHEN DB2-OK                                           12140016
                       EVALUATE VHAC-MONEDA-CTA                         12150016
                           WHEN 'PEN'                                   12160016
                                EVALUATE VHAC-OPERAC                    12170016
                                    WHEN 07                             12180016
                                         ADD VHAC-IMPLIQ TO WA-DIVPEN   12190016
                                    WHEN 08                             12200016
                                         ADD VHAC-IMPLIQ TO WA-INTPEN   12210016
                                    WHEN 11                             12220016
                                         ADD VHAC-IMPLIQ TO WA-AMTPEN   12230016
                                END-EVALUATE                            12240016
                           WHEN 'USD'                                   12250016
                                EVALUATE VHAC-OPERAC                    12260016
                                    WHEN 07                             12270016
                                         ADD VHAC-IMPLIQ TO WA-DIVUSD   12280016
                                    WHEN 08                             12290016
                                         ADD VHAC-IMPLIQ TO WA-INTUSD   12300016
                                    WHEN 11                             12310016
                                         ADD VHAC-IMPLIQ TO WA-AMTUSD   12320016
                                END-EVALUATE                            12330016
                       END-EVALUATE                                     12340016
                  WHEN DB2-NOTFND                                       12350016
                       CONTINUE                                         12360016
                  WHEN OTHER                                            12370016
                       MOVE 'VL4C9FTX'                TO  W801-PROGRAMA 12380016
                       MOVE 'VLDTHAC'                 TO  W801-TABLA    12390016
                       MOVE 'FETCH'                   TO  W801-ACCION   12400016
                       MOVE WX-CUENTA-ARC7            TO  W801-CLAVE    12410016
                       MOVE SQLCODE                   TO  W801-SQLCODE  12420016
                       MOVE SPACES                    TO  W801-SQLWARN  12430016
                       MOVE '20033-OPERAC-FINANCIERA' TO W801-PARRAFO   12440016
                       PERFORM VLPC8010-DISP-ABEND-DB2                  12450016
                       PERFORM VLPC8010-ABEND-DB2                       12460016
              END-EVALUATE                                              12470016
           END-PERFORM.                                                 12480016
      *                                                                 12490016
           EXEC SQL                                                     12500016
                CLOSE VLDCHAC                                           12510016
           END-EXEC                                                     12520016
      *                                                                 12530016
           MOVE SQLCODE TO SQLCODE-AUX                                  12540016
      *                                                                 12550016
           EVALUATE TRUE                                                12560016
               WHEN DB2-OK                                              12570016
                    CONTINUE                                            12580016
               WHEN OTHER                                               12590016
                    MOVE 'VL4C9FTX'                TO  W801-PROGRAMA    12600016
                    MOVE 'VLDTHAC'                 TO  W801-TABLA       12610016
                    MOVE 'CLOSE'                   TO  W801-ACCION      12620016
                    MOVE WX-CUENTA-ARC7            TO  W801-CLAVE       12630016
                    MOVE SQLCODE                   TO  W801-SQLCODE     12640016
                    MOVE SPACES                    TO  W801-SQLWARN     12650016
                    MOVE '20033-OPERAC-FINANCIERA' TO  W801-PARRAFO     12660016
                    PERFORM VLPC8010-DISP-ABEND-DB2                     12670016
                    PERFORM VLPC8010-ABEND-DB2                          12680016
           END-EVALUATE.                                                12690016
      *                                                                *12700016
      *     *------------*                                              12710016
       20034-SALDO-CTAREG.                                              12720016
      *     *------------*                                              12730016
      *                                                                *12740016
           MOVE E01-CTAVAL20    TO  VSMM-CTAVAL                         12750016
AAAA       MOVE WSV-FECHA-HAS-A TO  VSMM-FECONTA (01:04)                12760016
           MOVE '-'             TO  VSMM-FECONTA (05:01)                12770016
12         MOVE WSV-FECHA-HAS-M TO  VSMM-FECONTA (06:02)                12780016
           MOVE '-'             TO  VSMM-FECONTA (08:01)                12790016
31         MOVE WSV-FECHA-HAS-D TO  VSMM-FECONTA (09:02)                12800016
           MOVE ZEROS           TO  VSMM-SALDO-AUT                      12810016
      *                                                                *12820016
           EXEC SQL                                                     12830016
                OPEN VLDCSMM                                            12840016
           END-EXEC                                                     12850016
      *                                                                 12860016
           MOVE SQLCODE TO SQLCODE-AUX                                  12870016
      *                                                                 12880016
           EVALUATE TRUE                                                12890016
               WHEN DB2-OK                                              12900016
                    CONTINUE                                            12910016
               WHEN OTHER                                               12920016
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      12930016
                    MOVE 'VLDTSMM'               TO  W801-TABLA         12940016
                    MOVE 'OPEN'                  TO  W801-ACCION        12950016
                    MOVE E01-CTAVAL20            TO  W801-CLAVE         12960016
                    MOVE SQLCODE                 TO  W801-SQLCODE       12970016
                    MOVE SPACES                  TO  W801-SQLWARN       12980016
                    MOVE '20034-SALDO-CTAREG   ' TO  W801-PARRAFO       12990016
                    PERFORM VLPC8010-DISP-ABEND-DB2                     13000016
                    PERFORM VLPC8010-ABEND-DB2                          13010016
           END-EVALUATE.                                                13020016
      *                                                                 13030016
           EXEC SQL                                                     13040016
                FETCH  VLDCSMM                                          13050016
                 INTO :VSMM-SALDO-AUT                                   13060016
                    , :VSMM-NUMREF                                      13070016
           END-EXEC                                                     13080016
                                                                        13090016
           MOVE  SQLCODE TO SQLCODE-AUX                                 13100016
                                                                        13110016
           EVALUATE TRUE                                                13120016
               WHEN DB2-OK                                              13130016
                    CONTINUE                                            13140016
               WHEN DB2-NOTFND                                          13150016
                    MOVE ZEROS                   TO  VSMM-SALDO-AUT     13160016
                    MOVE ZEROS                   TO  VSMM-NUMREF        13170016
                    MOVE '9999-12-31'            TO  VSMM-FECONTA       13180016
               WHEN OTHER                                               13190016
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      13200016
                    MOVE 'VLDTSMM'               TO  W801-TABLA         13210016
                    MOVE 'SELECT-SALDO'          TO  W801-ACCION        13220016
                    MOVE E01-CTAVAL20            TO  W801-CLAVE         13230016
                    MOVE SQLCODE                 TO  W801-SQLCODE       13240016
                    MOVE SPACES                  TO  W801-SQLWARN       13250016
                    MOVE '20034-SALDO-CTAREG   ' TO  W801-PARRAFO       13260016
                    PERFORM VLPC8010-DISP-ABEND-DB2                     13270016
                    PERFORM VLPC8010-ABEND-DB2                          13280016
           END-EVALUATE.                                                13290016
                                                                        13300016
           EXEC SQL                                                     13310016
                CLOSE VLDCSMM                                           13320016
           END-EXEC                                                     13330016
      *                                                                 13340016
           MOVE SQLCODE TO SQLCODE-AUX                                  13350016
      *                                                                 13360016
           EVALUATE TRUE                                                13370016
               WHEN DB2-OK                                              13380016
                    CONTINUE                                            13390016
               WHEN OTHER                                               13400016
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      13410016
                    MOVE 'VLDTSMM'               TO  W801-TABLA         13420016
                    MOVE 'CLOSE'                 TO  W801-ACCION        13430016
                    MOVE E01-CTAVAL20            TO  W801-CLAVE         13440016
                    MOVE SQLCODE                 TO  W801-SQLCODE       13450016
                    MOVE SPACES                  TO  W801-SQLWARN       13460016
                    MOVE '20034-SALDO-CTAREG   ' TO  W801-PARRAFO       13470016
                    PERFORM VLPC8010-DISP-ABEND-DB2                     13480016
                    PERFORM VLPC8010-ABEND-DB2                          13490016
           END-EVALUATE.                                                13500016
      *                                                                *13510016
      *     *------------*                                              13520016
       20035-SALDO-CTAREG.                                              13530016
      *     *------------*                                              13540016
      *                                                                *13550016
           MOVE E01-CTAVAL20    TO  VSMM-CTAVAL                         13560016
AAAA       MOVE WSV-FECHA-HAS-A TO  VSMM-FECONTA (01:04)                13570016
           MOVE '-'             TO  VSMM-FECONTA (05:01)                13580016
12         MOVE WSV-FECHA-HAS-M TO  VSMM-FECONTA (06:02)                13590016
           MOVE '-'             TO  VSMM-FECONTA (08:01)                13600016
31         MOVE WSV-FECHA-HAS-D TO  VSMM-FECONTA (09:02)                13610016
           MOVE ZEROS           TO  WSMM-SALDO-AUT                      13620016
      *                                                                *13630016
           EXEC SQL                                                     13640016
                OPEN VLDUSMM                                            13650016
           END-EXEC                                                     13660016
      *                                                                 13670016
           MOVE SQLCODE TO SQLCODE-AUX                                  13680016
      *                                                                 13690016
           EVALUATE TRUE                                                13700016
               WHEN DB2-OK                                              13710016
                    CONTINUE                                            13720016
               WHEN OTHER                                               13730016
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      13740016
                    MOVE 'VLDTSMM'               TO  W801-TABLA         13750016
                    MOVE 'OPEN'                  TO  W801-ACCION        13760016
                    MOVE E01-CTAVAL20            TO  W801-CLAVE         13770016
                    MOVE SQLCODE                 TO  W801-SQLCODE       13780016
                    MOVE SPACES                  TO  W801-SQLWARN       13790016
                    MOVE '20035-SALDO-CTAREG   ' TO  W801-PARRAFO       13800016
                    PERFORM VLPC8010-DISP-ABEND-DB2                     13810016
                    PERFORM VLPC8010-ABEND-DB2                          13820016
           END-EVALUATE.                                                13830016
      *                                                                 13840016
           EXEC SQL                                                     13850016
                FETCH  VLDUSMM                                          13860016
                 INTO :WSMM-SALDO-AUT                                   13870016
                    , :VSMM-FECONTA                                     13880016
           END-EXEC                                                     13890016
                                                                        13900016
           MOVE  SQLCODE TO SQLCODE-AUX                                 13910016
                                                                        13920016
           EVALUATE TRUE                                                13930016
               WHEN DB2-OK                                              13940016
                    CONTINUE                                            13950016
               WHEN DB2-NOTFND                                          13960016
                    MOVE ZEROS                   TO  WSMM-SALDO-AUT     13970016
                    MOVE '9999-12-31'            TO  VSMM-FECONTA       13980016
               WHEN OTHER                                               13990016
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      14000016
                    MOVE 'VLDTSMM'               TO  W801-TABLA         14010016
                    MOVE 'SELECT-SALDO'          TO  W801-ACCION        14020016
                    MOVE E01-CTAVAL20            TO  W801-CLAVE         14030016
                    MOVE SQLCODE                 TO  W801-SQLCODE       14040016
                    MOVE SPACES                  TO  W801-SQLWARN       14050016
                    MOVE '20035-SALDO-CTAREG   ' TO  W801-PARRAFO       14060016
                    PERFORM VLPC8010-DISP-ABEND-DB2                     14070016
                    PERFORM VLPC8010-ABEND-DB2                          14080016
           END-EVALUATE.                                                14090016
                                                                        14100016
           EXEC SQL                                                     14110016
                CLOSE VLDUSMM                                           14120016
           END-EXEC                                                     14130016
      *                                                                 14140016
           MOVE SQLCODE TO SQLCODE-AUX                                  14150016
      *                                                                 14160016
           EVALUATE TRUE                                                14170016
               WHEN DB2-OK                                              14180016
                    CONTINUE                                            14190016
               WHEN OTHER                                               14200016
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      14210016
                    MOVE 'VLDTSMM'               TO  W801-TABLA         14220016
                    MOVE 'CLOSE'                 TO  W801-ACCION        14230016
                    MOVE E01-CTAVAL20            TO  W801-CLAVE         14240016
                    MOVE SQLCODE                 TO  W801-SQLCODE       14250016
                    MOVE SPACES                  TO  W801-SQLWARN       14260016
                    MOVE '20035-SALDO-CTAREG   ' TO  W801-PARRAFO       14270016
                    PERFORM VLPC8010-DISP-ABEND-DB2                     14280016
                    PERFORM VLPC8010-ABEND-DB2                          14290016
           END-EVALUATE.                                                14300016
      *                                                                *14310016
      *     *--------------*                                            14320016
       20036-SELECT-VLDTSMO.                                            14330016
      *     *--------------*                                            14340016
      *                                                                *14350016
           MOVE E01-CTAVAL20    TO  VSMO-CTAVAL                         14360016
      *                                                                *14370016
           EXEC SQL                                                     14380016
                SELECT  VSMO_FEALTREG                                   14390016
                  INTO :VSMO-FEALTREG                                   14400016
                  FROM VLDTSMO                                          14410016
                  WHERE VSMO_CTAVAL    = :VSMO-CTAVAL                   14420016
           END-EXEC.                                                    14430016
      *                                                                 14440016
           EVALUATE SQLCODE                                             14450016
               WHEN ZEROS                                               14460016
                    CONTINUE                                            14470016
               WHEN 100                                                 14480016
                    MOVE '9999-12-31'           TO  VSMO-FEALTREG       14490016
               WHEN OTHER                                               14500016
                    MOVE 'VL4C9FTX'             TO  W801-PROGRAMA       14510016
                    MOVE 'VLDTSDO'              TO  W801-TABLA          14520016
                    MOVE 'SELECT'               TO  W801-ACCION         14530016
                    MOVE E01-CTAVAL20           TO  W801-CLAVE (01:07)  14540016
                    MOVE  SQLCODE               TO  W801-SQLCODE        14550016
                    MOVE  SPACES                TO  W801-SQLWARN        14560016
                    MOVE '20036-SELECT-VLDTSMO' TO  W801-PARRAFO        14570016
                    PERFORM  VLPC8010-DISP-ABEND-DB2                    14580016
                    PERFORM  VLPC8010-ABEND-DB2                         14590016
           END-EVALUATE.                                                14600016
      *                                                                *14610016
      *     *--------------*                                            14620016
       20040-SELECT-VLDTXEN.                                            14630016
      *     *--------------*                                            14640016
      *                                                                *14650016
           EXEC SQL                                                     14660016
                SELECT  VXEN_NOMINEM                                    14670016
                     ,  VXEN_SUSPDT                                     14680016
                     ,  VXEN_TIPINT                                     14690016
                     ,  VXEN_MINPOL                                     14700016
                     ,  VXEN_FPROXA                                     14710016
                     ,  VXEN_FPROXC                                     14720016
                     ,  VXEN_FILLER                                     14730016
                  INTO :VXEN-NOMINEM                                    14740016
                     , :VXEN-SUSPDT                                     14750016
                     , :VXEN-TIPINT                                     14760016
                     , :VXEN-MINPOL                                     14770016
                     , :VXEN-FPROXA                                     14780016
                     , :VXEN-FPROXC                                     14790016
                     , :VXEN-FILLER                                     14800016
                  FROM VLDTXEN                                          14810016
                  WHERE VXEN_PAVAL     = :VXEN-PAVAL                    14820016
                    AND VXEN_VALOR     = :VXEN-VALOR                    14830016
                    AND VXEN_ISIN      = :VXEN-ISIN                     14840016
           END-EXEC.                                                    14850016
      *                                                                 14860016
           EVALUATE SQLCODE                                             14870016
               WHEN ZEROS                                               14880016
                    CONTINUE                                            14890016
                    IF VXEN-SUSPDT > ZEROS                              14900016
                       MOVE VXEN-SUSPDT         TO WXEN-SUSPDT          14910016
                       MOVE WXEN-NOMITEMP       TO VXEN-NOMINEM         14920016
                    END-IF                                              14930016
               WHEN OTHER                                               14940016
                    MOVE 'VL4C9FTX'             TO  W801-PROGRAMA       14950016
                    MOVE 'VLDTXEN'              TO  W801-TABLA          14960016
                    MOVE 'SELECT'               TO  W801-ACCION         14970016
                    MOVE WX-CUENTA-ARC7         TO  W801-CLAVE (01:07)  14980016
                    MOVE VXEN-PAVAL             TO  W801-CLAVE (09:03)  14990016
                    MOVE VXEN-VALOR             TO  W801-CLAVE (12:07)  15000016
                    MOVE VXEN-ISIN              TO  W801-CLAVE (19:01)  15010016
                    MOVE  SQLCODE               TO  W801-SQLCODE        15020016
                    MOVE  SPACES                TO  W801-SQLWARN        15030016
                    MOVE '20040-SELECT-VLDTXEN' TO  W801-PARRAFO        15040016
                    PERFORM  VLPC8010-DISP-ABEND-DB2                    15050016
                    PERFORM  VLPC8010-ABEND-DB2                         15060016
           END-EVALUATE.                                                15070016
      *                                                                *15080016
      *     *-----------*                                               15090016
       20050-MAX-VLDTHIS.                                               15100016
      *     *-----------*                                               15110016
      *    BUSCA AÑO DE INFORMACION                                    *15120016
           MOVE WA-CUENTA-ARC7 TO  VHIS-CTAVAL.                         15130016
           MOVE LK-F-AA-H      TO  WHIS-ANO.                            15140016
           MOVE ZEROS          TO  VHIS-ANO.                            15150016
           MOVE ZEROS          TO  VHIS-MES.                            15160016
      *                                                                *15170016
           EXEC SQL                                                     15180016
                SELECT  MAX(VHIS_ANO)                                   15190016
                  INTO     :VHIS-ANO                                    15200016
                  FROM VLDTHIS                                          15210016
                 WHERE VHIS_CTAVAL    = :VHIS-CTAVAL                    15220016
                   AND VHIS_ANO      <= :WHIS-ANO                       15230016
                   AND VHIS_TIPGAS   IN (48, 49)                        15240016
           END-EXEC.                                                    15250016
      *                                                                 15260016
           EVALUATE SQLCODE                                             15270016
               WHEN ZEROS                                               15280016
                    CONTINUE                                            15290016
               WHEN -305                                                15300016
                    MOVE ZEROS                  TO  VHIS-ANO            15310016
                    MOVE ZEROS                  TO  VHIS-MES            15320016
               WHEN OTHER                                               15330016
                    MOVE 'VL4C9FTX'             TO  W801-PROGRAMA       15340016
                    MOVE 'VLDTHIS'              TO  W801-TABLA          15350016
                    MOVE 'MAX-AÑO'              TO  W801-ACCION         15360016
                    MOVE WX-CUENTA-ARC7         TO  W801-CLAVE (01:07)  15370016
                    MOVE  SQLCODE               TO  W801-SQLCODE        15380016
                    MOVE  SPACES                TO  W801-SQLWARN        15390016
                    MOVE '20050-MAX-VLDTHIS'    TO  W801-PARRAFO        15400016
                    PERFORM  VLPC8010-DISP-ABEND-DB2                    15410016
                    PERFORM  VLPC8010-ABEND-DB2                         15420016
           END-EVALUATE.                                                15430016
      *    BUSCA MES DE INFORMACION                                    *15440016
           MOVE WA-CUENTA-ARC7 TO  VHIS-CTAVAL.                         15450016
           MOVE ZEROS          TO  VHIS-MES.                            15460016
      *                                                                *15470016
           EXEC SQL                                                     15480016
                SELECT  MAX(VHIS_MES)                                   15490016
                  INTO     :VHIS-MES                                    15500016
                  FROM VLDTHIS                                          15510016
                 WHERE VHIS_CTAVAL = :VHIS-CTAVAL                       15520016
                   AND VHIS_ANO    = :VHIS-ANO                          15530016
                   AND VHIS_TIPGAS IN (48, 49)                          15540016
           END-EXEC.                                                    15550016
      *                                                                 15560016
           EVALUATE SQLCODE                                             15570016
               WHEN ZEROS                                               15580016
                    CONTINUE                                            15590016
               WHEN -305                                                15600016
                    MOVE ZEROS                  TO  VHIS-ANO            15610016
                    MOVE ZEROS                  TO  VHIS-MES            15620016
               WHEN OTHER                                               15630016
                    MOVE 'VL4C9FTX'             TO  W801-PROGRAMA       15640016
                    MOVE 'VLDTHIS'              TO  W801-TABLA          15650016
                    MOVE 'MAX-MES'              TO  W801-ACCION         15660016
                    MOVE WX-CUENTA-ARC7         TO  W801-CLAVE (01:07)  15670016
                    MOVE  SQLCODE               TO  W801-SQLCODE        15680016
                    MOVE  SPACES                TO  W801-SQLWARN        15690016
                    MOVE '20050-MAX-VLDTHIS'    TO  W801-PARRAFO        15700016
                    PERFORM  VLPC8010-DISP-ABEND-DB2                    15710016
                    PERFORM  VLPC8010-ABEND-DB2                         15720016
           END-EVALUATE.                                                15730016
      *                                                                *15740016
      *     *--------------*                                            15750016
       20055-SDO-INVER-CERO.                                            15760016
      *     *--------------*                                           *15770016
      *                                                                *15780016
           MOVE WA-CUENTA-ARC7 TO VHIS-CTAVAL.                          15790016
           MOVE ZEROS          TO WHIS-FEC1RA-N.                        15800016
           MOVE SPACES         TO VHIS-CODVALOR.                        15810016
      *                                                                *15820016
           EXEC SQL                                                     15830016
                OPEN VLDUHIS                                            15840016
           END-EXEC                                                     15850016
      *                                                                 15860016
           MOVE SQLCODE TO SQLCODE-AUX                                  15870016
      *                                                                 15880016
           EVALUATE TRUE                                                15890016
               WHEN DB2-OK                                              15900016
                    CONTINUE                                            15910016
               WHEN OTHER                                               15920016
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      15930016
                    MOVE 'VLDTHIS'               TO  W801-TABLA         15940016
                    MOVE 'OPEN'                  TO  W801-ACCION        15950016
                    MOVE E01-CTAVAL20            TO  W801-CLAVE         15960016
                    MOVE SQLCODE                 TO  W801-SQLCODE       15970016
                    MOVE SPACES                  TO  W801-SQLWARN       15980016
                    MOVE '20055-SDO-INVER-CERO ' TO  W801-PARRAFO       15990016
                    PERFORM VLPC8010-DISP-ABEND-DB2                     16000016
                    PERFORM VLPC8010-ABEND-DB2                          16010016
           END-EVALUATE.                                                16020016
      *                                                                 16030016
           PERFORM UNTIL SQLCODE = 100                                  16040016
              EXEC SQL                                                  16050016
                   FETCH  VLDUHIS                                       16060016
                    INTO :VHIS-CODVALOR                                 16070016
                       , :VHIS-TITULOS1                                 16080016
                       , :VHIS-MOVIMI1                                  16090016
                       , :VHIS-CUSTODIA1                                16100016
                       , :VHIS-CAMBIO1                                  16110016
                       , :VHIS-COBRADO1                                 16120016
                       , :VHIS-TITULOS2                                 16130016
                       , :VHIS-MOVIMI2                                  16140016
                       , :VHIS-CUSTODIA2                                16150016
                       , :VHIS-CAMBIO2                                  16160016
                       , :VHIS-COBRADO2                                 16170016
                       , :VHIS-TITULOS3                                 16180016
                       , :VHIS-MOVIMI3                                  16190016
                       , :VHIS-CUSTODIA3                                16200016
                       , :VHIS-CAMBIO3                                  16210016
                       , :VHIS-COBRADO3                                 16220016
                       , :VHIS-TITULOS4                                 16230016
                       , :VHIS-MOVIMI4                                  16240016
                       , :VHIS-CUSTODIA4                                16250016
                       , :VHIS-CAMBIO4                                  16260016
                       , :VHIS-COBRADO4                                 16270016
                       , :VHIS-TITULOS5                                 16280016
                       , :VHIS-MOVIMI5                                  16290016
                       , :VHIS-CUSTODIA5                                16300016
                       , :VHIS-CAMBIO5                                  16310016
                       , :VHIS-COBRADO5                                 16320016
                       , :VHIS-TITULOS6                                 16330016
                       , :VHIS-MOVIMI6                                  16340016
                       , :VHIS-CUSTODIA6                                16350016
                       , :VHIS-CAMBIO6                                  16360016
                       , :VHIS-COBRADO6                                 16370016
                       , :VHIS-TITULOS7                                 16380016
                       , :VHIS-MOVIMI7                                  16390016
                       , :VHIS-CUSTODIA7                                16400016
                       , :VHIS-CAMBIO7                                  16410016
                       , :VHIS-COBRADO7                                 16420016
                       , :VHIS-TITULOS8                                 16430016
                       , :VHIS-MOVIMI8                                  16440016
                       , :VHIS-CUSTODIA8                                16450016
                       , :VHIS-CAMBIO8                                  16460016
                       , :VHIS-COBRADO8                                 16470016
                       , :VHIS-TITULOS9                                 16480016
                       , :VHIS-MOVIMI9                                  16490016
                       , :VHIS-CUSTODIA9                                16500016
                       , :VHIS-CAMBIO9                                  16510016
                       , :VHIS-COBRADO9                                 16520016
                       , :VHIS-TITULOS10                                16530016
                       , :VHIS-MOVIMI10                                 16540016
                       , :VHIS-CUSTODIA10                               16550016
                       , :VHIS-CAMBIO10                                 16560016
                       , :VHIS-COBRADO10                                16570016
                       , :VHIS-TITULOS11                                16580016
                       , :VHIS-MOVIMI11                                 16590016
                       , :VHIS-CUSTODIA11                               16600016
                       , :VHIS-CAMBIO11                                 16610016
                       , :VHIS-COBRADO11                                16620016
                       , :VHIS-TITULOS12                                16630016
                       , :VHIS-MOVIMI12                                 16640016
                       , :VHIS-CUSTODIA12                               16650016
                       , :VHIS-CAMBIO12                                 16660016
                       , :VHIS-COBRADO12                                16670016
                       , :VHIS-TITULOS13                                16680016
                       , :VHIS-MOVIMI13                                 16690016
                       , :VHIS-CUSTODIA13                               16700016
                       , :VHIS-CAMBIO13                                 16710016
                       , :VHIS-COBRADO13                                16720016
                       , :VHIS-TITULOS14                                16730016
                       , :VHIS-MOVIMI14                                 16740016
                       , :VHIS-CUSTODIA14                               16750016
                       , :VHIS-CAMBIO14                                 16760016
                       , :VHIS-COBRADO14                                16770016
                       , :VHIS-TITULOS15                                16780016
                       , :VHIS-MOVIMI15                                 16790016
                       , :VHIS-CUSTODIA15                               16800016
                       , :VHIS-CAMBIO15                                 16810016
                       , :VHIS-COBRADO15                                16820016
                       , :VHIS-TITULOS16                                16830016
                       , :VHIS-MOVIMI16                                 16840016
                       , :VHIS-CUSTODIA16                               16850016
                       , :VHIS-CAMBIO16                                 16860016
                       , :VHIS-COBRADO16                                16870016
                       , :VHIS-TITULOS17                                16880016
                       , :VHIS-MOVIMI17                                 16890016
                       , :VHIS-CUSTODIA17                               16900016
                       , :VHIS-CAMBIO17                                 16910016
                       , :VHIS-COBRADO17                                16920016
                       , :VHIS-TITULOS18                                16930016
                       , :VHIS-MOVIMI18                                 16940016
                       , :VHIS-CUSTODIA18                               16950016
                       , :VHIS-CAMBIO18                                 16960016
                       , :VHIS-COBRADO18                                16970016
                       , :VHIS-TITULOS19                                16980016
                       , :VHIS-MOVIMI19                                 16990016
                       , :VHIS-CUSTODIA19                               17000016
                       , :VHIS-CAMBIO19                                 17010016
                       , :VHIS-COBRADO19                                17020016
                       , :VHIS-TITULOS20                                17030016
                       , :VHIS-MOVIMI20                                 17040016
                       , :VHIS-CUSTODIA20                               17050016
                       , :VHIS-CAMBIO20                                 17060016
                       , :VHIS-COBRADO20                                17070016
                       , :VHIS-TITULOS21                                17080016
                       , :VHIS-MOVIMI21                                 17090016
                       , :VHIS-CUSTODIA21                               17100016
                       , :VHIS-CAMBIO21                                 17110016
                       , :VHIS-COBRADO21                                17120016
                       , :VHIS-TITULOS22                                17130016
                       , :VHIS-MOVIMI22                                 17140016
                       , :VHIS-CUSTODIA22                               17150016
                       , :VHIS-CAMBIO22                                 17160016
                       , :VHIS-COBRADO22                                17170016
                       , :VHIS-TITULOS23                                17180016
                       , :VHIS-MOVIMI23                                 17190016
                       , :VHIS-CUSTODIA23                               17200016
                       , :VHIS-CAMBIO23                                 17210016
                       , :VHIS-COBRADO23                                17220016
                       , :VHIS-TITULOS24                                17230016
                       , :VHIS-MOVIMI24                                 17240016
                       , :VHIS-CUSTODIA24                               17250016
                       , :VHIS-CAMBIO24                                 17260016
                       , :VHIS-COBRADO24                                17270016
                       , :VHIS-TITULOS25                                17280016
                       , :VHIS-MOVIMI25                                 17290016
                       , :VHIS-CUSTODIA25                               17300016
                       , :VHIS-CAMBIO25                                 17310016
                       , :VHIS-COBRADO25                                17320016
                       , :VHIS-TITULOS26                                17330016
                       , :VHIS-MOVIMI26                                 17340016
                       , :VHIS-CUSTODIA26                               17350016
                       , :VHIS-CAMBIO26                                 17360016
                       , :VHIS-COBRADO26                                17370016
                       , :VHIS-TITULOS27                                17380016
                       , :VHIS-MOVIMI27                                 17390016
                       , :VHIS-CUSTODIA27                               17400016
                       , :VHIS-CAMBIO27                                 17410016
                       , :VHIS-COBRADO27                                17420016
                       , :VHIS-TITULOS28                                17430016
                       , :VHIS-MOVIMI28                                 17440016
                       , :VHIS-CUSTODIA28                               17450016
                       , :VHIS-CAMBIO28                                 17460016
                       , :VHIS-COBRADO28                                17470016
                       , :VHIS-TITULOS29                                17480016
                       , :VHIS-MOVIMI29                                 17490016
                       , :VHIS-CUSTODIA29                               17500016
                       , :VHIS-CAMBIO29                                 17510016
                       , :VHIS-COBRADO29                                17520016
                       , :VHIS-TITULOS30                                17530016
                       , :VHIS-MOVIMI30                                 17540016
                       , :VHIS-CUSTODIA30                               17550016
                       , :VHIS-CAMBIO30                                 17560016
                       , :VHIS-COBRADO30                                17570016
                       , :VHIS-TITULOS31                                17580016
                       , :VHIS-MOVIMI31                                 17590016
                       , :VHIS-CUSTODIA31                               17600016
                       , :VHIS-CAMBIO31                                 17610016
                       , :VHIS-COBRADO31                                17620016
                       , :VHIS-FEALTREG                                 17630016
                       , :VHIS-FEULMOD                                  17640016
                       , :VHIS-HORULMOD                                 17650016
                       , :VHIS-NUMTER                                   17660016
                       , :VHIS-USUARIO                                  17670016
              END-EXEC                                                  17680016
                                                                        17690016
              MOVE  SQLCODE TO SQLCODE-AUX                              17700016
                                                                        17710016
              EVALUATE TRUE                                             17720016
                  WHEN DB2-OK                                           17730016
                       MOVE DCLVLDTHIS            TO DCLVLTCHIS         17740016
                       MOVE ZEROS          TO  WH-SALD0                 17750016
                       MOVE ZEROS          TO  WH-NOMINEM               17760016
                       PERFORM VARYING WI FROM 31 BY -1                 17770016
                                 UNTIL WI    = ZEROS                    17780016
                                    OR WH-SALD0 > ZEROS                 17790016
                          IF CVHIS-TITULOS (WI) > ZEROS                 17800016
                             MOVE CVHIS-TITULOS (WI) TO WH-SALD0        17810016
                             MOVE CVHIS-CAMBIO  (WI) TO WH-NOMINEM      17820016
                             MOVE WI                 TO WHIS-FECHIS-D   17830016
                          END-IF                                        17840016
                       END-PERFORM                                      17850016
                       MOVE VHIS-CODVALOR (01:03) TO VXEN-PAVAL         17860016
                       MOVE VHIS-CODVALOR (04:08) TO VXEN-VALOR         17870016
                       MOVE VHIS-CODVALOR (12:01) TO VXEN-ISIN          17880016
                       PERFORM 20040-SELECT-VLDTXEN                     17890016
                       MOVE VHIS-CODVALOR         TO VCAM-CODVALOR      17900016
                       MOVE VHIS-ANO              TO WHIS-FECHIS-A      17910016
                       MOVE VHIS-MES              TO WHIS-FECHIS-M      17920016
                       MOVE WHIS-FECHIS-N         TO VCAM-FECDIA        17930016
                       IF WHIS-FECHIS-N > ZEROS AND                     17940016
                          WHIS-FEC1RA-N = ZEROS                         17950016
                          MOVE WHIS-FECHIS-N      TO WHIS-FEC1RA-N      17960016
                       END-IF                                           17970016
                       PERFORM 10030-PRECIO-VALOR                       17980016
                       IF VXEN-TIPINT = 'F'                             17990016
                          COMPUTE WA-SALDO-INVER-0   = WA-SALDO-INVER-0 18000016
                                         + (WH-SALD0 * VCAM-CIERRE-D    18010016
                                                     * WH-NOMINEM       18020016
                                                     / 100)             18030016
                       ELSE                                             18040016
                          COMPUTE WA-SALDO-INVER-0   = WA-SALDO-INVER-0 18050016
                                         + (WH-SALD0 * VCAM-CIERRE-D)   18060016
                       END-IF                                           18070016
                  WHEN DB2-NOTFND                                       18080016
                       CONTINUE                                         18090016
                  WHEN OTHER                                            18100016
                       MOVE 'VL4C9FTX'              TO  W801-PROGRAMA   18110016
                       MOVE 'VLDTHIS'               TO  W801-TABLA      18120016
                       MOVE 'FETCH       '          TO  W801-ACCION     18130016
                       MOVE E01-CTAVAL20            TO  W801-CLAVE      18140016
                       MOVE SQLCODE                 TO  W801-SQLCODE    18150016
                       MOVE SPACES                  TO  W801-SQLWARN    18160016
                       MOVE '20055-SDO-INVER-CERO ' TO  W801-PARRAFO    18170016
                       PERFORM VLPC8010-DISP-ABEND-DB2                  18180016
                       PERFORM VLPC8010-ABEND-DB2                       18190016
              END-EVALUATE                                              18200016
           END-PERFORM.                                                 18210016
      *                                                                 18220016
           EXEC SQL                                                     18230016
                CLOSE VLDUHIS                                           18240016
           END-EXEC                                                     18250016
      *                                                                 18260016
           MOVE SQLCODE TO SQLCODE-AUX                                  18270016
      *                                                                 18280016
           EVALUATE TRUE                                                18290016
               WHEN DB2-OK                                              18300016
                    CONTINUE                                            18310016
               WHEN OTHER                                               18320016
                    MOVE 'VL4C9FTX'              TO  W801-PROGRAMA      18330016
                    MOVE 'VLDTHIS'               TO  W801-TABLA         18340016
                    MOVE 'CLOSE'                 TO  W801-ACCION        18350016
                    MOVE E01-CTAVAL20            TO  W801-CLAVE         18360016
                    MOVE SQLCODE                 TO  W801-SQLCODE       18370016
                    MOVE SPACES                  TO  W801-SQLWARN       18380016
                    MOVE '20055-SDO-INVER-CERO ' TO  W801-PARRAFO       18390016
                    PERFORM VLPC8010-DISP-ABEND-DB2                     18400016
                    PERFORM VLPC8010-ABEND-DB2                          18410016
           END-EVALUATE.                                                18420016
      *                                                                *18430016
      *     *--------------*                                            18440016
      ******************************************************************18450016
      *                   30000-FIN                                    *18460016
      ******************************************************************18470016
      *---------*                                                       18480016
       30000-FIN.                                                       18490016
      *---------*                                                       18500016
      *                                                                 18510016
           DISPLAY 'READ  S1DQ9FTC.... : ' WSV-LEIDOS.                  18520016
           DISPLAY 'WRITE E1DQ9FTC.... : ' WSV-ESCRITOS.                18530016
           STOP RUN                                                     18540016
           .                                                            18550016
      *                                                                *18560016
      *    *-----------------*                                          18570016
       3100-DISP-TOTALIMETROS.                                          18580016
      *    *-----------------*                                          18590016
           DISPLAY '*************************************************'. 18600016
           DISPLAY '********    T O T A L I M E T R O S   ***********'. 18610016
           DISPLAY '********   D E L     P R O G R A M A  ***********'. 18620016
           DISPLAY '********           VL4C9FTX           ***********'. 18630016
           DISPLAY '*************************************************'. 18640016
           DISPLAY '*************************************************'. 18650016
      *                                                                *18660016
      *    *---------------------*                                      18670016
       3100-DISP-TOTALIMETROS-FIN.                                      18680016
      *    *---------------------*                                      18690016
           EXIT.                                                        18700016
      ******************************************************************18710016
      **                  COPYS DE ERRORES DE PROCEDURE               **18720016
      ******************************************************************18730016
      *                                                                *18740016
            COPY  QRWCDB20.                                             18750016
            COPY  VLPC8010.                                             18760016
            COPY  VLPC8020.                                             18770016
            COPY  VLPCRUTI.                                             18780016
      *                                                                *18790016
      *------------------*                                              18800016
      *99999-FIN-PROGRAMA*                                              18810016
      *------------------*                                              18820016
