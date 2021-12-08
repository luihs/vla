      * ============================================================== *00010058
      * BBVA BANCO CONTINENTAL - STEFANINI IT.SOLUTIONS       VL4C3050 *00020058
      * ============================================================== *00030058
      * AUTOR    : GUSTAVO A. RAMIREZ DE LA ROSA                       *00040058
      * PROGRAMA : VL4C3050                                            *00050058
      * OBJETIVO : PRELIQUIDACIONES LIQUIDACION ENTRE LA 1 Y LAS 4     *00060058
      * -------------------------------------------------------------- *00070058
      *                  LOG DE MODIFICACIONES                         *00080058
      * -------------------------------------------------------------- *00090058
      * PETICION  FECHA    AUTOR           DESCRIPCION                 *00100058
      * --------- -------- --------------- ----------------------------*00110058
VCH   * 200712129          VANESSA CALVO H ADECUAR A 8 ENTEROS.        *00120058
      * --------- -------- --------------- ----------------------------*00130058
JPC@1 * 200712129 13.07.09 JHONNY PINEDO C LIQUIDACION MONEDA CRUZADA Y*00140058
      *                                    CONSIDERAR COMSIONES.       *00150058
      * --------- -------- --------------- ----------------------------*00160058
JPC@2 * 200712031 13.01.11 JHONNY PINEDO C LIQUIDACION DIVIDENDO, INTER*00170058
      *                                    ES, AMORTIZACION Y LISTADO. *00180058
      * --------- -------- --------------- ----------------------------*00190058
JPC@3 * 201408017 01.07.14 JHONNY PINEDO C DETALLE DE LIQUIDACION VA   *00200058
      *                                    VXEN-NOMBRE + OPERACION.    *00210058
      *                                   °GUARDAR TIPO DE CAMBIO POR  *00220058
      *                                    MONEDAS DIFERENTES ENTRE    *00230058
      *                                    IMPORTE Y CUENTA ECONOMICA. *00240058
      * --------- -------- --------------- ----------------------------*00250058
JPC@4 *           04.02.14 JHONNY PINEDO C GUARDA TIPO DE CAMBIO EXTRJ.*00260058
      *                                    -GPCAMADE Y -GPCAMABO.      *00270058
      * --------- -------- --------------- ----------------------------*00280058
      * IGH-XXXX  22.08.19 IGH GROUP       CAMBIO MARCA BBVA           *00290058
      * --------- -------- --------------- ----------------------------*00300058
      ******************************************************************00310058
       IDENTIFICATION DIVISION.                                         00320058
      *************************                                         00330058
       PROGRAM-ID.   VL4C3050.                                          00340058
       AUTHOR.       GUSTAVO RAMIREZ DE LA ROSA.                        00350058
       DATE-WRITTEN. ABRIL 2009.                                        00360058
       DATE-COMPILED.                                                   00370058
      ****************************************************************  00380058
      *       GENERA MIR PARA CUENTAS PERSONALES                     *  00390058
      ****************************************************************  00400058
       ENVIRONMENT DIVISION.                                            00410058
       CONFIGURATION SECTION.                                           00420058
       SPECIAL-NAMES.                                                   00430058
      *    DECIMAL-POINT IS COMMA.                                      00440058
       INPUT-OUTPUT SECTION.                                            00450058
       FILE-CONTROL.                                                    00460058
      *                                                                 00470058
           SELECT VLLS3050 ASSIGN TO UT-VLLS3050.                       00480058
      *                                                                 00490058
      ******************************************************************00500058
      **                                                              **00510058
      **                  D A T A    D I V I S I O N                  **00520058
      **                                                              **00530058
      ******************************************************************00540058
       DATA DIVISION.                                                   00550058
      *=============*                                                   00560058
      ******************************************************************00570058
      **              DESCRIPCION FICHEROS DE SALIDA                  **00580058
      ******************************************************************00590058
       FILE SECTION.                                                    00600058
       FD  VLLS3050                                                     00610058
           RECORDING MODE  IS   F                                       00620058
           LABEL RECORD    IS   OMITTED.                                00630058
       01  R-VLLS3050           PIC X(132).                             00640058
      *                                                                 00650058
       WORKING-STORAGE SECTION.                                         00660058
      *=======================*                                         00670058
       77  SW-ERROR                PIC 9           VALUE ZEROS.         00680058
       77  WA-COBCOM               PIC X           VALUE SPACES.        00690058
       77  FS-FBGEDMT              PIC XX          VALUE ZEROS.         00700058
       77  W-RETURN-CODE           PIC 9(02)       VALUE ZEROS.         00710058
       77  CTE-SIGLOXX             PIC 9(02)       VALUE 19.            00720058
       77  CTE-SIGLOXXI            PIC 9(02)       VALUE 20.            00730058
       77  CONT-PAG                PIC 9(06)       VALUE ZEROS.         00740058
       77  CONT-LIN                PIC 9(06)       VALUE ZEROS.         00750058
       77  WA-COMISION             PIC 9(12)V9(02) VALUE ZEROS.         00760058
       77  WT-COMISION             PIC 9(12)V9(02) VALUE ZEROS.         00770058
       77  WE-COMISION             PIC 9(12)V9(02) VALUE ZEROS.         00780058
       77  WT-IMPLIQ               PIC 9(12)V9(02) VALUE ZEROS.         00790058
       77  WE-IMPLIQ               PIC 9(12)V9(02) VALUE ZEROS.         00800058
       77  WSAB-CTAECOS            PIC X(20)       VALUE SPACES.        00810058
       77  WSAB-CTAECOD            PIC X(20)       VALUE SPACES.        00820058
                                                                        00830058
       77  SW-TIENE-ERROR          PIC  X(02)      VALUE SPACES.        00840058
           88 NO-TIENE-ERROR                       VALUE 'NO'.          00850058
           88 SI-TIENE-ERROR                       VALUE 'SI'.          00860058
                                                                        00870058
       77  SW-PROCESO              PIC  X(02)      VALUE SPACES.        00880058
           88 SW-PROCESO-SI                        VALUE 'SI'.          00890058
           88 SW-PROCESO-NO                        VALUE 'NO'.          00900058
                                                                        00910058
       77  SW-OPE-FINANCIERA       PIC  X(02)      VALUE SPACES.        00920058
           88 OPE-SUSCRIPCION                      VALUE 'SS'.          00930058
           88 OPE-DIVIDENDOS                       VALUE 'DD'.          00940058
           88 OPE-INTERESES                        VALUE 'DV'.          00950058
           88 OPE-AMORTIZACION                     VALUE 'AT', 'AR'.    00960058
JPC@3      88 OPE-AMORTIZA-A                       VALUE 'AR'.          00970058
JPC@3      88 OPE-AMORTIZA-R                       VALUE 'AT'.          00980058
      *                                                                 00990058
       01  W-LIMITE-AUT            PIC  X(08)      VALUE SPACES.        01000058
       01  W-LIMITE-AUT-9 REDEFINES W-LIMITE-AUT                        01010058
                                   PIC S9(13)V9(02) COMP-3.             01020058
                                                                        01030058
       01  WDET-NUM-RETX.                                               01040058
           02 WDET-NUM-RET         PIC S9(5)V USAGE COMP-3.             01050058
                                                                        01060058
       01  W-REFAPU                PIC 9(9).                            01070058
       01  W-DATE                  PIC 9(6).                            01080058
       01  FILLER                  REDEFINES W-DATE.                    01090058
           02 W-ANO                PIC 99.                              01100058
           02 W-MES                PIC 99.                              01110058
           02 W-DIA                PIC 99.                              01120058
                                                                        01130058
JPC@1  01  W-VLWCCCO0.                                                  01140058
JPC@1      COPY VLWCCCO0.                                               01150058
JPC@1  01  W-BGECMDC.                                                   01160058
JPC@1      COPY BGECMDC.                                                01170058
                                                                        01180058
       01  REG-G10FTI0.                                                 01190058
           COPY BXECTI0.                                                01200058
                                                                        01210058
JPC@2  01  W-PEWC5201.                                                  01220058
JPC@2      COPY PEWC5201.                                               01230058
                                                                        01240058
JPC@3  01  GPWC950.                                                     01250058
JPC@3      COPY GPWC950.                                                01260058
                                                                        01270058
       01  W-CCC.                                                       01280058
           02  W-ENTIDAD           PIC  X(04).                          01290058
           02  W-CENTRO-ALTA       PIC  X(04).                          01300058
           02  W-CUENTA            PIC  X(10).                          01310058
                                                                        01320058
       01  W-MIR-FECHA-VALOR        PIC X(10).                          01330058
       01  FILLER                   REDEFINES W-MIR-FECHA-VALOR.        01340058
           02 W-MIR-FECHA-VA1      PIC 99.                              01350058
           02 W-MIR-FECHA-VA2      PIC 99.                              01360058
           02 W-MIR-FECHA-FI1      PIC X.                               01370058
           02 W-MIR-FECHA-VMM      PIC 99.                              01380058
           02 W-MIR-FECHA-FI2      PIC X.                               01390058
           02 W-MIR-FECHA-VDD      PIC 99.                              01400058
                                                                        01410058
JPC@2  01  W-OPER-FINANCIERA.                                           01420058
JPC@2      02 WOPE-FORMAT-S        PIC X(01)       VALUE 'S'.           01430058
JPC@2      02 WOPE-FORMAT-D        PIC X(01)       VALUE 'D'.           01440058
JPC@2      02 WOPE-FORMAT-A        PIC X(01)       VALUE 'A'.           01450058
JPC@2                                                                   01460058
JPC@2      02 WOPE-TIPOPE-A        PIC X(01)       VALUE 'A'.           01470058
JPC@2      02 WOPE-TIPOPE-D        PIC X(01)       VALUE 'D'.           01480058
JPC@2      02 WOPE-TIPOPE-V        PIC X(01)       VALUE 'V'.           01490058
JPC@2      02 WOPE-TIPOPE-T        PIC X(01)       VALUE 'T'.           01500058
JPC@2      02 WOPE-TIPOPE-R        PIC X(01)       VALUE 'R'.           01510058
                                                                        01520058
       01  W-VARIABLES.                                                 01530058
            10 W-IMPORTE           PIC S9(13)V9(2) VALUE ZEROS.         01540058
            10 W-CODI-RETORN       PIC 99          VALUE ZEROS.         01550058
               88 BENFET           VALUES 0 1 2 3 4 5 6 7.              01560058
               88 ER-TC8C3010      VALUE  18.                           01570058
               88 ER-TC9C1000      VALUE  19.                           01580058
               88 ER-TC9C1200      VALUE  20.                           01590058
               88 ERROR-DB2        VALUE  33.                           01600058
            10 CT-BG9CBFT0         PIC X(08)       VALUE 'BG9CBFT0'.    01610058
            10 CT-QR4CDB0          PIC X(08)       VALUE 'QR4CDB0 '.    01620058
            10 CT-VL4C3050         PIC X(08)       VALUE 'VL4C3050'.    01630058
            10 CT-BG9CMIR0         PIC X(08)       VALUE 'BG9CMIR0'.    01640058
JPC@3       10 CT-GP8C1950         PIC X(08)       VALUE 'GP8C1950'.    01650058
            10 PE9C5201            PIC X(08)       VALUE 'PE9C5201'.    01660058
            10 VL9CBCOO            PIC X(08)       VALUE 'VL9CBCOO'.    01670058
            10 BG9CMDC0            PIC X(08)       VALUE 'BG9CMDC0'.    01680058
            10 CT-T5               PIC X(02)       VALUE 'T5'.          01690058
            10 CT-PEN              PIC X(03)       VALUE 'PEN'.         01700058
            10 CT-USD              PIC X(03)       VALUE 'USD'.         01710058
            10 WCLAVE              PIC X(10)       VALUE SPACES.        01720058
            10 WESTADO             PIC X(02)       VALUE SPACES.        01730058
            10 WFILES              PIC X(08)       VALUE SPACES.        01740058
            10 WMENS               PIC X(30)       VALUE SPACES.        01750058
            10 WA-MIR-OK           PIC 9(09)       VALUE ZEROES.        01760058
            10 WA-MIR-SAB          PIC 9(09)       VALUE ZEROES.        01770058
            10 WA-LEIDOS-OPE       PIC 9(09)       VALUE ZEROES.        01780058
            10 WA-LEIDOS-DET       PIC 9(09)       VALUE ZEROES.        01790058
            10 WA-LEIDOS-OPE-OK    PIC 9(09)       VALUE ZEROES.        01800058
            10 WA-PROCES-OPE-OK    PIC 9(09)       VALUE ZEROES.        01810058
            10 WA-LEIDOS-DET-OK    PIC 9(09)       VALUE ZEROES.        01820058
            10 INSERT-VCON         PIC 9(09)       VALUE ZEROES.        01830058
            10 WA-ACTUALIZA-DET    PIC 9(09)       VALUE ZEROES.        01840058
JPC@3       10 WA-CALL-GP8C1950    PIC 9(09)       VALUE ZEROES.        01850058
                                                                        01860058
JPC@3  01  W-CAMBIO               PIC S9(03)V9(15) COMP-3  VALUE 0.     01870058
JPC@3  01  W-CAMBIO-PEN           PIC S9(03)V9(15) COMP-3  VALUE 0.     01880058
JPC@3  01  W-WOPS-TIPO-CAMBIO     PIC  9(08)V9(05).                     01890058
                                                                        01900058
JPC@4  01  W-TC-GP8C1950.                                               01910058
JPC@4      02 WTC-GPCAMADE        PIC S9(03)V9(11) COMP-3  VALUE 0.     01920058
JPC@4      02 WTC-GPCAMABO        PIC S9(03)V9(11) COMP-3  VALUE 0.     01930058
                                                                        01940058
JPC@3  01 WA-IMPORTE-LIQ2.                                              01950058
JPC@3     02 W-IMPLIQ             PIC S9(13)V9(02) VALUE 0.             01960058
JPC@3     02 W-IMPOR1             PIC S9(13)V9(02) VALUE 0.             01970058
JPC@3     02 W-IMPOR2             PIC S9(13)V9(02) VALUE 0.             01980058
JPC@3     02 W-IMPOR3             PIC S9(13)V9(02) VALUE 0.             01990058
JPC@3     02 W-IMPOR4             PIC S9(13)V9(02) VALUE 0.             02000058
JPC@3     02 W-IMPOR5             PIC S9(13)V9(02) VALUE 0.             02010058
JPC@3     02 W-IMPOR6             PIC S9(13)V9(02) VALUE 0.             02020058
JPC@3     02 W-IMPOR7             PIC S9(13)V9(02) VALUE 0.             02030058
JPC@3     02 W-IMPOR8             PIC S9(13)V9(02) VALUE 0.             02040058
JPC@3     02 W-IMPOR9             PIC S9(13)V9(02) VALUE 0.             02050058
                                                                        02060058
       01  W-HORA-CURRENT.                                              02070058
           02  W-HORA             PIC 99.                               02080058
           02  W-MINUTOS          PIC 99.                               02090058
           02  W-SEGUNDOS         PIC 99.                               02100058
       01  W-HORA-CURRENT-N REDEFINES W-HORA-CURRENT PIC 9(6).          02110058
      *                                                                 02120058
       01  FECHA-SYSIN.                                                 02130058
           03  DD-SYS             PIC 99.                               02140058
           03  MM-SYS             PIC 99.                               02150058
           03  AAAA-SYS           PIC 9999.                             02160058
      *                                                                 02170058
       01  W-FECHA-AMD.                                                 02180058
           05 W-AA-AMD            PIC 9(4).                             02190058
           05 W-MM-AMD            PIC 9(2).                             02200058
           05 W-DD-AMD            PIC 9(2).                             02210058
       01  W-FECHA-AMD-N REDEFINES W-FECHA-AMD PIC 9(8).                02220058
       01  W-FECHA-FFRUEDA-1      PIC 9(8).                             02230058
       01  W-FECHA-FFRUEDA-2      PIC 9(8).                             02240058
       01  W-FECHA-FFRUEDA-3      PIC 9(8).                             02250058
      *                                                                 02260058
      ******************************************************************02270058
      *                       CONTADORES                               *02280058
      ******************************************************************02290058
       01  WSA-CONTADORES.                                              02300058
           02 WSA-CONTA-OPE       PIC  9(04)      VALUE ZEROS.          02310058
           02 WSA-CONTA-DET       PIC  9(04)      VALUE ZEROS.          02320058
      *************************************************************     02330058
      *        SWITCHES                                                 02340058
      *************************************************************     02350058
JPC@3  01  SW-MONEDA-CRUZADA          PIC  X(01)   VALUE 'N'.           02360058
JPC@3      88 MONEDA-CRUZADA                       VALUE 'S'.           02370058
                                                                        02380058
JPC@3  01  SW-MONEDA-EXTRANJ          PIC  X(01)   VALUE 'N'.           02390058
JPC@3      88 MONEDA-EXTRANJ                       VALUE 'S'.           02400058
                                                                        02410058
JPC@3  01  SW-COMVEN                  PIC  X(01)   VALUE SPACES.        02420058
JPC@3      88 COMPRA                               VALUE 'C'.           02430058
JPC@3      88 VENTA                                VALUE 'V'.           02440058
                                                                        02450058
JPC@3  01  SW-OPE-ABOCAR              PIC  9(02)   VALUE ZEROS.         02460058
JPC@3      88 OPE-ABONO                            VALUE 01 02 07 08 11 02470058
JPC@3                                                    32 37 03.      02480058
JPC@3      88 OPE-CARGO                            VALUE 21 22 34 35 36 02490058
JPC@3                                                    38 39 49 60 61 02500058
JPC@3                                                    66 67 68 69 70 02510058
JPC@3                                                    71 72 73 81 92 02520058
JPC@3                                                    93 94 33 48 23.02530058
                                                                        02540058
       01  FILE-STATUS.                                                 02550058
           02  FS-DMT                  PIC  99     VALUE ZEROS.         02560058
           02  SW-FIN                  PIC   9     VALUE ZEROS.         02570058
                                                                        02580058
      *>> INDICA FIN DE CURSOR.                                         02590058
       01  SW-FIN-CUR-OPE              PIC X       VALUE SPACE.         02600058
           88 SW-SI-FIN-CUR-OPE                    VALUE 'S'.           02610058
           88 SW-NO-FIN-CUR-OPE                    VALUE 'N'.           02620058
                                                                        02630058
       01  SW-FIN-CUR-DET              PIC X       VALUE SPACE.         02640058
           88 SW-SI-FIN-CUR-DET                    VALUE 'S'.           02650058
           88 SW-NO-FIN-CUR-DET                    VALUE 'N'.           02660058
                                                                        02670058
       01  SW-EXEC-SQL-OK              PIC X       VALUE SPACE.         02680058
           88 SW-EXEC-SQL-OK-SI                    VALUE 'S'.           02690058
           88 SW-EXEC-SQL-OK-NO                    VALUE 'N'.           02700058
                                                                        02710058
      **** COPY DE LA COPY DE DATOS PARA LA MIR                         02720058
       01  REG-G10FMIR.                                                 02730058
           COPY BGECMIR.                                                02740058
                                                                        02750058
      **** AREA DE COMUNICACIONES RUTINAS                               02760058
           COPY QRECDB2.                                                02770058
           COPY VLWC8000.                                               02780058
           COPY VLWC8010.                                               02790058
           COPY VLWC8020.                                               02800058
           COPY VLWCRUTI.                                               02810058
                                                                        02820058
      **** COPYS DE DATOS ***                                           02830058
      *    COPY VLECFDE1.                                               02840058
 VCH  *    COPY VLECFDE0.                                               02850058
           COPY VLECFDEA.                                               02860058
           COPY VLECFOP9.                                               02870058
           COPY VLECFOP1.                                               02880058
                                                                        02890058
      ******************************************************************02900058
      ********* REGISTRO DE ENTRADA - SALIDA                 ***********02910058
      ******************************************************************02920058
      *                                                                *02930058
       01  REG-VLLS3050.                                                02940058
           03 L01-VLLS3050.                                             02950058
      *IGH-INI                                                          02960058
      *       05  FILLER        PIC X(20) VALUE ' BANCO CONTINENTAL  '. 02970058
              05  FILLER        PIC X(20) VALUE ' BBVA               '. 02980058
      *IGH-FIN                                                          02990058
              05  FILLER        PIC X(80) VALUE SPACES.                 03000058
              05  FILLER        PIC X(11) VALUE '     FECHA '.          03010058
              05  CAB-DD        PIC 9(02).                              03020058
              05  FILLER        PIC X(01) VALUE '-'.                    03030058
              05  CAB-MM        PIC 9(02).                              03040058
              05  FILLER        PIC X(01) VALUE '-'.                    03050058
              05  CAB-AAAA      PIC 9(04).                              03060058
              05  FILLER        PIC X(06) VALUE ' HORA '.               03070058
              05  CAB-HORA      PIC X(02).                              03080058
              05  FILLER        PIC X(01) VALUE ':'.                    03090058
              05  CAB-MINU      PIC X(02).                              03100058
           03 L02-VLLS3050.                                             03110058
              05  FILLER        PIC X(01) VALUE SPACES.                 03120058
              05  CAB-OPER      PIC X(19) VALUE SPACES.                 03130058
              05  FILLER        PIC X(02) VALUE SPACES.                 03140058
              05  CAB-ISIN      PIC X(12) VALUE SPACES.                 03150058
              05  FILLER        PIC X(07) VALUE SPACES.                 03160058
              05  FILLER        PIC X(31) VALUE                         03170058
                               'LIQUIDA OPERACION FINANCIERA : '.       03180058
              05  CAB-DIA       PIC 9(02).                              03190058
              05  FILLER        PIC X(01) VALUE '-'.                    03200058
              05  CAB-MES       PIC 9(02).                              03210058
              05  FILLER        PIC X(01) VALUE '-'.                    03220058
              05  CAB-AYO       PIC 9(04).                              03230058
              05  FILLER        PIC X(18) VALUE SPACES.                 03240058
              05  FILLER        PIC X(20) VALUE '      PROG  VL4C3050'. 03250058
              05  FILLER        PIC X(08) VALUE '  PAG.  '.             03260058
              05  CAB2-PAG      PIC ZZZ9.                               03270058
           03 L03-VLLS3050.                                             03280058
              05  FILLER        PIC X(20)  VALUE ' -------------------'.03290058
              05  FILLER        PIC X(20)  VALUE '--------------------'.03300058
              05  FILLER        PIC X(20)  VALUE '--------------------'.03310058
              05  FILLER        PIC X(20)  VALUE '--------------------'.03320058
              05  FILLER        PIC X(20)  VALUE '--------------------'.03330058
              05  FILLER        PIC X(20)  VALUE '--------------------'.03340058
              05  FILLER        PIC X(13)  VALUE '-------------'.       03350058
           03 L04-VLLS3050.                                             03360058
01            05  FILLER        PIC X(01)  VALUE SPACES.                03370058
08            05  FILLER        PIC X(09)  VALUE 'CTA-VALOR'.           03380058
12            05  FILLER        PIC X(02)  VALUE SPACES.                03390058
21            05  FILLER        PIC X(10)  VALUE 'REFERENCIA'.          03400058
23            05  FILLER        PIC X(01)  VALUE SPACES.                03410058
58            05  FILLER        PIC X(30)  VALUE ' CLIENTE'.            03420058
54            05  FILLER        PIC X(01)  VALUE SPACES.                03430058
64            05  FILLER        PIC X(10)  VALUE 'TITULOS   '.          03440058
65            05  FILLER        PIC X(01)  VALUE SPACES.                03450058
78            05  FILLER        PIC X(13)  VALUE 'COMISIONES   '.       03460058
79            05  FILLER        PIC X(01)  VALUE SPACES.                03470058
92            05  FILLER        PIC X(13)  VALUE ' IMPORTE-LIQ.'.       03480058
93            05  FILLER        PIC X(01)  VALUE SPACES.                03490058
113           05  FILLER        PIC X(20)  VALUE 'CTA. ECONOMICA      '.03500058
114           05  FILLER        PIC X(01)  VALUE SPACES.                03510058
117           05  FILLER        PIC X(03)  VALUE 'MON'.                 03520058
119           05  FILLER        PIC X(02)  VALUE SPACES.                03530058
131           05  FILLER        PIC X(12)  VALUE 'OBSERVACION '.        03540058
           03 L05-VLLS3050.                                             03550058
              05  FILLER        PIC X(20)  VALUE ' -------------------'.03560058
              05  FILLER        PIC X(20)  VALUE '--------------------'.03570058
              05  FILLER        PIC X(20)  VALUE '--------------------'.03580058
              05  FILLER        PIC X(20)  VALUE '--------------------'.03590058
              05  FILLER        PIC X(20)  VALUE '--------------------'.03600058
              05  FILLER        PIC X(20)  VALUE '--------------------'.03610058
              05  FILLER        PIC X(13)  VALUE '-------------'.       03620058
           03 L06-VLLS3050.                                             03630058
01            05  FILLER        PIC X(01)  VALUE SPACES.                03640058
08            05  DET1-CTAVAL   PIC 9(07).                              03650058
09            05  DET1-RAYA     PIC X(01)  VALUE '-'.                   03660058
10            05  DET1-CTAVAL-D PIC 9(01).                              03670058
12            05  FILLER        PIC X(02)  VALUE SPACES.                03680058
21            05  DET1-REFERE   PIC 9(09).                              03690058
23            05  FILLER        PIC X(02)  VALUE SPACES.                03700058
58            05  DET1-CLIENT   PIC X(30).                              03710058
54            05  FILLER        PIC X(01)  VALUE SPACES.                03720058
64            05  DET1-CANACC   PIC ZZZZZZ,ZZ9.                         03730058
65            05  FILLER        PIC X(01)  VALUE SPACES.                03740058
78            05  DET1-COMISI   PIC ZZ,ZZZ,ZZ9.99.                      03750058
79            05  FILLER        PIC X(01)  VALUE SPACES.                03760058
92            05  DET1-IMPORT   PIC ZZ,ZZZ,ZZ9.99.                      03770058
93            05  FILLER        PIC X(01)  VALUE SPACES.                03780058
113           05  DET1-CTAECO   PIC X(20).                              03790058
114           05  FILLER        PIC X(01)  VALUE SPACES.                03800058
117           05  DET1-MONCTA   PIC X(03).                              03810058
119           05  FILLER        PIC X(02)  VALUE SPACES.                03820058
131           05  DET1-OBSERV   PIC X(12).                              03830058
           03 L07-VLLS3050.                                             03840058
              05  FILLER        PIC X(40)  VALUE SPACES.                03850058
              05  FILLER        PIC X(20)  VALUE '***   F I N A L   D '.03860058
              05  FILLER        PIC X(20)  VALUE 'E L   L I S T A D O '.03870058
              05  FILLER        PIC X(20)  VALUE '  VL4C3050 ***      '.03880058
      *                                                                *03890058
      ******************************************************************03900058
      *                   INCLUDES TABLAS DB2.                       ***03910058
      ******************************************************************03920058
           EXEC SQL  INCLUDE   SQLCA        END-EXEC.                   03930058
 VCH  *    EXEC SQL  INCLUDE  VLGTXEN       END-EXEC.                   03940058
           EXEC SQL  INCLUDE  VLGTXEN1      END-EXEC.                   03950058
           EXEC SQL  INCLUDE  VLGTDET       END-EXEC.                   03960058
 VCH  *    EXEC SQL  INCLUDE  VLGTOPE       END-EXEC.                   03970058
           EXEC SQL  INCLUDE  VLGTOPE1      END-EXEC.                   03980058
JPC@1      EXEC SQL  INCLUDE  VLGTXBO       END-EXEC.                   03990058
JPC@1      EXEC SQL  INCLUDE  VLGTCON1      END-EXEC.                   04000058
JPC@1      EXEC SQL  INCLUDE  VLGTARC       END-EXEC.                   04010058
JPC@2      EXEC SQL  INCLUDE  VLGTXMI       END-EXEC.                   04020058
JPC@2      EXEC SQL  INCLUDE  VLGTREL       END-EXEC.                   04030058
                                                                        04040058
 VCH  *    EXEC SQL  INCLUDE  VLTCXEN       END-EXEC.                   04050058
           EXEC SQL  INCLUDE  VLTCXEN1      END-EXEC.                   04060058
           EXEC SQL  INCLUDE  VLTCDET       END-EXEC.                   04070058
 VCH  *    EXEC SQL  INCLUDE  VLTCOPE       END-EXEC.                   04080058
           EXEC SQL  INCLUDE  VLTCOPE1      END-EXEC.                   04090058
                                                                        04100058
      *    ----------------------------------------------------------- *04110058
      *                  CURSOR VLDTOPE-01                             *04120058
      *    ----------------------------------------------------------- *04130058
           EXEC SQL                                                     04140058
                DECLARE  VLDTOPE-01 CURSOR FOR                          04150058
                 SELECT  VOPE_PAVAL       , VOPE_VALOR       ,          04160058
                         VOPE_ISIN        , VOPE_FECHOP      ,          04170058
                         VOPE_FORMAT      , VOPE_TIPOP       ,          04180058
                         VOPE_CUPON       , VOPE_SITUAC      ,          04190058
                         VOPE_AVANCE      , VOPE_RETROC      ,          04200058
                         VOPE_FCONTA      , VOPE_FJUSTI      ,          04210058
                         VOPE_TOTIMP      , VOPE_TITSCON     ,          04220058
                         VOPE_TITSJUS     , VOPE_TITSPTE     ,          04230058
                         VOPE_DATOS_FINANZ, VOPE_REAL_FICTI  ,          04240058
                         VOPE_LETRAA      , VOPE_LETRAG      ,          04250058
                         VOPE_FVALOR      , VOPE_BANQUE      ,          04260058
                         VOPE_FEALTREG    , VOPE_FEULMOD     ,          04270058
                         VOPE_HORULMOD    , VOPE_NUMTER      ,          04280058
                         VOPE_USUARIO                                   04290058
                   FROM  VLDTOPE                                        04300058
                  WHERE VOPE_FCONTA =:VOPE-FCONTA                       04310058
           END-EXEC.                                                    04320058
      *    ----------------------------------------------------------- *04330058
      *                  CURSORES VLDTDET-01                   *        04340058
      *    ----------------------------------------------------------- *04350058
           EXEC SQL                                                     04360058
                DECLARE  VLDTDET-01 CURSOR FOR                          04370058
                 SELECT  VDET_FECHOP     , VDET_PAVAL     ,             04380058
                         VDET_VALOR      , VDET_ISIN      ,             04390058
                         VDET_FORMAT     , VDET_CTAVAL    ,             04400058
                         VDET_CLAREG     , VDET_REFER     ,             04410058
                         VDET_DATOS_DETAL, VDET_FEALTREG  ,             04420058
                         VDET_FEULMOD    , VDET_HORULMOD  ,             04430058
                         VDET_NUMTER     , VDET_USUARIO                 04440058
                   FROM  VLDTDET                                        04450058
                  WHERE  VDET_FECHOP  = :VDET-FECHOP                    04460058
                    AND  VDET_PAVAL   = :VDET-PAVAL                     04470058
                    AND  VDET_VALOR   = :VDET-VALOR                     04480058
                    AND  VDET_ISIN    = :VDET-ISIN                      04490058
                    AND  VDET_FORMAT  = :VDET-FORMAT                    04500058
                    AND  VDET_CTAVAL >  :VDET-CTAVAL                    04510058
                    AND  VDET_CLAREG >= :VDET-CLAREG                    04520058
                    AND  VDET_REFER  >= :VDET-REFER                     04530058
                  ORDER  BY VDET_CTAVAL ASC                             04540058
           END-EXEC.                                                    04550058
                                                                        04560058
      *==================*                                              04570058
       PROCEDURE DIVISION.                                              04580058
      *==================*                                              04590058
           PERFORM 1-INICIO                                             04600058
           PERFORM 2-PROCESO                                            04610058
           PERFORM 3-FINAL                                              04620058
           STOP RUN.                                                    04630058
                                                                        04640058
       1-INICIO.                                                        04650058
      *========*                                                        04660058
      *  - SE ACEPTA LA FECHA DEL SYSTEM                                04670058
JPC@3      ACCEPT W-DATE FROM DATE                                      04680058
JPC@3      MOVE   W-DIA            TO DD-SYS                            04690058
JPC@3      MOVE   W-MES            TO MM-SYS                            04700058
JPC@3      ADD    2000, W-ANO  GIVING AAAA-SYS                          04710058
                                                                        04720058
JPC@3 *    ACCEPT FECHA-SYSIN.                                          04730058
           MOVE   DD-SYS           TO  W-DD-AMD CAB-DD,   CAB-DIA.      04740058
           MOVE   MM-SYS           TO  W-MM-AMD CAB-MM,   CAB-MES.      04750058
           MOVE   AAAA-SYS         TO  W-AA-AMD CAB-AAAA, CAB-AYO.      04760058
           ACCEPT W-HORA-CURRENT  FROM  TIME.                           04770058
           MOVE   W-HORA           TO  CAB-HORA                         04780058
           MOVE   W-MINUTOS        TO  CAB-MINU                         04790058
      *                                                                *04800058
           PERFORM SELECT-VLDTXMI.                                      04810058
      *                                                                *04820058
           INITIALIZE                    DCLVLDTCON.                    04830058
           MOVE 01                   TO  VXBO-CLABOL.                   04840058
           PERFORM SELECT-VLDTXBO.                                      04850058
           MOVE VXBO-CTAECOS         TO  WSAB-CTAECOS.                  04860058
           MOVE VXBO-CTAECOD         TO  WSAB-CTAECOD.                  04870058
                                                                        04880058
           MOVE WSAB-CTAECOS(01:04)  TO  MDC-ENTIDAD.                   04890058
           MOVE WSAB-CTAECOS(05:04)  TO  MDC-CENTRO-ALTA.               04900058
           MOVE WSAB-CTAECOS(11:10)  TO  MDC-CUENTA.                    04910058
           PERFORM CALL-BG9CMDC0.                                       04920058
           MOVE MDC-DIGICCC1         TO  WSAB-CTAECOS (09:01).          04930058
           MOVE MDC-DIGICCC2         TO  WSAB-CTAECOS (10:01).          04940058
                                                                        04950058
           MOVE WSAB-CTAECOD(01:04)  TO  MDC-ENTIDAD.                   04960058
           MOVE WSAB-CTAECOD(05:04)  TO  MDC-CENTRO-ALTA.               04970058
           MOVE WSAB-CTAECOD(11:10)  TO  MDC-CUENTA.                    04980058
           PERFORM CALL-BG9CMDC0.                                       04990058
           MOVE MDC-DIGICCC1         TO  WSAB-CTAECOD (09:01).          05000058
           MOVE MDC-DIGICCC2         TO  WSAB-CTAECOD (10:01).          05010058
      *                                                                *05020058
           MOVE 99                   TO  VXBO-CLABOL.                   05030058
           PERFORM SELECT-VLDTXBO.                                      05040058
      *                                                                *05050058
           OPEN OUTPUT VLLS3050.                                        05060058
      *                                                                *05070058
       2-PROCESO.                                                       05080058
      *=========                                                        05090058
           PERFORM PROCESO-CURSOR-OPE.                                  05100058
                                                                        05110058
       PROCESO-CURSOR-OPE.                                              05120058
      *==================*                                              05130058
           PERFORM 200100-OPEN-CUR-OPE                                  05140058
           PERFORM 200200-FETCH-CUR-OPE UNTIL SW-SI-FIN-CUR-OPE         05150058
           PERFORM 200300-CLOSE-CUR-OPE.                                05160058
                                                                        05170058
       200100-OPEN-CUR-OPE.                                             05180058
      *===================*                                             05190058
JPC@2 *    MOVE 'S'                        TO VOPE-FORMAT               05200058
JPC@2 *    MOVE 'A'                        TO VOPE-TIPOP                05210058
JPC@2      INITIALIZE                         DCLVLDTOPE                05220058
JPC@2      MOVE W-FECHA-AMD-N              TO VOPE-FCONTA               05230058
                                                                        05240058
           EXEC SQL OPEN VLDTOPE-01 END-EXEC.                           05250058
                                                                        05260058
           EVALUATE  SQLCODE                                            05270058
               WHEN (ZERO)                                              05280058
                     CONTINUE                                           05290058
               WHEN  OTHER                                              05300058
                     MOVE CT-VL4C3050           TO  W801-PROGRAMA       05310058
                     MOVE 'VLDTOPE'             TO  W801-TABLA          05320058
                     MOVE 'OPEN  '              TO  W801-ACCION         05330058
                     MOVE  VOPE-FORMAT          TO  W801-CLAVE(1:1)     05340058
                     MOVE  VOPE-TIPOP           TO  W801-CLAVE(2:1)     05350058
                     MOVE  SQLCODE              TO  W801-SQLCODE        05360058
                     MOVE  SPACES               TO  W801-SQLWARN        05370058
                     MOVE '200100-OPEN-CUR-OPE' TO  W801-PARRAFO        05380058
                     PERFORM  VLPC8010-DISP-ABEND-DB2                   05390058
                     PERFORM  VLPC8010-ABEND-DB2                        05400058
           END-EVALUATE.                                                05410058
                                                                        05420058
       200200-FETCH-CUR-OPE.                                            05430058
      *====================*                                            05440058
           EXEC SQL FETCH VLDTOPE-01 INTO                               05450058
                           :VOPE-PAVAL       , :VOPE-VALOR       ,      05460058
                           :VOPE-ISIN        , :VOPE-FECHOP      ,      05470058
                           :VOPE-FORMAT      , :VOPE-TIPOP       ,      05480058
                           :VOPE-CUPON       , :VOPE-SITUAC      ,      05490058
                           :VOPE-AVANCE      , :VOPE-RETROC      ,      05500058
                           :VOPE-FCONTA      , :VOPE-FJUSTI      ,      05510058
                           :VOPE-TOTIMP      , :VOPE-TITSCON     ,      05520058
                           :VOPE-TITSJUS     , :VOPE-TITSPTE     ,      05530058
                           :VOPE-DATOS-FINANZ, :VOPE-REAL-FICTI  ,      05540058
                           :VOPE-LETRAA      , :VOPE-LETRAG      ,      05550058
                           :VOPE-FVALOR      , :VOPE-BANQUE      ,      05560058
                           :VOPE-FEALTREG    , :VOPE-FEULMOD     ,      05570058
                           :VOPE-HORULMOD    , :VOPE-NUMTER      ,      05580058
                           :VOPE-USUARIO                                05590058
           END-EXEC.                                                    05600058
                                                                        05610058
           MOVE 'VLDTOPE'            TO  W801-TABLA                     05620058
           PERFORM 200210-EVALUAR-SQLCODE                               05630058
           PERFORM 200220-CONTINUACION-FETCH.                           05640058
                                                                        05650058
       200210-EVALUAR-SQLCODE.                                          05660058
      *======================*                                          05670058
           EVALUATE  SQLCODE                                            05680058
               WHEN (ZEROS)                                             05690058
                     SET SW-EXEC-SQL-OK-SI TO TRUE                      05700058
               WHEN (+100)                                              05710058
                     SET SW-EXEC-SQL-OK-NO TO TRUE                      05720058
               WHEN  OTHER                                              05730058
                     SET SW-EXEC-SQL-OK-NO TO TRUE                      05740058
                     MOVE CT-VL4C3050           TO  W801-PROGRAMA       05750058
                     MOVE 'FETCH '              TO  W801-ACCION         05760058
                     MOVE  SPACES               TO  W801-CLAVE          05770058
                     MOVE  SQLCODE              TO  W801-SQLCODE        05780058
                     MOVE  SPACES               TO  W801-SQLWARN        05790058
                     MOVE '200210-EVALUAR-SQLCODE'                      05800058
                                                TO  W801-PARRAFO        05810058
                     PERFORM  VLPC8010-DISP-ABEND-DB2                   05820058
                     PERFORM  VLPC8010-ABEND-DB2                        05830058
           END-EVALUATE.                                                05840058
                                                                        05850058
       200220-CONTINUACION-FETCH.                                       05860058
      *=========================*                                       05870058
           IF SW-EXEC-SQL-OK-SI                                         05880058
              ADD 1                         TO WA-LEIDOS-OPE            05890058
              MOVE VOPE-DATOS-FINANZ        TO  V-DATOS-FINANZ1         05900058
              MOVE VOPE-DATOS-FINANZ        TO V1-DATOS-FINANZ1         05910058
              MOVE VOPE-FORMAT              TO SW-OPE-FINANCIERA (01:01)05920058
              MOVE VOPE-TIPOP               TO SW-OPE-FINANCIERA (02:01)05930058
              EVALUATE TRUE                                             05940058
                  WHEN OPE-SUSCRIPCION                                  05950058
                       MOVE VOPE-SUS-FCORUEDA1     TO W-FECHA-FFRUEDA-1 05960058
                       MOVE VOPE-SUS-FCORUEDA2     TO W-FECHA-FFRUEDA-2 05970058
                       MOVE VOPE-SUS-FCORUEDA3     TO W-FECHA-FFRUEDA-3 05980058
                       IF ( W-FECHA-AMD EQUAL  W-FECHA-FFRUEDA-1   AND  05990058
                            VOPE-SUS-CORUEDA1  = 'S' )                  06000058
                       OR ( W-FECHA-AMD EQUAL  W-FECHA-FFRUEDA-2   AND  06010058
                            VOPE-SUS-CORUEDA2  = 'S' )                  06020058
                       OR ( W-FECHA-AMD EQUAL  W-FECHA-FFRUEDA-3   AND  06030058
                            VOPE-SUS-CORUEDA3  = 'S' )                  06040058
                            SET SW-PROCESO-SI          TO TRUE          06050058
                       END-IF                                           06060058
      *           WHEN OPE-DIVIDENDOS                                   06070058
                  WHEN OPE-INTERESES                                    06080058
                  WHEN OPE-AMORTIZACION                                 06090058
                       IF VOPE-FCONTA = W-FECHA-AMD-N AND               06100058
                          VOPE-SITUAC = '1'                             06110058
                          SET SW-PROCESO-SI          TO TRUE            06120058
                       END-IF                                           06130058
                  WHEN OTHER                                            06140058
                       SET SW-PROCESO-NO             TO TRUE            06150058
              END-EVALUATE                                              06160058
              SET SW-NO-FIN-CUR-OPE      TO TRUE                        06170058
              ADD  1                     TO WA-LEIDOS-OPE-OK            06180058
              IF SW-PROCESO-SI                                          06190058
                 ADD  1                  TO WA-PROCES-OPE-OK            06200058
                 PERFORM PROCESO-CURSOR-DET                             06210058
              END-IF                                                    06220058
           ELSE                                                         06230058
              SET SW-SI-FIN-CUR-OPE        TO TRUE                      06240058
           END-IF.                                                      06250058
                                                                        06260058
       PROCESO-CURSOR-DET.                                              06270058
      *==================*                                              06280058
           MOVE ZEROS                   TO WT-COMISION.                 06290058
           MOVE ZEROS                   TO WE-COMISION.                 06300058
           MOVE ZEROS                   TO WT-IMPLIQ.                   06310058
           MOVE ZEROS                   TO WE-IMPLIQ.                   06320058
           MOVE 80                      TO CONT-LIN.                    06330058
           PERFORM SELECT-VLDTXEN.                                      06340058
JPC@2      PERFORM SELECT-VLDTREL.                                      06350058
           SET SW-NO-FIN-CUR-DET        TO TRUE                         06360058
           PERFORM 200400-OPEN-CUR-DET                                  06370058
           PERFORM 200500-FETCH-CUR-DET UNTIL SW-SI-FIN-CUR-DET         06380058
           PERFORM 200600-CLOSE-CUR-DET.                                06390058
           PERFORM 200700-IMPRIME-TOTAL.                                06400058
JPC@2      SET SW-PROCESO-NO            TO TRUE.                        06410058
       200400-OPEN-CUR-DET.                                             06420058
      *===================*                                             06430058
           MOVE VOPE-FECHOP          TO  VDET-FECHOP                    06440058
           MOVE VOPE-PAVAL           TO  VDET-PAVAL                     06450058
           MOVE VOPE-VALOR           TO  VDET-VALOR                     06460058
           MOVE VOPE-ISIN            TO  VDET-ISIN                      06470058
           MOVE VOPE-FORMAT          TO  VDET-FORMAT                    06480058
           MOVE ZEROS                TO  VDET-CTAVAL                    06490058
           MOVE SPACES               TO  VDET-CLAREG                    06500058
           MOVE ZEROS                TO  VDET-REFER                     06510058
                                                                        06520058
           EXEC SQL OPEN VLDTDET-01 END-EXEC.                           06530058
                                                                        06540058
           EVALUATE  SQLCODE                                            06550058
               WHEN (ZERO)                                              06560058
                     CONTINUE                                           06570058
               WHEN  OTHER                                              06580058
                     MOVE CT-VL4C3050           TO  W801-PROGRAMA       06590058
                     MOVE 'VLDTDET'             TO  W801-TABLA          06600058
                     MOVE 'OPEN  '              TO  W801-ACCION         06610058
                     MOVE  VDET-PAVAL           TO  W801-CLAVE(01:03)   06620058
                     MOVE  VDET-VALOR           TO  W801-CLAVE(04:08)   06630058
                     MOVE  VDET-ISIN            TO  W801-CLAVE(13:01)   06640058
                     MOVE  VDET-FECHOP          TO  W801-CLAVE(14:10)   06650058
                     MOVE  SQLCODE              TO  W801-SQLCODE        06660058
                     MOVE  SPACES               TO  W801-SQLWARN        06670058
                     MOVE '200100-OPEN-CUR-OPE' TO  W801-PARRAFO        06680058
                     PERFORM  VLPC8010-DISP-ABEND-DB2                   06690058
                     PERFORM  VLPC8010-ABEND-DB2                        06700058
           END-EVALUATE.                                                06710058
                                                                        06720058
       200500-FETCH-CUR-DET.                                            06730058
      *====================*                                            06740058
           EXEC SQL FETCH VLDTDET-01 INTO                               06750058
                            :VDET-FECHOP     , :VDET-PAVAL     ,        06760058
                            :VDET-VALOR      , :VDET-ISIN      ,        06770058
                            :VDET-FORMAT     , :VDET-CTAVAL    ,        06780058
                            :VDET-CLAREG     , :VDET-REFER     ,        06790058
                            :VDET-DATOS-DETAL, :VDET-FEALTREG  ,        06800058
                            :VDET-FEULMOD    , :VDET-HORULMOD  ,        06810058
                            :VDET-NUMTER     , :VDET-USUARIO            06820058
           END-EXEC.                                                    06830058
                                                                        06840058
           MOVE 'VLDTDET'            TO  W801-TABLA.                    06850058
JPC@4      MOVE ZEROS                TO  W-WOPS-TIPO-CAMBIO.            06860058
JPC@4      INITIALIZE                    W-TC-GP8C1950.                 06870058
           PERFORM 200210-EVALUAR-SQLCODE.                              06880058
           PERFORM 200310-CONTINUACION-FETCH.                           06890058
                                                                        06900058
       200310-CONTINUACION-FETCH.                                       06910058
      *=========================*                                       06920058
           IF SW-EXEC-SQL-OK-SI                                         06930058
              ADD 1                           TO WA-LEIDOS-DET          06940058
              MOVE VDET-DATOS-DETAL           TO V-DATOS-DETAL          06950058
                                                                        06960058
              IF VDET-DATOS-DETAL (107:1) NOT = 'P'                     06970058
                 IF OPE-SUSCRIPCION                                     06980058
                    IF (VDET-SUS-ORDSUS > ZEROS           ) AND         06990058
                       (VDET-SUS-CTAECO  NOT = VXBO-CTAECOS AND         07000058
                                               VXBO-CTAECOD AND         07010058
                                               VXMI-CTACARGO)           07020058
                       ADD 1                  TO WA-LEIDOS-DET-OK       07030058
                       PERFORM PROCESO-CONTABLE                         07040058
                       PERFORM PROCESO-MIR                              07050058
                       PERFORM 610-IMPRIME-OPE                          07060058
                       IF VCON-NUMCTA (11:02) = '91'                    07070058
                          PERFORM PROCESO-MIR-SAB                       07080058
                       END-IF                                           07090058
                    END-IF                                              07100058
                 ELSE                                                   07110058
                    IF (VDET-PTS-COMVEN > ZEROS           ) AND         07120058
                       (VDET-NUMCTA      NOT = VXBO-CTAECOS AND         07130058
                                               VXBO-CTAECOD AND         07140058
                                               VXMI-CTACARGO)           07150058
                       ADD 1                        TO WA-LEIDOS-DET-OK 07160058
                       PERFORM PROCESO-CONTABLE2                        07170058
                       PERFORM PROCESO-MIR2                             07180058
                       PERFORM 610-IMPRIME-OPE                          07190058
                       IF VCON-NUMCTA (11:02) = '91'                    07200058
                          PERFORM PROCESO-MIR-SAB                       07210058
                       END-IF                                           07220058
                    END-IF                                              07230058
                 END-IF                                                 07240058
              END-IF                                                    07250058
           ELSE                                                         07260058
              SET SW-SI-FIN-CUR-DET        TO TRUE                      07270058
           END-IF.                                                      07280058
                                                                        07290058
       200600-CLOSE-CUR-DET.                                            07300058
      *====================*                                            07310058
           EXEC SQL CLOSE VLDTDET-01 END-EXEC.                          07320058
                                                                        07330058
           EVALUATE  SQLCODE                                            07340058
               WHEN (ZERO)                                              07350058
                     CONTINUE                                           07360058
               WHEN  OTHER                                              07370058
                     MOVE CT-VL4C3050           TO  W801-PROGRAMA       07380058
                     MOVE 'VLDTDET'             TO  W801-TABLA          07390058
                     MOVE 'CLOSET'              TO  W801-ACCION         07400058
                     MOVE  VDET-PAVAL           TO  W801-CLAVE(01:03)   07410058
                     MOVE  VDET-VALOR           TO  W801-CLAVE(04:08)   07420058
                     MOVE  VDET-ISIN            TO  W801-CLAVE(13:01)   07430058
                     MOVE  VDET-FECHOP          TO  W801-CLAVE(14:10)   07440058
                     MOVE  SQLCODE              TO  W801-SQLCODE        07450058
                     MOVE  SPACES               TO  W801-SQLWARN        07460058
                     MOVE '200600-CLOSE-CUR-DET'                        07470058
                                                TO  W801-PARRAFO        07480058
                     PERFORM  VLPC8010-DISP-ABEND-DB2                   07490058
                     PERFORM  VLPC8010-ABEND-DB2                        07500058
           END-EVALUATE.                                                07510058
                                                                        07520058
       200300-CLOSE-CUR-OPE.                                            07530058
      *====================*                                            07540058
           EXEC SQL CLOSE VLDTOPE-01 END-EXEC.                          07550058
                                                                        07560058
           EVALUATE  SQLCODE                                            07570058
               WHEN (ZERO)                                              07580058
                     CONTINUE                                           07590058
               WHEN  OTHER                                              07600058
                     MOVE CT-VL4C3050            TO  W801-PROGRAMA      07610058
                     MOVE 'VLDTOPE'              TO  W801-TABLA         07620058
                     MOVE 'CLOSE '               TO  W801-ACCION        07630058
                     MOVE  VOPE-FORMAT           TO  W801-CLAVE(1:1)    07640058
                     MOVE  VOPE-TIPOP            TO  W801-CLAVE(2:1)    07650058
                     MOVE  SQLCODE               TO  W801-SQLCODE       07660058
                     MOVE  SPACES                TO  W801-SQLWARN       07670058
                     MOVE '200300-CLOSE-CUR-OPE' TO  W801-PARRAFO       07680058
                     PERFORM  VLPC8010-DISP-ABEND-DB2                   07690058
                     PERFORM  VLPC8010-ABEND-DB2                        07700058
           END-EVALUATE.                                                07710058
                                                                        07720058
       SELECT-VLDTXEN.                                                  07730058
      *==============*                                                  07740058
           MOVE VOPE-PAVAL                 TO VXEN-PAVAL                07750058
           MOVE VOPE-VALOR                 TO VXEN-VALOR                07760058
           MOVE VOPE-ISIN                  TO VXEN-ISIN                 07770058
      *                                                                 07780058
           EXEC SQL                                                     07790058
                SELECT  VXEN_NEMOTEC                                    07800058
                     ,  VXEN_CODDIVI                                    07810058
                     ,  VXEN_NOMBRE                                     07820058
                     ,  VXEN_NOMINEM                                    07830058
                     ,  VXEN_IND_A5R                                    07840058
                  INTO :VXEN-NEMOTEC                                    07850058
                     , :VXEN-CODDIVI                                    07860058
                     , :VXEN-NOMBRE                                     07870058
                     , :VXEN-NOMINEM                                    07880058
                     , :VXEN-IND-A5R                                    07890058
                  FROM  VLDTXEN                                         07900058
                 WHERE  VXEN_PAVAL    = :VXEN-PAVAL                     07910058
                   AND  VXEN_VALOR    = :VXEN-VALOR                     07920058
                   AND  VXEN_ISIN     = :VXEN-ISIN                      07930058
           END-EXEC                                                     07940058
                                                                        07950058
           MOVE SQLCODE                        TO SQLCODE-AUX           07960058
           EVALUATE TRUE                                                07970058
               WHEN DB2-OK                                              07980058
                    CONTINUE                                            07990058
               WHEN OTHER                                               08000058
                    MOVE CT-VL4C3050            TO  W801-PROGRAMA       08010058
                    MOVE 'VLDTXEN'              TO  W801-TABLA          08020058
                    MOVE 'SELECT'               TO  W801-ACCION         08030058
                    MOVE  VXEN-PAVAL            TO  W801-CLAVE(01:3)    08040058
                    MOVE  VXEN-VALOR            TO  W801-CLAVE(04:8)    08050058
                    MOVE  VXEN-ISIN             TO  W801-CLAVE(13:1)    08060058
                    MOVE  SQLCODE               TO  W801-SQLCODE        08070058
                    MOVE  SPACES                TO  W801-SQLWARN        08080058
                    MOVE 'SELECT-VLDTXEN'       TO  W801-PARRAFO        08090058
                    PERFORM  VLPC8010-DISP-ABEND-DB2                    08100058
                    PERFORM  VLPC8010-ABEND-DB2                         08110058
           END-EVALUATE.                                                08120058
                                                                        08130058
       SELECT-VLDTREL.                                                  08140058
      *==============*                                                  08150058
           MOVE VXEN-PAVAL                 TO VREL-CODVALOR (01:03)     08160058
           MOVE VXEN-VALOR                 TO VREL-CODVALOR (04:08)     08170058
           MOVE VXEN-ISIN                  TO VREL-CODVALOR (12:01)     08180058
      *                                                                 08190058
           EXEC SQL                                                     08200058
                SELECT VREL_INDICFIS,                                   08210058
                       VREL_CODVALEQ                                    08220058
                 INTO :VREL-INDICFIS,                                   08230058
                      :VREL-CODVALEQ                                    08240058
                  FROM VLDTREL                                          08250058
                 WHERE VREL_CODVALOR = :VREL-CODVALOR                   08260058
           END-EXEC.                                                    08270058
      *                                                                 08280058
           MOVE SQLCODE                        TO SQLCODE-AUX           08290058
           EVALUATE TRUE                                                08300058
               WHEN DB2-OK                                              08310058
                    CONTINUE                                            08320058
               WHEN OTHER                                               08330058
                    MOVE CT-VL4C3050            TO  W801-PROGRAMA       08340058
                    MOVE 'VLDTREL'              TO  W801-TABLA          08350058
                    MOVE 'SELECT'               TO  W801-ACCION         08360058
                    MOVE  VREL-CODVALOR         TO  W801-CLAVE(01:12)   08370058
                    MOVE  SQLCODE               TO  W801-SQLCODE        08380058
                    MOVE  SPACES                TO  W801-SQLWARN        08390058
                    MOVE 'SELECT-VLDTREL'       TO  W801-PARRAFO        08400058
                    PERFORM  VLPC8010-DISP-ABEND-DB2                    08410058
                    PERFORM  VLPC8010-ABEND-DB2                         08420058
           END-EVALUATE.                                                08430058
                                                                        08440058
       PROCESO-MIR.                                                     08450058
      *===========*                                                     08460058
           INITIALIZE  REG-G10FMIR                                      08470058
                       REG-G10FTI0.                                     08480058
                                                                        08490058
JPC@3      IF VCON-MONEDA-CTA NOT = MDC-CDDIVIS                         08500058
JPC@3         PERFORM 200800-TIPO-CAMBIO                                08510058
JPC@3            THRU 200800-TIPO-CAMBIO-FIN                            08520058
JPC@3      END-IF.                                                      08530058
                                                                        08540058
           MOVE VDET-SUS-CTAECO(01:04) TO MIR-ENTIDAD                   08550058
           MOVE VDET-SUS-CTAECO(05:04) TO MIR-CENTRO-ALTA               08560058
JPC@2      MOVE '0567'                 TO MIR-CENTRO-ORI                08570058
           MOVE MDC-DIGICCC1           TO MIR-DIGICCC1                  08580058
           MOVE MDC-DIGICCC2           TO MIR-DIGICCC2                  08590058
           MOVE VDET-SUS-CTAECO(11:10) TO MIR-CUENTA                    08600058
           MOVE '0011'                 TO MIR-ENTIDAD-ORI               08610058
           MOVE CT-VL4C3050            TO MIR-USERID-ORI                08620058
           MOVE SPACES                 TO MIR-NETNAME-ORI               08630058
           MOVE '065'                  TO MIR-CODIGO                    08640058
JPC@3 *    COMPUTE MIR-IMPORTE = VCON-IMPLIQ * -1                       08650058
JPC@3      COMPUTE MIR-IMPORTE =    W-IMPLIQ * -1                       08660058
           MOVE 'OPS'                  TO MIR-TIPO-CONTAB               08670058
           MOVE 'R'                    TO MIR-IND-REALPRUE              08680058
           MOVE '0'                    TO MIR-IND-RETENCI               08690058
           MOVE 'N'                    TO MIR-IND-CARGO-DISP            08700058
           MOVE 'N'                    TO MIR-IND-OPER-INTER            08710058
           MOVE '1'                    TO MIR-IND-ORIGEN-OP             08720058
           MOVE 'S'                    TO MIR-IND-CARTA                 08730058
           MOVE VXEN-CODDIVI           TO MIR-DIVISA                    08740058
           MOVE W-FECHA-AMD(1:4)       TO MIR-FECHA-VALOR(1:4)          08750058
           MOVE '-'                    TO MIR-FECHA-VALOR(5:1)          08760058
           MOVE W-FECHA-AMD(5:2)       TO MIR-FECHA-VALOR(6:2)          08770058
           MOVE '-'                    TO MIR-FECHA-VALOR(8:1)          08780058
           MOVE W-FECHA-AMD(7:2)       TO MIR-FECHA-VALOR(9:2)          08790058
           MOVE MIR-FECHA-VALOR        TO MIR-FECHA-OPER                08800058
           MOVE MIR-FECHA-VALOR        TO MIR-FECHA-CONTA               08810058
           MOVE 'SUSCRIPCION  :'       TO MIR-OBSERVA(01:14)            08820058
           MOVE VXEN-NEMOTEC           TO MIR-OBSERVA(16:10)            08830058
           MOVE VDET-CTAVAL            TO MIR-REF-INTERNA(1:7)          08840058
           MOVE VDET-REFAPU            TO W-REFAPU                      08850058
           MOVE W-REFAPU (2:8)         TO MIR-REF-INTERNA(8:8)          08860058
           MOVE W-HORA-CURRENT         TO MIR-HORA-OPER                 08870058
           MOVE 'VL '                  TO MIR-APLC-ORI                  08880058
           IF VDET-SUS-CTAECO(11:02) = '91'                             08890058
              MOVE 'BGE0515'           TO MIR-CODI-ERROR (04)           08900058
              MOVE -9999999999999.99   TO W-LIMITE-AUT-9                08910058
              MOVE W-LIMITE-AUT        TO MIR-SITU-ERROR (04)           08920058
           END-IF                                                       08930058
           MOVE VDET-DATOS-DETAL (104:3) TO WDET-NUM-RETX               08940058
           IF WDET-NUM-RET > ZEROS                                      08950058
              MOVE WDET-NUM-RET        TO MIR-NUMER-RET                 08960058
              MOVE '999'               TO MIR-CODIG-RET                 08970058
              MOVE ZEROS               TO MIR-IMP-RET                   08980058
              SET MIR-88-IND-CANCE-RET TO TRUE                          08990058
           END-IF                                                       09000058
                                                                        09010058
           CALL CT-BG9CMIR0     USING  BGECMIR.                         09020058
                                                                        09030058
           IF MIR-COD-ERROR-DEV EQUAL SPACES                            09040058
              ADD 1                    TO WA-MIR-OK                     09050058
              PERFORM INSERT-VLDTCON                                    09060058
              PERFORM UPDATE-VLDTDET                                    09070058
           ELSE                                                         09080058
              MOVE CT-VL4C3050          TO WRUT-PROGRAMA                09090058
              MOVE MIR-COD-ERROR-DEV    TO WRUT-RETOR                   09100058
              MOVE VOPE-PAVAL           TO WRUT-ACCION (01:03)          09110058
              MOVE VOPE-VALOR           TO WRUT-ACCION (04:08)          09120058
              MOVE VOPE-ISIN            TO WRUT-ACCION (12:01)          09130058
              MOVE VOPE-FECHOP          TO WRUT-ACCION (14:08)          09140058
              MOVE MIR-CCC              TO WRUT-CLAVE  (01:20)          09150058
              MOVE VDET-CTAVAL          TO WRUT-CLAVE  (22:07)          09160058
              MOVE 'PROCESO-MIR      '  TO WRUT-PARRAFO                 09170058
              PERFORM VLPCRUTI-DISP-ABEND-RUTI                          09180058
              PERFORM VLPCRUTI-ABEND-RUTI                               09190058
           END-IF.                                                      09200058
                                                                        09210058
       PROCESO-MIR2.                                                    09220058
      *============*                                                    09230058
           INITIALIZE  REG-G10FMIR                                      09240058
                       REG-G10FTI0.                                     09250058
                                                                        09260058
JPC@3      IF VCON-MONEDA-CTA NOT = MDC-CDDIVIS                         09270058
JPC@3         PERFORM 200800-TIPO-CAMBIO                                09280058
JPC@3            THRU 200800-TIPO-CAMBIO-FIN                            09290058
JPC@3      END-IF.                                                      09300058
                                                                        09310058
           MOVE VCON-NUMCTA (01:04)    TO MIR-ENTIDAD                   09320058
           MOVE VCON-NUMCTA (05:04)    TO MIR-CENTRO-ALTA               09330058
JPC@2      MOVE '0567'                 TO MIR-CENTRO-ORI                09340058
           MOVE MDC-DIGICCC1           TO MIR-DIGICCC1                  09350058
           MOVE MDC-DIGICCC2           TO MIR-DIGICCC2                  09360058
           MOVE VCON-NUMCTA (11:10)    TO MIR-CUENTA                    09370058
           MOVE '0011'                 TO MIR-ENTIDAD-ORI               09380058
           MOVE CT-VL4C3050            TO MIR-USERID-ORI                09390058
           MOVE SPACES                 TO MIR-NETNAME-ORI               09400058
           MOVE '066'                  TO MIR-CODIGO                    09410058
           COMPUTE MIR-IMPORTE = W-IMPLIQ *  1                          09420058
           MOVE 'OPS'                  TO MIR-TIPO-CONTAB               09430058
           MOVE 'R'                    TO MIR-IND-REALPRUE              09440058
           MOVE '0'                    TO MIR-IND-RETENCI               09450058
           MOVE 'N'                    TO MIR-IND-CARGO-DISP            09460058
           MOVE 'N'                    TO MIR-IND-OPER-INTER            09470058
           MOVE '1'                    TO MIR-IND-ORIGEN-OP             09480058
           MOVE 'S'                    TO MIR-IND-CARTA                 09490058
JPC@3 *    MOVE VCON-MONEDA-CTA        TO MIR-DIVISA                    09500058
JPC@3      MOVE MDC-CDDIVIS            TO MIR-DIVISA                    09510058
           MOVE W-FECHA-AMD(1:4)       TO MIR-FECHA-VALOR(1:4)          09520058
           MOVE '-'                    TO MIR-FECHA-VALOR(5:1)          09530058
           MOVE W-FECHA-AMD(5:2)       TO MIR-FECHA-VALOR(6:2)          09540058
           MOVE '-'                    TO MIR-FECHA-VALOR(8:1)          09550058
           MOVE W-FECHA-AMD(7:2)       TO MIR-FECHA-VALOR(9:2)          09560058
           MOVE MIR-FECHA-VALOR        TO MIR-FECHA-OPER                09570058
           MOVE MIR-FECHA-VALOR        TO MIR-FECHA-CONTA               09580058
JPC@3      MOVE SPACES                       TO MIR-OBSERVA             09590058
           EVALUATE TRUE                                                09600058
               WHEN OPE-DIVIDENDOS                                      09610058
JPC@3 *             MOVE 'DIVIDENDO '        TO MIR-OBSERVA(01:10)      09620058
JPC@3 *             MOVE VXEN-NEMOTEC        TO MIR-OBSERVA(11:10)      09630058
JPC@3               STRING VXEN-NOMBRE       DELIMITED BY '  ' ' '      09640058
JPC@3                      '- DIVIDENDOS'    DELIMITED BY '  '          09650058
JPC@3                                        INTO MIR-OBSERVA           09660058
               WHEN OPE-INTERESES                                       09670058
JPC@3 *             MOVE 'VCTO. INTERESES '  TO MIR-OBSERVA(01:16)      09680058
JPC@3 *             MOVE VXEN-NEMOTEC        TO MIR-OBSERVA(17:10)      09690058
JPC@3               STRING VXEN-NOMBRE       DELIMITED BY '  ' ' '      09700058
JPC@3                      '- INTERESES'     DELIMITED BY '  '          09710058
JPC@3                                        INTO MIR-OBSERVA           09720058
JPC@3 *        WHEN OPE-AMORTIZACION                                    09730058
JPC@3 *             MOVE 'AMORTIZACION '     TO MIR-OBSERVA(01:13)      09740058
JPC@3 *             MOVE VXEN-NEMOTEC        TO MIR-OBSERVA(14:10)      09750058
JPC@3          WHEN OPE-AMORTIZA-A                                      09760058
JPC@3               STRING VXEN-NOMBRE       DELIMITED BY '  ' ' '      09770058
JPC@3                      '-AMORTIZACION'   DELIMITED BY '  '          09780058
JPC@3                                        INTO MIR-OBSERVA           09790058
JPC@3          WHEN OPE-AMORTIZA-R                                      09800058
JPC@3               STRING VXEN-NOMBRE       DELIMITED BY '  ' ' '      09810058
JPC@3                      '- REDENCION'     DELIMITED BY '  '          09820058
JPC@3                                        INTO MIR-OBSERVA           09830058
           END-EVALUATE                                                 09840058
           MOVE VCON-CUENTA            TO MIR-REF-INTERNA(1:7)          09850058
           MOVE VCON-REFER             TO W-REFAPU                      09860058
           MOVE W-REFAPU (2:8)         TO MIR-REF-INTERNA(8:8)          09870058
           MOVE W-HORA-CURRENT         TO MIR-HORA-OPER                 09880058
           MOVE 'VL '                  TO MIR-APLC-ORI                  09890058
           IF VCON-NUMCTA (11:02) = '91'                                09900058
              MOVE 'BGE0515'           TO MIR-CODI-ERROR (04)           09910058
              MOVE -9999999999999.99   TO W-LIMITE-AUT-9                09920058
              MOVE W-LIMITE-AUT        TO MIR-SITU-ERROR (04)           09930058
           END-IF                                                       09940058
                                                                        09950058
           CALL CT-BG9CMIR0     USING  BGECMIR.                         09960058
                                                                        09970058
           IF MIR-COD-ERROR-DEV EQUAL SPACES                            09980058
              ADD 1                    TO WA-MIR-OK                     09990058
              PERFORM INSERT-VLDTCON                                    10000058
              PERFORM UPDATE-VLDTDET                                    10010058
           ELSE                                                         10020058
              MOVE CT-VL4C3050          TO WRUT-PROGRAMA                10030058
              MOVE MIR-COD-ERROR-DEV    TO WRUT-RETOR                   10040058
              MOVE VOPE-PAVAL           TO WRUT-ACCION (01:03)          10050058
              MOVE VOPE-VALOR           TO WRUT-ACCION (04:08)          10060058
              MOVE VOPE-ISIN            TO WRUT-ACCION (12:01)          10070058
              MOVE VOPE-FECHOP          TO WRUT-ACCION (14:08)          10080058
              MOVE MIR-CCC              TO WRUT-CLAVE  (01:20)          10090058
              MOVE VDET-CTAVAL          TO WRUT-CLAVE  (22:07)          10100058
              MOVE 'PROCESO-MIR2     '  TO WRUT-PARRAFO                 10110058
              PERFORM VLPCRUTI-DISP-ABEND-RUTI                          10120058
              PERFORM VLPCRUTI-ABEND-RUTI                               10130058
           END-IF.                                                      10140058
      *                                                                *10150058
       PROCESO-MIR-SAB.                                                 10160058
      *===============*                                                 10170058
      *                                                                *10180058
      ****                                                          ****10190058
      *    ABONO A CUENTA DE LA SAB POR MOVIMIENTO CUENTA REGISTRO     *10200058
      ****                                                          ****10210058
      *                                                                *10220058
           INITIALIZE                     REG-G10FMIR,                  10230058
                                          REG-G10FTI0.                  10240058
                                                                        10250058
           IF VCON-MONEDA-CTA = 'PEN'                                   10260058
              MOVE WSAB-CTAECOS(01:04) TO MIR-ENTIDAD                   10270058
              MOVE WSAB-CTAECOS(05:04) TO MIR-CENTRO-ALTA               10280058
JPC@2         MOVE '0567'              TO MIR-CENTRO-ORI                10290058
              MOVE WSAB-CTAECOS(09:01) TO MIR-DIGICCC1                  10300058
              MOVE WSAB-CTAECOS(10:01) TO MIR-DIGICCC2                  10310058
              MOVE WSAB-CTAECOS(11:10) TO MIR-CUENTA                    10320058
           ELSE                                                         10330058
              MOVE WSAB-CTAECOD(01:04) TO MIR-ENTIDAD                   10340058
              MOVE WSAB-CTAECOD(05:04) TO MIR-CENTRO-ALTA               10350058
JPC@2         MOVE '0567'              TO MIR-CENTRO-ORI                10360058
              MOVE WSAB-CTAECOD(09:01) TO MIR-DIGICCC1                  10370058
              MOVE WSAB-CTAECOD(10:01) TO MIR-DIGICCC2                  10380058
              MOVE WSAB-CTAECOD(11:10) TO MIR-CUENTA                    10390058
           END-IF                                                       10400058
           MOVE '0011'                 TO MIR-ENTIDAD-ORI               10410058
           MOVE CT-VL4C3050            TO MIR-USERID-ORI                10420058
           MOVE SPACES                 TO MIR-NETNAME-ORI               10430058
           MOVE '066'                  TO MIR-CODIGO                    10440058
           COMPUTE MIR-IMPORTE = VCON-IMPLIQ *  1                       10450058
           MOVE 'OPS'                  TO MIR-TIPO-CONTAB               10460058
           MOVE 'R'                    TO MIR-IND-REALPRUE              10470058
           MOVE '0'                    TO MIR-IND-RETENCI               10480058
           MOVE 'N'                    TO MIR-IND-CARGO-DISP            10490058
           MOVE 'N'                    TO MIR-IND-OPER-INTER            10500058
           MOVE '1'                    TO MIR-IND-ORIGEN-OP             10510058
           MOVE 'S'                    TO MIR-IND-CARTA                 10520058
           MOVE VCON-MONEDA-CTA        TO MIR-DIVISA                    10530058
           MOVE W-FECHA-AMD(1:4)       TO MIR-FECHA-VALOR(1:4)          10540058
           MOVE '-'                    TO MIR-FECHA-VALOR(5:1)          10550058
           MOVE W-FECHA-AMD(5:2)       TO MIR-FECHA-VALOR(6:2)          10560058
           MOVE '-'                    TO MIR-FECHA-VALOR(8:1)          10570058
           MOVE W-FECHA-AMD(7:2)       TO MIR-FECHA-VALOR(9:2)          10580058
           MOVE MIR-FECHA-VALOR        TO MIR-FECHA-OPER                10590058
           MOVE MIR-FECHA-VALOR        TO MIR-FECHA-CONTA               10600058
JPC@3      MOVE SPACES                       TO MIR-OBSERVA             10610058
           EVALUATE TRUE                                                10620058
               WHEN OPE-DIVIDENDOS                                      10630058
JPC@3 *             MOVE 'DIVIDENDO '        TO MIR-OBSERVA(01:10)      10640058
JPC@3 *             MOVE VXEN-NEMOTEC        TO MIR-OBSERVA(11:10)      10650058
JPC@3               STRING VXEN-NOMBRE       DELIMITED BY '  ' ' '      10660058
JPC@3                      '- DIVIDENDOS'    DELIMITED BY '  '          10670058
JPC@3                                        INTO MIR-OBSERVA           10680058
               WHEN OPE-INTERESES                                       10690058
JPC@3 *             MOVE 'VCTO. INTERESES '  TO MIR-OBSERVA(01:16)      10700058
JPC@3 *             MOVE VXEN-NEMOTEC        TO MIR-OBSERVA(17:10)      10710058
JPC@3               STRING VXEN-NOMBRE       DELIMITED BY '  ' ' '      10720058
JPC@3                      '- INTERESES'     DELIMITED BY '  '          10730058
JPC@3                                        INTO MIR-OBSERVA           10740058
JPC@3 *        WHEN OPE-AMORTIZACION                                    10750058
JPC@3 *             MOVE 'AMORTIZACION '     TO MIR-OBSERVA(01:13)      10760058
JPC@3 *             MOVE VXEN-NEMOTEC        TO MIR-OBSERVA(14:10)      10770058
JPC@3          WHEN OPE-AMORTIZA-A                                      10780058
JPC@3               STRING VXEN-NOMBRE       DELIMITED BY '  ' ' '      10790058
JPC@3                      '-AMORTIZACION'   DELIMITED BY '  '          10800058
JPC@3                                        INTO MIR-OBSERVA           10810058
JPC@3          WHEN OPE-AMORTIZA-R                                      10820058
JPC@3               STRING VXEN-NOMBRE       DELIMITED BY '  ' ' '      10830058
JPC@3                      '- REDENCION'     DELIMITED BY '  '          10840058
JPC@3                                        INTO MIR-OBSERVA           10850058
JPC@3      END-EVALUATE                                                 10860058
           MOVE VCON-CUENTA            TO MIR-REF-INTERNA(1:7)          10870058
           MOVE VCON-REFER             TO W-REFAPU                      10880058
           MOVE W-REFAPU (2:8)         TO MIR-REF-INTERNA(8:8)          10890058
           MOVE W-HORA-CURRENT         TO MIR-HORA-OPER                 10900058
           MOVE 'VL '                  TO MIR-APLC-ORI                  10910058
                                                                        10920058
           CALL CT-BG9CMIR0     USING  BGECMIR.                         10930058
                                                                        10940058
           IF MIR-COD-ERROR-DEV EQUAL SPACES                            10950058
              ADD  1                    TO WA-MIR-SAB                   10960058
           ELSE                                                         10970058
              MOVE CT-VL4C3050          TO WRUT-PROGRAMA                10980058
              MOVE MIR-COD-ERROR-DEV    TO WRUT-RETOR                   10990058
              MOVE VOPE-PAVAL           TO WRUT-ACCION (01:03)          11000058
              MOVE VOPE-VALOR           TO WRUT-ACCION (04:08)          11010058
              MOVE VOPE-ISIN            TO WRUT-ACCION (12:01)          11020058
              MOVE VOPE-FECHOP          TO WRUT-ACCION (14:08)          11030058
              MOVE MIR-CCC              TO WRUT-CLAVE  (01:20)          11040058
              MOVE VDET-CTAVAL          TO WRUT-CLAVE  (22:07)          11050058
              MOVE 'PROCESO-MIR-SAB-A'  TO WRUT-PARRAFO                 11060058
              PERFORM VLPCRUTI-DISP-ABEND-RUTI                          11070058
              PERFORM VLPCRUTI-ABEND-RUTI                               11080058
           END-IF.                                                      11090058
      *                                                                *11100058
      ****                                                          ****11110058
      *    CARGO A CUENTA DE LA SAB POR MOVIMIENTO CUENTA REGISTRO     *11120058
      ****                                                          ****11130058
      *                                                                *11140058
           INITIALIZE                     REG-G10FMIR,                  11150058
                                          REG-G10FTI0.                  11160058
           IF VCON-MONEDA-CTA = 'PEN'                                   11170058
              MOVE WSAB-CTAECOS(01:04) TO MIR-ENTIDAD                   11180058
              MOVE WSAB-CTAECOS(05:04) TO MIR-CENTRO-ALTA               11190058
JPC@2         MOVE '0567'              TO MIR-CENTRO-ORI                11200058
              MOVE WSAB-CTAECOS(09:01) TO MIR-DIGICCC1                  11210058
              MOVE WSAB-CTAECOS(10:01) TO MIR-DIGICCC2                  11220058
              MOVE WSAB-CTAECOS(11:10) TO MIR-CUENTA                    11230058
           ELSE                                                         11240058
              MOVE WSAB-CTAECOD(01:04) TO MIR-ENTIDAD                   11250058
              MOVE WSAB-CTAECOD(05:04) TO MIR-CENTRO-ALTA               11260058
JPC@2         MOVE '0567'              TO MIR-CENTRO-ORI                11270058
              MOVE WSAB-CTAECOD(09:01) TO MIR-DIGICCC1                  11280058
              MOVE WSAB-CTAECOD(10:01) TO MIR-DIGICCC2                  11290058
              MOVE WSAB-CTAECOD(11:10) TO MIR-CUENTA                    11300058
           END-IF                                                       11310058
           MOVE '0011'                 TO MIR-ENTIDAD-ORI               11320058
           MOVE CT-VL4C3050            TO MIR-USERID-ORI                11330058
           MOVE SPACES                 TO MIR-NETNAME-ORI               11340058
           MOVE '065'                  TO MIR-CODIGO                    11350058
           COMPUTE MIR-IMPORTE = VCON-IMPLIQ * -1                       11360058
           MOVE 'OPS'                  TO MIR-TIPO-CONTAB               11370058
           MOVE 'R'                    TO MIR-IND-REALPRUE              11380058
           MOVE '0'                    TO MIR-IND-RETENCI               11390058
           MOVE 'N'                    TO MIR-IND-CARGO-DISP            11400058
           MOVE 'N'                    TO MIR-IND-OPER-INTER            11410058
           MOVE '1'                    TO MIR-IND-ORIGEN-OP             11420058
           MOVE 'S'                    TO MIR-IND-CARTA                 11430058
           MOVE VCON-MONEDA-CTA        TO MIR-DIVISA                    11440058
           MOVE W-FECHA-AMD(1:4)       TO MIR-FECHA-VALOR(1:4)          11450058
           MOVE '-'                    TO MIR-FECHA-VALOR(5:1)          11460058
           MOVE W-FECHA-AMD(5:2)       TO MIR-FECHA-VALOR(6:2)          11470058
           MOVE '-'                    TO MIR-FECHA-VALOR(8:1)          11480058
           MOVE W-FECHA-AMD(7:2)       TO MIR-FECHA-VALOR(9:2)          11490058
           MOVE MIR-FECHA-VALOR        TO MIR-FECHA-OPER                11500058
           MOVE MIR-FECHA-VALOR        TO MIR-FECHA-CONTA               11510058
JPC@3      MOVE SPACES                       TO MIR-OBSERVA             11520058
           EVALUATE TRUE                                                11530058
               WHEN OPE-DIVIDENDOS                                      11540058
JPC@3 *             MOVE 'DIVIDENDO '        TO MIR-OBSERVA(01:10)      11550058
JPC@3 *             MOVE VXEN-NEMOTEC        TO MIR-OBSERVA(11:10)      11560058
JPC@3               STRING VXEN-NOMBRE       DELIMITED BY '  ' ' '      11570058
JPC@3                      '- DIVIDENDOS'    DELIMITED BY '  '          11580058
JPC@3                                        INTO MIR-OBSERVA           11590058
               WHEN OPE-INTERESES                                       11600058
JPC@3 *             MOVE 'VCTO. INTERESES '  TO MIR-OBSERVA(01:16)      11610058
JPC@3 *             MOVE VXEN-NEMOTEC        TO MIR-OBSERVA(17:10)      11620058
JPC@3               STRING VXEN-NOMBRE       DELIMITED BY '  ' ' '      11630058
JPC@3                      '- INTERESES'     DELIMITED BY '  '          11640058
JPC@3                                        INTO MIR-OBSERVA           11650058
JPC@3 *        WHEN OPE-AMORTIZACION                                    11660058
JPC@3 *             MOVE 'AMORTIZACION '     TO MIR-OBSERVA(01:13)      11670058
JPC@3 *             MOVE VXEN-NEMOTEC        TO MIR-OBSERVA(14:10)      11680058
JPC@3          WHEN OPE-AMORTIZA-A                                      11690058
JPC@3               STRING VXEN-NOMBRE       DELIMITED BY '  ' ' '      11700058
JPC@3                      '-AMORTIZACION'   DELIMITED BY '  '          11710058
JPC@3                                        INTO MIR-OBSERVA           11720058
JPC@3          WHEN OPE-AMORTIZA-R                                      11730058
JPC@3               STRING VXEN-NOMBRE       DELIMITED BY '  ' ' '      11740058
JPC@3                      '- REDENCION'     DELIMITED BY '  '          11750058
JPC@3                                        INTO MIR-OBSERVA           11760058
JPC@3      END-EVALUATE                                                 11770058
           MOVE VCON-CUENTA            TO MIR-REF-INTERNA(1:7)          11780058
           MOVE VCON-REFER             TO W-REFAPU                      11790058
           MOVE W-REFAPU (2:8)         TO MIR-REF-INTERNA(8:8)          11800058
           MOVE W-HORA-CURRENT         TO MIR-HORA-OPER                 11810058
           MOVE 'VL '                  TO MIR-APLC-ORI                  11820058
      *    EN CASO SALDO INSUFICIENTE SE REALIZA EL SOBREGIRO           11830058
           MOVE 'BGE0515'              TO MIR-CODI-ERROR (04)           11840058
           MOVE -9999999999999.99      TO W-LIMITE-AUT-9                11850058
           MOVE W-LIMITE-AUT           TO MIR-SITU-ERROR (04)           11860058
      *    EN CASO SALDO INSUFICIENTE SE REALIZA EL SOBREGIRO           11870058
                                                                        11880058
           CALL CT-BG9CMIR0     USING  BGECMIR.                         11890058
                                                                        11900058
           IF MIR-COD-ERROR-DEV EQUAL SPACES                            11910058
              ADD  1                    TO WA-MIR-SAB                   11920058
           ELSE                                                         11930058
              MOVE CT-VL4C3050          TO WRUT-PROGRAMA                11940058
              MOVE MIR-COD-ERROR-DEV    TO WRUT-RETOR                   11950058
              MOVE VOPE-PAVAL           TO WRUT-ACCION (01:03)          11960058
              MOVE VOPE-VALOR           TO WRUT-ACCION (04:08)          11970058
              MOVE VOPE-ISIN            TO WRUT-ACCION (12:01)          11980058
              MOVE VOPE-FECHOP          TO WRUT-ACCION (14:08)          11990058
              MOVE MIR-CCC              TO WRUT-CLAVE  (01:20)          12000058
              MOVE VDET-CTAVAL          TO WRUT-CLAVE  (22:07)          12010058
              MOVE 'PROCESO-MIR-SAB-C'  TO WRUT-PARRAFO                 12020058
              PERFORM VLPCRUTI-DISP-ABEND-RUTI                          12030058
              PERFORM VLPCRUTI-ABEND-RUTI                               12040058
           END-IF.                                                      12050058
      *==============*                                                  12060058
       UPDATE-VLDTDET.                                                  12070058
      *==============*                                                  12080058
           MOVE 'P'                        TO VDET-DATOS-DETAL (107:1)  12090058
JPC@2 * EQUIVALE A MOVE 'P' TO VDET-DESEM                               12100058
JPC@2      MOVE 'P'                        TO VDET-DATOS-DETAL (070:1)  12110058
      * AQUI SE GUARDA INFORMACION OFICINA Y EMPLEADO Q' ATENDIO AL CLTE12120058
      *    MOVE W-FECHA-AMD-N              TO VDET-FEULMOD              12130058
      *    MOVE W-HORA-CURRENT-N           TO VDET-HORULMOD             12140058
      *    MOVE 'BATC'                     TO VDET-NUMTER               12150058
      *    MOVE 'VL4C3050'                 TO VDET-USUARIO              12160058
      * AQUI SE GUARDA INFORMACION OFICINA Y EMPLEADO Q' ATENDIO AL CLTE12170058
      *                                                                *12180058
           EXEC SQL                                                     12190058
                UPDATE VLDTDET                                          12200058
                SET VDET_DATOS_DETAL = :VDET-DATOS-DETAL                12210058
                  , VDET_FEULMOD     = :VDET-FEULMOD                    12220058
                  , VDET_HORULMOD    = :VDET-HORULMOD                   12230058
                  , VDET_NUMTER      = :VDET-NUMTER                     12240058
                  , VDET_USUARIO     = :VDET-USUARIO                    12250058
                WHERE VDET_FECHOP    = :VDET-FECHOP                     12260058
                  AND VDET_PAVAL     = :VDET-PAVAL                      12270058
                  AND VDET_VALOR     = :VDET-VALOR                      12280058
                  AND VDET_ISIN      = :VDET-ISIN                       12290058
                  AND VDET_FORMAT    = :VDET-FORMAT                     12300058
                  AND VDET_CTAVAL    = :VDET-CTAVAL                     12310058
                  AND VDET_REFER     = :VDET-REFER                      12320058
           END-EXEC.                                                    12330058
                                                                        12340058
           MOVE SQLCODE                         TO SQLCODE-AUX          12350058
           EVALUATE TRUE                                                12360058
               WHEN DB2-OK                                              12370058
                    ADD 1                       TO WA-ACTUALIZA-DET     12380058
               WHEN OTHER                                               12390058
                    MOVE CT-VL4C3050            TO  W801-PROGRAMA       12400058
                    MOVE 'VLDTDET'              TO  W801-TABLA          12410058
                    MOVE 'UPDATE'               TO  W801-ACCION         12420058
                    MOVE  VDET-FECHOP           TO  W801-CLAVE(01:10)   12430058
                    MOVE  VDET-PAVAL            TO  W801-CLAVE(11:03)   12440058
                    MOVE  VDET-VALOR            TO  W801-CLAVE(15:08)   12450058
                    MOVE  VDET-ISIN             TO  W801-CLAVE(24:01)   12460058
                    MOVE  SQLCODE               TO  W801-SQLCODE        12470058
                    MOVE  SPACES                TO  W801-SQLWARN        12480058
                    MOVE 'UPDATE-VLDTDET'       TO  W801-PARRAFO        12490058
                    PERFORM  VLPC8010-DISP-ABEND-DB2                    12500058
                    PERFORM  VLPC8010-ABEND-DB2                         12510058
           END-EVALUATE.                                                12520058
      *                                                                 12530058
       PROCESO-CONTABLE.                                                12540058
      *================*                                                12550058
           PERFORM SELECT-VLDTARC.                                      12560058
           INITIALIZE                           DCLVLDTCON.             12570058
           MOVE W-FECHA-AMD-N                TO VCON-FCONTA             12580058
           MOVE VDET-SUS-REFAPU              TO VCON-REFER              12590058
           MOVE 34                           TO VCON-OPERAC             12600058
           MOVE VDET-CTAVAL                  TO VCON-CUENTA             12610058
           MOVE VOPE-FECHOP                  TO VCON-FECHOP             12620058
           MOVE VOPE-FVALOR                  TO VCON-FVALOR             12630058
           MOVE VOPE-PAVAL                   TO VCON-PAVAL              12640058
           MOVE VOPE-VALOR                   TO VCON-VALOR              12650058
           MOVE VOPE-ISIN                    TO VCON-ISIN               12660058
           MOVE VDET-SUS-ORDSUS              TO VCON-TITS               12670058
           MOVE VARC-SUCURS                  TO VCON-SUCDES             12680058
           MOVE VDET-SUS-CTAECO              TO VCON-NUMCTA             12690058
           MOVE VCON-NUMCTA (01:04)          TO MDC-ENTIDAD.            12700058
           MOVE VCON-NUMCTA (05:04)          TO MDC-CENTRO-ALTA.        12710058
           MOVE VCON-NUMCTA (11:10)          TO MDC-CUENTA.             12720058
           PERFORM CALL-BG9CMDC0                                        12730058
           MOVE MDC-CENTRO-CONTAB            TO VCON-SUCCTA             12740058
           MOVE 'P'                          TO VCON-CLCTA              12750058
           MOVE 'P'                          TO VCON-TIPCTA             12760058
           MOVE '0567'                       TO VCON-FILLER2(1:4)       12770058
           MOVE 'C'                          TO VCON-FILLER2(7:1)       12780058
           MOVE 'D'                          TO VCON-CL-DH              12790058
           COMPUTE VCON-IMPOR1 ROUNDED = VCON-TITS * VOPE-SUS-PREEMIS   12800058
           IF VOPE-SUS-COBCOM = 'N'                                     12810058
              MOVE 'N'                    TO VCON-FILLER3               12820058
              MOVE  ZEROES                TO VCON-IMPOR3                12830058
                                             VCON-IMPOR2                12840058
                                             VCON-IMPOR4                12850058
           ELSE                                                         12860058
              MOVE SPACES                 TO VCON-FILLER3               12870058
              PERFORM RUTINA-COMISIONES                                 12880058
                 THRU RUTINA-COMISIONES-FIN                             12890058
              MOVE  VL7COO-COMIS-PORTE    TO VCON-IMPOR3                12900058
              MOVE  VL7COO-COMISIONES     TO VCON-IMPOR2                12910058
              MOVE  VL7COO-IGV            TO VCON-IMPOR4                12920058
           END-IF                                                       12930058
                                                                        12940058
           COMPUTE VCON-IMPLIQ = VCON-IMPOR1 + VCON-IMPOR2 +            12950058
                                 VCON-IMPOR3 + VCON-IMPOR4              12960058
           MOVE VXEN-NOMBRE                  TO VCON-TEXVAL             12970058
           MOVE VXEN-NOMINEM                 TO VCON-NOMIN              12980058
           MOVE 'SUS'                        TO VCON-OPE-CON08          12990058
           MOVE VOPE-SUS-CUSTODIO(01:03)     TO VCON-MONEDA-CTA         13000058
           MOVE VXEN-CODDIVI                 TO VCON-MONEDA-VAL         13010058
           MOVE CT-VL4C3050                  TO VCON-PROGRA             13020058
           MOVE 'P'                          TO VCON-TIPOP              13030058
           MOVE 'N'                          TO VCON-REPEPROG           13040058
           MOVE 0                            TO VCON-LIQ-CON08-N        13050058
           MOVE 0                            TO VCON-TEM-CON08-N        13060058
           MOVE 0                            TO VCON-GAS-CON08-N        13070058
           MOVE W-FECHA-AMD-N                TO VCON-FEALTREG           13080058
           MOVE W-FECHA-AMD-N                TO VCON-FEULMOD            13090058
           MOVE W-HORA-CURRENT-N             TO VCON-HORULMOD           13100058
           MOVE 'BATC'                       TO VCON-NUMTER             13110058
           MOVE CT-VL4C3050                  TO VCON-USUARIO.           13120058
      *                                                                *13130058
      *    CARGA VARIABLES PARA CONVERSION TIPO DE CAMBIO              *13140058
      *                                                                *13150058
           MOVE VCON-IMPLIQ                  TO W-IMPLIQ.               13160058
           MOVE VCON-IMPOR1                  TO W-IMPOR1.               13170058
           MOVE VCON-IMPOR2                  TO W-IMPOR2.               13180058
           MOVE VCON-IMPOR3                  TO W-IMPOR3.               13190058
           MOVE VCON-IMPOR4                  TO W-IMPOR4.               13200058
           MOVE VCON-IMPOR5                  TO W-IMPOR5.               13210058
           MOVE VCON-IMPOR6                  TO W-IMPOR6.               13220058
           MOVE VCON-IMPOR7                  TO W-IMPOR7.               13230058
           MOVE VCON-IMPOR8                  TO W-IMPOR8.               13240058
           MOVE VCON-IMPOR9                  TO W-IMPOR9.               13250058
      *                                                                *13260058
      *    SE REALIZA EL INSERT SI EJECUCION DE LA MIR ES OK.          *13270058
      *                                                                *13280058
      *=================*                                               13290058
       PROCESO-CONTABLE2.                                               13300058
      *=================*                                               13310058
           PERFORM SELECT-VLDTARC.                                      13320058
           INITIALIZE                             DCLVLDTCON.           13330058
           MOVE SPACES                         TO WA-COBCOM             13340058
           EVALUATE TRUE                                                13350058
               WHEN OPE-DIVIDENDOS                                      13360058
                    MOVE VDET-REFAPU           TO VCON-REFER            13370058
                    MOVE 07                    TO VCON-OPERAC           13380058
                    MOVE VDET-TITUL            TO VCON-TITS             13390058
                    MOVE VDET-NUMCTA           TO VCON-NUMCTA           13400058
                    MOVE VDET-PTS-COMVEN       TO VCON-IMPOR1           13410058
                    MOVE 'DIV'                 TO VCON-OPE-CON08        13420058
                    MOVE VOPE-MON-FACT         TO VCON-MONEDA-CTA       13430058
                    MOVE VOPE-COBCOD           TO WA-COBCOM             13440058
               WHEN OPE-INTERESES                                       13450058
                    MOVE VDET-REFAPU           TO VCON-REFER            13460058
                    MOVE 08                    TO VCON-OPERAC           13470058
                    MOVE VDET-TITUL            TO VCON-TITS             13480058
                    MOVE VDET-NUMCTA           TO VCON-NUMCTA           13490058
                    MOVE VDET-PTS-COMVEN       TO VCON-IMPOR1           13500058
                    MOVE 'INT'                 TO VCON-OPE-CON08        13510058
                    MOVE VOPE-MON-FACT         TO VCON-MONEDA-CTA       13520058
                    MOVE VOPE-COBCOD           TO WA-COBCOM             13530058
               WHEN OPE-AMORTIZACION                                    13540058
                    MOVE VDET-REFAPU           TO VCON-REFER            13550058
                    MOVE 11                    TO VCON-OPERAC           13560058
                    MOVE VDET-TITUL            TO VCON-TITS             13570058
                    MOVE VDET-NUMCTA           TO VCON-NUMCTA           13580058
                    MOVE VDET-PTS-COMVEN       TO VCON-IMPOR1           13590058
                    MOVE 'AMT'                 TO VCON-OPE-CON08        13600058
                    MOVE VOPE-MON-REDEN        TO VCON-MONEDA-CTA       13610058
                    MOVE VOPE-COBCOM           TO WA-COBCOM             13620058
           END-EVALUATE                                                 13630058
           MOVE W-FECHA-AMD-N                TO VCON-FCONTA             13640058
           MOVE VDET-CTAVAL                  TO VCON-CUENTA             13650058
           MOVE VOPE-FECHOP                  TO VCON-FECHOP             13660058
           MOVE VOPE-FVALOR                  TO VCON-FVALOR             13670058
           MOVE VOPE-PAVAL                   TO VCON-PAVAL              13680058
           MOVE VOPE-VALOR                   TO VCON-VALOR              13690058
           MOVE VOPE-ISIN                    TO VCON-ISIN               13700058
           MOVE VARC-SUCURS                  TO VCON-SUCDES             13710058
           MOVE VCON-NUMCTA (01:04)          TO MDC-ENTIDAD.            13720058
           MOVE VCON-NUMCTA (05:04)          TO MDC-CENTRO-ALTA.        13730058
           MOVE VCON-NUMCTA (11:10)          TO MDC-CUENTA.             13740058
           PERFORM CALL-BG9CMDC0                                        13750058
           MOVE MDC-CENTRO-CONTAB            TO VCON-SUCCTA             13760058
           MOVE 'P'                          TO VCON-CLCTA              13770058
           MOVE 'P'                          TO VCON-TIPCTA             13780058
           MOVE '0567'                       TO VCON-FILLER2(1:4)       13790058
           MOVE 'C'                          TO VCON-FILLER2(7:1)       13800058
           MOVE 'D'                          TO VCON-CL-DH              13810058
           IF WA-COBCOM = 'N' OR SPACES                                 13820058
              MOVE 'N'                    TO VCON-FILLER3               13830058
              MOVE  ZEROES                TO VCON-IMPOR2                13840058
                                             VCON-IMPOR6                13850058
                                             VCON-IMPOR3                13860058
           ELSE                                                         13870058
              MOVE SPACES                 TO VCON-FILLER3               13880058
              PERFORM RUTINA-COMISIONES                                 13890058
                 THRU RUTINA-COMISIONES-FIN                             13900058
              MOVE  VL7COO-COMISIONES     TO VCON-IMPOR2                13910058
              MOVE  VL7COO-COMIS-PORTE    TO VCON-IMPOR6                13920058
              MOVE  VL7COO-IGV            TO VCON-IMPOR3                13930058
           END-IF                                                       13940058
                                                                        13950058
           COMPUTE VCON-IMPLIQ = VCON-IMPOR1 - VCON-IMPOR2 -            13960058
                                 VCON-IMPOR3 - VCON-IMPOR6              13970058
           MOVE VXEN-NOMBRE                  TO VCON-TEXVAL             13980058
           MOVE VXEN-NOMINEM                 TO VCON-NOMIN              13990058
           MOVE VXEN-CODDIVI                 TO VCON-MONEDA-VAL         14000058
           MOVE CT-VL4C3050                  TO VCON-PROGRA             14010058
           MOVE 'P'                          TO VCON-TIPOP              14020058
           MOVE 'N'                          TO VCON-REPEPROG           14030058
           MOVE 0                            TO VCON-LIQ-CON08-N        14040058
           MOVE 0                            TO VCON-TEM-CON08-N        14050058
           MOVE 0                            TO VCON-GAS-CON08-N        14060058
           MOVE W-FECHA-AMD-N                TO VCON-FEALTREG           14070058
           MOVE W-FECHA-AMD-N                TO VCON-FEULMOD            14080058
           MOVE W-HORA-CURRENT-N             TO VCON-HORULMOD           14090058
           MOVE 'BATC'                       TO VCON-NUMTER             14100058
           MOVE CT-VL4C3050                  TO VCON-USUARIO.           14110058
      *                                                                *14120058
      *    CARGA VARIABLES PARA CONVERSION TIPO DE CAMBIO              *14130058
      *                                                                *14140058
           MOVE VCON-IMPLIQ                  TO W-IMPLIQ.               14150058
           MOVE VCON-IMPOR1                  TO W-IMPOR1.               14160058
           MOVE VCON-IMPOR2                  TO W-IMPOR2.               14170058
           MOVE VCON-IMPOR3                  TO W-IMPOR3.               14180058
           MOVE VCON-IMPOR4                  TO W-IMPOR4.               14190058
           MOVE VCON-IMPOR5                  TO W-IMPOR5.               14200058
           MOVE VCON-IMPOR6                  TO W-IMPOR6.               14210058
           MOVE VCON-IMPOR7                  TO W-IMPOR7.               14220058
           MOVE VCON-IMPOR8                  TO W-IMPOR8.               14230058
           MOVE VCON-IMPOR9                  TO W-IMPOR9.               14240058
      *                                                                *14250058
      *    SE REALIZA EL INSERT SI EJECUCION DE LA MIR ES OK.          *14260058
      *                                                                *14270058
      *==============*                                                  14280058
       INSERT-VLDTCON.                                                  14290058
      *==============*                                                  14300058
      *                                                                *14310058
JPC@3      IF VCON-MONEDA-CTA NOT = MDC-CDDIVIS                         14320058
JPC@3         MOVE MIR-NUMER-MOV               TO  VCON-PERSMAY         14330058
JPC@3         MOVE MIR-TIPCAM-ESPECIAL         TO  VCON-CAMBOP          14340058
JPC@3      END-IF.                                                      14350058
JPC@4      MOVE W-WOPS-TIPO-CAMBIO             TO  VCON-CAMBOP.         14360058
JPC@4      MOVE W-TC-GP8C1950                  TO  VCON-TEX-CON08-T1.   14370058
      *                                                                *14380058
           EXEC SQL                                                     14390058
                INSERT                                                  14400058
                  INTO  VLDTCON                                         14410058
                VALUES (:DCLVLDTCON)                                    14420058
           END-EXEC                                                     14430058
      *                                                                *14440058
           EVALUATE SQLCODE                                             14450058
               WHEN 0                                                   14460058
                    ADD 1                     TO  INSERT-VCON           14470058
              WHEN  OTHER                                               14480058
                    MOVE CT-VL4C3050          TO  W801-PROGRAMA         14490058
                    MOVE 'VLDTCON'            TO  W801-TABLA            14500058
                    MOVE 'INSERT'             TO  W801-ACCION           14510058
                    MOVE VCON-CUENTA          TO  W801-CLAVE (01:07)    14520058
                    MOVE VCON-REFER           TO  W801-CLAVE (09:09)    14530058
                    MOVE VOPE-FORMAT          TO  W801-CLAVE (20:01)    14540058
                    MOVE VOPE-TIPOP           TO  W801-CLAVE (21:01)    14550058
                    MOVE  SQLCODE             TO  W801-SQLCODE          14560058
                    MOVE  SPACES              TO  W801-SQLWARN          14570058
                    MOVE 'INSERT-VLDTCON  '   TO  W801-PARRAFO          14580058
                    PERFORM  VLPC8010-DISP-ABEND-DB2                    14590058
                    PERFORM  VLPC8010-ABEND-DB2                         14600058
           END-EVALUATE.                                                14610058
      *                                                                *14620058
      *=================*                                               14630058
       RUTINA-COMISIONES.                                               14640058
      *=================*                                               14650058
           INITIALIZE                          W-VLWCCCO0.              14660058
           MOVE VCON-OPERAC                 TO VL7COO-NUMOPE            14670058
           MOVE VARC-INVERSOR               TO VL7COO-CODTARIF          14680058
           MOVE VCON-IMPOR1                 TO VL7COO-IMPORTE           14690058
           MOVE VDET-CTAVAL                 TO VL7COO-CUENTA            14700058
           MOVE VCON-MONEDA-CTA             TO VL7COO-MONEDA            14710058
           EVALUATE VXEN-IND-A5R(1:1)                                   14720058
               WHEN 'I'   MOVE VXEN-IND-A5R(1:1) TO VL7COO-TIPTARIF     14730058
               WHEN OTHER MOVE VREL-INDICFIS     TO VL7COO-TIPTARIF     14740058
           END-EVALUATE                                                 14750058
                                                                        14760058
           CALL VL9CBCOO USING W-VLWCCCO0                               14770058
                                                                        14780058
           EVALUATE VL7COO-RETORNO                                      14790058
               WHEN '00'                                                14800058
                    CONTINUE                                            14810058
               WHEN OTHER                                               14820058
                    MOVE 'VL9CBCOO'           TO WRUT-PROGRAMA          14830058
                    MOVE VL7COO-RETORNO       TO WRUT-RETOR             14840058
                    MOVE VL7COO-ENTRADA       TO WRUT-ACCION            14850058
                    MOVE VL7COO-MSG           TO WRUT-CLAVE             14860058
                    MOVE 'RUTINA-COMISIONES'  TO WRUT-PARRAFO           14870058
                    PERFORM VLPCRUTI-DISP-ABEND-RUTI                    14880058
                    PERFORM VLPCRUTI-ABEND-RUTI                         14890058
             END-EVALUATE.                                              14900058
                                                                        14910058
      *=====================*                                           14920058
       RUTINA-COMISIONES-FIN.                                           14930058
      *=====================*                                           14940058
           EXIT.                                                        14950058
      *=============*                                                   14960058
       CALL-BG9CMDC0.                                                   14970058
      *=============*                                                   14980058
      *                                                                *14990058
           CALL BG9CMDC0 USING W-BGECMDC.                               15000058
      *                                                                *15010058
           EVALUATE MDC-CODERR                                          15020058
               WHEN SPACES                                              15030058
                    CONTINUE                                            15040058
               WHEN OTHER                                               15050058
                    MOVE 'BG9CMDC0'           TO WRUT-PROGRAMA          15060058
                    MOVE MDC-CODERR           TO WRUT-RETOR             15070058
                    MOVE MDC-DATOS-ENT        TO WRUT-ACCION            15080058
                    MOVE VCON-CUENTA          TO WRUT-CLAVE (01:07)     15090058
                    MOVE VCON-REFER           TO WRUT-CLAVE (09:09)     15100058
                    MOVE VCON-OPERAC          TO WRUT-CLAVE (20:03)     15110058
                    MOVE 'CALL-BG9CMDC0'      TO WRUT-PARRAFO           15120058
                    PERFORM VLPCRUTI-DISP-ABEND-RUTI                    15130058
                    PERFORM VLPCRUTI-ABEND-RUTI                         15140058
           END-EVALUATE.                                                15150058
      *                                                                *15160058
      *==============*                                                  15170058
       SELECT-VLDTXBO.                                                  15180058
      *==============*                                                  15190058
      *                                                                 15200058
           EXEC SQL                                                     15210058
                SELECT  VXBO_CTAECOS                                    15220058
                     ,  VXBO_CTAECOD                                    15230058
                  INTO :VXBO-CTAECOS                                    15240058
                     , :VXBO-CTAECOD                                    15250058
                  FROM  VLDTXBO                                         15260058
                 WHERE  VXBO_CLABOL  = :VXBO-CLABOL                     15270058
           END-EXEC.                                                    15280058
      *                                                                 15290058
           MOVE SQLCODE  TO  SQLCODE-AUX                                15300058
           EVALUATE TRUE                                                15310058
               WHEN DB2-OK                                              15320058
                    CONTINUE                                            15330058
               WHEN OTHER                                               15340058
                    MOVE CT-VL4C3050          TO  W801-PROGRAMA         15350058
                    MOVE 'VLDTXBO'            TO  W801-TABLA            15360058
                    MOVE 'SELECT      '       TO  W801-ACCION           15370058
                    MOVE  '99'                TO  W801-CLAVE            15380058
                    MOVE  SQLCODE             TO  W801-SQLCODE          15390058
                    MOVE  SPACES              TO  W801-SQLWARN          15400058
                    MOVE 'SELECT-VLDTXBO    ' TO  W801-PARRAFO          15410058
                    PERFORM  VLPC8010-DISP-ABEND-DB2                    15420058
                    PERFORM  VLPC8010-ABEND-DB2                         15430058
           END-EVALUATE.                                                15440058
      *                                                                *15450058
      *==============*                                                  15460058
       SELECT-VLDTXMI.                                                  15470058
      *==============*                                                  15480058
      *                                                                 15490058
           MOVE '0069'                     TO VXMI-CODBE                15500058
           EXEC SQL                                                     15510058
                SELECT VXMI_CTACARGO                                    15520058
                INTO  :VXMI-CTACARGO                                    15530058
                FROM VLDTXMI                                            15540058
                WHERE VXMI_CODBE     = :VXMI-CODBE                      15550058
           END-EXEC.                                                    15560058
      *                                                                 15570058
           MOVE SQLCODE  TO  SQLCODE-AUX                                15580058
           EVALUATE TRUE                                                15590058
               WHEN DB2-OK                                              15600058
                    CONTINUE                                            15610058
               WHEN OTHER                                               15620058
                    MOVE CT-VL4C3050          TO  W801-PROGRAMA         15630058
                    MOVE 'VLDTXMI'            TO  W801-TABLA            15640058
                    MOVE 'SELECT      '       TO  W801-ACCION           15650058
                    MOVE VXMI-CODBE           TO  W801-CLAVE            15660058
                    MOVE  SQLCODE             TO  W801-SQLCODE          15670058
                    MOVE  SPACES              TO  W801-SQLWARN          15680058
                    MOVE 'SELECT-VLDTXMI    ' TO  W801-PARRAFO          15690058
                    PERFORM  VLPC8010-DISP-ABEND-DB2                    15700058
                    PERFORM  VLPC8010-ABEND-DB2                         15710058
           END-EVALUATE.                                                15720058
      *                                                                *15730058
      *==============*                                                  15740058
       SELECT-VLDTARC.                                                  15750058
      *==============*                                                  15760058
      *                                                                 15770058
           MOVE VDET-CTAVAL  TO   VARC-CUENTA.                          15780058
      *                                                                 15790058
           EXEC SQL                                                     15800058
                SELECT  VARC_SUCURS                                     15810058
                     ,  VARC_INVERSOR                                   15820058
                     ,  VARC_CTAVAL20                                   15830058
                     ,  VARC_NUMCLI                                     15840058
                  INTO :VARC-SUCURS                                     15850058
                     , :VARC-INVERSOR                                   15860058
                     , :VARC-CTAVAL20                                   15870058
                     , :VARC-NUMCLI                                     15880058
                  FROM  VLDTARC                                         15890058
                 WHERE  VARC_CUENTA  = :VARC-CUENTA                     15900058
           END-EXEC.                                                    15910058
      *                                                                *15920058
           MOVE SQLCODE  TO  SQLCODE-AUX                                15930058
           EVALUATE TRUE                                                15940058
               WHEN DB2-OK                                              15950058
                    CONTINUE                                            15960058
               WHEN OTHER                                               15970058
                    MOVE CT-VL4C3050          TO  W801-PROGRAMA         15980058
                    MOVE 'VLDTARC'            TO  W801-TABLA            15990058
                    MOVE 'SELECT      '       TO  W801-ACCION           16000058
                    MOVE VARC-CUENTA          TO  W801-CLAVE            16010058
                    MOVE  SQLCODE             TO  W801-SQLCODE          16020058
                    MOVE  SPACES              TO  W801-SQLWARN          16030058
                    MOVE 'SELECT-VLDTARC    ' TO  W801-PARRAFO          16040058
                    PERFORM  VLPC8010-DISP-ABEND-DB2                    16050058
                    PERFORM  VLPC8010-ABEND-DB2                         16060058
           END-EVALUATE.                                                16070058
      *                                                                *16080058
      *=======*                                                         16090058
       3-FINAL.                                                         16100058
      *=======*                                                         16110058
      *                                                                *16120058
           CLOSE VLLS3050.                                              16130058
      *                                                                *16140058
           MOVE ZEROS TO RETURN-CODE.                                   16150058
           PERFORM 3100-DISP-TOTALIMETROS                               16160058
              THRU 3100-DISP-TOTALIMETROS-FIN.                          16170058
      *                                                                *16180058
           IF SI-TIENE-ERROR                                            16190058
              EXEC SQL                                                  16200058
                   ROLLBACK                                             16210058
              END-EXEC                                                  16220058
                                                                        16230058
              MOVE SQLCODE                       TO SQLCODE-AUX         16240058
                                                                        16250058
              EVALUATE TRUE                                             16260058
                  WHEN DB2-OK                                           16270058
                       CONTINUE                                         16280058
                  WHEN OTHER                                            16290058
                       MOVE  CT-VL4C3050           TO W801-PROGRAMA     16300058
                       MOVE  SPACES                TO W801-TABLA        16310058
                       MOVE 'ROLLBACK    '         TO W801-ACCION       16320058
                       MOVE  SPACES                TO W801-CLAVE        16330058
                       MOVE  SQLCODE               TO W801-SQLCODE      16340058
                       MOVE  SPACES                TO W801-SQLWARN      16350058
                       MOVE '3-FINAL             ' TO W801-PARRAFO      16360058
                       PERFORM  VLPC8010-DISP-ABEND-DB2                 16370058
                       PERFORM  VLPC8010-ABEND-DB2                      16380058
              END-EVALUATE                                              16390058
           END-IF                                                       16400058
           IF NO-TIENE-ERROR                                            16410058
              EXEC SQL                                                  16420058
                   COMMIT                                               16430058
              END-EXEC                                                  16440058
                                                                        16450058
              MOVE SQLCODE                       TO SQLCODE-AUX         16460058
                                                                        16470058
              EVALUATE TRUE                                             16480058
                  WHEN DB2-OK                                           16490058
                       CONTINUE                                         16500058
                  WHEN OTHER                                            16510058
                       MOVE  CT-VL4C3050           TO W801-PROGRAMA     16520058
                       MOVE  SPACES                TO W801-TABLA        16530058
                       MOVE 'COMMIT      '         TO W801-ACCION       16540058
                       MOVE  SPACES                TO W801-CLAVE        16550058
                       MOVE  SQLCODE               TO W801-SQLCODE      16560058
                       MOVE  SPACES                TO W801-SQLWARN      16570058
                       MOVE '3-FINAL             ' TO W801-PARRAFO      16580058
                       PERFORM  VLPC8010-DISP-ABEND-DB2                 16590058
                       PERFORM  VLPC8010-ABEND-DB2                      16600058
              END-EVALUATE                                              16610058
           END-IF.                                                      16620058
      *                                                                *16630058
       3100-DISP-TOTALIMETROS.                                          16640058
      *-----------------------*                                         16650058
           DISPLAY 'FECHA-HOY : ' FECHA-SYSIN '  ' W-FECHA-AMD          16660058
           DISPLAY '*************************************************'. 16670058
           DISPLAY '********    T O T A L I M E T R O S   ***********'. 16680058
           DISPLAY '********   D E L     P R O G R A M A  ***********'. 16690058
           DISPLAY '********          VL4C3050            ***********'. 16700058
           DISPLAY '*************************************************'. 16710058
           DISPLAY 'REG. LEIDOS    VLDTOPE       => ' WA-LEIDOS-OPE.    16720058
           DISPLAY 'REG. LEIDOS    VLDTDET       => ' WA-LEIDOS-DET.    16730058
           DISPLAY 'REG. LEIDOS    VLDTOPE OK    => ' WA-LEIDOS-OPE-OK. 16740058
           DISPLAY 'REG. PROCESADO VLDTOPE OK    => ' WA-LEIDOS-OPE-OK. 16750058
           DISPLAY 'REG. LEIDOS    VLDTDET OK    => ' WA-LEIDOS-DET-OK. 16760058
           DISPLAY 'REG. INSERT    VLDTCON OK    => ' INSERT-VCON.      16770058
           DISPLAY 'RET. OK DE RUTINA MIR        => ' WA-MIR-OK.        16780058
           DISPLAY 'RET. OK DE RUTINA MIR-SAB    => ' WA-MIR-SAB.       16790058
           DISPLAY 'REG. ACTUALIZADOS VLDTDET    => ' WA-ACTUALIZA-DET. 16800058
           DISPLAY 'CALL RUTINA    GP8C1950      => ' WA-CALL-GP8C1950. 16810058
           DISPLAY '*************************************************'. 16820058
      *                                                                *16830058
       3100-DISP-TOTALIMETROS-FIN.                                      16840058
      *--------------------------*                                      16850058
           EXIT.                                                        16860058
      *-------------*                                                   16870058
       999-ABEND-DB2.                                                   16880058
      *-------------*                                                   16890058
      *                                                                *16900058
           MOVE  16          TO RETURN-CODE                             16910058
           INITIALIZE           DB2-CODERR                              16920058
           MOVE SQLCA        TO DB2-SQLCA                               16930058
           MOVE 'S'          TO DB2-ABEND                               16940058
           MOVE 'D'          TO DB2-ABEND-DB2                           16950058
           CALL 'QR4CDB0'    USING QRECDB2.                             16960058
      *                                                                *16970058
      *   *------------*                                                16980058
       600-IMP-CABECERA.                                                16990058
      *   *------------*                                                17000058
      *                                                                *17010058
           MOVE VOPE-PAVAL                TO  CAB-ISIN (01:03)          17020058
           MOVE VOPE-VALOR                TO  CAB-ISIN (04:08)          17030058
           MOVE VOPE-ISIN                 TO  CAB-ISIN (12:01)          17040058
           EVALUATE TRUE                                                17050058
               WHEN OPE-SUSCRIPCION                                     17060058
                    MOVE 'SUSCRIPCION '   TO  CAB-OPER                  17070058
               WHEN OPE-DIVIDENDOS                                      17080058
                    MOVE 'DIVIDENDOS  '   TO  CAB-OPER                  17090058
               WHEN OPE-INTERESES                                       17100058
                    MOVE 'INTERESES   '   TO  CAB-OPER                  17110058
               WHEN OPE-AMORTIZACION                                    17120058
                    MOVE 'AMORTIZACION'   TO  CAB-OPER                  17130058
               WHEN OTHER                                               17140058
                    MOVE 'FORMATO-*   TIPO-*  ' TO  CAB-OPER            17150058
                    MOVE VOPE-FORMAT            TO  CAB-OPER (09:01)    17160058
                    MOVE VOPE-TIPOP             TO  CAB-OPER (18:01)    17170058
           END-EVALUATE.                                                17180058
      *                                                                *17190058
           ADD  1              TO  CONT-PAG.                            17200058
           MOVE CONT-PAG       TO  CAB2-PAG.                            17210058
      *                                                                *17220058
           WRITE R-VLLS3050   FROM L01-VLLS3050 AFTER PAGE.             17230058
           WRITE R-VLLS3050   FROM L02-VLLS3050 AFTER 1.                17240058
           WRITE R-VLLS3050   FROM L03-VLLS3050 AFTER 1.                17250058
           WRITE R-VLLS3050   FROM L04-VLLS3050 AFTER 1.                17260058
           WRITE R-VLLS3050   FROM L05-VLLS3050 AFTER 1.                17270058
           MOVE  5             TO  CONT-LIN.                            17280058
      *                                                                *17290058
      *   *-----------*                                                 17300058
       610-IMPRIME-OPE.                                                 17310058
      *   *-----------*                                                 17320058
      *                                                                *17330058
           PERFORM 620-CALL-PE9C5000.                                   17340058
           MOVE VARC-CUENTA           TO  DET1-CTAVAL                   17350058
           MOVE VARC-CTAVAL20 (20:01) TO  DET1-CTAVAL-D                 17360058
           MOVE VCON-REFER            TO  DET1-REFERE                   17370058
           MOVE VCON-TITS             TO  DET1-CANACC                   17380058
           COMPUTE WA-COMISION = WA-COMISION + VCON-IMPOR2              17390058
                               + VCON-IMPOR3 + VCON-IMPOR4.             17400058
           MOVE WA-COMISION           TO  DET1-COMISI                   17410058
           MOVE VCON-IMPLIQ           TO  DET1-IMPORT                   17420058
           MOVE VCON-NUMCTA           TO  DET1-CTAECO                   17430058
           MOVE MDC-CDDIVIS           TO  DET1-MONCTA                   17440058
                                                                        17450058
           IF MIR-COD-ERROR-DEV = SPACES                                17460058
              MOVE 'O.K.      '       TO  DET1-OBSERV                   17470058
              ADD  WA-COMISION        TO  WT-COMISION                   17480058
              ADD  VCON-IMPLIQ        TO  WT-IMPLIQ                     17490058
           ELSE                                                         17500058
              MOVE MIR-COD-ERROR-DEV  TO  DET1-OBSERV (01:07)           17510058
              ADD  WA-COMISION        TO  WE-COMISION                   17520058
              ADD  VCON-IMPLIQ        TO  WE-IMPLIQ                     17530058
           END-IF.                                                      17540058
                                                                        17550058
           MOVE SPACES                TO  DET1-CLIENT                   17560058
           IF W520-SUJGRUP = 'F'                                        17570058
              STRING W520-PRIAPE DELIMITED BY '  ' ' '                  17580058
                     W520-SEGAPE DELIMITED BY '  ' ' '                  17590058
                     W520-NOMBRE DELIMITED BY '  '                      17600058
                                           INTO DET1-CLIENT             17610058
           ELSE                                                         17620058
              STRING W520-NOMBRE DELIMITED BY SIZE                      17630058
                     W520-PRIAPE DELIMITED BY SIZE                      17640058
                     W520-SEGAPE DELIMITED BY SIZE                      17650058
                                           INTO DET1-CLIENT             17660058
           END-IF.                                                      17670058
      *                                                                *17680058
           IF CONT-LIN > 60                                             17690058
              PERFORM 600-IMP-CABECERA                                  17700058
           END-IF.                                                      17710058
      *                                                                *17720058
           WRITE R-VLLS3050 FROM L06-VLLS3050 AFTER 1.                  17730058
           ADD   1           TO  CONT-LIN.                              17740058
      *                                                                *17750058
      *   *-------------*                                               17760058
       620-CALL-PE9C5000.                                               17770058
      *   *-------------*                                               17780058
      *                                                                *17790058
           INITIALIZE             W520-REGISTRO.                        17800058
           MOVE VARC-NUMCLI    TO W520-NUMCLIEN.                        17810058
           CALL PE9C5201    USING W-PEWC5201.                           17820058
      *                                                                *17830058
           EVALUATE W520-PECRETOR                                       17840058
               WHEN '00'                                                17850058
                    CONTINUE                                            17860058
               WHEN OTHER                                               17870058
                    MOVE '** NO EXISTE CODIGO '   TO W520-PRIAPE        17880058
                    MOVE 'PERSONA XXXXXXXX ***'   TO W520-SEGAPE        17890058
                    MOVE W520-NUMCLIEN            TO W520-SEGAPE (09:08)17900058
                    MOVE W520-PECRETOR            TO W520-SEGAPE (19:02)17910058
           END-EVALUATE.                                                17920058
      *                                                                *17930058
      *--------------------*                                            17940058
       200700-IMPRIME-TOTAL.                                            17950058
      *--------------------*                                            17960058
      *                                                                *17970058
           MOVE SPACES                     TO  L06-VLLS3050             17980058
           IF WT-IMPLIQ > ZEROS OR WE-IMPLIQ > ZEROS                    17990058
              MOVE WT-COMISION             TO  DET1-COMISI              18000058
              MOVE WT-IMPLIQ               TO  DET1-IMPORT              18010058
              MOVE 'TOTAL EVENTO O.K.:'    TO  DET1-CLIENT              18020058
              WRITE R-VLLS3050            FROM L06-VLLS3050 AFTER 1     18030058
              ADD   1                      TO  CONT-LIN                 18040058
              MOVE WE-COMISION             TO  DET1-COMISI              18050058
              MOVE WE-IMPLIQ               TO  DET1-IMPORT              18060058
              MOVE 'TOTAL EVENTO NO O.K.'  TO  DET1-CLIENT              18070058
              WRITE R-VLLS3050            FROM L06-VLLS3050 AFTER 1     18080058
              ADD   1                      TO  CONT-LIN                 18090058
           END-IF.                                                      18100058
           MOVE '-'                        TO  DET1-RAYA.               18110058
      *                                                                *18120058
      *    *-------------*                                              18130058
       200800-TIPO-CAMBIO.                                              18140058
      *    *-------------*                                              18150058
      *                                                                *18160058
           MOVE 'N'                TO SW-MONEDA-CRUZADA.                18170058
           MOVE 'N'                TO SW-MONEDA-EXTRANJ.                18180058
           MOVE VCON-OPERAC        TO SW-OPE-ABOCAR.                    18190058
      *                                                                *18200058
           IF VCON-MONEDA-CTA NOT = MDC-CDDIVIS                         18210058
              SET MONEDA-CRUZADA       TO TRUE                          18220058
              IF (MDC-CDDIVIS         NOT = CT-PEN) AND                 18230058
                 (VCON-MONEDA-CTA     NOT = CT-PEN)                     18240058
                 SET MONEDA-EXTRANJ    TO TRUE                          18250058
              END-IF                                                    18260058
              IF OPE-ABONO                                              18270058
                 IF MONEDA-EXTRANJ                                      18280058
                    SET COMPRA            TO TRUE                       18290058
                 ELSE                                                   18300058
                    IF MDC-CDDIVIS = CT-PEN                             18310058
                       SET COMPRA         TO TRUE                       18320058
                    ELSE                                                18330058
                       SET VENTA          TO TRUE                       18340058
                    END-IF                                              18350058
                 END-IF                                                 18360058
              ELSE                                                      18370058
                 IF MONEDA-EXTRANJ                                      18380058
                    SET VENTA             TO TRUE                       18390058
                 ELSE                                                   18400058
                    IF MDC-CDDIVIS = CT-PEN                             18410058
                       SET VENTA          TO TRUE                       18420058
                    ELSE                                                18430058
                       SET COMPRA         TO TRUE                       18440058
                    END-IF                                              18450058
                 END-IF                                                 18460058
              END-IF                                                    18470058
           END-IF.                                                      18480058
      ***                                                            ***18490058
      * SE LLAMA A LA RUTINA DE CAMBIO CON LA OPCION 1 (SIMULACION) :  *18500058
      *1°PARA QUE NO RETENGA LA TABLA GPDT021 Y ENTRE EN CONFLICTO CON *18510058
      *  OTROS PROCESOS (CAJEROS O BATCH).                             *18520058
      *2°ESTE PROCESO ES UN RASTREO Y SOLO DEBE DE DEJAR LA POSICION   *18530058
      *  DE TIPO DE CAMBIO A LOS PROCESADOS OK, QUE ES LUEGO DEL       *18540058
      *  BPJP8800 QUIEN REALIZA LAS IMPUTACIONES DE VALORES.           *18550058
      *3°LA ACTUALIZACION DE POSICION DE CAMBIO LA REALIZARA EL PGM.   *18560058
      *  VL4C9018.                                                     *18570058
      ***                                                            ***18580058
      *                                                                *18590058
           MOVE 1                    TO W950-GPCOPACC                   18600058
           MOVE 'VL'                 TO W950-GPAPLICA                   18610058
           MOVE VCON-REFER           TO W950-GPNOPERA                   18620058
           MOVE 0011                 TO W950-GPNBCORG                   18630058
           MOVE VCON-FILLER2 (01:04) TO W950-GPNOFORG                   18640058
           MOVE CT-VL4C3050          TO W950-GPNTRMIN                   18650058
           MOVE 'VALBATCH'           TO W950-GPCUSUAR                   18660058
VENTA      IF VENTA                                                     18670058
              MOVE '722  '           TO W950-GPCTPOPE                   18680058
COMPRA     ELSE                                                         18690058
              MOVE '721  '           TO W950-GPCTPOPE                   18700058
           END-IF.                                                      18710058
      *                                                                 18720058
           MOVE AAAA-SYS             TO W950-GPFPCPRO(1:4)              18730058
           MOVE '-'                  TO W950-GPFPCPRO(5:1)              18740058
           MOVE MM-SYS               TO W950-GPFPCPRO(6:2)              18750058
           MOVE '-'                  TO W950-GPFPCPRO(8:1)              18760058
           MOVE DD-SYS               TO W950-GPFPCPRO(9:2)              18770058
           MOVE 'O'                  TO W950-INDDIVBI                   18780058
      ***                                                            ***18790058
      *  SEGUN G. VIVES ENVIAR ESTOS VALORES PARA LIQUIDACION QUE NO   *18800058
      *  SEAN DE : PEN -> USD  o  USD -> PEN, HASTA QUE SE MODIFIQUE   *18810058
      *  ESTA RUTINA.                                                  *18820058
      *  MOVE '8'                    TO W950-GPCCTABO o W950-GPCCTADE  *18830058
      *  MOVE 'OC'                   TO W950-GPNCTABO o W950-GPNCTADE  *18840058
      ***                                                            ***18850058
           IF MDC-CDDIVIS NOT = CT-PEN                                  18860058
              IF VCON-MONEDA-CTA NOT = CT-PEN                           18870058
                 IF COMPRA                                              18880058
PEN                 MOVE VCON-MONEDA-CTA      TO W950-GPCDVADE          18890058
                    MOVE '8'                  TO W950-GPCCTADE          18900058
                    MOVE CT-T5                TO W950-GPNCTADE          18910058
                    IF VCON-IMPLIQ NOT = 0                              18920058
                       MOVE VCON-IMPLIQ       TO W950-GPINOADE          18930058
                    ELSE                                                18940058
                       MOVE 1                 TO W950-GPINOADE          18950058
                    END-IF                                              18960058
                                                                        18970058
???                 MOVE MDC-CDDIVIS          TO W950-GPCDVABO          18980058
                    MOVE '8'                  TO W950-GPCCTABO          18990058
                    MOVE 'OC'                 TO W950-GPNCTABO          19000058
                    INITIALIZE                   W950-GPINOABO          19010058
                 ELSE                                                   19020058
???                 MOVE MDC-CDDIVIS          TO W950-GPCDVADE          19030058
                    MOVE '8'                  TO W950-GPCCTADE          19040058
                    MOVE 'OC'                 TO W950-GPNCTADE          19050058
                    INITIALIZE                   W950-GPINOADE          19060058
                                                                        19070058
PEN                 MOVE VCON-MONEDA-CTA      TO W950-GPCDVABO          19080058
                    MOVE '8'                  TO W950-GPCCTABO          19090058
                    MOVE CT-T5                TO W950-GPNCTABO          19100058
                    IF VCON-IMPLIQ NOT = 0                              19110058
                       MOVE VCON-IMPLIQ       TO W950-GPINOABO          19120058
                    ELSE                                                19130058
                       MOVE 1                 TO W950-GPINOABO          19140058
                    END-IF                                              19150058
                 END-IF                                                 19160058
              ELSE                                                      19170058
                 IF COMPRA                                              19180058
???                 MOVE MDC-CDDIVIS          TO W950-GPCDVADE          19190058
                    IF MDC-CDDIVIS = CT-USD                             19200058
                       MOVE '1'               TO W950-GPCCTADE          19210058
                       MOVE VCON-NUMCTA       TO W950-GPNCTADE          19220058
                    ELSE                                                19230058
                       MOVE '8'               TO W950-GPCCTADE          19240058
                       MOVE 'OC'              TO W950-GPNCTADE          19250058
                    END-IF                                              19260058
                    IF VCON-IMPLIQ NOT = 0                              19270058
                       MOVE VCON-IMPLIQ       TO W950-GPINOADE          19280058
                    ELSE                                                19290058
                       MOVE 1                 TO W950-GPINOADE          19300058
                    END-IF                                              19310058
                                                                        19320058
PEN                 MOVE VCON-MONEDA-CTA      TO W950-GPCDVABO          19330058
                    MOVE '8'                  TO W950-GPCCTABO          19340058
                    MOVE CT-T5                TO W950-GPNCTABO          19350058
                    INITIALIZE                   W950-GPINOABO          19360058
                 ELSE                                                   19370058
PEN                 MOVE VCON-MONEDA-CTA      TO W950-GPCDVADE          19380058
                    MOVE '8'                  TO W950-GPCCTADE          19390058
                    MOVE CT-T5                TO W950-GPNCTADE          19400058
                    INITIALIZE                   W950-GPINOADE          19410058
                                                                        19420058
                    MOVE MDC-CDDIVIS          TO W950-GPCDVABO          19430058
                    IF MDC-CDDIVIS = CT-USD                             19440058
                       MOVE '1'               TO W950-GPCCTABO          19450058
                       MOVE VCON-NUMCTA       TO W950-GPNCTABO          19460058
                    ELSE                                                19470058
                       MOVE '8'               TO W950-GPCCTABO          19480058
                       MOVE 'OC'              TO W950-GPNCTABO          19490058
                    END-IF                                              19500058
                    IF VCON-IMPLIQ NOT = 0                              19510058
                       MOVE VCON-IMPLIQ       TO W950-GPINOABO          19520058
                    ELSE                                                19530058
                       MOVE 1                 TO W950-GPINOABO          19540058
                    END-IF                                              19550058
                 END-IF                                                 19560058
              END-IF                                                    19570058
           ELSE                                                         19580058
              IF COMPRA                                                 19590058
                 MOVE VCON-MONEDA-CTA      TO W950-GPCDVADE             19600058
                 MOVE '8'                  TO W950-GPCCTADE             19610058
                 IF VCON-MONEDA-CTA = CT-USD                            19620058
                    MOVE CT-T5             TO W950-GPNCTADE             19630058
                 ELSE                                                   19640058
                    MOVE 'OC'              TO W950-GPNCTADE             19650058
                 END-IF                                                 19660058
                 INITIALIZE                   W950-GPINOADE             19670058
PEN              MOVE MDC-CDDIVIS          TO W950-GPCDVABO             19680058
                 MOVE '1'                  TO W950-GPCCTABO             19690058
                 MOVE VCON-NUMCTA          TO W950-GPNCTABO             19700058
                 IF VCON-IMPLIQ NOT = 0                                 19710058
                    MOVE VCON-IMPLIQ       TO W950-GPINOABO             19720058
                 ELSE                                                   19730058
                    MOVE 1                 TO W950-GPINOABO             19740058
                 END-IF                                                 19750058
              ELSE                                                      19760058
PEN              MOVE MDC-CDDIVIS          TO W950-GPCDVADE             19770058
                 MOVE '1'                  TO W950-GPCCTADE             19780058
                 MOVE VCON-NUMCTA          TO W950-GPNCTADE             19790058
                 IF VCON-IMPLIQ NOT = 0                                 19800058
                    MOVE VCON-IMPLIQ       TO W950-GPINOADE             19810058
                 ELSE                                                   19820058
                    MOVE 1                 TO W950-GPINOADE             19830058
                 END-IF                                                 19840058
???              MOVE VCON-MONEDA-CTA      TO W950-GPCDVABO             19850058
                 MOVE '8'                  TO W950-GPCCTABO             19860058
                 IF VCON-MONEDA-CTA = CT-USD                            19870058
                    MOVE CT-T5             TO W950-GPNCTABO             19880058
                 ELSE                                                   19890058
                    MOVE 'OC'              TO W950-GPNCTABO             19900058
                 END-IF                                                 19910058
                 INITIALIZE                   W950-GPINOABO             19920058
              END-IF                                                    19930058
           END-IF                                                       19940058
           MOVE SPACES                     TO W950-GPFVIVBI.            19950058
           ADD  1                          TO WA-CALL-GP8C1950.         19960058
      *                                                                *19970058
           MOVE 'VL4C3050 CALCULO DEL CAMBIO DE LA MONEDA'              19980058
                                                 TO W950-GPDATADI.      19990058
      *                                                                *20000058
           CALL CT-GP8C1950 USING GPWC950.                              20010058
      *                                                                *20020058
           IF W950-GPCRTMOD = '00'                                      20030058
JPC@4         MOVE W950-GPCAMADE          TO WTC-GPCAMADE               20040058
JPC@4         MOVE W950-GPCAMABO          TO WTC-GPCAMABO               20050058
              IF COMPRA                                                 20060058
                 IF MONEDA-EXTRANJ                                      20070058
                    COMPUTE W-CAMBIO = W950-GPCAMADE / W950-GPCAMABO    20080058
                    MOVE W-CAMBIO         TO W-WOPS-TIPO-CAMBIO         20090058
                    MOVE W950-GPCAMADE    TO W-CAMBIO-PEN               20100058
                 ELSE                                                   20110058
                    MOVE W950-GPCAMADE    TO W-WOPS-TIPO-CAMBIO         20120058
                    IF OPE-ABONO                                        20130058
                       MOVE W950-GPCAMADE TO W-CAMBIO                   20140058
                    ELSE                                                20150058
                       COMPUTE W-CAMBIO   =  1 / W950-GPCAMADE          20160058
                    END-IF                                              20170058
                    MOVE W-CAMBIO         TO W-CAMBIO-PEN               20180058
                 END-IF                                                 20190058
              ELSE                                                      20200058
                 IF MONEDA-EXTRANJ                                      20210058
                    COMPUTE W-CAMBIO = W950-GPCAMABO / W950-GPCAMADE    20220058
                    MOVE W-CAMBIO         TO W-WOPS-TIPO-CAMBIO         20230058
                    MOVE W950-GPCAMABO    TO W-CAMBIO-PEN               20240058
                 ELSE                                                   20250058
                    MOVE W950-GPCAMABO    TO W-WOPS-TIPO-CAMBIO         20260058
                    IF OPE-ABONO                                        20270058
                       COMPUTE W-CAMBIO   =  1 / W950-GPCAMABO          20280058
                    ELSE                                                20290058
                       MOVE W950-GPCAMABO TO W-CAMBIO                   20300058
                    END-IF                                              20310058
                    MOVE W-CAMBIO         TO W-CAMBIO-PEN               20320058
                 END-IF                                                 20330058
              END-IF                                                    20340058
           ELSE                                                         20350058
              MOVE CT-VL4C3050          TO WRUT-PROGRAMA                20360058
              MOVE W950-GPCRTMOD        TO WRUT-RETOR                   20370058
              MOVE VOPE-PAVAL           TO WRUT-ACCION (01:03)          20380058
              MOVE VOPE-VALOR           TO WRUT-ACCION (04:08)          20390058
              MOVE VOPE-ISIN            TO WRUT-ACCION (12:01)          20400058
              MOVE VOPE-FECHOP          TO WRUT-ACCION (14:08)          20410058
              MOVE MIR-CCC              TO WRUT-CLAVE  (01:20)          20420058
              MOVE VDET-CTAVAL          TO WRUT-CLAVE  (22:07)          20430058
              MOVE W950-GPCCOSQL        TO W801-SQLCODE                 20440058
              MOVE '200800-TIPO-CAMBIO' TO WRUT-PARRAFO                 20450058
              PERFORM VLPCRUTI-DISP-ABEND-RUTI                          20460058
              DISPLAY W801-MSG8                                         20470058
              PERFORM VLPCRUTI-ABEND-RUTI                               20480058
           END-IF.                                                      20490058
      ****                                                          ****20500058
      *    CONVERSION A MONEDA DE LA CUENTA ECONOMICA                  *20510058
      ****                                                          ****20520058
           COMPUTE W-IMPOR1 ROUNDED = W-IMPOR1 * W-CAMBIO               20530058
           COMPUTE W-IMPOR2 ROUNDED = W-IMPOR2 * W-CAMBIO               20540058
           COMPUTE W-IMPOR3 ROUNDED = W-IMPOR3 * W-CAMBIO               20550058
           COMPUTE W-IMPOR4 ROUNDED = W-IMPOR4 * W-CAMBIO               20560058
           COMPUTE W-IMPOR5 ROUNDED = W-IMPOR5 * W-CAMBIO               20570058
           COMPUTE W-IMPOR6 ROUNDED = W-IMPOR6 * W-CAMBIO               20580058
           COMPUTE W-IMPOR7 ROUNDED = W-IMPOR7 * W-CAMBIO               20590058
           COMPUTE W-IMPOR8 ROUNDED = W-IMPOR8 * W-CAMBIO               20600058
           COMPUTE W-IMPOR9 ROUNDED = W-IMPOR9 * W-CAMBIO               20610058
                                                                        20620058
           EVALUATE VCON-OPERAC                                         20630058
               WHEN 07                                                  20640058
               WHEN 08 COMPUTE W-IMPLIQ = W-IMPOR1 - W-IMPOR2           20650058
                                        - W-IMPOR3 - W-IMPOR4           20660058
                                        - W-IMPOR5 - W-IMPOR6           20670058
                                        - W-IMPOR9                      20680058
                    IF VCON-IMPLIQ = ZEROS                              20690058
                       COMPUTE W-IMPOR1 =          + W-IMPOR2           20700058
                                        + W-IMPOR3 + W-IMPOR4           20710058
                                        + W-IMPOR5 + W-IMPOR6           20720058
                    END-IF                                              20730058
                                                                        20740058
               WHEN 11 COMPUTE W-IMPLIQ = W-IMPOR1 - W-IMPOR2           20750058
                                        - W-IMPOR3 - W-IMPOR4           20760058
                                        - W-IMPOR5 - W-IMPOR6           20770058
                                                                        20780058
               WHEN 34 COMPUTE W-IMPLIQ = W-IMPOR1 + W-IMPOR2           20790058
                                        + W-IMPOR3 + W-IMPOR4           20800058
           END-EVALUATE                                                 20810058
           IF VCON-IMPLIQ = ZEROS                                       20820058
              MOVE ZEROS    TO  W-IMPLIQ                                20830058
           END-IF.                                                      20840058
      *                                                                 20850058
      *    *-----------------*                                          20860058
       200800-TIPO-CAMBIO-FIN.                                          20870058
      *    *-----------------*                                          20880058
           EXIT.                                                        20890058
      *-------------------*                                             20900058
      *999-COPYS-ERRORES. *                                             20910058
      *-------------------*                                             20920058
           COPY  QRWCDB20.                                              20930058
           COPY  VLPC8010.                                              20940058
           COPY  VLPC8020.                                              20950058
           COPY  VLPCRUTI.                                              20960058
      *-------------------*                                             20970058
      *  FIN DE PROGRAMA  *                                             20980058
      *-------------------*                                             20990058
