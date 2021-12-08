      *I*PANTALLA MANTENIMIENTO CUENTAS-VALORES                *VL2C101000001014
      *A*DESCRIPCION:                                                   00002014
      *                                                                 00003014
      *     ALTAS, BAJAS, MODIFICACIONES Y CONSULTAS A CUENTAS DE VALO--00004014
      *     RES, CON UN NUMERO DE CLIENTE QUE ES EL TITULAR PRINCIPAL.  00005014
      *                                                                 00006014
       IDENTIFICATION DIVISION.                                         00007014
      *------------------------                                         00008014
       PROGRAM-ID.    VL2C1010.                                         00009014
       AUTHOR.        C.P.SOFTWARE.                                     00010014
      ******************************************************************00011014
      *REF.PETIC FECHA-MOD. PROGRAMADOR      DESCRIPCION               *00012014
      *--------- ---------- ---------------- --------------------------*00013014
      *200310189 18-08-2004 JHONNY PINEDO C. IMPLEMENTAR APERTURA DE   *00014014
      *                                      CTA-VALOR VINCULADO A UNA *00015014
      *                                      CUENTA DE CONTROL SOLO PARA00016014
      *                                      LA CONTINENTAL BOLSA      *00017014
      *--------- ---------- ---------------- --------------------------*00018014
      *200306088 25-01-2005 JHONNY PINEDO C. NO PERMITIR REACTIVAR CTAS*00019014
      *                                      INACTIVAS, AHORA SE CONSI-*00020014
      *                                      DERARAN COMO CANCELADAS   *00021014
      *--------- ---------- ---------------- --------------------------*00022014
      *200503172 28-03-2005 JHONNY PINEDO C. SE ADICIONA CODIGO CUSTODIO00023014
      *                                      INTERNACIONAL PARA CUENTAS*00024014
      *                                      CON TIPO CLIENTE BROKER   *00025014
      *--------- ---------- ---------------- --------------------------*00026014
      *200509007 07-10-2005 JHONNY PINEDO C. VALIDAR CUENTA CONTROL PARA00027014
      *                                      CAMBIO DE TARIFA          *00028014
      *--------- ---------- ---------------- --------------------------*00029014
      *200512055 05-11-2005 JHONNY PINEDO C. PERMITIR CAMBIAR TIPO DE  *00030014
      *                                      CLIENTE Y VALIDAR CUSTODIO*00031014
      *                                      DE TITULOS Y VALIDAR NUEVOS00032014
      *                                      VALORES PARA DETERMINAR SI*00033014
      *                                      ES EMPLEADO               *00034014
      *--------- ---------- ---------------- --------------------------*00035014
      *200605161 24-05-2006 JHONNY PINEDO C. CUENTA DE CONTROL PARA CUS*00036014
      *                                      TODIO 0011 Y 0312.        *00037014
      *--------- ---------- ---------------- --------------------------*00038014
      *200608070 24-08-2006 JHONNY PINEDO C. PERMITIR CUENTA ECONOMICAS*00039014
      *                                      DEL PRODUCTO 49 CON SUB-PRO00040014
      *                                      DUCTO 20/21/26/27.        *00041014
      *--------- ---------- ---------------- --------------------------*00042014
      *200509171 08-01-2007 JHONNY PINEDO C. VALIDAR CUENTAS ECONOMICAS*00043014
      *                                      PARA ADMINISTRACION DE    *00044014
      *                                      PORTAFOLIO.               *00045014
      *--------- ---------- ---------------- --------------------------*00046014
      *200702146 25-04-2007 JHONNY PINEDO C. CAMBIO DE CONTRATO JET FORM00047014
      *                                      PARA CUSTODIA 0069        *00048014
      *--------- ---------- ---------------- --------------------------*00049014
      *200703156 14-12-2007 KATTIA SACCSA J. NUEVA FUNCION BUSQUEDA DE *00050014
      *                                      CLIENTE POR RUT O NOMBRE  *00051014
      *--------- ---------- ---------------- --------------------------*00052014
      *200711038 07-02-2008 KATTIA SACCSA J. CAMBIO DEL CODIGO DE PROD.*00053014
      *                                      DE 49 A 91.               *00054014
      *--------- ---------- ---------------- --------------------------*00055014
      *200806094 13-06-2008 JHONNY PINEDO C. VALIDAR CAMBIO DE TARIFA  *00056014
      *                                      CUSTODIO PARA CTA-REGISTRO*00057014
      *--------- ---------- ---------------- --------------------------*00058014
      *200801059 23-06-2008 A. CASTAñEDA C.. ADICIONA CODIGO  C  PARA  *00059014
      *                                      CONDICIONES ESPECIALE BCO.*00060014
      *                                      CUSTODIO                  *00061014
      *--------- ---------- ---------------- --------------------------*00062014
      *200808196 18-09-2008 JHONNY PINEDO C. VALIDAR CUENTA CONTROL CON*00063014
      *                                      TARIFA 51 COMO VALIDO PARA*00064014
      *                                      OFICINA 0542 (SAB)        *00065014
      *--------- ---------- ---------------- --------------------------*00066014
      *200804248 17-11-2008 JHONNY PINEDO C °DELETE DE COLA IMPERSION  *00067014
      *                                     °NO MODIFICAR CTA REGISTRO *00068014
      *                                      SI TIENE SALDO ECONOMICO. *00069014
      *--------- ---------- ---------------- --------------------------*00070014
JPC@1 *200906119 18-11-2009 JHONNY PINEDO C °NO VALIDAR SITUACION DE   *00071014
      *                                      CONTRATO EN TABLA VLDTAPC *00072014
      *--------- ---------- ---------------- --------------------------*00073014
JPC@2 *200909071 03-12-2009 JHONNY PINEDO C °SOLICITAR DATOS DE CAMPAÑA*00074014
      *                                      PROYECTO INTI.            *00075014
      *--------- ---------- ---------------- --------------------------*00076014
JPC@3 *200905113 21-12-2009 JHONNY PINEDO C °VALIDAR SITUACION CONTRATO*00077014
      * REACTIVAR PARA PORTAFOLIO            PORTAFOLIO PARA BAJA.     *00078014
      *--------- ---------- ---------------- --------------------------*00079014
JPC@4 *201002213 27-04-2010 JHONNY PINEDO C.°VALIDAR CANAL/SUBCANAL/GES-00080014
      *                                      TOR DE VENTA NO VALIDOS CON00081014
      *                                      CODIGO RETORNO 80, 81 O 90*00082014
      *201004XXX 27-04-2010 JHONNY PINEDO C.°MOVER OFICINA GESTORA COMO*00083014
      *                                      OFICINA PROPIETARIA CONTRA-00084014
      *                                      TO VALOR.                 *00085014
      *--------- ---------- ---------------- --------------------------*00086014
      ******************************************************************00087014
      *--------- ---------- ---------------- --------------------------*00088014
@ZAL  *200712034 06-05-2010 ZHENIA ARTEAGA   CAMBIO DE VARIABLE        *00089014
      *                                      VARC-NUMMAN  POR          *00090014
      *                                      VARC-GRUPO-CTAS           *00091014
      *--------- ---------- ---------------- --------------------------*00092014
JPC@5 *201107005 22-08-2011 JHONNY PINEDO C. SI CTA-ALTERNANTE DE PORTA*00093014
      *                                      FOLIO ESTA INACTIVA NO IR *00094014
      *                                      A PARRAFO INACTIVAR.      *00095014
      *                                     °MAXIMO CTA-VALOR POR OFIC.*00096014
      *                                      ES UNO POR MONEDA.        *00097014
      *--------- ---------- ---------------- --------------------------*00098014
JPC@6 *201402029 04-09-2014 JHONNY PINEDO C. PERMITIR CUSTODIO TITULOS *00099014
      *                                      A CUSTODIO 0312.          *00100014
      *--------- ---------- ---------------- --------------------------*00101014
JPC@7 *201410050 10-11-2014 JHONNY PINEDO C. RUTINA VALIDA ESTADO FATCA*00102014
      *                                      DEL CLIENTE.              *00103014
      *---------- ---------- --------------- --------------------------*00104014
EZS@1 *6762018034 28-09-2018 EDGAR ZAVALETA  BLOQUEO AUTOMATICO DE APER*00105014
      *                                      TURA CTA VALOR CLIENTE    *00106014
      *                                      INELEGIBLE                *00107014
      ******************************************************************00108014
       ENVIRONMENT DIVISION.                                            00109014
      *-------------------------                                        00110014
       CONFIGURATION SECTION.                                           00111014
       SPECIAL-NAMES.                                                   00112014
           DECIMAL-POINT IS COMMA.                                      00113014
      *                                                                 00114014
       DATA DIVISION.                                                   00115014
      *-------------*                                                   00116014
       WORKING-STORAGE SECTION.                                         00117014
      *************************                                         00118014
       01  VL7CRLOG                     PIC X(008)  VALUE 'VL7CRLOG'.   00119014
       01  VL7CJETF                     PIC X(008)  VALUE 'VL7CJETF'.   00120014
       01  BG7CAPE4                     PIC X(008)  VALUE 'BG7CAPE4'.   00121014
       01  TC2C1000                     PIC X(008)  VALUE 'TC2C1000'.   00122014
       01  TC2C1500                     PIC X(008)  VALUE 'TC2C1500'.   00123014
       01  TC2C1700                     PIC X(008)  VALUE 'TC2C1700'.   00124014
       01  TC2C1820                     PIC X(008)  VALUE 'TC2C1820'.   00125014
       01  PE2C5000                     PIC X(008)  VALUE 'PE2C5000'.   00126014
       01  PE2C5100                     PIC X(008)  VALUE 'PE2C5100'.   00127014
       01  PE2C5201                     PIC X(008)  VALUE 'PE2C5201'.   00128014
       01  PE2C5390                     PIC X(008)  VALUE 'PE2C5390'.   00129014
       01  PE2C5400                     PIC X(008)  VALUE 'PE2C5400'.   00130014
       01  PE2C6000                     PIC X(008)  VALUE 'PE2C6000'.   00131014
       01  BG2CMDC0                     PIC X(008)  VALUE 'BG2CMDC0'.   00132014
       01  BG2CMSC0                     PIC X(008)  VALUE 'BG2CMSC0'.   00133014
       01  BR2CCDE0                     PIC X(008)  VALUE 'BR2CCDE0'.   00134014
       01  QG1CABC                      PIC X(008)  VALUE 'QG1CABC'.    00135014
       01  LE6CCFA0                     PIC X(008)  VALUE 'LE6CCFA0'.   00136014
JPC@2  01  SM7CNIN0                     PIC X(008)  VALUE 'SM7CNIN0'.   00137014
JPC@7  01  VL7C0088                     PIC X(008)  VALUE 'VL7C0088'.   00138014
EZS@1  01  PE7C4140                     PIC X(008)  VALUE 'PE7C4140'.   00139014
      *                                                                 00140014
       01  W-CCC-CAR-JET.                                               00141014
           03  W-ENT-CAR-JET            PIC 9(004)  VALUE ZEROES.       00142014
           03  W-SUC-CAR-JET            PIC 9(004)  VALUE ZEROES.       00143014
           03  W-DIG-CAR-JET            PIC 9(002)  VALUE ZEROES.       00144014
           03  W-CUENTA-JET             PIC 9(010)  VALUE ZEROES.       00145014
      *                                                                 00146014
       01  WA-COD-ERROR                 PIC X(007)  VALUE SPACES.       00147014
       01  WA-CTA-091                   PIC X(020)  VALUE SPACES.       00148014
       01  W-VARIABLES.                                                 00149014
LERS       03 SW-DET                    PIC 9(001)  VALUE 0.            00150014
           03 W-NUMECTA.                                                00151014
              05 W-NUMECTA-N            PIC 9(008)  VALUE ZEROS.        00152014
           03 W-CUSTINT.                                                00153014
              05 W-CUSTINT-N            PIC 9(004)  VALUE ZEROS.        00154014
           03 W-DOMICILI1               PIC X(150)  VALUE SPACES.       00155014
           03 W-2DOS-TITULARES          PIC X(070)  VALUE SPACES.       00156014
           03 W-2DOS-DIRECC             PIC X(070)  VALUE SPACES.       00157014
           03 W-TIPO-VINCUL             PIC X(020)  VALUE SPACES.       00158014
           03 W-FACULTAD                PIC X(092)  VALUE SPACES.       00159014
           03 W-JURIDI                  PIC X(001)  VALUE SPACES.       00160014
           03 W-SITUAC                  PIC X(001)  VALUE SPACES.       00161014
           03 W-COUNT                   PIC S9(04)  COMP  VALUE ZEROES. 00162014
           03 W-COUNT1                  PIC S9(04)  COMP  VALUE ZEROES. 00163014
      *    03 W-COUNT2                  PIC S9(04)  COMP  VALUE ZEROES. 00164014
           03 W-SQLCODE-NUM             PIC S9(04)  VALUE ZEROS.        00165014
           03 W-SQLCODE-EDIT            PIC +ZZZZ   VALUE ZEROS.        00166014
           03 W-SDOECON-EDIT            PIC ----.---.---,--.            00167014
           03 DCO0101-N                 PIC 9(003)  VALUE ZEROS.        00168014
           03 OFI-PRO.                                                  00169014
              05 OFI-PRO-N              PIC 9(004)  VALUE ZEROS.        00170014
           03 W-CTA0101.                                                00171014
              05 CTA0101-N              PIC 9(007)  VALUE ZEROS.        00172014
      *                                                                 00173014
JPC@4  01  VARIABLES-OFICINA.                                           00174014
JPC@4      02 IN-01                     PIC 9(004)  VALUE ZEROS.        00175014
JPC@5      02 IN-02                     PIC 9(004)  VALUE ZEROS.        00176014
JPC@5      02 IN-03                     PIC 9(004)  VALUE ZEROS.        00177014
JPC@4      02 WARC-NUMCLI               PIC S9(08)  COMP-3.             00178014
JPC@4      02 WARC-SITUAC               PIC X(001)  VALUE SPACES.       00179014
JPC@4      02 WARC-MONEDA               PIC X(003)  VALUE SPACES.       00180014
JPC@4      02 WARC-SUCURS               PIC S9(04)  COMP-3.             00181014
JPC@4      02 TB-CUENTAS-OFI.                                           00182014
JPC@4         04 TB-SUCURS OCCURS 20    PIC 9(004).                     00183014
      *                                                                 00184014
       01  W-MSG-2DOS-TIT.                                              00185014
           03 W-MSG-001                 PIC X(080)  VALUE               00186014
           'ADMINISTRACION DE CARTERA : COMPRA/VENTA VALORES CON CUENTAS00187014
      -    ' PROPIAS            '.                                      00188014
           03 W-MSG-002                 PIC X(080)  VALUE               00189014
           'ORDENAR COMPRAS/VENTAS DE VALORES CONTRA CUENTAS DEL TITULAR00190014
      -    ' DE LA CUENTA VALOR '.                                      00191014
           03 W-MSG-003                 PIC X(080)  VALUE               00192014
           'CONSULTAR SALDOS Y MOVIMIENTOS DE LA CUENTA VALOR           00193014
      -    '                    '.                                      00194014
           03 W-MSG-004                 PIC X(080)  VALUE               00195014
           'RECIBIR LOS BENEFICIOS DE VALORES DE RENTA FIJA Y RENTA VARI00196014
      -    'ABLE                '.                                      00197014
      *                                                                 00198014
       01  W-CUENTA-TOTAL.                                              00199014
           03  W-ENTIDAD-NUEVA          PIC 9(004)  VALUE ZEROES.       00200014
           03  W-OFICINA-NUEVA          PIC 9(004)  VALUE ZEROES.       00201014
           03  W-DIGCON-NUEVA           PIC 9(002)  VALUE ZEROES.       00202014
           03  W-CODISER-NUEVA          PIC 9(002)  VALUE ZEROES.       00203014
           03  W-CUENTA-NUEVA           PIC 9(008)  VALUE ZEROES.       00204014
      *                                                                 00205014
       01  W-CUENTA-ANTERIOR.                                           00206014
           03  W-ENTIDAD-ANT            PIC 9(004)  VALUE ZEROES.       00207014
           03  W-OFICINA-ANT            PIC 9(004)  VALUE ZEROES.       00208014
           03  W-DIGCON-ANT             PIC 9(002)  VALUE ZEROES.       00209014
           03  W-CODISER-ANT            PIC 9(002)  VALUE ZEROES.       00210014
           03  W-CUENTA-ANT             PIC 9(008)  VALUE ZEROES.       00211014
      *                                                                 00212014
       01  SW-HAYMAS                    PIC X(001)  VALUE 'N'.          00213014
      *                                                                 00214014
       01  W-AREA-VIAJA.                                                00215014
           03 OPT-COMM                  PIC X(001).                     00216014
           03 MSB-COMM                  PIC X(001).                     00217014
           03 CTA0101-COMM.                                             00218014
              05 CTA0101-COMM-N         PIC 9(007).                     00219014
           03 SUC0101-COMM.                                             00220014
              05 SUC0101-COMM-N         PIC 9(004).                     00221014
           03 NCC0101-COMM              PIC X(020).                     00222014
           03 NC20101-COMM              PIC X(020).                     00223014
           03 ENT0101-COMM.                                             00224014
              05 ENT0101-COMM-N         PIC 9(004).                     00225014
           03 TIT0101-COMM              PIC X(008).                     00226014
           03 OTROS-DATOS.                                              00227014
              05 SW-CUSTODIA-AL-CLIENTE PIC X(002).                     00228014
                 88 CUSTODIA-AL-CLIENTE             VALUE 'SI'.         00229014
              05 SW-OPERA-BOLSA         PIC X(002).                     00230014
                 88 OPERA-BOLSA                     VALUE 'SI'.         00231014
              05 W-CTA-CAR-CUSTODIO     PIC X(020).                     00232014
              05 W-CTA-ABO-CUSTODIO     PIC X(020).                     00233014
              05 W-CTA-CAR-JUR          PIC X(020).                     00234014
              05 W-CTA-ABO-JUR          PIC X(020).                     00235014
              05 W-TARIFA-CUS           PIC 9(002).                     00236014
              05 WXMI-IMPALT            PIC X(001).                     00237014
              05 WXMI-TIPCUST           PIC X(001).                     00238014
      *200306088-INI                                                    00239014
           03 SITUACI-COMM              PIC X(001).                     00240014
      *200306088-FIN                                                    00241014
      *200703156-INI                                                    00242014
JPC@2 *    03  FILLER                   PIC X(760).                     00243014
JPC@2      03 GVT0101-COMM              PIC X(010).                     00244014
JPC@2      03 CAV0101-COMM              PIC X(002).                     00245014
JPC@2      03 SCV0101-COMM              PIC X(002).                     00246014
JPC@2      03 CAM0101-COMM              PIC X(012).                     00247014
JPC@4      03 CTAGLOB-COMM              PIC X(020).                     00248014
914        03  FILLER                   PIC X(714).                     00249014
      * AREA USADA POR TRX VL3L                                         00250014
           03  CODTRAN-COMM             PIC X(04).                      00251014
           03  CTA-COMM-X.                                              00252014
               05 CTA-COMM7             PIC 9(07).                      00253014
               05 CTA-COMM1             PIC 9(01).                      00254014
1000       03  FILLER                   PIC X(74).                      00255014
      *200703156-FIN                                                    00256014
       01  DATOS-AUXILIARES.                                            00257014
           03 W-CLIENTE-CUSTODIO.                                       00258014
              05 W-CLIENTE-CUSTODIO-N   PIC 9(008).                     00259014
           03 W-CARGO                   PIC X(020).                     00260014
           03 W-ABO                     PIC X(020).                     00261014
           03 W-CUENTA                  PIC 9(007).                     00262014
           03 W-ENTIDAD                 PIC 9(004).                     00263014
           03 W-ENTI                    PIC 9(004).                     00264014
           03 W-TITULAR                 PIC 9(008).                     00265014
           03 W-SUCVAL                  PIC 9(004).                     00266014
           03 W-CODBE                   PIC 9(004).                     00267014
           03 W-PAIS                    PIC 9(003).                     00268014
           03 W-TARIFA                  PIC 9(002).                     00269014
      *    03 W-TARIFA                  PIC 9(001).                     00270014
           03 W-IND                     PIC 9(003).                     00271014
           03 W-I                       PIC 9(003).                     00272014
           03 W-DOC                     PIC 9(003).                     00273014
           03 W-SEC                     PIC 9(003).                     00274014
           03 W-MONEDA-OK               PIC X(003).                     00275014
           03 W-MONEDA-CAR              PIC X(003).                     00276014
           03 W-MONEDA-ABO              PIC X(003).                     00277014
      *                                                                 00278014
       01  W-PAIS-CLI                   PIC X(004).                     00279014
      *                                                                 00280014
      * ------------------ FECHAS AUXILIARES ---------                  00281014
      *                                                                 00282014
       01  W-FECHA-AMD.                                                 00283014
           05  W-AA-AMD                 PIC 9(004).                     00284014
           05  W-MM-AMD                 PIC 9(002).                     00285014
           05  W-DD-AMD                 PIC 9(002).                     00286014
       01  W-FECHA-AMD-N REDEFINES W-FECHA-AMD PIC 9(8).                00287014
      *                                                                 00288014
       01  W-FECHA-DMA-G.                                               00289014
           05  W-DD-DMA-G               PIC 9(002).                     00290014
           05  FILLE1-G                 PIC X(001)  VALUE '-'.          00291014
           05  W-MM-DMA-G               PIC 9(002).                     00292014
           05  FILLE2-G                 PIC X(001)  VALUE '-'.          00293014
           05  W-AA-DMA-G               PIC 9(004).                     00294014
      *                                                                 00295014
       01  CLA-TELEX-AUX.                                               00296014
           05  CLTELEX-AUX              PIC X(010).                     00297014
           05  TELEX2-AUX               PIC X(002).                     00298014
      **                                                                00299014
       01 SWITCHES.                                                     00300014
          05 SW-PERSONAS                PIC X(002)  VALUE 'NO'.         00301014
             88 PERSONAS                            VALUE 'SI'.         00302014
          05 SW-PERSONA                 PIC X(001)  VALUE ZEROES.       00303014
             88 NATURAL                             VALUE 'N'.          00304014
             88 JURIDICA                            VALUE 'J'.          00305014
          05 SW-ES-CTAVAL-CUS           PIC X(002)  VALUE 'NO'.         00306014
             88 ES-CTAVAL-CUS                       VALUE 'SI'.         00307014
          05 SW-YA-CTAVAL-CUS           PIC X(002)  VALUE 'NO'.         00308014
             88 YA-CTAVAL-CUS                       VALUE 'SI'.         00309014
          05 SW-VALIDA-CAR              PIC X(002)  VALUE 'NO'.         00310014
             88 VALIDA-CAR                          VALUE 'SI'.         00311014
          05 SW-VALIDA-ABO              PIC X(002)  VALUE 'NO'.         00312014
             88 VALIDA-ABO                          VALUE 'SI'.         00313014
          05 SW-VALCTA                  PIC 9(001)  VALUE ZEROES.       00314014
             88 VALCAR                              VALUE 1.            00315014
             88 VALABO                              VALUE 2.            00316014
          05 SW-FIN-HIS                 PIC X(001)  VALUE SPACES.       00317014
             88 FIN-HIS                             VALUE '1'.          00318014
             88 FIN-HIS-OK                          VALUE '2'.          00319014
          05 SW-CORRECTO                PIC X(001)  VALUE SPACES.       00320014
             88 CORRECTO                            VALUE 'S'.          00321014
             88 NO-CORRECTO                         VALUE 'N'.          00322014
          05 SW-CTA-OK                  PIC X(002)  VALUE SPACES.       00323014
             88 CTA-OK                              VALUE 'SI'.         00324014
          05 SW-ENTRO                   PIC X(002)  VALUE 'NO'.         00325014
             88 ENTRO                               VALUE 'SI'.         00326014
          05 SW-CTACLI                  PIC X(002)  VALUE SPACES.       00327014
             88 CTACLI                              VALUE 'SI'.         00328014
          05 SW-CTA-ESPECIAL            PIC X(002)  VALUE SPACES.       00329014
             88 CTA-ESPECIAL                        VALUE 'SI'.         00330014
          05 SW-TECLA                   PIC X(002)  VALUE SPACES.       00331014
             88 PFENTER                             VALUE '00'.         00332014
             88 PF1                                 VALUE '01'.         00333014
             88 PF2                                 VALUE '02'.         00334014
             88 PF3                                 VALUE '03'.         00335014
             88 PF4                                 VALUE '04'.         00336014
             88 PF6                                 VALUE '06'.         00337014
             88 PF7                                 VALUE '07'.         00338014
             88 PF8                                 VALUE '08'.         00339014
      *200703156-INI                                                    00340014
             88 PF10                                VALUE '10'.         00341014
      *200703156-FIN                                                    00342014
      * --------------------- COPY -----------------------              00343014
      *                                                                 00344014
      *              COPY'S DE LAS RUTINAS                              00345014
      *01 FILLER   PIC X(40)    VALUE 'COPYS RUTINAS'                   00346014
           COPY VLWC8000.                                               00347014
                                                                        00348014
           COPY TCWC2020.                                               00349014
      *DIVISAS                                                          00350014
       COPY  TCWC1200.                                                  00351014
                                                                        00352014
      *200711038-INI                                                    00353014
      **************** COPY PARA CUENTA DE REGISTRO         ****        00354014
       01  W-BGECAPE4.                                                  00355014
           COPY BGECAPE4.                                               00356014
      *200711038-FIN                                                    00357014
      *                                                                 00358014
      **************** COPY PARA LA RUTINA VL7CRLOG         ****        00359014
       01  W-VLWCLOG0.                                                  00360014
           COPY VLWCLOG0.                                               00361014
                                                                        00362014
      **************** COPY PARA LA INTERFASE DE FACULTADES ****        00363014
       01  LEWCCFA0-01.                                                 00364014
           COPY LEWCCFA0.                                               00365014
JPC@7 **************** COPY SITUACION FATCA PARA CLIENTES   ****        00366014
JPC@7  01  VLWC0088-FATCA.                                              00367014
JPC@7      COPY VLWC0088.                                               00368014
      *                                                                 00369014
      ******* COPY PARA LA DESCRIPCION DE OFICINAS ****                 00370014
      *01  REG-TCWC0600.                                                00371014
      *    COPY TCWC0600.                                               00372014
      *                                                                 00373014
      *A2012-INICIO.                                                    00374014
      *                                                                 00375014
JPC@2 *   INFORMAR CUENTA VALOR A GESTOR DE CAMPAÑAS                    00376014
JPC@2  01  W-SMWCNIN0.                                                  00377014
JPC@2      COPY SMWCNIN0.                                               00378014
      ******* RUTINA QUE RECUPERA POBLACION Y FECHA****                 00379014
       01  TCWC1820-01.                                                 00380014
           COPY TCWC1820.                                               00381014
      *                                                                 00382014
      *A2012-FIN.                                                       00383014
      *200310189-INI CUENTA DE CONTROL                                  00384014
       01  BRWCCDE0-01.                                                 00385014
           COPY BRWCCDE0.                                               00386014
      *200310189-FIN                                                    00387014
                                                                        00388014
       01  BGECMSC-01.                                                  00389014
           COPY BGECMSC.                                                00390014
                                                                        00391014
       01  W-TCWC0300.                                                  00392014
           COPY TCWC0300.                                               00393014
                                                                        00394014
       01  W-TCWC0500.                                                  00395014
           COPY TCWC0500.                                               00396014
                                                                        00397014
       01  PEWC5000.                                                    00398014
           COPY PEWC5000.                                               00399014
                                                                        00400014
       01  PEWC5100.                                                    00401014
           COPY PEWC5100.                                               00402014
                                                                        00403014
       01  PEWC5201.                                                    00404014
           COPY PEWC5201.                                               00405014
                                                                        00406014
JPC@5 *01  PEWC5300.                                                    00407014
JPC@5 *    COPY PEWC5300.                                               00408014
                                                                        00409014
       01  PEWC8235.                                                    00410014
           COPY PEWC8235.                                               00411014
                                                                        00412014
       01  W-PEWC8730.                                                  00413014
           COPY PEWC8730.                                               00414014
                                                                        00415014
      *01  W-PEWC8M41.                                                  00416014
      *    COPY PEWC8M41.                                               00417014
                                                                        00418014
       01  W-PEWC4390.                                                  00419014
           COPY PEWC4390.                                               00420014
                                                                        00421014
       01  W-PEWC6000.                                                  00422014
           COPY PEWC6000.                                               00423014
                                                                        00424014
       01  PEWC5400.                                                    00425014
           COPY PEWC5400.                                               00426014
                                                                        00427014
       01  W-BGECMDC.                                                   00428014
           COPY BGECMDC.                                                00429014
                                                                        00430014
           COPY VLWCCTA0.                                               00431014
      ****************** COPY PARA LA LINK   TC2C1000 ******************00432014
       01 W-TCWC0000.                                                   00433014
          COPY TCWC0000.                                                00434014
      ****************** COPY PARA LA RECIBIR TC2C0100 *****************00435014
          COPY TCTC0100.                                                00436014
          COPY TCTC2600.                                                00437014
          COPY TCWC2010.                                                00438014
      *                                                                 00439014
EZS@1 ****************** COPY PARA  LA RUTINA PE7C4140 *****************00440014
  |    01 REGI-PEWC4140.                                                00441014
  |       COPY PEWC4140.                                                00442014
EZS@1 *****************  WORKING DE LAS COLAS TS  **********************00443014
      *                                                                 00444014
      *200909071-INI DATOS ANULADOS POR PROYECTO INTI                   00445014
       01  DATO-ANULADOS.                                               00446014
           02  TEL0101L    COMP PIC  S9(04).                            00447014
           02  TEL0101F         PIC   X(01).                            00448014
           02  FILLER REDEFINES TEL0101F.                               00449014
               03 TEL0201A      PIC   X(01).                            00450014
           02  TEL0101I         PIC   X(12).                            00451014
           02  FILLER REDEFINES TEL0101I.                               00452014
               03 TEL0101O      PIC   X(12).                            00453014
      * CUENTA PORTAFOLIO PRINCIPAL.                                    00454014
       01  XX-CUENTRA-PORT.                                             00455014
           02 END0101X  PIC X(4).                                       00456014
           02 CEN0101X  PIC X(4).                                       00457014
           02 DGT0101X  PIC X(2).                                       00458014
           02 PRD0101X  PIC X(2).                                       00459014
           02 CTA0101X  PIC X(7).                                       00460014
           02 DG20101X  PIC X(1).                                       00461014
      *200805013-FIN                                                    00462014
       01  W-TS.                                                        00463014
           10 W-NOMBRE-COLA                 PIC X(4)  VALUE SPACES.     00464014
           10 W-SUFIJO-TS                   PIC X(4)  VALUE SPACES.     00465014
      *                                                                 00466014
       01  W-LONG-TS                        PIC S9(4) COMP VALUE +0.    00467014
      *                                                                 00468014
       01  W-CONTENIDO-TS.                                              00469014
           10 W-NOMBRE-FORMATO              PIC X(8)  VALUE SPACES.     00470014
           10 W-CONT-FORMATO                PIC X(2000) VALUE SPACES.   00471014
                                                                        00472014
      *  COPY PARA JETFORM - COPY GENERICO                              00473014
           COPY VLNC9999.                                               00474014
      *  COPY PARA JETFORM2                                             00475014
           COPY VLWC0010.                                               00476014
      *200703046-INI                                                    00477014
           COPY VLWCJ291.                                               00478014
      *200703046-FIN                                                    00479014
                                                                        00480014
      *A2012-INICIO.                                                    00481014
      ** COPY PARA RUTINA VL7CJETF                                      00482014
       01 W-VLWCJETF.                                                   00483014
           COPY VLWCJETF.                                               00484014
      *A2012-FIN.                                                       00485014
                                                                        00486014
      ** COPY PARA EL PROGRAMA ABEND                                    00487014
       01  QGECABC-01.                                                  00488014
           COPY QGECABC.                                                00489014
                                                                        00490014
      ** COPY ATRIBUTOS                                                 00491014
           COPY DFHBMSCA.                                               00492014
           COPY DFHAID.                                                 00493014
      *                                                                 00494014
      *INCLUDE SQLCA.                                                   00495014
      *                                                                 00496014
           EXEC SQL INCLUDE SQLCA    END-EXEC.                          00497014
           EXEC SQL INCLUDE VLGTMES  END-EXEC.                          00498014
           EXEC SQL INCLUDE VLGTARC  END-EXEC.                          00499014
           EXEC SQL INCLUDE VLGTADS1 END-EXEC.                          00500014
           EXEC SQL INCLUDE VLGTADT  END-EXEC.                          00501014
           EXEC SQL INCLUDE VLGTHIS1 END-EXEC.                          00502014
           EXEC SQL INCLUDE VLGTXMO  END-EXEC.                          00503014
           EXEC SQL INCLUDE VLGTXTA  END-EXEC.                          00504014
           EXEC SQL INCLUDE VLGTXMI  END-EXEC.                          00505014
           EXEC SQL INCLUDE VLGTHAC1 END-EXEC.                          00506014
           EXEC SQL INCLUDE VLGTPRO  END-EXEC.                          00507014
           EXEC SQL INCLUDE VLGTRPR  END-EXEC.                          00508014
           EXEC SQL INCLUDE VLGTTRA1 END-EXEC.                          00509014
           EXEC SQL INCLUDE VLGTXAG  END-EXEC.                          00510014
      * LERS 09-07-2001                                                 00511014
           EXEC SQL INCLUDE VLGTDET  END-EXEC.                          00512014
           EXEC SQL INCLUDE VLGTOPE1 END-EXEC.                          00513014
      * LERS 09-07-2001                                                 00514014
           EXEC SQL INCLUDE VLGTXBO  END-EXEC.                          00515014
      *200509171-INI                                                    00516014
           EXEC SQL INCLUDE VLGTAPC  END-EXEC.                          00517014
      *200509171-FIN                                                    00518014
JPC@3      EXEC SQL INCLUDE VLGTFPF2 END-EXEC.                          00519014
      *                                                                 00520014
      *A2011-RUTLOG-I. INCLUDES DE LAS DCLGEN DEL LOG                   00521014
           EXEC SQL INCLUDE VLTCMES  END-EXEC.                          00522014
           EXEC SQL INCLUDE VLTCARC  END-EXEC.                          00523014
           EXEC SQL INCLUDE VLTCADS1 END-EXEC.                          00524014
           EXEC SQL INCLUDE VLTCADT  END-EXEC.                          00525014
      *    EXEC SQL INCLUDE VLTCHIS1 END-EXEC.                          00526014
           EXEC SQL INCLUDE VLTCXTA  END-EXEC.                          00527014
           EXEC SQL INCLUDE VLTCXMI  END-EXEC.                          00528014
      *    EXEC SQL INCLUDE VLTCHAC1 END-EXEC.                          00529014
           EXEC SQL INCLUDE VLTCPRO  END-EXEC.                          00530014
           EXEC SQL INCLUDE VLTCRPR  END-EXEC.                          00531014
           EXEC SQL INCLUDE VLTCTRA1 END-EXEC.                          00532014
JPC@3      EXEC SQL INCLUDE VLTCFPF2 END-EXEC.                          00533014
      *A2011-RUTLOG-F                                                   00534014
      *--------------------------------------------------------*        00535014
      *    D E C L A R E S      C U R S O R                    *        00536014
      *--------------------------------------------------------*        00537014
      *                                                                 00538014
           EXEC SQL                                                     00539014
                DECLARE VLDCADT1  CURSOR FOR                            00540014
JPC@1 *         SELECT  *                                               00541014
                SELECT  VADT_CUENTA                                     00542014
                     ,  VADT_NUMCLI                                     00543014
                     ,  VADT_CLTITU                                     00544014
                     ,  VADT_NUMDOM                                     00545014
                     ,  VADT_ADMIN                                      00546014
                     ,  VADT_FEVENCTO                                   00547014
                     ,  VADT_FEALTREG                                   00548014
                     ,  VADT_FEULMOD                                    00549014
                     ,  VADT_HORULMOD                                   00550014
                     ,  VADT_NUMTER                                     00551014
                     ,  VADT_USUARIO                                    00552014
                 FROM   VLDTADT                                         00553014
                WHERE   VADT_CUENTA  = :VADT-CUENTA                     00554014
                ORDER BY VADT_CLTITU                                    00555014
           END-EXEC.                                                    00556014
      *                                                                 00557014
JPC@4      EXEC SQL                                                     00558014
JPC@4           DECLARE VLDCARC CURSOR FOR                              00559014
JPC@4           SELECT  VARC_SUCURS                                     00560014
JPC@4            FROM   VLDTARC                                         00561014
JPC@4           WHERE   VARC_CUENTA > 0                                 00562014
JPC@4             AND   VARC_NUMCLI = :WARC-NUMCLI                      00563014
JPC@4             AND   VARC_SITUAC = :WARC-SITUAC                      00564014
JPC@4             AND   VARC_MONEDA = :WARC-MONEDA                      00565014
JPC@4             AND   VARC_CENTAD IN (0069, 2010)                     00566014
JPC@4      END-EXEC.                                                    00567014
      *                                                                 00568014
           EXEC SQL                                                     00569014
                DECLARE VLDCHIS1  CURSOR FOR                            00570014
                SELECT  VHIS_TITULOS1   ,                               00571014
                        VHIS_COBRADO1   ,                               00572014
                        VHIS_TITULOS2   ,                               00573014
                        VHIS_COBRADO2   ,                               00574014
                        VHIS_TITULOS3   ,                               00575014
                        VHIS_COBRADO3   ,                               00576014
                        VHIS_TITULOS4   ,                               00577014
                        VHIS_COBRADO4   ,                               00578014
                        VHIS_TITULOS5   ,                               00579014
                        VHIS_COBRADO5   ,                               00580014
                        VHIS_TITULOS6   ,                               00581014
                        VHIS_COBRADO6   ,                               00582014
                        VHIS_TITULOS7   ,                               00583014
                        VHIS_COBRADO7   ,                               00584014
                        VHIS_TITULOS8   ,                               00585014
                        VHIS_COBRADO8   ,                               00586014
                        VHIS_TITULOS9   ,                               00587014
                        VHIS_COBRADO9   ,                               00588014
                        VHIS_TITULOS10  ,                               00589014
                        VHIS_COBRADO10  ,                               00590014
                        VHIS_TITULOS11  ,                               00591014
                        VHIS_COBRADO11  ,                               00592014
                        VHIS_TITULOS12  ,                               00593014
                        VHIS_COBRADO12  ,                               00594014
                        VHIS_TITULOS13  ,                               00595014
                        VHIS_COBRADO13  ,                               00596014
                        VHIS_TITULOS14  ,                               00597014
                        VHIS_COBRADO14  ,                               00598014
                        VHIS_TITULOS15  ,                               00599014
                        VHIS_COBRADO15  ,                               00600014
                        VHIS_TITULOS16  ,                               00601014
                        VHIS_COBRADO16  ,                               00602014
                        VHIS_TITULOS17  ,                               00603014
                        VHIS_COBRADO17  ,                               00604014
                        VHIS_TITULOS18  ,                               00605014
                        VHIS_COBRADO18  ,                               00606014
                        VHIS_TITULOS19  ,                               00607014
                        VHIS_COBRADO19  ,                               00608014
                        VHIS_TITULOS20  ,                               00609014
                        VHIS_COBRADO20  ,                               00610014
                        VHIS_TITULOS21  ,                               00611014
                        VHIS_COBRADO21  ,                               00612014
                        VHIS_TITULOS22  ,                               00613014
                        VHIS_COBRADO22  ,                               00614014
                        VHIS_TITULOS23  ,                               00615014
                        VHIS_COBRADO23  ,                               00616014
                        VHIS_TITULOS24  ,                               00617014
                        VHIS_COBRADO24  ,                               00618014
                        VHIS_TITULOS25  ,                               00619014
                        VHIS_COBRADO25  ,                               00620014
                        VHIS_TITULOS26  ,                               00621014
                        VHIS_COBRADO26  ,                               00622014
                        VHIS_TITULOS27  ,                               00623014
                        VHIS_COBRADO27  ,                               00624014
                        VHIS_TITULOS28  ,                               00625014
                        VHIS_COBRADO28  ,                               00626014
                        VHIS_TITULOS29  ,                               00627014
                        VHIS_COBRADO29  ,                               00628014
                        VHIS_TITULOS30  ,                               00629014
                        VHIS_COBRADO30  ,                               00630014
                        VHIS_TITULOS31  ,                               00631014
                        VHIS_COBRADO31                                  00632014
                 FROM   VLDTHIS                                         00633014
                 WHERE  VHIS_CTAVAL    = :VHIS-CTAVAL                   00634014
                  AND   VHIS_CODVALOR >= :VHIS-CODVALOR                 00635014
                  AND   VHIS_TIPGAS   >= :VHIS-TIPGAS                   00636014
                  AND   VHIS_ANO      >= :VHIS-ANO                      00637014
                  AND   VHIS_MES      >= :VHIS-MES                      00638014
           END-EXEC.                                                    00639014
      *                                                                 00640014
LERS       EXEC SQL                                                     00641014
 09             DECLARE VLDCADET  CURSOR FOR                            00642014
 07   *JPC@1    SELECT  *                                               00643014
                SELECT  VDET_FECHOP                                     00644014
                     ,  VDET_PAVAL                                      00645014
                     ,  VDET_VALOR                                      00646014
                     ,  VDET_ISIN                                       00647014
                     ,  VDET_FORMAT                                     00648014
                     ,  VDET_CTAVAL                                     00649014
                     ,  VDET_CLAREG                                     00650014
                     ,  VDET_REFER                                      00651014
                     ,  VDET_DATOS_DETAL                                00652014
                     ,  VDET_FEALTREG                                   00653014
                     ,  VDET_FEULMOD                                    00654014
                     ,  VDET_HORULMOD                                   00655014
                     ,  VDET_NUMTER                                     00656014
                     ,  VDET_USUARIO                                    00657014
2001             FROM   VLDTDET                                         00658014
 |              WHERE   VDET_CTAVAL  = :VDET-CTAVAL                     00659014
LERS       END-EXEC.                                                    00660014
      *                                                                 00661014
       77  FILLER        PIC X(30) VALUE '**** FINAL DE WORKING ****'.  00662014
      *                                                                 00663014
       LINKAGE SECTION.                                                 00664014
       01  DFHCOMMAREA.                                                 00665014
      * COMMAREA DE APLICACIONES                                        00666014
           COPY QGECCAA.                                                00667014
      * DATOS PROPIOS DEL MANTENIMIENTO DE ARQUITECTURA                 00668014
           COPY VLECCMA.                                                00669014
      * COPY DE LA PANTALLA                                             00670014
           COPY VLNC010.                                                00671014
      ******************************************************************00672014
      *                                                                 00673014
       PROCEDURE DIVISION.                                              00674014
      *                                                                 00675014
           PERFORM 1-INICIO                                             00676014
              THRU 1-INICIO-FIN.                                        00677014
      *                                                                 00678014
           PERFORM 2-PROCESO                                            00679014
              THRU 2-PROCESO-FIN.                                       00680014
      *                                                                 00681014
           PERFORM 3-FINAL.                                             00682014
      *                                                                 00683014
       1-INICIO.                                                        00684014
      *                                                                 00685014
           INITIALIZE QGECABC.                                          00686014
      *                                                                 00687014
           EXEC CICS  IGNORE CONDITION ERROR END-EXEC                   00688014
      *                                                                 00689014
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         00690014
              MOVE 'ERROR CICS  VL2C1010'   TO ABC-REFERENCIA           00691014
              MOVE 'VL2C1010'               TO ABC-OBJETO-ERROR         00692014
              PERFORM 999-ABEND-CICS                                    00693014
           END-IF                                                       00694014
      *                                                                 00695014
           INITIALIZE     CAA-COD-AVISO1                                00696014
                          CAA-COD-AVISO2                                00697014
                          CAA-COD-ERROR                                 00698014
                          CAA-VAR1-ERROR.                               00699014
      *                                                                 00700014
           SET ADDRESS OF VLNC010I          TO CAA-PTR-COPYIN           00701014
           SET ADDRESS OF VLECCMA           TO CAA-PTRDATA              00702014
           MOVE 'VL01'                      TO CAA-CODTRAN-SIG          00703014
      *                                                                 00704014
           MOVE CAA-TECLA                   TO   SW-TECLA               00705014
      *                                                                 00706014
           PERFORM 11-CAMBIO-A-BLANCOS                                  00707014
              THRU 11-CAMBIO-A-BLANCOS-FIN.                             00708014
      *                                                                 00709014
           PERFORM 12-LIMPIAR-PANTALLA                                  00710014
              THRU 12-LIMPIAR-PANTALLA-FIN.                             00711014
      *                                                                 00712014
       1-INICIO-FIN.  EXIT.                                             00713014
      *                                                                 00714014
       11-CAMBIO-A-BLANCOS.                                             00715014
      *                                                                 00716014
           IF END0101I < SPACES                                         00717014
              INSPECT END0101I REPLACING ALL LOW-VALUES BY SPACES       00718014
           END-IF                                                       00719014
      *                                                                 00720014
           IF CEN0101I < SPACES                                         00721014
              INSPECT CEN0101I REPLACING ALL LOW-VALUES BY SPACES       00722014
           END-IF                                                       00723014
      *                                                                 00724014
           IF DGT0101I < SPACES                                         00725014
              INSPECT DGT0101I REPLACING ALL LOW-VALUES BY SPACES       00726014
           END-IF                                                       00727014
      *                                                                 00728014
           IF PRD0101I < SPACES                                         00729014
              INSPECT PRD0101I REPLACING ALL LOW-VALUES BY SPACES       00730014
           END-IF                                                       00731014
      *                                                                 00732014
           IF CTA0101I < SPACES                                         00733014
              INSPECT CTA0101I REPLACING ALL LOW-VALUES BY SPACES       00734014
           END-IF                                                       00735014
      *                                                                 00736014
           IF DG20101I < SPACES                                         00737014
              INSPECT DG20101I REPLACING ALL LOW-VALUES BY SPACES       00738014
           END-IF                                                       00739014
      *                                                                 00740014
           IF ENT0101I < SPACES                                         00741014
              INSPECT ENT0101I REPLACING ALL LOW-VALUES BY SPACES       00742014
           END-IF                                                       00743014
      *                                                                 00744014
           IF NEN0101I < SPACES                                         00745014
              INSPECT NEN0101I REPLACING ALL LOW-VALUES BY SPACES       00746014
           END-IF                                                       00747014
      *                                                                 00748014
           IF TIT0101I < SPACES                                         00749014
              INSPECT TIT0101I REPLACING ALL LOW-VALUES BY SPACES       00750014
           END-IF.                                                      00751014
      *                                                                 00752014
           IF NOM0101I < SPACES                                         00753014
              INSPECT NOM0101I REPLACING ALL LOW-VALUES BY SPACES       00754014
           END-IF.                                                      00755014
      *                                                                 00756014
           IF NCC0101I < SPACES                                         00757014
              INSPECT NCC0101I REPLACING ALL LOW-VALUES BY SPACES       00758014
           END-IF.                                                      00759014
      *                                                                 00760014
           IF NC20101I < SPACES                                         00761014
              INSPECT NC20101I REPLACING ALL LOW-VALUES BY SPACES       00762014
           END-IF.                                                      00763014
      *                                                                 00764014
           IF MON0101I < SPACES                                         00765014
              INSPECT MON0101I REPLACING ALL LOW-VALUES BY SPACES       00766014
           END-IF.                                                      00767014
      *                                                                 00768014
           IF MO20101I < SPACES                                         00769014
              INSPECT MO20101I REPLACING ALL LOW-VALUES BY SPACES       00770014
           END-IF.                                                      00771014
      *                                                                 00772014
           IF SUC0101I < SPACES                                         00773014
              INSPECT SUC0101I REPLACING ALL LOW-VALUES BY SPACES       00774014
           END-IF.                                                      00775014
      *                                                                 00776014
           IF NOF0101I < SPACES                                         00777014
              INSPECT NOF0101I REPLACING ALL LOW-VALUES BY SPACES       00778014
           END-IF.                                                      00779014
      *                                                                 00780014
           IF IDI0101I < SPACES                                         00781014
              INSPECT IDI0101I REPLACING ALL LOW-VALUES BY SPACES       00782014
           END-IF.                                                      00783014
      *                                                                 00784014
           IF MDA0101I < SPACES                                         00785014
              INSPECT MDA0101I REPLACING ALL LOW-VALUES BY SPACES       00786014
           END-IF.                                                      00787014
      *200503172-INI                                                    00788014
           IF CIN0101I < SPACES                                         00789014
              INSPECT CIN0101I REPLACING ALL LOW-VALUES BY SPACES       00790014
           END-IF.                                                      00791014
           IF NCU0101I < SPACES                                         00792014
              INSPECT NCU0101I REPLACING ALL LOW-VALUES BY SPACES       00793014
           END-IF.                                                      00794014
      *200503172-INI                                                    00795014
      *                                                                 00796014
           IF DCO0101I < SPACES                                         00797014
              INSPECT DCO0101I REPLACING ALL LOW-VALUES BY SPACES       00798014
           END-IF.                                                      00799014
      *                                                                 00800014
           IF TCL0101I < SPACES                                         00801014
              INSPECT TCL0101I REPLACING ALL LOW-VALUES BY SPACES       00802014
           END-IF.                                                      00803014
      *                                                                 00804014
           IF CSU0101I < SPACES                                         00805014
              INSPECT CSU0101I REPLACING ALL LOW-VALUES BY SPACES       00806014
           END-IF.                                                      00807014
      *                                                                 00808014
           IF ODI0101I < SPACES                                         00809014
              INSPECT ODI0101I REPLACING ALL LOW-VALUES BY SPACES       00810014
           END-IF.                                                      00811014
      *                                                                 00812014
           IF PAI0101I < SPACES                                         00813014
              INSPECT PAI0101I REPLACING ALL LOW-VALUES BY SPACES       00814014
           END-IF.                                                      00815014
      *                                                                 00816014
           IF TAF0101I < SPACES                                         00817014
              INSPECT TAF0101I REPLACING ALL LOW-VALUES BY SPACES       00818014
           END-IF.                                                      00819014
      *                                                                 00820014
           IF SOT0101I < SPACES                                         00821014
              INSPECT SOT0101I REPLACING ALL LOW-VALUES BY SPACES       00822014
           END-IF.                                                      00823014
      *                                                                 00824014
           IF TEL0101I < SPACES                                         00825014
              INSPECT TEL0101I REPLACING ALL LOW-VALUES BY SPACES       00826014
           END-IF.                                                      00827014
      *                                                                 00828014
JPC@2      IF GVT0101I < SPACES                                         00829014
JPC@2         INSPECT GVT0101I REPLACING ALL LOW-VALUES BY SPACES       00830014
JPC@2      END-IF.                                                      00831014
      *                                                                 00832014
JPC@2      IF CAV0101I < SPACES                                         00833014
JPC@2         INSPECT CAV0101I REPLACING ALL LOW-VALUES BY SPACES       00834014
JPC@2      END-IF.                                                      00835014
      *                                                                 00836014
JPC@2      IF SCV0101I < SPACES                                         00837014
JPC@2         INSPECT SCV0101I REPLACING ALL LOW-VALUES BY SPACES       00838014
JPC@2      END-IF.                                                      00839014
      *                                                                 00840014
JPC@2      IF CAM0101I < SPACES                                         00841014
JPC@2         INSPECT CAM0101I REPLACING ALL LOW-VALUES BY SPACES       00842014
JPC@2      END-IF.                                                      00843014
      *                                                                 00844014
           IF CVE0101I NOT NUMERIC                                      00845014
              MOVE ZEROS            TO CVE0101I                         00846014
           END-IF.                                                      00847014
      *                                                                 00848014
           IF MCV0101I NOT NUMERIC                                      00849014
              MOVE ZEROS            TO MCV0101I                         00850014
           END-IF.                                                      00851014
      *                                                                 00852014
           IF PAJ0101I NOT NUMERIC                                      00853014
              MOVE ZEROS            TO PAJ0101I                         00854014
           END-IF.                                                      00855014
      *                                                                 00856014
           IF MPJ0101I NOT NUMERIC                                      00857014
              MOVE ZEROS            TO MPJ0101I                         00858014
           END-IF.                                                      00859014
      *                                                                 00860014
           IF DCU0101I NOT NUMERIC                                      00861014
              MOVE ZEROS          TO DCU0101I                           00862014
           END-IF.                                                      00863014
      *                                                                 00864014
           IF MDC0101I NOT NUMERIC                                      00865014
              MOVE ZEROS          TO MDC0101I                           00866014
           END-IF.                                                      00867014
      *                                                                 00868014
           IF DIV0101I NOT NUMERIC                                      00869014
              MOVE ZEROS          TO DIV0101I                           00870014
           END-IF.                                                      00871014
      *                                                                 00872014
           IF MDI0101I NOT NUMERIC                                      00873014
              MOVE ZEROS          TO MDI0101I                           00874014
           END-IF.                                                      00875014
      *                                                                 00876014
           IF SUS0101I NOT NUMERIC                                      00877014
              MOVE ZEROS           TO SUS0101I                          00878014
           END-IF.                                                      00879014
      *                                                                 00880014
           IF MSU0101I NOT NUMERIC                                      00881014
              MOVE ZEROS           TO MSU0101I                          00882014
           END-IF.                                                      00883014
      *                                                                 00884014
           IF AMO0101I NOT NUMERIC                                      00885014
              MOVE ZEROS           TO AMO0101I                          00886014
           END-IF.                                                      00887014
      *                                                                 00888014
           IF MAM0101I NOT NUMERIC                                      00889014
              MOVE ZEROS           TO MAM0101I                          00890014
           END-IF.                                                      00891014
      *                                                                 00892014
           IF MAN0101I NOT NUMERIC                                      00893014
              MOVE ZEROS           TO MAN0101I                          00894014
           END-IF.                                                      00895014
      *LIMA-24-04-1999.SE QUITA EL CAMPO CORREO                         00896014
      *     IF CCO0101I < SPACES                                        00897014
      *        INSPECT CCO0101I REPLACING ALL LOW-VALUES BY SPACES      00898014
      *     END-IF.                                                     00899014
      *LIMA-24-04-1999.SE QUITA EL CAMPO CORREO                         00900014
      *                                                                 00901014
           IF FUA0101I < SPACES                                         00902014
              INSPECT FUA0101I REPLACING ALL LOW-VALUES BY SPACES       00903014
           END-IF.                                                      00904014
      *200503172-INI                                                    00905014
           IF ALT0101I < SPACES                                         00906014
              INSPECT ALT0101I REPLACING ALL LOW-VALUES BY SPACES       00907014
           END-IF.                                                      00908014
           IF HUM0101I < SPACES                                         00909014
              INSPECT HUM0101I REPLACING ALL LOW-VALUES BY SPACES       00910014
           END-IF.                                                      00911014
           IF USU0101I < SPACES                                         00912014
              INSPECT USU0101I REPLACING ALL LOW-VALUES BY SPACES       00913014
           END-IF.                                                      00914014
      *200503172-FIN                                                    00915014
      *200711038-INI                                                    00916014
           IF REG0101I < SPACES                                         00917014
              INSPECT REG0101I REPLACING ALL LOW-VALUES BY SPACES       00918014
           END-IF.                                                      00919014
      *200711038-FIN                                                    00920014
      *                                                                 00921014
       11-CAMBIO-A-BLANCOS-FIN.                                         00922014
           EXIT.                                                        00923014
      *                                                                 00924014
       12-LIMPIAR-PANTALLA.                                             00925014
      *                                                                 00926014
           IF CAA-88-ESTADO-INICIO                                      00927014
              MOVE LOW-VALUES    TO    VLNC010O                         00928014
           ELSE                                                         00929014
              IF CAA-88-ESTADO-CONTIN AND PF4                           00930014
                 INITIALIZE   W-AREA-VIAJA  VCMA-AREA-VIAJA             00931014
                 INITIALIZE   END0101O  CSU0101O   DCU0101O             00932014
                              CEN0101O  ODI0101O   MDC0101O             00933014
                              DGT0101O  PAI0101O   DIV0101O             00934014
                              PRD0101O  TAF0101O   MDI0101O             00935014
                              CTA0101O  SOT0101O   SUS0101O             00936014
                              DG20101O  TEL0101O   MSU0101O             00937014
                              ENT0101O  CVE0101O   AMO0101O             00938014
                              NEN0101O  MCV0101O   MAM0101O             00939014
                              TIT0101O  PAJ0101O   FUA0101O             00940014
                              NOM0101O  MPJ0101O   MO20101O             00941014
                              NCC0101O  MON0101O   DCO0101O             00942014
                              NC20101O  MAN0101O   TCL0101O             00943014
                              SUC0101O  NOF0101O   IDI0101O             00944014
                              MDA0101O  CIN0101O   NCU0101O             00945014
                              ALT0101O  HUM0101O   USU0101O             00946014
JPC@2                         GVT0101O  CAV0101O   SCV0101O CAM0101O    00947014
                              REG0101O                                  00948014
              END-IF                                                    00949014
           END-IF.                                                      00950014
      *                                                                 00951014
       12-LIMPIAR-PANTALLA-FIN.                                         00952014
           EXIT.                                                        00953014
      *                                                                 00954014
       2-PROCESO.                                                       00955014
      *                                                                 00956014
           EVALUATE TRUE                                                00957014
               WHEN CAA-88-ESTADO-INICIO                                00958014
                       PERFORM 21-INICIACION                            00959014
                          THRU 21-INICIACION-FIN                        00960014
               WHEN CAA-88-ESTADO-CONTIN                                00961014
                       PERFORM 22-CONTINUACION                          00962014
                          THRU 22-CONTINUACION-FIN                      00963014
           END-EVALUATE.                                                00964014
      *                                                                 00965014
       2-PROCESO-FIN. EXIT.                                             00966014
      *                                                                 00967014
       21-INICIACION.                                                   00968014
      *                                                                 00969014
JPC@4      IF CAA-CENTRO-CONT NOT = '0567'                              00970014
JPC@4         MOVE '0'      TO  SUC0101A                                00971014
JPC@4      END-IF                                                       00972014
      *200703156-INI                                                    00973014
           MOVE VCMA-AREA-VIAJA TO W-AREA-VIAJA                         00974014
           IF CODTRAN-COMM = 'VL01'                                     00975014
              MOVE CTA-COMM7    TO CTA0101O                             00976014
           ELSE                                                         00977014
              INITIALIZE           CTA0101O                             00978014
           END-IF                                                       00979014
      *200703156-FIN                                                    00980014
                                                                        00981014
           INITIALIZE   W-AREA-VIAJA  VCMA-AREA-VIAJA                   00982014
           INITIALIZE   END0101O  CSU0101O   DCU0101O                   00983014
                        CEN0101O  ODI0101O   MDC0101O                   00984014
                        DGT0101O  PAI0101O   DIV0101O                   00985014
                        PRD0101O  TAF0101O   MDI0101O                   00986014
                                  SOT0101O   SUS0101O                   00987014
      *200703156-INI                                                    00988014
      *                 CTA0101O                                        00989014
      *200703156-FIN                                                    00990014
                        DG20101O  TEL0101O   MSU0101O                   00991014
                        ENT0101O  CVE0101O   AMO0101O                   00992014
                        NEN0101O  MCV0101O   MAM0101O                   00993014
                        TIT0101O  PAJ0101O   FUA0101O                   00994014
                        NOM0101O  MPJ0101O   MON0101O                   00995014
                        NCC0101O  IDI0101O   MO20101O                   00996014
                        NC20101O  DCO0101O   MAN0101O                   00997014
                        NOF0101O  MDA0101O   SUC0101O                   00998014
                        TCL0101O  CIN0101O   NCU0101O REG0101O          00999014
JPC@2                   GVT0101O  CAV0101O   SCV0101O CAM0101O          01000014
                        ALT0101O  HUM0101O   USU0101O.                  01001014
      *                                                                 01002014
       21-INICIACION-FIN. EXIT.                                         01003014
      *                                                                 01004014
       22-CONTINUACION.                                                 01005014
      *                                                                 01006014
           MOVE VCMA-AREA-VIAJA  TO  W-AREA-VIAJA                       01007014
           IF PF3                                                       01008014
              IF MSB-COMM NOT = 'I'                                     01009014
                 INITIALIZE W-AREA-VIAJA VCMA-AREA-VIAJA                01010014
              END-IF                                                    01011014
           END-IF                                                       01012014
      *                                                                 01013014
           IF PF2 OR PF3 OR PF6 OR PF8 OR PFENTER                       01014014
              PERFORM 999999-VERIFICA-FACULTADES                        01015014
           END-IF                                                       01016014
      *200703156-INI                                                    01017014
           MOVE SPACES          TO CODTRAN-COMM.                        01018014
      *200703156-FIN                                                    01019014
      *                                                                 01020014
           EVALUATE TRUE                                                01021014
              WHEN  PFENTER                                             01022014
                    MOVE 'C'     TO OPT-COMM                            01023014
                    MOVE SPACES  TO MSB-COMM                            01024014
                    PERFORM EJECUTAR                                    01025014
                       THRU EJECUTAR-FIN                                01026014
              WHEN  PF1                                                 01027014
                    CONTINUE                                            01028014
              WHEN  PF2                                                 01029014
                    IF MSB-COMM = SPACES                                01030014
                       MOVE 'M'   TO MSB-COMM                           01031014
                    END-IF                                              01032014
                    PERFORM EJECUTAR                                    01033014
                       THRU EJECUTAR-FIN                                01034014
              WHEN  PF3                                                 01035014
                    MOVE 'A'     TO OPT-COMM                            01036014
                    MOVE SPACES  TO MSB-COMM                            01037014
                    PERFORM EJECUTAR                                    01038014
                       THRU EJECUTAR-FIN                                01039014
              WHEN  PF4                                                 01040014
                    PERFORM 12-LIMPIAR-PANTALLA                         01041014
                       THRU 12-LIMPIAR-PANTALLA-FIN                     01042014
              WHEN  PF6                                                 01043014
                    MOVE 'B'   TO MSB-COMM                              01044014
                    PERFORM EJECUTAR                                    01045014
                       THRU EJECUTAR-FIN                                01046014
      *A2012-I                                                          01047014
              WHEN  PF7                                                 01048014
                    IF OPT-COMM = ('A' OR 'C') AND MSB-COMM = SPACES    01049014
                       PERFORM TRATAR-IMPRESO                           01050014
                          THRU TRATAR-IMPRESO-FIN                       01051014
                    ELSE                                                01052014
                       MOVE 'VLE1676' TO CAA-COD-ERROR                  01053014
                       MOVE -1        TO CTA0101L                       01054014
                       PERFORM 3-FINAL                                  01055014
                    END-IF                                              01056014
      *A2012-F                                                          01057014
              WHEN  PF8                                                 01058014
      *200306088-INI                                                    01059014
                    MOVE 'VLE1415' TO CAA-COD-ERROR                     01060014
                    MOVE -1        TO CTA0101L                          01061014
                    PERFORM 3-FINAL                                     01062014
      *200306088-INI                                                    01063014
      *200703156-INI                                                    01064014
               WHEN PF10                                                01065014
                    MOVE 'VL01' TO CODTRAN-COMM                         01066014
      *200703156-FIN                                                    01067014
           END-EVALUATE.                                                01068014
      *                                                                 01069014
       22-CONTINUACION-FIN. EXIT.                                       01070014
      *                                                                 01071014
       3-FINAL.                                                         01072014
      *                                                                 01073014
           PERFORM  31-POSICIONAR-CURSOR                                01074014
              THRU  31-POSICIONAR-CURSOR-FIN                            01075014
      *                                                                 01076014
           IF  CAA-88-COD-ERROR-VACIO                                   01077014
               MOVE W-AREA-VIAJA    TO VCMA-AREA-VIAJA                  01078014
           ELSE                                                         01079014
               EXEC CICS SYNCPOINT ROLLBACK   END-EXEC                  01080014
           END-IF                                                       01081014
      *                                                                 01082014
           EVALUATE  TRUE                                               01083014
               WHEN CAA-88-ESTADO-INICIO                                01084014
                    PERFORM 32-INFORMAR-INICIO                          01085014
                       THRU 32-INFORMAR-INICIO-FIN                      01086014
               WHEN CAA-88-ESTADO-CONTIN                                01087014
                    PERFORM 33-INFORMAR-CONTIN                          01088014
                       THRU 33-INFORMAR-CONTIN-FIN                      01089014
           END-EVALUATE                                                 01090014
      *                                                                 01091014
           PERFORM 34-INFORMAR-ANALITICA                                01092014
              THRU 34-INFORMAR-ANALITICA-FIN                            01093014
      *                                                                 01094014
           EXEC CICS                                                    01095014
                RETURN                                                  01096014
           END-EXEC.                                                    01097014
      *                                                                 01098014
       31-POSICIONAR-CURSOR.                                            01099014
      *                                                                 01100014
           IF  END0101L = -1 OR                                         01101014
               CEN0101L = -1 OR                                         01102014
               DGT0101L = -1 OR                                         01103014
               PRD0101L = -1 OR                                         01104014
               CTA0101L = -1 OR                                         01105014
               DG20101L = -1 OR                                         01106014
               ENT0101L = -1 OR                                         01107014
               NEN0101L = -1 OR                                         01108014
               TIT0101L = -1 OR                                         01109014
               NOM0101L = -1 OR                                         01110014
               NCC0101L = -1 OR                                         01111014
               NC20101L = -1 OR                                         01112014
               SUC0101L = -1 OR                                         01113014
               NOF0101L = -1 OR                                         01114014
               IDI0101L = -1 OR                                         01115014
               MDA0101L = -1 OR                                         01116014
               DCO0101L = -1 OR                                         01117014
               TCL0101L = -1 OR                                         01118014
               CSU0101L = -1 OR                                         01119014
               ODI0101L = -1 OR                                         01120014
               PAI0101L = -1 OR                                         01121014
               TAF0101L = -1 OR                                         01122014
               SOT0101L = -1 OR                                         01123014
               TEL0101L = -1 OR                                         01124014
JPC@2          GVT0101L = -1 OR                                         01125014
JPC@2          CAV0101L = -1 OR                                         01126014
JPC@2          SCV0101L = -1 OR                                         01127014
JPC@2          CAM0101L = -1 OR                                         01128014
               CVE0101L = -1 OR                                         01129014
               MCV0101L = -1 OR                                         01130014
               PAJ0101L = -1 OR                                         01131014
               MPJ0101L = -1 OR                                         01132014
               DCU0101L = -1 OR                                         01133014
               MDC0101L = -1 OR                                         01134014
               DIV0101L = -1 OR                                         01135014
               MDI0101L = -1 OR                                         01136014
               SUS0101L = -1 OR                                         01137014
               MSU0101L = -1 OR                                         01138014
               AMO0101L = -1 OR                                         01139014
               MAM0101L = -1 OR                                         01140014
               FUA0101L = -1 OR                                         01141014
               MAN0101L = -1 OR                                         01142014
               CIN0101L = -1 OR                                         01143014
               REG0101L = -1 OR                                         01144014
               NCU0101L = -1                                            01145014
               CONTINUE                                                 01146014
           ELSE                                                         01147014
               MOVE    -1    TO   CTA0101L                              01148014
           END-IF.                                                      01149014
      *                                                                 01150014
       31-POSICIONAR-CURSOR-FIN.                                        01151014
           EXIT.                                                        01152014
      *                                                                 01153014
       32-INFORMAR-INICIO.                                              01154014
      *                                                                 01155014
           IF CAA-88-COD-ERROR-VACIO                                    01156014
              SET CAA-88-ESTADO-CONTIN     TO  TRUE                     01157014
              SET CAA-88-ACCION-TERMINAL   TO  TRUE                     01158014
              MOVE CAA-CODTRAN             TO  CAA-CODTRAN-SIG          01159014
           ELSE                                                         01160014
              SET CAA-88-ESTADO-INICIO     TO  TRUE                     01161014
              SET CAA-88-ACCION-PROGRAMA   TO  TRUE                     01162014
              SET CAA-88-CODTRAN-SIG-ULTI  TO  TRUE                     01163014
           END-IF.                                                      01164014
      *                                                                 01165014
       32-INFORMAR-INICIO-FIN. EXIT.                                    01166014
      *                                                                 01167014
       33-INFORMAR-CONTIN.                                              01168014
      *                                                                 01169014
           IF PERSONAS                                                  01170014
              MOVE 'PE27'                  TO CAA-CODTRAN-SIG           01171014
              SET CAA-88-CADENA-INICIO     TO  TRUE                     01172014
              SET CAA-88-ESTADO-INICIO     TO  TRUE                     01173014
              SET CAA-88-ACCION-PROGRAMA   TO  TRUE                     01174014
           ELSE                                                         01175014
      *200703156-INI                                                    01176014
      *       SET CAA-88-ACCION-TERMINAL   TO  TRUE                     01177014
      *       SET CAA-88-CODTRAN-SIG-SAME  TO  TRUE                     01178014
      *       SET CAA-88-ESTADO-CONTIN     TO  TRUE                     01179014
              IF CAA-88-COD-ERROR-VACIO AND CODTRAN-COMM NOT = SPACES   01180014
                 MOVE 'VL3L'                 TO CAA-CODTRAN-SIG         01181014
                 SET CAA-88-CADENA-ANADIR    TO TRUE                    01182014
                 SET CAA-88-ESTADO-INICIO    TO TRUE                    01183014
                 SET CAA-88-ACCION-PROGRAMA  TO TRUE                    01184014
              ELSE                                                      01185014
                 SET CAA-88-ACCION-TERMINAL  TO TRUE                    01186014
                 SET CAA-88-ESTADO-CONTIN    TO TRUE                    01187014
                 SET CAA-88-CODTRAN-SIG-SAME TO TRUE                    01188014
              END-IF                                                    01189014
           END-IF.                                                      01190014
      *200703156-FIN                                                    01191014
      *                                                                 01192014
       33-INFORMAR-CONTIN-FIN. EXIT.                                    01193014
      *                                                                 01194014
       34-INFORMAR-ANALITICA.                                           01195014
      *                                                                 01196014
           MOVE CAA-ENTIDAD        TO   CAA-ENTIDAD-ANA                 01197014
           MOVE CAA-CENTRO-CONT    TO   CAA-CENTRO-ANA.                 01198014
      *                                                                 01199014
       34-INFORMAR-ANALITICA-FIN.                                       01200014
           EXIT.                                                        01201014
      *                                                                 01202014
       EJECUTAR.                                                        01203014
      ******************************************************************01204014
      *                                                                 01205014
      *****      CONTROL  DE  LAS  OPCIONES   DEL    MENU          *****01206014
      *                                                                 01207014
           IF OPT-COMM NOT = 'A' AND 'C'                                01208014
      * SECUENCIA DE TRATAMIENTO ILOGICA                                01209014
              MOVE 'VLE1002' TO CAA-COD-ERROR                           01210014
              MOVE -1        TO CTA0101L                                01211014
              PERFORM 3-FINAL                                           01212014
           ELSE                                                         01213014
              IF (MSB-COMM     = 'I' AND PF2)                           01214014
      * LA CUENTA DE VALORES ESTA INACTIVA                              01215014
                 MOVE 'VLE0141' TO CAA-COD-ERROR                        01216014
                 MOVE -1        TO CTA0101L                             01217014
                 PERFORM 3-FINAL                                        01218014
              END-IF                                                    01219014
              IF (MSB-COMM = 'M' OR 'B')  AND                           01220014
                 (CTA0101I NOT = CTA0101-COMM)                          01221014
      * HA CAMBIADO LA CLAVE, VUELVA A CONSULTAR                        01222014
                 MOVE 'VLE1003' TO CAA-COD-ERROR                        01223014
                 MOVE -1        TO CTA0101L                             01224014
                 PERFORM 3-FINAL                                        01225014
              END-IF                                                    01226014
      *MADRID-04-04-1999.INI.                                           01227014
              IF (MSB-COMM = 'M' OR 'B')  AND                           01228014
                 (TIT0101I NOT = TIT0101-COMM)                          01229014
      * HA CAMBIADO LA CLAVE, VUELVA A CONSULTAR                        01230014
                 MOVE 'VLE1724' TO CAA-COD-ERROR                        01231014
                 MOVE -1        TO TIT0101L                             01232014
                 PERFORM 3-FINAL                                        01233014
              END-IF                                                    01234014
      *MADRID-04-04-1999.FIN.                                           01235014
           END-IF                                                       01236014
      *                                                                 01237014
      ** COMPRUEBO DATOS TECLEADOS                                      01238014
      *                                                                 01239014
           EVALUATE OPT-COMM                                            01240014
             WHEN 'A'                                                   01241014
                IF  MSB-COMM = 'M'                                      01242014
                    PERFORM 23-MODIFICACION                             01243014
                       THRU 23-MODIFICACION-FIN                         01244014
                ELSE                                                    01245014
                    IF MSB-COMM = 'B'                                   01246014
                       PERFORM 25-INACTIVAR                             01247014
                          THRU 25-INACTIVAR-FIN                         01248014
                    ELSE                                                01249014
                       PERFORM 22-ALTA                                  01250014
                          THRU 22-ALTA-FIN                              01251014
                    END-IF                                              01252014
                END-IF                                                  01253014
             WHEN 'C'                                                   01254014
                IF  MSB-COMM = 'M'                                      01255014
                    PERFORM 23-MODIFICACION                             01256014
                       THRU 23-MODIFICACION-FIN                         01257014
                ELSE                                                    01258014
                    IF MSB-COMM = 'B'                                   01259014
                       PERFORM 25-INACTIVAR                             01260014
                          THRU 25-INACTIVAR-FIN                         01261014
                    ELSE                                                01262014
                        PERFORM 21-CONSULTA                             01263014
                           THRU 21-CONSULTA-FIN                         01264014
                    END-IF                                              01265014
                END-IF                                                  01266014
           END-EVALUATE.                                                01267014
      *                                                                 01268014
       EJECUTAR-FIN. EXIT.                                              01269014
      *                                                                 01270014
       21-CONSULTA.                                                     01271014
      *                                                                 01272014
JIPC       MOVE SPACES   TO WA-COD-ERROR.                               01273014
      *                                                                 01274014
           IF CTA0101I  NOT NUMERIC                                     01275014
           OR CTA0101I  = ZEROS                                         01276014
           OR CTA0101I  = SPACES                                        01277014
              MOVE 'VLE0139'                TO CAA-COD-ERROR            01278014
              MOVE -1                       TO CTA0101L                 01279014
              PERFORM 3-FINAL                                           01280014
           END-IF                                                       01281014
      *                                                                 01282014
           MOVE CTA0101I                    TO W-CUENTA                 01283014
           MOVE W-CUENTA                    TO VARC-CUENTA              01284014
      *                                                                 01285014
           EXEC SQL                                                     01286014
JPC@1 *         SELECT  *                                               01287014
                SELECT  VARC_CUENTA                                     01288014
                     ,  VARC_CENTAD                                     01289014
                     ,  VARC_NUMCLI                                     01290014
                     ,  VARC_CLMAST                                     01291014
                     ,  VARC_MONEDA                                     01292014
                     ,  VARC_SUCURS                                     01293014
                     ,  VARC_CTACAR                                     01294014
                     ,  VARC_CTAABO                                     01295014
                     ,  VARC_TEXTO                                      01296014
                     ,  VARC_PRESEN                                     01297014
                     ,  VARC_GRUPO                                      01298014
                     ,  VARC_RUT                                        01299014
                     ,  VARC_CNAE                                       01300014
                     ,  VARC_SITUAC                                     01301014
                     ,  VARC_EXEN1                                      01302014
                     ,  VARC_EXEN2                                      01303014
                     ,  VARC_EXEN3                                      01304014
                     ,  VARC_EXEN4                                      01305014
                     ,  VARC_EXEN5                                      01306014
                     ,  VARC_EXEN6                                      01307014
                     ,  VARC_EXEN7                                      01308014
                     ,  VARC_EXEN8                                      01309014
                     ,  VARC_EXEN9                                      01310014
                     ,  VARC_EXEN10                                     01311014
                     ,  VARC_ANALIS                                     01312014
                     ,  VARC_CLACARGO                                   01313014
                     ,  VARC_CLABONO                                    01314014
                     ,  VARC_NUMDOM                                     01315014
                     ,  VARC_CODSUS                                     01316014
                     ,  VARC_FE_ULT_EXT                                 01317014
                     ,  VARC_PAIS                                       01318014
                     ,  VARC_FE_CARTERA                                 01319014
                     ,  VARC_CLTELEX                                    01320014
                     ,  VARC_FE_ALTA                                    01321014
                     ,  VARC_VALORACION                                 01322014
                     ,  VARC_VALEXTRJ                                   01323014
                     ,  VARC_INVERSOR                                   01324014
                     ,  VARC_DIRECTA                                    01325014
                     ,  VARC_MAX_CVE_1                                  01326014
                     ,  VARC_MAX_DCU_5                                  01327014
                     ,  VARC_MAX_SUS_6                                  01328014
                     ,  VARC_MAX_DIV_7                                  01329014
                     ,  VARC_MAX_AMO_8                                  01330014
                     ,  VARC_MAX_PAJ_9                                  01331014
                     ,  VARC_FECHA_102                                  01332014
                     ,  VARC_TARIFACUS                                  01333014
                     ,  VARC_SWIFT_TELEX                                01334014
                     ,  VARC_TELEX_2                                    01335014
                     ,  VARC_GRUPO_CTAS                                 01336014
                     ,  VARC_OPER_TIT                                   01337014
                     ,  VARC_FEALTREG                                   01338014
                     ,  VARC_FEULMOD                                    01339014
                     ,  VARC_HORULMOD                                   01340014
                     ,  VARC_NUMTER                                     01341014
                     ,  VARC_USUARIO                                    01342014
                     ,  VARC_FILLER                                     01343014
                     ,  VARC_CTAVAL20                                   01344014
      *@ZAL-INI                                                         01345014
      *              ,  VARC_NUMMAN                                     01346014
                     ,  VARC_GRUPO_CTAS                                 01347014
      *@ZAL-FIN                                                         01348014
                     ,  VARC_INDIMP                                     01349014
                     ,  VARC_INDSAB                                     01350014
JPC@1 *           INTO :DCLVLDTARC                                      01351014
                  INTO :VARC-CUENTA                                     01352014
                     , :VARC-CENTAD                                     01353014
                     , :VARC-NUMCLI                                     01354014
                     , :VARC-CLMAST                                     01355014
                     , :VARC-MONEDA                                     01356014
                     , :VARC-SUCURS                                     01357014
                     , :VARC-CTACAR                                     01358014
                     , :VARC-CTAABO                                     01359014
                     , :VARC-TEXTO                                      01360014
                     , :VARC-PRESEN                                     01361014
                     , :VARC-GRUPO                                      01362014
                     , :VARC-RUT                                        01363014
                     , :VARC-CNAE                                       01364014
                     , :VARC-SITUAC                                     01365014
                     , :VARC-EXEN1                                      01366014
                     , :VARC-EXEN2                                      01367014
                     , :VARC-EXEN3                                      01368014
                     , :VARC-EXEN4                                      01369014
                     , :VARC-EXEN5                                      01370014
                     , :VARC-EXEN6                                      01371014
                     , :VARC-EXEN7                                      01372014
                     , :VARC-EXEN8                                      01373014
                     , :VARC-EXEN9                                      01374014
                     , :VARC-EXEN10                                     01375014
                     , :VARC-ANALIS                                     01376014
                     , :VARC-CLACARGO                                   01377014
                     , :VARC-CLABONO                                    01378014
                     , :VARC-NUMDOM                                     01379014
                     , :VARC-CODSUS                                     01380014
                     , :VARC-FE-ULT-EXT                                 01381014
                     , :VARC-PAIS                                       01382014
                     , :VARC-FE-CARTERA                                 01383014
                     , :VARC-CLTELEX                                    01384014
                     , :VARC-FE-ALTA                                    01385014
                     , :VARC-VALORACION                                 01386014
                     , :VARC-VALEXTRJ                                   01387014
                     , :VARC-INVERSOR                                   01388014
                     , :VARC-DIRECTA                                    01389014
                     , :VARC-MAX-CVE-1                                  01390014
                     , :VARC-MAX-DCU-5                                  01391014
                     , :VARC-MAX-SUS-6                                  01392014
                     , :VARC-MAX-DIV-7                                  01393014
                     , :VARC-MAX-AMO-8                                  01394014
                     , :VARC-MAX-PAJ-9                                  01395014
                     , :VARC-FECHA-102                                  01396014
                     , :VARC-TARIFACUS                                  01397014
                     , :VARC-SWIFT-TELEX                                01398014
                     , :VARC-TELEX-2                                    01399014
                     , :VARC-GRUPO-CTAS                                 01400014
                     , :VARC-OPER-TIT                                   01401014
                     , :VARC-FEALTREG                                   01402014
                     , :VARC-FEULMOD                                    01403014
                     , :VARC-HORULMOD                                   01404014
                     , :VARC-NUMTER                                     01405014
                     , :VARC-USUARIO                                    01406014
                     , :VARC-FILLER                                     01407014
                     , :VARC-CTAVAL20                                   01408014
      *@ZAL-INI                                                         01409014
      *              , :VARC-NUMMAN                                     01410014
                     , :VARC-GRUPO-CTAS                                 01411014
      *@ZAL-FIN                                                         01412014
                     , :VARC-INDIMP                                     01413014
                     , :VARC-INDSAB                                     01414014
                  FROM  VLDTARC                                         01415014
                 WHERE  VARC_CUENTA  = :VARC-CUENTA                     01416014
           END-EXEC                                                     01417014
      *                                                                 01418014
           MOVE SQLCODE TO SQLCODE-AUX                                  01419014
      *                                                                 01420014
           EVALUATE TRUE                                                01421014
              WHEN DB2-OK                                               01422014
                   IF VARC-SITUAC = 'B'                                 01423014
      *200306088-INI                                                    01424014
      *               MOVE 'VLA0055'   TO  CAA-COD-AVISO2               01425014
                      MOVE 'VLA0086'   TO  CAA-COD-AVISO2               01426014
      *200306088-FIN                                                    01427014
                   END-IF                                               01428014
                   IF VARC-SITUAC = 'U'                                 01429014
                      MOVE 'VLA0076'   TO  CAA-COD-AVISO2               01430014
                   END-IF                                               01431014
                   IF VARC-SITUAC = 'X'                                 01432014
                      MOVE 'VLA0086'   TO  CAA-COD-AVISO2               01433014
                   END-IF                                               01434014
      *A2011-RUTLOG-I                                                   01435014
                   INITIALIZE W-VLWCLOG0                                01436014
                              LOGVLDTARC                                01437014
                   MOVE 'VLDTARC'             TO  VL7LOG-TABLA          01438014
                   MOVE 'SELECT'              TO  VL7LOG-OPERACION      01439014
                   MOVE LENGTH OF DCLVLDTARC  TO  VL7LOG-REGISTRO-LEN   01440014
                   MOVE DCLVLDTARC            TO  LOGVLDTARC            01441014
                   MOVE LOGVLDTARC            TO  VL7LOG-REGISTRO-TEXT  01442014
                   PERFORM LLAMAR-VL7CRLOG                              01443014
                      THRU LLAMAR-VL7CRLOG-FIN                          01444014
      *A2011-RUTLOG-F                                                   01445014
      *                                                                 01446014
              WHEN  DB2-NOTFND                                          01447014
                    MOVE  'VLE0142'   TO  CAA-COD-ERROR                 01448014
                    MOVE  -1          TO  CTA0101L                      01449014
                    PERFORM  3-FINAL                                    01450014
      *                                                                 01451014
              WHEN OTHER                                                01452014
                   MOVE 'SELECT'      TO  ABC-REFERENCIA                01453014
                   MOVE 'VLDTARC'     TO  ABC-OBJETO-ERROR              01454014
                   PERFORM 999-ABEND-DB2                                01455014
      *                                                                 01456014
           END-EVALUATE.                                                01457014
      *                                                                 01458014
      *200808196-INI                                                    01459014
           IF (CAA-CENTRO-CONT NOT = '0567' AND '0542') AND             01460014
              (VARC-FILLER (11:02) = '91' OR                            01461014
               VARC-FILLER (31:02) = '91')                              01462014
              MOVE 'VLE1702' TO CAA-COD-ERROR                           01463014
              MOVE -1        TO CTA0101L                                01464014
              PERFORM 3-FINAL                                           01465014
           END-IF                                                       01466014
      *200808196-FIN                                                    01467014
JPC@4      IF VARC-CENTAD = 0069 OR 2010                                01468014
JPC@4         PERFORM OBTENER-CTAGLOBAL                                 01469014
JPC@4      ELSE                                                         01470014
JPC@4         MOVE 'SOLO@S.A.B.'   TO  CTAGLOB-COMM                     01471014
JPC@4      END-IF                                                       01472014
      *                                                                 01473014
      *  SI LA CUENTA ESTA INACTIVA , MUESTRO IGUALMENTE                01474014
      *  LA CONSULTA, PERO NO PUEDO MODIFICAR, PERO DAR                 01475014
      *  DE ALTA  SI AUNQUE SUPONE UNA REACTIVACION                     01476014
      *                                                                 01477014
           MOVE VARC-CENTAD           TO W-ENTIDAD                      01478014
           MOVE W-ENTIDAD             TO ENT0101O VXMI-CODBE            01479014
      *                                                                 01480014
           PERFORM BUSCAR-ENTIDAD THRU BUSCAR-ENTIDAD-FIN.              01481014
      *A2012-I.                                                         01482014
           IF VXMI-CONPANT = 'N' AND CAA-CENTRO-CONT NOT = '0567'       01483014
              MOVE 'VLE1702' TO CAA-COD-ERROR                           01484014
              MOVE -1        TO CTA0101L                                01485014
              PERFORM 3-FINAL                                           01486014
           END-IF                                                       01487014
      *A2012-F.                                                         01488014
           MOVE VXMI-DENOM            TO NEN0101O                       01489014
           IF VXMI-OPEBOLSA = 'S'                                       01490014
              MOVE 'SI'              TO SW-OPERA-BOLSA                  01491014
           END-IF                                                       01492014
           IF VXMI-COMCUST = 'S'                                        01493014
              MOVE 'SI'              TO SW-CUSTODIA-AL-CLIENTE          01494014
              MOVE VXMI-CODCLI       TO W-CLIENTE-CUSTODIO-N            01495014
              IF W-CLIENTE-CUSTODIO = TIT0101I                          01496014
                 MOVE 'SI'           TO SW-ES-CTAVAL-CUS                01497014
                 IF VXMI-CTAVAL NOT = 0                                 01498014
                    MOVE 'SI'        TO SW-YA-CTAVAL-CUS                01499014
                 END-IF                                                 01500014
              END-IF                                                    01501014
              MOVE VXMI-CTACARGO     TO W-CTA-CAR-JUR                   01502014
              MOVE VXMI-CTAABONO     TO W-CTA-ABO-JUR                   01503014
           ELSE                                                         01504014
              MOVE 'NO'              TO SW-CUSTODIA-AL-CLIENTE          01505014
              MOVE VXMI-CODCLI       TO W-CLIENTE-CUSTODIO-N            01506014
              IF W-CLIENTE-CUSTODIO = TIT0101I                          01507014
                 MOVE 'SI'           TO SW-ES-CTAVAL-CUS                01508014
                 IF VXMI-CTAVAL NOT = 0                                 01509014
                    MOVE 'SI'        TO SW-YA-CTAVAL-CUS                01510014
                 END-IF                                                 01511014
              END-IF                                                    01512014
              IF VXMI-CTAVAL = ZEROES AND NOT ES-CTAVAL-CUS             01513014
                 MOVE  'VLE1731'     TO  CAA-COD-ERROR                  01514014
                 MOVE  -1            TO  CTA0101L                       01515014
                 PERFORM  3-FINAL                                       01516014
              ELSE                                                      01517014
                 IF NOT ES-CTAVAL-CUS                                   01518014
                    PERFORM CTAVAL-CUSTODIO                             01519014
                       THRU CTAVAL-CUSTODIO-FIN                         01520014
                 END-IF                                                 01521014
              END-IF                                                    01522014
              MOVE VXMI-CTACARGO     TO W-CTA-CAR-CUSTODIO NCC0101O     01523014
              MOVE VXMI-CTAABONO     TO W-CTA-ABO-CUSTODIO NC20101O     01524014
           END-IF                                                       01525014
      *                                                                 01526014
           MOVE VXMI-TIPCUST          TO WXMI-TIPCUST                   01527014
           MOVE VXMI-IMPALT           TO WXMI-IMPALT                    01528014
      *                                                                 01529014
           MOVE VARC-NUMCLI           TO W-TITULAR                      01530014
           MOVE W-TITULAR             TO TIT0101O                       01531014
      *                                                                 01532014
      * ACCESO A ALTAMIRA PARA OBTENER Nº CUENTA VALOR 20.              01533014
      *                                                                 01534014
      *     MODIFICADO DAVID 26-06-98                                   01535014
      *                                                                 01536014
      *     PERFORM NUM-CUENTA                                          01537014
      *        THRU NUM-CUENTA-FIN                                      01538014
      *                                                                 01539014
            MOVE VARC-CTAVAL20(01:4)   TO END0101O                      01540014
            MOVE VARC-CTAVAL20(05:4)   TO CEN0101O                      01541014
            MOVE VARC-CTAVAL20(09:2)   TO DGT0101O                      01542014
            MOVE VARC-CTAVAL20(11:2)   TO PRD0101O                      01543014
            MOVE VARC-CTAVAL20(20:1)   TO DG20101O                      01544014
      *                                                                 01545014
      * DE ESTE ACCESO OBTENDREMOS SOLO EL NOMBRE DEL TITULAR           01546014
      *                                                                 01547014
           PERFORM BUSCAR-NOMBRE                                        01548014
              THRU BUSCAR-NOMBRE-FIN                                    01549014
      *                                                                 01550014
      * EL INDICADOR DE PERSONAL/MAYOR DESAPARECE                       01551014
      *                                                                 01552014
      *    MOVE VARC-CLACARGO         TO CCT0101O                       01553014
           MOVE VARC-FILLER           TO VLWCCTA0                       01554014
      *                                                                 01555014
           IF W-CCC-CAR NOT = 0                                         01556014
             MOVE W-CCC-CAR           TO NCC0101O                       01557014
             PERFORM OBTENER-MONEDA1-CONSULTA                           01558014
                THRU OBTENER-MONEDA1-CONSULTA-FIN                       01559014
           END-IF                                                       01560014
      *                                                                 01561014
      * EL INDICADOR DE PERSONAL/MAYOR DESAPARECE                       01562014
      *                                                                 01563014
      *                                                                 01564014
      *    MOVE VARC-CLABONO          TO CAT0101O                       01565014
           MOVE VARC-FILLER           TO VLWCCTA0                       01566014
      *                                                                 01567014
           IF W-CCC-ABO NOT = 0                                         01568014
             MOVE W-CCC-ABO          TO NC20101O                        01569014
             PERFORM OBTENER-MONEDA2-CONSULTA                           01570014
                THRU OBTENER-MONEDA2-CONSULTA-FIN                       01571014
           END-IF                                                       01572014
      *                                                                 01573014
           MOVE VARC-SUCURS           TO W-SUCVAL                       01574014
           MOVE W-SUCVAL              TO SUC0101O                       01575014
           PERFORM VALIDAR-CENTRO                                       01576014
              THRU VALIDAR-CENTRO-FIN                                   01577014
                                                                        01578014
      *200503172-INI                                                    01579014
           MOVE VARC-CODSUS   TO CIN0101O                               01580014
           MOVE SPACES        TO NCU0101O                               01581014
           IF VARC-VALEXTRJ   = 'B'                                     01582014
              PERFORM 2233-SELECT-CUST-INTER                            01583014
                 THRU 2233-SELECT-CUST-INTER-FIN                        01584014
      *200512055-INI                                                    01585014
           ELSE                                                         01586014
              MOVE SPACES     TO CIN0101O                               01587014
JPC@6         IF VARC-CENTAD   = 0312                                   01588014
JPC@6            MOVE VARC-CODSUS   TO CIN0101O                         01589014
JPC@6         END-IF                                                    01590014
      *200512055-FIN                                                    01591014
           END-IF.                                                      01592014
      *200503172-FIN                                                    01593014
                                                                        01594014
      *                                                                 01595014
           MOVE VARC-MONEDA           TO MDA0101O                       01596014
           INITIALIZE                    TCWC1200                       01597014
           MOVE MDA0101I              TO W120-CDDIVISS                  01598014
           IF MDA0101I NOT = SPACES                                     01599014
              PERFORM OBTENER-MONEDA                                    01600014
                 THRU OBTENER-MONEDA-FIN                                01601014
           END-IF                                                       01602014
                                                                        01603014
           MOVE VARC-TEXTO            TO IDI0101O                       01604014
      *                                                                 01605014
      *    MOVE VARC-NUMDOM           TO DCO0101-N                      01606014
      *    MOVE DCO0101-N             TO DCO0101O                       01607014
                                                                        01608014
      * MODIFICADO DAVID 10-11-1998*                                    01609014
      **************************************************************    01610014
      ***********************RUTINA DE DOMICILIOS*******************    01611014
      **************************************************************    01612014
           INITIALIZE                            PEWC5100               01613014
      *                                                                 01614014
           MOVE END0101I                      TO W510-PECENTID          01615014
           MOVE CEN0101I                      TO W510-OFIAPE            01616014
      *    MOVE TIT0101I                      TO W510-NUMCLIEN          01617014
           MOVE PRD0101I                      TO W510-CODISER           01618014
           MOVE CTA0101I                      TO W510-NUMECTA(1:7)      01619014
           MOVE DG20101I                      TO W510-NUMECTA(8:1)      01620014
                                                                        01621014
           EXEC CICS                                                    01622014
                LINK PROGRAM  (PE2C5100)                                01623014
                     COMMAREA (PEWC5100)                                01624014
           END-EXEC.                                                    01625014
      *                                                                 01626014
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         01627014
              MOVE 'ERROR EN PE2C5100'  TO ABC-REFERENCIA               01628014
              MOVE 'PE2C5100'           TO ABC-OBJETO-ERROR             01629014
              PERFORM 999-ABEND-CICS                                    01630014
           END-IF.                                                      01631014
                                                                        01632014
           EVALUATE W510-PECRETOR                                       01633014
               WHEN ZEROS                                               01634014
                    MOVE W510-NUMDOMIC        TO DCO0101O               01635014
                    MOVE W510-NUMDOMIC        TO VARC-NUMDOM            01636014
               WHEN 10                                                  01637014
                    MOVE 'XXX'                TO DCO0101O               01638014
               WHEN 99                                                  01639014
                    INITIALIZE   QGECABC                                01640014
                    MOVE 'LINK PE2C5100'      TO ABC-REFERENCIA         01641014
                    MOVE W510-TABLENAME       TO ABC-OBJETO-ERROR       01642014
                    PERFORM 999-ABEND-DB2                               01643014
               WHEN OTHER                                               01644014
                    MOVE 'AAA'                TO DCO0101O               01645014
           END-EVALUATE.                                                01646014
      *                                                                 01647014
           MOVE VARC-RUT              TO CSU0101O                       01648014
           MOVE VARC-INDSAB           TO ODI0101O                       01649014
      *                                                                 01650014
           MOVE VARC-PAIS(1:3)        TO PAI0101O                       01651014
      *MODIFICACION TEMPORAL-23-03-1999-CAMBIAR RAPIDO                  01652014
      *    MOVE VARC-TARIFACUS        TO W-TARIFA                       01653014
      *    MOVE W-TARIFA              TO TAF0101O                       01654014
           MOVE VARC-INVERSOR         TO W-TARIFA                       01655014
           MOVE W-TARIFA              TO TAF0101O                       01656014
      *                                                                 01657014
           MOVE VARC-SWIFT-TELEX      TO SOT0101O                       01658014
      *                                                                 01659014
           MOVE VARC-CLTELEX          TO CLTELEX-AUX                    01660014
      *                                                                 01661014
           MOVE VARC-TELEX-2          TO TELEX2-AUX                     01662014
      *                                                                 01663014
           MOVE CLA-TELEX-AUX         TO TEL0101O                       01664014
      *                                                                 01665014
           MOVE VARC-EXEN1            TO CVE0101O                       01666014
           MOVE VARC-MAX-CVE-1        TO MCV0101O                       01667014
      *                                                                 01668014
           MOVE VARC-EXEN9            TO PAJ0101O                       01669014
           MOVE VARC-MAX-PAJ-9        TO MPJ0101O                       01670014
      *                                                                 01671014
           MOVE VARC-EXEN5            TO DCU0101O                       01672014
           MOVE VARC-MAX-DCU-5        TO MDC0101O                       01673014
      *                                                                 01674014
           MOVE VARC-EXEN7            TO DIV0101O                       01675014
           MOVE VARC-MAX-DIV-7        TO MDI0101O                       01676014
      *                                                                 01677014
           MOVE VARC-EXEN6            TO SUS0101O                       01678014
           MOVE VARC-MAX-SUS-6        TO MSU0101O                       01679014
      *                                                                 01680014
           MOVE VARC-EXEN8            TO AMO0101O                       01681014
           MOVE VARC-MAX-AMO-8        TO MAM0101O                       01682014
      *LIMA-24-04-1999.SE QUITA EL CAMPO CORREO                         01683014
      *    IF VARC-EXEN10 = 0                                           01684014
      *        MOVE 'S'               TO CCO0101O                       01685014
      *    ELSE                                                         01686014
      *        MOVE 'N'               TO CCO0101O                       01687014
      *    END-IF                                                       01688014
      *LIMA-24-04-1999.SE QUITA EL CAMPO CORREO                         01689014
      *                                                                 01690014
           MOVE VARC-VALEXTRJ         TO TCL0101O                       01691014
      *200712034-INI                                                    01692014
      *    MOVE VARC-NUMMAN           TO MAN0101O                       01693014
           MOVE VARC-GRUPO-CTAS       TO MAN0101O                       01694014
      *200712034-FIN                                                    01695014
      *200503172-INI                                                    01696014
           MOVE VARC-FEALTREG         TO W-FECHA-AMD-N                  01697014
           MOVE W-DD-AMD              TO W-DD-DMA-G                     01698014
           MOVE W-MM-AMD              TO W-MM-DMA-G                     01699014
           MOVE W-AA-AMD              TO W-AA-DMA-G                     01700014
           MOVE W-FECHA-DMA-G         TO ALT0101O                       01701014
           MOVE VARC-HORULMOD         TO HUM0101O                       01702014
           MOVE VARC-USUARIO          TO USU0101O                       01703014
      *200503172-FIN                                                    01704014
      *                                                                 01705014
           MOVE VARC-FEULMOD          TO W-FECHA-AMD-N                  01706014
           MOVE W-DD-AMD              TO W-DD-DMA-G                     01707014
           MOVE W-MM-AMD              TO W-MM-DMA-G                     01708014
           MOVE W-AA-AMD              TO W-AA-DMA-G                     01709014
           MOVE W-FECHA-DMA-G         TO FUA0101O                       01710014
      *                                                                 01711014
           MOVE CTA0101I              TO CTA0101-COMM                   01712014
           MOVE SUC0101I              TO SUC0101-COMM                   01713014
           MOVE NCC0101I              TO NCC0101-COMM                   01714014
           MOVE NC20101I              TO NC20101-COMM                   01715014
           MOVE ENT0101I              TO ENT0101-COMM                   01716014
           MOVE TIT0101I              TO TIT0101-COMM                   01717014
      *                                                                 01718014
           MOVE 'VLA0006'             TO CAA-COD-AVISO1                 01719014
      *                                                                 01720014
           IF WA-COD-ERROR NOT = SPACES                                 01721014
              MOVE WA-COD-ERROR       TO CAA-COD-ERROR                  01722014
              PERFORM 3-FINAL                                           01723014
           END-IF                                                       01724014
      *                                                                 01725014
           IF VARC-SITUAC = 'B' OR 'U' OR 'X'                           01726014
              MOVE 'I'                TO MSB-COMM                       01727014
           ELSE                                                         01728014
              MOVE SPACES             TO MSB-COMM                       01729014
           END-IF                                                       01730014
      *                                                                 01731014
JPC@2 *    CONSULTA CAMPAÑAS                                            01732014
JPC@2      INITIALIZE                     W-SMWCNIN0.                   01733014
JPC@2      MOVE 'C'                   TO  NIN0-OPCION.                  01734014
JPC@2      MOVE 'VL00'                TO  NIN0-APLICACION.              01735014
JPC@2      MOVE VARC-CTAVAL20 (01:08) TO  NIN0-CONTRATO (01:08).        01736014
JPC@2      MOVE VARC-CTAVAL20 (11:10) TO  NIN0-CONTRATO (09:10).        01737014
JPC@2 *                                                                 01738014
JPC@2      EXEC CICS                                                    01739014
JPC@2        LINK PROGRAM  (SM7CNIN0)                                   01740014
JPC@2             COMMAREA (SMWCNIN0)                                   01741014
JPC@2      END-EXEC                                                     01742014
JPC@2 *                                                                 01743014
JPC@2      IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         01744014
JPC@2         MOVE 'ERROR EN SMWCNIN0'  TO ABC-REFERENCIA               01745014
JPC@2         MOVE 'SM7CNIN0'           TO ABC-OBJETO-ERROR             01746014
JPC@2         PERFORM 999-ABEND-CICS                                    01747014
JPC@2      END-IF                                                       01748014
JPC@2 *                                                                 01749014
JPC@2      EVALUATE NIN0-CODRETORN                                      01750014
JPC@2          WHEN '00'                                                01751014
JPC@2               CONTINUE                                            01752014
JPC@2          WHEN '98'                                                01753014
JPC@2          WHEN '99'                                                01754014
JPC@2               MOVE -1               TO GVT0101L                   01755014
JPC@2               MOVE NIN0-COD-ERROR   TO CAA-COD-ERROR              01756014
JPC@2               MOVE NIN0-VAR1-ERROR  TO CAA-VAR1-ERROR             01757014
JPC@2               MOVE NIN0-VAR2-ERROR  TO CAA-VAR2-ERROR             01758014
JPC@2               PERFORM 3-FINAL                                     01759014
JPC@2          WHEN OTHER                                               01760014
JPC@2               INITIALIZE               W-SMWCNIN0                 01761014
JPC@2      END-EVALUATE.                                                01762014
JPC@2 *                                                                 01763014
JPC@2      MOVE NIN0-GEST-VTA         TO GVT0101O GVT0101-COMM          01764014
JPC@2      MOVE NIN0-CANAL-VTA        TO CAV0101O CAV0101-COMM          01765014
JPC@2      MOVE NIN0-SUBCANAL-VTA     TO SCV0101O SCV0101-COMM          01766014
JPC@2      MOVE NIN0-CODCAMP          TO CAM0101O CAM0101-COMM          01767014
JPC@2 *                                                                 01768014
      *200306088-INI                                                    01769014
           MOVE MSB-COMM              TO SITUACI-COMM                   01770014
      *200306088-FIN                                                    01771014
           MOVE 'C'                   TO OPT-COMM                       01772014
           MOVE -1                    TO CTA0101L.                      01773014
      *                                                                 01774014
      * YA NOS HEMOS GUARDADO PREVIAMENTE EN COMMAREA LOS DATOS CLAVE   01775014
      *                                                                 01776014
       21-CONSULTA-FIN.    EXIT.                                        01777014
      *                                                                 01778014
       OBTENER-MONEDA1.                                                 01779014
      *                                                                 01780014
      * OBTENEMOS LA MONEDA DE LA CTA DE CARGO Y LA OFICINA             01781014
      * PROPIETARIA CON SU DESCRIPCION                                  01782014
      *                                                                 01783014
           INITIALIZE                          W-BGECMDC                01784014
           MOVE NCC0101I(1:4)                 TO MDC-ENTIDAD            01785014
           MOVE NCC0101I(5:4)                 TO MDC-CENTRO-ALTA.       01786014
           MOVE NCC0101I(11:2)                TO MDC-CUENTA(1:2).       01787014
           MOVE NCC0101I(13:8)                TO MDC-CUENTA(3:8).       01788014
      *                                                                 01789014
           EXEC CICS                                                    01790014
             LINK PROGRAM  (BG2CMDC0)                                   01791014
                  COMMAREA (BGECMDC)                                    01792014
           END-EXEC                                                     01793014
      *                                                                 01794014
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         01795014
              MOVE 'ERROR EN BGECMDC0'  TO ABC-REFERENCIA               01796014
              MOVE 'BG2CMDC0'           TO ABC-OBJETO-ERROR             01797014
              PERFORM 999-ABEND-CICS                                    01798014
           END-IF                                                       01799014
      *                                                                 01800014
           EVALUATE MDC-CODERR                                          01801014
               WHEN SPACES                                              01802014
JIPC  *             IF MDC-INDESTA = 'A' OR 'R' OR 'P'                  01803014
                    IF MDC-INDESTA = 'A'                                01804014
                       MOVE MDC-CDDIVIS         TO MON0101O             01805014
                                                   W-MONEDA-CAR         01806014
JPC@4 *                MOVE MDC-CENTRO-CONTAB   TO OFI-PRO              01807014
JPC@4                  IF CAA-CENTRO-CONT NOT = '0567'                  01808014
JPC@4                     IF MSB-COMM = 'M'                             01809014
JPC@4                        MOVE SUC0101I             TO OFI-PRO       01810014
JPC@4                     ELSE                                          01811014
JPC@4                        IF ENT0101I = '0069' OR '2010'             01812014
JPC@4                           MOVE CAA-CENTRO-CONT   TO OFI-PRO       01813014
JPC@4                        ELSE                                       01814014
JPC@4                           MOVE MDC-CENTRO-CONTAB TO OFI-PRO       01815014
JPC@4                        END-IF                                     01816014
JPC@4                     END-IF                                        01817014
JPC@4                  ELSE                                             01818014
JPC@4                     MOVE SUC0101I                TO OFI-PRO       01819014
JPC@4                  END-IF                                           01820014
                    ELSE                                                01821014
                       MOVE MDC-CDDIVIS         TO MON0101O             01822014
                       MOVE -1                  TO NCC0101L             01823014
                       MOVE 'VLE1101'           TO CAA-COD-ERROR        01824014
                       PERFORM 3-FINAL                                  01825014
                    END-IF                                              01826014
               WHEN OTHER                                               01827014
                    MOVE -1                     TO NCC0101L             01828014
                    MOVE MDC-CODERR             TO CAA-COD-ERROR        01829014
                    PERFORM 3-FINAL                                     01830014
           END-EVALUATE.                                                01831014
      *                                                                 01832014
      *A2008-I. 10-08-99. OFICINA PROPIETARIA LA DE LA CTA DE CARGO     01833014
           MOVE OFI-PRO             TO SUC0101O.                        01834014
      *    MOVE CEN0101I            TO SUC0101O.                        01835014
      *A2008-F. 10-08-99. OFICINA PROPIETARIA LA DE LA CTA DE CARGO     01836014
      *                                                                 01837014
           PERFORM DESCRIPCION-OFICINA                                  01838014
              THRU DESCRIPCION-OFICINA-FIN.                             01839014
      *                                                                 01840014
       OBTENER-MONEDA1-FIN. EXIT.                                       01841014
      *                                                                 01842014
       OBTENER-MONEDA1-CONSULTA.                                        01843014
      *                                                                 01844014
      * OBTENEMOS LA MONEDA DE LA CTA DE CARGO Y LA OFICINA             01845014
      * PROPIETARIA CON SU DESCRIPCION                                  01846014
      *                                                                 01847014
           INITIALIZE                            W-BGECMDC              01848014
           MOVE NCC0101I(01:04)               TO MDC-ENTIDAD            01849014
           MOVE NCC0101I(05:04)               TO MDC-CENTRO-ALTA.       01850014
           MOVE NCC0101I(11:02)               TO MDC-CUENTA(1:2).       01851014
           MOVE NCC0101I(13:08)               TO MDC-CUENTA(3:8).       01852014
      *                                                                 01853014
           EXEC CICS                                                    01854014
             LINK PROGRAM (BG2CMDC0)                                    01855014
                 COMMAREA (BGECMDC)                                     01856014
           END-EXEC                                                     01857014
      *                                                                 01858014
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         01859014
              MOVE 'ERROR EN BGECMDC0'  TO ABC-REFERENCIA               01860014
              MOVE 'BG2CMDC0'           TO ABC-OBJETO-ERROR             01861014
              PERFORM 999-ABEND-CICS                                    01862014
           END-IF                                                       01863014
      *                                                                 01864014
           EVALUATE MDC-CODERR                                          01865014
               WHEN SPACES                                              01866014
JIPC  *             IF MDC-INDESTA = 'A' OR 'R' OR 'P'                  01867014
                    IF MDC-INDESTA = 'A'                                01868014
                       MOVE MDC-CDDIVIS         TO MON0101O             01869014
                                                   W-MONEDA-CAR         01870014
JPC@4 *                MOVE MDC-CENTRO-CONTAB   TO OFI-PRO              01871014
JPC@4                  MOVE VARC-SUCURS         TO OFI-PRO-N            01872014
      *             ELSE                                                01873014
      *                MOVE MDC-CDDIVIS         TO MON0101O             01874014
      *                MOVE -1                  TO NCC0101L             01875014
      *                MOVE 'VLE1101'           TO CAA-COD-ERROR        01876014
      *                PERFORM 3-FINAL                                  01877014
                    END-IF                                              01878014
JIPC           WHEN 'BGE0002'                                           01879014
 ||   *  NO EXISTE LA CUENTA EN LA BGDTMAE                              01880014
 ||                 MOVE SPACES                 TO MON0101O             01881014
 ||                                                W-MONEDA-CAR         01882014
 ||                 MOVE MDC-CODERR             TO WA-COD-ERROR         01883014
 ||                 MOVE VARC-SUCURS            TO OFI-PRO-N            01884014
JIPC                MOVE -1                     TO NCC0101L             01885014
               WHEN OTHER                                               01886014
                    MOVE -1                     TO NCC0101L             01887014
                    MOVE MDC-CODERR             TO CAA-COD-ERROR        01888014
                    PERFORM 3-FINAL                                     01889014
           END-EVALUATE.                                                01890014
      *                                                                 01891014
           MOVE OFI-PRO             TO SUC0101O.                        01892014
      *                                                                 01893014
           PERFORM DESCRIPCION-OFICINA                                  01894014
              THRU DESCRIPCION-OFICINA-FIN.                             01895014
      *                                                                 01896014
       OBTENER-MONEDA1-CONSULTA-FIN. EXIT.                              01897014
      *                                                                 01898014
      *                                                                 01899014
       OBTENER-MONEDA2.                                                 01900014
      *                                                                 01901014
           INITIALIZE                          W-BGECMDC                01902014
           MOVE NC20101I(1:4)                 TO MDC-ENTIDAD            01903014
           MOVE NC20101I(5:4)                 TO MDC-CENTRO-ALTA.       01904014
           MOVE NC20101I(11:2)                TO MDC-CUENTA(1:2).       01905014
           MOVE NC20101I(13:8)                TO MDC-CUENTA(3:8).       01906014
      *                                                                 01907014
           EXEC CICS                                                    01908014
             LINK PROGRAM  (BG2CMDC0)                                   01909014
                  COMMAREA (BGECMDC)                                    01910014
           END-EXEC                                                     01911014
      *                                                                 01912014
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         01913014
              MOVE 'ERROR EN BGECMDC0'  TO ABC-REFERENCIA               01914014
              MOVE 'BG2CMDC0'           TO ABC-OBJETO-ERROR             01915014
              PERFORM 999-ABEND-CICS                                    01916014
           END-IF                                                       01917014
      *                                                                 01918014
           EVALUATE MDC-CODERR                                          01919014
               WHEN SPACES                                              01920014
JIPC  *             IF MDC-INDESTA = 'A' OR 'R' OR 'P'                  01921014
                    IF MDC-INDESTA = 'A'                                01922014
                       MOVE MDC-CDDIVIS TO MO20101O                     01923014
                                           W-MONEDA-ABO                 01924014
                    ELSE                                                01925014
                       MOVE MDC-CDDIVIS TO MO20101O                     01926014
                       MOVE -1           TO NC20101L                    01927014
                       MOVE 'VLE1101'    TO CAA-COD-ERROR               01928014
                       PERFORM 3-FINAL                                  01929014
                    END-IF                                              01930014
               WHEN OTHER                                               01931014
                    MOVE -1           TO NC20101L                       01932014
                    MOVE MDC-CODERR   TO CAA-COD-ERROR                  01933014
                    PERFORM 3-FINAL                                     01934014
                                                                        01935014
           END-EVALUATE.                                                01936014
      *                                                                 01937014
      *                                                                 01938014
       OBTENER-MONEDA2-FIN. EXIT.                                       01939014
      *                                                                 01940014
       OBTENER-MONEDA2-CONSULTA.                                        01941014
      *                                                                 01942014
           INITIALIZE                          W-BGECMDC                01943014
           MOVE NC20101I(1:4)                 TO MDC-ENTIDAD            01944014
           MOVE NC20101I(5:4)                 TO MDC-CENTRO-ALTA.       01945014
           MOVE NC20101I(11:2)                TO MDC-CUENTA(1:2).       01946014
           MOVE NC20101I(13:8)                TO MDC-CUENTA(3:8).       01947014
      *                                                                 01948014
           EXEC CICS                                                    01949014
             LINK PROGRAM  (BG2CMDC0)                                   01950014
                  COMMAREA (BGECMDC)                                    01951014
           END-EXEC                                                     01952014
      *                                                                 01953014
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         01954014
              MOVE 'ERROR EN BGECMDC0'  TO ABC-REFERENCIA               01955014
              MOVE 'BG2CMDC0'           TO ABC-OBJETO-ERROR             01956014
              PERFORM 999-ABEND-CICS                                    01957014
           END-IF                                                       01958014
      *                                                                 01959014
           EVALUATE MDC-CODERR                                          01960014
               WHEN SPACES                                              01961014
JIPC  *             IF MDC-INDESTA = 'A' OR 'R' OR 'P'                  01962014
                    IF MDC-INDESTA = 'A'                                01963014
                       MOVE MDC-CDDIVIS  TO MO20101O                    01964014
                                            W-MONEDA-ABO                01965014
      *             ELSE                                                01966014
      *                MOVE MDC-CDDIVIS  TO MO20101O                    01967014
      *                MOVE -1           TO NC20101L                    01968014
      *                MOVE 'VLE1101'    TO CAA-COD-ERROR               01969014
      *                PERFORM 3-FINAL                                  01970014
                    END-IF                                              01971014
JIPC           WHEN 'BGE0002'                                           01972014
 ||   *  NO EXISTE LA CUENTA EN LA BGDTMAE                              01973014
 ||                 MOVE SPACES                 TO MO20101O             01974014
 ||                                                W-MONEDA-ABO         01975014
 ||                 MOVE MDC-CODERR             TO WA-COD-ERROR         01976014
JIPC                MOVE -1                     TO NC20101L             01977014
               WHEN OTHER                                               01978014
                    MOVE -1                TO NC20101L                  01979014
                    MOVE MDC-CODERR        TO CAA-COD-ERROR             01980014
                    PERFORM 3-FINAL                                     01981014
           END-EVALUATE.                                                01982014
      *                                                                 01983014
       OBTENER-MONEDA2-CONSULTA-FIN. EXIT.                              01984014
      *                                                                 01985014
       22-ALTA.                                                         01986014
      *                                                                 01987014
           INITIALIZE DCLVLDTARC                                        01988014
                                                                        01989014
           IF CTA0101I  = SPACES                                        01990014
              MOVE SPACES TO  END0101O                                  01991014
                              CEN0101O                                  01992014
                              DGT0101O                                  01993014
                              PRD0101O                                  01994014
                              DG20101O                                  01995014
JPC@4         IF CAA-CENTRO-CONT NOT = '0567'                           01996014
JPC@4            MOVE SPACES      TO  SUC0101O                          01997014
JPC@4         END-IF                                                    01998014
                                                                        01999014
EZS@1         INITIALIZE               REG-PEWC4140                     02000014
  |           MOVE '2'                 TO W4140-CASO                    02001014
  |           MOVE TIT0101I            TO W4140-NUMCLIEN                02002014
  |           MOVE '403'               TO W4140-CODINRE-I               02003014
  |   *                                                                 02004014
  |           EXEC CICS                                                 02005014
  |             LINK PROGRAM (PE7C4140)                                 02006014
  |                  COMMAREA (REG-PEWC4140)                            02007014
  |           END-EXEC                                                  02008014
  |   *                                                                 02009014
  |           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                      02010014
  |              MOVE 'ERROR EN PE7C4140'  TO ABC-REFERENCIA            02011014
  |              MOVE 'PE7C4140'           TO ABC-OBJETO-ERROR          02012014
  |              PERFORM 999-ABEND-CICS                                 02013014
  |           END-IF                                                    02014014
  |   *                                                                 02015014
  |           EVALUATE W4140-PCRETOR                                    02016014
  |              WHEN '00'                                              02017014
  |                   MOVE 'VLE3006' TO CAA-COD-ERROR                   02018014
  |                   MOVE W4140-CODINRE-I TO CAA-VAR1-ERROR            02019014
  |                   MOVE -1        TO TIT0101L                        02020014
  |                   PERFORM 3-FINAL                                   02021014
  |              WHEN '50'                                              02022014
  |                   CONTINUE                                          02023014
  |              WHEN '10'                                              02024014
  |              WHEN '20'                                              02025014
  |              WHEN '40'                                              02026014
  |                 MOVE -1                TO TIT0101L                  02027014
  |                 MOVE 'VLE2169'              TO CAA-COD-ERROR        02028014
  |                 MOVE 'ERROR RUT-PE7C4140 '  TO CAA-VAR1-ERROR       02029014
  |                 MOVE W4140-PCRETOR          TO CAA-VAR2-ERROR       02030014
  |              WHEN '99'                                              02031014
  |                   INITIALIZE   QGECABC                              02032014
  |                   MOVE 'LINK PE7C4140'  TO ABC-REFERENCIA           02033014
  |                   MOVE W4140-SQLCODE    TO ABC-OBJETO-ERROR         02034014
  |                   PERFORM 999-ABEND-DB2                             02035014
  |           END-EVALUATE                                              02036014
  |   *                                                                 02037014
  |           MOVE '408'       TO W4140-CODINRE-I                       02038014
  | *                                                                   02039014
  |           EXEC CICS                                                 02040014
  |             LINK PROGRAM (PE7C4140)                                 02041014
  |                  COMMAREA (REG-PEWC4140)                            02042014
  |           END-EXEC                                                  02043014
  |   *                                                                 02044014
  |           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                      02045014
  |              MOVE 'ERROR EN PE7C4140'  TO ABC-REFERENCIA            02046014
  |              MOVE 'PE7C4140'           TO ABC-OBJETO-ERROR          02047014
  |              PERFORM 999-ABEND-CICS                                 02048014
  |           END-IF                                                    02049014
  |   *                                                                 02050014
  |           EVALUATE W4140-PCRETOR                                    02051014
  |              WHEN '00'                                              02052014
  |                   MOVE 'VLE3006' TO CAA-COD-ERROR                   02053014
  |                   MOVE W4140-CODINRE-I TO CAA-VAR1-ERROR            02054014
  |                   MOVE -1        TO TIT0101L                        02055014
  |                   PERFORM 3-FINAL                                   02056014
  |              WHEN '50'                                              02057014
  |                   CONTINUE                                          02058014
  |              WHEN '10'                                              02059014
  |              WHEN '20'                                              02060014
  |              WHEN '40'                                              02061014
  |                 MOVE -1                TO TIT0101L                  02062014
  |                 MOVE 'VLE2169'              TO CAA-COD-ERROR        02063014
  |                 MOVE 'ERROR RUT-PE7C4140 '  TO CAA-VAR1-ERROR       02064014
  |                 MOVE W4140-PCRETOR          TO CAA-VAR2-ERROR       02065014
  |              WHEN '99'                                              02066014
  |                 INITIALIZE QGECABC                                  02067014
  |                 MOVE 'LINK PE7C4140' TO ABC-REFERENCIA              02068014
  |                 MOVE W4140-SQLCODE   TO ABC-OBJETO-ERROR            02069014
  |                 PERFORM 999-ABEND-DB2                               02070014
  |           END-EVALUATE                                              02071014
EZS@1 *                                                                 02072014
              PERFORM 220-ALTA-NUEVA                                    02073014
                 THRU 220-ALTA-NUEVA-FIN                                02074014
              IF ES-CTAVAL-CUS AND NOT YA-CTAVAL-CUS                    02075014
                 MOVE VARC-CENTAD        TO W-CODBE                     02076014
                 MOVE W-CODBE            TO LXMI-CODBE                  02077014
                 PERFORM SELUND-VLDTXMI                                 02078014
                    THRU SELUND-VLDTXMI-FIN                             02079014
      *                                                                 02080014
                 MOVE VARC-CTAVAL20(5:4) TO VXMI-SUCVAL                 02081014
                 MOVE VARC-CENTAD        TO W-CODBE                     02082014
                 MOVE W-CODBE            TO VXMI-CODBE                  02083014
      *                                                                 02084014
                 EXEC SQL UPDATE VLDTXMI                                02085014
                          SET VXMI_CTAVAL = :VARC-CUENTA,               02086014
                              VXMI_SUCVAL = :VXMI-SUCVAL                02087014
                          WHERE VXMI_CODBE = :VXMI-CODBE                02088014
                 END-EXEC                                               02089014
      *                                                                 02090014
                 MOVE SQLCODE TO SQLCODE-AUX                            02091014
      *                                                                 02092014
                 EVALUATE TRUE                                          02093014
                     WHEN DB2-OK                                        02094014
                           INITIALIZE W-VLWCLOG0                        02095014
                                        LOGVLDTXMI                      02096014
                           MOVE 'VLDTXMI'            TO VL7LOG-TABLA    02097014
                           MOVE 'UPDATE'             TO VL7LOG-OPERACION02098014
                           MOVE LENGTH OF DCLVLDTXMI TO                 02099014
                                                     VL7LOG-REGISTRO-LEN02100014
                           MOVE VARC-CUENTA          TO LXMI-CTAVAL     02101014
                           MOVE VXMI-SUCVAL          TO LXMI-SUCVAL     02102014
                           MOVE VXMI-CODBE           TO LXMI-CODBE      02103014
                           MOVE LOGVLDTXMI       TO VL7LOG-REGISTRO-TEXT02104014
                           PERFORM LLAMAR-VL7CRLOG                      02105014
                              THRU LLAMAR-VL7CRLOG-FIN                  02106014
                     WHEN OTHER                                         02107014
                          MOVE 'UPDATE'      TO  ABC-REFERENCIA         02108014
                          MOVE 'VLDTXMI'     TO  ABC-OBJETO-ERROR       02109014
                          PERFORM 999-ABEND-DB2                         02110014
                 END-EVALUATE                                           02111014
              END-IF                                                    02112014
      *                                                                 02113014
              MOVE CTA0101I         TO CTA0101-COMM                     02114014
              MOVE SUC0101I         TO SUC0101-COMM                     02115014
              MOVE NCC0101I         TO NCC0101-COMM                     02116014
              MOVE NC20101I         TO NC20101-COMM                     02117014
              MOVE TIT0101I         TO TIT0101-COMM                     02118014
              MOVE 'VLA0001'        TO CAA-COD-AVISO1                   02119014
              MOVE 'A'              TO OPT-COMM                         02120014
              MOVE ' '              TO MSB-COMM                         02121014
              MOVE -1               TO CTA0101L                         02122014
              IF VXMI-IMPALT = 'N'                                      02123014
                 PERFORM 223-RELACION-PRODUCTO                          02124014
                    THRU 223-RELACION-PRODUCTO-FIN                      02125014
                 PERFORM ACTUALIZAR-VLDTARC                             02126014
                    THRU ACTUALIZAR-VLDTARC-FIN                         02127014
              END-IF                                                    02128014
           ELSE                                                         02129014
      *200306088-INI                                                    02130014
              IF SITUACI-COMM = 'I'                                     02131014
                 MOVE 'VLE1945' TO CAA-COD-ERROR                        02132014
                 MOVE -1        TO CTA0101L                             02133014
                 PERFORM 3-FINAL                                        02134014
              ELSE                                                      02135014
                 MOVE 'VLE2177' TO CAA-COD-ERROR                        02136014
                 MOVE -1        TO CTA0101L                             02137014
                 PERFORM 3-FINAL                                        02138014
              END-IF                                                    02139014
      *200306088-FIN                                                    02140014
           END-IF.                                                      02141014
      *                                                                 02142014
       22-ALTA-FIN.    EXIT.                                            02143014
      *                                                                 02144014
       220-ALTA-NUEVA.                                                  02145014
      *                                                                 02146014
      * VALIDAMOS LOS DATOS DE LA CUENTA                                02147014
      *                                                                 02148014
           PERFORM VALIDAR-CAMPOS                                       02149014
              THRU VALIDAR-CAMPOS-FIN                                   02150014
JPC@4 *                                                                 02151014
JPC@4 * VALIDAMOS SI TIENE MAS CUENTA VALOR CON LA MISMO MONEDA         02152014
JPC@4 *                                                                 02153014
JPC@4      IF CAA-CENTRO-CONT NOT = '0567'                              02154014
JPC@4         IF ENT0101I = '0069' OR '2010'                            02155014
JPC@4            PERFORM VALIDAR-OTRAS-CTAS                             02156014
JPC@4               THRU VALIDAR-OTRAS-CTAS-FIN                         02157014
JPC@4         END-IF                                                    02158014
JPC@4      END-IF                                                       02159014
JPC@4 *                                                                 02160014
      *                                                                 02161014
      * OBTENEMOS NUEVO NUMERO DE CTA. VALORES                          02162014
           MOVE 'NO' TO SW-CTA-OK                                       02163014
      *                                                                 02164014
           PERFORM UNTIL CTA-OK                                         02165014
              INITIALIZE W-TCWC0500                                     02166014
              MOVE CAA-ENTIDAD       TO   W050-TCCENTITE                02167014
              MOVE 9999              TO   W050-TCCOFICIE                02168014
              MOVE 91                TO   W050-TCCCONTRE                02169014
      *                                                                 02170014
              EXEC CICS                                                 02171014
                   LINK PROGRAM (TC2C1700)                              02172014
                   COMMAREA     (W-TCWC0500)                            02173014
              END-EXEC                                                  02174014
      *                                                                 02175014
              IF EIBRESP NOT = DFHRESP(NORMAL)                          02176014
                 MOVE 'ERROR EN TC2C1700'          TO   ABC-REFERENCIA  02177014
                 MOVE 'TC2C1700'                   TO   ABC-OBJETO-ERROR02178014
                 PERFORM 999-ABEND-CICS                                 02179014
              END-IF                                                    02180014
      *                                                                 02181014
              EVALUATE W050-CDRETORN                                    02182014
                  WHEN '00'                                             02183014
                       CONTINUE                                         02184014
                  WHEN '99'                                             02185014
                       INITIALIZE   QGECABC                             02186014
                       MOVE 'TC2C1700'             TO ABC-REFERENCIA    02187014
                       MOVE W050-TABLENAME         TO ABC-OBJETO-ERROR  02188014
                       MOVE W050-SQLCODE           TO SQLCODE           02189014
                       MOVE W050-SQLERRM           TO SQLERRM           02190014
                       PERFORM 999-ABEND-DB2                            02191014
                  WHEN OTHER                                            02192014
                       MOVE  -1                    TO CTA0101L          02193014
                       MOVE 'VLE0907'              TO CAA-COD-ERROR     02194014
                       MOVE 'TC2C1700'             TO CAA-VAR1-ERROR    02195014
                       MOVE W050-CDRETORN          TO CAA-VAR2-ERROR    02196014
                       PERFORM 3-FINAL                                  02197014
              END-EVALUATE                                              02198014
      *                                                                 02199014
              INITIALIZE TCWC2020                                       02200014
      *                                                                 02201014
              MOVE SUC0101I            TO W202-TCTCNCTO                 02202014
              MOVE 91                  TO W202-TCTCNCTE                 02203014
              MOVE W050-TCNCONTR(1:7)  TO W202-TCNCNCT7                 02204014
              MOVE 0                   TO W202-TCNDIGI1                 02205014
              MOVE '1'                 TO W202-TCTOPCIO                 02206014
      *                                                                 02207014
              CALL 'TC8C2020' USING TCWC2020                            02208014
      *                                                                 02209014
              EVALUATE W202-TCCESRET                                    02210014
                  WHEN '00'                                             02211014
                       MOVE 'SI' TO SW-CTA-OK                           02212014
                       MOVE W202-TCNDIGI1 TO W050-TCNCONTR(8:1)         02213014
                  WHEN '15'                                             02214014
                       CONTINUE                                         02215014
                  WHEN OTHER                                            02216014
                       MOVE  -1                  TO CTA0101L            02217014
                       MOVE 'VLE0907'            TO CAA-COD-ERROR       02218014
                       MOVE 'TC8C2020'           TO CAA-VAR1-ERROR      02219014
                       MOVE W202-TCCESRET        TO CAA-VAR2-ERROR      02220014
                       PERFORM 3-FINAL                                  02221014
               END-EVALUATE                                             02222014
           END-PERFORM.                                                 02223014
      *                                                                 02224014
           MOVE CAA-ENTIDAD          TO END0101O                        02225014
           MOVE SUC0101I             TO CEN0101O                        02226014
           MOVE '91'                 TO PRD0101O                        02227014
           MOVE W050-TCNCONTR(1:7)   TO CTA0101O                        02228014
           MOVE '00'                 TO DGT0101O                        02229014
           MOVE W050-TCNCONTR(8:1)   TO DG20101O                        02230014
      *                                                                 02231014
      *200808196-INI                                                    02232014
           MOVE END0101I   TO   WA-CTA-091 (01:04).                     02233014
           MOVE CEN0101I   TO   WA-CTA-091 (05:04).                     02234014
           MOVE '00'       TO   WA-CTA-091 (09:02).                     02235014
           MOVE PRD0101I   TO   WA-CTA-091 (11:02).                     02236014
           MOVE CTA0101I   TO   WA-CTA-091 (13:07).                     02237014
           MOVE DG20101I   TO   WA-CTA-091 (20:01).                     02238014
           IF NCC0101I (11:02)   = '91' OR                              02239014
              NC20101I (11:02)   = '91'                                 02240014
              IF WA-CTA-091 NOT = NCC0101I                              02241014
                 MOVE  -1                    TO NCC0101L                02242014
                 MOVE 'VLE2169'              TO CAA-COD-ERROR           02243014
                 MOVE 'CTA-REGISTRO DEBE SE' TO CAA-VAR1-ERROR          02244014
                 MOVE 'R IGUAL A CTA-VALOR ' TO CAA-VAR2-ERROR          02245014
                 PERFORM 3-FINAL                                        02246014
              END-IF                                                    02247014
              IF WA-CTA-091 NOT = NC20101I                              02248014
                 MOVE  -1                    TO NC20101L                02249014
                 MOVE 'VLE2169'              TO CAA-COD-ERROR           02250014
                 MOVE 'CTA-REGISTRO DEBE SE' TO CAA-VAR1-ERROR          02251014
                 MOVE 'R IGUAL A CTA-VALOR ' TO CAA-VAR2-ERROR          02252014
                 PERFORM 3-FINAL                                        02253014
              END-IF                                                    02254014
           END-IF.                                                      02255014
      *200808196-FIN                                                    02256014
      ***************************************************************** 02257014
      * ACCESO A LA RUTINA PE2C6000 PARA INDICAR A ALTAMIRA EL NUEVO    02258014
      * INTERVINIENTE                                                   02259014
      ***************************************************************** 02260014
      *                                                                 02261014
           INITIALIZE                   W600-REGISTRO.                  02262014
      *                                                                 02263014
           MOVE 'A'                  TO W600-PEYOPCIO                   02264014
           MOVE TIT0101I             TO W600-NUMCLIEN                   02265014
           MOVE 'T'                  TO W600-CLAINTER                   02266014
           MOVE '01'                 TO W600-SECINTER                   02267014
      *                                                                 02268014
           MOVE CAA-ENTIDAD          TO W600-PECENTID                   02269014
           MOVE SUC0101I             TO W600-OFIAPE                     02270014
                                        W600-PENOFMOD                   02271014
           MOVE '91'                 TO W600-CODISER                    02272014
           MOVE W050-TCNCONTR        TO W600-NUMECTA                    02273014
      *                                                                 02274014
           MOVE CAA-USERID           TO W600-USUARIO                    02275014
           MOVE 'VL'                 TO W600-APLICACIO                  02276014
           MOVE CAA-FECHA-OPER(1:4)  TO W600-FECHAPE(1:4)               02277014
                                        W600-FEALRELA(1:4)              02278014
           MOVE '-'                  TO W600-FECHAPE(5:1)               02279014
                                        W600-FEALRELA(5:1)              02280014
           MOVE CAA-FECHA-OPER(5:2)  TO W600-FECHAPE(6:2)               02281014
                                        W600-FEALRELA(6:2)              02282014
           MOVE '-'                  TO W600-FECHAPE(8:1)               02283014
                                        W600-FEALRELA(8:1)              02284014
           MOVE CAA-FECHA-OPER(7:2)  TO W600-FECHAPE(9:2)               02285014
                                        W600-FEALRELA(9:2)              02286014
           MOVE '0001-01-01'         TO W600-FECANCEL                   02287014
      *                                                                 02288014
           EXEC CICS                                                    02289014
               LINK PROGRAM (PE2C6000)                                  02290014
               COMMAREA     (W-PEWC6000)                                02291014
           END-EXEC                                                     02292014
      *                                                                 02293014
           IF EIBRESP NOT = DFHRESP(NORMAL)                             02294014
              MOVE 'ERROR EN PE2C6000'  TO ABC-REFERENCIA               02295014
              MOVE 'PE2C6000'           TO ABC-OBJETO-ERROR             02296014
              PERFORM 999-ABEND-CICS                                    02297014
           END-IF                                                       02298014
      *                                                                 02299014
           EVALUATE W600-PECRETOR                                       02300014
           WHEN '00'                                                    02301014
                CONTINUE                                                02302014
           WHEN OTHER                                                   02303014
               MOVE  -1                  TO CTA0101L                    02304014
               MOVE 'VLE0907'            TO CAA-COD-ERROR               02305014
               MOVE 'PE2C6000'           TO CAA-VAR1-ERROR              02306014
               MOVE W600-PECRETOR        TO CAA-VAR2-ERROR              02307014
               PERFORM 3-FINAL                                          02308014
           END-EVALUATE.                                                02309014
      *A2012-I.                                                         02310014
           IF VXMI-APCTAOFI = 'N' AND CAA-CENTRO-CONT NOT = '0567'      02311014
               MOVE  -1                  TO CTA0101L                    02312014
               MOVE 'VLE1653'            TO CAA-COD-ERROR               02313014
               PERFORM 3-FINAL                                          02314014
           END-IF                                                       02315014
      *A2012-F.                                                         02316014
      *200711038-INI                                                    02317014
           IF CAA-CENTRO-CONT = '0542' AND                              02318014
              REG0101I        = 'S'                                     02319014
              PERFORM CUENTA-REGISTRO                                   02320014
                 THRU CUENTA-REGISTRO-FIN                               02321014
           END-IF                                                       02322014
      *200711038-FIN                                                    02323014
           PERFORM MOVER-A-TABLA                                        02324014
              THRU MOVER-A-TABLA-FIN                                    02325014
      *                                                                 02326014
           MOVE 'N'               TO  VARC-CLMAST.                      02327014
           MOVE CAA-FECHA-OPER    TO  VARC-FE-ULT-EXT VARC-FE-ALTA.     02328014
           MOVE 0                 TO  VARC-FE-CARTERA.                  02329014
           MOVE 'A'               TO  VARC-SITUAC                       02330014
      *                                                                 02331014
           MOVE CAA-FECHA-OPER    TO  W-FECHA-AMD                       02332014
           MOVE W-AA-AMD          TO  W-AA-DMA-G                        02333014
           MOVE W-MM-AMD          TO  W-MM-DMA-G                        02334014
           MOVE W-DD-AMD          TO  W-DD-DMA-G                        02335014
           MOVE W-FECHA-DMA-G     TO  FUA0101O                          02336014
      *200503172-INI                                                    02337014
           MOVE W-FECHA-DMA-G     TO  ALT0101O                          02338014
           MOVE CAA-HORA-TRANS    TO  HUM0101O                          02339014
           MOVE CAA-USERID        TO  USU0101O                          02340014
      *200503172-FIN                                                    02341014
           MOVE 0                 TO  VARC-FECHA-102                    02342014
      *                                                                 02343014
           MOVE 'N'               TO  VARC-OPER-TIT                     02344014
      *                                                                 02345014
           MOVE END0101I          TO  VARC-CTAVAL20(1:4)                02346014
           MOVE CEN0101I          TO  VARC-CTAVAL20(5:4)                02347014
           MOVE DGT0101I          TO  VARC-CTAVAL20(9:2)                02348014
           MOVE PRD0101I          TO  VARC-CTAVAL20(11:2)               02349014
           MOVE CTA0101I          TO  VARC-CTAVAL20(13:7)               02350014
           MOVE DG20101I          TO  VARC-CTAVAL20(20:1)               02351014
      *                                                                 02352014
      *  SE MUEVE EL VALOR DE 3 AL CAMPO VARC-GRUPO, POR CONSIDERARSE   02353014
      *  CUENTA APERTURADA DE LOS CLIENTES CON CTA GLOBAL               02354014
      *                                                                 02355014
JIPC       IF TAF0101I = '51'          AND                              02356014
27-10         OPERA-BOLSA              AND                              02357014
              W-CTA-ABO-JUR = NC20101I AND                              02358014
              W-CTA-CAR-JUR = NCC0101I                                  02359014
              MOVE 3              TO  VARC-GRUPO                        02360014
           END-IF                                                       02361014
      *                                                                 02362014
      *  SE GRABA OFICNA QUE APERTURA LA OFICINA                        02363014
      *  JIPC                         17-04-2001                        02364014
      *                                                                 02365014
           MOVE CAA-CENTRO-CONT   TO  VARC-PRESEN                       02366014
      *                                                                 02367014
           MOVE CAA-FECHA-OPER    TO  VARC-FEULMOD                      02368014
                                      VARC-FEALTREG                     02369014
           MOVE CAA-HORA-TRANS    TO  VARC-HORULMOD                     02370014
           MOVE CAA-TERMINAL      TO  VARC-NUMTER                       02371014
           MOVE CAA-USERID        TO  VARC-USUARIO                      02372014
                                                                        02373014
           EXEC SQL                                                     02374014
                INSERT INTO VLDTARC                                     02375014
                VALUES (:DCLVLDTARC)                                    02376014
           END-EXEC                                                     02377014
      *                                                                 02378014
           MOVE SQLCODE TO SQLCODE-AUX                                  02379014
      *                                                                 02380014
           EVALUATE TRUE                                                02381014
              WHEN DB2-OK                                               02382014
                   INITIALIZE W-VLWCLOG0                                02383014
                              LOGVLDTARC                                02384014
                   MOVE 'VLDTARC'             TO  VL7LOG-TABLA          02385014
                   MOVE 'INSERT'              TO  VL7LOG-OPERACION      02386014
                   MOVE LENGTH OF DCLVLDTARC  TO  VL7LOG-REGISTRO-LEN   02387014
                   MOVE DCLVLDTARC            TO  LOGVLDTARC            02388014
                   MOVE LOGVLDTARC            TO  VL7LOG-REGISTRO-TEXT  02389014
                   PERFORM LLAMAR-VL7CRLOG                              02390014
                      THRU LLAMAR-VL7CRLOG-FIN                          02391014
                                                                        02392014
                   PERFORM INSERTAR-VCTASMES                            02393014
                      THRU INSERTAR-VCTASMES-FIN                        02394014
      *                                                                 02395014
              WHEN OTHER                                                02396014
                   MOVE 'INSERT'      TO  ABC-REFERENCIA                02397014
                   MOVE 'VLDTARC'     TO  ABC-OBJETO-ERROR              02398014
                   PERFORM 999-ABEND-DB2                                02399014
      *                                                                 02400014
           END-EVALUATE.                                                02401014
      *                                                                 02402014
      **************************************************************    02403014
      *****************RUTINA BGECMSC****DAVID  06-11-1998**********    02404014
      * RUTINA PARA DAR ALTA, VINCULA LA CTA ECONOMICA CON LA CTA*      02405014
      * VALOR, IMPIDE LA BAJA DE LA CTA ECONOMICA VINCULADA       *     02406014
      **************************************************************    02407014
      *                                                                 02408014
           MOVE NCC0101I       TO  W-CARGO                              02409014
           MOVE NC20101I       TO  W-ABO                                02410014
                                                                        02411014
      *200711038-INI                                                    02412014
           IF REG0101I = 'N'                                            02413014
      *200711038-FIN                                                    02414014
              IF W-CARGO = W-ABO                                        02415014
                 INITIALIZE                     BGECMSC                 02416014
                                                                        02417014
                 MOVE '1'                   TO MSC-FUNCION              02418014
                 MOVE NCC0101I(11:2)        TO MSC-CUENTA(1:2)          02419014
                 MOVE NCC0101I(13:8)        TO MSC-CUENTA(3:8)          02420014
                 MOVE NCC0101I(1:4)         TO MSC-ENTIDAD              02421014
                 MOVE NCC0101I(5:4)         TO MSC-CENTRO-ALTA          02422014
                                                                        02423014
                 PERFORM RUTINA-BGECMSC                                 02424014
                    THRU RUTINA-BGECMSC-F                               02425014
              ELSE                                                      02426014
                 INITIALIZE                     BGECMSC                 02427014
                                                                        02428014
                 MOVE '1'                   TO MSC-FUNCION              02429014
                 MOVE NCC0101I(11:2)        TO MSC-CUENTA(1:2)          02430014
                 MOVE NCC0101I(13:8)        TO MSC-CUENTA(3:8)          02431014
                 MOVE NCC0101I(1:4)         TO MSC-ENTIDAD              02432014
                 MOVE NCC0101I(5:4)         TO MSC-CENTRO-ALTA          02433014
                                                                        02434014
                 PERFORM RUTINA-BGECMSC                                 02435014
                    THRU RUTINA-BGECMSC-F                               02436014
                                                                        02437014
                 INITIALIZE                     BGECMSC                 02438014
                                                                        02439014
                 MOVE '1'                   TO MSC-FUNCION              02440014
                 MOVE NC20101I(11:2)        TO MSC-CUENTA(1:2)          02441014
                 MOVE NC20101I(13:8)        TO MSC-CUENTA(3:8)          02442014
                 MOVE NC20101I(1:4)         TO MSC-ENTIDAD              02443014
                 MOVE NC20101I(5:4)         TO MSC-CENTRO-ALTA          02444014
                                                                        02445014
                 PERFORM RUTINA-BGECMSC                                 02446014
                    THRU RUTINA-BGECMSC-F                               02447014
              END-IF                                                    02448014
      *200711038-INI                                                    02449014
           END-IF.                                                      02450014
      *200711038-FIN                                                    02451014
      ******************************************************************02452014
      *****************RUTINA BGECMSC****DAVID  06-11-1998**************02453014
JPC@2 *                                                                *02454014
JPC@2 ******************************************************************02455014
JPC@2 *****************RUTINA SM7CNIN0 ** CAMPAÑAS *********************02456014
JPC@2 ******************************************************************02457014
JPC@2      INITIALIZE                     W-SMWCNIN0                    02458014
JPC@2      MOVE 'I'                   TO  NIN0-OPCION                   02459014
JPC@2      MOVE 'VL00'                TO  NIN0-APLICACION.              02460014
JPC@2      MOVE VARC-CTAVAL20 (01:08) TO  NIN0-CONTRATO (01:08)         02461014
JPC@2      MOVE VARC-CTAVAL20 (11:10) TO  NIN0-CONTRATO (09:10)         02462014
JPC@2      MOVE 'O'                   TO  NIN0-IND-CAPTURA.             02463014
JPC@2      MOVE 'A'                   TO  NIN0-IND-ACTIVO.              02464014
JPC@2      MOVE GVT0101I              TO  NIN0-GEST-VTA.                02465014
JPC@2      MOVE CAV0101I              TO  NIN0-CANAL-VTA.               02466014
JPC@2      MOVE SCV0101I              TO  NIN0-SUBCANAL-VTA.            02467014
JPC@2      MOVE CAM0101I              TO  NIN0-CODCAMP.                 02468014
JPC@2      MOVE CAA-USERID            TO  NIN0-GESTOR-CONT.             02469014
JPC@2 *                                                                 02470014
JPC@2      EXEC CICS                                                    02471014
JPC@2        LINK PROGRAM  (SM7CNIN0)                                   02472014
JPC@2             COMMAREA (SMWCNIN0)                                   02473014
JPC@2      END-EXEC                                                     02474014
JPC@2 *                                                                 02475014
JPC@2      IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         02476014
JPC@2         MOVE 'ERROR EN SM7CNIN0'  TO ABC-REFERENCIA               02477014
JPC@2         MOVE 'SM7CNIN0'           TO ABC-OBJETO-ERROR             02478014
JPC@2         PERFORM 999-ABEND-CICS                                    02479014
JPC@2      END-IF                                                       02480014
JPC@2 *                                                                 02481014
JPC@2      EVALUATE NIN0-CODRETORN                                      02482014
JPC@2          WHEN '00'                                                02483014
JPC@2               CONTINUE                                            02484014
JPC@2          WHEN '10'                                                02485014
JPC@2               MOVE  -1                    TO GVT0101L             02486014
JPC@2               MOVE 'VLE2169'              TO CAA-COD-ERROR        02487014
JPC@2               MOVE 'ERROR GESTOR-VTA/CAN' TO CAA-VAR1-ERROR       02488014
JPC@2               MOVE 'AL/SUBCANAL/CAMPAÑA ' TO CAA-VAR2-ERROR       02489014
JPC@2               PERFORM 3-FINAL                                     02490014
JPC@4          WHEN '80'                                                02491014
JPC@4               MOVE  -1                    TO CAV0101L             02492014
JPC@4               MOVE 'VLE2169'              TO CAA-COD-ERROR        02493014
JPC@4               MOVE 'CANAL DE VENTA NO   ' TO CAA-VAR1-ERROR       02494014
JPC@4               MOVE 'VALIDO              ' TO CAA-VAR2-ERROR       02495014
JPC@4               PERFORM 3-FINAL                                     02496014
JPC@4          WHEN '81'                                                02497014
JPC@4               MOVE  -1                    TO SCV0101L             02498014
JPC@4               MOVE 'VLE2169'              TO CAA-COD-ERROR        02499014
JPC@4               MOVE 'SUBCANAL DE VENTA NO' TO CAA-VAR1-ERROR       02500014
JPC@4               MOVE ' VALIDO             ' TO CAA-VAR2-ERROR       02501014
JPC@4               PERFORM 3-FINAL                                     02502014
JPC@4          WHEN '90'                                                02503014
JPC@4               MOVE  -1                    TO GVT0101L             02504014
JPC@4               MOVE 'VLE2169'              TO CAA-COD-ERROR        02505014
JPC@4               MOVE 'GESTOR DE VENTA NO  ' TO CAA-VAR1-ERROR       02506014
JPC@4               MOVE 'VALIDO              ' TO CAA-VAR2-ERROR       02507014
JPC@4               PERFORM 3-FINAL                                     02508014
JPC@2          WHEN '98'                                                02509014
JPC@2          WHEN '99'                                                02510014
JPC@2               MOVE -1                     TO GVT0101L             02511014
JPC@2               MOVE NIN0-COD-ERROR         TO CAA-COD-ERROR        02512014
JPC@2               MOVE NIN0-VAR1-ERROR        TO CAA-VAR1-ERROR       02513014
JPC@2               MOVE NIN0-VAR2-ERROR        TO CAA-VAR2-ERROR       02514014
JPC@2               PERFORM 3-FINAL                                     02515014
JPC@2          WHEN OTHER                                               02516014
JPC@2               MOVE 'VLA0112'              TO CAA-COD-AVISO1       02517014
JPC@2               MOVE 'NUEVA INFORMACION NO' TO CAA-VAR1-AVISO1      02518014
JPC@2               MOVE 'SATISFACTORIA.RET:@@' TO CAA-VAR2-AVISO1      02519014
JPC@2               MOVE NIN0-CODRETORN         TO CAA-VAR2-AVISO1(19:2)02520014
JPC@2      END-EVALUATE.                                                02521014
JPC@2 *                                                                 02522014
       220-ALTA-NUEVA-FIN. EXIT.                                        02523014
      *                                                                 02524014
      *                                                                 02525014
      *A2011.                                                           02526014
      ******************************************************************02527014
      * CADA VEZ QUE SE DE DE ALTA UN NUEVA CUENTA HAY QUE COMPROBAR QUE02528014
      * EXISTA EL PRODUCTO 1 ( CUSTODIA ) EN LA TABLA DE PRODUCTOS, DES-02529014
      * PUES SE DA DE ALTA UN NUEVO REGISTRO EN LA TABLA DE RELACION    02530014
      * CUENTA VALOR/PRODUCTO CON LA NUEVA CUENTA Y EL PRODUCTO CUSTODIA02531014
      *                                                                 02532014
      ******************************************************************02533014
      *                                                                 02534014
       223-RELACION-PRODUCTO.                                           02535014
                                                                        02536014
           MOVE      1                TO VPRO-PRODUCT                   02537014
                                                                        02538014
           PERFORM 2231-SELECT-PRODUCTO                                 02539014
              THRU 2231-SELECT-PRODUCTO-FIN                             02540014
                                                                        02541014
           INITIALIZE DCLVLDTRPR                                        02542014
      *                                                                 02543014
           MOVE VPRO-PRODUCT        TO VRPR-PRODUCT                     02544014
           MOVE CTA0101O            TO VRPR-CUENTA                      02545014
           MOVE VARC-RUT            TO VRPR-RUT                         02546014
           MOVE VARC-INDSAB         TO VRPR-INDSAB                      02547014
                                                                        02548014
           MOVE CAA-FECHA-OPER      TO  VRPR-FEULMOD                    02549014
                                        VRPR-FEALTREG                   02550014
           MOVE CAA-HORA-TRANS      TO  VRPR-HORULMOD                   02551014
           MOVE CAA-TERMINAL        TO  VRPR-NUMTER                     02552014
           MOVE CAA-USERID          TO  VRPR-USUARIO                    02553014
                                                                        02554014
           PERFORM 2232-INSERT-RELACPRO                                 02555014
              THRU 2232-INSERT-RELACPRO-FIN.                            02556014
                                                                        02557014
       223-RELACION-PRODUCTO-FIN.                                       02558014
           EXIT.                                                        02559014
      ******************************************************************02560014
      *                 2231-SELECT-PRODUCTO                           *02561014
      *  ACCEDER A TABLA PRODUCTOS PARA COMPROBAR QUE EXISTA EL PRODUC *02562014
      *  1 ( CUSTODIA )                                                *02563014
      ******************************************************************02564014
      *                                                                 02565014
       2231-SELECT-PRODUCTO.                                            02566014
      *                                                                 02567014
           EXEC SQL                                                     02568014
                SELECT  VPRO_PRODUCT                                    02569014
                  INTO :VPRO-PRODUCT                                    02570014
                  FROM  VLDTPRO                                         02571014
                 WHERE  VPRO_PRODUCT = :VPRO-PRODUCT                    02572014
           END-EXEC                                                     02573014
      *                                                                 02574014
           MOVE SQLCODE             TO SQLCODE-AUX                      02575014
      *                                                                 02576014
           EVALUATE TRUE                                                02577014
              WHEN DB2-OK                                               02578014
      *A2011-RUTLOG-I                                                   02579014
                   INITIALIZE W-VLWCLOG0                                02580014
                              LOGVLDTPRO                                02581014
                   MOVE 'VLDTPRO'             TO  VL7LOG-TABLA          02582014
                   MOVE 'SELECT'              TO  VL7LOG-OPERACION      02583014
                   MOVE LENGTH OF DCLVLDTPRO  TO  VL7LOG-REGISTRO-LEN   02584014
                   MOVE VPRO-PRODUCT          TO  LPRO-PRODUCT          02585014
                   MOVE LOGVLDTPRO            TO  VL7LOG-REGISTRO-TEXT  02586014
                   PERFORM LLAMAR-VL7CRLOG                              02587014
                      THRU LLAMAR-VL7CRLOG-FIN                          02588014
      *A2011-RUTLOG-F                                                   02589014
                                                                        02590014
              WHEN DB2-NOTFND                                           02591014
                   MOVE  'VLE0901'  TO  CAA-COD-ERROR                   02592014
                   MOVE  'VLDTPRO'  TO  CAA-VAR1-ERROR                  02593014
                   MOVE  -1         TO  CTA0101L                        02594014
                   PERFORM  3-FINAL                                     02595014
              WHEN OTHER                                                02596014
                   MOVE 'SELECT'    TO  ABC-REFERENCIA                  02597014
                   MOVE 'VLDTPRO'   TO  ABC-OBJETO-ERROR                02598014
                   PERFORM 999-ABEND-DB2                                02599014
           END-EVALUATE.                                                02600014
      *                                                                 02601014
       2231-SELECT-PRODUCTO-FIN.                                        02602014
           EXIT.                                                        02603014
                                                                        02604014
      ******************************************************************02605014
      *                 2232-INSERT-RELACPRO                           *02606014
      *  INSERTAR NUEVO REGISTRO EN LA TABLA DE RELAC. PRODUCTOS       *02607014
      ******************************************************************02608014
      *                                                                 02609014
       2232-INSERT-RELACPRO.                                            02610014
      *                                                                 02611014
            EXEC SQL                                                    02612014
                 INSERT INTO VLDTRPR                                    02613014
                 VALUES (:DCLVLDTRPR)                                   02614014
            END-EXEC                                                    02615014
      *                                                                 02616014
            MOVE SQLCODE            TO SQLCODE-AUX                      02617014
      *                                                                 02618014
            EVALUATE TRUE                                               02619014
               WHEN DB2-OK                                              02620014
      *A2011-RUTLOG-I                                                   02621014
                   INITIALIZE W-VLWCLOG0                                02622014
                              LOGVLDTRPR                                02623014
                   MOVE 'VLDTRPR'             TO  VL7LOG-TABLA          02624014
                   MOVE 'INSERT'              TO  VL7LOG-OPERACION      02625014
                   MOVE LENGTH OF DCLVLDTRPR  TO  VL7LOG-REGISTRO-LEN   02626014
                   MOVE DCLVLDTRPR            TO  LOGVLDTRPR            02627014
                   MOVE LOGVLDTRPR            TO  VL7LOG-REGISTRO-TEXT  02628014
                   PERFORM LLAMAR-VL7CRLOG                              02629014
                      THRU LLAMAR-VL7CRLOG-FIN                          02630014
      *A2011-RUTLOG-F                                                   02631014
                                                                        02632014
               WHEN DB2-DUPREC                                          02633014
                    MOVE 'VLE0244'  TO  CAA-COD-ERROR                   02634014
                    MOVE  -1        TO  CTA0101L                        02635014
                    PERFORM  3-FINAL                                    02636014
               WHEN OTHER                                               02637014
                    MOVE 'INSERT'   TO  ABC-REFERENCIA                  02638014
                    MOVE 'VLDTRPR'  TO  ABC-OBJETO-ERROR                02639014
                    PERFORM 999-ABEND-DB2                               02640014
           END-EVALUATE.                                                02641014
      *                                                                 02642014
       2232-INSERT-RELACPRO-FIN.                                        02643014
           EXIT.                                                        02644014
      ******************************************************************02645014
      *                 2233-SELECT-CUST-INTERNA                       *02646014
      *  ACCEDER A TABLA CUSTODIOS INTERNACIONAL                       *02647014
      ******************************************************************02648014
      *                                                                 02649014
       2233-SELECT-CUST-INTER.                                          02650014
      *                                                                *02651014
           MOVE 1            TO  VXAG-TIPINT.                           02652014
           MOVE CIN0101I     TO  W-CUSTINT.                             02653014
           MOVE W-CUSTINT-N  TO  VXAG-INTERV.                           02654014
      *                                                                *02655014
           EXEC SQL                                                     02656014
                SELECT  VXAG_NOMB_AGE                                   02657014
                  INTO :VXAG-NOMB-AGE                                   02658014
                  FROM  VLDTXAG                                         02659014
                 WHERE  VXAG_TIPINT = :VXAG-TIPINT                      02660014
                   AND  VXAG_INTERV = :VXAG-INTERV                      02661014
           END-EXEC                                                     02662014
      *                                                                *02663014
           MOVE SQLCODE             TO SQLCODE-AUX                      02664014
      *                                                                *02665014
           EVALUATE TRUE                                                02666014
               WHEN DB2-OK                                              02667014
                    MOVE  VXAG-NOMB-AGE TO NCU0101O                     02668014
               WHEN DB2-NOTFND                                          02669014
                    MOVE  'NO EXIST CUSTODIO' TO NCU0101O               02670014
               WHEN OTHER                                               02671014
                    MOVE -1          TO  CIN0101L                       02672014
                    MOVE 'SELECT'    TO  ABC-REFERENCIA                 02673014
                    MOVE 'VLDTXAG'   TO  ABC-OBJETO-ERROR               02674014
                    PERFORM 999-ABEND-DB2                               02675014
           END-EVALUATE.                                                02676014
      *                                                                *02677014
       2233-SELECT-CUST-INTER-FIN.                                      02678014
           EXIT.                                                        02679014
      *                                                                 02680014
       23-MODIFICACION.                                                 02681014
      *                                                                 02682014
      *200711038-INI                                                    02683014
           IF CAA-CENTRO-CONT = '0542' AND                              02684014
              REG0101I        = 'S'                                     02685014
              PERFORM CUENTA-REGISTRO                                   02686014
                 THRU CUENTA-REGISTRO-FIN                               02687014
JPC@4         MOVE CAA-CENTRO-CONT TO SUC0101I                          02688014
JPC@4         MOVE '99'            TO TAF0101I                          02689014
           END-IF                                                       02690014
      *200711038-FIN                                                    02691014
      *                                                                 02692014
           MOVE CTA0101-COMM-N  TO VARC-CUENTA                          02693014
      *                                                                 02694014
           PERFORM LEER-VLDTARC-MOD                                     02695014
              THRU LEER-VLDTARC-MOD-FIN                                 02696014
      *                                                                 02697014
      *A2011-RUTLOG-I                                                   02698014
                                                                        02699014
           MOVE VARC-CUENTA             TO LARC-CUENTA                  02700014
           PERFORM SELUND-VLDTARC                                       02701014
              THRU SELUND-VLDTARC-FIN                                   02702014
                                                                        02703014
      *A2011-RUTLOG-F                                                   02704014
                                                                        02705014
           PERFORM VALIDAR-CAMPOS                                       02706014
              THRU VALIDAR-CAMPOS-FIN                                   02707014
      *                                                                 02708014
      *200808196-INI                                                    02709014
           MOVE END0101I   TO   WA-CTA-091 (01:04).                     02710014
           MOVE CEN0101I   TO   WA-CTA-091 (05:04).                     02711014
           MOVE '00'       TO   WA-CTA-091 (09:02).                     02712014
           MOVE PRD0101I   TO   WA-CTA-091 (11:02).                     02713014
           MOVE CTA0101I   TO   WA-CTA-091 (13:07).                     02714014
           MOVE DG20101I   TO   WA-CTA-091 (20:01).                     02715014
           IF NCC0101I (11:02)   = '91' OR                              02716014
              NC20101I (11:02)   = '91'                                 02717014
              IF WA-CTA-091 NOT = NCC0101I                              02718014
                 MOVE  -1                    TO NCC0101L                02719014
                 MOVE 'VLE2169'              TO CAA-COD-ERROR           02720014
                 MOVE 'CTA-REGISTRO DEBE SE' TO CAA-VAR1-ERROR          02721014
                 MOVE 'R IGUAL A CTA-VALOR ' TO CAA-VAR2-ERROR          02722014
                 PERFORM 3-FINAL                                        02723014
              END-IF                                                    02724014
              IF WA-CTA-091 NOT = NC20101I                              02725014
                 MOVE  -1                    TO NC20101L                02726014
                 MOVE 'VLE2169'              TO CAA-COD-ERROR           02727014
                 MOVE 'CTA-REGISTRO DEBE SE' TO CAA-VAR1-ERROR          02728014
                 MOVE 'R IGUAL A CTA-VALOR ' TO CAA-VAR2-ERROR          02729014
                 PERFORM 3-FINAL                                        02730014
              END-IF                                                    02731014
           END-IF.                                                      02732014
      *200808196-FIN                                                    02733014
                                                                        02734014
      *A2012-I                                                          02735014
      *200712034-INI                                                    02736014
      *    IF MAN0101I NOT = VARC-NUMMAN                                02737014
           IF MAN0101I NOT = VARC-GRUPO-CTAS                            02738014
              IF CAA-CENTRO-CONT = '0567'                               02739014
                 PERFORM ACCEDER-VLDTADT                                02740014
                    THRU ACCEDER-VLDTADT-FIN                            02741014
                 IF MAN0101I NOT < W-COUNT                              02742014
      *             MOVE MAN0101I          TO VARC-NUMMAN               02743014
                    MOVE MAN0101I          TO VARC-GRUPO-CTAS           02744014
                 ELSE                                                   02745014
      *             MOVE VARC-NUMMAN       TO MAN0101I                  02746014
                    MOVE VARC-GRUPO-CTAS   TO MAN0101I                  02747014
                    MOVE  -1               TO MAN0101L                  02748014
                    MOVE 'VLE1652'         TO CAA-COD-ERROR             02749014
                    PERFORM 3-FINAL                                     02750014
                 END-IF                                                 02751014
              ELSE                                                      02752014
      *          MOVE VARC-NUMMAN          TO MAN0101I                  02753014
                 MOVE VARC-GRUPO-CTAS      TO MAN0101I                  02754014
      *200712034-FIN                                                    02755014
                 MOVE  -1                  TO MAN0101L                  02756014
                 MOVE 'VLE1652'            TO CAA-COD-ERROR             02757014
                 PERFORM 3-FINAL                                        02758014
              END-IF                                                    02759014
           END-IF                                                       02760014
      *A2012-I                                                          02761014
JPC@4      MOVE SUC0101I        TO W-SUCVAL                             02762014
JPC@4      IF ENT0101I = '0069' OR '2010'                               02763014
JPC@4         IF (CTAGLOB-COMM  = NCC0101-COMM)  AND                    02764014
JPC@4            (CTAGLOB-COMM  = NC20101-COMM)  AND                    02765014
JPC@4            ((NCC0101I NOT = NCC0101-COMM)                         02766014
JPC@4         OR  (NC20101I NOT = NC20101-COMM)) AND                    02767014
JIP@4            (W-SUCVAL      = VARC-SUCURS)                          02768014
                 MOVE  -1                  TO SUC0101L                  02769014
                 MOVE 'VLE2257'            TO CAA-COD-ERROR             02770014
                 PERFORM 3-FINAL                                        02771014
JPC@4         END-IF                                                    02772014
JPC@4      END-IF                                                       02773014
           MOVE ENT0101I        TO W-ENTIDAD                            02774014
           MOVE W-ENTIDAD       TO VARC-CENTAD                          02775014
           MOVE TIT0101I        TO W-TITULAR                            02776014
           MOVE W-TITULAR       TO VARC-NUMCLI                          02777014
      *    MOVE ZEROES          TO VARC-CTACAR                          02778014
      *    MOVE ZEROES          TO VARC-CTAABO                          02779014
           MOVE VARC-FILLER     TO VLWCCTA0                             02780014
           MOVE NCC0101I        TO W-CCC-CAR                            02781014
           MOVE NC20101I        TO W-CCC-ABO                            02782014
           MOVE VLWCCTA0        TO VARC-FILLER                          02783014
      *                                                                 02784014
JPC@4 *    MOVE SUC0101I        TO W-SUCVAL                             02785014
JIPC  *    MOVE W-SUCVAL        TO VARC-SUCURS                          02786014
           PERFORM VALIDAR-CENTRO                                       02787014
              THRU VALIDAR-CENTRO-FIN                                   02788014
      ***                                                            ***02789014
      * SE GUARDA LA OFICINA ANTERIOR PARA CONTABILIZAR LOS SALDOS Y   *02790014
      * BLOQUEOS PARA LA NUEVA OFICINA PROPIETARIA.                     02791014
      ***  JIPC                                         18-12-2000   ***02792014
JIPC       IF W-SUCVAL NOT = VARC-SUCURS                                02793014
JIPC          MOVE VARC-SUCURS     TO VARC-CNAE                         02794014
JIPC          MOVE CAA-FECHA-OPER  TO VARC-FE-CARTERA                   02795014
JIPC       END-IF                                                       02796014
      *                                                                 02797014
JIPC       MOVE W-SUCVAL        TO VARC-SUCURS                          02798014
      *                                                                 02799014
           MOVE IDI0101I        TO VARC-TEXTO                           02800014
           MOVE DCO0101I        TO DCO0101-N                            02801014
           MOVE DCO0101-N       TO VARC-NUMDOM                          02802014
           MOVE TCL0101I        TO VARC-VALEXTRJ                        02803014
      *                                                                 02804014
           MOVE VARC-RUT        TO CSU0101I                             02805014
           MOVE VARC-INDSAB     TO ODI0101I                             02806014
      *                                                                 02807014
           MOVE PAI0101I        TO VARC-PAIS(1:3)                       02808014
           MOVE TAF0101I        TO W-TARIFA                             02809014
           MOVE W-TARIFA        TO VARC-INVERSOR                        02810014
           MOVE SOT0101I        TO VARC-SWIFT-TELEX                     02811014
      *                                                                 02812014
           MOVE TEL0101I        TO CLA-TELEX-AUX                        02813014
           MOVE CLTELEX-AUX     TO VARC-CLTELEX                         02814014
           MOVE TELEX2-AUX      TO VARC-TELEX-2                         02815014
      *                                                                 02816014
           MOVE CVE0101I        TO VARC-EXEN1                           02817014
           MOVE ZEROS           TO VARC-EXEN2                           02818014
           MOVE ZEROS           TO VARC-EXEN3                           02819014
           MOVE 100             TO VARC-EXEN4                           02820014
           MOVE DCU0101I        TO VARC-EXEN5                           02821014
           MOVE SUS0101I        TO VARC-EXEN6                           02822014
           MOVE DIV0101I        TO VARC-EXEN7                           02823014
           MOVE AMO0101I        TO VARC-EXEN8                           02824014
           MOVE PAJ0101I        TO VARC-EXEN9                           02825014
           MOVE MCV0101I        TO VARC-MAX-CVE-1                       02826014
           MOVE MPJ0101I        TO VARC-MAX-PAJ-9                       02827014
           MOVE MDC0101I        TO VARC-MAX-DCU-5                       02828014
           MOVE MDI0101I        TO VARC-MAX-DIV-7                       02829014
           MOVE MSU0101I        TO VARC-MAX-SUS-6                       02830014
           MOVE MAM0101I        TO VARC-MAX-AMO-8                       02831014
      *200503172-INI                                                    02832014
           MOVE CIN0101I        TO VARC-CODSUS                          02833014
      *200503172-FIN                                                    02834014
      *LIMA-24-04-1999.SE QUITA EL CAMPO CORREO                         02835014
      *    IF CCO0101I = 'S'                                            02836014
      *        MOVE   0         TO VARC-EXEN10                          02837014
      *    ELSE                                                         02838014
      *        MOVE 100         TO VARC-EXEN10                          02839014
      *    END-IF.                                                      02840014
      **                                                                02841014
           MOVE   0         TO VARC-EXEN10                              02842014
      **                                                                02843014
      *LIMA-24-04-1999.SE QUITA EL CAMPO CORREO                         02844014
      *                                                                 02845014
           IF MDA0101I = SPACES                                         02846014
               MOVE  -1                  TO MDA0101L                    02847014
               MOVE 'VLE1614'            TO CAA-COD-ERROR               02848014
               PERFORM 3-FINAL                                          02849014
           ELSE                                                         02850014
              IF WXMI-TIPCUST = 'I' AND MDA0101I = 'PEN'                02851014
                 MOVE  -1                  TO MDA0101L                  02852014
      * CUANDO CUSTODIA INTERNACIONAL LA MONEDA NO PUEDE SER SOLES      02853014
                 MOVE 'VLE1894'            TO CAA-COD-ERROR             02854014
                 PERFORM 3-FINAL                                        02855014
              END-IF                                                    02856014
              INITIALIZE                  TCWC1200                      02857014
              MOVE MDA0101I          TO   W120-CDDIVISS                 02858014
              PERFORM OBTENER-MONEDA                                    02859014
                 THRU OBTENER-MONEDA-FIN                                02860014
           END-IF                                                       02861014
      *                                                                 02862014
           MOVE MDA0101I                TO VARC-MONEDA                  02863014
      *                                                                 02864014
           MOVE CAA-FECHA-OPER          TO W-FECHA-AMD                  02865014
           MOVE W-DD-AMD                TO W-DD-DMA-G                   02866014
           MOVE W-MM-AMD                TO W-MM-DMA-G                   02867014
           MOVE W-AA-AMD                TO W-AA-DMA-G                   02868014
           MOVE W-FECHA-DMA-G           TO FUA0101O                     02869014
      *200503172-INI                                                    02870014
           MOVE W-FECHA-DMA-G           TO ALT0101O                     02871014
           MOVE CAA-HORA-TRANS          TO HUM0101O                     02872014
           MOVE CAA-USERID              TO USU0101O                     02873014
      *200503172-FIN                                                    02874014
      *                                                                 02875014
      * SE REALIZAN LAS SIGUIENTES MODIFICACIONES DE LAS CUENTAS VALOR  02876014
      * APERTURDAS CON LAS CTAS ECONOMICAS DEL CUSTODIO (BATCH O MANUAL)02877014
      *                                                 JIPC 26-10-2000 02878014
           IF (VARC-GRUPO =   1  OR   3 )    AND                        02879014
              (TAF0101I   = '51' OR '99')    AND                        02880014
              (OPERA-BOLSA)                  AND                        02881014
              (W-CTA-ABO-JUR NOT = NC20101I) AND                        02882014
              (W-CTA-CAR-JUR NOT = NCC0101I)                            02883014
              IF VARC-GRUPO = 1                                         02884014
                 MOVE 2           TO  VARC-GRUPO                        02885014
              END-IF                                                    02886014
              IF VARC-GRUPO = 3                                         02887014
                 MOVE 4           TO  VARC-GRUPO                        02888014
              END-IF                                                    02889014
           END-IF                                                       02890014
           MOVE CAA-FECHA-OPER    TO  VARC-FEULMOD                      02891014
           MOVE CAA-HORA-TRANS    TO  VARC-HORULMOD                     02892014
           MOVE CAA-TERMINAL      TO  VARC-NUMTER                       02893014
           MOVE CAA-USERID        TO  VARC-USUARIO                      02894014
      *                                                                 02895014
           EXEC SQL                                                     02896014
                UPDATE VLDTARC                                          02897014
                   SET VARC_CENTAD      = :VARC-CENTAD                  02898014
                     , VARC_NUMCLI      = :VARC-NUMCLI                  02899014
                     , VARC_MONEDA      = :VARC-MONEDA                  02900014
                     , VARC_SUCURS      = :VARC-SUCURS                  02901014
                     , VARC_CTACAR      = :VARC-CTACAR                  02902014
                     , VARC_CTAABO      = :VARC-CTAABO                  02903014
                     , VARC_EXEN1       = :VARC-EXEN1                   02904014
                     , VARC_EXEN2       = :VARC-EXEN2                   02905014
                     , VARC_EXEN3       = :VARC-EXEN3                   02906014
                     , VARC_EXEN4       = :VARC-EXEN4                   02907014
                     , VARC_EXEN5       = :VARC-EXEN5                   02908014
                     , VARC_EXEN6       = :VARC-EXEN6                   02909014
                     , VARC_EXEN7       = :VARC-EXEN7                   02910014
                     , VARC_EXEN8       = :VARC-EXEN8                   02911014
                     , VARC_EXEN9       = :VARC-EXEN9                   02912014
                     , VARC_EXEN10      = :VARC-EXEN10                  02913014
                     , VARC_MAX_CVE_1   = :VARC-MAX-CVE-1               02914014
                     , VARC_MAX_PAJ_9   = :VARC-MAX-PAJ-9               02915014
                     , VARC_MAX_DCU_5   = :VARC-MAX-DCU-5               02916014
                     , VARC_MAX_DIV_7   = :VARC-MAX-DIV-7               02917014
                     , VARC_MAX_SUS_6   = :VARC-MAX-SUS-6               02918014
                     , VARC_MAX_AMO_8   = :VARC-MAX-AMO-8               02919014
                     , VARC_RUT         = :VARC-RUT                     02920014
                     , VARC_INDSAB      = :VARC-INDSAB                  02921014
                     , VARC_CODSUS      = :VARC-CODSUS                  02922014
                     , VARC_PAIS        = :VARC-PAIS                    02923014
                     , VARC_VALEXTRJ    = :VARC-VALEXTRJ                02924014
                     , VARC_CLTELEX     = :VARC-CLTELEX                 02925014
                     , VARC_TELEX_2     = :VARC-TELEX-2                 02926014
                     , VARC_NUMDOM      = :VARC-NUMDOM                  02927014
                     , VARC_SWIFT_TELEX = :VARC-SWIFT-TELEX             02928014
                     , VARC_INVERSOR    = :VARC-INVERSOR                02929014
                     , VARC_TEXTO       = :VARC-TEXTO                   02930014
                     , VARC_FEULMOD     = :VARC-FEULMOD                 02931014
                     , VARC_HORULMOD    = :VARC-HORULMOD                02932014
                     , VARC_NUMTER      = :VARC-NUMTER                  02933014
                     , VARC_USUARIO     = :VARC-USUARIO                 02934014
                     , VARC_FILLER      = :VARC-FILLER                  02935014
JIPC                 , VARC_GRUPO       = :VARC-GRUPO                   02936014
JIPC                 , VARC_CNAE        = :VARC-CNAE                    02937014
JIPC                 , VARC_FE_CARTERA  = :VARC-FE-CARTERA              02938014
      *@ZAL-INI                                                         02939014
JIPC  *              , VARC_NUMMAN      = :VARC-NUMMAN                  02940014
                     , VARC_GRUPO_CTAS  = :VARC-GRUPO-CTAS              02941014
      *@ZAL-FIN                                                         02942014
                 WHERE VARC_CUENTA = :VARC-CUENTA                       02943014
           END-EXEC                                                     02944014
      *                                                                 02945014
           MOVE SQLCODE TO SQLCODE-AUX                                  02946014
      *                                                                 02947014
           EVALUATE TRUE                                                02948014
              WHEN DB2-OK                                               02949014
                   INITIALIZE W-VLWCLOG0                                02950014
                              LOGVLDTARC                                02951014
                   MOVE 'VLDTARC'             TO  VL7LOG-TABLA          02952014
                   MOVE 'UPDATE'              TO  VL7LOG-OPERACION      02953014
                   MOVE LENGTH OF DCLVLDTARC  TO  VL7LOG-REGISTRO-LEN   02954014
                   MOVE VARC-CENTAD           TO  LARC-CENTAD           02955014
                   MOVE VARC-NUMCLI           TO  LARC-NUMCLI           02956014
                   MOVE VARC-MONEDA           TO  LARC-MONEDA           02957014
                   MOVE VARC-SUCURS           TO  LARC-SUCURS           02958014
                   MOVE VARC-CTACAR           TO  LARC-CTACAR           02959014
                   MOVE VARC-CTAABO           TO  LARC-CTAABO           02960014
                   MOVE VARC-EXEN1            TO  LARC-EXEN1            02961014
                   MOVE VARC-EXEN2            TO  LARC-EXEN2            02962014
                   MOVE VARC-EXEN3            TO  LARC-EXEN3            02963014
                   MOVE VARC-EXEN4            TO  LARC-EXEN4            02964014
                   MOVE VARC-EXEN5            TO  LARC-EXEN5            02965014
                   MOVE VARC-EXEN6            TO  LARC-EXEN6            02966014
                   MOVE VARC-EXEN7            TO  LARC-EXEN7            02967014
                   MOVE VARC-EXEN8            TO  LARC-EXEN8            02968014
                   MOVE VARC-EXEN9            TO  LARC-EXEN9            02969014
                   MOVE VARC-EXEN10           TO  LARC-EXEN10           02970014
                   MOVE VARC-MAX-CVE-1        TO  LARC-MAX-CVE-1        02971014
                   MOVE VARC-MAX-PAJ-9        TO  LARC-MAX-PAJ-9        02972014
                   MOVE VARC-MAX-DCU-5        TO  LARC-MAX-DCU-5        02973014
                   MOVE VARC-MAX-DIV-7        TO  LARC-MAX-DIV-7        02974014
                   MOVE VARC-MAX-SUS-6        TO  LARC-MAX-SUS-6        02975014
                   MOVE VARC-MAX-AMO-8        TO  LARC-MAX-AMO-8        02976014
                   MOVE VARC-RUT              TO  LARC-RUT              02977014
                   MOVE VARC-INDSAB           TO  LARC-INDSAB           02978014
                   MOVE VARC-CODSUS           TO  LARC-CODSUS           02979014
                   MOVE VARC-PAIS             TO  LARC-PAIS             02980014
                   MOVE VARC-VALEXTRJ         TO  LARC-VALEXTRJ         02981014
                   MOVE VARC-MONEDA           TO  LARC-MONEDA           02982014
                   MOVE VARC-CLTELEX          TO  LARC-CLTELEX          02983014
                   MOVE VARC-TELEX-2          TO  LARC-TELEX-2          02984014
                   MOVE VARC-NUMDOM           TO  LARC-NUMDOM           02985014
                   MOVE VARC-SWIFT-TELEX      TO  LARC-SWIFT-TELEX      02986014
                   MOVE VARC-INVERSOR         TO  LARC-INVERSOR         02987014
                   MOVE VARC-TEXTO            TO  LARC-TEXTO            02988014
                   MOVE VARC-FEULMOD          TO  LARC-FEULMOD          02989014
                   MOVE VARC-HORULMOD         TO  LARC-HORULMOD         02990014
                   MOVE VARC-NUMTER           TO  LARC-NUMTER           02991014
                   MOVE VARC-USUARIO          TO  LARC-USUARIO          02992014
                   MOVE VARC-FILLER           TO  LARC-FILLER           02993014
                   MOVE VARC-CUENTA           TO  LARC-CUENTA           02994014
                   MOVE LOGVLDTARC            TO  VL7LOG-REGISTRO-TEXT  02995014
                   PERFORM LLAMAR-VL7CRLOG                              02996014
                      THRU LLAMAR-VL7CRLOG-FIN                          02997014
      *A2011-RUTLOG-F                                                   02998014
      *                                                                 02999014
              WHEN OTHER                                                03000014
                   MOVE 'UPDATE-MOD'  TO  ABC-REFERENCIA                03001014
                   MOVE 'VLDTARC'     TO  ABC-OBJETO-ERROR              03002014
                   PERFORM 999-ABEND-DB2                                03003014
      *                                                                 03004014
           END-EVALUATE.                                                03005014
      *                                                                 03006014
      **************************************************************    03007014
      *****************RUTINA BGECMSC****DAVID  06-11-1998**********    03008014
      * RUTINA PARA DAR ALTA O BAJA LA VINCULACION DE LA CTA       *    03009014
      * ECONOMICA CON LA CTA. VALOR. IMPIDE O PERMITE LA BAJA DE   *    03010014
      * LA CTA. ECONOMICA VINCULADA                                *    03011014
      **************************************************************    03012014
      **************************************************************    03013014
      **************************************************************    03014014
                                                                        03015014
      *- SE HA MODIFICADO ALGUNA DE LAS CUENTAS ECONOMICAS??            03016014
                                                                        03017014
           IF NCC0101I NOT = NCC0101-COMM                               03018014
      *- HAN MODIFICADO LA CTA. CARGO                                   03019014
              IF NCC0101I = NC20101-COMM                                03020014
      *- LA NUEVA CTA. CARGO ES IGUAL A LA ANTIGUA CTA. ABONO. YA       03021014
      *- EXISTE LA RELACION                                             03022014
                 CONTINUE                                               03023014
              ELSE                                                      03024014
                 INITIALIZE                     BGECMSC                 03025014
                                                                        03026014
                 MOVE '1'                   TO MSC-FUNCION              03027014
                 MOVE NCC0101I(11:2)        TO MSC-CUENTA(1:2)          03028014
                 MOVE NCC0101I(13:8)        TO MSC-CUENTA(3:8)          03029014
                 MOVE NCC0101I(1:4)         TO MSC-ENTIDAD              03030014
                 MOVE NCC0101I(5:4)         TO MSC-CENTRO-ALTA          03031014
                 PERFORM RUTINA-BGECMSC                                 03032014
                    THRU RUTINA-BGECMSC-F                               03033014
              END-IF                                                    03034014
              IF NCC0101-COMM NOT = NC20101I                            03035014
      *- LA ANTIGUA CTA. CARGO NO ES LA NUEVA ABONO POR LO QUE LA       03036014
      *- RELACION DEBE DESHACERSE                                       03037014
                 INITIALIZE                     BGECMSC                 03038014
                                                                        03039014
                 MOVE '2'                   TO MSC-FUNCION              03040014
                 MOVE NCC0101-COMM(11:2)    TO MSC-CUENTA(1:2)          03041014
                 MOVE NCC0101-COMM(13:8)    TO MSC-CUENTA(3:8)          03042014
                 MOVE NCC0101-COMM(1:4)     TO MSC-ENTIDAD              03043014
                 MOVE NCC0101-COMM(5:4)     TO MSC-CENTRO-ALTA          03044014
                                                                        03045014
                 PERFORM RUTINA-BGECMSC                                 03046014
                    THRU RUTINA-BGECMSC-F                               03047014
              END-IF                                                    03048014
           END-IF                                                       03049014
           IF NC20101I NOT = NC20101-COMM                               03050014
      *- HAN MODIFICADO LA CTA. ABONO                                   03051014
              IF NC20101-COMM NOT = NCC0101I                            03052014
      *- LA ANTIGUA CTA. ABONO NO APARECE COMO NUEVA CTA. CARGO POR     03053014
      *- LO QUE LA RELACION DEBE DESHACERSE                             03054014
                 INITIALIZE                     BGECMSC                 03055014
                                                                        03056014
                 MOVE '2'                   TO MSC-FUNCION              03057014
                 MOVE NC20101-COMM(11:2)        TO MSC-CUENTA(1:2)      03058014
                 MOVE NC20101-COMM(13:8)        TO MSC-CUENTA(3:8)      03059014
                 MOVE NC20101-COMM(1:4)         TO MSC-ENTIDAD          03060014
                 MOVE NC20101-COMM(5:4)         TO MSC-CENTRO-ALTA      03061014
                 PERFORM RUTINA-BGECMSC                                 03062014
                    THRU RUTINA-BGECMSC-F                               03063014
              END-IF                                                    03064014
              IF NC20101I NOT = NCC0101I AND                            03065014
                 NC20101I NOT = NCC0101-COMM                            03066014
      *- LA NUEVA CTA. ABONO NO EXISTE NI EXISTIO, DEBE DARSE DE ALTA   03067014
      *- LA RELACION                                                    03068014
                 INITIALIZE                     BGECMSC                 03069014
                                                                        03070014
                 MOVE '1'                   TO MSC-FUNCION              03071014
                 MOVE NC20101I(11:2)        TO MSC-CUENTA(1:2)          03072014
                 MOVE NC20101I(13:8)        TO MSC-CUENTA(3:8)          03073014
                 MOVE NC20101I(1:4)         TO MSC-ENTIDAD              03074014
                 MOVE NC20101I(5:4)         TO MSC-CENTRO-ALTA          03075014
                 PERFORM RUTINA-BGECMSC                                 03076014
                    THRU RUTINA-BGECMSC-F                               03077014
              END-IF                                                    03078014
           END-IF                                                       03079014
      *                                                                 03080014
      **************************************************************    03081014
      *****************RUTINA BGECMSC****DAVID  06-11-1998**********    03082014
      **************************************************************    03083014
      *                                                                 03084014
      *                                                                 03085014
      *A2012-I.                                                         03086014
           IF WXMI-IMPALT = 'S' AND VARC-INDIMP = 'S'                   03087014
      *200702146-INI                                                    03088014
      *       PERFORM 999-TRATAR-JETFORM                                03089014
      *          THRU 999-TRATAR-JETFORM-FIN                            03090014
              IF ENT0101I = '0069' OR '2010'                            03091014
                 PERFORM 069-TRATAR-JETFORM                             03092014
                    THRU 069-TRATAR-JETFORM-FIN                         03093014
              ELSE                                                      03094014
                 PERFORM 999-TRATAR-JETFORM                             03095014
                    THRU 999-TRATAR-JETFORM-FIN                         03096014
              END-IF                                                    03097014
      *200702146-FIN                                                    03098014
              PERFORM 999-GRABAR-JETFORM                                03099014
                 THRU 999-GRABAR-JETFORM-FIN                            03100014
      *                                                                 03101014
              MOVE SPACES            TO OPT-COMM                        03102014
              MOVE SPACES            TO MSB-COMM                        03103014
              MOVE SPACES            TO CTA0101-COMM                    03104014
              MOVE 'VLA0069'         TO CAA-COD-AVISO1                  03105014
              MOVE -1                TO CTA0101L                        03106014
      *A2012-F.                                                         03107014
      *                                                                 03108014
           ELSE                                                         03109014
              MOVE SPACES            TO OPT-COMM                        03110014
              MOVE SPACES            TO MSB-COMM                        03111014
              MOVE SPACES            TO CTA0101-COMM                    03112014
              MOVE 'VLA0014'         TO CAA-COD-AVISO1                  03113014
              MOVE -1                TO CTA0101L                        03114014
           END-IF.                                                      03115014
      *                                                                 03116014
       23-MODIFICACION-FIN.  EXIT.                                      03117014
      *                                                                 03118014
       25-INACTIVAR.                                                    03119014
      *                                                                 03120014
           MOVE CTA0101-COMM-N TO VARC-CUENTA                           03121014
      *                                                                 03122014
JPC@3      PERFORM VALIDA-CTA-PORTAFOLIO.                               03123014
      *                                                                 03124014
JPC@1 *    EXEC SQL                                                     03125014
JPC@1 *         SELECT  *                                               03126014
JPC@1 *           INTO :DCLVLDTARC                                      03127014
JPC@1 *           FROM  VLDTARC                                         03128014
JPC@1 *          WHERE  VARC_CUENTA  = :VARC-CUENTA                     03129014
JPC@1 *    END-EXEC                                                     03130014
           EXEC SQL                                                     03131014
                SELECT  VARC_CUENTA                                     03132014
                     ,  VARC_CENTAD                                     03133014
                     ,  VARC_NUMCLI                                     03134014
                     ,  VARC_CLMAST                                     03135014
                     ,  VARC_MONEDA                                     03136014
                     ,  VARC_SUCURS                                     03137014
                     ,  VARC_CTACAR                                     03138014
                     ,  VARC_CTAABO                                     03139014
                     ,  VARC_TEXTO                                      03140014
                     ,  VARC_PRESEN                                     03141014
                     ,  VARC_GRUPO                                      03142014
                     ,  VARC_RUT                                        03143014
                     ,  VARC_CNAE                                       03144014
                     ,  VARC_SITUAC                                     03145014
                     ,  VARC_EXEN1                                      03146014
                     ,  VARC_EXEN2                                      03147014
                     ,  VARC_EXEN3                                      03148014
                     ,  VARC_EXEN4                                      03149014
                     ,  VARC_EXEN5                                      03150014
                     ,  VARC_EXEN6                                      03151014
                     ,  VARC_EXEN7                                      03152014
                     ,  VARC_EXEN8                                      03153014
                     ,  VARC_EXEN9                                      03154014
                     ,  VARC_EXEN10                                     03155014
                     ,  VARC_ANALIS                                     03156014
                     ,  VARC_CLACARGO                                   03157014
                     ,  VARC_CLABONO                                    03158014
                     ,  VARC_NUMDOM                                     03159014
                     ,  VARC_CODSUS                                     03160014
                     ,  VARC_FE_ULT_EXT                                 03161014
                     ,  VARC_PAIS                                       03162014
                     ,  VARC_FE_CARTERA                                 03163014
                     ,  VARC_CLTELEX                                    03164014
                     ,  VARC_FE_ALTA                                    03165014
                     ,  VARC_VALORACION                                 03166014
                     ,  VARC_VALEXTRJ                                   03167014
                     ,  VARC_INVERSOR                                   03168014
                     ,  VARC_DIRECTA                                    03169014
                     ,  VARC_MAX_CVE_1                                  03170014
                     ,  VARC_MAX_DCU_5                                  03171014
                     ,  VARC_MAX_SUS_6                                  03172014
                     ,  VARC_MAX_DIV_7                                  03173014
                     ,  VARC_MAX_AMO_8                                  03174014
                     ,  VARC_MAX_PAJ_9                                  03175014
                     ,  VARC_FECHA_102                                  03176014
                     ,  VARC_TARIFACUS                                  03177014
                     ,  VARC_SWIFT_TELEX                                03178014
                     ,  VARC_TELEX_2                                    03179014
                     ,  VARC_GRUPO_CTAS                                 03180014
                     ,  VARC_OPER_TIT                                   03181014
                     ,  VARC_FEALTREG                                   03182014
                     ,  VARC_FEULMOD                                    03183014
                     ,  VARC_HORULMOD                                   03184014
                     ,  VARC_NUMTER                                     03185014
                     ,  VARC_USUARIO                                    03186014
                     ,  VARC_FILLER                                     03187014
                     ,  VARC_CTAVAL20                                   03188014
      *@ZAL-INI                                                         03189014
      *              ,  VARC_NUMMAN                                     03190014
                     ,  VARC_GRUPO_CTAS                                 03191014
      *@ZAL-FIN                                                         03192014
                     ,  VARC_INDIMP                                     03193014
                     ,  VARC_INDSAB                                     03194014
                  INTO :VARC-CUENTA                                     03195014
                     , :VARC-CENTAD                                     03196014
                     , :VARC-NUMCLI                                     03197014
                     , :VARC-CLMAST                                     03198014
                     , :VARC-MONEDA                                     03199014
                     , :VARC-SUCURS                                     03200014
                     , :VARC-CTACAR                                     03201014
                     , :VARC-CTAABO                                     03202014
                     , :VARC-TEXTO                                      03203014
                     , :VARC-PRESEN                                     03204014
                     , :VARC-GRUPO                                      03205014
                     , :VARC-RUT                                        03206014
                     , :VARC-CNAE                                       03207014
                     , :VARC-SITUAC                                     03208014
                     , :VARC-EXEN1                                      03209014
                     , :VARC-EXEN2                                      03210014
                     , :VARC-EXEN3                                      03211014
                     , :VARC-EXEN4                                      03212014
                     , :VARC-EXEN5                                      03213014
                     , :VARC-EXEN6                                      03214014
                     , :VARC-EXEN7                                      03215014
                     , :VARC-EXEN8                                      03216014
                     , :VARC-EXEN9                                      03217014
                     , :VARC-EXEN10                                     03218014
                     , :VARC-ANALIS                                     03219014
                     , :VARC-CLACARGO                                   03220014
                     , :VARC-CLABONO                                    03221014
                     , :VARC-NUMDOM                                     03222014
                     , :VARC-CODSUS                                     03223014
                     , :VARC-FE-ULT-EXT                                 03224014
                     , :VARC-PAIS                                       03225014
                     , :VARC-FE-CARTERA                                 03226014
                     , :VARC-CLTELEX                                    03227014
                     , :VARC-FE-ALTA                                    03228014
                     , :VARC-VALORACION                                 03229014
                     , :VARC-VALEXTRJ                                   03230014
                     , :VARC-INVERSOR                                   03231014
                     , :VARC-DIRECTA                                    03232014
                     , :VARC-MAX-CVE-1                                  03233014
                     , :VARC-MAX-DCU-5                                  03234014
                     , :VARC-MAX-SUS-6                                  03235014
                     , :VARC-MAX-DIV-7                                  03236014
                     , :VARC-MAX-AMO-8                                  03237014
                     , :VARC-MAX-PAJ-9                                  03238014
                     , :VARC-FECHA-102                                  03239014
                     , :VARC-TARIFACUS                                  03240014
                     , :VARC-SWIFT-TELEX                                03241014
                     , :VARC-TELEX-2                                    03242014
                     , :VARC-GRUPO-CTAS                                 03243014
                     , :VARC-OPER-TIT                                   03244014
                     , :VARC-FEALTREG                                   03245014
                     , :VARC-FEULMOD                                    03246014
                     , :VARC-HORULMOD                                   03247014
                     , :VARC-NUMTER                                     03248014
                     , :VARC-USUARIO                                    03249014
                     , :VARC-FILLER                                     03250014
                     , :VARC-CTAVAL20                                   03251014
      *@ZAL-INI                                                         03252014
      *              , :VARC-NUMMAN                                     03253014
                     , :VARC-GRUPO-CTAS                                 03254014
      *@ZAL-FIN                                                         03255014
                     , :VARC-INDIMP                                     03256014
                     , :VARC-INDSAB                                     03257014
                  FROM  VLDTARC                                         03258014
                 WHERE  VARC_CUENTA  = :VARC-CUENTA                     03259014
           END-EXEC                                                     03260014
      *                                                                 03261014
           MOVE SQLCODE TO SQLCODE-AUX                                  03262014
      *                                                                 03263014
           EVALUATE TRUE                                                03264014
              WHEN DB2-OK                                               03265014
      *SE MODIFICA PORQUE DEJABA INACTIVAR UNA CUENTA QUE ESTABA        03266014
      *CANCELADA.16-07-1999.                                            03267014
      *            IF VARC-SITUAC = 'B'                                 03268014
                   IF VARC-SITUAC = 'X' OR 'B'                          03269014
                      MOVE 'VLE0141'  TO CAA-COD-ERROR                  03270014
                      MOVE -1         TO CTA0101L                       03271014
                      PERFORM 3-FINAL                                   03272014
                   END-IF                                               03273014
      *A2011-RUTLOG-I                                                   03274014
                   INITIALIZE W-VLWCLOG0                                03275014
                              LOGVLDTARC                                03276014
                   MOVE 'VLDTARC'             TO  VL7LOG-TABLA          03277014
                   MOVE 'SELECT'              TO  VL7LOG-OPERACION      03278014
                   MOVE LENGTH OF DCLVLDTARC  TO  VL7LOG-REGISTRO-LEN   03279014
                   MOVE DCLVLDTARC            TO  LOGVLDTARC            03280014
                   MOVE LOGVLDTARC            TO  VL7LOG-REGISTRO-TEXT  03281014
                   PERFORM LLAMAR-VL7CRLOG                              03282014
                      THRU LLAMAR-VL7CRLOG-FIN                          03283014
                                                                        03284014
      *A2011-RUTLOG-F                                                   03285014
      *                                                                 03286014
              WHEN OTHER                                                03287014
                   MOVE 'SELECT'      TO  ABC-REFERENCIA                03288014
                   MOVE 'VLDTARC'     TO  ABC-OBJETO-ERROR              03289014
                   PERFORM 999-ABEND-DB2                                03290014
      *                                                                 03291014
           END-EVALUATE.                                                03292014
                                                                        03293014
      *200711038-INI                                                    03294014
           IF VARC-FILLER (11:02) = '91'                                03295014
              INITIALIZE                 W-BGECMDC                      03296014
              MOVE VARC-FILLER (01:4) TO MDC-ENTIDAD                    03297014
              MOVE VARC-FILLER (05:4) TO MDC-CENTRO-ALTA                03298014
              MOVE VARC-FILLER (11:2) TO MDC-CUENTA(1:2)                03299014
              MOVE VARC-FILLER (13:8) TO MDC-CUENTA(3:8)                03300014
      *                                                                 03301014
              EXEC CICS                                                 03302014
                   LINK PROGRAM  (BG2CMDC0)                             03303014
                        COMMAREA (BGECMDC)                              03304014
              END-EXEC                                                  03305014
      *                                                                 03306014
              IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                      03307014
                 MOVE 'ERROR EN BGECMDC0'  TO ABC-REFERENCIA            03308014
                 MOVE 'BG2CMDC0'           TO ABC-OBJETO-ERROR          03309014
                 PERFORM 999-ABEND-CICS                                 03310014
              END-IF                                                    03311014
      *                                                                 03312014
              EVALUATE MDC-CODERR                                       03313014
                  WHEN SPACES                                           03314014
                       IF MDC-SALDO-DISPON NOT = ZEROS                  03315014
                          MOVE MDC-SALDO-DISPON       TO W-SDOECON-EDIT 03316014
                          MOVE 'VLE2169'              TO CAA-COD-ERROR  03317014
                          MOVE 'CTA-REGISTRO CON SAL' TO CAA-VAR1-ERROR 03318014
                          MOVE 'DO                  ' TO CAA-VAR2-ERROR 03319014
                          MOVE W-SDOECON-EDIT TO CAA-VAR2-ERROR (04:15) 03320014
                          MOVE -1         TO CTA0101L                   03321014
                          PERFORM 3-FINAL                               03322014
                       END-IF                                           03323014
                  WHEN OTHER                                            03324014
                       MOVE -1          TO NCC0101L                     03325014
                       MOVE MDC-CODERR  TO CAA-COD-ERROR                03326014
                       PERFORM 3-FINAL                                  03327014
              END-EVALUATE                                              03328014
           END-IF.                                                      03329014
      *200711038-FIN                                                    03330014
      *                                                                 03331014
           MOVE VARC-CUENTA        TO VADT-CUENTA                       03332014
           MOVE 8                  TO VADT-CLTITU                       03333014
      *                                                                 03334014
           INITIALIZE      VADT-NUMCLI                                  03335014
      *                                                                 03336014
           EXEC SQL                                                     03337014
JPC@1 *         SELECT  *                                               03338014
                SELECT  VADT_CUENTA                                     03339014
                     ,  VADT_NUMCLI                                     03340014
                     ,  VADT_CLTITU                                     03341014
                     ,  VADT_NUMDOM                                     03342014
                     ,  VADT_ADMIN                                      03343014
                     ,  VADT_FEVENCTO                                   03344014
                     ,  VADT_FEALTREG                                   03345014
                     ,  VADT_FEULMOD                                    03346014
                     ,  VADT_HORULMOD                                   03347014
                     ,  VADT_NUMTER                                     03348014
                     ,  VADT_USUARIO                                    03349014
JPC@1 *           INTO :DCLVLDTADT                                      03350014
JPC@1             INTO :VADT-CUENTA                                     03351014
                     , :VADT-NUMCLI                                     03352014
                     , :VADT-CLTITU                                     03353014
                     , :VADT-NUMDOM                                     03354014
                     , :VADT-ADMIN                                      03355014
                     , :VADT-FEVENCTO                                   03356014
                     , :VADT-FEALTREG                                   03357014
                     , :VADT-FEULMOD                                    03358014
                     , :VADT-HORULMOD                                   03359014
                     , :VADT-NUMTER                                     03360014
                     , :VADT-USUARIO                                    03361014
                  FROM  VLDTADT                                         03362014
                 WHERE  VADT_CUENTA  = :VADT-CUENTA                     03363014
                   AND  VADT_CLTITU  = :VADT-CLTITU                     03364014
                   AND  VADT_NUMCLI >= :VADT-NUMCLI                     03365014
           END-EXEC                                                     03366014
      *                                                                 03367014
           MOVE SQLCODE TO SQLCODE-AUX                                  03368014
      *                                                                 03369014
           EVALUATE TRUE                                                03370014
              WHEN DB2-OK                                               03371014
              WHEN DB2-DUPLINE                                          03372014
                   MOVE 'VLE2087'  TO CAA-COD-ERROR                     03373014
                   MOVE -1         TO CTA0101L                          03374014
                   PERFORM 3-FINAL                                      03375014
      *                                                                 03376014
              WHEN DB2-NOTFND                                           03377014
                   CONTINUE                                             03378014
      *                                                                 03379014
              WHEN OTHER                                                03380014
                   MOVE 'SELECT'      TO  ABC-REFERENCIA                03381014
                   MOVE 'VLDTADT'     TO  ABC-OBJETO-ERROR              03382014
                   PERFORM 999-ABEND-DB2                                03383014
      *                                                                 03384014
           END-EVALUATE.                                                03385014
      *                                                                 03386014
      *A2011-RUTLOG-I                                                   03387014
           INITIALIZE W-VLWCLOG0                                        03388014
           MOVE 'VLDTADT'             TO  VL7LOG-TABLA                  03389014
           MOVE 'SELECT'              TO  VL7LOG-OPERACION              03390014
           MOVE LENGTH OF DCLVLDTADT  TO  VL7LOG-REGISTRO-LEN           03391014
           MOVE DCLVLDTADT            TO  LOGVLDTADT                    03392014
           MOVE LOGVLDTADT            TO  VL7LOG-REGISTRO-TEXT          03393014
           PERFORM LLAMAR-VL7CRLOG                                      03394014
              THRU LLAMAR-VL7CRLOG-FIN                                  03395014
      *A2011-RUTLOG-F                                                   03396014
                                                                        03397014
           MOVE VARC-CUENTA     TO VADS-CUENTA                          03398014
           MOVE ZEROES          TO VADS-ISIN                            03399014
           MOVE SPACES          TO VADS-PAVAL                           03400014
                                   VADS-VALOR                           03401014
                                   VADS-TIPREG                          03402014
                                   VADS-NUMGRUN                         03403014
      *                                                                 03404014
      *A2008-I. 14-06-2000. SE PERMITE INACTIVAR LA CUENTA SI NO TIENE  03405014
      *                     SALDO                                       03406014
      *                                                                 03407014
           MOVE ZEROS           TO VADS-DEPOS                           03408014
                                   VADS-COMPR                           03409014
                                   VADS-SUSCR                           03410014
                                   VADS-VENTA                           03411014
                                   VADS-ORDVE                           03412014
                                   VADS-BLOQ                            03413014
      *                                                                 03414014
      *A2008-F. 14-06-2000. SE PERMITE INACTIVAR LA CUENTA SI NO TIENE  03415014
      *                     SALDO                                       03416014
      *                                                                 03417014
           EXEC SQL                                                     03418014
                SELECT  VADS_DEPOS                                      03419014
                     ,  VADS_COMPR                                      03420014
                     ,  VADS_SUSCR                                      03421014
                     ,  VADS_VENTA                                      03422014
                     ,  VADS_ORDVE                                      03423014
                     ,  VADS_BLOQ                                       03424014
                  INTO :VADS-DEPOS                                      03425014
                     , :VADS-COMPR                                      03426014
                     , :VADS-SUSCR                                      03427014
                     , :VADS-VENTA                                      03428014
                     , :VADS-ORDVE                                      03429014
                     , :VADS-BLOQ                                       03430014
                  FROM  VLDTADS                                         03431014
                 WHERE  VADS_CUENTA   = :VADS-CUENTA                    03432014
                   AND  VADS_PAVAL   >= :VADS-PAVAL                     03433014
                   AND  VADS_VALOR   >= :VADS-VALOR                     03434014
                   AND  VADS_ISIN    >= :VADS-ISIN                      03435014
                   AND  VADS_TIPREG  >= :VADS-TIPREG                    03436014
                   AND  VADS_NUMGRUN >= :VADS-NUMGRUN                   03437014
      *                                                                 03438014
      *A2008-I. 14-06-2000. SE PERMITE INACTIVAR LA CUENTA SI NO TIENE  03439014
      *                     SALDO                                       03440014
      *                                                                 03441014
                   AND (VADS_DEPOS   > :VADS-DEPOS                      03442014
                    OR  VADS_COMPR   > :VADS-COMPR                      03443014
                    OR  VADS_SUSCR   > :VADS-SUSCR                      03444014
                    OR  VADS_VENTA   > :VADS-VENTA                      03445014
                    OR  VADS_ORDVE   > :VADS-ORDVE                      03446014
                    OR  VADS_BLOQ    > :VADS-BLOQ  )                    03447014
      *                                                                 03448014
      *A2008-F. 14-06-2000. SE PERMITE INACTIVAR LA CUENTA SI NO TIENE  03449014
      *                     SALDO                                       03450014
      *                                                                 03451014
           END-EXEC                                                     03452014
      *                                                                 03453014
           MOVE SQLCODE TO SQLCODE-AUX                                  03454014
      *                                                                 03455014
           EVALUATE TRUE                                                03456014
              WHEN DB2-OK                                               03457014
              WHEN DB2-DUPLINE                                          03458014
                   MOVE 'VLE0028'  TO CAA-COD-ERROR                     03459014
                   MOVE -1         TO CTA0101L                          03460014
                   PERFORM 3-FINAL                                      03461014
      *                                                                 03462014
              WHEN DB2-NOTFND                                           03463014
                   CONTINUE                                             03464014
      *                                                                 03465014
              WHEN OTHER                                                03466014
                   MOVE 'SELECT'      TO  ABC-REFERENCIA                03467014
                   MOVE 'VLDTADS'     TO  ABC-OBJETO-ERROR              03468014
                   PERFORM 999-ABEND-DB2                                03469014
      *                                                                 03470014
           END-EVALUATE.                                                03471014
      *                                                                 03472014
           INITIALIZE W-VLWCLOG0                                        03473014
                      LOGVLDTADS                                        03474014
           MOVE 'VLDTADS'             TO  VL7LOG-TABLA                  03475014
           MOVE 'SELECT'              TO  VL7LOG-OPERACION              03476014
           MOVE LENGTH OF DCLVLDTADS  TO  VL7LOG-REGISTRO-LEN           03477014
           MOVE DCLVLDTADS            TO  LOGVLDTADS                    03478014
           MOVE LOGVLDTADS            TO  VL7LOG-REGISTRO-TEXT          03479014
           PERFORM LLAMAR-VL7CRLOG                                      03480014
              THRU LLAMAR-VL7CRLOG-FIN                                  03481014
      *A2011-RUTLOG-F                                                   03482014
LERS       MOVE VARC-CUENTA     TO VDET-CTAVAL                          03483014
09                                                                      03484014
07         PERFORM ABRIR-CURSOR-DET                                     03485014
2001          THRU ABRIR-CURSOR-DET-EXIT.                               03486014
 |                                                                      03487014
 |         MOVE 0 TO SW-DET.                                            03488014
 |         PERFORM FETCH-DET                                            03489014
 |            THRU FETCH-DET-EXIT.                                      03490014
 |                                                                      03491014
 |         IF DB2-NOTFND                                                03492014
 |            MOVE 1 TO SW-DET                                          03493014
 |         END-IF.                                                      03494014
 |                                                                      03495014
 |         PERFORM TRATAR-DET                                           03496014
 |            THRU TRATAR-DET-EXIT                                      03497014
 |         UNTIL SW-DET = 1.                                            03498014
 |                                                                      03499014
 |         PERFORM CERRAR-CURSOR-DET                                    03500014
 |            THRU CERRAR-CURSOR-DET-EXIT.                              03501014
LERS                                                                    03502014
      *200306088-INI                                                    03503014
           MOVE VARC-CUENTA     TO VTRA-CUENTA                          03504014
                                                                        03505014
           EXEC SQL                                                     03506014
                SELECT  VTRA_REFER                                      03507014
                  INTO :VTRA-REFER                                      03508014
                  FROM  VLDTTRA                                         03509014
                 WHERE  VTRA_CUENTA   = :VTRA-CUENTA                    03510014
                   AND  VTRA_SITUAC  IN ('OP','PL','PD', 'OE', 'PG')    03511014
           END-EXEC                                                     03512014
                                                                        03513014
           MOVE SQLCODE TO SQLCODE-AUX                                  03514014
                                                                        03515014
           EVALUATE TRUE                                                03516014
               WHEN DB2-OK                                              03517014
               WHEN DB2-DUPLINE                                         03518014
                    MOVE 'VLE2104'  TO CAA-COD-ERROR                    03519014
                    MOVE -1         TO CTA0101L                         03520014
                    PERFORM 3-FINAL                                     03521014
                                                                        03522014
               WHEN DB2-NOTFND                                          03523014
                    CONTINUE                                            03524014
                                                                        03525014
               WHEN OTHER                                               03526014
                    MOVE 'SELECT'      TO  ABC-REFERENCIA               03527014
                    MOVE 'VLDTTRA'     TO  ABC-OBJETO-ERROR             03528014
                    PERFORM 999-ABEND-DB2                               03529014
                                                                        03530014
           END-EVALUATE.                                                03531014
      *200306088-FIN                                                    03532014
                                                                        03533014
      *A2011-RUTLOG-I                                                   03534014
           MOVE VARC-CUENTA            TO LARC-CUENTA                   03535014
           PERFORM SELUND-VLDTARC                                       03536014
              THRU SELUND-VLDTARC-FIN                                   03537014
      *A2011-RUTLOG-F                                                   03538014
                                                                        03539014
           MOVE 'B'               TO  VARC-SITUAC                       03540014
      *                                                                 03541014
           MOVE CAA-FECHA-OPER    TO  VARC-FEULMOD                      03542014
           MOVE CAA-HORA-TRANS    TO  VARC-HORULMOD                     03543014
           MOVE CAA-TERMINAL      TO  VARC-NUMTER                       03544014
           MOVE CAA-USERID        TO  VARC-USUARIO                      03545014
      *                                                                 03546014
           EXEC SQL                                                     03547014
                UPDATE VLDTARC                                          03548014
                   SET VARC_SITUAC      = :VARC-SITUAC                  03549014
                     , VARC_FEULMOD     = :VARC-FEULMOD                 03550014
                     , VARC_HORULMOD    = :VARC-HORULMOD                03551014
                     , VARC_NUMTER      = :VARC-NUMTER                  03552014
                     , VARC_USUARIO     = :VARC-USUARIO                 03553014
                 WHERE VARC_CUENTA = :VARC-CUENTA                       03554014
           END-EXEC                                                     03555014
      *                                                                 03556014
           MOVE SQLCODE TO SQLCODE-AUX                                  03557014
      *                                                                 03558014
           EVALUATE TRUE                                                03559014
              WHEN DB2-OK                                               03560014
      *A2011-RUTLOG-I                                                   03561014
                   INITIALIZE W-VLWCLOG0                                03562014
                              LOGVLDTARC                                03563014
                   MOVE 'VLDTARC'             TO  VL7LOG-TABLA          03564014
                   MOVE 'UPDATE'              TO  VL7LOG-OPERACION      03565014
                   MOVE LENGTH OF DCLVLDTARC  TO  VL7LOG-REGISTRO-LEN   03566014
                   MOVE VARC-SITUAC           TO  LARC-SITUAC           03567014
                   MOVE VARC-FEULMOD          TO  LARC-FEULMOD          03568014
                   MOVE VARC-HORULMOD         TO  LARC-HORULMOD         03569014
                   MOVE VARC-NUMTER           TO  LARC-NUMTER           03570014
                   MOVE VARC-USUARIO          TO  LARC-USUARIO          03571014
                   MOVE VARC-CUENTA           TO  LARC-CUENTA           03572014
                   MOVE LOGVLDTARC            TO  VL7LOG-REGISTRO-TEXT  03573014
                   PERFORM LLAMAR-VL7CRLOG                              03574014
                      THRU LLAMAR-VL7CRLOG-FIN                          03575014
      *A2011-RUTLOG-F                                                   03576014
      *                                                                 03577014
              WHEN OTHER                                                03578014
                   MOVE 'UPDATE-BAJA' TO  ABC-REFERENCIA                03579014
                   MOVE 'VLDTARC'     TO  ABC-OBJETO-ERROR              03580014
                   PERFORM 999-ABEND-DB2                                03581014
           END-EVALUATE                                                 03582014
      *                                                                 03583014
      **************************************************************    03584014
      *****************RUTINA BGECMSC****DAVID  06-11-1998**********    03585014
      * RUTINA PARA DAR BAJA, DESVINCULA LA CTA ECONOMICA          *    03586014
      * CON LA CUENTA VALOR.  BAJITA                               *    03587014
      **************************************************************    03588014
      **************************************************************    03589014
      **************************************************************    03590014
      *    MOVE NCC0101I       TO  W-CARGO                              03591014
      *    MOVE NC20101I       TO  W-ABO                                03592014
                                                                        03593014
           IF NCC0101-COMM = NC20101-COMM                               03594014
              INITIALIZE                     BGECMSC                    03595014
                                                                        03596014
              MOVE '2'                   TO MSC-FUNCION                 03597014
              MOVE NCC0101-COMM(11:2)    TO MSC-CUENTA(1:2)             03598014
              MOVE NCC0101-COMM(13:8)    TO MSC-CUENTA(3:8)             03599014
              MOVE NCC0101-COMM(1:4)     TO MSC-ENTIDAD                 03600014
              MOVE NCC0101-COMM(5:4)     TO MSC-CENTRO-ALTA             03601014
              PERFORM RUTINA-BGECMSC                                    03602014
                 THRU RUTINA-BGECMSC-F                                  03603014
           ELSE                                                         03604014
              INITIALIZE                     BGECMSC                    03605014
                                                                        03606014
              MOVE '2'                   TO MSC-FUNCION                 03607014
              MOVE NCC0101-COMM(11:2)    TO MSC-CUENTA(1:2)             03608014
              MOVE NCC0101-COMM(13:8)    TO MSC-CUENTA(3:8)             03609014
              MOVE NCC0101-COMM(1:4)     TO MSC-ENTIDAD                 03610014
              MOVE NCC0101-COMM(5:4)     TO MSC-CENTRO-ALTA             03611014
              PERFORM RUTINA-BGECMSC                                    03612014
                 THRU RUTINA-BGECMSC-F                                  03613014
                                                                        03614014
              INITIALIZE                     BGECMSC                    03615014
                                                                        03616014
              MOVE '2'                   TO MSC-FUNCION                 03617014
              MOVE NC20101-COMM(11:2)    TO MSC-CUENTA(1:2)             03618014
              MOVE NC20101-COMM(13:8)    TO MSC-CUENTA(3:8)             03619014
              MOVE NC20101-COMM(1:4)     TO MSC-ENTIDAD                 03620014
              MOVE NC20101-COMM(5:4)     TO MSC-CENTRO-ALTA             03621014
              PERFORM RUTINA-BGECMSC                                    03622014
                 THRU RUTINA-BGECMSC-F                                  03623014
           END-IF.                                                      03624014
      **************************************************************    03625014
      *****************RUTINA BGECMSC****DAVID  06-11-1998**********    03626014
      **************************************************************    03627014
      *ACA*                                                             03628014
      * SE QUITA POR INDICACIONES DE REFAEL HERMOZA LA CTA. ESTA        03629014
      * INACTIVADA Y NO DEJA DE EXISTIR (10/4/99)                       03630014
      *ACA*                                                             03631014
      *    PERFORM BAJA-INTERVINIENTE                                   03632014
      *       THRU BAJA-INTERVINIENTE-F                                 03633014
      *200306088-INI SE HABILITA ESTE PARRAFO                           03634014
           PERFORM BAJA-INTERVINIENTE                                   03635014
              THRU BAJA-INTERVINIENTE-F                                 03636014
      *200306088-FIN                                                    03637014
JPC@3 *    SI CUENTA VALOR PERTENECE A PORTAFOLIO, BAJA A ALTERNANTE.   03638014
JPC@3      IF VFPF-CUENTA-ALTE > ZEROS                                  03639014
JPC@3         PERFORM 25-INACTIVAR-PORTA                                03640014
JPC@3            THRU 25-INACTIVAR-PORTA-FIN                            03641014
JPC@3      END-IF                                                       03642014
JPC@3 *    SI CUENTA VALOR PERTENECE A PORTAFOLIO, BAJA A ALTERNANTE.   03643014
      *                                                                 03644014
           MOVE CTA0101I         TO CTA0101-COMM                        03645014
           MOVE SUC0101I         TO SUC0101-COMM                        03646014
           MOVE NCC0101I         TO NCC0101-COMM                        03647014
           MOVE NC20101I         TO NC20101-COMM                        03648014
           MOVE ENT0101I         TO ENT0101-COMM                        03649014
      *MADRID-04-04-1999.INI.                                           03650014
           MOVE TIT0101I         TO TIT0101-COMM                        03651014
           MOVE SPACES            TO OPT-COMM                           03652014
           MOVE SPACES            TO MSB-COMM                           03653014
           MOVE -1                TO CTA0101L                           03654014
      *200306088-INI                                                    03655014
      *    MOVE 'VLA0077'         TO CAA-COD-AVISO1.                    03656014
      *200306088-FIN                                                    03657014
           MOVE 'VLA0086'         TO CAA-COD-AVISO1.                    03658014
      *                                                                 03659014
       25-INACTIVAR-FIN.  EXIT.                                         03660014
      *                                                                 03661014
JPC@3  VALIDA-CTA-PORTAFOLIO.                                           03662014
JPC@3 *                                                                 03663014
JPC@3      MOVE ZEROS           TO VFPF-CUENTA-PORT.                    03664014
JPC@3      MOVE ZEROS           TO VFPF-CUENTA-ALTE.                    03665014
JPC@3 *                                                                 03666014
JPC@3      EXEC SQL                                                     03667014
JPC@3           SELECT  VFPF_CUENTA_PORT                                03668014
JPC@3                ,  VFPF_CUENTA_ALTE                                03669014
JPC@3                ,  VFPF_CIND_ACTIVO                                03670014
JPC@3             INTO :VFPF-CUENTA-PORT                                03671014
JPC@3                , :VFPF-CUENTA-ALTE                                03672014
JPC@3                , :VFPF-CIND-ACTIVO                                03673014
JPC@3             FROM  VLDTFPF                                         03674014
JPC@3            WHERE  VFPF_CUENTA_PORT = :VARC-CUENTA                 03675014
JPC@3               OR  VFPF_CUENTA_ALTE = :VARC-CUENTA                 03676014
JPC@3      END-EXEC                                                     03677014
JPC@3 *                                                                 03678014
JPC@3      MOVE SQLCODE TO SQLCODE-AUX                                  03679014
JPC@3 *                                                                 03680014
JPC@3      EVALUATE TRUE                                                03681014
JPC@3          WHEN DB2-OK                                              03682014
JPC@3               IF VARC-CUENTA NOT = VFPF-CUENTA-PORT               03683014
JPC@3                  MOVE 'VLE2169'              TO CAA-COD-ERROR     03684014
JPC@3                  MOVE 'CUENTA PORTAFOLIO NO' TO CAA-VAR1-ERROR    03685014
JPC@3                  MOVE ' ES LA PRINCIPAL    ' TO CAA-VAR2-ERROR    03686014
JPC@3                  MOVE -1                     TO CTA0101L          03687014
JPC@3                  PERFORM 3-FINAL                                  03688014
JPC@3               END-IF                                              03689014
JPC@3               IF VFPF-CIND-ACTIVO NOT = 'S'                       03690014
JPC@3                  MOVE 'VLE1945'  TO CAA-COD-ERROR                 03691014
JPC@3                  MOVE -1         TO CTA0101L                      03692014
JPC@3                  PERFORM 3-FINAL                                  03693014
JPC@3               END-IF                                              03694014
JPC@3               INITIALIZE W-VLWCLOG0                               03695014
JPC@3                          LOGVLDTFPF                               03696014
JPC@3               MOVE 'VLDTFPF'             TO  VL7LOG-TABLA         03697014
JPC@3               MOVE 'SELECT'              TO  VL7LOG-OPERACION     03698014
JPC@3               MOVE LENGTH OF DCLVLDTFPF  TO  VL7LOG-REGISTRO-LEN  03699014
JPC@3               MOVE DCLVLDTFPF            TO  LOGVLDTFPF           03700014
JPC@3               MOVE LOGVLDTFPF            TO  VL7LOG-REGISTRO-TEXT 03701014
JPC@3               PERFORM LLAMAR-VL7CRLOG                             03702014
JPC@3                  THRU LLAMAR-VL7CRLOG-FIN                         03703014
JPC@3          WHEN DB2-NOTFND                                          03704014
JPC@3               INITIALIZE             DCLVLDTFPF                   03705014
JPC@3          WHEN OTHER                                               03706014
JPC@3               MOVE 'SELECT'      TO  ABC-REFERENCIA               03707014
JPC@3               MOVE 'VLDTFPF'     TO  ABC-OBJETO-ERROR             03708014
JPC@3               PERFORM 999-ABEND-DB2                               03709014
JPC@3      END-EVALUATE.                                                03710014
JPC@3 *                                                                 03711014
JPC@3 * VALIDA SITUACION CONTRATO PORTAFOLIO - CUENTA PRINCIPAL         03712014
JPC@3 *                                                                 03713014
JPC@3      IF DB2-OK                                                    03714014
JPC@3         EXEC SQL                                                  03715014
JPC@3              SELECT  VARC_SITUAC                                  03716014
JPC@3                INTO :VARC-SITUAC                                  03717014
JPC@3                FROM  VLDTARC                                      03718014
JPC@3               WHERE  VARC_CUENTA = :VFPF-CUENTA-PORT              03719014
JPC@3         END-EXEC                                                  03720014
JPC@3 *                                                                 03721014
JPC@3         MOVE SQLCODE TO SQLCODE-AUX                               03722014
JPC@3 *                                                                 03723014
JPC@3         EVALUATE TRUE                                             03724014
JPC@3             WHEN DB2-OK                                           03725014
JPC@3                  IF VARC-SITUAC NOT = 'A'                         03726014
JPC@3                     MOVE 'VLE2169'              TO CAA-COD-ERROR  03727014
JPC@3                     MOVE 'CUENTA PRINCIPAL NO ' TO CAA-VAR1-ERROR 03728014
JPC@3                     MOVE 'ACTIVO :            ' TO CAA-VAR1-ERROR 03729014
JPC@3                     MOVE VFPF-CUENTA-PORT  TO CAA-VAR2-ERROR(12:7)03730014
JPC@3                     MOVE -1                  TO CTA0101L          03731014
JPC@3                     PERFORM 3-FINAL                               03732014
JPC@3                  END-IF                                           03733014
JPC@3             WHEN OTHER                                            03734014
JPC@3                  MOVE 'SELECT'      TO  ABC-REFERENCIA            03735014
JPC@3                  MOVE 'VLDTARC-P1'  TO  ABC-OBJETO-ERROR          03736014
JPC@3                  PERFORM 999-ABEND-DB2                            03737014
JPC@3         END-EVALUATE                                              03738014
JPC@3 *                                                                 03739014
JPC@3 * VALIDA SITUACION CONTRATO PORTAFOLIO - CUENTA ALTERNANTE        03740014
JPC@3 *                                                                 03741014
JPC@3         EXEC SQL                                                  03742014
JPC@3              SELECT  VARC_SITUAC                                  03743014
JPC@3                INTO :VARC-SITUAC                                  03744014
JPC@3                FROM  VLDTARC                                      03745014
JPC@3               WHERE  VARC_CUENTA = :VFPF-CUENTA-ALTE              03746014
JPC@3         END-EXEC                                                  03747014
JPC@3 *                                                                 03748014
JPC@3         MOVE SQLCODE TO SQLCODE-AUX                               03749014
JPC@3 *                                                                 03750014
JPC@3         EVALUATE TRUE                                             03751014
JPC@3             WHEN DB2-OK                                           03752014
JPC@3                  IF VARC-SITUAC NOT = 'A'                         03753014
JPC@5                     MOVE ZEROS                 TO VFPF-CUENTA-ALTE03754014
JPC@5 *JPC@3              MOVE 'VLE2169'              TO CAA-COD-ERROR  03755014
JPC@5 *JPC@3              MOVE 'CUENTA ALTERNANTE NO' TO CAA-VAR1-ERROR 03756014
JPC@5 *JPC@3              MOVE ' ACTIVO :           ' TO CAA-VAR2-ERROR 03757014
JPC@5 *JPC@3              MOVE VFPF-CUENTA-PORT  TO CAA-VAR2-ERROR(11:7)03758014
JPC@5 *JPC@3              MOVE -1                  TO CTA0101L          03759014
JPC@5 *JPC@3              PERFORM 3-FINAL                               03760014
JPC@3                  END-IF                                           03761014
JPC@3             WHEN OTHER                                            03762014
JPC@3                  MOVE 'SELECT'      TO  ABC-REFERENCIA            03763014
JPC@3                  MOVE 'VLDTARC-P2'  TO  ABC-OBJETO-ERROR          03764014
JPC@3                  PERFORM 999-ABEND-DB2                            03765014
JPC@3         END-EVALUATE                                              03766014
JPC@3      END-IF.                                                      03767014
JPC@3 *                                                                 03768014
JPC@3 *------------------*                                              03769014
JPC@3  25-INACTIVAR-PORTA.                                              03770014
JPC@3 *------------------*                                              03771014
JPC@3 *                                                                 03772014
JPC@3      MOVE VFPF-CUENTA-ALTE TO VARC-CUENTA                         03773014
JPC@3 *                                                                 03774014
JPC@3      EXEC SQL                                                     03775014
JPC@3           SELECT  VARC_CUENTA                                     03776014
JPC@3                ,  VARC_CENTAD                                     03777014
JPC@3                ,  VARC_NUMCLI                                     03778014
JPC@3                ,  VARC_CLMAST                                     03779014
JPC@3                ,  VARC_MONEDA                                     03780014
JPC@3                ,  VARC_SUCURS                                     03781014
JPC@3                ,  VARC_CTACAR                                     03782014
JPC@3                ,  VARC_CTAABO                                     03783014
JPC@3                ,  VARC_TEXTO                                      03784014
JPC@3                ,  VARC_PRESEN                                     03785014
JPC@3                ,  VARC_GRUPO                                      03786014
JPC@3                ,  VARC_RUT                                        03787014
JPC@3                ,  VARC_CNAE                                       03788014
JPC@3                ,  VARC_SITUAC                                     03789014
JPC@3                ,  VARC_EXEN1                                      03790014
JPC@3                ,  VARC_EXEN2                                      03791014
JPC@3                ,  VARC_EXEN3                                      03792014
JPC@3                ,  VARC_EXEN4                                      03793014
JPC@3                ,  VARC_EXEN5                                      03794014
JPC@3                ,  VARC_EXEN6                                      03795014
JPC@3                ,  VARC_EXEN7                                      03796014
JPC@3                ,  VARC_EXEN8                                      03797014
JPC@3                ,  VARC_EXEN9                                      03798014
JPC@3                ,  VARC_EXEN10                                     03799014
JPC@3                ,  VARC_ANALIS                                     03800014
JPC@3                ,  VARC_CLACARGO                                   03801014
JPC@3                ,  VARC_CLABONO                                    03802014
JPC@3                ,  VARC_NUMDOM                                     03803014
JPC@3                ,  VARC_CODSUS                                     03804014
JPC@3                ,  VARC_FE_ULT_EXT                                 03805014
JPC@3                ,  VARC_PAIS                                       03806014
JPC@3                ,  VARC_FE_CARTERA                                 03807014
JPC@3                ,  VARC_CLTELEX                                    03808014
JPC@3                ,  VARC_FE_ALTA                                    03809014
JPC@3                ,  VARC_VALORACION                                 03810014
JPC@3                ,  VARC_VALEXTRJ                                   03811014
JPC@3                ,  VARC_INVERSOR                                   03812014
JPC@3                ,  VARC_DIRECTA                                    03813014
JPC@3                ,  VARC_MAX_CVE_1                                  03814014
JPC@3                ,  VARC_MAX_DCU_5                                  03815014
JPC@3                ,  VARC_MAX_SUS_6                                  03816014
JPC@3                ,  VARC_MAX_DIV_7                                  03817014
JPC@3                ,  VARC_MAX_AMO_8                                  03818014
JPC@3                ,  VARC_MAX_PAJ_9                                  03819014
JPC@3                ,  VARC_FECHA_102                                  03820014
JPC@3                ,  VARC_TARIFACUS                                  03821014
JPC@3                ,  VARC_SWIFT_TELEX                                03822014
JPC@3                ,  VARC_TELEX_2                                    03823014
JPC@3                ,  VARC_GRUPO_CTAS                                 03824014
JPC@3                ,  VARC_OPER_TIT                                   03825014
JPC@3                ,  VARC_FEALTREG                                   03826014
JPC@3                ,  VARC_FEULMOD                                    03827014
JPC@3                ,  VARC_HORULMOD                                   03828014
JPC@3                ,  VARC_NUMTER                                     03829014
JPC@3                ,  VARC_USUARIO                                    03830014
JPC@3                ,  VARC_FILLER                                     03831014
JPC@3                ,  VARC_CTAVAL20                                   03832014
      *@ZAL-INI                                                         03833014
      *              ,  VARC_NUMMAN                                     03834014
                     ,  VARC_GRUPO_CTAS                                 03835014
      *@ZAL-FIN                                                         03836014
JPC@3                ,  VARC_INDIMP                                     03837014
JPC@3                ,  VARC_INDSAB                                     03838014
JPC@3             INTO :VARC-CUENTA                                     03839014
JPC@3                , :VARC-CENTAD                                     03840014
JPC@3                , :VARC-NUMCLI                                     03841014
JPC@3                , :VARC-CLMAST                                     03842014
JPC@3                , :VARC-MONEDA                                     03843014
JPC@3                , :VARC-SUCURS                                     03844014
JPC@3                , :VARC-CTACAR                                     03845014
JPC@3                , :VARC-CTAABO                                     03846014
JPC@3                , :VARC-TEXTO                                      03847014
JPC@3                , :VARC-PRESEN                                     03848014
JPC@3                , :VARC-GRUPO                                      03849014
JPC@3                , :VARC-RUT                                        03850014
JPC@3                , :VARC-CNAE                                       03851014
JPC@3                , :VARC-SITUAC                                     03852014
JPC@3                , :VARC-EXEN1                                      03853014
JPC@3                , :VARC-EXEN2                                      03854014
JPC@3                , :VARC-EXEN3                                      03855014
JPC@3                , :VARC-EXEN4                                      03856014
JPC@3                , :VARC-EXEN5                                      03857014
JPC@3                , :VARC-EXEN6                                      03858014
JPC@3                , :VARC-EXEN7                                      03859014
JPC@3                , :VARC-EXEN8                                      03860014
JPC@3                , :VARC-EXEN9                                      03861014
JPC@3                , :VARC-EXEN10                                     03862014
JPC@3                , :VARC-ANALIS                                     03863014
JPC@3                , :VARC-CLACARGO                                   03864014
JPC@3                , :VARC-CLABONO                                    03865014
JPC@3                , :VARC-NUMDOM                                     03866014
JPC@3                , :VARC-CODSUS                                     03867014
JPC@3                , :VARC-FE-ULT-EXT                                 03868014
JPC@3                , :VARC-PAIS                                       03869014
JPC@3                , :VARC-FE-CARTERA                                 03870014
JPC@3                , :VARC-CLTELEX                                    03871014
JPC@3                , :VARC-FE-ALTA                                    03872014
JPC@3                , :VARC-VALORACION                                 03873014
JPC@3                , :VARC-VALEXTRJ                                   03874014
JPC@3                , :VARC-INVERSOR                                   03875014
JPC@3                , :VARC-DIRECTA                                    03876014
JPC@3                , :VARC-MAX-CVE-1                                  03877014
JPC@3                , :VARC-MAX-DCU-5                                  03878014
JPC@3                , :VARC-MAX-SUS-6                                  03879014
JPC@3                , :VARC-MAX-DIV-7                                  03880014
JPC@3                , :VARC-MAX-AMO-8                                  03881014
JPC@3                , :VARC-MAX-PAJ-9                                  03882014
JPC@3                , :VARC-FECHA-102                                  03883014
JPC@3                , :VARC-TARIFACUS                                  03884014
JPC@3                , :VARC-SWIFT-TELEX                                03885014
JPC@3                , :VARC-TELEX-2                                    03886014
JPC@3                , :VARC-GRUPO-CTAS                                 03887014
JPC@3                , :VARC-OPER-TIT                                   03888014
JPC@3                , :VARC-FEALTREG                                   03889014
JPC@3                , :VARC-FEULMOD                                    03890014
JPC@3                , :VARC-HORULMOD                                   03891014
JPC@3                , :VARC-NUMTER                                     03892014
JPC@3                , :VARC-USUARIO                                    03893014
JPC@3                , :VARC-FILLER                                     03894014
JPC@3                , :VARC-CTAVAL20                                   03895014
      *@ZAL-INI                                                         03896014
JPC@3 *              , :VARC-NUMMAN                                     03897014
                     , :VARC-GRUPO-CTAS                                 03898014
      *@ZAL-FIN                                                         03899014
JPC@3                , :VARC-INDIMP                                     03900014
JPC@3                , :VARC-INDSAB                                     03901014
JPC@3             FROM  VLDTARC                                         03902014
JPC@3            WHERE  VARC_CUENTA  = :VARC-CUENTA                     03903014
JPC@3      END-EXEC                                                     03904014
JPC@3 *                                                                 03905014
JPC@3      MOVE SQLCODE TO SQLCODE-AUX                                  03906014
JPC@3 *                                                                 03907014
JPC@3      EVALUATE TRUE                                                03908014
JPC@3         WHEN DB2-OK                                               03909014
JPC@3              IF VARC-SITUAC = 'X' OR 'B'                          03910014
JPC@3                 MOVE 'VLE0141'  TO CAA-COD-ERROR                  03911014
JPC@3                 MOVE -1         TO CTA0101L                       03912014
JPC@3                 PERFORM 3-FINAL                                   03913014
JPC@3              END-IF                                               03914014
JPC@3              INITIALIZE W-VLWCLOG0                                03915014
JPC@3                         LOGVLDTARC                                03916014
JPC@3              MOVE 'VLDTARC'             TO  VL7LOG-TABLA          03917014
JPC@3              MOVE 'SELECT'              TO  VL7LOG-OPERACION      03918014
JPC@3              MOVE LENGTH OF DCLVLDTARC  TO  VL7LOG-REGISTRO-LEN   03919014
JPC@3              MOVE DCLVLDTARC            TO  LOGVLDTARC            03920014
JPC@3              MOVE LOGVLDTARC            TO  VL7LOG-REGISTRO-TEXT  03921014
JPC@3              PERFORM LLAMAR-VL7CRLOG                              03922014
JPC@3                 THRU LLAMAR-VL7CRLOG-FIN                          03923014
JPC@3         WHEN OTHER                                                03924014
JPC@3              MOVE 'SELECT'      TO  ABC-REFERENCIA                03925014
JPC@3              MOVE 'VLDTARC'     TO  ABC-OBJETO-ERROR              03926014
JPC@3              PERFORM 999-ABEND-DB2                                03927014
JPC@3      END-EVALUATE.                                                03928014
JPC@3 *    GUARDA CUENTA PRINCIPAL PARA LUEGO RETORNAR DATOS A VARIABLE 03929014
JPC@3      MOVE END0101I              TO END0101X                       03930014
JPC@3      MOVE CEN0101I              TO CEN0101X                       03931014
JPC@3      MOVE DGT0101I              TO DGT0101X                       03932014
JPC@3      MOVE CTA0101I              TO CTA0101X                       03933014
JPC@3      MOVE DG20101I              TO DG20101X                       03934014
JPC@3 *                                                                 03935014
JPC@3      MOVE VARC-CTAVAL20 (01:04) TO END0101I                       03936014
JPC@3      MOVE VARC-CTAVAL20 (05:04) TO CEN0101I                       03937014
JPC@3      MOVE VARC-CTAVAL20 (09:02) TO DGT0101I                       03938014
JPC@3      MOVE VARC-CTAVAL20 (13:07) TO CTA0101I                       03939014
JPC@3      MOVE VARC-CTAVAL20 (20:01) TO DG20101I                       03940014
JPC@3 *                                                                 03941014
JPC@3      IF VARC-FILLER (11:02) = '91'                                03942014
JPC@3         INITIALIZE                 W-BGECMDC                      03943014
JPC@3         MOVE VARC-FILLER (01:4) TO MDC-ENTIDAD                    03944014
JPC@3         MOVE VARC-FILLER (05:4) TO MDC-CENTRO-ALTA                03945014
JPC@3         MOVE VARC-FILLER (11:2) TO MDC-CUENTA(1:2)                03946014
JPC@3         MOVE VARC-FILLER (13:8) TO MDC-CUENTA(3:8)                03947014
JPC@3 *                                                                 03948014
JPC@3         EXEC CICS                                                 03949014
JPC@3              LINK PROGRAM  (BG2CMDC0)                             03950014
JPC@3                   COMMAREA (BGECMDC)                              03951014
JPC@3         END-EXEC                                                  03952014
JPC@3 *                                                                 03953014
JPC@3         IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                      03954014
JPC@3            MOVE 'ERROR EN BGECMDC0'  TO ABC-REFERENCIA            03955014
JPC@3            MOVE 'BG2CMDC0'           TO ABC-OBJETO-ERROR          03956014
JPC@3            PERFORM 999-ABEND-CICS                                 03957014
JPC@3         END-IF                                                    03958014
JPC@3 *                                                                 03959014
JPC@3         EVALUATE MDC-CODERR                                       03960014
JPC@3             WHEN SPACES                                           03961014
JPC@3                  IF MDC-SALDO-DISPON NOT = ZEROS                  03962014
JPC@3                     MOVE MDC-SALDO-DISPON       TO W-SDOECON-EDIT 03963014
JPC@3                     MOVE 'VLE2169'              TO CAA-COD-ERROR  03964014
JPC@3                     MOVE 'CTA-REGISTRO CON SAL' TO CAA-VAR1-ERROR 03965014
JPC@3                     MOVE 'DO                  ' TO CAA-VAR2-ERROR 03966014
JPC@3                     MOVE W-SDOECON-EDIT TO CAA-VAR2-ERROR (04:15) 03967014
JPC@3                     MOVE -1         TO CTA0101L                   03968014
JPC@3                     PERFORM 3-FINAL                               03969014
JPC@3                  END-IF                                           03970014
JPC@3             WHEN OTHER                                            03971014
JPC@3                  MOVE -1          TO NCC0101L                     03972014
JPC@3                  MOVE MDC-CODERR  TO CAA-COD-ERROR                03973014
JPC@3                  PERFORM 3-FINAL                                  03974014
JPC@3         END-EVALUATE                                              03975014
JPC@3      END-IF.                                                      03976014
JPC@3 *                                                                 03977014
JPC@3      MOVE VARC-CUENTA        TO VADT-CUENTA                       03978014
JPC@3      MOVE 8                  TO VADT-CLTITU                       03979014
JPC@3 *                                                                 03980014
JPC@3      INITIALIZE      VADT-NUMCLI                                  03981014
JPC@3 *                                                                 03982014
JPC@3      EXEC SQL                                                     03983014
JPC@3           SELECT  VADT_CUENTA                                     03984014
JPC@3                ,  VADT_NUMCLI                                     03985014
JPC@3                ,  VADT_CLTITU                                     03986014
JPC@3                ,  VADT_NUMDOM                                     03987014
JPC@3                ,  VADT_ADMIN                                      03988014
JPC@3                ,  VADT_FEVENCTO                                   03989014
JPC@3                ,  VADT_FEALTREG                                   03990014
JPC@3                ,  VADT_FEULMOD                                    03991014
JPC@3                ,  VADT_HORULMOD                                   03992014
JPC@3                ,  VADT_NUMTER                                     03993014
JPC@3                ,  VADT_USUARIO                                    03994014
JPC@3             INTO :VADT-CUENTA                                     03995014
JPC@3                , :VADT-NUMCLI                                     03996014
JPC@3                , :VADT-CLTITU                                     03997014
JPC@3                , :VADT-NUMDOM                                     03998014
JPC@3                , :VADT-ADMIN                                      03999014
JPC@3                , :VADT-FEVENCTO                                   04000014
JPC@3                , :VADT-FEALTREG                                   04001014
JPC@3                , :VADT-FEULMOD                                    04002014
JPC@3                , :VADT-HORULMOD                                   04003014
JPC@3                , :VADT-NUMTER                                     04004014
JPC@3                , :VADT-USUARIO                                    04005014
JPC@3             FROM  VLDTADT                                         04006014
JPC@3            WHERE  VADT_CUENTA  = :VADT-CUENTA                     04007014
JPC@3              AND  VADT_CLTITU  = :VADT-CLTITU                     04008014
JPC@3              AND  VADT_NUMCLI >= :VADT-NUMCLI                     04009014
JPC@3      END-EXEC                                                     04010014
JPC@3 *                                                                 04011014
JPC@3      MOVE SQLCODE TO SQLCODE-AUX                                  04012014
JPC@3 *                                                                 04013014
JPC@3      EVALUATE TRUE                                                04014014
JPC@3         WHEN DB2-OK                                               04015014
JPC@3         WHEN DB2-DUPLINE                                          04016014
JPC@3              MOVE 'VLE2087'  TO CAA-COD-ERROR                     04017014
JPC@3              MOVE -1         TO CTA0101L                          04018014
JPC@3              PERFORM 3-FINAL                                      04019014
JPC@3         WHEN DB2-NOTFND                                           04020014
JPC@3              CONTINUE                                             04021014
JPC@3         WHEN OTHER                                                04022014
JPC@3              MOVE 'SELECT'      TO  ABC-REFERENCIA                04023014
JPC@3              MOVE 'VLDTADT'     TO  ABC-OBJETO-ERROR              04024014
JPC@3              PERFORM 999-ABEND-DB2                                04025014
JPC@3      END-EVALUATE.                                                04026014
JPC@3 *                                                                 04027014
JPC@3      INITIALIZE W-VLWCLOG0                                        04028014
JPC@3      MOVE 'VLDTADT'             TO  VL7LOG-TABLA                  04029014
JPC@3      MOVE 'SELECT'              TO  VL7LOG-OPERACION              04030014
JPC@3      MOVE LENGTH OF DCLVLDTADT  TO  VL7LOG-REGISTRO-LEN           04031014
JPC@3      MOVE DCLVLDTADT            TO  LOGVLDTADT                    04032014
JPC@3      MOVE LOGVLDTADT            TO  VL7LOG-REGISTRO-TEXT          04033014
JPC@3      PERFORM LLAMAR-VL7CRLOG                                      04034014
JPC@3         THRU LLAMAR-VL7CRLOG-FIN                                  04035014
JPC@3 *                                                                 04036014
JPC@3 *SE PERMITE INACTIVAR LA CUENTA SI NO TIENE SALDO                 04037014
JPC@3      MOVE VARC-CUENTA     TO VADS-CUENTA                          04038014
JPC@3      MOVE ZEROES          TO VADS-ISIN                            04039014
JPC@3      MOVE SPACES          TO VADS-PAVAL                           04040014
JPC@3                              VADS-VALOR                           04041014
JPC@3                              VADS-TIPREG                          04042014
JPC@3                              VADS-NUMGRUN                         04043014
JPC@3      MOVE ZEROS           TO VADS-DEPOS                           04044014
JPC@3                              VADS-COMPR                           04045014
JPC@3                              VADS-SUSCR                           04046014
JPC@3                              VADS-VENTA                           04047014
JPC@3                              VADS-ORDVE                           04048014
JPC@3                              VADS-BLOQ                            04049014
JPC@3 *                                                                 04050014
JPC@3      EXEC SQL                                                     04051014
JPC@3           SELECT  VADS_DEPOS                                      04052014
JPC@3                ,  VADS_COMPR                                      04053014
JPC@3                ,  VADS_SUSCR                                      04054014
JPC@3                ,  VADS_VENTA                                      04055014
JPC@3                ,  VADS_ORDVE                                      04056014
JPC@3                ,  VADS_BLOQ                                       04057014
JPC@3             INTO :VADS-DEPOS                                      04058014
JPC@3                , :VADS-COMPR                                      04059014
JPC@3                , :VADS-SUSCR                                      04060014
JPC@3                , :VADS-VENTA                                      04061014
JPC@3                , :VADS-ORDVE                                      04062014
JPC@3                , :VADS-BLOQ                                       04063014
JPC@3             FROM  VLDTADS                                         04064014
JPC@3            WHERE  VADS_CUENTA   = :VADS-CUENTA                    04065014
JPC@3              AND  VADS_PAVAL   >= :VADS-PAVAL                     04066014
JPC@3              AND  VADS_VALOR   >= :VADS-VALOR                     04067014
JPC@3              AND  VADS_ISIN    >= :VADS-ISIN                      04068014
JPC@3              AND  VADS_TIPREG  >= :VADS-TIPREG                    04069014
JPC@3              AND  VADS_NUMGRUN >= :VADS-NUMGRUN                   04070014
JPC@3              AND (VADS_DEPOS   > :VADS-DEPOS                      04071014
JPC@3               OR  VADS_COMPR   > :VADS-COMPR                      04072014
JPC@3               OR  VADS_SUSCR   > :VADS-SUSCR                      04073014
JPC@3               OR  VADS_VENTA   > :VADS-VENTA                      04074014
JPC@3               OR  VADS_ORDVE   > :VADS-ORDVE                      04075014
JPC@3               OR  VADS_BLOQ    > :VADS-BLOQ  )                    04076014
JPC@3      END-EXEC                                                     04077014
JPC@3 *                                                                 04078014
JPC@3      MOVE SQLCODE TO SQLCODE-AUX                                  04079014
JPC@3 *                                                                 04080014
JPC@3      EVALUATE TRUE                                                04081014
JPC@3         WHEN DB2-OK                                               04082014
JPC@3         WHEN DB2-DUPLINE                                          04083014
JPC@3              MOVE 'VLE0028'  TO CAA-COD-ERROR                     04084014
JPC@3              MOVE -1         TO CTA0101L                          04085014
JPC@3              PERFORM 3-FINAL                                      04086014
JPC@3         WHEN DB2-NOTFND                                           04087014
JPC@3              CONTINUE                                             04088014
JPC@3         WHEN OTHER                                                04089014
JPC@3              MOVE 'SELECT'      TO  ABC-REFERENCIA                04090014
JPC@3              MOVE 'VLDTADS'     TO  ABC-OBJETO-ERROR              04091014
JPC@3              PERFORM 999-ABEND-DB2                                04092014
JPC@3      END-EVALUATE.                                                04093014
JPC@3 *                                                                 04094014
JPC@3      INITIALIZE W-VLWCLOG0                                        04095014
JPC@3                 LOGVLDTADS                                        04096014
JPC@3      MOVE 'VLDTADS'             TO  VL7LOG-TABLA                  04097014
JPC@3      MOVE 'SELECT'              TO  VL7LOG-OPERACION              04098014
JPC@3      MOVE LENGTH OF DCLVLDTADS  TO  VL7LOG-REGISTRO-LEN           04099014
JPC@3      MOVE DCLVLDTADS            TO  LOGVLDTADS                    04100014
JPC@3      MOVE LOGVLDTADS            TO  VL7LOG-REGISTRO-TEXT          04101014
JPC@3      PERFORM LLAMAR-VL7CRLOG                                      04102014
JPC@3         THRU LLAMAR-VL7CRLOG-FIN                                  04103014
JPC@3      MOVE VARC-CUENTA     TO VDET-CTAVAL                          04104014
JPC@3 *                                                                 04105014
JPC@3      PERFORM ABRIR-CURSOR-DET                                     04106014
JPC@3         THRU ABRIR-CURSOR-DET-EXIT.                               04107014
JPC@3 *                                                                 04108014
JPC@3      MOVE 0 TO SW-DET.                                            04109014
JPC@3      PERFORM FETCH-DET                                            04110014
JPC@3         THRU FETCH-DET-EXIT.                                      04111014
JPC@3 *                                                                 04112014
JPC@3      IF DB2-NOTFND                                                04113014
JPC@3         MOVE 1 TO SW-DET                                          04114014
JPC@3      END-IF.                                                      04115014
JPC@3 *                                                                 04116014
JPC@3      PERFORM TRATAR-DET                                           04117014
JPC@3         THRU TRATAR-DET-EXIT                                      04118014
JPC@3      UNTIL SW-DET = 1.                                            04119014
JPC@3 *                                                                 04120014
JPC@3      PERFORM CERRAR-CURSOR-DET                                    04121014
JPC@3         THRU CERRAR-CURSOR-DET-EXIT.                              04122014
JPC@3 *                                                                 04123014
JPC@3      MOVE VARC-CUENTA     TO VTRA-CUENTA                          04124014
JPC@3 *                                                                 04125014
JPC@3      EXEC SQL                                                     04126014
JPC@3           SELECT  VTRA_REFER                                      04127014
JPC@3             INTO :VTRA-REFER                                      04128014
JPC@3             FROM  VLDTTRA                                         04129014
JPC@3            WHERE  VTRA_CUENTA   = :VTRA-CUENTA                    04130014
JPC@3              AND  VTRA_SITUAC  IN ('OP','PL','PD', 'OE', 'PG')    04131014
JPC@3      END-EXEC                                                     04132014
JPC@3 *                                                                 04133014
JPC@3      MOVE SQLCODE TO SQLCODE-AUX                                  04134014
JPC@3 *                                                                 04135014
JPC@3      EVALUATE TRUE                                                04136014
JPC@3          WHEN DB2-OK                                              04137014
JPC@3          WHEN DB2-DUPLINE                                         04138014
JPC@3               MOVE 'VLE2104'  TO CAA-COD-ERROR                    04139014
JPC@3               MOVE -1         TO CTA0101L                         04140014
JPC@3               PERFORM 3-FINAL                                     04141014
JPC@3          WHEN DB2-NOTFND                                          04142014
JPC@3               CONTINUE                                            04143014
JPC@3          WHEN OTHER                                               04144014
JPC@3               MOVE 'SELECT'      TO  ABC-REFERENCIA               04145014
JPC@3               MOVE 'VLDTTRA'     TO  ABC-OBJETO-ERROR             04146014
JPC@3               PERFORM 999-ABEND-DB2                               04147014
JPC@3      END-EVALUATE.                                                04148014
JPC@3 *                                                                 04149014
JPC@3      MOVE VARC-CUENTA            TO LARC-CUENTA                   04150014
JPC@3      PERFORM SELUND-VLDTARC                                       04151014
JPC@3         THRU SELUND-VLDTARC-FIN                                   04152014
JPC@3 *                                                                 04153014
JPC@3      MOVE 'B'               TO  VARC-SITUAC                       04154014
JPC@3 *                                                                 04155014
JPC@3      MOVE CAA-FECHA-OPER    TO  VARC-FEULMOD                      04156014
JPC@3      MOVE CAA-HORA-TRANS    TO  VARC-HORULMOD                     04157014
JPC@3      MOVE CAA-TERMINAL      TO  VARC-NUMTER                       04158014
JPC@3      MOVE CAA-USERID        TO  VARC-USUARIO                      04159014
JPC@3 *                                                                 04160014
JPC@3      EXEC SQL                                                     04161014
JPC@3           UPDATE VLDTARC                                          04162014
JPC@3              SET VARC_SITUAC      = :VARC-SITUAC                  04163014
JPC@3                , VARC_FEULMOD     = :VARC-FEULMOD                 04164014
JPC@3                , VARC_HORULMOD    = :VARC-HORULMOD                04165014
JPC@3                , VARC_NUMTER      = :VARC-NUMTER                  04166014
JPC@3                , VARC_USUARIO     = :VARC-USUARIO                 04167014
JPC@3            WHERE VARC_CUENTA = :VARC-CUENTA                       04168014
JPC@3      END-EXEC                                                     04169014
JPC@3 *                                                                 04170014
JPC@3      MOVE SQLCODE TO SQLCODE-AUX                                  04171014
JPC@3 *                                                                 04172014
JPC@3      EVALUATE TRUE                                                04173014
JPC@3         WHEN DB2-OK                                               04174014
JPC@3              INITIALIZE W-VLWCLOG0                                04175014
JPC@3                         LOGVLDTARC                                04176014
JPC@3              MOVE 'VLDTARC'             TO  VL7LOG-TABLA          04177014
JPC@3              MOVE 'UPDATE'              TO  VL7LOG-OPERACION      04178014
JPC@3              MOVE LENGTH OF DCLVLDTARC  TO  VL7LOG-REGISTRO-LEN   04179014
JPC@3              MOVE VARC-SITUAC           TO  LARC-SITUAC           04180014
JPC@3              MOVE VARC-FEULMOD          TO  LARC-FEULMOD          04181014
JPC@3              MOVE VARC-HORULMOD         TO  LARC-HORULMOD         04182014
JPC@3              MOVE VARC-NUMTER           TO  LARC-NUMTER           04183014
JPC@3              MOVE VARC-USUARIO          TO  LARC-USUARIO          04184014
JPC@3              MOVE VARC-CUENTA           TO  LARC-CUENTA           04185014
JPC@3              MOVE LOGVLDTARC            TO  VL7LOG-REGISTRO-TEXT  04186014
JPC@3              PERFORM LLAMAR-VL7CRLOG                              04187014
JPC@3                 THRU LLAMAR-VL7CRLOG-FIN                          04188014
JPC@3         WHEN OTHER                                                04189014
JPC@3              MOVE 'UPDATE-BAJA' TO  ABC-REFERENCIA                04190014
JPC@3              MOVE 'VLDTARC-P2'  TO  ABC-OBJETO-ERROR              04191014
JPC@3              PERFORM 999-ABEND-DB2                                04192014
JPC@3      END-EVALUATE                                                 04193014
JPC@3 *                                                                 04194014
JPC@3 ******************************************************************04195014
JPC@3 * RUTINA PARA DAR BAJA, DESVINCULA LA CTA ECONOMICA              *04196014
JPC@3 * CON LA CUENTA VALOR.                                           *04197014
JPC@3 ******************************************************************04198014
JPC@3 *                                                                 04199014
JPC@3      IF VARC-FILLER (01:20) = VARC-FILLER (21:20)                 04200014
JPC@3         INITIALIZE                     BGECMSC                    04201014
JPC@3         MOVE '2'                   TO MSC-FUNCION                 04202014
JPC@3         MOVE VARC-FILLER (11:2)    TO MSC-CUENTA (01:02)          04203014
JPC@3         MOVE VARC-FILLER (13:8)    TO MSC-CUENTA (03:08)          04204014
JPC@3         MOVE VARC-FILLER (01:4)    TO MSC-ENTIDAD                 04205014
JPC@3         MOVE VARC-FILLER (05:4)    TO MSC-CENTRO-ALTA             04206014
JPC@3         PERFORM RUTINA-BGECMSC                                    04207014
JPC@3            THRU RUTINA-BGECMSC-F                                  04208014
JPC@3      ELSE                                                         04209014
JPC@3         INITIALIZE                     BGECMSC                    04210014
JPC@3         MOVE '2'                   TO MSC-FUNCION                 04211014
JPC@3         MOVE VARC-FILLER (11:2)    TO MSC-CUENTA (01:02)          04212014
JPC@3         MOVE VARC-FILLER (13:8)    TO MSC-CUENTA (03:08)          04213014
JPC@3         MOVE VARC-FILLER (01:4)    TO MSC-ENTIDAD                 04214014
JPC@3         MOVE VARC-FILLER (05:4)    TO MSC-CENTRO-ALTA             04215014
JPC@3         PERFORM RUTINA-BGECMSC                                    04216014
JPC@3            THRU RUTINA-BGECMSC-F                                  04217014
JPC@3 *                                                                 04218014
JPC@3         INITIALIZE                     BGECMSC                    04219014
JPC@3         MOVE '2'                   TO MSC-FUNCION                 04220014
JPC@3         MOVE VARC-FILLER (31:2)    TO MSC-CUENTA (01:02)          04221014
JPC@3         MOVE VARC-FILLER (33:8)    TO MSC-CUENTA (03:08)          04222014
JPC@3         MOVE VARC-FILLER (21:4)    TO MSC-ENTIDAD                 04223014
JPC@3         MOVE VARC-FILLER (25:4)    TO MSC-CENTRO-ALTA             04224014
JPC@3         PERFORM RUTINA-BGECMSC                                    04225014
JPC@3            THRU RUTINA-BGECMSC-F                                  04226014
JPC@3      END-IF.                                                      04227014
JPC@3 *                                                                 04228014
JPC@3      PERFORM BAJA-INTERVINIENTE                                   04229014
JPC@3         THRU BAJA-INTERVINIENTE-F                                 04230014
JPC@3 *                                                                 04231014
JPC@3      MOVE END0101X TO END0101I.                                   04232014
JPC@3      MOVE CEN0101X TO CEN0101I.                                   04233014
JPC@3      MOVE DGT0101X TO DGT0101I.                                   04234014
JPC@3      MOVE CTA0101X TO CTA0101I.                                   04235014
JPC@3      MOVE DG20101X TO DG20101I.                                   04236014
JPC@3 *----------------------*                                          04237014
JPC@3  25-INACTIVAR-PORTA-FIN.                                          04238014
JPC@3 *----------------------*                                          04239014
JPC@3      EXIT.                                                        04240014
LERS   ABRIR-CURSOR-DET.                                                04241014
 09   *----------------*                                                04242014
 07                                                                     04243014
2201       EXEC SQL                                                     04244014
 |            OPEN VLDCADET                                             04245014
 |         END-EXEC.                                                    04246014
 |                                                                      04247014
 |         IF SQLCODE NOT = ZEROS                                       04248014
 |            INITIALIZE   QGECABC                                      04249014
 |            MOVE 'OPEN'        TO  ABC-REFERENCIA                     04250014
 |            MOVE 'VLDTDET'     TO  ABC-OBJETO-ERROR                   04251014
 |            PERFORM 999-ABEND-DB2                                     04252014
 |         END-IF.                                                      04253014
 |                                                                      04254014
 |     ABRIR-CURSOR-DET-EXIT.                                           04255014
 |    *---------------------*                                           04256014
 |         EXIT.                                                        04257014
 |                                                                      04258014
 |     FETCH-DET.                                                       04259014
 |    *---------*                                                       04260014
 |         EXEC SQL                                                     04261014
 |              FETCH  VLDCADET                                         04262014
JPC@1 *         INTO  :DCLVLDTDET                                       04263014
JPC@1           INTO   :VDET-FECHOP                                     04264014
                     , :VDET-PAVAL                                      04265014
                     , :VDET-VALOR                                      04266014
                     , :VDET-ISIN                                       04267014
                     , :VDET-FORMAT                                     04268014
                     , :VDET-CTAVAL                                     04269014
                     , :VDET-CLAREG                                     04270014
                     , :VDET-REFER                                      04271014
                     , :VDET-DATOS-DETAL                                04272014
                     , :VDET-FEALTREG                                   04273014
                     , :VDET-FEULMOD                                    04274014
                     , :VDET-HORULMOD                                   04275014
                     , :VDET-NUMTER                                     04276014
                     , :VDET-USUARIO                                    04277014
 |         END-EXEC.                                                    04278014
 |                                                                      04279014
 |         MOVE SQLCODE TO SQLCODE-AUX                                  04280014
 |                                                                      04281014
 |         EVALUATE TRUE                                                04282014
 |            WHEN DB2-OK                                               04283014
 |                 PERFORM CHECK-OP-FINA                                04284014
 |                    THRU CHECK-OP-FINA-EXIT                           04285014
 |                    IF VOPE-SITUAC(1:1) = '1' OR                      04286014
 |                       VOPE-SITUAC = 'PA'                             04287014
 |                       MOVE 'VLE2112'     TO CAA-COD-ERROR            04288014
 |                       MOVE VOPE-PAVAL    TO CAA-VAR1-ERROR(01:03)    04289014
 |                       MOVE VOPE-VALOR    TO CAA-VAR1-ERROR(04:08)    04290014
 |                       MOVE VOPE-ISIN     TO CAA-VAR1-ERROR(12:01)    04291014
 |                       MOVE VOPE-FORMAT   TO CAA-VAR2-ERROR(01:02)    04292014
 |                       MOVE VOPE-FECHOP   TO CAA-VAR2-ERROR(04:08)    04293014
 |                       MOVE -1            TO CTA0101L                 04294014
 |                       PERFORM 3-FINAL                                04295014
 |                     END-IF                                           04296014
 |                                                                      04297014
 |            WHEN DB2-NOTFND                                           04298014
 |                 CONTINUE                                             04299014
 |                                                                      04300014
 |            WHEN OTHER                                                04301014
 |                 INITIALIZE   QGECABC                                 04302014
 |                 MOVE 'FETCH'       TO  ABC-REFERENCIA                04303014
 |                 MOVE 'VLDTDET'     TO  ABC-OBJETO-ERROR              04304014
 |                 PERFORM 999-ABEND-DB2                                04305014
 |                                                                      04306014
 |         END-EVALUATE.                                                04307014
 |                                                                      04308014
 |     FETCH-DET-EXIT.                                                  04309014
 |    *--------------*                                                  04310014
 |         EXIT.                                                        04311014
 |                                                                      04312014
 |     TRATAR-DET.                                                      04313014
 |    *----------*                                                      04314014
 |         PERFORM FETCH-DET                                            04315014
 |            THRU FETCH-DET-EXIT.                                      04316014
 |                                                                      04317014
 |         IF DB2-NOTFND                                                04318014
 |            MOVE 1 TO SW-DET                                          04319014
 |         END-IF.                                                      04320014
 |                                                                      04321014
 |     TRATAR-DET-EXIT.                                                 04322014
 |    *---------------*                                                 04323014
 |         EXIT.                                                        04324014
 |                                                                      04325014
 |     CERRAR-CURSOR-DET.                                               04326014
 |    *-----------------*                                               04327014
 |         EXEC SQL                                                     04328014
 |              CLOSE VLDCADET                                          04329014
 |         END-EXEC.                                                    04330014
 |                                                                      04331014
 |         IF SQLCODE NOT = ZEROS                                       04332014
 |            INITIALIZE   QGECABC                                      04333014
 |            MOVE 'CLOSE'       TO  ABC-REFERENCIA                     04334014
 |            MOVE 'VLDTDET'     TO  ABC-OBJETO-ERROR                   04335014
 |            PERFORM 999-ABEND-DB2                                     04336014
 |         END-IF.                                                      04337014
 |                                                                      04338014
 |     CERRAR-CURSOR-DET-EXIT.                                          04339014
 |    *----------------------*                                          04340014
LERS       EXIT.                                                        04341014
                                                                        04342014
       CHECK-OP-FINA.                                                   04343014
      *-------------*                                                   04344014
LERS       MOVE VDET-PAVAL      TO VOPE-PAVAL                           04345014
09         MOVE VDET-VALOR      TO VOPE-VALOR                           04346014
07         MOVE VDET-ISIN       TO VOPE-ISIN                            04347014
2001       MOVE VDET-FECHOP     TO VOPE-FECHOP                          04348014
 |         MOVE VDET-FORMAT     TO VOPE-FORMAT                          04349014
 |                                                                      04350014
 |         EXEC SQL                                                     04351014
 |              SELECT  VOPE_PAVAL                                      04352014
                     ,  VOPE_VALOR                                      04353014
                     ,  VOPE_ISIN                                       04354014
                     ,  VOPE_FORMAT                                     04355014
                     ,  VOPE_FECHOP                                     04356014
                     ,  VOPE_SITUAC                                     04357014
                  INTO :VOPE-PAVAL                                      04358014
                     , :VOPE-VALOR                                      04359014
                     , :VOPE-ISIN                                       04360014
                     , :VOPE-FORMAT                                     04361014
                     , :VOPE-FECHOP                                     04362014
                     , :VOPE-SITUAC                                     04363014
 |                FROM  VLDTOPE                                         04364014
 |               WHERE  VOPE_PAVAL  = :VOPE-PAVAL                       04365014
 |                 AND  VOPE_VALOR  = :VOPE-VALOR                       04366014
 |                 AND  VOPE_ISIN   = :VOPE-ISIN                        04367014
 |                 AND  VOPE_FECHOP = :VOPE-FECHOP                      04368014
 |                 AND  VOPE_FORMAT = :VOPE-FORMAT                      04369014
 |         END-EXEC                                                     04370014
 |                                                                      04371014
 |         MOVE SQLCODE TO SQLCODE-AUX                                  04372014
 |                                                                      04373014
 |         EVALUATE TRUE                                                04374014
 |            WHEN DB2-OK                                               04375014
 |                 CONTINUE                                             04376014
 |                                                                      04377014
 |            WHEN DB2-NOTFND                                           04378014
 |                 MOVE 'VLE2112'            TO CAA-COD-ERROR           04379014
 |                 MOVE VOPE-PAVAL           TO CAA-VAR1-ERROR(01:03)   04380014
 |                 MOVE VOPE-VALOR           TO CAA-VAR1-ERROR(04:08)   04381014
 |                 MOVE VOPE-ISIN            TO CAA-VAR1-ERROR(12:01)   04382014
 |                 MOVE VOPE-FORMAT          TO CAA-VAR2-ERROR(01:02)   04383014
 |                 MOVE '99999999'           TO CAA-VAR2-ERROR(04:08)   04384014
 |                 MOVE -1         TO CTA0101L                          04385014
 |                 PERFORM 3-FINAL                                      04386014
 |                                                                      04387014
 |            WHEN OTHER                                                04388014
 |                 MOVE 'SELECT'      TO  ABC-REFERENCIA                04389014
 |                 MOVE 'VLDTOPE'     TO  ABC-OBJETO-ERROR              04390014
 |                 PERFORM 999-ABEND-DB2                                04391014
 |                                                                      04392014
 |         END-EVALUATE.                                                04393014
LERS                                                                    04394014
       CHECK-OP-FINA-EXIT.                                              04395014
      *------------------*                                              04396014
           EXIT.                                                        04397014
                                                                        04398014
       VALIDAR-CAMPOS.                                                  04399014
      *                                                                *04400014
      ****** VALIDAMOS TITULAR *****                                    04401014
      *                                                                *04402014
           MOVE 'NO'    TO         SW-CTA-ESPECIAL                      04403014
      *                                                                *04404014
           IF TIT0101I = SPACES AND PF3                                 04405014
              MOVE 'SI'        TO SW-PERSONAS                           04406014
              PERFORM 3-FINAL                                           04407014
           ELSE                                                         04408014
              MOVE 'NO'        TO SW-PERSONAS                           04409014
           END-IF                                                       04410014
      *                                                                *04411014
           IF TIT0101I IS NOT NUMERIC                                   04412014
              MOVE 'VLE0384'   TO CAA-COD-ERROR                         04413014
              MOVE -1          TO TIT0101L                              04414014
              MOVE SPACES      TO NOM0101O                              04415014
              PERFORM 3-FINAL                                           04416014
           END-IF                                                       04417014
      *                                                                *04418014
      ***                                                            ***04419014
      * BUSCAMOS TODOS LOS DATOS DEL TITULAR                           *04420014
      ***                                                            ***04421014
      *                                                                *04422014
           PERFORM BUSCAR-TITULAR                                       04423014
              THRU BUSCAR-TITULAR-FIN                                   04424014
      *                                                                *04425014
JPC@7 ****                                                           ***04426014
JPC@7 *    VALIDAMOS SITUACION FATCA DEL CLIENTE.                      *04427014
JPC@7 ****                                                           ***04428014
JPC@7 *                                                                *04429014
JPC@7      INITIALIZE                REG-VLWC0088.                      04430014
JPC@7      MOVE '1'              TO  VL088-OPCION.                      04431014
JPC@7      IF MSB-COMM = 'M'                                            04432014
JPC@7         MOVE CTA0101I      TO  VL088-CUENTA                       04433014
JPC@7      ELSE                                                         04434014
JPC@7         MOVE SPACES        TO  VL088-CUENTA                       04435014
JPC@7      END-IF                                                       04436014
JPC@7      MOVE TIT0101I         TO  VL088-CODCLI.                      04437014
JPC@4      IF W520-SUJGRUP = 'F'                                        04438014
JPC@4         MOVE W520-PECNACIO TO  VL088-PECNACIO                     04439014
JPC@4      ELSE                                                         04440014
JPC@4         MOVE W520-PECNARES TO  VL088-PECNACIO                     04441014
JPC@4      END-IF.                                                      04442014
JPC@7      MOVE W520-SUJGRUP     TO  VL088-SUJGRUP.                     04443014
JPC@7 *                                                                *04444014
JPC@7      EXEC CICS                                                    04445014
JPC@7           LINK PROGRAM  (VL7C0088)                                04446014
JPC@7                COMMAREA (REG-VLWC0088)                            04447014
JPC@7      END-EXEC.                                                    04448014
JPC@7 *                                                                 04449014
JPC@7      IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         04450014
JPC@7         MOVE 'ERROR EN VL7C0088'  TO ABC-REFERENCIA               04451014
JPC@7         MOVE 'VL7C0088'           TO ABC-OBJETO-ERROR             04452014
JPC@7         PERFORM 999-ABEND-CICS                                    04453014
JPC@7      END-IF.                                                      04454014
JPC@7 *                                                                *04455014
JPC@7      EVALUATE VL088-COD-RETORNO                                   04456014
JPC@7          WHEN '00'                                                04457014
JPC@7               IF VL088-INDMARCA NOT = 'S' AND 'R'                 04458014
JPC@7                  MOVE VL088-COD-ERROR-DEV TO CAA-COD-ERROR        04459014
JPC@7                  MOVE VL088-VAR1-ERROR    TO CAA-VAR1-ERROR       04460014
JPC@7                  MOVE VL088-VAR2-ERROR    TO CAA-VAR2-ERROR       04461014
JPC@7               END-IF                                              04462014
JPC@7          WHEN OTHER                                               04463014
JPC@7               MOVE -1                   TO TIT0101L               04464014
JPC@7               MOVE VL088-COD-ERROR-DEV  TO CAA-COD-ERROR          04465014
JPC@7               MOVE VL088-VAR1-ERROR     TO CAA-VAR1-ERROR         04466014
JPC@7               MOVE VL088-VAR2-ERROR     TO CAA-VAR2-ERROR         04467014
JPC@7               PERFORM 3-FINAL                                     04468014
JPC@7      END-EVALUATE.                                                04469014
JPC@7 *                                                                *04470014
      ****                                                           ***04471014
      ***                                                            ***04472014
      *    VALIDAMOS QUE SI EL TITULAR ES EMPLEADO, ENTONCES LA        *04473014
      *    TARIFA DEBE SER 99 (NO SE LE COBRARA COMISIONES)            *04474014
      ***     10-DIC-2001       JIPC                                 ***04475014
      *200512055-INI                                                   *04476014
JIPC  *    IF  W520-IRELPAT = 'E' AND                                   04477014
  |        IF (W520-IRELPAT = 'E' OR 'D' OR 'F' OR 'J') AND             04478014
  |   *200512055-FIN                                                   *04479014
  |           TAF0101I NOT = 99                                         04480014
  |           MOVE 'VLE2130'   TO CAA-COD-ERROR                         04481014
  |           MOVE -1          TO TAF0101L                              04482014
  |           PERFORM 3-FINAL                                           04483014
JIPC       END-IF                                                       04484014
      *                                                                *04485014
      ***                                                            ***04486014
      *        EL CAMPO ENTIDAD EN ALTAS PUEDE IR A BLANCOS            *04487014
      *        SE LE ASIGNA 0011 BANCO CONTINENTAL                     *04488014
      *MODIFICACION 17-07-1999.SE INCLUYE EL OPT-COMM = 'C' PARA QUE   *04489014
      *LEA DE LA TABLA VLDTXMI.                                        *04490014
      *    IF OPT-COMM = 'A'                                           *04491014
      ***                                                            ***04492014
      *                                                                *04493014
           IF OPT-COMM = 'A' OR                                         04494014
             (OPT-COMM = 'C' AND MSB-COMM = ' ')                        04495014
              IF ENT0101I = SPACES                                      04496014
JIPC  *(22-3-01) MOVE '0011'         TO VXMI-CODBE (SE CAMBIA A 0069)   04497014
                 MOVE '0069'         TO VXMI-CODBE                      04498014
                                        ENT0101-COMM                    04499014
                                        ENT0101O                        04500014
              ELSE                                                      04501014
                 MOVE ENT0101I     TO VXMI-CODBE                        04502014
                                      ENT0101-COMM                      04503014
              END-IF                                                    04504014
      *                                                                 04505014
              EXEC SQL                                                  04506014
JPC@1 *            SELECT  *                                            04507014
                   SELECT VXMI_CODBE                                    04508014
                        , VXMI_CODCLI                                   04509014
                        , VXMI_DENOM                                    04510014
                        , VXMI_NIF                                      04511014
                        , VXMI_DOMIC                                    04512014
                        , VXMI_LOCAL                                    04513014
                        , VXMI_CODPOS                                   04514014
                        , VXMI_CNAE                                     04515014
                        , VXMI_SUCVAL                                   04516014
                        , VXMI_NUMFAC                                   04517014
                        , VXMI_VALENT                                   04518014
                        , VXMI_CTAVAL                                   04519014
                        , VXMI_VALCER                                   04520014
                        , VXMI_MULPLA                                   04521014
                        , VXMI_RETEN                                    04522014
                        , VXMI_IVA                                      04523014
                        , VXMI_INCLUS                                   04524014
                        , VXMI_EXCLUS                                   04525014
                        , VXMI_PROVIS                                   04526014
                        , VXMI_FLISOP                                   04527014
                        , VXMI_LISENT                                   04528014
                        , VXMI_LISPAG                                   04529014
                        , VXMI_INCORP                                   04530014
                        , VXMI_CONTRT                                   04531014
                        , VXMI_CONTRT6                                  04532014
                        , VXMI_REF9                                     04533014
                        , VXMI_DELEGHAC                                 04534014
                        , VXMI_ADMINHAC                                 04535014
                        , VXMI_PRETELHAC                                04536014
                        , VXMI_TELEFHAC                                 04537014
                        , VXMI_APNOMHAC                                 04538014
                        , VXMI_LUNES                                    04539014
                        , VXMI_VIERNES                                  04540014
                        , VXMI_YAPRESEN                                 04541014
                        , VXMI_IMPRE1                                   04542014
                        , VXMI_IMPRE2                                   04543014
                        , VXMI_FILLER                                   04544014
                        , VXMI_LISCTIMP                                 04545014
                        , VXMI_CONTCTA                                  04546014
                        , VXMI_PASS1                                    04547014
                        , VXMI_PASS2                                    04548014
                        , VXMI_LISCTA                                   04549014
                        , VXMI_LISAGTES                                 04550014
                        , VXMI_LISREDUC                                 04551014
                        , VXMI_LISFESTI                                 04552014
                        , VXMI_LISMONED                                 04553014
                        , VXMI_LISCONTA                                 04554014
                        , VXMI_LISENT_1                                 04555014
                        , VXMI_LISCTA_SUC                               04556014
                        , VXMI_LISVALOR                                 04557014
                        , VXMI_CONT_REV                                 04558014
                        , VXMI_VALORACION                               04559014
                        , VXMI_LIS_EXTRJ                                04560014
                        , VXMI_FILLER1                                  04561014
                        , VXMI_APCTAOFI                                 04562014
                        , VXMI_TIPCUST                                  04563014
                        , VXMI_MANFIS                                   04564014
                        , VXMI_OPECUST                                  04565014
                        , VXMI_OPEBOLSA                                 04566014
                        , VXMI_AVISOS                                   04567014
                        , VXMI_CONPANT                                  04568014
                        , VXMI_COMCUST                                  04569014
                        , VXMI_IMPALT                                   04570014
                        , VXMI_CTACARGO                                 04571014
                        , VXMI_CTAABONO                                 04572014
                        , VXMI_CONTEN                                   04573014
                        , VXMI_CONTEV                                   04574014
                        , VXMI_CONTSN                                   04575014
                        , VXMI_CONTSV                                   04576014
                        , VXMI_LIS_RESTOS                               04577014
                        , VXMI_DIAS_LIMIT                               04578014
                        , VXMI_LIS_C_EXEN                               04579014
                        , VXMI_LIS_GJUD_BLO                             04580014
                        , VXMI_FEALTREG                                 04581014
                        , VXMI_FEULMOD                                  04582014
                        , VXMI_HORULMOD                                 04583014
                        , VXMI_NUMTER                                   04584014
                        , VXMI_USUARIO                                  04585014
                        , VXMI_FILLER2                                  04586014
JPC@1 *            INTO :DCLVLDTXMI                                     04587014
                   INTO  :VXMI-CODBE                                    04588014
                      ,  :VXMI-CODCLI                                   04589014
                      ,  :VXMI-DENOM                                    04590014
                      ,  :VXMI-NIF                                      04591014
                      ,  :VXMI-DOMIC                                    04592014
                      ,  :VXMI-LOCAL                                    04593014
                      ,  :VXMI-CODPOS                                   04594014
                      ,  :VXMI-CNAE                                     04595014
                      ,  :VXMI-SUCVAL                                   04596014
                      ,  :VXMI-NUMFAC                                   04597014
                      ,  :VXMI-VALENT                                   04598014
                      ,  :VXMI-CTAVAL                                   04599014
                      ,  :VXMI-VALCER                                   04600014
                      ,  :VXMI-MULPLA                                   04601014
                      ,  :VXMI-RETEN                                    04602014
                      ,  :VXMI-IVA                                      04603014
                      ,  :VXMI-INCLUS                                   04604014
                      ,  :VXMI-EXCLUS                                   04605014
                      ,  :VXMI-PROVIS                                   04606014
                      ,  :VXMI-FLISOP                                   04607014
                      ,  :VXMI-LISENT                                   04608014
                      ,  :VXMI-LISPAG                                   04609014
                      ,  :VXMI-INCORP                                   04610014
                      ,  :VXMI-CONTRT                                   04611014
                      ,  :VXMI-CONTRT6                                  04612014
                      ,  :VXMI-REF9                                     04613014
                      ,  :VXMI-DELEGHAC                                 04614014
                      ,  :VXMI-ADMINHAC                                 04615014
                      ,  :VXMI-PRETELHAC                                04616014
                      ,  :VXMI-TELEFHAC                                 04617014
                      ,  :VXMI-APNOMHAC                                 04618014
                      ,  :VXMI-LUNES                                    04619014
                      ,  :VXMI-VIERNES                                  04620014
                      ,  :VXMI-YAPRESEN                                 04621014
                      ,  :VXMI-IMPRE1                                   04622014
                      ,  :VXMI-IMPRE2                                   04623014
                      ,  :VXMI-FILLER                                   04624014
                      ,  :VXMI-LISCTIMP                                 04625014
                      ,  :VXMI-CONTCTA                                  04626014
                      ,  :VXMI-PASS1                                    04627014
                      ,  :VXMI-PASS2                                    04628014
                      ,  :VXMI-LISCTA                                   04629014
                      ,  :VXMI-LISAGTES                                 04630014
                      ,  :VXMI-LISREDUC                                 04631014
                      ,  :VXMI-LISFESTI                                 04632014
                      ,  :VXMI-LISMONED                                 04633014
                      ,  :VXMI-LISCONTA                                 04634014
                      ,  :VXMI-LISENT-1                                 04635014
                      ,  :VXMI-LISCTA-SUC                               04636014
                      ,  :VXMI-LISVALOR                                 04637014
                      ,  :VXMI-CONT-REV                                 04638014
                      ,  :VXMI-VALORACION                               04639014
                      ,  :VXMI-LIS-EXTRJ                                04640014
                      ,  :VXMI-FILLER1                                  04641014
                      ,  :VXMI-APCTAOFI                                 04642014
                      ,  :VXMI-TIPCUST                                  04643014
                      ,  :VXMI-MANFIS                                   04644014
                      ,  :VXMI-OPECUST                                  04645014
                      ,  :VXMI-OPEBOLSA                                 04646014
                      ,  :VXMI-AVISOS                                   04647014
                      ,  :VXMI-CONPANT                                  04648014
                      ,  :VXMI-COMCUST                                  04649014
                      ,  :VXMI-IMPALT                                   04650014
                      ,  :VXMI-CTACARGO                                 04651014
                      ,  :VXMI-CTAABONO                                 04652014
                      ,  :VXMI-CONTEN                                   04653014
                      ,  :VXMI-CONTEV                                   04654014
                      ,  :VXMI-CONTSN                                   04655014
                      ,  :VXMI-CONTSV                                   04656014
                      ,  :VXMI-LIS-RESTOS                               04657014
                      ,  :VXMI-DIAS-LIMIT                               04658014
                      ,  :VXMI-LIS-C-EXEN                               04659014
                      ,  :VXMI-LIS-GJUD-BLO                             04660014
                      ,  :VXMI-FEALTREG                                 04661014
                      ,  :VXMI-FEULMOD                                  04662014
                      ,  :VXMI-HORULMOD                                 04663014
                      ,  :VXMI-NUMTER                                   04664014
                      ,  :VXMI-USUARIO                                  04665014
                      ,  :VXMI-FILLER2                                  04666014
                   FROM  VLDTXMI                                        04667014
                   WHERE  VXMI_CODBE  = :VXMI-CODBE                     04668014
              END-EXEC                                                  04669014
                                                                        04670014
              MOVE SQLCODE TO SQLCODE-AUX                               04671014
                                                                        04672014
              EVALUATE TRUE                                             04673014
                  WHEN DB2-OK                                           04674014
                       MOVE VXMI-DENOM       TO NEN0101O                04675014
                  WHEN DB2-NOTFND                                       04676014
                       MOVE  'VLE1667'   TO  CAA-COD-ERROR              04677014
                       MOVE  -1          TO  ENT0101L                   04678014
                       PERFORM  3-FINAL                                 04679014
                  WHEN OTHER                                            04680014
                       MOVE 'SELECT'      TO  ABC-REFERENCIA            04681014
                       MOVE 'VLDTXMI'     TO  ABC-OBJETO-ERROR          04682014
                       PERFORM 999-ABEND-DB2                            04683014
              END-EVALUATE                                              04684014
      *                                                                 04685014
              INITIALIZE                    W-VLWCLOG0                  04686014
                                            LOGVLDTXMI                  04687014
              MOVE 'VLDTXMI'             TO VL7LOG-TABLA                04688014
              MOVE 'SELECT'              TO VL7LOG-OPERACION            04689014
              MOVE LENGTH OF DCLVLDTXMI  TO VL7LOG-REGISTRO-LEN         04690014
              MOVE DCLVLDTXMI            TO LOGVLDTXMI                  04691014
              MOVE LOGVLDTXMI            TO VL7LOG-REGISTRO-TEXT        04692014
              PERFORM LLAMAR-VL7CRLOG                                   04693014
                 THRU LLAMAR-VL7CRLOG-FIN                               04694014
                                                                        04695014
              IF VXMI-OPEBOLSA = 'S'                                    04696014
                 MOVE 'SI'               TO SW-OPERA-BOLSA              04697014
              END-IF                                                    04698014
              IF VXMI-COMCUST = 'S'                                     04699014
                 MOVE 'SI'               TO SW-CUSTODIA-AL-CLIENTE      04700014
                 MOVE VXMI-CODCLI        TO W-CLIENTE-CUSTODIO-N        04701014
                 IF W-CLIENTE-CUSTODIO = TIT0101I                       04702014
                    MOVE 'SI'            TO SW-ES-CTAVAL-CUS            04703014
                    IF VXMI-CTAVAL NOT = 0                              04704014
                       MOVE 'SI'         TO SW-YA-CTAVAL-CUS            04705014
                    END-IF                                              04706014
                 END-IF                                                 04707014
                 MOVE VXMI-CTACARGO      TO W-CTA-CAR-JUR               04708014
                 MOVE VXMI-CTAABONO      TO W-CTA-ABO-JUR               04709014
              ELSE                                                      04710014
                 MOVE 'NO'               TO SW-CUSTODIA-AL-CLIENTE      04711014
                 MOVE VXMI-CODCLI        TO W-CLIENTE-CUSTODIO-N        04712014
                 IF W-CLIENTE-CUSTODIO = TIT0101I                       04713014
                    MOVE 'SI'            TO SW-ES-CTAVAL-CUS            04714014
                    IF VXMI-CTAVAL NOT = 0                              04715014
                       MOVE 'SI'         TO SW-YA-CTAVAL-CUS            04716014
                    END-IF                                              04717014
                 END-IF                                                 04718014
                 IF VXMI-CTAVAL = ZEROES AND NOT ES-CTAVAL-CUS          04719014
                    MOVE  'VLE1731'      TO  CAA-COD-ERROR              04720014
                    MOVE  -1             TO  CTA0101L                   04721014
                    PERFORM  3-FINAL                                    04722014
                 ELSE                                                   04723014
                    IF NOT ES-CTAVAL-CUS                                04724014
                       PERFORM CTAVAL-CUSTODIO                          04725014
                          THRU CTAVAL-CUSTODIO-FIN                      04726014
                    END-IF                                              04727014
                 END-IF                                                 04728014
                 MOVE VXMI-CTACARGO      TO W-CTA-CAR-CUSTODIO NCC0101O 04729014
                 PERFORM OBTENER-MONEDA1                                04730014
                    THRU OBTENER-MONEDA1-FIN                            04731014
                 MOVE VXMI-CTAABONO      TO W-CTA-ABO-CUSTODIO NC20101O 04732014
                 PERFORM OBTENER-MONEDA2                                04733014
                    THRU OBTENER-MONEDA2-FIN                            04734014
              END-IF                                                    04735014
      *                                                                 04736014
              MOVE VXMI-TIPCUST          TO WXMI-TIPCUST                04737014
              MOVE VXMI-IMPALT           TO WXMI-IMPALT                 04738014
           END-IF                                                       04739014
                                                                        04740014
           IF MSB-COMM = 'M'                                            04741014
              MOVE ENT0101I             TO W-ENTIDAD                    04742014
              IF W-ENTIDAD NOT = VARC-CENTAD                            04743014
                 MOVE  'VLE1677'   TO  CAA-COD-ERROR                    04744014
                 MOVE  -1          TO  ENT0101L                         04745014
                 PERFORM  3-FINAL                                       04746014
              END-IF                                                    04747014
              IF NOT CUSTODIA-AL-CLIENTE AND                            04748014
                (W-CTA-CAR-CUSTODIO NOT = NCC0101I OR                   04749014
                 W-CTA-ABO-CUSTODIO NOT = NC20101I)                     04750014
                 MOVE W-CTA-CAR-CUSTODIO TO  NCC0101O                   04751014
                 MOVE W-CTA-ABO-CUSTODIO TO  NC20101O                   04752014
                 MOVE  'VLE1730'    TO  CAA-COD-ERROR                   04753014
                 MOVE  -1           TO  NCC0101L                        04754014
                 PERFORM  3-FINAL                                       04755014
              END-IF                                                    04756014
JIPC  * 16.12.02 SE VALIDA QUE CAMBIO SEA SOLO POR UNIDAD VALORES 567   04757014
              IF W-ENTIDAD = 2003                      AND              04758014
                 (CAA-CENTRO-CONT NOT = '0567')        AND              04759014
                 ((NCC0101I NOT = VARC-FILLER (01:20)) OR               04760014
                  (NC20101I NOT = VARC-FILLER (21:20)))                 04761014
                 MOVE  'VLE2137'    TO  CAA-COD-ERROR                   04762014
                 MOVE  -1           TO  NCC0101L                        04763014
                 PERFORM  3-FINAL                                       04764014
              END-IF                                                    04765014
           END-IF                                                       04766014
      *                                                                 04767014
      ****** VALIDAMOS N. DE MANCOMUNADOS**********                     04768014
      *                                                                 04769014
           IF MAN0101I IS NOT NUMERIC                                   04770014
              MOVE 'VLE0056'   TO CAA-COD-ERROR                         04771014
              MOVE -1          TO MAN0101L                              04772014
              PERFORM 3-FINAL                                           04773014
           END-IF                                                       04774014
      *                                                                 04775014
      *200711038-INI                                                    04776014
      ****** SOLICIUD APERTURA CTA-REGISTRO *******                     04777014
      *                                                                 04778014
           IF REG0101I            = SPACES                              04779014
              MOVE 'N'    TO REG0101I                                   04780014
           END-IF                                                       04781014
           IF REG0101I NOT = 'S' AND 'N'                                04782014
              MOVE 'VLE0997'   TO CAA-COD-ERROR                         04783014
              MOVE -1          TO REG0101L                              04784014
              PERFORM 3-FINAL                                           04785014
           END-IF                                                       04786014
           IF REG0101I            = 'S' AND                             04787014
              CAA-CENTRO-CONT NOT = '0542'                              04788014
              MOVE  -1                    TO REG0101L                   04789014
              MOVE 'VLE0955'              TO CAA-COD-ERROR              04790014
              PERFORM 3-FINAL                                           04791014
           END-IF                                                       04792014
           IF (REG0101I = 'S') AND                                      04793014
              (MDA0101I NOT = 'PEN' AND 'USD')                          04794014
              MOVE  -1                    TO REG0101L                   04795014
              MOVE 'VLE2169'              TO CAA-COD-ERROR              04796014
              MOVE 'CUENTA REGISTRO SOLO' TO CAA-VAR1-ERROR             04797014
              MOVE 'PARA -PEN- ó -USD-  ' TO CAA-VAR2-ERROR             04798014
              PERFORM 3-FINAL                                           04799014
           END-IF                                                       04800014
      *200711038-FIN                                                    04801014
      *                                                                 04802014
      * BUSCAR TODAS LAS CTA. RELACIONADAS CON EL CLIENTE(PE2C8M41)     04803014
      *                                                                 04804014
           PERFORM BUSCAR-CTASCLI                                       04805014
              THRU BUSCAR-CTASCLI-FIN                                   04806014
                                                                        04807014
      * TARIFA 51 SOLO VALIDO PARA LA CENTRAL DE VALORES                04808014
      *                                                                 04809014
      *    IF OPT-COMM = 'A'                                            04810014
      *       IF TAF0101I = '51' AND                                    04811014
      *          CAA-CENTRO-CONT NOT = '0567'                           04812014
      *          MOVE -1          TO TAF0101L                           04813014
      *          MOVE 'VLE2097'   TO CAA-COD-ERROR                      04814014
      *          PERFORM 3-FINAL                                        04815014
      *       END-IF                                                    04816014
      *    END-IF                                                       04817014
      *   A PETICION DEL USUARIO Y METODO EL 17-04-2001 SE DEJA LIBRE   04818014
      *** PARA QUE LAS OFICINAS HABRAN CTAS CON CTA GLOBAL  ** JIPC **  04819014
                                                                        04820014
      **                                                                04821014
      * EN LA OPCION DE MODIFICACION, CUANDO TARIFA ES 51 NO DEBE IR    04822014
      * EN BLANCO LA CUENTA (CUENTAS NO MIGRADAS )                      04823014
      **                                            27-10-2000 JIPC     04824014
           IF MSB-COMM = 'M'                                            04825014
              IF NCC0101I = SPACES AND                                  04826014
                 TAF0101I = '51'   AND                                  04827014
                 OPERA-BOLSA                                            04828014
                 MOVE -1          TO NCC0101L                           04829014
                 MOVE 'VLE2096'   TO CAA-COD-ERROR                      04830014
                 PERFORM 3-FINAL                                        04831014
              END-IF                                                    04832014
           END-IF                                                       04833014
      *                                                                 04834014
      * BUSCAMOS LA CUENTA DE CARGO/ABONO POR DEFECTO SI CORRESPONDE    04835014
      *                                                                 04836014
      * SE DEJA LIBRE PARA LAS OFICINAS PUEDAN ABRI CTAS CON TARIFA     04837014
      * 51 SIEMPRE QUE EL CLIENTE NO TENGA CUENTA CON EL BCO. 17-04-200104838014
      *                                                                 04839014
      *** AL NO TECLEAR LA CTA. RECUPERAMOS LA PRIMERA VALIDA DE LAS    04840014
      *** CTAS RELACIONADAS DEL CLIENTE                                 04841014
           IF NCC0101I = SPACES AND                                     04842014
      *200711038-INI                                                    04843014
              REG0101I = 'N'                                            04844014
      *200711038-FIN                                                    04845014
              MOVE 1                TO SW-VALCTA                        04846014
              PERFORM BUSCAR-CTAS                                       04847014
                 THRU BUSCAR-CTAS-FIN                                   04848014
              IF NOT VALIDA-CAR                                         04849014
                 IF (TAF0101I NOT = '51') OR                            04850014
                    (NOT OPERA-BOLSA)                                   04851014
                    MOVE -1          TO NCC0101L                        04852014
                    MOVE SPACES      TO NCC0101I                        04853014
                    MOVE SPACES      TO MON0101I                        04854014
                    MOVE 'VLE1516'   TO CAA-COD-ERROR                   04855014
                    PERFORM 3-FINAL                                     04856014
                 ELSE                                                   04857014
                    MOVE W-CTA-CAR-JUR TO NCC0101I                      04858014
                    PERFORM OBTENER-MONJUR1                             04859014
                       THRU OBTENER-MONJUR1-FIN                         04860014
                 END-IF                                                 04861014
              ELSE                                                      04862014
                 IF TAF0101I = '51' AND                                 04863014
                    OPERA-BOLSA                                         04864014
                    MOVE -1          TO NCC0101L                        04865014
                    MOVE 'VLE2097'   TO CAA-COD-ERROR                   04866014
                    PERFORM 3-FINAL                                     04867014
                 END-IF                                                 04868014
              END-IF                                                    04869014
           ELSE                                                         04870014
              IF CUSTODIA-AL-CLIENTE                                    04871014
      ***200310189-INI VALIDAMOS SI ES CUENTA DE CONTROL                04872014
                 IF NCC0101I (11:02) = '16'                             04873014
                    PERFORM VALIDAR-CTA-CONTROL                         04874014
                       THRU VALIDAR-CTA-CONTROL-FIN                     04875014
                 ELSE                                                   04876014
      *200509171-INI                                                    04877014
      *200711038-INI                                                    04878014
      *             MOVE 99           TO         VXBO-CLABOL            04879014
      *             PERFORM ACCESO-VLDTXBO                              04880014
      *                THRU ACCESO-VLDTXBO-FIN                          04881014
      *             IF ((NCC0101I (01:08) = VXBO-CTAECOS (01:08)  AND   04882014
      *                  NCC0101I (11:10) = VXBO-CTAECOS (11:10))       04883014
      *             OR  (NCC0101I (01:08) = VXBO-CTAECOD (01:08)  AND   04884014
      *                  NCC0101I (11:10) = VXBO-CTAECOD (11:10)))      04885014
      *            AND ((NC20101I (01:08) = VXBO-CTAECOS (01:08)  AND   04886014
      *                  NC20101I (11:10) = VXBO-CTAECOS (11:10))       04887014
      *             OR  (NC20101I (01:08) = VXBO-CTAECOD (01:08)  AND   04888014
      *                  NC20101I (11:10) = VXBO-CTAECOD (11:10)))      04889014
                    IF REG0101I = 'S'                                   04890014
      *200711038-FIN                                                    04891014
                       MOVE 'SI'      TO         SW-CTA-ESPECIAL        04892014
      *200509171-FIN                                                    04893014
                    ELSE                                                04894014
      ***        VALIDAMOS QUE LA CTA. TECLEADA PERTENEZCA AL CLIENTE   04895014
                       PERFORM VALIDAR-CTA-CAR                          04896014
                          THRU VALIDAR-CTA-CAR-FIN                      04897014
                    END-IF                                              04898014
9-9-->           END-IF                                                 04899014
-04JP ***200310189-FIN                                                  04900014
              END-IF                                                    04901014
           END-IF.                                                      04902014
      *                                                                 04903014
      * SE DEJA LIBRE PARA LAS OFICINAS PUEDAN ABRI CTAS CON TARIFA     04904014
      * 51 SIEMPRE QUE EL CLIENTE NO TENGA CUENTA CON EL BCO. 17-04-200104905014
      *                                                                 04906014
      **                                                                04907014
      * EN LA OPCION DE MODIFICACION, CUANDO TARIFA ES 51 NO DEBE IR    04908014
      * EN BLANCO LA CUENTA (CUENTAS NO MIGRADAS )                      04909014
      **                                            27-10-2000 JIPC     04910014
           IF MSB-COMM = 'M'                                            04911014
              IF NC20101I = SPACES AND                                  04912014
                 TAF0101I = '51'   AND                                  04913014
                 OPERA-BOLSA                                            04914014
                 MOVE -1          TO NC20101L                           04915014
                 MOVE 'VLE2096'   TO CAA-COD-ERROR                      04916014
                 PERFORM 3-FINAL                                        04917014
              END-IF                                                    04918014
           END-IF                                                       04919014
      *                                                                 04920014
           IF NC20101I = SPACES AND                                     04921014
      *200711038-INI                                                    04922014
              REG0101I = 'N'                                            04923014
      *200711038-FIN                                                    04924014
              MOVE 2                TO SW-VALCTA                        04925014
              PERFORM BUSCAR-CTAS                                       04926014
                 THRU BUSCAR-CTAS-FIN                                   04927014
              IF NOT VALIDA-ABO                                         04928014
17-04>           IF (TAF0101I NOT = '51') OR                            04929014
     >              (NOT OPERA-BOLSA)                                   04930014
                    MOVE -1          TO NC20101L                        04931014
                    MOVE SPACES      TO NC20101I                        04932014
                    MOVE SPACES      TO MO20101I                        04933014
                    MOVE 'VLE1516'   TO CAA-COD-ERROR                   04934014
                    PERFORM 3-FINAL                                     04935014
                 ELSE                                                   04936014
                    MOVE W-CTA-ABO-JUR TO NC20101I                      04937014
                    PERFORM OBTENER-MONJUR2                             04938014
                       THRU OBTENER-MONJUR2-FIN                         04939014
                 END-IF                                                 04940014
17-07>        ELSE                                                      04941014
     >           IF TAF0101I = '51' AND                                 04942014
     >              OPERA-BOLSA                                         04943014
     >              MOVE -1          TO NC20101L                        04944014
     >              MOVE 'VLE2097'   TO CAA-COD-ERROR                   04945014
     >              PERFORM 3-FINAL                                     04946014
     >           END-IF                                                 04947014
              END-IF                                                    04948014
           ELSE                                                         04949014
              IF CUSTODIA-AL-CLIENTE                                    04950014
      ***200310189-INI VALIDAMOS SI ES CUENTA DE CONTROL                04951014
                 IF NCC0101I (11:02) = '16'                             04952014
                    CONTINUE                                            04953014
                 ELSE                                                   04954014
      ***200310189-FIN                                                  04955014
      *200509171-INI                                                    04956014
      *200711038-INI                                                    04957014
      *             MOVE 99           TO         VXBO-CLABOL            04958014
      *             PERFORM ACCESO-VLDTXBO                              04959014
      *                THRU ACCESO-VLDTXBO-FIN                          04960014
      *             IF ((NCC0101I (01:08) = VXBO-CTAECOS (01:08)  AND   04961014
      *                  NCC0101I (11:10) = VXBO-CTAECOS (11:10))       04962014
      *             OR  (NCC0101I (01:08) = VXBO-CTAECOD (01:08)  AND   04963014
      *                  NCC0101I (11:10) = VXBO-CTAECOD (11:10)))      04964014
      *            AND ((NC20101I (01:08) = VXBO-CTAECOS (01:08)  AND   04965014
      *                  NC20101I (11:10) = VXBO-CTAECOS (11:10))       04966014
      *             OR  (NC20101I (01:08) = VXBO-CTAECOD (01:08)  AND   04967014
      *                  NC20101I (11:10) = VXBO-CTAECOD (11:10)))      04968014
                    IF REG0101I = 'S'                                   04969014
      *200711038-FIN                                                    04970014
                         MOVE 'SI'    TO         SW-CTA-ESPECIAL        04971014
      *200509171-FIN                                                    04972014
                    ELSE                                                04973014
      *** VALIDAMOS QUE LA CTA. TECLEADA PERTENEZCA AL CLIENTE          04974014
                       PERFORM VALIDAR-CTA-ABO                          04975014
                          THRU VALIDAR-CTA-ABO-FIN                      04976014
                    END-IF                                              04977014
                 END-IF                                                 04978014
              END-IF                                                    04979014
           END-IF                                                       04980014
      *                                                                 04981014
      * COMPROBAMOS QUE NO HAYAN MODIFICADO LAS CTA. CARGO/ABONO SI     04982014
      * TIENE SALDO O ALGUNA CONTABILIZACION PDTE.                      04983014
      *                                                                 04984014
           IF MSB-COMM = 'M'                                            04985014
              PERFORM VAL-MOD-CTAS                                      04986014
                 THRU VAL-MOD-CTAS-FIN                                  04987014
           END-IF                                                       04988014
      *                                                                 04989014
      * VALIDAMOS EL RESTO DE CAMPOS                                    04990014
      *                                                                 04991014
      *                                                                 04992014
      * VALIDAMOS LA SUCURSAL DE LA CTA. VALORES                        04993014
      *                                                                 04994014
           IF MSB-COMM = 'M'                                            04995014
      *200808196-INI                                                    04996014
              IF CAA-CENTRO-CONT = '0567'                               04997014
                 IF (VARC-FILLER(11:02) = '91') OR                      04998014
                    (VARC-FILLER(31:02) = '91') OR                      04999014
                    (NCC0101I   (11:02) = '91') OR                      05000014
                    (NC20101I   (11:02) = '91')                         05001014
                     MOVE  -1                    TO NCC0101L            05002014
                     MOVE 'VLE2169'              TO CAA-COD-ERROR       05003014
                     MOVE 'CTA-ECONOMICA VALIDA' TO CAA-VAR1-ERROR      05004014
                     MOVE 'PARA CONTINENTAL SAB' TO CAA-VAR2-ERROR      05005014
                     PERFORM 3-FINAL                                    05006014
                 END-IF                                                 05007014
              END-IF                                                    05008014
      *200808196-FIN                                                    05009014
              IF (SUC0101I NOT = SUC0101-COMM) AND                      05010014
                 (TAF0101I NOT = '51')                                  05011014
                 IF TAF0101I   = '99' AND                               05012014
                    CAA-CENTRO-CONT = '0567'                            05013014
                    CONTINUE                                            05014014
                 ELSE                                                   05015014
      *200808196-INI                                                    05016014
                    IF CAA-CENTRO-CONT = '0542'                         05017014
                       IF (VARC-FILLER(01:20) NOT = NCC0101I)   OR      05018014
                          (VARC-FILLER(21:20) NOT = NC20101I)           05019014
                           CONTINUE                                     05020014
                       ELSE                                             05021014
                          MOVE  -1                    TO NCC0101L       05022014
                          MOVE 'VLE2169'              TO CAA-COD-ERROR  05023014
                          MOVE 'CTA-ECONOMICA VALIDA' TO CAA-VAR1-ERROR 05024014
                          MOVE 'PARA CONTINENTAL SAB' TO CAA-VAR2-ERROR 05025014
                          PERFORM 3-FINAL                               05026014
                       END-IF                                           05027014
      *200808196-FIN                                                    05028014
                    ELSE                                                05029014
                       IF CTA-ESPECIAL                                  05030014
                          CONTINUE                                      05031014
                       ELSE                                             05032014
                          MOVE -1          TO SUC0101L                  05033014
                          MOVE 'VLE1107'   TO CAA-COD-ERROR             05034014
                          MOVE SUC0101-COMM TO SUC0101O                 05035014
                          PERFORM 3-FINAL                               05036014
                       END-IF                                           05037014
                    END-IF                                              05038014
                 END-IF                                                 05039014
              END-IF                                                    05040014
           ELSE                                                         05041014
              IF SUC0101I = SPACES                                      05042014
      *A2003**   MOVE CAA-CENTRO-CONT  TO SUC0101O                      05043014
      *200711038-INI                                                    05044014
      *          PERFORM OBTENER-MONEDA1                                05045014
      *             THRU OBTENER-MONEDA1-FIN                            05046014
                 IF REG0101I = 'S'                                      05047014
JPC@4 *             MOVE '0486'   TO OFI-PRO                            05048014
JPC@4               MOVE '0542'   TO OFI-PRO                            05049014
                 ELSE                                                   05050014
                    PERFORM OBTENER-MONEDA1                             05051014
                       THRU OBTENER-MONEDA1-FIN                         05052014
                 END-IF                                                 05053014
      *200711038-FIN                                                    05054014
                 MOVE OFI-PRO     TO SUC0101O                           05055014
              END-IF                                                    05056014
              PERFORM VALIDAR-CENTRO                                    05057014
                 THRU VALIDAR-CENTRO-FIN                                05058014
           END-IF                                                       05059014
      *                                                                 05060014
           MOVE SUC0101I        TO OFI-PRO                              05061014
      *                                                                 05062014
           PERFORM DESCRIPCION-OFICINA                                  05063014
              THRU DESCRIPCION-OFICINA-FIN                              05064014
      *                                                                 05065014
      * VALIDAMOS EL IDIOMA                                             05066014
      *                                                                 05067014
           IF IDI0101I = SPACES                                         05068014
              MOVE 'C'            TO IDI0101O                           05069014
           END-IF                                                       05070014
      *                                                                 05071014
           IF IDI0101I NOT = 'C' AND 'I'                                05072014
              MOVE -1          TO IDI0101L                              05073014
              MOVE 'VLE1103'   TO CAA-COD-ERROR                         05074014
              PERFORM 3-FINAL                                           05075014
           END-IF                                                       05076014
      *                                                                 05077014
      * VALIDAMOS EL CUSTODIO INTERNACIONAL                             05078014
      *                                                                 05079014
      *200503172-INI                                                    05080014
           MOVE SPACES             TO NCU0101O                          05081014
           IF TCL0101I = 'B'                                            05082014
              IF CIN0101I IS NOT NUMERIC                                05083014
                 MOVE 'VLE1754'   TO CAA-COD-ERROR                      05084014
                 MOVE -1          TO CIN0101L                           05085014
                 PERFORM 3-FINAL                                        05086014
              END-IF                                                    05087014
              PERFORM 2233-SELECT-CUST-INTER                            05088014
                 THRU 2233-SELECT-CUST-INTER-FIN                        05089014
              IF DB2-NOTFND                                             05090014
                 MOVE  'VLE0182'  TO  CAA-COD-ERROR                     05091014
                 MOVE  -1         TO  CIN0101L                          05092014
                PERFORM  3-FINAL                                        05093014
              END-IF                                                    05094014
      *200512055-INI                                                    05095014
           ELSE                                                         05096014
              IF CIN0101I NOT = SPACES                                  05097014
JPC@6         AND ENT0101I NOT = '0312'                                 05098014
                 MOVE 'VLE1709'   TO CAA-COD-ERROR                      05099014
                 MOVE -1          TO CIN0101L                           05100014
                 PERFORM 3-FINAL                                        05101014
              END-IF                                                    05102014
      *200512055-FIN                                                    05103014
           END-IF                                                       05104014
      *200503172-FIN                                                    05105014
                                                                        05106014
      *                                                                 05107014
      * VALIDAMOS LA MONEDA                                             05108014
      *                                                                 05109014
           IF MDA0101I = SPACES                                         05110014
              MOVE 'PEN'                TO MDA0101I                     05111014
              IF WXMI-TIPCUST = 'I'                                     05112014
                 MOVE 'USD'                TO MDA0101I                  05113014
              END-IF                                                    05114014
           ELSE                                                         05115014
              IF WXMI-TIPCUST = 'I' AND MDA0101I = 'PEN'                05116014
                 MOVE  -1                  TO MDA0101L                  05117014
      * CUANDO CUSTODIA INTERNACIONAL LA MONEDA NO PUEDE SER SOLES      05118014
                 MOVE 'VLE1894'            TO CAA-COD-ERROR             05119014
                 PERFORM 3-FINAL                                        05120014
              END-IF                                                    05121014
              INITIALIZE                      TCWC1200                  05122014
              MOVE MDA0101I                TO W120-CDDIVISS             05123014
              PERFORM OBTENER-MONEDA                                    05124014
                 THRU OBTENER-MONEDA-FIN                                05125014
           END-IF                                                       05126014
      *                                                                 05127014
           PERFORM ACCEDER-VLDTADS1                                     05128014
              THRU ACCEDER-VLDTADS1-FIN                                 05129014
      *                                                                 05130014
      *ACA- EN UNA MODIFICACION NO SE PERMITIRA MODIFICAR LA MONEDA     05131014
      *ACA- NUNCA SEGUN INDICACIONES DE RAFA (23/3/00)                  05132014
      *                                                                 05133014
      *ACA IF MSB-COMM = 'M' AND W-COUNT1 > 0                           05134014
           IF MSB-COMM = 'M'                                            05135014
              IF  VARC-MONEDA NOT = MDA0101I                            05136014
                   MOVE -1          TO MDA0101L                         05137014
                   MOVE 'VLE1700'   TO CAA-COD-ERROR                    05138014
                   PERFORM 3-FINAL                                      05139014
              END-IF                                                    05140014
           END-IF.                                                      05141014
      *                                                                 05142014
      * VALIDAMOS EL DOMICILIO DE CORRESPONDENCIA                       05143014
      *                                                                 05144014
           IF DCO0101I = SPACES                                         05145014
           OR DCO0101I = '000'                                          05146014
              MOVE '000'         TO DCO0101O                            05147014
      ****** PDTE ASIGNAR RUTINA (ACA)                                  05148014
      *    ELSE                                                         05149014
      *       PERFORM BUSCAR-DOM                                        05150014
      *          THRU BUSCAR-DOM-FIN                                    05151014
           END-IF                                                       05152014
      *                                                                 05153014
      * VALIDAMOS EL TIPO DE CLIENTE                                    05154014
      *                                                                 05155014
      *ACA IF TCL0101I = SPACES AND NOT CUSTODIA-AL-CLIENTE             05156014
      *ACA    MOVE 'J'            TO TCL0101O                           05157014
      *ACA END-IF                                                       05158014
           IF TCL0101I = SPACES                                         05159014
              MOVE 'N'            TO TCL0101O                           05160014
           END-IF                                                       05161014
      *                                                                 05162014
           IF TCL0101I NOT = 'N' AND 'J' AND 'F' AND 'M' AND 'B' AND 'E'05163014
                                 AND 'P'                                05164014
              MOVE -1          TO TCL0101L                              05165014
              MOVE 'VLE1411'   TO CAA-COD-ERROR                         05166014
              PERFORM 3-FINAL                                           05167014
           END-IF                                                       05168014
                                                                        05169014
      *TIPO DE CLIENTE = 'B' SOLO SERA POSIBLE PARA PERSONAS JURIDICAS  05170014
      *Y EN CUSTODIOS QUE TENGAN EL INIDICADOR "OPERA BOLSA" = 'S'.     05171014
      *DAVID07-06-1999                                                  05172014
           IF TCL0101I = 'B'                                            05173014
      *200310189-INI                                                    05174014
      *       IF VXMI-OPEBOLSA = 'S'                                    05175014
              IF OPERA-BOLSA                                            05176014
      *200310189-FIN                                                    05177014
      *ACA    AND W-JURIDI NOT = 'F'                                    05178014
                 CONTINUE                                               05179014
              ELSE                                                      05180014
                 MOVE -1            TO TCL0101L                         05181014
                 MOVE 'VLE1844'     TO CAA-COD-ERROR                    05182014
                 PERFORM 3-FINAL                                        05183014
              END-IF                                                    05184014
           END-IF                                                       05185014
      *                                                                 05186014
           IF OPT-COMM = 'A'                                            05187014
              MOVE 'N'               TO VARC-INDSAB                     05188014
              MOVE ZEROS             TO VARC-RUT                        05189014
              MOVE VARC-RUT          TO CSU0101I                        05190014
              MOVE VARC-INDSAB       TO ODI0101I                        05191014
           END-IF                                                       05192014
           IF PF2                                                       05193014
              IF TCL0101I NOT = VARC-VALEXTRJ AND VARC-INDSAB = 'S'     05194014
      *200512055-INI                                                    05195014
      *          MOVE VARC-VALEXTRJ TO TCL0101I                         05196014
      *          MOVE -1            TO TCL0101L                         05197014
      *          MOVE 'VLE1734'     TO CAA-COD-ERROR                    05198014
      *          PERFORM 3-FINAL                                        05199014
                 IF ((TCL0101I      = 'J' OR 'B') AND                   05200014
                     (VARC-VALEXTRJ = 'J' OR 'B'))                      05201014
                 OR ((TCL0101I      = 'N' OR 'P') AND                   05202014
                     (VARC-VALEXTRJ = 'N' OR 'P'))                      05203014
                    CONTINUE                                            05204014
                 ELSE                                                   05205014
                    MOVE VARC-VALEXTRJ TO TCL0101I                      05206014
                    MOVE -1            TO TCL0101L                      05207014
                    MOVE 'VLE1734'     TO CAA-COD-ERROR                 05208014
                    PERFORM 3-FINAL                                     05209014
                 END-IF                                                 05210014
      *200512055-FIN                                                    05211014
              END-IF                                                    05212014
           END-IF                                                       05213014
      *                                                                 05214014
      * VALIDAMOS EL CODIGO DEL PAIS CREEMOS QUE POR DEFECTO SERA EL    05215014
      * DEL CLIENTE Y HABRA SIDO RECUPERADO ANTERIORMENTE               05216014
      *                                                                 05217014
           IF PAI0101I NOT = SPACES AND                                 05218014
              PAI0101I IS NUMERIC                                       05219014
              MOVE -1          TO PAI0101L                              05220014
              MOVE 'VLE0090'   TO CAA-COD-ERROR                         05221014
              PERFORM 3-FINAL                                           05222014
           ELSE                                                         05223014
              IF PAI0101I = SPACES                                      05224014
                 MOVE -1          TO PAI0101L                           05225014
                 MOVE 'VLE0367'   TO CAA-COD-ERROR                      05226014
                 PERFORM 3-FINAL                                        05227014
              ELSE                                                      05228014
                 PERFORM BUSCAR-PAIS                                    05229014
                    THRU BUSCAR-PAIS-FIN                                05230014
              END-IF                                                    05231014
           END-IF                                                       05232014
      *                                                                 05233014
      * VALIDAMOS SI TIENE TARIFA DERECHOS CUSTODIA                     05234014
      *                                                                 05235014
           IF NOT CUSTODIA-AL-CLIENTE AND NOT ES-CTAVAL-CUS             05236014
              MOVE W-TARIFA-CUS    TO TAF0101I                          05237014
           END-IF                                                       05238014
      *200608070-INI                                                    05239014
      *200711038-INI                                                    05240014
      *    IF (NCC0101I (11:02) = '49'  OR                              05241014
      *        NC20101I (11:02) = '49') AND                             05242014
      *200711038-FIN                                                    05243014
           IF (NCC0101I (11:02) = '91'  OR                              05244014
               NC20101I (11:02) = '91') AND                             05245014
              (TAF0101I = SPACES OR LOW-VALUES)                         05246014
              MOVE '99'            TO TAF0101I                          05247014
           END-IF                                                       05248014
      *                                                                 05249014
      *200711038-INI                                                    05250014
      *    IF (NCC0101I (11:02) = '49'  OR                              05251014
      *        NC20101I (11:02) = '49')                                 05252014
           IF (NCC0101I (11:02) = '91'  OR                              05253014
               NC20101I (11:02) = '91')                                 05254014
      *200711038-FIN                                                    05255014
               IF MDA0101I = MON0101I AND MO20101I                      05256014
                  CONTINUE                                              05257014
               ELSE                                                     05258014
                  MOVE  -1            TO  NCC0101L                      05259014
                  MOVE 'VLE2198'      TO  CAA-COD-ERROR                 05260014
                  PERFORM 3-FINAL                                       05261014
               END-IF                                                   05262014
           END-IF                                                       05263014
      *200608070-FIN                                                    05264014
           IF TAF0101I = SPACES OR LOW-VALUES                           05265014
              MOVE '01'            TO TAF0101I                          05266014
           END-IF                                                       05267014
           IF TAF0101I IS NOT NUMERIC                                   05268014
              MOVE -1              TO TAF0101L                          05269014
              MOVE 'VLE0056'       TO CAA-COD-ERROR                     05270014
              PERFORM 3-FINAL                                           05271014
           ELSE                                                         05272014
              PERFORM ACCESO-VLDTXTA                                    05273014
                 THRU ACCESO-VLDTXTA-FIN                                05274014
           END-IF                                                       05275014
      *                                                                 05276014
      * VALIDAMOS RELACION INDICADOR FAX/TELEX/SWIFT                    05277014
      *                                                                 05278014
           IF TEL0101I NOT = SPACES                                     05279014
              IF SOT0101I NOT = 'S' AND 'T' AND 'F' AND 'C'             05280014
                 MOVE -1          TO SOT0101L                           05281014
                 MOVE 'VLE0192'   TO CAA-COD-ERROR                      05282014
                 PERFORM 3-FINAL                                        05283014
              END-IF                                                    05284014
           ELSE                                                         05285014
              IF SOT0101I NOT = SPACES                                  05286014
                 MOVE -1          TO SOT0101L                           05287014
                 MOVE 'VLE0569'   TO CAA-COD-ERROR                      05288014
                 PERFORM 3-FINAL                                        05289014
              END-IF                                                    05290014
           END-IF                                                       05291014
      *                                                                 05292014
      *VALIDAMOS NUMERO DE MANCOMUNADOS                                 05293014
      *                                                                 05294014
           IF TCL0101I = 'J' OR 'M' OR 'F' OR 'B'                       05295014
              IF MAN0101I NOT = 0                                       05296014
                 MOVE  -1                  TO MAN0101L                  05297014
                 MOVE 'VLE1739'            TO CAA-COD-ERROR             05298014
                 PERFORM 3-FINAL                                        05299014
              END-IF                                                    05300014
           END-IF                                                       05301014
      *MODIFICACION-04-08-1999.LOLO.CONTROL TIP.CLI 'E'                 05302014
           IF TCL0101I = 'E' AND SW-PERSONA = 'J'                       05303014
              IF MAN0101I NOT = 0                                       05304014
                 MOVE  -1                  TO MAN0101L                  05305014
                 MOVE 'VLE1739'            TO CAA-COD-ERROR             05306014
                 PERFORM 3-FINAL                                        05307014
              END-IF                                                    05308014
           END-IF                                                       05309014
      *MODIFICACION-04-08-1999.LOLO.CONTROL TIP.CLI 'E'                 05310014
      *                                                                 05311014
      * VALIDAMOS LOS PORCENTAJES DE EXENCION                           05312014
      *                                                                 05313014
           IF CVE0101I > 100 OR                                         05314014
              DCU0101I > 100 OR                                         05315014
              SUS0101I > 100 OR                                         05316014
              DIV0101I > 100 OR                                         05317014
              AMO0101I > 100 OR                                         05318014
              PAJ0101I > 100                                            05319014
                 MOVE -1          TO CVE0101L                           05320014
                 MOVE 'VLE0009'   TO CAA-COD-ERROR                      05321014
                 PERFORM 3-FINAL                                        05322014
           END-IF                                                       05323014
      *                                                                 05324014
      * VALIDAMOS LOS IMPORTES MAXIMOS                                  05325014
      *                                                                 05326014
           IF MCV0101I = 0                                              05327014
              MOVE 999999         TO MCV0101O                           05328014
           END-IF                                                       05329014
           IF MPJ0101I = 0                                              05330014
              MOVE 999999         TO MPJ0101O                           05331014
           END-IF                                                       05332014
           IF MDC0101I = 0                                              05333014
              MOVE 999999         TO MDC0101O                           05334014
           END-IF                                                       05335014
           IF MDI0101I = 0                                              05336014
              MOVE 999999         TO MDI0101O                           05337014
           END-IF                                                       05338014
           IF MSU0101I = 0                                              05339014
              MOVE 999999         TO MSU0101O                           05340014
           END-IF                                                       05341014
           IF MAM0101I = 0                                              05342014
              MOVE 999999         TO MAM0101O                           05343014
           END-IF.                                                      05344014
      *                                                                 05345014
      * VALIDAMOS EL COBRO DE CORREO                                    05346014
      *                                                                 05347014
      *LIMA-24-04-1999.SE QUITA EL CAMPO CORREO                         05348014
      *    IF CCO0101I = SPACES                                         05349014
      *       MOVE 'S'         TO CCO0101O                              05350014
      *    END-IF                                                       05351014
      *                                                                 05352014
      *    IF CCO0101I NOT = 'S' AND 'N'                                05353014
      *       MOVE -1          TO CCO0101L                              05354014
      *       MOVE 'VLE0074'   TO CAA-COD-ERROR                         05355014
      *       PERFORM 3-FINAL                                           05356014
      *    END-IF.                                                      05357014
      *LIMA-24-04-1999.SE QUITA EL CAMPO CORREO                         05358014
JPC@2 *    VALIDA   CAMPAÑAS                                            05359014
JPC@2      IF PF2                                                       05360014
JPC@2         IF (GVT0101-COMM NOT = GVT0101I) OR                       05361014
JPC@2            (CAV0101-COMM NOT = CAV0101I) OR                       05362014
JPC@2            (SCV0101-COMM NOT = SCV0101I) OR                       05363014
JPC@2            (CAM0101-COMM NOT = CAM0101I)                          05364014
JPC@2            MOVE  -1                      TO GVT0101L              05365014
JPC@2            MOVE 'VLE2169'                TO CAA-COD-ERROR         05366014
JPC@2            MOVE 'DATOS VENTA NO SE   '   TO CAA-VAR1-ERROR        05367014
JPC@2            MOVE 'PERMITE MODIFICAR   '   TO CAA-VAR2-ERROR        05368014
JPC@2            PERFORM 3-FINAL                                        05369014
JPC@2         END-IF                                                    05370014
JPC@2      END-IF.                                                      05371014
JPC@4      IF PF3                                                       05372014
JPC@4         IF GVT0101I = SPACES                                      05373014
JPC@4            MOVE  -1                      TO GVT0101L              05374014
JPC@4            MOVE 'VLE2169'                TO CAA-COD-ERROR         05375014
JPC@4            MOVE 'INFORMAR GESTOR DE  '   TO CAA-VAR1-ERROR        05376014
JPC@4            MOVE 'VENTA               '   TO CAA-VAR2-ERROR        05377014
JPC@4            PERFORM 3-FINAL                                        05378014
JPC@4         END-IF                                                    05379014
JPC@4         IF SCV0101I     = SPACES AND                              05380014
JPC@4            CAV0101I NOT = SPACES                                  05381014
JPC@4            MOVE  -1                      TO SCV0101L              05382014
JPC@4            MOVE 'VLE2169'                TO CAA-COD-ERROR         05383014
JPC@4            MOVE 'INFORMAR SUB-CANAL  '   TO CAA-VAR1-ERROR        05384014
JPC@4            MOVE 'DE VENTA            '   TO CAA-VAR2-ERROR        05385014
JPC@4            PERFORM 3-FINAL                                        05386014
JPC@4         END-IF                                                    05387014
JPC@4         IF CAV0101I     = SPACES AND                              05388014
JPC@4            SCV0101I NOT = SPACES                                  05389014
JPC@4            MOVE  -1                      TO CAV0101L              05390014
JPC@4            MOVE 'VLE2169'                TO CAA-COD-ERROR         05391014
JPC@4            MOVE 'INFORMAR CANAL DE   '   TO CAA-VAR1-ERROR        05392014
JPC@4            MOVE 'VENTA               '   TO CAA-VAR2-ERROR        05393014
JPC@4            PERFORM 3-FINAL                                        05394014
JPC@4         END-IF                                                    05395014
JPC@4      END-IF.                                                      05396014
      *                                                                 05397014
       VALIDAR-CAMPOS-FIN.   EXIT.                                      05398014
      *                                                                 05399014
      *                                                                 05400014
       CTAVAL-CUSTODIO.                                                 05401014
      *                                                                 05402014
           MOVE VXMI-CTAVAL                 TO VARC-CUENTA              05403014
      *                                                                 05404014
           EXEC SQL                                                     05405014
                SELECT  VARC_INVERSOR                                   05406014
                  INTO :VARC-INVERSOR                                   05407014
                  FROM  VLDTARC                                         05408014
                 WHERE  VARC_CUENTA  = :VARC-CUENTA                     05409014
           END-EXEC                                                     05410014
      *                                                                 05411014
           MOVE SQLCODE TO SQLCODE-AUX                                  05412014
      *                                                                 05413014
           EVALUATE TRUE                                                05414014
              WHEN DB2-OK                                               05415014
      *A2011-RUTLOG-I                                                   05416014
                   INITIALIZE W-VLWCLOG0                                05417014
                              LOGVLDTARC                                05418014
                   MOVE 'VLDTARC'             TO  VL7LOG-TABLA          05419014
                   MOVE 'SELECT'              TO  VL7LOG-OPERACION      05420014
                   MOVE LENGTH OF DCLVLDTARC  TO  VL7LOG-REGISTRO-LEN   05421014
                   MOVE VARC-INVERSOR         TO  LARC-INVERSOR         05422014
                   MOVE VARC-CUENTA           TO  LARC-CUENTA           05423014
                   MOVE LOGVLDTARC            TO  VL7LOG-REGISTRO-TEXT  05424014
                   PERFORM LLAMAR-VL7CRLOG                              05425014
                      THRU LLAMAR-VL7CRLOG-FIN                          05426014
      *A2011-RUTLOG-F                                                   05427014
                                                                        05428014
      *            MOVE VARC-TARIFACUS      TO W-TARIFA-CUS             05429014
                   MOVE VARC-INVERSOR       TO W-TARIFA-CUS             05430014
      *                                                                 05431014
              WHEN OTHER                                                05432014
                   MOVE 'SELECT-CUS'  TO  ABC-REFERENCIA                05433014
                   MOVE 'VLDTARC'     TO  ABC-OBJETO-ERROR              05434014
                   PERFORM 999-ABEND-DB2                                05435014
      *                                                                 05436014
           END-EVALUATE.                                                05437014
      *                                                                 05438014
       CTAVAL-CUSTODIO-FIN. EXIT.                                       05439014
      *                                                                 05440014
       VAL-MOD-CTAS.                                                    05441014
      *                                                                 05442014
           MOVE CTA0101I                    TO W-CUENTA                 05443014
           MOVE W-CUENTA                    TO VARC-CUENTA              05444014
      *                                                                 05445014
JPC@1 *    EXEC SQL                                                     05446014
JPC@1 *         SELECT  *                                               05447014
JPC@1 *           INTO :DCLVLDTARC                                      05448014
JPC@1 *           FROM  VLDTARC                                         05449014
JPC@1 *          WHERE  VARC_CUENTA  = :VARC-CUENTA                     05450014
JPC@1 *    END-EXEC                                                     05451014
           EXEC SQL                                                     05452014
                SELECT  VARC_CUENTA                                     05453014
                     ,  VARC_CENTAD                                     05454014
                     ,  VARC_NUMCLI                                     05455014
                     ,  VARC_CLMAST                                     05456014
                     ,  VARC_MONEDA                                     05457014
                     ,  VARC_SUCURS                                     05458014
                     ,  VARC_CTACAR                                     05459014
                     ,  VARC_CTAABO                                     05460014
                     ,  VARC_TEXTO                                      05461014
                     ,  VARC_PRESEN                                     05462014
                     ,  VARC_GRUPO                                      05463014
                     ,  VARC_RUT                                        05464014
                     ,  VARC_CNAE                                       05465014
                     ,  VARC_SITUAC                                     05466014
                     ,  VARC_EXEN1                                      05467014
                     ,  VARC_EXEN2                                      05468014
                     ,  VARC_EXEN3                                      05469014
                     ,  VARC_EXEN4                                      05470014
                     ,  VARC_EXEN5                                      05471014
                     ,  VARC_EXEN6                                      05472014
                     ,  VARC_EXEN7                                      05473014
                     ,  VARC_EXEN8                                      05474014
                     ,  VARC_EXEN9                                      05475014
                     ,  VARC_EXEN10                                     05476014
                     ,  VARC_ANALIS                                     05477014
                     ,  VARC_CLACARGO                                   05478014
                     ,  VARC_CLABONO                                    05479014
                     ,  VARC_NUMDOM                                     05480014
                     ,  VARC_CODSUS                                     05481014
                     ,  VARC_FE_ULT_EXT                                 05482014
                     ,  VARC_PAIS                                       05483014
                     ,  VARC_FE_CARTERA                                 05484014
                     ,  VARC_CLTELEX                                    05485014
                     ,  VARC_FE_ALTA                                    05486014
                     ,  VARC_VALORACION                                 05487014
                     ,  VARC_VALEXTRJ                                   05488014
                     ,  VARC_INVERSOR                                   05489014
                     ,  VARC_DIRECTA                                    05490014
                     ,  VARC_MAX_CVE_1                                  05491014
                     ,  VARC_MAX_DCU_5                                  05492014
                     ,  VARC_MAX_SUS_6                                  05493014
                     ,  VARC_MAX_DIV_7                                  05494014
                     ,  VARC_MAX_AMO_8                                  05495014
                     ,  VARC_MAX_PAJ_9                                  05496014
                     ,  VARC_FECHA_102                                  05497014
                     ,  VARC_TARIFACUS                                  05498014
                     ,  VARC_SWIFT_TELEX                                05499014
                     ,  VARC_TELEX_2                                    05500014
                     ,  VARC_GRUPO_CTAS                                 05501014
                     ,  VARC_OPER_TIT                                   05502014
                     ,  VARC_FEALTREG                                   05503014
                     ,  VARC_FEULMOD                                    05504014
                     ,  VARC_HORULMOD                                   05505014
                     ,  VARC_NUMTER                                     05506014
                     ,  VARC_USUARIO                                    05507014
                     ,  VARC_FILLER                                     05508014
                     ,  VARC_CTAVAL20                                   05509014
      *@ZAL-INI                                                         05510014
      *              ,  VARC_NUMMAN                                     05511014
                     ,  VARC_GRUPO_CTAS                                 05512014
      *@ZAL-FIN                                                         05513014
                     ,  VARC_INDIMP                                     05514014
                     ,  VARC_INDSAB                                     05515014
                  INTO :VARC-CUENTA                                     05516014
                     , :VARC-CENTAD                                     05517014
                     , :VARC-NUMCLI                                     05518014
                     , :VARC-CLMAST                                     05519014
                     , :VARC-MONEDA                                     05520014
                     , :VARC-SUCURS                                     05521014
                     , :VARC-CTACAR                                     05522014
                     , :VARC-CTAABO                                     05523014
                     , :VARC-TEXTO                                      05524014
                     , :VARC-PRESEN                                     05525014
                     , :VARC-GRUPO                                      05526014
                     , :VARC-RUT                                        05527014
                     , :VARC-CNAE                                       05528014
                     , :VARC-SITUAC                                     05529014
                     , :VARC-EXEN1                                      05530014
                     , :VARC-EXEN2                                      05531014
                     , :VARC-EXEN3                                      05532014
                     , :VARC-EXEN4                                      05533014
                     , :VARC-EXEN5                                      05534014
                     , :VARC-EXEN6                                      05535014
                     , :VARC-EXEN7                                      05536014
                     , :VARC-EXEN8                                      05537014
                     , :VARC-EXEN9                                      05538014
                     , :VARC-EXEN10                                     05539014
                     , :VARC-ANALIS                                     05540014
                     , :VARC-CLACARGO                                   05541014
                     , :VARC-CLABONO                                    05542014
                     , :VARC-NUMDOM                                     05543014
                     , :VARC-CODSUS                                     05544014
                     , :VARC-FE-ULT-EXT                                 05545014
                     , :VARC-PAIS                                       05546014
                     , :VARC-FE-CARTERA                                 05547014
                     , :VARC-CLTELEX                                    05548014
                     , :VARC-FE-ALTA                                    05549014
                     , :VARC-VALORACION                                 05550014
                     , :VARC-VALEXTRJ                                   05551014
                     , :VARC-INVERSOR                                   05552014
                     , :VARC-DIRECTA                                    05553014
                     , :VARC-MAX-CVE-1                                  05554014
                     , :VARC-MAX-DCU-5                                  05555014
                     , :VARC-MAX-SUS-6                                  05556014
                     , :VARC-MAX-DIV-7                                  05557014
                     , :VARC-MAX-AMO-8                                  05558014
                     , :VARC-MAX-PAJ-9                                  05559014
                     , :VARC-FECHA-102                                  05560014
                     , :VARC-TARIFACUS                                  05561014
                     , :VARC-SWIFT-TELEX                                05562014
                     , :VARC-TELEX-2                                    05563014
                     , :VARC-GRUPO-CTAS                                 05564014
                     , :VARC-OPER-TIT                                   05565014
                     , :VARC-FEALTREG                                   05566014
                     , :VARC-FEULMOD                                    05567014
                     , :VARC-HORULMOD                                   05568014
                     , :VARC-NUMTER                                     05569014
                     , :VARC-USUARIO                                    05570014
                     , :VARC-FILLER                                     05571014
                     , :VARC-CTAVAL20                                   05572014
      *@ZAL-INI                                                         05573014
      *              , :VARC-NUMMAN                                     05574014
                     , :VARC-GRUPO-CTAS                                 05575014
      *@ZAL-FIN                                                         05576014
                     , :VARC-INDIMP                                     05577014
                     , :VARC-INDSAB                                     05578014
                  FROM  VLDTARC                                         05579014
                 WHERE  VARC_CUENTA  = :VARC-CUENTA                     05580014
           END-EXEC                                                     05581014
      *                                                                 05582014
           MOVE SQLCODE TO SQLCODE-AUX                                  05583014
      *                                                                 05584014
           EVALUATE TRUE                                                05585014
              WHEN DB2-OK                                               05586014
      *A2011-RUTLOG-I                                                   05587014
                   INITIALIZE W-VLWCLOG0                                05588014
                              LOGVLDTARC                                05589014
                   MOVE 'VLDTARC'             TO  VL7LOG-TABLA          05590014
                   MOVE 'SELECT'              TO  VL7LOG-OPERACION      05591014
                   MOVE LENGTH OF DCLVLDTARC  TO  VL7LOG-REGISTRO-LEN   05592014
                   MOVE DCLVLDTARC            TO  LOGVLDTARC            05593014
                   MOVE LOGVLDTARC            TO  VL7LOG-REGISTRO-TEXT  05594014
                   PERFORM LLAMAR-VL7CRLOG                              05595014
                      THRU LLAMAR-VL7CRLOG-FIN                          05596014
      *A2011-RUTLOG-F                                                   05597014
      *                                                                 05598014
              WHEN  DB2-NOTFND                                          05599014
                    MOVE  'VLE0142'   TO  CAA-COD-ERROR                 05600014
                    MOVE  -1          TO  CTA0101L                      05601014
                    PERFORM  3-FINAL                                    05602014
      *                                                                 05603014
              WHEN OTHER                                                05604014
                   MOVE 'SELECT'      TO  ABC-REFERENCIA                05605014
                   MOVE 'VLDTARC'     TO  ABC-OBJETO-ERROR              05606014
                   PERFORM 999-ABEND-DB2                                05607014
      *                                                                 05608014
           END-EVALUATE.                                                05609014
      *                                                                 05610014
           IF VARC-FILLER(01:20) NOT = NCC0101I OR                      05611014
              VARC-FILLER(21:20) NOT = NC20101I                         05612014
      *200804248-INI                                                    05613014
              IF VARC-FILLER (11:02) = '91'                             05614014
                 INITIALIZE                 W-BGECMDC                   05615014
                 MOVE VARC-FILLER (01:4) TO MDC-ENTIDAD                 05616014
                 MOVE VARC-FILLER (05:4) TO MDC-CENTRO-ALTA             05617014
                 MOVE VARC-FILLER (11:2) TO MDC-CUENTA(1:2)             05618014
                 MOVE VARC-FILLER (13:8) TO MDC-CUENTA(3:8)             05619014
      *                                                                 05620014
                 EXEC CICS                                              05621014
                      LINK PROGRAM (BG2CMDC0)                           05622014
                          COMMAREA (BGECMDC)                            05623014
                 END-EXEC                                               05624014
      *                                                                 05625014
                 IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                   05626014
                    MOVE 'ERROR EN BGECMDC0'  TO ABC-REFERENCIA         05627014
                    MOVE 'BG2CMDC0'           TO ABC-OBJETO-ERROR       05628014
                    PERFORM 999-ABEND-CICS                              05629014
                 END-IF                                                 05630014
      *                                                                 05631014
                 EVALUATE MDC-CODERR                                    05632014
                     WHEN SPACES                                        05633014
                          IF MDC-SALDO-DISPON NOT = ZEROS               05634014
                             MOVE MDC-SALDO-DISPON     TO W-SDOECON-EDIT05635014
                             MOVE 'VLE2169'            TO CAA-COD-ERROR 05636014
                           MOVE 'CTA-REGISTRO CON SAL' TO CAA-VAR1-ERROR05637014
                             MOVE 'DO                ' TO CAA-VAR2-ERROR05638014
                             MOVE W-SDOECON-EDIT TO CAA-VAR2-ERROR(4:15)05639014
                             MOVE -1         TO CTA0101L                05640014
                             PERFORM 3-FINAL                            05641014
                          END-IF                                        05642014
                     WHEN OTHER                                         05643014
                          MOVE -1          TO NCC0101L                  05644014
                          MOVE MDC-CODERR  TO CAA-COD-ERROR             05645014
                          PERFORM 3-FINAL                               05646014
                 END-EVALUATE                                           05647014
              END-IF                                                    05648014
      *200804248-FIN                                                    05649014
      * SOLO SI LA MONEDA DE LA CUENTA A MODIFICAR ES DIFERENTE         05650014
      *                                                                 05651014
              PERFORM OBTENER-MONEDA1                                   05652014
                 THRU OBTENER-MONEDA1-FIN                               05653014
      *                                                                 05654014
              PERFORM OBTENER-MONEDA2                                   05655014
                 THRU OBTENER-MONEDA2-FIN                               05656014
      *                                                                 05657014
           END-IF.                                                      05658014
      *                                                                 05659014
      *    MOVE VARC-TARIFACUS TO W-TARIFA                              05660014
           MOVE VARC-INVERSOR  TO W-TARIFA                              05661014
      *                                                                 05662014
           IF (W-TARIFA       NOT = TAF0101I OR                         05663014
               VARC-EXEN1     NOT = CVE0101I OR                         05664014
               VARC-EXEN5     NOT = DCU0101I OR                         05665014
               VARC-EXEN6     NOT = SUS0101I OR                         05666014
               VARC-EXEN7     NOT = DIV0101I OR                         05667014
               VARC-EXEN8     NOT = AMO0101I OR                         05668014
               VARC-EXEN9     NOT = PAJ0101I OR                         05669014
               VARC-MAX-CVE-1 NOT = MCV0101I OR                         05670014
               VARC-MAX-PAJ-9 NOT = MPJ0101I OR                         05671014
               VARC-MAX-DCU-5 NOT = MDC0101I OR                         05672014
               VARC-MAX-DIV-7 NOT = MDI0101I OR                         05673014
               VARC-MAX-SUS-6 NOT = MSU0101I OR                         05674014
      * MODIFICACION - MADRID - 24/05/1999.SE MODIFICA POR DESAPARECER  05675014
      * EL CAMPO CORREO                                                 05676014
               VARC-MAX-AMO-8 NOT = MAM0101I) AND                       05677014
               CAA-CENTRO-CONT NOT = 0567                               05678014
      *        VARC-MAX-AMO-8 NOT = MAM0101I OR                         05679014
      *       (VARC-EXEN10        = 100      AND                        05680014
      *        CCO0101I       NOT = 'N')       OR                       05681014
      *       (VARC-EXEN10        = 000      AND                        05682014
      *        CCO0101I       NOT = 'S'))      AND                      05683014
      *        CAA-CENTRO-CONT NOT = 0567                               05684014
      *FIN-MODIFICACION - MADRID - 24/05/1999.SE MODIFICA PORDESAPARECER05685014
      * EL CAMPO CORREO                                                 05686014
      *200806094-INI                                                    05687014
      *        MOVE 'VLE1412'  TO CAA-COD-ERROR                         05688014
      *        MOVE -1         TO CTA0101L                              05689014
      *        PERFORM 3-FINAL                                          05690014
               IF W-TARIFA        NOT = TAF0101I AND                    05691014
                  CAA-CENTRO-CONT     = 0542     AND                    05692014
                ((NCC0101I (11:02)    = '91'     AND                    05693014
                  TAF0101I            =  99)     OR                     05694014
      *200808196-INI                                                    05695014
                 (NCC0101I (11:02)    = '16'     AND                    05696014
                  TAF0101I            =  51))                           05697014
      *200808196-FIN                                                    05698014
                  CONTINUE                                              05699014
               ELSE                                                     05700014
                  MOVE 'VLE1412'  TO CAA-COD-ERROR                      05701014
                  MOVE -1         TO CTA0101L                           05702014
                  PERFORM 3-FINAL                                       05703014
               END-IF                                                   05704014
      *200806094-FIN                                                    05705014
           END-IF.                                                      05706014
      *                                                                 05707014
       VAL-MOD-CTAS-FIN.   EXIT.                                        05708014
      *                                                                 05709014
      *                                                                 05710014
      * VALIDAMOS EL CODIGO DE DOMICILIO TECLEADO O RECOGIDO DEL        05711014
      * CLIENTE Y OBTENEMOS SI DESCRIPCION                              05712014
      *                                                                 05713014
      *BUSCAR-DOM.                                                      05714014
      *                                                                 05715014
      *    INITIALIZE                            PEWC5000               05716014
      *                                                                 05717014
      *    MOVE CAA-ENTIDAD                   TO W500-PECENTID          05718014
      *    MOVE                               TO W500-OFIAPE            05719014
      *    MOVE                               TO W500-CODISER           05720014
      *    MOVE CTA0101I                      TO W500-NUMECTA(1:7)      05721014
      *    MOVE DG20101I                      TO W500-NUMECTA(8:1)      05722014
      *                                                                 05723014
      *    EXEC CICS                                                    05724014
      *         LINK PROGRAM ('PE2C5000')                               05725014
      *         COMMAREA (PEWC5000)                                     05726014
      *    END-EXEC.                                                    05727014
      *                                                                 05728014
      *    IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         05729014
      *       MOVE 'ERROR EN PE2C5000'  TO ABC-REFERENCIA               05730014
      *       MOVE 'PE2C5000'           TO ABC-OBJETO-ERROR             05731014
      *       PERFORM 999-ABEND-CICS                                    05732014
      *    END-IF.                                                      05733014
      *                                                                 05734014
      *    EVALUATE W000-CDRETORN                                       05735014
      *      WHEN '00'                                                  05736014
      *           MOVE W000-CONTOCUR        TO T260-DATOS               05737014
      *           MOVE T260-DATOS(1:15)     TO DEC0101O                 05738014
      *      WHEN '70'                                                  05739014
      *      WHEN '80'                                                  05740014
      *           MOVE  -1                  TO DCO0101L                 05741014
      *           MOVE 'VLE1000'            TO CAA-COD-ERROR            05742014
      *           MOVE 'TC2C1000'           TO CAA-VAR1-ERROR           05743014
      *           MOVE W000-SQLCODE         TO W-SQLCODE-NUM            05744014
      *           MOVE W-SQLCODE-NUM        TO W-SQLCODE-EDIT           05745014
      *           MOVE W-SQLCODE-EDIT       TO CAA-VAR2-ERROR           05746014
      *           PERFORM 3-FINAL                                       05747014
      *      WHEN OTHER                                                 05748014
      *           MOVE  -1                  TO DCO0101L                 05749014
      *           MOVE 'VLE1225'            TO CAA-COD-ERROR            05750014
      *           PERFORM 3-FINAL                                       05751014
      *    END-EVALUATE.                                                05752014
      *                                                                 05753014
      *BUSCAR-DOM-FIN.   EXIT.                                          05754014
      *                                                                 05755014
      *                                                                 05756014
      * VALIDAMOS EL CODIGO DE PAIS RECOGIDO DEL                        05757014
      * CLIENTE                                                         05758014
      *                                                                 05759014
       BUSCAR-PAIS.                                                     05760014
      *                                                                 05761014
           INITIALIZE                            TCWC0000               05762014
           MOVE '0112'                        TO W000-CDTABLA           05763014
           MOVE CAA-ENTIDAD                   TO W000-STBANCO           05764014
           MOVE CAA-IDIOMA-TERM               TO W000-TCCIDIOM          05765014
           MOVE PAI0101I                      TO W000-CLAVTG(1:3)       05766014
      *    MOVE W-PAIS-CLI                    TO W000-CLAVTG(1:3)       05767014
           MOVE 01                            TO W000-NUCLAVE           05768014
      *                                                                 05769014
           EXEC CICS                                                    05770014
                LINK PROGRAM (TC2C1000)                                 05771014
                COMMAREA (TCWC0000)                                     05772014
           END-EXEC.                                                    05773014
      *                                                                 05774014
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         05775014
              MOVE 'ERROR EN TC2C1000'  TO ABC-REFERENCIA               05776014
              MOVE 'TC2C1000'           TO ABC-OBJETO-ERROR             05777014
              PERFORM 999-ABEND-CICS                                    05778014
           END-IF                                                       05779014
      *                                                                 05780014
           EVALUATE W000-CDRETORN                                       05781014
             WHEN '00'                                                  05782014
                  CONTINUE                                              05783014
      *           MOVE W000-CONTOCUR        TO T010-DATOS               05784014
      *           MOVE T010-DATOS(41:3)     TO PAI0101O                 05785014
             WHEN '70'                                                  05786014
             WHEN '80'                                                  05787014
                  MOVE  -1                  TO PAI0101L                 05788014
                  MOVE 'VLE1000'            TO CAA-COD-ERROR            05789014
                  MOVE 'TC2C1000'           TO CAA-VAR1-ERROR           05790014
                  MOVE W000-SQLCODE         TO W-SQLCODE-NUM            05791014
                  MOVE W-SQLCODE-NUM        TO W-SQLCODE-EDIT           05792014
                  MOVE W-SQLCODE-EDIT       TO CAA-VAR2-ERROR           05793014
                  PERFORM 3-FINAL                                       05794014
             WHEN OTHER                                                 05795014
      *           CONTINUE                                              05796014
                  MOVE  -1                  TO PAI0101L                 05797014
                  MOVE 'VLE0090'            TO CAA-COD-ERROR            05798014
                  PERFORM 3-FINAL                                       05799014
           END-EVALUATE.                                                05800014
      *                                                                 05801014
       BUSCAR-PAIS-FIN.   EXIT.                                         05802014
      *                                                                 05803014
      *                                                                 05804014
       MOVER-A-TABLA.                                                   05805014
      *                                                                 05806014
           MOVE CTA0101I        TO W-CUENTA                             05807014
           MOVE W-CUENTA        TO VARC-CUENTA                          05808014
           MOVE MDA0101I        TO VARC-MONEDA                          05809014
           MOVE ENT0101I        TO W-ENTIDAD                            05810014
           MOVE W-ENTIDAD       TO VARC-CENTAD                          05811014
           MOVE TIT0101I        TO W-TITULAR                            05812014
           MOVE W-TITULAR       TO VARC-NUMCLI                          05813014
           MOVE ZEROES          TO VARC-CTACAR                          05814014
           MOVE ZEROES          TO VARC-CTAABO                          05815014
           MOVE NCC0101I        TO W-CCC-CAR                            05816014
           MOVE NC20101I        TO W-CCC-ABO                            05817014
           MOVE VLWCCTA0        TO VARC-FILLER                          05818014
           MOVE SUC0101I        TO W-SUCVAL                             05819014
           MOVE W-SUCVAL        TO VARC-SUCURS                          05820014
           MOVE IDI0101I        TO VARC-TEXTO                           05821014
           MOVE DCO0101I        TO DCO0101-N                            05822014
           MOVE DCO0101-N       TO VARC-NUMDOM                          05823014
           MOVE TCL0101I        TO VARC-VALEXTRJ                        05824014
      *   ????????' MODIFICAR EL CSU0101I, POR RUT0101I                 05825014
      *    MOVE CSU0101I        TO VARC-CODSUS                          05826014
           MOVE CSU0101I        TO VARC-RUT                             05827014
           MOVE PAI0101I        TO VARC-PAIS(1:3)                       05828014
           MOVE TAF0101I        TO W-TARIFA                             05829014
      *    MOVE W-TARIFA        TO VARC-TARIFACUS                       05830014
           MOVE W-TARIFA        TO VARC-INVERSOR                        05831014
           MOVE SOT0101I        TO VARC-SWIFT-TELEX                     05832014
      *                                                                 05833014
           MOVE TEL0101I        TO CLA-TELEX-AUX                        05834014
           MOVE CLTELEX-AUX     TO VARC-CLTELEX                         05835014
           MOVE TELEX2-AUX      TO VARC-TELEX-2                         05836014
      *                                                                 05837014
           MOVE CVE0101I        TO VARC-EXEN1                           05838014
           MOVE ZEROS           TO VARC-EXEN2                           05839014
           MOVE ZEROS           TO VARC-EXEN3                           05840014
           MOVE 100             TO VARC-EXEN4                           05841014
           MOVE DCU0101I        TO VARC-EXEN5                           05842014
           MOVE SUS0101I        TO VARC-EXEN6                           05843014
           MOVE DIV0101I        TO VARC-EXEN7                           05844014
           MOVE AMO0101I        TO VARC-EXEN8                           05845014
           MOVE PAJ0101I        TO VARC-EXEN9                           05846014
           MOVE MCV0101I        TO VARC-MAX-CVE-1                       05847014
           MOVE MPJ0101I        TO VARC-MAX-PAJ-9                       05848014
           MOVE MDC0101I        TO VARC-MAX-DCU-5                       05849014
           MOVE MDI0101I        TO VARC-MAX-DIV-7                       05850014
           MOVE MSU0101I        TO VARC-MAX-SUS-6                       05851014
           MOVE MAM0101I        TO VARC-MAX-AMO-8                       05852014
      *200503172-INI                                                    05853014
           MOVE CIN0101I        TO VARC-CODSUS                          05854014
      *200503172-FIN                                                    05855014
      *LIMA-24-04-1999.SE QUITA EL CAMPO CORREO                         05856014
      *    IF CCO0101I = 'S'                                            05857014
      *        MOVE   0         TO VARC-EXEN10                          05858014
      *    ELSE                                                         05859014
      *        MOVE 100         TO VARC-EXEN10                          05860014
      *    END-IF.                                                      05861014
      **                                                                05862014
           MOVE   0         TO VARC-EXEN10                              05863014
      **                                                                05864014
      *LIMA-24-04-1999.SE QUITA EL CAMPO CORREO                         05865014
      *200712034-INI                                                    05866014
      *A2012-I.                                                         05867014
      *    MOVE MAN0101I        TO VARC-NUMMAN.                         05868014
      *A2012-F.                                                         05869014
           MOVE MAN0101I        TO VARC-GRUPO-CTAS.                     05870014
      *200712034-FIN                                                    05871014
      *                                                                 05872014
       MOVER-A-TABLA-FIN.   EXIT.                                       05873014
      *                                                                 05874014
       BUSCAR-NOMBRE.                                                   05875014
      *                                                                 05876014
           INITIALIZE                     W520-REGISTRO                 05877014
           MOVE VARC-NUMCLI            TO W520-NUMCLIEN                 05878014
      *                                                                 05879014
           EXEC CICS                                                    05880014
              LINK PROGRAM (PE2C5201)                                   05881014
              COMMAREA (W520-REGISTRO)                                  05882014
              LENGTH   (LENGTH OF W520-REGISTRO)                        05883014
           END-EXEC                                                     05884014
      *                                                                 05885014
           IF EIBRESP NOT = DFHRESP(NORMAL)                             05886014
              MOVE 'ERROR EN PE2C5201'  TO ABC-REFERENCIA               05887014
              MOVE 'PE2C5201'           TO ABC-OBJETO-ERROR             05888014
              PERFORM 999-ABEND-CICS                                    05889014
           END-IF                                                       05890014
      *                                                                 05891014
           MOVE SPACES   TO NOM0101O                                    05892014
      *                                                                 05893014
           EVALUATE W520-PECRETOR                                       05894014
             WHEN ZEROS                                                 05895014
               CONTINUE                                                 05896014
             WHEN 10                                                    05897014
               MOVE '********* CLIENTE INEXISTENTE' TO NOM0101O         05898014
             WHEN 99                                                    05899014
               INITIALIZE   QGECABC                                     05900014
               MOVE 'LINK PE2C5201'        TO ABC-REFERENCIA            05901014
               MOVE W520-TABLENAME         TO ABC-OBJETO-ERROR          05902014
               PERFORM 999-ABEND-DB2                                    05903014
             WHEN OTHER                                                 05904014
               MOVE  -1                  TO TIT0101L                    05905014
               MOVE 'VLE0907'            TO CAA-COD-ERROR               05906014
               MOVE 'PE2C5201'           TO CAA-VAR1-ERROR              05907014
               MOVE W520-PECRETOR        TO CAA-VAR2-ERROR              05908014
               PERFORM 3-FINAL                                          05909014
           END-EVALUATE.                                                05910014
      *                                                                 05911014
           IF  W520-PECRETOR = ZEROS                                    05912014
              IF W520-SUJGRUP = 'F'                                     05913014
                 STRING W520-NOMBRE DELIMITED BY '  ' ' '               05914014
                        W520-PRIAPE DELIMITED BY '  ' ' '               05915014
                        W520-SEGAPE DELIMITED BY '  '                   05916014
                                             INTO NOM0101O              05917014
              ELSE                                                      05918014
                 STRING W520-NOMBRE DELIMITED BY SIZE                   05919014
                        W520-PRIAPE DELIMITED BY SIZE                   05920014
                        W520-SEGAPE DELIMITED BY SIZE                   05921014
                                             INTO NOM0101O              05922014
              END-IF                                                    05923014
      *MODIFICACION-LOLO-04-08-1999.CONTROL PARA TIP.CLIENTE 'E'        05924014
              IF W520-SUJGRUP = 'F'                                     05925014
                   MOVE   'N'              TO   SW-PERSONA              05926014
              ELSE                                                      05927014
                   MOVE   'J'              TO   SW-PERSONA              05928014
              END-IF                                                    05929014
      *MODIFICACION-LOLO-04-08-1999.CONTROL PARA TIP.CLIENTE 'E'        05930014
           END-IF.                                                      05931014
      *                                                                 05932014
       BUSCAR-NOMBRE-FIN.                                               05933014
           EXIT.                                                        05934014
      *                                                                 05935014
       BUSCAR-TITULAR.                                                  05936014
      *                                                                 05937014
           INITIALIZE                     W520-REGISTRO                 05938014
           MOVE TIT0101I               TO W520-NUMCLIEN                 05939014
           MOVE SPACES                 TO NOM0101O                      05940014
                                          WS-VL01-L05-TITULAR           05941014
                                          W-DOMICILI1.                  05942014
      *                                                                 05943014
           EXEC CICS                                                    05944014
              LINK PROGRAM (PE2C5201)                                   05945014
              COMMAREA     (W520-REGISTRO)                              05946014
              LENGTH       (LENGTH OF W520-REGISTRO)                    05947014
           END-EXEC                                                     05948014
      *                                                                 05949014
           IF EIBRESP NOT = DFHRESP(NORMAL)                             05950014
              MOVE 'ERROR EN PE2C5201'  TO ABC-REFERENCIA               05951014
              MOVE 'PE2C5201'           TO ABC-OBJETO-ERROR             05952014
              PERFORM 999-ABEND-CICS                                    05953014
           END-IF                                                       05954014
      *                                                                 05955014
           EVALUATE W520-PECRETOR                                       05956014
               WHEN ZEROS                                               05957014
                    CONTINUE                                            05958014
               WHEN 10                                                  05959014
                    MOVE '********* CLIENTE INEXISTENTE' TO NOM0101O    05960014
                    MOVE  -1                  TO TIT0101L               05961014
                    MOVE 'VLE0677'            TO CAA-COD-ERROR          05962014
                    PERFORM 3-FINAL                                     05963014
               WHEN 99                                                  05964014
                    INITIALIZE   QGECABC                                05965014
                    MOVE 'LINK PE2C5201'        TO ABC-REFERENCIA       05966014
                    MOVE W520-TABLENAME         TO ABC-OBJETO-ERROR     05967014
                    PERFORM 999-ABEND-DB2                               05968014
               WHEN OTHER                                               05969014
                    MOVE  -1                  TO TIT0101L               05970014
                    MOVE 'VLE0907'            TO CAA-COD-ERROR          05971014
                    MOVE 'PE2C5201'           TO CAA-VAR1-ERROR         05972014
                    MOVE W520-PECRETOR        TO CAA-VAR2-ERROR         05973014
                    PERFORM 3-FINAL                                     05974014
           END-EVALUATE.                                                05975014
      *                                                                 05976014
           STRING W520-IDEDIRE1 ' ' W520-DIREC1   ' '                   05977014
                  W520-DIREC3   ' ' W520-APTTO    ' '                   05978014
                  W520-IDEDIRE2 ' ' W520-DIREC2   ' '                   05979014
                  W520-POBLACI  ' ' W520-CODPOST  DELIMITED BY '  '     05980014
                                    INTO W-DOMICILI1.                   05981014
                                                                        05982014
           IF W520-PECRETOR = ZEROS                                     05983014
      * CONTROL PARA TIP.CLIENTE 'E'                                    05984014
              IF W520-SUJGRUP = 'F'                                     05985014
                 MOVE   'N'            TO SW-PERSONA                    05986014
              ELSE                                                      05987014
                 MOVE   'J'            TO SW-PERSONA                    05988014
              END-IF                                                    05989014
              MOVE W520-SUJGRUP        TO W-JURIDI                      05990014
              IF W520-SUJGRUP = 'F'                                     05991014
                 STRING W520-NOMBRE DELIMITED BY '  ' ' '               05992014
                        W520-PRIAPE DELIMITED BY '  ' ' '               05993014
                        W520-SEGAPE DELIMITED BY '  '                   05994014
                                             INTO WS-VL01-L05-TITULAR   05995014
                 IF TCL0101I = SPACES                                   05996014
                    MOVE   'N'                 TO TCL0101O              05997014
                 ELSE                                                   05998014
                     IF TCL0101I NOT = 'N' AND                          05999014
                        TCL0101I NOT = 'P' AND                          06000014
                        TCL0101I NOT = 'B' AND                          06001014
                        TCL0101I NOT = 'E'                              06002014
                        MOVE -1          TO TCL0101L                    06003014
                        MOVE 'VLE1517'   TO CAA-COD-ERROR               06004014
                        PERFORM 3-FINAL                                 06005014
                     END-IF                                             06006014
                 END-IF                                                 06007014
              ELSE                                                      06008014
                 STRING W520-NOMBRE DELIMITED BY SIZE                   06009014
                        W520-PRIAPE DELIMITED BY SIZE                   06010014
                        W520-SEGAPE DELIMITED BY SIZE                   06011014
                                         INTO WS-VL01-L05-TITULAR       06012014
                 IF TCL0101I = SPACES                                   06013014
                        MOVE   'J'         TO TCL0101O                  06014014
                 ELSE                                                   06015014
                    IF TCL0101I = 'N' OR 'P'                            06016014
                       MOVE -1          TO TCL0101L                     06017014
                       MOVE 'VLE1518'   TO CAA-COD-ERROR                06018014
                       PERFORM 3-FINAL                                  06019014
                    END-IF                                              06020014
      * COMNTROL TIP.CLI. 'E'                                           06021014
                    IF TCL0101I = 'E' AND SW-PERSONA = 'N'              06022014
                        MOVE -1          TO TCL0101L                    06023014
                        MOVE 'VLE1518'   TO CAA-COD-ERROR               06024014
                        PERFORM 3-FINAL                                 06025014
                     END-IF                                             06026014
                 END-IF                                                 06027014
              END-IF                                                    06028014
              MOVE WS-VL01-L05-TITULAR   TO NOM0101O                    06029014
           END-IF.                                                      06030014
                                                                        06031014
           IF PAI0101O = SPACES                                         06032014
              MOVE W520-CODPAIS(1:3)     TO PAI0101O                    06033014
           END-IF.                                                      06034014
      *                                                                 06035014
       BUSCAR-TITULAR-FIN.                                              06036014
           EXIT.                                                        06037014
                                                                        06038014
       VER-TITULAR2.                                                    06039014
           MOVE SPACES TO WS-VL01-L07-TITULAR  WS-VL01-L09-TITULAR      06040014
                          WS-VL01-L07-TIPDOC   WS-VL01-L09-TIPDOC       06041014
                          WS-VL01-L07-NRODOC   WS-VL01-L09-NRODOC       06042014
                                                                        06043014
                          WS-VL01-L08-TXTDIRE  WS-VL01-L10-TXTDIRE      06044014
                          WS-VL01-L08-DIRECCI  WS-VL01-L10-DIRECCI      06045014
                                                                        06046014
                          WS-VL01-L11-TITULAR  WS-VL01-L13-TITULAR      06047014
                          WS-VL01-L11-TIPDOC   WS-VL01-L13-TIPDOC       06048014
                          WS-VL01-L11-NRODOC   WS-VL01-L13-NRODOC       06049014
                                                                        06050014
                          WS-VL01-L12-TXTDIRE  WS-VL01-L14-TXTDIRE      06051014
                          WS-VL01-L12-DIRECCI  WS-VL01-L14-DIRECCI      06052014
                                                                        06053014
                          WS-VL01-L15-TITULAR  WS-VL01-L17-TITULAR      06054014
                          WS-VL01-L15-TIPDOC   WS-VL01-L17-TIPDOC       06055014
                          WS-VL01-L15-NRODOC   WS-VL01-L17-NRODOC       06056014
                                                                        06057014
                          WS-VL01-L16-TXTDIRE  WS-VL01-L18-TXTDIRE      06058014
                          WS-VL01-L16-DIRECCI  WS-VL01-L18-DIRECCI      06059014
                                                                        06060014
                          WS-VL01-L19-TITULAR  WS-VL01-L21-TITULAR      06061014
                          WS-VL01-L19-TIPDOC   WS-VL01-L21-TIPDOC       06062014
                          WS-VL01-L19-NRODOC   WS-VL01-L21-NRODOC       06063014
                                                                        06064014
                          WS-VL01-L20-TXTDIRE  WS-VL01-L22-TXTDIRE      06065014
                          WS-VL01-L20-DIRECCI  WS-VL01-L22-DIRECCI      06066014
                           W-2DOS-TITULARES     W-2DOS-DIRECC.          06067014
      *                                                                 06068014
      *    IF VARC-CLMAST = 'S'                                         06069014
              MOVE VARC-CUENTA    TO VADT-CUENTA                        06070014
      *                                                                 06071014
              PERFORM ABRIR-CURSOR-VADT                                 06072014
                 THRU ABRIR-CURSOR-VADT-FIN                             06073014
      *                                                                 06074014
              PERFORM LEER-CURSOR-VADT                                  06075014
                 THRU LEER-CURSOR-VADT-FIN                              06076014
      *                                                                 06077014
              MOVE ZEROS                     TO  W-COUNT                06078014
              PERFORM UNTIL SQLCODE = 100                               06079014
                 IF VADT-CLTITU = '4'                                   06080014
                    INITIALIZE                      W520-REGISTRO       06081014
                                                    W-2DOS-TITULARES    06082014
                                                    W-2DOS-DIRECC       06083014
                    MOVE VADT-NUMCLI            TO  W520-NUMCLIEN       06084014
      *                                                                 06085014
                    EXEC CICS                                           06086014
                         LINK PROGRAM (PE2C5201)                        06087014
                         COMMAREA     (W520-REGISTRO)                   06088014
                         LENGTH       (LENGTH OF W520-REGISTRO)         06089014
                    END-EXEC                                            06090014
      *                                                                 06091014
                    IF EIBRESP NOT = DFHRESP(NORMAL)                    06092014
                       MOVE 'ERROR EN PE2C5201-2' TO ABC-REFERENCIA     06093014
                       MOVE 'PE2C5201'            TO ABC-OBJETO-ERROR   06094014
                       PERFORM 999-ABEND-CICS                           06095014
                    END-IF                                              06096014
      *                                                                 06097014
                    EVALUATE W520-PECRETOR                              06098014
                        WHEN ZEROS                                      06099014
                             ADD   1                   TO W-COUNT       06100014
                        WHEN 99                                         06101014
                             INITIALIZE   QGECABC                       06102014
                             MOVE 'LINK PE2C5201'      TO ABC-REFERENCIA06103014
                             MOVE W520-TABLENAME     TO ABC-OBJETO-ERROR06104014
                             PERFORM 999-ABEND-DB2                      06105014
                        WHEN OTHER                                      06106014
                             MOVE  -1                  TO TIT0101L      06107014
                             MOVE 'VLE0907'            TO CAA-COD-ERROR 06108014
                             MOVE 'PE2C5201'           TO CAA-VAR1-ERROR06109014
                             MOVE W520-PECRETOR        TO CAA-VAR2-ERROR06110014
                             PERFORM 3-FINAL                            06111014
                    END-EVALUATE                                        06112014
      *                                                                 06113014
                    IF W520-PECRETOR  = ZEROS                           06114014
                       IF W520-SUJGRUP = 'F'                            06115014
                          STRING W520-NOMBRE DELIMITED BY '  ' ' '      06116014
                                 W520-PRIAPE DELIMITED BY '  ' ' '      06117014
                                 W520-SEGAPE DELIMITED BY '  '          06118014
                                                  INTO W-2DOS-TITULARES 06119014
                       ELSE                                             06120014
                          STRING W520-NOMBRE DELIMITED BY SIZE          06121014
                                 W520-PRIAPE DELIMITED BY SIZE          06122014
                                 W520-SEGAPE DELIMITED BY SIZE          06123014
                                                  INTO W-2DOS-TITULARES 06124014
                       END-IF                                           06125014
                                                                        06126014
                       STRING W520-IDEDIRE1 ' ' W520-DIREC1   ' '       06127014
                              W520-DIREC3   ' ' W520-APTTO    ' '       06128014
                              W520-IDEDIRE2 ' ' W520-DIREC2   ' '       06129014
                              W520-POBLACI  ' ' W520-CODPOST            06130014
                              DELIMITED BY '  '                         06131014
                              INTO W-2DOS-DIRECC                        06132014
      *                                                                 06133014
                       EVALUATE W-COUNT                                 06134014
                       WHEN 1                                           06135014
                           MOVE W-2DOS-TITULARES  TO WS-VL01-L07-TITULAR06136014
                           MOVE W520-CODIDENT     TO WS-VL01-L07-TIPDOC 06137014
                           MOVE W520-CLAIDENT     TO WS-VL01-L07-NRODOC 06138014
                                                                        06139014
                           MOVE  'DIRECCION : '   TO WS-VL01-L08-TXTDIRE06140014
                           MOVE W-2DOS-DIRECC     TO WS-VL01-L08-DIRECCI06141014
                       WHEN 2                                           06142014
                           MOVE W-2DOS-TITULARES  TO WS-VL01-L09-TITULAR06143014
                           MOVE W520-CODIDENT     TO WS-VL01-L09-TIPDOC 06144014
                           MOVE W520-CLAIDENT     TO WS-VL01-L09-NRODOC 06145014
                                                                        06146014
                           MOVE  'DIRECCION : '   TO WS-VL01-L10-TXTDIRE06147014
                           MOVE W-2DOS-DIRECC     TO WS-VL01-L10-DIRECCI06148014
                       WHEN 3                                           06149014
                           MOVE W-2DOS-TITULARES  TO WS-VL01-L11-TITULAR06150014
                           MOVE W520-CODIDENT     TO WS-VL01-L11-TIPDOC 06151014
                           MOVE W520-CLAIDENT     TO WS-VL01-L11-NRODOC 06152014
                                                                        06153014
                           MOVE  'DIRECCION : '   TO WS-VL01-L12-TXTDIRE06154014
                           MOVE W-2DOS-DIRECC     TO WS-VL01-L12-DIRECCI06155014
                       WHEN 4                                           06156014
                           MOVE W-2DOS-TITULARES  TO WS-VL01-L13-TITULAR06157014
                           MOVE W520-CODIDENT     TO WS-VL01-L13-TIPDOC 06158014
                           MOVE W520-CLAIDENT     TO WS-VL01-L13-NRODOC 06159014
                                                                        06160014
                           MOVE  'DIRECCION : '   TO WS-VL01-L14-TXTDIRE06161014
                           MOVE W-2DOS-DIRECC     TO WS-VL01-L14-DIRECCI06162014
                       WHEN 5                                           06163014
                           MOVE W-2DOS-TITULARES  TO WS-VL01-L15-TITULAR06164014
                           MOVE W520-CODIDENT     TO WS-VL01-L15-TIPDOC 06165014
                           MOVE W520-CLAIDENT     TO WS-VL01-L15-NRODOC 06166014
                                                                        06167014
                           MOVE  'DIRECCION : '   TO WS-VL01-L16-TXTDIRE06168014
                           MOVE W-2DOS-DIRECC     TO WS-VL01-L16-DIRECCI06169014
                       WHEN 6                                           06170014
                           MOVE W-2DOS-TITULARES  TO WS-VL01-L17-TITULAR06171014
                           MOVE W520-CODIDENT     TO WS-VL01-L17-TIPDOC 06172014
                           MOVE W520-CLAIDENT     TO WS-VL01-L17-NRODOC 06173014
                                                                        06174014
                           MOVE  'DIRECCION : '   TO WS-VL01-L18-TXTDIRE06175014
                           MOVE W-2DOS-DIRECC     TO WS-VL01-L18-DIRECCI06176014
                       WHEN 7                                           06177014
                           MOVE W-2DOS-TITULARES  TO WS-VL01-L19-TITULAR06178014
                           MOVE W520-CODIDENT     TO WS-VL01-L19-TIPDOC 06179014
                           MOVE W520-CLAIDENT     TO WS-VL01-L19-NRODOC 06180014
                                                                        06181014
                           MOVE  'DIRECCION : '   TO WS-VL01-L20-TXTDIRE06182014
                           MOVE W-2DOS-DIRECC     TO WS-VL01-L20-DIRECCI06183014
                       WHEN 8                                           06184014
                           MOVE W-2DOS-TITULARES  TO WS-VL01-L21-TITULAR06185014
                           MOVE W520-CODIDENT     TO WS-VL01-L21-TIPDOC 06186014
                           MOVE W520-CLAIDENT     TO WS-VL01-L21-NRODOC 06187014
                                                                        06188014
                           MOVE  'DIRECCION : '   TO WS-VL01-L22-TXTDIRE06189014
                           MOVE W-2DOS-DIRECC     TO WS-VL01-L22-DIRECCI06190014
                       END-EVALUATE                                     06191014
                    END-IF                                              06192014
                 END-IF                                                 06193014
      *                                                                 06194014
                 PERFORM LEER-CURSOR-VADT                               06195014
                    THRU LEER-CURSOR-VADT-FIN                           06196014
      *                                                                 06197014
              END-PERFORM                                               06198014
      *                                                                 06199014
              PERFORM CERRAR-CURSOR-VADT                                06200014
                 THRU CERRAR-CURSOR-VADT-FIN.                           06201014
      *                                                                 06202014
      *    END-IF.                                                      06203014
      *                                                                 06204014
       VER-TITULAR2-F.                                                  06205014
           EXIT.                                                        06206014
      *                                                                 06207014
       VER-REPRESEN.                                                    06208014
           MOVE SPACES TO WS-VL01-L24-REPR01   WS-VL01-L26-REPR02       06209014
                          WS-VL01-L24-NOMB01   WS-VL01-L26-NOMB02       06210014
                          WS-VL01-L24-TIPO01   WS-VL01-L26-TIPO02       06211014
                                                                        06212014
                          WS-VL01-L25-VCTO01   WS-VL01-L27-VCTO02       06213014
                          WS-VL01-L25-FACU01   WS-VL01-L27-FACU02       06214014
                                                                        06215014
                          WS-VL01-L28-REPR03   WS-VL01-L30-REPR04       06216014
                          WS-VL01-L28-NOMB03   WS-VL01-L30-NOMB04       06217014
                          WS-VL01-L28-TIPO03   WS-VL01-L30-TIPO04       06218014
                                                                        06219014
                          WS-VL01-L29-VCTO03   WS-VL01-L31-VCTO04       06220014
                          WS-VL01-L29-FACU03   WS-VL01-L31-FACU04       06221014
                                                                        06222014
                           W-2DOS-TITULARES.                            06223014
      *                                                                 06224014
      *    IF VARC-CLMAST = 'S'                                         06225014
              MOVE VARC-CUENTA    TO VADT-CUENTA                        06226014
      *                                                                 06227014
              PERFORM ABRIR-CURSOR-VADT                                 06228014
                 THRU ABRIR-CURSOR-VADT-FIN                             06229014
      *                                                                 06230014
              PERFORM LEER-CURSOR-VADT                                  06231014
                 THRU LEER-CURSOR-VADT-FIN                              06232014
      *                                                                 06233014
              MOVE ZEROS                     TO  W-COUNT                06234014
              PERFORM UNTIL SQLCODE = 100                               06235014
                      OR    W-COUNT >  4                                06236014
                 IF VADT-CLTITU NOT = '4'                               06237014
                    INITIALIZE                      W520-REGISTRO       06238014
                                                    W-2DOS-TITULARES    06239014
                    MOVE VADT-NUMCLI            TO  W520-NUMCLIEN       06240014
      *                                                                 06241014
                    EXEC CICS                                           06242014
                         LINK PROGRAM (PE2C5201)                        06243014
                         COMMAREA     (W520-REGISTRO)                   06244014
                         LENGTH       (LENGTH OF W520-REGISTRO)         06245014
                    END-EXEC                                            06246014
      *                                                                 06247014
                    IF EIBRESP NOT = DFHRESP(NORMAL)                    06248014
                       MOVE 'ERROR EN PE2C5201-2' TO ABC-REFERENCIA     06249014
                       MOVE 'PE2C5201'            TO ABC-OBJETO-ERROR   06250014
                       PERFORM 999-ABEND-CICS                           06251014
                    END-IF                                              06252014
      *                                                                 06253014
                    EVALUATE W520-PECRETOR                              06254014
                        WHEN ZEROS                                      06255014
                             ADD   1                   TO W-COUNT       06256014
                        WHEN 99                                         06257014
                             INITIALIZE   QGECABC                       06258014
                             MOVE 'LINK PE2C5201'      TO ABC-REFERENCIA06259014
                             MOVE W520-TABLENAME     TO ABC-OBJETO-ERROR06260014
                             PERFORM 999-ABEND-DB2                      06261014
                        WHEN OTHER                                      06262014
                             MOVE  -1                  TO TIT0101L      06263014
                             MOVE 'VLE0907'            TO CAA-COD-ERROR 06264014
                             MOVE 'PE2C5201'           TO CAA-VAR1-ERROR06265014
                             MOVE W520-PECRETOR        TO CAA-VAR2-ERROR06266014
                             PERFORM 3-FINAL                            06267014
                    END-EVALUATE                                        06268014
      *                                                                 06269014
                    IF W520-PECRETOR  = ZEROS                           06270014
                       IF W520-SUJGRUP = 'F'                            06271014
                          STRING W520-NOMBRE DELIMITED BY '  ' ' '      06272014
                                 W520-PRIAPE DELIMITED BY '  ' ' '      06273014
                                 W520-SEGAPE DELIMITED BY '  '          06274014
                                                  INTO W-2DOS-TITULARES 06275014
                       ELSE                                             06276014
                          STRING W520-NOMBRE DELIMITED BY SIZE          06277014
                                 W520-PRIAPE DELIMITED BY SIZE          06278014
                                 W520-SEGAPE DELIMITED BY SIZE          06279014
                                                  INTO W-2DOS-TITULARES 06280014
                       END-IF                                           06281014
                                                                        06282014
                       MOVE VADT-FEVENCTO     TO W-FECHA-AMD-N          06283014
                       MOVE W-AA-AMD          TO W-AA-DMA-G             06284014
                       MOVE W-MM-AMD          TO W-MM-DMA-G             06285014
                       MOVE W-DD-AMD          TO W-DD-DMA-G             06286014
                                                                        06287014
                       EVALUATE VADT-CLTITU                             06288014
                       WHEN 2                                           06289014
                         IF VADT-ADMIN = 'S'                            06290014
                            MOVE 'ADMIN. CARTERA      ' TO W-TIPO-VINCUL06291014
                            MOVE W-MSG-001              TO W-FACULTAD   06292014
                         ELSE                                           06293014
                            MOVE 'REPRESENTANTE       ' TO W-TIPO-VINCUL06294014
                            MOVE W-MSG-002              TO W-FACULTAD   06295014
                         END-IF                                         06296014
                       WHEN 3                                           06297014
                            MOVE 'AUTORIZADO          ' TO W-TIPO-VINCUL06298014
                            MOVE W-MSG-003              TO W-FACULTAD   06299014
                       WHEN 8                                           06300014
                            MOVE 'USUFRUCTUARIO       ' TO W-TIPO-VINCUL06301014
                            MOVE W-MSG-004              TO W-FACULTAD   06302014
                       WHEN OTHER                                       06303014
                            MOVE SPACES                 TO W-TIPO-VINCUL06304014
                                                           W-FACULTAD   06305014
                       END-EVALUATE                                     06306014
                                                                        06307014
                       EVALUATE W-COUNT                                 06308014
                       WHEN 1                                           06309014
                           MOVE VADT-NUMCLI       TO WS-VL01-L24-REPR01 06310014
                           MOVE W-2DOS-TITULARES  TO WS-VL01-L24-NOMB01 06311014
                           MOVE W-TIPO-VINCUL     TO WS-VL01-L24-TIPO01 06312014
                                                                        06313014
                           MOVE W-FECHA-DMA-G     TO WS-VL01-L25-VCTO01 06314014
                           MOVE W-FACULTAD        TO WS-VL01-L25-FACU01 06315014
                       WHEN 2                                           06316014
                           MOVE VADT-NUMCLI       TO WS-VL01-L26-REPR02 06317014
                           MOVE W-2DOS-TITULARES  TO WS-VL01-L26-NOMB02 06318014
                           MOVE W-TIPO-VINCUL     TO WS-VL01-L26-TIPO02 06319014
                                                                        06320014
                           MOVE W-FECHA-DMA-G     TO WS-VL01-L27-VCTO02 06321014
                           MOVE W-FACULTAD        TO WS-VL01-L27-FACU02 06322014
                       WHEN 3                                           06323014
                           MOVE VADT-NUMCLI       TO WS-VL01-L28-REPR03 06324014
                           MOVE W-2DOS-TITULARES  TO WS-VL01-L28-NOMB03 06325014
                           MOVE W-TIPO-VINCUL     TO WS-VL01-L28-TIPO03 06326014
                                                                        06327014
                           MOVE W-FECHA-DMA-G     TO WS-VL01-L29-VCTO03 06328014
                           MOVE W-FACULTAD        TO WS-VL01-L29-FACU03 06329014
                       WHEN 4                                           06330014
                           MOVE VADT-NUMCLI       TO WS-VL01-L30-REPR04 06331014
                           MOVE W-2DOS-TITULARES  TO WS-VL01-L30-NOMB04 06332014
                           MOVE W-TIPO-VINCUL     TO WS-VL01-L30-TIPO04 06333014
                                                                        06334014
                           MOVE W-FECHA-DMA-G     TO WS-VL01-L31-VCTO04 06335014
                           MOVE W-FACULTAD        TO WS-VL01-L31-FACU04 06336014
                       END-EVALUATE                                     06337014
                    END-IF                                              06338014
                 END-IF                                                 06339014
      *                                                                 06340014
                 PERFORM LEER-CURSOR-VADT                               06341014
                    THRU LEER-CURSOR-VADT-FIN                           06342014
      *                                                                 06343014
              END-PERFORM                                               06344014
      *                                                                 06345014
              PERFORM CERRAR-CURSOR-VADT                                06346014
                 THRU CERRAR-CURSOR-VADT-FIN.                           06347014
      *                                                                 06348014
      *    END-IF.                                                      06349014
      *                                                                 06350014
       VER-REPRESEN-F.                                                  06351014
           EXIT.                                                        06352014
       DIREC-CORRES.                                                    06353014
           INITIALIZE                PEWC5400                           06354014
                                     WS-VL01-L23-DIRECORR.              06355014
           MOVE END0101O          TO W540-PECENTID                      06356014
           MOVE CEN0101O          TO W540-OFIAPE                        06357014
           MOVE PRD0101O          TO W540-CODISER                       06358014
           MOVE CTA0101O          TO W540-NUMECTA (01:07)               06359014
           MOVE DG20101O          TO W540-NUMECTA (08:01)               06360014
           MOVE 'R'               TO W540-IDIOMA.                       06361014
           EXEC CICS                                                    06362014
                LINK PROGRAM (PE2C5400)                                 06363014
                COMMAREA (PEWC5400)                                     06364014
           END-EXEC.                                                    06365014
      *                                                                 06366014
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         06367014
              MOVE 'ERROR EN PE2C5400'  TO ABC-REFERENCIA               06368014
              MOVE 'PE2C5400'           TO ABC-OBJETO-ERROR             06369014
              PERFORM 999-ABEND-CICS                                    06370014
           END-IF.                                                      06371014
                                                                        06372014
           EVALUATE W540-PECRETOR                                       06373014
               WHEN '00'                                                06374014
                    CONTINUE                                            06375014
               WHEN OTHER                                               06376014
                    MOVE 'VLE0907'      TO CAA-COD-ERROR                06377014
                    MOVE 'PE2C5400'     TO CAA-VAR1-ERROR               06378014
                    MOVE W540-PECRETOR  TO CAA-VAR2-ERROR               06379014
                    PERFORM 3-FINAL                                     06380014
           END-EVALUATE.                                                06381014
           STRING W540-IDEDIRE1 ' ' W540-DIREC1   ' '                   06382014
                  W540-DIREC3   ' ' W540-APTTO    ' '                   06383014
                  W540-IDEDIRE2 ' ' W540-DIREC2   ' '                   06384014
                  W540-POBLACI  ' ' W540-CODPOST  DELIMITED BY '  '     06385014
                                    INTO WS-VL01-L23-DIRECORR.          06386014
       DIREC-CORRES-F.                                                  06387014
           EXIT.                                                        06388014
       LLAMAR-SEGUNDA-RUTINA.                                           06389014
      *                                                                 06390014
           INITIALIZE W-PEWC4390                                        06391014
      *                                                                 06392014
           MOVE TIT0101I                    TO  W4390-NUMCLIEN          06393014
      *                                                                 06394014
           MOVE W-ENTIDAD-ANT               TO  W4390-PECENTID-I        06395014
           MOVE W-OFICINA-ANT               TO  W4390-OFIAPE-I          06396014
           MOVE W-CODISER-ANT               TO  W4390-CODISER-I         06397014
           MOVE W-CUENTA-ANT                TO  W4390-NUMECTA-I         06398014
      *                                                                 06399014
           EXEC CICS                                                    06400014
                LINK PROGRAM (PE2C5390)                                 06401014
                COMMAREA   (W-PEWC4390)                                 06402014
           END-EXEC.                                                    06403014
      *                                                                 06404014
           IF EIBRESP NOT = DFHRESP(NORMAL)                             06405014
              MOVE 'ERROR EN PE2C5390'      TO  ABC-REFERENCIA          06406014
              MOVE 'PE2C5390'               TO  ABC-OBJETO-ERROR        06407014
              PERFORM 999-ABEND-CICS                                    06408014
           END-IF.                                                      06409014
      *                                                                 06410014
           EVALUATE W4390-PCRETOR                                       06411014
               WHEN '00'                                                06412014
               WHEN '20'                                                06413014
               WHEN '30'                                                06414014
               WHEN '40'                                                06415014
                    CONTINUE                                            06416014
               WHEN OTHER                                               06417014
                    MOVE 'VLE0907'           TO CAA-COD-ERROR           06418014
                    MOVE 'PE2C5390'          TO CAA-VAR1-ERROR          06419014
                    MOVE W4390-PCRETOR       TO CAA-VAR2-ERROR          06420014
                    PERFORM 3-FINAL                                     06421014
           END-EVALUATE.                                                06422014
      *                                                                 06423014
       LLAMAR-SEGUNDA-RUTINA-FIN.                                       06424014
                      EXIT.                                             06425014
      *                                                                 06426014
      *                                                                 06427014
       BUSCAR-CTASCLI.                                                  06428014
      *                                                                 06429014
           INITIALIZE W-PEWC4390                                        06430014
      *                                                                 06431014
           MOVE TIT0101I                    TO  W4390-NUMCLIEN          06432014
      *                                                                 06433014
           EXEC CICS                                                    06434014
                LINK PROGRAM (PE2C5390)                                 06435014
                COMMAREA   (W-PEWC4390)                                 06436014
           END-EXEC.                                                    06437014
      *                                                                 06438014
           IF EIBRESP NOT = DFHRESP(NORMAL)                             06439014
              MOVE 'ERROR EN PE2C5390'      TO  ABC-REFERENCIA          06440014
              MOVE 'PE2C5390'               TO  ABC-OBJETO-ERROR        06441014
              PERFORM 999-ABEND-CICS                                    06442014
           END-IF.                                                      06443014
      *                                                                 06444014
           EVALUATE W4390-PCRETOR                                       06445014
               WHEN '00'                                                06446014
               WHEN '20'                                                06447014
               WHEN '30'                                                06448014
               WHEN '40'                                                06449014
                    CONTINUE                                            06450014
               WHEN OTHER                                               06451014
                    MOVE 'VLE0907'           TO CAA-COD-ERROR           06452014
                    MOVE 'PE2C5390'          TO CAA-VAR1-ERROR          06453014
                    MOVE W4390-PCRETOR       TO CAA-VAR2-ERROR          06454014
                    PERFORM 3-FINAL                                     06455014
           END-EVALUATE.                                                06456014
      *                                                                 06457014
       BUSCAR-CTASCLI-FIN. EXIT.                                        06458014
      *                                                                 06459014
      *A2012-INICIO                                                     06460014
       ABRIR-CURSOR-VADT.                                               06461014
      *                                                                 06462014
           EXEC SQL                                                     06463014
               OPEN VLDCADT1                                            06464014
           END-EXEC.                                                    06465014
      *                                                                 06466014
           IF SQLCODE NOT = ZEROS                                       06467014
              INITIALIZE   QGECABC                                      06468014
              MOVE 'OPEN'        TO  ABC-REFERENCIA                     06469014
              MOVE 'VLDTADT'     TO  ABC-OBJETO-ERROR                   06470014
              PERFORM 999-ABEND-DB2                                     06471014
           END-IF.                                                      06472014
      *                                                                 06473014
       ABRIR-CURSOR-VADT-FIN.                                           06474014
           EXIT.                                                        06475014
      *                                                                 06476014
       LEER-CURSOR-VADT.                                                06477014
      *                                                                 06478014
           EXEC SQL                                                     06479014
                FETCH VLDCADT1                                          06480014
JPC@1 *         INTO :DCLVLDTADT                                        06481014
                INTO   :VADT-CUENTA                                     06482014
                     , :VADT-NUMCLI                                     06483014
                     , :VADT-CLTITU                                     06484014
                     , :VADT-NUMDOM                                     06485014
                     , :VADT-ADMIN                                      06486014
                     , :VADT-FEVENCTO                                   06487014
                     , :VADT-FEALTREG                                   06488014
                     , :VADT-FEULMOD                                    06489014
                     , :VADT-HORULMOD                                   06490014
                     , :VADT-NUMTER                                     06491014
                     , :VADT-USUARIO                                    06492014
           END-EXEC.                                                    06493014
      *                                                                 06494014
           MOVE SQLCODE TO SQLCODE-AUX                                  06495014
      *                                                                 06496014
           EVALUATE TRUE                                                06497014
              WHEN DB2-OK                                               06498014
      *A2011-RUTLOG-I                                                   06499014
                   INITIALIZE W-VLWCLOG0                                06500014
                              LOGVLDTADT                                06501014
                   MOVE 'VLDTADT'             TO  VL7LOG-TABLA          06502014
                   MOVE 'FETCH'               TO  VL7LOG-OPERACION      06503014
                   MOVE LENGTH OF DCLVLDTADT  TO  VL7LOG-REGISTRO-LEN   06504014
                   MOVE DCLVLDTADT            TO  LOGVLDTADT            06505014
                   MOVE LOGVLDTADT            TO  VL7LOG-REGISTRO-TEXT  06506014
                   PERFORM LLAMAR-VL7CRLOG                              06507014
                      THRU LLAMAR-VL7CRLOG-FIN                          06508014
      *A2011-RUTLOG-F                                                   06509014
              WHEN DB2-NOTFND                                           06510014
                   CONTINUE                                             06511014
                                                                        06512014
              WHEN OTHER                                                06513014
                   INITIALIZE   QGECABC                                 06514014
                   MOVE 'FETCH'       TO  ABC-REFERENCIA                06515014
                   MOVE 'VLDTADT'     TO  ABC-OBJETO-ERROR              06516014
                   PERFORM 999-ABEND-DB2                                06517014
                                                                        06518014
           END-EVALUATE.                                                06519014
                                                                        06520014
      *                                                                 06521014
       LEER-CURSOR-VADT-FIN.                                            06522014
           EXIT.                                                        06523014
      *                                                                 06524014
       CERRAR-CURSOR-VADT.                                              06525014
      *                                                                 06526014
           EXEC SQL                                                     06527014
                CLOSE VLDCADT1                                          06528014
           END-EXEC.                                                    06529014
      *                                                                 06530014
           IF SQLCODE NOT = ZEROS                                       06531014
              INITIALIZE   QGECABC                                      06532014
              MOVE 'CLOSE'       TO  ABC-REFERENCIA                     06533014
              MOVE 'VLDTADT'     TO  ABC-OBJETO-ERROR                   06534014
              PERFORM 999-ABEND-DB2                                     06535014
           END-IF.                                                      06536014
      *                                                                 06537014
       CERRAR-CURSOR-VADT-FIN.                                          06538014
           EXIT.                                                        06539014
      *                                                                 06540014
      *A2012-FIN                                                        06541014
      *                                                                 06542014
       BUSCAR-CTAS.                                                     06543014
      *                                                                 06544014
           MOVE 'NO'          TO SW-CTACLI                              06545014
           MOVE 'S'           TO SW-HAYMAS                              06546014
      *                                                                 06547014
           PERFORM UNTIL SW-CTACLI = 'SI' OR SW-HAYMAS NOT = 'S'        06548014
              PERFORM VARYING  W-IND   FROM 1 BY 1                      06549014
                        UNTIL (W-IND > W4390-NOCCURS) OR                06550014
                              (W-IND > 100)                             06551014
                 MOVE W4390-PECENTID (W-IND) TO  W-ENTIDAD-ANT          06552014
                 MOVE W4390-OFIAPE   (W-IND) TO  W-OFICINA-ANT          06553014
                 MOVE W4390-CODISER  (W-IND) TO  W-CODISER-ANT          06554014
                 MOVE W4390-NUMECTA  (W-IND) TO  W-CUENTA-ANT           06555014
                 IF  (W4390-CODISER  (W-IND)  =  '01' OR '02') AND      06556014
                     (W4390-CLAINTER (W-IND)  =  'T')                   06557014
                     IF NCC0101I = SPACES AND VALCAR                    06558014
                        MOVE W4390-PECENTID (W-IND) TO NCC0101I(01:04)  06559014
                        MOVE W4390-OFIAPE   (W-IND) TO NCC0101I(05:04)  06560014
                        MOVE '00'                   TO NCC0101I(09:02)  06561014
                        MOVE W4390-CODISER  (W-IND) TO NCC0101I(11:02)  06562014
                        MOVE W4390-NUMECTA  (W-IND) TO NCC0101I(13:08)  06563014
                        PERFORM VALCAR-CTAS2 THRU VALCAR-CTAS2-FIN      06564014
                     END-IF                                             06565014
                     IF NC20101I = SPACES AND VALABO                    06566014
                       MOVE W4390-PECENTID  (W-IND) TO NC20101I(01:04)  06567014
                       MOVE W4390-OFIAPE    (W-IND) TO NC20101I(05:04)  06568014
                       MOVE '00'                    TO NC20101I(09:02)  06569014
                       MOVE W4390-CODISER   (W-IND) TO NC20101I(11:02)  06570014
                       MOVE W4390-NUMECTA   (W-IND) TO NC20101I(13:08)  06571014
                       PERFORM VALABO-CTAS2 THRU VALABO-CTAS2-FIN       06572014
                     END-IF                                             06573014
                 END-IF                                                 06574014
              END-PERFORM                                               06575014
              MOVE W4390-HAYMAS TO SW-HAYMAS                            06576014
              IF W4390-HAYMAS = 'S' AND SW-CTACLI = 'NO'                06577014
                 PERFORM LLAMAR-SEGUNDA-RUTINA                          06578014
                    THRU LLAMAR-SEGUNDA-RUTINA-FIN                      06579014
              END-IF                                                    06580014
           END-PERFORM.                                                 06581014
      *                                                                 06582014
       BUSCAR-CTAS-FIN.                                                 06583014
           EXIT.                                                        06584014
      *                                                                 06585014
       VALCAR-CTAS2.                                                    06586014
      *-------------                                                    06587014
      *                                                                 06588014
      *      SE VALIDARA QUE LA CTA. ESTE ACTIVA                        06589014
      *                                                                 06590014
      *  CTA. CARGO *****                                               06591014
      *                                                                 06592014
           IF NCC0101I IS NOT NUMERIC                                   06593014
              MOVE -1           TO NCC0101L                             06594014
              MOVE 'VLE1099'    TO CAA-COD-ERROR                        06595014
              PERFORM 3-FINAL                                           06596014
           END-IF                                                       06597014
      *                                                                 06598014
           INITIALIZE                          W-BGECMDC                06599014
           MOVE NCC0101I(1:4)                 TO MDC-ENTIDAD            06600014
           MOVE NCC0101I(5:4)                 TO MDC-CENTRO-ALTA.       06601014
           MOVE NCC0101I(11:2)                TO MDC-CUENTA(1:2).       06602014
           MOVE NCC0101I(13:8)                TO MDC-CUENTA(3:8).       06603014
      *                                                                 06604014
           EXEC CICS                                                    06605014
             LINK PROGRAM (BG2CMDC0)                                    06606014
             COMMAREA (BGECMDC)                                         06607014
           END-EXEC                                                     06608014
      *                                                                 06609014
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         06610014
              MOVE 'ERROR EN BGECMDC0'  TO ABC-REFERENCIA               06611014
              MOVE 'BG2CMDC0'           TO ABC-OBJETO-ERROR             06612014
              PERFORM 999-ABEND-CICS                                    06613014
           END-IF                                                       06614014
      *                                                                 06615014
           EVALUATE MDC-CODERR                                          06616014
             WHEN SPACES                                                06617014
JIPC  *           IF MDC-INDESTA = 'A' OR 'R' OR 'P'                    06618014
                  IF MDC-INDESTA = 'A'                                  06619014
                     MOVE 'SI'        TO SW-VALIDA-CAR                  06620014
                     MOVE 'SI'        TO SW-CTACLI                      06621014
                     MOVE 101         TO W-IND                          06622014
                     MOVE MDC-CDDIVIS TO W-MONEDA-OK                    06623014
                                         MON0101O                       06624014
                  ELSE                                                  06625014
                     MOVE SPACES      TO NCC0101I                       06626014
                  END-IF                                                06627014
             WHEN OTHER                                                 06628014
                  MOVE SPACES         TO NCC0101I                       06629014
      *           CONTINUE                                              06630014
           END-EVALUATE.                                                06631014
      *                                                                 06632014
      *                                                                 06633014
       VALCAR-CTAS2-FIN. EXIT.                                          06634014
      *-------------                                                    06635014
      *                                                                 06636014
       VALABO-CTAS2.                                                    06637014
      *-------------                                                    06638014
      *                                                                 06639014
      * VALIDAR QUE LA CTA. ESTE ACTIVA                                 06640014
      *                                                                 06641014
      *  CTA. ABONO *****                                               06642014
      *                                                                 06643014
           IF NC20101I IS NOT NUMERIC                                   06644014
              MOVE -1           TO NC20101L                             06645014
              MOVE 'VLE1100'    TO CAA-COD-ERROR                        06646014
              PERFORM 3-FINAL                                           06647014
           END-IF                                                       06648014
      *                                                                 06649014
           INITIALIZE                          W-BGECMDC                06650014
           MOVE NC20101I(1:4)                 TO MDC-ENTIDAD            06651014
           MOVE NC20101I(5:4)                 TO MDC-CENTRO-ALTA.       06652014
           MOVE NC20101I(11:2)                TO MDC-CUENTA(1:2).       06653014
           MOVE NC20101I(13:8)                TO MDC-CUENTA(3:8).       06654014
      *                                                                 06655014
           EXEC CICS                                                    06656014
             LINK PROGRAM (BG2CMDC0)                                    06657014
             COMMAREA (BGECMDC)                                         06658014
           END-EXEC                                                     06659014
      *                                                                 06660014
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         06661014
              MOVE 'ERROR EN BGECMDC0'  TO ABC-REFERENCIA               06662014
              MOVE 'BG2CMDC0'           TO ABC-OBJETO-ERROR             06663014
              PERFORM 999-ABEND-CICS                                    06664014
           END-IF                                                       06665014
      *                                                                 06666014
           EVALUATE MDC-CODERR                                          06667014
             WHEN SPACES                                                06668014
JIPC  *           IF MDC-INDESTA = 'A' OR 'R' OR 'P'                    06669014
                  IF MDC-INDESTA = 'A'                                  06670014
                     IF MDC-CDDIVIS NOT = W-MONEDA-OK                   06671014
                        CONTINUE                                        06672014
                     ELSE                                               06673014
                        MOVE 'SI'        TO SW-VALIDA-ABO               06674014
                        MOVE 'SI'        TO SW-CTACLI                   06675014
                        MOVE MDC-CDDIVIS TO MO20101O                    06676014
                        MOVE 101         TO W-IND                       06677014
                     END-IF                                             06678014
                  ELSE                                                  06679014
                     MOVE SPACES         TO NC20101I                    06680014
                  END-IF                                                06681014
             WHEN OTHER                                                 06682014
                  MOVE SPACES            TO NC20101I                    06683014
      *           CONTINUE                                              06684014
           END-EVALUATE.                                                06685014
      *                                                                 06686014
       VALABO-CTAS2-FIN. EXIT.                                          06687014
      *                                                                 06688014
      *                                                                 06689014
       VALIDAR-CTA-CAR.                                                 06690014
      *----------------                                                 06691014
      *                                                                 06692014
      *      SE VALIDARA QUE EXISTAN LA CUENTAS CON LOS PROD. VALIDOS   06693014
      *      Y ACTIVAS.                                                 06694014
      *                                                                 06695014
      *  CTA. CARGO *****                                               06696014
      *                                                                 06697014
           IF NCC0101I IS NOT NUMERIC                                   06698014
              MOVE -1           TO NCC0101L                             06699014
              MOVE 'VLE1099'    TO CAA-COD-ERROR                        06700014
              PERFORM 3-FINAL                                           06701014
           END-IF                                                       06702014
      *                                                                 06703014
           MOVE 'NO' TO SW-CTACLI                                       06704014
           MOVE 'S'  TO SW-HAYMAS                                       06705014
      *                                                                 06706014
           PERFORM UNTIL SW-CTACLI = 'SI' OR SW-HAYMAS NOT EQUAL 'S'    06707014
              PERFORM VARYING  W-IND   FROM 1 BY 1                      06708014
                        UNTIL (W-IND > W4390-NOCCURS) OR                06709014
                              (W-IND > 100)                             06710014
                 MOVE W4390-PECENTID (W-IND)    TO  W-ENTIDAD-ANT       06711014
                 MOVE W4390-OFIAPE   (W-IND)    TO  W-OFICINA-ANT       06712014
                 MOVE W4390-CODISER  (W-IND)    TO  W-CODISER-ANT       06713014
                 MOVE W4390-NUMECTA  (W-IND)    TO  W-CUENTA-ANT        06714014
      *200608070-INI                                                    06715014
      *          IF  (W4390-CODISER  (W-IND) = '01' OR '02') AND        06716014
      *200711038-INI                                                    06717014
      *          IF  (W4390-CODISER  (W-IND) = '01' OR '02' OR '49') AND06718014
                 IF  (W4390-CODISER  (W-IND) = '01' OR '02' OR '91') AND06719014
      *200711038-FIN                                                    06720014
      *200608070-FIN                                                    06721014
                     (W4390-CLAINTER (W-IND) = 'T')                     06722014
                     MOVE W4390-PECENTID(W-IND) TO  W-ENTIDAD-NUEVA     06723014
                     MOVE W4390-OFIAPE(W-IND)   TO  W-OFICINA-NUEVA     06724014
                     MOVE '00'                  TO  W-DIGCON-NUEVA      06725014
                     MOVE W4390-CODISER(W-IND)  TO  W-CODISER-NUEVA     06726014
                     MOVE W4390-NUMECTA(W-IND)  TO  W-CUENTA-NUEVA      06727014
                     IF NCC0101I = W-CUENTA-TOTAL                       06728014
                        MOVE 'SI' TO SW-CTACLI                          06729014
                        MOVE 101  TO W-IND                              06730014
                     END-IF                                             06731014
                 END-IF                                                 06732014
              END-PERFORM                                               06733014
              MOVE W4390-HAYMAS TO SW-HAYMAS                            06734014
              IF W4390-HAYMAS = 'S' AND SW-CTACLI = 'NO'                06735014
                 PERFORM LLAMAR-SEGUNDA-RUTINA                          06736014
                    THRU LLAMAR-SEGUNDA-RUTINA-FIN                      06737014
              END-IF                                                    06738014
           END-PERFORM.                                                 06739014
      *CUENTA                                                           06740014
           IF NOT CTACLI                                                06741014
              IF NOT OPERA-BOLSA                                        06742014
                 MOVE -1           TO NCC0101L                          06743014
                 MOVE 'VLE1395'    TO CAA-COD-ERROR                     06744014
                 PERFORM 3-FINAL                                        06745014
              ELSE                                                      06746014
                 IF JURIDICA                                            06747014
                    IF NCC0101I = W-CTA-CAR-JUR                         06748014
                       CONTINUE                                         06749014
                    ELSE                                                06750014
                       MOVE NCC0101-COMM TO NCC0101I                    06751014
                       MOVE -1           TO NCC0101L                    06752014
                       MOVE 'VLE2061'    TO CAA-COD-ERROR               06753014
                       PERFORM 3-FINAL                                  06754014
                    END-IF                                              06755014
                 ELSE                                                   06756014
                    MOVE -1           TO NCC0101L                       06757014
                    MOVE 'VLE1395'    TO CAA-COD-ERROR                  06758014
                    PERFORM 3-FINAL                                     06759014
                 END-IF                                                 06760014
              END-IF                                                    06761014
           END-IF                                                       06762014
      *                                                                 06763014
           INITIALIZE                          W-BGECMDC                06764014
           MOVE NCC0101I(1:4)                 TO MDC-ENTIDAD            06765014
           MOVE NCC0101I(5:4)                 TO MDC-CENTRO-ALTA.       06766014
           MOVE NCC0101I(11:2)                TO MDC-CUENTA(1:2).       06767014
           MOVE NCC0101I(13:8)                TO MDC-CUENTA(3:8).       06768014
      *                                                                 06769014
           EXEC CICS                                                    06770014
             LINK PROGRAM (BG2CMDC0)                                    06771014
             COMMAREA (BGECMDC)                                         06772014
           END-EXEC                                                     06773014
      *                                                                 06774014
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         06775014
              MOVE 'ERROR EN BGECMDC0'  TO ABC-REFERENCIA               06776014
              MOVE 'BG2CMDC0'           TO ABC-OBJETO-ERROR             06777014
              PERFORM 999-ABEND-CICS                                    06778014
           END-IF                                                       06779014
      *                                                                 06780014
           EVALUATE MDC-CODERR                                          06781014
             WHEN SPACES                                                06782014
JIPC  *           IF MDC-INDESTA = 'A' OR 'R' OR 'P'                    06783014
                  IF MDC-INDESTA = 'A'                                  06784014
                     MOVE MDC-CDDIVIS  TO W-MONEDA-OK                   06785014
                     MOVE MDC-CDDIVIS  TO MON0101O                      06786014
                     MOVE MDC-CENTRO-CONTAB  TO OFI-PRO                 06787014
JPC@4                IF ENT0101I = '0069' OR '2010'                     06788014
JPC@4                   MOVE CAA-CENTRO-CONT TO OFI-PRO                 06789014
JPC@4                END-IF                                             06790014
                  ELSE                                                  06791014
                     MOVE MDC-CDDIVIS  TO MON0101O                      06792014
                     MOVE -1           TO NCC0101L                      06793014
                     MOVE 'VLE1101'    TO CAA-COD-ERROR                 06794014
                     PERFORM 3-FINAL                                    06795014
                  END-IF                                                06796014
      *200608070-INI                                                    06797014
      *200711038-INI                                                    06798014
      *           IF MDC-PRODUCTO  = '49'                               06799014
                  IF MDC-PRODUCTO  = '91'                               06800014
      *200711038-FIN                                                    06801014
                     IF (MDC-SUBPRODUC = '0020' OR '0021' OR            06802014
                                         '0026' OR '0027')              06803014
                        CONTINUE                                        06804014
                     ELSE                                               06805014
                        MOVE -1           TO NCC0101L                   06806014
                        MOVE 'VLE2061'    TO CAA-COD-ERROR              06807014
                        PERFORM 3-FINAL                                 06808014
                     END-IF                                             06809014
                  END-IF                                                06810014
      *200608070-FIN                                                    06811014
             WHEN OTHER                                                 06812014
                  MOVE -1           TO NCC0101L                         06813014
                  MOVE 'VLE0907'    TO CAA-COD-ERROR                    06814014
                  MOVE 'BG2CMDC0'   TO CAA-VAR1-ERROR                   06815014
                  MOVE MDC-CODERR   TO CAA-VAR2-ERROR                   06816014
                  PERFORM 3-FINAL                                       06817014
           END-EVALUATE.                                                06818014
                                                                        06819014
      *    PERFORM DESCRIPCION-OFICINA                                  06820014
      *       THRU DESCRIPCION-OFICINA-FIN.                             06821014
                                                                        06822014
      * DAVID                                                           06823014
       VALIDAR-CTA-CAR-FIN. EXIT.                                       06824014
      *----------------                                                 06825014
      *200310189-INI                                                    06826014
       VALIDAR-CTA-CONTROL.                                             06827014
      *-------------------                                              06828014
      *                                                                 06829014
      *      SE VALIDARA QUE EXISTA LA CUENTA DE CONTROL Y QUE SEA      06830014
      *      DESDE LA OFICINA 0542 (BOLSA) LA CUENTA DE CARGO = ABONO   06831014
      *                                                                 06832014
      *200605161-INI                                                    06833014
      *    IF ENT0101I NOT = '0069'                                     06834014
           IF ENT0101I NOT = '0069' AND '0011' AND '0312'               06835014
      *200605161-FIN                                                    06836014
              MOVE -1                     TO NCC0101L                   06837014
              MOVE 'VLE2166'              TO CAA-COD-ERROR              06838014
              MOVE '0069 0011 0312      ' TO CAA-VAR1-ERROR             06839014
              MOVE '                    ' TO CAA-VAR2-ERROR             06840014
              PERFORM 3-FINAL                                           06841014
           END-IF                                                       06842014
      *                                                                 06843014
      *200605161-INI                                                    06844014
      *    IF CAA-CENTRO-CONT NOT = '0542'                              06845014
      *       MOVE -1          TO NCC0101L                              06846014
      *       MOVE 'VLE2163'   TO CAA-COD-ERROR                         06847014
      *       PERFORM 3-FINAL                                           06848014
           IF CAA-CENTRO-CONT = '0542'                                  06849014
              IF ENT0101I = '0069'                                      06850014
                 CONTINUE                                               06851014
              ELSE                                                      06852014
                 IF ENT0101I = '0011' OR '0312'                         06853014
                    MOVE -1          TO NCC0101L                        06854014
                    MOVE 'VLE2192'   TO CAA-COD-ERROR                   06855014
                    PERFORM 3-FINAL                                     06856014
                 ELSE                                                   06857014
                    MOVE -1          TO NCC0101L                        06858014
                    MOVE 'VLE1452'   TO CAA-COD-ERROR                   06859014
                    PERFORM 3-FINAL                                     06860014
                 END-IF                                                 06861014
              END-IF                                                    06862014
           ELSE                                                         06863014
              IF CAA-CENTRO-CONT = '0567'                               06864014
                 IF ENT0101I = '0011' OR '0312'                         06865014
JPC@??                                OR '0069'                         06866014
                    CONTINUE                                            06867014
                 ELSE                                                   06868014
                    IF ENT0101I = '0069'                                06869014
                       MOVE -1          TO NCC0101L                     06870014
                       MOVE 'VLE2163'   TO CAA-COD-ERROR                06871014
                       PERFORM 3-FINAL                                  06872014
                    ELSE                                                06873014
                       MOVE -1          TO NCC0101L                     06874014
                       MOVE 'VLE1452'   TO CAA-COD-ERROR                06875014
                       PERFORM 3-FINAL                                  06876014
                    END-IF                                              06877014
                 END-IF                                                 06878014
              ELSE                                                      06879014
                 MOVE -1          TO NCC0101L                           06880014
                 MOVE 'VLE1452'   TO CAA-COD-ERROR                      06881014
                 PERFORM 3-FINAL                                        06882014
              END-IF                                                    06883014
           END-IF                                                       06884014
      *200605161-FIN                                                    06885014
      *                                                                 06886014
           IF TAF0101I = SPACES                                         06887014
              MOVE '51'        TO TAF0101I                              06888014
           END-IF                                                       06889014
      *                                                                 06890014
           IF TAF0101I NOT = '51'                                       06891014
              MOVE -1          TO NCC0101L                              06892014
              MOVE 'VLE2164'   TO CAA-COD-ERROR                         06893014
              PERFORM 3-FINAL                                           06894014
           END-IF                                                       06895014
      *                                                                 06896014
           IF NCC0101I IS NOT NUMERIC                                   06897014
              MOVE -1           TO NCC0101L                             06898014
              MOVE 'VLE1099'    TO CAA-COD-ERROR                        06899014
              PERFORM 3-FINAL                                           06900014
           END-IF                                                       06901014
      *                                                                 06902014
           INITIALIZE                            BRWCCDE0               06903014
           MOVE '0011'                        TO WCDE-CLV-ENTIDAD       06904014
           MOVE '0486'                        TO WCDE-CLV-CENTRO        06905014
           MOVE '9'                           TO WCDE-TIPOCTA           06906014
           MOVE 'VA'                          TO WCDE-CLASEC            06907014
           IF MDA0101I = SPACES                                         06908014
              MOVE 'PEN'                      TO WCDE-DIVISA            06909014
           ELSE                                                         06910014
              MOVE MDA0101I                   TO WCDE-DIVISA            06911014
           END-IF                                                       06912014
      *                                                                 06913014
           EXEC CICS                                                    06914014
                LINK PROGRAM (BR2CCDE0)                                 06915014
                    COMMAREA (BRWCCDE0)                                 06916014
           END-EXEC                                                     06917014
      *                                                                 06918014
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         06919014
              MOVE 'ERROR EN BR2CCDE0'  TO ABC-REFERENCIA               06920014
              MOVE 'BR2CCDE0'           TO ABC-OBJETO-ERROR             06921014
              PERFORM 999-ABEND-CICS                                    06922014
           END-IF                                                       06923014
      *                                                                 06924014
           EVALUATE WCDE-COD-ERROR                                      06925014
               WHEN SPACES                                              06926014
                    IF NCC0101I (01:04) = WCDE-ENTIDAD     AND          06927014
                       NCC0101I (05:04) = WCDE-CENTRO-ALTA AND          06928014
                       NCC0101I (11:10) = WCDE-CUENTA                   06929014
                       CONTINUE                                         06930014
                    ELSE                                                06931014
                       MOVE -1           TO NCC0101L                    06932014
                       MOVE 'VLE2162'    TO CAA-COD-ERROR               06933014
                       PERFORM 3-FINAL                                  06934014
                    END-IF                                              06935014
               WHEN OTHER                                               06936014
                    MOVE -1                     TO NCC0101L             06937014
                    MOVE 'VLE0907'              TO CAA-COD-ERROR        06938014
                    MOVE 'BR2CCDE0'             TO CAA-VAR1-ERROR       06939014
                    MOVE WCDE-COD-ERROR         TO CAA-VAR2-ERROR       06940014
                    PERFORM 3-FINAL                                     06941014
           END-EVALUATE.                                                06942014
      *                                                                 06943014
           INITIALIZE                            W-BGECMDC              06944014
           MOVE NCC0101I(01:4)                TO MDC-ENTIDAD            06945014
           MOVE NCC0101I(05:4)                TO MDC-CENTRO-ALTA.       06946014
           MOVE NCC0101I(11:2)                TO MDC-CUENTA(1:2).       06947014
           MOVE NCC0101I(13:8)                TO MDC-CUENTA(3:8).       06948014
      *                                                                 06949014
           EXEC CICS                                                    06950014
                LINK PROGRAM (BG2CMDC0)                                 06951014
                COMMAREA (BGECMDC)                                      06952014
           END-EXEC                                                     06953014
      *                                                                 06954014
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         06955014
              MOVE 'ERROR EN BGECMDC0'  TO ABC-REFERENCIA               06956014
              MOVE 'BG2CMDC0'           TO ABC-OBJETO-ERROR             06957014
              PERFORM 999-ABEND-CICS                                    06958014
           END-IF                                                       06959014
      *                                                                 06960014
           EVALUATE MDC-CODERR                                          06961014
               WHEN SPACES                                              06962014
                    IF MDC-INDESTA = 'A'                                06963014
                       MOVE MDC-CDDIVIS        TO W-MONEDA-OK           06964014
                       MOVE MDC-CDDIVIS        TO MON0101O, MO20101O    06965014
                       MOVE MDC-CENTRO-CONTAB  TO OFI-PRO               06966014
JPC@4                  IF ENT0101I = '0069' OR '2010'                   06967014
JPC@4                     MOVE CAA-CENTRO-CONT TO OFI-PRO               06968014
JPC@4                  END-IF                                           06969014
                    ELSE                                                06970014
                       MOVE MDC-CDDIVIS        TO MON0101O              06971014
                       MOVE -1                 TO NCC0101L              06972014
                       MOVE 'VLE1101'          TO CAA-COD-ERROR         06973014
                       PERFORM 3-FINAL                                  06974014
                   END-IF                                               06975014
              WHEN OTHER                                                06976014
                   MOVE -1                     TO NCC0101L              06977014
                   MOVE 'VLE0907'              TO CAA-COD-ERROR         06978014
                   MOVE 'BG2CMDC0'             TO CAA-VAR1-ERROR        06979014
                   MOVE MDC-CODERR             TO CAA-VAR2-ERROR        06980014
                   PERFORM 3-FINAL                                      06981014
           END-EVALUATE.                                                06982014
                                                                        06983014
           MOVE NCC0101I TO NC20101I.                                   06984014
                                                                        06985014
       VALIDAR-CTA-CONTROL-FIN. EXIT.                                   06986014
      *200310189-FIN.                                                   06987014
      *                                                                *06988014
       VALIDAR-CTA-ABO.                                                 06989014
      *----------------                                                 06990014
      *                                                                 06991014
      *  CTA. ABONO *****                                               06992014
      *                                                                 06993014
           IF NC20101I IS NOT NUMERIC                                   06994014
              MOVE -1           TO NC20101L                             06995014
              MOVE 'VLE1100'    TO CAA-COD-ERROR                        06996014
              PERFORM 3-FINAL                                           06997014
           END-IF                                                       06998014
      *                                                                 06999014
           MOVE 'NO' TO SW-CTACLI                                       07000014
           MOVE 'S'  TO SW-HAYMAS                                       07001014
      *                                                                 07002014
           PERFORM UNTIL SW-CTACLI = 'SI' OR SW-HAYMAS NOT EQUAL 'S'    07003014
              PERFORM VARYING  W-IND   FROM 1 BY 1                      07004014
                        UNTIL (W-IND > W4390-NOCCURS) OR                07005014
                              (W-IND > 100)                             07006014
                 MOVE W4390-PECENTID (W-IND)  TO  W-ENTIDAD-ANT         07007014
                 MOVE W4390-OFIAPE   (W-IND)  TO  W-OFICINA-ANT         07008014
                 MOVE W4390-CODISER  (W-IND)  TO  W-CODISER-ANT         07009014
                 MOVE W4390-NUMECTA  (W-IND)  TO  W-CUENTA-ANT          07010014
      *200608070-INI                                                    07011014
      *          IF  (W4390-CODISER  (W-IND) = '01' OR '02') AND        07012014
      *200711038-INI                                                    07013014
      *          IF  (W4390-CODISER  (W-IND) = '01' OR '02' OR '49') AND07014014
                 IF  (W4390-CODISER  (W-IND) = '01' OR '02' OR '91') AND07015014
      *200711038-FIN                                                    07016014
      *200608070-INI                                                    07017014
                     (W4390-CLAINTER (W-IND)   =  'T')                  07018014
                      MOVE W4390-PECENTID (W-IND)  TO  W-ENTIDAD-NUEVA  07019014
                      MOVE W4390-OFIAPE   (W-IND)  TO  W-OFICINA-NUEVA  07020014
                      MOVE '00'                    TO  W-DIGCON-NUEVA   07021014
                      MOVE W4390-CODISER  (W-IND)  TO  W-CODISER-NUEVA  07022014
                      MOVE W4390-NUMECTA  (W-IND)  TO  W-CUENTA-NUEVA   07023014
                      IF NC20101I = W-CUENTA-TOTAL                      07024014
                         MOVE 'SI' TO SW-CTACLI                         07025014
                         MOVE 101  TO W-IND                             07026014
                      END-IF                                            07027014
                 END-IF                                                 07028014
              END-PERFORM                                               07029014
              MOVE W4390-HAYMAS   TO SW-HAYMAS                          07030014
              IF W4390-HAYMAS = 'S' AND SW-CTACLI = 'NO'                07031014
                 PERFORM LLAMAR-SEGUNDA-RUTINA                          07032014
                    THRU LLAMAR-SEGUNDA-RUTINA-FIN                      07033014
              END-IF                                                    07034014
           END-PERFORM.                                                 07035014
      *CUENTA                                                           07036014
           IF NOT CTACLI                                                07037014
              IF NOT OPERA-BOLSA                                        07038014
                 MOVE -1           TO NC20101L                          07039014
                 MOVE 'VLE1395'    TO CAA-COD-ERROR                     07040014
                 PERFORM 3-FINAL                                        07041014
              ELSE                                                      07042014
                 IF JURIDICA                                            07043014
                    IF NC20101I = W-CTA-ABO-JUR                         07044014
                       CONTINUE                                         07045014
                    ELSE                                                07046014
                       MOVE NC20101-COMM TO NC20101I                    07047014
                       MOVE -1           TO NC20101L                    07048014
                       MOVE 'VLE2061'    TO CAA-COD-ERROR               07049014
                       PERFORM 3-FINAL                                  07050014
                    END-IF                                              07051014
                 ELSE                                                   07052014
                    MOVE -1           TO NC20101L                       07053014
                    MOVE 'VLE1395'    TO CAA-COD-ERROR                  07054014
                    PERFORM 3-FINAL                                     07055014
                 END-IF                                                 07056014
              END-IF                                                    07057014
           END-IF                                                       07058014
      *                                                                 07059014
           INITIALIZE                          W-BGECMDC                07060014
           MOVE NC20101I(1:4)                 TO MDC-ENTIDAD            07061014
           MOVE NC20101I(5:4)                 TO MDC-CENTRO-ALTA.       07062014
           MOVE NC20101I(11:2)                TO MDC-CUENTA(1:2).       07063014
           MOVE NC20101I(13:8)                TO MDC-CUENTA(3:8).       07064014
      *                                                                 07065014
           EXEC CICS                                                    07066014
             LINK PROGRAM (BG2CMDC0)                                    07067014
             COMMAREA (BGECMDC)                                         07068014
           END-EXEC                                                     07069014
      *                                                                 07070014
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         07071014
              MOVE 'ERROR EN BGECMDC0'  TO ABC-REFERENCIA               07072014
              MOVE 'BG2CMDC0'           TO ABC-OBJETO-ERROR             07073014
              PERFORM 999-ABEND-CICS                                    07074014
           END-IF                                                       07075014
      *                                                                 07076014
           EVALUATE MDC-CODERR                                          07077014
             WHEN SPACES                                                07078014
JIPC  *           IF MDC-INDESTA = 'A' OR 'R' OR 'P'                    07079014
                  IF MDC-INDESTA = 'A'                                  07080014
                     MOVE MDC-CDDIVIS     TO MO20101O                   07081014
      *              IF MDC-CDDIVIS NOT = W-MONEDA-OK                   07082014
      *                 MOVE -1           TO NC20101L                   07083014
      *                 MOVE 'VLE1101'    TO CAA-COD-ERROR              07084014
      *                 PERFORM 3-FINAL                                 07085014
      *              ELSE                                               07086014
      *                 CONTINUE                                        07087014
      *              END-IF                                             07088014
                  ELSE                                                  07089014
                     MOVE MDC-CDDIVIS     TO MO20101O                   07090014
                     MOVE -1              TO NC20101L                   07091014
                     MOVE 'VLE1101'       TO CAA-COD-ERROR              07092014
                     PERFORM 3-FINAL                                    07093014
                  END-IF                                                07094014
      *200608070-INI                                                    07095014
      *200711038-INI                                                    07096014
      *           IF MDC-PRODUCTO  = '49'                               07097014
                  IF MDC-PRODUCTO  = '91'                               07098014
      *200711038-FIN                                                    07099014
                     IF (MDC-SUBPRODUC = '0020' OR '0021' OR            07100014
                                         '0026' OR '0027')              07101014
                        CONTINUE                                        07102014
                     ELSE                                               07103014
                        MOVE -1           TO NC20101L                   07104014
                        MOVE 'VLE2061'    TO CAA-COD-ERROR              07105014
                        PERFORM 3-FINAL                                 07106014
                     END-IF                                             07107014
                  END-IF                                                07108014
      *200608070-FIN                                                    07109014
             WHEN OTHER                                                 07110014
                  MOVE -1           TO NC20101L                         07111014
      *           MOVE 'VLE0907'    TO CAA-COD-ERROR                    07112014
      *           MOVE 'BG2CMDC0'   TO CAA-VAR1-ERROR                   07113014
                  MOVE MDC-CODERR   TO CAA-COD-ERROR                    07114014
                  PERFORM 3-FINAL                                       07115014
           END-EVALUATE.                                                07116014
      *                                                                 07117014
       VALIDAR-CTA-ABO-FIN. EXIT.                                       07118014
      *----------------                                                 07119014
      *                                                                 07120014
      *----------------                                                 07121014
      *                                                                 07122014
       VALIDAR-CENTRO.                                                  07123014
      *                                                                 07124014
      *                                                                 07125014
           INITIALIZE                          W030-TCWC0300            07126014
      *                                                                 07127014
           MOVE  1                          TO W030-CDOPCIO             07128014
      *    MOVE END0101I                    TO W030-TCCENTITE           07129014
           MOVE CAA-ENTIDAD                 TO W030-TCCENTITE           07130014
      *                                                                 07131014
      *A2008-I. 12-8-99. SE ACCEDE SIEMPRE CON OFICINA PROPIETARIA      07132014
           MOVE SUC0101O                    TO W030-TCCOFICIE           07133014
      *    IF MSB-COMM = 'C' OR OPT-COMM = 'C'                          07134014
      *        MOVE VARC-SUCURS           TO W030-TCCOFICIE             07135014
      *    ELSE                                                         07136014
      *        MOVE CAA-CENTRO-CONT       TO W030-TCCOFICIE             07137014
      *    END-IF.                                                      07138014
      *A2008-F. 12-8-99. SE ACCEDE SIEMPRE CON OFICINA PROPIETARIA      07139014
      *                                                                 07140014
           EXEC CICS                                                    07141014
                LINK PROGRAM (TC2C1500)                                 07142014
                COMMAREA     (W-TCWC0300)                               07143014
           END-EXEC.                                                    07144014
      *                                                                 07145014
           IF EIBRESP NOT = DFHRESP(NORMAL)                             07146014
              MOVE 'ERROR EN TC2C1500'          TO   ABC-REFERENCIA     07147014
              MOVE 'TC2C1500'                   TO   ABC-OBJETO-ERROR   07148014
              PERFORM 999-ABEND-CICS                                    07149014
           END-IF                                                       07150014
      *                                                                 07151014
           EVALUATE W030-RETORN                                         07152014
               WHEN  '00'                                               07153014
                     MOVE  W030-TCMOFCUR        TO   NOF0101O           07154014
      *              CONTINUE                                           07155014
               WHEN  '10'                                               07156014
                     MOVE -1              TO SUC0101L                   07157014
                     MOVE  'VLE0388'      TO CAA-COD-ERROR              07158014
                     PERFORM 3-FINAL                                    07159014
               WHEN  '20'                                               07160014
               WHEN  '30'                                               07161014
                     MOVE -1              TO SUC0101L                   07162014
                     MOVE  'VLE0790'      TO CAA-COD-ERROR              07163014
                     PERFORM 3-FINAL                                    07164014
               WHEN  OTHER                                              07165014
                     MOVE -1              TO SUC0101L                   07166014
                     MOVE 'VLE0907'       TO CAA-COD-ERROR              07167014
                     MOVE 'TC2C1500'      TO CAA-VAR1-ERROR             07168014
                     MOVE W030-RETORN     TO CAA-VAR2-ERROR             07169014
                     PERFORM 3-FINAL                                    07170014
           END-EVALUATE.                                                07171014
      *                                                                 07172014
      *    MOVE W030-TCMOFICI    TO  NSU0101O.                          07173014
      *                                                                 07174014
       VALIDAR-CENTRO-FIN. EXIT.                                        07175014
      *                                                                 07176014
      *                                                                 07177014
       ACCESO-VLDTXTA.                                                  07178014
      *                                                                 07179014
           MOVE TAF0101I    TO  W-TARIFA                                07180014
           MOVE W-TARIFA    TO  VXTA-CODTARIF                           07181014
           MOVE  49         TO  VXTA-OPECON                             07182014
           MOVE 'F'         TO  VXTA-TIPTARIF                           07183014
           MOVE MDA0101I    TO  VXTA-MONEDA                             07184014
      *                                                                 07185014
           EXEC SQL                                                     07186014
JPC@1 *         SELECT  *                                               07187014
                SELECT VXTA_OPECON                                      07188014
                     , VXTA_TIPTARIF                                    07189014
                     , VXTA_CODTARIF                                    07190014
                     , VXTA_MONEDA                                      07191014
                     , VXTA_NOMTARIF                                    07192014
                     , VXTA_PERIODO                                     07193014
                     , VXTA_TIPCALCU                                    07194014
                     , VXTA_MINTAR                                      07195014
                     , VXTA_MAXTAR                                      07196014
                     , VXTA_DESDE1                                      07197014
                     , VXTA_HASTA1                                      07198014
                     , VXTA_PORMIL1                                     07199014
                     , VXTA_DESDE2                                      07200014
                     , VXTA_HASTA2                                      07201014
                     , VXTA_PORMIL2                                     07202014
                     , VXTA_DESDE3                                      07203014
                     , VXTA_HASTA3                                      07204014
                     , VXTA_PORMIL3                                     07205014
                     , VXTA_DESDE4                                      07206014
                     , VXTA_HASTA4                                      07207014
                     , VXTA_PORMIL4                                     07208014
                     , VXTA_DESDE5                                      07209014
                     , VXTA_HASTA5                                      07210014
                     , VXTA_PORMIL5                                     07211014
                     , VXTA_DESDE6                                      07212014
                     , VXTA_HASTA6                                      07213014
                     , VXTA_PORMIL6                                     07214014
                     , VXTA_DESDE7                                      07215014
                     , VXTA_HASTA7                                      07216014
                     , VXTA_PORMIL7                                     07217014
                     , VXTA_DESDE8                                      07218014
                     , VXTA_HASTA8                                      07219014
                     , VXTA_PORMIL8                                     07220014
                     , VXTA_DESDE9                                      07221014
                     , VXTA_HASTA9                                      07222014
                     , VXTA_PORMIL9                                     07223014
                     , VXTA_DESDE10                                     07224014
                     , VXTA_HASTA10                                     07225014
                     , VXTA_PORMIL10                                    07226014
                     , VXTA_DESDE11                                     07227014
                     , VXTA_HASTA11                                     07228014
                     , VXTA_PORMIL11                                    07229014
                     , VXTA_DESDE12                                     07230014
                     , VXTA_HASTA12                                     07231014
                     , VXTA_PORMIL12                                    07232014
                     , VXTA_DESDE13                                     07233014
                     , VXTA_HASTA13                                     07234014
                     , VXTA_PORMIL13                                    07235014
                     , VXTA_DESDE14                                     07236014
                     , VXTA_HASTA14                                     07237014
                     , VXTA_PORMIL14                                    07238014
                     , VXTA_DESDE15                                     07239014
                     , VXTA_HASTA15                                     07240014
                     , VXTA_PORMIL15                                    07241014
                     , VXTA_DESDE16                                     07242014
                     , VXTA_HASTA16                                     07243014
                     , VXTA_PORMIL16                                    07244014
                     , VXTA_DESDE17                                     07245014
                     , VXTA_HASTA17                                     07246014
                     , VXTA_PORMIL17                                    07247014
                     , VXTA_DESDE18                                     07248014
                     , VXTA_HASTA18                                     07249014
                     , VXTA_PORMIL18                                    07250014
                     , VXTA_DESDE19                                     07251014
                     , VXTA_HASTA19                                     07252014
                     , VXTA_PORMIL19                                    07253014
                     , VXTA_DESDE20                                     07254014
                     , VXTA_HASTA20                                     07255014
                     , VXTA_PORMIL20                                    07256014
                     , VXTA_INDVALO                                     07257014
                     , VXTA_FEALTREG                                    07258014
                     , VXTA_FEULMOD                                     07259014
                     , VXTA_HORULMOD                                    07260014
                     , VXTA_NUMTER                                      07261014
                     , VXTA_USUARIO                                     07262014
                     , VXTA_FILLER                                      07263014
JPC@1 *           INTO :DCLVLDTXTA                                      07264014
                  INTO :VXTA-OPECON                                     07265014
                     , :VXTA-TIPTARIF                                   07266014
                     , :VXTA-CODTARIF                                   07267014
                     , :VXTA-MONEDA                                     07268014
                     , :VXTA-NOMTARIF                                   07269014
                     , :VXTA-PERIODO                                    07270014
                     , :VXTA-TIPCALCU                                   07271014
                     , :VXTA-MINTAR                                     07272014
                     , :VXTA-MAXTAR                                     07273014
                     , :VXTA-DESDE1                                     07274014
                     , :VXTA-HASTA1                                     07275014
                     , :VXTA-PORMIL1                                    07276014
                     , :VXTA-DESDE2                                     07277014
                     , :VXTA-HASTA2                                     07278014
                     , :VXTA-PORMIL2                                    07279014
                     , :VXTA-DESDE3                                     07280014
                     , :VXTA-HASTA3                                     07281014
                     , :VXTA-PORMIL3                                    07282014
                     , :VXTA-DESDE4                                     07283014
                     , :VXTA-HASTA4                                     07284014
                     , :VXTA-PORMIL4                                    07285014
                     , :VXTA-DESDE5                                     07286014
                     , :VXTA-HASTA5                                     07287014
                     , :VXTA-PORMIL5                                    07288014
                     , :VXTA-DESDE6                                     07289014
                     , :VXTA-HASTA6                                     07290014
                     , :VXTA-PORMIL6                                    07291014
                     , :VXTA-DESDE7                                     07292014
                     , :VXTA-HASTA7                                     07293014
                     , :VXTA-PORMIL7                                    07294014
                     , :VXTA-DESDE8                                     07295014
                     , :VXTA-HASTA8                                     07296014
                     , :VXTA-PORMIL8                                    07297014
                     , :VXTA-DESDE9                                     07298014
                     , :VXTA-HASTA9                                     07299014
                     , :VXTA-PORMIL9                                    07300014
                     , :VXTA-DESDE10                                    07301014
                     , :VXTA-HASTA10                                    07302014
                     , :VXTA-PORMIL10                                   07303014
                     , :VXTA-DESDE11                                    07304014
                     , :VXTA-HASTA11                                    07305014
                     , :VXTA-PORMIL11                                   07306014
                     , :VXTA-DESDE12                                    07307014
                     , :VXTA-HASTA12                                    07308014
                     , :VXTA-PORMIL12                                   07309014
                     , :VXTA-DESDE13                                    07310014
                     , :VXTA-HASTA13                                    07311014
                     , :VXTA-PORMIL13                                   07312014
                     , :VXTA-DESDE14                                    07313014
                     , :VXTA-HASTA14                                    07314014
                     , :VXTA-PORMIL14                                   07315014
                     , :VXTA-DESDE15                                    07316014
                     , :VXTA-HASTA15                                    07317014
                     , :VXTA-PORMIL15                                   07318014
                     , :VXTA-DESDE16                                    07319014
                     , :VXTA-HASTA16                                    07320014
                     , :VXTA-PORMIL16                                   07321014
                     , :VXTA-DESDE17                                    07322014
                     , :VXTA-HASTA17                                    07323014
                     , :VXTA-PORMIL17                                   07324014
                     , :VXTA-DESDE18                                    07325014
                     , :VXTA-HASTA18                                    07326014
                     , :VXTA-PORMIL18                                   07327014
                     , :VXTA-DESDE19                                    07328014
                     , :VXTA-HASTA19                                    07329014
                     , :VXTA-PORMIL19                                   07330014
                     , :VXTA-DESDE20                                    07331014
                     , :VXTA-HASTA20                                    07332014
                     , :VXTA-PORMIL20                                   07333014
                     , :VXTA-INDVALO                                    07334014
                     , :VXTA-FEALTREG                                   07335014
                     , :VXTA-FEULMOD                                    07336014
                     , :VXTA-HORULMOD                                   07337014
                     , :VXTA-NUMTER                                     07338014
                     , :VXTA-USUARIO                                    07339014
                     , :VXTA-FILLER                                     07340014
                  FROM  VLDTXTA                                         07341014
                 WHERE  VXTA_CODTARIF = :VXTA-CODTARIF                  07342014
                   AND  VXTA_OPECON   = :VXTA-OPECON                    07343014
                   AND  VXTA_TIPTARIF = :VXTA-TIPTARIF                  07344014
                   AND  VXTA_MONEDA   = :VXTA-MONEDA                    07345014
           END-EXEC                                                     07346014
      *                                                                 07347014
           IF SQLCODE NOT = 0 AND 100                                   07348014
              MOVE 'SELECT'       TO   ABC-REFERENCIA                   07349014
              MOVE 'VLDTXTA'      TO   ABC-OBJETO-ERROR                 07350014
              PERFORM 999-ABEND-DB2                                     07351014
           END-IF                                                       07352014
      *                                                                 07353014
           IF SQLCODE = 100                                             07354014
              MOVE 'VLE1769'   TO CAA-COD-ERROR                         07355014
              MOVE -1          TO TAF0101L                              07356014
              PERFORM 3-FINAL                                           07357014
           END-IF.                                                      07358014
                                                                        07359014
      *A2011-RUTLOG-I                                                   07360014
           INITIALIZE W-VLWCLOG0                                        07361014
                      LOGVLDTXTA                                        07362014
           MOVE 'VLDTXTA'             TO  VL7LOG-TABLA                  07363014
           MOVE 'SELECT'              TO  VL7LOG-OPERACION              07364014
           MOVE LENGTH OF DCLVLDTXTA  TO  VL7LOG-REGISTRO-LEN           07365014
           MOVE DCLVLDTXTA            TO  LOGVLDTXTA                    07366014
           MOVE LOGVLDTXTA            TO  VL7LOG-REGISTRO-TEXT          07367014
           PERFORM LLAMAR-VL7CRLOG                                      07368014
              THRU LLAMAR-VL7CRLOG-FIN                                  07369014
      *A2011-RUTLOG-F                                                   07370014
           MOVE TAF0101I    TO  W-TARIFA                                07371014
           MOVE W-TARIFA    TO  VXTA-CODTARIF                           07372014
           MOVE  49         TO  VXTA-OPECON                             07373014
           MOVE 'D'         TO  VXTA-TIPTARIF                           07374014
           MOVE MDA0101I    TO  VXTA-MONEDA                             07375014
      *                                                                 07376014
           EXEC SQL                                                     07377014
JPC@1 *         SELECT  *                                               07378014
                SELECT VXTA_OPECON                                      07379014
                     , VXTA_TIPTARIF                                    07380014
                     , VXTA_CODTARIF                                    07381014
                     , VXTA_MONEDA                                      07382014
                     , VXTA_NOMTARIF                                    07383014
                     , VXTA_PERIODO                                     07384014
                     , VXTA_TIPCALCU                                    07385014
                     , VXTA_MINTAR                                      07386014
                     , VXTA_MAXTAR                                      07387014
                     , VXTA_DESDE1                                      07388014
                     , VXTA_HASTA1                                      07389014
                     , VXTA_PORMIL1                                     07390014
                     , VXTA_DESDE2                                      07391014
                     , VXTA_HASTA2                                      07392014
                     , VXTA_PORMIL2                                     07393014
                     , VXTA_DESDE3                                      07394014
                     , VXTA_HASTA3                                      07395014
                     , VXTA_PORMIL3                                     07396014
                     , VXTA_DESDE4                                      07397014
                     , VXTA_HASTA4                                      07398014
                     , VXTA_PORMIL4                                     07399014
                     , VXTA_DESDE5                                      07400014
                     , VXTA_HASTA5                                      07401014
                     , VXTA_PORMIL5                                     07402014
                     , VXTA_DESDE6                                      07403014
                     , VXTA_HASTA6                                      07404014
                     , VXTA_PORMIL6                                     07405014
                     , VXTA_DESDE7                                      07406014
                     , VXTA_HASTA7                                      07407014
                     , VXTA_PORMIL7                                     07408014
                     , VXTA_DESDE8                                      07409014
                     , VXTA_HASTA8                                      07410014
                     , VXTA_PORMIL8                                     07411014
                     , VXTA_DESDE9                                      07412014
                     , VXTA_HASTA9                                      07413014
                     , VXTA_PORMIL9                                     07414014
                     , VXTA_DESDE10                                     07415014
                     , VXTA_HASTA10                                     07416014
                     , VXTA_PORMIL10                                    07417014
                     , VXTA_DESDE11                                     07418014
                     , VXTA_HASTA11                                     07419014
                     , VXTA_PORMIL11                                    07420014
                     , VXTA_DESDE12                                     07421014
                     , VXTA_HASTA12                                     07422014
                     , VXTA_PORMIL12                                    07423014
                     , VXTA_DESDE13                                     07424014
                     , VXTA_HASTA13                                     07425014
                     , VXTA_PORMIL13                                    07426014
                     , VXTA_DESDE14                                     07427014
                     , VXTA_HASTA14                                     07428014
                     , VXTA_PORMIL14                                    07429014
                     , VXTA_DESDE15                                     07430014
                     , VXTA_HASTA15                                     07431014
                     , VXTA_PORMIL15                                    07432014
                     , VXTA_DESDE16                                     07433014
                     , VXTA_HASTA16                                     07434014
                     , VXTA_PORMIL16                                    07435014
                     , VXTA_DESDE17                                     07436014
                     , VXTA_HASTA17                                     07437014
                     , VXTA_PORMIL17                                    07438014
                     , VXTA_DESDE18                                     07439014
                     , VXTA_HASTA18                                     07440014
                     , VXTA_PORMIL18                                    07441014
                     , VXTA_DESDE19                                     07442014
                     , VXTA_HASTA19                                     07443014
                     , VXTA_PORMIL19                                    07444014
                     , VXTA_DESDE20                                     07445014
                     , VXTA_HASTA20                                     07446014
                     , VXTA_PORMIL20                                    07447014
                     , VXTA_INDVALO                                     07448014
                     , VXTA_FEALTREG                                    07449014
                     , VXTA_FEULMOD                                     07450014
                     , VXTA_HORULMOD                                    07451014
                     , VXTA_NUMTER                                      07452014
                     , VXTA_USUARIO                                     07453014
                     , VXTA_FILLER                                      07454014
JPC@1 *           INTO :DCLVLDTXTA                                      07455014
                  INTO :VXTA-OPECON                                     07456014
                     , :VXTA-TIPTARIF                                   07457014
                     , :VXTA-CODTARIF                                   07458014
                     , :VXTA-MONEDA                                     07459014
                     , :VXTA-NOMTARIF                                   07460014
                     , :VXTA-PERIODO                                    07461014
                     , :VXTA-TIPCALCU                                   07462014
                     , :VXTA-MINTAR                                     07463014
                     , :VXTA-MAXTAR                                     07464014
                     , :VXTA-DESDE1                                     07465014
                     , :VXTA-HASTA1                                     07466014
                     , :VXTA-PORMIL1                                    07467014
                     , :VXTA-DESDE2                                     07468014
                     , :VXTA-HASTA2                                     07469014
                     , :VXTA-PORMIL2                                    07470014
                     , :VXTA-DESDE3                                     07471014
                     , :VXTA-HASTA3                                     07472014
                     , :VXTA-PORMIL3                                    07473014
                     , :VXTA-DESDE4                                     07474014
                     , :VXTA-HASTA4                                     07475014
                     , :VXTA-PORMIL4                                    07476014
                     , :VXTA-DESDE5                                     07477014
                     , :VXTA-HASTA5                                     07478014
                     , :VXTA-PORMIL5                                    07479014
                     , :VXTA-DESDE6                                     07480014
                     , :VXTA-HASTA6                                     07481014
                     , :VXTA-PORMIL6                                    07482014
                     , :VXTA-DESDE7                                     07483014
                     , :VXTA-HASTA7                                     07484014
                     , :VXTA-PORMIL7                                    07485014
                     , :VXTA-DESDE8                                     07486014
                     , :VXTA-HASTA8                                     07487014
                     , :VXTA-PORMIL8                                    07488014
                     , :VXTA-DESDE9                                     07489014
                     , :VXTA-HASTA9                                     07490014
                     , :VXTA-PORMIL9                                    07491014
                     , :VXTA-DESDE10                                    07492014
                     , :VXTA-HASTA10                                    07493014
                     , :VXTA-PORMIL10                                   07494014
                     , :VXTA-DESDE11                                    07495014
                     , :VXTA-HASTA11                                    07496014
                     , :VXTA-PORMIL11                                   07497014
                     , :VXTA-DESDE12                                    07498014
                     , :VXTA-HASTA12                                    07499014
                     , :VXTA-PORMIL12                                   07500014
                     , :VXTA-DESDE13                                    07501014
                     , :VXTA-HASTA13                                    07502014
                     , :VXTA-PORMIL13                                   07503014
                     , :VXTA-DESDE14                                    07504014
                     , :VXTA-HASTA14                                    07505014
                     , :VXTA-PORMIL14                                   07506014
                     , :VXTA-DESDE15                                    07507014
                     , :VXTA-HASTA15                                    07508014
                     , :VXTA-PORMIL15                                   07509014
                     , :VXTA-DESDE16                                    07510014
                     , :VXTA-HASTA16                                    07511014
                     , :VXTA-PORMIL16                                   07512014
                     , :VXTA-DESDE17                                    07513014
                     , :VXTA-HASTA17                                    07514014
                     , :VXTA-PORMIL17                                   07515014
                     , :VXTA-DESDE18                                    07516014
                     , :VXTA-HASTA18                                    07517014
                     , :VXTA-PORMIL18                                   07518014
                     , :VXTA-DESDE19                                    07519014
                     , :VXTA-HASTA19                                    07520014
                     , :VXTA-PORMIL19                                   07521014
                     , :VXTA-DESDE20                                    07522014
                     , :VXTA-HASTA20                                    07523014
                     , :VXTA-PORMIL20                                   07524014
                     , :VXTA-INDVALO                                    07525014
                     , :VXTA-FEALTREG                                   07526014
                     , :VXTA-FEULMOD                                    07527014
                     , :VXTA-HORULMOD                                   07528014
                     , :VXTA-NUMTER                                     07529014
                     , :VXTA-USUARIO                                    07530014
                     , :VXTA-FILLER                                     07531014
                  FROM  VLDTXTA                                         07532014
                 WHERE  VXTA_CODTARIF = :VXTA-CODTARIF                  07533014
                   AND  VXTA_OPECON   = :VXTA-OPECON                    07534014
                   AND  VXTA_TIPTARIF = :VXTA-TIPTARIF                  07535014
                   AND  VXTA_MONEDA   = :VXTA-MONEDA                    07536014
           END-EXEC                                                     07537014
      *                                                                 07538014
           IF SQLCODE NOT = 0 AND 100                                   07539014
              MOVE 'SELECT'       TO   ABC-REFERENCIA                   07540014
              MOVE 'VLDTXTA'      TO   ABC-OBJETO-ERROR                 07541014
              PERFORM 999-ABEND-DB2                                     07542014
           END-IF                                                       07543014
      *                                                                 07544014
           IF SQLCODE = 100                                             07545014
              MOVE 'VLE1770'   TO CAA-COD-ERROR                         07546014
              MOVE -1          TO TAF0101L                              07547014
              PERFORM 3-FINAL                                           07548014
           END-IF.                                                      07549014
      *                                                                 07550014
      *A2011-RUTLOG-I                                                   07551014
           INITIALIZE W-VLWCLOG0                                        07552014
                      LOGVLDTXTA                                        07553014
           MOVE 'VLDTXTA'             TO  VL7LOG-TABLA                  07554014
           MOVE 'SELECT'              TO  VL7LOG-OPERACION              07555014
           MOVE LENGTH OF DCLVLDTXTA  TO  VL7LOG-REGISTRO-LEN           07556014
           MOVE DCLVLDTXTA            TO  LOGVLDTXTA                    07557014
           MOVE LOGVLDTXTA            TO  VL7LOG-REGISTRO-TEXT          07558014
           PERFORM LLAMAR-VL7CRLOG                                      07559014
              THRU LLAMAR-VL7CRLOG-FIN                                  07560014
      *A2011-RUTLOG-F                                                   07561014
                                                                        07562014
           MOVE TAF0101I    TO  W-TARIFA                                07563014
           MOVE W-TARIFA    TO  VXTA-CODTARIF                           07564014
           MOVE  49         TO  VXTA-OPECON                             07565014
           MOVE 'I'         TO  VXTA-TIPTARIF                           07566014
           MOVE MDA0101I    TO  VXTA-MONEDA                             07567014
      *                                                                 07568014
           EXEC SQL                                                     07569014
JPC@1 *         SELECT  *                                               07570014
                SELECT VXTA_OPECON                                      07571014
                     , VXTA_TIPTARIF                                    07572014
                     , VXTA_CODTARIF                                    07573014
                     , VXTA_MONEDA                                      07574014
                     , VXTA_NOMTARIF                                    07575014
                     , VXTA_PERIODO                                     07576014
                     , VXTA_TIPCALCU                                    07577014
                     , VXTA_MINTAR                                      07578014
                     , VXTA_MAXTAR                                      07579014
                     , VXTA_DESDE1                                      07580014
                     , VXTA_HASTA1                                      07581014
                     , VXTA_PORMIL1                                     07582014
                     , VXTA_DESDE2                                      07583014
                     , VXTA_HASTA2                                      07584014
                     , VXTA_PORMIL2                                     07585014
                     , VXTA_DESDE3                                      07586014
                     , VXTA_HASTA3                                      07587014
                     , VXTA_PORMIL3                                     07588014
                     , VXTA_DESDE4                                      07589014
                     , VXTA_HASTA4                                      07590014
                     , VXTA_PORMIL4                                     07591014
                     , VXTA_DESDE5                                      07592014
                     , VXTA_HASTA5                                      07593014
                     , VXTA_PORMIL5                                     07594014
                     , VXTA_DESDE6                                      07595014
                     , VXTA_HASTA6                                      07596014
                     , VXTA_PORMIL6                                     07597014
                     , VXTA_DESDE7                                      07598014
                     , VXTA_HASTA7                                      07599014
                     , VXTA_PORMIL7                                     07600014
                     , VXTA_DESDE8                                      07601014
                     , VXTA_HASTA8                                      07602014
                     , VXTA_PORMIL8                                     07603014
                     , VXTA_DESDE9                                      07604014
                     , VXTA_HASTA9                                      07605014
                     , VXTA_PORMIL9                                     07606014
                     , VXTA_DESDE10                                     07607014
                     , VXTA_HASTA10                                     07608014
                     , VXTA_PORMIL10                                    07609014
                     , VXTA_DESDE11                                     07610014
                     , VXTA_HASTA11                                     07611014
                     , VXTA_PORMIL11                                    07612014
                     , VXTA_DESDE12                                     07613014
                     , VXTA_HASTA12                                     07614014
                     , VXTA_PORMIL12                                    07615014
                     , VXTA_DESDE13                                     07616014
                     , VXTA_HASTA13                                     07617014
                     , VXTA_PORMIL13                                    07618014
                     , VXTA_DESDE14                                     07619014
                     , VXTA_HASTA14                                     07620014
                     , VXTA_PORMIL14                                    07621014
                     , VXTA_DESDE15                                     07622014
                     , VXTA_HASTA15                                     07623014
                     , VXTA_PORMIL15                                    07624014
                     , VXTA_DESDE16                                     07625014
                     , VXTA_HASTA16                                     07626014
                     , VXTA_PORMIL16                                    07627014
                     , VXTA_DESDE17                                     07628014
                     , VXTA_HASTA17                                     07629014
                     , VXTA_PORMIL17                                    07630014
                     , VXTA_DESDE18                                     07631014
                     , VXTA_HASTA18                                     07632014
                     , VXTA_PORMIL18                                    07633014
                     , VXTA_DESDE19                                     07634014
                     , VXTA_HASTA19                                     07635014
                     , VXTA_PORMIL19                                    07636014
                     , VXTA_DESDE20                                     07637014
                     , VXTA_HASTA20                                     07638014
                     , VXTA_PORMIL20                                    07639014
                     , VXTA_INDVALO                                     07640014
                     , VXTA_FEALTREG                                    07641014
                     , VXTA_FEULMOD                                     07642014
                     , VXTA_HORULMOD                                    07643014
                     , VXTA_NUMTER                                      07644014
                     , VXTA_USUARIO                                     07645014
                     , VXTA_FILLER                                      07646014
JPC@1 *           INTO :DCLVLDTXTA                                      07647014
                  INTO :VXTA-OPECON                                     07648014
                     , :VXTA-TIPTARIF                                   07649014
                     , :VXTA-CODTARIF                                   07650014
                     , :VXTA-MONEDA                                     07651014
                     , :VXTA-NOMTARIF                                   07652014
                     , :VXTA-PERIODO                                    07653014
                     , :VXTA-TIPCALCU                                   07654014
                     , :VXTA-MINTAR                                     07655014
                     , :VXTA-MAXTAR                                     07656014
                     , :VXTA-DESDE1                                     07657014
                     , :VXTA-HASTA1                                     07658014
                     , :VXTA-PORMIL1                                    07659014
                     , :VXTA-DESDE2                                     07660014
                     , :VXTA-HASTA2                                     07661014
                     , :VXTA-PORMIL2                                    07662014
                     , :VXTA-DESDE3                                     07663014
                     , :VXTA-HASTA3                                     07664014
                     , :VXTA-PORMIL3                                    07665014
                     , :VXTA-DESDE4                                     07666014
                     , :VXTA-HASTA4                                     07667014
                     , :VXTA-PORMIL4                                    07668014
                     , :VXTA-DESDE5                                     07669014
                     , :VXTA-HASTA5                                     07670014
                     , :VXTA-PORMIL5                                    07671014
                     , :VXTA-DESDE6                                     07672014
                     , :VXTA-HASTA6                                     07673014
                     , :VXTA-PORMIL6                                    07674014
                     , :VXTA-DESDE7                                     07675014
                     , :VXTA-HASTA7                                     07676014
                     , :VXTA-PORMIL7                                    07677014
                     , :VXTA-DESDE8                                     07678014
                     , :VXTA-HASTA8                                     07679014
                     , :VXTA-PORMIL8                                    07680014
                     , :VXTA-DESDE9                                     07681014
                     , :VXTA-HASTA9                                     07682014
                     , :VXTA-PORMIL9                                    07683014
                     , :VXTA-DESDE10                                    07684014
                     , :VXTA-HASTA10                                    07685014
                     , :VXTA-PORMIL10                                   07686014
                     , :VXTA-DESDE11                                    07687014
                     , :VXTA-HASTA11                                    07688014
                     , :VXTA-PORMIL11                                   07689014
                     , :VXTA-DESDE12                                    07690014
                     , :VXTA-HASTA12                                    07691014
                     , :VXTA-PORMIL12                                   07692014
                     , :VXTA-DESDE13                                    07693014
                     , :VXTA-HASTA13                                    07694014
                     , :VXTA-PORMIL13                                   07695014
                     , :VXTA-DESDE14                                    07696014
                     , :VXTA-HASTA14                                    07697014
                     , :VXTA-PORMIL14                                   07698014
                     , :VXTA-DESDE15                                    07699014
                     , :VXTA-HASTA15                                    07700014
                     , :VXTA-PORMIL15                                   07701014
                     , :VXTA-DESDE16                                    07702014
                     , :VXTA-HASTA16                                    07703014
                     , :VXTA-PORMIL16                                   07704014
                     , :VXTA-DESDE17                                    07705014
                     , :VXTA-HASTA17                                    07706014
                     , :VXTA-PORMIL17                                   07707014
                     , :VXTA-DESDE18                                    07708014
                     , :VXTA-HASTA18                                    07709014
                     , :VXTA-PORMIL18                                   07710014
                     , :VXTA-DESDE19                                    07711014
                     , :VXTA-HASTA19                                    07712014
                     , :VXTA-PORMIL19                                   07713014
                     , :VXTA-DESDE20                                    07714014
                     , :VXTA-HASTA20                                    07715014
                     , :VXTA-PORMIL20                                   07716014
                     , :VXTA-INDVALO                                    07717014
                     , :VXTA-FEALTREG                                   07718014
                     , :VXTA-FEULMOD                                    07719014
                     , :VXTA-HORULMOD                                   07720014
                     , :VXTA-NUMTER                                     07721014
                     , :VXTA-USUARIO                                    07722014
                     , :VXTA-FILLER                                     07723014
                  FROM  VLDTXTA                                         07724014
                 WHERE  VXTA_CODTARIF = :VXTA-CODTARIF                  07725014
                   AND  VXTA_OPECON   = :VXTA-OPECON                    07726014
                   AND  VXTA_TIPTARIF = :VXTA-TIPTARIF                  07727014
                   AND  VXTA_MONEDA   = :VXTA-MONEDA                    07728014
           END-EXEC                                                     07729014
      *                                                                 07730014
           IF SQLCODE NOT = 0 AND 100                                   07731014
              MOVE 'SELECT'       TO   ABC-REFERENCIA                   07732014
              MOVE 'VLDTXTA'      TO   ABC-OBJETO-ERROR                 07733014
              PERFORM 999-ABEND-DB2                                     07734014
           END-IF                                                       07735014
      *                                                                 07736014
           IF SQLCODE = 100                                             07737014
              MOVE 'VLE1771'   TO CAA-COD-ERROR                         07738014
              MOVE -1          TO TAF0101L                              07739014
              PERFORM 3-FINAL                                           07740014
           END-IF.                                                      07741014
      *                                                                 07742014
      *A2011-RUTLOG-I                                                   07743014
           INITIALIZE W-VLWCLOG0                                        07744014
                      LOGVLDTXTA                                        07745014
           MOVE 'VLDTXTA'             TO  VL7LOG-TABLA                  07746014
           MOVE 'SELECT'              TO  VL7LOG-OPERACION              07747014
           MOVE LENGTH OF DCLVLDTXTA  TO  VL7LOG-REGISTRO-LEN           07748014
           MOVE DCLVLDTXTA            TO  LOGVLDTXTA                    07749014
           MOVE LOGVLDTXTA            TO  VL7LOG-REGISTRO-TEXT          07750014
           PERFORM LLAMAR-VL7CRLOG                                      07751014
              THRU LLAMAR-VL7CRLOG-FIN.                                 07752014
      *A2011-RUTLOG-F                                                   07753014
                                                                        07754014
       ACCESO-VLDTXTA-FIN. EXIT.                                        07755014
      *                                                                 07756014
      *                                                                 07757014
       INSERTAR-VCTASMES.                                               07758014
      *                                                                 07759014
           MOVE VARC-CUENTA       TO  VMES-CUENTA.                      07760014
           MOVE VARC-FEALTREG     TO  VMES-FALTA.                       07761014
           MOVE 0                 TO  VMES-REACTIVA.                    07762014
      *                                                                 07763014
           MOVE CAA-FECHA-OPER    TO  VMES-FEULMOD                      07764014
                                      VMES-FEALTREG                     07765014
           MOVE CAA-HORA-TRANS    TO  VMES-HORULMOD                     07766014
           MOVE CAA-TERMINAL      TO  VMES-NUMTER                       07767014
           MOVE CAA-USERID        TO  VMES-USUARIO                      07768014
           MOVE SPACES            TO  VMES-FILLER.                      07769014
      *                                                                 07770014
           EXEC SQL                                                     07771014
                INSERT INTO VLDTMES                                     07772014
                VALUES (:DCLVLDTMES)                                    07773014
           END-EXEC                                                     07774014
      *                                                                 07775014
           IF SQLCODE NOT = 0                                           07776014
              MOVE 'INSERT'      TO  ABC-REFERENCIA                     07777014
              MOVE 'VLDTMES'     TO  ABC-OBJETO-ERROR                   07778014
              PERFORM 999-ABEND-DB2                                     07779014
           END-IF.                                                      07780014
                                                                        07781014
      *A2011-RUTLOG-I                                                   07782014
           INITIALIZE W-VLWCLOG0                                        07783014
                      LOGVLDTMES                                        07784014
           MOVE 'VLDTMES'             TO  VL7LOG-TABLA                  07785014
           MOVE 'INSERT'              TO  VL7LOG-OPERACION              07786014
           MOVE LENGTH OF DCLVLDTMES  TO  VL7LOG-REGISTRO-LEN           07787014
           MOVE DCLVLDTMES            TO  LOGVLDTMES                    07788014
           MOVE LOGVLDTMES            TO  VL7LOG-REGISTRO-TEXT          07789014
           PERFORM LLAMAR-VL7CRLOG                                      07790014
              THRU LLAMAR-VL7CRLOG-FIN.                                 07791014
      *A2011-RUTLOG-F                                                   07792014
                                                                        07793014
      *                                                                 07794014
       INSERTAR-VCTASMES-FIN. EXIT.                                     07795014
      *                                                                 07796014
       UPDATE-VCTASMES.                                                 07797014
      *                                                                 07798014
           MOVE VARC-CUENTA       TO LMES-CUENTA.                       07799014
                                                                        07800014
      *A2011-RUTLOG-I                                                   07801014
                                                                        07802014
           PERFORM SELUND-VLDTMES                                       07803014
              THRU SELUND-VLDTMES-FIN                                   07804014
                                                                        07805014
      *A2011-F                                                          07806014
                                                                        07807014
           MOVE VARC-CUENTA       TO  VMES-CUENTA.                      07808014
           MOVE VARC-FEULMOD      TO  VMES-FALTA.                       07809014
           MOVE 1                 TO  VMES-REACTIVA.                    07810014
      *                                                                 07811014
           MOVE CAA-FECHA-OPER    TO  VMES-FEULMOD                      07812014
           MOVE CAA-HORA-TRANS    TO  VMES-HORULMOD                     07813014
           MOVE CAA-TERMINAL      TO  VMES-NUMTER                       07814014
           MOVE CAA-USERID        TO  VMES-USUARIO                      07815014
      *                                                                 07816014
           EXEC SQL                                                     07817014
                UPDATE VLDTMES                                          07818014
                   SET VMES_FEULMOD    = :VMES-FEULMOD                  07819014
                     , VMES_HORULMOD   = :VMES-HORULMOD                 07820014
                     , VMES_NUMTER     = :VMES-NUMTER                   07821014
                     , VMES_USUARIO    = :VMES-USUARIO                  07822014
                     , VMES_FALTA      = :VMES-FALTA                    07823014
                     , VMES_REACTIVA   = :VMES-REACTIVA                 07824014
                 WHERE VMES_CUENTA     = :VMES-CUENTA                   07825014
           END-EXEC                                                     07826014
      *                                                                 07827014
      *A2011-RUTLOG-I                                                   07828014
           IF SQLCODE = 0                                               07829014
              INITIALIZE W-VLWCLOG0                                     07830014
                         LOGVLDTMES                                     07831014
              MOVE 'VLDTMES'             TO  VL7LOG-TABLA               07832014
              MOVE 'UPDATE'              TO  VL7LOG-OPERACION           07833014
              MOVE LENGTH OF DCLVLDTMES  TO  VL7LOG-REGISTRO-LEN        07834014
              MOVE VMES-FEULMOD          TO  LMES-FEULMOD               07835014
              MOVE VMES-HORULMOD         TO  LMES-HORULMOD              07836014
              MOVE VMES-NUMTER           TO  LMES-NUMTER                07837014
              MOVE VMES-USUARIO          TO  LMES-USUARIO               07838014
              MOVE VMES-FALTA            TO  LMES-FALTA                 07839014
              MOVE VMES-REACTIVA         TO  LMES-REACTIVA              07840014
              MOVE VMES-CUENTA           TO  LMES-CUENTA                07841014
              MOVE LOGVLDTMES            TO  VL7LOG-REGISTRO-TEXT       07842014
              PERFORM LLAMAR-VL7CRLOG                                   07843014
                 THRU LLAMAR-VL7CRLOG-FIN                               07844014
           END-IF                                                       07845014
      *A2011-RUTLOG-F                                                   07846014
                                                                        07847014
           IF SQLCODE NOT = 0                                           07848014
              IF SQLCODE = 100                                          07849014
                 PERFORM INSERTAR-VCTASMES2                             07850014
                    THRU INSERTAR-VCTASMES2-FIN                         07851014
              ELSE                                                      07852014
                 MOVE 'UPDATE'      TO  ABC-REFERENCIA                  07853014
                 MOVE 'VLDTMES'     TO  ABC-OBJETO-ERROR                07854014
                 PERFORM 999-ABEND-DB2                                  07855014
              END-IF                                                    07856014
           END-IF.                                                      07857014
      *                                                                 07858014
      *                                                                 07859014
       UPDATE-VCTASMES-FIN. EXIT.                                       07860014
      *                                                                 07861014
                                                                        07862014
       INSERTAR-VCTASMES2.                                              07863014
      *                                                                 07864014
           MOVE VARC-CUENTA       TO  VMES-CUENTA.                      07865014
           MOVE VARC-FEULMOD      TO  VMES-FALTA.                       07866014
           MOVE 1                 TO  VMES-REACTIVA.                    07867014
      *                                                                 07868014
           MOVE CAA-FECHA-OPER    TO  VMES-FEULMOD                      07869014
                                      VMES-FEALTREG                     07870014
           MOVE CAA-HORA-TRANS    TO  VMES-HORULMOD                     07871014
           MOVE CAA-TERMINAL      TO  VMES-NUMTER                       07872014
           MOVE CAA-USERID        TO  VMES-USUARIO                      07873014
           MOVE SPACES            TO  VMES-FILLER.                      07874014
      *                                                                 07875014
           EXEC SQL                                                     07876014
                INSERT INTO VLDTMES                                     07877014
                VALUES (:DCLVLDTMES)                                    07878014
           END-EXEC                                                     07879014
      *                                                                 07880014
           IF SQLCODE NOT = 0                                           07881014
              MOVE 'INSERT2'     TO  ABC-REFERENCIA                     07882014
              MOVE 'VLDTMES'     TO  ABC-OBJETO-ERROR                   07883014
              PERFORM 999-ABEND-DB2                                     07884014
           END-IF.                                                      07885014
      *                                                                 07886014
      *A2011-RUTLOG-I                                                   07887014
           INITIALIZE W-VLWCLOG0                                        07888014
                      LOGVLDTMES                                        07889014
           MOVE 'VLDTMES'             TO  VL7LOG-TABLA                  07890014
           MOVE 'INSERT'              TO  VL7LOG-OPERACION              07891014
           MOVE LENGTH OF DCLVLDTMES  TO  VL7LOG-REGISTRO-LEN           07892014
           MOVE DCLVLDTMES            TO  LOGVLDTMES                    07893014
           MOVE LOGVLDTMES            TO  VL7LOG-REGISTRO-TEXT          07894014
           PERFORM LLAMAR-VL7CRLOG                                      07895014
              THRU LLAMAR-VL7CRLOG-FIN.                                 07896014
      *A2011-RUTLOG-F                                                   07897014
                                                                        07898014
       INSERTAR-VCTASMES2-FIN. EXIT.                                    07899014
      *                                                                 07900014
      *                                                                 07901014
       BUSCAR-ENTIDAD.                                                  07902014
      *                                                                 07903014
              EXEC SQL                                                  07904014
JPC@1 *            SELECT  *                                            07905014
                   SELECT VXMI_CODBE                                    07906014
                        , VXMI_CODCLI                                   07907014
                        , VXMI_DENOM                                    07908014
                        , VXMI_NIF                                      07909014
                        , VXMI_DOMIC                                    07910014
                        , VXMI_LOCAL                                    07911014
                        , VXMI_CODPOS                                   07912014
                        , VXMI_CNAE                                     07913014
                        , VXMI_SUCVAL                                   07914014
                        , VXMI_NUMFAC                                   07915014
                        , VXMI_VALENT                                   07916014
                        , VXMI_CTAVAL                                   07917014
                        , VXMI_VALCER                                   07918014
                        , VXMI_MULPLA                                   07919014
                        , VXMI_RETEN                                    07920014
                        , VXMI_IVA                                      07921014
                        , VXMI_INCLUS                                   07922014
                        , VXMI_EXCLUS                                   07923014
                        , VXMI_PROVIS                                   07924014
                        , VXMI_FLISOP                                   07925014
                        , VXMI_LISENT                                   07926014
                        , VXMI_LISPAG                                   07927014
                        , VXMI_INCORP                                   07928014
                        , VXMI_CONTRT                                   07929014
                        , VXMI_CONTRT6                                  07930014
                        , VXMI_REF9                                     07931014
                        , VXMI_DELEGHAC                                 07932014
                        , VXMI_ADMINHAC                                 07933014
                        , VXMI_PRETELHAC                                07934014
                        , VXMI_TELEFHAC                                 07935014
                        , VXMI_APNOMHAC                                 07936014
                        , VXMI_LUNES                                    07937014
                        , VXMI_VIERNES                                  07938014
                        , VXMI_YAPRESEN                                 07939014
                        , VXMI_IMPRE1                                   07940014
                        , VXMI_IMPRE2                                   07941014
                        , VXMI_FILLER                                   07942014
                        , VXMI_LISCTIMP                                 07943014
                        , VXMI_CONTCTA                                  07944014
                        , VXMI_PASS1                                    07945014
                        , VXMI_PASS2                                    07946014
                        , VXMI_LISCTA                                   07947014
                        , VXMI_LISAGTES                                 07948014
                        , VXMI_LISREDUC                                 07949014
                        , VXMI_LISFESTI                                 07950014
                        , VXMI_LISMONED                                 07951014
                        , VXMI_LISCONTA                                 07952014
                        , VXMI_LISENT_1                                 07953014
                        , VXMI_LISCTA_SUC                               07954014
                        , VXMI_LISVALOR                                 07955014
                        , VXMI_CONT_REV                                 07956014
                        , VXMI_VALORACION                               07957014
                        , VXMI_LIS_EXTRJ                                07958014
                        , VXMI_FILLER1                                  07959014
                        , VXMI_APCTAOFI                                 07960014
                        , VXMI_TIPCUST                                  07961014
                        , VXMI_MANFIS                                   07962014
                        , VXMI_OPECUST                                  07963014
                        , VXMI_OPEBOLSA                                 07964014
                        , VXMI_AVISOS                                   07965014
                        , VXMI_CONPANT                                  07966014
                        , VXMI_COMCUST                                  07967014
                        , VXMI_IMPALT                                   07968014
                        , VXMI_CTACARGO                                 07969014
                        , VXMI_CTAABONO                                 07970014
                        , VXMI_CONTEN                                   07971014
                        , VXMI_CONTEV                                   07972014
                        , VXMI_CONTSN                                   07973014
                        , VXMI_CONTSV                                   07974014
                        , VXMI_LIS_RESTOS                               07975014
                        , VXMI_DIAS_LIMIT                               07976014
                        , VXMI_LIS_C_EXEN                               07977014
                        , VXMI_LIS_GJUD_BLO                             07978014
                        , VXMI_FEALTREG                                 07979014
                        , VXMI_FEULMOD                                  07980014
                        , VXMI_HORULMOD                                 07981014
                        , VXMI_NUMTER                                   07982014
                        , VXMI_USUARIO                                  07983014
                        , VXMI_FILLER2                                  07984014
JPC@1 *            INTO :DCLVLDTXMI                                     07985014
                   INTO  :VXMI-CODBE                                    07986014
                      ,  :VXMI-CODCLI                                   07987014
                      ,  :VXMI-DENOM                                    07988014
                      ,  :VXMI-NIF                                      07989014
                      ,  :VXMI-DOMIC                                    07990014
                      ,  :VXMI-LOCAL                                    07991014
                      ,  :VXMI-CODPOS                                   07992014
                      ,  :VXMI-CNAE                                     07993014
                      ,  :VXMI-SUCVAL                                   07994014
                      ,  :VXMI-NUMFAC                                   07995014
                      ,  :VXMI-VALENT                                   07996014
                      ,  :VXMI-CTAVAL                                   07997014
                      ,  :VXMI-VALCER                                   07998014
                      ,  :VXMI-MULPLA                                   07999014
                      ,  :VXMI-RETEN                                    08000014
                      ,  :VXMI-IVA                                      08001014
                      ,  :VXMI-INCLUS                                   08002014
                      ,  :VXMI-EXCLUS                                   08003014
                      ,  :VXMI-PROVIS                                   08004014
                      ,  :VXMI-FLISOP                                   08005014
                      ,  :VXMI-LISENT                                   08006014
                      ,  :VXMI-LISPAG                                   08007014
                      ,  :VXMI-INCORP                                   08008014
                      ,  :VXMI-CONTRT                                   08009014
                      ,  :VXMI-CONTRT6                                  08010014
                      ,  :VXMI-REF9                                     08011014
                      ,  :VXMI-DELEGHAC                                 08012014
                      ,  :VXMI-ADMINHAC                                 08013014
                      ,  :VXMI-PRETELHAC                                08014014
                      ,  :VXMI-TELEFHAC                                 08015014
                      ,  :VXMI-APNOMHAC                                 08016014
                      ,  :VXMI-LUNES                                    08017014
                      ,  :VXMI-VIERNES                                  08018014
                      ,  :VXMI-YAPRESEN                                 08019014
                      ,  :VXMI-IMPRE1                                   08020014
                      ,  :VXMI-IMPRE2                                   08021014
                      ,  :VXMI-FILLER                                   08022014
                      ,  :VXMI-LISCTIMP                                 08023014
                      ,  :VXMI-CONTCTA                                  08024014
                      ,  :VXMI-PASS1                                    08025014
                      ,  :VXMI-PASS2                                    08026014
                      ,  :VXMI-LISCTA                                   08027014
                      ,  :VXMI-LISAGTES                                 08028014
                      ,  :VXMI-LISREDUC                                 08029014
                      ,  :VXMI-LISFESTI                                 08030014
                      ,  :VXMI-LISMONED                                 08031014
                      ,  :VXMI-LISCONTA                                 08032014
                      ,  :VXMI-LISENT-1                                 08033014
                      ,  :VXMI-LISCTA-SUC                               08034014
                      ,  :VXMI-LISVALOR                                 08035014
                      ,  :VXMI-CONT-REV                                 08036014
                      ,  :VXMI-VALORACION                               08037014
                      ,  :VXMI-LIS-EXTRJ                                08038014
                      ,  :VXMI-FILLER1                                  08039014
                      ,  :VXMI-APCTAOFI                                 08040014
                      ,  :VXMI-TIPCUST                                  08041014
                      ,  :VXMI-MANFIS                                   08042014
                      ,  :VXMI-OPECUST                                  08043014
                      ,  :VXMI-OPEBOLSA                                 08044014
                      ,  :VXMI-AVISOS                                   08045014
                      ,  :VXMI-CONPANT                                  08046014
                      ,  :VXMI-COMCUST                                  08047014
                      ,  :VXMI-IMPALT                                   08048014
                      ,  :VXMI-CTACARGO                                 08049014
                      ,  :VXMI-CTAABONO                                 08050014
                      ,  :VXMI-CONTEN                                   08051014
                      ,  :VXMI-CONTEV                                   08052014
                      ,  :VXMI-CONTSN                                   08053014
                      ,  :VXMI-CONTSV                                   08054014
                      ,  :VXMI-LIS-RESTOS                               08055014
                      ,  :VXMI-DIAS-LIMIT                               08056014
                      ,  :VXMI-LIS-C-EXEN                               08057014
                      ,  :VXMI-LIS-GJUD-BLO                             08058014
                      ,  :VXMI-FEALTREG                                 08059014
                      ,  :VXMI-FEULMOD                                  08060014
                      ,  :VXMI-HORULMOD                                 08061014
                      ,  :VXMI-NUMTER                                   08062014
                      ,  :VXMI-USUARIO                                  08063014
                      ,  :VXMI-FILLER2                                  08064014
                  FROM  VLDTXMI                                         08065014
                 WHERE  VXMI_CODBE  = :VXMI-CODBE                       08066014
           END-EXEC                                                     08067014
      *                                                                 08068014
           MOVE SQLCODE TO SQLCODE-AUX                                  08069014
      *                                                                 08070014
           EVALUATE TRUE                                                08071014
              WHEN DB2-OK                                               08072014
      *A2011-RUTLOG-I                                                   08073014
                INITIALIZE W-VLWCLOG0                                   08074014
                           LOGVLDTXMI                                   08075014
                MOVE 'VLDTXMI'             TO  VL7LOG-TABLA             08076014
                MOVE 'SELECT'              TO  VL7LOG-OPERACION         08077014
                MOVE LENGTH OF DCLVLDTXMI  TO  VL7LOG-REGISTRO-LEN      08078014
                MOVE DCLVLDTXMI            TO  LOGVLDTXMI               08079014
                MOVE LOGVLDTXMI            TO  VL7LOG-REGISTRO-TEXT     08080014
                PERFORM LLAMAR-VL7CRLOG                                 08081014
                   THRU LLAMAR-VL7CRLOG-FIN                             08082014
      *A2011-RUTLOG-F                                                   08083014
      *                                                                 08084014
              WHEN  DB2-NOTFND                                          08085014
                    MOVE  'VLE1523'   TO  CAA-COD-ERROR                 08086014
                    MOVE  -1          TO  ENT0101L                      08087014
                    PERFORM  3-FINAL                                    08088014
      *                                                                 08089014
              WHEN OTHER                                                08090014
                   MOVE 'SELECT'      TO  ABC-REFERENCIA                08091014
                   MOVE 'VLDTXMI'     TO  ABC-OBJETO-ERROR              08092014
                   PERFORM 999-ABEND-DB2                                08093014
      *                                                                 08094014
           END-EVALUATE.                                                08095014
      *                                                                 08096014
      *                                                                 08097014
       BUSCAR-ENTIDAD-FIN. EXIT.                                        08098014
      *                                                                 08099014
       LEER-VLDTARC-MOD.                                                08100014
      *                                                                 08101014
           EXEC SQL                                                     08102014
JPC@1 *         SELECT  *                                               08103014
                SELECT  VARC_CUENTA                                     08104014
                     ,  VARC_CENTAD                                     08105014
                     ,  VARC_NUMCLI                                     08106014
                     ,  VARC_CLMAST                                     08107014
                     ,  VARC_MONEDA                                     08108014
                     ,  VARC_SUCURS                                     08109014
                     ,  VARC_CTACAR                                     08110014
                     ,  VARC_CTAABO                                     08111014
                     ,  VARC_TEXTO                                      08112014
                     ,  VARC_PRESEN                                     08113014
                     ,  VARC_GRUPO                                      08114014
                     ,  VARC_RUT                                        08115014
                     ,  VARC_CNAE                                       08116014
                     ,  VARC_SITUAC                                     08117014
                     ,  VARC_EXEN1                                      08118014
                     ,  VARC_EXEN2                                      08119014
                     ,  VARC_EXEN3                                      08120014
                     ,  VARC_EXEN4                                      08121014
                     ,  VARC_EXEN5                                      08122014
                     ,  VARC_EXEN6                                      08123014
                     ,  VARC_EXEN7                                      08124014
                     ,  VARC_EXEN8                                      08125014
                     ,  VARC_EXEN9                                      08126014
                     ,  VARC_EXEN10                                     08127014
                     ,  VARC_ANALIS                                     08128014
                     ,  VARC_CLACARGO                                   08129014
                     ,  VARC_CLABONO                                    08130014
                     ,  VARC_NUMDOM                                     08131014
                     ,  VARC_CODSUS                                     08132014
                     ,  VARC_FE_ULT_EXT                                 08133014
                     ,  VARC_PAIS                                       08134014
                     ,  VARC_FE_CARTERA                                 08135014
                     ,  VARC_CLTELEX                                    08136014
                     ,  VARC_FE_ALTA                                    08137014
                     ,  VARC_VALORACION                                 08138014
                     ,  VARC_VALEXTRJ                                   08139014
                     ,  VARC_INVERSOR                                   08140014
                     ,  VARC_DIRECTA                                    08141014
                     ,  VARC_MAX_CVE_1                                  08142014
                     ,  VARC_MAX_DCU_5                                  08143014
                     ,  VARC_MAX_SUS_6                                  08144014
                     ,  VARC_MAX_DIV_7                                  08145014
                     ,  VARC_MAX_AMO_8                                  08146014
                     ,  VARC_MAX_PAJ_9                                  08147014
                     ,  VARC_FECHA_102                                  08148014
                     ,  VARC_TARIFACUS                                  08149014
                     ,  VARC_SWIFT_TELEX                                08150014
                     ,  VARC_TELEX_2                                    08151014
                     ,  VARC_GRUPO_CTAS                                 08152014
                     ,  VARC_OPER_TIT                                   08153014
                     ,  VARC_FEALTREG                                   08154014
                     ,  VARC_FEULMOD                                    08155014
                     ,  VARC_HORULMOD                                   08156014
                     ,  VARC_NUMTER                                     08157014
                     ,  VARC_USUARIO                                    08158014
                     ,  VARC_FILLER                                     08159014
                     ,  VARC_CTAVAL20                                   08160014
      *@ZAL-INI                                                         08161014
      *              ,  VARC_NUMMAN                                     08162014
                     ,  VARC_GRUPO_CTAS                                 08163014
      *@ZAL-INI                                                         08164014
                     ,  VARC_INDIMP                                     08165014
                     ,  VARC_INDSAB                                     08166014
JPC@1 *           INTO :DCLVLDTARC                                      08167014
                  INTO :VARC-CUENTA                                     08168014
                     , :VARC-CENTAD                                     08169014
                     , :VARC-NUMCLI                                     08170014
                     , :VARC-CLMAST                                     08171014
                     , :VARC-MONEDA                                     08172014
                     , :VARC-SUCURS                                     08173014
                     , :VARC-CTACAR                                     08174014
                     , :VARC-CTAABO                                     08175014
                     , :VARC-TEXTO                                      08176014
                     , :VARC-PRESEN                                     08177014
                     , :VARC-GRUPO                                      08178014
                     , :VARC-RUT                                        08179014
                     , :VARC-CNAE                                       08180014
                     , :VARC-SITUAC                                     08181014
                     , :VARC-EXEN1                                      08182014
                     , :VARC-EXEN2                                      08183014
                     , :VARC-EXEN3                                      08184014
                     , :VARC-EXEN4                                      08185014
                     , :VARC-EXEN5                                      08186014
                     , :VARC-EXEN6                                      08187014
                     , :VARC-EXEN7                                      08188014
                     , :VARC-EXEN8                                      08189014
                     , :VARC-EXEN9                                      08190014
                     , :VARC-EXEN10                                     08191014
                     , :VARC-ANALIS                                     08192014
                     , :VARC-CLACARGO                                   08193014
                     , :VARC-CLABONO                                    08194014
                     , :VARC-NUMDOM                                     08195014
                     , :VARC-CODSUS                                     08196014
                     , :VARC-FE-ULT-EXT                                 08197014
                     , :VARC-PAIS                                       08198014
                     , :VARC-FE-CARTERA                                 08199014
                     , :VARC-CLTELEX                                    08200014
                     , :VARC-FE-ALTA                                    08201014
                     , :VARC-VALORACION                                 08202014
                     , :VARC-VALEXTRJ                                   08203014
                     , :VARC-INVERSOR                                   08204014
                     , :VARC-DIRECTA                                    08205014
                     , :VARC-MAX-CVE-1                                  08206014
                     , :VARC-MAX-DCU-5                                  08207014
                     , :VARC-MAX-SUS-6                                  08208014
                     , :VARC-MAX-DIV-7                                  08209014
                     , :VARC-MAX-AMO-8                                  08210014
                     , :VARC-MAX-PAJ-9                                  08211014
                     , :VARC-FECHA-102                                  08212014
                     , :VARC-TARIFACUS                                  08213014
                     , :VARC-SWIFT-TELEX                                08214014
                     , :VARC-TELEX-2                                    08215014
                     , :VARC-GRUPO-CTAS                                 08216014
                     , :VARC-OPER-TIT                                   08217014
                     , :VARC-FEALTREG                                   08218014
                     , :VARC-FEULMOD                                    08219014
                     , :VARC-HORULMOD                                   08220014
                     , :VARC-NUMTER                                     08221014
                     , :VARC-USUARIO                                    08222014
                     , :VARC-FILLER                                     08223014
                     , :VARC-CTAVAL20                                   08224014
      *@ZAL-INI                                                         08225014
      *              , :VARC-NUMMAN                                     08226014
                     , :VARC-GRUPO-CTAS                                 08227014
      *@ZAL-FIN                                                         08228014
                     , :VARC-INDIMP                                     08229014
                     , :VARC-INDSAB                                     08230014
                  FROM  VLDTARC                                         08231014
                 WHERE  VARC_CUENTA  = :VARC-CUENTA                     08232014
           END-EXEC                                                     08233014
      *                                                                 08234014
           MOVE SQLCODE TO SQLCODE-AUX                                  08235014
      *                                                                 08236014
           EVALUATE TRUE                                                08237014
              WHEN DB2-OK                                               08238014
                   IF VARC-SITUAC = 'X'                                 08239014
                      MOVE 'VLE1945'  TO CAA-COD-ERROR                  08240014
                      MOVE -1         TO CTA0101L                       08241014
                      PERFORM 3-FINAL                                   08242014
                   END-IF                                               08243014
                   IF VARC-SITUAC = 'B'                                 08244014
      *200306088-FIN                                                    08245014
      *               MOVE 'VLA0055'   TO  CAA-COD-AVISO2               08246014
                      MOVE 'VLA0086'   TO  CAA-COD-AVISO2               08247014
      *200306088-FIN                                                    08248014
                   END-IF                                               08249014
                   IF VARC-SITUAC = 'U'                                 08250014
                      MOVE 'VLA0076'   TO  CAA-COD-AVISO2               08251014
                   END-IF                                               08252014
      *                                                                 08253014
              WHEN  DB2-NOTFND                                          08254014
                    MOVE  'VLE0142'   TO  CAA-COD-ERROR                 08255014
                    MOVE  -1          TO  CTA0101L                      08256014
                    PERFORM  3-FINAL                                    08257014
      *                                                                 08258014
              WHEN OTHER                                                08259014
                   MOVE 'SELECT'      TO  ABC-REFERENCIA                08260014
                   MOVE 'VLDTARC'     TO  ABC-OBJETO-ERROR              08261014
                   PERFORM 999-ABEND-DB2                                08262014
      *                                                                 08263014
           END-EVALUATE.                                                08264014
      *A2011-RUTLOG-I                                                   08265014
           INITIALIZE W-VLWCLOG0                                        08266014
                      LOGVLDTARC                                        08267014
           MOVE 'VLDTARC'             TO  VL7LOG-TABLA                  08268014
           MOVE 'SELECT'              TO  VL7LOG-OPERACION              08269014
           MOVE LENGTH OF DCLVLDTARC  TO  VL7LOG-REGISTRO-LEN           08270014
           MOVE DCLVLDTARC            TO  LOGVLDTARC                    08271014
           MOVE LOGVLDTARC            TO  VL7LOG-REGISTRO-TEXT          08272014
           PERFORM LLAMAR-VL7CRLOG                                      08273014
              THRU LLAMAR-VL7CRLOG-FIN.                                 08274014
      *A2011-RUTLOG-F                                                   08275014
                                                                        08276014
      *                                                                 08277014
       LEER-VLDTARC-MOD-FIN.  EXIT.                                     08278014
      *                                                                 08279014
       999-ABEND-CICS.                                                  08280014
      *                                                                 08281014
           MOVE 'S'        TO  ABC-ABEND.                               08282014
           MOVE 'VL2C1010' TO  ABC-PROGRAMA.                            08283014
           MOVE EIBFN      TO  ABC-EIBFN.                               08284014
           MOVE EIBRSRCE   TO  ABC-EIBRSRCE.                            08285014
           MOVE EIBRCODE   TO  ABC-EIBRCODE.                            08286014
           MOVE EIBRESP    TO  ABC-EIBRESP1.                            08287014
           MOVE EIBRESP2   TO  ABC-EIBRESP2.                            08288014
           EXEC CICS                                                    08289014
                LINK PROGRAM  (QG1CABC)                                 08290014
                     COMMAREA ( QGECABC )                               08291014
           END-EXEC.                                                    08292014
      *                                                                 08293014
       999-ABEND-DB2.                                                   08294014
      *                                                                 08295014
           MOVE 'N'        TO  ABC-ABEND.                               08296014
           MOVE 'VL2C1010' TO  ABC-PROGRAMA.                            08297014
           MOVE SQLCODE    TO  ABC-SQLCODE   W-SQLCODE-NUM              08298014
           MOVE SQLERRM    TO  ABC-SQLERRM.                             08299014
           MOVE 'VLE1000'        TO CAA-COD-ERROR                       08300014
           MOVE ABC-OBJETO-ERROR TO CAA-VAR1-ERROR                      08301014
           MOVE W-SQLCODE-NUM    TO W-SQLCODE-EDIT                      08302014
           MOVE W-SQLCODE-EDIT   TO CAA-VAR2-ERROR                      08303014
                                                                        08304014
           EXEC CICS                                                    08305014
                LINK PROGRAM  (QG1CABC)                                 08306014
                     COMMAREA  (QGECABC)                                08307014
           END-EXEC.                                                    08308014
                                                                        08309014
           PERFORM 3-FINAL.                                             08310014
      *                                                                 08311014
      *                                                                 08312014
      *************************NUM-CUENTA***********************        08313014
      *                                                                 08314014
       NUM-CUENTA.                                                      08315014
      *                                                                 08316014
           INITIALIZE W-PEWC4390                                        08317014
                                                                        08318014
           MOVE TIT0101I                    TO  W4390-NUMCLIEN          08319014
                                                                        08320014
           EXEC CICS                                                    08321014
                LINK PROGRAM (PE2C5390)                                 08322014
                COMMAREA   (W-PEWC4390)                                 08323014
           END-EXEC.                                                    08324014
                                                                        08325014
           IF EIBRESP NOT = DFHRESP(NORMAL)                             08326014
              MOVE 'ERROR EN PE2C5390'      TO  ABC-REFERENCIA          08327014
              MOVE 'PE2C5390'               TO  ABC-OBJETO-ERROR        08328014
              PERFORM 999-ABEND-CICS                                    08329014
           END-IF.                                                      08330014
                                                                        08331014
           EVALUATE W4390-PCRETOR                                       08332014
               WHEN '00'                                                08333014
               WHEN '20'                                                08334014
               WHEN '30'                                                08335014
               WHEN '40'                                                08336014
                    CONTINUE                                            08337014
               WHEN OTHER                                               08338014
                    MOVE 'VLE1398'          TO CAA-COD-ERROR            08339014
                    MOVE 'PE2C5390'         TO CAA-VAR1-ERROR           08340014
                    PERFORM 3-FINAL                                     08341014
           END-EVALUATE.                                                08342014
                                                                        08343014
                                                                        08344014
           PERFORM VARYING  W-I   FROM 1 BY 1                           08345014
                     UNTIL (W-I > W4390-NOCCURS) OR                     08346014
                           (W-I > 100)                                  08347014
              IF W4390-NOCCURS = '101'  AND                             08348014
                 W4390-NUMECTA (W-I) (2:7) = W-CUENTA                   08349014
                 MOVE W4390-PECENTID (W-I)       TO END0101O            08350014
                 MOVE W4390-OFIAPE   (W-I)       TO CEN0101O            08351014
                 MOVE '00'                       TO DGT0101O            08352014
                 MOVE W4390-CODISER  (W-I)       TO PRD0101O            08353014
                 MOVE W4390-NUMECTA  (W-I) (8:1) TO DG20101O            08354014
                 MOVE 101                        TO W-I                 08355014
                 MOVE 'SI'                       TO SW-ENTRO            08356014
              END-IF                                                    08357014
           END-PERFORM.                                                 08358014
                                                                        08359014
           IF NOT ENTRO                                                 08360014
              MOVE -1 TO CTA0101L                                       08361014
              MOVE 'VLE1398'  TO CAA-COD-ERROR                          08362014
              PERFORM 3-FINAL                                           08363014
           END-IF.                                                      08364014
      *                                                                 08365014
       NUM-CUENTA-FIN. EXIT.                                            08366014
      *                                                                 08367014
      *                                                                 08368014
      *200702146-INI                                                    08369014
       069-TRATAR-JETFORM.                                              08370014
      *200804248-INI                                                    08371014
           MOVE '-RVL1FM '             TO W-TS.                         08372014
           EXEC CICS                                                    08373014
                DELETEQ TS QUEUE(W-TS) NOHANDLE                         08374014
           END-EXEC.                                                    08375014
      *200804248-INI                                                    08376014
      *                                                                *08377014
           MOVE SPACES            TO VL291-L01-LIBRE.                   08378014
           MOVE SPACES            TO VL291-L02-LIBRE.                   08379014
                                                                        08380014
           PERFORM 000069-IMPRIMIR-CONTRATO                             08381014
              THRU 000069-IMPRIMIR-CONTRATO-FIN.                        08382014
                                                                        08383014
           MOVE 'J'                         TO CAA-IND-PANDOC  (1).     08384014
           MOVE '+DC1'                      TO CAA-DESTINO     (1).     08385014
           MOVE '1'                         TO CAA-NUM-DOCUM   (1).     08386014
           MOVE '00'                        TO CAA-PRILIN-DOCUM(1).     08387014
       069-TRATAR-JETFORM-FIN.                                          08388014
           EXIT.                                                        08389014
      *200702146-FIN                                                    08390014
      *                                                                 08391014
       999-TRATAR-JETFORM.                                              08392014
      *                                                                 08393014
           MOVE CAA-FECHA-OPER    TO W-FECHA-AMD-N                      08394014
           MOVE W-AA-AMD          TO W-AA-DMA-G                         08395014
           MOVE W-MM-AMD          TO W-MM-DMA-G                         08396014
           MOVE W-DD-AMD          TO W-DD-DMA-G                         08397014
           MOVE W-FECHA-DMA-G     TO WS-VL01-L01-FECHA                  08398014
      *                                                                 08399014
           MOVE END0101O          TO WS-VL01-L02-BANCO                  08400014
           MOVE CEN0101O          TO WS-VL01-L02-OFICI                  08401014
           MOVE PRD0101O          TO WS-VL01-L02-CUENTA (01:02)         08402014
           MOVE CTA0101O          TO WS-VL01-L02-CUENTA (03:07)         08403014
           MOVE DG20101O          TO WS-VL01-L02-CUENTA (10:01)         08404014
           MOVE DGT0101O          TO WS-VL01-L02-DGC                    08405014
                                                                        08406014
           INITIALIZE                TCWC2010.                          08407014
           MOVE END0101O          TO RUTI-ENTIDAD.                      08408014
           MOVE END0101O          TO RUTI-CAMPO (01:04)                 08409014
           MOVE CEN0101O          TO RUTI-CAMPO (05:04)                 08410014
           MOVE 0                 TO RUTI-CAMPO (09:01)                 08411014
           MOVE +9                TO RUTI-LONG.                         08412014
           CALL 'TC8C2030'           USING TCWC2010.                    08413014
           IF RUTI-CODERR NOT EQUAL SPACES AND 'QRE0006'                08414014
              MOVE '0'            TO WS-VL01-L02-DGC (01:01)            08415014
           ELSE                                                         08416014
              MOVE RUTI-DIG       TO WS-VL01-L02-DGC (01:01)            08417014
           END-IF                                                       08418014
                                                                        08419014
           INITIALIZE                TCWC2010.                          08420014
           MOVE END0101O          TO RUTI-ENTIDAD.                      08421014
           MOVE PRD0101O          TO RUTI-CAMPO (01:02)                 08422014
           MOVE CTA0101O          TO RUTI-CAMPO (03:07)                 08423014
           MOVE DG20101O          TO RUTI-CAMPO (10:01)                 08424014
           MOVE 0                 TO RUTI-CAMPO (11:01)                 08425014
           MOVE +11               TO RUTI-LONG.                         08426014
           CALL 'TC8C2030'           USING TCWC2010.                    08427014
           IF RUTI-CODERR NOT EQUAL SPACES AND 'QRE0006'                08428014
              MOVE '0'            TO WS-VL01-L02-DGC (02:01)            08429014
           ELSE                                                         08430014
              MOVE RUTI-DIG       TO WS-VL01-L02-DGC (02:01)            08431014
           END-IF                                                       08432014
      *                                                                 08433014
      *                                                                 08434014
           PERFORM FORMATEO-FECHA                                       08435014
              THRU FORMATEO-FECHA-FIN.                                  08436014
      *                                                                 08437014
           IF W520-SUJGRUP = 'F'                                        08438014
              MOVE 'NATURAL'      TO WS-VL01-L04-TIPPER                 08439014
           ELSE                                                         08440014
              MOVE 'JURIDICA'     TO WS-VL01-L04-TIPPER                 08441014
           END-IF                                                       08442014
                                                                        08443014
           PERFORM ACCEDER-VLDTADT                                      08444014
              THRU ACCEDER-VLDTADT-FIN                                  08445014
                                                                        08446014
           IF W-COUNT > 0                                               08447014
              MOVE 'MANCOMUNADO'  TO WS-VL01-L04-TIPCTA                 08448014
           ELSE                                                         08449014
              MOVE 'INDIVIDUAL '  TO WS-VL01-L04-TIPCTA                 08450014
           END-IF                                                       08451014
                                                                        08452014
                                                                        08453014
           INITIALIZE                TCWC1200                           08454014
           MOVE VARC-MONEDA       TO W120-CDDIVISS                      08455014
           PERFORM OBTENER-MONEDA                                       08456014
              THRU OBTENER-MONEDA-FIN                                   08457014
           MOVE W120-NBDIVIC (01) TO WS-VL01-L04-MONEDA                 08458014
      *                                                                 08459014
           MOVE W520-CODIDENT     TO WS-VL01-L05-TIPDOC                 08460014
           MOVE W520-CLAIDENT     TO WS-VL01-L05-NRODOC                 08461014
                                                                        08462014
           MOVE  'DIRECCION : '   TO WS-VL01-L06-TXTDIRE                08463014
           MOVE  W-DOMICILI1      TO WS-VL01-L06-DIRECCI                08464014
      *                                                                 08465014
           PERFORM DIREC-CORRES                                         08466014
              THRU DIREC-CORRES-F                                       08467014
      *                                                                 08468014
           PERFORM VER-REPRESEN                                         08469014
              THRU VER-REPRESEN-F                                       08470014
      *                                                                 08471014
           PERFORM VER-TITULAR2                                         08472014
              THRU VER-TITULAR2-F                                       08473014
      *                                                                 08474014
      * CUENTA CARGO                                                    08475014
           MOVE VARC-FILLER(01:20)   TO W-CCC-CAR-JET                   08476014
           MOVE W-SUC-CAR-JET        TO WS-VL01-L32-OFI01               08477014
           MOVE W-CUENTA-JET         TO WS-VL01-L32-CTA01               08478014
           MOVE W-DIG-CAR-JET        TO WS-VL01-L32-DGC01               08479014
                                                                        08480014
           INITIALIZE                   W-BGECMDC                       08481014
           MOVE W-ENT-CAR-JET        TO MDC-ENTIDAD                     08482014
           MOVE W-SUC-CAR-JET        TO MDC-CENTRO-ALTA                 08483014
           MOVE W-CUENTA-JET         TO MDC-CUENTA                      08484014
      *                                                                 08485014
           EXEC CICS                                                    08486014
                LINK PROGRAM (BG2CMDC0)                                 08487014
                COMMAREA (BGECMDC)                                      08488014
           END-EXEC                                                     08489014
      *                                                                 08490014
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         08491014
              MOVE 'ERROR EN BGECMDC0' TO ABC-REFERENCIA                08492014
              MOVE 'BG2CMDC0'          TO ABC-OBJETO-ERROR              08493014
              PERFORM 999-ABEND-CICS                                    08494014
           END-IF                                                       08495014
      *                                                                 08496014
           IF MDC-CODERR = SPACES                                       08497014
              INITIALIZE                TCWC1200                        08498014
              MOVE MDC-CDDIVIS       TO W120-CDDIVISS                   08499014
              MOVE MDC-DIGICCC1      TO WS-VL01-L32-DGC01 (01:01)       08500014
              MOVE MDC-DIGICCC2      TO WS-VL01-L32-DGC01 (02:01)       08501014
              PERFORM OBTENER-MONEDA                                    08502014
                 THRU OBTENER-MONEDA-FIN                                08503014
              MOVE W120-NBDIVIC (01) TO WS-VL01-L33-MDACA               08504014
           ELSE                                                         08505014
              MOVE SPACES            TO WS-VL01-L33-MDACA               08506014
           END-IF                                                       08507014
      *                                                                 08508014
      * CUENTA ABONO                                                    08509014
           MOVE VARC-FILLER(21:20)   TO W-CCC-CAR-JET                   08510014
           MOVE W-SUC-CAR-JET        TO WS-VL01-L32-OFI02               08511014
           MOVE W-CUENTA-JET         TO WS-VL01-L32-CTA02               08512014
           MOVE W-DIG-CAR-JET        TO WS-VL01-L32-DGC02               08513014
                                                                        08514014
           INITIALIZE                   W-BGECMDC                       08515014
           MOVE W-ENT-CAR-JET        TO MDC-ENTIDAD                     08516014
           MOVE W-SUC-CAR-JET        TO MDC-CENTRO-ALTA                 08517014
           MOVE W-CUENTA-JET         TO MDC-CUENTA                      08518014
      *                                                                 08519014
           EXEC CICS                                                    08520014
                LINK PROGRAM (BG2CMDC0)                                 08521014
                COMMAREA (BGECMDC)                                      08522014
           END-EXEC                                                     08523014
      *                                                                 08524014
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         08525014
              MOVE 'ERROR EN BGECMDC0' TO ABC-REFERENCIA                08526014
              MOVE 'BG2CMDC0'          TO ABC-OBJETO-ERROR              08527014
              PERFORM 999-ABEND-CICS                                    08528014
           END-IF                                                       08529014
      *                                                                 08530014
           IF MDC-CODERR = SPACES                                       08531014
              INITIALIZE                TCWC1200                        08532014
              MOVE MDC-CDDIVIS       TO W120-CDDIVISS                   08533014
              MOVE MDC-DIGICCC1      TO WS-VL01-L32-DGC02 (01:01)       08534014
              MOVE MDC-DIGICCC2      TO WS-VL01-L32-DGC02 (02:01)       08535014
              PERFORM OBTENER-MONEDA                                    08536014
                 THRU OBTENER-MONEDA-FIN                                08537014
              MOVE W120-NBDIVIC (01) TO WS-VL01-L33-MDAAB               08538014
           ELSE                                                         08539014
              MOVE SPACES            TO WS-VL01-L33-MDAAB               08540014
           END-IF                                                       08541014
      *                                                                 08542014
      * CUENTA USUFRUCTUARIO                                            08543014
           MOVE VARC-FILLER(41:20)   TO W-CCC-CAR-JET                   08544014
           IF W-CUENTA-JET > ZEROS                                      08545014
              MOVE W-ENT-CAR-JET     TO WS-VL01-L32-ENT03               08546014
              MOVE W-SUC-CAR-JET     TO WS-VL01-L32-OFI03               08547014
              MOVE W-CUENTA-JET      TO WS-VL01-L32-CTA03               08548014
              MOVE W-DIG-CAR-JET     TO WS-VL01-L32-DGC03               08549014
      *                                                                 08550014
              INITIALIZE                W-BGECMDC                       08551014
              MOVE W-ENT-CAR-JET     TO MDC-ENTIDAD                     08552014
              MOVE W-SUC-CAR-JET     TO MDC-CENTRO-ALTA                 08553014
              MOVE W-CUENTA-JET      TO MDC-CUENTA                      08554014
      *                                                                 08555014
              EXEC CICS                                                 08556014
                   LINK PROGRAM (BG2CMDC0)                              08557014
                   COMMAREA (BGECMDC)                                   08558014
              END-EXEC                                                  08559014
      *                                                                 08560014
              IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                      08561014
                 MOVE 'ERROR EN BGECMDC0' TO ABC-REFERENCIA             08562014
                 MOVE 'BG2CMDC0'          TO ABC-OBJETO-ERROR           08563014
                 PERFORM 999-ABEND-CICS                                 08564014
              END-IF                                                    08565014
      *                                                                 08566014
              IF MDC-CODERR = SPACES                                    08567014
                 INITIALIZE                TCWC1200                     08568014
                 MOVE MDC-CDDIVIS       TO W120-CDDIVISS                08569014
                 MOVE MDC-DIGICCC1      TO WS-VL01-L32-DGC03 (01:01)    08570014
                 MOVE MDC-DIGICCC2      TO WS-VL01-L32-DGC03 (02:01)    08571014
                 PERFORM OBTENER-MONEDA                                 08572014
                    THRU OBTENER-MONEDA-FIN                             08573014
                 MOVE W120-NBDIVIC (01) TO WS-VL01-L33-MDAUS            08574014
              ELSE                                                      08575014
                 MOVE SPACES            TO WS-VL01-L33-MDAUS            08576014
              END-IF                                                    08577014
           END-IF.                                                      08578014
      *                                                                 08579014
           PERFORM OBTENER-CUSTODIO                                     08580014
      *                                                                 08581014
      *200804248-INI                                                    08582014
           MOVE '-RVL1FM '             TO W-TS.                         08583014
           EXEC CICS                                                    08584014
                DELETEQ TS QUEUE(W-TS) NOHANDLE                         08585014
           END-EXEC.                                                    08586014
      *200804248-INI                                                    08587014
      *                                                                 08588014
           PERFORM 999999-IMPRIMIR-CONTRATO                             08589014
              THRU 999999-IMPRIMIR-CONTRATO-FIN.                        08590014
      *                                                                 08591014
           MOVE 'J'                         TO CAA-IND-PANDOC(1)        08592014
           MOVE '+DC1'                      TO CAA-DESTINO(1)           08593014
           MOVE '1'                         TO CAA-NUM-DOCUM(1)         08594014
           MOVE '00'                        TO CAA-PRILIN-DOCUM(1).     08595014
      *                                                                 08596014
       999-TRATAR-JETFORM-FIN. EXIT.                                    08597014
      *                                                                 08598014
       999-GRABAR-JETFORM.                                              08599014
           INITIALIZE R-VLWCJETF.                                       08600014
           MOVE '+DC1'               TO JETF-NOMBRE                     08601014
           MOVE CAA-TERMINAL         TO JETF-SUFIJO                     08602014
      *200702146-INI                                                    08603014
      *    MOVE WS-VL01-L02-CUENTA   TO JETF-NUMDOC                     08604014
      *    MOVE +34                  TO JETF-CQUEUE                     08605014
      *    MOVE 'CONTRATO VAL.'      TO JETF-DESCRI                     08606014
      *    MOVE 'VL011'              TO JETF-TRANSC                     08607014
           MOVE PRD0101O             TO JETF-NUMDOC (01:02).            08608014
           MOVE CTA0101O             TO JETF-NUMDOC (03:07).            08609014
           MOVE DG20101O             TO JETF-NUMDOC (10:01).            08610014
           IF ENT0101I = '0069' OR '2010'                               08611014
              MOVE +2                TO JETF-CQUEUE                     08612014
              MOVE 'CONTRATO SAB.'   TO JETF-DESCRI                     08613014
              MOVE 'VL291'           TO JETF-TRANSC                     08614014
           ELSE                                                         08615014
              MOVE +34               TO JETF-CQUEUE                     08616014
              MOVE 'CONTRATO VAL.'   TO JETF-DESCRI                     08617014
              MOVE 'VL011'           TO JETF-TRANSC                     08618014
           END-IF.                                                      08619014
      *200702146-FIN                                                    08620014
           MOVE CAA-FECHA-OPER       TO JETF-FECHA-OPER                 08621014
           MOVE CAA-HORA-TRANS       TO JETF-HORA-TRANS                 08622014
           MOVE CAA-USERID           TO JETF-USERID                     08623014
           MOVE CAA-CENTRO-CONT      TO JETF-CENTRO-CONT                08624014
           MOVE CAA-TERMINAL         TO JETF-TERMINAL                   08625014
      *                                                                 08626014
           EXEC CICS                                                    08627014
             LINK PROGRAM (VL7CJETF)                                    08628014
             COMMAREA (R-VLWCJETF)                                      08629014
           END-EXEC                                                     08630014
      *                                                                 08631014
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         08632014
              MOVE 'ERROR EN VL7CJETF'  TO ABC-REFERENCIA               08633014
              MOVE 'VL7CJETF'           TO ABC-OBJETO-ERROR             08634014
              PERFORM 999-ABEND-CICS                                    08635014
           END-IF.                                                      08636014
      *                                                                 08637014
           EVALUATE JETF-RESP                                           08638014
                 WHEN '00'                                              08639014
                      CONTINUE                                          08640014
                 WHEN OTHER                                             08641014
                     INITIALIZE   QGECABC                               08642014
                     MOVE 'VL7CJETF'             TO ABC-REFERENCIA      08643014
                     MOVE JETF-TABLENAME         TO ABC-OBJETO-ERROR    08644014
                     MOVE JETF-SQLCODE           TO SQLCODE             08645014
                     MOVE JETF-SQLERRM           TO SQLERRM             08646014
                     PERFORM 999-ABEND-DB2                              08647014
           END-EVALUATE.                                                08648014
      *                                                                 08649014
      *                                                                 08650014
       999-GRABAR-JETFORM-FIN. EXIT.                                    08651014
      *A2012-F.                                                         08652014
      *                                                                 08653014
       FORMATEO-FECHA.                                                  08654014
      *                                                                 08655014
           INITIALIZE TCWC1820-01                                       08656014
      *                                                                 08657014
           MOVE CAA-ENTIDAD                TO T1820-ENTIDAD             08658014
           MOVE CAA-CENTRO-CONT            TO T1820-CENTRO              08659014
           MOVE CAA-FECHA-OPER(1:4)        TO T1820-FECHA(1:4)          08660014
           MOVE '-'                        TO T1820-FECHA(5:1)          08661014
           MOVE CAA-FECHA-OPER(5:2)        TO T1820-FECHA(6:2)          08662014
           MOVE '-'                        TO T1820-FECHA(8:1)          08663014
           MOVE CAA-FECHA-OPER(7:2)        TO T1820-FECHA(9:2)          08664014
      *                                                                 08665014
           EXEC CICS                                                    08666014
              LINK                                                      08667014
              PROGRAM  (TC2C1820)                                       08668014
              COMMAREA (TCWC1820-01)                                    08669014
           END-EXEC                                                     08670014
      *                                                                 08671014
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         08672014
               MOVE 'ERROR LINK TC2C1820'     TO ABC-REFERENCIA         08673014
               PERFORM 999-ABEND-CICS                                   08674014
           END-IF                                                       08675014
      *                                                                 08676014
           EVALUATE T1820-CDRETORN                                      08677014
             WHEN '00'                                                  08678014
               MOVE T1820-FORMATO         TO WS-VL01-L34-LUGAR          08679014
      *                                                                 08680014
             WHEN OTHER                                                 08681014
               INITIALIZE   QGECABC                                     08682014
               MOVE 'LINK TC2C1820'        TO ABC-REFERENCIA            08683014
               PERFORM 999-ABEND-CICS                                   08684014
           END-EVALUATE.                                                08685014
      *                                                                 08686014
       FORMATEO-FECHA-FIN.                                              08687014
           EXIT.                                                        08688014
       OBTENER-CUSTODIO.                                                08689014
           MOVE VARC-CENTAD  TO VXMI-CODBE                              08690014
              EXEC SQL                                                  08691014
JPC@1 *            SELECT  *                                            08692014
                   SELECT VXMI_CODBE                                    08693014
                        , VXMI_CODCLI                                   08694014
                        , VXMI_DENOM                                    08695014
                        , VXMI_NIF                                      08696014
                        , VXMI_DOMIC                                    08697014
                        , VXMI_LOCAL                                    08698014
                        , VXMI_CODPOS                                   08699014
                        , VXMI_CNAE                                     08700014
                        , VXMI_SUCVAL                                   08701014
                        , VXMI_NUMFAC                                   08702014
                        , VXMI_VALENT                                   08703014
                        , VXMI_CTAVAL                                   08704014
                        , VXMI_VALCER                                   08705014
                        , VXMI_MULPLA                                   08706014
                        , VXMI_RETEN                                    08707014
                        , VXMI_IVA                                      08708014
                        , VXMI_INCLUS                                   08709014
                        , VXMI_EXCLUS                                   08710014
                        , VXMI_PROVIS                                   08711014
                        , VXMI_FLISOP                                   08712014
                        , VXMI_LISENT                                   08713014
                        , VXMI_LISPAG                                   08714014
                        , VXMI_INCORP                                   08715014
                        , VXMI_CONTRT                                   08716014
                        , VXMI_CONTRT6                                  08717014
                        , VXMI_REF9                                     08718014
                        , VXMI_DELEGHAC                                 08719014
                        , VXMI_ADMINHAC                                 08720014
                        , VXMI_PRETELHAC                                08721014
                        , VXMI_TELEFHAC                                 08722014
                        , VXMI_APNOMHAC                                 08723014
                        , VXMI_LUNES                                    08724014
                        , VXMI_VIERNES                                  08725014
                        , VXMI_YAPRESEN                                 08726014
                        , VXMI_IMPRE1                                   08727014
                        , VXMI_IMPRE2                                   08728014
                        , VXMI_FILLER                                   08729014
                        , VXMI_LISCTIMP                                 08730014
                        , VXMI_CONTCTA                                  08731014
                        , VXMI_PASS1                                    08732014
                        , VXMI_PASS2                                    08733014
                        , VXMI_LISCTA                                   08734014
                        , VXMI_LISAGTES                                 08735014
                        , VXMI_LISREDUC                                 08736014
                        , VXMI_LISFESTI                                 08737014
                        , VXMI_LISMONED                                 08738014
                        , VXMI_LISCONTA                                 08739014
                        , VXMI_LISENT_1                                 08740014
                        , VXMI_LISCTA_SUC                               08741014
                        , VXMI_LISVALOR                                 08742014
                        , VXMI_CONT_REV                                 08743014
                        , VXMI_VALORACION                               08744014
                        , VXMI_LIS_EXTRJ                                08745014
                        , VXMI_FILLER1                                  08746014
                        , VXMI_APCTAOFI                                 08747014
                        , VXMI_TIPCUST                                  08748014
                        , VXMI_MANFIS                                   08749014
                        , VXMI_OPECUST                                  08750014
                        , VXMI_OPEBOLSA                                 08751014
                        , VXMI_AVISOS                                   08752014
                        , VXMI_CONPANT                                  08753014
                        , VXMI_COMCUST                                  08754014
                        , VXMI_IMPALT                                   08755014
                        , VXMI_CTACARGO                                 08756014
                        , VXMI_CTAABONO                                 08757014
                        , VXMI_CONTEN                                   08758014
                        , VXMI_CONTEV                                   08759014
                        , VXMI_CONTSN                                   08760014
                        , VXMI_CONTSV                                   08761014
                        , VXMI_LIS_RESTOS                               08762014
                        , VXMI_DIAS_LIMIT                               08763014
                        , VXMI_LIS_C_EXEN                               08764014
                        , VXMI_LIS_GJUD_BLO                             08765014
                        , VXMI_FEALTREG                                 08766014
                        , VXMI_FEULMOD                                  08767014
                        , VXMI_HORULMOD                                 08768014
                        , VXMI_NUMTER                                   08769014
                        , VXMI_USUARIO                                  08770014
                        , VXMI_FILLER2                                  08771014
JPC@1 *            INTO :DCLVLDTXMI                                     08772014
                   INTO  :VXMI-CODBE                                    08773014
                      ,  :VXMI-CODCLI                                   08774014
                      ,  :VXMI-DENOM                                    08775014
                      ,  :VXMI-NIF                                      08776014
                      ,  :VXMI-DOMIC                                    08777014
                      ,  :VXMI-LOCAL                                    08778014
                      ,  :VXMI-CODPOS                                   08779014
                      ,  :VXMI-CNAE                                     08780014
                      ,  :VXMI-SUCVAL                                   08781014
                      ,  :VXMI-NUMFAC                                   08782014
                      ,  :VXMI-VALENT                                   08783014
                      ,  :VXMI-CTAVAL                                   08784014
                      ,  :VXMI-VALCER                                   08785014
                      ,  :VXMI-MULPLA                                   08786014
                      ,  :VXMI-RETEN                                    08787014
                      ,  :VXMI-IVA                                      08788014
                      ,  :VXMI-INCLUS                                   08789014
                      ,  :VXMI-EXCLUS                                   08790014
                      ,  :VXMI-PROVIS                                   08791014
                      ,  :VXMI-FLISOP                                   08792014
                      ,  :VXMI-LISENT                                   08793014
                      ,  :VXMI-LISPAG                                   08794014
                      ,  :VXMI-INCORP                                   08795014
                      ,  :VXMI-CONTRT                                   08796014
                      ,  :VXMI-CONTRT6                                  08797014
                      ,  :VXMI-REF9                                     08798014
                      ,  :VXMI-DELEGHAC                                 08799014
                      ,  :VXMI-ADMINHAC                                 08800014
                      ,  :VXMI-PRETELHAC                                08801014
                      ,  :VXMI-TELEFHAC                                 08802014
                      ,  :VXMI-APNOMHAC                                 08803014
                      ,  :VXMI-LUNES                                    08804014
                      ,  :VXMI-VIERNES                                  08805014
                      ,  :VXMI-YAPRESEN                                 08806014
                      ,  :VXMI-IMPRE1                                   08807014
                      ,  :VXMI-IMPRE2                                   08808014
                      ,  :VXMI-FILLER                                   08809014
                      ,  :VXMI-LISCTIMP                                 08810014
                      ,  :VXMI-CONTCTA                                  08811014
                      ,  :VXMI-PASS1                                    08812014
                      ,  :VXMI-PASS2                                    08813014
                      ,  :VXMI-LISCTA                                   08814014
                      ,  :VXMI-LISAGTES                                 08815014
                      ,  :VXMI-LISREDUC                                 08816014
                      ,  :VXMI-LISFESTI                                 08817014
                      ,  :VXMI-LISMONED                                 08818014
                      ,  :VXMI-LISCONTA                                 08819014
                      ,  :VXMI-LISENT-1                                 08820014
                      ,  :VXMI-LISCTA-SUC                               08821014
                      ,  :VXMI-LISVALOR                                 08822014
                      ,  :VXMI-CONT-REV                                 08823014
                      ,  :VXMI-VALORACION                               08824014
                      ,  :VXMI-LIS-EXTRJ                                08825014
                      ,  :VXMI-FILLER1                                  08826014
                      ,  :VXMI-APCTAOFI                                 08827014
                      ,  :VXMI-TIPCUST                                  08828014
                      ,  :VXMI-MANFIS                                   08829014
                      ,  :VXMI-OPECUST                                  08830014
                      ,  :VXMI-OPEBOLSA                                 08831014
                      ,  :VXMI-AVISOS                                   08832014
                      ,  :VXMI-CONPANT                                  08833014
                      ,  :VXMI-COMCUST                                  08834014
                      ,  :VXMI-IMPALT                                   08835014
                      ,  :VXMI-CTACARGO                                 08836014
                      ,  :VXMI-CTAABONO                                 08837014
                      ,  :VXMI-CONTEN                                   08838014
                      ,  :VXMI-CONTEV                                   08839014
                      ,  :VXMI-CONTSN                                   08840014
                      ,  :VXMI-CONTSV                                   08841014
                      ,  :VXMI-LIS-RESTOS                               08842014
                      ,  :VXMI-DIAS-LIMIT                               08843014
                      ,  :VXMI-LIS-C-EXEN                               08844014
                      ,  :VXMI-LIS-GJUD-BLO                             08845014
                      ,  :VXMI-FEALTREG                                 08846014
                      ,  :VXMI-FEULMOD                                  08847014
                      ,  :VXMI-HORULMOD                                 08848014
                      ,  :VXMI-NUMTER                                   08849014
                      ,  :VXMI-USUARIO                                  08850014
                      ,  :VXMI-FILLER2                                  08851014
                FROM    VLDTXMI                                         08852014
                WHERE   VXMI_CODBE  = :VXMI-CODBE                       08853014
           END-EXEC                                                     08854014
                                                                        08855014
           MOVE SQLCODE TO SQLCODE-AUX                                  08856014
                                                                        08857014
           EVALUATE TRUE                                                08858014
               WHEN DB2-OK                                              08859014
                    CONTINUE                                            08860014
               WHEN DB2-NOTFND                                          08861014
                    MOVE  'VLE1667'    TO CAA-COD-ERROR                 08862014
                    MOVE  -1           TO ENT0101L                      08863014
                    PERFORM  3-FINAL                                    08864014
               WHEN OTHER                                               08865014
                    MOVE 'SELECT-C'    TO ABC-REFERENCIA                08866014
                    MOVE 'VLDTXMI'     TO ABC-OBJETO-ERROR              08867014
                    PERFORM 999-ABEND-DB2                               08868014
           END-EVALUATE.                                                08869014
      *                                                                 08870014
           INITIALIZE                     W520-REGISTRO                 08871014
           MOVE VXMI-CODCLI            TO W520-NUMCLIEN.                08872014
      *                                                                 08873014
           EXEC CICS                                                    08874014
              LINK PROGRAM (PE2C5201)                                   08875014
              COMMAREA (W520-REGISTRO)                                  08876014
              LENGTH   (LENGTH OF W520-REGISTRO)                        08877014
           END-EXEC                                                     08878014
      *                                                                 08879014
           IF EIBRESP NOT = DFHRESP(NORMAL)                             08880014
              MOVE 'ERROR EN PE2C5201'  TO ABC-REFERENCIA               08881014
              MOVE 'PE2C5201'           TO ABC-OBJETO-ERROR             08882014
              PERFORM 999-ABEND-CICS                                    08883014
           END-IF                                                       08884014
      *                                                                 08885014
           MOVE SPACES            TO WS-VL01-L03-CUSTOD                 08886014
      *                                                                 08887014
           EVALUATE W520-PECRETOR                                       08888014
             WHEN ZEROS                                                 08889014
               CONTINUE                                                 08890014
             WHEN 10                                                    08891014
               MOVE '***CLTE CUSTODIO INEXISTENTE' TO WS-VL01-L03-CUSTOD08892014
             WHEN 99                                                    08893014
               INITIALIZE   QGECABC                                     08894014
               MOVE 'LINK PE2C5201'      TO ABC-REFERENCIA              08895014
               MOVE W520-TABLENAME       TO ABC-OBJETO-ERROR            08896014
               PERFORM 999-ABEND-DB2                                    08897014
             WHEN OTHER                                                 08898014
               MOVE  -1                  TO TIT0101L                    08899014
               MOVE 'VLE0907'            TO CAA-COD-ERROR               08900014
               MOVE 'PE2C5201'           TO CAA-VAR1-ERROR              08901014
               MOVE W520-PECRETOR        TO CAA-VAR2-ERROR              08902014
               PERFORM 3-FINAL                                          08903014
           END-EVALUATE.                                                08904014
      *                                                                 08905014
           IF  W520-PECRETOR = ZEROS                                    08906014
              IF W520-SUJGRUP = 'F'                                     08907014
                 STRING W520-NOMBRE DELIMITED BY '  ' ' '               08908014
                        W520-PRIAPE DELIMITED BY '  ' ' '               08909014
                        W520-SEGAPE DELIMITED BY '  '                   08910014
                                             INTO WS-VL01-L03-CUSTOD    08911014
              ELSE                                                      08912014
                 STRING W520-NOMBRE DELIMITED BY SIZE                   08913014
                        W520-PRIAPE DELIMITED BY SIZE                   08914014
                        W520-SEGAPE DELIMITED BY SIZE                   08915014
                                             INTO WS-VL01-L03-CUSTOD    08916014
              END-IF                                                    08917014
           END-IF.                                                      08918014
      *200702146-INI                                                    08919014
      ******************************************************************08920014
      *                    000069-IMPRIMIR-CONTRATO                    *08921014
      *   IMPRIMIMOS EL CONTRATO PARA CUSTODIA 0069 CONTINENTAL BOLSA  *08922014
      ******************************************************************08923014
       000069-IMPRIMIR-CONTRATO.                                        08924014
      *-------------------------                                        08925014
      *                                                                *08926014
           MOVE SPACES               TO W-CONTENIDO-TS.                 08927014
           MOVE 'VL291FM'            TO W-NOMBRE-FORMATO.               08928014
      *                                                                *08929014
           MOVE WS-291-LINEA-01      TO L99-LINEA.                      08930014
           PERFORM 999999-GRABAR-TS.                                    08931014
      *                                                                *08932014
           MOVE WS-291-LINEA-02      TO L99-LINEA.                      08933014
           PERFORM 999999-GRABAR-TS.                                    08934014
      *                                                                 08935014
       000069-IMPRIMIR-CONTRATO-FIN.                                    08936014
           EXIT.                                                        08937014
      *200702146-FIN                                                    08938014
      ******************************************************************08939014
      *                    999999-IMPRIMIR-CONTRATO                    *08940014
      *   IMPRIMIMOS EL CONTRATO CON TODOS LOS DATOS NECESARIOS        *08941014
      ******************************************************************08942014
       999999-IMPRIMIR-CONTRATO.                                        08943014
      *---------------------------                                      08944014
      *200702146-INI                                                   *08945014
           MOVE SPACES                     TO W-CONTENIDO-TS.           08946014
           MOVE 'VL011FM'                  TO W-NOMBRE-FORMATO.         08947014
      *200702146-FIN                                                   *08948014
           MOVE WS-CAB-LINEA-01            TO L99-LINEA                 08949014
           PERFORM 999999-GRABAR-TS.                                    08950014
      *                                                                 08951014
           MOVE WS-CAB-LINEA-02            TO L99-LINEA                 08952014
           PERFORM 999999-GRABAR-TS.                                    08953014
      *                                                                 08954014
           MOVE WS-CAB-LINEA-03            TO L99-LINEA                 08955014
           PERFORM 999999-GRABAR-TS.                                    08956014
      *                                                                 08957014
           MOVE WS-CAB-LINEA-04            TO L99-LINEA                 08958014
           PERFORM 999999-GRABAR-TS.                                    08959014
      *                                                                 08960014
           MOVE WS-CAB-LINEA-05            TO L99-LINEA                 08961014
           PERFORM 999999-GRABAR-TS.                                    08962014
      *                                                                 08963014
           MOVE WS-CAB-LINEA-06            TO L99-LINEA                 08964014
           PERFORM 999999-GRABAR-TS.                                    08965014
      *                                                                 08966014
           MOVE WS-CAB-LINEA-07            TO L99-LINEA                 08967014
           PERFORM 999999-GRABAR-TS.                                    08968014
      *                                                                 08969014
           MOVE WS-CAB-LINEA-08            TO L99-LINEA                 08970014
           PERFORM 999999-GRABAR-TS.                                    08971014
      *                                                                 08972014
           MOVE WS-CAB-LINEA-09            TO L99-LINEA                 08973014
           PERFORM 999999-GRABAR-TS.                                    08974014
      *                                                                 08975014
           MOVE WS-CAB-LINEA-10            TO L99-LINEA                 08976014
           PERFORM 999999-GRABAR-TS.                                    08977014
      *                                                                 08978014
           MOVE WS-CAB-LINEA-11            TO L99-LINEA                 08979014
           PERFORM 999999-GRABAR-TS.                                    08980014
      *                                                                 08981014
           MOVE WS-CAB-LINEA-12            TO L99-LINEA                 08982014
           PERFORM 999999-GRABAR-TS.                                    08983014
      *                                                                 08984014
           MOVE WS-CAB-LINEA-13            TO L99-LINEA                 08985014
           PERFORM 999999-GRABAR-TS.                                    08986014
      *                                                                 08987014
           MOVE WS-CAB-LINEA-14            TO L99-LINEA                 08988014
           PERFORM 999999-GRABAR-TS.                                    08989014
      *                                                                 08990014
           MOVE WS-CAB-LINEA-15            TO L99-LINEA                 08991014
           PERFORM 999999-GRABAR-TS.                                    08992014
      *                                                                 08993014
           MOVE WS-CAB-LINEA-16            TO L99-LINEA                 08994014
           PERFORM 999999-GRABAR-TS.                                    08995014
      *                                                                 08996014
           MOVE WS-CAB-LINEA-17            TO L99-LINEA                 08997014
           PERFORM 999999-GRABAR-TS.                                    08998014
      *                                                                 08999014
           MOVE WS-CAB-LINEA-18            TO L99-LINEA                 09000014
           PERFORM 999999-GRABAR-TS.                                    09001014
      *                                                                 09002014
           MOVE WS-CAB-LINEA-19            TO L99-LINEA                 09003014
           PERFORM 999999-GRABAR-TS.                                    09004014
      *                                                                 09005014
           MOVE WS-CAB-LINEA-20            TO L99-LINEA                 09006014
           PERFORM 999999-GRABAR-TS.                                    09007014
      *                                                                 09008014
           MOVE WS-CAB-LINEA-21            TO L99-LINEA                 09009014
           PERFORM 999999-GRABAR-TS.                                    09010014
      *                                                                 09011014
           MOVE WS-CAB-LINEA-22            TO L99-LINEA                 09012014
           PERFORM 999999-GRABAR-TS.                                    09013014
      *                                                                 09014014
           MOVE WS-CAB-LINEA-23            TO L99-LINEA                 09015014
           PERFORM 999999-GRABAR-TS.                                    09016014
      *                                                                 09017014
           MOVE WS-CAB-LINEA-24            TO L99-LINEA                 09018014
           PERFORM 999999-GRABAR-TS.                                    09019014
      *                                                                 09020014
           MOVE WS-CAB-LINEA-25            TO L99-LINEA                 09021014
           PERFORM 999999-GRABAR-TS.                                    09022014
      *                                                                 09023014
           MOVE WS-CAB-LINEA-26            TO L99-LINEA                 09024014
           PERFORM 999999-GRABAR-TS.                                    09025014
      *                                                                 09026014
           MOVE WS-CAB-LINEA-27            TO L99-LINEA                 09027014
           PERFORM 999999-GRABAR-TS.                                    09028014
      *                                                                 09029014
           MOVE WS-CAB-LINEA-28            TO L99-LINEA                 09030014
           PERFORM 999999-GRABAR-TS.                                    09031014
      *                                                                 09032014
           MOVE WS-CAB-LINEA-29            TO L99-LINEA                 09033014
           PERFORM 999999-GRABAR-TS.                                    09034014
      *                                                                 09035014
           MOVE WS-CAB-LINEA-30            TO L99-LINEA                 09036014
           PERFORM 999999-GRABAR-TS.                                    09037014
      *                                                                 09038014
           MOVE WS-CAB-LINEA-31            TO L99-LINEA                 09039014
           PERFORM 999999-GRABAR-TS.                                    09040014
      *                                                                 09041014
           MOVE WS-CAB-LINEA-32            TO L99-LINEA                 09042014
           PERFORM 999999-GRABAR-TS.                                    09043014
      *                                                                 09044014
           MOVE WS-CAB-LINEA-33            TO L99-LINEA                 09045014
           PERFORM 999999-GRABAR-TS.                                    09046014
      *                                                                 09047014
           MOVE WS-CAB-LINEA-34            TO L99-LINEA                 09048014
           PERFORM 999999-GRABAR-TS.                                    09049014
      *                                                                 09050014
       999999-IMPRIMIR-CONTRATO-FIN.                                    09051014
           EXIT.                                                        09052014
      ******************************************************************09053014
      *                    999999-GRABAR-TS.                           *09054014
      ******************************************************************09055014
       999999-GRABAR-TS.                                                09056014
      *                                                                 09057014
           MOVE '+DC1'                      TO W-NOMBRE-COLA            09058014
           MOVE CAA-TERMINAL                TO W-SUFIJO-TS              09059014
           MOVE LENGTH OF VLNC9999          TO W-LONG-TS                09060014
      *200702146-INI                                                    09061014
      *    MOVE SPACES                      TO W-CONTENIDO-TS           09062014
      *    MOVE 'VL011FM'                   TO W-NOMBRE-FORMATO         09063014
      *200702146-FIN                                                    09064014
           MOVE VLNC9999                    TO W-CONT-FORMATO           09065014
           ADD +8                           TO W-LONG-TS.               09066014
      *                                                                 09067014
           EXEC CICS                                                    09068014
              WRITEQ TS  QUEUE (W-TS)                                   09069014
                   FROM (W-CONTENIDO-TS)                                09070014
                 LENGTH (W-LONG-TS)                                     09071014
                   MAIN  NOHANDLE                                       09072014
           END-EXEC.                                                    09073014
      *                                                                 09074014
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         09075014
              PERFORM 999-ABEND-CICS                                    09076014
           END-IF.                                                      09077014
      *                                                                 09078014
      ******************************************************************09079014
       999999-GRABAR-TS-FIN. EXIT.                                      09080014
      *                                                                 09081014
       RUTINA-BGECMSC.                                                  09082014
      *                                                                 09083014
           MOVE END0101I                  TO MSC-SERVICIO(1:4)          09084014
           MOVE CEN0101I                  TO MSC-SERVICIO(5:4)          09085014
           MOVE DGT0101I                  TO MSC-SERVICIO(9:2)          09086014
           MOVE '91'                      TO MSC-SERVICIO(11:2)         09087014
           MOVE CTA0101I                  TO MSC-SERVICIO(13:7)         09088014
           MOVE DG20101I                  TO MSC-SERVICIO(20:1)         09089014
           MOVE '67'                      TO MSC-IND-SERVICIO           09090014
           MOVE 'N'                       TO MSC-IND-ACUMULAR           09091014
           MOVE 'S'                       TO MSC-IND-CANC               09092014
           MOVE 'A'                       TO MSC-TIPO-OPER              09093014
           MOVE CAA-ENTIDAD               TO MSC-ENTIDAD-ORI            09094014
           MOVE CAA-CENTRO-CONT           TO MSC-CENTRO-ORI             09095014
           MOVE CAA-USERID                TO MSC-USERID-ORI             09096014
      *200306088-INI CAMBIO POR ASTA PET:200502035                      09097014
      *    MOVE CAA-NETNAME-CONT          TO MSC-NETNAME-ORI            09098014
           MOVE CAA-TERMINAL-CONT         TO MSC-NETNAME-ORI            09099014
      *200306088-FIN                                                    09100014
      *                                                                 09101014
           EXEC CICS                                                    09102014
              LINK PROGRAM  (BG2CMSC0)                                  09103014
                   COMMAREA (BGECMSC)                                   09104014
           END-EXEC.                                                    09105014
      *                                                                 09106014
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         09107014
              MOVE 'ERROR EN BG2CMSC0'  TO ABC-REFERENCIA               09108014
              MOVE 'BG2CMSC0'           TO ABC-OBJETO-ERROR             09109014
              PERFORM 999-ABEND-CICS                                    09110014
           END-IF                                                       09111014
      *                                                                 09112014
           EVALUATE MSC-CODERR                                          09113014
               WHEN SPACES                                              09114014
               WHEN 'BGE0002'                                           09115014
                    CONTINUE                                            09116014
               WHEN OTHER                                               09117014
                    MOVE  -1            TO CTA0101L                     09118014
                    MOVE 'VLE0907'      TO CAA-COD-ERROR                09119014
                    MOVE 'BG2CMSC0'     TO CAA-VAR1-ERROR               09120014
                    MOVE MSC-CODERR     TO CAA-VAR2-ERROR               09121014
                    PERFORM 3-FINAL                                     09122014
           END-EVALUATE.                                                09123014
      *                                                                 09124014
       RUTINA-BGECMSC-F. EXIT.                                          09125014
      *                                                                 09126014
      *200306088-INI SE HABILITA ESTE PARRAFO FEB-2005                  09127014
       BAJA-INTERVINIENTE.                                              09128014
      ***************************************************************** 09129014
      * ACCESO A LA RUTINA PE2C6000 PARA INDICAR A ALTAMIRA EL NUEVO    09130014
      * INTERVINIENTE                                                   09131014
      *                                                                 09132014
      * PARA DAR DE BAJA EL CAMPO W600-FECHAPE DEBE IR A ESPACIOS     * 09133014
      *                                                                 09134014
      ***************************************************************** 09135014
                                                                        09136014
           INITIALIZE                   W600-REGISTRO.                  09137014
           MOVE 'B'                  TO W600-PEYOPCIO                   09138014
           MOVE SPACES               TO W600-NUMCLIEN                   09139014
           MOVE SPACES               TO W600-CLAINTER                   09140014
           MOVE SPACES               TO W600-SECINTER                   09141014
                                                                        09142014
           MOVE END0101O             TO W600-PECENTID                   09143014
           MOVE CEN0101O             TO W600-OFIAPE                     09144014
                                        W600-PENOFMOD                   09145014
           MOVE PRD0101O             TO W600-CODISER                    09146014
           MOVE CTA0101I(1:7)        TO W600-NUMECTA (1:7)              09147014
           MOVE DG20101I             TO W600-NUMECTA (8:1)              09148014
           MOVE CAA-USERID           TO W600-USUARIO                    09149014
           MOVE 'VL'                 TO W600-APLICACIO                  09150014
           MOVE SPACES               TO W600-FECHAPE                    09151014
           MOVE CAA-FECHA-OPER(1:4)  TO W600-FECANCEL(1:4)              09152014
           MOVE '-'                  TO W600-FECANCEL(5:1)              09153014
           MOVE CAA-FECHA-OPER(5:2)  TO W600-FECANCEL(6:2)              09154014
           MOVE '-'                  TO W600-FECANCEL(8:1)              09155014
           MOVE CAA-FECHA-OPER(7:2)  TO W600-FECANCEL(9:2)              09156014
                                                                        09157014
           EXEC CICS                                                    09158014
               LINK PROGRAM (PE2C6000)                                  09159014
               COMMAREA     (W-PEWC6000)                                09160014
           END-EXEC                                                     09161014
                                                                        09162014
           IF EIBRESP NOT = DFHRESP(NORMAL)                             09163014
              MOVE 'ERROR EN PE2C6000'  TO ABC-REFERENCIA               09164014
              MOVE 'PE2C6000'           TO ABC-OBJETO-ERROR             09165014
              PERFORM 999-ABEND-CICS                                    09166014
           END-IF                                                       09167014
                                                                        09168014
           EVALUATE W600-PECRETOR                                       09169014
      *200306088-INI                                                    09170014
               WHEN '60'                                                09171014
      *200306088-FIN                                                    09172014
               WHEN '00'                                                09173014
                    CONTINUE                                            09174014
               WHEN OTHER                                               09175014
                    MOVE  -1                  TO CTA0101L               09176014
                    MOVE 'VLE0907'            TO CAA-COD-ERROR          09177014
                    MOVE 'PE2C6000'           TO CAA-VAR1-ERROR         09178014
                    MOVE W600-PECRETOR        TO CAA-VAR2-ERROR         09179014
                    PERFORM 3-FINAL                                     09180014
           END-EVALUATE.                                                09181014
                                                                        09182014
       BAJA-INTERVINIENTE-F. EXIT.                                      09183014
      *200306088-FIN                                                    09184014
      *                                                                 09185014
       REVINCULAR-CTA.                                                  09186014
      *                                                                 09187014
           IF NCC0101-COMM = NC20101-COMM                               09188014
                                                                        09189014
              INITIALIZE                     BGECMSC                    09190014
                                                                        09191014
              MOVE '1'                   TO MSC-FUNCION                 09192014
              MOVE NCC0101-COMM(11:2)    TO MSC-CUENTA(1:2)             09193014
              MOVE NCC0101-COMM(13:8)    TO MSC-CUENTA(3:8)             09194014
              MOVE NCC0101-COMM(1:4)     TO MSC-ENTIDAD                 09195014
              MOVE NCC0101-COMM(5:4)     TO MSC-CENTRO-ALTA             09196014
              PERFORM RUTINA-BGECMSC                                    09197014
                 THRU RUTINA-BGECMSC-F                                  09198014
           ELSE                                                         09199014
                                                                        09200014
              INITIALIZE                     BGECMSC                    09201014
                                                                        09202014
              MOVE '1'                   TO MSC-FUNCION                 09203014
              MOVE NCC0101-COMM(11:2)    TO MSC-CUENTA(1:2)             09204014
              MOVE NCC0101-COMM(13:8)    TO MSC-CUENTA(3:8)             09205014
              MOVE NCC0101-COMM(1:4)     TO MSC-ENTIDAD                 09206014
              MOVE NCC0101-COMM(5:4)     TO MSC-CENTRO-ALTA             09207014
              PERFORM RUTINA-BGECMSC                                    09208014
                 THRU RUTINA-BGECMSC-F                                  09209014
                                                                        09210014
              INITIALIZE                     BGECMSC                    09211014
                                                                        09212014
              MOVE '1'                   TO MSC-FUNCION                 09213014
              MOVE NC20101-COMM(11:2)    TO MSC-CUENTA(1:2)             09214014
              MOVE NC20101-COMM(13:8)    TO MSC-CUENTA(3:8)             09215014
              MOVE NC20101-COMM(1:4)     TO MSC-ENTIDAD                 09216014
              MOVE NC20101-COMM(5:4)     TO MSC-CENTRO-ALTA             09217014
              PERFORM RUTINA-BGECMSC                                    09218014
                 THRU RUTINA-BGECMSC-F                                  09219014
           END-IF.                                                      09220014
      *                                                                 09221014
       REVINCULAR-CTA-FIN.   EXIT.                                      09222014
      *                                                                 09223014
       OBTENER-MONEDA.                                                  09224014
      *                                                                 09225014
           MOVE 1                 TO   W120-CDOPCION                    09226014
           CALL 'TC9C1800'    USING  TCWC1200                           09227014
                                                                        09228014
           EVALUATE W120-RETORNO                                        09229014
              WHEN '00'                                                 09230014
                   CONTINUE                                             09231014
              WHEN '20'                                                 09232014
                   MOVE  'VLE1615'            TO  CAA-COD-ERROR         09233014
                   MOVE  -1                   TO  MDA0101L              09234014
                   PERFORM  3-FINAL                                     09235014
              WHEN '99'                                                 09236014
                   MOVE -1                    TO MDA0101L               09237014
                   MOVE 'VLE0907'             TO CAA-COD-ERROR          09238014
                   MOVE 'TC9C1800'            TO CAA-VAR1-ERROR         09239014
                   MOVE W120-RETORNO          TO CAA-VAR2-ERROR         09240014
                   PERFORM 3-FINAL                                      09241014
              WHEN OTHER                                                09242014
                   MOVE -1                    TO MDA0101L               09243014
                   MOVE 'VLE0907'             TO CAA-COD-ERROR          09244014
                   MOVE 'TC9C1800'            TO CAA-VAR1-ERROR         09245014
                   MOVE W120-RETORNO          TO CAA-VAR2-ERROR         09246014
                   PERFORM 3-FINAL                                      09247014
           END-EVALUATE.                                                09248014
                                                                        09249014
       OBTENER-MONEDA-FIN.                                              09250014
           EXIT.                                                        09251014
       DESCRIPCION-OFICINA.                                             09252014
           INITIALIZE                      W030-TCWC0300                09253014
      *                                                                 09254014
           MOVE  1                      TO W030-CDOPCIO                 09255014
           MOVE CAA-ENTIDAD             TO W030-TCCENTITE               09256014
           MOVE SUC0101O                TO W030-TCCOFICIE               09257014
      *                                                                 09258014
           EXEC CICS                                                    09259014
                LINK PROGRAM (TC2C1500)                                 09260014
                COMMAREA     (W-TCWC0300)                               09261014
           END-EXEC.                                                    09262014
      *                                                                 09263014
           IF EIBRESP NOT = DFHRESP(NORMAL)                             09264014
              INITIALIZE QGECABC                                        09265014
              MOVE 'ERROR EN TC2C1500'  TO   ABC-REFERENCIA             09266014
              MOVE 'TC2C1500'           TO   ABC-OBJETO-ERROR           09267014
              PERFORM 999-ABEND-CICS                                    09268014
           END-IF                                                       09269014
      *                                                                 09270014
           EVALUATE W030-RETORN                                         09271014
               WHEN '00'                                                09272014
                    MOVE W030-TCMOFCUR  TO NOF0101O                     09273014
               WHEN '10'                                                09274014
               WHEN '30'                                                09275014
                    MOVE -1             TO CTA0101L                     09276014
                    MOVE 'VLE0688'      TO CAA-COD-ERROR                09277014
                    PERFORM 3-FINAL                                     09278014
JIPC           WHEN '20'                                                09279014
 ||                 IF OPT-COMM = 'C' AND MSB-COMM = SPACES             09280014
 ||                    MOVE '** NO EXISTE **' TO NOF0101O               09281014
 ||                 ELSE                                                09282014
 ||                    MOVE -1             TO CTA0101L                  09283014
 ||                    MOVE 'VLE0688'      TO CAA-COD-ERROR             09284014
 ||                    PERFORM 3-FINAL                                  09285014
JIPC                END-IF                                              09286014
               WHEN OTHER                                               09287014
                    MOVE -1             TO CTA0101L                     09288014
                    MOVE 'VLE0907'      TO CAA-COD-ERROR                09289014
                    MOVE 'TC2C1500'     TO CAA-VAR1-ERROR               09290014
                    MOVE W030-RETORN    TO CAA-VAR2-ERROR               09291014
                    PERFORM 3-FINAL                                     09292014
           END-EVALUATE.                                                09293014
      *                                                                 09294014
       DESCRIPCION-OFICINA-FIN.   EXIT.                                 09295014
      *                                                                 09296014
      *ACCESO-VLDTSAB.                                                  09297014
      *                                                                 09298014
      *    EXEC SQL                                                     09299014
      *         SELECT  VSAB_RUT                                        09300014
      *           INTO :VSAB-RUT                                        09301014
      *           FROM  VLDTSAB                                         09302014
      *          WHERE  VSAB_CTAVALOR  = :VSAB-CTAVALOR                 09303014
      *    END-EXEC                                                     09304014
      *                                                                 09305014
      *    MOVE SQLCODE TO SQLCODE-AUX                                  09306014
      *                                                                 09307014
      *    EVALUATE TRUE                                                09308014
      *       WHEN DB2-OK                                               09309014
      *            MOVE VSAB-RUT       TO  VARC-RUT                     09310014
      *            MOVE 'S'            TO  VARC-INDSAB                  09311014
      *                                                                 09312014
      *       WHEN  DB2-NOTFND                                          09313014
      *             MOVE  ZEROS        TO  VARC-RUT                     09314014
      *             MOVE  'N'          TO  VARC-INDSAB                  09315014
      *                                                                 09316014
      *       WHEN OTHER                                                09317014
      *            MOVE 'SELECT'       TO  ABC-REFERENCIA               09318014
      *            MOVE 'VLDTSAB'      TO  ABC-OBJETO-ERROR             09319014
      *            PERFORM 999-ABEND-DB2                                09320014
      *                                                                 09321014
      *    END-EVALUATE.                                                09322014
      *                                                                 09323014
      *A2011-RUTLOG-I                                                   09324014
      *    INITIALIZE W-VLWCLOG0                                        09325014
      *               LOGVLDTSAB                                        09326014
      *    MOVE 'VLDTSAB'             TO  VL7LOG-TABLA                  09327014
      *    MOVE 'SELECT'              TO  VL7LOG-OPERACION              09328014
      *    MOVE LENGTH OF DCLVLDTSAB  TO  VL7LOG-REGISTRO-LEN           09329014
      *    MOVE DCLVLDTSAB            TO  LOGVLDTSAB                    09330014
      *    MOVE LOGVLDTSAB            TO  VL7LOG-REGISTRO-TEXT          09331014
      *    PERFORM LLAMAR-VL7CRLOG                                      09332014
      *       THRU LLAMAR-VL7CRLOG-FIN.                                 09333014
      *A2011-RUTLOG-F                                                   09334014
      *ACCESO-VLDTSAB-FIN.    EXIT.                                     09335014
      *                                                                 09336014
       ACCESO-VLDTXBO.                                                  09337014
      *                                                                 09338014
           EXEC SQL                                                     09339014
                SELECT  VXBO_CTAECOS                                    09340014
                     ,  VXBO_CTAECOD                                    09341014
                  INTO :VXBO-CTAECOS                                    09342014
                     , :VXBO-CTAECOD                                    09343014
                  FROM  VLDTXBO                                         09344014
                 WHERE  VXBO_CLABOL = :VXBO-CLABOL                      09345014
           END-EXEC.                                                    09346014
      *                                                                 09347014
           MOVE SQLCODE TO SQLCODE-AUX                                  09348014
      *                                                                 09349014
           EVALUATE TRUE                                                09350014
               WHEN DB2-OK                                              09351014
                    CONTINUE                                            09352014
               WHEN DB2-NOTFND                                          09353014
                    MOVE -1          TO  NCC0101L                       09354014
                    MOVE 'VLE0032'   TO  CAA-COD-ERROR                  09355014
                    PERFORM 3-FINAL                                     09356014
               WHEN OTHER                                               09357014
                    MOVE 'SELECT'    TO  ABC-REFERENCIA                 09358014
                    MOVE 'VLDTXBO'   TO  ABC-OBJETO-ERROR               09359014
                    PERFORM 999-ABEND-DB2                               09360014
           END-EVALUATE.                                                09361014
      *                                                                 09362014
       ACCESO-VLDTXBO-FIN.    EXIT.                                     09363014
      *                                                                 09364014
      *RUTINA-DOMICILIOS.                                               09365014
      *                                                                 09366014
      *RUTINA-DOMICILIOS-FIN.   EXIT.                                   09367014
      *                                                                 09368014
      *DOMICILIO.                                                       09369014
      *                                                                 09370014
      *    INITIALIZE                     PEWC8235                      09371014
      *                                                                 09372014
      *    MOVE TIT0101I              TO PEWC8035-NUMCLIEN              09373014
      *    MOVE DCO0101I              TO PEWC8035-NUMDOMIC              09374014
      *                                                                 09375014
      *    EXEC CICS                                                    09376014
      *       LINK PROGRAM ('PE2C8035')                                 09377014
      *       COMMAREA (PEWC8235)                                       09378014
      *    END-EXEC                                                     09379014
      *                                                                 09380014
      *    IF EIBRESP NOT = DFHRESP(NORMAL)                             09381014
      *       MOVE 'ERROR EN PE2C8035'  TO ABC-REFERENCIA               09382014
      *       MOVE 'PE2C8035'           TO ABC-OBJETO-ERROR             09383014
      *       PERFORM 999-ABEND-CICS                                    09384014
      *    END-IF.                                                      09385014
      *                                                                 09386014
      *DOMICILIO-F. EXIT.                                               09387014
      *                                                                 09388014
      ******************************************************************09389014
      *  PARA OBTENER LA FACULTAD Y LA CONFORMIDAD CORRESPONDIENTE.     09390014
      ******************************************************************09391014
       999999-VERIFICA-FACULTADES.                                      09392014
           INITIALIZE LEWCCFA0-01.                                      09393014
                                                                        09394014
           MOVE CAA-USERID       TO WFA-USERID.                         09395014
           MOVE CAA-CODTRAN      TO WFA-CODTRA.                         09396014
           MOVE CAA-CODTRAN(1:2) TO WFA-CODSER.                         09397014
           MOVE CAA-TECLA        TO WFA-TECLA.                          09398014
           MOVE '00'             TO WFA-NROCAM.                         09399014
           MOVE CAA-FECHA-OPER   TO WFA-FECHA-OPER.                     09400014
           MOVE CAA-ENTIDAD      TO WFA-ENTIDAD.                        09401014
           MOVE CAA-CENTRO-CONT  TO WFA-CENTRO-CONT.                    09402014
                                                                        09403014
           EXEC CICS                                                    09404014
              LINK  PROGRAM (LE6CCFA0)                                  09405014
              COMMAREA    (LEWCCFA0-01)                                 09406014
           END-EXEC.                                                    09407014
                                                                        09408014
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         09409014
              MOVE  'ERROR EN LE6CCFA0'  TO  ABC-REFERENCIA             09410014
              MOVE  'LE6CCFA0'           TO  ABC-OBJETO-ERROR           09411014
              PERFORM 999-ABEND-CICS                                    09412014
           END-IF.                                                      09413014
                                                                        09414014
           EVALUATE WFA-COD-ERROR                                       09415014
               WHEN '00'                                                09416014
                   CONTINUE                                             09417014
               WHEN '99'                                                09418014
                   MOVE -1                   TO  END0101L               09419014
                   MOVE WFA-MEN-ERROR        TO  CAA-COD-ERROR          09420014
                   MOVE WFA-VAR1-ERROR       TO  CAA-VAR1-ERROR         09421014
                   MOVE WFA-VAR1-ERROR       TO  CAA-VAR2-ERROR         09422014
                   PERFORM 3-FINAL                                      09423014
               WHEN OTHER                                               09424014
                   MOVE WFA-MEN-ERROR        TO  CAA-COD-ERROR          09425014
                   MOVE WFA-VAR1-ERROR       TO  CAA-VAR1-ERROR         09426014
                   MOVE WFA-VAR1-ERROR       TO  CAA-VAR2-ERROR         09427014
                   PERFORM 3-FINAL                                      09428014
           END-EVALUATE.                                                09429014
      *A2012-I                                                          09430014
       TRATAR-IMPRESO.                                                  09431014
      *                                                                 09432014
           PERFORM ACCEDER-VLDTARC                                      09433014
              THRU ACCEDER-VLDTARC-FIN                                  09434014
           PERFORM BUSCAR-TITULAR                                       09435014
              THRU BUSCAR-TITULAR-FIN                                   09436014
           PERFORM ACCEDER-VLDTXMI                                      09437014
              THRU ACCEDER-VLDTXMI-FIN                                  09438014
           IF VARC-CENTAD = 0069 OR 2010                                09439014
              MOVE  -1                    TO NCC0101L                   09440014
              MOVE 'VLE2169'              TO CAA-COD-ERROR              09441014
              MOVE 'IMPRESION POR OPCION' TO CAA-VAR1-ERROR             09442014
              MOVE 'FICHA REGISTRO S.A.B' TO CAA-VAR2-ERROR             09443014
              PERFORM 3-FINAL                                           09444014
           END-IF                                                       09445014
           IF VXMI-IMPALT NOT = 'N'                                     09446014
              IF VARC-INDIMP NOT = 'S'                                  09447014
                 IF VARC-VALEXTRJ = 'J' OR 'F' OR 'M' OR 'B' OR         09448014
                   (VARC-VALEXTRJ = 'E' AND SW-PERSONA = 'J')           09449014
      *** COMPROBAMOS SI TIENE REPRESENTANTES ADMIN. CARTERA - SAB      09450014
                    PERFORM ACCEDER-VLDTADT-2                           09451014
                       THRU ACCEDER-VLDTADT-2-FIN                       09452014
                 END-IF                                                 09453014
                 PERFORM ACCEDER-VLDTADT                                09454014
                    THRU ACCEDER-VLDTADT-FIN                            09455014
      *@ZAL-INI                                                         09456014
      *          IF VARC-NUMMAN = W-COUNT                               09457014
                 IF VARC-GRUPO-CTAS = W-COUNT                           09458014
      *@ZAL-FIN                                                         09459014
      *200702146-INI                                                    09460014
      *              PERFORM 999-TRATAR-JETFORM                         09461014
      *                 THRU 999-TRATAR-JETFORM-FIN                     09462014
                     IF ENT0101I = '0069' OR '2010'                     09463014
                        PERFORM 069-TRATAR-JETFORM                      09464014
                           THRU 069-TRATAR-JETFORM-FIN                  09465014
                     ELSE                                               09466014
                        PERFORM 999-TRATAR-JETFORM                      09467014
                           THRU 999-TRATAR-JETFORM-FIN                  09468014
                     END-IF                                             09469014
      *200702146-FIN                                                    09470014
                     PERFORM 999-GRABAR-JETFORM                         09471014
                        THRU 999-GRABAR-JETFORM-FIN                     09472014
                     PERFORM 223-RELACION-PRODUCTO                      09473014
                        THRU 223-RELACION-PRODUCTO-FIN                  09474014
                     PERFORM ACTUALIZAR-VLDTARC                         09475014
                        THRU ACTUALIZAR-VLDTARC-FIN                     09476014
                     MOVE 'VLA0068'         TO CAA-COD-AVISO1           09477014
                     MOVE -1                TO CTA0101L                 09478014
                 ELSE                                                   09479014
                     MOVE 'VLE1648' TO CAA-COD-ERROR                    09480014
                     MOVE -1        TO CTA0101L                         09481014
                     PERFORM 3-FINAL                                    09482014
                 END-IF                                                 09483014
              ELSE                                                      09484014
                MOVE 'VLE1647' TO CAA-COD-ERROR                         09485014
                MOVE -1        TO CTA0101L                              09486014
                PERFORM 3-FINAL                                         09487014
              END-IF                                                    09488014
           ELSE                                                         09489014
              MOVE 'VLA0072'         TO CAA-COD-AVISO1                  09490014
              MOVE -1                TO CTA0101L                        09491014
           END-IF.                                                      09492014
       TRATAR-IMPRESO-FIN. EXIT.                                        09493014
      *                                                                 09494014
       ACCEDER-VLDTADT.                                                 09495014
      *                                                                 09496014
           MOVE CTA0101I                    TO W-CUENTA                 09497014
           MOVE W-CUENTA                    TO VADT-CUENTA              09498014
           MOVE '4'                         TO VADT-CLTITU              09499014
      *                                                                 09500014
           EXEC SQL                                                     09501014
                SELECT  COUNT(*)                                        09502014
                  INTO :W-COUNT                                         09503014
                  FROM  VLDTADT                                         09504014
                 WHERE  VADT_CUENTA  = :VADT-CUENTA                     09505014
                  AND   VADT_CLTITU  = :VADT-CLTITU                     09506014
           END-EXEC                                                     09507014
      *                                                                 09508014
           MOVE SQLCODE TO SQLCODE-AUX                                  09509014
      *                                                                 09510014
           EVALUATE TRUE                                                09511014
              WHEN DB2-OK                                               09512014
                   CONTINUE                                             09513014
      *                                                                 09514014
              WHEN OTHER                                                09515014
                   MOVE 'SELECT-COUNT'      TO  ABC-REFERENCIA          09516014
                   MOVE 'VLDTADT'     TO  ABC-OBJETO-ERROR              09517014
                   PERFORM 999-ABEND-DB2                                09518014
      *                                                                 09519014
           END-EVALUATE.                                                09520014
      *                                                                 09521014
       ACCEDER-VLDTADT-FIN.                                             09522014
           EXIT.                                                        09523014
      *                                                                 09524014
       ACCEDER-VLDTADT-2.                                               09525014
      *                                                                 09526014
           MOVE CTA0101I                    TO W-CUENTA                 09527014
           MOVE W-CUENTA                    TO VADT-CUENTA              09528014
           MOVE '2'                         TO VADT-CLTITU              09529014
           MOVE 'S'                         TO VADT-ADMIN               09530014
      *                                                                 09531014
           EXEC SQL                                                     09532014
                SELECT  COUNT(*)                                        09533014
                  INTO :W-COUNT                                         09534014
                  FROM  VLDTADT                                         09535014
                 WHERE  VADT_CUENTA  = :VADT-CUENTA                     09536014
                  AND   VADT_CLTITU  = :VADT-CLTITU                     09537014
                  AND   VADT_ADMIN  <> :VADT-ADMIN                      09538014
           END-EXEC                                                     09539014
      *                                                                 09540014
           MOVE SQLCODE TO SQLCODE-AUX                                  09541014
      *                                                                 09542014
           EVALUATE TRUE                                                09543014
              WHEN DB2-OK                                               09544014
                   IF W-COUNT NOT > 0                                   09545014
                      MOVE 'VLE1735'   TO CAA-COD-ERROR                 09546014
                      MOVE -1          TO CTA0101L                      09547014
                      PERFORM 3-FINAL                                   09548014
                   END-IF                                               09549014
                   MOVE ZEROES         TO W-COUNT                       09550014
      *                                                                 09551014
              WHEN OTHER                                                09552014
                   MOVE 'SELECT-COUNT' TO  ABC-REFERENCIA               09553014
                   MOVE 'VLDTADT'      TO  ABC-OBJETO-ERROR             09554014
                   PERFORM 999-ABEND-DB2                                09555014
      *                                                                 09556014
           END-EVALUATE.                                                09557014
      *                                                                 09558014
       ACCEDER-VLDTADT-2-FIN.                                           09559014
           EXIT.                                                        09560014
      *                                                                 09561014
       ACTUALIZAR-VLDTARC.                                              09562014
      *                                                                 09563014
      *A2011-RUTLOG-I                                                   09564014
           MOVE VARC-CUENTA       TO LARC-CUENTA                        09565014
           PERFORM SELUND-VLDTARC                                       09566014
              THRU SELUND-VLDTARC-FIN                                   09567014
      *A2011-RUTLOG-F                                                   09568014
           MOVE 'S'               TO  VARC-INDIMP                       09569014
           IF (VARC-GRUPO    = 2 OR 4) AND                              09570014
              (VARC-INVERSOR = 51    ) AND                              09571014
      *200509007-INI                                                    09572014
              (VARC-FILLER (11:02) NOT = '16')                          09573014
      *200509007-FIN                                                    09574014
              MOVE 1              TO  VARC-INVERSOR                     09575014
           END-IF                                                       09576014
      *                                                                 09577014
           MOVE CAA-FECHA-OPER    TO  VARC-FEULMOD                      09578014
           MOVE CAA-HORA-TRANS    TO  VARC-HORULMOD                     09579014
           MOVE CAA-TERMINAL      TO  VARC-NUMTER                       09580014
           MOVE CAA-USERID        TO  VARC-USUARIO                      09581014
      *                                                                 09582014
           EXEC SQL                                                     09583014
                UPDATE  VLDTARC                                         09584014
                   SET  VARC_INDIMP    = :VARC-INDIMP                   09585014
                     ,  VARC_INVERSOR  = :VARC-INVERSOR                 09586014
                     ,  VARC_FEULMOD   = :VARC-FEULMOD                  09587014
                     ,  VARC_HORULMOD  = :VARC-HORULMOD                 09588014
                     ,  VARC_NUMTER    = :VARC-NUMTER                   09589014
                     ,  VARC_USUARIO   = :VARC-USUARIO                  09590014
                 WHERE  VARC_CUENTA    = :VARC-CUENTA                   09591014
           END-EXEC.                                                    09592014
      *                                                                 09593014
           MOVE SQLCODE TO SQLCODE-AUX                                  09594014
      *                                                                 09595014
           EVALUATE TRUE                                                09596014
              WHEN DB2-OK                                               09597014
                   CONTINUE                                             09598014
                                                                        09599014
              WHEN OTHER                                                09600014
                   MOVE 'UPDATE-ALTA2' TO  ABC-REFERENCIA               09601014
                   MOVE 'VLDTARC'      TO  ABC-OBJETO-ERROR             09602014
                   PERFORM 999-ABEND-DB2                                09603014
                                                                        09604014
           END-EVALUATE.                                                09605014
      *                                                                 09606014
           INITIALIZE W-VLWCLOG0                                        09607014
           INITIALIZE LOGVLDTARC                                        09608014
           MOVE 'VLDTARC'             TO  VL7LOG-TABLA                  09609014
           MOVE 'UPDATE'              TO  VL7LOG-OPERACION              09610014
           MOVE LENGTH OF DCLVLDTARC  TO  VL7LOG-REGISTRO-LEN           09611014
           MOVE VARC-INDIMP           TO  LARC-INDIMP                   09612014
           MOVE VARC-FEULMOD          TO  LARC-FEULMOD                  09613014
           MOVE VARC-HORULMOD         TO  LARC-HORULMOD                 09614014
           MOVE VARC-NUMTER           TO  LARC-NUMTER                   09615014
           MOVE VARC-USUARIO          TO  LARC-USUARIO                  09616014
           MOVE VARC-CUENTA           TO  LARC-CUENTA                   09617014
           MOVE LOGVLDTARC            TO  VL7LOG-REGISTRO-TEXT          09618014
           PERFORM LLAMAR-VL7CRLOG                                      09619014
              THRU LLAMAR-VL7CRLOG-FIN.                                 09620014
      *A2011-RUTLOG-F                                                   09621014
                                                                        09622014
       ACTUALIZAR-VLDTARC-FIN.                                          09623014
            EXIT.                                                       09624014
      *                                                                 09625014
       ACCEDER-VLDTARC.                                                 09626014
      *                                                                 09627014
           MOVE CTA0101-COMM-N              TO VARC-CUENTA              09628014
      *                                                                 09629014
           EXEC SQL                                                     09630014
                SELECT  VARC_CUENTA                                     09631014
                     ,  VARC_CENTAD                                     09632014
                     ,  VARC_NUMCLI                                     09633014
                     ,  VARC_CLMAST                                     09634014
                     ,  VARC_MONEDA                                     09635014
                     ,  VARC_SUCURS                                     09636014
                     ,  VARC_CTACAR                                     09637014
                     ,  VARC_CTAABO                                     09638014
                     ,  VARC_TEXTO                                      09639014
                     ,  VARC_PRESEN                                     09640014
                     ,  VARC_GRUPO                                      09641014
                     ,  VARC_RUT                                        09642014
                     ,  VARC_CNAE                                       09643014
                     ,  VARC_SITUAC                                     09644014
                     ,  VARC_EXEN1                                      09645014
                     ,  VARC_EXEN2                                      09646014
                     ,  VARC_EXEN3                                      09647014
                     ,  VARC_EXEN4                                      09648014
                     ,  VARC_EXEN5                                      09649014
                     ,  VARC_EXEN6                                      09650014
                     ,  VARC_EXEN7                                      09651014
                     ,  VARC_EXEN8                                      09652014
                     ,  VARC_EXEN9                                      09653014
                     ,  VARC_EXEN10                                     09654014
                     ,  VARC_ANALIS                                     09655014
                     ,  VARC_CLACARGO                                   09656014
                     ,  VARC_CLABONO                                    09657014
                     ,  VARC_NUMDOM                                     09658014
                     ,  VARC_CODSUS                                     09659014
                     ,  VARC_FE_ULT_EXT                                 09660014
                     ,  VARC_PAIS                                       09661014
                     ,  VARC_FE_CARTERA                                 09662014
                     ,  VARC_CLTELEX                                    09663014
                     ,  VARC_FE_ALTA                                    09664014
                     ,  VARC_VALORACION                                 09665014
                     ,  VARC_VALEXTRJ                                   09666014
                     ,  VARC_INVERSOR                                   09667014
                     ,  VARC_DIRECTA                                    09668014
                     ,  VARC_MAX_CVE_1                                  09669014
                     ,  VARC_MAX_DCU_5                                  09670014
                     ,  VARC_MAX_SUS_6                                  09671014
                     ,  VARC_MAX_DIV_7                                  09672014
                     ,  VARC_MAX_AMO_8                                  09673014
                     ,  VARC_MAX_PAJ_9                                  09674014
                     ,  VARC_FECHA_102                                  09675014
                     ,  VARC_TARIFACUS                                  09676014
                     ,  VARC_SWIFT_TELEX                                09677014
                     ,  VARC_TELEX_2                                    09678014
                     ,  VARC_GRUPO_CTAS                                 09679014
                     ,  VARC_OPER_TIT                                   09680014
                     ,  VARC_FEALTREG                                   09681014
                     ,  VARC_FEULMOD                                    09682014
                     ,  VARC_HORULMOD                                   09683014
                     ,  VARC_NUMTER                                     09684014
                     ,  VARC_USUARIO                                    09685014
                     ,  VARC_FILLER                                     09686014
                     ,  VARC_CTAVAL20                                   09687014
      *@ZAL-INI                                                         09688014
      *              ,  VARC_NUMMAN                                     09689014
                     ,  VARC_GRUPO_CTAS                                 09690014
      *@ZAL-FIN                                                         09691014
                     ,  VARC_INDIMP                                     09692014
                     ,  VARC_INDSAB                                     09693014
                  INTO :VARC-CUENTA                                     09694014
                     , :VARC-CENTAD                                     09695014
                     , :VARC-NUMCLI                                     09696014
                     , :VARC-CLMAST                                     09697014
                     , :VARC-MONEDA                                     09698014
                     , :VARC-SUCURS                                     09699014
                     , :VARC-CTACAR                                     09700014
                     , :VARC-CTAABO                                     09701014
                     , :VARC-TEXTO                                      09702014
                     , :VARC-PRESEN                                     09703014
                     , :VARC-GRUPO                                      09704014
                     , :VARC-RUT                                        09705014
                     , :VARC-CNAE                                       09706014
                     , :VARC-SITUAC                                     09707014
                     , :VARC-EXEN1                                      09708014
                     , :VARC-EXEN2                                      09709014
                     , :VARC-EXEN3                                      09710014
                     , :VARC-EXEN4                                      09711014
                     , :VARC-EXEN5                                      09712014
                     , :VARC-EXEN6                                      09713014
                     , :VARC-EXEN7                                      09714014
                     , :VARC-EXEN8                                      09715014
                     , :VARC-EXEN9                                      09716014
                     , :VARC-EXEN10                                     09717014
                     , :VARC-ANALIS                                     09718014
                     , :VARC-CLACARGO                                   09719014
                     , :VARC-CLABONO                                    09720014
                     , :VARC-NUMDOM                                     09721014
                     , :VARC-CODSUS                                     09722014
                     , :VARC-FE-ULT-EXT                                 09723014
                     , :VARC-PAIS                                       09724014
                     , :VARC-FE-CARTERA                                 09725014
                     , :VARC-CLTELEX                                    09726014
                     , :VARC-FE-ALTA                                    09727014
                     , :VARC-VALORACION                                 09728014
                     , :VARC-VALEXTRJ                                   09729014
                     , :VARC-INVERSOR                                   09730014
                     , :VARC-DIRECTA                                    09731014
                     , :VARC-MAX-CVE-1                                  09732014
                     , :VARC-MAX-DCU-5                                  09733014
                     , :VARC-MAX-SUS-6                                  09734014
                     , :VARC-MAX-DIV-7                                  09735014
                     , :VARC-MAX-AMO-8                                  09736014
                     , :VARC-MAX-PAJ-9                                  09737014
                     , :VARC-FECHA-102                                  09738014
                     , :VARC-TARIFACUS                                  09739014
                     , :VARC-SWIFT-TELEX                                09740014
                     , :VARC-TELEX-2                                    09741014
                     , :VARC-GRUPO-CTAS                                 09742014
                     , :VARC-OPER-TIT                                   09743014
                     , :VARC-FEALTREG                                   09744014
                     , :VARC-FEULMOD                                    09745014
                     , :VARC-HORULMOD                                   09746014
                     , :VARC-NUMTER                                     09747014
                     , :VARC-USUARIO                                    09748014
                     , :VARC-FILLER                                     09749014
                     , :VARC-CTAVAL20                                   09750014
      *@ZAL-INI                                                         09751014
      *              , :VARC-NUMMAN                                     09752014
                     , :VARC-GRUPO-CTAS                                 09753014
      *@ZAL-FIN                                                         09754014
                     , :VARC-INDIMP                                     09755014
                     , :VARC-INDSAB                                     09756014
                  FROM  VLDTARC                                         09757014
                 WHERE  VARC_CUENTA  = :VARC-CUENTA                     09758014
           END-EXEC                                                     09759014
      *                                                                 09760014
           MOVE SQLCODE TO SQLCODE-AUX                                  09761014
      *                                                                 09762014
           EVALUATE TRUE                                                09763014
              WHEN DB2-OK                                               09764014
                   IF VARC-SITUAC = 'X' OR 'B'                          09765014
                      MOVE 'VLE1945'  TO CAA-COD-ERROR                  09766014
                      MOVE -1         TO CTA0101L                       09767014
                      PERFORM 3-FINAL                                   09768014
                   END-IF                                               09769014
                                                                        09770014
                   IF VARC-GRUPO  = 1 OR 3                              09771014
                      MOVE 'VLE2095'  TO CAA-COD-ERROR                  09772014
                      MOVE -1         TO CTA0101L                       09773014
                      PERFORM 3-FINAL                                   09774014
                   END-IF                                               09775014
      *                                                                 09776014
              WHEN OTHER                                                09777014
                   MOVE 'SELECT'      TO  ABC-REFERENCIA                09778014
                   MOVE 'VLDTARC'     TO  ABC-OBJETO-ERROR              09779014
                   PERFORM 999-ABEND-DB2                                09780014
      *                                                                 09781014
           END-EVALUATE.                                                09782014
      *                                                                 09783014
      *A2011-RUTLOG-I                                                   09784014
           INITIALIZE W-VLWCLOG0                                        09785014
                      LOGVLDTARC                                        09786014
           MOVE 'VLDTARC'             TO  VL7LOG-TABLA                  09787014
           MOVE 'SELECT'              TO  VL7LOG-OPERACION              09788014
           MOVE LENGTH OF DCLVLDTARC  TO  VL7LOG-REGISTRO-LEN           09789014
           MOVE DCLVLDTARC            TO  LOGVLDTARC                    09790014
           MOVE LOGVLDTARC            TO  VL7LOG-REGISTRO-TEXT          09791014
           PERFORM LLAMAR-VL7CRLOG                                      09792014
              THRU LLAMAR-VL7CRLOG-FIN.                                 09793014
      *A2011-RUTLOG-F                                                   09794014
                                                                        09795014
       ACCEDER-VLDTARC-FIN.                                             09796014
             EXIT.                                                      09797014
      *                                                                 09798014
       ACCEDER-VLDTXMI.                                                 09799014
      *                                                                 09800014
           MOVE VARC-CENTAD                 TO VXMI-CODBE               09801014
      *                                                                 09802014
JPC@1 *          EXEC SQL                                               09803014
JPC@1 *             SELECT  *                                           09804014
JPC@1 *              INTO :DCLVLDTXMI                                   09805014
JPC@1 *              FROM  VLDTXMI                                      09806014
JPC@1 *             WHERE  VXMI_CODBE  = :VXMI-CODBE                    09807014
JPC@1 *          END-EXEC                                               09808014
                                                                        09809014
              EXEC SQL                                                  09810014
                   SELECT VXMI_CODBE                                    09811014
                        , VXMI_CODCLI                                   09812014
                        , VXMI_DENOM                                    09813014
                        , VXMI_NIF                                      09814014
                        , VXMI_DOMIC                                    09815014
                        , VXMI_LOCAL                                    09816014
                        , VXMI_CODPOS                                   09817014
                        , VXMI_CNAE                                     09818014
                        , VXMI_SUCVAL                                   09819014
                        , VXMI_NUMFAC                                   09820014
                        , VXMI_VALENT                                   09821014
                        , VXMI_CTAVAL                                   09822014
                        , VXMI_VALCER                                   09823014
                        , VXMI_MULPLA                                   09824014
                        , VXMI_RETEN                                    09825014
                        , VXMI_IVA                                      09826014
                        , VXMI_INCLUS                                   09827014
                        , VXMI_EXCLUS                                   09828014
                        , VXMI_PROVIS                                   09829014
                        , VXMI_FLISOP                                   09830014
                        , VXMI_LISENT                                   09831014
                        , VXMI_LISPAG                                   09832014
                        , VXMI_INCORP                                   09833014
                        , VXMI_CONTRT                                   09834014
                        , VXMI_CONTRT6                                  09835014
                        , VXMI_REF9                                     09836014
                        , VXMI_DELEGHAC                                 09837014
                        , VXMI_ADMINHAC                                 09838014
                        , VXMI_PRETELHAC                                09839014
                        , VXMI_TELEFHAC                                 09840014
                        , VXMI_APNOMHAC                                 09841014
                        , VXMI_LUNES                                    09842014
                        , VXMI_VIERNES                                  09843014
                        , VXMI_YAPRESEN                                 09844014
                        , VXMI_IMPRE1                                   09845014
                        , VXMI_IMPRE2                                   09846014
                        , VXMI_FILLER                                   09847014
                        , VXMI_LISCTIMP                                 09848014
                        , VXMI_CONTCTA                                  09849014
                        , VXMI_PASS1                                    09850014
                        , VXMI_PASS2                                    09851014
                        , VXMI_LISCTA                                   09852014
                        , VXMI_LISAGTES                                 09853014
                        , VXMI_LISREDUC                                 09854014
                        , VXMI_LISFESTI                                 09855014
                        , VXMI_LISMONED                                 09856014
                        , VXMI_LISCONTA                                 09857014
                        , VXMI_LISENT_1                                 09858014
                        , VXMI_LISCTA_SUC                               09859014
                        , VXMI_LISVALOR                                 09860014
                        , VXMI_CONT_REV                                 09861014
                        , VXMI_VALORACION                               09862014
                        , VXMI_LIS_EXTRJ                                09863014
                        , VXMI_FILLER1                                  09864014
                        , VXMI_APCTAOFI                                 09865014
                        , VXMI_TIPCUST                                  09866014
                        , VXMI_MANFIS                                   09867014
                        , VXMI_OPECUST                                  09868014
                        , VXMI_OPEBOLSA                                 09869014
                        , VXMI_AVISOS                                   09870014
                        , VXMI_CONPANT                                  09871014
                        , VXMI_COMCUST                                  09872014
                        , VXMI_IMPALT                                   09873014
                        , VXMI_CTACARGO                                 09874014
                        , VXMI_CTAABONO                                 09875014
                        , VXMI_CONTEN                                   09876014
                        , VXMI_CONTEV                                   09877014
                        , VXMI_CONTSN                                   09878014
                        , VXMI_CONTSV                                   09879014
                        , VXMI_LIS_RESTOS                               09880014
                        , VXMI_DIAS_LIMIT                               09881014
                        , VXMI_LIS_C_EXEN                               09882014
                        , VXMI_LIS_GJUD_BLO                             09883014
                        , VXMI_FEALTREG                                 09884014
                        , VXMI_FEULMOD                                  09885014
                        , VXMI_HORULMOD                                 09886014
                        , VXMI_NUMTER                                   09887014
                        , VXMI_USUARIO                                  09888014
                        , VXMI_FILLER2                                  09889014
                   INTO  :VXMI-CODBE                                    09890014
                      ,  :VXMI-CODCLI                                   09891014
                      ,  :VXMI-DENOM                                    09892014
                      ,  :VXMI-NIF                                      09893014
                      ,  :VXMI-DOMIC                                    09894014
                      ,  :VXMI-LOCAL                                    09895014
                      ,  :VXMI-CODPOS                                   09896014
                      ,  :VXMI-CNAE                                     09897014
                      ,  :VXMI-SUCVAL                                   09898014
                      ,  :VXMI-NUMFAC                                   09899014
                      ,  :VXMI-VALENT                                   09900014
                      ,  :VXMI-CTAVAL                                   09901014
                      ,  :VXMI-VALCER                                   09902014
                      ,  :VXMI-MULPLA                                   09903014
                      ,  :VXMI-RETEN                                    09904014
                      ,  :VXMI-IVA                                      09905014
                      ,  :VXMI-INCLUS                                   09906014
                      ,  :VXMI-EXCLUS                                   09907014
                      ,  :VXMI-PROVIS                                   09908014
                      ,  :VXMI-FLISOP                                   09909014
                      ,  :VXMI-LISENT                                   09910014
                      ,  :VXMI-LISPAG                                   09911014
                      ,  :VXMI-INCORP                                   09912014
                      ,  :VXMI-CONTRT                                   09913014
                      ,  :VXMI-CONTRT6                                  09914014
                      ,  :VXMI-REF9                                     09915014
                      ,  :VXMI-DELEGHAC                                 09916014
                      ,  :VXMI-ADMINHAC                                 09917014
                      ,  :VXMI-PRETELHAC                                09918014
                      ,  :VXMI-TELEFHAC                                 09919014
                      ,  :VXMI-APNOMHAC                                 09920014
                      ,  :VXMI-LUNES                                    09921014
                      ,  :VXMI-VIERNES                                  09922014
                      ,  :VXMI-YAPRESEN                                 09923014
                      ,  :VXMI-IMPRE1                                   09924014
                      ,  :VXMI-IMPRE2                                   09925014
                      ,  :VXMI-FILLER                                   09926014
                      ,  :VXMI-LISCTIMP                                 09927014
                      ,  :VXMI-CONTCTA                                  09928014
                      ,  :VXMI-PASS1                                    09929014
                      ,  :VXMI-PASS2                                    09930014
                      ,  :VXMI-LISCTA                                   09931014
                      ,  :VXMI-LISAGTES                                 09932014
                      ,  :VXMI-LISREDUC                                 09933014
                      ,  :VXMI-LISFESTI                                 09934014
                      ,  :VXMI-LISMONED                                 09935014
                      ,  :VXMI-LISCONTA                                 09936014
                      ,  :VXMI-LISENT-1                                 09937014
                      ,  :VXMI-LISCTA-SUC                               09938014
                      ,  :VXMI-LISVALOR                                 09939014
                      ,  :VXMI-CONT-REV                                 09940014
                      ,  :VXMI-VALORACION                               09941014
                      ,  :VXMI-LIS-EXTRJ                                09942014
                      ,  :VXMI-FILLER1                                  09943014
                      ,  :VXMI-APCTAOFI                                 09944014
                      ,  :VXMI-TIPCUST                                  09945014
                      ,  :VXMI-MANFIS                                   09946014
                      ,  :VXMI-OPECUST                                  09947014
                      ,  :VXMI-OPEBOLSA                                 09948014
                      ,  :VXMI-AVISOS                                   09949014
                      ,  :VXMI-CONPANT                                  09950014
                      ,  :VXMI-COMCUST                                  09951014
                      ,  :VXMI-IMPALT                                   09952014
                      ,  :VXMI-CTACARGO                                 09953014
                      ,  :VXMI-CTAABONO                                 09954014
                      ,  :VXMI-CONTEN                                   09955014
                      ,  :VXMI-CONTEV                                   09956014
                      ,  :VXMI-CONTSN                                   09957014
                      ,  :VXMI-CONTSV                                   09958014
                      ,  :VXMI-LIS-RESTOS                               09959014
                      ,  :VXMI-DIAS-LIMIT                               09960014
                      ,  :VXMI-LIS-C-EXEN                               09961014
                      ,  :VXMI-LIS-GJUD-BLO                             09962014
                      ,  :VXMI-FEALTREG                                 09963014
                      ,  :VXMI-FEULMOD                                  09964014
                      ,  :VXMI-HORULMOD                                 09965014
                      ,  :VXMI-NUMTER                                   09966014
                      ,  :VXMI-USUARIO                                  09967014
                      ,  :VXMI-FILLER2                                  09968014
                   FROM  VLDTXMI                                        09969014
                   WHERE  VXMI_CODBE  = :VXMI-CODBE                     09970014
              END-EXEC                                                  09971014
                                                                        09972014
                 MOVE SQLCODE TO SQLCODE-AUX                            09973014
                                                                        09974014
                 EVALUATE TRUE                                          09975014
                    WHEN DB2-OK                                         09976014
                       CONTINUE                                         09977014
                                                                        09978014
                    WHEN  DB2-NOTFND                                    09979014
                      MOVE  'VLE0007'   TO  CAA-COD-ERROR               09980014
                      MOVE  -1          TO  CTA0101L                    09981014
                      PERFORM  3-FINAL                                  09982014
                                                                        09983014
                    WHEN OTHER                                          09984014
                      MOVE 'SELECT'      TO  ABC-REFERENCIA             09985014
                      MOVE 'VLDTXMI'     TO  ABC-OBJETO-ERROR           09986014
                      PERFORM 999-ABEND-DB2                             09987014
                                                                        09988014
                 END-EVALUATE.                                          09989014
      *                                                                 09990014
      *A2011-RUTLOG-I                                                   09991014
           INITIALIZE W-VLWCLOG0                                        09992014
                      LOGVLDTXMI                                        09993014
           MOVE 'VLDTXMI'             TO  VL7LOG-TABLA                  09994014
           MOVE 'SELECT'              TO  VL7LOG-OPERACION              09995014
           MOVE LENGTH OF DCLVLDTXMI  TO  VL7LOG-REGISTRO-LEN           09996014
           MOVE DCLVLDTXMI            TO  LOGVLDTXMI                    09997014
           MOVE LOGVLDTXMI            TO  VL7LOG-REGISTRO-TEXT          09998014
           PERFORM LLAMAR-VL7CRLOG                                      09999014
              THRU LLAMAR-VL7CRLOG-FIN.                                 10000014
      *A2011-RUTLOG-I                                                   10010014
                                                                        10020014
       ACCEDER-VLDTXMI-FIN.                                             10030014
               EXIT.                                                    10040014
      *                                                                 10050014
       ACCEDER-VLDTADS1.                                                10060014
      *                                                                 10070014
           MOVE VARC-CUENTA        TO VADS-CUENTA                       10080014
           MOVE SPACES             TO VADS-PAVAL                        10090014
           MOVE SPACES             TO VADS-VALOR                        10100014
           MOVE 0                  TO VADS-ISIN                         10110014
           MOVE 'M'                TO VADS-TIPREG                       10120014
           MOVE SPACES             TO VADS-NUMGRUN                      10130014
      *                                                                 10140014
           EXEC SQL                                                     10150014
                SELECT   COUNT(*)                                       10160014
                  INTO  :W-COUNT1                                       10170014
                  FROM   VLDTADS                                        10180014
                 WHERE VADS_CUENTA  = :VADS-CUENTA                      10190014
                   AND VADS_PAVAL   > :VADS-PAVAL                       10200014
                   AND VADS_VALOR   > :VADS-VALOR                       10210014
                   AND VADS_ISIN    > :VADS-ISIN                        10220014
                   AND VADS_TIPREG  = :VADS-TIPREG                      10230014
                   AND VADS_NUMGRUN = :VADS-NUMGRUN                     10240014
           END-EXEC                                                     10250014
      *                                                                 10260014
           MOVE SQLCODE TO SQLCODE-AUX                                  10270014
      *                                                                 10280014
           EVALUATE TRUE                                                10290014
                    WHEN DB2-OK                                         10300014
                        CONTINUE                                        10310014
                    WHEN OTHER                                          10320014
                        MOVE 'SELECT-COUNT'   TO  ABC-REFERENCIA        10330014
                        MOVE 'VLDTADS'        TO  ABC-OBJETO-ERROR      10340014
                       PERFORM 999-ABEND-DB2                            10350014
           END-EVALUATE.                                                10360014
      *                                                                 10370014
                                                                        10380014
       ACCEDER-VLDTADS1-FIN.                                            10390014
           EXIT.                                                        10400014
      *A2012-F.                                                         10410014
      *                                                                 10420014
      *A2011-I-RUTLOG-I                                                 10430014
      *       PARRAFOS NUEVOS POR MOTIVO DE LA RUTINA VL7CRLOG          10440014
      *                                                                 10450014
       LLAMAR-VL7CRLOG.                                                 10460014
                                                                        10470014
           MOVE CAA-FECHA-OPER        TO  VL7LOG-FECHA                  10480014
           MOVE CAA-HORA-TRANS        TO  VL7LOG-HORA                   10490014
           MOVE CAA-TERMINAL          TO  VL7LOG-NUMTER                 10500014
           MOVE CAA-USERID            TO  VL7LOG-NUMUSER.               10510014
           MOVE 'VL2C1010'            TO  VL7LOG-CODTRAN.               10520014
                                                                        10530014
           EXEC CICS                                                    10540014
              LINK PROGRAM (VL7CRLOG)                                   10550014
              COMMAREA (VLWCLOG0)                                       10560014
           END-EXEC.                                                    10570014
                                                                        10580014
           IF EIBRESP NOT = DFHRESP(NORMAL)                             10590014
              MOVE 'ERROR EN VL7CRLOG'      TO  ABC-REFERENCIA          10600014
              MOVE 'VL7CRLOG'               TO  ABC-OBJETO-ERROR        10610014
              PERFORM 999-ABEND-CICS                                    10620014
           END-IF.                                                      10630014
                                                                        10640014
           IF VL7LOG-CODRESP = 00                                       10650014
              CONTINUE                                                  10660014
           ELSE                                                         10670014
           IF VL7LOG-OPERACION(1:5) = '- 904'                           10680014
              MOVE 'VLE2105'             TO CAA-COD-ERROR               10690014
              MOVE 'VL7CRLOG'            TO CAA-VAR1-ERROR              10700014
              MOVE VL7LOG-OPERACION(1:5) TO CAA-VAR2-ERROR              10710014
              PERFORM 3-FINAL                                           10720014
           ELSE                                                         10730014
              MOVE 'VLE0907'             TO CAA-COD-ERROR               10740014
              MOVE 'VL7CRLOG'            TO CAA-VAR1-ERROR              10750014
              MOVE VL7LOG-CODRESP        TO CAA-VAR2-ERROR              10760014
              PERFORM 3-FINAL                                           10770014
           END-IF.                                                      10780014
                                                                        10790014
      *                                                                 10800014
       LLAMAR-VL7CRLOG-FIN.                                             10810014
           EXIT.                                                        10820014
       SELUND-VLDTXMI.                                                  10830014
                                                                        10840014
           EXEC SQL                                                     10850014
                SELECT  VXMI_CODBE     ,                                10860014
                        VXMI_CODCLI    ,                                10870014
                        VXMI_DENOM     ,                                10880014
                        VXMI_NIF       ,                                10890014
                        VXMI_DOMIC     ,                                10900014
                        VXMI_LOCAL     ,                                10910014
                        VXMI_CODPOS    ,                                10920014
                        VXMI_CNAE      ,                                10930014
                        VXMI_SUCVAL    ,                                10940014
                        VXMI_NUMFAC    ,                                10950014
                        VXMI_VALENT    ,                                10960014
                        VXMI_CTAVAL    ,                                10970014
                        VXMI_VALCER    ,                                10980014
                        VXMI_MULPLA    ,                                10990014
                        VXMI_RETEN   ,                                  11000014
                        VXMI_IVA     ,                                  11010014
                        VXMI_INCLUS  ,                                  11020014
                        VXMI_EXCLUS  ,                                  11030014
                        VXMI_PROVIS  ,                                  11040014
                        VXMI_FLISOP  ,                                  11050014
                        VXMI_LISENT  ,                                  11060014
                        VXMI_LISPAG  ,                                  11070014
                        VXMI_INCORP  ,                                  11080014
                        VXMI_CONTRT  ,                                  11090014
                        VXMI_CONTRT6 ,                                  11100014
                        VXMI_REF9    ,                                  11110014
                        VXMI_DELEGHAC,                                  11120014
                        VXMI_ADMINHAC,                                  11130014
                        VXMI_PRETELHAC,                                 11140014
                        VXMI_TELEFHAC ,                                 11150014
                        VXMI_APNOMHAC ,                                 11160014
                        VXMI_LUNES    ,                                 11170014
                        VXMI_VIERNES  ,                                 11180014
                        VXMI_YAPRESEN ,                                 11190014
                        VXMI_IMPRE1   ,                                 11200014
                        VXMI_IMPRE2   ,                                 11210014
                        VXMI_FILLER   ,                                 11220014
                        VXMI_LISCTIMP ,                                 11230014
                        VXMI_CONTCTA  ,                                 11240014
                        VXMI_PASS1    ,                                 11250014
                        VXMI_PASS2    ,                                 11260014
                        VXMI_LISCTA   ,                                 11270014
                        VXMI_LISAGTES ,                                 11280014
                        VXMI_LISREDUC ,                                 11290014
                        VXMI_LISFESTI ,                                 11300014
                        VXMI_LISMONED ,                                 11310014
                        VXMI_LISCONTA ,                                 11320014
                        VXMI_LISENT_1 ,                                 11330014
                        VXMI_LISCTA_SUC,                                11340014
                        VXMI_LISVALOR  ,                                11350014
                        VXMI_CONT_REV  ,                                11360014
                        VXMI_VALORACION ,                               11370014
                        VXMI_LIS_EXTRJ ,                                11380014
                        VXMI_FILLER1   ,                                11390014
                        VXMI_APCTAOFI  ,                                11400014
                        VXMI_TIPCUST   ,                                11410014
                        VXMI_MANFIS    ,                                11420014
                        VXMI_OPECUST   ,                                11430014
                        VXMI_OPEBOLSA  ,                                11440014
                        VXMI_AVISOS    ,                                11450014
                        VXMI_CONPANT   ,                                11460014
                        VXMI_COMCUST   ,                                11470014
                        VXMI_IMPALT    ,                                11480014
                        VXMI_CTACARGO  ,                                11490014
                        VXMI_CTAABONO  ,                                11500014
                        VXMI_CONTEN    ,                                11510014
                        VXMI_CONTEV    ,                                11520014
                        VXMI_CONTSN    ,                                11530014
                        VXMI_CONTSV    ,                                11540014
                        VXMI_LIS_RESTOS,                                11550014
                        VXMI_DIAS_LIMIT,                                11560014
                        VXMI_LIS_C_EXEN,                                11570014
                        VXMI_LIS_GJUD_BLO,                              11580014
                        VXMI_FEALTREG ,                                 11590014
                        VXMI_FEULMOD  ,                                 11600014
                        VXMI_HORULMOD ,                                 11610014
                        VXMI_NUMTER   ,                                 11620014
                        VXMI_USUARIO  ,                                 11630014
                        VXMI_FILLER2                                    11640014
                  INTO :LXMI-CODBE     ,                                11650014
                       :LXMI-CODCLI    ,                                11660014
                       :LXMI-DENOM     ,                                11670014
                       :LXMI-NIF       ,                                11680014
                       :LXMI-DOMIC     ,                                11690014
                       :LXMI-LOCAL     ,                                11700014
                       :LXMI-CODPOS    ,                                11710014
                       :LXMI-CNAE      ,                                11720014
                       :LXMI-SUCVAL    ,                                11730014
                       :LXMI-NUMFAC    ,                                11740014
                       :LXMI-VALENT    ,                                11750014
                       :LXMI-CTAVAL    ,                                11760014
                       :LXMI-VALCER    ,                                11770014
                       :LXMI-MULPLA    ,                                11780014
                       :LXMI-RETEN   ,                                  11790014
                       :LXMI-IVA     ,                                  11800014
                       :LXMI-INCLUS  ,                                  11810014
                       :LXMI-EXCLUS  ,                                  11820014
                       :LXMI-PROVIS  ,                                  11830014
                       :LXMI-FLISOP  ,                                  11840014
                       :LXMI-LISENT  ,                                  11850014
                       :LXMI-LISPAG  ,                                  11860014
                       :LXMI-INCORP  ,                                  11870014
                       :LXMI-CONTRT  ,                                  11880014
                       :LXMI-CONTRT6 ,                                  11890014
                       :LXMI-REF9    ,                                  11900014
                       :LXMI-DELEGHAC,                                  11910014
                       :LXMI-ADMINHAC,                                  11920014
                       :LXMI-PRETELHAC,                                 11930014
                       :LXMI-TELEFHAC ,                                 11940014
                       :LXMI-APNOMHAC ,                                 11950014
                       :LXMI-LUNES    ,                                 11960014
                       :LXMI-VIERNES  ,                                 11970014
                       :LXMI-YAPRESEN ,                                 11980014
                       :LXMI-IMPRE1   ,                                 11990014
                       :LXMI-IMPRE2   ,                                 12000014
                       :LXMI-FILLER   ,                                 12010014
                       :LXMI-LISCTIMP ,                                 12020014
                       :LXMI-CONTCTA  ,                                 12030014
                       :LXMI-PASS1    ,                                 12040014
                       :LXMI-PASS2    ,                                 12050014
                       :LXMI-LISCTA   ,                                 12060014
                       :LXMI-LISAGTES ,                                 12070014
                       :LXMI-LISREDUC ,                                 12080014
                       :LXMI-LISFESTI ,                                 12090014
                       :LXMI-LISMONED ,                                 12100014
                       :LXMI-LISCONTA ,                                 12110014
                       :LXMI-LISENT-1 ,                                 12120014
                       :LXMI-LISCTA-SUC,                                12130014
                       :LXMI-LISVALOR  ,                                12140014
                       :LXMI-CONT-REV  ,                                12150014
                       :LXMI-VALORACION ,                               12160014
                       :LXMI-LIS-EXTRJ ,                                12170014
                       :LXMI-FILLER1   ,                                12180014
                       :LXMI-APCTAOFI  ,                                12190014
                       :LXMI-TIPCUST   ,                                12200014
                       :LXMI-MANFIS    ,                                12210014
                       :LXMI-OPECUST   ,                                12220014
                       :LXMI-OPEBOLSA  ,                                12230014
                       :LXMI-AVISOS    ,                                12240014
                       :LXMI-CONPANT   ,                                12250014
                       :LXMI-COMCUST   ,                                12260014
                       :LXMI-IMPALT    ,                                12270014
                       :LXMI-CTACARGO  ,                                12280014
                       :LXMI-CTAABONO  ,                                12290014
                       :LXMI-CONTEN    ,                                12300014
                       :LXMI-CONTEV    ,                                12310014
                       :LXMI-CONTSN    ,                                12320014
                       :LXMI-CONTSV    ,                                12330014
                       :LXMI-LIS-RESTOS,                                12340014
                       :LXMI-DIAS-LIMIT,                                12350014
                       :LXMI-LIS-C-EXEN,                                12360014
                       :LXMI-LIS-GJUD-BLO,                              12370014
                       :LXMI-FEALTREG ,                                 12380014
                       :LXMI-FEULMOD  ,                                 12390014
                       :LXMI-HORULMOD ,                                 12400014
                       :LXMI-NUMTER   ,                                 12410014
                       :LXMI-USUARIO  ,                                 12420014
                       :LXMI-FILLER2                                    12430014
                  FROM  VLDTXMI                                         12440014
                 WHERE  VXMI_CODBE   = :LXMI-CODBE                      12450014
           END-EXEC                                                     12460014
                                                                        12470014
           MOVE SQLCODE TO SQLCODE-AUX                                  12480014
                                                                        12490014
           EVALUATE TRUE                                                12500014
              WHEN DB2-OK                                               12510014
                   INITIALIZE W-VLWCLOG0                                12520014
                   MOVE 'VLDTXMI'             TO  VL7LOG-TABLA          12530014
                   MOVE 'SELUND'              TO  VL7LOG-OPERACION      12540014
                   MOVE LENGTH OF DCLVLDTXMI  TO  VL7LOG-REGISTRO-LEN   12550014
                   MOVE LOGVLDTXMI            TO  VL7LOG-REGISTRO-TEXT  12560014
                   PERFORM LLAMAR-VL7CRLOG                              12570014
                      THRU LLAMAR-VL7CRLOG-FIN                          12580014
              WHEN DB2-NOTFND                                           12590014
                   CONTINUE                                             12600014
              WHEN OTHER                                                12610014
                   MOVE 'SELECT'      TO  ABC-REFERENCIA                12620014
                   MOVE 'VLDTXMI'     TO  ABC-OBJETO-ERROR              12630014
                   PERFORM 999-ABEND-DB2                                12640014
              END-EVALUATE.                                             12650014
                                                                        12660014
       SELUND-VLDTXMI-FIN.                                              12670014
           EXIT.                                                        12680014
      *                                                                 12690014
       SELUND-VLDTMES.                                                  12700014
                                                                        12710014
           EXEC SQL                                                     12720014
                SELECT  VMES_CUENTA    ,                                12730014
                        VMES_FALTA     ,                                12740014
                        VMES_REACTIVA  ,                                12750014
                        VMES_FEALTREG  ,                                12760014
                        VMES_FEULMOD   ,                                12770014
                        VMES_HORULMOD  ,                                12780014
                        VMES_NUMTER    ,                                12790014
                        VMES_USUARIO   ,                                12800014
                        VMES_FILLER                                     12810014
                  INTO :LMES-CUENTA   ,                                 12820014
                       :LMES-FALTA ,                                    12830014
                       :LMES-REACTIVA ,                                 12840014
                       :LMES-FEALTREG ,                                 12850014
                       :LMES-FEULMOD ,                                  12860014
                       :LMES-HORULMOD ,                                 12870014
                       :LMES-NUMTER ,                                   12880014
                       :LMES-USUARIO ,                                  12890014
                       :LMES-FILLER                                     12900014
                  FROM  VLDTMES                                         12910014
                 WHERE  VMES_CUENTA  = :LMES-CUENTA                     12920014
           END-EXEC                                                     12930014
                                                                        12940014
           MOVE SQLCODE TO SQLCODE-AUX                                  12950014
                                                                        12960014
           EVALUATE TRUE                                                12970014
              WHEN DB2-OK                                               12980014
                   INITIALIZE W-VLWCLOG0                                12990014
                   MOVE 'VLDTMES'             TO  VL7LOG-TABLA          13000014
                   MOVE 'SELUND'              TO  VL7LOG-OPERACION      13010014
                   MOVE LENGTH OF DCLVLDTMES  TO  VL7LOG-REGISTRO-LEN   13020014
                   MOVE LOGVLDTMES            TO  VL7LOG-REGISTRO-TEXT  13030014
                   PERFORM LLAMAR-VL7CRLOG                              13040014
                      THRU LLAMAR-VL7CRLOG-FIN                          13050014
              WHEN DB2-NOTFND                                           13060014
                   CONTINUE                                             13070014
              WHEN OTHER                                                13080014
                   MOVE 'SELECT'      TO  ABC-REFERENCIA                13090014
                   MOVE 'VLDTMES'     TO  ABC-OBJETO-ERROR              13100014
                   PERFORM 999-ABEND-DB2                                13110014
              END-EVALUATE.                                             13120014
                                                                        13130014
       SELUND-VLDTMES-FIN.                                              13140014
           EXIT.                                                        13150014
      *                                                                 13160014
       SELUND-VLDTARC.                                                  13170014
                                                                        13180014
           EXEC SQL                                                     13190014
                SELECT  VARC_CUENTA    ,                                13200014
                        VARC_CENTAD    ,                                13210014
                        VARC_NUMCLI    ,                                13220014
                        VARC_CLMAST    ,                                13230014
                        VARC_MONEDA    ,                                13240014
                        VARC_SUCURS    ,                                13250014
                        VARC_CTACAR    ,                                13260014
                        VARC_CTAABO    ,                                13270014
                        VARC_TEXTO     ,                                13280014
                        VARC_PRESEN    ,                                13290014
                        VARC_GRUPO     ,                                13300014
                        VARC_RUT       ,                                13310014
                        VARC_CNAE      ,                                13320014
                        VARC_SITUAC    ,                                13330014
                        VARC_EXEN1     ,                                13340014
                        VARC_EXEN2     ,                                13350014
                        VARC_EXEN3     ,                                13360014
                        VARC_EXEN4     ,                                13370014
                        VARC_EXEN5     ,                                13380014
                        VARC_EXEN6     ,                                13390014
                        VARC_EXEN7     ,                                13400014
                        VARC_EXEN8     ,                                13410014
                        VARC_EXEN9     ,                                13420014
                        VARC_EXEN10    ,                                13430014
                        VARC_ANALIS    ,                                13440014
                        VARC_CLACARGO  ,                                13450014
                        VARC_CLABONO   ,                                13460014
                        VARC_NUMDOM    ,                                13470014
                        VARC_CODSUS    ,                                13480014
                        VARC_FE_ULT_EXT,                                13490014
                        VARC_PAIS      ,                                13500014
                        VARC_FE_CARTERA,                                13510014
                        VARC_CLTELEX   ,                                13520014
                        VARC_FE_ALTA   ,                                13530014
                        VARC_VALORACION,                                13540014
                        VARC_VALEXTRJ  ,                                13550014
                        VARC_INVERSOR  ,                                13560014
                        VARC_DIRECTA   ,                                13570014
                        VARC_MAX_CVE_1 ,                                13580014
                        VARC_MAX_DCU_5 ,                                13590014
                        VARC_MAX_SUS_6 ,                                13600014
                        VARC_MAX_DIV_7 ,                                13610014
                        VARC_MAX_AMO_8 ,                                13620014
                        VARC_MAX_PAJ_9 ,                                13630014
                        VARC_FECHA_102 ,                                13640014
                        VARC_TARIFACUS ,                                13650014
                        VARC_SWIFT_TELEX,                               13660014
                        VARC_TELEX_2   ,                                13670014
                        VARC_GRUPO_CTAS,                                13680014
                        VARC_OPER_TIT  ,                                13690014
                        VARC_FEALTREG  ,                                13700014
                        VARC_FEULMOD   ,                                13710014
                        VARC_HORULMOD  ,                                13720014
                        VARC_NUMTER    ,                                13730014
                        VARC_USUARIO   ,                                13740014
                        VARC_FILLER    ,                                13750014
                        VARC_CTAVAL20  ,                                13760014
                        VARC_NUMMAN    ,                                13770014
                        VARC_INDIMP    ,                                13780014
                        VARC_INDSAB                                     13790014
                  INTO :LARC-CUENTA   ,                                 13800014
                       :LARC-CENTAD    ,                                13810014
                       :LARC-NUMCLI    ,                                13820014
                       :LARC-CLMAST    ,                                13830014
                       :LARC-MONEDA    ,                                13840014
                       :LARC-SUCURS    ,                                13850014
                       :LARC-CTACAR    ,                                13860014
                       :LARC-CTAABO    ,                                13870014
                       :LARC-TEXTO     ,                                13880014
                       :LARC-PRESEN    ,                                13890014
                       :LARC-GRUPO     ,                                13900014
                       :LARC-RUT       ,                                13910014
                       :LARC-CNAE      ,                                13920014
                       :LARC-SITUAC    ,                                13930014
                       :LARC-EXEN1     ,                                13940014
                       :LARC-EXEN2     ,                                13950014
                       :LARC-EXEN3     ,                                13960014
                       :LARC-EXEN4     ,                                13970014
                       :LARC-EXEN5     ,                                13980014
                       :LARC-EXEN6     ,                                13990014
                       :LARC-EXEN7     ,                                14000014
                       :LARC-EXEN8     ,                                14010014
                       :LARC-EXEN9     ,                                14020014
                       :LARC-EXEN10    ,                                14030014
                       :LARC-ANALIS    ,                                14040014
                       :LARC-CLACARGO  ,                                14050014
                       :LARC-CLABONO   ,                                14060014
                       :LARC-NUMDOM    ,                                14070014
                       :LARC-CODSUS    ,                                14080014
                       :LARC-FE-ULT-EXT,                                14090014
                       :LARC-PAIS      ,                                14100014
                       :LARC-FE-CARTERA,                                14110014
                       :LARC-CLTELEX   ,                                14120014
                       :LARC-FE-ALTA   ,                                14130014
                       :LARC-VALORACION,                                14140014
                       :LARC-VALEXTRJ  ,                                14150014
                       :LARC-INVERSOR  ,                                14160014
                       :LARC-DIRECTA   ,                                14170014
                       :LARC-MAX-CVE-1 ,                                14180014
                       :LARC-MAX-DCU-5 ,                                14190014
                       :LARC-MAX-SUS-6 ,                                14200014
                       :LARC-MAX-DIV-7 ,                                14210014
                       :LARC-MAX-AMO-8 ,                                14220014
                       :LARC-MAX-PAJ-9 ,                                14230014
                       :LARC-FECHA-102 ,                                14240014
                       :LARC-TARIFACUS ,                                14250014
                       :LARC-SWIFT-TELEX,                               14260014
                       :LARC-TELEX-2   ,                                14270014
                       :LARC-GRUPO-CTAS,                                14280014
                       :LARC-OPER-TIT  ,                                14290014
                       :LARC-FEALTREG  ,                                14300014
                       :LARC-FEULMOD   ,                                14310014
                       :LARC-HORULMOD  ,                                14320014
                       :LARC-NUMTER    ,                                14330014
                       :LARC-USUARIO   ,                                14340014
                       :LARC-FILLER    ,                                14350014
                       :LARC-CTAVAL20  ,                                14360014
                       :LARC-NUMMAN    ,                                14370014
                       :LARC-INDIMP    ,                                14380014
                       :LARC-INDSAB                                     14390014
                  FROM  VLDTARC                                         14400014
                 WHERE  VARC_CUENTA  = :LARC-CUENTA                     14410014
           END-EXEC                                                     14420014
                                                                        14430014
           MOVE SQLCODE TO SQLCODE-AUX                                  14440014
                                                                        14450014
           EVALUATE TRUE                                                14460014
              WHEN DB2-OK                                               14470014
                   INITIALIZE W-VLWCLOG0                                14480014
                   MOVE 'VLDTARC'             TO  VL7LOG-TABLA          14490014
                   MOVE 'SELUND'              TO  VL7LOG-OPERACION      14500014
                   MOVE LENGTH OF DCLVLDTARC  TO  VL7LOG-REGISTRO-LEN   14510014
                   MOVE LOGVLDTARC            TO  VL7LOG-REGISTRO-TEXT  14520014
                   PERFORM LLAMAR-VL7CRLOG                              14530014
                      THRU LLAMAR-VL7CRLOG-FIN                          14540014
              WHEN DB2-NOTFND                                           14550014
                   CONTINUE                                             14560014
              WHEN OTHER                                                14570014
                   MOVE 'SELECT'      TO  ABC-REFERENCIA                14580014
                   MOVE 'VLDTARC'     TO  ABC-OBJETO-ERROR              14590014
                   PERFORM 999-ABEND-DB2                                14600014
              END-EVALUATE.                                             14610014
                                                                        14620014
       SELUND-VLDTARC-FIN.                                              14630014
           EXIT.                                                        14640014
      *                                                                 14650014
       ACCESO-VLDTHIS.                                                  14660014
      *                                                                 14670014
           MOVE CTA0101I                    TO W-CUENTA                 14680014
           MOVE W-CUENTA                    TO VHIS-CTAVAL              14690014
           MOVE LOW-VALUES                  TO VHIS-CODVALOR            14700014
           MOVE ZEROES                      TO VHIS-TIPGAS              14710014
                                               VHIS-ANO                 14720014
                                               VHIS-MES                 14730014
                                                                        14740014
           MOVE SPACES                      TO SW-FIN-HIS               14750014
                                                                        14760014
           PERFORM ABRIR-CURSOR-VHIS                                    14770014
              THRU ABRIR-CURSOR-VHIS-FIN                                14780014
                                                                        14790014
           PERFORM LEER-CURSOR-VHIS                                     14800014
              THRU LEER-CURSOR-VHIS-FIN                                 14810014
                                                                        14820014
           PERFORM UNTIL FIN-HIS OR FIN-HIS-OK                          14830014
                                                                        14840014
              IF ((VHIS-TITULOS1  NOT = 0 AND VHIS-COBRADO1  = ' ')     14850014
               OR (VHIS-TITULOS2  NOT = 0 AND VHIS-COBRADO2  = ' ')     14860014
               OR (VHIS-TITULOS3  NOT = 0 AND VHIS-COBRADO3  = ' ')     14870014
               OR (VHIS-TITULOS4  NOT = 0 AND VHIS-COBRADO4  = ' ')     14880014
               OR (VHIS-TITULOS5  NOT = 0 AND VHIS-COBRADO5  = ' ')     14890014
               OR (VHIS-TITULOS6  NOT = 0 AND VHIS-COBRADO6  = ' ')     14900014
               OR (VHIS-TITULOS7  NOT = 0 AND VHIS-COBRADO7  = ' ')     14910014
               OR (VHIS-TITULOS8  NOT = 0 AND VHIS-COBRADO8  = ' ')     14920014
               OR (VHIS-TITULOS9  NOT = 0 AND VHIS-COBRADO9  = ' ')     14930014
               OR (VHIS-TITULOS10 NOT = 0 AND VHIS-COBRADO10 = ' ')     14940014
               OR (VHIS-TITULOS11 NOT = 0 AND VHIS-COBRADO11 = ' ')     14950014
               OR (VHIS-TITULOS12 NOT = 0 AND VHIS-COBRADO12 = ' ')     14960014
               OR (VHIS-TITULOS13 NOT = 0 AND VHIS-COBRADO13 = ' ')     14970014
               OR (VHIS-TITULOS14 NOT = 0 AND VHIS-COBRADO14 = ' ')     14980014
               OR (VHIS-TITULOS15 NOT = 0 AND VHIS-COBRADO15 = ' ')     14990014
               OR (VHIS-TITULOS16 NOT = 0 AND VHIS-COBRADO16 = ' ')     15000014
               OR (VHIS-TITULOS17 NOT = 0 AND VHIS-COBRADO17 = ' ')     15010014
               OR (VHIS-TITULOS18 NOT = 0 AND VHIS-COBRADO18 = ' ')     15020014
               OR (VHIS-TITULOS19 NOT = 0 AND VHIS-COBRADO19 = ' ')     15030014
               OR (VHIS-TITULOS20 NOT = 0 AND VHIS-COBRADO20 = ' ')     15040014
               OR (VHIS-TITULOS21 NOT = 0 AND VHIS-COBRADO21 = ' ')     15050014
               OR (VHIS-TITULOS22 NOT = 0 AND VHIS-COBRADO22 = ' ')     15060014
               OR (VHIS-TITULOS23 NOT = 0 AND VHIS-COBRADO23 = ' ')     15070014
               OR (VHIS-TITULOS24 NOT = 0 AND VHIS-COBRADO24 = ' ')     15080014
               OR (VHIS-TITULOS25 NOT = 0 AND VHIS-COBRADO25 = ' ')     15090014
               OR (VHIS-TITULOS26 NOT = 0 AND VHIS-COBRADO26 = ' ')     15100014
               OR (VHIS-TITULOS27 NOT = 0 AND VHIS-COBRADO27 = ' ')     15110014
               OR (VHIS-TITULOS28 NOT = 0 AND VHIS-COBRADO28 = ' ')     15120014
               OR (VHIS-TITULOS29 NOT = 0 AND VHIS-COBRADO29 = ' ')     15130014
               OR (VHIS-TITULOS30 NOT = 0 AND VHIS-COBRADO30 = ' ')     15140014
               OR (VHIS-TITULOS31 NOT = 0 AND VHIS-COBRADO31 = ' '))    15150014
                 MOVE '2' TO SW-FIN-HIS                                 15160014
              ELSE                                                      15170014
                 PERFORM LEER-CURSOR-VHIS                               15180014
                    THRU LEER-CURSOR-VHIS-FIN                           15190014
              END-IF                                                    15200014
           END-PERFORM                                                  15210014
                                                                        15220014
           PERFORM CERRAR-CURSOR-VHIS                                   15230014
              THRU CERRAR-CURSOR-VHIS-FIN                               15240014
      *                                                                 15250014
           IF FIN-HIS-OK                                                15260014
              MOVE 'VLE1843' TO CAA-COD-ERROR                           15270014
              MOVE -1        TO CTA0101L                                15280014
              PERFORM 3-FINAL                                           15290014
           END-IF.                                                      15300014
      *                                                                 15310014
       ACCESO-VLDTHIS-FIN.                                              15320014
           EXIT.                                                        15330014
      *                                                                 15340014
       ABRIR-CURSOR-VHIS.                                               15350014
      *                                                                 15360014
           EXEC SQL                                                     15370014
               OPEN VLDCHIS1                                            15380014
           END-EXEC.                                                    15390014
      *                                                                 15400014
           IF SQLCODE NOT = ZEROS                                       15410014
              INITIALIZE   QGECABC                                      15420014
              MOVE 'OPEN'        TO  ABC-REFERENCIA                     15430014
              MOVE 'VLDTHIS'     TO  ABC-OBJETO-ERROR                   15440014
              PERFORM 999-ABEND-DB2                                     15450014
           END-IF.                                                      15460014
      *                                                                 15470014
       ABRIR-CURSOR-VHIS-FIN.                                           15480014
           EXIT.                                                        15490014
      *                                                                 15500014
       LEER-CURSOR-VHIS.                                                15510014
      *                                                                 15520014
           EXEC SQL                                                     15530014
                FETCH VLDCHIS1                                          15540014
                INTO :VHIS-TITULOS1   ,                                 15550014
                     :VHIS-COBRADO1   ,                                 15560014
                     :VHIS-TITULOS2   ,                                 15570014
                     :VHIS-COBRADO2   ,                                 15580014
                     :VHIS-TITULOS3   ,                                 15590014
                     :VHIS-COBRADO3   ,                                 15600014
                     :VHIS-TITULOS4   ,                                 15610014
                     :VHIS-COBRADO4   ,                                 15620014
                     :VHIS-TITULOS5   ,                                 15630014
                     :VHIS-COBRADO5   ,                                 15640014
                     :VHIS-TITULOS6   ,                                 15650014
                     :VHIS-COBRADO6   ,                                 15660014
                     :VHIS-TITULOS7   ,                                 15670014
                     :VHIS-COBRADO7   ,                                 15680014
                     :VHIS-TITULOS8   ,                                 15690014
                     :VHIS-COBRADO8   ,                                 15700014
                     :VHIS-TITULOS9   ,                                 15710014
                     :VHIS-COBRADO9   ,                                 15720014
                     :VHIS-TITULOS10  ,                                 15730014
                     :VHIS-COBRADO10  ,                                 15740014
                     :VHIS-TITULOS11  ,                                 15750014
                     :VHIS-COBRADO11  ,                                 15760014
                     :VHIS-TITULOS12  ,                                 15770014
                     :VHIS-COBRADO12  ,                                 15780014
                     :VHIS-TITULOS13  ,                                 15790014
                     :VHIS-COBRADO13  ,                                 15800014
                     :VHIS-TITULOS14  ,                                 15810014
                     :VHIS-COBRADO14  ,                                 15820014
                     :VHIS-TITULOS15  ,                                 15830014
                     :VHIS-COBRADO15  ,                                 15840014
                     :VHIS-TITULOS16  ,                                 15850014
                     :VHIS-COBRADO16  ,                                 15860014
                     :VHIS-TITULOS17  ,                                 15870014
                     :VHIS-COBRADO17  ,                                 15880014
                     :VHIS-TITULOS18  ,                                 15890014
                     :VHIS-COBRADO18  ,                                 15900014
                     :VHIS-TITULOS19  ,                                 15910014
                     :VHIS-COBRADO19  ,                                 15920014
                     :VHIS-TITULOS20  ,                                 15930014
                     :VHIS-COBRADO20  ,                                 15940014
                     :VHIS-TITULOS21  ,                                 15950014
                     :VHIS-COBRADO21  ,                                 15960014
                     :VHIS-TITULOS22  ,                                 15970014
                     :VHIS-COBRADO22  ,                                 15980014
                     :VHIS-TITULOS23  ,                                 15990014
                     :VHIS-COBRADO23  ,                                 16000014
                     :VHIS-TITULOS24  ,                                 16010014
                     :VHIS-COBRADO24  ,                                 16020014
                     :VHIS-TITULOS25  ,                                 16030014
                     :VHIS-COBRADO25  ,                                 16040014
                     :VHIS-TITULOS26  ,                                 16050014
                     :VHIS-COBRADO26  ,                                 16060014
                     :VHIS-TITULOS27  ,                                 16070014
                     :VHIS-COBRADO27  ,                                 16080014
                     :VHIS-TITULOS28  ,                                 16090014
                     :VHIS-COBRADO28  ,                                 16100014
                     :VHIS-TITULOS29  ,                                 16110014
                     :VHIS-COBRADO29  ,                                 16120014
                     :VHIS-TITULOS30  ,                                 16130014
                     :VHIS-COBRADO30  ,                                 16140014
                     :VHIS-TITULOS31  ,                                 16150014
                     :VHIS-COBRADO31                                    16160014
           END-EXEC.                                                    16170014
      *                                                                 16180014
           MOVE SQLCODE TO SQLCODE-AUX                                  16190014
      *                                                                 16200014
           EVALUATE TRUE                                                16210014
              WHEN DB2-OK                                               16220014
      *A2011-RUTLOG-I                                                   16230014
      *            INITIALIZE W-VLWCLOG0                                16240014
      *                       LOGVLDTHIS                                16250014
      *            MOVE 'VLDTHIS'             TO  VL7LOG-TABLA          16260014
      *            MOVE 'FETCH'               TO  VL7LOG-OPERACION      16270014
      *            MOVE LENGTH OF DCLVLDTHIS  TO  VL7LOG-REGISTRO-LEN   16280014
      *            MOVE DCLVLDTHIS            TO  LOGVLDTHIS            16290014
      *            MOVE LOGVLDTHIS            TO  VL7LOG-REGISTRO-TEXT  16300014
      *            PERFORM LLAMAR-VL7CRLOG                              16310014
      *               THRU LLAMAR-VL7CRLOG-FIN                          16320014
      *A2011-RUTLOG-F                                                   16330014
              WHEN DB2-NOTFND                                           16340014
                   MOVE '1'           TO  SW-FIN-HIS                    16350014
                                                                        16360014
              WHEN OTHER                                                16370014
                   INITIALIZE   QGECABC                                 16380014
                   MOVE 'FETCH'       TO  ABC-REFERENCIA                16390014
                   MOVE 'VLDTHIS'     TO  ABC-OBJETO-ERROR              16400014
                   PERFORM 999-ABEND-DB2                                16410014
                                                                        16420014
           END-EVALUATE.                                                16430014
                                                                        16440014
      *                                                                 16450014
       LEER-CURSOR-VHIS-FIN.                                            16460014
           EXIT.                                                        16470014
      *                                                                 16480014
       CERRAR-CURSOR-VHIS.                                              16490014
      *                                                                 16500014
           EXEC SQL                                                     16510014
                CLOSE VLDCHIS1                                          16520014
           END-EXEC.                                                    16530014
      *                                                                 16540014
           IF SQLCODE NOT = ZEROS                                       16550014
              INITIALIZE   QGECABC                                      16560014
              MOVE 'CLOSE'       TO  ABC-REFERENCIA                     16570014
              MOVE 'VLDTHIS'     TO  ABC-OBJETO-ERROR                   16580014
              PERFORM 999-ABEND-DB2                                     16590014
           END-IF.                                                      16600014
      *                                                                 16610014
       CERRAR-CURSOR-VHIS-FIN.                                          16620014
           EXIT.                                                        16630014
      *                                                                 16640014
      *                                                                 16650014
       OBTENER-MONJUR1.                                                 16660014
      *                                                                 16670014
      * OBTENEMOS LA MONEDA DE LA CTA DE CARGO Y LA OFICINA             16680014
      * PROPIETARIA CON SU DESCRIPCION                                  16690014
      *                                                                 16700014
           INITIALIZE                          W-BGECMDC                16710014
           MOVE NCC0101I(1:4)                 TO MDC-ENTIDAD            16720014
           MOVE NCC0101I(5:4)                 TO MDC-CENTRO-ALTA.       16730014
           MOVE NCC0101I(11:2)                TO MDC-CUENTA(1:2).       16740014
           MOVE NCC0101I(13:8)                TO MDC-CUENTA(3:8).       16750014
      *                                                                 16760014
           EXEC CICS                                                    16770014
             LINK PROGRAM (BG2CMDC0)                                    16780014
             COMMAREA (BGECMDC)                                         16790014
           END-EXEC                                                     16800014
      *                                                                 16810014
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         16820014
              MOVE 'ERROR EN BGECMDC0'  TO ABC-REFERENCIA               16830014
              MOVE 'BG2CMDC0'           TO ABC-OBJETO-ERROR             16840014
              PERFORM 999-ABEND-CICS                                    16850014
           END-IF                                                       16860014
      *                                                                 16870014
           EVALUATE MDC-CODERR                                          16880014
             WHEN SPACES                                                16890014
JIPC  *           IF MDC-INDESTA = 'A' OR 'R' OR 'P'                    16900014
                  IF MDC-INDESTA = 'A'                                  16910014
                     MOVE MDC-CDDIVIS TO MON0101O                       16920014
                                         W-MONEDA-CAR                   16930014
                     MOVE MDC-CENTRO-CONTAB  TO OFI-PRO                 16940014
                  ELSE                                                  16950014
                     MOVE MDC-CDDIVIS TO MON0101O                       16960014
                     MOVE -1           TO NCC0101L                      16970014
                     MOVE 'VLE1101'    TO CAA-COD-ERROR                 16980014
                     PERFORM 3-FINAL                                    16990014
                  END-IF                                                17000014
             WHEN OTHER                                                 17010014
                  MOVE -1           TO NCC0101L                         17020014
                  MOVE MDC-CODERR   TO CAA-COD-ERROR                    17030014
                  PERFORM 3-FINAL                                       17040014
           END-EVALUATE.                                                17050014
      *                                                                 17060014
JPC@4      IF ENT0101I = '0069' OR '2010'                               17070014
JPC@4         MOVE CAA-CENTRO-CONT  TO OFI-PRO                          17080014
JPC@4      END-IF                                                       17090014
      *A2008-I. 10-08-99. OFICINA PROPIETARIA LA DE LA CTA DE CARGO     17100014
           MOVE OFI-PRO             TO SUC0101O.                        17110014
      *    MOVE CEN0101I            TO SUC0101O.                        17120014
      *A2008-F. 10-08-99. OFICINA PROPIETARIA LA DE LA CTA DE CARGO     17130014
      *                                                                 17140014
           PERFORM DESCRIPCION-OFICINA                                  17150014
              THRU DESCRIPCION-OFICINA-FIN.                             17160014
      *                                                                 17170014
       OBTENER-MONJUR1-FIN. EXIT.                                       17180014
      *                                                                 17190014
      *                                                                 17200014
       OBTENER-MONJUR2.                                                 17210014
      *                                                                 17220014
           INITIALIZE                          W-BGECMDC                17230014
           MOVE NC20101I(1:4)                 TO MDC-ENTIDAD            17240014
           MOVE NC20101I(5:4)                 TO MDC-CENTRO-ALTA.       17250014
           MOVE NC20101I(11:2)                TO MDC-CUENTA(1:2).       17260014
           MOVE NC20101I(13:8)                TO MDC-CUENTA(3:8).       17270014
      *                                                                 17280014
           EXEC CICS                                                    17290014
             LINK PROGRAM (BG2CMDC0)                                    17300014
             COMMAREA (BGECMDC)                                         17310014
           END-EXEC                                                     17320014
      *                                                                 17330014
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         17340014
              MOVE 'ERROR EN BGECMDC0'  TO ABC-REFERENCIA               17350014
              MOVE 'BG2CMDC0'           TO ABC-OBJETO-ERROR             17360014
              PERFORM 999-ABEND-CICS                                    17370014
           END-IF                                                       17380014
      *                                                                 17390014
           EVALUATE MDC-CODERR                                          17400014
             WHEN SPACES                                                17410014
JIPC  *           IF MDC-INDESTA = 'A' OR 'R' OR 'P'                    17420014
                  IF MDC-INDESTA = 'A'                                  17430014
                     MOVE MDC-CDDIVIS TO MO20101O                       17440014
                                         W-MONEDA-ABO                   17450014
                  ELSE                                                  17460014
                     MOVE MDC-CDDIVIS TO MO20101O                       17470014
                     MOVE -1           TO NC20101L                      17480014
                     MOVE 'VLE1101'    TO CAA-COD-ERROR                 17490014
                     PERFORM 3-FINAL                                    17500014
                  END-IF                                                17510014
             WHEN OTHER                                                 17520014
                  MOVE -1           TO NC20101L                         17530014
                  MOVE MDC-CODERR   TO CAA-COD-ERROR                    17540014
                  PERFORM 3-FINAL                                       17550014
                                                                        17560014
           END-EVALUATE.                                                17570014
      *                                                                 17580014
       OBTENER-MONJUR2-FIN. EXIT.                                       17590014
      *                                                                 17600014
      *200711038-INI                                                    17610014
       CUENTA-REGISTRO.                                                 17620014
           INITIALIZE                 BGECAPE4.                         17630014
           MOVE END0101I          TO  APE4-CCC (01:04).                 17640014
           MOVE CEN0101I          TO  APE4-CCC (05:04).                 17650014
           MOVE DGT0101I          TO  APE4-CCC (09:02).                 17660014
           MOVE PRD0101I          TO  APE4-CCC (11:02).                 17670014
           MOVE CTA0101I          TO  APE4-CCC (13:07).                 17680014
           MOVE DG20101I          TO  APE4-CCC (20:01).                 17690014
           INITIALIZE                 TCWC2010.                         17700014
           MOVE END0101O          TO  RUTI-ENTIDAD.                     17710014
           MOVE END0101O          TO  RUTI-CAMPO (01:04)                17720014
           MOVE CEN0101O          TO  RUTI-CAMPO (05:04)                17730014
           MOVE 0                 TO  RUTI-CAMPO (09:01)                17740014
           MOVE +9                TO  RUTI-LONG.                        17750014
           CALL 'TC8C2030'            USING TCWC2010.                   17760014
           IF RUTI-CODERR NOT EQUAL SPACES AND 'QRE0006'                17770014
              MOVE '0'            TO  APE4-CCC (09:01)                  17780014
           ELSE                                                         17790014
              MOVE RUTI-DIG       TO  APE4-CCC (09:01)                  17800014
           END-IF                                                       17810014
           INITIALIZE                 TCWC2010.                         17820014
           MOVE END0101O          TO  RUTI-ENTIDAD.                     17830014
           MOVE PRD0101O          TO  RUTI-CAMPO (01:02)                17840014
           MOVE CTA0101O          TO  RUTI-CAMPO (03:07)                17850014
           MOVE DG20101O          TO  RUTI-CAMPO (10:01)                17860014
           MOVE 0                 TO  RUTI-CAMPO (11:01)                17870014
           MOVE +11               TO  RUTI-LONG.                        17880014
           CALL 'TC8C2030'            USING TCWC2010.                   17890014
           IF RUTI-CODERR NOT EQUAL SPACES AND 'QRE0006'                17900014
              MOVE '0'            TO  APE4-CCC (10:01)                  17910014
           ELSE                                                         17920014
              MOVE RUTI-DIG       TO  APE4-CCC (10:01)                  17930014
           END-IF                                                       17940014
           MOVE PRD0101I          TO  APE4-PRODUCT.                     17950014
           EVALUATE TCL0101I                                            17960014
               WHEN 'N'                                                 17970014
                    EVALUATE MDA0101I                                   17980014
                        WHEN 'PEN' MOVE '0026' TO APE4-SUBPROD          17990014
                        WHEN 'USD' MOVE '0027' TO APE4-SUBPROD          18000014
                    END-EVALUATE                                        18010014
               WHEN OTHER                                               18020014
                    EVALUATE MDA0101I                                   18030014
                        WHEN 'PEN' MOVE '0020' TO APE4-SUBPROD          18040014
                        WHEN 'USD' MOVE '0021' TO APE4-SUBPROD          18050014
                    END-EVALUATE                                        18060014
           END-EVALUATE                                                 18070014
           MOVE SPACES            TO  APE4-CCCMODE.                     18080014
           MOVE 'TF'              TO  APE4-CANAL.                       18090014
           MOVE 'VL'              TO  APE4-APLICATIVO.                  18100014
           MOVE '00'              TO  APE4-GESTOR  (01:02).             18110014
           MOVE CAA-CENTRO-CONT   TO  APE4-GESTOR  (03:04).             18120014
           MOVE W520-SUJGRUP      TO  APE4-EMPRESA (01:01).             18130014
           MOVE W520-SUJSUBG1     TO  APE4-EMPRESA (02:02).             18140014
           MOVE 'N'               TO  APE4-VINCPER.                     18150014
           MOVE CAA-FECHA-OPER    TO  APE4-FCHAPER.                     18160014
           MOVE SPACES            TO  APE4-CCCCARG.                     18170014
           MOVE CAA-ENTIDAD       TO  APE4-CAA-ENTIDAD.                 18180014
           MOVE CAA-ENTIDAD-9     TO  APE4-CAA-ENTIDAD-9.               18190014
           MOVE CAA-CENTRO-CONT   TO  APE4-CAA-CENTRO-CONT.             18200014
           MOVE CAA-CENTRO-CONT-9 TO  APE4-CAA-CENTRO-CONT-9.           18210014
           MOVE CAA-NETNAME-CONT  TO  APE4-CAA-NETNAME-CONT.            18220014
           MOVE CAA-FECHA-CONT2   TO  APE4-CAA-FECHA-CONT2.             18230014
           MOVE CAA-IDIOMA-TERM   TO  APE4-CAA-IDIOMA-TERM.             18240014
           MOVE CAA-FECHA-OPER2   TO  APE4-CAA-FECHA-OPER2.             18250014
           MOVE CAA-FECHA-OPER    TO  APE4-CAA-FECHA-OPER.              18260014
           MOVE CAA-FECHA-TRANS2  TO  APE4-CAA-FECHA-TRANS2.            18270014
           MOVE CAA-FECHA-TRANSED TO  APE4-CAA-FECHA-TRANSED.           18280014
           MOVE CAA-HORA-TRANS    TO  APE4-CAA-HORA-TRANS.              18290014
           MOVE CAA-TIPO-TERM     TO  APE4-CAA-TIPO-TERM.               18300014
           MOVE CAA-USERID        TO  APE4-CAA-USERID.                  18310014
           MOVE CAA-CAJERO        TO  APE4-CAA-CAJERO.                  18320014
           MOVE CAA-TERMINAL-CONT TO  APE4-CAA-TERMINAL-CONT.           18330014
           MOVE CAA-TIOPER        TO  APE4-CAA-TIOPER.                  18340014
           MOVE CAA-REFER-AUTO    TO  APE4-CAA-REFER-AUTO.              18350014
           MOVE CAA-IND-AUTO      TO  APE4-CAA-IND-AUTO.                18360014
           MOVE CAA-CONTABLE      TO  APE4-CAA-CONTABLE.                18370014
           MOVE CAA-DIARIO-LOCAL  TO  APE4-CAA-DIARIO-LOCAL.            18380014
           MOVE CAA-AUTORIZ       TO  APE4-CAA-AUTORIZ.                 18390014
      *                                                                 18400014
           EXEC CICS                                                    18410014
                LINK PROGRAM (BG7CAPE4)                                 18420014
                    COMMAREA (BGECAPE4)                                 18430014
           END-EXEC                                                     18440014
      *                                                                 18450014
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)                         18460014
              MOVE 'ERROR EN BG7CAPE4'  TO ABC-REFERENCIA               18470014
              MOVE 'BG7CAPE4'           TO ABC-OBJETO-ERROR             18480014
              PERFORM 999-ABEND-CICS                                    18490014
           END-IF.                                                      18500014
      *                                                                 18510014
           EVALUATE APE4-RETORNO                                        18520014
               WHEN '00'                                                18530014
                    CONTINUE                                            18540014
               WHEN OTHER                                               18550014
                    IF APE4-CAA-COD-ERROR = 'BGE0274' AND               18560014
                       PF2                                              18570014
                       CONTINUE                                         18580014
                    ELSE                                                18590014
                       MOVE -1                  TO REG0101L             18600014
                       MOVE APE4-CAA-COD-ERROR  TO CAA-COD-ERROR        18610014
                       MOVE APE4-CAA-VAR1-ERROR TO CAA-VAR1-ERROR       18620014
                       MOVE APE4-CAA-VAR2-ERROR TO CAA-VAR2-ERROR       18630014
                       PERFORM 3-FINAL                                  18640014
                    END-IF                                              18650014
           END-EVALUATE.                                                18660014
                                                                        18670014
           MOVE END0101I   TO   NCC0101I (01:04).                       18680014
           MOVE CEN0101I   TO   NCC0101I (05:04).                       18690014
           MOVE DGT0101I   TO   NCC0101I (09:02).                       18700014
           MOVE PRD0101I   TO   NCC0101I (11:02).                       18710014
           MOVE CTA0101I   TO   NCC0101I (13:07).                       18720014
           MOVE DG20101I   TO   NCC0101I (20:01).                       18730014
           MOVE MDA0101I   TO   MON0101I.                               18740014
                                                                        18750014
           MOVE END0101I   TO   NC20101I (01:04).                       18760014
           MOVE CEN0101I   TO   NC20101I (05:04).                       18770014
           MOVE DGT0101I   TO   NC20101I (09:02).                       18780014
           MOVE PRD0101I   TO   NC20101I (11:02).                       18790014
           MOVE CTA0101I   TO   NC20101I (13:07).                       18800014
           MOVE DG20101I   TO   NC20101I (20:01).                       18810014
           MOVE MDA0101I   TO   MO20101I.                               18820014
      *                                                                 18830014
       CUENTA-REGISTRO-FIN.                                             18840014
           EXIT.                                                        18850014
      *200711038-FIN                                                    18860014
JPC@4  VALIDAR-OTRAS-CTAS.                                              18870014
JPC@4      MOVE TIT0101I        TO WARC-NUMCLI                          18880014
JPC@4      MOVE 'A'             TO WARC-SITUAC                          18890014
JPC@4      MOVE MDA0101I        TO WARC-MONEDA                          18900014
JPC@4      EXEC SQL                                                     18910014
JPC@4          OPEN VLDCARC                                             18920014
JPC@4      END-EXEC.                                                    18930014
JPC@4 *                                                                 18940014
JPC@4      IF SQLCODE NOT = ZEROS                                       18950014
JPC@4         INITIALIZE   QGECABC                                      18960014
JPC@4         MOVE 'OPEN'        TO  ABC-REFERENCIA                     18970014
JPC@4         MOVE 'VLDTARC'     TO  ABC-OBJETO-ERROR                   18980014
JPC@4         PERFORM 999-ABEND-DB2                                     18990014
JPC@4      END-IF.                                                      19000014
JPC@4 *                                                                 19010014
JPC@4      INITIALIZE TB-CUENTAS-OFI.                                   19020014
JPC@4      MOVE ZEROS TO IN-01, IN-02, IN-03.                           19030014
JPC@4      PERFORM UNTIL SQLCODE NOT = ZEROS                            19040014
JPC@4                 OR IN-01       > 19                               19050014
JPC@4         EXEC SQL                                                  19060014
JPC@4              FETCH  VLDCARC                                       19070014
JPC@4               INTO :WARC-SUCURS                                   19080014
JPC@4         END-EXEC                                                  19090014
JPC@4 *                                                                 19100014
JPC@4         EVALUATE SQLCODE                                          19110014
JPC@4             WHEN ZEROS                                            19120014
JPC@4                  ADD  1             TO  IN-01                     19130014
JPC@4                  MOVE WARC-SUCURS   TO  TB-SUCURS (IN-01)         19140014
JPC@4             WHEN 100                                              19150014
JPC@4                  CONTINUE                                         19160014
JPC@4             WHEN OTHER                                            19170014
JPC@4                  INITIALIZE   QGECABC                             19180014
JPC@4                  MOVE 'FETCH'       TO  ABC-REFERENCIA            19190014
JPC@4                  MOVE 'VLDTARC'     TO  ABC-OBJETO-ERROR          19200014
JPC@4                  PERFORM 999-ABEND-DB2                            19210014
JPC@4         END-EVALUATE                                              19220014
JPC@4      END-PERFORM.                                                 19230014
JPC@4 *                                                                 19240014
JPC@4      EXEC SQL                                                     19250014
JPC@4           CLOSE VLDCARC                                           19260014
JPC@4      END-EXEC.                                                    19270014
JPC@4 *                                                                 19280014
JPC@4      IF SQLCODE NOT = ZEROS                                       19290014
JPC@4         INITIALIZE   QGECABC                                      19300014
JPC@4         MOVE 'CLOSE'       TO  ABC-REFERENCIA                     19310014
JPC@4         MOVE 'VLDTARC'     TO  ABC-OBJETO-ERROR                   19320014
JPC@4         PERFORM 999-ABEND-DB2                                     19330014
JPC@4      END-IF.                                                      19340014
JPC@4 *                                                                 19350014
JPC@4      IF TB-SUCURS (01) = ZEROS                                    19360014
JPC@4         CONTINUE                                                  19370014
JPC@4      ELSE                                                         19380014
JPC@4         PERFORM VARYING  IN-01 FROM 01 BY 01                      19390014
JPC@4                   UNTIL  IN-01    > 20                            19400014
JPC@4                      OR  TB-SUCURS (IN-01) = ZEROS                19410014
JPC@5            ADD  1             TO    IN-03                         19420014
JPC@4            IF SUC0101I = TB-SUCURS (IN-01)                        19430014
JPC@4 *JPC@5        MOVE 990        TO    IN-01                         19440014
JPC@5               ADD  1          TO    IN-02                         19450014
JPC@4            END-IF                                                 19460014
JPC@4         END-PERFORM                                               19470014
JPC@4 *JPC@5  IF IN-01 < 990                                            19480014
JPC@5         IF IN-02 > ZEROS                                          19490014
JPC@5         OR (IN-03 - IN-02) > ZEROS                                19500014
JPC@4            IF (CAA-CENTRO-CONT  = '0542'   AND                    19510014
JPC@4                REG0101I         = 'S'       )                     19520014
JPC@4                                                                   19530014
JPC@4            OR ((NCC0101I (11:02) = '16') AND                      19540014
JPC@4                                                                   19550014
JPC@4                ((CAA-CENTRO-CONT = '0567'      AND                19560014
JPC@4                 (ENT0101I = '0011' OR '0312'))  OR                19570014
JPC@4                                                                   19580014
JPC@4                 (CAA-CENTRO-CONT = '0542'     AND                 19590014
JPC@4                 (ENT0101I = '0069' OR '2010'))))                  19600014
JPC@4                CONTINUE                                           19610014
JPC@4            ELSE                                                   19620014
JPC@4               MOVE  -1           TO CTA0101L                      19630014
JPC@4               MOVE 'VLE2256'     TO CAA-COD-ERROR                 19640014
JPC@4               PERFORM 3-FINAL                                     19650014
JPC@4            END-IF                                                 19660014
JPC@4         END-IF                                                    19670014
JPC@4      END-IF.                                                      19680014
JPC@4 *                                                                 19690014
JPC@4  VALIDAR-OTRAS-CTAS-FIN.                                          19700014
JPC@4      EXIT.                                                        19710014
JPC@4  OBTENER-CTAGLOBAL.                                               19720014
JPC@4      MOVE '0069'      TO VXMI-CODBE                               19730014
JPC@4      EXEC SQL                                                     19740014
JPC@4           SELECT   VXMI_CTACARGO                                  19750014
JPC@4             INTO  :VXMI-CTACARGO                                  19760014
JPC@4             FROM  VLDTXMI                                         19770014
JPC@4            WHERE  VXMI_CODBE  = :VXMI-CODBE                       19780014
JPC@4      END-EXEC                                                     19790014
JPC@4 *                                                                 19800014
JPC@4      MOVE SQLCODE TO SQLCODE-AUX                                  19810014
JPC@4 *                                                                 19820014
JPC@4      EVALUATE TRUE                                                19830014
JPC@4          WHEN DB2-OK                                              19840014
JPC@4               MOVE VXMI-CTACARGO TO  CTAGLOB-COMM                 19850014
JPC@4          WHEN OTHER                                               19860014
JPC@4               MOVE 'SOLO@S.A.B.' TO  CTAGLOB-COMM                 19870014
JPC@4      END-EVALUATE.                                                19880014
      *                                                                 19890014
      *-------------------*                                             19900014
      *  FIN DE PROGRAMA  *                                             19910014
      *-------------------*                                             19920014
