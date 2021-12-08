      *=================================================================00010000
       IDENTIFICATION DIVISION.                                         00020000
      *=================================================================00030000
       PROGRAM-ID.    VL7CIF01.                                         00040001
       AUTHOR.        TATA CONSULTANCY SERVICE.                         00050001
       DATE-WRITTEN.  26/03/2019.                                       00060001
      ******************************************************************00070000
      *APLICATIVO   : VALORES                                          *00080011
      *TRANSACCION  : ESTADO DE CUENTA ON LINE                         *00090011
      *OBJETIVO     : OBTIENE INFORMACION DE POSICION DE VALORES       *00100011
      *                         RENTA  VARIABLE                        *00101011
      ******************************************************************00110000
      *REF.PETIC FECHA-MOD. PROGRAMADOR       DESCRIPCION              *00120000
      *--------- ---------- ----------------- -------------------------*00130000
  LRH *PSDAEMCIB 2-4  11-08-21 LUIS RIVERA H. ESTADOS DE CUENTA SALDO 0*00140011
      *----------------------------------------------------------------*00150000
      ******************************************************************00190000
      *                                                                 00200000
      *=================================================================00210000
       ENVIRONMENT DIVISION.                                            00220000
      *=================================================================00230000
       CONFIGURATION SECTION.                                           00240000
      *-----------------------------------------------------------------00250000
       SPECIAL-NAMES.                                                   00260000
      *--------------                                                   00270000
      *    DECIMAL-POINT IS COMMA.                                      00280000
                                                                        00290000
      *=================================================================00300000
       DATA DIVISION.                                                   00310000
      *=================================================================00320000
                                                                        00330000
      *-----------------------------------------------------------------00340000
       WORKING-STORAGE SECTION.                                         00350000
      *-----------------------------------------------------------------00360000
       01  WA-PROGRAMA             PIC X(08)        VALUE 'VL7CIF01'.   00370006
                                                                        00390000
      *-----------------------------------------------------------------00400000
      *    SWITCHES.                                                    00410000
      *-----------------------------------------------------------------00420000
       01  W-SWITCHES.                                                  00430010
           05  SW-ENCONTRO-XEN         PIC X(01).                       00450010
               88  SI-ENCONTRO-XEN                 VALUE 'S'.           00451010
               88  NO-ENCONTRO-XEN                 VALUE 'N'.           00452010
      *-----------------------------------------------------------------00460000
      *    CONSTANTES                                                   00470000
      *-----------------------------------------------------------------00480000
                                                                        00490000
      *-----------------------------------------------------------------00500000
      *    VARIABLES.                                                   00510000
      *-----------------------------------------------------------------00520000
       01  W-VARIABLES.                                                 00530000
           03 IX                       PIC 9(02)       VALUE ZEROS.     00540011
           03 W-SQLCODE-EDIT           PIC +ZZZZ       VALUE ZEROS.     00560000
                                                                        00730000
           03  WS-NEGLOT.                                               00931006
               05  WS-TIPNEG               PIC X(1) VALUE 'L'.          00932006
               05  WA-NEGLOT               PIC S9(07)V USAGE COMP-3.    00933006
      *                                                                 00940000
                                                                        01060000
      *-----------------------------------------------------------------01070000
      *    COPYS.                                                       01080000
      *-----------------------------------------------------------------01090000
           COPY VLTCHI2.                                                01100011
                                                                        01101011
      *-----------------------------------------------------------------01110000
      *    INCLUDES (DEFINICION DE TABLAS).                             01120000
      *-----------------------------------------------------------------01130000
           EXEC SQL INCLUDE SQLCA   END-EXEC.                           01140000
           EXEC SQL INCLUDE VLGTHIS END-EXEC.                           01150001
           EXEC SQL INCLUDE VLGTCAM END-EXEC.                           01151005
           EXEC SQL INCLUDE VLGTREL END-EXEC.                           01152007
           EXEC SQL INCLUDE VLGTXEN END-EXEC.                           01153007
                                                                        01160002
      *-----------------------------------------------------------------01161002
      *                   C U R S O R E S                              *01162005
      *-----------------------------------------------------------------01163002
           EXEC SQL                                                     01170002
                DECLARE VLDCHIS1  CURSOR FOR                            01170102
                SELECT  VHIS_CTAVAL     ,                               01171002
                        VHIS_CODVALOR   ,                               01172002
                        VHIS_TIPGAS     ,                               01173002
                        VHIS_ANO        ,                               01174002
                        VHIS_MES        ,                               01175002
                        VHIS_TITULOS1   ,                               01176002
                        VHIS_MOVIMI1    ,                               01177002
                        VHIS_CUSTODIA1  ,                               01178002
                        VHIS_CAMBIO1    ,                               01179002
                        VHIS_COBRADO1   ,                               01179102
                        VHIS_TITULOS2   ,                               01179202
                        VHIS_MOVIMI2    ,                               01179302
                        VHIS_CUSTODIA2  ,                               01179402
                        VHIS_CAMBIO2    ,                               01179502
                        VHIS_COBRADO2   ,                               01179602
                        VHIS_TITULOS3   ,                               01179702
                        VHIS_MOVIMI3    ,                               01179802
                        VHIS_CUSTODIA3  ,                               01179902
                        VHIS_CAMBIO3    ,                               01180002
                        VHIS_COBRADO3   ,                               01180102
                        VHIS_TITULOS4   ,                               01180202
                        VHIS_MOVIMI4    ,                               01180302
                        VHIS_CUSTODIA4  ,                               01180402
                        VHIS_CAMBIO4    ,                               01180502
                        VHIS_COBRADO4   ,                               01180602
                        VHIS_TITULOS5   ,                               01180702
                        VHIS_MOVIMI5    ,                               01180802
                        VHIS_CUSTODIA5  ,                               01180902
                        VHIS_CAMBIO5    ,                               01181002
                        VHIS_COBRADO5   ,                               01181102
                        VHIS_TITULOS6   ,                               01181202
                        VHIS_MOVIMI6    ,                               01181302
                        VHIS_CUSTODIA6  ,                               01181402
                        VHIS_CAMBIO6    ,                               01181502
                        VHIS_COBRADO6   ,                               01181602
                        VHIS_TITULOS7   ,                               01181702
                        VHIS_MOVIMI7    ,                               01181802
                        VHIS_CUSTODIA7  ,                               01181902
                        VHIS_CAMBIO7    ,                               01182002
                        VHIS_COBRADO7   ,                               01182102
                        VHIS_TITULOS8   ,                               01182202
                        VHIS_MOVIMI8    ,                               01182302
                        VHIS_CUSTODIA8  ,                               01182402
                        VHIS_CAMBIO8    ,                               01182502
                        VHIS_COBRADO8   ,                               01182602
                        VHIS_TITULOS9   ,                               01182702
                        VHIS_MOVIMI9    ,                               01182802
                        VHIS_CUSTODIA9  ,                               01182902
                        VHIS_CAMBIO9    ,                               01183002
                        VHIS_COBRADO9   ,                               01183102
                        VHIS_TITULOS10  ,                               01183202
                        VHIS_MOVIMI10   ,                               01183302
                        VHIS_CUSTODIA10 ,                               01183402
                        VHIS_CAMBIO10   ,                               01183502
                        VHIS_COBRADO10  ,                               01183602
                        VHIS_TITULOS11  ,                               01183702
                        VHIS_MOVIMI11   ,                               01183802
                        VHIS_CUSTODIA11 ,                               01183902
                        VHIS_CAMBIO11   ,                               01184002
                        VHIS_COBRADO11  ,                               01184102
                        VHIS_TITULOS12  ,                               01184202
                        VHIS_MOVIMI12   ,                               01184302
                        VHIS_CUSTODIA12 ,                               01184402
                        VHIS_CAMBIO12   ,                               01184502
                        VHIS_COBRADO12  ,                               01184602
                        VHIS_TITULOS13  ,                               01184702
                        VHIS_MOVIMI13   ,                               01184802
                        VHIS_CUSTODIA13 ,                               01184902
                        VHIS_CAMBIO13   ,                               01185002
                        VHIS_COBRADO13  ,                               01185102
                        VHIS_TITULOS14  ,                               01185202
                        VHIS_MOVIMI14   ,                               01185302
                        VHIS_CUSTODIA14 ,                               01185402
                        VHIS_CAMBIO14   ,                               01185502
                        VHIS_COBRADO14  ,                               01185602
                        VHIS_TITULOS15  ,                               01185702
                        VHIS_MOVIMI15   ,                               01185802
                        VHIS_CUSTODIA15 ,                               01185902
                        VHIS_CAMBIO15   ,                               01186002
                        VHIS_COBRADO15  ,                               01186102
                        VHIS_TITULOS16  ,                               01186202
                        VHIS_MOVIMI16   ,                               01186302
                        VHIS_CUSTODIA16 ,                               01186402
                        VHIS_CAMBIO16   ,                               01186502
                        VHIS_COBRADO16  ,                               01186602
                        VHIS_TITULOS17  ,                               01186702
                        VHIS_MOVIMI17   ,                               01186802
                        VHIS_CUSTODIA17 ,                               01186902
                        VHIS_CAMBIO17   ,                               01187002
                        VHIS_COBRADO17  ,                               01187102
                        VHIS_TITULOS18  ,                               01187202
                        VHIS_MOVIMI18   ,                               01187302
                        VHIS_CUSTODIA18 ,                               01187402
                        VHIS_CAMBIO18   ,                               01187502
                        VHIS_COBRADO18  ,                               01187602
                        VHIS_TITULOS19  ,                               01187702
                        VHIS_MOVIMI19   ,                               01187802
                        VHIS_CUSTODIA19 ,                               01187902
                        VHIS_CAMBIO19   ,                               01188002
                        VHIS_COBRADO19  ,                               01188102
                        VHIS_TITULOS20  ,                               01188202
                        VHIS_MOVIMI20   ,                               01188302
                        VHIS_CUSTODIA20 ,                               01188402
                        VHIS_CAMBIO20   ,                               01188502
                        VHIS_COBRADO20  ,                               01188602
                        VHIS_TITULOS21  ,                               01188702
                        VHIS_MOVIMI21   ,                               01188802
                        VHIS_CUSTODIA21 ,                               01188902
                        VHIS_CAMBIO21   ,                               01189002
                        VHIS_COBRADO21  ,                               01189102
                        VHIS_TITULOS22  ,                               01189202
                        VHIS_MOVIMI22   ,                               01189302
                        VHIS_CUSTODIA22 ,                               01189402
                        VHIS_CAMBIO22   ,                               01189502
                        VHIS_COBRADO22  ,                               01189602
                        VHIS_TITULOS23  ,                               01189702
                        VHIS_MOVIMI23   ,                               01189802
                        VHIS_CUSTODIA23 ,                               01189902
                        VHIS_CAMBIO23   ,                               01190002
                        VHIS_COBRADO23  ,                               01190102
                        VHIS_TITULOS24  ,                               01190202
                        VHIS_MOVIMI24   ,                               01190302
                        VHIS_CUSTODIA24 ,                               01190402
                        VHIS_CAMBIO24   ,                               01190502
                        VHIS_COBRADO24  ,                               01190602
                        VHIS_TITULOS25  ,                               01190702
                        VHIS_MOVIMI25   ,                               01190802
                        VHIS_CUSTODIA25 ,                               01190902
                        VHIS_CAMBIO25   ,                               01191002
                        VHIS_COBRADO25  ,                               01191102
                        VHIS_TITULOS26  ,                               01191202
                        VHIS_MOVIMI26   ,                               01191302
                        VHIS_CUSTODIA26 ,                               01191402
                        VHIS_CAMBIO26   ,                               01191502
                        VHIS_COBRADO26  ,                               01191602
                        VHIS_TITULOS27  ,                               01191702
                        VHIS_MOVIMI27   ,                               01191802
                        VHIS_CUSTODIA27 ,                               01191902
                        VHIS_CAMBIO27   ,                               01192002
                        VHIS_COBRADO27  ,                               01192102
                        VHIS_TITULOS28  ,                               01192202
                        VHIS_MOVIMI28   ,                               01192302
                        VHIS_CUSTODIA28 ,                               01192402
                        VHIS_CAMBIO28   ,                               01192502
                        VHIS_COBRADO28  ,                               01192602
                        VHIS_TITULOS29  ,                               01192702
                        VHIS_MOVIMI29   ,                               01192802
                        VHIS_CUSTODIA29 ,                               01192902
                        VHIS_CAMBIO29   ,                               01193002
                        VHIS_COBRADO29  ,                               01193102
                        VHIS_TITULOS30  ,                               01193202
                        VHIS_MOVIMI30   ,                               01193302
                        VHIS_CUSTODIA30 ,                               01193402
                        VHIS_CAMBIO30   ,                               01193502
                        VHIS_COBRADO30  ,                               01193602
                        VHIS_TITULOS31  ,                               01193702
                        VHIS_MOVIMI31   ,                               01193802
                        VHIS_CUSTODIA31 ,                               01193902
                        VHIS_CAMBIO31   ,                               01194002
                        VHIS_COBRADO31  ,                               01194102
                        VHIS_FEALTREG   ,                               01194202
                        VHIS_FEULMOD    ,                               01194302
                        VHIS_HORULMOD   ,                               01194402
                        VHIS_NUMTER     ,                               01194502
                        VHIS_USUARIO                                    01194602
                    FROM   VLDTHIS                                      01194802
                    WHERE  VHIS_CTAVAL = :VHIS-CTAVAL                   01194902
                    AND    VHIS_TIPGAS =  55                            01195002
                    AND    VHIS_ANO    = :VHIS-ANO                      01195102
                    AND    VHIS_MES    = :VHIS-MES                      01195202
           END-EXEC.                                                    01195305
                                                                        01195405
           EXEC SQL                                                     01195505
                DECLARE VLDCCAM  CURSOR FOR                             01195605
                 SELECT VCAM_CIERRE_D                                   01195705
                      , VCAM_FECDIA                                     01195805
                      , VCAM_FILLER                                     01195905
                   FROM VLDTCAM                                         01196005
                  WHERE VCAM_CODVALOR  = :VCAM-CODVALOR                 01196105
                    AND VCAM_FECDIA   <= :VCAM-FECDIA                   01196205
                    AND VCAM_CIERRE_D <>  0                             01196305
                  ORDER BY VCAM_FECDIA DESC                             01196405
                  OPTIMIZE FOR 1 ROW                                    01196505
           END-EXEC.                                                    01196605
                                                                        01196705
      *---------------*                                                 01196800
       LINKAGE SECTION.                                                 01196900
      *---------------*                                                 01197000
       01  DFHCOMMAREA.                                                 01200000
           COPY VLWCIF01.                                               01210001
      *-----------------------------------------------------------------01220000
                                                                        01230000
      *==================                                               01240000
       PROCEDURE DIVISION.                                              01250000
      *==================                                               01260000
           PERFORM 1000-INICIO                                          01270002
           PERFORM 2000-PROCESO                                         01280002
           PERFORM 9999-FIN.                                            01290002
                                                                        01300000
      *---------------------------------------------------------------- 01310000
       1000-INICIO.                                                     01320002
      *---------------------------------------------------------------- 01330000
           INITIALIZE      IF01-DATOS-SALIDA.                           01340003
                                                                        01350000
           MOVE '00'                TO  IF01-COD-RETORNO.               01360011
           MOVE ZEROS               TO  IF01-TOT-MTO-INV                01361011
                                        IF01-TOT-VAL-MER                01362011
                                        IF01-TOT-BEN-PER                01363011
           .                                                            01370011
      *                                                                *01910000
      *----------------------------------------------------------------*01920000
       2000-PROCESO.                                                    01930002
      *----------------------------------------------------------------*01940000
      *                                                                *01950000
           PERFORM  2010-OPEN-VLDTHIS.                                  01950102
           PERFORM  2020-FETCH-VLDTHIS.                                 01950202
           PERFORM UNTIL IX NOT LESS 50                                 01956211
                    OR   SQLCODE NOT = 0                                01956311
               PERFORM  2030-MUEVE-DATOS                                01956411
               PERFORM  2020-FETCH-VLDTHIS                              01956511
           END-PERFORM                                                  01956611
           PERFORM  2040-CLOSE-VLDTHIS                                  01956711
           MOVE  IX      TO  IF01-NUM-OCCURS                            01956811
           .                                                            01956911
      *                                                                *02150000
      *-----------------------------------------------------------------02160000
       2010-OPEN-VLDTHIS.                                               02170002
      *-----------------------------------------------------------------02180000
      *                                                                *02190000
           INITIALIZE DCLVLDTHIS                                        02190402
           MOVE IF01-CTAVAL    TO VHIS-CTAVAL                           02190702
           MOVE IF01-ANO       TO VHIS-ANO                              02190802
           MOVE IF01-MES       TO VHIS-MES                              02190902
                                                                        02191002
           EXEC SQL                                                     02191102
              OPEN VLDCHIS1                                             02191202
           END-EXEC                                                     02191302
                                                                        02191402
           EVALUATE SQLCODE                                             02191502
             WHEN (ZERO)                                                02191602
                CONTINUE                                                02191702
             WHEN OTHER                                                 02191802
                    MOVE '99'                  TO IF01-COD-RETORNO      02192211
                    MOVE SQLCODE               TO W-SQLCODE-EDIT        02192311
                    MOVE 'VLE1000'             TO IF01-COD-ERROR-DEV    02192411
                    MOVE 'VLDTHIS '            TO IF01-VAR1-ERROR       02192511
                    MOVE W-SQLCODE-EDIT        TO IF01-VAR2-ERROR       02192611
                    PERFORM 9999-FIN                                    02192811
             END-EVALUATE.                                              02192902
      *                                                                *02193002
      *-----------------------------------------------------------------02193102
       2020-FETCH-VLDTHIS.                                              02193202
      *-----------------------------------------------------------------02193302
      *                                                                *02193402
           EXEC SQL                                                     02193503
                FETCH  VLDCHIS1                                         02193603
                 INTO :DCLVLDTHIS                                       02193703
           END-EXEC                                                     02194003
                                                                        02195002
      *                                                                *02370000
           EVALUATE SQLCODE                                             02380000
               WHEN 0                                                   02390000
                    MOVE DCLVLDTHIS            TO DCLVLTCHIS            02390103
               WHEN 100                                                 02391003
                    CONTINUE                                            02400000
               WHEN OTHER                                               02470000
                    MOVE '99'                  TO IF01-COD-RETORNO      02480005
                    MOVE SQLCODE               TO W-SQLCODE-EDIT        02490000
                    MOVE 'VLE1000'             TO IF01-COD-ERROR-DEV    02500011
                    MOVE 'VLDTHIS '            TO IF01-VAR1-ERROR       02510011
                    MOVE W-SQLCODE-EDIT        TO IF01-VAR2-ERROR       02520011
                    PERFORM 9999-FIN                                    02550011
           END-EVALUATE.                                                02560000
                                                                        02570000
      *                                                                *02580004
      *-----------------------------------------------------------------02590004
       2030-MUEVE-DATOS.                                                02600004
      *-----------------------------------------------------------------02610004
      *                                                                *02620004
           PERFORM 2031-LEER-VLDTXEN                                    02621007
           IF  SI-ENCONTRO-XEN                                          02621111
              ADD  1          TO IX                                     02621211
              PERFORM 2032-PRECIO-MERCADO                               02621311
                                                                        02622005
   LRH      IF CVHIS-CUSTODIA (IF01-DIA) NOT= 0                         02623013
              MOVE VXEN-CODDIVI TO IF01-MONEDA                          02631011
              MOVE VXEN-NEMOTEC TO IF01-NEMOTEC (IX)                    02640011
              MOVE CVHIS-CUSTODIA (IF01-DIA)    TO IF01-VAL-POSIC (IX)  02670011
              MOVE CVHIS-CAMBIO   (IF01-DIA)    TO IF01-VAL-DISPO (IX)  02680011
              COMPUTE IF01-VAL-NDISP(IX) = CVHIS-CUSTODIA (IF01-DIA) -  02682011
                                           CVHIS-CAMBIO   (IF01-DIA)    02683011
              MOVE CVHIS-MOVIMI (IF01-DIA)    TO IF01-PRE-PROM (IX)     02690011
              MOVE VCAM-CIERRE-D              TO IF01-PRE-MERCA (IX)    02691011
              COMPUTE IF01-MTO-INVER(IX) = CVHIS-CUSTODIA (IF01-DIA) *  02700011
                                           CVHIS-MOVIMI (IF01-DIA)      02700111
              COMPUTE IF01-VAL-MERCA(IX) = CVHIS-CUSTODIA (IF01-DIA) *  02701111
                                           VCAM-CIERRE-D                02701211
              COMPUTE IF01-MTO-BEN-PER(IX) = IF01-VAL-MERCA(IX) -       02702011
                                             IF01-MTO-INVER(IX)         02702111
              COMPUTE IF01-TOT-MTO-INV = IF01-TOT-MTO-INV +             02703011
                                         IF01-MTO-INVER(IX)             02704011
              COMPUTE IF01-TOT-VAL-MER = IF01-TOT-VAL-MER +             02705011
                                         IF01-VAL-MERCA(IX)             02706011
              COMPUTE IF01-TOT-BEN-PER = IF01-TOT-BEN-PER +             02730011
                                         IF01-MTO-BEN-PER(IX)           02740011
   LRH      END-IF                                                      02740112
           END-IF                                                       02741011
           .                                                            02750011
                                                                        02760011
      *-----------------------------------------------------------------02820005
       2031-LEER-VLDTXEN.                                               02830007
      *-----------------------------------------------------------------02840005
      *                                                                *02850005
           MOVE CVHIS-CODVALOR                TO VREL-CODVALOR          02860011
                                                                        02860108
           EXEC SQL                                                     02860207
               SELECT                                                   02860307
                     VREL_CODVALOR,                                     02860407
                     VREL_INDICFIS,                                     02860507
                     VREL_CODVALEQ                                      02860607
                 INTO                                                   02860707
                    :VREL-CODVALOR,                                     02860807
                    :VREL-INDICFIS,                                     02860907
                    :VREL-CODVALEQ                                      02861007
                 FROM VLDTREL                                           02861107
                WHERE VREL_CODVALOR = :VREL-CODVALOR                    02861207
           END-EXEC                                                     02861307
                                                                        02861407
           EVALUATE SQLCODE                                             02861507
               WHEN +000                                                02861607
                    IF VREL-INDICFIS = 'D'                              02861707
                       MOVE VREL-CODVALOR (01:03) TO VXEN-PAVAL         02861807
                       MOVE VREL-CODVALOR (04:08) TO VXEN-VALOR         02861907
                       MOVE VREL-CODVALOR (12:01) TO VXEN-ISIN          02862007
                    ELSE                                                02862107
                       MOVE VREL-CODVALEQ (01:03) TO VXEN-PAVAL         02862207
                       MOVE VREL-CODVALEQ (04:08) TO VXEN-VALOR         02862307
                       MOVE VREL-CODVALEQ (12:01) TO VXEN-ISIN          02862407
                    END-IF                                              02862507
               WHEN OTHER                                               02862607
                    MOVE '99'                  TO IF01-COD-RETORNO      02862707
                    MOVE SQLCODE               TO W-SQLCODE-EDIT        02862807
                    MOVE 'VLE1000'             TO IF01-COD-ERROR-DEV    02862911
                    MOVE 'VLDTREL '            TO IF01-VAR1-ERROR       02863011
                    MOVE W-SQLCODE-EDIT        TO IF01-VAR2-ERROR       02863111
                    PERFORM 9999-FIN                                    02863311
           END-EVALUATE.                                                02863407
                                                                        02863507
           EXEC SQL                                                     02863608
               SELECT                                                   02863708
                      VXEN_NOMINEM,                                     02863808
                      VXEN_NEMOTEC,                                     02863908
                      VXEN_CODDIVI,                                     02864008
                      VXEN_TIPINT                                       02864108
                 INTO                                                   02864208
                     :VXEN-NOMINEM,                                     02864308
                     :VXEN-NEMOTEC,                                     02864408
                     :VXEN-CODDIVI,                                     02864508
                     :VXEN-TIPINT                                       02864608
                 FROM VLDTXEN                                           02864708
                WHERE VXEN_PAVAL  = :VXEN-PAVAL                         02864808
                  AND VXEN_VALOR  = :VXEN-VALOR                         02864908
                  AND VXEN_ISIN   = :VXEN-ISIN                          02865008
           END-EXEC                                                     02865108
                                                                        02865208
           EVALUATE SQLCODE                                             02865308
               WHEN +000                                                02865408
                   IF VXEN-TIPINT = 'V'                                 02865508
                      SET SI-ENCONTRO-XEN      TO TRUE                  02865608
                   ELSE                                                 02865708
                      SET NO-ENCONTRO-XEN      TO TRUE                  02865808
                   END-IF                                               02865908
               WHEN +100                                                02866008
                   SET NO-ENCONTRO-XEN         TO TRUE                  02866108
               WHEN OTHER                                               02866208
                    MOVE '99'                  TO IF01-COD-RETORNO      02866408
                    MOVE SQLCODE               TO W-SQLCODE-EDIT        02866508
                    MOVE 'VLE1000'             TO IF01-COD-ERROR-DEV    02866611
                    MOVE 'VLDTXEN '            TO IF01-VAR1-ERROR       02866711
                    MOVE W-SQLCODE-EDIT        TO IF01-VAR2-ERROR       02866811
                    PERFORM 9999-FIN                                    02867011
           END-EVALUATE.                                                02867108
      *                                                                 02867208
      *-----------------------------------------------------------------02867307
       2032-PRECIO-MERCADO.                                             02867411
      *-----------------------------------------------------------------02867507
      *                                                                *02867607
           MOVE CVHIS-CODVALOR    TO VCAM-CODVALOR                      02867711
           MOVE IF01-FECHA-N      TO VCAM-FECDIA                        02868011
                                                                        02869009
           EXEC SQL                                                     02870005
               OPEN VLDCCAM                                             02880005
           END-EXEC                                                     02890005
                                                                        02900005
           EVALUATE SQLCODE                                             02910005
               WHEN +000                                                02920005
                   CONTINUE                                             02930005
               WHEN OTHER                                               02940005
                    MOVE '99'                  TO IF01-COD-RETORNO      03011005
                    MOVE SQLCODE               TO W-SQLCODE-EDIT        03012005
                    MOVE 'VLE1000'             TO IF01-COD-ERROR-DEV    03013011
                    MOVE 'VLDTCAM '            TO IF01-VAR1-ERROR       03014011
                    MOVE W-SQLCODE-EDIT        TO IF01-VAR2-ERROR       03015011
                    PERFORM 9999-FIN                                    03017011
           END-EVALUATE                                                 03020005
                                                                        03450000
           EXEC SQL                                                     03451005
               FETCH VLDCCAM                                            03452005
               INTO :VCAM-CIERRE-D,                                     03453005
                    :VCAM-FECDIA,                                       03454005
                    :VCAM-FILLER                                        03455005
           END-EXEC                                                     03456005
                                                                        03457005
           EVALUATE SQLCODE                                             03458005
               WHEN +000                                                03459005
                   MOVE VCAM-FILLER(01:05)       TO WS-NEGLOT           03459105
                   IF WS-TIPNEG = 'L' AND WA-NEGLOT > 0                 03459205
                      COMPUTE VCAM-CIERRE-D = VCAM-CIERRE-D / WA-NEGLOT 03459305
                   END-IF                                               03459405
               WHEN +100                                                03459505
                        MOVE VXEN-NOMINEM      TO VCAM-CIERRE-D         03459711
               WHEN OTHER                                               03459905
                    MOVE '99'                  TO IF01-COD-RETORNO      03460005
                    MOVE SQLCODE               TO W-SQLCODE-EDIT        03460105
                    MOVE 'VLE1000'             TO IF01-COD-ERROR-DEV    03460211
                    MOVE 'VLDTCAM '            TO IF01-VAR1-ERROR       03460311
                    MOVE W-SQLCODE-EDIT        TO IF01-VAR2-ERROR       03460411
                    PERFORM 9999-FIN                                    03460611
           END-EVALUATE                                                 03461205
                                                                        03461305
           EXEC SQL                                                     03461405
               CLOSE VLDCCAM                                            03461505
           END-EXEC                                                     03461605
                                                                        03461705
           EVALUATE SQLCODE                                             03461805
               WHEN +000                                                03461905
                   CONTINUE                                             03462005
               WHEN OTHER                                               03462105
                    MOVE '99'                  TO IF01-COD-RETORNO      03462205
                    MOVE SQLCODE               TO W-SQLCODE-EDIT        03462305
                    MOVE 'VLE1000'             TO IF01-COD-ERROR-DEV    03462411
                    MOVE 'VLDTCAM '            TO IF01-VAR1-ERROR       03462511
                    MOVE W-SQLCODE-EDIT        TO IF01-VAR2-ERROR       03462611
                    PERFORM 9999-FIN                                    03462811
           END-EVALUATE.                                                03462905
      *                                                                 03463011
      *-----------------------------------------------------------------03463111
       2040-CLOSE-VLDTHIS.                                              03463211
      *-----------------------------------------------------------------03463311
                                                                        03463411
           EXEC SQL                                                     03463511
               CLOSE VLDCHIS1                                           03463611
           END-EXEC                                                     03463711
                                                                        03463811
           EVALUATE SQLCODE                                             03463911
               WHEN +000                                                03464011
                   CONTINUE                                             03464111
               WHEN OTHER                                               03464211
                    MOVE '99'                  TO IF01-COD-RETORNO      03464311
                    MOVE SQLCODE               TO W-SQLCODE-EDIT        03464411
                    MOVE 'VLE1000'             TO IF01-COD-ERROR-DEV    03464511
                    MOVE 'VLDTHIS '            TO IF01-VAR1-ERROR       03464611
                    MOVE W-SQLCODE-EDIT        TO IF01-VAR2-ERROR       03464711
                    PERFORM 9999-FIN                                    03464911
           END-EVALUATE.                                                03465011
      *                                                                *03465111
      *----------------------------------------------------------------*03466000
       9999-FIN.                                                        03470011
      *----------------------------------------------------------------*03480000
           GOBACK.                                                      03490000
      *=================================================================03500000
      *   FIN DEL PROGRAMA                                              03510000
      *=================================================================03520000
