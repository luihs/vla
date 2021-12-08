//*  %%INCLIB MBVP.PROD.SYMBOLS %%INCMEM GLOBALES                       00010008
//PVLP9SAB JOB (EEVL,1),'VL',                                           00020008
//             CLASS=C,                                                 00030008
//             MSGCLASS=0,                                              00040008
//             MSGLEVEL=(1,1),                                          00050008
//             REGION=0M                                                00060008
//*--------------------------------------------------------------------*00070008
//* PROYECTO     : 6762014060 - ESTADOS DE CUENTA DE BOLSA             *00080008
//* APLICATIVO   : VL (VALORES).                                       *00090008
//* PROCESO      : PVLP9SAB.                                           *00100008
//* PERIODICIDAD : MENSUAL                                             *00110008
//* OBJETIVO     : DESCARGA DE TABLAS DE ESTADOS DE CUENTAS DE BOLSA.  *00120008
//* FECHA        : 21 DE SEPTIEMBRE DEL 2021.                          *00130008
//* REALIZO      : LRH (LRIVERAH).                                     *00140008
//* D&D BANCO    :                                                     *00150008
//*--------------------------------------------------------------------*00160008
//*             L O G   D E   M O D I F I C A C I O N E S              *00170008
//*--------------------------------------------------------------------*00180008
//*   MARCA   AUTOR    FECHA   DESCRIPCION                             *00190008
//*   ------  ------  -------  -------------------------------------   *00200008
//*--------------------------------------------------------------------*00210008
//*  BIBLIOTECAS BASICAS DE LA APLICACION.                             *00220008
//*--------------------------------------------------------------------*00230008
//JOBLIB   DD DISP=SHR,DSN=PEBP.ALTAMIRA.LOADLIB.BATCH.EVENTUAL         00240008
//         DD DISP=SHR,DSN=PEBP.ALTAMIRA.LOADLIB.RUTINAS.EVENTUAL       00250008
//         DD DSN=PEBP.ALTAMIRA.LOADLIB.BATCH,DISP=SHR                  00260008
//         DD DSN=PEBP.ALTAMIRA.LOADLIB.RUTINAS,DISP=SHR                00270008
//         DD DSN=LDB2DPPA.SDSNEXIT,DISP=SHR                            00280008
//         DD DSN=LDB2DPPA.SDSNLOAD,DISP=SHR                            00290008
//         DD DSN=CEE.SCEERUN,DISP=SHR                                  00300008
//*--------------------------------------------------------------------*00310008
//* %%SET %%FECHAS  = %%$ODATE                                          00320008
//* %%SET %%ANO     = %%SUBSTR %%FECHAS 1 4                             00330008
//* %%SET %%MES     = %%SUBSTR %%FECHAS 5 2                             00340008
//* %%SET %%DIA     = 01                                                00350008
//* %%SET %%DIAHA   = %%SUBSTR %%FECHAS 7 2                             00360008
//* %%SET %%FECDES  = %%ANO.%%MES.%%DIA                                 00370008
//* %%SET %%FECDESE = '%%ANO-%%MES-%%DIA'                               00380008
//* %%SET %%FECHASE = '%%ANO-%%MES-%%DIAHA'                             00390008
//* SE ELIMINO TRATAMIENTO POR EL TRIMESTE COPIA ORIGINAL JCL VLJPEDCO  00400008
//*--------------------------------------------------------------------*00410008
//*                  >>>> VLJPEDC0 <<<<                                *00420008
//*--------------------------------------------------------------------*00430008
//* PASO     : VLEDC010                                                *00440008
//* PGRM/UTIL: ADUUMAIN                                                *00450008
//* DESCRIP  : DESCARGA DE TABLA VLDTARC CON CUENTAS ACTIVAS.          *00460008
//*--------------------------------------------------------------------*00470008
//VLEDC010 EXEC PGM=ADUUMAIN,                                           00480008
//             REGION=0M,                                               00490008
//             PARM='DSNP,VLDTARCB%%E,NEW,,MSGLEVEL(1)',COND=(0,NE)     00500008
//STEPLIB  DD  DISP=SHR,                                                00510008
//             DSN='BMC.DB2.LOAD'                                       00520008
//         DD  DISP=SHR,                                                00530008
//             DSN='LDB2DPPA.SDSNEXIT'                                  00540008
//         DD  DISP=SHR,                                                00550008
//             DSN='LDB2DPPA.SDSNLOAD'                                  00560008
//SORTWK01 DD  UNIT=3390,                                               00570008
//             SPACE=(CYL,(50,10),RLSE)                                 00580008
//SYSPRINT DD  SYSOUT=*                                                 00590008
//UTPRINT  DD  SYSOUT=*                                                 00600008
//SYSOUT   DD  SYSOUT=*                                                 00610008
//SYSTRACE DD  SYSOUT=*                                                 00620008
//SYSPUNCH DD  SYSOUT=*                                                 00630008
//SYSCNTL1 DD  DUMMY                                                    00640008
//SYSREC   DD  DSN=PEBP.VLFD.FIX.UNLOEECC.VLDTARC.D1%%ODATE,            00650008
//             DISP=(NEW,CATLG,DELETE),                                 00660008
//             SPACE=(CYL,(150,100),RLSE),                              00670008
//             RECFM=FB,                                                00680008
//             UNIT=3390,                                               00690008
//             BLKSIZE=0                                                00700008
//SYSIN    DD  *                                                        00710008
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    00720008
  SELECT * FROM                                                         00730008
  MBVP.VLDTARC                                                          00740008
  WHERE VARC_CUENTA IN (0296889, 0294471, 0295319, 0299278, 9686990,    00750008
    4890518, 9447191)                                                   00760008
  ORDER BY VARC_CUENTA                                                  00770008
//*--------------------------------------------------------------------*00780008
//* PASO     : VLEDC020                                                *00790008
//* PGRM/UTIL: ADUUMAIN                                                *00800008
//* DESCRIP  : DESCARGA DE TABLA VLDTADS.                              *00810008
//*--------------------------------------------------------------------*00820008
//VLEDC020 EXEC PGM=ADUUMAIN,                                           00830008
//             REGION=0M,                                               00840008
//             PARM='DSNP,VLDTADSB%%E,NEW,,MSGLEVEL(1)',COND=(0,NE)     00850008
//STEPLIB  DD  DISP=SHR,                                                00860008
//             DSN='BMC.DB2.LOAD'                                       00870008
//         DD  DISP=SHR,                                                00880008
//             DSN='LDB2DPPA.SDSNEXIT'                                  00890008
//         DD  DISP=SHR,                                                00900008
//             DSN='LDB2DPPA.SDSNLOAD'                                  00910008
//SORTWK01 DD  UNIT=3390,                                               00920008
//             SPACE=(CYL,(50,10),RLSE)                                 00930008
//SYSPRINT DD  SYSOUT=*                                                 00940008
//UTPRINT  DD  SYSOUT=*                                                 00950008
//SYSOUT   DD  SYSOUT=*                                                 00960008
//SYSTRACE DD  SYSOUT=*                                                 00970008
//SYSPUNCH DD  SYSOUT=*                                                 00980008
//SYSCNTL1 DD  DUMMY                                                    00990008
//SYSREC   DD  DSN=PEBP.VLFD.FIX.UNLOEECC.VLDTADS.D1%%ODATE,            01000008
//             DISP=(NEW,CATLG,DELETE),                                 01010008
//             SPACE=(CYL,(150,100),RLSE),                              01020008
//             RECFM=FB,                                                01030008
//             UNIT=3390,                                               01040008
//             BLKSIZE=0                                                01050008
//SYSIN    DD  *                                                        01060008
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    01070008
  SELECT * FROM                                                         01080008
  MBVP.VLDTADS                                                          01090008
  WHERE VADS_TIPREG = 'M'                                               01100008
  AND   VADS_CUENTA IN (0296889, 0294471, 0295319, 0299278, 9686990,    01110008
    4890518, 9447191)                                                   01120008
  ORDER BY VADS_CUENTA, VADS_PAVAL, VADS_VALOR, VADS_ISIN               01130008
//*--------------------------------------------------------------------*01140008
//* PASO     : VLEDC030                                                *01150008
//* PGRM/UTIL: ADUUMAIN                                                *01160008
//* DESCRIP  : DESCARGA DE TABLA VLDTMMO.                              *01170008
//*--------------------------------------------------------------------*01180008
//VLEDC030 EXEC PGM=ADUUMAIN,                                           01190008
//             REGION=0M,                                               01200008
//             PARM='DSNP,VLDTMMOB%%E,NEW,,MSGLEVEL(1)',COND=(0,NE)     01210008
//STEPLIB  DD  DISP=SHR,                                                01220008
//             DSN='BMC.DB2.LOAD'                                       01230008
//         DD  DISP=SHR,                                                01240008
//             DSN='LDB2DPPA.SDSNEXIT'                                  01250008
//         DD  DISP=SHR,                                                01260008
//             DSN='LDB2DPPA.SDSNLOAD'                                  01270008
//SORTWK01 DD  UNIT=3390,                                               01280008
//             SPACE=(CYL,(50,10),RLSE)                                 01290008
//SYSPRINT DD  SYSOUT=*                                                 01300008
//UTPRINT  DD  SYSOUT=*                                                 01310008
//SYSOUT   DD  SYSOUT=*                                                 01320008
//SYSTRACE DD  SYSOUT=*                                                 01330008
//SYSPUNCH DD  SYSOUT=*                                                 01340008
//SYSCNTL1 DD  DUMMY                                                    01350008
//SYSREC   DD  DSN=PEBP.VLFD.FIX.UNLOEECC.VLDTMMO.D1%%ODATE,            01360008
//             DISP=(NEW,CATLG,DELETE),                                 01370008
//             SPACE=(CYL,(150,100),RLSE),                              01380008
//             RECFM=FB,                                                01390008
//             UNIT=3390,                                               01400008
//             BLKSIZE=0                                                01410008
//SYSIN    DD  *                                                        01420008
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    01430008
  SELECT * FROM                                                         01440008
  MBVP.VLDTMMO                                                          01450008
  WHERE VMMO_CUENTA IN (0296889, 0294471, 0295319, 0299278, 9686990,    01460008
    4890518, 9447191)                                                   01470008
//*--------------------------------------------------------------------*01480008
//* PASO     : VLEDC040                                                *01490008
//* PGRM/UTIL: ADUUMAIN                                                *01500008
//* DESCRIP  : DESCARGA DE TABLA VLDTPOL. PARA MOVIMIENTO DE VALORES.  *01510008
//*--------------------------------------------------------------------*01520008
//VLEDC040 EXEC PGM=ADUUMAIN,                                           01530008
//             REGION=0M,                                               01540008
//             PARM='DSNP,VLDTPOLB%%E,NEW,,MSGLEVEL(1)',COND=(0,NE)     01550008
//STEPLIB  DD  DISP=SHR,                                                01560008
//             DSN='BMC.DB2.LOAD'                                       01570008
//         DD  DISP=SHR,                                                01580008
//             DSN='LDB2DPPA.SDSNEXIT'                                  01590008
//         DD  DISP=SHR,                                                01600008
//             DSN='LDB2DPPA.SDSNLOAD'                                  01610008
//SORTWK01 DD  UNIT=3390,                                               01620008
//             SPACE=(CYL,(50,10),RLSE)                                 01630008
//SYSPRINT DD  SYSOUT=*                                                 01640008
//UTPRINT  DD  SYSOUT=*                                                 01650008
//SYSOUT   DD  SYSOUT=*                                                 01660008
//SYSTRACE DD  SYSOUT=*                                                 01670008
//SYSPUNCH DD  SYSOUT=*                                                 01680008
//SYSCNTL1 DD  DUMMY                                                    01690008
//SYSREC   DD  DSN=PEBP.VLFD.FIX.UNLOEECC.VLDTPOL.D1%%ODATE,            01700008
//             DISP=(NEW,CATLG,DELETE),                                 01710008
//             SPACE=(CYL,(150,100),RLSE),                              01720008
//             RECFM=FB,                                                01730008
//             UNIT=3390,                                               01740008
//             BLKSIZE=0                                                01750008
//SYSIN    DD  *                                                        01760008
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    01770008
  SELECT * FROM                                                         01780008
  MBVP.VLDTPOL                                                          01790008
  WHERE VPOL_CUENTA IN (0296889, 0294471, 0295319, 0299278, 9686990,    01800008
    4890518, 9447191)                                                   01810008
    AND (VPOL_SITUAC <> 'OA')                                           01820008
    AND (VPOL_FVALOR <> 99999999)                                       01830008
    AND ((VPOL_FECHEJ >= %%FECDES AND VPOL_FECHEJ <= %%FECHAS)   OR     01840008
         ((VPOL_FVALOR >= %%FECDES AND VPOL_FVALOR <= %%FECHAS) AND     01850008
           VPOL_CLACONT IN (3, 6)))                                     01860008
//*--------------------------------------------------------------------*01870008
//* PASO     : VLEDC050                                                *01880008
//* PGRM/UTIL: ADUUMAIN                                                *01890008
//* DESCRIP  : DESCARGA DE TABLA VLDTPOL. PARA MOVIMIENTO ECONOMICO.   *01900008
//*--------------------------------------------------------------------*01910008
//VLEDC050 EXEC PGM=ADUUMAIN,                                           01920008
//             REGION=0M,                                               01930008
//             PARM='DSNP,VLDTPOLA%%E,NEW,,MSGLEVEL(1)',COND=(0,NE)     01940008
//STEPLIB  DD  DISP=SHR,                                                01950008
//             DSN='BMC.DB2.LOAD'                                       01960008
//         DD  DISP=SHR,                                                01970008
//             DSN='LDB2DPPA.SDSNEXIT'                                  01980008
//         DD  DISP=SHR,                                                01990008
//             DSN='LDB2DPPA.SDSNLOAD'                                  02000008
//SORTWK01 DD  UNIT=3390,                                               02010008
//             SPACE=(CYL,(50,10),RLSE)                                 02020008
//SYSPRINT DD  SYSOUT=*                                                 02030008
//UTPRINT  DD  SYSOUT=*                                                 02040008
//SYSOUT   DD  SYSOUT=*                                                 02050008
//SYSTRACE DD  SYSOUT=*                                                 02060008
//SYSPUNCH DD  SYSOUT=*                                                 02070008
//SYSCNTL1 DD  DUMMY                                                    02080008
//SYSREC   DD  DSN=PEBP.VLFD.FIX.UNLOEECC.POLMOVE.D1%%ODATE,            02090008
//             DISP=(NEW,CATLG,DELETE),                                 02100008
//             SPACE=(CYL,(150,100),RLSE),                              02110008
//             RECFM=FB,                                                02120008
//             UNIT=3390,                                               02130008
//             BLKSIZE=0                                                02140008
//SYSIN    DD  *                                                        02150008
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    02160008
  SELECT * FROM                                                         02170008
  MBVP.VLDTPOL                                                          02180008
  WHERE VPOL_CUENTA IN (0296889, 0294471, 0295319, 0299278, 9686990,    02190008
    4890518, 9447191)                                                   02200008
    AND (VPOL_SITUAC <> 'OA')                                           02210008
    AND ((VPOL_FECHEJ  > 20161231) OR (SUBSTR(VPOL_CTAPER,11,2) = '91'))02220008
    AND (VPOL_FVALOR <> 99999999)                                       02230008
    AND ((VPOL_FECHEJ >= %%FECDES AND VPOL_FECHEJ <= %%FECHAS)          02240008
     OR  (VPOL_FVALOR >= %%FECDES AND VPOL_FVALOR <= %%FECHAS))         02250008
//*--------------------------------------------------------------------*02260008
//* PASO     : VLEDC080                                                *02270008
//* PGRM/UTIL: ADUUMAIN                                                *02280008
//* DESCRIP  : DESCARGA DE TABLA VLDTSMM. PARA MOVIMIENTO ECONOMICO.   *02290008
//*--------------------------------------------------------------------*02300008
//VLEDC080 EXEC PGM=ADUUMAIN,                                           02310008
//             REGION=0M,                                               02320008
//             PARM='DSNP,VLDTSMMB%%E,NEW,,MSGLEVEL(1)',COND=(0,NE)     02330008
//STEPLIB  DD  DISP=SHR,                                                02340008
//             DSN='BMC.DB2.LOAD'                                       02350008
//         DD  DISP=SHR,                                                02360008
//             DSN='LDB2DPPA.SDSNEXIT'                                  02370008
//         DD  DISP=SHR,                                                02380008
//             DSN='LDB2DPPA.SDSNLOAD'                                  02390008
//SORTWK01 DD  UNIT=3390,                                               02400008
//             SPACE=(CYL,(50,10),RLSE)                                 02410008
//SYSPRINT DD  SYSOUT=*                                                 02420008
//UTPRINT  DD  SYSOUT=*                                                 02430008
//SYSOUT   DD  SYSOUT=*                                                 02440008
//SYSTRACE DD  SYSOUT=*                                                 02450008
//SYSPUNCH DD  SYSOUT=*                                                 02460008
//SYSCNTL1 DD  DUMMY                                                    02470008
//SYSREC   DD  DSN=PEBP.VLFD.FIX.UNLOEECC.VLDTSMM.D1%%ODATE,            02480008
//             DISP=(NEW,CATLG,DELETE),                                 02490008
//             SPACE=(CYL,(150,100),RLSE),                              02500008
//             RECFM=FB,                                                02510008
//             UNIT=3390,                                               02520008
//             BLKSIZE=0                                                02530008
//SYSIN    DD  *                                                        02540008
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    02550008
  SELECT * FROM                                                         02560008
  MBVP.VLDTSMM                                                          02570008
  WHERE VSMM_CTAVAL IN (                                                02580008
   '00110249049102968899',                                              02590008
   '00110178199102944719',                                              02600008
   '00110542849102953198',                                              02610008
   '00110766379102992787',                                              02620008
   '00110249910296869905',                                              02630008
   '00110660910248905188',                                              02640008
   '00110178910294471919')                                              02650008
/*                                                                      02660008
//*--------------------------------------------------------------------*02670008
//* PASO     : VLEDC090                                                *02680008
//* PGRM/UTIL: ADUUMAIN                                                *02690008
//* DESCRIP  : DESCARGA DE TABLA VLDTSMM. PARA MOVIMIENTO ECONOMICO.   *02700008
//*--------------------------------------------------------------------*02710008
//VLEDC090 EXEC PGM=ADUUMAIN,                                           02720008
//             REGION=0M,                                               02730008
//             PARM='DSNP,VLDTCOMB%%E,NEW,,MSGLEVEL(1)',COND=(0,NE)     02740008
//STEPLIB  DD  DISP=SHR,                                                02750008
//             DSN='BMC.DB2.LOAD'                                       02760008
//         DD  DISP=SHR,                                                02770008
//             DSN='LDB2DPPA.SDSNEXIT'                                  02780008
//         DD  DISP=SHR,                                                02790008
//             DSN='LDB2DPPA.SDSNLOAD'                                  02800008
//SORTWK01 DD  UNIT=3390,                                               02810008
//             SPACE=(CYL,(50,10),RLSE)                                 02820008
//SYSPRINT DD  SYSOUT=*                                                 02830008
//UTPRINT  DD  SYSOUT=*                                                 02840008
//SYSOUT   DD  SYSOUT=*                                                 02850008
//SYSTRACE DD  SYSOUT=*                                                 02860008
//SYSPUNCH DD  SYSOUT=*                                                 02870008
//SYSCNTL1 DD  DUMMY                                                    02880008
//SYSREC   DD  DSN=PEBP.VLFD.FIX.UNLOEECC.VLDTCOM.D1%%ODATE,            02890008
//             DISP=(NEW,CATLG,DELETE),                                 02900008
//             SPACE=(CYL,(150,100),RLSE),                              02910008
//             RECFM=FB,                                                02920008
//             UNIT=3390,                                               02930008
//             BLKSIZE=0                                                02940008
//SYSIN    DD  *                                                        02950008
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    02960008
  SELECT * FROM                                                         02970008
  MBVP.VLDTCOM                                                          02980008
/*                                                                      02990008
//* ============================================================        03000008
//*  JS * PASO OBLIGADO POR EL VL PARA EVENTUALES              *        03010008
//* ============================================================        03020008
//VLPAS000 EXEC PGM=IEFBR14,COND=(0,NE)                                 03030008
//*                                                                     03040008
//*--------------------------------------------------------------------*03050008
//* FIN DE JCL                                                         *03060008
//*--------------------------------------------------------------------*03070008
