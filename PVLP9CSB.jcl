//*  %%INCLIB MBVP.PROD.SYMBOLS %%INCMEM GLOBALES                       00010024
//PVLP9CSB JOB (EEVL,1),'VL',                                           00020026
//             CLASS=C,                                                 00030024
//             MSGCLASS=0,                                              00040024
//             MSGLEVEL=(1,1),                                          00050024
//             REGION=0M                                                00060024
//*--------------------------------------------------------------------*00070024
//* PROYECTO     : AJUSTE DE COMISION SAB                              *00080024
//* APLICATIVO   : VL (VALORES).                                       *00090024
//* OBJETIVO     : GENERACION DE REPORTE PARA AJUSTE DE COMISION SAB.  *00100024
//* FECHA        : 26 DE SEPTIEMBRE DEL 2021.                          *00110024
//* REALIZO      : LRH (LRIVERAH).                                     *00120024
//*--------------------------------------------------------------------*00130024
//*             L O G   D E   M O D I F I C A C I O N E S              *00140024
//*--------------------------------------------------------------------*00150024
//*   MARCA   AUTOR    FECHA   DESCRIPCION                             *00160024
//*   ------  ------  -------  -------------------------------------   *00170024
//*--------------------------------------------------------------------*00180024
//*  BIBLIOTECAS BASICAS DE LA APLICACION.                             *00190024
//*--------------------------------------------------------------------*00200024
//JOBLIB   DD DISP=SHR,DSN=PEBP.ALTAMIRA.LOADLIB.BATCH.EVENTUAL         00210024
//         DD DISP=SHR,DSN=PEBP.ALTAMIRA.LOADLIB.RUTINAS.EVENTUAL       00220024
//         DD DSN=PEBP.ALTAMIRA.LOADLIB.BATCH,DISP=SHR                  00230024
//         DD DSN=PEBP.ALTAMIRA.LOADLIB.RUTINAS,DISP=SHR                00240024
//         DD DSN=LDB2DPPA.SDSNEXIT,DISP=SHR                            00250024
//         DD DSN=LDB2DPPA.SDSNLOAD,DISP=SHR                            00260024
//         DD DSN=CEE.SCEERUN,DISP=SHR                                  00270024
//*--------------------------------------------------------------------*00280024
//* %%SET %%FECHAS  = %%$ODATE                                          00290024
//* %%SET %%ANO     = %%SUBSTR %%FECHAS 1 4                             00300024
//* %%SET %%MES     = %%SUBSTR %%FECHAS 5 2                             00310024
//* %%SET %%DIA     = 01                                                00320024
//* %%SET %%DIAHA   = %%SUBSTR %%FECHAS 7 2                             00330024
//* %%SET %%FECDES  = %%ANO.%%MES.%%DIA                                 00340024
//* %%SET %%FECDESE = '%%ANO-%%MES-%%DIA'                               00350024
//* %%SET %%FECHASE = '%%ANO-%%MES-%%DIAHA'                             00360024
//**********************************************************************00370024
//* BORRADO DE FICHERO                                                 *00380024
//**********************************************************************00390024
//VLDEL001  EXEC PGM=IDCAMS                                             00400024
//SYSPRINT DD  SYSOUT=*                                                 00410024
//SYSIN    DD  *                                                        00420024
        DELETE PEBP.VLFD.FIX.UNLOCSAB.VLDTCOM.D1%%ODATE    PURGE        00430024
        DELETE PEBP.VLFD.FIX.UNLOCSAB.VLDTARC.D1%%ODATE    PURGE        00440024
        DELETE PEBP.VLFD.FVM01.REPOR.NEGOSB                PURGE        00450025
        IF MAXCC = 8 THEN SET MAXCC = 0                                 00460024
//*--------------------------------------------------------------------*00470024
//* PASO     : VLEDC010                                                *00480024
//* PGRM/UTIL: ADUUMAIN                                                *00490024
//* DESCRIP  : DESCARGA DE TABLA VLDTCOM CON CUENTAS ACTIVAS.          *00500024
//*--------------------------------------------------------------------*00510024
//VLEDC010 EXEC PGM=ADUUMAIN,                                           00520024
//             REGION=0M,                                               00530024
//             PARM='DSNP,VLDTCOMB%%E,NEW,,MSGLEVEL(1)',COND=(0,NE)     00540024
//STEPLIB  DD  DISP=SHR,                                                00550024
//             DSN='BMC.DB2.LOAD'                                       00560024
//         DD  DISP=SHR,                                                00570024
//             DSN='LDB2DPPA.SDSNEXIT'                                  00580024
//         DD  DISP=SHR,                                                00590024
//             DSN='LDB2DPPA.SDSNLOAD'                                  00600024
//SORTWK01 DD  UNIT=3390,                                               00610024
//             SPACE=(CYL,(50,10),RLSE)                                 00620024
//SYSPRINT DD  SYSOUT=*                                                 00630024
//UTPRINT  DD  SYSOUT=*                                                 00640024
//SYSOUT   DD  SYSOUT=*                                                 00650024
//SYSTRACE DD  SYSOUT=*                                                 00660024
//SYSPUNCH DD  SYSOUT=*                                                 00670024
//SYSCNTL1 DD  DUMMY                                                    00680024
//SYSREC   DD  DSN=PEBP.VLFD.FIX.UNLOCSAB.VLDTCOM.D1%%ODATE,            00690024
//             DISP=(NEW,CATLG,DELETE),                                 00700024
//             SPACE=(CYL,(150,100),RLSE),                              00710024
//             RECFM=FB,                                                00720024
//             UNIT=3390,                                               00730024
//             BLKSIZE=0                                                00740024
//SYSIN    DD  *                                                        00750024
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    00760024
  SELECT * FROM                                                         00770024
  MBVP.VLDTCOM                                                          00780024
  WHERE VCOM_SITUAC = 'A'                                               00790024
  ORDER BY VCOM_CUENTA, VCOM_CLACONT                                    00800024
//*--------------------------------------------------------------------*00810024
//* PASO     : VLEDC020                                                *00820024
//* PGRM/UTIL: ADUUMAIN                                                *00830024
//* DESCRIP  : DESCARGA DE TABLA VLDTARC.                              *00840024
//*--------------------------------------------------------------------*00850024
//VLEDC020 EXEC PGM=ADUUMAIN,                                           00860024
//             REGION=0M,                                               00870024
//             PARM='DSNP,VLDTARCB%%E,NEW,,MSGLEVEL(1)',COND=(0,NE)     00880024
//STEPLIB  DD  DISP=SHR,                                                00890024
//             DSN='BMC.DB2.LOAD'                                       00900024
//         DD  DISP=SHR,                                                00910024
//             DSN='LDB2DPPA.SDSNEXIT'                                  00920024
//         DD  DISP=SHR,                                                00930024
//             DSN='LDB2DPPA.SDSNLOAD'                                  00940024
//SORTWK01 DD  UNIT=3390,                                               00950024
//             SPACE=(CYL,(50,10),RLSE)                                 00960024
//SYSPRINT DD  SYSOUT=*                                                 00970024
//UTPRINT  DD  SYSOUT=*                                                 00980024
//SYSOUT   DD  SYSOUT=*                                                 00990024
//SYSTRACE DD  SYSOUT=*                                                 01000024
//SYSPUNCH DD  SYSOUT=*                                                 01010024
//SYSCNTL1 DD  DUMMY                                                    01020024
//SYSREC   DD  DSN=PEBP.VLFD.FIX.UNLOCSAB.VLDTARC.D1%%ODATE,            01030024
//             DISP=(NEW,CATLG,DELETE),                                 01040024
//             SPACE=(CYL,(150,100),RLSE),                              01050024
//             RECFM=FB,                                                01060024
//             UNIT=3390,                                               01070024
//             BLKSIZE=0                                                01080024
//SYSIN    DD  *                                                        01090024
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    01100024
  SELECT * FROM                                                         01110024
  MBVP.VLDTARC                                                          01120024
  WHERE VARC_SITUAC = 'A'                                               01130024
  ORDER BY VARC_CUENTA, VARC_CENTAD                                     01140024
//**********************************************************************01150024
//* GENERA REPORTE NEGOCIASION EN BOLSA                                *01160024
//**********************************************************************01170024
//VLEDC030  EXEC  PGM=IKJEFT1A,DYNAMNBR=20,COND=(0,NE)                  01180024
//SYSOUT    DD SYSOUT=*                                                 01190024
//SYSPRINT  DD SYSOUT=*                                                 01200024
//SYSABOUT  DD SYSOUT=*                                                 01210024
//SYSDBOUT  DD SYSOUT=*                                                 01220024
//SYSTSPRT  DD SYSOUT=*                                                 01230024
//SYSUDUMP  DD SYSOUT=*                                                 01240024
//E1VDTARC  DD DSN=PEBP.VLFD.FIX.UNLOCSAB.VLDTARC.D1%%ODATE,DISP=SHR    01260026
//S1RNEGBL  DD DSN=PEBP.VLFD.FVM01.REPOR.NEGOSB,                        01270025
//             DISP=(NEW,CATLG,DELETE),                                 01280024
//             SPACE=(CYL,(50,25),RLSE),UNIT=3390,                      01290024
//             DCB=(RECFM=FB,LRECL=159,BLKSIZE=0,DSORG=PS)              01300024
//SYSTSIN  DD  *                                                        01310024
   DSN SYSTEM(DSNP) RETRY(0) TEST (0)                                   01320024
   RUN PROGRAM(VL4CCACS) PLAN(BVPVLPB)                                  01330026
   END                                                                  01340024
/*                                                                      01350024
//* ============================================================        01360024
//*  JS * PASO OBLIGADO POR EL VL PARA EVENTUALES              *        01370024
//* ============================================================        01380024
//VLPAS000 EXEC PGM=IEFBR14,COND=(0,NE)                                 01390024
//*                                                                     01400024
//*--------------------------------------------------------------------*01410024
//* FIN DE JCL                                                         *01420024
//*--------------------------------------------------------------------*01430024
