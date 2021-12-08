//*  %%INCLIB MBVP.PROD.SYMBOLS %%INCMEM GLOBALES                       00010021
//PVLP9ARC JOB (EEVL,1),'VL',                                           00020024
//             CLASS=C,                                                 00030021
//             MSGCLASS=0,                                              00040021
//             MSGLEVEL=(1,1),                                          00050021
//             REGION=0M                                                00060021
//*--------------------------------------------------------------------*00070021
//* PROYECTO     : FATCA VALORES                                       *00080024
//* APLICATIVO   : VL (VALORES).                                       *00090021
//* OBJETIVO     : GENERACION DE INFORMACION PARA FATCA VALORES        *00110024
//* REALIZO      : LRH (LRIVERAH).                                     *00130021
//*--------------------------------------------------------------------*00140021
//*             L O G   D E   M O D I F I C A C I O N E S              *00150021
//*--------------------------------------------------------------------*00160021
//*   MARCA   AUTOR    FECHA   DESCRIPCION                             *00170021
//*   ------  ------  -------  -------------------------------------   *00180021
//*--------------------------------------------------------------------*00190021
//*  BIBLIOTECAS BASICAS DE LA APLICACION.                             *00200021
//*--------------------------------------------------------------------*00210021
//JOBLIB   DD DISP=SHR,DSN=PEBP.ALTAMIRA.LOADLIB.BATCH.EVENTUAL         00220021
//         DD DISP=SHR,DSN=PEBP.ALTAMIRA.LOADLIB.RUTINAS.EVENTUAL       00230021
//         DD DSN=PEBP.ALTAMIRA.LOADLIB.BATCH,DISP=SHR                  00240021
//         DD DSN=PEBP.ALTAMIRA.LOADLIB.RUTINAS,DISP=SHR                00250021
//         DD DSN=LDB2DPPA.SDSNEXIT,DISP=SHR                            00260021
//         DD DSN=LDB2DPPA.SDSNLOAD,DISP=SHR                            00270021
//         DD DSN=CEE.SCEERUN,DISP=SHR                                  00280021
//*--------------------------------------------------------------------*00290021
//* %%SET %%FECHAS  = %%$ODATE                                          00300021
//* %%SET %%ANO     = %%SUBSTR %%FECHAS 1 4                             00310021
//* %%SET %%MES     = %%SUBSTR %%FECHAS 5 2                             00320021
//* %%SET %%DIA     = 01                                                00330021
//* %%SET %%DIAHA   = %%SUBSTR %%FECHAS 7 2                             00340021
//* %%SET %%FECDES  = %%ANO.%%MES.%%DIA                                 00350021
//* %%SET %%FECDESE = '%%ANO-%%MES-%%DIA'                               00360021
//* %%SET %%FECHASE = '%%ANO-%%MES-%%DIAHA'                             00370021
//**********************************************************************00380021
//* BORRADO DE FICHERO                                                 *00390021
//**********************************************************************00400021
//VLDEL001  EXEC PGM=IDCAMS                                             00410021
//SYSPRINT DD  SYSOUT=*                                                 00420021
//SYSIN    DD  *                                                        00430021
        DELETE PEBP.VLFD.FIX.UNLOCSAB.VLDTARC.D1%%ODATE    PURGE        00450021
        IF MAXCC = 8 THEN SET MAXCC = 0                                 00480021
//*--------------------------------------------------------------------*00830021
//* PASO     : VLEDC010                                                *00840024
//* PGRM/UTIL: ADUUMAIN                                                *00850021
//* DESCRIP  : DESCARGA DE TABLA VLDTARC.                              *00860021
//*--------------------------------------------------------------------*00870021
//VLEDC010 EXEC PGM=ADUUMAIN,                                           00880024
//             REGION=0M,                                               00890021
//             PARM='DSNP,VLDTARCB%%E,NEW,,MSGLEVEL(1)',COND=(0,NE)     00900021
//STEPLIB  DD  DISP=SHR,                                                00910021
//             DSN='BMC.DB2.LOAD'                                       00920021
//         DD  DISP=SHR,                                                00930021
//             DSN='LDB2DPPA.SDSNEXIT'                                  00940021
//         DD  DISP=SHR,                                                00950021
//             DSN='LDB2DPPA.SDSNLOAD'                                  00960021
//SORTWK01 DD  UNIT=3390,                                               00970021
//             SPACE=(CYL,(50,10),RLSE)                                 00980021
//SYSPRINT DD  SYSOUT=*                                                 00990021
//UTPRINT  DD  SYSOUT=*                                                 01000021
//SYSOUT   DD  SYSOUT=*                                                 01010021
//SYSTRACE DD  SYSOUT=*                                                 01020021
//SYSPUNCH DD  SYSOUT=*                                                 01030021
//SYSCNTL1 DD  DUMMY                                                    01040021
//SYSREC   DD  DSN=PEBP.VLFD.FIX.UNLOCSAB.VLDTARC.D1%%ODATE,            01050021
//             DISP=(NEW,CATLG,DELETE),                                 01060021
//             SPACE=(CYL,(150,100),RLSE),                              01070021
//             RECFM=FB,                                                01080021
//             UNIT=3390,                                               01090021
//             BLKSIZE=0                                                01100021
//SYSIN    DD  *                                                        01110021
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    01120021
  SELECT * FROM                                                         01130021
  MBVP.VLDTARC                                                          01140021
/*                                                                      01560021
//* ============================================================        01570021
//*  JS * PASO OBLIGADO POR EL VL PARA EVENTUALES              *        01580021
//* ============================================================        01590021
//VLPAS000 EXEC PGM=IEFBR14,COND=(0,NE)                                 01600021
//*                                                                     01610021
//*--------------------------------------------------------------------*01620021
//* FIN DE JCL                                                         *01630021
//*--------------------------------------------------------------------*01640021
