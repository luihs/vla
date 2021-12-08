//*  %%INCLIB MBVP.PROD.SYMBOLS %%INCMEM GLOBALES                       00010021
//PVLP9ASB JOB (EEVL,1),'VL',                                           00020028
//             CLASS=C,                                                 00030021
//             MSGCLASS=0,                                              00040021
//             MSGLEVEL=(1,1),                                          00050021
//             REGION=0M                                                00060021
//*--------------------------------------------------------------------*00070021
//* PROYECTO     : AJUSTE DE COMISION SAB                              *00080021
//* APLICATIVO   : VL (VALORES).                                       *00090021
//* OBJETIVO     : GENERACION DE REPORTE PARA AJUSTE DE COMISION SAB.  *00110021
//* FECHA        : 26 DE SEPTIEMBRE DEL 2021.                          *00120024
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
        DELETE PEBP.VLFD.FIX.UNLOCSAB.VLDTCOM.D1%%ODATE    PURGE        00440021
        DELETE PEBP.VLFD.FIX.UNLOCSAB.VLDTARC.D1%%ODATE    PURGE        00450021
        DELETE PEBP.VLFD.FVM01.REPOR.NEGBOL                PURGE        00460021
        DELETE PEBP.VLFD.FVM01.REPOR.NEGSAB                PURGE        00470021
        IF MAXCC = 8 THEN SET MAXCC = 0                                 00480021
//*--------------------------------------------------------------------*00490021
//* PASO     : VLEDC010                                                *00500021
//* PGRM/UTIL: ADUUMAIN                                                *00510021
//* DESCRIP  : DESCARGA DE TABLA VLDTCOM CON CUENTAS ACTIVAS.          *00520021
//*--------------------------------------------------------------------*00530021
//VLEDC010 EXEC PGM=ADUUMAIN,                                           00540021
//             REGION=0M,                                               00550021
//             PARM='DSNP,VLDTCOMB%%E,NEW,,MSGLEVEL(1)',COND=(0,NE)     00560021
//STEPLIB  DD  DISP=SHR,                                                00570021
//             DSN='BMC.DB2.LOAD'                                       00580021
//         DD  DISP=SHR,                                                00590021
//             DSN='LDB2DPPA.SDSNEXIT'                                  00600021
//         DD  DISP=SHR,                                                00610021
//             DSN='LDB2DPPA.SDSNLOAD'                                  00620021
//SORTWK01 DD  UNIT=3390,                                               00630021
//             SPACE=(CYL,(50,10),RLSE)                                 00640021
//SYSPRINT DD  SYSOUT=*                                                 00650021
//UTPRINT  DD  SYSOUT=*                                                 00660021
//SYSOUT   DD  SYSOUT=*                                                 00670021
//SYSTRACE DD  SYSOUT=*                                                 00680021
//SYSPUNCH DD  SYSOUT=*                                                 00690021
//SYSCNTL1 DD  DUMMY                                                    00700021
//SYSREC   DD  DSN=PEBP.VLFD.FIX.UNLOCSAB.VLDTCOM.D1%%ODATE,            00710021
//             DISP=(NEW,CATLG,DELETE),                                 00720021
//             SPACE=(CYL,(150,100),RLSE),                              00730021
//             RECFM=FB,                                                00740021
//             UNIT=3390,                                               00750021
//             BLKSIZE=0                                                00760021
//SYSIN    DD  *                                                        00770021
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    00780021
  SELECT * FROM                                                         00790021
  MBVP.VLDTCOM                                                          00800021
  WHERE VCOM_SITUAC = 'A'                                               00810021
  ORDER BY VCOM_CUENTA, VCOM_CLACONT                                    00820021
//*--------------------------------------------------------------------*00830021
//* PASO     : VLEDC020                                                *00840021
//* PGRM/UTIL: ADUUMAIN                                                *00850021
//* DESCRIP  : DESCARGA DE TABLA VLDTARC.                              *00860021
//*--------------------------------------------------------------------*00870021
//VLEDC020 EXEC PGM=ADUUMAIN,                                           00880021
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
  WHERE VARC_SITUAC = 'A'                                               01150021
  ORDER BY VARC_CUENTA, VARC_CENTAD                                     01160021
//**********************************************************************01170021
//* GENERA REPORTE NEGOCIASION EN BOLSA                                *01180021
//**********************************************************************01190021
//VLEDC030  EXEC  PGM=IKJEFT1A,DYNAMNBR=20,COND=(0,NE)                  01200021
//SYSOUT    DD SYSOUT=*                                                 01210021
//SYSPRINT  DD SYSOUT=*                                                 01220021
//SYSABOUT  DD SYSOUT=*                                                 01230021
//SYSDBOUT  DD SYSOUT=*                                                 01240021
//SYSTSPRT  DD SYSOUT=*                                                 01250021
//SYSUDUMP  DD SYSOUT=*                                                 01260021
//E1VDTCOM  DD DSN=PEBP.VLFD.FIX.UNLOCSAB.VLDTCOM.D1%%ODATE,DISP=SHR    01270021
//E2VDTARC  DD DSN=PEBP.VLFD.FIX.UNLOCSAB.VLDTARC.D1%%ODATE,DISP=SHR    01280021
//S1RNEGBL  DD DSN=PEBP.VLFD.FVM01.REPOR.NEGBOL,                        01290021
//             DISP=(NEW,CATLG,DELETE),                                 01300021
//             SPACE=(CYL,(50,25),RLSE),UNIT=3390,                      01310021
//             DCB=(RECFM=FB,LRECL=270,BLKSIZE=0,DSORG=PS)              01320028
//SYSTSIN  DD  *                                                        01330021
   DSN SYSTEM(DSNP) RETRY(0) TEST (0)                                   01340021
   RUN PROGRAM(VL4CBJUS) PLAN(BVPVLPB)                                  01350028
   END                                                                  01360021
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