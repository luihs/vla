//*  %%INCLIB MBVP.PROD.SYMBOLS %%INCMEM GLOBALES                       00010005
//VLJPFTCA JOB (EEVL,1),'VL',                                           00020005
//             CLASS=C,                                                 00030005
//             MSGCLASS=0,                                              00040005
//             MSGLEVEL=(1,1),                                          00050005
//             REGION=0M                                                00060005
//*--------------------------------------------------------------------*00070005
//*             L O G   D E   M O D I F I C A C I O N E S              *00080005
//*--------------------------------------------------------------------*00090005
//*   MARCA   AUTOR    FECHA   DESCRIPCION                             *00100005
//*   ------  ------  -------  -------------------------------------   *00110005
//*  PRB41599  LRH   16-09-21  CAMBIO DE PARAMETROS DESCARGA DB2       *00120005
//*--------------------------------------------------------------------*00130005
//*  BIBLIOTECAS BASICAS DE LA APLICACION.                             *00140005
//*--------------------------------------------------------------------*00150005
//JOBLIB   DD DSN=PEBP.ALTAMIRA.LOADLIB.BATCH,DISP=SHR                  00160005
//         DD DSN=PEBP.ALTAMIRA.LOADLIB.RUTINAS,DISP=SHR                00170005
//         DD DSN=LDB2DPPA.SDSNEXIT,DISP=SHR                            00180005
//         DD DSN=LDB2DPPA.SDSNLOAD,DISP=SHR                            00190005
//*                                                                     00200005
//*--------------------------------------------------------------------*00210005
//* %%SET %%FECHAS  = %%$ODATE                                          00220005
//* %%SET %%ANO     = %%SUBSTR %%FECHAS 1 4                             00230005
//* %%SET %%MES     = %%SUBSTR %%FECHAS 5 2                             00240005
//* %%SET %%DIA     = 01                                                00250005
//* %%SET %%PROC    = 1                                                 00251006
//* %%SET %%DIAHA   = %%SUBSTR %%FECHAS 7 2                             00260005
//* %%SET %%FECDES  = %%ANO.%%MES.%%DIA                                 00270005
//* %%SET %%FECDESE = '%%ANO-%%MES-%%DIA'                               00280005
//* %%SET %%FECHASE = '%%ANO-%%MES-%%DIAHA'                             00290005
//*--------------------------------------------------------------------*00300005
//* PGRM/UTIL: ADUUMAIN                                                *00310005
//* DESCRIP  : DESCARGA DE TABLA VLDTARC.                              *00320005
//*--------------------------------------------------------------------*00330005
//VLFTCA40 EXEC PGM=ADUUMAIN,                                           00340005
//             REGION=0M,                                               00350005
//*PRB41599-INI                                                         00360005
//*            PARM='DSNP,VLDTARCB%%E,NEW,,MSGLEVEL(1)'                 00370005
//             PARM='DSNP,VLDTARCB2,NEW,,MSGLEVEL(1)'                   00380005
//*PRB41599-FIN                                                         00390005
//STEPLIB  DD  DISP=SHR,                                                00400005
//             DSN='BMC.DB2.LOAD'                                       00410005
//         DD  DISP=SHR,                                                00420005
//             DSN='LDB2DPPA.SDSNEXIT'                                  00430005
//         DD  DISP=SHR,                                                00440005
//             DSN='LDB2DPPA.SDSNLOAD'                                  00450005
//SORTWK01 DD  UNIT=3390,                                               00460005
//             SPACE=(CYL,(100,50),RLSE)                                00470005
//SYSPRINT DD  SYSOUT=*                                                 00480005
//UTPRINT  DD  SYSOUT=*                                                 00490005
//SYSOUT   DD  SYSOUT=*                                                 00500005
//SYSTRACE DD  SYSOUT=*                                                 00510005
//SYSPUNCH DD  SYSOUT=*                                                 00520005
//SYSCNTL1 DD  DUMMY                                                    00530005
//SYSREC   DD  DSN=PEBP.VLFD.FIX.UNLFTCA.VLDTARC.D1%%ODATE,             00540005
//             DISP=(NEW,CATLG,DELETE),                                 00550005
//             SPACE=(CYL,(250,100),RLSE),                              00560005
//             RECFM=FB,                                                00570005
//             UNIT=3390,                                               00580005
//             BLKSIZE=0                                                00590005
//SYSIN    DD  *                                                        00600005
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    00610005
  SELECT * FROM MBVP.VLDTARC                                            00620005
   WHERE VARC_CENTAD = 0069                                             00630005
   ORDER BY VARC_CUENTA                                                 00640005
//*                                                                     00650005
//*--------------------------------------------------------------------*00660005
//* PGRM/UTIL: ADUUMAIN                                                *00670005
//* DESCRIP  : DESCARGA DE TABLA VLDTADS.                              *00680005
//*--------------------------------------------------------------------*00690005
//VLFTCA30 EXEC PGM=ADUUMAIN,                                           00700005
//             REGION=0M,                                               00710005
//             PARM='DSNP,VLDTADSB%%E,NEW,,MSGLEVEL(1)',COND=(0,NE)     00720005
//STEPLIB  DD  DISP=SHR,                                                00730005
//             DSN='BMC.DB2.LOAD'                                       00740005
//         DD  DISP=SHR,                                                00750005
//             DSN='LDB2DPPA.SDSNEXIT'                                  00760005
//         DD  DISP=SHR,                                                00770005
//             DSN='LDB2DPPA.SDSNLOAD'                                  00780005
//SORTWK01 DD  UNIT=3390,                                               00790005
//             SPACE=(CYL,(100,50),RLSE)                                00800005
//SYSPRINT DD  SYSOUT=*                                                 00810005
//UTPRINT  DD  SYSOUT=*                                                 00820005
//SYSOUT   DD  SYSOUT=*                                                 00830005
//SYSTRACE DD  SYSOUT=*                                                 00840005
//SYSPUNCH DD  SYSOUT=*                                                 00850005
//SYSCNTL1 DD  DUMMY                                                    00860005
//SYSREC   DD  DSN=PEBP.VLFD.FIX.UNLFTCA.VLDTADS.D1%%ODATE,             00870005
//             DISP=(NEW,CATLG,DELETE),                                 00880005
//             SPACE=(CYL,(250,100),RLSE),                              00890005
//             RECFM=FB,                                                00900005
//             UNIT=3390,                                               00910005
//             BLKSIZE=0                                                00920005
//SYSIN    DD  *                                                        00930005
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    00940005
  SELECT * FROM MBVP.VLDTADS                                            00950005
   WHERE VADS_TIPREG = 'M'                                              00960005
   ORDER BY VADS_CUENTA, VADS_PAVAL, VADS_VALOR, VADS_ISIN              00970005
//*                                                                     00980005
//*--------------------------------------------------------------------*00990005
//* DESCRIP  : GENERA REPORTE                                          *01000005
//*--------------------------------------------------------------------*01010005
//VLFTCA20 EXEC  PGM=IKJEFT1A,COND=(0,NE)                               01020005
//SYSOUT   DD  SYSOUT=*                                                 01030005
//SYSPRINT DD  SYSOUT=*                                                 01040005
//SYSABOUT DD  SYSOUT=*                                                 01050005
//SYSDBOUT DD  SYSOUT=*                                                 01060005
//SYSTSPRT DD  SYSOUT=*                                                 01070005
//QRLSDB2  DD  SYSOUT=*                                                 01080005
//E1DQ9FTC DD  DSN=PEBP.VLFD.FIX.UNLFTCA.VLDTARC.D1%%ODATE,DISP=SHR     01090005
//S1DQ9FTC DD  DSN=PEBP.VLFD.FIX.VLFTCA.CTAS0069.D1%%ODATE,             01100005
//             DISP=(NEW,CATLG,DELETE),                                 01110005
//             SPACE=(CYL,(100,50),RLSE),UNIT=3390,                     01120005
//             DCB=(LRECL=135,RECFM=FB,BLKSIZE=0)                       01130005
//SYSTSIN  DD  *                                                        01140005
  DSN SYSTEM(DSNP)                                                      01150005
  RUN PROGRAM(VL3CFTC0) PLAN(BVPVLPB) PARM('%%FECHAS.%%FECHAS.%%PROC')  01160007
//*                                                                     01170005
//*--------------------------------------------------------------------*01180005
//* DESCRIP  : GENERA REPORTE DE DIV-INT-VTCO-VTA                      *01190005
//*--------------------------------------------------------------------*01200005
//VLFTCA10 EXEC  PGM=IKJEFT1A,COND=(0,NE)                               01210005
//SYSOUT   DD  SYSOUT=*                                                 01220005
//SYSPRINT DD  SYSOUT=*                                                 01230005
//SYSABOUT DD  SYSOUT=*                                                 01240005
//SYSDBOUT DD  SYSOUT=*                                                 01250005
//SYSTSPRT DD  SYSOUT=*                                                 01260005
//QRLSDB2  DD  SYSOUT=*                                                 01270005
//E1DQ9FTC DD  DSN=PEBP.VLFD.FIX.VLFTCA.CTAS0069.D1%%ODATE,DISP=SHR     01280005
//E2DQ9ADS DD  DSN=PEBP.VLFD.FIX.UNLFTCA.VLDTADS.D1%%ODATE,DISP=SHR     01290005
//S1DQ9FTC DD  DSN=PEBP.VLFD.FIX.VLFTCA.FTCPAGO.D1%%ODATE,              01300005
//             DISP=(NEW,CATLG,DELETE),                                 01310005
//             SPACE=(CYL,(500,250),RLSE),UNIT=3390,                    01320005
//             DCB=(LRECL=214,RECFM=FB,BLKSIZE=0)                       01330005
//SYSTSIN  DD  *                                                        01340005
  DSN SYSTEM(DSNP)                                                      01350005
  RUN PROGRAM(VL4CFTC0) PLAN(BVPVLPB) PARM('%%FECHAS.%%FECHAS.%%PROC')  01360007
//*                                                                     01370005
//**********************************************************************01380005
//* F  I  N    D  E    J  C  L                                          01390005
//**********************************************************************01400005
