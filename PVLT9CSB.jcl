//*  %%INCLIB MBVT.PROD.SYMBOLS %%INCMEM GLOBALES                       00010020
//PVLT9CSB JOB (EEVL,1),'VL',                                           00020020
//             CLASS=M,                                                 00030020
//             MSGCLASS=X,                                              00040020
//             MSGLEVEL=(1,1),                                          00050020
//             REGION=0M                                                00060020
//*--------------------------------------------------------------------*00070020
//*  BIBLIOTECAS BASICAS DE LA APLICACION.                             *00080020
//*--------------------------------------------------------------------*00090020
//JOBLIB   DD DSN=PEBT.ALTAMIRA.LOADLIB.BATCH,DISP=SHR                  00100020
//         DD DSN=PEBT.ALTAMIRA.LOADLIB.RUTINAS,DISP=SHR                00110020
//         DD DSN=LDB2DTDB.SDSNEXIT,DISP=SHR                            00120020
//         DD DSN=LDB2DTDB.SDSNLOAD,DISP=SHR                            00130020
//         DD DSN=CEE.SCEERUN,DISP=SHR                                  00140020
//*--------------------------------------------------------------------*00150020
//* %%SET %%FECHAS  = %%$ODATE                                          00160020
//* %%SET %%ANO     = %%SUBSTR %%FECHAS 1 4                             00170020
//* %%SET %%MES     = %%SUBSTR %%FECHAS 5 2                             00180020
//* %%SET %%DIA     = 01                                                00190020
//* %%SET %%DIAHA   = %%SUBSTR %%FECHAS 7 2                             00200020
//* %%SET %%FECDES  = %%ANO.%%MES.%%DIA                                 00210020
//* %%SET %%FECDESE = '%%ANO-%%MES-%%DIA'                               00220020
//* %%SET %%FECHASE = '%%ANO-%%MES-%%DIAHA'                             00230020
//* %%SET %%COMIT   = 000001                                            00240020
//* SE ELIMINO TRATAMIENTO POR EL TRIMESTE COPIA ORIGINAL JCL VLJPEDCO  00250020
//**********************************************************************00260020
//* BORRADO DE FICHERO                                                 *00270020
//**********************************************************************00280020
//VLDEL001  EXEC PGM=IDCAMS                                             00290020
//SYSPRINT DD  SYSOUT=*                                                 00300020
//SYSIN    DD  *                                                        00310020
        DELETE PEBT.VLFD.FIX.UNLOECD.VLDTARC.D1%%ODATE     PURGE        00320020
        DELETE PEBT.VLFD.FIX.UNLOECD.VLDTCOM.D1%%ODATE     PURGE        00330020
        DELETE PEBT.VLFD.FVM01.REPOR.NEGBOL                PURGE        00340020
        IF MAXCC = 8 THEN SET MAXCC = 0                                 00350020
//*--------------------------------------------------------------------*00360020
//*                  >>>> VLJPEDC0 <<<<                                *00370020
//*--------------------------------------------------------------------*00380020
//* PASO     : VLEDC010                                                *00390020
//* PGRM/UTIL: ADUUMAIN                                                *00400020
//* DESCRIP  : DESCARGA DE TABLA VLDTARC CON CUENTAS ACTIVAS.          *00410020
//*--------------------------------------------------------------------*00420020
//VLEDC010 EXEC PGM=ADUUMAIN,                                           00430020
//             REGION=0M,                                               00440020
//             PARM='DTDB,VLDTARCB%%E,NEW,,MSGLEVEL(1)',COND=(0,NE)     00450020
//STEPLIB  DD  DISP=SHR,                                                00460020
//             DSN='BMC.DB2.LOAD'                                       00470020
//         DD  DISP=SHR,                                                00480020
//             DSN='LDB2DTDB.SDSNEXIT'                                  00490020
//         DD  DISP=SHR,                                                00500020
//             DSN='LDB2DTDB.SDSNLOAD'                                  00510020
//SORTWK01 DD  UNIT=3390,                                               00520020
//             SPACE=(CYL,(50,10),RLSE)                                 00530020
//SYSPRINT DD  SYSOUT=*                                                 00540020
//UTPRINT  DD  SYSOUT=*                                                 00550020
//SYSOUT   DD  SYSOUT=*                                                 00560020
//SYSTRACE DD  SYSOUT=*                                                 00570020
//SYSPUNCH DD  SYSOUT=*                                                 00580020
//SYSCNTL1 DD  DUMMY                                                    00590020
//SYSREC   DD  DSN=PEBT.VLFD.FIX.UNLOECD.VLDTARC.D1%%ODATE,             00600020
//             DISP=(NEW,CATLG,DELETE),                                 00610020
//             SPACE=(CYL,(150,100),RLSE),                              00620020
//             RECFM=FB,                                                00630020
//             UNIT=3390,                                               00640020
//             BLKSIZE=0                                                00650020
//SYSIN    DD  *                                                        00660020
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    00670020
  SELECT * FROM                                                         00680020
  GPBT.VLDTARC                                                          00690020
  WHERE VARC_SITUAC = 'A'                                               00700020
  ORDER BY VARC_CUENTA                                                  00710020
//*--------------------------------------------------------------------*00720020
//* PASO     : VLEDC020                                                *00730020
//* PGRM/UTIL: ADUUMAIN                                                *00740020
//* DESCRIP  : DESCARGA DE TABLA VLDTADS.                              *00750020
//*--------------------------------------------------------------------*00760020
//VLEDC020 EXEC PGM=ADUUMAIN,                                           00770020
//             REGION=0M,                                               00780020
//             PARM='DTDB,VLDTCOMB%%E,NEW,,MSGLEVEL(1)',COND=(0,NE)     00790020
//STEPLIB  DD  DISP=SHR,                                                00800020
//             DSN='BMC.DB2.LOAD'                                       00810020
//         DD  DISP=SHR,                                                00820020
//             DSN='LDB2DTDB.SDSNEXIT'                                  00830020
//         DD  DISP=SHR,                                                00840020
//             DSN='LDB2DTDB.SDSNLOAD'                                  00850020
//SORTWK01 DD  UNIT=3390,                                               00860020
//             SPACE=(CYL,(50,10),RLSE)                                 00870020
//SYSPRINT DD  SYSOUT=*                                                 00880020
//UTPRINT  DD  SYSOUT=*                                                 00890020
//SYSOUT   DD  SYSOUT=*                                                 00900020
//SYSTRACE DD  SYSOUT=*                                                 00910020
//SYSPUNCH DD  SYSOUT=*                                                 00920020
//SYSCNTL1 DD  DUMMY                                                    00930020
//SYSREC   DD  DSN=PEBT.VLFD.FIX.UNLOECD.VLDTCOM.D1%%ODATE,             00940020
//             DISP=(NEW,CATLG,DELETE),                                 00950020
//             SPACE=(CYL,(150,100),RLSE),                              00960020
//             RECFM=FB,                                                00970020
//             LRECL=127,                                               00980020
//             UNIT=3390,                                               00990020
//             BLKSIZE=0                                                01000020
//SYSIN    DD  *                                                        01010020
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    01020020
  SELECT * FROM                                                         01030020
  GPBT.VLDTCOM                                                          01040020
  WHERE VCOM_SITUAC = 'A'                                               01050020
  ORDER BY VCOM_CUENTA, VCOM_CLACONT                                    01060020
//**********************************************************************01070020
//* INFORMACION PARA PROVISIONAR PDT - SUNAT                           *01080020
//**********************************************************************01090020
//VLEDC030  EXEC  PGM=IKJEFT1A,DYNAMNBR=20                              01100020
//SYSOUT    DD SYSOUT=*                                                 01110020
//SYSPRINT  DD SYSOUT=*                                                 01120020
//SYSABOUT  DD SYSOUT=*                                                 01130020
//SYSDBOUT  DD SYSOUT=*                                                 01140020
//SYSTSPRT  DD SYSOUT=*                                                 01150020
//SYSUDUMP  DD SYSOUT=*                                                 01160020
//*TCDF0000  DD DSN=PEBT.TCFD.VSL.TCDF0001,DISP=SHR                     01170020
//E1VDTCOM  DD DSN=PEBT.VLFD.FIX.UNLOECD.VLDTCOM.D1%%ODATE,DISP=SHR     01180020
//E2VDTARC  DD DSN=PEBT.VLFD.FIX.UNLOECD.VLDTARC.D1%%ODATE,DISP=SHR     01190020
//S1RNEGBL  DD DSN=PEBT.VLFD.FVM01.REPOR.NEGBOL,                        01200020
//             DISP=(NEW,CATLG,DELETE),                                 01210020
//             SPACE=(CYL,(50,25),RLSE),UNIT=3390,                      01220020
//             DCB=(RECFM=FB,LRECL=225,BLKSIZE=0,DSORG=PS)              01230020
//SYSTSIN  DD  *                                                        01240020
   DSN SYSTEM(DTDB) RETRY(0) TEST (0)                                   01250020
   RUN PROGRAM(VL4CSABC) PLAN(PBTVLPB)                                  01260020
   END                                                                  01270020
//**********************************************************************01280020
//* INFORMACION PARA PROVISIONAR PDT - SUNAT                           *01290020
//**********************************************************************01300020
//VLEDC040  EXEC  PGM=IKJEFT1A,DYNAMNBR=20                              01310020
//SYSOUT    DD SYSOUT=*                                                 01320020
//SYSPRINT  DD SYSOUT=*                                                 01330020
//SYSABOUT  DD SYSOUT=*                                                 01340020
//SYSDBOUT  DD SYSOUT=*                                                 01350020
//SYSTSPRT  DD SYSOUT=*                                                 01360020
//SYSUDUMP  DD SYSOUT=*                                                 01370020
//E1VDTARC  DD DSN=PEBT.VLFD.FIX.UNLOECD.VLDTARC.D1%%ODATE,DISP=SHR     01380020
//S1RNEGBL  DD DSN=PEBT.VLFD.FVM01.REPOR.NEGSAB,                        01390020
//             DISP=(NEW,CATLG,DELETE),                                 01400020
//             SPACE=(CYL,(50,25),RLSE),UNIT=3390,                      01410020
//             DCB=(RECFM=FB,LRECL=83,BLKSIZE=0,DSORG=PS)               01420020
//SYSTSIN  DD  *                                                        01430020
   DSN SYSTEM(DTDB) RETRY(0) TEST (0)                                   01440020
   RUN PROGRAM(VL4CSBOL) PLAN(PBTVLPB)                                  01450020
   END                                                                  01460020
/*                                                                      01470020
//**********************************************************************01480020
//*--------------------------------------------------------------------*01490020
//* FIN DE JCL                                                         *01500020
//*--------------------------------------------------------------------*01510020
