//*  %%INCLIB MBVP.PROD.SYMBOLS %%INCMEM GLOBALES                       00010006
//VLJPFTCB JOB (EEVL,1),'VL',                                           00020006
//             CLASS=C,                                                 00030006
//             MSGCLASS=0,                                              00040006
//             MSGLEVEL=(1,1),                                          00050006
//             REGION=0M                                                00060006
//*--------------------------------------------------------------------*00070006
//*  BIBLIOTECAS BASICAS DE LA APLICACION.                             *00080006
//*--------------------------------------------------------------------*00090006
//JOBLIB   DD DSN=PEBP.ALTAMIRA.LOADLIB.BATCH,DISP=SHR                  00100006
//         DD DSN=PEBP.ALTAMIRA.LOADLIB.RUTINAS,DISP=SHR                00110006
//         DD DSN=LDB2DPPA.SDSNEXIT,DISP=SHR                            00120006
//         DD DSN=LDB2DPPA.SDSNLOAD,DISP=SHR                            00130006
//*                                                                     00140006
//*--------------------------------------------------------------------*00150006
//* %%SET %%FECHAS  = %%$ODATE                                          00160006
//* %%SET %%ANO     = %%SUBSTR %%FECHAS 1 4                             00170006
//* %%SET %%MES     = %%SUBSTR %%FECHAS 5 2                             00180006
//* %%SET %%DIA     = 01                                                00190006
//* %%SET %%PROC    = 2                                                 00191007
//* %%SET %%DIAHA   = %%SUBSTR %%FECHAS 7 2                             00200006
//* %%SET %%FECDES  = %%ANO.%%MES.%%DIA                                 00210006
//* %%SET %%FECDESE = '%%ANO-%%MES-%%DIA'                               00220006
//* %%SET %%FECHASE = '%%ANO-%%MES-%%DIAHA'                             00230006
//* %%SET %%ANIOMES = %%ANO.%%MES                                       00240006
//*--------------------------------------------------------------------*00242008
//* DESCRIP  : GENERA REPORTE                                          *00243008
//*--------------------------------------------------------------------*00244008
//VLFTCA10 EXEC  PGM=IKJEFT1A,COND=(0,NE)                               00245008
//SYSOUT   DD  SYSOUT=*                                                 00246008
//SYSPRINT DD  SYSOUT=*                                                 00247008
//SYSABOUT DD  SYSOUT=*                                                 00248008
//SYSDBOUT DD  SYSOUT=*                                                 00249008
//SYSTSPRT DD  SYSOUT=*                                                 00249108
//QRLSDB2  DD  SYSOUT=*                                                 00249208
//E1DQ9FTC DD  DSN=PEBP.VLFD.FIX.UNLFTCA.VLDTARC.D1%%ODATE,DISP=SHR     00249308
//S1DQ9FTC DD  DSN=PEBP.VLFD.FIX.VLFTCA.CTASM069.D1%%ODATE,             00249408
//             DISP=(NEW,CATLG,DELETE),                                 00249508
//             SPACE=(CYL,(100,50),RLSE),UNIT=3390,                     00249608
//             DCB=(LRECL=135,RECFM=FB,BLKSIZE=0)                       00249708
//SYSTSIN  DD  *                                                        00249808
  DSN SYSTEM(DSNP)                                                      00249908
  RUN PROGRAM(VL3CFTC0) PLAN(BVPVLPB) PARM('%%FECHAS.%%FECHAS.%%PROC')  00250008
//*                                                                     00250108
//*--------------------------------------------------------------------*00251006
//* DESCRIP  : GENERA REPORTE DE DIV-INT-VTCO-VTA                      *00260006
//*--------------------------------------------------------------------*00270006
//VLFTCA20 EXEC  PGM=IKJEFT1A,COND=(0,NE)                               00280008
//SYSOUT   DD  SYSOUT=*                                                 00290006
//SYSPRINT DD  SYSOUT=*                                                 00300006
//SYSABOUT DD  SYSOUT=*                                                 00310006
//SYSDBOUT DD  SYSOUT=*                                                 00320006
//SYSTSPRT DD  SYSOUT=*                                                 00330006
//QRLSDB2  DD  SYSOUT=*                                                 00340006
//E1DQ9FTC DD  DSN=PEBP.VLFD.FIX.VLFTCA.CTASM069.D1%%ODATE,DISP=SHR     00350008
//E2DQ9ADS DD  DSN=PEBP.VLFD.FIX.UNLFTCA.VLDTADS.D1%%ODATE,DISP=SHR     00360006
//S1DQ9FTC DD  DSN=PEBP.VLFD.FVA01.VLFTCA.FTCMENS.D1%%ANIOMES,          00370006
//             DISP=(NEW,CATLG,DELETE),                                 00380006
//             SPACE=(CYL,(500,250),RLSE),UNIT=3390,                    00390006
//             DCB=(LRECL=214,RECFM=FB,BLKSIZE=0)                       00400006
//SYSTSIN  DD  *                                                        00410006
  DSN SYSTEM(DSNP)                                                      00420006
  RUN PROGRAM(VL4CFTC0) PLAN(BVPVLPB) PARM('%%FECDES.%%FECHAS.%%PROC')  00430007
//*                                                                     00440006
//**********************************************************************00450006
//* F  I  N    D  E    J  C  L                                          00460006
//**********************************************************************00470006
