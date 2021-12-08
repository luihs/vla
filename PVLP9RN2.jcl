//*  %%INCLIB MBVP.PROD.SYMBOLS %%INCMEM GLOBALES                       00010025
//PVLP9RN2 JOB (EEVL,1),'VL',                                           00020025
//             CLASS=C,                                                 00030025
//             MSGCLASS=0,                                              00040025
//             MSGLEVEL=(1,1),                                          00050020
//             REGION=0M                                                00060020
//*--------------------------------------------------------------------*00070020
//*  BIBLIOTECAS BASICAS DE LA APLICACION.                             *00080020
//*--------------------------------------------------------------------*00090020
//JOBLIB   DD DISP=SHR,DSN=PEBP.ALTAMIRA.LOADLIB.BATCH.EVENTUAL         00100023
//         DD DISP=SHR,DSN=PEBP.ALTAMIRA.LOADLIB.RUTINAS.EVENTUAL       00110023
//         DD DSN=PEBP.ALTAMIRA.LOADLIB.BATCH,DISP=SHR                  00120023
//         DD DSN=PEBP.ALTAMIRA.LOADLIB.RUTINAS,DISP=SHR                00130023
//         DD DSN=LDB2DPPA.SDSNEXIT,DISP=SHR                            00140023
//         DD DSN=LDB2DPPA.SDSNLOAD,DISP=SHR                            00141023
//         DD DSN=CEE.SCEERUN,DISP=SHR                                  00142023
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
//**********************************************************************00260020
//* BORRADO DE FICHERO                                                 *00270020
//**********************************************************************00280020
//VLDEL001  EXEC PGM=IDCAMS                                             00290022
//SYSPRINT DD  SYSOUT=*                                                 00300022
//SYSIN    DD  *                                                        00310022
        DELETE PEBP.VLFD.FVM06.EECCBOL.MENSUAL.D1211130    PURGE        00320022
        IF MAXCC = 8 THEN SET MAXCC = 0                                 00350022
//**********************************************************************01200022
//* COPIA DATA AL ARCHIVO ORIGINAL                                     *01210025
//**********************************************************************01220022
//VLEDC020  EXEC  PGM=IEFBR14,COND=(4,LT)                               01230022
//SYSOUT   DD SYSOUT=*                                                  01240022
//SYSPRINT DD SYSOUT=*                                                  01250022
//DATA1    DD DSN=PLZAQ.XS2.XMIT.EECCBOL.MENSUAL.D1211130,DISP=SHR      01260022
//DATA2    DD DSN=PEBP.VLFD.FVM06.EECCBOL.MENSUAL.D1211130,             01270022
//            DISP=(NEW,CATLG,DELETE),UNIT=3390,                        01280026
//            SPACE=(CYL,(500,250),RLSE),                               01290026
//            DCB=(RECFM=FB,BLKSIZE=0,DSORG=PS,LRECL=2232)              01300026
//SYSIN    DD *                                                         01310022
        REPRO IFILE(DATA1) OFILE(DATA2)                                 01320022
/*                                                                      01470020
//* ============================================================        01471024
//*  JS * PASO OBLIGADO POR EL VL PARA EVENTUALES              *        01472024
//* ============================================================        01473024
//VLPAS000 EXEC PGM=IEFBR14,COND=(0,NE)                                 01474024
//*                                                                     01475024
//*--------------------------------------------------------------------*01490020
//* FIN DE JCL                                                         *01500020
//*--------------------------------------------------------------------*01510020
