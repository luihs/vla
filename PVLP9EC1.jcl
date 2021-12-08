//*  %%INCLIB MBVP.PROD.SYMBOLS %%INCMEM GLOBALES                       00010021
//PVLP9EC1 JOB (EEVL,1),'VL',                                           00020031
//             CLASS=C,                                                 00030021
//             MSGCLASS=0,                                              00040021
//             MSGLEVEL=(1,1),                                          00050021
//             REGION=0M                                                00060021
//*--------------------------------------------------------------------*00070021
//* PROYECTO     : FATCA.                                              *00080029
//* APLICATIVO   : VL (VALORES).                                       *00090021
//* FECHA        : 23 DE NOVIEMBRE DEL 2021.                           *00120031
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
        DELETE PEBP.VLFD.FIX.UNLOCFTA.VLDTHIS.D1%%ODATE    PURGE        00440029
        DELETE PEBP.VLFD.FIX.UNLOCFTA.VLDTXEN.D1%%ODATE    PURGE        00450029
        DELETE PEBP.VLFD.FIX.UNLOCFTA.VLDTCAM.D1%%ODATE    PURGE        00460029
        DELETE PEBP.VLFD.FIX.UNLOCFTA.VLDTHAC.D1%%ODATE    PURGE        00470029
        IF MAXCC = 8 THEN SET MAXCC = 0                                 00480021
//*--------------------------------------------------------------------*00490031
//* PASO     : VLEDC110                                                *00500031
//* PGRM/UTIL: ICEMAN.                                                 *00510031
//* DESCRIP  : SE ORDENA POR ARCHIVO EECC POR                          *00520031
//*--------------------------------------------------------------------*00530031
//VLPRC001 EXEC PGM=ICEMAN,COND=(4,LT)                                  00540031
//SORTIN   DD DSN=PEBP.VLFD.FVM06.EECCBOL.MENSUAL.D1211029,             00550031
//            DISP=SHR                                                  00560031
//SORTOUT  DD DSN=PEBP.VLFD.FVM06.EECCBOL.ORDENA,                       00570031
//            DISP=(NEW,CATLG,DELETE),UNIT=3390,                        00580031
//            SPACE=(CYL,(500,250),RLSE),                               00590031
//            DCB=(RECFM=FB,BLKSIZE=0,DSORG=PS,LRECL=256)               00600031
//SYSPRINT DD SYSOUT=*                                                  00610031
//SYSOUT   DD SYSOUT=*                                                  00620031
//SYSIN    DD *                                                         00630031
  SORT FIELDS=(1,4,PD,A,8,5,PD,A)                                       00640031
  END                                                                   00650031
//*--------------------------------------------------------------------*00660031
//* PASO     : VLEDC110                                                *00670031
//* PGRM/UTIL: ICEMAN.                                                 *00680031
//* DESCRIP  : SE ORDENA ARCHIVO EECC REPROCESO POR                    *00690031
//*--------------------------------------------------------------------*00700031
//VLPRC001 EXEC PGM=ICEMAN,COND=(4,LT)                                  00710031
//SORTIN   DD DSN=PEBP.VLFD.FVM06.EECCREP.MENSUAL.D1211029,             00720034
//            DISP=SHR                                                  00730031
//SORTOUT  DD DSN=PEBP.VLFD.FVM06.EECCREP.ORDENA,                       00740031
//            DISP=(NEW,CATLG,DELETE),UNIT=3390,                        00750031
//            SPACE=(CYL,(500,250),RLSE),                               00760031
//            DCB=(RECFM=FB,BLKSIZE=0,DSORG=PS,LRECL=256)               00770031
//SYSPRINT DD SYSOUT=*                                                  00780031
//SYSOUT   DD SYSOUT=*                                                  00790031
//SYSIN    DD *                                                         00800031
  SORT FIELDS=(1,4,PD,A,8,5,PD,A)                                       00810031
  END                                                                   00820031
//**********************************************************************00830032
//* MATCH PARA DIFERENCIAR LOS MONTOS                                  *00840032
//**********************************************************************00850032
//VLEDC030  EXEC  PGM=IKJEFT1A,DYNAMNBR=20,COND=(0,NE)                  00860032
//SYSOUT    DD SYSOUT=*                                                 00870032
//SYSPRINT  DD SYSOUT=*                                                 00880032
//SYSABOUT  DD SYSOUT=*                                                 00890032
//SYSDBOUT  DD SYSOUT=*                                                 00900032
//SYSTSPRT  DD SYSOUT=*                                                 00910032
//SYSUDUMP  DD SYSOUT=*                                                 00920032
//E1VDTARC  DD DSN=PEBP.VLFD.FVM06.EECCBOL.ORDENA,DISP=SHR              00930032
//E2VDTARC  DD DSN=PEBP.VLFD.FVM06.EECCREP.ORDENA,DISP=SHR              00931033
//S1RNEGBL  DD DSN=PEBP.VLFD.FVM06.EECCBOL.FINAL,                       00940033
//             DISP=(NEW,CATLG,DELETE),                                 00950032
//             SPACE=(CYL,(50,25),RLSE),UNIT=3390,                      00960032
//             DCB=(RECFM=FB,LRECL=159,BLKSIZE=0,DSORG=PS)              00970032
//SYSTSIN  DD  *                                                        00980032
   DSN SYSTEM(DSNP) RETRY(0) TEST (0)                                   00990032
   RUN PROGRAM(VL4CECPA) PLAN(BVPVLPB)                                  01000033
   END                                                                  01010032
/*                                                                      01020032
//* ============================================================        01572021
//*  JS * PASO OBLIGADO POR EL VL PARA EVENTUALES              *        01580021
//* ============================================================        01590021
//VLPAS000 EXEC PGM=IEFBR14,COND=(0,NE)                                 01600021
//*                                                                     01610021
//*--------------------------------------------------------------------*01620021
//* FIN DE JCL                                                         *01630021
//*--------------------------------------------------------------------*01640021
