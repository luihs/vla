//*  %%INCLIB MBVP.PROD.SYMBOLS %%INCMEM GLOBALES                       00010012
//PVLP988R JOB (ELBG,1),'VL',                                           00020017
//             CLASS=C,                                                 00030015
//             MSGCLASS=X,                                              00040010
//             MSGLEVEL=(1,1),                                          00050010
//             REGION=0M                                                00060010
//**********************************************************************00070010
//JOBLIB   DD DISP=SHR,DSN=PEBP.ALTAMIRA.LOADLIB.BATCH.EVENTUAL         00100011
//         DD DISP=SHR,DSN=PEBP.ALTAMIRA.LOADLIB.RUTINAS.EVENTUAL       00110011
//         DD DISP=SHR,DSN=PEBP.ALTAMIRA.LOADLIB.BATCH                  00120011
//         DD DISP=SHR,DSN=PEBP.ALTAMIRA.LOADLIB.RUTINAS                00130011
//**********************************************************************00210010
//* BORRADO DE FICHERO **                                              *00220019
//**********************************************************************00230010
//VLDEL001  EXEC PGM=IDCAMS                                             00240010
//SYSPRINT DD  SYSOUT=*                                                 00250010
//SYSIN    DD  *                                                        00260010
        DELETE PEBP.VLFD.FIX.FVM01.SUNAT.VALO2021         PURGE         00270011
        IF MAXCC = 8 THEN SET MAXCC = 0                                 00280010
//**********************************************************************00290010
//* INFORMACION PARA PROVISIONAR PDT - SUNAT                           *00300010
//**********************************************************************00310010
//VLPASO10  EXEC  PGM=IKJEFT1A,COND=(0,NE)                              00321014
//SYSOUT    DD SYSOUT=*                                                 00330010
//SYSPRINT  DD SYSOUT=*                                                 00340010
//SYSABOUT  DD SYSOUT=*                                                 00350010
//SYSDBOUT  DD SYSOUT=*                                                 00360010
//SYSTSPRT  DD SYSOUT=*                                                 00370010
//SYSUDUMP  DD SYSOUT=*                                                 00380010
//TCDF0000  DD DSN=PEBP.TCFD.VSL.TCDF0001,DISP=SHR                      00390011
//E1DQDAE0  DD DSN=PEBP.QGFD.FIX.A000DAE0.X0000,DISP=SHR                00400011
//VLSUNAT   DD DSN=PEBP.VLFD.FIX.FVM01.SUNAT.VALO2021,                  00410011
//             DISP=(NEW,CATLG,DELETE),                                 00420010
//             SPACE=(CYL,(50,25),RLSE),UNIT=3390,                      00430010
//             DCB=(RECFM=FB,LRECL=253,BLKSIZE=0,DSORG=PS)              00440010
//SYSTSIN  DD  *                                                        00450010
   DSN SYSTEM(DSNP) RETRY(0) TEST (0)                                   00460015
   RUN PROGRAM(VL4C7R67) PLAN(PBPVLPB) -                                00470018
                         PARM('2021010120210630')                       00480011
   END                                                                  00490010
/*                                                                      00500010
//* ============================================================        00501014
//*  JS * PASO OBLIGADO POR EL VL PARA EVENTUALES              *        00502014
//* ============================================================        00503014
//VLPAS000 EXEC PGM=IEFBR14,COND=(0,NE)                                 00504014
//*                                                                     00505014
//**********************************************************************00510010
//* FIN DE JCL                                                         *00520010
//**********************************************************************00530010
