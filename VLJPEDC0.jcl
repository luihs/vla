//*  %%INCLIB MBVP.PROD.SYMBOLS %%INCMEM GLOBALES                       00010000
//VLJPEDC0 JOB (EEVL,1),'VL',                                           00020000
//             CLASS=C,                                                 00030000
//             MSGCLASS=0,                                              00040000
//             MSGLEVEL=(1,1),                                          00050000
//             REGION=0M                                                00060000
//*--------------------------------------------------------------------*00070000
//* PROYECTO     : 6762014060 - ESTADOS DE CUENTA DE BOLSA             *00080000
//* APLICATIVO   : VL (VALORES).                                       *00090000
//* PROCESO      : VLJPEDC0.                                           *00100000
//* PERIODICIDAD : MENSUAL                                             *00110000
//* OBJETIVO     : DESCARGA DE TABLAS DE ESTADOS DE CUENTAS DE BOLSA.  *00120000
//* FECHA        : 21 DE SEPTIEMBRE DEL 2015.                          *00130000
//* REALIZO      : MDP (BAGUILAR).                                     *00140000
//* D&D BANCO    :                                                     *00150000
//*--------------------------------------------------------------------*00160000
//*             L O G   D E   M O D I F I C A C I O N E S              *00170000
//*--------------------------------------------------------------------*00180000
//*   MARCA   AUTOR    FECHA   DESCRIPCION                             *00190000
//*   ------  ------  -------  -------------------------------------   *00200000
//*--------------------------------------------------------------------*00210002
//*  BIBLIOTECAS BASICAS DE LA APLICACION.                             *00220002
//*--------------------------------------------------------------------*00230002
//JOBLIB   DD DSN=PEBP.ALTAMIRA.LOADLIB.BATCH,DISP=SHR                  00240005
//         DD DSN=PEBP.ALTAMIRA.LOADLIB.RUTINAS,DISP=SHR                00250002
//         DD DSN=LDB2DPPA.SDSNEXIT,DISP=SHR                            00260002
//         DD DSN=LDB2DPPA.SDSNLOAD,DISP=SHR                            00270002
//         DD DSN=CEE.SCEERUN,DISP=SHR                                  00280002
//*--------------------------------------------------------------------*00290002
//* %%SET %%FECHAS  = %%$ODATE                                          00300002
//* %%SET %%ANO     = %%SUBSTR %%FECHAS 1 4                             00310002
//* %%SET %%MES     = %%SUBSTR %%FECHAS 5 2                             00320002
//* %%SET %%DIA     = 01                                                00330002
//* %%SET %%DIAHA   = %%SUBSTR %%FECHAS 7 2                             00340002
//* %%SET %%FECDES  = %%ANO.%%MES.%%DIA                                 00350002
//* %%SET %%FECDESE = '%%ANO-%%MES-%%DIA'                               00360002
//* %%SET %%FECHASE = '%%ANO-%%MES-%%DIAHA'                             00370002
//* SE ELIMINO TRATAMIENTO POR EL TRIMESTE COPIA ORIGINAL JCL VLJPEDCO  00380005
//*--------------------------------------------------------------------*00390002
//*                  >>>> VLJPEDC0 <<<<                                *00400002
//*--------------------------------------------------------------------*00410002
//* PASO     : VLEDC010                                                *00420002
//* PGRM/UTIL: ADUUMAIN                                                *00430002
//* DESCRIP  : DESCARGA DE TABLA VLDTARC CON CUENTAS ACTIVAS.          *00440002
//*--------------------------------------------------------------------*00450002
//VLEDC010 EXEC PGM=ADUUMAIN,                                           00460002
//             REGION=0M,                                               00470002
//             PARM='DSNP,VLDTARCB%%E,NEW,,MSGLEVEL(1)',COND=(0,NE)     00480002
//STEPLIB  DD  DISP=SHR,                                                00490002
//             DSN='BMC.DB2.LOAD'                                       00500002
//         DD  DISP=SHR,                                                00510002
//             DSN='LDB2DPPA.SDSNEXIT'                                  00520002
//         DD  DISP=SHR,                                                00530002
//             DSN='LDB2DPPA.SDSNLOAD'                                  00540002
//SORTWK01 DD  UNIT=3390,                                               00550002
//             SPACE=(CYL,(50,10),RLSE)                                 00560002
//SYSPRINT DD  SYSOUT=*                                                 00570002
//UTPRINT  DD  SYSOUT=*                                                 00580002
//SYSOUT   DD  SYSOUT=*                                                 00590002
//SYSTRACE DD  SYSOUT=*                                                 00600002
//SYSPUNCH DD  SYSOUT=*                                                 00610002
//SYSCNTL1 DD  DUMMY                                                    00620002
//SYSREC   DD  DSN=PEBP.VLFD.FIX.UNLOEECC.VLDTARC.D1%%ODATE,            00630002
//             DISP=(NEW,CATLG,DELETE),                                 00640002
//             SPACE=(CYL,(150,100),RLSE),                              00650002
//             RECFM=FB,                                                00660002
//             UNIT=3390,                                               00670002
//             BLKSIZE=0                                                00680002
//SYSIN    DD  *                                                        00690002
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    00700002
  SELECT * FROM                                                         00710002
  MBVP.VLDTARC                                                          00720002
  WHERE VARC_SITUAC = 'A'                                               00730002
    AND VARC_CENTAD IN (0069, 2010)                                     00740002
  ORDER BY VARC_CUENTA                                                  00750002
//*--------------------------------------------------------------------*00760002
//* PASO     : VLEDC020                                                *00770002
//* PGRM/UTIL: ADUUMAIN                                                *00780002
//* DESCRIP  : DESCARGA DE TABLA VLDTADS.                              *00790002
//*--------------------------------------------------------------------*00800002
//VLEDC020 EXEC PGM=ADUUMAIN,                                           00810002
//             REGION=0M,                                               00820002
//             PARM='DSNP,VLDTADSB%%E,NEW,,MSGLEVEL(1)',COND=(0,NE)     00830002
//STEPLIB  DD  DISP=SHR,                                                00840002
//             DSN='BMC.DB2.LOAD'                                       00850002
//         DD  DISP=SHR,                                                00860002
//             DSN='LDB2DPPA.SDSNEXIT'                                  00870002
//         DD  DISP=SHR,                                                00880002
//             DSN='LDB2DPPA.SDSNLOAD'                                  00890002
//SORTWK01 DD  UNIT=3390,                                               00900002
//             SPACE=(CYL,(50,10),RLSE)                                 00910002
//SYSPRINT DD  SYSOUT=*                                                 00920002
//UTPRINT  DD  SYSOUT=*                                                 00930002
//SYSOUT   DD  SYSOUT=*                                                 00940002
//SYSTRACE DD  SYSOUT=*                                                 00950002
//SYSPUNCH DD  SYSOUT=*                                                 00960002
//SYSCNTL1 DD  DUMMY                                                    00970002
//SYSREC   DD  DSN=PEBP.VLFD.FIX.UNLOEECC.VLDTADS.D1%%ODATE,            00980002
//             DISP=(NEW,CATLG,DELETE),                                 00990002
//             SPACE=(CYL,(150,100),RLSE),                              01000002
//             RECFM=FB,                                                01010002
//             UNIT=3390,                                               01020002
//             BLKSIZE=0                                                01030002
//SYSIN    DD  *                                                        01040002
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    01050002
  SELECT * FROM                                                         01060002
  MBVP.VLDTADS                                                          01070002
  WHERE VADS_TIPREG = 'M'                                               01080002
  ORDER BY VADS_CUENTA, VADS_PAVAL, VADS_VALOR, VADS_ISIN               01090002
//*--------------------------------------------------------------------*01100002
//* PASO     : VLEDC030                                                *01110002
//* PGRM/UTIL: ADUUMAIN                                                *01120002
//* DESCRIP  : DESCARGA DE TABLA VLDTMMO.                              *01130002
//*--------------------------------------------------------------------*01140002
//VLEDC030 EXEC PGM=ADUUMAIN,                                           01150002
//             REGION=0M,                                               01160002
//             PARM='DSNP,VLDTMMOB%%E,NEW,,MSGLEVEL(1)',COND=(0,NE)     01170002
//STEPLIB  DD  DISP=SHR,                                                01180002
//             DSN='BMC.DB2.LOAD'                                       01190002
//         DD  DISP=SHR,                                                01200002
//             DSN='LDB2DPPA.SDSNEXIT'                                  01210002
//         DD  DISP=SHR,                                                01220002
//             DSN='LDB2DPPA.SDSNLOAD'                                  01230002
//SORTWK01 DD  UNIT=3390,                                               01240002
//             SPACE=(CYL,(50,10),RLSE)                                 01250002
//SYSPRINT DD  SYSOUT=*                                                 01260002
//UTPRINT  DD  SYSOUT=*                                                 01270002
//SYSOUT   DD  SYSOUT=*                                                 01280002
//SYSTRACE DD  SYSOUT=*                                                 01290002
//SYSPUNCH DD  SYSOUT=*                                                 01300002
//SYSCNTL1 DD  DUMMY                                                    01310002
//SYSREC   DD  DSN=PEBP.VLFD.FIX.UNLOEECC.VLDTMMO.D1%%ODATE,            01320002
//             DISP=(NEW,CATLG,DELETE),                                 01330002
//             SPACE=(CYL,(150,100),RLSE),                              01340002
//             RECFM=FB,                                                01350002
//             UNIT=3390,                                               01360002
//             BLKSIZE=0                                                01370002
//SYSIN    DD  *                                                        01380002
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    01390002
  SELECT * FROM                                                         01400002
  MBVP.VLDTMMO                                                          01410002
  WHERE VMMO_FECHOR >= %%FECDES AND VMMO_FECHOR <= %%FECHAS             01420002
//*--------------------------------------------------------------------*01430002
//* PASO     : VLEDC040                                                *01440002
//* PGRM/UTIL: ADUUMAIN                                                *01450002
//* DESCRIP  : DESCARGA DE TABLA VLDTPOL. PARA MOVIMIENTO DE VALORES.  *01460002
//*--------------------------------------------------------------------*01470002
//VLEDC040 EXEC PGM=ADUUMAIN,                                           01480002
//             REGION=0M,                                               01490002
//             PARM='DSNP,VLDTPOLB%%E,NEW,,MSGLEVEL(1)',COND=(0,NE)     01500002
//STEPLIB  DD  DISP=SHR,                                                01510002
//             DSN='BMC.DB2.LOAD'                                       01520002
//         DD  DISP=SHR,                                                01530002
//             DSN='LDB2DPPA.SDSNEXIT'                                  01540002
//         DD  DISP=SHR,                                                01550002
//             DSN='LDB2DPPA.SDSNLOAD'                                  01560002
//SORTWK01 DD  UNIT=3390,                                               01570002
//             SPACE=(CYL,(50,10),RLSE)                                 01580002
//SYSPRINT DD  SYSOUT=*                                                 01590002
//UTPRINT  DD  SYSOUT=*                                                 01600002
//SYSOUT   DD  SYSOUT=*                                                 01610002
//SYSTRACE DD  SYSOUT=*                                                 01620002
//SYSPUNCH DD  SYSOUT=*                                                 01630002
//SYSCNTL1 DD  DUMMY                                                    01640002
//SYSREC   DD  DSN=PEBP.VLFD.FIX.UNLOEECC.VLDTPOL.D1%%ODATE,            01650002
//             DISP=(NEW,CATLG,DELETE),                                 01660002
//             SPACE=(CYL,(150,100),RLSE),                              01670002
//             RECFM=FB,                                                01680002
//             UNIT=3390,                                               01690002
//             BLKSIZE=0                                                01700002
//SYSIN    DD  *                                                        01710002
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    01720002
  SELECT * FROM                                                         01730002
  MBVP.VLDTPOL                                                          01740002
  WHERE (VPOL_SITUAC <> 'OA')                                           01750002
    AND (VPOL_FVALOR <> 99999999)                                       01760002
    AND ((VPOL_FECHEJ >= %%FECDES AND VPOL_FECHEJ <= %%FECHAS)   OR     01770002
         ((VPOL_FVALOR >= %%FECDES AND VPOL_FVALOR <= %%FECHAS) AND     01780002
           VPOL_CLACONT IN (3, 6)))                                     01790002
//*--------------------------------------------------------------------*01800002
//* PASO     : VLEDC050                                                *01810002
//* PGRM/UTIL: ADUUMAIN                                                *01820002
//* DESCRIP  : DESCARGA DE TABLA VLDTPOL. PARA MOVIMIENTO ECONOMICO.   *01830002
//*--------------------------------------------------------------------*01840002
//VLEDC050 EXEC PGM=ADUUMAIN,                                           01850002
//             REGION=0M,                                               01860002
//             PARM='DSNP,VLDTPOLA%%E,NEW,,MSGLEVEL(1)',COND=(0,NE)     01870002
//STEPLIB  DD  DISP=SHR,                                                01880002
//             DSN='BMC.DB2.LOAD'                                       01890002
//         DD  DISP=SHR,                                                01900002
//             DSN='LDB2DPPA.SDSNEXIT'                                  01910002
//         DD  DISP=SHR,                                                01920002
//             DSN='LDB2DPPA.SDSNLOAD'                                  01930002
//SORTWK01 DD  UNIT=3390,                                               01940002
//             SPACE=(CYL,(50,10),RLSE)                                 01950002
//SYSPRINT DD  SYSOUT=*                                                 01960002
//UTPRINT  DD  SYSOUT=*                                                 01970002
//SYSOUT   DD  SYSOUT=*                                                 01980002
//SYSTRACE DD  SYSOUT=*                                                 01990002
//SYSPUNCH DD  SYSOUT=*                                                 02000002
//SYSCNTL1 DD  DUMMY                                                    02010002
//SYSREC   DD  DSN=PEBP.VLFD.FIX.UNLOEECC.POLMOVE.D1%%ODATE,            02020002
//             DISP=(NEW,CATLG,DELETE),                                 02030002
//             SPACE=(CYL,(150,100),RLSE),                              02040002
//             RECFM=FB,                                                02050002
//             UNIT=3390,                                               02060002
//             BLKSIZE=0                                                02070002
//SYSIN    DD  *                                                        02080002
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    02090002
  SELECT * FROM                                                         02100002
  MBVP.VLDTPOL                                                          02110002
  WHERE (VPOL_SITUAC <> 'OA')                                           02120002
    AND ((VPOL_FECHEJ  > 20161231) OR (SUBSTR(VPOL_CTAPER,11,2) = '91'))02130002
    AND (VPOL_FVALOR <> 99999999)                                       02140002
    AND ((VPOL_FECHEJ >= %%FECDES AND VPOL_FECHEJ <= %%FECHAS)          02150002
     OR  (VPOL_FVALOR >= %%FECDES AND VPOL_FVALOR <= %%FECHAS))         02160002
//*--------------------------------------------------------------------*02170002
//* PASO     : VLEDC060                                                *02180002
//* PGRM/UTIL: ADUUMAIN                                                *02190002
//* DESCRIP  : DESCARGA DE TABLA VLDTOPE. PARA MOVIMIENTO ECONOMICO.   *02200002
//*--------------------------------------------------------------------*02210002
//VLEDC060 EXEC PGM=ADUUMAIN,                                           02220002
//             REGION=0M,                                               02230002
//             PARM='DSNP,VLDTOPEB%%E,NEW,,MSGLEVEL(1)',COND=(0,NE)     02240002
//STEPLIB  DD  DISP=SHR,                                                02250002
//             DSN='BMC.DB2.LOAD'                                       02260002
//         DD  DISP=SHR,                                                02270002
//             DSN='LDB2DPPA.SDSNEXIT'                                  02280002
//         DD  DISP=SHR,                                                02290002
//             DSN='LDB2DPPA.SDSNLOAD'                                  02300002
//SORTWK01 DD  UNIT=3390,                                               02310002
//             SPACE=(CYL,(50,10),RLSE)                                 02320002
//SYSPRINT DD  SYSOUT=*                                                 02330002
//UTPRINT  DD  SYSOUT=*                                                 02340002
//SYSOUT   DD  SYSOUT=*                                                 02350002
//SYSTRACE DD  SYSOUT=*                                                 02360002
//SYSPUNCH DD  SYSOUT=*                                                 02370002
//SYSCNTL1 DD  DUMMY                                                    02380002
//SYSREC   DD  DSN=PEBP.VLFD.FIX.UNLOEECC.VLDTOPE.D1%%ODATE,            02390002
//             DISP=(NEW,CATLG,DELETE),                                 02400002
//             SPACE=(CYL,(150,100),RLSE),                              02410002
//             RECFM=FB,                                                02420002
//             UNIT=3390,                                               02430002
//             BLKSIZE=0                                                02440002
//SYSIN    DD  *                                                        02450002
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    02460002
  SELECT * FROM                                                         02470002
  MBVP.VLDTOPE                                                          02480002
  WHERE (VOPE_FCONTA >= %%FECDES AND VOPE_FCONTA <= %%FECHAS)           02490002
    AND  VOPE_FORMAT IN ('D','A')                                       02500002
    AND  VOPE_TIPOP  IN ('D','V','R','T')                               02510002
    AND  VOPE_SITUAC = '2'                                              02520002
  ORDER BY VOPE_PAVAL,VOPE_VALOR,VOPE_ISIN,VOPE_FECHOP,VOPE_FORMAT      02530002
//*--------------------------------------------------------------------*02540002
//* PASO     : VLEDC070                                                *02550002
//* PGRM/UTIL: VL4CEDCD                                                *02560002
//* DESCRIP  : GENERA DETALLE DE OP. FINANCIERA DESDE CABECERA VLDTOPE.*02570002
//*--------------------------------------------------------------------*02580002
//VLEDC070 EXEC  PGM=IKJEFT1A,COND=(0,NE)                               02590002
//SYSOUT   DD  SYSOUT=*                                                 02600002
//SYSPRINT DD  SYSOUT=*                                                 02610002
//SYSABOUT DD  SYSOUT=*                                                 02620002
//SYSDBOUT DD  SYSOUT=*                                                 02630002
//SYSTSPRT DD  SYSOUT=*                                                 02640002
//QRLSDB2  DD  SYSOUT=*                                                 02650002
//E1DQEDCD DD  DSN=PEBP.VLFD.FIX.UNLOEECC.VLDTOPE.D1%%ODATE,DISP=SHR    02660002
//S1DQEDCD DD  DSN=PEBP.VLFD.FIX.VL4CEDCD.VLDTDET.D1%%ODATE,            02670002
//             DISP=(NEW,CATLG,DELETE),                                 02680002
//             SPACE=(CYL,(100,50),RLSE),UNIT=3390,                     02690002
//             DCB=(LRECL=160,RECFM=FB,BLKSIZE=0)                       02700002
//SYSTSIN  DD  *                                                        02710002
  DSN SYSTEM(DSNP)                                                      02720002
  RUN PROGRAM(VL4CEDCD) PLAN(BVPVLPB) PARM('%%FECHAS')                  02730002
//*--------------------------------------------------------------------*02740002
//* PASO     : VLEDC075                                                *02750002
//* PGRM/UTIL: ICEMAN                                                  *02760002
//* DESCRIP  : ORDENA DETALLE POR VALOR, FECHA CORTE Y FORMATO         *02770002
//*--------------------------------------------------------------------*02780002
//VLEDC075 EXEC PGM=ICEMAN,COND=(4,LT)                                  02790002
//SORTIN   DD DSN=PEBP.VLFD.FIX.VL4CEDCD.VLDTDET.D1%%ODATE,             02800002
//            DISP=SHR                                                  02810002
//SORTOUT  DD DSN=PEBP.VLFD.FIX.UNLOEECC.VLDTDET.D1%%ODATE,             02820002
//            DISP=(NEW,CATLG,DELETE),UNIT=3390,                        02830002
//            SPACE=(CYL,(500,250),RLSE),                               02840002
//            DCB=(RECFM=FB,BLKSIZE=0,DSORG=PS,LRECL=160)               02850002
//SYSPRINT DD SYSOUT=*                                                  02860002
//SYSOUT   DD SYSOUT=*                                                  02870002
//SYSIN    DD *                                                         02880002
  SORT FIELDS=(6,12,CH,A,1,5,PD,A,18,1,CH,A)                            02890002
  END                                                                   02900002
//*--------------------------------------------------------------------*02910002
//* PASO     : VLEDC080                                                *02920006
//* PGRM/UTIL: ADUUMAIN                                                *02930002
//* DESCRIP  : DESCARGA DE TABLA VLDTSMM. PARA MOVIMIENTO ECONOMICO.   *02940002
//*--------------------------------------------------------------------*02950002
//VLEDC080 EXEC PGM=ADUUMAIN,                                           02960002
//             REGION=0M,                                               02970002
//             PARM='DSNP,VLDTSMMB%%E,NEW,,MSGLEVEL(1)',COND=(0,NE)     02980002
//STEPLIB  DD  DISP=SHR,                                                02990002
//             DSN='BMC.DB2.LOAD'                                       03000002
//         DD  DISP=SHR,                                                03010002
//             DSN='LDB2DPPA.SDSNEXIT'                                  03020002
//         DD  DISP=SHR,                                                03030002
//             DSN='LDB2DPPA.SDSNLOAD'                                  03040002
//SORTWK01 DD  UNIT=3390,                                               03050002
//             SPACE=(CYL,(50,10),RLSE)                                 03060002
//SYSPRINT DD  SYSOUT=*                                                 03070002
//UTPRINT  DD  SYSOUT=*                                                 03080002
//SYSOUT   DD  SYSOUT=*                                                 03090002
//SYSTRACE DD  SYSOUT=*                                                 03100002
//SYSPUNCH DD  SYSOUT=*                                                 03110002
//SYSCNTL1 DD  DUMMY                                                    03120002
//SYSREC   DD  DSN=PEBP.VLFD.FIX.UNLOEECC.VLDTSMM.D1%%ODATE,            03130002
//             DISP=(NEW,CATLG,DELETE),                                 03140002
//             SPACE=(CYL,(150,100),RLSE),                              03150002
//             RECFM=FB,                                                03160002
//             UNIT=3390,                                               03170002
//             BLKSIZE=0                                                03180002
//SYSIN    DD  *                                                        03190002
  UNLOAD SHRLEVEL CHANGE DIRECT AUTO                                    03200002
  SELECT * FROM                                                         03210002
  MBVP.VLDTSMM                                                          03220002
  WHERE VSMM_FECONTA >= %%FECDESE AND VSMM_FECONTA <= %%FECHASE         03230002
    AND (VSMM_TRANSACION_OR = 'L3J'                                     03240002
     OR  SUBSTR(VSMM_OBSERVA,1,21) = 'DERECHOS DE CUSTODIA ')           03250002
  ORDER BY VSMM_CTAVAL, VSMM_FECONTA, VSMM_NUMREF                       03260002
//**********************************************************************03270002
//* PASO     : VLEDC090                                                *03280006
//* EJECUCION PROGRAMA VL4CEDCR                                        *03290004
//* CTA REGISTRO SIN MOV. EN EL MES DE PROCESO                         *03300002
//**********************************************************************03310002
//VLEDC090 EXEC PGM=IKJEFT1A,COND=(0,NE)                                03320002
//SYSOUT   DD  SYSOUT=*                                                 03330002
//SYSPRINT DD  SYSOUT=*                                                 03340002
//SYSABOUT DD  SYSOUT=*                                                 03350002
//SYSDBOUT DD  SYSOUT=*                                                 03360002
//SYSTSPRT DD  SYSOUT=*                                                 03370002
//QRLSDB2  DD  SYSOUT=*                                                 03380002
//SYSUDUMP DD  SYSOUT=*                                                 03390002
//EEDC3E01 DD DSN=PEBP.VLFD.FIX.UNLOEECC.VLDTARC.D1%%ODATE,DISP=SHR     03400002
//EEDC3S01 DD DSN=PEBP.VLFD.FIX.VLDTSMM.SINMOVIM.D1%%ODATE,             03410002
//            DISP=(NEW,CATLG,DELETE),UNIT=3390,                        03420002
//            SPACE=(CYL,(100,50),RLSE),                                03430002
//            DCB=(RECFM=FB,BLKSIZE=0,DSORG=PS,LRECL=202)               03440002
//SYSTSIN  DD  *                                                        03450002
  DSN SYSTEM(DSNP)                                                      03460002
  RUN PROGRAM(VL4CEDCR) PLAN(BVPVLPB) PARM('%%FECHAS')                  03470004
/*                                                                      03480002
//*--------------------------------------------------------------------*03490002
//* FIN DE JCL                                                         *03500002
//*--------------------------------------------------------------------*03510002
