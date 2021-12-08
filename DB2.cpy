--UPDATE
UPDATE GPBT.VLDTCON 
    SET 
        VCON_CUENTA = 501865
WHERE
 VCON_FCONTA = 20191113 AND 
 VCON_REFER = 090098940 

--SELECT
SELECT * FROM GPBT.VLDTCON WHERE VCON_REFER = 090098941
SELECT * FROM GPBT.VLDTCOM WHERE VCOM_SITUAC = 'A'
SELECT * FROM GPBT.VLDTARC WHERE VARC_SITUAC = 'A'

SELECT colname, typename, length, scale, default, nulls
  FROM syscat.columns
 WHERE tabname = 'VLDTCOM'
   AND tabschema = 'GPBT'

SELECT  VCOM_CORRE_FIJO,   
        VCOM_CORRE_MAXIMO, 
        VCOM_CORRE_MINIMO,
        VCOM_CORRE_PORCEN 
FROM GPBT.VLDTCOM WHERE VCOM_SITUAC  = 'A' AND 
     VCOM_CUENTA = 032108, 000804

--MUESTRA DATO DE COLUMNAS DE UNA TABLA
   Select distinct(name), ColType, Length 
   from Sysibm.syscolumns where tbname = 'VLDTARC';

SELECT  VCAM_CIERRE_D,
        VCAM_FECDIA,
        VCAM_FILLER
FROM GPBT.VLDTCAM

SELECT * FROM MBVP.VLDTCOM 
  WHERE VCOM_CUENTA = 000071

SELECT * FROM GPBT.VLDTXTA
WHERE VXTA_OPECON (48, 49)

SELECT VARC_CTAVAL20, 
       VARC_NUMCLI,
       VARC_FEULMOD,
       VARC_SITUAC
       FROM GPBT.VLDTARC 
WHERE VARC_FEULMOD = 20091124 AND
      VARC_NUMCLI = 20064205 AND
      VARC_CTAVAL20 = 00110239009105001751

SELECT VARC_CTAVAL20, 
       VARC_NUMCLI,
       VARC_FEULMOD,
       VARC_SITUAC
       FROM GPBT.VLDTARC 
WHERE 
      VARC_NUMCLI = 20056050 AND
      VARC_FEULMOD = 20091124

 SELECT VARC_FEULMOD FROM GPBC.VLDTARC  WHERE VARC_SITUAC = 'B' 

20121016
20121016
20121016

SELECT VHIS_CTAVAL,  
VHIS_CODVALOR,
VHIS_TIPGAS,  
VHIS_ANO,     
VHIS_MES,
VHIS_TITULOS29, 
VHIS_MOVIMI29,  
VHIS_CUSTODIA29,
VHIS_CAMBIO29,  
VHIS_COBRADO29,
VHIS_FEULMOD,
VHIS_USUARIO      
 FROM MBVP.VLDTHIS WHERE VHIS_FEULMOD = '20211118' 
 AND VHIS_USUARIO = 'L4CEDC8'

 SELECT (*) COUNT  FROM MBVP.VLDTHIS WHERE VHIS_FEULMOD = '20211118' 
 AND VHIS_USUARIO = 'L4CEDC8' --5662

SELECT * FROM MBVP.VLDTARC WHERE VARC_CUENTA = 804

SELECT *    
 FROM MBVP.VLDTHIS WHERE VHIS_ANO = 2021 
 AND VHIS_MES = 10 AND
 VHIS_MOVIMI29 > 0