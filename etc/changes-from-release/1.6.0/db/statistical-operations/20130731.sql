-- -------------------------------------------------------------------------------------
-- Modificación del metadato URN_INTERNAL y URN por URN y URN_PROVIDER respectivamente
-- -------------------------------------------------------------------------------------

ALTER TABLE TB_EXTERNAL_ITEMS RENAME COLUMN URN_INTERNAL TO URN_PROVIDER;

UPDATE TB_EXTERNAL_ITEMS SET URN = URN_PROVIDER WHERE URN is null AND TYPE LIKE 'structuralResources#%';


				  	