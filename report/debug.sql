-- !preview conn=con

--   with
--   buf_aoi as (
--    select ST_BUFFER(('SRID=4326;POLYGON ((-124.0978 34.14489, -121.7816 31.57838, -117.5698 33.91363, -117.0261 35.37949, -124.0978 34.14489))'::geometry)::geography, 10 * 1852) as geom)
  SELECT
  cmn_name || ' (' || sci_name || ')' AS "Species", bia_type AS "Behavior",
  bia_time AS "Time", bia_name AS "Place", 
  -- ST_GeometryType(geometry) AS geom_type, 
  -- ST_IsSimple(geometry) AS geom_simple, 
  -- ST_IsValid(geometry) AS geom_valid,
  geometry
  FROM
  "shp_CetMap_BIA_WGS84" 
  -- as ds
  -- inner join buf_aoi on st_intersects(ds.geometry, buf_aoi.geom );
  LIMIT 1;
