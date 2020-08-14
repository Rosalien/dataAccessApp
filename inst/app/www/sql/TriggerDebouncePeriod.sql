--Selon https://onethingsimple.com/2017/10/sync-materialized-views-after-debounce-period/

-- Create the table
CREATE TABLE IF NOT EXISTS table_update_records (
  -- COLUMNS
  id SERIAL NOT NULL,
  table_name VARCHAR(50) NOT NULL,
  rel VARCHAR(50) NOT NULL,
  updated TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  -- CONSTRAINTS
  PRIMARY KEY (id),
  UNIQUE (table_name, rel)
);

-- ASSUME CHANGE WAS MADE IF TRIGGERED, NO QUESTIONS ASKED
CREATE OR REPLACE FUNCTION t_fn_update_table_change ()
  RETURNS TRIGGER AS
  $fn$
    DECLARE
      t_report_table_update    TEXT;
    BEGIN
      t_report_table_update := TG_ARGV[0];
      INSERT INTO table_update_records (table_name, rel) values (TG_TABLE_NAME, t_report_table_update)
        ON CONFLICT (table_name, rel)
        DO UPDATE
        SET updated = CURRENT_TIMESTAMP 
          WHERE table_update_records.table_name = TG_TABLE_NAME
          AND table_update_records.rel = t_report_table_update;
      RETURN NULL;
    END;
  $fn$
  LANGUAGE plpgsql;

create trigger t_report_table_update
after insert or update or delete or truncate on public.localisation  
execute procedure t_fn_update_table_change('carac_data_sensor_method_prod');

create trigger t_report_table_update
after insert or update or delete or truncate on public.realnode  
execute procedure t_fn_update_table_change('carac_data_sensor_method_prod');

create trigger t_report_table_update
after insert or update or delete or truncate on public.insertion_dataset_ids  
execute procedure t_fn_update_table_change('carac_data_sensor_method_prod');

create trigger t_report_table_update
after insert or update or delete or truncate on public.site  
execute procedure t_fn_update_table_change('carac_data_sensor_method_prod');

create trigger t_report_table_update
after insert or update or delete or truncate on public.site_snot  
execute procedure t_fn_update_table_change('carac_data_sensor_method_prod');

create trigger t_report_table_update
after insert or update or delete or truncate on public.composite_nodeable  
execute procedure t_fn_update_table_change('carac_data_sensor_method_prod');

create trigger t_report_table_update
after insert or update or delete or truncate on public.variable  
execute procedure t_fn_update_table_change('carac_data_sensor_method_prod');

create trigger t_report_table_update
after insert or update or delete or truncate on public.instrument  
execute procedure t_fn_update_table_change('refresh_carac_data_sensor_method_prod');

create trigger t_report_table_update
after insert or update or delete or truncate on public.periode_utilisation_instrument  
execute procedure t_fn_update_table_change('carac_data_sensor_method_prod');

create trigger t_report_table_update
after insert or update or delete or truncate on public.jeu  
execute procedure t_fn_update_table_change('carac_data_sensor_method_prod');

create trigger t_report_table_update
after insert or update or delete or truncate on public.methode_calcul  
execute procedure t_fn_update_table_change('carac_data_sensor_method_prod');

create trigger t_report_table_update
after insert or update or delete or truncate on public.periode_application_methode  
execute procedure t_fn_update_table_change('carac_data_sensor_method_prod');

------------------------------------------------------------------------------------
create or replace function fn_cron_smart_mv_sync(debounce_time INTERVAL, mat_view_name TEXT)
  RETURNS TEXT as $fn$
  DECLARE
    updates        TIMESTAMP[];
    last_table     TEXT;
    last_update    TIMESTAMP;
  BEGIN
    -- Select most recent updated table, if not mat_view_name it may need a refresh.
    SELECT table_name, updated INTO last_table, last_update 
      FROM table_update_records 
      WHERE rel = mat_view_name
      ORDER BY updated DESC;
    
    IF NOT FOUND THEN
      RETURN 'The table_update_records table has nothing related to ' || mat_view_name;
    END IF;
    
    -- Test if the materialized view is in sync (most recent recorded updated table)
    IF (last_table = mat_view_name) THEN
      RETURN 'Materialized View ' || mat_view_name || ' is in sync currently';
    END IF;
    
    -- The materialized view is out of sync, so we check if debounce period has passed.
    IF (age(current_timestamp, last_update) < debounce_time) THEN
      -- "debounce" period has not elapsed since the last time, so return. 
      RETURN 'Materialized View ' || mat_view_name || ' will be updated in '
             || age(last_update + debounce_time, current_timestamp) || '.';
    END IF;
    
    -- We do need to do a refresh, set updated to current_timestamp for materialized view
    -- This way if a table updates while the materialized view is refreshing, that update
    -- won't be swallowed.
    INSERT INTO table_update_records (table_name, rel) 
      VALUES (mat_view_name, mat_view_name)
      ON CONFLICT (table_name, rel) DO UPDATE
        SET updated = current_timestamp 
        WHERE table_update_records.table_name = mat_view_name
          AND table_update_records.rel = mat_view_name;
    
    -- Do the work
    EXECUTE 'REFRESH MATERIALIZED VIEW ' || mat_view_name::regclass;
      
    -- All Done
    RETURN 'Materialized View ' || mat_view_name || ' has been refreshed';
  END;
$fn$
language plpgsql;

--A lancer dans un crontab
select fn_cron_smart_mv_sync('10 minutes', 'carac_data_sensor_method_prod');
