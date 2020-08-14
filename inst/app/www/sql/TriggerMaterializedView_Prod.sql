--Pour data_infraj_prod
CREATE OR REPLACE FUNCTION refresh_data_infraj_prod() RETURNS trigger AS
$$
BEGIN
    REFRESH MATERIALIZED VIEW data_infraj_prod;
    RETURN NULL;
END;
$$
LANGUAGE plpgsql ;

create trigger refresh_data_infraj_prod
after insert or update or delete or truncate
on public.insertion_dataset_ids for each statement 
execute procedure refresh_data_infraj_prod();
