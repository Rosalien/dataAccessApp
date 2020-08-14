# Sql query to create and refresh materialized view

- MaterializedView_Prod.sql : sql query to create materialized view in production
- TriggerMaterializedView_Prod.sql : Triggers function to update data_infraj_prod materialized view
- TriggerDebouncePeriod.sql : Triggers function to update carac_data_sensor_method_prod materialized view
- RefreshMaterializedView.sh : Bash script to refresh carac_data_sensor_method_prod materialized view