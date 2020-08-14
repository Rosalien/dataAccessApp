#!/bin/sh
psql -U snouser -d sno -p 5432 -h localhost -c "select fn_cron_smart_mv_sync('1 minutes', 'carac_data_sensor_method_prod')";
