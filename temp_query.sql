WITH data_tot AS (
  SELECT * FROM data
  JOIN metadata meta ON data.meta_id=meta.id
  WHERE variable_id=1
),
mean_temp AS (
  SELECT data_tot.id,
  AVG(value) AS t_avg
  FROM data_tot
  GROUP BY data_tot.id
),
day_temp AS (
  SELECT data_tot.id,
  AVG(value) AS t_day
  FROM data_tot
  WHERE date_part('hour', tstamp) >= 6 AND date_part('hour', tstamp) < 18
  GROUP BY data_tot.id
),
night_temp AS (
  SELECT data_tot.id,
  AVG(value) AS t_night
  FROM data_tot
  WHERE date_part('hour', tstamp) < 6 OR date_part('hour', tstamp) >= 18
  GROUP BY data_tot.id
),
t_var AS (
  SELECT tv.id,
  t_max - t_min AS t_t_var
  FROM (
    SELECT  data_tot.id,
    		date_trunc('day', tstamp) AS day,
    MIN(value) AS t_min,
    MAX(value) AS t_max
    FROM data_tot
    GROUP BY data_tot.id, date_trunc('day', tstamp)
  ) tv
),
amount AS (
  SELECT id, count(*) AS "count" FROM data_tot GROUP BY id
)
SELECT  mean_temp.t_avg,
		day_temp.t_day,
		night_temp.t_night,
		t_var.t_t_var,
		amount."count",
		meta.*
FROM metadata meta
JOIN mean_temp ON meta.id=mean_temp.id
JOIN day_temp ON meta.id=day_temp.id
JOIN night_temp ON meta.id=night_temp.id
JOIN amount ON meta.id=amount.id
JOIN t_var ON meta.id=t_var.id