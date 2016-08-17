begin;

CREATE TEMPORARY TABLE measure_tmp (
    station_external_id varchar(10) NOT NULL,
    variable_id integer NOT NULL,
    technique integer NOT NULL,
    periodicity integer NOT NULL,
    date date NOT NULL,
    hour integer NOT NULL,
    value numeric NOT NULL,
    valid boolean NOT NULL
);
CREATE UNIQUE INDEX idx_measure_tmp ON measure_tmp USING btree (station_external_id, variable_id, date, hour);

\copy measure_tmp FROM '/tmp/measure_tmp.tsv' WITH CSV DELIMITER E'\t' HEADER;

UPDATE measure m
SET technique = t.technique,
    periodicity = t.periodicity,
    value = t.value,
    valid = t.valid
FROM measure_tmp t, station s
WHERE t.station_external_id = s.external_id
AND m.station_id = s.id
AND m.variable_id = t.variable_id
AND m.date = t.date
AND m.hour = t.hour;

INSERT INTO measure (station_id, variable_id, technique, periodicity, date, hour, value, valid)
SELECT s.id, t.variable_id, t.technique, t.periodicity, t.date, t.hour, t.value, t.valid
FROM measure_tmp t
JOIN station s ON s.external_id = t.station_external_id
LEFT JOIN measure m ON m.variable_id = t.variable_id AND m.date = t.date AND m.hour = t.hour
                    AND s.id = m.station_id
WHERE m.id IS NULL AND s.id IS NOT NULL
ORDER by station_id, variable_id, date, hour;

commit;
