/*merge data while calculating duration of each ride*/
CREATE OR REPLACE TEMPORARY TABLE tempo
    SELECT rideable_type, started_at, ended_at, member_casual, ABS(ROUND(TIMESTAMPDIFF(SECOND, started_at, ended_at)/60,2)) AS duration
    FROM 2021_08
    UNION
    SELECT rideable_type, started_at, ended_at, member_casual, ABS(ROUND(TIMESTAMPDIFF(SECOND, started_at, ended_at)/60,2)) AS duration
    FROM 2021_09
    UNION
    SELECT rideable_type, started_at, ended_at, member_casual, ABS(ROUND(TIMESTAMPDIFF(SECOND, started_at, ended_at)/60,2)) AS duration
    FROM 2021_10
    UNION
    SELECT rideable_type, started_at, ended_at, member_casual, ABS(ROUND(TIMESTAMPDIFF(SECOND, started_at, ended_at)/60,2)) AS duration
    FROM 2021_11
     UNION
    SELECT rideable_type, started_at, ended_at, member_casual, ABS(ROUND(TIMESTAMPDIFF(SECOND, started_at, ended_at)/60,2)) AS duration
    FROM 2021_12
    UNION
    SELECT rideable_type, started_at, ended_at, member_casual, ABS(ROUND(TIMESTAMPDIFF(SECOND, started_at, ended_at)/60,2)) AS duration
    FROM 2022_01
    UNION
    SELECT rideable_type, started_at, ended_at, member_casual, ABS(ROUND(TIMESTAMPDIFF(SECOND, started_at, ended_at)/60,2)) AS duration
    FROM 2022_02
    UNION
    SELECT rideable_type, started_at, ended_at, member_casual, ABS(ROUND(TIMESTAMPDIFF(SECOND, started_at, ended_at)/60,2)) AS duration
    FROM 2022_03
    UNION
    SELECT rideable_type, started_at, ended_at, member_casual, ABS(ROUND(TIMESTAMPDIFF(SECOND, started_at, ended_at)/60,2)) AS duration
    FROM 2022_04
    UNION
    SELECT rideable_type, started_at, ended_at, member_casual, ABS(ROUND(TIMESTAMPDIFF(SECOND, started_at, ended_at)/60,2)) AS duration
    FROM 2022_05
    UNION
    SELECT rideable_type, started_at, ended_at, member_casual, ABS(ROUND(TIMESTAMPDIFF(SECOND, started_at, ended_at)/60,2)) AS duration
    FROM 2022_06
    UNION
    SELECT rideable_type, started_at, ended_at, member_casual, ABS(ROUND(TIMESTAMPDIFF(SECOND, started_at, ended_at)/60,2)) AS duration
    FROM 2022_07;

/*delete rows that has null values of duration or customer type, also remove inconsistencies in duration values*/
DELETE FROM tempo
WHERE duration < 5 OR duration >600 OR CONCAT(duration, member_casual) IS NULL;

/*aggregate the data to useful ones only*/
SELECT DATE_FORMAT(started_at, '%Y-%m-01') AS mon, member_casual, ROUND(AVG(duration),2) AS average_duration, ROUND(VAR(duration),2) AS var_dur, COUNT(*) AS total_rides, MIN(duration), MAX(duration)
FROM tempo
GROUP BY MONTH(started_at), member_casual;

SELECT DATE_FORMAT(started_at, '%W') AS weekday, member_casual, ROUND(AVG(duration),2) AS average_duration, ROUND(VAR(duration),2) AS var_dur, COUNT(*) AS total_rides, MIN(duration), MAX(duration)
FROM tempo
GROUP BY WEEKDAY(started_at), member_casual;