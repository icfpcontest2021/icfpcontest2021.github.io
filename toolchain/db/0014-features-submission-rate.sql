DO
$do$
DECLARE
    _version INT := 14;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        ALTER TABLE features ADD COLUMN submission_rate_seconds INT;
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
