DO
$do$
DECLARE
    _version INT := 25;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        ALTER TABLE features ADD COLUMN submission_limit_kb INT NOT NULL DEFAULT 32;
    END IF;
END
$do$
