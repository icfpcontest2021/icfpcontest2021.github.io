DO
$do$
DECLARE
    _version INT := 24;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        ALTER TABLE teams ADD COLUMN region TEXT;
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
