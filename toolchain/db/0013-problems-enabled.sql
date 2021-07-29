DO
$do$
DECLARE
    _version INT := 13;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        ALTER TABLE problems ADD COLUMN enabled BOOL NOT NULL DEFAULT FALSE;
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
