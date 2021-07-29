DO
$do$
DECLARE
    _version INT := 27;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        ALTER TABLE judgements ALTER COLUMN bonus_ok DROP NOT NULL;
        ALTER TABLE judgements ALTER COLUMN bonus_ok SET DEFAULT NULL;
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
