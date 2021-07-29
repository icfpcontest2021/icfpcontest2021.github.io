DO
$do$
DECLARE
    _version INT := 4;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        ALTER TABLE teams ADD COLUMN api_key UUID NOT NULL UNIQUE DEFAULT uuid_generate_v4();
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
