DO
$do$
DECLARE
    _version INT := 19;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        ALTER TABLE features ADD COLUMN web_password VARCHAR(32);
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
