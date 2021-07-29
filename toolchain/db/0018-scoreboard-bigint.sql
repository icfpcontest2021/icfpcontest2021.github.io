DO
$do$
DECLARE
    _version INT := 18;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        ALTER TABLE scoreboard ALTER COLUMN score TYPE BIGINT;
        ALTER TABLE public_scoreboard ALTER COLUMN score TYPE BIGINT;
        ALTER TABLE lightning_scoreboard ALTER COLUMN score TYPE BIGINT;
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
