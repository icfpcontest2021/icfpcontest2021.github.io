DO
$do$
DECLARE
    _version INT := 28;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        CREATE INDEX ON solutions (submitted_at DESC);
        CREATE INDEX ON judgements (completed_at DESC);
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
