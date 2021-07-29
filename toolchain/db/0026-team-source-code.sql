DO
$do$
DECLARE
    _version INT := 26;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        ALTER TABLE features ADD COLUMN source_code_limit_kb INT NOT NULL DEFAULT 8192;
        CREATE TABLE teams_source_code (
            team_id UUID NOT NULL REFERENCES teams(id),
            submitted_at TIMESTAMPTZ NOT NULL,
            sha256 TEXT NOT NULL,
            archive BYTEA NOT NULL
        );
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
