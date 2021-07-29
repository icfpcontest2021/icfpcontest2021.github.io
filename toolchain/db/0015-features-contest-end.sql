DO
$do$
DECLARE
    _version INT := 15;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        ALTER TABLE features ADD COLUMN scoreboard_freeze_at TIMESTAMPTZ;
        ALTER TABLE features ADD COLUMN submission_end_at TIMESTAMPTZ;
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
