DO
$do$
DECLARE
    _version INT := 16;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        ALTER TABLE features ADD COLUMN lightning_end_at TIMESTAMPTZ;
        CREATE TABLE lightning_scoreboard (
             team_id UUID PRIMARY KEY REFERENCES teams(id),
             score DOUBLE PRECISION NOT NULL
        );
        CREATE INDEX lightning_scoreboard_score
        ON lightning_scoreboard (score ASC);
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
