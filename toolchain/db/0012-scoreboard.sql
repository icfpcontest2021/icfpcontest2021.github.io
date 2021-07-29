DO
$do$
DECLARE
    _version INT := 12;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        CREATE TABLE scoreboard (
             team_id UUID PRIMARY KEY REFERENCES teams(id),
             score DOUBLE PRECISION NOT NULL
        );
        CREATE INDEX scoreboard_score ON scoreboard (score ASC);
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
