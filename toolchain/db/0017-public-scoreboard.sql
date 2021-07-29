DO
$do$
DECLARE
    _version INT := 17;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        CREATE TABLE public_scoreboard (
             team_id UUID PRIMARY KEY REFERENCES teams(id),
             score DOUBLE PRECISION NOT NULL
        );
        CREATE INDEX public_scoreboard_score
        ON public_scoreboard (score ASC);
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
