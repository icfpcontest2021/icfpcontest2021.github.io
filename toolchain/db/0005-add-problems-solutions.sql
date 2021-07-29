DO
$do$
DECLARE
    _version INT := 5;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        CREATE TABLE problems (
            id SERIAL NOT NULL PRIMARY KEY,
            body TEXT NOT NULL
        );
        CREATE TABLE solutions (
            id UUID NOT NULL PRIMARY KEY DEFAULT uuid_generate_v4(),
            problem_id INT NOT NULL REFERENCES problems(id),
            team_id UUID NOT NULL REFERENCES teams(id),
            body TEXT NOT NULL,
            submitted_at TIMESTAMPTZ NOT NULL
        );
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
