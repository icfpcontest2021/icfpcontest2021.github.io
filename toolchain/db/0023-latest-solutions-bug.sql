DO
$do$
DECLARE
    _version INT := 23;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        DROP VIEW latest_solutions;
        CREATE VIEW latest_solutions AS
        SELECT DISTINCT ON (problem_id, team_id) *
        FROM solutions
        ORDER BY problem_id, team_id, submitted_at DESC;
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
