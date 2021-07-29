DO
$do$
DECLARE
    _version INT := 10;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        CREATE VIEW latest_solutions AS
        SELECT DISTINCT ON (problem_id) *
        FROM solutions
        ORDER BY problem_id, submitted_at DESC;
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
