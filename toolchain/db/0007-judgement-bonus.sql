DO
$do$
DECLARE
    _version INT := 7;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        ALTER TABLE judgements ADD COLUMN bonus_ok BOOL NOT NULL DEFAULT FALSE;
        CREATE TABLE bonuses (
            judgement_id INT NOT NULL REFERENCES judgements(id),
            bonus TEXT NOT NULL,
            problem_id INT NOT NULL REFERENCES problems(id)
        );
        CREATE VIEW latest_judgements AS
        SELECT DISTINCT ON (solution_id) *
        FROM judgements
        ORDER BY solution_id, completed_at DESC;
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
