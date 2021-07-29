DO
$do$
DECLARE
    _version INT := 20;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        ALTER TABLE judgements RENAME COLUMN score TO dislikes;
        DROP VIEW latest_judgements;
        CREATE VIEW latest_judgements AS
        SELECT DISTINCT ON (solution_id) *
        FROM judgements
        ORDER BY solution_id, completed_at DESC;
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
