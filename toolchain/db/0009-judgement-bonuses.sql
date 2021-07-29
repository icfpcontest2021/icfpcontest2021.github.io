DO
$do$
DECLARE
    _version INT := 9;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        DROP TABLE bonuses;
        CREATE TABLE judgements_bonuses (
            judgement_id INT NOT NULL REFERENCES judgements(id),
            bonus TEXT NOT NULL,
            problem_id INT NOT NULL REFERENCES problems(id),
            get BOOL NOT NULL
        );
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
