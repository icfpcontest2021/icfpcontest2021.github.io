DO
$do$
DECLARE
    _version INT := 22;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        CREATE TABLE problems_min_dislikes (
            problem_id INT NOT NULL REFERENCES problems(id) PRIMARY KEY,
            dislikes INT NOT NULL
        );
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
