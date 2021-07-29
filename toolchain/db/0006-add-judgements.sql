DO
$do$
DECLARE
    _version INT := 6;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        CREATE TYPE judgement_status AS ENUM (
            'IN_PROGRESS',
            'VALID',
            'INVALID',
            'ERROR'
        );
        CREATE TABLE judgements (
            id SERIAL NOT NULL PRIMARY KEY,
            solution_id UUID NOT NULL REFERENCES solutions(id),
            status judgement_status NOT NULL,
            started_at TIMESTAMPTZ NOT NULL,
            completed_at TIMESTAMPTZ
        );
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
