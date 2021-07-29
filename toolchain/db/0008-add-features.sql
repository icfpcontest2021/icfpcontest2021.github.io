DO
$do$
DECLARE
    _version INT := 8;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        CREATE TABLE features (
            id BOOL PRIMARY KEY DEFAULT TRUE,
            bonuses BOOL NOT NULL DEFAULT FALSE,
            -- Only allow a single row.
            CONSTRAINT features_id_single CHECK (id)
        );
        INSERT INTO features DEFAULT VALUES;
        REVOKE DELETE, TRUNCATE ON features FROM public;
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
