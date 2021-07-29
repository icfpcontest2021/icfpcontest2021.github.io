DO
$do$
DECLARE
    _version INT := 2;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        CREATE TABLE teams (
            id UUID NOT NULL PRIMARY KEY DEFAULT uuid_generate_v4(),
            -- We don't add a foreign key constrait here to the `login` table,
            -- because the `login` table gets created by the users library at
            -- runtime so it may not be set up yet.
            login_id INT NOT NULL UNIQUE
        );
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
