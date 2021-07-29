DO
$do$
DECLARE
    _version INT := 30;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        UPDATE features SET bonus_superflex_enable_at = now();
        UPDATE features SET bonus_globalist_enable_at = now();
        UPDATE features SET bonus_breakaleg_enable_at = now();
        UPDATE features SET bonus_wallhack_enable_at = now();
        UPDATE features SET submission_rate_seconds = 300;
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
