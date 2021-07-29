DO
$do$
DECLARE
    _version INT := 21;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        ALTER TABLE features DROP COLUMN bonuses;
        ALTER TABLE features ADD COLUMN bonus_superflex_enable_at TIMESTAMPTZ;
        ALTER TABLE features ADD COLUMN bonus_globalist_enable_at TIMESTAMPTZ;
        ALTER TABLE features ADD COLUMN bonus_breakaleg_enable_at TIMESTAMPTZ;
        ALTER TABLE features ADD COLUMN bonus_wallhack_enable_at TIMESTAMPTZ;
        UPDATE db_version SET version = _version;
    END IF;
END
$do$
