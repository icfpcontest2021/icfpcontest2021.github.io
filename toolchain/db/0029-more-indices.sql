DO
$do$
DECLARE
    _version INT := 29;
BEGIN
    IF EXISTS (SELECT version FROM db_version WHERE version < _version) THEN
        CREATE INDEX ON judgements (solution_id);

        CREATE INDEX ON judgements_bonuses (judgement_id);
        CREATE INDEX ON judgements_bonuses (problem_id);

        CREATE INDEX ON solutions (problem_id);
        CREATE INDEX ON solutions (team_id);

        CREATE INDEX ON teams (login_id);

        UPDATE db_version SET version = _version;
    END IF;
END
$do$
