CREATE TABLE IF NOT EXISTS db_version (
  version INT NOT NULL
);

INSERT INTO db_version (version)
SELECT 1
WHERE NOT EXISTS (SELECT * FROM db_version);
