-- Stepper schema.  Tables are singular.

CREATE TABLE IF NOT EXISTS state_machine (
  id         TEXT PRIMARY KEY,
  name       TEXT NOT NULL UNIQUE,
  definition TEXT NOT NULL,            -- ASL document, JSON
  created_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
  updated_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now'))
);

CREATE TABLE IF NOT EXISTS execution (
  id               TEXT PRIMARY KEY,
  state_machine_id TEXT NOT NULL REFERENCES state_machine(id),
  name             TEXT NOT NULL,
  status           TEXT NOT NULL DEFAULT 'RUNNING',  -- RUNNING | SUCCEEDED | FAILED | TIMED_OUT | ABORTED
  input            TEXT,               -- JSON
  output           TEXT,               -- JSON
  error            TEXT,
  cause            TEXT,
  started_at       TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
  stopped_at       TEXT,
  UNIQUE (state_machine_id, name)
);

CREATE TABLE IF NOT EXISTS execution_event (
  id           INTEGER PRIMARY KEY AUTOINCREMENT,
  execution_id TEXT NOT NULL REFERENCES execution(id),
  type         TEXT NOT NULL,          -- StateEntered, StateExited, ExecutionSucceeded, ...
  state_name   TEXT,
  detail       TEXT,                   -- JSON
  created_at   TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now'))
);

CREATE INDEX IF NOT EXISTS execution_event_execution_id ON execution_event (execution_id);

CREATE TABLE IF NOT EXISTS schedule (
  id               TEXT PRIMARY KEY,
  state_machine_id TEXT NOT NULL REFERENCES state_machine(id),
  expression       TEXT NOT NULL,      -- cron 'm h dom mon dow' or rate(N seconds|minutes|hours|days)
  input            TEXT,               -- JSON template for execution input
  enabled          INTEGER NOT NULL DEFAULT 1,
  next_run_at      TEXT,
  created_at       TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now'))
);
