# Dated investigation fixture. Run only against a disposable/scratch database.
# PGHOST and PGDATABASE must explicitly select it. This fixture creates the
# `mood` ENUM and `pair` composite type if absent; it does not delete or replace
# existing database objects. No host or database is inferred from an old run.
postgres_fixture_connection <- function() {
  host <- Sys.getenv('PGHOST')
  database <- Sys.getenv('PGDATABASE')
  if (!nzchar(host) || !nzchar(database)) {
    stop('Set PGHOST and PGDATABASE to an explicitly selected disposable/scratch PostgreSQL database.')
  }
  con <- DBI::dbConnect(RPostgres::Postgres(), host = host, dbname = database)
  success <- FALSE
  on.exit(if (!success) DBI::dbDisconnect(con), add = TRUE)
  for (type in c('mood', 'pair')) {
    present <- DBI::dbGetQuery(con, paste0("SELECT to_regtype('", type, "') IS NOT NULL AS present"))$present
    if (present) next
    statement <- switch(type,
      mood = "CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy')",
      pair = 'CREATE TYPE pair AS (i integer, s text)')
    DBI::dbExecute(con, statement)
  }
  success <- TRUE
  con
}

postgres_types <- list(
  smallint = '1::smallint', integer = '1::integer',
  bigint = '9007199254740995::bigint', numeric = '1.23::numeric(20,5)',
  real = '1.25::real', double = '1.25::double precision',
  text = "'a'::text", char = "'a'::char(5)", varchar = "'a'::varchar(5)",
  bool = 'true', date = "'2026-01-01'::date",
  timestamp = "'2026-01-01'::timestamp",
  timestamptz = "'2026-01-01 12:00:00+09'::timestamptz",
  time = "'12:00:00'::time", timetz = "'12:00:00+09'::timetz",
  interval = "'1 day'::interval", bytea = "'abc'::bytea",
  uuid = "'550e8400-e29b-41d4-a716-446655440000'::uuid",
  json = "'{\"a\":1}'::json", jsonb = "'{\"a\":1}'::jsonb",
  arr = 'ARRAY[1,2]', arr2 = 'ARRAY[ARRAY[1,2],ARRAY[3,4]]',
  pair = "ROW(1,'a')::pair", mood = "'ok'::mood", point = 'point(1,2)',
  inet = "'127.0.0.1'::inet", macaddr = "'08:00:2b:01:02:03'::macaddr",
  bit = "B'101'::bit(3)", varbit = "B'101'::bit varying",
  range = "'[1,4)'::int4range", multirange = "'{[1,4),[6,9)}'::int4multirange",
  tsvector = "'fat rats'::tsvector", xml = "'<x>3</x>'::xml"
)
