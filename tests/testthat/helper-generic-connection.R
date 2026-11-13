# A DBIConnection that forwards to `inner` but is no class marginplyr or
# dbplyr recognises, so both take their generic ANSI path while a real engine
# executes the SQL. The class is defined once per session in its own
# environment; the identifiers it quotes are ANSI's.
generic_forwarding_connection <- local({
  where <- NULL
  function(inner) {
    if (is.null(where)) {
      where <<- new.env(parent = asNamespace("DBI"))
      define_forwarding_class(where)
    }
    methods::new("MarginplyrGenericConnection", inner = inner)
  }
})

define_forwarding_class <- function(where) {
  class <- "MarginplyrGenericConnection"
  methods::setClass(
    class, contains = "DBIConnection", slots = c(inner = "DBIConnection"),
    where = where
  )
  forward <- function(generic, signature, fun) {
    methods::setMethod(generic, signature, fun, where = where)
  }
  forward("dbSendQuery", c(class, "character"), function(conn, statement, ...) {
    DBI::dbSendQuery(conn@inner, statement, ...)
  })
  forward("dbGetQuery", c(class, "character"), function(conn, statement, ...) {
    DBI::dbGetQuery(conn@inner, statement, ...)
  })
  forward("dbExecute", c(class, "character"), function(conn, statement, ...) {
    DBI::dbExecute(conn@inner, statement, ...)
  })
  forward("dbExistsTable", c(class, "ANY"), function(conn, name, ...) {
    DBI::dbExistsTable(conn@inner, name, ...)
  })
  forward("dbListFields", c(class, "ANY"), function(conn, name, ...) {
    DBI::dbListFields(conn@inner, name, ...)
  })
  forward("dbQuoteIdentifier", c(class, "character"), function(conn, x, ...) {
    DBI::dbQuoteIdentifier(DBI::ANSI(), x, ...)
  })
  forward("dbQuoteIdentifier", c(class, "SQL"), function(conn, x, ...) x)
  forward("dbQuoteString", c(class, "character"), function(conn, x, ...) {
    DBI::dbQuoteString(DBI::ANSI(), x, ...)
  })
  # DBI's generics fix the argument name `dbObj` these methods must repeat.
  # nolint start: object_name_linter.
  forward("dbIsValid", class, function(dbObj, ...) {
    DBI::dbIsValid(dbObj@inner)
  })
  forward("dbGetInfo", class, function(dbObj, ...) {
    list(dbname = "generic", db.version = "1")
  })
  forward("dbDataType", class, function(dbObj, obj, ...) {
    DBI::dbDataType(dbObj@inner, obj, ...)
  })
  # nolint end
  registerS3method(
    "dbplyr_edition", class, function(con) 2L, envir = asNamespace("dbplyr")
  )
}
