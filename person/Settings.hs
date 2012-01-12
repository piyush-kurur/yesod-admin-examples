{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
module Settings
       ( database
       , poolSize
       , port
       , withConnPool
       , runConnPool
       , ConnectionPool
       , SqlPersist
       ) where


import Database.Persist.GenericSql
import Database.Persist.Sqlite



database = "person.db"
poolSize = 10
port     = 3000

withConnPool = withSqlitePool database poolSize
runConnPool  = runSqlPool
