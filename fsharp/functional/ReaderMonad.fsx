#r "nuget: CCFSharpUtils"
#r "nuget: FSharpPlus"
#r "nuget: FsToolkit.ErrorHandling"
open CCFSharpUtils
open CCFSharpUtils.Collections
open CCFSharpUtils.Operators
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Operators

type Logger = { log: string -> unit }
type Database = { query: string -> string }
type AppEnv = { logger: Logger; db: Database }

let logMessage msg : Reader<AppEnv, unit> =
    Reader(fun env -> env.logger.log msg)

let queryDb sql : Reader<AppEnv, string> =
    Reader(fun env -> env.db.query sql)

let getUserData userId : Reader<AppEnv, string> =
    monad {
        do! logMessage $"Fetching user {userId}"
        let! result = queryDb $"SELECT * FROM users WHERE id={userId}"
        do! logMessage "User fetched successfully"
        return result
    }

let appEnv = {
    logger = { log = printfn "%s" }
    db = { query = fun sql -> $"Result of: {sql}" }
}

Reader.run (getUserData 333) appEnv
