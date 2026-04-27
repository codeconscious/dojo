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
type Database = { query: string -> string; execute: string -> unit }
type AppEnv = { logger: Logger; db: Database }

// ===== Fundamental reader operations =====
// Note that they all return Reader<AppEnv, 'a>.

let logMessage msg : Reader<AppEnv, unit> =
    Reader(fun env -> env.logger.log msg)

let queryDb sql : Reader<AppEnv, string> =
    Reader(fun env -> env.db.query sql)

let executeDb sql : Reader<AppEnv, unit> =
    Reader(fun env -> env.db.execute sql)

// ===== Application functions =====

let getUserData userId : Reader<AppEnv, string> =
    monad {
        do! logMessage $"[INFO] Fetching user {userId}"
        let! result = queryDb $"SELECT * FROM users WHERE id={userId}"
        do! logMessage $"[INFO] User {userId} fetched successfully"
        return result
    }

// Insert operation with validation
let createUser name email : Reader<AppEnv, bool> =
    monad {
        do! logMessage $"[INFO] Creating user: {name} ({email})"

        if String.length name < 2 then
            do! logMessage "[ERROR] Name too short"
            return false
        else
            do! executeDb $"INSERT INTO users (name, email) VALUES ('{name}', '{email}')"
            do! logMessage $"[INFO] User {name} created successfully"
            return true
    }

let findUserByEmail email : Reader<AppEnv, string option> =
    monad {
        do! logMessage $"[INFO] Searching for user with email: {email}"
        let! result = queryDb $"SELECT * FROM users WHERE email='{email}'"

        if result.Contains "not found" then
            do! logMessage $"[WARNING] No user found with email {email}"
            return None
        else
            do! logMessage $"[INFO] User found"
            return Some result
    }

// Chained operations
let updateUserEmail userId newEmail : Reader<AppEnv, bool> =
    monad {
        do! logMessage $"[INFO] Updating user {userId} email to {newEmail}"
        let! userData = getUserData userId

        if userData.Contains "not found" then
            do! logMessage $"[ERROR] User {userId} does not exist"
            return false
        else
            do! executeDb $"UPDATE users SET email='{newEmail}' WHERE id={userId}"
            do! logMessage $"[INFO] Email updated successfully"
            return true
    }

// Transaction-like operation: multiple steps
let transferUserData sourceId targetId : Reader<AppEnv, bool> =
    monad {
        do! logMessage $"[INFO] Starting data transfer from {sourceId} to {targetId}"

        let! sourceData = getUserData sourceId
        let! targetData = getUserData targetId

        if sourceData.Contains "not found" || targetData.Contains "not found" then
            do! logMessage "[ERROR] One or both users not found"
            return false
        else
            do! executeDb $"UPDATE data SET owner={targetId} WHERE owner={sourceId}"
            do! logMessage $"[INFO] Data transfer completed from {sourceId} to {targetId}"
            return true
    }

// Get multiple related records
let getUserWithOrders userId : Reader<AppEnv, string> =
    monad {
        let! user = getUserData userId
        do! logMessage $"[INFO] Fetching orders for user {userId}"
        let! orders = queryDb $"SELECT * FROM orders WHERE user_id={userId}"
        return $"{user}\nOrders:\n{orders}"
    }

// Conditional branch
let getAdminUser userId : Reader<AppEnv, string option> =
    monad {
        let! userData = getUserData userId

        if userData.Contains "admin" then
            do! logMessage $"[INFO] Admin user {userId} accessed"
            return Some userData
        else
            do! logMessage $"[WARNING] Non-admin user {userId} attempted admin access"
            return None
    }

// ===== Running the computations =====

let appEnv = {
    logger = { log = printfn "[%s] %s" (System.DateTime.Now.ToString("HH:mm:ss")) }
    db = {
        query = fun sql -> $"Result: {sql}"
        execute = fun sql -> printfn "Executed: %s" sql
    }
}

// Execute different operations
let result1 = Reader.run (getUserData 1) appEnv
let result2 = Reader.run (createUser "Alice" "alice@example.com") appEnv
let result3 = Reader.run (findUserByEmail "bob@example.com") appEnv
let result4 = Reader.run (updateUserEmail 1 "newemail@example.com") appEnv
let result5 = Reader.run (transferUserData 1 2) appEnv
let result6 = Reader.run (getUserWithOrders 1) appEnv

// Composing multiple operations
let workflow : Reader<AppEnv, bool> =
    monad {
        let! created = createUser "Charlie" "charlie@example.com"
        if created then
            let! updated = updateUserEmail 1 "charlie.new@example.com"
            return updated
        else
            return false
    }

Reader.run workflow appEnv
