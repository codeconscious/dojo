#r "nuget: CCFSharpUtils"
#r "nuget: FSharpPlus"
#r "nuget: FsToolkit.ErrorHandling"
open CCFSharpUtils
open CCFSharpUtils.Collections
open CCFSharpUtils.Operators
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Operators

type User = {
    id: int
    name: string
    email: string
}

// State includes in-memory database, log, and operation counter
type AppState = {
    users: Map<int, User>
    nextUserId: int
    logs: string list
    operationCount: int
}

// ===== Fundamental state operations =====

let logOperation msg : State<AppState, unit> =
    State(fun state ->
        let timestamp = System.DateTime.Now.ToString("HH:mm:ss")
        let newState = {
            state with
                logs = state.logs @ [$"[{timestamp}] {msg}"]
                operationCount = state.operationCount + 1
        }
        ((), newState)
    )

let getState : State<AppState, AppState> =
    State(fun state -> (state, state))

let setState newState : State<AppState, unit> =
    State(fun _ -> ((), newState))

// ===== Application functions =====

// Get user by ID
let getUserData userId : State<AppState, User option> =
    monad {
        do! logOperation $"[INFO] Fetching user {userId}"
        let! state = getState

        match Map.tryFind userId state.users with
        | Some user ->
            do! logOperation $"[INFO] User {userId} found"
            return Some user
        | None ->
            do! logOperation $"[WARNING] User {userId} not found"
            return None
    }

// Create a new user
let createUser name email : State<AppState, User> =
    monad {
        do! logOperation $"[INFO] Creating user: {name} ({email})"
        let! state = getState

        let newUser = {
            id = state.nextUserId
            name = name
            email = email
        }

        let newState = {
            state with
                users = Map.add state.nextUserId newUser state.users
                nextUserId = state.nextUserId + 1
        }

        do! setState newState
        do! logOperation $"[INFO] User {newUser.id} created successfully"
        return newUser
    }

// Find user by email
let findUserByEmail email : State<AppState, User option> =
    monad {
        do! logOperation $"[INFO] Searching for user with email: {email}"
        let! state = getState

        let result =
            state.users
            |> Map.tryFindKey (fun _ u -> u.email = email)
            |> Option.bind (fun id -> Map.tryFind id state.users)

        match result with
        | Some user ->
            do! logOperation $"[INFO] User found: {user.name}"
            return Some user
        | None ->
            do! logOperation $"[WARNING] No user found with email {email}"
            return None
    }

// Update user email
let updateUserEmail userId newEmail : State<AppState, bool> =
    monad {
        do! logOperation $"[INFO] Updating user {userId} email to {newEmail}"
        let! state = getState

        match Map.tryFind userId state.users with
        | None ->
            do! logOperation $"[ERROR] User {userId} does not exist"
            return false
        | Some user ->
            let updatedUser = { user with email = newEmail }
            let newState = {
                state with
                    users = Map.add userId updatedUser state.users
            }
            do! setState newState
            do! logOperation $"[INFO] User {userId} email updated successfully"
            return true
    }

// Delete a user
let deleteUser userId : State<AppState, bool> =
    monad {
        do! logOperation $"[INFO] Deleting user {userId}"
        let! state = getState

        if Map.containsKey userId state.users then
            let newState = {
                state with
                    users = Map.remove userId state.users
            }
            do! setState newState
            do! logOperation $"[INFO] User {userId} deleted successfully"
            return true
        else
            do! logOperation $"[ERROR] User {userId} not found"
            return false
    }

// Get all users
let getAllUsers: State<AppState, User list> =
    monad {
        do! logOperation "[INFO] Retrieving all users"
        let! state = getState

        let users = state.users |> Map.values |> List.ofSeq
        do! logOperation $"[INFO] Retrieved {users.Length} users"
        return users
    }

// Get user with operation count
let getUserWithStats userId: State<AppState, (User option * int)> =
    monad {
        let! user = getUserData userId
        let! state = getState
        return (user, state.operationCount)
    }

// Batch operation: create multiple users
let createMultipleUsers (names: (string * string) list) : State<AppState, User list> =
    monad {
        do! logOperation $"[INFO] Starting batch creation of {names.Length} users"

        let! users = List.traverse (fun (name, email) -> createUser name email) names

        do! logOperation $"[INFO] Batch creation completed with {names.Length} users"
        return users
    }

// Conditional operation: update only if user exists
let updateUserConditional userId newEmail : State<AppState, string> =
    monad {
        let! userOpt = getUserData userId

        match userOpt with
        | None ->
            do! logOperation $"[ERROR] Cannot update: User {userId} not found"
            return "User not found"
        | Some user ->
            let! updated = updateUserEmail userId newEmail
            if updated then
                return $"User {userId} updated from {user.email} to {newEmail}"
            else
                return "Update failed"
    }

// ===== Workflow: Complex multi-step operation =====

let complexWorkflow : State<AppState, string> =
    monad {
        do! logOperation "[INFO] Starting complex workflow"

        // Create some users
        let! user1 = createUser "Alice" "alice@example.com"
        let! user2 = createUser "Bob" "bob@example.com"
        let! user3 = createUser "Charlie" "charlie@example.com"

        // Update an email
        let! updated = updateUserEmail user1.id "alice.new@example.com"

        // Find a user
        let! found = findUserByEmail "bob@example.com"

        // Get all users
        let! allUsers = getAllUsers

        do! logOperation "[INFO] Complex workflow completed"
        return $"Created {allUsers.Length} users, updated: {updated}"
    }

// ===== Running the computations =====

let initialState = {
    users = Map.empty
    nextUserId = 1
    logs = []
    operationCount = 0
}

// Execute individual operations
let (user1, state1) = State.run (createUser "Alice" "alice@example.com") initialState
let (user2, state2) = State.run (createUser "Bob" "bob@example.com") state1
let (found, state3) = State.run (findUserByEmail "alice@example.com") state2
let (updated, state4) = State.run (updateUserEmail user1.id "alice.new@example.com") state3
let (allUsers, state5) = State.run getAllUsers state4

// Print logs
printfn "Operation logs:"
state5.logs |> List.iter (printfn "%s")

// Execute complex workflow
let (workflowResult, finalState) = State.run complexWorkflow initialState

printfn "\nWorkflow result: %s" workflowResult
printfn "\nFinal state:"
printfn "Total users: %d" (Map.count finalState.users)
printfn "Total operations: %d" finalState.operationCount
printfn "\nAll logs:"
finalState.logs |> List.iter (printfn "%s")

// Batch operations
let (batchUsers, batchState) =
    State.run
        (createMultipleUsers [("Diana", "diana@example.com"); ("Eve", "eve@example.com"); ("Frank", "frank@example.com")])
        initialState

printfn "\nBatch created users:"
batchUsers |> List.iter (fun u -> printfn "  ID: %d, Name: %s, Email: %s" u.id u.name u.email)
