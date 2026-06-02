// https://fsharpforfunandprofit.com/posts/dependencies-3/
type Reader<'env, 'a> = Reader of action: ('env -> 'a)

module Reader =
  /// Run a Reader with a given environment
  let run (env: 'env) (Reader action) : 'b  =
    action env  // simply call the inner function

  /// Create a Reader which returns the environment itself
  let ask: Reader<'env, 'env> = Reader id

  /// Map a function over a Reader
  let map (f: 'a -> 'b) (reader: Reader<'env, 'a>) : Reader<'env, 'b> =
    Reader (fun env -> f (run env reader))

  /// flatMap a function over a Reader
  let bind (f: 'a -> Reader<'b,'c>) (reader: Reader<'b,'a>) : Reader<'b,'c> =
    let newAction env =
      let x = run env reader
      run env (f x)
    Reader newAction
