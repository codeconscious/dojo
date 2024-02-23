module Unions

module SingleCaseDiscrimatedUnions =
    type OrderId = Order of string
    let orderId : OrderId = Order "12"
    let (Order id) = orderId  // id = "12"  // Using pattern matching to deconstruct single-case DU
    printfn "%s" id
