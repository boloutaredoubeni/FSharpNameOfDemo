module FSharpNameOfDemo.Tests

open FSharpNameOfDemo
open NUnit.Framework

[<Test>]
let ``hello returns 42`` () =
  let result = Library.hello 42
  printfn "%i" result
  Assert.AreEqual(42,result)

// [<Test>]
let ``has nameof`` () = ()
  // Assert.AreNotEqual(nameof "", "hello")