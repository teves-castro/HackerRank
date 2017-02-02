module Common

open System

let split (s: String) = s.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries) 
