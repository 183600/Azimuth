// Copyright 2026 International Digital Economy Academy
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

///|
test "option basic operations" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  inspect(some_value, content="Some(42)")
  inspect(none_value, content="None")
  
  let mapped_some = some_value.map(x => x + 1)
  inspect(mapped_some, content="Some(43)")
  
  let mapped_none = none_value.map(x => x + 1)
  inspect(mapped_none, content="None")
}

///|
test "int bit operations" {
  let a = 0b10101010  // 170
  let b = 0b01010101  // 85
  
  inspect(a & b, content="0")
  inspect(a | b, content="255")
  inspect(a ^ b, content="255")
  
  let x = 0b1010  // 10
  inspect((x << 2) >> 1, content="20")
  inspect((x >> 1) << 2, content="8")
}

///|
test "list basic operations" {
  let numbers = @list.from_array([1, 2, 3])
  let result = numbers.map(x => x * 2)
  inspect(result, content="@list.from_array([2, 4, 6])")
  
  let filtered = numbers.filter(x => x > 1)
  inspect(filtered, content="@list.from_array([2, 3])")
  
  let sum = numbers.fold(0, (acc, x) => acc + x)
  inspect(sum, content="6")
  
  let first_three = numbers.take(3)
  inspect(first_three, content="@list.from_array([1, 2, 3])")
}

///|
test "result error handling" {
  let success : Result[Int, String] = Ok(42)
  let failure : Result[Int, String] = Err("error")
  
  inspect(success, content="Ok(42)")
  inspect(failure, content="Err(\"error\")")
  
  let mapped_success = success.map(x => x * 2)
  inspect(mapped_success, content="Ok(84)")
  
  let mapped_failure = failure.map(x => x * 2)
  inspect(mapped_failure, content="Err(\"error\")")
}

///|
test "string basic operations" {
  let text = "Hello, MoonBit!"
  inspect(text.length(), content="14")
  
  let parts = text.split(" ")
  inspect(parts.length(), content="2")
  inspect(parts[0], content="Hello,")
  inspect(parts[1], content="MoonBit!")
  
  let joined = parts.join(" ")
  inspect(joined, content="Hello, MoonBit!")
}

///|
test "option and result combination" {
  let user : (String, Int)? = Some(("Alice", 25))
  inspect(user, content="(\"Alice\", 25)")
  
  let validation = user.map(pair => {
    if pair.1 >= 18 { Ok(pair.0) } else { Err("Too young") }
  })
  inspect(validation, content="Some(\"Alice\")")
  
  let no_user : (String, Int)? = None
  inspect(no_user, content="None")
  
  let no_validation = no_user.map(pair => {
    if pair.1 >= 18 { Ok(pair.0) } else { Err("Too young") }
  })
  inspect(no_validation, content="None")
}

///|
test "list functional patterns" {
  let numbers = @list.from_array([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  
  let evens = numbers.filter(x => x % 2 == 0)
  inspect(evens, content="@list.from_array([2, 4, 6, 8, 10])")
  
  let doubled = evens.map(x => x * 2)
  inspect(doubled, content="@list.from_array([4, 8, 12, 16, 20])")
  
  let sum = numbers.fold(0, (acc, x) => acc + x)
  inspect(sum, content="55")
  
  let first_three = numbers.take(3)
  inspect(first_three, content="@list.from_array([1, 2, 3])")
  
  let last_three = numbers.rev().take(3).rev()
  inspect(last_three, content="@list.from_array([8, 9, 10])")
}

///|
test "int mathematical functions" {
  inspect(Int::abs(5), content="5")
  inspect(Int::abs(-5), content="5")
  inspect(Int::abs(0), content="0")
  
  inspect(Int::pow(2, 0), content="1")
  inspect(Int::pow(2, 1), content="2")
  inspect(Int::pow(2, 3), content="8")
  inspect(Int::pow(3, 2), content="9")
  
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
  
  // Test arithmetic overflow behavior
  inspect(@int.max_value + 1, content="-2147483648")
  inspect(@int.min_value - 1, content="2147483647")
}

///|
test "string character operations" {
  let text = "Hello"
  inspect(text.char_at(0), content="'H'")
  inspect(text.char_at(4), content="'o'")
  
  let substring = text.slice(0, 3)
  inspect(substring, content="Hel")
  
  let mixed = "HeLLo WoRlD"
  let lower = mixed.to_lower()
  let upper = mixed.to_upper()
  
  inspect(lower, content="hello world")
  inspect(upper, content="HELLO WORLD")
  
  // Test string with unicode
  let unicode_text = "Hello 世界"
  inspect(unicode_text.length(), content="8")
}

///|
test "combined operations" {
  let process_string = (s : String) -> Result[Int, String] {
    let trimmed = s.trim()
    if trimmed.is_empty() {
      Err("Empty string")
    } else {
      match Int::parse(trimmed) {
        Some(n) => if n > 0 { Ok(n) } else { Err("Non-positive") }
        None => Err("Invalid format")
      }
    }
  }
  
  let result1 = process_string("  42  ")
  inspect(result1, content="Ok(42)")
  
  let result2 = process_string("  -5  ")
  inspect(result2, content="Err(\"Non-positive\")")
  
  let result3 = process_string("  abc  ")
  inspect(result3, content="Err(\"Invalid format\")")
  
  let result4 = process_string("   ")
  inspect(result4, content="Err(\"Empty string\")")
}