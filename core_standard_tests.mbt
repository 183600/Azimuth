// 标准 MoonBit 测试用例集合
// 包含核心功能的测试，使用标准 MoonBit 测试语法

///|
test "int basic arithmetic operations" {
  inspect(10 + 5, content="15")
  inspect(10 - 3, content="7")
  inspect(6 * 7, content="42")
  inspect(20 / 4, content="5")
  inspect(15 % 4, content="3")
}

///|
test "int comparison operations" {
  assert_eq(10 > 5, true)
  assert_eq(3 < 8, true)
  assert_eq(7 >= 7, true)
  assert_eq(9 <= 9, true)
  assert_eq(5 == 5, true)
  assert_eq(4 != 2, true)
}

///|
test "string basic operations" {
  let hello = "Hello"
  let world = "World"
  let combined = hello + ", " + world + "!"
  inspect(combined, content="Hello, World!")
}

///|
test "string indexing and slicing" {
  let text = "MoonBit"
  inspect(text[0], content="'M'")
  inspect(text[3], content="'B'")
  inspect(text.slice(0, 4), content="Moon")
  inspect(text.slice(4, 7), content="Bit")
}

///|
test "array creation and access" {
  let numbers = [1, 2, 3, 4, 5]
  let strings = ["apple", "banana", "cherry"]
  inspect(numbers.length(), content="5")
  inspect(strings.length(), content="3")
  inspect(numbers[2], content="3")
  inspect(strings[1], content="banana")
}

///|
test "array operations" {
  let numbers = [1, 2, 3, 4, 5]
  let doubled = numbers.map(x => x * 2)
  let evens = numbers.filter(x => x % 2 == 0)
  let sum = numbers.fold(0, (acc, x) => acc + x)
  
  inspect(doubled, content="[2, 4, 6, 8, 10]")
  inspect(evens, content="[2, 4]")
  inspect(sum, content="15")
}

///|
test "boolean logic operations" {
  assert_eq(true && true, true)
  assert_eq(true && false, false)
  assert_eq(false && true, false)
  assert_eq(false && false, false)
  
  assert_eq(true || true, true)
  assert_eq(true || false, true)
  assert_eq(false || true, true)
  assert_eq(false || false, false)
  
  assert_eq(!true, false)
  assert_eq(!false, true)
}

///|
test "option type operations" {
  let some_value : Option[Int] = Some(42)
  let none_value : Option[Int] = None
  
  inspect(some_value, content="Some(42)")
  inspect(none_value, content="None")
  
  let is_some = some_value.is_some()
  let is_none = none_value.is_none()
  inspect(is_some, content="true")
  inspect(is_none, content="true")
}

///|
test "result type operations" {
  let ok_result : Result[Int, String] = Ok(100)
  let err_result : Result[Int, String] = Err("error message")
  
  inspect(ok_result, content="Ok(100)")
  inspect(err_result, content="Err(\"error message\")")
  
  let is_ok = ok_result.is_ok()
  let is_err = err_result.is_err()
  inspect(is_ok, content="true")
  inspect(is_err, content="true")
}

///|
test "type conversion operations" {
  let int_val = 42
  let uint_val = int_val.to_uint()
  let int64_val = int_val.to_int64()
  let double_val = int_val.to_double()
  
  inspect(uint_val, content="42")
  inspect(int64_val, content="42")
  inspect(double_val, content="42.0")
}