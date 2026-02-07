// Simple test file for MoonBit core modules

test "basic int operations" {
  let a = 10
  let b = 5
  assert_eq(a + b, 15)
  assert_eq(a - b, 5)
  assert_eq(a * b, 50)
  assert_eq(a / b, 2)
}

test "basic string operations" {
  let hello = "Hello"
  let world = "World"
  let greeting = hello + ", " + world + "!"
  assert_eq(greeting, "Hello, World!")
  assert_eq(hello.length(), 5)
}

test "basic list operations" {
  let list = [1, 2, 3, 4, 5]
  assert_eq(list.length(), 5)
  assert_eq(list.head(), 1)
  let doubled = list.map(x => x * 2)
  assert_eq(doubled, [2, 4, 6, 8, 10])
}

test "basic option operations" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  assert_eq(some_value.unwrap_or(0), 42)
  assert_eq(none_value.unwrap_or(0), 0)
}

test "basic result operations" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error")
  assert_eq(ok_result.unwrap_or(0), 42)
  assert_eq(err_result.unwrap_or(0), 0)
}