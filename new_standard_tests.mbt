// 标准 MoonBit 测试用例
// 测试 MoonBit 核心功能

test "integer arithmetic operations" {
  assert_eq(1 + 2, 3)
  assert_eq(10 - 5, 5)
  assert_eq(3 * 4, 12)
  assert_eq(20 / 4, 5)
  assert_eq(17 % 5, 2)
  assert_eq(-5 + 10, 5)
  assert_eq(-3 * -3, 9)
}

test "integer comparison operations" {
  assert_eq(5 > 3, true)
  assert_eq(5 < 3, false)
  assert_eq(5 >= 5, true)
  assert_eq(5 <= 3, false)
  assert_eq(5 == 5, true)
  assert_eq(5 != 3, true)
}

test "string basic operations" {
  let empty = ""
  let hello = "Hello"
  let world = "World"
  
  assert_eq(empty.length(), 0)
  assert_eq(hello.length(), 5)
  assert_eq(hello + " " + world, "Hello World")
  assert_eq(hello == "Hello", true)
  assert_eq(hello != "world", true)
}

test "option type operations" {
  let some_int : Int? = Some(42)
  let none_int : Int? = None
  
  assert_true(some_int is Some(_))
  assert_true(none_int is None)
  
  assert_eq(some_int.unwrap_or(0), 42)
  assert_eq(none_int.unwrap_or(0), 0)
}

test "array basic operations" {
  let numbers = [1, 2, 3, 4, 5]
  let empty_array : Array[Int] = []
  
  assert_eq(numbers.length(), 5)
  assert_eq(empty_array.length(), 0)
  assert_eq(numbers[0], 1)
  assert_eq(numbers[4], 5)
  
  let doubled = numbers.map(x => x * 2)
  assert_eq(doubled, [2, 4, 6, 8, 10])
  
  let evens = numbers.filter(x => x % 2 == 0)
  assert_eq(evens, [2, 4])
}

test "boolean logical operations" {
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

test "result type operations" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error")
  
  assert_true(ok_result is Ok(_))
  assert_true(err_result is Err(_))
  
  match (ok_result) {
    Ok(x) => assert_eq(x, 42)
    Err(_) => @builtin.abort("Expected Ok")
  }
  
  match (err_result) {
    Ok(_) => @builtin.abort("Expected Err")
    Err(msg) => assert_eq(msg, "error")
  }
}

test "integer boundary values" {
  assert_eq(Int::max_value, 2147483647)
  assert_eq(Int::min_value, -2147483648)
  
  assert_eq(Int::max_value + 1, Int::min_value)
  assert_eq(Int::min_value - 1, Int::max_value)
  
  assert_eq(Int::abs(-5), 5)
  assert_eq(Int::abs(0), 0)
  assert_eq(Int::abs(7), 7)
}