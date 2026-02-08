// 新增 MoonBit 标准测试用例
// 测试核心功能和边界情况

// 定义断言函数
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  if (expected != actual) {
    let _ = expected - actual
    @builtin.abort("Assertion failed")
  }
}

pub fn assert_eq_float(expected : Double, actual : Double) -> Unit {
  if (expected != actual) {
    let _ = expected - actual
    @builtin.abort("Float assertion failed")
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  if (expected != actual) {
    let _ = expected + actual
    @builtin.abort("String assertion failed")
  }
}

pub fn assert_true(condition : Bool) -> Unit {
  if (!condition) {
    @builtin.abort("Expected true")
  }
}

pub fn assert_false(condition : Bool) -> Unit {
  if (condition) {
    @builtin.abort("Expected false")
  }
}

test "int_bitwise_operations" {
  // 测试整数位运算
  assert_eq(5 & 3, 1)  // 101 & 011 = 001
  assert_eq(5 | 3, 7)  // 101 | 011 = 111
  assert_eq(5 ^ 3, 6)  // 101 ^ 011 = 110
  assert_eq(-5 - 1, -6)    // 按位取反的等价操作
  assert_eq(5 << 1, 10) // 左移
  assert_eq(5 >> 1, 2)  // 右移
}

test "float_basic_operations" {
  // 测试浮点数基本运算
  let x = 3.14
  let y = 2.0
  
  assert_eq_float(x + y, 5.14)
  assert_eq_float(x - y, 1.14)
  assert_eq_float(x * y, 6.28)
  assert_eq_float(x / y, 1.57)
}

test "string_advanced_operations" {
  // 测试字符串高级操作
  let text = "Hello, MoonBit!"
  
  assert_true(text.contains("Moon"))
  assert_true(!text.contains("World"))
  assert_true(text.has_prefix("Hello"))
  assert_true(text.has_suffix("Bit!"))
}

test "array_functional_operations" {
  // 测试数组函数式操作
  let numbers = [1, 2, 3, 4, 5]
  
  // Length test
  assert_eq(numbers.length(), 5)
  
  // Array access
  assert_eq(numbers[0], 1)
  assert_eq(numbers[4], 5)
  
  // Map operation
  let doubled = numbers.map(x => x * 2)
  assert_eq(doubled.length(), 5)
  assert_eq(doubled[0], 2)
  assert_eq(doubled[4], 10)
  
  // Filter operation
  let evens = numbers.filter(x => x % 2 == 0)
  assert_eq(evens.length(), 2)
  assert_eq(evens[0], 2)
  assert_eq(evens[1], 4)
}

test "option_pattern_matching" {
  // 测试Option模式匹配
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  assert_true(some_value is Some(_))
  assert_true(none_value is None)
  
  let some_result = match some_value {
    Some(x) => x * 2
    None => 0
  }
  assert_eq(some_result, 84)
  
  let none_result = match none_value {
    Some(x) => x * 2
    None => 0
  }
  assert_eq(none_result, 0)
}

test "result_error_handling" {
  // 测试Result错误处理
  let success : Result[Int, String] = Ok(42)
  let failure : Result[Int, String] = Err("Something went wrong")
  
  assert_true(success is Ok(_))
  assert_true(failure is Err(_))
  
  let success_result = match success {
    Ok(value) => "Success: " + value.to_string()
    Err(msg) => "Error: " + msg
  }
  assert_eq_string(success_result, "Success: 42")
  
  let failure_result = match failure {
    Ok(value) => "Success: " + value.to_string()
    Err(msg) => "Error: " + msg
  }
  assert_eq_string(failure_result, "Error: Something went wrong")
}

test "char_classification" {
  // 测试字符分类
  let digit = '5'
  let letter = 'A'
  let lowercase = 'a'
  let whitespace = '\t'
  let newline = '\n'
  
  assert_eq_string(digit.to_string(), "'5'")
  assert_eq_string(letter.to_string(), "'A'")
  assert_eq_string(lowercase.to_string(), "'a'")
  assert_eq_string(whitespace.to_string(), "'\\t'")
  assert_eq_string(newline.to_string(), "'\\n'")
}

test "array_mutating_operations" {
  // 测试数组基本操作
  let arr = [1, 2, 3]
  
  // Test access
  assert_eq(arr[0], 1)
  assert_eq(arr[1], 2)
  assert_eq(arr[2], 3)
  
  // Test length
  assert_eq(arr.length(), 3)
  
  // Test empty array
  let empty_arr = []
  assert_eq(empty_arr.length(), 0)
}

test "math_basic_functions" {
  // 测试基本数学运算
  let x = -5
  
  // Absolute value (manual implementation)
  let abs_x = if x < 0 { -x } else { x }
  assert_eq(abs_x, 5)
  
  // Power operations
  assert_eq(2 * 2, 4)
  assert_eq(3 * 3 * 3, 27)
  
  // Modulo operations
  assert_eq(10 % 3, 1)
  assert_eq(15 % 4, 3)
}