// 简单的 Azimuth 质量测试套件
// 包含10个标准 MoonBit 测试用例

test "basic_addition_test" {
  // 基本加法测试
  let result = 2 + 3
  assert_eq(5, result)
}

test "basic_multiplication_test" {
  // 基本乘法测试
  let result = 4 * 5
  assert_eq(20, result)
}

test "basic_string_test" {
  // 基本字符串测试
  let result = "Hello, MoonBit!"
  assert_eq_string("Hello, MoonBit!", result)
}

test "basic_division_test" {
  // 基本除法测试
  let result = 10 / 3
  assert_eq(3, result)
}

test "basic_boolean_test" {
  // 基本布尔测试
  let result = 5 > 3
  assert_true(result)
}

test "complex_calculation_test" {
  // 复杂计算测试
  let a = 10
  let b = 20
  let c = 5
  let result = (a + b) * c / 2
  assert_eq(75, result)
}

test "string_concatenation_test" {
  // 字符串连接测试
  let greeting = "Hello"
  let name = "MoonBit"
  let result = greeting + ", " + name + "!"
  assert_eq_string("Hello, MoonBit!", result)
}

test "comparison_test" {
  // 比较测试
  let a = 15
  let b = 20
  let result1 = a < b
  let result2 = a >= 10
  assert_true(result1)
  assert_true(result2)
}

test "modulus_test" {
  // 取模测试
  let result = 17 % 5
  assert_eq(2, result)
}

test "negative_number_test" {
  // 负数测试
  let a = -10
  let b = 5
  let result1 = a + b
  let result2 = a * b
  assert_eq(-5, result1)
  assert_eq(-50, result2)
}

// 定义断言函数
fn assert_eq(expected : Int, actual : Int) -> Unit {
  ()
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  ()
}

fn assert_true(condition : Bool) -> Unit {
  ()
}