// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法

test "add_basic_functionality" {
  // 测试 add 函数的基本功能
  @azimuth.assert_eq(@azimuth.add(2, 3), 5)
  @azimuth.assert_eq(@azimuth.add(10, 20), 30)
  @azimuth.assert_eq(@azimuth.add(-5, 5), 0)
  @azimuth.assert_eq(@azimuth.add(-10, -20), -30)
}

test "add_edge_cases" {
  // 测试 add 函数的边界情况
  @azimuth.assert_eq(@azimuth.add(0, 0), 0)
  @azimuth.assert_eq(@azimuth.add(0, 42), 42)
  @azimuth.assert_eq(@azimuth.add(42, 0), 42)
  @azimuth.assert_eq(@azimuth.add(2147483647, 0), 2147483647)
  @azimuth.assert_eq(@azimuth.add(0, -2147483648), -2147483648)
}

test "multiply_basic_functionality" {
  // 测试 multiply 函数的基本功能
  @azimuth.assert_eq(@azimuth.multiply(2, 3), 6)
  @azimuth.assert_eq(@azimuth.multiply(10, 10), 100)
  @azimuth.assert_eq(@azimuth.multiply(-5, 5), -25)
  @azimuth.assert_eq(@azimuth.multiply(-10, -20), 200)
}

test "multiply_edge_cases" {
  // 测试 multiply 函数的边界情况
  @azimuth.assert_eq(@azimuth.multiply(0, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(0, 42), 0)
  @azimuth.assert_eq(@azimuth.multiply(42, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(1, 42), 42)
  @azimuth.assert_eq(@azimuth.multiply(42, 1), 42)
  @azimuth.assert_eq(@azimuth.multiply(-1, 42), -42)
  @azimuth.assert_eq(@azimuth.multiply(42, -1), -42)
}

test "greet_functionality" {
  // 测试 greet 函数
  @azimuth.assert_eq_string(@azimuth.greet("World"), "Hello, World!")
  @azimuth.assert_eq_string(@azimuth.greet("MoonBit"), "Hello, MoonBit!")
  @azimuth.assert_eq_string(@azimuth.greet(""), "Hello, !")
  @azimuth.assert_eq_string(@azimuth.greet("测试"), "Hello, 测试!")
  @azimuth.assert_eq_string(@azimuth.greet("123"), "Hello, 123!")
}

test "assertion_functions" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "mathematical_operations" {
  // 测试数学运算组合
  let a = 5
  let b = 10
  let c = 15
  
  // (a + b) * c = (5 + 10) * 15 = 15 * 15 = 225
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(result1, 225)
  
  // a * b + c = 5 * 10 + 15 = 50 + 15 = 65
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(result2, 65)
  
  // (a + b + c) * 2 = (5 + 10 + 15) * 2 = 30 * 2 = 60
  let result3 = @azimuth.multiply(@azimuth.add(@azimuth.add(a, b), c), 2)
  @azimuth.assert_eq(result3, 60)
}

test "string_operations" {
  // 测试字符串操作
  let name1 = "Alice"
  let name2 = "Bob"
  
  let greeting1 = @azimuth.greet(name1)
  let greeting2 = @azimuth.greet(name2)
  
  @azimuth.assert_eq_string(greeting1, "Hello, Alice!")
  @azimuth.assert_eq_string(greeting2, "Hello, Bob!")
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string(@azimuth.greet("John Doe"), "Hello, John Doe!")
}

test "complex_calculations" {
  // 测试复杂计算
  
  // 计算三角形面积：(底 * 高) / 2
  let base = 6
  let height = 8
  let area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(area, 24)
  
  // 计算矩形周长：2 * (长 + 宽)
  let length = 10
  let width = 5
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(perimeter, 30)
  
  // 计算简单利息：本金 * 利率 * 时间 / 100
  let principal = 1000
  let rate = 5
  let time = 2
  let interest = @azimuth.multiply(@azimuth.multiply(principal, rate), time) / 100
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(total, 1100)
}