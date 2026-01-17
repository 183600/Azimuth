// Azimuth 项目综合测试用例
// 使用标准 MoonBit 测试语法，覆盖项目核心功能

test "add_basic_functionality" {
  // 基本加法功能测试
  assert_eq(azimuth::add(5, 3), 8)
  assert_eq(azimuth::add(-2, 7), 5)
  assert_eq(azimuth::add(-4, -6), -10)
  assert_eq(azimuth::add(0, 42), 42)
  assert_eq(azimuth::add(100, 0), 100)
}

test "add_edge_cases" {
  // 加法边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(azimuth::add(max_val, 1), max_val)
  assert_eq(azimuth::add(min_val, -1), min_val)
  
  // 测试最小值特殊情况
  assert_eq(azimuth::add(min_val, 0), min_val)
  assert_eq(azimuth::add(min_val, 1), -2147483647)
}

test "multiply_basic_functionality" {
  // 基本乘法功能测试
  assert_eq(azimuth::multiply(4, 5), 20)
  assert_eq(azimuth::multiply(-3, 7), -21)
  assert_eq(azimuth::multiply(-2, -8), 16)
  assert_eq(azimuth::multiply(12, 0), 0)
  assert_eq(azimuth::multiply(0, 25), 0)
}

test "multiply_edge_cases" {
  // 乘法边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试1和-1的情况
  assert_eq(azimuth::multiply(42, 1), 42)
  assert_eq(azimuth::multiply(1, -17), -17)
  assert_eq(azimuth::multiply(-5, -1), 5)
  
  // 测试最小值的特殊情况
  assert_eq(azimuth::multiply(min_val, -1), min_val)
  assert_eq(azimuth::multiply(min_val, 1), min_val)
  assert_eq(azimuth::multiply(min_val, 2), min_val)
}

test "greet_functionality" {
  // 问候函数测试
  assert_eq(azimuth::greet("World"), "Hello, World!")
  assert_eq(azimuth::greet("MoonBit"), "Hello, MoonBit!")
  assert_eq(azimuth::greet(""), "Hello, !")
  assert_eq(azimuth::greet(" "), "Hello,  !")
}

test "mathematical_expressions" {
  // 复杂数学表达式测试
  // 计算 (a + b) * c
  let a = 3
  let b = 4
  let c = 5
  let result = azimuth::multiply(azimuth::add(a, b), c)
  assert_eq(result, 35)
  
  // 计算 a * b + c * d
  let d = 2
  let result2 = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(c, d))
  assert_eq(result2, 22)
}

test "practical_calculations" {
  // 实际计算场景测试
  // 计算购物总价：单价 × 数量
  let unit_price = 15
  let quantity = 6
  let total = azimuth::multiply(unit_price, quantity)
  assert_eq(total, 90)
  
  // 计算平均分：(分数1 + 分数2 + 分数3) / 3
  let score1 = 85
  let score2 = 90
  let score3 = 95
  let sum = azimuth::add(azimuth::add(score1, score2), score3)
  // 注意：这里使用整数除法
  let average = sum / 3
  assert_eq(average, 90)
}

test "string_operations" {
  // 字符串操作测试
  let name1 = "Alice"
  let name2 = "张三"
  let name3 = "🚀"
  
  assert_eq(azimuth::greet(name1), "Hello, Alice!")
  assert_eq(azimuth::greet(name2), "Hello, 张三!")
  assert_eq(azimuth::greet(name3), "Hello, 🚀!")
}

test "assertion_functions" {
  // 断言函数测试
  // 测试 assert_eq
  azimuth::assert_eq(10, 10)
  azimuth::assert_eq(-5, -5)
  azimuth::assert_eq(0, 0)
  
  // 测试 assert_eq_string
  azimuth::assert_eq_string("test", "test")
  azimuth::assert_eq_string("", "")
  azimuth::assert_eq_string("Hello, World!", "Hello, World!")
  
  // 测试 assert_true 和 assert_false
  azimuth::assert_true(true)
  azimuth::assert_true(1 == 1)
  azimuth::assert_false(false)
  azimuth::assert_false(1 == 0)
}