// Azimuth 项目新增综合测试用例
// 使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  let a = 15
  let b = 7
  
  // 加法测试
  let add_result = add(a, b)
  assert_eq(22, add_result)
  
  // 乘法测试
  let mul_result = multiply(a, b)
  assert_eq(105, mul_result)
}

test "string_operations" {
  // 测试字符串操作
  let name = "Azimuth"
  let result = greet(name)
  assert_eq_string("Hello, Azimuth!", result)
}

test "negative_number_operations" {
  // 测试负数运算
  let a = -10
  let b = 5
  
  // 负数加法
  let add_result = add(a, b)
  assert_eq(-5, add_result)
  
  // 负数乘法
  let mul_result = multiply(a, b)
  assert_eq(-50, mul_result)
}

test "zero_operations" {
  // 测试零值运算
  let a = 42
  let b = 0
  
  // 与零相加
  let add_result1 = add(a, b)
  let add_result2 = add(b, a)
  assert_eq(42, add_result1)
  assert_eq(42, add_result2)
  
  // 与零相乘
  let mul_result1 = multiply(a, b)
  let mul_result2 = multiply(b, a)
  assert_eq(0, mul_result1)
  assert_eq(0, mul_result2)
}

test "large_number_operations" {
  // 测试大数运算
  let a = 10000
  let b = 25000
  
  // 大数加法
  let add_result = add(a, b)
  assert_eq(35000, add_result)
  
  // 大数乘法
  let mul_result = multiply(100, 500)
  assert_eq(50000, mul_result)
}

test "complex_expressions" {
  // 测试复杂表达式
  let x = 3
  let y = 4
  let z = 5
  
  // (x + y) * z
  let expr1 = multiply(add(x, y), z)
  assert_eq(35, expr1)
  
  // x * y + z
  let expr2 = add(multiply(x, y), z)
  assert_eq(17, expr2)
}

test "edge_cases" {
  // 测试边界情况
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 最大值加0
  let max_add_zero = add(max_int, 0)
  assert_eq(max_int, max_add_zero)
  
  // 最小值加0
  let min_add_zero = add(min_int, 0)
  assert_eq(min_int, min_add_zero)
}

test "unicode_string_handling" {
  // 测试Unicode字符串处理
  let chinese_name = "中文测试"
  let emoji_name = "🌍🚀"
  let mixed_name = "Hello世界🌍"
  
  // 中文字符串
  let chinese_result = greet(chinese_name)
  assert_eq_string("Hello, 中文测试!", chinese_result)
  
  // Emoji字符串
  let emoji_result = greet(emoji_name)
  assert_eq_string("Hello, 🌍🚀!", emoji_result)
  
  // 混合字符串
  let mixed_result = greet(mixed_name)
  assert_eq_string("Hello, Hello世界🌍!", mixed_result)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  let commutative1 = add(a, b)
  let commutative2 = add(b, a)
  assert_eq(commutative1, commutative2)
  
  // 乘法交换律
  let mul_commutative1 = multiply(a, b)
  let mul_commutative2 = multiply(b, a)
  assert_eq(mul_commutative1, mul_commutative2)
}