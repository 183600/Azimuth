// MoonBit 增强测试用例
// 包含10个标准的MoonBit测试用例，使用标准的MoonBit测试语法

test "enhanced_basic_arithmetic" {
  // 基础算术运算测试
  assert_eq(10, add(3, 7))
  assert_eq(6, multiply(2, 3))
  assert_eq(0, add(5, -5))
  assert_eq(-8, multiply(-2, 4))
}

test "enhanced_string_greeting" {
  // 字符串问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, User123!", greet("User123"))
}

test "enhanced_boundary_conditions" {
  // 边界条件测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
}

test "enhanced_complex_calculations" {
  // 复杂计算测试
  // 计算表达式：(a + b) * c - d
  let a = 8
  let b = 12
  let c = 3
  let d = 15
  
  let sum_ab = add(a, b)
  let product = multiply(sum_ab, c)
  let result = add(product, -d)
  
  assert_eq(45, result)
}

test "enhanced Mathematical_properties" {
  // 数学性质验证测试
  let x = 15
  let y = 25
  
  // 验证加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 验证乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 验证分配律
  assert_eq(multiply(x, add(y, 5)), add(multiply(x, y), multiply(x, 5)))
}

test "enhanced_assertion_functions" {
  // 断言函数测试
  assert_true(10 > 5)
  assert_false(3 > 7)
  assert_eq(42, multiply(6, 7))
  assert_eq_string("Hello, Test!", greet("Test"))
}

test "enhanced Practical_scenarios" {
  // 实际应用场景测试
  // 购物车总价计算
  let item_price = 199
  let quantity = 2
  let shipping = 15
  
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(413, total)
}

test "enhanced_error_handling" {
  // 错误处理测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(add(min_val, -100) == min_val)
  assert_true(multiply(min_val, 2) == min_val)
}

test "enhanced_sequence_operations" {
  // 序列操作测试
  // 计算斐波那契数列的前几项
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)
  let fib4 = add(fib2, fib3)
  let fib5 = add(fib3, fib4)
  
  assert_eq(1, fib1)
  assert_eq(1, fib2)
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
}

test "enhanced_data_processing" {
  // 数据处理测试
  // 计算一组数字的平均值
  let numbers_sum = add(add(add(add(85, 90), 78), 92), 80)
  let count = 5
  let average = numbers_sum / count
  
  assert_eq(425, numbers_sum)
  assert_eq(85, average)
  
  // 测试温度转换：摄氏度转华氏度（简化版）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}