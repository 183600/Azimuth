// 标准 MoonBit 测试套件
// 包含10个核心测试用例，验证项目的基本功能

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(0, multiply(8, 0))
}

test "greet_function_basic_tests" {
  // 测试greet函数的基本功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "overflow_protection_tests" {
  // 测试溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试负数溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "special_value_handling" {
  // 测试特殊值的处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与0的运算
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  
  // 测试与1的运算
  assert_eq(6, multiply(6, 1))
  assert_eq(6, multiply(1, 6))
  
  // 测试最小值的特殊处理
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 计算矩形周长：2 × (长 + 宽)
  let length = 15
  let width = 8
  let perimeter = multiply(2, add(length, width))
  assert_eq(46, perimeter)
  
  // 计算总价：单价 × 数量 + 运费
  let price = 199
  let quantity = 3
  let shipping = 20
  let total = add(multiply(price, quantity), shipping)
  assert_eq(617, total)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, User_123!", greet("User_123"))
  assert_eq_string("Hello, \"John\"!", greet("\"John\""))
  
  // 测试包含空格的字符串
  assert_eq_string("Hello, Mary Jane!", greet("Mary Jane"))
  assert_eq_string("Hello,  Test !", greet(" Test "))
}

test "assertion_functions_validation" {
  // 测试断言函数本身
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("MoonBit", "MoonBit")
  
  // 测试条件断言
  let x = 10
  let y = 20
  assert_true(x < y)
  assert_false(x > y)
  assert_eq(add(x, y), 30)
}

test "real_world_application_tests" {
  // 测试实际应用场景
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 简单利息计算：本金 × (1 + 利率 × 时间)
  let principal = 1000
  let rate = 5  // 5%
  let time = 3
  let amount = multiply(principal, add(1, multiply(rate, time)))
  assert_eq(16000, amount)
}