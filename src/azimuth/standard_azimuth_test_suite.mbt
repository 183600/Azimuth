// 标准 Azimuth 测试套件
// 包含 10 个标准的 MoonBit 测试用例

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
  assert_eq(100, add(50, 50))
}

test "add_function_overflow_protection" {
  // 测试 add 函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试负数溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  
  // 测试边界值相加
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
  assert_eq(0, multiply(0, 100))
}

test "multiply_function_special_cases" {
  // 测试 multiply 函数的特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与1和-1相乘
  assert_eq(100, multiply(100, 1))
  assert_eq(-100, multiply(100, -1))
  assert_eq(100, multiply(-100, -1))
  
  // 测试最小值与-1相乘的特殊情况
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, 2))
}

test "greet_function_standard_cases" {
  // 测试 greet 函数的标准情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, Alice!", greet("Alice"))
}

test "greet_function_special_characters" {
  // 测试 greet 函数处理特殊字符
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, user@123!", greet("user@123"))
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "complex_arithmetic_operations" {
  // 测试复杂的算术运算组合
  let a = 10
  let b = 20
  let c = 5
  
  // 测试 (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(150, result1)
  
  // 测试 a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(205, result2)
  
  // 测试 (a + b + c) * 2
  let result3 = multiply(add(add(a, b), c), 2)
  assert_eq(70, result3)
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let x = 15
  let y = 25
  
  // 测试加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 测试乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 测试分配律
  let z = 5
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
}

test "real_world_calculation_scenarios" {
  // 测试实际应用场景的计算
  // 场景1：计算矩形周长
  let length = 12
  let width = 8
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(40, perimeter)
  
  // 场景2：计算购物车总价
  let price1 = 199
  let qty1 = 2
  let price2 = 99
  let qty2 = 3
  let total = add(multiply(price1, qty1), multiply(price2, qty2))
  assert_eq(695, total)
  
  // 场景3：简单温度转换（摄氏度转华氏度简化版）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

test "edge_cases_and_boundary_conditions" {
  // 测试边界条件和极端情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值的各种运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 测试连续运算的边界情况
  let chain_result = add(multiply(add(1, 1), add(2, 2)), add(3, 3))
  assert_eq(14, chain_result)
  
  // 测试字符串边界情况
  assert_eq_string("Hello, x!", greet("x"))
  assert_eq_string("Hello, ABCDEFGHIJKLMNOPQRSTUVWXYZ!", greet("ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
}