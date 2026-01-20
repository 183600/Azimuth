// 标准 MoonBit 测试套件
// 包含针对 azimuth 包核心功能的标准测试用例

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-10, add(-3, -7))
  assert_eq(100, add(50, 50))
}

test "add_function_boundary_values" {
  // 测试 add 函数的边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  
  // 测试零值处理
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, add(0, 0))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(3, -5))
  assert_eq(100, multiply(10, 10))
}

test "multiply_function_special_cases" {
  // 测试 multiply 函数的特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试1和-1的情况
  assert_eq(42, multiply(42, 1))
  assert_eq(42, multiply(1, 42))
  assert_eq(-42, multiply(42, -1))
  assert_eq(-42, multiply(-1, 42))
  
  // 测试最小值与-1相乘的特殊情况
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试溢出保护
  assert_eq(max_val, multiply(46341, 46341))
}

test "greet_function_comprehensive" {
  // 测试 greet 函数的各种情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "divide_with_ceil_function" {
  // 测试向上取整除法函数
  // 正常除法（无余数）
  assert_eq(5, divide_with_ceil(10, 2))
  assert_eq(3, divide_with_ceil(9, 3))
  
  // 向上取整（有余数）
  assert_eq(6, divide_with_ceil(11, 2))
  assert_eq(4, divide_with_ceil(10, 3))
  
  // 负数情况
  assert_eq(-5, divide_with_ceil(-10, 2))
  assert_eq(-3, divide_with_ceil(-11, 4))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 圆的周长计算（简化版：2 × π × r，π≈3）
  let radius = 5
  let circumference = multiply(multiply(2, 3), radius)
  assert_eq(30, circumference)
  
  // 矩形面积计算
  let length = 8
  let width = 6
  let area = multiply(length, width)
  assert_eq(48, area)
  
  // 平均值计算
  let num1 = 85
  let num2 = 90
  let num3 = 95
  let sum = add(add(num1, num2), num3)
  let average = sum / 3
  assert_eq(90, average)
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  let c = 5
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 购物车总价计算
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  
  let total1 = multiply(item1_price, item1_qty)
  let total2 = multiply(item2_price, item2_qty)
  let cart_total = add(total1, total2)
  
  assert_eq(697, cart_total)
  
  // 温度转换（简化版：摄氏度转华氏度 F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

test "assertion_functions_validation" {
  // 测试断言函数本身
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("MoonBit", "MoonBit")
  
  // 测试复合条件
  let x = 10
  let y = 20
  assert_true(x < y)
  assert_false(x > y)
  assert_eq(add(x, y), 30)
  assert_eq_string(greet("Test"), "Hello, Test!")
}