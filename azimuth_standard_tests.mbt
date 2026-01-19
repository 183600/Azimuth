// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法

// 导入必要的模块
use azimuth

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  assert_eq(5, azimuth.add(2, 3))
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(-3, azimuth.add(-1, -2))
  assert_eq(10, azimuth.add(15, -5))
}

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  assert_eq(max_val, azimuth.add(max_val, 1))
  assert_eq(max_val, azimuth.add(max_val, max_val))
  
  // 测试最小值边界
  assert_eq(min_val, azimuth.add(min_val, -1))
  assert_eq(min_val, azimuth.add(min_val, min_val))
  
  // 测试最大值与最小值相加
  assert_eq(-1, azimuth.add(max_val, min_val))
}

test "multiply_function_basic_cases" {
  // 测试乘法函数的基本情况
  assert_eq(6, azimuth.multiply(2, 3))
  assert_eq(0, azimuth.multiply(0, 5))
  assert_eq(-15, azimuth.multiply(-3, 5))
  assert_eq(12, azimuth.multiply(-4, -3))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与0和1的乘法
  assert_eq(0, azimuth.multiply(max_val, 0))
  assert_eq(max_val, azimuth.multiply(max_val, 1))
  assert_eq(min_val, azimuth.multiply(min_val, 1))
  
  // 测试与-1的乘法
  assert_eq(min_val, azimuth.multiply(min_val, -1))
  assert_eq(-max_val, azimuth.multiply(max_val, -1))
  
  // 测试溢出情况
  assert_eq(max_val, azimuth.multiply(46341, 46341))
}

test "greet_function_various_inputs" {
  // 测试greet函数的各种输入
  assert_eq_string("Hello, World!", azimuth.greet("World"))
  assert_eq_string("Hello, !", azimuth.greet(""))
  assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  assert_eq_string("Hello, 测试!", azimuth.greet("测试"))
  assert_eq_string("Hello, 123!", azimuth.greet("123"))
}

test "complex_calculations" {
  // 测试复杂计算场景
  // 计算矩形面积：长 × 宽
  let length = 10
  let width = 5
  let area = azimuth.multiply(length, width)
  assert_eq(50, area)
  
  // 计算矩形周长：2 × (长 + 宽)
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  assert_eq(30, perimeter)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 乘法交换律
  assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 分配律
  let left = azimuth.multiply(a, azimuth.add(b, 3))
  let right = azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, 3))
  assert_eq(left, right)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 20
  let subtotal = azimuth.multiply(item_price, quantity)
  let total = azimuth.add(subtotal, shipping)
  
  assert_eq(617, total)
  
  // 温度转换（简化版）：摄氏度转华氏度 F = C × 2 + 30
  let celsius = 25
  let fahrenheit = azimuth.add(azimuth.multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

test "assertion_functions" {
  // 测试断言函数
  azimuth.assert_true(true)
  azimuth.assert_false(false)
  azimuth.assert_eq(42, 42)
  azimuth.assert_eq_string("test", "test")
}

test "performance_calculations" {
  // 测试性能相关计算
  // 计算等差数列的和：1 + 2 + 3 + ... + 10
  let n = 10
  let sum = azimuth.multiply(n, azimuth.add(n, 1)) / 2
  assert_eq(55, sum)
  
  // 计算等比数列的和：2 + 4 + 8 + 16
  let term1 = 2
  let term2 = 4
  let term3 = 8
  let term4 = 16
  let geo_sum = azimuth.add(azimuth.add(azimuth.add(term1, term2), term3), term4)
  assert_eq(30, geo_sum)
}