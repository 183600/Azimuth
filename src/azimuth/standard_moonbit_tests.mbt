// 标准 MoonBit 测试用例
// 包含10个测试用例，覆盖不同的功能场景

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(10, azimuth::add(3, 7))
  assert_eq(0, azimuth::add(-5, 5))
  assert_eq(-10, azimuth::add(-3, -7))
  assert_eq(100, azimuth::add(50, 50))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正溢出测试
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(max_val, azimuth::add(max_val, 100))
  
  // 负溢出测试
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(min_val, azimuth::add(min_val, -100))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(15, azimuth::multiply(3, 5))
  assert_eq(-15, azimuth::multiply(-3, 5))
  assert_eq(15, azimuth::multiply(-3, -5))
  assert_eq(0, azimuth::multiply(0, 12345))
}

test "multiply_boundary_values" {
  // 测试乘法边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值乘法测试
  assert_eq(0, azimuth::multiply(max_val, 0))
  assert_eq(max_val, azimuth::multiply(max_val, 1))
  assert_eq(min_val, azimuth::multiply(min_val, 1))
  assert_eq(min_val, azimuth::multiply(min_val, -1))
}

test "greet_function_variations" {
  // 测试 greet 函数的各种输入
  assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth::greet(""))
  assert_eq_string("Hello, 123!", azimuth::greet("123"))
  assert_eq_string("Hello, 张三!", azimuth::greet("张三"))
}

test "complex_calculations" {
  // 测试复合计算场景
  // 计算 (a + b) * c - d
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  
  let sum_ab = azimuth::add(a, b)
  let product = azimuth::multiply(sum_ab, c)
  let result = azimuth::add(product, -d)
  
  assert_eq(75, result)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 8
  let y = 12
  
  // 加法交换律
  assert_eq(azimuth::add(x, y), azimuth::add(y, x))
  
  // 乘法交换律
  assert_eq(azimuth::multiply(x, y), azimuth::multiply(y, x))
  
  // 分配律简化测试
  let distributive = azimuth::multiply(x, azimuth::add(y, 5))
  let expanded = azimuth::add(azimuth::multiply(x, y), azimuth::multiply(x, 5))
  assert_eq(distributive, expanded)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车总价计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  let total1 = azimuth::multiply(item1_price, item1_quantity)
  let total2 = azimuth::multiply(item2_price, item2_quantity)
  let cart_total = azimuth::add(total1, total2)
  
  assert_eq(697, cart_total)
}

test "edge_case_combinations" {
  // 测试边界情况组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  assert_eq(-1, azimuth::add(max_val, min_val))
  assert_eq(min_val, azimuth::multiply(min_val, 1))
  
  // 接近边界值的运算
  assert_eq(2147483646, azimuth::add(max_val, -1))
  assert_eq(-2147483647, azimuth::add(min_val, 1))
}

test "assertion_functions" {
  // 测试断言函数本身
  azimuth::assert_true(true)
  azimuth::assert_false(false)
  azimuth::assert_eq(42, 42)
  azimuth::assert_eq_string("test", "test")
  
  // 测试计算结果的断言
  let add_result = azimuth::add(15, 25)
  azimuth::assert_eq(40, add_result)
  
  let multiply_result = azimuth::multiply(6, 7)
  azimuth::assert_eq(42, multiply_result)
  
  let greet_result = azimuth::greet("Tester")
  azimuth::assert_eq_string("Hello, Tester!", greet_result)
}