// 标准 MoonBit 测试套件
// 测试 azimuth 包的核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(100, add(50, 50))
}

test "add_boundary_values" {
  // 测试加法边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, add(min_val, 1))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(25, multiply(5, 5))
}

test "multiply_boundary_values" {
  // 测试乘法边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, multiply(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_functionality" {
  // 测试问候函数
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "complex_calculations" {
  // 测试复杂计算
  // (a + b) * c
  let a = 5
  let b = 3
  let c = 4
  let result = multiply(add(a, b), c)
  assert_eq(32, result)
  
  // a * b + c * d
  let a2 = 10
  let b2 = 2
  let c2 = 3
  let d2 = 4
  let result2 = add(multiply(a2, b2), multiply(c2, d2))
  assert_eq(32, result2)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算矩形周长：2 * (length + width)
  let length = 10
  let width = 5
  let perimeter = multiply(2, add(length, width))
  assert_eq(30, perimeter)
  
  // 计算简单折扣：原价 - 折扣金额
  let original_price = 100
  let discount_rate = 20  // 20%
  let discount_amount = multiply(original_price, discount_rate) / 100
  let final_price = add(original_price, -discount_amount)
  assert_eq(80, final_price)
}

test "assert_functions" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

test "edge_cases" {
  // 测试边缘情况
  // 连续运算
  let result1 = add(add(1, 2), add(3, 4))
  assert_eq(10, result1)
  
  let result2 = multiply(multiply(2, 3), multiply(4, 5))
  assert_eq(120, result2)
  
  // 混合运算
  let result3 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result3)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}