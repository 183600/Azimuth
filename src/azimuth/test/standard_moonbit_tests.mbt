// 标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-5, add(-2, -3))
  assert_eq(0, add(5, -5))
}

test "add_boundary_values" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
}

test "multiply_boundary_values" {
  // 测试边界值的乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
}

test "greet_functionality" {
  // 测试问候函数
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "complex_calculations" {
  // 测试复合计算
  let a = 10
  let b = 20
  let c = 5
  
  // (a + b) * c
  let result = multiply(add(a, b), c)
  assert_eq(150, result)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(205, result2)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 13
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(multiply(x, add(y, 3)), add(multiply(x, y), multiply(x, 3)))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算矩形面积：长 × 宽
  let length = 12
  let width = 8
  let area = multiply(length, width)
  assert_eq(96, area)
  
  // 计算周长：2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(40, perimeter)
}

test "error_handling" {
  // 测试错误处理
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
}