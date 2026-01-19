// 标准 MoonBit 测试套件
// 包含核心功能的基本测试用例

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-1, 1))
  assert_eq(-3, add(-1, -2))
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
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
}

test "multiply_boundary_values" {
  // 测试边界值的乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(max_val, 1))
}

test "greet_functionality" {
  // 测试问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_calculation" {
  // 测试复杂计算场景
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let sum_ab = add(a, b)
  let result = multiply(sum_ab, c)
  assert_eq(900, result)
}

test "real_world_scenario" {
  // 测试实际应用场景：计算矩形周长
  let length = 15
  let width = 10
  
  // 周长 = 2 * (length + width)
  let sum_sides = add(length, width)
  let perimeter = multiply(2, sum_sides)
  assert_eq(50, perimeter)
}

test "assert_functions" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}