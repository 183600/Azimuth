// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-1, -2))
  assert_eq(10, add(15, -5))
}

test "add_boundary_values" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, add(min_val, 1))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(3, -4))
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
  // 测试问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "complex_calculations" {
  // 测试复合计算
  let a = 10
  let b = 5
  let c = 3
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(45, result1)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(53, result2)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 11
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(multiply(x, add(y, 3)), add(multiply(x, y), multiply(x, 3)))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算矩形周长：2 * (长 + 宽)
  let length = 8
  let width = 5
  let perimeter = multiply(2, add(length, width))
  assert_eq(26, perimeter)
  
  // 计算折扣价格：原价 - 折扣
  let original_price = 100
  let discount = 20
  let discounted_price = add(original_price, -discount)
  assert_eq(80, discounted_price)
}

test "assert_functions" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试实际计算结果
  let sum_result = add(15, 25)
  assert_eq(40, sum_result)
  
  let product_result = multiply(6, 7)
  assert_eq(42, product_result)
  
  let greet_result = greet("User")
  assert_eq_string("Hello, User!", greet_result)
}