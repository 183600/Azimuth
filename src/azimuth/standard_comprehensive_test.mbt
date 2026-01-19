// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-10, add(-3, -7))
  assert_eq(100, add(50, 50))
}

test "add_edge_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试零值
  assert_eq(5, add(5, 0))
  assert_eq(-3, add(0, -3))
  assert_eq(0, add(0, 0))
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(3, -5))
  assert_eq(25, multiply(-5, -5))
}

test "multiply_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试单位元素
  assert_eq(7, multiply(7, 1))
  assert_eq(7, multiply(1, 7))
  assert_eq(-7, multiply(7, -1))
  assert_eq(-7, multiply(-1, 7))
  
  // 测试最小值特殊情况
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(-1, min_val))
  assert_eq(0, multiply(min_val, 0))
}

test "greet_functionality" {
  // 测试问候函数
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-3, divide_with_ceil(-9, 3))
}

test "complex_calculations" {
  // 测试复杂计算场景
  // 计算矩形周长：2 × (长 + 宽)
  let length = 8
  let width = 5
  let perimeter = multiply(2, add(length, width))
  assert_eq(26, perimeter)
  
  // 计算平均数
  let a = 15
  let b = 25
  let c = 35
  let average = add(add(a, b), c) / 3
  assert_eq(25, average)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(multiply(x, add(y, 3)), add(multiply(x, y), multiply(x, 3)))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let price = 199
  let quantity = 3
  let shipping = 15
  let total = add(multiply(price, quantity), shipping)
  assert_eq(612, total)
  
  // 温度转换（简化版）：华氏度 = 摄氏度 × 2 + 30
  let celsius = 20
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(70, fahrenheit)
}