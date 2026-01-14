// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-1, -1))
  assert_eq(10, add(15, -5))
}

test "add_overflow_handling" {
  // 测试加法溢出处理
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

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(2, -3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(100, multiply(10, 10))
}

test "multiply_overflow_handling" {
  // 测试乘法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, multiply(46341, 46341))
  
  // 测试负数溢出
  assert_eq(min_val, multiply(-46341, 46341))
  
  // 测试最小值特殊处理
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 2))
}

test "greet_basic_functionality" {
  // 测试基本的问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
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

test "complex_calculations" {
  // 测试复杂计算
  // 计算矩形面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = multiply(length, width)
  assert_eq(40, area)
  
  // 周长 = 2 × 长 + 2 × 宽
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(26, perimeter)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  // 商品总价 = 单价 × 数量
  let subtotal = multiply(item_price, quantity)
  // 总价 = 商品总价 + 运费
  let total = add(subtotal, shipping)
  
  assert_eq(612, total)
}

test "edge_cases_and_boundary_values" {
  // 测试边界情况和特殊值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与0的运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 测试与1的运算
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  
  // 测试字符串边界情况
  assert_eq_string("Hello, A!", greet("A"))
  let long_name = "ThisIsAVeryLongNameForTestingPurposes"
  assert_eq_string("Hello, " + long_name + "!", greet(long_name))
}