// 标准 MoonBit 测试用例
// 测试基本功能：加法、乘法和字符串操作

test "add_basic_functionality" {
  // 基本加法功能测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(-1, @azimuth.add(-5, 4))
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
}

test "multiply_basic_functionality" {
  // 基本乘法功能测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(5, 3))
}

test "greet_basic_functionality" {
  // 基本字符串拼接功能测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "add_edge_cases" {
  // 加法边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 测试与零相加
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-42, @azimuth.add(-42, 0))
}

test "multiply_edge_cases" {
  // 乘法边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试与0和1相乘
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(12345, @azimuth.multiply(12345, 1))
}

test "complex_calculations" {
  // 复杂计算测试
  let a = 10
  let b = 20
  let c = 5
  
  // (a + b) * c
  let result = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(150, result)
  
  // a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(205, result2)
}

test "string_processing" {
  // 字符串处理测试
  let name1 = "Alice"
  let name2 = "Bob"
  let name3 = "张三"
  
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet(name1))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet(name2))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet(name3))
}

test "mathematical_properties" {
  // 数学性质测试
  let x = 7
  let y = 11
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(x, @azimuth.add(y, 3)), @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 3)))
}

test "real_world_scenarios" {
  // 实际应用场景测试
  // 计算矩形的周长
  let length = 8
  let width = 5
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(26, perimeter)
  
  // 计算购物车总价
  let price = 199
  let quantity = 3
  let total = @azimuth.multiply(price, quantity)
  @azimuth.assert_eq(597, total)
}

test "assertion_functions" {
  // 断言函数测试
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试计算结果
  let result = @azimuth.add(10, 20)
  @azimuth.assert_true(result == 30)
  @azimuth.assert_false(result != 30)
}