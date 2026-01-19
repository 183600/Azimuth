// 标准 MoonBit 测试用例
// 测试 azimuth 包中的基本功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
}

test "greet_basic_functionality" {
  // 测试基本的问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "add_boundary_values" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_boundary_values" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
}

test "complex_calculation" {
  // 测试复合计算
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  
  @azimuth.assert_eq(900, result)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 11
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(x, @azimuth.add(y, 3)), @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 3)))
}

test "real_world_scenario" {
  // 测试实际应用场景：购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  // 计算总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(612, total)
}

test "assert_functions" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "string_processing" {
  // 测试字符串处理
  let name = "MoonBit"
  let greeting = @azimuth.greet(name)
  
  // 验证问候语格式
  @azimuth.assert_eq_string("Hello, MoonBit!", greeting)
}