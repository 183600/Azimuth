// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

// 测试 add 函数的基本功能
test "add_basic_functionality" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-1, @azimuth.add(2, -3))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
}

// 测试 add 函数的边界情况
test "add_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试零值加法
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
}

// 测试 multiply 函数的基本功能
test "multiply_basic_functionality" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
}

// 测试 multiply 函数的边界情况
test "multiply_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试单位元素
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(-7, @azimuth.multiply(7, -1))
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

// 测试 greet 函数
test "greet_functionality" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
}

// 测试 divide_with_ceil 函数
test "divide_with_ceil_functionality" {
  // 测试整除情况
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(6, 2))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-6, 2))
  
  // 测试需要向上取整的情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(7, 2))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-3, 2))
  
  // 测试除零情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

// 测试复合运算
test "complex_arithmetic_operations" {
  // 测试 (a + b) * c
  let sum = @azimuth.add(3, 4)
  let result = @azimuth.multiply(sum, 5)
  @azimuth.assert_eq(35, result)
  
  // 测试 a * b + c
  let product = @azimuth.multiply(3, 4)
  let result2 = @azimuth.add(product, 5)
  @azimuth.assert_eq(17, result2)
}

// 测试断言函数
test "assertion_functions" {
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

// 测试实际应用场景
test "practical_application_scenarios" {
  // 购物车计算
  let item1 = 199
  let item2 = 299
  let subtotal = @azimuth.add(item1, item2)
  let total = @azimuth.multiply(subtotal, 1)  // 无折扣
  @azimuth.assert_eq(498, total)
  
  // 个性化问候
  let user_name = "张三"
  let greeting = @azimuth.greet(user_name)
  @azimuth.assert_eq_string("Hello, 张三!", greeting)
}

// 测试数学性质
test "mathematical_properties" {
  // 加法交换律
  let a = 123
  let b = 456
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 零元素性质
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
}