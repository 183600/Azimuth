// 标准 MoonBit 测试套件
// 包含10个全面的测试用例，覆盖项目的主要功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "add_boundary_values" {
  // 测试加法的边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试零值处理
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(3, -4))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
}

test "multiply_boundary_values" {
  // 测试乘法的边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试特殊值
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
}

test "greet_function_variations" {
  // 测试greet函数的各种输入
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "complex_calculations" {
  // 测试复合计算场景
  // 计算矩形周长：2 × (长 + 宽)
  let length = 10
  let width = 5
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(30, perimeter)
  
  // 计算 (a + b) × (c + d)
  let a = 3
  let b = 4
  let c = 5
  let d = 6
  let result = @azimuth.multiply(@azimuth.add(a, b), @azimuth.add(c, d))
  @azimuth.assert_eq(77, result)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 12
  let y = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(x, @azimuth.add(y, 3)), @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 3)))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 20
  let total = @azimuth.add(@azimuth.multiply(item_price, quantity), shipping)
  @azimuth.assert_eq(617, total)
  
  // 温度转换（简化版）：华氏度 = 摄氏度 × 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
}

test "assert_function_verification" {
  // 测试断言函数的正确性
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("MoonBit", "MoonBit")
  
  // 测试条件断言
  let x = 10
  let y = 20
  @azimuth.assert_true(x < y)
  @azimuth.assert_false(x > y)
  @azimuth.assert_eq(@azimuth.add(x, y), 30)
}

test "edge_case_handling" {
  // 测试边界情况处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 连续运算的边界情况
  let result1 = @azimuth.add(@azimuth.add(max_val, -1), @azimuth.add(min_val, 1))
  @azimuth.assert_eq(0, result1)
  
  // 混合符号运算
  let result2 = @azimuth.multiply(@azimuth.add(10, -5), @azimuth.add(-3, 8))
  @azimuth.assert_eq(25, result2)
  
  // 字符串特殊字符
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
}