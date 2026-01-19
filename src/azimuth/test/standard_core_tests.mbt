// 标准 MoonBit 核心测试用例
// 测试 azimuth 模块的核心功能

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  @azimuth.assert_eq(8, @azimuth.add(5, 3))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(300, @azimuth.add(100, 200))
}

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试零值
  @azimuth.assert_eq(max_val, @azimuth.add(0, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(0, min_val))
}

test "multiply_function_basic_cases" {
  // 测试乘法函数的基本情况
  @azimuth.assert_eq(20, @azimuth.multiply(4, 5))
  @azimuth.assert_eq(-20, @azimuth.multiply(-4, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(-7, @azimuth.multiply(7, -1))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  
  // 测试特殊值
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "divide_with_ceil_function" {
  // 测试向上取整除法函数
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(7, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(8, 4))
}

test "complex_mathematical_operations" {
  // 测试复杂数学运算
  let a = 12
  let b = 8
  let c = 5
  
  // (a + b) * c
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(100, product)
  
  // a * b + c
  let product2 = @azimuth.multiply(a, b)
  let result = @azimuth.add(product2, c)
  @azimuth.assert_eq(101, result)
}

test "real_world_calculations" {
  // 测试实际应用场景的计算
  // 计算矩形的面积和周长
  let length = 10
  let width = 6
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(60, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter_sum = @azimuth.add(length, width)
  let perimeter = @azimuth.multiply(2, perimeter_sum)
  @azimuth.assert_eq(32, perimeter)
}

test "mathematical_properties_validation" {
  // 测试数学性质的验证
  let x = 15
  let y = 25
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律：x * (y + 3) = x * y + x * 3
  let left_side = @azimuth.multiply(x, @azimuth.add(y, 3))
  let right_side = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 3))
  @azimuth.assert_eq(left_side, right_side)
}

test "error_handling_and_boundary_conditions" {
  // 测试错误处理和边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试边界值运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, min_val))
}