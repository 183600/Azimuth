// 新增的标准 MoonBit 测试用例
// 测试核心功能模块

test "add_basic_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-1, 1))
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "add_overflow_handling" {
  // 测试加法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(1000000000, 1500000000))
  
  // 测试负数溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(-1000000000, -1500000000))
  
  // 测试边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
}

test "multiply_overflow_handling" {
  // 测试乘法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试负数溢出
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试特殊边界情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_function_basic" {
  // 测试基本问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced   "))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(2, 5))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-3, 5))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算简单利息和复利
  let principal = 1000
  let rate = 5
  let time = 3
  
  // 简单利息：本金 × (1 + 利率 × 时间)
  let simple_interest = @azimuth.multiply(principal, @azimuth.add(1, @azimuth.multiply(rate, time)))
  @azimuth.assert_eq(16000, simple_interest)
  
  // 复利计算（简化版）：本金 × (1 + 利率)^时间
  let compound_growth = @azimuth.add(1, rate)
  let compound_factor = @azimuth.multiply(compound_growth, compound_growth)  // (1+rate)^2
  let compound_amount = @azimuth.multiply(principal, compound_factor)
  @azimuth.assert_eq(36000, compound_amount)
}

test "geometry_calculations" {
  // 测试几何计算
  let radius = 5
  
  // 圆的周长（简化版）：2 × π × r ≈ 2 × 3 × r
  let circumference = @azimuth.multiply(@azimuth.multiply(2, 3), radius)
  @azimuth.assert_eq(30, circumference)
  
  // 圆的面积（简化版）：π × r² ≈ 3 × r²
  let area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(75, area)
  
  // 球体体积（简化版）：4/3 × π × r³ ≈ 4 × r³
  let volume = @azimuth.multiply(4, @azimuth.multiply(radius, @azimuth.multiply(radius, radius)))
  @azimuth.assert_eq(500, volume)
}

test "assertion_functions_test" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试数学表达式的断言
  let a = 10
  let b = 20
  @azimuth.assert_true(@azimuth.add(a, b) == 30)
  @azimuth.assert_false(@azimuth.add(a, b) == 25)
  @azimuth.assert_eq(200, @azimuth.multiply(a, b))
}