// 标准 MoonBit 测试用例
// 覆盖 azimuth 包中的主要功能

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.add(min_val, max_val))
}

test "multiply_function_special_cases" {
  // 测试乘法函数的特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_function_unicode_support" {
  // 测试 greet 函数的 Unicode 支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
}

test "mathematical_operations_combination" {
  // 测试数学运算的组合
  let a = 10
  let b = 20
  let c = 5
  
  // 计算 (a + b) * c
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(150, result)
  
  // 计算 a * b + c
  let product = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product, c)
  @azimuth.assert_eq(205, result2)
}

test "divide_with_ceil_function_various_cases" {
  // 测试向上取整除法的各种情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(2, 5))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, a b c!", @azimuth.greet("a b c"))
}

test "complex_business_logic" {
  // 测试复杂的业务逻辑
  // 计算订单总价：商品价格 × 数量 + 运费 - 折扣
  let price = 99
  let quantity = 3
  let shipping = 10
  let discount = 20
  
  let subtotal = @azimuth.multiply(price, quantity)
  let total = @azimuth.add(@azimuth.add(subtotal, shipping), -discount)
  @azimuth.assert_eq(287, total)
}

test "assert_functions_comprehensive" {
  // 全面测试断言函数
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_true(0 == 0)
  @azimuth.assert_true(-10 < -5)
  
  @azimuth.assert_false(5 < 3)
  @azimuth.assert_false(1 == 2)
  @azimuth.assert_false(-10 > -5)
  
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq(-100, -100)
  @azimuth.assert_eq(0, 0)
  
  @azimuth.assert_eq_string("MoonBit", "MoonBit")
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("测试", "测试")
}