// 用户自定义 MoonBit 测试用例
// 包含10个实用的测试用例，使用标准 MoonBit 测试语法

// 导入 azimuth 模块
@azimuth

test "basic_addition_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(3, -4))
  @azimuth.assert_eq(20, @azimuth.multiply(-4, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(10, 0))
}

test "greet_function_basic_usage" {
  // 测试 greet 函数的基本用法
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算 (a + b) * c - d
  let a = 8
  let b = 12
  let c = 3
  let d = 15
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(45, result)  // (8 + 12) * 3 - 15 = 20 * 3 - 15 = 60 - 15 = 45
}

test "boundary_value_addition" {
  // 测试边界值的加法运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试最小值加法
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "boundary_value_multiplication" {
  // 测试边界值的乘法运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 测试最小值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_properties" {
  // 测试数学性质：交换律
  let a = 15
  let b = 25
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试负数情况
  let c = -10
  let d = 20
  
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  @azimuth.assert_eq(@azimuth.multiply(c, d), @azimuth.multiply(d, c))
}

test "real_world_shopping_cart" {
  // 测试实际应用场景：购物车计算
  let apple_price = 5
  let apple_quantity = 4
  let banana_price = 3
  let banana_quantity = 6
  
  // 计算每种水果总价
  let apple_total = @azimuth.multiply(apple_price, apple_quantity)
  let banana_total = @azimuth.multiply(banana_price, banana_quantity)
  
  // 计算购物车总价
  let cart_total = @azimuth.add(apple_total, banana_total)
  
  // 验证结果：5×4 + 3×6 = 20 + 18 = 38
  @azimuth.assert_eq(38, cart_total)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, test@123!", @azimuth.greet("test@123"))
  
  // 测试包含Unicode字符的字符串
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  
  // 测试包含数字的字符串
  @azimuth.assert_eq_string("Hello, User2023!", @azimuth.greet("User2023"))
}

test "assertion_functions_validation" {
  // 测试断言函数本身
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试断言函数与实际功能结合
  let add_result = @azimuth.add(15, 25)
  @azimuth.assert_eq(40, add_result)
  @azimuth.assert_true(add_result > 30)
  @azimuth.assert_false(add_result < 30)
  
  let greet_result = @azimuth.greet("Tester")
  @azimuth.assert_eq_string("Hello, Tester!", greet_result)
}