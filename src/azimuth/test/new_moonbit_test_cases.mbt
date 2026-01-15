// 新增的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，包含10个测试用例

test "basic_addition_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
}

test "basic_multiplication_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(-21, @azimuth.multiply(7, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
}

test "greet_function_comprehensive" {
  // 测试 greet 函数的各种情况
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "overflow_protection_tests" {
  // 测试溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 测试乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 15
  let b = 27
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试负数的交换律
  let x = -12
  let y = 8
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
}

test "real_world_shopping_cart" {
  // 测试实际购物车计算场景
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  let discount = 50
  let shipping = 15
  
  // 计算总价
  let subtotal1 = @azimuth.multiply(item1_price, item1_qty)
  let subtotal2 = @azimuth.multiply(item2_price, item2_qty)
  let subtotal = @azimuth.add(subtotal1, subtotal2)
  let total = @azimuth.add(@azimuth.add(subtotal, -discount), shipping)
  
  @azimuth.assert_eq(652, total)
}

test "temperature_conversion_scenario" {
  // 测试温度转换场景（简化版）
  let celsius = 20
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(70, fahrenheit)
  
  // 测试冰点
  let freezing = @azimuth.add(@azimuth.multiply(0, 2), 30)
  @azimuth.assert_eq(30, freezing)
  
  // 测试沸点
  let boiling = @azimuth.add(@azimuth.multiply(100, 2), 30)
  @azimuth.assert_eq(230, boiling)
}

test "complex_calculation_chain" {
  // 测试复杂计算链
  let base = 5
  let multiplier = 3
  let addend = 10
  
  // 计算 (base * multiplier) + addend
  let step1 = @azimuth.multiply(base, multiplier)
  let result = @azimuth.add(step1, addend)
  @azimuth.assert_eq(25, result)
  
  // 更复杂的计算：((a + b) * c) - d
  let a = 8
  let b = 12
  let c = 3
  let d = 15
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let final_result = @azimuth.add(product, -d)
  @azimuth.assert_eq(45, final_result)
}

test "edge_case_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  
  // 最大值与其他值组合
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, zero))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, zero))
  
  // 最小值与其他值组合
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, zero))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, zero))
  
  // 边界值之间的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "assertion_functions_verification" {
  // 测试断言函数本身
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("MoonBit", "MoonBit")
  
  // 测试断言函数与实际计算结合
  let calc_result = @azimuth.add(17, 25)
  @azimuth.assert_eq(42, calc_result)
  
  let greet_result = @azimuth.greet("Tester")
  @azimuth.assert_eq_string("Hello, Tester!", greet_result)
  
  // 测试条件断言
  @azimuth.assert_true(@azimuth.add(5, 3) > @azimuth.add(2, 3))
  @azimuth.assert_false(@azimuth.multiply(4, 5) < @azimuth.multiply(3, 6))
}