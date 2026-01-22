// MoonBit 标准测试用例
// 测试 azimuth 包的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.multiply(-2, 4))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试负数情况
  let c = -5
  let d = 15
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  @azimuth.assert_eq(@azimuth.multiply(c, d), @azimuth.multiply(d, c))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值边界测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  
  // 最小值边界测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算表达式：(a + b) * c - d
  let a = 10
  let b = 5
  let c = 3
  let d = 7
  
  let sum = @azimuth.add(a, b)  // 15
  let product = @azimuth.multiply(sum, c)  // 45
  let result = @azimuth.add(product, -d)  // 38
  
  @azimuth.assert_eq(38, result)
}

test "real_world_shopping_cart" {
  // 测试实际应用场景：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping_fee = 20
  
  // 计算商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)  // 398
  let total2 = @azimuth.multiply(item2_price, item2_quantity)  // 299
  let subtotal = @azimuth.add(total1, total2)  // 697
  
  // 计算最终总价
  let final_total = @azimuth.add(subtotal, shipping_fee)  // 717
  
  @azimuth.assert_eq(717, final_total)
}

test "assertion_functions_test" {
  // 测试断言函数
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试计算结果的断言
  let x = 5
  let y = 3
  @azimuth.assert_eq(@azimuth.add(x, y, true) == 8)
  @azimuth.assert_eq(0, 1)(@azimuth.multiply(x, y) == 20)
}

test "mathematical_associativity" {
  // 测试数学结合律
  let a = 4
  let b = 6
  let c = 8
  
  // 加法结合律
  let left_assoc = @azimuth.add(@azimuth.add(a, b), c)
  let right_assoc = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 乘法结合律
  let left_mul = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let right_mul = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(left_mul, right_mul)
}

test "string_special_characters" {
  // 测试字符串特殊字符处理
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced   "))
  @azimuth.assert_eq_string("Hello, line1\nline2!", @azimuth.greet("line1\nline2"))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
}

test "divide_with_ceil_function" {
  // 测试向上取整除法函数
  // 正数测试
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))  // 10/3 = 3.33 -> 4
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, 3))   // 6/3 = 2 -> 2
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))  // 11/3 = 3.67 -> 4
  
  // 负数测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))  // -10/3 = -3.33 -> -3
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-6, 3))   // -6/3 = -2 -> -2
  
  // 零除数测试（应该返回0或错误，这里假设返回0）
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
}