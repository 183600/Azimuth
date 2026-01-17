// 标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-25, @azimuth.multiply(-5, 5))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  let a = 12
  let b = 17
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算 (a + b) * c - d
  let a = 10
  let b = 15
  let c = 3
  let d = 20
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(55, result)
}

test "real_world_application" {
  // 测试实际应用场景：计算矩形面积和周长
  let length = 8
  let width = 6
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(48, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(28, perimeter)
}

test "negative_number_operations" {
  // 测试负数运算
  let neg_a = -10
  let neg_b = -5
  let pos_c = 7
  
  // 负数相加
  @azimuth.assert_eq(-15, @azimuth.add(neg_a, neg_b))
  
  // 正负数相加
  @azimuth.assert_eq(-3, @azimuth.add(neg_a, pos_c))
  
  // 负数相乘
  @azimuth.assert_eq(50, @azimuth.multiply(neg_a, neg_b))
  
  // 正负数相乘
  @azimuth.assert_eq(-70, @azimuth.multiply(neg_a, pos_c))
}

test "string_special_characters" {
  // 测试特殊字符字符串处理
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced   "))
  @azimuth.assert_eq_string("Hello, emoji🚀!", @azimuth.greet("emoji🚀"))
}

test "sequential_operations" {
  // 测试连续运算
  // 计算 (((1 + 2) * 3) + 4) * 5
  let step1 = @azimuth.add(1, 2)
  let step2 = @azimuth.multiply(step1, 3)
  let step3 = @azimuth.add(step2, 4)
  let result = @azimuth.multiply(step3, 5)
  
  @azimuth.assert_eq(95, result)
}

test "assertion_functions" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试复合条件
  let x = 5
  let y = 10
  @azimuth.assert_true(@azimuth.add(x, y) == 15)
  @azimuth.assert_true(@azimuth.multiply(x, y) == 50)
}