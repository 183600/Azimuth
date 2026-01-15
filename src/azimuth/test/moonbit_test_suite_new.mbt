// MoonBit 测试套件 - 新增测试用例
// 覆盖基本功能和边界情况

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-25, @azimuth.multiply(-5, 5))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
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
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值边界测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值边界测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "zero_value_operations" {
  // 测试零值运算
  let test_value = 42
  
  // 加法零值测试
  @azimuth.assert_eq(test_value, @azimuth.add(test_value, 0))
  @azimuth.assert_eq(test_value, @azimuth.add(0, test_value))
  
  // 乘法零值测试
  @azimuth.assert_eq(0, @azimuth.multiply(test_value, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, test_value))
}

test "negative_number_properties" {
  // 测试负数性质
  let positive = 15
  let negative = -10
  
  // 正负数相加
  @azimuth.assert_eq(5, @azimuth.add(positive, negative))
  @azimuth.assert_eq(-5, @azimuth.add(negative, positive))
  
  // 负数相乘得正
  @azimuth.assert_eq(100, @azimuth.multiply(negative, negative))
  
  // 正负数相乘得负
  @azimuth.assert_eq(-150, @azimuth.multiply(positive, negative))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算 (a + b) * c - d
  let a = 8
  let b = 7
  let c = 3
  let d = 10
  
  let sum_ab = @azimuth.add(a, b)  // 15
  let product = @azimuth.multiply(sum_ab, c)  // 45
  let result = @azimuth.add(product, -d)  // 35
  
  @azimuth.assert_eq(35, result)
}

test "assert_function_operations" {
  // 测试断言函数操作
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
}

test "real_world_application" {
  // 测试实际应用场景：计算矩形面积和周长
  let length = 10
  let width = 6
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(60, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(32, perimeter)
}

test "edge_case_combinations" {
  // 测试边界情况组合
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  
  // 边界值与零的组合
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, zero))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, zero))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, zero))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, zero))
  
  // 边界值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}