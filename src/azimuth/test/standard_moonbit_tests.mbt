// 标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法测试 azimuth 库的核心功能

// 基本加法测试
test "basic_addition" {
  let result = @azimuth.add(10, 20)
  @azimuth.assert_eq(30, result)
}

// 基本乘法测试
test "basic_multiplication" {
  let result = @azimuth.multiply(5, 6)
  @azimuth.assert_eq(30, result)
}

// 基本字符串问候测试
test "basic_greeting" {
  let result = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", result)
}

// 边界值测试：最大值和最小值
test "boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值加法溢出保护
  let overflow_result = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, overflow_result)
  
  // 测试最小值加法下溢保护
  let underflow_result = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(min_val, underflow_result)
  
  // 测试最大值乘法溢出保护
  let mul_overflow = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, mul_overflow)
}

// 负数运算测试
test "negative_operations" {
  // 负数相加
  let neg_add = @azimuth.add(-10, -20)
  @azimuth.assert_eq(-30, neg_add)
  
  // 正负数相加
  let mixed_add = @azimuth.add(50, -20)
  @azimuth.assert_eq(30, mixed_add)
  
  // 负数相乘
  let neg_mul = @azimuth.multiply(-5, -6)
  @azimuth.assert_eq(30, neg_mul)
  
  // 正负数相乘
  let mixed_mul = @azimuth.multiply(5, -6)
  @azimuth.assert_eq(-30, mixed_mul)
}

// 零值运算测试
test "zero_operations" {
  // 加法零值
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 乘法零值
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
}

// 字符串处理测试
test "string_processing" {
  // 空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 特殊字符
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  
  // Unicode字符
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

// 复合运算测试
test "compound_operations" {
  // (a + b) * c
  let a = 5
  let b = 3
  let c = 4
  
  let sum = @azimuth.add(a, b)  // 8
  let product = @azimuth.multiply(sum, c)  // 32
  @azimuth.assert_eq(32, product)
  
  // a * b + c * d
  let d = 2
  let product1 = @azimuth.multiply(a, b)  // 15
  let product2 = @azimuth.multiply(c, d)  // 8
  let result = @azimuth.add(product1, product2)  // 23
  @azimuth.assert_eq(23, result)
}

// 数学性质测试
test "mathematical_properties" {
  let x = 12
  let y = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 加法结合律
  let z = 5
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(x, y), z), @azimuth.add(x, @azimuth.add(y, z)))
}

// 实际应用场景测试
test "real_world_scenario" {
  // 计算矩形周长：2 * (length + width)
  let length = 10
  let width = 6
  
  let sum_sides = @azimuth.add(length, width)  // 16
  let perimeter = @azimuth.multiply(2, sum_sides)  // 32
  @azimuth.assert_eq(32, perimeter)
  
  // 计算折扣价格：原价 - 折扣金额
  let original_price = 200
  let discount_rate = 20  // 20%
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100  // 40
  let final_price = @azimuth.add(original_price, -discount_amount)  // 160
  @azimuth.assert_eq(160, final_price)
}