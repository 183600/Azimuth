// 标准 MoonBit 测试用例
// 为 Azimuth 项目添加的核心功能测试

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
}

test "greet_function_basic" {
  // 测试问候函数的基本功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "zero_value_operations" {
  // 测试零值运算
  @azimuth.assert_eq(100, @azimuth.add(100, 0))
  @azimuth.assert_eq(100, @azimuth.add(0, 100))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
}

test "negative_values_arithmetic" {
  // 测试负数算术
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(0, @azimuth.add(-10, 10))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
}

test "string_edge_cases" {
  // 测试字符串边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet("\n"))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 15
  let b = 27
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "compound_calculations" {
  // 测试复合计算
  let base = 10
  let multiplier = 3
  let offset = 5
  
  // 计算 (base * multiplier) + offset
  let product = @azimuth.multiply(base, multiplier)
  let result = @azimuth.add(product, offset)
  @azimuth.assert_eq(35, result)
  
  // 计算 base + (offset * multiplier)
  let offset_product = @azimuth.multiply(offset, multiplier)
  let result2 = @azimuth.add(base, offset_product)
  @azimuth.assert_eq(25, result2)
}

test "sequential_operations" {
  // 测试连续运算
  let start = 100
  
  // 连续加法
  let step1 = @azimuth.add(start, 10)
  let step2 = @azimuth.add(step1, 20)
  let step3 = @azimuth.add(step2, 30)
  @azimuth.assert_eq(160, step3)
  
  // 连续乘法
  let mul_step1 = @azimuth.multiply(start, 2)
  let mul_step2 = @azimuth.multiply(mul_step1, 3)
  let mul_step3 = @azimuth.multiply(mul_step2, 4)
  @azimuth.assert_eq(2400, mul_step3)
}

test "mixed_type_operations" {
  // 测试混合类型操作
  // 字符串与数字结合
  let num1 = 42
  let num2 = 24
  let sum = @azimuth.add(num1, num2)
  
  @azimuth.assert_eq(66, sum)
  @azimuth.assert_eq_string("Hello, 66!", @azimuth.greet("66"))
  
  // 使用计算结果进行进一步运算
  let doubled = @azimuth.multiply(sum, 2)
  @azimuth.assert_eq(132, doubled)
}

test "identity_elements" {
  // 测试恒等元素
  let value = 42
  
  // 加法恒等元素 (0)
  @azimuth.assert_eq(value, @azimuth.add(value, 0))
  @azimuth.assert_eq(value, @azimuth.add(0, value))
  
  // 乘法恒等元素 (1)
  @azimuth.assert_eq(value, @azimuth.multiply(value, 1))
  @azimuth.assert_eq(value, @azimuth.multiply(1, value))
  
  // 零元性质
  @azimuth.assert_eq(0, @azimuth.multiply(value, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, value))
}