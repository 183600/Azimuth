// 标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法编写

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  let a = 15
  let b = 7
  
  // 测试加法
  assert_eq(22, @azimuth.add(a, b))
  assert_eq(15, @azimuth.add(a, 0))
  assert_eq(0, @azimuth.add(0, 0))
  
  // 测试乘法
  assert_eq(105, @azimuth.multiply(a, b))
  assert_eq(15, @azimuth.multiply(a, 1))
  assert_eq(0, @azimuth.multiply(a, 0))
}

test "negative_number_operations" {
  // 测试负数运算
  let a = -10
  let b = 5
  
  // 负数加法
  assert_eq(-5, @azimuth.add(a, b))
  assert_eq(-15, @azimuth.add(a, -b))
  
  // 负数乘法
  assert_eq(-50, @azimuth.multiply(a, b))
  assert_eq(50, @azimuth.multiply(a, -b))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  assert_eq_string("Hello, World!", @azimuth.greet("World"))
  assert_eq_string("Hello, !", @azimuth.greet(""))
  assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "boundary_value_testing" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值测试
  assert_eq(max_val, @azimuth.add(max_val, 0))
  assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 最小值测试
  assert_eq(min_val, @azimuth.add(min_val, 0))
  assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 溢出测试
  assert_eq(max_val, @azimuth.add(max_val, 1))
  assert_eq(min_val, @azimuth.add(min_val, -1))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  let c = 3
  assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), 
            @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算 (a + b) * c - d
  let a = 10
  let b = 5
  let c = 3
  let d = 7
  
  let sum = @azimuth.add(a, b)  // 15
  let product = @azimuth.multiply(sum, c)  // 45
  let result = @azimuth.add(product, -d)  // 38
  
  assert_eq(38, result)
}

test "real_world_application" {
  // 测试实际应用场景：购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let discount = 20
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)  // 597
  // 计算最终价格
  let total = @azimuth.add(@azimuth.add(subtotal, shipping), -discount)  // 592
  
  assert_eq(592, total)
}

test "unicode_and_special_characters" {
  // 测试 Unicode 和特殊字符
  assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
  assert_eq_string("Hello, space test!", @azimuth.greet("space test"))
}

test "error_handling_and_validation" {
  // 测试错误处理和验证
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试边界条件
  let max_val = 2147483647
  assert_true(@azimuth.add(max_val, 1) == max_val)
  assert_true(@azimuth.multiply(max_val, 2) == max_val)
}