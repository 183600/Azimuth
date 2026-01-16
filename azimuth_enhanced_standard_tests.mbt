// Azimuth 项目增强标准测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能

test "add_function_comprehensive" {
  // 测试正数加法
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  
  // 测试负数加法
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  
  // 测试正负数混合加法
  @azimuth.assert_eq(5, @azimuth.add(10, -5))
  
  // 测试与零相加
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-17, @azimuth.add(0, -17))
}

test "multiply_function_comprehensive" {
  // 测试正数乘法
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  
  // 测试负数乘法
  @azimuth.assert_eq(35, @azimuth.multiply(-5, -7))
  
  // 测试正负数混合乘法
  @azimuth.assert_eq(-30, @azimuth.multiply(6, -5))
  
  // 测试与零相乘
  @azimuth.assert_eq(0, @azimuth.multiply(123, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -456))
}

test "greet_function_various_inputs" {
  // 测试标准输入
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试包含空格的输入
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含数字的输入
  @azimuth.assert_eq_string("Hello, Test123!", @azimuth.greet("Test123"))
  
  // 测试包含特殊字符的输入
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
}

test "mathematical_commutative_properties" {
  // 加法交换律验证
  let x = 123
  let y = 456
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律验证
  let a = 12
  let b = 34
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "mathematical_associative_properties" {
  // 加法结合律验证
  let x = 10
  let y = 20
  let z = 30
  let left_assoc = @azimuth.add(@azimuth.add(x, y), z)
  let right_assoc = @azimuth.add(x, @azimuth.add(y, z))
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 乘法结合律验证
  let a = 2
  let b = 3
  let c = 4
  let left_mult = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let right_mult = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(left_mult, right_mult)
}

test "boundary_value_comprehensive" {
  let int_max = 2147483647
  let int_min = -2147483648
  
  // 最大值边界测试
  @azimuth.assert_eq(int_max, @azimuth.add(int_max, 0))
  @azimuth.assert_eq(int_max, @azimuth.add(int_max, 1))
  @azimuth.assert_eq(int_max, @azimuth.multiply(int_max, 1))
  
  // 最小值边界测试
  @azimuth.assert_eq(int_min, @azimuth.add(int_min, 0))
  @azimuth.assert_eq(int_min, @azimuth.add(int_min, -1))
  @azimuth.assert_eq(int_min, @azimuth.multiply(int_min, 1))
  
  // 零值边界测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(int_max, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(int_min, 0))
}

test "practical_application_scenarios" {
  // 场景1：计算购物车总价
  let item1_price = 299
  let item2_price = 199
  let item3_price = 99
  let cart_total = @azimuth.add(@azimuth.add(item1_price, item2_price), item3_price)
  @azimuth.assert_eq(597, cart_total)
  
  // 场景2：计算折扣后的价格（8折）
  let original_price = 1000
  let discount_rate = 80  // 80%
  let discounted_price = @azimuth.multiply(original_price, discount_rate) / 100
  @azimuth.assert_eq(800, discounted_price)
  
  // 场景3：计算矩形面积
  let length = 15
  let width = 8
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(120, area)
}

test "complex_calculation_workflows" {
  // 计算：(a + b) * (c - d) + e
  let a = 10
  let b = 20
  let c = 100
  let d = 50
  let e = 5
  
  let sum_ab = @azimuth.add(a, b)  // 30
  let diff_cd = @azimuth.add(c, -d)  // 50
  let product = @azimuth.multiply(sum_ab, diff_cd)  // 1500
  let final_result = @azimuth.add(product, e)  // 1505
  
  @azimuth.assert_eq(1505, final_result)
}

test "assertion_function_variations" {
  // 测试各种断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  
  // 数值相等断言
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq(-50, -50)
  @azimuth.assert_eq(0, 0)
  
  // 字符串相等断言
  @azimuth.assert_eq_string("MoonBit", "MoonBit")
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("12345", "12345")
}

test "error_handling_and_edge_cases" {
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  let large_sum = @azimuth.add(large_num1, large_num2)
  @azimuth.assert_eq(3000000, large_sum)
  
  let large_product = @azimuth.multiply(large_num1, 2)
  @azimuth.assert_eq(2000000, large_product)
  
  // 测试连续运算
  let chain_result = @azimuth.add(
    @azimuth.multiply(100, 3),
    @azimuth.multiply(200, 2)
  )
  @azimuth.assert_eq(700, chain_result)
  
  // 测试混合运算链
  let mixed_chain = @azimuth.multiply(
    @azimuth.add(50, 25),
    @azimuth.add(10, -5)
  )
  @azimuth.assert_eq(375, mixed_chain)
}