// 高质量 MoonBit 测试用例集
// 覆盖各种边界条件和实际应用场景

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  // 加法恒等元
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(-42, @azimuth.add(-42, 0))
  
  // 乘法恒等元
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(-42, @azimuth.multiply(-42, 1))
  
  // 乘法零元
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(-42, 0))
}

test "associative_properties_verification" {
  // 验证结合律性质
  let a = 12
  let b = 34
  let c = 56
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let left_assoc = @azimuth.add(@azimuth.add(a, b), c)
  let right_assoc = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  let mult_left = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let mult_right = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(mult_left, mult_right)
}

test "distributive_property_verification" {
  // 验证分配律性质: a * (b + c) = a * b + a * c
  let a = 7
  let b = 13
  let c = 19
  
  // a * (b + c)
  let sum = @azimuth.add(b, c)
  let left_side = @azimuth.multiply(a, sum)
  
  // a * b + a * c
  let product1 = @azimuth.multiply(a, b)
  let product2 = @azimuth.multiply(a, c)
  let right_side = @azimuth.add(product1, product2)
  
  @azimuth.assert_eq(left_side, right_side)
}

test "divide_with_ceil_real_world_applications" {
  // 测试向上取整除法的实际应用场景
  
  // 场景1：计算需要的容器数量
  let items = 25
  let container_capacity = 6
  let containers_needed = @azimuth.divide_with_ceil(items, container_capacity)
  @azimuth.assert_eq(5, containers_needed)  // 25/6 = 4.17，向上取整为5
  
  // 场景2：计算需要的运输次数
  let packages = 47
  let truck_capacity = 10
  let trips_needed = @azimuth.divide_with_ceil(packages, truck_capacity)
  @azimuth.assert_eq(5, trips_needed)  // 47/10 = 4.7，向上取整为5
  
  // 场景3：计算需要的页数
  let words = 1050
  let words_per_page = 300
  let pages_needed = @azimuth.divide_with_ceil(words, words_per_page)
  @azimuth.assert_eq(4, pages_needed)  // 1050/300 = 3.5，向上取整为4
}

test "negative_number_divide_with_ceil" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))  // -10/3 = -3.33，向上取整为-3
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))  // -11/3 = -3.67，向上取整为-4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))  // -10/-3 = 3.33，向上取整为4
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))   // 10/-3 = -3.33，向上取整为-3
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 100))   // -1/100 = -0.01，向上取整为0
}

test "string_greeting_multilingual_support" {
  // 测试多语言问候支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Mundo!", @azimuth.greet("Mundo"))
  @azimuth.assert_eq_string("Hello, Мир!", @azimuth.greet("Мир"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, Mr. Smith!", @azimuth.greet("Mr. Smith"))
  @azimuth.assert_eq_string("Hello, user_123!", @azimuth.greet("user_123"))
}

test "complex_calculation_pipeline" {
  // 测试复杂的计算流水线
  // 模拟实际业务逻辑：计算订单总价
  
  // 基础价格
  let base_price = 100
  
  // 添加税费 (20%)
  let tax_amount = @azimuth.multiply(base_price, 20)  // 2000 (代表20.00%)
  let price_with_tax = @azimuth.add(base_price, @azimuth.divide_with_ceil(tax_amount, 100))  // 120
  
  // 添加运费
  let shipping = 15
  let subtotal = @azimuth.add(price_with_tax, shipping)  // 135
  
  // 批量折扣 (购买3件)
  let quantity = 3
  let total_before_discount = @azimuth.multiply(subtotal, quantity)  // 405
  
  // 折扣金额 (10%)
  let discount_amount = @azimuth.multiply(total_before_discount, 10)
  let final_total = @azimuth.add(total_before_discount, -@azimuth.divide_with_ceil(discount_amount, 100))
  
  @azimuth.assert_eq(364, final_total)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试接近边界的加法
  @azimuth.assert_eq(max_int, @azimuth.add(max_int - 1, 1))
  @azimuth.assert_eq(min_int, @azimuth.add(min_int + 1, -1))
  
  // 测试接近边界的乘法
  @azimuth.assert_eq(max_int, @azimuth.multiply(46340, 46341))
  @azimuth.assert_eq(min_int, @azimuth.multiply(-46340, 46341))
  
  // 测试边界值的除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(max_int, max_int))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(min_int, min_int))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(1, max_int))
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  
  // 除数为零的处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 空字符串和特殊字符的问候
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, \t\n\r!", @azimuth.greet("\t\n\r"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  
  // 零值运算
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
}