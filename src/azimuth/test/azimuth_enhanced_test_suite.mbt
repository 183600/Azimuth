// Azimuth 项目增强测试套件
// 使用标准 MoonBit 测试语法，提供高质量的综合测试覆盖

test "add_function_comprehensive" {
  // 全面测试 add 函数的各种情况
  // 基本加法
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  
  // 与0相加
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  
  // 负数加法
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  
  // 混合符号加法
  @azimuth.assert_eq(3, @azimuth.add(10, -7))
  @azimuth.assert_eq(-3, @azimuth.add(-10, 7))
}

test "multiply_function_comprehensive" {
  // 全面测试 multiply 函数的各种情况
  // 基本乘法
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  
  // 与0和1相乘
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  
  // 负数乘法
  @azimuth.assert_eq(21, @azimuth.multiply(-3, -7))
  @azimuth.assert_eq(-21, @azimuth.multiply(3, -7))
  @azimuth.assert_eq(-21, @azimuth.multiply(-3, 7))
  
  // 大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "greet_function_comprehensive" {
  // 全面测试 greet 函数的各种情况
  // 基本问候
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  
  // 空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 特殊字符
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  
  // Unicode字符
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
}

test "mathematical_properties_validation" {
  // 验证基本数学性质
  let a = 15
  let b = 7
  let c = 3
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法结合律
  @azimuth.assert_eq(
    @azimuth.add(@azimuth.add(a, b), c),
    @azimuth.add(a, @azimuth.add(b, c))
  )
  
  // 乘法结合律
  @azimuth.assert_eq(
    @azimuth.multiply(@azimuth.multiply(a, b), c),
    @azimuth.multiply(a, @azimuth.multiply(b, c))
  )
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, c)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  )
}

test "boundary_values_extreme" {
  // 测试极端边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值加法测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1000000))
  
  // 最小值加法测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1000000))
  
  // 最大值与最小值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法测试
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "real_world_financial_calculations" {
  // 实际金融计算场景
  let principal = 10000  // 本金
  let rate = 5          // 年利率（百分比）
  let years = 3         // 年数
  
  // 简单利息计算
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(160000, total)  // 10000 + 10000*5*3 = 160000
  
  // 复利计算（简化版）
  let compound_total = @azimuth.multiply(principal, @azimuth.add(1, @azimuth.multiply(rate, years)))
  @azimuth.assert_eq(160000, compound_total)
  
  // 贷款分期计算（简化）
  let loan_amount = 50000
  let monthly_payment = 2500
  let months = @azimuth.divide_with_ceil(loan_amount, monthly_payment)
  @azimuth.assert_eq(20, months)
}

test "geometry_and_physics_calculations" {
  // 几何和物理计算
  // 矩形计算
  let length = 12
  let width = 8
  let rectangle_area = @azimuth.multiply(length, width)
  let rectangle_perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(96, rectangle_area)
  @azimuth.assert_eq(40, rectangle_perimeter)
  
  // 圆形计算（简化：π ≈ 3）
  let radius = 5
  let circle_area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  let circle_circumference = @azimuth.multiply(@azimuth.multiply(2, 3), radius)
  @azimuth.assert_eq(75, circle_area)
  @azimuth.assert_eq(30, circle_circumference)
  
  // 速度计算（简化）
  let distance = 120  // 公里
  let time = 2        // 小时
  let speed = @azimuth.divide_with_ceil(distance, time)
  @azimuth.assert_eq(60, speed)
}

test "complex_business_logic" {
  // 复杂业务逻辑测试
  // 购物车计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let discount_threshold = 1000
  let discount_rate = 10
  let shipping_fee = 20
  let tax_rate = 8
  
  // 计算商品总价
  let subtotal = @azimuth.add(
    @azimuth.multiply(item1_price, item1_quantity),
    @azimuth.multiply(item2_price, item2_quantity)
  )
  
  // 计算折扣
  let discount = if (subtotal > discount_threshold) {
    @azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算最终价格
  let after_discount = @azimuth.add(subtotal, -discount)
  let tax = @azimuth.multiply(after_discount, tax_rate) / 100
  let final_total = @azimuth.add(@azimuth.add(after_discount, tax), shipping_fee)
  
  @azimuth.assert_eq(1294, subtotal)
  @azimuth.assert_eq(129, discount)
  @azimuth.assert_eq(1165, after_discount)
  @azimuth.assert_eq(93, tax)
  @azimuth.assert_eq(1278, final_total)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 测试特殊乘法情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试字符串边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含特殊字符的字符串
  let special_chars = "\"Hello\"\n\tWorld"
  @azimuth.assert_eq_string("Hello, \"Hello\"\n\tWorld!", @azimuth.greet(special_chars))
}

test "statistical_and_analytical_calculations" {
  // 统计分析计算
  // 数据集
  let data1 = 85
  let data2 = 90
  let data3 = 78
  let data4 = 92
  let data5 = 80
  
  // 计算总和
  let sum = @azimuth.add(
    @azimuth.add(@azimuth.add(@azimuth.add(data1, data2), data3), data4),
    data5
  )
  @azimuth.assert_eq(425, sum)
  
  // 计算平均值
  let mean = sum / 5
  @azimuth.assert_eq(85, mean)
  
  // 计算极差
  let max_val = 92
  let min_val = 78
  let range = @azimuth.add(max_val, -min_val)
  @azimuth.assert_eq(14, range)
  
  // 计算方差（简化）
  let diff1 = @azimuth.add(data1, -mean)
  let diff2 = @azimuth.add(data2, -mean)
  let diff3 = @azimuth.add(data3, -mean)
  let diff4 = @azimuth.add(data4, -mean)
  let diff5 = @azimuth.add(data5, -mean)
  
  let variance = @azimuth.add(
    @azimuth.add(@azimuth.add(
      @azimuth.multiply(diff1, diff1),
      @azimuth.multiply(diff2, diff2)
    ), @azimuth.add(
      @azimuth.multiply(diff3, diff3),
      @azimuth.multiply(diff4, diff4)
    )),
    @azimuth.multiply(diff5, diff5)
  ) / 5
  
  @azimuth.assert_eq(26, variance)
}
