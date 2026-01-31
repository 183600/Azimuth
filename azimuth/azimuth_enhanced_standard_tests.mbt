// Azimuth 增强标准测试用例
// 包含10个高质量的标准 MoonBit 测试用例，覆盖核心功能和各种场景

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  // 测试加法运算
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(2, add(-5, 7))
  
  // 测试乘法运算
  assert_eq(21, multiply(3, 7))
  assert_eq(0, multiply(5, 0))
  assert_eq(-14, multiply(-2, 7))
  assert_eq(14, multiply(-2, -7))
  
  // 测试减法运算
  assert_eq(-4, subtract(3, 7))
  assert_eq(5, subtract(5, 0))
  assert_eq(-9, subtract(-2, 7))
  assert_eq(9, subtract(7, -2))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律验证
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律验证
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法结合律验证
  assert_eq(add(add(x, y), 5), add(x, add(y, 5)))
  
  // 乘法结合律验证
  assert_eq(multiply(multiply(x, y), 2), multiply(x, multiply(y, 2)))
  
  // 分配律验证
  assert_eq(multiply(x, add(y, 3)), add(multiply(x, y), multiply(x, 3)))
  
  // 零元素性质验证
  assert_eq(x, add(x, 0))
  assert_eq(0, multiply(x, 0))
  assert_eq(x, multiply(x, 1))
}

test "boundary_conditions_test" {
  // 边界条件测试
  // 测试极值情况
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(0, multiply(999999, 0))
  assert_eq(999999, multiply(999999, 1))
  
  // 测试除法边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(100, divide_with_ceil(100, 1))
  assert_eq(1, divide_with_ceil(99, 99))
  assert_eq(0, divide_with_ceil(0, 99))
  assert_eq(0, divide_with_ceil(99, 0))
  
  // 测试减法边界情况
  assert_eq(0, subtract(5, 5))
  assert_eq(5, subtract(5, 0))
  assert_eq(-5, subtract(0, 5))
}

test "negative_number_operations" {
  // 负数运算测试
  // 负数加法测试
  assert_eq(-10, add(-3, -7))
  assert_eq(4, add(-3, 7))
  assert_eq(-4, add(3, -7))
  
  // 负数乘法测试
  assert_eq(21, multiply(-3, -7))
  assert_eq(-21, multiply(-3, 7))
  assert_eq(-21, multiply(3, -7))
  
  // 负数除法测试
  assert_eq(-2, divide_with_ceil(-10, 5))
  assert_eq(-1, divide_with_ceil(-3, 5))
  assert_eq(2, divide_with_ceil(-10, -5))
  assert_eq(-2, divide_with_ceil(10, -5))
  
  // 负数减法测试
  assert_eq(4, subtract(-3, -7))
  assert_eq(-10, subtract(-3, 7))
  assert_eq(10, subtract(7, -3))
}

test "large_number_computations" {
  // 大数计算测试
  let large_a = 100000
  let large_b = 250000
  
  // 大数加法测试
  assert_eq(350000, add(large_a, large_b))
  assert_eq(200000, add(large_a, large_a))
  
  // 大数乘法测试
  assert_eq(25000000000, multiply(large_a, large_b))
  assert_eq(10000000000, multiply(large_a, large_a))
  
  // 大数除法测试
  assert_eq(1, divide_with_ceil(large_a, large_b))
  assert_eq(2, divide_with_ceil(large_b, large_a))
  assert_eq(100000, divide_with_ceil(large_a, 1))
  
  // 大数减法测试
  assert_eq(-150000, subtract(large_a, large_b))
  assert_eq(0, subtract(large_a, large_a))
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  // 基本字符串测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 空字符串和特殊字符测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  
  // 数字和字母组合测试
  assert_eq_string("Hello, test123!", greet("test123"))
  assert_eq_string("Hello, ABC!", greet("ABC"))
  assert_eq_string("Hello, xyz!", greet("xyz"))
  
  // 长字符串测试
  assert_eq_string("Hello, this is a very long string!", greet("this is a very long string"))
}

test "business_logic_packaging" {
  // 包装业务逻辑测试
  // 场景：计算包装箱需求
  let total_items = 157
  let box_capacity = 24
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  assert_eq(7, boxes_needed)
  
  // 计算总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(11, remaining_space)
  
  // 验证空间利用率
  assert_true(total_items <= total_capacity)
  assert_true(remaining_space < box_capacity)
}

test "error_handling_division" {
  // 除法错误处理测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(1000000, 0))
  assert_eq(0, divide_with_ceil(-1000000, 0))
  
  // 极小除数测试
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(-1000000, divide_with_ceil(-1000000, 1))
  assert_eq(1000000, divide_with_ceil(1000000, -1))
  assert_eq(-1000000, divide_with_ceil(-1000000, -1))
  
  // 相等数值测试
  assert_eq(1, divide_with_ceil(500, 500))
  assert_eq(1, divide_with_ceil(-500, -500))
  assert_eq(-1, divide_with_ceil(500, -500))
  assert_eq(-1, divide_with_ceil(-500, 500))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目预算和资源分配
  
  let team_members = 8
  let hourly_rate = 75
  let hours_per_day = 8
  let project_days = 15
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, project_days))
  assert_eq(960, total_hours)  // 8 * 8 * 15 = 960
  
  // 计算总预算
  let total_budget = multiply(total_hours, hourly_rate)
  assert_eq(72000, total_budget)  // 960 * 75 = 72000
  
  // 计算支付周期（每周期支付不超过10000）
  let payment_limit = 10000
  let payment_cycles = divide_with_ceil(total_budget, payment_limit)
  assert_eq(8, payment_cycles)  // ceil(72000/10000) = 8
  
  // 计算平均每期支付金额
  let average_payment = divide_with_ceil(total_budget, payment_cycles)
  assert_eq(9000, average_payment)  // ceil(72000/8) = 9000
  
  // 验证总支付金额不超过预算
  let total_payments = multiply(payment_cycles - 1, average_payment)
  let final_payment = subtract(total_budget, total_payments)
  assert_true(final_payment <= average_payment)
}

test "international_character_support" {
  // 国际字符支持测试
  // 中文字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 北京!", greet("北京"))
  
  // 日文字符测试
  assert_eq_string("Hello, こんにちは!", greet("こんにちは"))
  assert_eq_string("Hello, 東京!", greet("東京"))
  
  // 韩文字符测试
  assert_eq_string("Hello, 안녕하세요!", greet("안녕하세요"))
  assert_eq_string("Hello, 서울!", greet("서울"))
  
  // 阿拉伯字符测试
  assert_eq_string("Hello, مرحبا!", greet("مرحبا"))
  
  // 俄文字符测试
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, Привет!", greet("Привет"))
  
  // 欧洲特殊字符测试
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, naïve!", greet("naïve"))
  assert_eq_string("Hello, résumé!", greet("résumé"))
  assert_eq_string("Hello, piñata!", greet("piñata"))
  
  // Emoji 表情符号测试
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 🎉!", greet("🎉"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
  assert_eq_string("Hello, 👋!", greet("👋"))
  
  // 混合字符测试
  assert_eq_string("Hello, Hello世界🌟!", greet("Hello世界🌟"))
  assert_eq_string("Hello, Test123测试!", greet("Test123测试"))
}