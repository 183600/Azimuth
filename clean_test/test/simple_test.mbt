test "basic_add_test" {
  let result = @clean_test.add(2, 3)
  @clean_test.assert_eq(5, result)
}
test "basic_multiply_test" {
  let result = @clean_test.multiply(2, 3)
  @clean_test.assert_eq(6, result)
}
test "basic_greet_test" {
  let result = @clean_test.greet("World")
  @clean_test.assert_eq_string("Hello, World!", result)
}

// 新增的 MoonBit 测试用例
test "add_associative_property" {
  // 测试加法结合律: (a + b) + c = a + (b + c)
  let a = 100
  let b = 200
  let c = 300
  
  let result1 = @clean_test.add(@clean_test.add(a, b), c)
  let result2 = @clean_test.add(a, @clean_test.add(b, c))
  @clean_test.assert_eq(result1, result2)
}

test "multiply_commutative_property" {
  // 测试乘法交换律: a * b = b * a
  let a = 12
  let b = 15
  
  let result1 = @clean_test.multiply(a, b)
  let result2 = @clean_test.multiply(b, a)
  @clean_test.assert_eq(result1, result2)
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的周长
  let length = 10
  let width = 5
  let perimeter = @clean_test.add(@clean_test.multiply(2, length), @clean_test.multiply(2, width))
  @clean_test.assert_eq(30, perimeter)
}

test "financial_calculation_scenario" {
  // 测试金融计算场景：复利计算（简化版）
  let principal = 1000
  let rate = 5
  let years = 3
  // 简化的复利计算：本息和 = 本金 + 本金 × 利率 × 年数
  let interest = @clean_test.multiply(principal, @clean_test.multiply(rate, years))
  let total = @clean_test.add(principal, interest)
  @clean_test.assert_eq(16000, total)  // 1000 + 1000*5*3 = 16000，表示1600元
}

test "shopping_cart_calculation" {
  // 测试实际应用场景：购物车计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let item3_price = 99
  let item3_quantity = 1
  
  // 计算每种商品的总价
  let total1 = @clean_test.multiply(item1_price, item1_quantity)
  let total2 = @clean_test.multiply(item2_price, item2_quantity)
  let total3 = @clean_test.multiply(item3_price, item3_quantity)
  
  // 计算购物车总价
  let cart_total = @clean_test.add(@clean_test.add(total1, total2), total3)
  
  // 验证总价：299*2 + 199*3 + 99*1 = 598 + 597 + 99 = 1294
  @clean_test.assert_eq(1294, cart_total)
}

test "temperature_conversion" {
  // 测试实际应用场景：温度转换（简化版）
  // 摄氏度转华氏度：F = C * 9/5 + 32
  // 由于我们只有整数运算，我们使用近似值：F = C * 2 + 30
  
  let celsius_temp = 25
  let fahrenheit_temp = @clean_test.add(@clean_test.multiply(celsius_temp, 2), 30)
  @clean_test.assert_eq(80, fahrenheit_temp)  // 25 * 2 + 30 = 80
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = @clean_test.add(@clean_test.multiply(freezing_celsius, 2), 30)
  @clean_test.assert_eq(30, freezing_fahrenheit)  // 0 * 2 + 30 = 30
  
  // 测试沸点温度
  let boiling_celsius = 100
  let boiling_fahrenheit = @clean_test.add(@clean_test.multiply(boiling_celsius, 2), 30)
  @clean_test.assert_eq(230, boiling_fahrenheit)  // 100 * 2 + 30 = 230
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试非常长的字符串
  let long_name = "ThisIsAVeryLongNameThatMightCauseIssuesInSomeSystemsButShouldWorkFineInMoonBit"
  let result = @clean_test.greet(long_name)
  @clean_test.assert_eq_string("Hello, " + long_name + "!", result)
  
  // 测试包含引号的字符串
  let quoted_name = "\"John Doe\""
  let result2 = @clean_test.greet(quoted_name)
  @clean_test.assert_eq_string("Hello, \"John Doe\"!", result2)
  
  // 测试包含换行符的字符串
  let newline_name = "Line1\nLine2"
  let result3 = @clean_test.greet(newline_name)
  @clean_test.assert_eq_string("Hello, Line1\nLine2!", result3)
}

test "mathematical_series_sum" {
  // 计算等差数列的和：1 + 3 + 5 + ... + 19（前10个奇数）
  // 使用公式：n × (首项 + 末项) / 2
  let n = 10
  let first = 1
  let last = 19
  
  let sum = @clean_test.multiply(n, @clean_test.add(first, last)) / 2
  @clean_test.assert_eq(100, sum)  // 10 × (1 + 19) / 2 = 100
}

// 新增的核心功能测试用例
test "basic_addition_functionality" {
  // 测试基本加法功能
  @clean_test.assert_eq(5, @clean_test.add(2, 3))
  @clean_test.assert_eq(0, @clean_test.add(0, 0))
  @clean_test.assert_eq(-5, @clean_test.add(-2, -3))
  @clean_test.assert_eq(10, @clean_test.add(7, 3))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  @clean_test.assert_eq(6, @clean_test.multiply(2, 3))
  @clean_test.assert_eq(0, @clean_test.multiply(0, 5))
  @clean_test.assert_eq(-6, @clean_test.multiply(-2, 3))
  @clean_test.assert_eq(15, @clean_test.multiply(3, 5))
}

test "greeting_functionality" {
  // 测试问候功能
  @clean_test.assert_eq_string("Hello, World!", @clean_test.greet("World"))
  @clean_test.assert_eq_string("Hello, MoonBit!", @clean_test.greet("MoonBit"))
  @clean_test.assert_eq_string("Hello, !", @clean_test.greet(""))
}

test "zero_value_operations" {
  // 测试零值运算
  @clean_test.assert_eq(0, @clean_test.multiply(5, 0))
  @clean_test.assert_eq(0, @clean_test.multiply(0, 0))
  @clean_test.assert_eq(5, @clean_test.add(5, 0))
}

test "large_number_operations" {
  // 测试大数运算
  @clean_test.assert_eq(2000000, @clean_test.add(1000000, 1000000))
  @clean_test.assert_eq(1000000, @clean_test.multiply(1000, 1000))
  @clean_test.assert_eq(10000, @clean_test.multiply(100, 100))
}

test "combined_operations" {
  // 测试组合运算
  let result1 = @clean_test.add(@clean_test.multiply(2, 3), @clean_test.multiply(4, 5))
  @clean_test.assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = @clean_test.add(@clean_test.multiply(2, 2), @clean_test.multiply(3, 3))
  @clean_test.assert_eq(13, result2)  // 2*2 + 3*3 = 4 + 9 = 13
}

test "practical_packaging_calculation" {
  // 实际应用场景：包装计算
  let total_items = 25
  let items_per_box = 6
  let mut boxes_needed = total_items / items_per_box
  if total_items % items_per_box > 0 {
    boxes_needed = boxes_needed + 1
  }
  @clean_test.assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 另一个包装场景
  let total_items2 = 47
  let items_per_box2 = 12
  let mut boxes_needed2 = total_items2 / items_per_box2
  if total_items2 % items_per_box2 > 0 {
    boxes_needed2 = boxes_needed2 + 1
  }
  @clean_test.assert_eq(4, boxes_needed2)  // ceil(47/12) = 4
}

test "special_characters_handling" {
  // 测试特殊字符处理
  @clean_test.assert_eq_string("Hello, 世界!", @clean_test.greet("世界"))
  @clean_test.assert_eq_string("Hello, 🚀!", @clean_test.greet("🚀"))
  @clean_test.assert_eq_string("Hello, 123!", @clean_test.greet("123"))
  @clean_test.assert_eq_string("Hello, !@#$!", @clean_test.greet("!@#$"))
}

test "overflow_protection_add" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  @clean_test.assert_eq(max_val, @clean_test.add(max_val, 1))
  @clean_test.assert_eq(max_val, @clean_test.add(max_val, max_val))
  @clean_test.assert_eq(min_val, @clean_test.add(min_val, -1))
  @clean_test.assert_eq(min_val, @clean_test.add(min_val, min_val))
}

test "overflow_protection_multiply" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  @clean_test.assert_eq(max_val, @clean_test.multiply(max_val, 2))
  @clean_test.assert_eq(min_val, @clean_test.multiply(min_val, 2))
  @clean_test.assert_eq(min_val, @clean_test.multiply(min_val, -1))
}

// 新增的标准 MoonBit 测试用例
test "comprehensive_add_function" {
  // 全面测试加法函数的各种情况
  @clean_test.assert_eq(10, @clean_test.add(7, 3))
  @clean_test.assert_eq(0, @clean_test.add(0, 0))
  @clean_test.assert_eq(-10, @clean_test.add(-7, -3))
  @clean_test.assert_eq(4, @clean_test.add(-7, 11))
  @clean_test.assert_eq(100, @clean_test.add(50, 50))
}

test "greet_international_characters" {
  // 测试问候函数的国际字符支持
  @clean_test.assert_eq_string("Hello, 世界!", @clean_test.greet("世界"))
  @clean_test.assert_eq_string("Hello, café!", @clean_test.greet("café"))
  @clean_test.assert_eq_string("Hello, 🚀!", @clean_test.greet("🚀"))
  @clean_test.assert_eq_string("Hello, empty!", @clean_test.greet("empty"))
}

test "mathematical_distributive_law" {
  // 验证数学分配律: a * (b + c) = a * b + a * c
  let a = 6
  let b = 4
  let c = 3
  
  let left_side = @clean_test.multiply(a, @clean_test.add(b, c))
  let right_side = @clean_test.add(
    @clean_test.multiply(a, b),
    @clean_test.multiply(a, c)
  )
  
  @clean_test.assert_eq(left_side, right_side)  // 6*(4+3) = 6*4 + 6*3 = 42
}

test "real_world_packaging_calculation" {
  // 真实世界包装计算场景
  let total_items = 47
  let items_per_box = 12
  
  // 计算需要的箱子数量
  let mut boxes_needed = total_items / items_per_box
  if total_items % items_per_box > 0 {
    boxes_needed = boxes_needed + 1
  }
  
  // 计算总容量
  let total_capacity = @clean_test.multiply(boxes_needed, items_per_box)
  
  // 验证结果
  @clean_test.assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  @clean_test.assert_eq(48, total_capacity)  // 4 * 12 = 48
}

// 新增的标准 MoonBit 测试用例
test "add_function_edge_cases" {
  // 加法函数边界情况测试
  @clean_test.assert_eq(2147483647, @clean_test.add(2147483646, 1))
  @clean_test.assert_eq(-2147483648, @clean_test.add(-2147483647, -1))
  @clean_test.assert_eq(0, @clean_test.add(2147483647, -2147483647))
}

test "multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  @clean_test.assert_eq(2147483647, @clean_test.multiply(2147483647, 1))
  @clean_test.assert_eq(-2147483648, @clean_test.multiply(-2147483648, 1))
  @clean_test.assert_eq(0, @clean_test.multiply(2147483647, 0))
}

test "greet_function_unicode_support" {
  // 问候函数Unicode支持测试
  @clean_test.assert_eq_string("Hello, 🌟!", @clean_test.greet("🌟"))
  @clean_test.assert_eq_string("Hello, café!", @clean_test.greet("café"))
  @clean_test.assert_eq_string("Hello, Москва!", @clean_test.greet("Москва"))
  @clean_test.assert_eq_string("Hello, 🏳️‍🌈!", @clean_test.greet("🏳️‍🌈"))
}

test "mathematical_distributive_property" {
  // 验证数学分配律: a * (b + c) = a * b + a * c
  let a = 6
  let b = 4
  let c = 3
  
  let left_side = @clean_test.multiply(a, @clean_test.add(b, c))
  let right_side = @clean_test.add(
    @clean_test.multiply(a, b),
    @clean_test.multiply(a, c)
  )
  
  @clean_test.assert_eq(left_side, right_side)  // 6*(4+3) = 6*4 + 6*3 = 42
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  // 场景：计算项目资源需求
  let team_members = 8
  let tasks_per_member = 5
  let total_tasks = @clean_test.multiply(team_members, tasks_per_member)
  let tasks_per_sprint = 15
  let mut sprints_needed = total_tasks / tasks_per_sprint
  if total_tasks % tasks_per_sprint > 0 {
    sprints_needed = sprints_needed + 1
  }
  @clean_test.assert_eq(3, sprints_needed)  // ceil(40/15) = 3
}

test "overflow_protection_comprehensive" {
  // 溢出保护综合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出保护
  @clean_test.assert_eq(max_val, @clean_test.add(max_val, 1))
  @clean_test.assert_eq(max_val, @clean_test.add(max_val, max_val))
  @clean_test.assert_eq(min_val, @clean_test.add(min_val, -1))
  @clean_test.assert_eq(min_val, @clean_test.add(min_val, min_val))
  
  // 乘法溢出保护
  @clean_test.assert_eq(max_val, @clean_test.multiply(max_val, 2))
  @clean_test.assert_eq(min_val, @clean_test.multiply(min_val, 2))
  @clean_test.assert_eq(min_val, @clean_test.multiply(min_val, -1))
}

test "real_world_inventory_management" {
  // 真实世界库存管理测试
  let total_products = 157
  let box_capacity = 24
  let mut boxes_needed = total_products / box_capacity
  if total_products % box_capacity > 0 {
    boxes_needed = boxes_needed + 1
  }
  
  // 验证需要的箱子数量
  @clean_test.assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = @clean_test.multiply(boxes_needed, box_capacity)
  @clean_test.assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = total_capacity - total_products
  @clean_test.assert_eq(11, remaining_space)
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  @clean_test.assert_eq_string("Hello, World!", @clean_test.greet("World"))
  @clean_test.assert_eq_string("Hello, MoonBit!", @clean_test.greet("MoonBit"))
  @clean_test.assert_eq_string("Hello, !", @clean_test.greet(""))
  @clean_test.assert_eq_string("Hello, 123!", @clean_test.greet("123"))
  @clean_test.assert_eq_string("Hello, 世界!", @clean_test.greet("世界"))
  @clean_test.assert_eq_string("Hello,   !", @clean_test.greet("  "))
  @clean_test.assert_eq_string("Hello, special@chars!", @clean_test.greet("special@chars"))
}