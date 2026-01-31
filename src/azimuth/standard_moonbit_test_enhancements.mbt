// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "subtract_function_basic_functionality" {
  // 测试减法函数的基本功能
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(20, 5))
  assert_eq(-15, subtract(-10, 5))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式的计算
  let x = 8
  let y = 12
  let z = 6
  
  // 测试表达式：(x + y) * z - x / y
  let result = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  assert_eq(119, result)  // (8 + 12) * 6 - ceil(8/12) = 20*6 - 1 = 120 - 1 = 119
  
  // 测试嵌套表达式：x * (y + z) + y * (x - z)
  let nested_result = add(multiply(x, add(y, z)), multiply(y, subtract(x, z)))
  assert_eq(168, nested_result)  // 8*(12+6) + 12*(8-6) = 8*18 + 12*2 = 144 + 24 = 168
}

test "ecommerce_shopping_cart_calculation" {
  // 测试电商购物车计算场景
  let item1_price = 29
  let item1_quantity = 2
  let item2_price = 45
  let item2_quantity = 1
  let item3_price = 15
  let item3_quantity = 3
  let tax_rate = 8  // 8%
  let shipping_cost = 12
  
  // 计算商品小计
  let subtotal1 = multiply(item1_price, item1_quantity)
  let subtotal2 = multiply(item2_price, item2_quantity)
  let subtotal3 = multiply(item3_price, item3_quantity)
  let total_subtotal = add(add(subtotal1, subtotal2), subtotal3)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(total_subtotal, tax_rate), 100)
  
  // 计算总价
  let total_cost = add(add(total_subtotal, tax_amount), shipping_cost)
  
  assert_eq(58, subtotal1)   // 29 * 2 = 58
  assert_eq(45, subtotal2)   // 45 * 1 = 45
  assert_eq(45, subtotal3)   // 15 * 3 = 45
  assert_eq(148, total_subtotal)  // 58 + 45 + 45 = 148
  assert_eq(12, tax_amount)  // ceil(148 * 8 / 100) = ceil(11.84) = 12
  assert_eq(172, total_cost)  // 148 + 12 + 12 = 172
}

test "boundary_values_testing" {
  // 测试边界值
  let large_positive = 100000
  let large_negative = -100000
  let zero_value = 0
  
  // 测试大数运算
  assert_eq(200000, add(large_positive, large_positive))
  assert_eq(0, add(large_positive, large_negative))
  assert_eq(-200000, add(large_negative, large_negative))
  
  // 测试大数乘法（避免溢出）
  assert_eq(10000000000, multiply(large_positive, 100000))
  
  // 测试零的运算
  assert_eq(0, multiply(large_positive, zero_value))
  assert_eq(0, multiply(zero_value, large_negative))
  assert_eq(large_positive, add(large_positive, zero_value))
  
  // 测试极端除法情况
  assert_eq(1, divide_with_ceil(large_positive, large_positive))
  assert_eq(0, divide_with_ceil(zero_value, large_positive))
  assert_eq(0, divide_with_ceil(large_positive, zero_value))
}

test "internationalization_greeting_scenarios" {
  // 测试国际化的问候场景
  let chinese_name = "张三"
  let japanese_name = "田中太郎"
  let korean_name = "김철수"
  let arabic_name = "أحمد"
  let russian_name = "Иван"
  let emoji_name = "🌟🚀💫"
  let mixed_name = "John张三田中"
  
  assert_eq_string("Hello, 张三!", greet(chinese_name))
  assert_eq_string("Hello, 田中太郎!", greet(japanese_name))
  assert_eq_string("Hello, 김철수!", greet(korean_name))
  assert_eq_string("Hello, أحمد!", greet(arabic_name))
  assert_eq_string("Hello, Иван!", greet(russian_name))
  assert_eq_string("Hello, 🌟🚀💫!", greet(emoji_name))
  assert_eq_string("Hello, John张三田中!", greet(mixed_name))
}

test "comprehensive_division_scenarios" {
  // 全面的除法场景测试
  // 测试各种正数除法情况
  assert_eq(1, divide_with_ceil(5, 10))   // 0.5 -> 1
  assert_eq(2, divide_with_ceil(15, 8))   // 1.875 -> 2
  assert_eq(3, divide_with_ceil(20, 7))   // 2.857 -> 3
  assert_eq(10, divide_with_ceil(100, 10)) // 10 -> 10
  
  // 测试负数除法情况
  assert_eq(-1, divide_with_ceil(-5, 10))  // -0.5 -> -1 (向上取整)
  assert_eq(-2, divide_with_ceil(-15, 8))  // -1.875 -> -2 (向上取整)
  assert_eq(-1, divide_with_ceil(15, -8))  // -1.875 -> -1
  assert_eq(2, divide_with_ceil(-15, -8))  // 1.875 -> 2
  
  // 测试特殊情况
  assert_eq(1, divide_with_ceil(1, 1))     // 1 -> 1
  assert_eq(0, divide_with_ceil(0, 100))   // 0 -> 0
  assert_eq(0, divide_with_ceil(100, 0))   // 除数为0，返回0
  assert_eq(0, divide_with_ceil(0, 0))     // 0/0，返回0
}

test "mathematical_laws_verification" {
  // 验证数学定律
  let a = 7
  let b = 11
  let c = 13
  
  // 验证加法结合律：(a + b) + c = a + (b + c)
  let associative_add_left = add(add(a, b), c)
  let associative_add_right = add(a, add(b, c))
  assert_eq(associative_add_left, associative_add_right)
  
  // 验证乘法结合律：(a * b) * c = a * (b * c)
  let associative_mul_left = multiply(multiply(a, b), c)
  let associative_mul_right = multiply(a, multiply(b, c))
  assert_eq(associative_mul_left, associative_mul_right)
  
  // 验证分配律：a * (b + c) = a * b + a * c
  let distributive_left = multiply(a, add(b, c))
  let distributive_right = add(multiply(a, b), multiply(a, c))
  assert_eq(distributive_left, distributive_right)
  
  // 验证减法性质：a - (b + c) = (a - b) - c
  let subtract_property_left = subtract(a, add(b, c))
  let subtract_property_right = subtract(subtract(a, b), c)
  assert_eq(subtract_property_left, subtract_property_right)
}

test "monthly_budget_planning_scenario" {
  // 月度预算规划场景
  let monthly_income = 8000
  let rent = 2500
  let utilities = 300
  let groceries = 800
  let transportation = 400
  let entertainment = 300
  let savings_goal = 2000
  
  // 计算固定支出
  let fixed_expenses = add(add(add(rent, utilities), groceries), transportation)
  
  // 计算可变支出
  let variable_expenses = entertainment
  
  // 计算总支出
  let total_expenses = add(fixed_expenses, variable_expenses)
  
  // 计算可用于储蓄的金额
  let available_for_savings = subtract(monthly_income, total_expenses)
  
  // 计算实际储蓄百分比
  let actual_savings_percentage = divide_with_ceil(multiply(available_for_savings, 100), monthly_income)
  
  assert_eq(4000, fixed_expenses)  // 2500 + 300 + 800 + 400 = 4000
  assert_eq(300, variable_expenses)  // 300
  assert_eq(4300, total_expenses)  // 4000 + 300 = 4300
  assert_eq(3700, available_for_savings)  // 8000 - 4300 = 3700
  assert_eq(47, actual_savings_percentage)  // ceil(3700 * 100 / 8000) = ceil(46.25) = 47
}

test "data_analysis_statistics_scenario" {
  // 数据分析和统计场景（手动计算避免数组操作）
  let data1 = 12
  let data2 = 15
  let data3 = 18
  let data4 = 20
  let data5 = 22
  let data6 = 25
  let data7 = 28
  let data8 = 30
  let data9 = 32
  let data10 = 35
  let n = 10
  
  // 计算总和（手动累加）
  let sum = add(add(add(add(add(add(add(add(add(data1, data2), data3), data4), data5), data6), data7), data8), data9), data10)
  
  // 计算平均值（向上取整）
  let average = divide_with_ceil(sum, n)
  
  // 计算最大值和最小值的差
  let max_value = 35
  let min_value = 12
  let range = subtract(max_value, min_value)
  
  // 计算中位数位置
  let median_position = divide_with_ceil(n, 2)
  
  // 计算数据分组（每3个数据一组）
  let groups_needed = divide_with_ceil(n, 3)
  
  assert_eq(237, sum)  // 12 + 15 + 18 + 20 + 22 + 25 + 28 + 30 + 32 + 35 = 237
  assert_eq(24, average)  // ceil(237 / 10) = ceil(23.7) = 24
  assert_eq(23, range)  // 35 - 12 = 23
  assert_eq(5, median_position)  // ceil(10 / 2) = 5
  assert_eq(4, groups_needed)  // ceil(10 / 3) = 4
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 测试边界情况的减法
  assert_eq(0, subtract(5, 5))
  assert_eq(-5, subtract(0, 5))
  assert_eq(5, subtract(5, 0))
  assert_eq(10, subtract(5, -5))
  
  // 测试极端值的除法
  assert_eq(0, divide_with_ceil(-1, 1000000))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  
  // 测试字符串边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \t!", greet("\t"))
  
  // 测试复杂表达式的健壮性
  let complex_result = add(
    multiply(divide_with_ceil(subtract(100, 25), 3), 4),
    subtract(divide_with_ceil(multiply(15, 6), 8), 2)
  )
  assert_eq(110, complex_result)  // ceil(75/3)*4 + ceil(90/8)-2 = 25*4 + 12-2 = 100 + 10 = 110
}