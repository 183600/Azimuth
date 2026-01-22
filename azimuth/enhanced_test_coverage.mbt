// 补充的 MoonBit 测试用例
// 专注于覆盖现有测试中较少涉及的方面

test "assertion_functions_test" {
  // 测试断言函数本身的功能
  assert_eq(10, 10)  // 相等断言
  assert_eq_string("test", "test")  // 字符串相等断言
  assert_true(true)  // 真值断言
  assert_false(false)  // 假值断言
  
  // 测试断言函数的边界情况
  assert_eq(0, 0)
  assert_eq(-1, -1)
  assert_eq_string("", "")
  assert_true(1 == 1)
  assert_false(1 == 0)
}

test "extreme_boundary_values" {
  // 测试极值边界情况
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 大数加法测试
  assert_eq(max_int, add(max_int - 1, 1))
  assert_eq(min_int, add(min_int + 1, -1))
  
  // 大数乘法测试（避免溢出）
  assert_eq(46340, multiply(46340, 1))  // sqrt(2^31-1)
  assert_eq(-46340, multiply(-46340, 1))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(max_int, max_int))
  assert_eq(1, divide_with_ceil(min_int, min_int))
  assert_eq(0, divide_with_ceil(1, max_int))
}

test "advanced_function_combinations" {
  // 测试函数的高级组合使用
  let base1 = 10
  let base2 = 20
  let base3 = 30
  let base4 = 40
  let base5 = 50
  
  // 计算数列的总和与平均值相关的计算
  let sum1 = add(add(base1, base2), add(base3, base4))
  let total_sum = add(sum1, base5)
  assert_eq(150, total_sum)
  
  // 计算加权平均的相关计算
  let weight1 = 1
  let weight2 = 2
  let weight3 = 3
  let weighted_sum = add(multiply(base1, weight1),
                        add(multiply(base2, weight2),
                           multiply(base3, weight3)))
  assert_eq(140, weighted_sum)
  
  // 复杂的包装计算
  let items_per_package = 12
  let total_items = total_sum
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  assert_eq(13, packages_needed)  // ceil(150/12) = 13
}

test "business_logic_scenarios" {
  // 测试实际业务逻辑场景
  
  // 场景1：库存管理计算
  let initial_stock = 1000
  let sale1 = 150
  let sale2 = 200
  let sale3 = 175
  let sale4 = 125
  let sale5 = 180
  let total_sales = add(add(add(sale1, sale2), add(sale3, sale4)), sale5)
  let remaining_stock = add(initial_stock, -total_sales)
  assert_eq(170, remaining_stock)
  
  // 场景2：订单处理和物流计算
  let order1 = 25
  let order2 = 30
  let order3 = 15
  let order4 = 45
  let order5 = 20
  let box_capacity = 24
  let total_orders = add(add(order1, order2), add(order3, order4))
  let final_total = add(total_orders, order5)
  let boxes_needed = divide_with_ceil(final_total, box_capacity)
  assert_eq(6, boxes_needed)  // ceil(135/24) = 6
  
  // 场景3：价格计算和折扣
  let base_price = 50
  let discount1 = 0
  let discount2 = 5
  let quantity1 = 1
  let quantity2 = 5
  
  let price1 = multiply(base_price, quantity1)
  let price2 = multiply(add(base_price, -discount2), quantity2)
  assert_eq(50, price1)
  assert_eq(225, price2)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  
  // 空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, \n\r\t!", greet("\n\r\t"))
  
  // 极长字符串测试
  let long_name = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  let expected_long = "Hello, " + long_name + "!"
  assert_eq_string(expected_long, greet(long_name))
  
  // 特殊Unicode字符
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🎉🚀🌟!", greet("🎉🚀🌟"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  
  // 数字和字母混合
  assert_eq_string("Hello, test123!", greet("test123"))
  assert_eq_string("Hello, 123test!", greet("123test"))
}

test "mathematical_series_and_patterns" {
  // 测试数学序列和模式
  
  // 等差数列计算
  let first_term = 3
  let common_diff = 5
  
  let term1 = first_term
  let term2 = add(term1, common_diff)
  let term3 = add(term2, common_diff)
  let term4 = add(term3, common_diff)
  let term5 = add(term4, common_diff)
  let term6 = add(term5, common_diff)
  
  let series_sum = add(add(add(term1, term2), 
                           add(term3, term4)),
                       add(term5, term6))
  assert_eq(78, series_sum)  // 3+8+13+18+23+28 = 78
  
  // 等比数列计算（简单情况）
  let geo_first = 2
  let ratio = 3
  let geo_term1 = geo_first
  let geo_term2 = multiply(geo_term1, ratio)
  let geo_term3 = multiply(geo_term2, ratio)
  let geo_sum = add(add(geo_term1, geo_term2), geo_term3)
  assert_eq(26, geo_sum)  // 2+6+18 = 26
  
  // 平方数计算
  let num1 = 4
  let num2 = 7
  let square1 = multiply(num1, num1)
  let square2 = multiply(num2, num2)
  let sum_of_squares = add(square1, square2)
  assert_eq(65, sum_of_squares)  // 16+49 = 65
}

test "error_handling_and_robustness" {
  // 测试错误处理和健壮性
  
  // 除零情况的多种测试
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(1, 0))
  
  // 负数除法的边界情况
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(-1, divide_with_ceil(-2, 3))
  assert_eq(-2, divide_with_ceil(-3, 2))
  assert_eq(-5, divide_with_ceil(-21, 5))
  
  // 极小值除法
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(-1, divide_with_ceil(1, -1))
  assert_eq(-1, divide_with_ceil(-1, 1))
}

test "performance_and_stress_testing" {
  // 测试性能和压力情况
  
  // 连续运算测试
  let mut accumulator = 1
  accumulator = multiply(accumulator, 2)
  accumulator = add(accumulator, 3)
  accumulator = multiply(accumulator, 4)
  accumulator = add(accumulator, 5)
  accumulator = multiply(accumulator, 6)
  assert_eq(126, accumulator)  // ((1*2+3)*4+5)*6 = 126
  
  // 嵌套计算压力测试
  let nested_result = divide_with_ceil(
    multiply(
      add(10, 20),
      add(30, 40)
    ),
    add(5, 5)
  )
  assert_eq(210, nested_result)  // ceil((10+20)*(30+40)/10) = ceil(30*70/10) = ceil(2100/10) = 210
  
  // 大量数据处理模拟
  let batch_size = 50
  let items_per_batch = 12
  let total_batches = multiply(batch_size, items_per_batch)
  let containers = divide_with_ceil(total_batches, 100)
  assert_eq(6, containers)  // ceil(50*12/100) = ceil(600/100) = 6
}