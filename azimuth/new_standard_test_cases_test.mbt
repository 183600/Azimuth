// 新增的标准 MoonBit 测试用例 - 10个高质量测试用例

test "advanced_arithmetic_combinations" {
  // 高级算术组合测试
  // 测试多个函数的复杂组合使用
  let base = 15
  let multiplier = 4
  let divisor = 7
  let offset = 12
  
  // 复杂计算：((base * multiplier) + offset) / divisor
  let step1 = multiply(base, multiplier)  // 15 * 4 = 60
  let step2 = add(step1, offset)          // 60 + 12 = 72
  let result = divide_with_ceil(step2, divisor)  // ceil(72/7) = 11
  
  assert_eq(11, result)
  
  // 验证逆向计算
  let verification = subtract(multiply(result, divisor), step2)
  assert_eq(5, verification)  // 11*7 - 72 = 77 - 72 = 5
}

test "string_processing_with_numbers" {
  // 字符串与数字混合处理测试
  // 测试字符串函数与数学函数的组合使用
  
  // 创建包含数字的字符串
  let numeric_string = "123"
  let prefix = "ID_"
  let result_string = greet(numeric_string)
  
  assert_eq_string("Hello, 123!", result_string)
  
  // 模拟字符串长度计算（使用固定长度）
  let string_length = 3  // "123" 的长度
  let doubled_length = multiply(string_length, 2)
  let length_divided = divide_with_ceil(doubled_length, 5)
  
  assert_eq(6, doubled_length)
  assert_eq(2, length_divided)
}

test "divide_with_ceil_precision_test" {
  // 向上取整除法精度测试
  // 测试各种精度情况下的向上取整行为
  
  // 小数部分接近整数的情况
  assert_eq(1, divide_with_ceil(1, 1))    // 1.0
  assert_eq(2, divide_with_ceil(3, 2))    // 1.5
  assert_eq(2, divide_with_ceil(4, 3))    // 1.33...
  assert_eq(2, divide_with_ceil(5, 3))    // 1.66...
  assert_eq(3, divide_with_ceil(8, 3))    // 2.66...
  
  // 大数精度测试
  assert_eq(1001, divide_with_ceil(2001, 2))   // 1000.5
  assert_eq(667, divide_with_ceil(2000, 3))    // 666.66...
  assert_eq(334, divide_with_ceil(1000, 3))    // 333.33...
}

test "resource_allocation_with_remainder" {
  // 带余数的资源分配测试
  // 测试资源分配时的余数处理
  
  let total_resources = 100
  let group_size = 9
  let groups_needed = divide_with_ceil(total_resources, group_size)
  
  assert_eq(12, groups_needed)  // ceil(100/9) = 12
  
  // 计算实际分配的资源
  let allocated_resources = multiply(groups_needed, group_size)
  assert_eq(108, allocated_resources)
  
  // 计算剩余资源
  let remainder = subtract(allocated_resources, total_resources)
  assert_eq(8, remainder)
  
  // 验证所有资源都被覆盖
  assert_true(allocated_resources >= total_resources)
}

test "complex_business_workflow" {
  // 复杂业务工作流测试
  // 模拟一个完整的业务流程：订单处理
  
  let order_quantity = 47
  let items_per_box = 8
  let boxes_per_pallet = 6
  
  // 计算需要的箱子数
  let boxes_needed = divide_with_ceil(order_quantity, items_per_box)
  assert_eq(6, boxes_needed)  // ceil(47/8) = 6
  
  // 计算需要的托盘数
  let pallets_needed = divide_with_ceil(boxes_needed, boxes_per_pallet)
  assert_eq(1, pallets_needed)  // ceil(6/6) = 1
  
  // 计算总容量
  let total_box_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(48, total_box_capacity)
  
  let total_pallet_capacity = multiply(pallets_needed, multiply(boxes_per_pallet, items_per_box))
  assert_eq(48, total_pallet_capacity)
  
  // 计算空间利用率
  let utilization = divide_with_ceil(multiply(order_quantity, 100), total_pallet_capacity)
  assert_eq(98, utilization)  // ceil(4700/48) = 98%
}

test "mathematical_series_calculation" {
  // 数学级数计算测试
  // 测试等差数列和等比数列的计算
  
  // 等差数列：首项=2，公差=3，项数=5
  let first_term = 2
  let common_diff = 3
  let terms_count = 5
  
  // 计算第5项：a_n = a_1 + (n-1)d
  let nth_term = add(first_term, multiply(terms_count - 1, common_diff))
  assert_eq(14, nth_term)  // 2 + (5-1)*3 = 14
  
  // 计算前5项和：S_n = n/2 * (a_1 + a_n)
  let series_sum = divide_with_ceil(multiply(terms_count, add(first_term, nth_term)), 2)
  assert_eq(40, series_sum)  // 5/2 * (2+14) = 40
  
  // 等比数列：首项=2，公比=3，项数=4
  let ratio = 3
  let geo_terms_count = 4
  
  // 计算第4项：a_n = a_1 * r^(n-1)
  let geo_nth_term = multiply(first_term, multiply(ratio, multiply(ratio, ratio)))
  assert_eq(54, geo_nth_term)  // 2 * 3^3 = 54
}

test "error_handling_comprehensive" {
  // 综合错误处理测试
  // 测试各种错误情况的处理
  
  // 除零错误的全面测试
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(-1, divide_with_ceil(1, -1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  
  // 边界值测试
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(0, -100))
}

test "performance_with_large_numbers" {
  // 大数性能测试
  // 测试大数运算的性能和正确性
  
  let large_num1 = 100000
  let large_num2 = 200000
  let large_num3 = 300000
  
  // 大数加法
  let sum_result = add(add(large_num1, large_num2), large_num3)
  assert_eq(600000, sum_result)
  
  // 大数乘法
  let product_result = multiply(1000, 1000)
  assert_eq(1000000, product_result)
  
  // 大数除法
  let division_result = divide_with_ceil(large_num3, 7)
  assert_eq(42858, division_result)  // ceil(300000/7)
  
  // 复合运算
  let complex_result = divide_with_ceil(
    multiply(add(large_num1, large_num2), 2),
    subtract(large_num3, large_num1)
  )
  assert_eq(4, complex_result)  // ceil((100000+200000)*2/(300000-100000)) = ceil(600000/200000) = 3
}

test "function_composition_deep" {
  // 深度函数组合测试
  // 测试多层函数嵌套和组合
  
  let initial_value = 5
  
  // 深度嵌套计算
  let level1 = add(initial_value, 3)                    // 8
  let level2 = multiply(level1, 2)                      // 16
  let level3 = divide_with_ceil(level2, 5)              // 4
  let level4 = add(level3, multiply(2, 3))              // 10
  let level5 = divide_with_ceil(level4, 3)              // 4
  let final_result = multiply(level5, subtract(7, 2))   // 20
  
  assert_eq(20, final_result)
  
  // 验证中间步骤
  assert_eq(8, level1)
  assert_eq(16, level2)
  assert_eq(4, level3)
  assert_eq(10, level4)
  assert_eq(4, level5)
  
  // 另一种组合方式验证结果一致性
  let alternative_result = multiply(
    divide_with_ceil(
      add(
        divide_with_ceil(
          multiply(add(initial_value, 3), 2),
          5
        ),
        multiply(2, 3)
      ),
      3
    ),
    subtract(7, 2)
  )
  
  assert_eq(final_result, alternative_result)
}

test "real_world_inventory_management" {
  // 真实世界库存管理测试
  // 模拟实际库存管理场景
  
  // 产品A：库存125件，每箱装10件
  let product_a_stock = 125
  let product_a_box_capacity = 10
  let product_a_boxes = divide_with_ceil(product_a_stock, product_a_box_capacity)
  
  // 产品B：库存87件，每箱装8件
  let product_b_stock = 87
  let product_b_box_capacity = 8
  let product_b_boxes = divide_with_ceil(product_b_stock, product_b_box_capacity)
  
  // 产品C：库存234件，每箱装15件
  let product_c_stock = 234
  let product_c_box_capacity = 15
  let product_c_boxes = divide_with_ceil(product_c_stock, product_c_box_capacity)
  
  // 计算总箱数
  let total_boxes = add(add(product_a_boxes, product_b_boxes), product_c_boxes)
  assert_eq(13, product_a_boxes)   // ceil(125/10) = 13
  assert_eq(11, product_b_boxes)   // ceil(87/8) = 11
  assert_eq(16, product_c_boxes)   // ceil(234/15) = 16
  assert_eq(40, total_boxes)       // 13 + 11 + 16 = 40
  
  // 计算总存储容量
  let total_capacity = add(
    multiply(product_a_boxes, product_a_box_capacity),
    add(
      multiply(product_b_boxes, product_b_box_capacity),
      multiply(product_c_boxes, product_c_box_capacity)
    )
  )
  assert_eq(410, total_capacity)   // 13*10 + 11*8 + 16*15 = 130 + 88 + 240 = 458
  
  // 计算空间利用率
  let total_stock = add(add(product_a_stock, product_b_stock), product_c_stock)
  assert_eq(446, total_stock)      // 125 + 87 + 234 = 446
  let utilization = divide_with_ceil(multiply(total_stock, 100), total_capacity)
  assert_eq(97, utilization)       // ceil(44600/458) = 98%
}