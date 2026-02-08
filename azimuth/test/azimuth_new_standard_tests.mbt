// 标准 MoonBit 测试用例 - Azimuth 项目
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "azimuth_arithmetic_combinations" {
  // 算术运算组合测试
  let a = 12
  let b = 8
  let c = 5
  
  // 复合运算测试
  let result1 = @azimuth.add(@azimuth.multiply(a, b), @azimuth.subtract(c, a))
  let result2 = @azimuth.multiply(@azimuth.add(a, b), @azimuth.divide_with_ceil(c, 2))
  
  @azimuth.assert_eq(91, result1)  // 12*8 + (5-12) = 96 - 7 = 89
  @azimuth.assert_eq(50, result2)  // (12+8) * ceil(5/2) = 20 * 3 = 60
}

test "azimuth_boundary_extreme_values" {
  // 边界值和极值测试
  // 最大常见整数测试
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  
  // 最小值测试
  @azimuth.assert_eq(-2000000, @azimuth.add(-1000000, -1000000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(-1000, 1000))
  
  // 零边界测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 1000000))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1000000))
}

test "azimuth_mathematical_identities" {
  // 数学恒等式验证
  let x = 42
  let y = 17
  
  // 加法恒等式
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))  // 交换律
  
  // 乘法恒等式
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))  // 交换律
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  
  // 减法恒等式
  @azimuth.assert_eq(0, @azimuth.subtract(x, x))
  @azimuth.assert_eq(x, @azimuth.subtract(x, 0))
}

test "azimuth_business_inventory_management" {
  // 库存管理业务场景测试
  let total_products = 347
  let box_capacity = 24
  let warehouse_sections = 8
  
  // 计算需要的箱子数量
  let boxes_needed = @azimuth.divide_with_ceil(total_products, box_capacity)
  
  // 计算每个仓库区域的箱子数量
  let boxes_per_section = @azimuth.divide_with_ceil(boxes_needed, warehouse_sections)
  
  // 验证计算结果
  @azimuth.assert_eq(15, boxes_needed)        // ceil(347/24) = 15
  @azimuth.assert_eq(2, boxes_per_section)    // ceil(15/8) = 2
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  let remaining_space = @azimuth.subtract(total_capacity, total_products)
  @azimuth.assert_eq(13, remaining_space)     // 15*24 - 347 = 360 - 347 = 13
}

test "azimuth_error_handling_special_cases" {
  // 错误处理和特殊情况测试
  // 除零处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极小除数测试
  @azimuth.assert_eq(1000, @azimuth.divide_with_ceil(999, 1))
  @azimuth.assert_eq(500, @azimuth.divide_with_ceil(999, 2))
  
  // 负数除零测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-999, 0))
}

test "azimuth_string_processing_boundaries" {
  // 字符串处理边界测试
  // 空字符串和特殊字符
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 长字符串测试
  let long_name = "VeryLongNameThatMightCauseIssuesInSomeSystems"
  @azimuth.assert_eq_string("Hello, " + long_name + "!", @azimuth.greet(long_name))
  
  // 特殊Unicode字符
  @azimuth.assert_eq_string("Hello, 🌟🚀!", @azimuth.greet("🌟🚀"))
  @azimuth.assert_eq_string("Hello, \n\t\r!", @azimuth.greet("\n\t\r"))
  
  // 数字字符串
  @azimuth.assert_eq_string("Hello, 123456789!", @azimuth.greet("123456789"))
}

test "azimuth_complex_calculation_workflow" {
  // 复杂计算流程测试
  // 场景：计算项目资源分配
  let team_members = 12
  let tasks_per_member = 8
  let hours_per_task = 4
  let working_days = 15
  let hours_per_day = 8
  
  // 计算总任务数
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  // 计算总工时
  let total_hours = @azimuth.multiply(total_tasks, hours_per_task)
  
  // 计算总可用工时
  let available_hours = @azimuth.multiply(working_days, @azimuth.multiply(team_members, hours_per_day))
  
  // 计算需要的额外天数
  let extra_days_needed = @azimuth.divide_with_ceil(@azimuth.subtract(total_hours, available_hours), @azimuth.multiply(team_members, hours_per_day))
  
  @azimuth.assert_eq(96, total_tasks)          // 12 * 8 = 96
  @azimuth.assert_eq(384, total_hours)         // 96 * 4 = 384
  @azimuth.assert_eq(1440, available_hours)    // 15 * 12 * 8 = 1440
  @azimuth.assert_eq(0, extra_days_needed)     // ceil((384-1440)/96) = ceil(-1056/96) = 0
}

test "azimuth_performance_large_numbers" {
  // 性能相关的大数测试
  // 大数运算测试
  let large_num1 = 100000
  let large_num2 = 99999
  
  @azimuth.assert_eq(199999, @azimuth.add(large_num1, large_num2))
  @azimuth.assert_eq(99990000, @azimuth.multiply(large_num1, large_num2))
  @azimuth.assert_eq(1, @azimuth.subtract(large_num1, large_num2))
  
  // 大数除法测试
  @azimuth.assert_eq(100000, @azimuth.divide_with_ceil(100000000, 1000))
  @azimuth.assert_eq(100001, @azimuth.divide_with_ceil(100000999, 1000))
  
  // 极大数测试
  let very_large = 1000000
  @azimuth.assert_eq(2000000, @azimuth.add(very_large, very_large))
  @azimuth.assert_eq(1000000, @azimuth.multiply(very_large, very_large))
}

test "azimuth_data_type_compatibility" {
  // 数据类型兼容性测试
  // 正整数操作
  let positive = 42
  @azimuth.assert_eq(42, @azimuth.add(positive, 0))
  @azimuth.assert_eq(42, @azimuth.multiply(positive, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(positive, 0))
  
  // 负整数操作
  let negative = -42
  @azimuth.assert_eq(-42, @azimuth.add(negative, 0))
  @azimuth.assert_eq(-42, @azimuth.multiply(negative, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(negative, 0))
  
  // 混合符号操作
  @azimuth.assert_eq(0, @azimuth.add(positive, negative))
  @azimuth.assert_eq(-1764, @azimuth.multiply(positive, negative))
  @azimuth.assert_eq(84, @azimuth.subtract(positive, negative))
  
  // 字符串兼容性
  @azimuth.assert_eq_string("Hello, 42!", @azimuth.greet("42"))
  @azimuth.assert_eq_string("Hello, -42!", @azimuth.greet("-42"))
}

test "azimuth_integration_comprehensive" {
  // 综合集成测试
  // 场景：完整的订单处理系统计算
  // 订单数据（用于计算说明）
  let productA_qty = 15
  let productA_price = 3
  let productB_qty = 8
  let productB_price = 5
  let productC_qty = 12
  let productC_price = 2
  let productD_qty = 25
  let productD_price = 4
  
  // 计算订单总数量
  let total_quantity = @azimuth.add(
    @azimuth.add(
      @azimuth.multiply(productA_qty, productA_price),  // ProductA: 15 * 3 = 45
      @azimuth.multiply(productB_qty, productB_price)    // ProductB: 8 * 5 = 40
    ),
    @azimuth.add(
      @azimuth.multiply(productC_qty, productC_price),  // ProductC: 12 * 2 = 24
      @azimuth.multiply(productD_qty, productD_price)   // ProductD: 25 * 4 = 100
    )
  )
  
  // 计算包装需求（每箱最多装30个）
  let packages_needed = @azimuth.divide_with_ceil(total_quantity, 30)
  
  // 计算运输车辆需求（每车最多装5箱）
  let vehicles_needed = @azimuth.divide_with_ceil(packages_needed, 5)
  
  // 验证计算结果
  @azimuth.assert_eq(209, total_quantity)     // 45 + 40 + 24 + 100 = 209
  @azimuth.assert_eq(7, packages_needed)      // ceil(209/30) = 7
  @azimuth.assert_eq(2, vehicles_needed)      // ceil(7/5) = 2
  
  // 测试问候功能在订单确认中的应用
  let customer_name = " valued customer "
  let greeting_message = @azimuth.greet(customer_name)
  @azimuth.assert_eq_string("Hello,  valued customer !", greeting_message)
}