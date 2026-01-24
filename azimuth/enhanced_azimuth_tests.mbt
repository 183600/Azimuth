// 增强的 Azimuth 测试用例
// 包含10个高质量的 MoonBit 标准测试用例

// 导入被测试的模块
use azimuth

// 测试加法函数的边界值和特殊情况
test add_boundary_conditions {
  // 测试大数相加
  azimuth.assert_eq(2000000, azimuth.add(1000000, 1000000))
  
  // 测试正负数相加
  azimuth.assert_eq(0, azimuth.add(1000000, -1000000))
  azimuth.assert_eq(-500000, azimuth.add(-1000000, 500000))
  
  // 测试零值相加
  azimuth.assert_eq(123456, azimuth.add(0, 123456))
  azimuth.assert_eq(-123456, azimuth.add(0, -123456))
}

// 测试乘法函数的数学性质
test multiply_mathematical_properties {
  // 测试乘法交换律
  azimuth.assert_eq(azimuth.multiply(15, 25), azimuth.multiply(25, 15))
  azimuth.assert_eq(azimuth.multiply(-15, 25), azimuth.multiply(25, -15))
  
  // 测试乘法结合律
  azimuth.assert_eq(
    azimuth.multiply(azimuth.multiply(2, 3), 4),
    azimuth.multiply(2, azimuth.multiply(3, 4))
  )
  
  // 测试与零相乘
  azimuth.assert_eq(0, azimuth.multiply(123456, 0))
  azimuth.assert_eq(0, azimuth.multiply(-123456, 0))
}

// 测试问候函数的Unicode和特殊字符处理
test greet_unicode_and_special_chars {
  // 测试各种Unicode字符
  azimuth.assert_eq_string("Hello, 🌟!", azimuth.greet("🌟"))
  azimuth.assert_eq_string("Hello, 🎉!", azimuth.greet("🎉"))
  azimuth.assert_eq_string("Hello, 💻!", azimuth.greet("💻"))
  
  // 测试特殊空白字符
  azimuth.assert_eq_string("Hello, \t\n!", azimuth.greet("\t\n"))
  azimuth.assert_eq_string("Hello,   !", azimuth.greet("   "))
  
  // 测试数字和符号混合
  azimuth.assert_eq_string("Hello, test123!", azimuth.greet("test123"))
  azimuth.assert_eq_string("Hello, @#$%!", azimuth.greet("@#$%"))
}

// 测试向上取整除法的复杂业务场景
test divide_with_ceil_complex_scenarios {
  // 场景：计算项目资源分配
  let total_tasks = 123
  let team_members = 8
  let tasks_per_member = azimuth.divide_with_ceil(total_tasks, team_members)
  azimuth.assert_eq(16, tasks_per_member)  // ceil(123/8) = 16
  
  // 验证分配合理性
  let total_allocated = azimuth.multiply(tasks_per_member, team_members)
  azimuth.assert_eq(128, total_allocated)
  
  // 场景：计算存储空间需求
  let data_size = 987
  let block_size = 64
  let blocks_needed = azimuth.divide_with_ceil(data_size, block_size)
  azimuth.assert_eq(16, blocks_needed)  // ceil(987/64) = 16
}

// 测试复合运算的准确性
test complex_arithmetic_operations {
  // 测试复合运算：(a + b) * c / d
  let a = 12
  let b = 8
  let c = 3
  let d = 5
  
  let step1 = azimuth.add(a, b)  // 20
  let step2 = azimuth.multiply(step1, c)  // 60
  let result = azimuth.divide_with_ceil(step2, d)  // 12
  azimuth.assert_eq(12, result)
  
  // 测试另一个复合运算
  let x = 15
  let y = 4
  let z = 7
  
  let complex_result = azimuth.divide_with_ceil(
    azimuth.multiply(azimuth.add(x, y), z),
    azimuth.subtract(x, y)
  )
  azimuth.assert_eq(35, complex_result)  // ceil((15+4)*7/(15-4)) = ceil(133/11) = 13
}

// 测试错误处理和边界情况
test error_handling_and_edge_cases {
  // 测试除零的各种情况
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(100, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(-100, 0))
  
  // 测试极值情况
  azimuth.assert_eq(1, azimuth.divide_with_ceil(1, 1))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(-1, -1))
  azimuth.assert_eq(-1, azimuth.divide_with_ceil(1, -1))
  
  // 测试大数运算
  azimuth.assert_eq(1000000, azimuth.divide_with_ceil(999999, 1))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(1000000, 1000000))
}

// 测试实际业务计算场景
test real_world_business_calculations {
  // 场景：计算活动座位安排
  let participants = 256
  let tables = 15
  let seats_per_table = azimuth.divide_with_ceil(participants, tables)
  azimuth.assert_eq(18, seats_per_table)  // ceil(256/15) = 18
  
  // 验证总座位数
  let total_seats = azimuth.multiply(seats_per_table, tables)
  azimuth.assert_eq(270, total_seats)
  
  // 场景：计算运输成本
  let packages = 87
  let truck_capacity = 12
  let trucks_needed = azimuth.divide_with_ceil(packages, truck_capacity)
  let cost_per_truck = 150
  let total_cost = azimuth.multiply(trucks_needed, cost_per_truck)
  
  azimuth.assert_eq(8, trucks_needed)  // ceil(87/12) = 8
  azimuth.assert_eq(1200, total_cost)  // 8 * 150
}

// 测试数学恒等式和性质
test mathematical_identities {
  // 测试加法恒等式
  let test_value = 42
  azimuth.assert_eq(test_value, azimuth.add(test_value, 0))
  azimuth.assert_eq(test_value, azimuth.add(0, test_value))
  
  // 测试乘法恒等式
  azimuth.assert_eq(test_value, azimuth.multiply(test_value, 1))
  azimuth.assert_eq(test_value, azimuth.multiply(1, test_value))
  azimuth.assert_eq(0, azimuth.multiply(test_value, 0))
  
  // 测试分配律
  let a = 6
  let b = 7
  let c = 8
  azimuth.assert_eq(
    azimuth.multiply(a, azimuth.add(b, c)),
    azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c))
  )
}

// 测试字符串处理的安全性
test string_processing_security {
  // 测试空值和特殊字符串
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
  azimuth.assert_eq_string("Hello, null!", azimuth.greet("null"))
  azimuth.assert_eq_string("Hello, undefined!", azimuth.greet("undefined"))
  
  // 测试包含特殊字符的字符串
  azimuth.assert_eq_string("Hello, <script>!", azimuth.greet("<script>"))
  azimuth.assert_eq_string("Hello, 'quote'!", azimuth.greet("'quote'"))
  azimuth.assert_eq_string("Hello, \"double quote\"!", azimuth.greet("\"double quote\""))
  
  // 测试长字符串
  let long_string = "This_is_a_very_long_string_that_tests_the_greeting_function_with_extended_input"
  let expected = "Hello, " + long_string + "!"
  azimuth.assert_eq_string(expected, azimuth.greet(long_string))
}

// 测试性能和效率相关的计算
test performance_efficiency_calculations {
  // 测试批量计算效率
  let batch_size = 1000
  let operations_per_batch = 25
  
  // 计算需要的批次数
  let total_items = 12345
  let batches_needed = azimuth.divide_with_ceil(total_items, batch_size)
  azimuth.assert_eq(13, batches_needed)  // ceil(12345/1000) = 13
  
  // 计算总操作数
  let total_operations = azimuth.multiply(batches_needed, operations_per_batch)
  azimuth.assert_eq(325, total_operations)  // 13 * 25
  
  // 测试资源利用率
  let used_capacity = azimuth.multiply(batches_needed - 1, batch_size)
  let remaining_items = azimuth.subtract(total_items, used_capacity)
  azimuth.assert_eq(345, remaining_items)  // 12345 - 12*1000
}