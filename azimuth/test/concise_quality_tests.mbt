// 简洁高质量的 MoonBit 测试用例
// 测试 azimuth 库的核心功能

// 导入被测试的模块
use azimuth

// 测试基本算术运算
test basic_arithmetic_operations {
  // 测试加法
  assert_eq(7, azimuth.add(3, 4))
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(-10, azimuth.add(-5, -5))
  
  // 测试乘法
  assert_eq(12, azimuth.multiply(3, 4))
  assert_eq(0, azimuth.multiply(5, 0))
  assert_eq(-15, azimuth.multiply(-3, 5))
  
  // 测试减法
  assert_eq(2, azimuth.subtract(5, 3))
  assert_eq(-8, azimuth.subtract(3, 11))
}

// 测试向上取整除法的各种情况
test divide_with_ceil_comprehensive {
  // 整除情况
  assert_eq(4, azimuth.divide_with_ceil(8, 2))
  assert_eq(-3, azimuth.divide_with_ceil(-9, 3))
  
  // 需要向上取整的情况
  assert_eq(4, azimuth.divide_with_ceil(7, 2))
  assert_eq(3, azimuth.divide_with_ceil(8, 3))
  
  // 边界情况
  assert_eq(0, azimuth.divide_with_ceil(0, 5))
  assert_eq(0, azimuth.divide_with_ceil(10, 0))
  assert_eq(1, azimuth.divide_with_ceil(1, 1))
}

// 测试数学性质和恒等式
test mathematical_properties {
  let x = 15
  let y = 8
  
  // 加法交换律
  assert_eq(azimuth.add(x, y), azimuth.add(y, x))
  
  // 乘法交换律
  assert_eq(azimuth.multiply(x, y), azimuth.multiply(y, x))
  
  // 分配律
  assert_eq(
    azimuth.multiply(x, azimuth.add(y, 2)),
    azimuth.add(azimuth.multiply(x, y), azimuth.multiply(x, 2))
  )
  
  // 零元素性质
  assert_eq(x, azimuth.add(x, 0))
  assert_eq(0, azimuth.multiply(x, 0))
}

// 测试字符串处理功能
test string_processing_functionality {
  // 基本字符串拼接
  assert_eq_string("Hello, World!", azimuth.greet("World"))
  assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  
  // 特殊情况
  assert_eq_string("Hello, !", azimuth.greet(""))
  assert_eq_string("Hello, 123!", azimuth.greet("123"))
  
  // Unicode 支持
  assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  assert_eq_string("Hello, 🚀!", azimuth.greet("🚀"))
}

// 测试业务逻辑场景：包装计算
test business_logic_packaging {
  let items = 47
  let capacity_per_box = 12
  
  // 计算需要的箱子数量
  let boxes_needed = azimuth.divide_with_ceil(items, capacity_per_box)
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = azimuth.multiply(boxes_needed, capacity_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = azimuth.subtract(total_capacity, items)
  assert_eq(1, remaining_space)
}

// 测试复杂计算流水线
test complex_calculation_pipeline {
  // 场景：计算项目完成时间
  let tasks = 25
  let people = 4
  let tasks_per_person_per_day = 2
  
  // 计算每日处理能力
  let daily_capacity = azimuth.multiply(people, tasks_per_person_per_day)
  
  // 计算需要的天数
  let days_needed = azimuth.divide_with_ceil(tasks, daily_capacity)
  assert_eq(4, days_needed)  // ceil(25/(4*2)) = 4
  
  // 验证总处理能力
  let total_capacity = azimuth.multiply(days_needed, daily_capacity)
  assert_eq(32, total_capacity)
}

// 测试错误处理和边界条件
test error_handling_and_boundaries {
  // 除零错误处理
  assert_eq(0, azimuth.divide_with_ceil(10, 0))
  assert_eq(0, azimuth.divide_with_ceil(-10, 0))
  assert_eq(0, azimuth.divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, azimuth.divide_with_ceil(1, 1))
  assert_eq(1, azimuth.divide_with_ceil(-1, -1))
  assert_eq(0, azimuth.divide_with_ceil(0, 1))
  
  // 大数运算
  assert_eq(1000000, azimuth.multiply(1000, 1000))
  assert_eq(2000000, azimuth.add(1000000, 1000000))
}

// 测试真实世界应用场景：活动安排
test real_world_event_planning {
  let participants = 87
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = azimuth.divide_with_ceil(participants, chairs_per_table)
  assert_eq(11, tables_needed)  // ceil(87/8) = 11
  
  // 计算总座位数
  let total_seats = azimuth.multiply(tables_needed, chairs_per_table)
  assert_eq(88, total_seats)
  
  // 计算剩余座位
  let remaining_seats = azimuth.subtract(total_seats, participants)
  assert_eq(1, remaining_seats)
  
  // 验证座位充足性
  assert_true(total_seats >= participants)
}